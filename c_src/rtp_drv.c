/*
 * Copyright (c) 2008-2012 Peter Lemenkov <lemenkov@gmail.com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * * Neither the name of the authors nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <erl_driver.h>

#include <fcntl.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <sys/ioctl.h>

typedef struct {
	ErlDrvPort port;
	ErlDrvTermData owner;
	uint8_t *buf;
	ssize_t size;
	int rtp_socket;
	int rtcp_socket;
	struct sockaddr_in peer;
	socklen_t peer_len;
	uint16_t rtp_port; // Network-order
	uint16_t rtcp_port; // Network-order
	bool mux;
} rtp_data;

ErlDrvTermData atom_rtp;
ErlDrvTermData atom_rtcp;
ErlDrvTermData atom_udp;

/* Private functions*/
int prepare_socket(uint32_t ip, uint16_t port, uint16_t size)
{
	int sock = 0;
	int flags;
	struct sockaddr_in si;
	int reuse = 1;
	int n = 0;

	sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if( setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(reuse)) < 0 ){
		close(sock);
		return 0;
	}

	bzero(&si, sizeof(si));
	si.sin_family = AF_INET;
	si.sin_port = port;
	si.sin_addr.s_addr = ip;
	if(bind(sock, (struct sockaddr *)&si, sizeof(si)) == -1) {
		close(sock);
		return 0;
	}

	n = size;
	if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &n, sizeof(n)) == -1) {
		// FIXME
	}
	n = 1;
	if (setsockopt(sock, SOL_SOCKET, SO_NO_CHECK, &n, sizeof(n)) == -1) {
		// FIXME
	}

	flags = fcntl(sock, F_GETFL);
	assert(flags >= 0);
	assert(!fcntl(sock, F_SETFL, flags | O_NONBLOCK));
	return sock;
}

// Returns in host-order
uint16_t get_port(int sock)
{
	struct sockaddr_in sin;
	socklen_t addrlen = sizeof(sin);
	int port = -1;

	if(getsockname(sock, (struct sockaddr *)&sin, &addrlen) == 0)
		port = ntohs(sin.sin_port);
	return port;
}

// Returns in host-order
uint32_t get_ip(int sock)
{
	struct sockaddr_in sin;
	socklen_t addrlen = sizeof(sin);
	int ip = -1;

	if(getsockname(sock, (struct sockaddr *)&sin, &addrlen) == 0)
		ip = ntohl(sin.sin_addr.s_addr);
	return ip;
}

uint16_t get_sibling_port(int sock)
{
	uint16_t port = get_port(sock);
	div_t n = div(port, 2);
	if (n.rem == 0)
		port++; // RTP, so the next port should be RTCP (RTP+1)
	else
		port--; // RTCP, so the next port should be RTP (RTCP-1)
	return port;
}

bool is_rtp(int sock)
{
	uint16_t port = get_port(sock);
	div_t n = div(port, 2);
	if (n.rem == 0)
		return true; // RTP
	else
		return false; // RTCP
}

ErlDrvTermData get_type(ssize_t size, char* buf)
{
	if( (size>12) && ((buf[0] & 128) == 128) && (((buf[1] & 127) <= 34)||((96 <= buf[1]) & 127)))
		return atom_rtp;
	else if( (size>8) && ((buf[0] & 128) == 128) && (((64 < buf[1]) & 127)||((buf[1] & 127) < 82)))
		return atom_rtcp;
	else
		return atom_udp;
}

/* Public functions*/

static int rtp_drv_init(void)
{
	atom_rtp = driver_mk_atom("rtp");
	atom_rtcp = driver_mk_atom("rtcp");
	atom_udp = driver_mk_atom("udp");
	return 0;
}

static ErlDrvData rtp_drv_start(ErlDrvPort port, char *buff)
{
	rtp_data* d = (rtp_data*)driver_alloc(sizeof(rtp_data));
	d->port = port;
	d->owner = driver_caller(port);
	// FIXME why 1024?
	d->size = 1024;
	d->buf = (uint8_t *)driver_alloc(d->size);
	memset(d->buf, 0, d->size);
	d->rtp_socket = -1;
	d->rtcp_socket = -1;
	d->rtp_port = 0;
	d->rtcp_port = 0;
	d->mux = false;
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData)d;
}

static void rtp_drv_stop(ErlDrvData handle)
{
	rtp_data *d = (rtp_data *) handle;
	if(d->rtp_socket != -1){
		driver_select(d->port, (ErlDrvEvent)d->rtp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 0);
		close(d->rtp_socket);
	}
	if(d->rtcp_socket != -1){
		driver_select(d->port, (ErlDrvEvent)d->rtcp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 0);
		close(d->rtcp_socket);
	}
	d->rtp_socket = -1;
	d->rtcp_socket = -1;
	driver_free(d->buf);
	driver_free((char*)handle);
}

static void rtp_drv_output(ErlDrvData handle, char *buf, int len)
{
	rtp_data *d = (rtp_data *) handle;

	if(d->rtp_socket == -1)
		return; // main socket isn't ready/closed
	if(d->rtp_port == 0)
		return; // outgoing address isn't set yet

	ErlDrvTermData type = get_type(len, buf);
	if ((type == atom_rtp) || (type == atom_udp))
		sendto(d->rtp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
	else{
		if(d->mux)
			sendto(d->rtp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
		else{
			if(d->rtcp_socket == -1)
				return; // RTCP socket isn't ready/closed
			if(d->rtcp_port == 0)
				return; // outgoing address isn't set yet

			d->peer.sin_port = d->rtcp_port;
			sendto(d->rtcp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
			d->peer.sin_port = d->rtp_port;
		}
	}
}

static void rtp_drv_ready_output(ErlDrvData handle, ErlDrvEvent event)
{
	rtp_data *d = (rtp_data *) handle;
}

static void rtp_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
	rtp_data *d = (rtp_data *) handle;

	if ((d->rtp_socket != (int)event) && (d->rtcp_socket != (int)event))
		return;

	struct sockaddr_in peer;
	socklen_t peer_len = sizeof(struct sockaddr_in);
	ErlDrvTermData type;

	ssize_t s = 0;
	ssize_t s2 = 0;

	// FIXME use it for bufer adjustment
	ioctl((int)event, FIONREAD, &s);
	s = 0;
	do{
		s2 += s;
		s = recvfrom((int)event, d->buf+s2, d->size-s2, 0, (struct sockaddr *)&peer, &peer_len);
	} while (s>0);

	if(s2>0){
		/* Check for type */
		type = get_type(s2, d->buf);
		if(d->rtp_port == 0){
			if((type == atom_rtp)||(type == atom_udp)){
				bzero(&(d->peer), sizeof(d->peer));
				d->peer_len = sizeof(struct sockaddr_in);
				memcpy(&(d->peer), &peer, d->peer_len);
				d->rtp_port = peer.sin_port;
			}
		}
		/* FIXME consider removing this - just use d->rtp_port+1 */
		if((d->rtcp_port == 0) && (type == atom_rtcp))
			d->rtcp_port = peer.sin_port;

		ErlDrvTermData reply[] = {
			ERL_DRV_ATOM, type,
			ERL_DRV_PORT, driver_mk_port(d->port),
			ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[0],
			ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[1],
			ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[2],
			ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[3],
			ERL_DRV_TUPLE, 4,
			ERL_DRV_UINT, ntohs(peer.sin_port),
			ERL_DRV_BUF2BINARY, (ErlDrvTermData)d->buf, (ErlDrvTermData)s2,
			ERL_DRV_TUPLE, 5
		};
		driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
	}
	else{
		ErlDrvTermData reply[] = {
			ERL_DRV_ATOM, driver_mk_atom("error"),
			ERL_DRV_PORT, driver_mk_port(d->port),
			ERL_DRV_INT, errno,
			ERL_DRV_TUPLE, 3
		};
		driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
	}
}

static int rtp_drv_control(
		ErlDrvData handle,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen)
{
	rtp_data* d = (rtp_data*)handle;

	int ret = 0;
	ErlDrvBinary *out;
	switch(command) {
		case 1:{
			int sock0 = 0;
			int sock1 = 0;
			uint16_t port = 0;
			uint32_t ip = 0;
			memcpy(&port, buf, 2);
			memcpy(&ip, buf+2, 4);
			sock0 = prepare_socket(ip, port, d->size);
			if(sock0 <= 0){
				driver_failure_posix(d->port, errno);
				return 0;
			}
			sock1 = prepare_socket(ip, htons(get_sibling_port(sock0)), d->size);
			if(sock1 <= 0){
				driver_failure_posix(d->port, errno);
				return 0;
			}
			if(is_rtp(sock0)){
				d->rtp_socket = sock0;
				d->rtcp_socket = sock1;
			}
			else{
				d->rtp_socket = sock1;
				d->rtcp_socket = sock0;
			}
			driver_select(d->port, (ErlDrvEvent)d->rtp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 1);
			driver_select(d->port, (ErlDrvEvent)d->rtcp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 1);
			memcpy(*rbuf, "ok", 2);
			ret = 2;
			}
			break;
		case 2:{
			uint32_t ip = htonl(get_ip(d->rtp_socket));
			uint16_t p0 = htons(get_port(d->rtp_socket));
			uint16_t p1 = htons(get_port(d->rtcp_socket));
			memcpy(*rbuf, &ip, 4);
			memcpy(*rbuf+4, &p0, 2);
			memcpy(*rbuf+6, &p1, 2);
			ret = 8;
			}
			break;
		default:
			break;
	}

	return ret;
}

ErlDrvEntry rtp_driver_entry = {
	rtp_drv_init,			/* F_PTR init, N/A */
	rtp_drv_start,	/* L_PTR start, called when port is opened */
	rtp_drv_stop,	/* F_PTR stop, called when port is closed */
	rtp_drv_output,			/* F_PTR output, called when erlang has sent */
	rtp_drv_input,			/* F_PTR ready_input, called when input descriptor ready */
	rtp_drv_ready_output,			/* F_PTR ready_output, called when output descriptor ready */
	"rtp_drv",	/* char *driver_name, the argument to open_port */
	NULL,			/* F_PTR finish, called when unloaded */
	NULL,			/* handle */
	rtp_drv_control,	/* F_PTR control, port_command callback */
	NULL,			/* F_PTR timeout, reserved */
	NULL,			/* F_PTR outputv, reserved */
	NULL,
	NULL,
	NULL,
	NULL,
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	ERL_DRV_FLAG_USE_PORT_LOCKING,
	NULL,
	NULL,
	NULL
};

DRIVER_INIT(rtp_drv) /* must match name in driver_entry */
{
	return &rtp_driver_entry;
}
