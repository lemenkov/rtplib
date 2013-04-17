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
	struct sockaddr_in peer;
	socklen_t peer_len;
	int rtp_socket;
	int rtcp_socket;
	int other_rtp_socket;
	struct sockaddr_in other_peer;
	socklen_t other_peer_len;
	uint16_t rtp_port; // Network-order
	uint16_t rtcp_port; // Network-order
	bool mux;
	unsigned long tval;
} rtp_data;

ErlDrvTermData atom_rtp;
ErlDrvTermData atom_rtcp;
ErlDrvTermData atom_udp;
ErlDrvTermData atom_peer;
ErlDrvTermData atom_interim_update;

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

	n = 16384;
	if (setsockopt(sock, SOL_SOCKET, SO_RCVBUF, &n, sizeof(n)) == -1) {
		// FIXME
	}
	n = 229376;
	if (setsockopt(sock, SOL_SOCKET, SO_SNDBUF, &n, sizeof(n)) == -1) {
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
	atom_peer = driver_mk_atom("peer");
	atom_interim_update = driver_mk_atom("interim_update");
	return 0;
}

static ErlDrvData rtp_drv_start(ErlDrvPort port, char *buff)
{
	rtp_data* d = (rtp_data*)driver_alloc(sizeof(rtp_data));
	d->port = port;
	d->owner = driver_caller(port);
	d->size = 256;
	d->buf = (uint8_t *)driver_alloc(d->size);
	memset(d->buf, 0, d->size);
	d->rtp_socket = -1;
	d->rtcp_socket = -1;
	d->other_rtp_socket = 0;
	d->rtp_port = 0;
	d->rtcp_port = 0;
	d->mux = false;
	d->tval = 30000; // default value it 30 seconds
	driver_set_timer(d->port, d->tval);
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
	driver_cancel_timer(d->port);
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
	driver_select(d->port, event, ERL_DRV_WRITE, 0);
}

static void rtp_drv_timeout(ErlDrvData handle)
{
	rtp_data *d = (rtp_data *) handle;
	ErlDrvTermData reply[] = {
		ERL_DRV_ATOM, atom_interim_update,
		ERL_DRV_PORT, driver_mk_port(d->port),
		ERL_DRV_TUPLE, 2
	};
	driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
	driver_cancel_timer(d->port); // FIXME is it really needed
	driver_set_timer(d->port, d->tval);
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

	// FIXME use it for bufer adjustment
	ioctl((int)event, FIONREAD, &s);
	if(s > d->size){
		driver_free(d->buf);
		d->size = s;
		d->buf = (uint8_t *)driver_alloc(d->size);
	}
	s = recvfrom((int)event, d->buf, d->size, 0, (struct sockaddr *)&peer, &peer_len);

	if(s>0){
		/* Check for type */
		type = get_type(s, d->buf);
		if(d->rtp_port == 0){
			if((type == atom_rtp)||(type == atom_udp)){
				bzero(&(d->peer), sizeof(d->peer));
				d->peer_len = sizeof(struct sockaddr_in);
				memcpy(&(d->peer), &peer, d->peer_len);
				d->rtp_port = peer.sin_port;

				ErlDrvTermData reply[] = {
					ERL_DRV_ATOM, atom_peer,
					ERL_DRV_UINT, (int)event,
					ERL_DRV_UINT, ((unsigned char*)&(d->peer.sin_addr.s_addr))[0],
					ERL_DRV_UINT, ((unsigned char*)&(d->peer.sin_addr.s_addr))[1],
					ERL_DRV_UINT, ((unsigned char*)&(d->peer.sin_addr.s_addr))[2],
					ERL_DRV_UINT, ((unsigned char*)&(d->peer.sin_addr.s_addr))[3],
					ERL_DRV_TUPLE, 4,
					ERL_DRV_UINT, ntohs(d->peer.sin_port),
					ERL_DRV_TUPLE, 4
				};
				driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
			}
		}
		/* FIXME consider removing this - just use d->rtp_port+1 */
		if((d->rtcp_port == 0) && (type == atom_rtcp))
			d->rtcp_port = peer.sin_port;

		if((d->other_rtp_socket)&&((type == atom_rtp)||(type == atom_udp))){
			sendto(d->other_rtp_socket, d->buf, s, 0, (struct sockaddr *)&(d->other_peer), d->other_peer_len);
		}
		else{
			ErlDrvTermData reply[] = {
				ERL_DRV_ATOM, type,
				ERL_DRV_PORT, driver_mk_port(d->port),
				ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[0],
				ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[1],
				ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[2],
				ERL_DRV_UINT, ((unsigned char*)&(peer.sin_addr.s_addr))[3],
				ERL_DRV_TUPLE, 4,
				ERL_DRV_UINT, ntohs(peer.sin_port),
				ERL_DRV_BUF2BINARY, (ErlDrvTermData)d->buf, (ErlDrvTermData)s,
				ERL_DRV_TUPLE, 5
			};
			driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
		}
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
		case 3:
			{
			int id = htonl(d->rtp_socket); // Network-order
			uint32_t ip = d->peer.sin_addr.s_addr; // Network-order
			memcpy(*rbuf, &id, 4);
			memcpy(*rbuf+4, &(d->rtp_port), 2); // Network-order
			memcpy(*rbuf+6, &ip, 4);
			ret = 10;
			}
			break;
		case 4:
			{
			d->other_rtp_socket = ntohl(*(int*)buf); // Network-order to host-order
			uint16_t port =  ntohs(*(uint16_t*)(buf+4)); // Network-order
			uint32_t ip = *(uint32_t*)(buf+6); // Network-order
			bzero(&(d->other_peer), sizeof(d->other_peer));
			d->other_peer.sin_family = AF_INET;
			d->other_peer.sin_port = port;
			d->other_peer.sin_addr.s_addr = ip;
			d->other_peer_len = sizeof(d->other_peer);
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
	rtp_drv_timeout,			/* F_PTR timeout, reserved */
	NULL,			/* F_PTR outputv, reserved */
	NULL,
	NULL,
	NULL,
	NULL,
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	ERL_DRV_FLAG_USE_PORT_LOCKING | ERL_DRV_FLAG_SOFT_BUSY,
	NULL,
	NULL,
	NULL
};

DRIVER_INIT(rtp_drv) /* must match name in driver_entry */
{
	return &rtp_driver_entry;
}
