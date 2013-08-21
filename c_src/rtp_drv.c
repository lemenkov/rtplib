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
#include <time.h>

typedef union {
	struct sockaddr_in si;
	struct sockaddr_in6 si6;
} sock_peer;

typedef struct {
	ErlDrvPort port;
	ErlDrvTermData owner;
	ErlDrvTermData dport;
	uint8_t *buf;
	ssize_t size;
	sock_peer peer;
	socklen_t peer_len;
	int rtp_socket;
	int rtcp_socket;
	int other_rtp_socket;
	sock_peer other_peer;
	socklen_t other_peer_len;
	uint16_t rtp_port; // Network-order
	uint16_t rtcp_port; // Network-order
	bool mux;
	unsigned long tval;
	time_t lastseen;
	ssize_t rxbytes;
	ssize_t rxpackets;
	ssize_t txbytes;
	ssize_t txpackets;
	// This is a counter for a fast-sent packets
	ssize_t txbytes2;
	ssize_t txpackets2;
	unsigned long ssrc;
	uint8_t dtmf_id;
	int8_t type;
	void (*raise_data)(ErlDrvPort *, ErlDrvTermData *, ErlDrvTermData *, sock_peer *, uint8_t *, ssize_t);
} rtp_data;

ErlDrvTermData atom_rtp;
ErlDrvTermData atom_rtcp;
ErlDrvTermData atom_udp;
ErlDrvTermData atom_peer;

ErlDrvTermData atom_timeout;

enum payloadType {
	payloadRtp,
	payloadRtpDtmf, // A special type of RTP payload we'd like to handle n the higher level
	payloadRtcp,
	payloadUdp
};

/* Private functions*/
int prepare_socket(uint8_t sockfamily, uint32_t ip, uint16_t* ip6, uint16_t port)
{
	int sock = 0;
	int flags;
	sock_peer sa;

	int n = 0;

	if(sockfamily == 4)
		sock = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	else
		sock = socket(AF_INET6, SOCK_DGRAM, IPPROTO_UDP);

	n = 1;
	if( setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &n, sizeof(n)) < 0 ){
		close(sock);
		return 0;
	}

	bzero(&sa, sizeof(sa));
	if(sockfamily == 4){
		sa.si.sin_family = AF_INET;
		sa.si.sin_addr.s_addr = ip;
		sa.si.sin_port = port;
	}
	else{
		sa.si6.sin6_family = AF_INET6;
		memcpy(&(sa.si6.sin6_addr), ip6, sizeof(uint16_t) * 8);
		sa.si6.sin6_port = port;
	}

	if(bind(sock, (struct sockaddr *)&sa, sizeof(sa)) == -1) {
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
	assert(!fcntl(sock, F_SETFL, flags | O_NONBLOCK | O_NDELAY));
	return sock;
}

// Returns in host-order
uint16_t get_port(int sock)
{
	sock_peer sa;
	socklen_t addrlen = sizeof(sa);

	if(getsockname(sock, (struct sockaddr *)&sa, &addrlen) == 0){
		if (sa.si.sin_family == AF_INET)
			return ntohs(sa.si.sin_port);
		else
			return ntohs(sa.si6.sin6_port);
	}
	return -1;
}

uint16_t get_sibling_port(int sock)
{
	uint16_t port = get_port(sock);
	div_t n = div(port, 2);
	if (n.rem == 0)
		return port++; // RTP, so the next port should be RTCP (RTP+1)

	return port--; // RTCP, so the next port should be RTP (RTCP-1)
}

bool is_rtp(int sock)
{
	uint16_t port = get_port(sock);
	div_t n = div(port, 2);
	if (n.rem == 0)
		return true; // RTP

	return false; // RTCP
}

inline enum payloadType get_type(ssize_t size, char* buf, uint8_t dtmf_id)
{
	uint8_t payload_id = 0;
	if((buf[0] & 128) == 128){ // seems to be RTP or RTCP
		if(size>8){ // FIXME RTP header can be 12 bytes long
			payload_id = buf[1] & 127;
			if( (payload_id <= 34) || (96 <= payload_id))
				return dtmf_id == payload_id ? payloadRtpDtmf : payloadRtp;
			else
				return payloadRtcp;
		}
	}
	return payloadUdp;
}

void raise_data_4(ErlDrvPort *port, ErlDrvTermData *type, ErlDrvTermData *dport, sock_peer *peer, uint8_t *buf, ssize_t s)
{
	ErlDrvTermData reply[] = {
		ERL_DRV_ATOM, *type,
		ERL_DRV_PORT, *dport,
		ERL_DRV_UINT, ((unsigned char*)&(peer->si.sin_addr.s_addr))[0],
		ERL_DRV_UINT, ((unsigned char*)&(peer->si.sin_addr.s_addr))[1],
		ERL_DRV_UINT, ((unsigned char*)&(peer->si.sin_addr.s_addr))[2],
		ERL_DRV_UINT, ((unsigned char*)&(peer->si.sin_addr.s_addr))[3],
		ERL_DRV_TUPLE, 4,
		ERL_DRV_UINT, ntohs(peer->si.sin_port),
		ERL_DRV_BUF2BINARY, (ErlDrvTermData)buf, (ErlDrvTermData)s,
		ERL_DRV_TUPLE, 5
	};
	driver_output_term(*port, reply, sizeof(reply) / sizeof(reply[0]));
}

void raise_data_6(ErlDrvPort *port, ErlDrvTermData *type, ErlDrvTermData *dport, sock_peer *peer, uint8_t *buf, ssize_t s)
{
	uint16_t* tmp = &(peer->si6.sin6_addr);
	ErlDrvTermData reply[] = {
		ERL_DRV_ATOM, *type,
		ERL_DRV_PORT, *dport,
		ERL_DRV_UINT, ntohs(tmp[0]),
		ERL_DRV_UINT, ntohs(tmp[1]),
		ERL_DRV_UINT, ntohs(tmp[2]),
		ERL_DRV_UINT, ntohs(tmp[3]),
		ERL_DRV_UINT, ntohs(tmp[4]),
		ERL_DRV_UINT, ntohs(tmp[5]),
		ERL_DRV_UINT, ntohs(tmp[6]),
		ERL_DRV_UINT, ntohs(tmp[7]),
		ERL_DRV_TUPLE, 8,
		ERL_DRV_UINT, ntohs(peer->si6.sin6_port),
		ERL_DRV_BUF2BINARY, (ErlDrvTermData)buf, (ErlDrvTermData)s,
		ERL_DRV_TUPLE, 5
	};
	driver_output_term(*port, reply, sizeof(reply) / sizeof(reply[0]));
}

/* Public functions*/

static int rtp_drv_init(void)
{
	atom_rtp = driver_mk_atom("rtp");
	atom_rtcp = driver_mk_atom("rtcp");
	atom_udp = driver_mk_atom("udp");
	atom_peer = driver_mk_atom("peer");

	atom_timeout = driver_mk_atom("timeout");
	return 0;
}

static ErlDrvData rtp_drv_start(ErlDrvPort port, char *buff)
{
	rtp_data* d = (rtp_data*)driver_alloc(sizeof(rtp_data));
	d->port = port;
	d->dport = driver_mk_port(port);
	d->owner = driver_caller(port);
	d->size = 0;
	d->buf = NULL;
	d->rtp_socket = -1;
	d->rtcp_socket = -1;
	d->other_rtp_socket = 0;
	d->rtp_port = 0;
	d->rtcp_port = 0;
	d->mux = false;
	d->tval = 0;
	d->lastseen = NULL;
	d->rxbytes = 0;
	d->rxpackets = 0;
	d->txbytes = 0;
	d->txpackets = 0;
	d->txbytes2 = 0;
	d->txpackets2 = 0;
	d->ssrc = 0;
	d->dtmf_id = 255; // Actual DTMF payload id will be less than 128
	d->type = -1;
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

	switch(get_type(len, buf, d->dtmf_id))
	{
		case payloadRtp:
			d->txpackets++;
			d->txbytes += len - 12;
		case payloadRtpDtmf:
		case payloadUdp:
			sendto(d->rtp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
			break;
		case payloadRtcp:
			if(d->mux)
				sendto(d->rtp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
			else{
				if(d->rtcp_socket == -1)
					return; // RTCP socket isn't ready/closed
				if(d->rtcp_port == 0)
					return; // outgoing address isn't set yet

				if (d->peer.si.sin_family == AF_INET){
					d->peer.si.sin_port = d->rtcp_port;
					sendto(d->rtcp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
					d->peer.si.sin_port = d->rtp_port;
				}
				else{
					d->peer.si6.sin6_port = d->rtcp_port;
					sendto(d->rtcp_socket, buf, len, 0, (struct sockaddr *)&(d->peer), d->peer_len);
					d->peer.si6.sin6_port = d->rtp_port;
				}
			}
			break;
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

	if(difftime(time(NULL), d->lastseen)*1000 >= d->tval){
		ErlDrvTermData reply[] = {
			ERL_DRV_ATOM, atom_timeout,
			ERL_DRV_PORT, d->dport,
			ERL_DRV_TUPLE, 2
		};
		driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
	}
	// FIXME more precise timeouts
	driver_set_timer(d->port, d->tval);
}

static void rtp_drv_input(ErlDrvData handle, ErlDrvEvent event)
{
	rtp_data *d = (rtp_data *) handle;

	sock_peer peer;
	socklen_t peer_len = sizeof(peer);
	ErlDrvTermData* type = &atom_rtp; // by default
	enum payloadType ptype;

	ssize_t s = 0;

	ioctl((int)event, FIONREAD, &s);
	if(s > d->size){
		driver_free(d->buf);
		d->size = s;
		d->buf = (uint8_t *)driver_alloc(d->size);
	}
	s = recvfrom((int)event, d->buf, d->size, 0, (struct sockaddr *)&peer, &peer_len);

	/* reset timer */
	d->lastseen = time(NULL);

	/* Check for type */
	ptype = get_type(s, d->buf, d->dtmf_id);
	if(ptype == payloadRtp){
		d->type = d->buf[1] & 127;
		d->ssrc = ((uint32_t*)d->buf)[2]; // store it in network-order
		d->rxpackets++;
		d->rxbytes += s - 12;

		if(d->rtp_port == 0){
			bzero(&(d->peer), sizeof(d->peer));
			d->peer_len = sizeof(d->peer);
			memcpy(&(d->peer), &peer, d->peer_len);

			if (peer.si.sin_family == AF_INET){
				d->rtp_port = peer.si.sin_port;
				ErlDrvTermData reply[] = {
					ERL_DRV_ATOM, atom_peer,
					ERL_DRV_UINT, (int)event,
					ERL_DRV_UINT, ((unsigned char*)&(peer.si.sin_addr.s_addr))[0],
					ERL_DRV_UINT, ((unsigned char*)&(peer.si.sin_addr.s_addr))[1],
					ERL_DRV_UINT, ((unsigned char*)&(peer.si.sin_addr.s_addr))[2],
					ERL_DRV_UINT, ((unsigned char*)&(peer.si.sin_addr.s_addr))[3],
					ERL_DRV_TUPLE, 4,
					ERL_DRV_UINT, ntohs(peer.si.sin_port),
					ERL_DRV_TUPLE, 4
				};
				driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
			}
			else{
				d->rtp_port = peer.si6.sin6_port;
				uint16_t* tmp = &(peer.si6.sin6_addr);
				ErlDrvTermData reply[] = {
					ERL_DRV_ATOM, atom_peer,
					ERL_DRV_UINT, (int)event,
					ERL_DRV_UINT, ntohs(tmp[0]),
					ERL_DRV_UINT, ntohs(tmp[1]),
					ERL_DRV_UINT, ntohs(tmp[2]),
					ERL_DRV_UINT, ntohs(tmp[3]),
					ERL_DRV_UINT, ntohs(tmp[4]),
					ERL_DRV_UINT, ntohs(tmp[5]),
					ERL_DRV_UINT, ntohs(tmp[6]),
					ERL_DRV_UINT, ntohs(tmp[7]),
					ERL_DRV_TUPLE, 8,
					ERL_DRV_UINT, ntohs(peer.si6.sin6_port),
					ERL_DRV_TUPLE, 4
				};
				driver_output_term(d->port, reply, sizeof(reply) / sizeof(reply[0]));
			}
		}

		if(d->other_rtp_socket){
			sendto(d->other_rtp_socket, d->buf, s, 0, (struct sockaddr *)&(d->other_peer), d->other_peer_len);
			d->txpackets2++;
			d->txbytes2 += s - 12;
		}
		else
			d->raise_data(&(d->port), type, &(d->dport), &(d->peer), d->buf, s);
	}
	else{
		/* FIXME consider removing this - just use d->rtp_port+1 */
		if((d->rtcp_port == 0) && (ptype == payloadRtcp)){
			if (peer.si.sin_family == AF_INET)
				d->rtcp_port = peer.si.sin_port;
			else
				d->rtcp_port = peer.si6.sin6_port;
		}

		if(ptype == payloadRtcp)
			type = &atom_rtcp;
		else if(ptype == payloadRtpDtmf)
			type = &atom_rtp;
		else
			type = &atom_udp;

		d->raise_data(&(d->port), type, &(d->dport), &(d->peer), d->buf, s);
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
		case 1:	{
			int sock0 = 0;
			int sock1 = 0;
			uint16_t port = 0;
			uint8_t sockfamily = 4;
			uint32_t ip = 0;
			uint16_t ip6[8] = {0, 0, 0, 0, 0, 0, 0, 0};
			uint32_t tval_early = 0;
			uint32_t tval = 0;
			memcpy(&port, buf, 2);
			memcpy(&sockfamily, buf+2, 1);
			if(sockfamily == 4){
				memcpy(&ip, buf+3, 4);
				memcpy(&tval_early, buf+7, 4);
				memcpy(&tval, buf+11, 4);
				d->raise_data = &raise_data_4;
			}
			else{
				memcpy(&ip6, buf+3, 16);
				memcpy(&tval_early, buf+19, 4);
				memcpy(&tval, buf+23, 4);
				d->raise_data = &raise_data_6;
			}
			d->tval = ntohl(tval);
			sock0 = prepare_socket(sockfamily, ip, ip6, port);
			if(sock0 <= 0){
				driver_failure_posix(d->port, errno);
				return 0;
			}
			sock1 = prepare_socket(sockfamily, ip, ip6, htons(get_sibling_port(sock0)));
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
			driver_set_timer(d->port, ntohl(tval_early));
			driver_select(d->port, (ErlDrvEvent)d->rtp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 1);
			driver_select(d->port, (ErlDrvEvent)d->rtcp_socket, ERL_DRV_READ | ERL_DRV_WRITE, 1);
			memcpy(*rbuf, "ok", 2);
			ret = 2;
			}
			break;
		case 2: {
			uint32_t ip = 0;
			uint16_t ip6[8] = {0, 0, 0, 0, 0, 0, 0, 0};
			union {
				struct sockaddr_in si;
				struct sockaddr_in6 si6;
			} sa;
			socklen_t addrlen = sizeof(sa);

			uint16_t p0 = htons(get_port(d->rtp_socket));
			uint16_t p1 = htons(get_port(d->rtcp_socket));
			ssize_t ip_offset = 4;

			if(getsockname(d->rtp_socket, (struct sockaddr *)&sa, &addrlen) == 0){
				if (sa.si.sin_family == AF_INET)
					memcpy(*rbuf, &(sa.si.sin_addr.s_addr), 4);
				else{
					memcpy(*rbuf, &(sa.si6.sin6_addr), sizeof(uint16_t) * 8);
					ip_offset = 16;
				}
			}

			memcpy(*rbuf+ip_offset, &p0, 2);
			memcpy(*rbuf+ip_offset+2, &p1, 2);
			ret = ip_offset+4;
			}
			break;
		case 4: {
			uint8_t sockfamily = 4;
			d->other_rtp_socket = ntohl(*(int*)buf); // Network-order to host-order
			memcpy(&sockfamily, buf+6, 1);
			bzero(&(d->other_peer), sizeof(d->other_peer));
			if(sockfamily == 4){
				d->other_peer.si.sin_family = AF_INET;
				d->other_peer.si.sin_port =  *(uint16_t*)(buf+4); // Network-order
				memcpy(&(d->other_peer.si.sin_addr.s_addr), buf+7, 4); // Network-order
			}
			else{
				d->other_peer.si6.sin6_family = AF_INET6;
				d->other_peer.si6.sin6_port =  *(uint16_t*)(buf+4); // Network-order
				memcpy(&(d->other_peer.si6.sin6_addr), buf+7, sizeof(uint16_t) * 8); // Network-order
			}
			d->other_peer_len = sizeof(d->other_peer);
			}
			break;
		case 5: {
			uint32_t rxbytes = htonl(d->rxbytes);
			uint32_t rxpackets = htonl(d->rxpackets);
			uint32_t txbytes = htonl(d->txbytes);
			uint32_t txpackets = htonl(d->txpackets);
			uint32_t txbytes2 = htonl(d->txbytes2);
			uint32_t txpackets2 = htonl(d->txpackets2);
			memcpy(*rbuf, &(d->ssrc), 4);
			memcpy(*rbuf+4, &(d->type), 1);
			memcpy(*rbuf+5, &rxbytes, 4);
			memcpy(*rbuf+9, &rxpackets, 4);
			memcpy(*rbuf+13, &txbytes, 4);
			memcpy(*rbuf+17, &txpackets, 4);
			memcpy(*rbuf+21, &txbytes2, 4);
			memcpy(*rbuf+25, &txpackets2, 4);
			ret = 29;
			}
			break;
		case 6:
			memcpy(&(d->dtmf_id), buf, 1);
			memcpy(*rbuf, "ok", 2);
			ret = 2;
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
