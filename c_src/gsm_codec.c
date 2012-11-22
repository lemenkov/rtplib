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

/* Loosely based on Evgeniy Khramtsov's original approach - erlrtp */

#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include "erl_driver.h"
#include <spandsp/telephony.h>
#include <spandsp/bit_operations.h>
#include <spandsp/gsm0610.h>

typedef struct {
	ErlDrvPort port;
	gsm0610_state_t* dstate;
	gsm0610_state_t* estate;
} codec_data;

enum {
	CMD_ENCODE = 1,
	CMD_DECODE = 2
};

#define FRAME_SIZE 160
#define GSM_SIZE 33

static ErlDrvData codec_drv_start(ErlDrvPort port, char *buff)
{
	codec_data* d = (codec_data*)driver_alloc(sizeof(codec_data));
	d->port = port;
	d->dstate = gsm0610_init(NULL, GSM0610_PACKING_VOIP);
	d->estate = gsm0610_init(NULL, GSM0610_PACKING_VOIP);
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData)d;
}

static void codec_drv_stop(ErlDrvData handle)
{
	codec_data *d = (codec_data *) handle;
	gsm0610_free(d->dstate);
	gsm0610_free(d->estate);
	driver_free((char*)handle);
}

static int codec_drv_control(
		ErlDrvData handle,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen)
{
	codec_data* d = (codec_data*)handle;

	int ret = 0;
	ErlDrvBinary *out;
	*rbuf = NULL;

	switch(command) {
		case CMD_ENCODE:
			if (len != FRAME_SIZE * 2)
				break;
			out = driver_alloc_binary(GSM_SIZE);

			ret = gsm0610_encode(d->estate, (uint8_t*)out->orig_bytes, (const int16_t*)buf, len >> 1);
			*rbuf = (char *)out;
			break;
		 case CMD_DECODE:
			if (len != GSM_SIZE)
				break;
			out = driver_alloc_binary(FRAME_SIZE * 2);
			ret = gsm0610_decode(d->dstate, (int16_t*)out->orig_bytes, (const uint8_t*)buf, len) >> 1;

			*rbuf = (char *)out;
			break;
		 default:
			break;
	}
	return ret;
}

ErlDrvEntry codec_driver_entry = {
	NULL,			/* F_PTR init, N/A */
	codec_drv_start,	/* L_PTR start, called when port is opened */
	codec_drv_stop,		/* F_PTR stop, called when port is closed */
	NULL,			/* F_PTR output, called when erlang has sent */
	NULL,			/* F_PTR ready_input, called when input descriptor ready */
	NULL,			/* F_PTR ready_output, called when output descriptor ready */
	"gsm_codec_drv",	/* char *driver_name, the argument to open_port */
	NULL,			/* F_PTR finish, called when unloaded */
	NULL,			/* handle */
	codec_drv_control,	/* F_PTR control, port_command callback */
	NULL,			/* F_PTR timeout, reserved */
	NULL,			/* F_PTR outputv, reserved */
	NULL,
	NULL,
	NULL,
	NULL,
	ERL_DRV_EXTENDED_MARKER,
	ERL_DRV_EXTENDED_MAJOR_VERSION,
	ERL_DRV_EXTENDED_MINOR_VERSION,
	0,
	NULL,
	NULL,
	NULL
};

DRIVER_INIT(codec_drv) /* must match name in driver_entry */
{
	return &codec_driver_entry;
}
