#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include "erl_driver.h"
#include <spandsp/telephony.h>
#include <spandsp/ima_adpcm.h>

typedef struct {
	ErlDrvPort port;
	ima_adpcm_state_t* state;
} codec_data;

enum {
	CMD_ENCODE = 1,
	CMD_DECODE = 2
};

static ErlDrvData codec_drv_start(ErlDrvPort port, char *buff)
{
	codec_data* d = (codec_data*)driver_alloc(sizeof(codec_data));
	d->port = port;
	d->state = ima_adpcm_init(NULL, IMA_ADPCM_DVI4, 0);
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData)d;
}

static void codec_drv_stop(ErlDrvData handle)
{
	codec_data *d = (codec_data *) handle;
	ima_adpcm_free(d->state);
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
			out = driver_alloc_binary(len >> 1);
			ret = ima_adpcm_encode(d->estate, (uint8_t *)out->orig_bytes, (const int16_t *)buf, len >> 1);
			*rbuf = (char *) out;
			break;
		 case CMD_DECODE:
			out = driver_alloc_binary(len / 2);
			ret = ima_adpcm_decode(d->dstate, (int16_t *)out->orig_bytes, (const uint8_t *)buf, len);
			*rbuf = (char *) out;
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
	"dvi4_codec_drv",		/* char *driver_name, the argument to open_port */
	NULL,			/* F_PTR finish, called when unloaded */
	NULL,			/* handle */
	codec_drv_control,	/* F_PTR control, port_command callback */
	NULL,			/* F_PTR timeout, reserved */
	NULL			/* F_PTR outputv, reserved */
};

DRIVER_INIT(codec_drv) /* must match name in driver_entry */
{
	return &codec_driver_entry;
}
