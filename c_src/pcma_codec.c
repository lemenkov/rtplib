/* Loosely based on Evgeniy Khramtsov's original approach - erlrtp */

#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <erl_driver.h>
#include "g711.h"

typedef struct {
	ErlDrvPort port;
} codec_data;

enum {
	CMD_ENCODE = 1,
	CMD_DECODE = 2
};

static ErlDrvData codec_drv_start(ErlDrvPort port, char *buff)
{
	codec_data* d = (codec_data*)driver_alloc(sizeof(codec_data));
	d->port = port;
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData)d;
}

static void codec_drv_stop(ErlDrvData handle)
{
	driver_free((char*)handle);
}

static int codec_drv_control(
		ErlDrvData handle,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen)
{
	int i;
	int sample;
	int ret = 0;
	ErlDrvBinary *out;
	*rbuf = NULL;

	switch(command) {
		case CMD_ENCODE:
			if (len % 2 != 0)
				break;
			out = driver_alloc_binary(len / 2);
			for (i = 0; i < (len / 2); i++) {
				sample = (buf[i * 2 + 1] << 8) | (buf[i * 2] & 0xff);
				out->orig_bytes[i] = Snack_Lin2Alaw(sample);
			}
			*rbuf = (char *) out;
			ret = (len / 2);
			break;
		 case CMD_DECODE:
			out = driver_alloc_binary(len * 2);
			for (i = 0; i < len; i++) {
				sample = Snack_Alaw2Lin((unsigned char) buf[i]);
				out->orig_bytes[i * 2] = (char) (sample & 0xff);
				out->orig_bytes[i * 2 + 1] = (char) (sample >> 8);
			}
			*rbuf = (char *) out;
			ret = (len * 2);
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
	"pcma_codec_drv",		/* char *driver_name, the argument to open_port */
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
