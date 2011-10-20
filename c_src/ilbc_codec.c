#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include "erl_driver.h"
#include <ilbc/iLBC_define.h>
#include <ilbc/iLBC_decode.h>
#include <ilbc/iLBC_encode.h>
#include <syslog.h>

typedef struct {
	ErlDrvPort port;
	// 20 msec codec
	iLBC_Enc_Inst_t estate20;
	iLBC_Dec_Inst_t dstate20;
	// 30 msec codec
	iLBC_Enc_Inst_t estate30;
	iLBC_Dec_Inst_t dstate30;
} codec_data;

enum {
	CMD_ENCODE = 1,
	CMD_DECODE = 2
};

static ErlDrvData codec_drv_start(ErlDrvPort port, char *buff)
{
	codec_data* d = (codec_data*)driver_alloc(sizeof(codec_data));
	d->port = port;
	initEncode(&d->estate20, 20);
	initDecode(&d->dstate20, 20, 0 /* 1=use_enhancer */);
	initEncode(&d->estate30, 30);
	initDecode(&d->dstate30, 30, 0 /* 1=use_enhancer */);
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	openlog("ilbc_codec_drv", LOG_NDELAY, LOG_USER);
	return (ErlDrvData)d;
}

static void codec_drv_stop(ErlDrvData handle)
{
	driver_free((char*)handle);
	closelog();
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

	float block[BLOCKL_MAX];
	float sample;
	int i = 0;

//	syslog(LOG_USER | LOG_WARNING, "INIT: %d %d\n", command, len);

	switch(command) {
		case CMD_ENCODE:
			switch(len){
				case 320: // 20 msec
					out = driver_alloc_binary(38);
					for (i = 0; i < 160; i++)
						block[i] = (float) ((int16_t*)buf)[i];
					iLBC_encode(out->orig_bytes, block, &d->estate20);
					*rbuf = (char *)out;
					ret = 38;
					break;
				case 480: // 30 msec
					out = driver_alloc_binary(50);
					for (i = 0; i < 240; i++)
						block[i] = (float) ((int16_t*)buf)[i];
					iLBC_encode(out->orig_bytes, block, &d->estate30);
					*rbuf = (char *)out;
					ret = 50;
					break;
				default:
					break;
			}
			break;
		 case CMD_DECODE:
			switch(len){
				case 38: // 20 msec
					out = driver_alloc_binary(320);
					iLBC_decode(block, (unsigned char*)buf, &d->dstate20, 1); /* 1 == normal */
					for (i = 0; i < 160; i++){
						sample = block[i];
						if (sample < MIN_SAMPLE)
							sample = MIN_SAMPLE;
						else
							if (sample > MAX_SAMPLE)
								sample = MAX_SAMPLE;
						((int16_t *)out->orig_bytes)[i] = (short) sample;
					}
					*rbuf = (char *)out;
					ret = 320;
					break;
				case 50: // 30 msec
					out = driver_alloc_binary(480);
					iLBC_decode(block, (unsigned char*)buf, &d->dstate30, 1); /* 1 == normal */
					for (i = 0; i < 240; i++){
						sample = block[i];
						if (sample < MIN_SAMPLE)
							sample = MIN_SAMPLE;
						else
							if (sample > MAX_SAMPLE)
								sample = MAX_SAMPLE;
						((int16_t *)out->orig_bytes)[i] = (short) sample;
					}
					*rbuf = (char *)out;
					ret = 480;
					break;
				default:
					break;
			}
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
	"ilbc_codec_drv",		/* char *driver_name, the argument to open_port */
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
