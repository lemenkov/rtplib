#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include "erl_driver.h"
#include <opus.h>

typedef struct {
	OpusEncoder *encoder;
	OpusDecoder *decoder;
	ErlDrvPort port;
} codec_data;

enum {
	CMD_ENCODE = 1,
	CMD_DECODE = 2
};

static ErlDrvData codec_drv_start(ErlDrvPort port, char *buff)
{
	int err = 0;

	/* come up with a way to specify these */
//	int bitrate_bps = bits_per_second;
//	int mode = MODE_HYBRID;
//	int use_vbr = 1;
//	int complexity = 10;
//	int use_inbandfec = 1;
//	int use_dtx = 1;
//	int bandwidth = BANDWIDTH_FULLBAND;
	int sampling_rate = 8000;
	int number_of_channels = 1;

	codec_data* d = (codec_data*)driver_alloc(sizeof(codec_data));
	d->encoder = opus_encoder_create(sampling_rate, number_of_channels, OPUS_APPLICATION_VOIP, &err);
	d->decoder = opus_decoder_create(sampling_rate, number_of_channels, &err);

//	opus_encoder_ctl(d->encoder, OPUS_SET_MODE(mode));
//	opus_encoder_ctl(d->encoder, OPUS_SET_BITRATE(bitrate_bps));
//	opus_encoder_ctl(d->encoder, OPUS_SET_BANDWIDTH(bandwidth));
//	opus_encoder_ctl(d->encoder, OPUS_SET_VBR_FLAG(use_vbr));
//	opus_encoder_ctl(d->encoder, OPUS_SET_COMPLEXITY(complexity));
//	opus_encoder_ctl(d->encoder, OPUS_SET_INBAND_FEC_FLAG(use_inbandfec));
//	opus_encoder_ctl(d->encoder, OPUS_SET_DTX_FLAG(use_dtx));

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
#define MAX_PACKET 1500
#define STEREO 2

	codec_data* d = (codec_data*)handle;

	int ret = 0;
	ErlDrvBinary *out;
	*rbuf = NULL;
	opus_int16 pcm[960*6*STEREO];
	unsigned char opus[MAX_PACKET];

	switch(command) {
		case CMD_ENCODE:
			ret = opus_encode(d->encoder, (const opus_int16 *)buf, len >> 1, opus, MAX_PACKET);
			out = driver_alloc_binary(ret);
			memcpy(out->orig_bytes, opus, ret);
			*rbuf = (char *) out;
			break;
		 case CMD_DECODE:
			ret = 2 * opus_decode(d->decoder, (const unsigned char *)buf, len, pcm, 960*6*STEREO, 0);
			out = driver_alloc_binary(ret);
			memcpy(out->orig_bytes, pcm, ret);
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
	"opus_codec_drv",	/* char *driver_name, the argument to open_port */
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
