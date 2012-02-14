#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include "erl_driver.h"
#include <math.h>
#include <samplerate.h>

typedef struct {
	ErlDrvPort port;
	void* state;
} resampler_data;


int get_samplerate(int number){
	switch (number)
	{
		case 8:
			return 8000;
		case 11:
			return 11025;
		case 16:
			return 16000;
		case 22:
			return 22050;
		case 24:
			return 24000;
		case 32:
			return 32000;
		case 44:
			return 44100;
		case 48:
			return 48000;
		case 96:
			return 96000;
		default:
			return 0;
	}
}

static ErlDrvData resampler_drv_start(ErlDrvPort port, char *buff)
{
	resampler_data* d = (resampler_data*)driver_alloc(sizeof(resampler_data));
	d->port = port;
	set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
	return (ErlDrvData)d;
}

static void resampler_drv_stop(ErlDrvData handle)
{
	resampler_data *d = (resampler_data *) handle;
	driver_free((char*)handle);
}

static int resampler_drv_control(
		ErlDrvData handle,
		unsigned int command,
		char *buf, int len,
		char **rbuf, int rlen)
{
	resampler_data* d = (resampler_data*)handle;

	int ret = 0;
	ErlDrvBinary *out;
	*rbuf = NULL;

	int from_samplerate = get_samplerate((command >> 24));
	int from_channels = (command >> 16) & 0xFF;
	int to_samplerate = get_samplerate((command >> 8) & 0xFF);
	int to_channels = command & 0xFF;

	int i = 0;
	SRC_DATA data;
	data.data_in  = (float*)calloc(len / 2, sizeof(float));
	data.data_out = (float*)calloc(len * 8, sizeof(float));

	for(i = 0; i < len / 2; i++)
		data.data_in[i] = (float)((short*)buf)[i];

	data.input_frames = len / 2;
	data.output_frames = len * 8;

	data.src_ratio = (double)to_samplerate / (double)from_samplerate;

	ret = src_simple (&data, SRC_SINC_FASTEST, from_channels);

	out = driver_alloc_binary(data.output_frames_gen * 2);
	for(i = 0; i < data.output_frames_gen; i++)
		*((short*)(out->orig_bytes) + i) = (short)floor(data.data_out[i] + 0.5);

	free(data.data_in);
	free(data.data_out);

	*rbuf = (char *)out;
	ret = data.output_frames_gen * 2;

	return ret;
}

ErlDrvEntry resampler_driver_entry = {
	NULL,			/* F_PTR init, N/A */
	resampler_drv_start,	/* L_PTR start, called when port is opened */
	resampler_drv_stop,	/* F_PTR stop, called when port is closed */
	NULL,			/* F_PTR output, called when erlang has sent */
	NULL,			/* F_PTR ready_input, called when input descriptor ready */
	NULL,			/* F_PTR ready_output, called when output descriptor ready */
	"resampler_drv",	/* char *driver_name, the argument to open_port */
	NULL,			/* F_PTR finish, called when unloaded */
	NULL,			/* handle */
	resampler_drv_control,	/* F_PTR control, port_command callback */
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

DRIVER_INIT(resampler_drv) /* must match name in driver_entry */
{
	return &resampler_driver_entry;
}
