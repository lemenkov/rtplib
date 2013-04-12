if WITH_OPUS
OPUS_CODEC_DRV=opus_codec_drv.la
endif

if WITH_SPEEX
SPEEX_CODEC_DRV=speex_codec_drv.la
endif

if WITH_ILBC
ILBC_CODEC_DRV=ilbc_codec_drv.la
endif

if WITH_G729
G729_CODEC_DRV=g729_codec_drv.la
endif

lib_LTLIBRARIES = \
		  dvi4_codec_drv.la \
		  g722_codec_drv.la \
		  g726_codec_drv.la \
		  lpc_codec_drv.la \
		  gsm_codec_drv.la \
		  pcmu_codec_drv.la \
		  pcma_codec_drv.la \
		  $(OPUS_CODEC_DRV) \
		  $(SPEEX_CODEC_DRV) \
		  $(ILBC_CODEC_DRV) \
		  $(G729_CODEC_DRV) \
		  crc32c_nif.la \
		  sas_nif.la \
		  resampler_drv.la \
		  rtp_drv.la

dvi4_codec_drv_la_SOURCES = dvi4_codec.c
dvi4_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
dvi4_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
dvi4_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

g722_codec_drv_la_SOURCES = g722_codec.c
g722_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
g722_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
g722_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

g726_codec_drv_la_SOURCES = g726_codec.c
g726_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
g726_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
g726_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

lpc_codec_drv_la_SOURCES = lpc_codec.c
lpc_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
lpc_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
lpc_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

gsm_codec_drv_la_SOURCES = gsm_codec.c
gsm_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
gsm_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
gsm_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

pcmu_codec_drv_la_SOURCES = pcmu_codec.c
pcmu_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
pcmu_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
pcmu_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

pcma_codec_drv_la_SOURCES = pcma_codec.c
pcma_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPANDSP_CFLAGS@
pcma_codec_drv_la_LIBADD = @SPANDSP_LIBS@ @ERLANG_LIBS@
pcma_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

if WITH_OPUS
opus_codec_drv_la_SOURCES = opus_codec.c
opus_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @OPUS_CFLAGS@
opus_codec_drv_la_LIBADD = @OPUS_LIBS@ @ERLANG_LIBS@
opus_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version
endif

if WITH_SPEEX
speex_codec_drv_la_SOURCES = speex_codec.c
speex_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SPEEX_CFLAGS@
speex_codec_drv_la_LIBADD = @SPEEX_LIBS@ @ERLANG_LIBS@
speex_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version
endif

if WITH_ILBC
ilbc_codec_drv_la_SOURCES = ilbc_codec.c
ilbc_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @ILBC_CFLAGS@
ilbc_codec_drv_la_LIBADD = @ILBC_LIBS@ @ERLANG_LIBS@
ilbc_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version
endif

if WITH_G729
g729_codec_drv_la_SOURCES = g729_codec.c
g729_codec_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @G729_CFLAGS@
g729_codec_drv_la_LIBADD = @G729_LIBS@ @ERLANG_LIBS@
g729_codec_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version
endif

crc32c_nif_la_SOURCES = crc32c_nif.c
crc32c_nif_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@
crc32c_nif_la_LIBADD = @ERLANG_LIBS@
crc32c_nif_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

sas_nif_la_SOURCES = sas_nif.c
sas_nif_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@
sas_nif_la_LIBADD = @ERLANG_LIBS@
sas_nif_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

resampler_drv_la_SOURCES = resampler.c
resampler_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@ @SAMPLERATE_CFLAGS@
resampler_drv_la_LIBADD = @SAMPLERATE_LIBS@ @ERLANG_LIBS@
resampler_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

rtp_drv_la_SOURCES = rtp_drv.c
rtp_drv_la_CFLAGS = $(AM_CFLAGS) @ERLANG_CFLAGS@
rtp_drv_la_LIBADD = @ERLANG_LIBS@
rtp_drv_la_LDFLAGS = $(AM_LDFLAGS) -module -avoid-version

install:
	$(mkdir_p) $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/priv/
	$(install_sh_PROGRAM) .libs/*.so $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/priv/

uninstall-local:
	@rm $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/priv/*.so
	@rm -r $(DESTDIR)@ERLANG_DIR@/lib/rtplib-@PACKAGE_VERSION@/priv/
