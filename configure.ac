AC_INIT([erlang-rtplib], [0.6.2], [lemenkov@gmail.com])

#AC_PREREQ([2.68])

AC_CANONICAL_TARGET

AC_CONFIG_SRCDIR([c_src/resampler.c])
AM_INIT_AUTOMAKE([dist-bzip2 no-dist-gzip tar-ustar foreign])

# Checks for programs.
AC_PROG_CC
AC_PROG_LIBTOOL

AM_WITH_ERLANG

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h stdint.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_TYPE_INT16_T
AC_TYPE_SIZE_T
#AC_TYPE_UINT32_T
#AC_TYPE_UINT8_T

# Checks for library functions.
AC_CHECK_FUNCS([floor])

# Audio codecs:
audiocodec_aac=no
audiocodec_amr=no
audiocodec_amrwb=no
audiocodec_celt=no
audiocodec_dvi=no
audiocodec_pcma=no
audiocodec_pcmu=no
audiocodec_g726=no
audiocodec_g722=no
audiocodec_g7221=no
audiocodec_g7221c=no
audiocodec_g726=no
audiocodec_g7231=no
audiocodec_g726=no
audiocodec_g729=no
audiocodec_g7291=no
audiocodec_gsmfr=no
audiocodec_ilbc=no
audiocodec_isac=no
audiocodec_l16=no
audiocodec_lpc=no
audiocodec_opus=no
audiocodec_silk=no
audiocodec_speex=no
# Video codecs:
videocodec_h263=no
videocodec_h264=no
videocodec_i420=no
videocodec_vp8=no

AC_ARG_WITH([audiocodec-opus],
	    AC_HELP_STRING([--with-audiocodec-opus],[enable OPUS audiocodec [default=yes].]),
	    [audiocodec_opus="$withval"],[audiocodec_opus="yes"])

AC_ARG_WITH([audiocodec-speex],
	    AC_HELP_STRING([--with-audiocodec-speex],[enable SPEEX audiocodec [default=yes].]),
	    [audiocodec_speex="$withval"],[audiocodec_speex="yes"])

AC_ARG_WITH([audiocodec-ilbc],
	    AC_HELP_STRING([--with-audiocodec-ilbc],[enable iLBC audiocodec [default=yes].]),
	    [audiocodec_ilbc="$withval"],[audiocodec_ilbc="yes"])

AC_ARG_WITH([audiocodec-g729],
	    AC_HELP_STRING([--with-audiocodec-g729],[enable G.729 audiocodec [default=yes].]),
	    [audiocodec_g729="$withval"],[audiocodec_g729="yes"])

AC_CHECK_HEADER([spandsp/telephony.h],[audiocodec_dvi="yes"],[AC_MSG_ERROR([<spandsp/telephony.h> header not found])])
AC_CHECK_HEADER([spandsp/ima_adpcm.h],,[AC_MSG_ERROR([<spandsp/ima_adpcm.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])

AC_CHECK_HEADER([spandsp/g722.h],[audiocodec_g722="yes"],[AC_MSG_ERROR([<spandsp/g722.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/g726.h],[audiocodec_g726="yes"],[AC_MSG_ERROR([<spandsp/g726.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/bit_operations.h],,[AC_MSG_ERROR([<spandsp/bit_operations.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/gsm0610.h],[audiocodec_gsmfr="yes"],[AC_MSG_ERROR([<spandsp/gsm0610.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/lpc10.h],[audiocodec_lpc="yes"],[AC_MSG_ERROR([<spandsp/lpc10.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/g711.h],[
		 audiocodec_pcma="yes"
		 audiocodec_pcmu="yes"
		 ],[AC_MSG_ERROR([<spandsp/g711.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_LIB([spandsp],
	     [ima_adpcm_init],
	     [
	      SPANDSP_CFLAGS=`pkg-config --cflags spandsp`
	      SPANDSP_LIBS=`pkg-config --libs spandsp`
	      AC_SUBST([SPANDSP_CFLAGS])
	      AC_SUBST([SPANDSP_LIBS])
	     ],
	     [AC_MSG_ERROR([spandsp library missing])]
)

if test "$audiocodec_opus" = "yes"
then
#AC_CHECK_HEADER([opus.h],,[AC_MSG_ERROR([<opus.h> header not found])])
AC_CHECK_LIB([opus],
	     [opus_encoder_create],
	     [
	      OPUS_CFLAGS=`pkg-config --cflags opus`
	      OPUS_LIBS=`pkg-config --libs opus`
	      AC_SUBST([OPUS_CFLAGS])
	      AC_SUBST([OPUS_LIBS])
	     ],
	     [AC_MSG_ERROR([opus library missing])]
)
fi

if test "$audiocodec_speex" = "yes"
then
AC_CHECK_HEADER([speex/speex.h],,[AC_MSG_ERROR([<speex/speex.h> header not found])])
AC_CHECK_LIB([speex],
	     [speex_encoder_init],
	     [
	      SPEEX_CFLAGS=`pkg-config --cflags speex`
	      SPEEX_LIBS=`pkg-config --libs speex`
	      AC_SUBST([SPEEX_CFLAGS])
	      AC_SUBST([SPEEX_LIBS])
	     ],
	     [AC_MSG_ERROR([speex library missing])]
)
fi

if test "$audiocodec_ilbc" = "yes"
then
AC_CHECK_HEADER([ilbc.h],,[AC_MSG_ERROR([<ilbc.h> header not found])])
#AC_CHECK_LIB([webrtc_codecs_ilbc]
AC_CHECK_LIB([ilbc],
	     [WebRtcIlbcfix_EncoderCreate],
	     [
#	      ILBC_CFLAGS=`pkg-config --cflags webrtc`
#	      ILBC_LIBS="-lwebrtc_codecs_ilbc"
	      ILBC_CFLAGS=`pkg-config --cflags libilbc`
	      ILBC_LIBS=`pkg-config --libs libilbc`
	      AC_SUBST([ILBC_CFLAGS])
	      AC_SUBST([ILBC_LIBS])
	     ],
	     [AC_MSG_ERROR([ilbc library missing])]
)
fi

if test "$audiocodec_g729" = "yes"
then
AC_CHECK_HEADER([bcg729/decoder.h],,[AC_MSG_ERROR([<bcg729/decoder.h> header not found])])
AC_CHECK_HEADER([bcg729/encoder.h],,[AC_MSG_ERROR([<bcg729/encoder.h> header not found])])
AC_CHECK_LIB([bcg729],
	     [initBcg729DecoderChannel],
	     [
	      G729_CFLAGS=`pkg-config --cflags libbcg729`
	      G729_LIBS=`pkg-config --libs libbcg729`
	      AC_SUBST([G729_CFLAGS])
	      AC_SUBST([G729_LIBS])
	     ],
	     [AC_MSG_ERROR([bcg729 library missing])]
)
fi

AC_CHECK_HEADER([samplerate.h],,[AC_MSG_ERROR([<samplerate.h> header not found])])
AC_CHECK_LIB([samplerate],
	     [src_simple],
	     [
	      SAMPLERATE_CFLAGS=`pkg-config --cflags samplerate`
	      SAMPLERATE_LIBS=`pkg-config --libs samplerate`
	      AC_SUBST([SAMPLERATE_CFLAGS])
	      AC_SUBST([SAMPLERATE_LIBS])
	     ],
	     [AC_MSG_ERROR([samplerate library missing])]
)

BUILD_DATE=`LANG=C date +"%a %b %d %Y"`
AC_SUBST([BUILD_DATE])

AC_PATH_PROG(REBAR, [rebar], [./rebar])

AM_CONDITIONAL(WITH_ILBC, test "$audiocodec_ilbc" = "yes")
AM_CONDITIONAL(WITH_G729, test "$audiocodec_g729" = "yes")
AM_CONDITIONAL(WITH_OPUS, test "$audiocodec_opus" = "yes")
AM_CONDITIONAL(WITH_SPEEX, test "$audiocodec_speex" = "yes")

AC_CONFIG_FILES([
		 c_src/Makefile
		 priv/erlang-rtplib.spec
		 Makefile
])

AC_OUTPUT

AC_MSG_RESULT([
------------------------------------------------------------------------
  $PACKAGE $VERSION configuration.

    Compiler support:

      Target CPU: ................... ${target_cpu}
      Target OS: .................... ${target_os}

    Audio configuration:

      AAC codec: ..................... ${audiocodec_aac}
      AMR codec: ..................... ${audiocodec_amr}
      AMR-WB codec: .................. ${audiocodec_amrwb}
      CELT codec: .................... ${audiocodec_celt}
      DVI codec: ..................... ${audiocodec_dvi}
      G.711 (a-law) codec: ........... ${audiocodec_pcma}
      G.711 (μ-law) codec: ........... ${audiocodec_pcmu}
      G.721 codec: ................... ${audiocodec_g726}
      G.722 codec: ................... ${audiocodec_g722}
      G,722.1 codec: ................. ${audiocodec_g7221}
      G.722.1C codec: ................ ${audiocodec_g7221c}
      G.723 codec: ................... ${audiocodec_g726}
      G.723.1 codec: ................. ${audiocodec_g7231}
      G.726 codec: ................... ${audiocodec_g726}
      G.729 codec: ................... ${audiocodec_g729}
      G.729.1 codec: ................. ${audiocodec_g7291}
      GSM-FR codec: .................. ${audiocodec_gsmfr}
      iLBC codec: .................... ${audiocodec_ilbc}
      ISAC codec: .................... ${audiocodec_isac}
      L16 codec: ..................... ${audiocodec_l16}
      LPC codec: ..................... ${audiocodec_lpc}
      OPUS codec: .................... ${audiocodec_opus}
      SILK codec: .................... ${audiocodec_silk}
      SPEEX codec: ................... ${audiocodec_speex}

    Video configuration:

      H.263 codec: ................... ${videocodec_h263}
      H.264 codec: ................... ${videocodec_h264}
      I420 codec: .................... ${videocodec_i420}
      VP8 codec: ..................... ${videocodec_vp8}

    General configuration:

      SRTP support: .................. untested
      ZRTP support: .................. untested

------------------------------------------------------------------------
])
