AC_INIT([erlang-rtplib], [0.5.17], [lemenkov@gmail.com])

#AC_PREREQ([2.68])

AC_CANONICAL_TARGET

AC_CONFIG_SRCDIR([c_src/endianness.h])
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

AC_ARG_WITH([audiocodec-opus],
	    AC_HELP_STRING([--with-audiocodec-opus],[enable OPUS audiocodec.]),
	    [audiocodec_opus="$withval"],[audiocodec_opus="yes"])

AC_ARG_WITH([audiocodec-speex],
	    AC_HELP_STRING([--with-audiocodec-speex],[enable SPEEX audiocodec.]),
	    [audiocodec_speex="$withval"],[audiocodec_speex="yes"])

AC_ARG_WITH([audiocodec-ilbc],
	    AC_HELP_STRING([--with-audiocodec-ilbc],[enable iLBC audiocodec.]),
	    [audiocodec_ilbc="$withval"],[audiocodec_ilbc="yes"])

AC_CHECK_HEADER([spandsp/telephony.h],,[AC_MSG_ERROR([<spandsp/telephony.h> header not found])])
AC_CHECK_HEADER([spandsp/ima_adpcm.h],,[AC_MSG_ERROR([<spandsp/ima_adpcm.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/g722.h],,[AC_MSG_ERROR([<spandsp/g722.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/g726.h],,[AC_MSG_ERROR([<spandsp/g726.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/bit_operations.h],,[AC_MSG_ERROR([<spandsp/bit_operations.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/gsm0610.h],,[AC_MSG_ERROR([<spandsp/gsm0610.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/lpc10.h],,[AC_MSG_ERROR([<spandsp/lpc10.h> header not found])],[
		 AC_INCLUDES_DEFAULT
		 #include <spandsp/telephony.h>
		 ])
AC_CHECK_HEADER([spandsp/g711.h],,[AC_MSG_ERROR([<spandsp/g711.h> header not found])],[
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
#AC_CHECK_HEADER([ilbc.h],,[AC_MSG_ERROR([<ilbc.h> header not found])])
AC_CHECK_LIB([webrtc_codecs_ilbc],
	     [WebRtcIlbcfix_EncoderCreate],
	     [
	      ILBC_CFLAGS=`pkg-config --cflags webrtc`
	      ILBC_LIBS="-lwebrtc_codecs_ilbc"
	      AC_SUBST([ILBC_CFLAGS])
	      AC_SUBST([ILBC_LIBS])
	     ],
	     [AC_MSG_ERROR([ilbc library missing])]
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
AM_CONDITIONAL(WITH_OPUS, test "$audiocodec_opus" = "yes")
AM_CONDITIONAL(WITH_SPEEX, test "$audiocodec_speex" = "yes")

AC_CONFIG_FILES([
		 c_src/Makefile
		 priv/erlang-rtplib.spec
		 Makefile
])

AC_OUTPUT
