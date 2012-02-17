-module(codec_speex_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_speex_test_() ->
	[
		{"Test decoding from SPEEX to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/speex/sample-speex-16-mono-8khz.raw",
							"../test/samples/speex/sample-pcm-16-mono-8khz.from_spx",
							38,
							"SPEEX",
							{'SPEEX',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to SPEEX",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/speex/sample-pcm-16-mono-8khz.raw",
							"../test/samples/speex/sample-speex-16-mono-8khz.from_pcm",
							320,
							"SPEEX",
							{'SPEEX',8000,1}
						)
					) end
		}
	].
