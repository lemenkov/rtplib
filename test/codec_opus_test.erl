-module(codec_opus_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_opus_test_() ->
	[
		{"Test decoding from OPUS to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/opus/testvector01.bit",
							"../test/samples/opus/testvector01.dec",
							160,
							"OPUS",
							{'OPUS',8000,1}
						)
					) end
		}
	].
