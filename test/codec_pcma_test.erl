-module(codec_pcma_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_pcma_test_() ->
	[
		{"Test decoding from G.711a to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/pcma/raw-alaw.raw",
							"../test/samples/pcma/raw-pcm16.from_alaw",
							160,
							"G.711a / PCMA",
							{'PCMA',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to G.711a",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/pcma/raw-pcm16.raw",
							"../test/samples/pcma/raw-alaw.from_pcm",
							320,
							"G.711a / PCMA",
							{'PCMA',8000,1}
						)
					) end
		}
	].
