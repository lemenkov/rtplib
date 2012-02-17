-module(codec_pcmu_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_pcmu_test_() ->
	[
		{"Test decoding from G.711u to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/pcmu/raw-ulaw.raw",
							"../test/samples/pcmu/raw-pcm16.from_ulaw",
							160,
							"G.711u / PCMU",
							{'PCMU',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to G.711u",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/pcmu/raw-pcm16.raw",
							"../test/samples/pcmu/raw-ulaw.raw",
							320,
							"G.711u / PCMU",
							{'PCMU',8000,1}
						)
					) end
		}
	].
