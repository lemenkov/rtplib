-module(codec_g722_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_g722_test_() ->
	[
		{"Test decoding from G.722 to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/conf-adminmenu-162.g722",
							"../test/conf-adminmenu-162.raw",
							160,
							320,
							"G.722",
							{'G722',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to G.722",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/sample-pcm-16-mono-8khz.raw",
							"../test/sample-g722-16-mono-8khz.raw",
							320,
							160,
							"G.722",
							{'G722',8000,1}
						)
					) end
		}
	].
