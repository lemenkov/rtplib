-module(codec_g722_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_g722_test_() ->
	% Original G.722 bitstream
	{ok, G722In} = file:read_file("../test/conf-adminmenu-162.g722"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/conf-adminmenu-162.raw"),

	% Source PCM for encoding
	{ok, PcmIn} = file:read_file("../test/sample-pcm-16-mono-8khz.raw"),
	% The resulting G.722 bitstream
	{ok, G722Out} = file:read_file("../test/sample-g722-16-mono-8khz.raw"),

	{ok, Codec} = codec:start_link({'G722',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from G.722 to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("G.722", Codec, G722In, PcmOut, 160, 320)) end
			},
			{"Test encoding from PCM to G.722",
				fun() -> ?assertEqual(true, test_utils:encode("G.722", Codec, PcmIn, G722Out, 320, 160)) end
			}
		]
	}.
