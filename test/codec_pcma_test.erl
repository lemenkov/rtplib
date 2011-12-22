-module(codec_pcma_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_pcma_test_() ->
	% Original G.711a bitstream
	{ok, PCMAIn} = file:read_file("../test/raw-alaw.raw"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/raw-pcm16.from_alaw"),

	% Source PCM for encoding
	{ok, PcmIn} = file:read_file("../test/raw-pcm16.raw"),
	% The resulting G.711a bitstream
	{ok, PCMAOut} = file:read_file("../test/raw-alaw.from_pcm"),

	{ok, Codec} = codec:start_link({'PCMA',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from G.711a to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("G.711a / PCMA", Codec, PCMAIn, PcmOut, 160, 320)) end
			},
			{"Test encoding from PCM to G.711a",
				fun() -> ?assertEqual(true, test_utils:encode("G.711a / PCMA", Codec, PcmIn, PCMAOut, 320, 160)) end
			}
		]
	}.
