-module(codec_pcmu_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_pcmu_test_() ->
	% Original G.711u bitstream
	{ok, PCMUIn} = file:read_file("../test/raw-ulaw.raw"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/raw-pcm16.from_ulaw"),

	% Source PCM for encoding
	{ok, PcmIn} = file:read_file("../test/raw-pcm16.raw"),
	% The resulting G.711u bitstream
	{ok, PCMUOut} = file:read_file("../test/raw-ulaw.raw"),

	{ok, Codec} = codec:start_link({'PCMU',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			% FIXME segfaults here
			{"Test decoding from G.711u to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("G.711u / PCMU", Codec, PCMUIn, PcmOut, 160, 320)) end
			},
			{"Test encoding from PCM to G.711u",
				fun() -> ?assertEqual(true, test_utils:encode("G.711a / PCMA", Codec, PcmIn, PCMUOut, 320, 160)) end
			}
		]
	}.
