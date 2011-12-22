-module(codec_ilbc_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_ilbc_test_() ->
	% Original iLBC stream
	{ok, IlbcIn} = file:read_file("../test/samples/ilbc/F00.BIT20"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/samples/ilbc/F00.OUT20"),

	% Original PCM
	{ok, PcmIn} = file:read_file("../test/samples/ilbc/F00.INP"),
	% Original SPEEX stream
	{ok, IlbcOut} = file:read_file("../test/samples/ilbc/F00.BIT20"),

	{ok, Codec} = codec:start_link({'ILBC',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from iLBC to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("iLBC", Codec, IlbcIn, PcmOut, 38, 320)) end
			},
			{"Test encoding from PCM to iLBC",
				fun() -> ?assertEqual(true, test_utils:encode_f("iLBC", Codec, PcmIn, IlbcOut, 320, 38)) end
			}
		]
	}.
