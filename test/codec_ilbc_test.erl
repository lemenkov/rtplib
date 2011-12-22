-module(codec_ilbc_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_ilbc_test_() ->
	% Original PCM
	{ok, PcmIn} = file:read_file("../test/samples/ilbc/F00.INP"),

	% Encoded iLBC stream (with 20 msec)
	{ok, IlbcOut20} = file:read_file("../test/samples/ilbc/F00.BIT20"),
	% Encoded iLBC stream (with 30 msec)
	{ok, IlbcOut30} = file:read_file("../test/samples/ilbc/F00.BIT30"),

	% PCM, decoded from iLBC stream (with 20 msec)
	{ok, PcmOut20} = file:read_file("../test/samples/ilbc/F00.OUT20"),
	% PCM, decoded from iLBC stream (with 30 msec)
	{ok, PcmOut30} = file:read_file("../test/samples/ilbc/F00.OUT30"),

	{ok, Codec} = codec:start_link({'ILBC',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test encoding from PCM to iLBC(20)",
				fun() -> ?assertEqual(true, test_utils:encode_f("iLBC", Codec, PcmIn, IlbcOut20, 320, 38)) end
			},
			{"Test encoding from PCM to iLBC(30)",
				fun() -> ?assertEqual(true, test_utils:encode("iLBC", Codec, PcmIn, IlbcOut30, 480, 50)) end
			},
			{"Test decoding from iLBC(20) to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("iLBC", Codec, IlbcOut20, PcmOut20, 38, 320)) end
			},
			{"Test decoding from iLBC(30) to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("iLBC", Codec, IlbcOut30, PcmOut30, 50, 480)) end
			}
		]
	}.
