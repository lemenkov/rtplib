-module(codec_gsm_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_gsm_test_() ->
	% Original GSM stream
	{ok, GsmIn} = file:read_file("../test/sample-gsm-16-mono-8khz.raw"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/sample-pcm-16-mono-8khz.raw"),

	% Source PCM for encoding
	{ok, PcmIn} = file:read_file("../test/sample-pcm-16-mono-8khz.raw"),
	% The resulting GSM bitstream
	{ok, GsmOut} = file:read_file("../test/sample-gsm-16-mono-8khz.from_pcm"),

	{ok, Codec} = codec:start_link({'GSM',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from GSM to PCM",
				fun() -> ?assertEqual(true, test_utils:decode("GSM", Codec, GsmIn, PcmOut, 33, 320)) end
			},
			{"Test encoding from PCM to GSM",
				fun() -> ?assertEqual(true, test_utils:encode("GSM", Codec, PcmIn, GsmOut, 320, 33)) end
			}
		]
	}.
