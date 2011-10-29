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
				fun() -> ?assertEqual(true, decode(Codec, GsmIn, PcmOut)) end
			},
			{"Test encoding from PCM to GSM",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, GsmOut)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 33; size(B) < 320 ->
	true;
decode(Codec, <<GsmFrame:33/binary, GsmRaw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	{ok, {PcmFrame, 8000, 1, 16}} = codec:decode(Codec, GsmFrame),
	decode(Codec, GsmRaw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 33 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<GsmFrame:33/binary, GsmRaw/binary>>) ->
	{ok, GsmFrame} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
	encode(Codec, PcmRaw, GsmRaw).
