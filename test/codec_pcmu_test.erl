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
				fun() -> ?assertEqual(true, decode(Codec, PCMUIn, PcmOut)) end
			},
			{"Test encoding from PCM to G.711u",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, PCMUOut)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 160; size(B) < 320 ->
	true;
decode(Codec, <<PCMUFrame:160/binary, PCMURaw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	{ok, {PcmFrame, 8000, 1, 16}} = codec:decode(Codec, PCMUFrame),
	decode(Codec, PCMURaw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 160 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<PCMUFrame:160/binary, PCMURaw/binary>>) ->
	{ok, PCMUFrame} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
	encode(Codec, PcmRaw, PCMURaw).
