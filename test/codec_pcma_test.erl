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
	{ok, PCMAOut} = file:read_file("../test/raw-alaw.raw"),

	{ok, Codec} = codec:start_link({'PCMA',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from G.711a to PCM",
				fun() -> ?assertEqual(true, decode(Codec, PCMAIn, PcmOut)) end
			},
			{"Test encoding from PCM to G.711a",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, PCMAOut)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 160; size(B) < 320 ->
	true;
decode(Codec, <<PCMAFrame:160/binary, PCMARaw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	{ok, {PcmFrame, 8000, 1, 16}} = codec:decode(Codec, PCMAFrame),
	decode(Codec, PCMARaw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 160 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<PCMAFrame:160/binary, PCMARaw/binary>>) ->
	% FIXME add reference bitstream
	{ok, PCMAFrame1} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
%	error_logger:info_msg("~p~n~p~n~n", [PCMAFrame, PCMAFrame1]),
%	error_logger:info_msg("RET: ~p~n~n", [PCMAFrame == PCMAFrame1]),
	encode(Codec, PcmRaw, PCMARaw).
