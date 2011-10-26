-module(codec_g722_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_g722_test_() ->
	% Original GSM stream
	{ok, G722Raw} = file:read_file("../test/conf-adminmenu-162.g722"),
	% Decoded PCM
	{ok, PcmRaw} = file:read_file("../test/conf-adminmenu-162.pcm"),
	{ok, Codec} = codec:start_link({'G722',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			% FIXME segfaults here
			{"Test decoding from G.722 to PCM",
				fun() -> ?assertEqual(true, decode(Codec, G722Raw, PcmRaw)) end
			},
			{"Test encoding from PCM to G.722",
				fun() -> ?assertEqual(true, encode(Codec, PcmRaw, G722Raw)) end
			}
		]
	}.

decode(Codec, <<>>, <<>>) ->
	true;
decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 160; size(B) < 320 ->
	true;
decode(Codec, <<G722Frame:160/binary, G722Raw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	{ok, {PcmFrame1, 8000, 1, 16}} = codec:decode(Codec, G722Frame),
	error_logger:info_msg("~p~n~p~n~n", [PcmFrame, PcmFrame1]),
	decode(Codec, G722Raw, PcmRaw).

encode(Codec, <<>>, <<>>) ->
	true;
encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 160 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<G722Frame:160/binary, G722Raw/binary>>) ->
	% FIXME add reference bitstream
	{ok, G722Frame1} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
%	error_logger:info_msg("~p~n~p~n~n", [G722Frame, G722Frame1]),
	encode(Codec, PcmRaw, G722Raw).
