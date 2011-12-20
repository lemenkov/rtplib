-module(codec_ilbc_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_ilbc_test_() ->
	% Original iLBC stream
	{ok, IlbcIn} = file:read_file("../test/F00.BIT20"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/F00.OUT20"),

	% Original PCM
	{ok, PcmIn} = file:read_file("../test/F00.INP"),
	% Original SPEEX stream
	{ok, IlbcOut} = file:read_file("../test/F00.BIT20"),

	{ok, Codec} = codec:start_link({'ILBC',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from iLBC to PCM",
				fun() -> ?assertEqual(true, decode(Codec, IlbcIn, PcmOut)) end
			},
			{"Test encoding from PCM to iLBC",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, IlbcOut)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 38; size(B) < 320 ->
	true;
decode(Codec, <<IlbcFrame:38/binary, IlbcRaw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	% FIXME add reference bitstream
	{ok, {PcmFrame1, 8000, 1, 16}} = codec:decode(Codec, IlbcFrame),
%	error_logger:info_msg("~p~n~p~n~p ~p~n~n", [PcmFrame, PcmFrame1, size(PcmFrame1), diff(PcmFrame, PcmFrame1)]),
	decode(Codec, IlbcRaw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 38 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<IlbcFrame:38/binary, IlbcRaw/binary>>) ->
	% FIXME add reference bitstream
	{ok, IlbcFrame1} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
%	error_logger:info_msg("~p~n~p~n~p ~p~n~n", [IlbcFrame, IlbcFrame1, size(IlbcFrame1), diff(IlbcFrame, IlbcFrame1)]),
	encode(Codec, PcmRaw, IlbcRaw).

%diff(A, B) ->
%	diff(<<>>, A, B).
%
%diff(Ret, <<>>, _) ->
%	Ret;
%diff(Ret, _, <<>>) ->
%	Ret;
%
%diff(Ret, <<ByteA:8, RestA/binary>>, <<ByteB:8, RestB/binary>>) ->
%	Diff = ByteA - ByteB,
%	diff(<<Ret/binary, Diff:8>>, RestA, RestB).
