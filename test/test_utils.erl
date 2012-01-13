-module(test_utils).

-compile(export_all).

codec_encode(FileIn, FileOut, FrameSizeIn, FrameSizeOut, CodecName, CodecType) ->
	{ok, PcmIn}  = file:read_file(FileIn),
	{ok, BinOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:encode(CodecName, Codec, PcmIn, BinOut, FrameSizeIn, FrameSizeOut),

	codec:close(Codec),

	Ret.

codec_decode(FileIn, FileOut, FrameSizeIn, FrameSizeOut, CodecName, CodecType) ->
	{ok, BinIn}  = file:read_file(FileIn),
	{ok, PcmOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:decode(CodecName, Codec, BinIn, PcmOut, FrameSizeIn, FrameSizeOut),

	codec:close(Codec),

	Ret.

decode(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA, FrameSizeB) when size(A) < FrameSizeA; size(B) < FrameSizeB ->
	true;
decode(Name, Codec, A, B, FrameSizeA, FrameSizeB) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	{ok, {FrameB, 8000, 1, 16}} = codec:decode(Codec, FrameA),
	decode(Name, Codec, RestA, RestB, FrameSizeA, FrameSizeB).

encode(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA, FrameSizeB) when size(A) < FrameSizeA; size(B) < FrameSizeB ->
	true;
encode(Name, Codec, A, B, FrameSizeA, FrameSizeB) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	{ok, FrameB} = codec:encode(Codec, {FrameA, 8000, 1, 16}),
	encode(Name, Codec, RestA, RestB, FrameSizeA, FrameSizeB).

%%
%% These functions are not intended for the end user
%%

decode_f(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA, FrameSizeB) when size(A) < FrameSizeA; size(B) < FrameSizeB ->
	true;
decode_f(Name, Codec, A, B, FrameSizeA, FrameSizeB) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	case codec:decode(Codec, FrameA) of
		{ok, {FrameB, 8000, 1, 16}} -> ok;
		{ok, {FrameB1, 8000, 1, 16}} ->
			error_logger:info_msg(
				"Bitstream mismatch while decoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, FrameB, FrameB1, size(FrameB1), diff(FrameB, FrameB1)]
			)
	end,
	decode_f(Name, Codec, RestA, RestB, FrameSizeA, FrameSizeB).

encode_f(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA, FrameSizeB) when size(A) < FrameSizeA; size(B) < FrameSizeB ->
	true;
encode_f(Name, Codec, A, B, FrameSizeA, FrameSizeB) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	case codec:encode(Codec, {FrameA, 8000, 1, 16}) of
		{ok, FrameB} -> ok;
		{ok, FrameB1} ->
			error_logger:info_msg(
				"Bitstream mismatch while encoding to ~s frame.~nExpected:~n~p~nGot:~n~p~nEncoded size: ~p.~nEncoded diff: ~p~n",
				[Name, FrameB, FrameB1, size(FrameB1), diff(FrameB, FrameB1)]
			)
	end,
	encode_f(Name, Codec, RestA, RestB, FrameSizeA, FrameSizeB).
diff(A, B) ->
	diff(<<>>, A, B).

diff(Ret, <<>>, _) ->
	Ret;
diff(Ret, _, <<>>) ->
	Ret;

diff(Ret, <<ByteA:8, RestA/binary>>, <<ByteB:8, RestB/binary>>) ->
	Diff = ByteA - ByteB,
	diff(<<Ret/binary, Diff:8>>, RestA, RestB).
