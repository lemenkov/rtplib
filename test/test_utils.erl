-module(test_utils).

-compile(export_all).

codec_encode(FileIn, FileOut, FrameSize, CodecName, CodecType) ->
	{ok, PcmIn}  = file:read_file(FileIn),
	{ok, BinOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:encode(CodecName, Codec, PcmIn, BinOut, FrameSize),

	codec:close(Codec),

	Ret.

codec_decode(FileIn, FileOut, FrameSize, CodecName, CodecType) ->
	{ok, BinIn}  = file:read_file(FileIn),
	{ok, PcmOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:decode(CodecName, Codec, BinIn, PcmOut, FrameSize),

	codec:close(Codec),

	Ret.

decode(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA) when size(A) < FrameSizeA ->
	true;
decode(Name, Codec, A, B, FrameSizeA) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	{ok, {FrameB, 8000, 1, 16}} = codec:decode(Codec, FrameA),
	FrameSizeB = size(FrameB),
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	decode(Name, Codec, RestA, RestB, FrameSizeA).

encode(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA) when size(A) < FrameSizeA ->
	true;
encode(Name, Codec, A, B, FrameSizeA) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	{ok, FrameB} = codec:encode(Codec, {FrameA, 8000, 1, 16}),
	FrameSizeB = size(FrameB),
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	encode(Name, Codec, RestA, RestB, FrameSizeA).

%%
%% These functions are not intended for the end user
%%

decode_f(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA) when size(A) < FrameSizeA ->
	true;
decode_f(Name, Codec, A, B, FrameSizeA) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	{ok, {FrameB, 8000, 1, 16}} = codec:decode(Codec, FrameA),
	FrameSizeB = size(FrameB),
	case B of
		<<FrameB:FrameSizeB/binary, RestB/binary>> ->
			decode_f(Name, Codec, RestA, RestB, FrameSizeA);
		<<FrameB1:FrameSizeB/binary, RestB/binary>> ->
			error_logger:info_msg(
				"Bitstream mismatch while decoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, FrameB1, FrameB, size(FrameB), diff(FrameB1, FrameB)]
			),
			decode_f(Name, Codec, RestA, RestB, FrameSizeA);
		Else ->
			error_logger:info_msg(
				"Bitstream failure while decoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, Else, FrameB, size(FrameB), diff(Else, FrameB)]
			),
			true
	end.

encode_f(Name, Codec, <<_/binary>> = A, <<_/binary>> = B, FrameSizeA) when size(A) < FrameSizeA ->
	true;
encode_f(Name, Codec, A, B, FrameSizeA) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = A,
	{ok, FrameB} = codec:encode(Codec, {FrameA, 8000, 1, 16}),
	FrameSizeB = size(FrameB),
	case B of
		<<FrameB:FrameSizeB/binary, RestB/binary>> ->
			encode_f(Name, Codec, RestA, RestB, FrameSizeA);
		<<FrameB1:FrameSizeB/binary, RestB/binary>> ->
			error_logger:info_msg(
				"Bitstream mismatch while encoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, FrameB1, FrameB, size(FrameB), diff(FrameB1, FrameB1)]
			),
			encode_f(Name, Codec, RestA, RestB, FrameSizeA);
		Else ->
			error_logger:info_msg(
				"Bitstream failure while encoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, Else, FrameB, size(FrameB), diff(Else, FrameB)]
			),
			true
	end.

diff(A, B) ->
	diff(<<>>, A, B).

diff(Ret, <<>>, _) ->
	Ret;
diff(Ret, _, <<>>) ->
	Ret;

diff(Ret, <<ByteA:8, RestA/binary>>, <<ByteB:8, RestB/binary>>) ->
	Diff = ByteA - ByteB,
	diff(<<Ret/binary, Diff:8>>, RestA, RestB).
