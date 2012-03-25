%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(test_utils).

-compile(export_all).

codec_encode(FileIn, FileOut, FrameSize, CodecName, CodecType) ->
	{ok, PcmIn}  = file:read_file(FileIn),
	{ok, BinOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:encode(CodecName, Codec, le16toh(PcmIn), BinOut, FrameSize),

	codec:close(Codec),

	Ret.

codec_decode(FileIn, FileOut, FrameSize, CodecName, CodecType) ->
	{ok, BinIn}  = file:read_file(FileIn),
	{ok, PcmOut} = file:read_file(FileOut),

	{ok, Codec} = codec:start_link(CodecType),

	Ret = test_utils:decode(CodecName, Codec, BinIn, le16toh(PcmOut), FrameSize),

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
				"Bitstream mismatch while decoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nExpected size: ~p.~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, FrameB1, FrameB, size(FrameB1), size(FrameB), diff(FrameB1, FrameB)]
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
				"Bitstream mismatch while encoding from ~s frame.~nExpected:~n~p~nGot:~n~p~nExpected size: ~p.~nDecoded size: ~p.~nDecoded diff: ~p~n",
				[Name, FrameB1, FrameB, size(FrameB1), size(FrameB), diff(FrameB1, FrameB1)]
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

diff(Ret, <<>>, B) ->
	<<Ret/binary, B/binary>>;
diff(Ret, A, <<>>) ->
	<<Ret/binary, A/binary>>;

diff(Ret, <<ByteA:8, RestA/binary>>, <<ByteB:8, RestB/binary>>) ->
	Diff = ByteA - ByteB,
	diff(<<Ret/binary, Diff:8>>, RestA, RestB).

le16toh(Binary) ->
	le16toh(Binary, <<>>).
le16toh(<<>>, Binary) ->
	Binary;
le16toh(<<A:16/little-integer, Rest/binary>>, Converted) ->
	le16toh(Rest, <<Converted/binary, A:16>>).
