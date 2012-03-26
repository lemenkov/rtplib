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

-module(codec_opus_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_opus_test_() ->
	{ok, BinIn}  = file:read_file("../test/samples/opus/testvector01.bit"),
	{ok, PcmOut} = file:read_file("../test/samples/opus/testvector01.dec"),
	{ok, Codec} = codec:start_link({'OPUS',48000,2}),
%	codec:close(Codec),
	[
%		{"Test encoding from PCM to OPUS",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/opus/testvector01.dec",
%							"../test/samples/opus/testvector01.bit",
%							160,
%							"OPUS",
%							{'OPUS',8000,1}
%						)
%					) end
%		},
		{"Test decoding from OPUS to PCM",
			fun() -> ?assertEqual(true, decode("OPUS", Codec, BinIn, PcmOut)) end
%			fun() -> ?assertEqual(true, decode("OPUS", Codec, BinIn, test_utils:le16toh(PcmOut))) end
		}
	].

decode(Name, Codec, <<>>, <<>>) ->
	true;
decode(Name, Codec, <<FrameSizeA:32/big-integer, FinalRange:32/big-integer, Rest/binary>> = A, B) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = Rest,
	{ok, {FrameB, F, Nc, 16}} = codec:decode(Codec, FrameA),
	FrameSizeB = size(FrameB),
%	error_logger:info_msg("Frame: ~p~n, Freq: ~p~n, Nc: ~p~n, Size: ~p~n", [FrameB, F, Nc, FrameSizeB]),
	<<PossibleFrameB:FrameSizeB/binary, RestB/binary>> = B,
	if
		FrameB == PossibleFrameB ->
			ok;
		true ->
			error_logger:info_msg("Frame Size: ~p~n", [FrameSizeB div 4, test_utils:diff(FrameB, PossibleFrameB)])
	end,
%	error_logger:info_msg("PFrame: ~p~n, Freq: ~p~n, Nc: ~p~n, Size: ~p~n", [PossibleFrameB, F, Nc, FrameSizeB]),
%	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	decode(Name, Codec, RestA, RestB).
