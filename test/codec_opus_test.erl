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
	[
		{"Test decoding from OPUS to PCM (01)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector01.bit", "../test/samples/opus/testvector01.dec")) end
		},
		{"Test decoding from OPUS to PCM (02)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector02.bit", "../test/samples/opus/testvector02.dec")) end
		},
		{"Test decoding from OPUS to PCM (03)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector03.bit", "../test/samples/opus/testvector03.dec")) end
		},
		{"Test decoding from OPUS to PCM (04)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector04.bit", "../test/samples/opus/testvector04.dec")) end
		},
		{"Test decoding from OPUS to PCM (05)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector05.bit", "../test/samples/opus/testvector05.dec")) end
		},
		{"Test decoding from OPUS to PCM (06)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector06.bit", "../test/samples/opus/testvector06.dec")) end
		},
		{"Test decoding from OPUS to PCM (07)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector07.bit", "../test/samples/opus/testvector07.dec")) end
		},
		{"Test decoding from OPUS to PCM (08)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector08.bit", "../test/samples/opus/testvector08.dec")) end
		},
		{"Test decoding from OPUS to PCM (09)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector09.bit", "../test/samples/opus/testvector09.dec")) end
		},
		{"Test decoding from OPUS to PCM (10)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector10.bit", "../test/samples/opus/testvector10.dec")) end
		},
		{"Test decoding from OPUS to PCM (11)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector11.bit", "../test/samples/opus/testvector11.dec")) end
		},
		{"Test decoding from OPUS to PCM (12)",
			fun() -> ?assertEqual(true, decode("../test/samples/opus/testvector12.bit", "../test/samples/opus/testvector12.dec")) end
		}
	].

decode(FileIn, FileOut) ->
	{ok, BinIn}  = file:read_file(FileIn),
	{ok, PcmOut} = file:read_file(FileOut),
	{ok, Codec} = codec:start_link({'OPUS', 48000, 2}),
	Ret = decode("OPUS", Codec, BinIn, PcmOut),
	codec:close(Codec),
	Ret.

decode(Name, Codec, <<>>, <<>>) ->
	true;
decode(Name, Codec, <<FrameSizeA:32/big-integer, FinalRange:32/big-integer, Rest/binary>> = A, B) ->
	<<FrameA:FrameSizeA/binary, RestA/binary>> = Rest,
	{ok, {FrameB, _, _, _}} = codec:decode(Codec, FrameA),
	FrameSizeB = size(FrameB),
	<<FrameB:FrameSizeB/binary, RestB/binary>> = B,
	decode(Name, Codec, RestA, RestB).
