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

-module(zrtp_test).

-include("zrtp.hrl").
-include_lib("eunit/include/eunit.hrl").

zrtp_test_() ->
	ZrtpMarker = <<16,0>>, % 16#1000
	MagicCookie = <<90,82,84,80>>,

	HelloSequence = <<0,1>>, % 1

	HelloSSRC = <<12,97,171,169>>, % 16#0c61aba9

	HelloPayload = <<80,90,0,38,72,101,108,108,111,32,32,32,49,46,49,48,71,
	78,85,32,90,82,84,80,52,74,32,50,46,49,46,48,36,100,96,195,67,116,141,
	243,85,119,185,231,219,133,160,71,155,124,115,6,65,67,48,136,85,73,84,
	138,65,222,226,134,250,192,159,99,210,55,218,87,82,138,170,107,0,2,68,
	81,83,51,56,52,83,50,53,54,50,70,83,51,65,69,83,51,50,70,83,49,65,69,83,
	49,83,75,51,50,83,75,54,52,72,83,51,50,72,83,56,48,69,67,50,53,68,72,51,
	107,69,67,51,56,68,72,50,107,77,117,108,116,66,51,50,32,166,153,199,74,
	77,108,243,56>>,
	HelloCRC = <<115,174,9,180>>, % 16#73ae09b4

	HelloZrtpBin = <<ZrtpMarker/binary, HelloSequence/binary, MagicCookie/binary, HelloSSRC/binary, HelloPayload/binary, HelloCRC/binary>>,

	HelloMessage = #hello{
		clientid = <<"GNU ZRTP4J 2.1.0">>,
		h3 = <<36,100,96,195,67,116,141,243,85,119,185,231,219,133,160,71,155,124,115,6,65,67,48,136,85,73,84,138,65,222,226,134>>,
		zid = <<250,192,159,99,210,55,218,87,82,138,170,107>>,
		s = 0,
		m = 0,
		p = 0,
		hash = [<<"S384">>,<<"S256">>],
		cipher = [<<"2FS3">>,<<"AES3">>,<<"2FS1">>,<<"AES1">>],
		auth = [<<"SK32">>,<<"SK64">>,<<"HS32">>,<<"HS80">>],
		keyagr = [<<"EC25">>,<<"DH3k">>,<<"EC38">>,<<"DH2k">>,<<"Mult">>],
		sas = [<<"B32 ">>],
		mac = <<166,153,199,74,77,108,243,56>>
	},
	HelloZrtp = #zrtp{sequence = 1, ssrc = 16#0c61aba9, message = HelloMessage},

	HelloACKSequence = <<0,2>>, % 2
	HelloACKSSRC = <<131,2,99,21>>,
	HelloACKPayload = <<80,90,0,3,72,101,108, 108,111,65,67,75>>,
	HelloACKCRC = <<19,33,158,48>>,
	HelloACKZrtpBin = <<ZrtpMarker/binary, HelloACKSequence/binary, MagicCookie/binary, HelloACKSSRC/binary, HelloACKPayload/binary, HelloACKCRC/binary>>,
	HelloACKZrtp = #zrtp{sequence = 2, ssrc = 2197971733, message = helloack},

	[
		{"Simple decoding of ZRTP HELLO message payload",
			fun() -> ?assertEqual({ok, HelloMessage}, zrtp:decode_message(HelloPayload)) end
		},
		{"Simple encoding of ZRTP HELLO message to binary",
			fun() -> ?assertEqual(HelloPayload, zrtp:encode_message(HelloMessage)) end
		},
		{"Simple decoding of ZRTP HELLO data packet",
			fun() -> ?assertEqual({ok, HelloZrtp}, rtp:decode(HelloZrtpBin)) end
		},
		{"Check that we can reproduce original HELLO data stream from HELLO record",
			fun() -> ?assertEqual(HelloZrtpBin, rtp:encode(HelloZrtp)) end
		},
		{"Simple decoding of ZRTP HELLOACK message payload",
			fun() -> ?assertEqual({ok, helloack}, zrtp:decode_message(HelloACKPayload)) end
		},
		{"Simple encoding of ZRTP HELLOACK message to binary",
			fun() -> ?assertEqual(HelloACKPayload, zrtp:encode_message(helloack)) end
		},
		{"Simple decoding of ZRTP HELLO ACK data packet",
			fun() -> ?assertEqual({ok, HelloACKZrtp}, rtp:decode(HelloACKZrtpBin)) end
		},
		{"Check that we can reproduce original HELLOACK data stream from HELLOACK record",
			fun() -> ?assertEqual(HelloACKZrtpBin, rtp:encode(HelloACKZrtp)) end
		}
	].
