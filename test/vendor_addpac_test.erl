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

-module(vendor_addpac_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_addpac_test_() ->
	SrBin = <<129,200,0,12,37,184,163,28,0,2,12,253,17,249,156,55,0,158,81,164,0,0,81,18,0,50,171,64,23,171,200,54,0,0,1,43,0,0,0,0,0,0,0,0,29,182,43,19,0,0,62,118>>,
	SdesBin = <<129,202,0,7,37,184,163,28,1,19,222,173,65,100,100,80,97,99,32,86,111,73,80,32,71,97,116,101,119,97,121,0>>,
	SrSdesBin = <<SrBin/binary, SdesBin/binary>>,

	SdesBinFixed = <<129,202,0,7,37,184,163,28,1,19,65,100,100,80,97,99,32,86,111,73,80,32,71,97,116,101,119,97,121,0,0,0>>,
	SrSdesBinFixed = <<SrBin/binary, SdesBinFixed/binary>>,

	RBlock = #rblock{
		ssrc = 397133878,
		fraction = 0,
		lost = 299,
		last_seq = 0,
		jitter = 0,
		lsr = 498477843,
		dlsr = 15990
	},
	Sr = #sr{
		ssrc = 632857372,
		ntp = 577231021251639,
		timestamp = 10375588,
		packets = 20754,
		octets = 3320640,
		rblocks = [RBlock]
	},
	Sdes = #sdes{
		list = [
			[
				{ssrc, 632857372},
				{cname,"AddPac VoIP Gateway"},
				{eof,true}
			]
		]
	},

	[
		{"Decode the entire SR+SDES packet",
			fun() -> ?assertEqual({ok, [Sr, Sdes]}, rtcp:decode(SrSdesBin)) end
		},
		{"Encode the entire SR+SDES packet",
			fun() -> ?assertEqual(SrSdesBinFixed, rtcp:encode([Sr, Sdes])) end
		}
	].
