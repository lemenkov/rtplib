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

-module(vendor_pbxnsip_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_pbxnsip_test_() ->

	% There is a miscalculated size of the SR block this packet - 4th octet
	% must be 9 instead of 8. Also there is a strange padding at the end.
	%                      -=9=-
	RtcpSrBin =
		<<129,200,0,8,17,195,69,247,209,206,159,196,201,251,230,131,59,
		71,192,80,0,0,0,5,0,0,3,32,0,0,0,0,0,0,0,0,0,0,0,0>>,
	RtcpSrBinCorrect =
		<<128,200,0,6,17,195,69,247,209,206,159,196,201,251,230,
		131,59,71,192,80,0,0,0,5,0,0,3,32>>,
	RtcpSdesBin = <<129,202,0,9,17,195,69,247,1,27,116,101,115,116,97,99,
		99,116,64,115,105,112,46,101,120,97,109,112,108,101,48,48,46,
		110,101,116,0,0,0,0>>,

	RtcpSr = #sr{
		ssrc=298010103,
		ntp=15118196666680469123,
		timestamp=994558032,
		packets=5,
		octets=800,
		rblocks=[]
	},
	RtcpSdes = #sdes{list=[[
				{ssrc,298010103},
				{cname,"testacct@sip.example00.net" ++ [0]},
				{eof,true}
			]]},

	[
		{"Check that we still can decode broken RTCP packet correctly",
			fun() -> ?assertEqual({ok, [RtcpSr, RtcpSdes]},rtcp:decode(<<RtcpSrBin/binary, RtcpSdesBin/binary>>)) end
		},
		{"Check that we can produce fixed RTCP SR",
			fun() -> ?assertEqual(RtcpSrBinCorrect, rtcp:encode(RtcpSr)) end
		},
		{"Check that we can reproduce original RTCP SDES",
			fun() -> ?assertEqual(RtcpSdesBin, rtcp:encode(RtcpSdes)) end
		}
	].
