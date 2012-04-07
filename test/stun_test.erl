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

-module(stun_test).

-include("stun.hrl").
-include_lib("eunit/include/eunit.hrl").

stun_test_() ->
	% samples taken from http://thread.gmane.org/gmane.comp.voip.pjsip/s9522
	StunBindReqBin = <<0,1,0,16,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,128,34,0,12,112,106,110,97,116,104,45,49,46,52,0,0>>,
	StunBindReq = #stun{
		class = request,
		method = 1,
		transactionid = 45554200240623869818762035456,
		attrs = [{32802,<<"pjnath-1.4", 0, 0>>}]
	},

	StunBindRespBin = <<1,1,0,68,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,0,1,0,8,0,1,224,252,88,198,53,113,0,4,0,8,0,1,13,150,208,109,222,137,0,5,0,8,0,1,13,151,208,109,222,148,128,32,0,8,0,1,193,238,121,212,145,51,128,34,0,16,86,111,118,105,100,97,46,111,114,103,32,48,46,57,54,0>>,
	StunBindResp = #stun{
		class = success,
		method = 1,
		transactionid = 45554200240623869818762035456,
		attrs = [
			{1,<<0,1,224,252,88,198,53,113>>},
			{4,<<0,1,13,150,208,109,222,137>>},
			{5,<<0,1,13,151,208,109,222,148>>},
			{32800,<<0,1,193,238,121,212,145,51>>},
			{32802,<<"Vovida.org 0.96", 0>>}
		]
	},

	[
		{"Simple decoding of STUN Binding Request",
			fun() -> ?assertEqual({ok, StunBindReq}, stun:decode(StunBindReqBin)) end
		},
		{"Simple encoding of STUN Binding Request",
			fun() -> ?assertEqual(StunBindReqBin, stun:encode(StunBindReq)) end
		},
		{"Simple decoding of STUN Binding Responce",
			fun() -> ?assertEqual({ok, StunBindResp}, stun:decode(StunBindRespBin)) end
		},
		{"Simple encoding of STUN Binding Responce",
			fun() -> ?assertEqual(StunBindRespBin, stun:encode(StunBindResp)) end
		}
	].
