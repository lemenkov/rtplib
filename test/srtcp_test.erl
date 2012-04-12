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

-module(srtcp_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

zrtp_test_() ->
	SrtcpSrBin = <<129,200,0,12,131,2,101,199,102,248,250,11,232,111,44,166,210,29,192,15,102,98,25,191,215,224,156,194,134,209,132,213,198,231,202,132,85,127,137,8,253,142,229,114,2,151,209,173,42,238,131,200,170,244,100,163,18,43,48,105,212,99,7,227,26,180,246,78,83,154,31,36,213,204,121,109,0,29,1,116,9,90,69,67,47,219,29,45,213,160,168,102,15,31,248,218,79,25,173,4,111,185,89,143,175,62,209,121,192,26,218,244,69,244,237,152,20,231,248,11,108,139,148,75,103,59,69,148,57,183,249,149,149,11,186,0,128,0,0,0,178,174,231,18>>,
	SrtcpSr = #rtcp{encrypted = SrtcpSrBin},
	[
		{"Simple pass-thru decrypting of the SRTCP data",
			fun() -> ?assertEqual({ok, SrtcpSr}, srtp:decrypt(SrtcpSrBin, passthru)) end
		},
		{"Simple pass-thru encrypting of the SRTP structure",
			fun() -> ?assertEqual(SrtcpSrBin, srtp:encrypt(SrtcpSr, passthru)) end
		}
	].
