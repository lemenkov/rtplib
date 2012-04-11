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

-module(rtcp_nack_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_NACK_test_() ->
	NackBin = <<128,193,0,2,0,0,4,0,8,1,16,1>>,
	Nack = #nack{ssrc=1024, fsn=2049, blp=4097},
	[
		{"Simple encoding of NACK RTCP data stream",
			fun() -> ?assertEqual(NackBin, rtcp:encode_nack(1024, 2049, 4097)) end
		},
		{"Simple decoding NACK RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [Nack]}, rtcp:decode(NackBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(NackBin, rtcp:encode(Nack)) end
		}
	].
