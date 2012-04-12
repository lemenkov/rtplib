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

-module(rtcp_bye_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_BYE_test_() ->
	ByeBin = <<161,203,0,3,0,0,4,0,6,67,97,110,99,101,108,0>>,
	Bye = #rtcp{payloads = [#bye{message = "Cancel", ssrc = [1024]}]},
	[
		{"Simple encoding of BYE RTCP data stream",
			fun() -> ?assertEqual(ByeBin, rtcp:encode_bye([1024], "Cancel")) end
		},
		{"Simple decoding BYE RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, Bye}, rtcp:decode(ByeBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(ByeBin, rtcp:encode(Bye)) end
		}
	].

%% Taken from real RTCP capture from unidentified source
rtcp_BYE_padding_test_() ->
	% Padding at the end <<0,0,0,0>>
	ByeBin = <<129,203,0,5,128,171,245,31,15,68,105,115,99,111,110,110,101,99,116,32,67,97,108,108,0,0,0,0>>,
	ByeBinNoPadding = <<129,203,0,5,128,171,245,31,15,68,105,115,99,111,110,110,101,99,116,32,67,97,108,108>>,
	Bye = #rtcp{payloads = [#bye{message="Disconnect Call",ssrc=[2158753055]}]},
	[
		{"Decode BYE RTCP with unnecessary padding",
			fun() -> ?assertEqual({ok, Bye}, rtcp:decode(ByeBin)) end
		},
		{"Encode BYE RTCP properly (w/o padding)",
			fun() -> ?assertEqual(ByeBinNoPadding, rtcp:encode(Bye)) end
		}
	].
