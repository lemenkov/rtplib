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

-module(rtcp_app_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_APP_test_() ->
	AppBin = <<133,204,0,8,0,0,4,0,83,84,82,49,72,101,108,108,111,33,32,84,104,105,115,32,105,115,32,97,32,115,116,114,105,110,103,46>>,
	[
		{"Simple encoding of APP RTCP data stream",
			fun() -> ?assertEqual(AppBin, rtcp:encode_app(5, 1024, "STR1", <<"Hello! This is a string.">>)) end
		},
		{"Simple decoding APP RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>}]},
						rtcp:decode(AppBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(AppBin, rtcp:encode(#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>})) end
		}
	].
