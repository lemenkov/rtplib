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

-module(zrtp_infrastructure_test).

-include("zrtp.hrl").
-include_lib("eunit/include/eunit.hrl").

%%
%% Various ZRTP-related routines
%%

zrtp_infrastructure_test_() ->
	[
		{"Check hash negitiation",
			fun() -> ?assertEqual(?ZRTP_HASH_S256, zrtp:negotiate(?ZRTP_HASH_S256, [], [<<"S256">>, <<"S384">>])) end
		},
		{"Check cipher negitiation",
			fun() -> ?assertEqual(?ZRTP_CIPHER_AES1, zrtp:negotiate(?ZRTP_CIPHER_AES1, [], [<<"AES1">>])) end
		},
		{"Check auth negitiation",
			fun() -> ?assertEqual(?ZRTP_AUTH_TAG_HS32, zrtp:negotiate(?ZRTP_AUTH_TAG_HS32, [], [<<"HS32">>, <<"HS80">>])) end
		},
		{"Check key agreement negitiation",
			fun() -> ?assertEqual(?ZRTP_KEY_AGREEMENT_DH3K, zrtp:negotiate(?ZRTP_KEY_AGREEMENT_DH3K, [], [<<"DH3k">>, <<"Mult">>])) end
		},
		{"Check SAS negitiation",
			fun() -> ?assertEqual(?ZRTP_SAS_TYPE_B32, zrtp:negotiate(?ZRTP_SAS_TYPE_B32, [], [<<"B32 ">>])) end
		}
	].
