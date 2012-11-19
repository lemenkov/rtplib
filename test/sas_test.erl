%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
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

-module(sas_test).

-include_lib("eunit/include/eunit.hrl").

b32_test_() ->
	[
		% https://github.com/wernerd/ZRTPCPP/blob/master/zrtp/Base32.cpp#L255
		{"Vector 01",
			fun() -> ?assertEqual(<<"yyyy">>, sas:b32(<<16#00, 16#00, 16#00, 16#00>>)) end
		},
		{"Vector 02",
			fun() -> ?assertEqual(<<"oyyy">>, sas:b32(<<16#80, 16#00, 16#00, 16#00>>)) end
		},
		{"Vector 03",
			fun() -> ?assertEqual(<<"eyyy">>, sas:b32(<<16#40, 16#00, 16#00, 16#00>>)) end
		},
		{"Vector 04",
			fun() -> ?assertEqual(<<"ayyy">>, sas:b32(<<16#c0, 16#00, 16#00, 16#00>>)) end
		},
		{"Vector 05",
			fun() -> ?assertEqual(<<"yyyy">>, sas:b32(<<16#00, 16#00, 16#00, 16#00>>)) end
		},
		{"Vector 06",
			fun() -> ?assertEqual(<<"onyy">>, sas:b32(<<16#80, 16#80, 16#00, 16#00>>)) end
		},
		{"Vector 07",
			fun() -> ?assertEqual(<<"tqre">>, sas:b32(<<16#8b, 16#88, 16#80, 16#00>>)) end
		},
		{"Vector 08",
			fun() -> ?assertEqual(<<"6n9h">>, sas:b32(<<16#f0, 16#bf, 16#c7, 16#00>>)) end
		},
		{"Vector 09",
			fun() -> ?assertEqual(<<"4t7y">>, sas:b32(<<16#d4, 16#7a, 16#04, 16#00>>)) end
		},
		{"Vector 10",
			fun() -> ?assertEqual(<<"6im5">>, sas:b32(<<16#f5, 16#57, 16#bb, 16#0c>>)) end
		}
	].
