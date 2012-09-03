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

-module(crc32c_test).

-include_lib("eunit/include/eunit.hrl").

crc32c_test_() ->
	{setup,
		fun() -> crc32c:init() end,
		fun (_) -> ok end,
		[
			{"32 bytes of zeroes (see RFC 3270 B.4).",
				fun() -> ?assertEqual(<<16#aa, 16#36, 16#91, 16#8a>>, crc32c:crc32c(<< <<0:8>> || X <- lists:seq(0,31) >>)) end
			},
			{"32 bytes of 0xFF (see RFC 3270 B.4).",
				fun() -> ?assertEqual(<<16#43, 16#ab, 16#a8, 16#62>>, crc32c:crc32c(<< <<16#ff:8>> || X <- lists:seq(0,31) >>)) end
			},
			{"32 bytes of consequently incrementing values (see RFC 3270 B.4).",
				fun() -> ?assertEqual(<<16#4e, 16#79, 16#dd, 16#46>>, crc32c:crc32c(<< <<X:8>> || X <- lists:seq(0,31) >>)) end
			},
			{"32 bytes of consequently decrementing values (see RFC 3270 B.4).",
				fun() -> ?assertEqual(<<16#5c, 16#db, 16#3f, 16#11>>, crc32c:crc32c(<< <<(31-X):8>> || X <- lists:seq(0,31) >>)) end
			}
		]
	}.
