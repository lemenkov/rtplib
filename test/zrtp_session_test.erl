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

-module(zrtp_session_test).

-include("zrtp.hrl").
-include_lib("eunit/include/eunit.hrl").

zrtp_session_test_() ->
	Zid1 = crypto:rand_bytes(96),
	Zid2 = crypto:rand_bytes(96),

	Ssrc1 = crypto:rand_bytes(4),
	Ssrc2 = crypto:rand_bytes(4),

	{ok, Zrtp1} = zrtp_fsm:start_link([null, Zid1, Ssrc1]),
	{ok, Zrtp2} = zrtp_fsm:start_link([null, Zid2, Ssrc2]),

	Hello1 = gen_server:call(Zrtp1, init),
	Hello2 = gen_server:call(Zrtp2, init),

	Hello1Ack = gen_server:call(Zrtp1, Hello2),
	Hello2Ack = gen_server:call(Zrtp2, Hello1),

	Commit1 = gen_server:call(Zrtp1, Hello2Ack),
	Commit2 = gen_server:call(Zrtp2, Hello1Ack),

	Something1 = gen_server:call(Zrtp1, Commit2),
	Something2 = gen_server:call(Zrtp2, Commit1),

	{Alice, Bob, DHpart1} = case Something1 of
		ok -> {Zrtp1, Zrtp2, Something2};
		_ -> {Zrtp2, Zrtp1, Something1}
	end,

	% Now we clearly know who is initiator (Alice) and who is receiver (Bob)
	% Receiver must reply with DHpart1
	DHpart2 = gen_server:call(Alice, DHpart1),
	Confirm1 = gen_server:call(Bob, DHpart2),
	Confirm2 = gen_server:call(Alice, Confirm1),
	Conf2Ack = gen_server:call(Bob, Confirm2),

	Keys1 = gen_server:call(Alice, get_keys),
	Keys2 = gen_server:call(Bob, get_keys),

	[
		{"Check that resulting crypto data is equal",
			fun() -> ?assertEqual(Keys1, Keys2) end
		}
	].

