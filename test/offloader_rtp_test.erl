%%%----------------------------------------------------------------------
%%% Copyright (c) 2014 Peter Lemenkov <lemenkov@gmail.com>
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

-module(offloader_rtp_test).

-include("offloader_rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

offloader_rtp_cmd_noop_test_() ->
	NoopMsg = #mediaproxy_message{},
	NoopBin = << ?MEDIAPROXY_CMD_NOOP:8/little-unsigned-integer-unit:1, << <<0:8>> || I <- lists:seq(1,223) >>/binary>>,

	[
		{"Enconding of a NOOP message",
			fun() -> ?assertEqual(NoopBin, offloader_rtp:message_to_binary(NoopMsg)) end
		},
		{"Decoding of a NOOP bitstream",
			fun() -> ?assertEqual({ok, NoopMsg}, offloader_rtp:binary_to_message(NoopBin)) end
		}
	].


offloader_rtp_cmd_add_test_() ->
	AddMsg = #mediaproxy_message{
			cmd = add,
			target = #mediaproxy_target_info{
					target_port = 12345,
					src_addr = #mediaproxy_address{ip = {8,8,8,8}, port = 54321},
					dst_addr = #mediaproxy_address{ip = {8,8,4,4}, port = 12321}
			}
	},
	AddBin = <<2,0,0,0,0,0,0,0,57,48,0,0,2,0,0,0,8,8,8,8,0,0,0,0,0,0,0,0,0,0,0,0,49,212,0,0,2,0,0,0,8,8,4,4,0,0,0,0,0,0,0,0,0,0,0,0,33,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	[
		{"Enconding of an ADD message",
			fun() -> ?assertEqual(AddBin, offloader_rtp:message_to_binary(AddMsg)) end
		},
		{"Decoding of an ADD bitstream",
			fun() -> ?assertEqual({ok, AddMsg}, offloader_rtp:binary_to_message(AddBin)) end
		}
	].

offloader_rtp_cmd_del_test_() ->
	DelMsg = #mediaproxy_message{
			cmd = del,
			target = #mediaproxy_target_info{target_port = 12345}
	},
	% We care only about <<<3,0,0,0,0,0,0,0,57,48>> part - the rest can be anything.
	DelBin = <<3,0,0,0,0,0,0,0,57,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
	[
		{"Enconding of a DEL message",
			fun() -> ?assertEqual(DelBin, offloader_rtp:message_to_binary(DelMsg)) end
		},
		{"Decoding of a DEL bitstream",
			fun() -> ?assertEqual({ok, DelMsg}, offloader_rtp:binary_to_message(DelBin)) end
		}
	].

% FIXME implement test for this cmessage when it will be actually used
offloader_rtp_cmd_update_test_() ->
	[
	].
