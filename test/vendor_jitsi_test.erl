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

-module(vendor_jitsi_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Jitsi generates broken DTMF
vendor_jitsi_test_() ->
	RtpBin = <<128,229,246,244,0,134,97,64,204,140,115,227,1,0,0,160,34,203,
	22,168,158,31,33,59,170,151,50,30,71,46,44,61,176,232,88,146,169,32,223,
	46,167,183,48,206,63,110,32,165,48,72,86,13,211,164,154,57,43,160,154,
	155,194,47,51,29,22,189,48,31,181,110,203,147,185,171,144,175,153,162,
	149,143,55,33,30,227,25,35,57,34,151,213,46,166,171,173,194,186,154,212,
	254,172,27,48,35,190,160,26,176,196,198,171,30,92,60,37,172,155,158,186,
	63,167,170,33,205,233,34,190,175,57,26,152,158,22,180,39,16,52,166,68,
	34,60,54,229,169,153,97,21,37,159,49,23,26,38,159,38,55,66,30,37,31,190,
	50,35,41,201,175,159,152,215,186,34,50,181>>,

	Dtmf = #dtmf{
		event = 1,
		eof = false,
		volume = 0,
		duration = 160
	},

	Rtp = #rtp{
		padding = 0,
		marker = 1,
		payload_type = 101,
		sequence_number = 63220,
		timestamp = 8806720,
		ssrc = 3431756771,
		csrcs = [],
		extension = null,
		payload = Dtmf
	},

	{setup,
		fun() ->
				ok
		end,
		fun(_) ->
				%% Unset DTMF id
				erase(101)
		end,
		[
			{"Try to decode incorrectly padded DTMF",
				fun() ->	%% Set DTMF id to 101
						put(101, dtmf),
						?assertEqual({ok, Rtp}, rtp:decode(RtpBin))
				end
			}
		]
	}.
