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

-module(rtp_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtp_test_() ->
	PCMUBin = <<193,179,189,216,194,104,66,67,58,56,53,57,68,64,81,235,
		109,228,213,203,203,213,200,208,245,200,197,197,179,188,215,192,
		123,67,65,57,57,53,55,70,63,79,233,110,225,213,203,201,214,204,
		205,219,207,199,190,184,185,206,200,230,70,64,57,57,53,54,67,66,
		75,245,250,232,217,204,199,214,217,207,202,231,219,194,195,189,
		179,197,208,202,76,63,60,55,56,52,60,72,67,92,231,237,220,212,
		199,204,220,204,206,229,209,192,191,185,183,204,204,231,71,62,
		57,57,54,53,63,73,77,109,236,219,219,209,197,212,215,204,221,
		223,220,205,196,190,184,186,207,212,252,70,60,59,57,53,56,66,71,
		78,108,227,220,223,206,197>>,

	RtpPCMUBin = <<128,0,253,88,145,83,40,165,11,160,200,38, PCMUBin/binary>>,

	RtpPCMU = #rtp{
		padding = 0,
		marker = 0,
		payload_type = ?RTP_PAYLOAD_PCMU,
		sequence_number = 64856,
		timestamp = 2438146213,
		ssrc = 195086374,
		csrcs = [],
		extension = null,
		payload = PCMUBin
	},

	[
		{"Simple decoding of PCMU RTP data packet",
			fun() -> ?assertEqual({ok, RtpPCMU}, rtp:decode(RtpPCMUBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(RtpPCMUBin, rtp:encode(RtpPCMU)) end
		}
	].
