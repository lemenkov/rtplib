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

-module(vendor_cisco_7960_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_cisco_7960_test_() ->
	PCMUPayload1 = <<253,253,254,253,
		253,254,254,254,254,253,253,253,253,253,254,254,254,254,254,254,
		254,255,254,255,127,254,254,254,255,127,126,126,126,127,126,126,
		127,127,255,255,254,254,254,254,253,254,254,254,255,127,254,255,
		127,255,255,255,127,127,255,255,255,255,127,255,127,126,127,127,
		126,126,126,125,126,126,126,125,125,125,125,125,126,125,125,125,
		126,127,126,127,127,126,255,255,254,253,254,253,253,254,254,253,
		254,254,255,255,254,126,254,255,127,254,254,253,254,127,254,255,
		126,254,125,126,127,126,127,126,126,126,127,127,126,127,125,126,
		126,125,255,126,126,126,126,125,124,126,126,126,126,125,127,255,
		126,254,254,127,254,255,127,254,254,253,254,253>>,
	PCMUPayload2 = <<253,254,253,253,254,
		253,254,254,253,254,255,254,254,254,254,127,255,255,255,254,255,
		127,255,127,255,127,127,254,127,126,127,125,125,126,127,126,126,
		126,126,126,127,127,255,254,254,255,254,254,254,255,255,253,254,
		255,254,254,127,253,255,126,254,255,254,255,126,126,126,125,125,
		126,126,126,126,125,127,126,126,126,127,255,125,127,127,126,255,
		127,127,127,127,126,127,126,125,127,125,126,254,255,127,254,255,
		255,254,254,254,254,253,254,255,254,254,252,254,255,253,127,127,
		254,255,254,255,254,254,255,254,127,127,254,255,255,255,255,254,
		255,255,254,127,126,127,127,126,127,126,126,254,127,255,255,126,
		127,127,254,254,254,254,255,254,255,254,254>>,
	PCMUPayload3 = <<253,252,253,253,
		253,255,127,126,125,126,126,126,127,126,126,255,254,254,254,255,
		254,253,254,254,254,254,254,255,255,127,127,255,255,127,255,254,
		254,254,254,254,254,255,127,127,127,127,255,127,126,126,125,126,
		127,127,255,255,255,127,127,126,126,255,255,254,253,254,254,254,
		255,255,255,255,127,255,254,254,253,253,253,253,253,254,127,255,
		127,254,254,255,255,127,127,127,126,255,127,127,127,127,255,127,
		127,255,126,126,125,126,126,126,126,126,126,127,255,255,127,127,
		254,254,253,253,252,253,254,254,254,254,254,253,254,253,253,253,
		253,253,253,254,253,254,254,254,253,253,253,253,253,253,254,253,
		253,254,255,255,254,253,253,253,253,252,253,254>>,

	Rtp1Bin = <<128,128,51,27,9,120,82,160,12,197,12,227, PCMUPayload1/binary>>,
	Rtp2Bin = <<128,0,51,28,9,120,83,64,12,197,12,227, PCMUPayload2/binary>>,
	Rtp3Bin = <<128,128,221,21,9,228,158,96,12,197,12,227, PCMUPayload3/binary>>,

	Rtp1 = #rtp{
		padding = 0,
		marker = 1,
		payload_type = ?RTP_PAYLOAD_PCMU,
		sequence_number = 13083,
		timestamp = 158880416,
		ssrc = 214240483,
		csrcs = [],
		extension = null,
		payload = PCMUPayload1
	},
	Rtp2 = #rtp{
		padding = 0,
		marker = 0,
		payload_type = ?RTP_PAYLOAD_PCMU,
		sequence_number = 13084,
		timestamp = 158880576,
		ssrc = 214240483,
		csrcs = [],
		extension = null,
		payload = PCMUPayload2
	},
	Rtp3 = #rtp{
		padding = 0,
		marker = 1,
		payload_type = ?RTP_PAYLOAD_PCMU,
		sequence_number = 56597,
		timestamp = 165977696,
		ssrc = 214240483,
		csrcs = [],
		extension = null,
		payload = PCMUPayload3
	},

	RtpDTMF1Bin = <<128,101,221,245,9,229,4,64,12,197,12,227,7,10,2,128>>,
	RtpDTMF2Bin = <<128,101,221,228,9,228,251,128,12,197,12,227,8,138,3,192>>,

	RtpDTMF1 = #rtp{
		padding = 0,
		marker = 0,
		payload_type = 101,
		sequence_number = 56821,
		timestamp = 166003776,
		ssrc = 214240483,
		csrcs = [],
		extension = null,
		payload = <<7,10,2,128>>
	},
	RtpDTMF2 = #rtp{
		padding = 0,
		marker = 0,
		payload_type = 101,
		sequence_number = 56804,
		timestamp = 166001536,
		ssrc = 214240483,
		csrcs = [],
		extension = null,
		payload = <<8,138,3,192>>
	},

	[
		{"Decode PCMU RTP packet #1",
			fun() -> ?assertEqual({ok, Rtp1}, rtp:decode(Rtp1Bin)) end
		},
		{"Decode PCMU RTP packet #2",
			fun() -> ?assertEqual({ok, Rtp2}, rtp:decode(Rtp2Bin)) end
		},
		{"Decode PCMU RTP packet #3",
			fun() -> ?assertEqual({ok, Rtp3}, rtp:decode(Rtp3Bin)) end
		},
		{"Decode DTMF RTP packet #1",
			fun() -> ?assertEqual({ok, RtpDTMF1}, rtp:decode(RtpDTMF1Bin)) end
		},
		{"Decode DTMF RTP packet #2",
			fun() -> ?assertEqual({ok, RtpDTMF2}, rtp:decode(RtpDTMF2Bin)) end
		},
		{"Encode PCMU RTP packet #1",
			fun() -> ?assertEqual(Rtp1Bin, rtp:encode(Rtp1)) end
		},
		{"Encode PCMU RTP packet #2",
			fun() -> ?assertEqual(Rtp2Bin, rtp:encode(Rtp2)) end
		},
		{"Encode PCMU RTP packet #3",
			fun() -> ?assertEqual(Rtp3Bin, rtp:encode(Rtp3)) end
		},
		{"Encode DTMF RTP packet #1",
			fun() -> ?assertEqual(RtpDTMF1Bin, rtp:encode(RtpDTMF1)) end
		},
		{"Encode DTMF RTP packet #2",
			fun() -> ?assertEqual(RtpDTMF2Bin, rtp:encode(RtpDTMF2)) end
		}
	].
