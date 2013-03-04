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

-module(rtcp_sr_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_SR_test_() ->
	% First, we should prepare several RBlocks
	RBlock1Bin = <<0,0,4,0,2,0,4,2,0,0,4,3,0,0,4,4,0,0,4,5,0,0,4,6>>,
	RBlock2Bin = <<0,1,134,184,2,1,134,186,0,1,134,187,0,1,134,188,0,1,134,189,0,1,134,190>>,
	RBlocksBin = <<RBlock1Bin/binary, RBlock2Bin/binary>>,

	RBlock1 = #rblock{ssrc=1024, fraction=2, lost=1026, last_seq=1027, jitter=1028, lsr=1029, dlsr=1030},
	RBlock2 = #rblock{ssrc=100024, fraction=2, lost=100026, last_seq=100027, jitter=100028, lsr=100029, dlsr=100030},

	% random NTP timestamp with rtp_utils:now2ntp(now())
	<<NTP:64>> = <<210,79,225,24,250,129,85,222>>,
	% valid SR packet
	SRBin = <<130,200,0,18,0,0,16,0,210,79,225,24,250,129,85,222,0,0,16,2,0,
		0,255,255,0,1,0,0, RBlocksBin/binary>>,

	SR = #rtcp{payloads = [
			#sr{
				ssrc=4096,
				ntp=15154578768523253214,
				timestamp=4098,
				packets=65535,
				octets=65536,
				rblocks=[RBlock1, RBlock2]
			}
		]
	},

	[
		{"Encode one RBlock",
			fun() -> ?assertEqual(RBlock1Bin, rtcp:encode_rblock(1024, 2, 1026, 1027, 1028, 1029, 1030)) end
		},
		{"Encode another one RBlock",
			fun() -> ?assertEqual(RBlock2Bin, rtcp:encode_rblock(100024, 2, 100026, 100027, 100028, 100029, 100030)) end
		},
		{"Decode both binary RBLocks",
			fun() -> ?assertEqual({[RBlock1, RBlock2], <<>>}, rtcp:decode_rblocks(RBlocksBin, 2)) end
		},
		{"Check correct Report Blocks processing",
			fun() -> ?assertEqual(RBlocksBin,rtcp:encode_rblocks([RBlock1, RBlock2])) end
		},
		{"Simple encoding of SR RTCP data stream",
			fun() ->
					{Rblocks, _} = rtcp:decode_rblocks(RBlocksBin, 2),
					?assertEqual(
						SRBin,
						rtcp:encode_sr(4096, NTP, 4098, 65535, 65536, Rblocks)
					) end
		},
		{"Simple decoding SR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok,SR}, rtcp:decode(SRBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(SRBin, rtcp:encode(SR)) end
		}
	].
