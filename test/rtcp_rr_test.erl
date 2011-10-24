-module(rtcp_rr_test).

-include_lib("rtplib/include/rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_RR_test_() ->
	% First, we should prepare several RBlocks
	RBlock1Bin = <<0,0,4,0,2,0,4,2,0,0,4,3,0,0,4,4,0,0,4,5,0,0,4,6>>,
	RBlock2Bin = <<0,1,134,184,2,1,134,186,0,1,134,187,0,1,134,188,0,1,134,189,0,1,134,190>>,
	RBlocksBin = <<RBlock1Bin/binary, RBlock2Bin/binary>>,
	RRBin = <<130,201,0,13,0,0,16,0,RBlocksBin/binary>>,

	RBlock1 = #rblock{ssrc=1024, fraction=2, lost=1026, last_seq=1027, jitter=1028, lsr=1029, dlsr=1030},
	RBlock2 = #rblock{ssrc=100024, fraction=2, lost=100026, last_seq=100027, jitter=100028, lsr=100029, dlsr=100030},

	RR = #rr{ssrc=4096, rblocks=[RBlock1, RBlock2]},

	[
		{"Encode one RBlock",
			fun() -> ?assertEqual(RBlock1Bin, rtcp:encode_rblock(1024, 2, 1026, 1027, 1028, 1029, 1030)) end
		},
		{"Encode another one RBlock",
			fun() -> ?assertEqual(RBlock2Bin, rtcp:encode_rblock(100024, 2, 100026, 100027, 100028, 100029, 100030)) end
		},
		{"Decode both binary RBLocks",
			fun() -> ?assertEqual([RBlock1, RBlock2], rtcp:decode_rblocks(RBlocksBin, 2)) end
		},
		{"Check correct Report Blocks processing",
			fun() -> ?assertEqual(RBlocksBin, rtcp:encode_rblocks([RBlock1, RBlock2])) end
		},
		{"Simple encoding of RR RTCP data stream",
			fun() -> ?assertEqual(RRBin, rtcp:encode_rr(4096, [RBlock1, RBlock2])) end
		},
		{"Simple decoding RR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [RR]}, rtcp:decode(RRBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(RRBin, rtcp:encode(RR)) end
		}
	].
