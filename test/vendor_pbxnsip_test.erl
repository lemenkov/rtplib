-module(vendor_pbxnsip_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_pbxnsip_test_() ->

	% There is a miscalculated size of the SR block this packet - 4th octet
	% must be 9 instead of 8. Also there is a strange padding at the end.
	%                      -=9=-
	RtcpSrBin =
		<<129,200,0,8,17,195,69,247,209,206,159,196,201,251,230,131,59,
		71,192,80,0,0,0,5,0,0,3,32,0,0,0,0,0,0,0,0,0,0,0,0>>,
	RtcpSrBinCorrect =
		<<128,200,0,6,17,195,69,247,209,206,159,196,201,251,230,
		131,59,71,192,80,0,0,0,5,0,0,3,32>>,
	RtcpSdesBin = <<129,202,0,9,17,195,69,247,1,27,116,101,115,116,97,99,
		99,116,64,115,105,112,46,101,120,97,109,112,108,101,48,48,46,
		110,101,116,0,0,0,0>>,

	RtcpSr = #sr{
		ssrc=298010103,
		ntp=15118196666680469123,
		timestamp=994558032,
		packets=5,
		octets=800,
		rblocks=[]
	},
	RtcpSdes = #sdes{list=[[
				{ssrc,298010103},
				{cname,"testacct@sip.example00.net" ++ [0]},
				{eof,true}
			]]},

	[
		{"Check that we still can decode broken RTCP packet correctly",
			fun() -> ?assertEqual({ok, [RtcpSr, RtcpSdes]},rtcp:decode(<<RtcpSrBin/binary, RtcpSdesBin/binary>>)) end
		},
		{"Check that we can produce fixed RTCP SR",
			fun() -> ?assertEqual(RtcpSrBinCorrect, rtcp:encode(RtcpSr)) end
		},
		{"Check that we can reproduce original RTCP SDES",
			fun() -> ?assertEqual(RtcpSdesBin, rtcp:encode(RtcpSdes)) end
		}
	].
