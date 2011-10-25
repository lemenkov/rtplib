-module(rtcp_sdes_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_SDES_test_() ->
	SdesBin = <<129,202,0,6,0,0,4,0,1,7,104,101,108,108,111,32,49,2,7,104,101,108,108,111,32,50,0,0>>,
	[
		{"Simple encoding of SDES RTCP data stream",
			fun() -> ?assertEqual(SdesBin, rtcp:encode_sdes([[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]])) end
		},
		{"Simple decoding SDES RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual(
						{ok, [#sdes{list=[[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]}]},
						rtcp:decode(SdesBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<129,202,0,6,0,0,4,0,1,7,104,101,108,108,111,32,49,2,7,104,101,108,108,111,32,50,0,0>>,
						rtcp:encode(#sdes{list=[[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]})
					) end
		}
	].

% Both packets were extracted from real RTCP capture from unidentified source
rtcp_SDES_damaged_test_() ->
	% This one was damaged - first octet is 1 instead of 129
	Sdes1Bin = <<1,202,0,17,0,0,123,112,1,61,70,68,66,66,48,66,52,67,
			56,67,65,50,52,57,51,53, 65,50,50,65,69,48,68,68,57,52,
			51,57,57,53,52,50,64,117,110,105,113,117,101,46,122,57,
			67,48,65,70,53,56,51,67,54,51,56,52,52,50,56,46,111,114,
			103,0>>,
	Sdes1Fix = <<129,202,0,17,0,0,123,112,1,61,70,68,66,66,48,66,52,67,
			56,67,65,50,52,57,51,53, 65,50,50,65,69,48,68,68,57,52,
			51,57,57,53,52,50,64,117,110,105,113,117,101,46,122,57,
			67,48,65,70,53,56,51,67,54,51,56,52,52,50,56,46,111,114,
			103,0>>,
	Sdes1 = #sdes{list=[[{ssrc,31600},{cname,"FDBB0B4C8CA24935A22AE0DD94399542@unique.z9C0AF583C6384428.org"},{eof,true}]]},


	% Another broken SDES
	Sdes2Bin = <<1,202,0,2,0,0,120,143,0,0,0,0>>,
	Sdes2Fix = <<129,202,0,2,0,0,120,143,0,0,0,0>>,
	Sdes2 = #sdes{list=[[{ssrc,30863},{eof,true}]]},

	[
		{"Correctly decode first broken SDES",
			fun() -> ?assertEqual({ok, [Sdes1]}, rtcp:decode(Sdes1Bin)) end
		},
		{"Correctly decode second broken SDES",
			fun() -> ?assertEqual({ok, [Sdes2]}, rtcp:decode(Sdes2Bin)) end
		},
		{"Correctly encode first SDES",
			fun() -> ?assertEqual(Sdes1Fix, rtcp:encode(Sdes1)) end
		},
		{"Correctly encode second SDES",
			fun() -> ?assertEqual(Sdes2Fix, rtcp:encode(Sdes2)) end
		}
	].
