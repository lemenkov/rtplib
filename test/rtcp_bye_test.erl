-module(rtcp_bye_test).

-include_lib("rtplib/include/rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_BYE_test_() ->
	ByeBin = <<161,203,0,3,0,0,4,0,6,67,97,110,99,101,108,0>>,
	[
		{"Simple encoding of BYE RTCP data stream",
			fun() -> ?assertEqual(ByeBin, rtcp:encode_bye([1024], "Cancel")) end
		},
		{"Simple decoding BYE RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#bye{message = "Cancel", ssrc = [1024]}]}, rtcp:decode(ByeBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(ByeBin, rtcp:encode(#bye{message = "Cancel", ssrc = [1024]})) end
		}
	].

%% Taken from real RTCP capture from unidentified source
rtcp_BYE_padding_test_() ->
	% Padding at the end <<0,0,0,0>>
	ByeBin = <<129,203,0,5,128,171,245,31,15,68,105,115,99,111,110,110,101,99,116,32,67,97,108,108,0,0,0,0>>,
	ByeBinNoPadding = <<129,203,0,5,128,171,245,31,15,68,105,115,99,111,110,110,101,99,116,32,67,97,108,108>>,
	Bye = #bye{message="Disconnect Call",ssrc=[2158753055]},
	[
		{"Decode BYE RTCP with unnecessary padding",
			fun() -> ?assertEqual({ok, [Bye]}, rtcp:decode(ByeBin)) end
		},
		{"Encode BYE RTCP properly (w/o padding)",
			fun() -> ?assertEqual(ByeBinNoPadding, rtcp:encode(Bye)) end
		}
	].
