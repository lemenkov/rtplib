-module(rtcp_xr_test).

-include_lib("rtplib/include/rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_XR_test_() ->
	% First, we should prepare several XRBlocks
	XrBlock1Bin = <<254,255,0,5,116,101,115,116,49,32,120,114,98,108,111,99,107,32,100,97,116,97,32,49>>,
	XrBlock2Bin = <<127,128,0,5,116,101,115,116,50,32,120,114,98,108,111,99, 107,32,100,97,116,97,32,50>>,
	XrBlocksBin = <<XrBlock1Bin/binary, XrBlock2Bin/binary>>,

	XrBlock1 = #xrblock{type = 254, ts = 255, data = <<"test1 xrblock data 1">>},
	XrBlock2 = #xrblock{type = 127, ts = 128, data = <<"test2 xrblock data 2">>},

	XrBin = <<128,207,0,13,0,0,4,0,254,255,0,5,116,101,115,116,49,32,120,114,
		98,108,111,99,107,32,100,97,116,97,32,49,127,128,0,5,116,101,115,
		116,50,32,120,114,98,108,111,99,107,32,100,97,116,97,32,50>>,

	[
		{"Check correct eXtended Report Blocks processing",
			fun() -> ?assertEqual(XrBlocksBin, rtcp:encode_xrblocks([XrBlock1, XrBlock2])) end
		},
		{"Simple encoding of XR RTCP data stream",
			fun() -> ?assertEqual(XrBin, rtcp:encode_xr(1024, [XrBlock1, XrBlock2])) end
		},
		{"Simple decoding XR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#xr{ssrc=1024, xrblocks=[XrBlock1, XrBlock2]}]}, rtcp:decode(XrBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(XrBin, rtcp:encode(#xr{ssrc=1024, xrblocks=[XrBlock1, XrBlock2]})) end
		}
	].
