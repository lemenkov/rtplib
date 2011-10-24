-module(rtcp_nack_test).

-include_lib("rtplib/include/rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_NACK_test_() ->
	[
		{"Simple encoding of NACK RTCP data stream",
			fun() -> ?assertEqual(<<128,193,0,2,0,0,4,0,8,1,16,1>>, rtcp:encode_nack(1024, 2049, 4097)) end
		},
		{"Simple decoding NACK RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#nack{ssrc=1024, fsn=2049, blp=4097}]}, rtcp:decode(<<128,193,0,2,0,0,4,0,8,1,16,1>>)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<128,193,0,2,0,0,4,0,8,1,16,1>>, rtcp:encode(#nack{ssrc=1024, fsn=2049, blp=4097})) end
		}
	].
