-module(rtcp_fir_test).

-include_lib("rtplib/include/rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_FIR_test_() ->
	[
		{"Simple encoding of FIR RTCP data stream",
			fun() -> ?assertEqual(<<128,192,0,1,0,0,4,0>>, rtcp:encode_fir(1024)) end
		},
		{"Simple decoding FIR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#fir{ssrc = 1024}]}, rtcp:decode(<<128,192,0,1,0,0,4,0>>)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<128,192,0,1,0,0,4,0>>, rtcp:encode(#fir{ssrc = 1024})) end
		}
	].
