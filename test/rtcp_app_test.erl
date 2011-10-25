-module(rtcp_app_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtcp_APP_test_() ->
	AppBin = <<133,204,0,8,0,0,4,0,83,84,82,49,72,101,108,108,111,33,32,84,104,105,115,32,105,115,32,97,32,115,116,114,105,110,103,46>>,
	[
		{"Simple encoding of APP RTCP data stream",
			fun() -> ?assertEqual(AppBin, rtcp:encode_app(5, 1024, "STR1", <<"Hello! This is a string.">>)) end
		},
		{"Simple decoding APP RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>}]},
						rtcp:decode(AppBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(AppBin, rtcp:encode(#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>})) end
		}
	].
