-module(codec_ilbc_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_ilbc_test_() ->
	[
		{"Test decoding from iLBC(20) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F00.BIT20",
							"../test/samples/ilbc/F00.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F00.BIT30",
							"../test/samples/ilbc/F00.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
%		{"Test encoding from PCM to iLBC(20)",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/ilbc/F00.INP",
%							"../test/samples/ilbc/F00.BIT20",
%							320,
%							38,
%							"iLBC(20)",
%							{'ILBC',8000,1}
%						)
%					) end
%		},
		{"Test encoding from PCM to iLBC(30)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F00.INP",
							"../test/samples/ilbc/F00.BIT30",
							480,
							50,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(20) (1st set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F01.BIT20",
							"../test/samples/ilbc/F01.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (1st set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F01.BIT30",
							"../test/samples/ilbc/F01.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(20) (1st set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F01.INP",
							"../test/samples/ilbc/F01.BIT20",
							320,
							38,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(30) (1st set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F01.INP",
							"../test/samples/ilbc/F01.BIT30",
							480,
							50,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(20) (2nd set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F02.BIT20",
							"../test/samples/ilbc/F02.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (2nd set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F02.BIT30",
							"../test/samples/ilbc/F02.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
%		{"Test encoding from PCM to iLBC(20) (2nd set)",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/ilbc/F02.INP",
%							"../test/samples/ilbc/F02.BIT20",
%							320,
%							38,
%							"iLBC(20)",
%							{'ILBC',8000,1}
%						)
%					) end
%		},
%		{"Test encoding from PCM to iLBC(30) (2nd set)",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/ilbc/F02.INP",
%							"../test/samples/ilbc/F02.BIT30",
%							480,
%							50,
%							"iLBC(30)",
%							{'ILBC',8000,1}
%						)
%					) end
%		},
		{"Test decoding from iLBC(20) (3rd set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F03.BIT20",
							"../test/samples/ilbc/F03.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (3rd set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F03.BIT30",
							"../test/samples/ilbc/F03.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(20) (3rd set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F03.INP",
							"../test/samples/ilbc/F03.BIT20",
							320,
							38,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(30) (3rd set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F03.INP",
							"../test/samples/ilbc/F03.BIT30",
							480,
							50,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(20) (4th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F04.BIT20",
							"../test/samples/ilbc/F04.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (4th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F04.BIT30",
							"../test/samples/ilbc/F04.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(20) (4th set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F04.INP",
							"../test/samples/ilbc/F04.BIT20",
							320,
							38,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(30) (4th set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F04.INP",
							"../test/samples/ilbc/F04.BIT30",
							480,
							50,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(20) (5th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F05.BIT20",
							"../test/samples/ilbc/F05.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (5th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F05.BIT30",
							"../test/samples/ilbc/F05.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(20) (5th set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F05.INP",
							"../test/samples/ilbc/F05.BIT20",
							320,
							38,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test encoding from PCM to iLBC(30) (5th set)",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/ilbc/F05.INP",
							"../test/samples/ilbc/F05.BIT30",
							480,
							50,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(20) (6th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F06.BIT20",
							"../test/samples/ilbc/F06.OUT20",
							38,
							320,
							"iLBC(20)",
							{'ILBC',8000,1}
						)
					) end
		},
		{"Test decoding from iLBC(30) (6th set) to PCM",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_decode(
							"../test/samples/ilbc/F06.BIT30",
							"../test/samples/ilbc/F06.OUT30",
							50,
							480,
							"iLBC(30)",
							{'ILBC',8000,1}
						)
					) end
		}
%		{"Test encoding from PCM to iLBC(20) (6th set)",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/ilbc/F06.INP",
%							"../test/samples/ilbc/F06.BIT20",
%							320,
%							38,
%							"iLBC(20)",
%							{'ILBC',8000,1}
%						)
%					) end
%		},
%		{"Test encoding from PCM to iLBC(30) (6th set)",
%			fun() -> ?assertEqual(
%						true,
%						test_utils:codec_encode(
%							"../test/samples/ilbc/F06.INP",
%							"../test/samples/ilbc/F06.BIT30",
%							480,
%							50,
%							"iLBC(30)",
%							{'ILBC',8000,1}
%						)
%					) end
%		}
	].
