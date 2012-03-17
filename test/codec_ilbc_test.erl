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
%							"iLBC(30)",
%							{'ILBC',8000,1}
%						)
%					) end
%		}
	].
