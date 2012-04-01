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

-module(rtp_rtcp_muxed_test).

-include("rtcp.hrl").
-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtp_rtcp_muxed_test_() ->
	AppBin = <<133,204,0,8,0,0,4,0,83,84,82,49,72,101,108,108,111,33,32,84,104,105,115,32,105,115,32,97,32,115,116,114,105,110,103,46>>,
	ByeBin = <<161,203,0,3,0,0,4,0,6,67,97,110,99,101,108,0>>,
	RBlock1Bin = <<0,0,4,0,2,0,4,2,0,0,4,3,0,0,4,4,0,0,4,5,0,0,4,6>>,
	RBlock2Bin = <<0,1,134,184,2,1,134,186,0,1,134,187,0,1,134,188,0,1,134,189,0,1,134,190>>,
	RBlocksBin = <<RBlock1Bin/binary, RBlock2Bin/binary>>,
	RRBin = <<130,201,0,13,0,0,16,0,RBlocksBin/binary>>,
	RBlock1 = #rblock{ssrc=1024, fraction=2, lost=1026, last_seq=1027, jitter=1028, lsr=1029, dlsr=1030},
	RBlock2 = #rblock{ssrc=100024, fraction=2, lost=100026, last_seq=100027, jitter=100028, lsr=100029, dlsr=100030},
	RR = #rr{ssrc=4096, rblocks=[RBlock1, RBlock2]},
	SdesBin = <<129,202,0,6,0,0,4,0,1,7,104,101,108,108,111,32,49,2,7,104,101,108,108,111,32,50,0,0>>,
	RBlock1Bin = <<0,0,4,0,2,0,4,2,0,0,4,3,0,0,4,4,0,0,4,5,0,0,4,6>>,
	RBlock2Bin = <<0,1,134,184,2,1,134,186,0,1,134,187,0,1,134,188,0,1,134,189,0,1,134,190>>,
	RBlocksBin = <<RBlock1Bin/binary, RBlock2Bin/binary>>,

	RBlock1 = #rblock{ssrc=1024, fraction=2, lost=1026, last_seq=1027, jitter=1028, lsr=1029, dlsr=1030},
	RBlock2 = #rblock{ssrc=100024, fraction=2, lost=100026, last_seq=100027, jitter=100028, lsr=100029, dlsr=100030},

	% random NTP timestamp with rtp_utils:now2ntp(now())
	<<NTP:64>> = <<210,79,225,24,250,129,85,222>>,
	% valid SR packet
	SRBin = <<130,200,0,18,0,0,16,0,210,79,225,24,250,129,85,222,0,0,16,2,0,
		0,255,255,0,1,0,0, RBlocksBin/binary>>,

	SR = #sr{
		ssrc=4096,
		ntp=15154578768523253214,
		timestamp=4098,
		packets=65535,
		octets=65536,
		rblocks=[RBlock1, RBlock2]
	},
	XrBlock1Bin = <<254,255,0,5,116,101,115,116,49,32,120,114,98,108,111,99,107,32,100,97,116,97,32,49>>,
	XrBlock2Bin = <<127,128,0,5,116,101,115,116,50,32,120,114,98,108,111,99, 107,32,100,97,116,97,32,50>>,
	XrBlocksBin = <<XrBlock1Bin/binary, XrBlock2Bin/binary>>,

	XrBlock1 = #xrblock{type = 254, ts = 255, data = <<"test1 xrblock data 1">>},
	XrBlock2 = #xrblock{type = 127, ts = 128, data = <<"test2 xrblock data 2">>},

	XrBin = <<128,207,0,13,0,0,4,0,254,255,0,5,116,101,115,116,49,32,120,114,
		98,108,111,99,107,32,100,97,116,97,32,49,127,128,0,5,116,101,115,
		116,50,32,120,114,98,108,111,99,107,32,100,97,116,97,32,50>>,

	[
		{"Simple decoding APP RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>}]},
						rtp:decode(AppBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(AppBin, rtp:encode(#app{subtype = 5, ssrc = 1024, name = <<"STR1">>, data = <<"Hello! This is a string.">>})) end
		},
		{"Simple decoding BYE RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#bye{message = "Cancel", ssrc = [1024]}]}, rtp:decode(ByeBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(ByeBin, rtp:encode(#bye{message = "Cancel", ssrc = [1024]})) end
		},
		{"Simple decoding FIR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#fir{ssrc = 1024}]}, rtp:decode(<<128,192,0,1,0,0,4,0>>)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<128,192,0,1,0,0,4,0>>, rtp:encode(#fir{ssrc = 1024})) end
		},
		{"Simple decoding NACK RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#nack{ssrc=1024, fsn=2049, blp=4097}]}, rtp:decode(<<128,193,0,2,0,0,4,0,8,1,16,1>>)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<128,193,0,2,0,0,4,0,8,1,16,1>>, rtp:encode(#nack{ssrc=1024, fsn=2049, blp=4097})) end
		},
		{"Simple decoding RR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [RR]}, rtp:decode(RRBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(RRBin, rtp:encode(RR)) end
		},
		{"Simple decoding SDES RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual(
						{ok, [#sdes{list=[[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]}]},
						rtp:decode(SdesBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(<<129,202,0,6,0,0,4,0,1,7,104,101,108,108,111,32,49,2,7,104,101,108,108,111,32,50,0,0>>,
						rtp:encode(#sdes{list=[[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]})
					) end
		},
		{"Simple decoding SR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok,[SR]}, rtp:decode(SRBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(SRBin, rtp:encode(SR)) end
		},
		{"Simple decoding XR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, [#xr{ssrc=1024, xrblocks=[XrBlock1, XrBlock2]}]}, rtp:decode(XrBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(XrBin, rtp:encode(#xr{ssrc=1024, xrblocks=[XrBlock1, XrBlock2]})) end
		}
	].
