%%%---------------------------------------------------------------------
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

-module(rtcp_xr_test).

-include("rtcp.hrl").
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

	Xr = #rtcp{ payloads = [#xr{ssrc=1024, xrblocks=[XrBlock1, XrBlock2]}]},

	[
		{"Check correct eXtended Report Blocks processing",
			fun() -> ?assertEqual(XrBlocksBin, rtcp:encode_xrblocks([XrBlock1, XrBlock2])) end
		},
		{"Simple encoding of XR RTCP data stream",
			fun() -> ?assertEqual(XrBin, rtcp:encode_xr(1024, [XrBlock1, XrBlock2])) end
		},
		{"Simple decoding XR RTCP data stream and returning a list with only member - record",
			fun() -> ?assertEqual({ok, Xr}, rtcp:decode(XrBin)) end
		},
		{"Check that we can reproduce original data stream from record",
			fun() -> ?assertEqual(XrBin, rtcp:encode(Xr)) end
		}
	].
