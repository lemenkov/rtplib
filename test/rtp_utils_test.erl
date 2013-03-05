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

-module(rtp_utils_test).

-include("rtcp.hrl").
-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

rtp_utils_pp_test_() ->
	Rr = #rr{ssrc = 3300681489},
	Sdes = #sdes{list=[[{ssrc,3300681489},{cname,"sa@localhost.localdomain"},{eof,true}]]},
	Rtcp = #rtcp{
		payloads = [Rr, Sdes]
	},
	RtcpPP = "{\"type\":\"rr\",\"ssrc\":3300681489,\"rblocks\":[{}]}{\"type\":\"sdes\",\"list\":[{\"ssrc\":3300681489,\"cname\":\"sa@localhost.localdomain\",\"eof\":true}]}",
	[
		{"Simple pretty-printing of a simple RTCP packet",
			fun() -> ?assertEqual(RtcpPP, rtp_utils:pp(Rtcp)) end
		}
	].

rtp_utils_take_test_() ->
	Rr = #rr{ssrc = 3300681489},
	Sdes = #sdes{list=[[{ssrc,3300681489},{cname,"sa@localhost.localdomain"},{eof,true}]]},
	[
		{"Take Rr",
			fun() -> ?assertEqual(Rr, rtp_utils:take([Sdes, Sdes, Rr, Sdes], rr)) end
		},
		{"Take Sdes",
			fun() -> ?assertEqual(Sdes, rtp_utils:take([Sdes, Sdes, Rr, Sdes], sdes)) end
		},
		{"Take nothing",
			fun() -> ?assertEqual(false, rtp_utils:take([Sdes, Sdes, Rr, Sdes], sr)) end
		}
	].

rtp_utils_to_proplist_test_() ->
	RBlock1 = #rblock{ssrc=1024, fraction=2, lost=1026, last_seq=1027, jitter=1028, lsr=1029, dlsr=1030},
	RBlock2 = #rblock{ssrc=100024, fraction=2, lost=100026, last_seq=100027, jitter=100028, lsr=100029, dlsr=100030},

	Sr = #sr{
		ssrc=4096,
		ntp=15154578768523253214,
		timestamp=4098,
		packets=65535,
		octets=65536,
		rblocks=[RBlock1, RBlock2]
	},

	Rr =  #rr{ssrc=4096, rblocks=[RBlock1, RBlock2]},

	RrPl = [{rr,[{ssrc,4096},{rblocks,[{rblock,[{ssrc,1024},
                         {fraction,2},
                         {lost,1026},
                         {last_seq,1027},
                         {jitter,1028},
                         {lsr,1029},
                         {dlsr,1030}]},
                {rblock,[{ssrc,100024},
                         {fraction,2},
                         {lost,100026},
                         {last_seq,100027},
                         {jitter,100028},
                         {lsr,100029},
                         {dlsr,100030}]}]}]}],

	SrPl = [{sr,[{ssrc,4096},
	      {ntp,15154578768523253214},
	      {timestamp,4098},
	      {packets,65535},
	      {octets,65536},
	      {rblocks,[{rblock,[{ssrc,1024},
				 {fraction,2},
				 {lost,1026},
				 {last_seq,1027},
				 {jitter,1028},
				 {lsr,1029},
				 {dlsr,1030}]},
			{rblock,[{ssrc,100024},
				 {fraction,2},
				 {lost,100026},
				 {last_seq,100027},
				 {jitter,100028},
				 {lsr,100029},
				 {dlsr,100030}]}]}]}],

	[
		{"Transform Rr to proplist",
			fun() -> ?assertEqual(RrPl, rtp_utils:to_proplist(Rr)) end
		},
		{"Transform Sr to proplist",
			fun() -> ?assertEqual(SrPl, rtp_utils:to_proplist(Sr)) end
		}
	].
