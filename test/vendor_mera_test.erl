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

-module(vendor_mera_test).

-include("rtcp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_mera_test_() ->
	% Taken from MERA MVTS3G v.4.4.0-20

	% In fact this is a broken RTCP  compound of two packets - SR and SDES
	RtcpBroken = <<128,200,0,12,112,132,120,79,211,119,45,213,142,243,79,114,124,169,162,96,0,0,0,0,0,0,0,0,
	129,202,0,7,112,132,120,79,1,20,53,55,55,53,48,64,49,48,46,49,49,49,46,49,49,49,46,49,48,48>>,

	% Proper RTCP packet would look like this - notice 4th byte is 6 instead of 12 and two trailing zeroes
	RtcpProper = <<128,200,0,6,112,132,120,79,211,119,45,213,142,243,79,114,124,169,162,96,0,0,0,0,0,0,0,0,
	129,202,0,7,112,132,120,79,1,20,53,55,55,53,48,64,49,48,46,49,49,49,46,49,49,49,46,49,48,48,0,0>>,

	Sr = #sr{
		ssrc = 1887729743,
		ntp = 15237698259480956786,
		timestamp = 2091491936,
		packets = 0,
		octets = 0,
		rblocks = []
	},
	Sdes = #sdes{list=[
			[
				{ssrc,1887729743},
				{cname,"57750@10.111.111.100"},
				{eof,true}
			]
		]
	},

	[
		{"Check that we won't fail on parsing broken RTCP",
%			fun() -> ?assertEqual({ok, #rtcp{payloads = [Sr], encrypted = RtcpBroken}},rtcp:decode(RtcpBroken)) end
			fun() -> ?assertEqual({ok, #rtcp{payloads = [Sr, Sdes]}},rtcp:decode(RtcpBroken)) end
		},
		{"Check that we can parse fixed RTCP",
			fun() -> ?assertEqual({ok, #rtcp{payloads = [Sr, Sdes]}}, rtcp:decode(RtcpProper)) end
		},
		{"Check that we can produce fixed bitstream",
			fun() -> ?assertEqual(RtcpProper, rtcp:encode(#rtcp{payloads = [Sr, Sdes]})) end
		}
	].
