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

-module(vendor_unknown_test).

-include("rtcp.hrl").
-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

vendor_unknown_test_() ->
	% Quite complex RTCP packet with coupled RR, SDES and BYE
	RrSdesBye1Bin = <<128,201,0,1,15,159,200,54,129,202,0,7,15,159,200,
		54,1,20,48,46,48,46,48,64,49,57,50,46,49,54,56,46,49,48,48,46,
		53,52,0,0,129,203,0,1,15,159,200,54>>,

	Rr1 = #rr{ssrc=262129718,rblocks=[],ijs=[]},
	Sdes1 = #sdes{list=[[{ssrc,262129718},{cname,"0.0.0@192.168.100.54"},{eof,true}]]},
	Bye1 = #bye{message=[],ssrc=[262129718]},

	RrSdesBye1 = #rtcp{payloads = [Rr1, Sdes1, Bye1]},


	% Another quite complex RTCP packet with coupled SR, SDES and BYE
	% SDES packet contains 'priv' extension
	SrSdesBye2Bin = <<128,200,0,6,55,82,152,102,209,215,221,218,198,102,
		102,102,1,129,108,232,0,2,81,32,1,142,129,128,129,202,0,30,55,
		82,152,102,1,61,65,52,50,52,67,67,55,51,49,50,53,65,52,50,48,68,
		66,68,53,66,67,70,49,65,65,66,69,68,57,67,67,70,64,117,110,105,
		113,117,101,46,122,50,51,69,65,53,49,55,66,68,51,48,51,52,66,53,
		66,46,111,114,103,8,49,16,120,45,114,116,112,45,115,101,115,115,
		105,111,110,45,105,100,51,48,70,50,69,51,55,49,51,68,52,69,52,
		54,57,66,65,51,50,66,69,67,52,65,48,53,69,54,50,57,68,66,0,0,
		129,203,0,1,55,82,152,102>>,

	Sr2 = #sr{ssrc=928159846, ntp=15120798205620938342, timestamp=25259240, packets=151840, octets=26116480, rblocks=[]},
	Sdes2 = #sdes{list=[[{ssrc,928159846},{cname,"A424CC73125A420DBD5BCF1AABED9CCF@unique.z23EA517BD3034B5B.org"},{priv,{"x-rtp-session-id",<<"30F2E3713D4E469BA32BEC4A05E629DB">>}},{eof,true}]]},
	Bye2 = #bye{message=[], ssrc=[928159846]},

	SrSdesBye2 = #rtcp{payloads = [Sr2, Sdes2, Bye2]},

	[
		{"Encode the entire RR+SDES+BYE packet",
			fun() -> ?assertEqual({ok, RrSdesBye1}, rtcp:decode(RrSdesBye1Bin)) end
		},
		{"Encode the entire SR+SDES+BYE packet",
			fun() -> ?assertEqual({ok, SrSdesBye2}, rtcp:decode(SrSdesBye2Bin)) end
		},
		{"Check what we could reproduce previous packet from RR+SDES+BYE",
			fun() -> ?assertEqual(RrSdesBye1Bin, rtcp:encode(RrSdesBye1)) end
		},
		{"Check what we could reproduce previous packet from SR+SDES+BYE",
			fun() -> ?assertEqual(SrSdesBye2Bin, rtcp:encode(SrSdesBye2)) end
		}
	].
