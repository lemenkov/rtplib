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

-module(stun_test).

-include("stun.hrl").
-include_lib("eunit/include/eunit.hrl").

stun_test_() ->
	% samples taken from http://thread.gmane.org/gmane.comp.voip.pjsip/s9522
	StunBindReqBin = <<0,1,0,16,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,128,34,0,12,112,106,110,97,116,104,45,49,46,52,0,0>>,
	StunBindReq = #stun{
		class = request,
		method = binding,
		transactionid = 45554200240623869818762035456,
		attrs = [{'SOFTWARE',<<"pjnath-1.4">>}]
	},
	StunBindReqBinFixed = <<0,1,0,16,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,128,34,0,10,112,106,110,97,116,104,45,49,46,52,0,0>>,

	StunBindRespBin = <<1,1,0,68,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,0,1,0,8,0,1,224,252,88,198,53,113,0,4,0,8,0,1,13,150,208,109,222,137,0,5,0,8,0,1,13,151,208,109,222,148,128,32,0,8,0,1,193,238,121,212,145,51,128,34,0,16,86,111,118,105,100,97,46,111,114,103,32,48,46,57,54,0>>,
	StunBindResp = #stun{
		class = success,
		method = binding,
		transactionid = 45554200240623869818762035456,
		attrs = [
			{'MAPPED-ADDRESS',{{88,198,53,113},57596}},
			{'SOURCE-ADDRESS',{{208,109,222,137},3478}},
			{'CHANGED-ADDRESS',{{208,109,222,148},3479}},
			{'X-VOVIDA-XOR-MAPPED-ADDRESS',{{88,198,53,113},57596}},
			{'SOFTWARE',<<"Vovida.org 0.96">>}
		]
	},
	StunBindRespBinFixed = <<1,1,0,68,33,18,164,66,147,49,141,31,86,17,126,65,130,38,1,0,0,1,0,8,0,1,224,252,88,198,53,113,0,4,0,8,0,1,13,150,208,109,222,137,0,5,0,8,0,1,13,151,208,109,222,148,128,32,0,8,0,1,193,238,121,212,145,51,128,34,0,15,86,111,118,105,100,97,46,111,114,103,32,48,46,57,54,0>>,

	[
		{"Simple decoding of STUN Binding Request",
			fun() -> ?assertEqual({ok, StunBindReq}, stun:decode(StunBindReqBin)) end
		},
		{"Simple encoding of STUN Binding Request (with fixed length)",
			fun() -> ?assertEqual(StunBindReqBinFixed, stun:encode(StunBindReq)) end
		},
		{"Simple decoding of STUN Binding Responce",
			fun() -> ?assertEqual({ok, StunBindResp}, stun:decode(StunBindRespBin)) end
		},
		{"Simple encoding of STUN Binding Responce (with fixed length)",
			fun() -> ?assertEqual(StunBindRespBinFixed, stun:encode(StunBindResp)) end
		}
	].

stun_rfc5769_test_() ->
	ReqBin = <<16#00,16#01,16#00,16#58,16#21,16#12,16#a4,16#42,16#b7,16#e7,
		16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,16#ae,
		16#80,16#22,16#00,16#10,16#53,16#54,16#55,16#4e,16#20,16#74,
		16#65,16#73,16#74,16#20,16#63,16#6c,16#69,16#65,16#6e,16#74,
		16#00,16#24,16#00,16#04,16#6e,16#00,16#01,16#ff,16#80,16#29,
		16#00,16#08,16#93,16#2f,16#f9,16#b1,16#51,16#26,16#3b,16#36,
		16#00,16#06,16#00,16#09,16#65,16#76,16#74,16#6a,16#3a,16#68,
		16#36,16#76,16#59,16#20,16#20,16#20,16#00,16#08,16#00,16#14,
		16#9a,16#ea,16#a7,16#0c,16#bf,16#d8,16#cb,16#56,16#78,16#1e,
		16#f2,16#b5,16#b2,16#d3,16#f2,16#49,16#c1,16#b5,16#71,16#a2,
		16#80,16#28,16#00,16#04,16#e5,16#7a,16#3b,16#cf>>,

	ReqBinFixed = <<16#00,16#01,16#00,16#58,16#21,16#12,16#a4,16#42,16#b7,16#e7,
		16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,16#ae,
		16#80,16#22,16#00,16#10,16#53,16#54,16#55,16#4e,16#20,16#74,
		16#65,16#73,16#74,16#20,16#63,16#6c,16#69,16#65,16#6e,16#74,
		16#00,16#24,16#00,16#04,16#6e,16#00,16#01,16#ff,16#80,16#29,
		16#00,16#08,16#93,16#2f,16#f9,16#b1,16#51,16#26,16#3b,16#36,
		16#00,16#06,16#00,16#09,16#65,16#76,16#74,16#6a,16#3a,16#68,
		16#36,16#76,16#59,16#00,16#00,16#00,16#00,16#08,16#00,16#14,
		16#9a,16#ea,16#a7,16#0c,16#bf,16#d8,16#cb,16#56,16#78,16#1e,
		16#f2,16#b5,16#b2,16#d3,16#f2,16#49,16#c1,16#b5,16#71,16#a2,
		16#80,16#28,16#00,16#04,16#47,16#d9,16#7d,16#e0>>,

	Req = #stun{
		class = request,
		method = binding,
		transactionid = 56915807328848210473588875182,
		fingerprint = true,
		attrs = [
			{'SOFTWARE',<<"STUN test client">>},
			{'PRIORITY',<<110,0,1,255>>},
			{'ICE-CONTROLLED',<<147,47,249,177,81,38,59,54>>},
			{'USERNAME',<<"evtj:h6vY">>}, % 16#20,16#20,16#20 <- padding which is lost
			{'MESSAGE-INTEGRITY',<<154,234,167,12,191,216,203,86,120,30,242,181,178,211,242,73,193,181,113,162>>}
		]
	},

	RespIPv4Bin = <<16#01,16#01,16#00,16#3c,16#21,16#12,16#a4,16#42,16#b7,16#e7,
		16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,16#ae,
		16#80,16#22,16#00,16#0b,16#74,16#65,16#73,16#74,16#20,16#76,
		16#65,16#63,16#74,16#6f,16#72,16#20,16#00,16#20,16#00,16#08,
		16#00,16#01,16#a1,16#47,16#e1,16#12,16#a6,16#43,16#00,16#08,
		16#00,16#14,16#2b,16#91,16#f5,16#99,16#fd,16#9e,16#90,16#c3,
		16#8c,16#74,16#89,16#f9,16#2a,16#f9,16#ba,16#53,16#f0,16#6b,
		16#e7,16#d7,16#80,16#28,16#00,16#04,16#c0,16#7d,16#4c,16#96>>,

	RespIPv4BinFixed = <<16#01,16#01,16#00,16#3c,16#21,16#12,16#a4,16#42,16#b7,16#e7,
		16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,16#ae,
		16#80,16#22,16#00,16#0b,16#74,16#65,16#73,16#74,16#20,16#76,
		16#65,16#63,16#74,16#6f,16#72,16#00,16#00,16#20,16#00,16#08,
		16#00,16#01,16#a1,16#47,16#e1,16#12,16#a6,16#43,16#00,16#08,
		16#00,16#14,16#2b,16#91,16#f5,16#99,16#fd,16#9e,16#90,16#c3,
		16#8c,16#74,16#89,16#f9,16#2a,16#f9,16#ba,16#53,16#f0,16#6b,
		16#e7,16#d7,16#80,16#28,16#00,16#04,16#d4,16#0d,16#a7,16#66>>,

	RespIPv4 = #stun{
		class = success,
		method = binding,
		transactionid = 56915807328848210473588875182,
		fingerprint = true,
		attrs = [
			{'SOFTWARE',<<"test vector">>}, % 16#20 <- padding which is lost
			{'XOR-MAPPED-ADDRESS',{{192,0,2,1},32853}},
			{'MESSAGE-INTEGRITY',<<43,145,245,153,253,158,144,195,140,116,137,249,42,249,186,83,240,107,231,215>>}
		]
	},

	RespIPv6Bin = <<16#01,16#01,16#00,16#48,16#21,16#12,16#a4,16#42,16#b7,
		16#e7,16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,
		16#ae,16#80,16#22,16#00,16#0b,16#74,16#65,16#73,16#74,16#20,
		16#76,16#65,16#63,16#74,16#6f,16#72,16#20,16#00,16#20,16#00,
		16#14,16#00,16#02,16#a1,16#47,16#01,16#13,16#a9,16#fa,16#a5,
		16#d3,16#f1,16#79,16#bc,16#25,16#f4,16#b5,16#be,16#d2,16#b9,
		16#d9,16#00,16#08,16#00,16#14,16#a3,16#82,16#95,16#4e,16#4b,
		16#e6,16#7b,16#f1,16#17,16#84,16#c9,16#7c,16#82,16#92,16#c2,
		16#75,16#bf,16#e3,16#ed,16#41,16#80,16#28,16#00,16#04,16#c8,
		16#fb,16#0b,16#4c>>,
	RespIPv6BinFixed = <<16#01,16#01,16#00,16#48,16#21,16#12,16#a4,16#42,16#b7,
		16#e7,16#a7,16#01,16#bc,16#34,16#d6,16#86,16#fa,16#87,16#df,
		16#ae,16#80,16#22,16#00,16#0b,16#74,16#65,16#73,16#74,16#20,
		16#76,16#65,16#63,16#74,16#6f,16#72,16#00,16#00,16#20,16#00,
		16#14,16#00,16#02,16#a1,16#47,16#01,16#13,16#a9,16#fa,16#a5,
		16#d3,16#f1,16#79,16#bc,16#25,16#f4,16#b5,16#be,16#d2,16#b9,
		16#d9,16#00,16#08,16#00,16#14,16#a3,16#82,16#95,16#4e,16#4b,
		16#e6,16#7b,16#f1,16#17,16#84,16#c9,16#7c,16#82,16#92,16#c2,
		16#75,16#bf,16#e3,16#ed,16#41,16#80,16#28,16#00,16#04,16#2d,
		16#5f,16#83,16#d9>>,

	RespIPv6 = #stun{
		class = success,
		method = binding,
		transactionid = 56915807328848210473588875182,
		fingerprint = true,
		attrs = [
			{'SOFTWARE',<<"test vector">>}, % 16#20 <- padding which is lost
			{'XOR-MAPPED-ADDRESS',{{8193,3512,4660,22136,17,8755,17493,26231},32853}},
			{'MESSAGE-INTEGRITY',<<163,130,149,78,75,230,123,241,23,132,201,124,130,146,194,117,191,227,237,65>>}
		]
	},

	ReqAuthBin = <<16#00,16#01,16#00,16#60,16#21,16#12,16#a4,16#42,16#78,
		16#ad,16#34,16#33,16#c6,16#ad,16#72,16#c0,16#29,16#da,16#41,
		16#2e,16#00,16#06,16#00,16#12,16#e3,16#83,16#9e,16#e3,16#83,
		16#88,16#e3,16#83,16#aa,16#e3,16#83,16#83,16#e3,16#82,16#af,
		16#e3,16#82,16#b9,16#00,16#00,16#00,16#15,16#00,16#1c,16#66,
		16#2f,16#2f,16#34,16#39,16#39,16#6b,16#39,16#35,16#34,16#64,
		16#36,16#4f,16#4c,16#33,16#34,16#6f,16#4c,16#39,16#46,16#53,
		16#54,16#76,16#79,16#36,16#34,16#73,16#41,16#00,16#14,16#00,
		16#0b,16#65,16#78,16#61,16#6d,16#70,16#6c,16#65,16#2e,16#6f,
		16#72,16#67,16#00,16#00,16#08,16#00,16#14,16#f6,16#70,16#24,
		16#65,16#6d,16#d6,16#4a,16#3e,16#02,16#b8,16#e0,16#71,16#2e,
		16#85,16#c9,16#a2,16#8c,16#a8,16#96,16#66>>,

	ReqAuth = #stun{
		class = request,
		method = binding,
		transactionid = 37347591863512021035078271278,
		attrs = [
			{'USERNAME',<<227,131,158,227,131,136,227,131,170,227,131,131,227,130,175,227,130,185>>},
			{'NONCE',<<102,47,47,52,57,57,107,57,53,52,100,54,79,76,51,52,111,76,57,70,83,84,118,121,54,52,115,65>>},
			{'REALM',<<101,120,97,109,112,108,101,46,111,114,103>>},
			{'MESSAGE-INTEGRITY',<<246,112,36,101,109,214,74,62,2,184,224,113,46,133,201,162,140,168,150,102>>}
		]
	},
	[
		{"Simple decoding of STUN Request",
			fun() -> ?assertEqual({ok, Req}, stun:decode(ReqBin)) end
		},
		{"Simple encoding of STUN Request",
			fun() -> ?assertEqual(ReqBinFixed, stun:encode(Req)) end
		},
		{"Simple decoding of STUN IPv4 Response",
			fun() -> ?assertEqual({ok, RespIPv4}, stun:decode(RespIPv4Bin)) end
		},
		{"Simple encoding of STUN IPv4 Response",
			fun() -> ?assertEqual(RespIPv4BinFixed, stun:encode(RespIPv4)) end
		},
		{"Simple decoding of STUN IPv6 Response",
			fun() -> ?assertEqual({ok, RespIPv6}, stun:decode(RespIPv6Bin)) end
		},
		{"Simple encoding of STUN IPv6 Response",
			fun() -> ?assertEqual(RespIPv6BinFixed, stun:encode(RespIPv6)) end
		},
		{"Simple decoding of STUN Request with auth",
			fun() -> ?assertEqual({ok, ReqAuth}, stun:decode(ReqAuthBin)) end
		},
		{"Simple encoding of STUN Request with auth",
			fun() -> ?assertEqual(ReqAuthBin, stun:encode(ReqAuth)) end
		}
	].

%% Server names are taken from Ejabberd's tests
public_servers() ->
	[
		% address ------- UDP -- TCP -- TLS
		{"stun.ekiga.net",	3478, 3478, 5349},
%		{"stun.fwdnet.net",	3478, 3478, 5349},
		{"stun.ideasip.com",	3478, 3478, 5349},
%		{"stun01.sipphone.com",	3478, 3478, 5349},
		{"stun.softjoys.com",	3478, 3478, 5349},
		{"stun.voipbuster.com",	3478, 3478, 5349},
		{"stun.voxgratia.org",	3478, 3478, 5349},
%		{"stun.xten.com",	3478, 3478, 5349},
		{"stunserver.org",	3478, 3478, 5349},
		{"stun.sipgate.net",	10000, 10000, 5349},
		{"numb.viagenie.ca",	3478, 3478, 5349},
		{"stun.ipshka.com",	3478, 3478, 5349},

		{"stun.faktortel.com.au",	3478, 3478, 5349},
		{"provserver.televolution.net",	3478, 3478, 5349},
		%% it seems that they banned me
%		{"sip1.lakedestiny.cordiaip.com",	3478, 3478, 5349},
		{"stun1.voiceeclipse.net",	3478, 3478, 5349},
		{"stun.callwithus.com",	3478, 3478, 5349},
		{"stun.counterpath.net",	3478, 3478, 5349},
		{"stun.endigovoip.com",	3478, 3478, 5349},
		{"stun.internetcalls.com",	3478, 3478, 5349},
%		{"stun.ipns.com",	3478, 3478, 5349},
		{"stun.noc.ams-ix.net",	3478, 3478, 5349},
		{"stun.phonepower.com",	3478, 3478, 5349},
		{"stun.phoneserve.com",	3478, 3478, 5349},
		{"stun.rnktel.com",	3478, 3478, 5349},
%		{"stun.voxalot.com",	3478, 3478, 5349},
		{"stun.voip.aebc.com",	3478, 3478, 5349}
	].

mkstun() ->
	stun:encode(
		#stun{
			class = request,
			method = binding,
			transactionid = random:uniform(1 bsl 96),
			attrs = [{'SOFTWARE',<<"rtplib v. 0.5.12">>}]
		}
	).

test_recv(Addr, Port, gen_udp) ->
	{ok, Sock} = gen_udp:open(0, [binary, {active, false}]),
	gen_udp:send(Sock, Addr, Port, mkstun()),
	{ok, {_, _, Data}} = gen_udp:recv(Sock, 0, 1000),
	gen_udp:close(Sock),
	{ok, Ret} = stun:decode(Data),
	error_logger:info_msg("GOT STUN: ~p", [Ret]),
	Ret;
test_recv(Addr, Port, Mod) ->
	{ok, Sock} = Mod:connect(Addr, Port, [binary, {active, false}], 1000),
	Mod:send(Sock, mkstun()),
	{ok, Data} = Mod:recv(Sock, 0, 1000),
	Mod:close(Sock),
	{ok, Ret} = stun:decode(Data),
	error_logger:info_msg("GOT STUN: ~p", [Ret]),
	Ret.

stun_public_servers_test_DISABLE() ->
%	ssl:start(),
	lists:flatten(
	lists:map(fun({Addr, UdpP, TcpP, TlsP}) ->
				[
					{"Receive STUN resp via UDP from " ++ Addr,
						fun() ->
								?assertMatch(#stun{}, test_recv(Addr, UdpP, gen_udp))
						end
%					},
%					{"Receive STUN resp via TCP from " ++ Addr,
%						fun() ->
%								?assertMatch(#stun{}, test_recv(Addr, TcpP, gen_tcp))
%						end
%					},
%					{"Receive STUN resp via TLS from " ++ Addr,
%						fun() ->
%								?assertMatch(#stun{}, test_recv(Addr, TlsP, gen_udp))
%						end
					}
				]
		end, public_servers())).
