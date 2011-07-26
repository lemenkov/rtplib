#!/usr/bin/escript
%% -*- erlang -*-

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(5),

	% Padding at the end <<0,0,0,0>>
	BinRtcpBye1 = <<129,203,0,5,128,171,245,31,15,68,105,115,99,111,110,110, 101,99,116,32,67,97,108,108,0,0,0,0>>,
	{ok, [RtcpBye1]} = rtcp:decode(BinRtcpBye1),
	BinRtcpBye1_new = rtcp:encode(RtcpBye1),
	etap:is(BinRtcpBye1, <<BinRtcpBye1_new/binary, 0, 0, 0, 0>>, "Check that we can reproduce original RTCP bye w/o padding"),

	% This one was damaged - first octet is 1 instead of 129
	BinRtcpSdes1 = <<1,202,0,17,0,0,123,112,1,61,70,68,66,66,48,66,52,67,
			56,67,65,50,52,57,51,53, 65,50,50,65,69,48,68,68,57,52,
			51,57,57,53,52,50,64,117,110,105,113,117,101,46,122,57,
			67,48,65,70,53,56,51,67,54,51,56,52,52,50,56,46,111,114,
			103,0>>,
	FixRtcpSdes1 = <<129,202,0,17,0,0,123,112,1,61,70,68,66,66,48,66,52,67,
			56,67,65,50,52,57,51,53, 65,50,50,65,69,48,68,68,57,52,
			51,57,57,53,52,50,64,117,110,105,113,117,101,46,122,57,
			67,48,65,70,53,56,51,67,54,51,56,52,52,50,56,46,111,114,
			103,0>>,
	{ok, [RtcpSdes1]} = rtcp:decode(BinRtcpSdes1),
	etap:is(FixRtcpSdes1, rtcp:encode(RtcpSdes1), "Check that we can reproduce fixed RTCP sdes"),

	% Another broken SDES
	BinRtcpSdes2 = <<1,202,0,2,0,0,120,143,0,0,0,0>>,
	{ok, [RtcpSdes2]} = rtcp:decode(BinRtcpSdes2),
	etap:is(<<129,202,0,2,0,0,120,143,0,0,0,0>>, rtcp:encode(RtcpSdes2), "Check that we can reproduce fixed RTCP sdes"),

	% Quite complex RTCP packet with coupled RR, SDES and BYE
	BinRtcpRrSdesBye3 = <<128,201,0,1,15,159,200,54,129,202,0,7,15,159,200,54,1,20,48,46,48,46,48,64,49,57,50,46,49,54,56,46,49,48,48,46,53,52,0,0,129,203,0,1,15,159,200,54>>,
	{ok, [RtcpRr3, RtcpSdes3, RtcpBye3]} = rtcp:decode(BinRtcpRrSdesBye3),
	BinRtcpRr3 = rtcp:encode(RtcpRr3),
	BinRtcpSdes3 = rtcp:encode(RtcpSdes3),
	BinRtcpBye3 = rtcp:encode(RtcpBye3),
	etap:is(BinRtcpRrSdesBye3, <<BinRtcpRr3/binary, BinRtcpSdes3/binary, BinRtcpBye3/binary>>, "Check what we could reproduce previous packet from RR+SDES+BYE"),

	% Another quite complex RTCP packet with coupled RR, SDES and BYE
	% SDES packet contains 'priv' extension
	BinRtcpRrSdesBye4 = <<128,200,0,6,55,82,152,102,209,215,221,218,198,102,102,102,1,129,108,232,0,2,81,32,1,142,129,128,129,202,0,30,55,82,152,102,1,61,65,52,50,52,67,67,55,51,49,50,53,65,52,50,48,68,66,68,53,66,67,70,49,65,65,66,69,68,57,67,67,70,64,117,110,105,113,117,101,46,122,50,51,69,65,53,49,55,66,68,51,48,51,52,66,53,66,46,111,114,103,8,49,16,120,45,114,116,112,45,115,101,115,115,105,111,110,45,105,100,51,48,70,50,69,51,55,49,51,68,52,69,52,54,57,66,65,51,50,66,69,67,52,65,48,53,69,54,50,57,68,66,0,0,129,203,0,1,55,82,152,102>>,
	{ok, [RtcpRr4, RtcpSdes4, RtcpBye4]} = rtcp:decode(BinRtcpRrSdesBye4),
	BinRtcpRr4 = rtcp:encode(RtcpRr4),
	BinRtcpSdes4 = rtcp:encode(RtcpSdes4),
	BinRtcpBye4 = rtcp:encode(RtcpBye4),
	etap:is(BinRtcpRrSdesBye4, <<BinRtcpRr4/binary, BinRtcpSdes4/binary, BinRtcpBye4/binary>>, "Check what we could reproduce previous packet from RR+SDES+BYE"),

	etap:end_tests().
