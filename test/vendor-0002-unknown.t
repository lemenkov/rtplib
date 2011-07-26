#!/usr/bin/escript
%% -*- erlang -*-

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(3),

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

	etap:end_tests().
