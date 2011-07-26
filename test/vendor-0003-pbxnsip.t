#!/usr/bin/escript
%% -*- erlang -*-

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(2),

	% There is a miscalculated size of the SR block this packet - 4th octet
	% must be 9 instead of 8 (as it was originally). Also there is a strange padding at the end.
	%                      -=8=-
	BinRtcpSr1 = <<129,200,0,9,17,195,69,247,209,206,159,196,201,251,230,131,59,71,192,80,0,0,0, 5,0,0,3,32,0,0,0,0,0,0,0,0,0,0,0,0>>,
	BinRtcpSdes1 = <<129,202,0,9,17,195,69,247,1,27,116,101,115,116,97,99,99,116,64,115,105,112,46,101,120,97,109,112,108,101,48,48,46,110,101,116,0,0,0,0>>,

	{ok, [RtcpSr1, RtcpSdes1]} = rtcp:decode(<<BinRtcpSr1/binary, BinRtcpSdes1/binary>>),

	etap:is(<<128,200,0,6,17,195,69,247,209,206,159,196,201,251,214, 168,59,71,192,80,0,0,0,5,0,0,3,32>>, rtcp:encode(RtcpSr1), "Check that we can produce fixed RTCP sr"),
	etap:is(BinRtcpSdes1, rtcp:encode(RtcpSdes1), "Check that we can reproduce original RTCP sdes"),

	etap:end_tests().
