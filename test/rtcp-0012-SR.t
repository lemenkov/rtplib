#!/usr/bin/escript
%% -*- erlang -*-

% Test SR codec.

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(4),

	% First, we should prepare several RBlocks
	RBlocksData = list_to_binary([rtcp:encode_rblock(1024, 2, 1026, 1027, 1028, 1029, 1030), rtcp:encode_rblock(100024, 2, 100026, 100027, 100028, 100029, 100030)]),
	RBlocks = rtcp:decode_rblocks(RBlocksData, 2),
	etap:is(RBlocksData, rtcp:encode_rblocks(RBlocks), "Check correct Report Blocks processing"),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_sr(4096, now(), 4098, 65535, 65536, RBlocks), "Simple encoding of SR RTCP data stream"),
	Data = rtcp:encode_sr(4096, now(), 4098, 65535, 65536, RBlocks),

	etap:fun_is(fun ({ok, [#sr{ssrc=SSRC, ntp=Ntp, timestamp=TimeStamp, packets=Packets, octets=Octets, rblocks=RBlocks}]}) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding SR RTCP data stream and returning a list with only member - record"),
	{ok, [Rtcp]} = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

