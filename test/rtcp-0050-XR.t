#!/usr/bin/escript
%% -*- erlang -*-

% Test XR codec.

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(4),

	% First, we should prepare several XRBlocks
	XRBlocksData = list_to_binary([rtcp:encode_xrblock(254, 255, <<"test1 xrblock data 1">>), rtcp:encode_xrblock(127, 128, <<"test2 xrblock data 2">>)]),
	XRBlocks = rtcp:decode_xrblocks(XRBlocksData, size(XRBlocksData)),
	etap:is(XRBlocksData, rtcp:encode_xrblocks(XRBlocks), "Check correct eXtended Report Blocks processing"),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_xr(1024, XRBlocks), "Simple encoding of XR RTCP data stream"),
	Data = rtcp:encode_xr(1024, XRBlocks),

	etap:fun_is(fun ({ok, [#xr{ssrc=SSRC, xrblocks=XRBlocks}]}) when is_list(XRBlocks) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding XR RTCP data stream and returning a list with only member - record"),
	{ok, [Rtcp]} = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

