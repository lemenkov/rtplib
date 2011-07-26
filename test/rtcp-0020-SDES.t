#!/usr/bin/escript
%% -*- erlang -*-

% Test SDES codec.

-include_lib("rtplib/include/rtcp.hrl").

main(_) ->
	etap:plan(3),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_sdes([[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]), "Simple encoding of SDES RTCP data stream"),
	Data = rtcp:encode_sdes([[{ssrc, 1024}, {cname,"hello 1"}, {name, "hello 2"}, {eof, true}]]),

	etap:fun_is(fun ({ok, [#sdes{list=SdesItemsList}]}) when is_list(SdesItemsList) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding SDES RTCP data stream and returning a list with only member - record"),
	{ok, [Rtcp]} = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

