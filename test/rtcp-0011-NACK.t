#!/usr/bin/escript
%% -*- erlang -*-

% Test NACK codec.

-include_lib("rtplib/src/rtcp.hrl").

main(_) ->
	etap:plan(3),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_nack(1024, 2049, 4097), "Simple encoding of NACK RTCP data stream"),
	Data = rtcp:encode_nack(1024, 2049, 4097),

	etap:fun_is(fun ({ok, [#nack{ssrc=SSRC, fsn=FSN, blp=BLP}]}) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding NACK RTCP data stream and returning a list with only member - record"),
	{ok, [Rtcp]} = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

