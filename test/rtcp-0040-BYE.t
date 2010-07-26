#!/usr/bin/escript
%% -*- erlang -*-

% Test BYE codec.

-include_lib("rtplib/src/rtcp.hrl").

main(_) ->
	etap:plan(3),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_bye([1024], "Cancel"), "Simple encoding of BYE RTCP data stream"),
	Data = rtcp:encode_bye([1024], "Cancel"),

	etap:fun_is(fun ({ok, [#bye{message = Message, ssrc = SSRCs}]}) when is_list(SSRCs), is_list(Message) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding BYE RTCP data stream and returning a list with only member - record"),
	{ok, [Rtcp]} = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().
