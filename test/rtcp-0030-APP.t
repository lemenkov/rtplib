#!/usr/bin/escript
%% -*- erlang -*-
%%! -pz ../rtplib ebin

% Test APP codec.

-include_lib("rtplib/src/rtcp.hrl").

main(_) ->
	etap:plan(3),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_app(5, 1024, "STR1", <<"Hello! This is a string.">>), "Simple encoding of APP RTCP data stream"),
	Data = rtcp:encode_app(5, 1024, "STR1", <<"Hello! This is a string.">>),

	etap:fun_is(fun ([#app{subtype = Subtype, ssrc = SSRC, name = Name, data = Data}]) when is_binary(Data), is_binary(Name)  -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding APP RTCP data stream and returning a list with only member - record"),
	[Rtcp] = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

