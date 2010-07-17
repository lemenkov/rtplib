#!/usr/bin/escript
%% -*- erlang -*-

% Test FIR codec.

-include_lib("rtplib/src/rtcp.hrl").

main(_) ->
	etap:plan(3),

	etap:fun_is(fun(A) when is_binary(A) -> true; (_) -> false end, rtcp:encode_fir(1024), "Simple encoding of FIR RTCP data stream"),
	Data = rtcp:encode_fir(1024),

	etap:fun_is(fun ([#fir{ssrc = SSRC}]) -> true; (_) -> false end, rtcp:decode(Data), "Simple decoding FIR RTCP data stream and returning a list with only member - record"),
	[Rtcp] = rtcp:decode(Data),

	etap:is(Data, rtcp:encode(Rtcp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

