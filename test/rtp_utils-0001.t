#!/usr/bin/escript
%% -*- erlang -*-

% Test now2ntp and ntp2now functions.

main(_) ->
	etap:plan(1),

	{MegaSecs, Secs, MicroSecs} = now(),
	NTP = rtp_utils:now2ntp({MegaSecs, Secs, MicroSecs}),

	etap:is ({MegaSecs, Secs, MicroSecs}, rtp_utils:ntp2now(NTP), "Check that we can reproduce original timestamp"),

	etap:end_tests().
