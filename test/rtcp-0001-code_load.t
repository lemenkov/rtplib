#!/usr/bin/escript
%% -*- erlang -*-

% Test that we can load each module.

main(_) ->
    etap:plan(2),

    Modules = [
	rtp,
	rtcp
    ],

    lists:foreach(
        fun(Module) ->
            etap_can:loaded_ok(
                Module,
                lists:concat(["Loaded: ", Module])
            )
        end, Modules),
    etap:end_tests().
