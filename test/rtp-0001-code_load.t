#!/usr/bin/escript
%% -*- erlang -*-

% Test that we can load necessary modules.

main(_) ->
    etap:plan(2),

    Modules = [
	rtp,
	rtp_utils
    ],

    lists:foreach(
        fun(Module) ->
            etap_can:loaded_ok(
                Module,
                lists:concat(["Loaded: ", Module])
            )
        end, Modules),
    etap:end_tests().
