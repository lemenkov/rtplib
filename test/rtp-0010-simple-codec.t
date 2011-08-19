#!/usr/bin/escript
%% -*- erlang -*-

% Test RTP codec.

-include_lib("rtplib/include/rtp.hrl").

main(_) ->
	etap:plan(2),

	Data = <<128,0,253,88,145,83,40,165,11,160,200,38,193,179,189,216,194,104,66,67,58,56,
		53,57,68,64,81,235,109,228,213,203,203,213,200,208,245,200,197,197,179,188,
		215,192,123,67,65,57,57,53,55,70,63,79,233,110,225,213,203,201,214,204,205,
		219,207,199,190,184,185,206,200,230,70,64,57,57,53,54,67,66,75,245,250,232,
		217,204,199,214,217,207,202,231,219,194,195,189,179,197,208,202,76,63,60,55,
		56,52,60,72,67,92,231,237,220,212,199,204,220,204,206,229,209,192,191,185,
		183,204,204,231,71,62,57,57,54,53,63,73,77,109,236,219,219,209,197,212,215,
		204,221,223,220,205,196,190,184,186,207,212,252,70,60,59,57,53,56,66,71,78,
		108,227,220,223,206,197>>,

	etap:fun_is(fun({ok, #rtp{padding = P, marker = M, payload_type = PT, sequence_number = SN, timestamp = TS, ssrc = SSRC, csrcs = CSRCs, extension = X, payload = Payload}}) when is_binary(Payload) -> true; (_) -> false end, rtp:decode(Data), "Simple decoding of RTP data stream"),
	{ok, Rtp} = rtp:decode(Data),

	etap:is(Data, rtp:encode(Rtp), "Check that we can reproduce original data stream from record"),

	etap:end_tests().

