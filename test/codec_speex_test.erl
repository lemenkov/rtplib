-module(codec_speex_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_speex_test_() ->
	% Original SPEEX stream
	{ok, SpeexIn} = file:read_file("../test/sample-speex-16-mono-8khz.raw"),
	% Decoded PCM
	{ok, PcmOut} = file:read_file("../test/sample-pcm-16-mono-8khz.from_spx"),

	% Original PCM
	{ok, PcmIn} = file:read_file("../test/sample-pcm-16-mono-8khz.raw"),
	% Original SPEEX stream
	{ok, SpeexOut} = file:read_file("../test/sample-speex-16-mono-8khz.from_pcm"),

	{ok, Codec} = codec:start_link({'SPEEX',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			{"Test decoding from SPEEX to PCM",
				fun() -> ?assertEqual(true, decode(Codec, SpeexIn, PcmOut)) end
			},
			{"Test encoding from PCM to SPEEX",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, SpeexOut)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 38; size(B) < 320 ->
	true;
decode(Codec, <<SpeexFrame:38/binary, SpeexRaw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	% FIXME add reference bitstream
	{ok, {PcmFrame1, 8000, 1, 16}} = codec:decode(Codec, SpeexFrame),
	decode(Codec, SpeexRaw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 38 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<SpeexFrame:38/binary, SpeexRaw/binary>>) ->
	% FIXME add reference bitstream
	{ok, SpeexFrame1} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
	encode(Codec, PcmRaw, SpeexRaw).
