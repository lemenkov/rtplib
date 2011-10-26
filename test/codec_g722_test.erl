-module(codec_g722_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_g722_test_() ->
	% Original G.722 bitstream
	{ok, G722Raw} = file:read_file("../test/conf-adminmenu-162.g722"),
	% Decoded PCM
	{ok, PcmRaw} = file:read_file("../test/conf-adminmenu-162.raw"),

	% Source PCM for encoding
	{ok, PcmIn} = file:read_file("../test/sample-pcm-16-mono-8khz.raw"),
	% The resulting G.722 bitstream
	{ok, G722Out} = file:read_file("../test/sample-g722-16-mono-8khz.raw"),

	{ok, Codec} = codec:start_link({'G722',8000,1}),

	{setup,
		fun() -> Codec end,
		fun(C) -> codec:close(C) end,
		[
			% FIXME segfaults here
			{"Test decoding from G.722 to PCM",
				fun() -> ?assertEqual(true, decode(Codec, G722Raw, PcmRaw)) end
			},
			{"Test encoding from PCM to G.722",
				fun() -> ?assertEqual(true, encode(Codec, PcmIn, G722Out)) end
			}
		]
	}.

decode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 160; size(B) < 320 ->
	true;
decode(Codec, <<G722Frame:160/binary, G722Raw/binary>>, <<PcmFrame:320/binary, PcmRaw/binary>>) ->
	{ok, {PcmFrame1, 8000, 1, 16}} = codec:decode(Codec, G722Frame),
	decode(Codec, G722Raw, PcmRaw).

encode(Codec, <<_/binary>> = A, <<_/binary>> = B) when size(A) < 320; size(B) < 160 ->
	true;
encode(Codec, <<PcmFrame:320/binary, PcmRaw/binary>>, <<G722Frame:160/binary, G722Raw/binary>>) ->
	{ok, G722Frame} = codec:encode(Codec, {PcmFrame, 8000, 1, 16}),
	encode(Codec, PcmRaw, G722Raw).
