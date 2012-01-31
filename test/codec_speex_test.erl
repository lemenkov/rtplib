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
%			{"Test decoding from SPEEX to PCM",
%				fun() -> ?assertEqual(true, test_utils:decode("SPEEX", Codec, SpeexIn, PcmOut, 38, 320)) end
%			},
%			{"Test encoding from PCM to SPEEX",
%				fun() -> ?assertEqual(true, test_utils:encode_f("SPEEX", Codec, PcmIn, SpeexOut, 320, 38)) end
%			}
		]
	}.
