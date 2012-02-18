-module(codec_speex_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_speex_test_() ->
	[
		{"Test decoding from SPEEX to PCM",
			fun() -> ?assertEqual(
						true,
						(fun() ->
							{ok, BinIn}  = file:read_file("../test/samples/speex/sample-speex-16-mono-8khz.raw"),
							{ok, PcmOut} = file:read_file("../test/samples/speex/sample-pcm-16-mono-8khz.from_spx"),
							{ok, Codec} = codec:start_link({'SPEEX',8000,1}),
							Ret = test_utils:decode_f("SPEEX", Codec, BinIn, PcmOut, 38),
							codec:close(Codec),
							Ret
						end)()
%						test_utils:codec_decode(
%							"../test/samples/speex/sample-speex-16-mono-8khz.raw",
%							"../test/samples/speex/sample-pcm-16-mono-8khz.from_spx",
%							38,
%							"SPEEX",
%							{'SPEEX',8000,1}
%						)
					) end
		},
		{"Test encoding from PCM to SPEEX",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/speex/sample-pcm-16-mono-8khz.raw",
							"../test/samples/speex/sample-speex-16-mono-8khz.from_pcm",
							320,
							"SPEEX",
							{'SPEEX',8000,1}
						)
					) end
		}
	].
