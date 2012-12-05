%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2012 Peter Lemenkov <lemenkov@gmail.com>
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%%
%%% * Redistributions of source code must retain the above copyright notice, this
%%% list of conditions and the following disclaimer.
%%% * Redistributions in binary form must reproduce the above copyright notice,
%%% this list of conditions and the following disclaimer in the documentation
%%% and/or other materials provided with the distribution.
%%% * Neither the name of the authors nor the names of its contributors
%%% may be used to endorse or promote products derived from this software
%%% without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ''AS IS'' AND ANY
%%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%----------------------------------------------------------------------

-module(codec_g729_test).

-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

codec_g729a_test_() ->
	[
		{"Test encoding from PCM to G.729",
			fun() -> ?assertEqual(
						true,
						test_utils:codec_encode(
							"../test/samples/g729/default_en.16-mono-8khz.raw",
							"../test/samples/g729/default_en.g729",
							320,
							"G.729",
							{'G729',8000,1}
						)
					) end
		}
	].

codec_g729b_payload_and_sid_test_() ->
	Payload = <<192,143,182,224,138,90,129,73,128,86>>,
	Sid = <<164,78>>,
	{ok, Codec} = codec:start_link({'G729', 8000, 1}),
	[
		{"Test G.729 annex B Silence Insertion Descriptor frames",
			fun() -> ?assertMatch({ok,{Binary,8000,1,16}}, codec:decode(Codec, <<Payload/binary, Sid/binary>>)) end
		}
	].

codec_g729b_no_payload_sid_test_() ->
	Sid = <<182,00>>,
	{ok, Codec} = codec:start_link({'G729', 8000, 1}),
	[
		{"Test G.729 annex B Silence Insertion Descriptor frames",
			fun() -> ?assertEqual({ok,{<<>>,8000,1,16}}, codec:decode(Codec, Sid)) end
		}
	].
