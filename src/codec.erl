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

% Loosely based on Evgeniy Khramtsov's original approach - erlrtp

-module(codec).

-behaviour(gen_server).
-compile({parse_transform, do}).

-include("../include/rtp.hrl").

-export([decode/2]).
-export([encode/2]).
-export([close/1]).

-export([default_codecs/0]).
-export([is_supported/1]).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(CMD_SETUP, 0).
-define(CMD_ENCODE, 1).
-define(CMD_DECODE, 2).
-define(CMD_RESAMPLE(FromSR, FromCh, ToSR, ToCh), (FromSR div 1000) * 16777216 + FromCh * 65536  + (ToSR div 1000) * 256 + ToCh).


-record(state, {port, type, samplerate, channels, resolution, resampler}).

% For testing purposes only
default_codecs() ->
	[{'PCMU',8000,1},{'GSM', 8000,1},{'PCMA',8000,1},{'G722',8000,1},{'G729',8000,1}].

is_supported({'PCMU',8000,1}) -> true;
is_supported({'GSM', 8000,1}) -> true;
is_supported({'DVI4',8000,1}) -> true;
is_supported({'DVI4',16000,1}) -> true;
is_supported({'PCMA',8000,1}) -> true;
is_supported({'G722',8000,1}) -> true;
is_supported({'G726',8000,1}) -> true;
is_supported({'G729',8000,1}) -> true;
is_supported({'LPC',8000,1}) -> true;
is_supported({'DVI4',11025,1}) -> true;
is_supported({'DVI4',22050,1}) -> true;
is_supported({'SPEEX',8000,1}) -> true;
is_supported({'SPEEX',16000,1}) -> true;
is_supported({'SPEEX',32000,1}) -> true;
is_supported({'ILBC',8000,1}) -> true;
is_supported({'OPUS',8000,1}) -> true;
is_supported({'OPUS',8000,2}) -> true;
is_supported({'OPUS',12000,1}) -> true;
is_supported({'OPUS',12000,2}) -> true;
is_supported({'OPUS',16000,1}) -> true;
is_supported({'OPUS',16000,2}) -> true;
is_supported({'OPUS',24000,1}) -> true;
is_supported({'OPUS',24000,2}) -> true;
is_supported({'OPUS',48000,1}) -> true;
is_supported({'OPUS',48000,2}) -> true;
is_supported(_) -> false.

start_link(Args)  ->
	case is_supported(Args) of
		true -> gen_server:start_link(?MODULE, Args, []);
		false -> {stop, unsupported}
	end.

init({Format, SampleRate, Channels}) ->
	DriverName = case Format of
		'PCMU' -> pcmu_codec_drv;
		'GSM'  -> gsm_codec_drv;
		'DVI4' -> dvi4_codec_drv;
		'PCMA' -> pcma_codec_drv;
		'G722' -> g722_codec_drv;
		'G726' -> g726_codec_drv;
		'G729' -> g729_codec_drv;
		'LPC' -> lpc_codec_drv;
		'SPEEX' -> speex_codec_drv;
		'ILBC' -> ilbc_codec_drv;
		'OPUS' -> opus_codec_drv
	end,
	Result = do([error_m ||
			load_library(DriverName),
			load_library(resampler_drv)]),

	case Result of
		ok ->
			Port = open_port({spawn, DriverName}, [binary]),
			PortResampler = open_port({spawn, resampler_drv}, [binary]),
			% FIXME only 16-bits per sample currently
			port_control(Port, ?CMD_SETUP, <<SampleRate:32/native-unsigned-integer, Channels:32/native-unsigned-integer>>),
			{ok, #state{port = Port, type = Format, samplerate = SampleRate, channels = Channels, resolution = 16, resampler = PortResampler}};
		{error, Error} ->
			{stop, Error}
	end.

% Encoding doesn't require resampling
handle_call(
	{?CMD_ENCODE, {Binary, SampleRate, Channels, Resolution}},
	_From,
	#state{port = Port, samplerate = SampleRate, channels = Channels, resolution = Resolution} = State
) ->
	{reply, encode_binary(Port, ?CMD_ENCODE, Binary), State};

% Encoding requires resampling
handle_call(
	{?CMD_ENCODE, {Binary, SampleRate, Channels, Resolution}},
	_From,
	#state{port = Port, samplerate = NativeSampleRate, channels = NativeChannels, resolution = NativeResolution, resampler = PortResampler} = State
) ->
	Result = do([error_m ||
			ResampledBinary <- encode_binary(PortResampler, ?CMD_RESAMPLE(SampleRate, Channels, NativeSampleRate, NativeChannels), Binary),
			encode_binary(Port, ?CMD_ENCODE, ResampledBinary)]),

	{reply, Result, State};

handle_call(
	{?CMD_DECODE, Binary},
	_From,
	#state{port = Port, samplerate = SampleRate, channels = Channels, resolution = Resolution} = State
) ->
	case port_control(Port, ?CMD_DECODE, Binary) of
		NewBinary when is_binary(NewBinary) ->
			{reply, {ok, {NewBinary, SampleRate, Channels, Resolution}}, State};
		_ ->
			{reply, {error, codec_error}, State}
	end;

handle_call(_Other, _From, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({'DOWN', _, _, _, _}, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, #state{port = Port, resampler = PortResampler}) ->
	catch port_close(Port),
	catch port_close(PortResampler),
	ok.

close(Codec) when is_pid(Codec) ->
	gen_server:cast(Codec, stop).

decode(Codec, Payload) when is_pid(Codec), is_binary(Payload) ->
	gen_server:call(Codec, {?CMD_DECODE, Payload}).

encode(Codec, {Payload, SampleRate, Channels, Resolution}) when is_pid(Codec), is_binary(Payload) ->
	gen_server:call(Codec, {?CMD_ENCODE, {Payload, SampleRate, Channels, Resolution}}).

%%
%% Private fun
%%

load_library(Name) ->
	case erl_ddll:load_driver(get_priv(), Name) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, permanent} -> ok;
		{error, Error} ->
			error_logger:error_msg("Can't load ~p library: ~s~n", [Name, erl_ddll:format_error(Error)]),
			{error, Error}
	end.

-ifdef(TEST).
get_priv() -> "../priv". % Probably eunit session
-else.
get_priv() -> code:lib_dir(rtplib, priv).
-endif.

encode_binary(Port, Cmd, BinIn) ->
	case port_control(Port, Cmd, BinIn) of
		BinOut when is_binary(BinOut) -> {ok, BinOut};
		_ -> {error, codec_error}
	end.
