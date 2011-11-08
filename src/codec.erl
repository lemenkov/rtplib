% Loosely based on Evgeniy Khramtsov's original approach - erlrtp

-module(codec).

-behaviour(gen_server).

-include("../include/rtp.hrl").

-export([decode/2]).
-export([encode/2]).
-export([close/1]).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(CMD_ENCODE, 1).
-define(CMD_DECODE, 2).
-define(CMD_RESAMPLE(FromSR, FromCh, ToSR, ToCh), (FromSR div 1000) * 16777216 + FromCh * 65536  + (ToSR div 1000) * 256 + ToCh).

-record(state, {port, type, samplerate, channels, resolution, resampler}).

start_link(Args) when
	Args == {'PCMU',8000,1};
	Args == {'GSM', 8000,1};
	Args == {'DVI4',8000,1};
	Args == {'DVI4',16000,1};
	Args == {'PCMA',8000,1};
	Args == {'G722',8000,1};
	Args == {'DVI4',11025,1};
	Args == {'DVI4',22050,1};
	Args == {'SPEEX',8000,1};
	Args == {'SPEEX',16000,1};
	Args == {'SPEEX',32000,1}
	->
	gen_server:start_link(?MODULE, Args, []);
start_link(_) ->
	{stop, unsupported}.

init({Format, SampleRate, Channels}) ->
	DriverName = case Format of
		'PCMU' -> pcmu_codec_drv;
		'GSM'  -> gsm_codec_drv;
		'DVI4' -> dvi4_codec_drv;
		'PCMA' -> pcma_codec_drv;
		'G722' -> g722_codec_drv;
		'SPEEX' -> speex_codec_drv
	end,
	case
		case erl_ddll:load_driver(code:lib_dir(rtplib) ++ "/priv/", DriverName) of
			ok -> ok;
			{error, already_loaded} -> ok;
			{error, permanent} -> ok;
			{error, Error} -> error_logger:error_msg("Can't load ~p codec: ~p~n", [Format, erl_ddll:format_error(Error)]), {error, Error}
		end
	of
		ok ->
			case
				case erl_ddll:load_driver(code:lib_dir(rtplib) ++ "/priv/", resampler_drv) of
					ok -> ok;
					{error, already_loaded} -> ok;
					{error, permanent} -> ok;
					{error, Error0} -> error_logger:error_msg("Can't load ~p resampler: ~p~n", [Format, erl_ddll:format_error(Error0)]), {error, Error0}
				end
			of
				ok ->
					Port = open_port({spawn, DriverName}, [binary]),
					PortResampler = open_port({spawn, resampler_drv}, [binary]),
					% FIXME only 16-bits per sample currently
					{ok, #state{port = Port, type = Format, samplerate = SampleRate, channels = Channels, resolution = 16, resampler = PortResampler}};
				{error, Error2} ->
					{stop, Error2}
			end;
		{error, Error1} ->
			{stop, Error1}
	end.

% Encoding doesn't require resampling
handle_call(
	{?CMD_ENCODE, {Binary, SampleRate, Channels, Resolution}},
	_From,
	#state{port = Port, samplerate = SampleRate, channels = Channels, resolution = Resolution} = State
) ->
	case port_control(Port, ?CMD_ENCODE, Binary) of
		NewBinary when is_binary(NewBinary) ->
			{reply, {ok, NewBinary}, State};
		_ ->
			{reply, {error, codec_error}, State}
	end;

% Encoding requires resampling
handle_call(
	{?CMD_ENCODE, {Binary, SampleRate, Channels, Resolution}},
	_From,
	#state{port = Port, samplerate = NativeSampleRate, channels = NativeChannels, resolution = NativeResolution, resampler = PortResampler} = State
) ->
	case port_control(PortResampler, ?CMD_RESAMPLE(SampleRate, Channels, NativeSampleRate, NativeChannels), Binary) of
		ResampledBinary when is_binary(ResampledBinary) ->
			case port_control(Port, ?CMD_ENCODE, Binary) of
				NewBinary when is_binary(NewBinary) ->
					{reply, {ok, NewBinary}, State};
				_ ->
					{reply, {error, codec_error}, State}
			end;
		_ ->
			{reply, {error, resampler_error}, State}
	end;

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
