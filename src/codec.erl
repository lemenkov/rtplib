% Loosely based on Evgeniy Khramtsov's original approach - erlrtp

-module(codec).

-behaviour(gen_server).

-include("../include/rtp.hrl").

-export([decode/2]).
-export([encode/2]).
-export([close/1]).

-export([start/1]).
-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(CMD_ENCODE, 1).
-define(CMD_DECODE, 2).

-record(state, {port}).

start(Args) ->
	gen_server:start(?MODULE, Args, []).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Format) ->
	DriverName = case Format of
		?RTP_PAYLOAD_PCMU -> pcmu_codec_drv;
		?RTP_PAYLOAD_GSM -> gsm_codec_drv;
		?RTP_PAYLOAD_PCMA -> pcma_codec_drv;
		?RTP_PAYLOAD_G722 -> g722_codec_drv
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
			Port = open_port({spawn, DriverName}, [binary]),
			{ok, #state{port = Port}};
		{error, Error1} ->
			{stop, Error1}
	end.

handle_call({Action, Data}, _From, #state{port = Port} = State) when Action == ?CMD_ENCODE; Action == ?CMD_DECODE ->
	Reply = case port_control(Port, Action, Data) of
		Binary when is_binary(Binary) -> {ok, Binary};
		_ -> {error, codec_error}
	end,
	{reply, Reply, State};

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

terminate(_Reason, #state{port = Port}) ->
	catch port_close(Port),
	ok.

close(Codec) when is_pid(Codec) ->
	gen_server:cast(Codec, stop).

decode(Codec, Payload) when is_pid(Codec), is_binary(Payload) ->
	gen_server:call(Codec, {?CMD_DECODE, Payload}).

encode(Codec, Payload) when is_pid(Codec), is_binary(Payload) ->
	gen_server:call(Codec, {?CMD_ENCODE, Payload}).
