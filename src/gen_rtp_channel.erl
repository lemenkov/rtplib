%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
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

-module(gen_rtp_channel).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start/3]).
-export([start_link/3]).

-export([behaviour_info/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-include("../include/rtcp.hrl").
-include("../include/rtp.hrl").
-include("../include/srtp.hrl").
-include("../include/stun.hrl").
-include("../include/zrtp.hrl").

%% @private
behaviour_info(callbacks) ->
	[
		{init, 1},
		{handle_call, 3},
		{handle_cast, 2},
		{handle_info, 2},
		{code_change, 3},
		{terminate, 2}
	];
behaviour_info(_) ->
	undefined.

% Default value of RTP timeout in milliseconds.
-define(INTERIM_UPDATE, 30000).

-record(state, {
		mod = null,
		modstate = null,
		ssrc = null,
		encoder = false,
		decoders = []
	}
).

start(Module, Params, Addon) ->
	gen_rtp_phy:start(?MODULE, [Module, Params, Addon], []).
start_link(Module, Params, Addon) ->
	gen_rtp_phy:start_link(?MODULE, [Module, Params, Addon], []).

init([Module, Params, Addon]) when is_atom(Module) ->
	{Encoder, Decoders} = case proplists:get_value(transcode, Params, false) of
		false ->
			{
				false,
				[]
			};
		CodecDesc ->
			{
				codec:start_link(CodecDesc),
				lists:map(
					fun(CodecDesc) -> codec:start_link(CodecDesc) end,
					proplists:get_value(codecs, Params, [])
				)
			}
	end,

	{ok, ModState} = Module:init([Params, Addon]),

	{ok, #state{
			mod = Module,
			modstate = ModState,
			encoder = Encoder,
			decoders = Decoders
		}
	}.

handle_call(Request, From, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_call(Request, From, ModState) of
		{reply, Reply, NewModState} ->
			{reply, Reply, State#state{modstate=NewModState}};
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}};
		{stop, Reason, Reply, NewModState} ->
			{stop, Reason, Reply, State#state{modstate=NewModState}}
	 end.

handle_cast({#rtp{} = Pkt, _, _}, State) ->
	% FIXME - send to the next level?
	{noreply, State};
handle_cast({#rtp{} = Pkt, _, _}, #state{encoder = Encoder, decoders = Decoders} = State) ->
	Pkt2 = transcode(Pkt, Encoder, Decoders),
	% FIXME - send to the next level?
	{noreply, State};
handle_cast({#rtcp{} = Pkt, _, _}, State) ->
	% FIXME - send to the next level?
	{noreply, State};

handle_cast({raw, Ip, Port, {PayloadType, Msg}}, State) ->
	% FIXME
	{noreply, State};

handle_cast({update, Params}, State) ->
	% FIXME consider changing some parameters
	{ok, State};

handle_cast(Request, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_cast(Request, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}}
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{mod = Mod, modstate = ModState, encoder = Encoder, decoders = Decoders}) ->
	Mod:terminate(Reason, ModState),
	% Close encoder and decoders (if any)
	case Encoder of
		false -> ok;
		_ -> codec:close(Encoder)
	end,
	lists:foreach(fun(Codec) -> codec:close(Codec) end, Decoders).

handle_info(Info, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_info(Info, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}}
	end.

%%
%% Private functions
%%

transcode(Pkt, false, _) ->
	Pkt;
transcode(#rtp{payload_type = OldPayloadType, payload = Payload} = Rtp, {PayloadType, Encoder}, Decoders) ->
	Decoder = proplists:get_value(OldPayloadType, Decoders),
	{ok, RawData} = codec:decode(Decoder, Payload),
	{ok, NewPayload} = codec:encode(Encoder, RawData),
	Rtp#rtp{payload_type = PayloadType, payload = NewPayload}.
