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
		rtp,
		rtcp,
		ip = null,
		rtpport = null,
		rtcpport = null,
		ssrc = null,
		proxy,
		sendrecv,
		mux,
		lastseen = null,
		% If set to true then we'll have another one INTERIM_UPDATE
		% interval to wait for initial data
		alive = false,
		tref
	}
).

start_link(Module, Params, Addon) ->
	gen_server:start_link(?MODULE, [Module, Params, Addon], []).

init([Module, Params, Addon]) when is_atom(Module) ->
	% Deferred init
	self() ! {init, {Module, Params, Addon}},

	{ok, #state{}}.

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

handle_cast({#rtp{} = Pkt, Ip, Port}, #state{rtp = Fd} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkt)),
	{noreply, State};
handle_cast({#rtcp{} = Pkt, Ip, Port}, #state{rtp = Fd, mux = true} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	% If muxing is enabled (either explicitly or with a 'auto' parameter
	% then send RTCP acked within RTP stream
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkt)),
	{noreply, State};
handle_cast({#rtcp{} = Pkt, Ip, Port}, #state{rtcp = Fd} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkt)),
	{noreply, State};

handle_cast({raw, Ip, Port, {PayloadType, Msg}}, State) ->
	% FIXME
	{noreply, State};

handle_cast({update, Params}, State) ->
	% FIXME consider changing another params as well
	SendRecvStrategy = get_send_recv_strategy(Params),
	{ok, State#state{sendrecv = SendRecvStrategy}};

handle_cast(Request, State = #state{mod=Module, modstate=ModState}) ->
	case Module:handle_cast(Request, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState}}
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{mod = Mod, modstate = ModState, rtp = Fd0, rtcp = Fd1, tref = TRef}) ->
	timer:cancel(TRef),
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:close(Fd0),
	% FIXME We must send RTCP bye here
	gen_udp:close(Fd1),
	Mod:terminate(Reason, ModState).

handle_info({udp, Fd, Ip, Port, Msg}, #state{sendrecv = SendRecv} = State) ->
	inet:setopts(Fd, [{active, once}]),
	NewState = SendRecv(Msg, Ip, Port, State),
	{noreply, NewState};

handle_info(interim_update, #state{mod=Module, modstate=ModState, alive = true} = State) ->
	case Module:handle_info(interim_update, ModState) of
		{noreply, NewModState} ->
			{noreply, State#state{modstate=NewModState, alive=false}};
		{stop, Reason, NewModState} ->
			{stop, Reason, State#state{modstate=NewModState, alive=false}}
	end;
handle_info(interim_update, #state{alive = false} = State) ->
	{stop, timeout, State};

handle_info({init, {Module, Params, Addon}}, State) ->
	% Choose udp, tcp, sctp, dccp - FIXME only udp is supported
	Transport = proplists:get_value(transport, Params, udp),
	SockParams = proplists:get_value(sockparams, Params, []),
	% Either specify IPv4 or IPv6 explicitly or provide two special
	% values - "::" for any available IPv6 or "0.0.0.0" or "0" for
	% any available IPv4.
	{ok, IpAddr} = inet_parse:address(proplists:get_value(ip, Params, "0.0.0.0")),
	% Either specify port explicitly or provide none if don't care
	IpPort = proplists:get_value(port, Params, 0),
	% 'weak' - receives data from any Ip and Port with any SSRC
	% 'selective' - receives data from only one Ip and Port *or* with the same SSRC as before
	% 'enforcing' - Ip, Port and SSRC *must* match previously recorded data
	% 'srtp' - depends on SRTP/ZRTP
	SendRecvStrategy = get_send_recv_strategy(Params),
	% 'true' - act as a proxy (send RTP and RTCP packets further as is)
	% 'false' - strip-off everything except RTP payload. Regenerate RTCP. 
	% FIXME only proxy (bypass) mode for now
	Proxy = proplists:get_value(proxy, Params, true),
	% 'false' = no muxing at all (RTP will be sent in the RTP and RTCP - in the RTCP channels separately)
	% 'true' - both RTP and RTCP will be sent in the RTP channel
	% 'auto' - the same as 'false' until we'll find muxed packet.
	MuxRtpRtcp = proplists:get_value(rtcpmux, Params, auto),

	{ok, Timeout} = proplists:get_value(timeout, Params, ?INTERIM_UPDATE),
	{ok, TRef} = timer:send_interval(Timeout, interim_update),

	{Fd0, Fd1} = get_fd_pair({Transport, IpAddr, IpPort, SockParams}),

	{ok, ModState} = Module:init(Addon),

	{noreply, #state{
			mod = Module,
			modstate = ModState,
			rtp = Fd0,
			rtcp = Fd1,
			mux = MuxRtpRtcp,
			sendrecv = SendRecvStrategy,
			proxy = Proxy,
			tref = TRef
		}
	};

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

%% Open a pair of UDP ports - N and N+1 (for RTP and RTCP consequently)
get_fd_pair({Transport, {I0,I1,I2,I3,I4,I5,I6,I7} = IPv6, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 65536,
	is_integer(I1), 0 =< I1, I1 < 65536,
	is_integer(I2), 0 =< I2, I2 < 65536,
	is_integer(I3), 0 =< I3, I3 < 65536,
	is_integer(I4), 0 =< I4, I4 < 65536,
	is_integer(I5), 0 =< I5, I5 < 65536,
	is_integer(I6), 0 =< I6, I6 < 65536,
	is_integer(I7), 0 =< I7, I7 < 65536 ->
	get_fd_pair(Transport, IPv6, Port, proplists:delete(ipv6, SockParams) ++ [inet6], 10);
get_fd_pair({Transport, {I0,I1,I2,I3} = IPv4, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 256,
	is_integer(I1), 0 =< I1, I1 < 256,
	is_integer(I2), 0 =< I2, I2 < 256,
	is_integer(I3), 0 =< I3, I3 < 256 ->
	get_fd_pair(Transport, IPv4, Port, proplists:delete(ipv6, SockParams), 10).

get_fd_pair(Transport, Ip, Port, SockParams, 0) ->
	error_logger:error_msg("Create new socket at ~s:~b FAILED (~p)", [inet_parse:ntoa(Ip), Port,  SockParams]),
	error;
get_fd_pair(Transport, Ip, Port, SockParams, NTry) ->
	case gen_udp:open(Port, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
		{ok, Fd} ->
			{ok, {Ip,Port}} = inet:sockname(Fd),
			Port2 = case Port rem 2 of
				0 -> Port + 1;
				1 -> Port - 1
			end,
			case gen_udp:open(Port2, [binary, {ip, Ip}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
				{ok, Fd2} ->
					if
						Port > Port2 -> {Fd2, Fd};
						Port < Port2 -> {Fd, Fd2}
					end;
				{error, _} ->
					gen_udp:close(Fd),
					get_fd_pair(Transport, Ip, Port, SockParams, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(Transport, Ip, Port, SockParams, NTry - 1)
	end.

get_send_recv_strategy(Params) ->
	case proplists:get_value(sendrecv, Params, weak) of
		weak -> fun send_recv_simple/4;
		simple -> fun send_recv_simple/4;
		selective -> fun send_recv_selective/4;
		enforcing -> fun send_recv_enforcing/4;
		srtp -> fun send_recv_srtp/4
	end.
%%
%% Various callbacks
%%

% 'weak' mode - just get data, decode and notify subscriber
send_recv_simple(Msg, Ip, Port, #state{mod = Module, modstate = ModState} = State) ->
	case catch rtp:decode(Msg) of
		{ok, #rtp{} = Pkt} ->
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, lastseen = now(), ip = Ip, rtpport = Port, alive = true};
		{ok, #rtcp{} = Pkt} ->
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			Mux = (State#state.mux == true) or ((State#state.rtpport == Ip) and (State#state.mux == auto)),
			State#state{modstate = NewModState, lastseen = now(), ip = Ip, rtcpport = Port, alive = true, mux = Mux};
		_ ->
			rtp_utils:dump_packet(node(), self(), Msg),
			State
	end.

% 'selective' mode - get data, check for the Ip and Port or for the SSRC (after decoding), decode and notify subscriber
send_recv_selective(Msg, Ip, Port, #state{ip = I, rtpport = P1, rtcpport = P2, ssrc = S, mod = Module, modstate = ModState} = State) ->
	case {catch rtp:decode(Msg), I, P1, P2, S} of
		{{ok, #rtp{} = Pkt}, Ip, Port, _, _} ->
			% Legitimate RTP packet - discard SSRC matching
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, lastseen = now(), alive = true};
		{{ok, #rtp{ssrc = SSRC} = Pkt}, _, null, _, _} ->
			% First RTP packet - save parameters
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, ip = Ip, rtpport = Port, ssrc = SSRC, lastseen = now(), alive = true};
		{{ok, #rtp{ssrc = SSRC} = Pkt}, _, _, SSRC, _} ->
			% Changed address - roaming (drop RTCP port - we don't know anything about it)
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, ip = Ip, rtpport = Port, rtcpport = null, lastseen = now(), alive = true};
		{{ok, #rtcp{} = Pkt}, Ip, _, Port, _} ->
			% Legitimate RTCP packet
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			Mux = (State#state.mux == true) or ((State#state.rtpport == Ip) and (State#state.mux == auto)),
			State#state{modstate = NewModState, lastseen = now(), alive = true, mux = Mux};
		{{ok, #rtcp{} = Pkt}, _, _, null, _} ->
			% First RTCP packet - save parameters
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			Mux = (State#state.mux == true) or ((State#state.rtpport == Ip) and (State#state.mux == auto)),
			State#state{modstate = NewModState, lastseen = now(), ip = Ip, rtcpport = Port, alive = true, mux = Mux};
		_ ->
			rtp_utils:dump_packet(node(), self(), Msg),
			State
	end.

% 'enforcing' - Ip, Port and SSRC must match previously recorded data
send_recv_enforcing(Msg, Ip, Port, #state{ip = I, rtpport = P1, rtcpport = P2, ssrc = S, mod = Module, modstate = ModState} = State) ->
	case {catch rtp:decode(Msg), I, P1, P2, S} of
		{{ok, #rtp{ssrc = SSRC} = Pkt}, Ip, Port, _, SSRC} ->
			% Legitimate RTP packet
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, lastseen = now(), alive = true};
		{{ok, #rtp{ssrc = SSRC} = Pkt}, _, null, _, _} ->
			% First RTP packet - save parameters
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			State#state{modstate = NewModState, ip = Ip, rtpport = Port, ssrc = SSRC, lastseen = now(), alive = true};
		{{ok, #rtcp{} = Pkt}, Ip, _, Port, _} ->
			% Legitimate RTCP packet
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			Mux = (State#state.mux == true) or ((State#state.rtpport == Ip) and (State#state.mux == auto)),
			State#state{modstate = NewModState, lastseen = now(), alive = true, mux = Mux};
		{{ok, #rtcp{} = Pkt}, _, _, null, _} ->
			% First RTCP packet - save parameters
			{noreply, NewModState} = Module:handle_info({Pkt, Ip, Port}, ModState),
			Mux = (State#state.mux == true) or ((State#state.rtpport == Ip) and (State#state.mux == auto)),
			State#state{modstate = NewModState, lastseen = now(), ip = Ip, rtcpport = Port, alive = true, mux = Mux};
		_ ->
			rtp_utils:dump_packet(node(), self(), Msg),
			State
	end.

% 'srtp' - depends on SRTP/ZRTP
send_recv_srtp(Msg, Ip, Port, #state{mod = Module, modstate = ModState} = State) ->
	% TODO
	State.
