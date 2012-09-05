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

-module(gen_rtp_phy).
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
		rtp,
		rtcp,
		ip = null,
		rtpport = null,
		rtcpport = null,
		ssrc = null,
		sendrecv,
		mux,
		lastseen = null,
		% If set to true then we'll have another one INTERIM_UPDATE
		% interval to wait for initial data
		alive = false,
		tref
	}
).

start(Module, Params, Addon) ->
	gen_server:start(?MODULE, [Module, Params, Addon], []).
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

handle_cast({#rtp{} = Pkt, _, _}, #state{rtp = Fd, ip = Ip, rtpport = Port} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkt)),
	{noreply, State};
handle_cast({#rtcp{} = Pkt, _, _}, #state{rtp = Fd, ip = Ip, rtpport = Port, mux = true} = State) ->
	% FIXME - see transport parameter in the init(...) function arguments
	% If muxing is enabled (either explicitly or with a 'auto' parameter
	% then send RTCP acked within RTP stream
	gen_udp:send(Fd, Ip, Port, rtp:encode(Pkt)),
	{noreply, State};
handle_cast({#rtcp{} = Pkt, _, _}, #state{rtcp = Fd, ip = Ip, rtcpport = Port} = State) ->
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

handle_info({udp, Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PayloadType:7, _:48, SSRC:32, _/binary>> = Msg}, #state{sendrecv = SendRecv, mod = Module, modstate = ModState} = State)  when PayloadType =< 34; 96 =< PayloadType ->
	inet:setopts(Fd, [{active, once}]),
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{noreply, NewModState} = Module:handle_info({Msg, Ip, Port}, ModState),
			{noreply, State#state{lastseen = now(), alive = true, modstate = NewModState, ip = Ip, rtpport = Port, ssrc = SSRC}};
		false ->
			{noreply, State}
	end,
	{noreply, State};
handle_info({udp, Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PayloadType:7, _:48, SSRC:32, _/binary>> = Msg}, #state{sendrecv = SendRecv, mod = Module, modstate = ModState} = State)  when 64 =< PayloadType, PayloadType =< 82 ->
	inet:setopts(Fd, [{active, once}]),
	case SendRecv(Ip, Port, SSRC,  State#state.ip, State#state.rtcpport, State#state.ssrc) of
		true ->
			Mux = (State#state.mux == true) or ((State#state.rtpport == Port) and (State#state.mux == auto)),
			{noreply, NewModState} = Module:handle_info({Msg, Ip, Port}, ModState),
			{noreply, State#state{lastseen = now(), alive = true, modstate = NewModState, ip = Ip, rtcpport = Port, mux = Mux, ssrc = SSRC}};
		false ->
			{noreply, State}
	end,
	{noreply, State};
handle_info({udp, Fd, Ip, Port, <<?ZRTP_MARKER:16, _:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, _/binary>> = Msg}, #state{sendrecv = SendRecv, mod = Module, modstate = ModState} = State) ->
	inet:setopts(Fd, [{active, once}]),
	% Treat ZRTP in the same way as RTP
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{noreply, NewModState} = Module:handle_info({Msg, Ip, Port}, ModState),
			{noreply, State#state{lastseen = now(), alive = true, modstate = NewModState, ip = Ip, rtpport = Port, ssrc = SSRC}};
		false ->
			{noreply, State}
	end,
	{noreply, State};
handle_info({udp, Fd, Ip, Port, <<?STUN_MARKER:2, _:30, ?STUN_MAGIC_COOKIE:32, _/binary>> = Msg}, #state{sendrecv = SendRecv, mod = Module, modstate = ModState} = State) ->
	inet:setopts(Fd, [{active, once}]),
	% FIXME this is a STUN message - we should reply at this level
	{noreply, State};

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
	% 'roaming' - receives data from only one Ip and Port *or* with the same SSRC as before
	% 'enforcing' - Ip, Port and SSRC *must* match previously recorded data
	SendRecvStrategy = get_send_recv_strategy(Params),
	% 'false' = no muxing at all (RTP will be sent in the RTP and RTCP - in the RTCP channels separately)
	% 'true' - both RTP and RTCP will be sent in the RTP channel
	% 'auto' - the same as 'false' until we'll find muxed packet.
	MuxRtpRtcp = proplists:get_value(rtcpmux, Params, auto),

	Timeout = proplists:get_value(timeout, Params, ?INTERIM_UPDATE),
	{ok, TRef} = timer:send_interval(Timeout, interim_update),

	{Fd0, Fd1} = get_fd_pair({Transport, IpAddr, IpPort, SockParams}),

	{ok, {Ip, PortRtp}} = inet:sockname(Fd0),
	{ok, {Ip, PortRtcp}} = inet:sockname(Fd1),

	{ok, ModState} = Module:init([Ip, PortRtp, PortRtcp, Addon]),

	{noreply, #state{
			mod = Module,
			modstate = ModState,
			rtp = Fd0,
			rtcp = Fd1,
			mux = MuxRtpRtcp,
			sendrecv = SendRecvStrategy,
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

get_fd_pair(Transport, I, P, SockParams, 0) ->
	error_logger:error_msg("Create new socket at ~s:~b FAILED (~p)", [inet_parse:ntoa(I), P,  SockParams]),
	error;
get_fd_pair(Transport, I, P, SockParams, NTry) ->
	case gen_udp:open(P, [binary, {ip, I}, {active, once}, {raw,1,11,<<1:32/native>>}] ++ SockParams) of
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
					get_fd_pair(Transport, I, P, SockParams, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(Transport, I, P, SockParams, NTry - 1)
	end.

get_send_recv_strategy(Params) ->
	case proplists:get_value(sendrecv, Params, roaming) of
		weak -> fun send_recv_simple/6;
		roaming -> fun send_recv_roaming/6;
		enforcing -> fun send_recv_enforcing/6
	end.

%%
%% Various callbacks
%%

% 'weak' mode - just get data, decode and notify subscriber
send_recv_simple(_, _, _, _, _, _) -> true.

% 'roaming' mode - get data, check for the Ip and Port or for the SSRC (after decoding), decode and notify subscriber
% Legitimate RTP/RTCP packet - discard SSRC matching
send_recv_roaming(Ip, Port, _, Ip, Port, _) -> true;
% First RTP/RTCP packet
send_recv_roaming(Ip, Port, SSRC, _, null, _) -> true;
% Changed address - roaming
send_recv_roaming(Ip, Port, SSRC, _, _, SSRC) -> true;
% Different IP and SSRC - drop
send_recv_roaming(_, _, _, _, _, _) -> false.

% 'enforcing' - Ip, Port and SSRC must match previously recorded data
% Legitimate RTP/RTCP packet
send_recv_enforcing(Ip, Port, SSRC, Ip, Port, SSRC) -> true;
% First RTP/RTCP packet
send_recv_enforcing(Ip, Port, SSRC, _, null, _) -> true;
% Different IP and/or SSRC - drop
send_recv_enforcing(_, _, _, _, _, _) -> false.
