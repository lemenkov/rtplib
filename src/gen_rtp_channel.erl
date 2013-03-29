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

-export([open/1]).
-export([open/2]).
-export([close/1]).

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

% Default value of RTP timeout in milliseconds.
-define(INTERIM_UPDATE, 30000).

-record(state, {
		parent = null,
		rtp_subscriber = null,
		rtp,
		rtcp,
		ip = null,
		rtpport = null,
		rtcpport = null,
		tmod = null,
		active = null,
		ssrc = null,
		type,
		rxbytes = 0,
		rxpackets = 0,
		txbytes = 0,
		txpackets = 0,
		sendrecv,
		mux,
		zrtp = null,
		ctxI = passthru,
		ctxO = passthru,
		other_ssrc = null,
		lastseen = {0,0,0},
		process_chain_up = [],
		process_chain_down = [],
		encoder = false,
		decoder = false,
		% If set to true then we'll have another one INTERIM_UPDATE
		% interval to wait for initial data
		keepalive = true,
		timeout = ?INTERIM_UPDATE,
		counter = 0,
		tref
	}
).

open(Port) ->
	open(Port, []).
open(Port, Params) ->
	PPid = self(),
	gen_server:start_link(?MODULE, [Params ++ [{parent, PPid}, {port, Port}]], []).
close(Pid) ->
	gen_server:cast(Pid, stop).

init([Params]) ->
	process_flag(trap_exit, true),

	% Deferred init
	self() ! {init, Params},

	{ok, #state{}}.

handle_call({
		prepcrypto,
		{SSRCI, Cipher, Auth, TagLen, KeyI, SaltI},
		{SSRCO, Cipher, Auth, TagLen, KeyO, SaltO}
	},
	From,
	State) ->
	CtxI = srtp:new_ctx(SSRCI, Cipher, Auth, KeyI, SaltI, TagLen),
	CtxO = srtp:new_ctx(SSRCO, Cipher, Auth, KeyO, SaltO, TagLen),
	% Prepare starting SRTP - set up Ctx but wait for the SRTP from the other side
	{reply, ok, State#state{ctxI = CtxI, ctxO = CtxO}};

handle_call({
		gocrypto,
		{SSRCI, Cipher, Auth, TagLen, KeyI, SaltI},
		{SSRCO, Cipher, Auth, TagLen, KeyO, SaltO}
	},
	From,
	State) ->
	% Start SRTP immediately after setting up Ctx
	CtxI = srtp:new_ctx(SSRCI, Cipher, Auth, KeyI, SaltI, TagLen),
	CtxO = srtp:new_ctx(SSRCO, Cipher, Auth, KeyO, SaltO, TagLen),
	{reply, ok, State#state{ctxI = CtxI, ctxO = CtxO}};

handle_call(get_stats, _, #state{ip = Ip, rtpport = RtpPort, rtcpport = RtcpPort, ssrc = SSRC, type = Type, rxbytes = RxBytes, rxpackets = RxPackets, txbytes = TxBytes, txpackets = TxPackets} = State) ->
	{reply, {Ip, RtpPort, RtcpPort, SSRC, Type, RxBytes, RxPackets, TxBytes, TxPackets}, State};

handle_call({rtp_subscriber, {set, Subscriber}}, _, State) ->
	{reply, ok, State#state{rtp_subscriber = Subscriber}};
handle_call({rtp_subscriber, {add, Subscriber}}, _, #state{rtp_subscriber = OldSubscriber} = State) ->
	{reply, ok, State#state{rtp_subscriber = append_subscriber(OldSubscriber, Subscriber)}};

handle_call(get_rtp_phy, _, #state{tmod = TMod, rtp = Fd, ip = Ip, rtpport = Port} = State) ->
	{reply, {TMod, Fd, Ip, Port}, State};

handle_call(Request, From, State) ->
	{reply, ok, State}.

%%
%% Other side's RTP handling - we should send it downstream
%%

handle_cast({Pkt, Ip, Port}, #state{rtp = Fd, ip = DefIp, rtpport = DefPort, tmod = TMod, txbytes = TxBytes, txpackets = TxPackets} = State) when is_binary(Pkt) ->
	% If it's binary then treat it like RTP
	send(TMod, Fd, Pkt, DefIp, DefPort, Ip, Port),
	{noreply, State#state{txbytes = TxBytes + size(Pkt) - 12, txpackets = TxPackets + 1}};
handle_cast(
	{#rtp{ssrc = OtherSSRC} = Pkt, Ip, Port},
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort, tmod = TMod, process_chain_down = Chain, other_ssrc = OtherSSRC, txbytes = TxBytes, txpackets = TxPackets} = State
) ->
	{NewPkt, NewState} = process_chain(Chain, Pkt, State),
	send(TMod, Fd, NewPkt, DefIp, DefPort, Ip, Port),
	{noreply, NewState#state{txbytes = TxBytes + size(NewPkt) - 12, txpackets = TxPackets + 1}};
handle_cast({#rtp{ssrc = OtherSSRC} = Pkt, Ip, Port}, #state{other_ssrc = null, zrtp = ZrtpFsm} = State) ->
	% Initial other party SSRC setup
	(ZrtpFsm == null) orelse gen_server:call(ZrtpFsm, {ssrc, OtherSSRC}),
	handle_cast({Pkt, Ip, Port}, State#state{other_ssrc = OtherSSRC});
handle_cast({#rtp{ssrc = OtherSSRC} = Pkt, Ip, Port}, #state{other_ssrc = OtherSSRC2} = State) ->
	% Changed SSRC on the other side
	error_logger:warning_msg("gen_rtp SSRC changed from [~p] to [~p] (call transfer/music-on-hold?)", [OtherSSRC2, OtherSSRC]),
	% FIXME needs ZRTP reset here
	handle_cast({Pkt, Ip, Port}, State#state{other_ssrc = OtherSSRC});

%%
%% Other side's RTCP handling - we should send it downstream
%%

handle_cast(
	{#rtcp{} = Pkt, Ip, Port},
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort, tmod = TMod, mux = true} = State
) ->
	% If muxing is enabled (either explicitly or with a 'auto' parameter
	% then send RTCP muxed within RTP stream
	NewPkt = rtcp:encode(Pkt),
	send(TMod, Fd, NewPkt, DefIp, DefPort, Ip, Port),
	{noreply, State};
handle_cast(
	{#rtcp{} = Pkt, Ip, Port},
	#state{rtcp = Fd, ip = DefIp, rtcpport = DefPort, tmod = TMod} = State
) ->
	NewPkt = rtcp:encode(Pkt),
	send(TMod, Fd, NewPkt, DefIp, DefPort, Ip, Port),
	{noreply, State};

%%
%% Other side's RTCP handling - we should send it downstream
%%

handle_cast(
	{#zrtp{} = Pkt, Ip, Port},
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort, tmod = TMod} = State
) ->
	send(TMod, Fd, zrtp:encode(Pkt), DefIp, DefPort, Ip, Port),
	{noreply, State};

handle_cast({update, Params}, State) ->
	SendRecvStrategy = get_send_recv_strategy(Params),
	{PreIp, PrePort} = proplists:get_value(prefill, Params, {null, null}),
	% Re-set parameters
	{noreply, State#state{sendrecv = SendRecvStrategy, ip = PreIp, rtpport = PrePort}};

handle_cast({keepalive, enable}, State) ->
	{noreply, State#state{keepalive = true}};
handle_cast({keepalive, disable}, State) ->
	{noreply, State#state{keepalive = false}};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Request, State) ->
	error_logger:error_msg("gen_rtp unmatched cast [~p] STATE[~p]", [Request, State]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, #state{rtp = Fd0, rtcp = Fd1, tmod = TMod, tref = TRef, encoder = Encoder, decoder = Decoder}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	timer:cancel(TRef),
	TMod:close(Fd0),
	% FIXME We must send RTCP bye here
	TMod:close(Fd1),
	case Encoder of
		false -> ok;
		{_, E} -> codec:close(E)
	end,
	case Decoder of
		false -> ok;
		{_, D} -> codec:close(D)
	end,
	error_logger:error_msg("gen_rtp ~p: terminated due to reason [~p] (allocated ~b bytes)", [self(), Reason, Bytes]).

%% Handle short-circuit RTP message
handle_info({#rtp{} = Msg, Ip, Port}, State) ->
	handle_cast({Msg, Ip, Port}, State);
handle_info({Msg, Ip, Port}, State) when is_binary(Msg) ->
	handle_cast({Msg, Ip, Port}, State);

%% Handle incoming RTP message
handle_info({udp, Fd, Ip, Port, Msg}, #state{active = true} = State) ->
	NewState = process_data(Fd, Ip, Port, Msg, State),
	{noreply, NewState};
handle_info({udp, Fd, Ip, Port, Msg}, #state{active = once} = State) ->
	% FIXME should we move it 1 line below?
	inet:setopts(Fd, [{active, once}]),
	NewState = process_data(Fd, Ip, Port, Msg, State),
	{noreply, NewState};

handle_info(pre_interim_update, #state{tref = TRef, timeout = Timeout} = State) ->
	timer:cancel(TRef),
	{ok, T} = timer:send_interval(1000, interim_update),
	handle_info(interim_update, State#state{tref = T});
handle_info(interim_update, #state{keepalive = false} = State) ->
	error_logger:error_msg("gen_rtp_channel ignore timeout"),
	{noreply, State};
handle_info(interim_update, #state{parent = Parent, timeout = Timeout, lastseen = LS, keepalive = KA, counter = C} = State) ->
	Now = os:timestamp(),
	C == 0 andalso (Parent ! interim_update),
	case timer:now_diff(Now, LS) div 1000 < Timeout of
		true -> {noreply, State#state{counter = case C of 0 -> Timeout div 1000; _ -> C - 1 end}};
		false -> {stop, timeout, State}
	end;

handle_info({init, Params}, State) ->
	% Choose udp, tcp, sctp, dccp - FIXME only udp is supported
	TMod = proplists:get_value(transport, Params, gen_udp),
	SockParams = proplists:get_value(sockparams, Params, []),
	ActiveStrategy = proplists:get_value(active, Params, once),
	% Either specify IPv4 or IPv6 explicitly or provide two special
	% values - "::" for any available IPv6 or "0.0.0.0" or "0" for
	% any available IPv4.
	IpAddr = proplists:get_value(ip, Params, {0,0,0,0}),
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

	% Don't start timer if timeout value is set to zero
	{Timeout, TRef} = case proplists:get_value(timeout, Params, ?INTERIM_UPDATE) of
		0 -> {0, null};
		TimeoutMain ->
			TimeoutEarly = proplists:get_value(timeout_early, Params, ?INTERIM_UPDATE),
			{ok, T} = timer:send_interval(TimeoutEarly, pre_interim_update),
			{TimeoutMain, T}
	end,

	{Fd0, Fd1} = get_fd_pair({TMod, IpAddr, IpPort, [{active, ActiveStrategy} | SockParams]}),

	Parent = proplists:get_value(parent, Params),

	{ok, {Ip, PortRtp}} = inet:sockname(Fd0),
	{ok, {Ip, PortRtcp}} = inet:sockname(Fd1),

	% Notify parent
	Parent ! {phy, {Ip, PortRtp, PortRtcp, self()}},

	% Select crypto scheme (none, srtp, zrtp)
	Ctx = proplists:get_value(ctx, Params, none),

	% Enable/disable transcoding
	Transcoding = proplists:get_value(transcode, Params, none),

	% Either get explicit SRTP params or rely on ZRTP (which needs SSRC and ZID at least)
	% FIXME FIXME FIXME
%	{Zrtp, CtxI, CtxO, SSRC, OtherSSRC, FunEncode, FunDecode} = case Ctx of
%		none ->
%			{null, null, null, null, null, [fun rtp_encode/2], [fun rtp_decode/2]};
%		zrtp ->
%			{ok, ZrtpFsm} = zrtp_fsm:start_link([self()]),
%			{ZrtpFsm, passthru, passthru, null, null, [fun srtp_encode/2], [fun srtp_decode/2]};
%		{{SI, CipherI, AuthI, AuthLenI, KeyI, SaltI}, {SR, CipherR, AuthR, AuthLenR, KeyR, SaltR}} ->
%			CI = srtp:new_ctx(SI, CipherI, AuthI, KeyI, SaltI, AuthLenI),
%			CR = srtp:new_ctx(SR, CipherR, AuthR, KeyR, SaltR, AuthLenR),
%			{null, CI, CR, SI, SR, [fun srtp_encode/2], [fun srtp_decode/2]}
%	end,

	% Shall we entirely parse Rtp?
	% In case of re-packetization or transcoding or crypto we require it anyway
	RebuildRtp = proplists:get_value(rebuildrtp, Params, false),

	{FunDecode, FunEncode} = case RebuildRtp or (Ctx /= none) or (Transcoding /= none)  of
		false ->
			{[], []};
		true ->
			{[fun rtp_decode/2], [fun rtp_encode/2]}
	end,

	{PreIp, PrePort} = proplists:get_value(prefill, Params, {null, null}),

	% Set DTMF ID mapping
	Dtmf = proplists:get_value(dtmf, Params, null),
	Dtmf /= null andalso put(Dtmf, dtmf),

	% Set codec ID mapping
	lists:foreach(fun({Key, Val}) -> put(Key, Val) end, proplists:get_value(cmap, Params, [])),

	% FIXME
	{Encoder, FunTranscode} = case Transcoding of
		none -> {false, []};
		EncoderDesc ->
			case codec:start_link(EncoderDesc) of
				{stop,unsupported} ->
					{false, []};
				{ok, C} ->
					{{rtp_utils:get_payload_from_codec(EncoderDesc), C}, [fun transcode/2]}
			end
	end,

	{noreply, #state{
			parent = Parent,
			rtp_subscriber = Parent,
			rtp = Fd0,
			rtcp = Fd1,
			ip = PreIp,
			rtpport = PrePort,
%			zrtp = Zrtp,
%			ctxI = CtxI,
%			ctxO = CtxO,
%			ssrc = SSRC,
%			other_ssrc = OtherSSRC,
			% FIXME - properly set transport
			tmod = TMod,
			active = ActiveStrategy,
			process_chain_up = FunDecode,
			process_chain_down = FunTranscode ++ FunEncode,
			encoder = Encoder,
			mux = MuxRtpRtcp,
			sendrecv = SendRecvStrategy,
			tref = TRef,
			timeout = Timeout
		}
	};

handle_info(Info, State) ->
	error_logger:error_msg("gen_rtp unmatched info [~p]", [Info]),
	{noreply, State}.

%%
%% Private functions
%%

process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{parent = Parent, rtp_subscriber = Subscriber, sendrecv = SendRecv, process_chain_up = [], rxbytes = RxBytes, rxpackets = RxPackets} = State) when PType =< 34; 96 =< PType ->
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			send_subscriber(Parent, Subscriber, Msg, Ip, Port),
			State#state{lastseen = os:timestamp(), ip = Ip, rtpport = Port, ssrc = SSRC, type = PType, rxbytes = RxBytes + size(Msg) - 12, rxpackets = RxPackets + 1};
		false ->
			State
	end;
process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{parent = Parent, rtp_subscriber = Subscriber, sendrecv = SendRecv, process_chain_up = Chain, rxbytes = RxBytes, rxpackets = RxPackets} = State) when PType =< 34; 96 =< PType ->
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{NewMsg, NewState} = process_chain(Chain, Msg, State),
			send_subscriber(Parent, Subscriber, NewMsg, Ip, Port),
			NewState#state{lastseen = os:timestamp(), ip = Ip, rtpport = Port, ssrc = SSRC, type = PType, rxbytes = RxBytes + size(Msg) - 12, rxpackets = RxPackets + 1};
		false ->
			State
	end;
%% Handle incoming RTCP message
process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{parent = Parent, sendrecv = SendRecv} = State) when 64 =< PType, PType =< 82 ->
	case SendRecv(Ip, Port, SSRC,  State#state.ip, State#state.rtcpport, State#state.ssrc) of
		true ->
			Mux = (State#state.mux == true) or ((State#state.rtpport == Port) and (State#state.mux == auto)),
			{ok, NewMsg} = rtcp:decode(Msg),
			Parent ! {NewMsg, Ip, Port},
			State#state{lastseen = os:timestamp(), ip = Ip, rtcpport = Port, mux = Mux, ssrc = SSRC};
		false ->
			State
	end;
%% Handle incoming ZRTP message
process_data(Fd, Ip, Port, <<?ZRTP_MARKER:16, _:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, _/binary>> = Msg, #state{parent = Parent, sendrecv = SendRecv} = State) ->
	% Treat ZRTP in the same way as RTP
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{ok, Zrtp} = zrtp:decode(Msg),
			case State#state.zrtp of
				% If we didn't setup ZRTP FSM then we are acting
				% as pass-thru ZRTP proxy
				null -> Parent ! {Zrtp, Ip, Port};
				ZrtpFsm -> gen_server:cast(self(), {gen_server:call(ZrtpFsm, Zrtp), Ip, Port})
			end,
			State#state{lastseen = os:timestamp(), ip = Ip, rtpport = Port, ssrc = SSRC};
		false ->
			State
	end;
%% Handle incoming STUN message
process_data(Fd, Ip, Port, <<?STUN_MARKER:2, _:30, ?STUN_MAGIC_COOKIE:32, _/binary>> = Msg, State) ->
	% FIXME this is a STUN message - we should reply at this level
	State;
%% Handle incoming UKNOWN message
process_data(_, _, _, _, State) ->
	State.

-define(SOL_SOCKET, 1).
-define(SO_NO_CHECK, 11).
-define(ON, <<1:32/native>>).

%% Open a pair of UDP ports - N and N+1 (for RTP and RTCP consequently)
get_fd_pair({TMod, {I0,I1,I2,I3,I4,I5,I6,I7} = IPv6, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 65536,
	is_integer(I1), 0 =< I1, I1 < 65536,
	is_integer(I2), 0 =< I2, I2 < 65536,
	is_integer(I3), 0 =< I3, I3 < 65536,
	is_integer(I4), 0 =< I4, I4 < 65536,
	is_integer(I5), 0 =< I5, I5 < 65536,
	is_integer(I6), 0 =< I6, I6 < 65536,
	is_integer(I7), 0 =< I7, I7 < 65536 ->
	get_fd_pair(TMod, IPv6, Port, proplists:delete(ipv6, SockParams) ++ [inet6], 10);
get_fd_pair({TMod, {I0,I1,I2,I3} = IPv4, Port, SockParams}) when
	is_integer(I0), 0 =< I0, I0 < 256,
	is_integer(I1), 0 =< I1, I1 < 256,
	is_integer(I2), 0 =< I2, I2 < 256,
	is_integer(I3), 0 =< I3, I3 < 256 ->
	get_fd_pair(TMod, IPv4, Port, proplists:delete(ipv6, SockParams), 10).

get_fd_pair(_, I, P, SockParams, 0) ->
	error_logger:error_msg("Create new socket at ~s:~b FAILED (~p)", [inet_parse:ntoa(I), P,  SockParams]),
	error;
get_fd_pair(gen_udp, I, P, SockParams, NTry) ->
	case gen_udp:open(P, [binary, {ip, I}, {raw, ?SOL_SOCKET, ?SO_NO_CHECK, ?ON} |  SockParams]) of
		{ok, Fd} ->
			{ok, {Ip,Port}} = inet:sockname(Fd),
			Port2 = case Port rem 2 of
				0 -> Port + 1;
				1 -> Port - 1
			end,
			case gen_udp:open(Port2, [binary, {ip, Ip}, {raw, ?SOL_SOCKET, ?SO_NO_CHECK, ?ON} |  SockParams]) of
				{ok, Fd2} ->
					if
						Port > Port2 -> {Fd2, Fd};
						Port < Port2 -> {Fd, Fd2}
					end;
				{error, _} ->
					gen_udp:close(Fd),
					get_fd_pair(gen_udp, I, P, SockParams, NTry - 1)
			end;
		{error, _} ->
			get_fd_pair(gen_udp, I, P, SockParams, NTry - 1)
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
send_recv_roaming(Ip, Port, SSRC, _, _, null) -> true;
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

process_chain([], Pkt, State) ->
	{Pkt, State};
process_chain([Fun|Funs], Pkt, State) ->
	{NewPkt, NewState} = Fun(Pkt, State),
	process_chain(Funs, NewPkt, NewState).

rtp_encode(Pkt, S) ->
	{rtp:encode(Pkt), S}.
rtp_decode(Pkt, S) ->
	{ok, NewPkt} = rtp:decode(Pkt),
	{NewPkt, S}.

srtp_encode(Pkt, State = #state{ctxO = Ctx}) ->
	{ok, NewPkt, NewCtx} = srtp:encrypt(Pkt, Ctx),
	{NewPkt, State#state{ctxO = NewCtx}}.
srtp_decode(Pkt, State = #state{ctxI = Ctx}) ->
	{ok, NewPkt, NewCtx} = srtp:decrypt(Pkt, Ctx),
	{NewPkt, State#state{ctxI = NewCtx}}.

transcode(#rtp{payload_type = PayloadType} = Rtp, State = #state{encoder = {PayloadType, _}}) ->
	{Rtp, State};
transcode(#rtp{payload_type = OldPayloadType, payload = Payload} = Rtp, State = #state{encoder = {PayloadType, Encoder}, decoder = {OldPayloadType, Decoder}}) ->
	{ok, RawData} = codec:decode(Decoder, Payload),
	{ok, NewPayload} = codec:encode(Encoder, RawData),
	{Rtp#rtp{payload_type = PayloadType, payload = NewPayload}, State};
transcode(#rtp{payload_type = OldPayloadType, payload = Payload} = Rtp, State = #state{encoder = {PayloadType, Encoder}, decoder = {DifferentPayloadType, Decoder}}) ->
	case codec:is_supported(rtp_utils:get_codec_from_payload(OldPayloadType)) of
		true ->
			error_logger:warning_msg("New payload ~b found while transcoding (was ~b)~n", [OldPayloadType, DifferentPayloadType]),
			codec:close(Decoder),
			transcode(Rtp, State#state{decoder = false});
		_ ->
			error_logger:error_msg("Unsupported payload ~b found while transcoding~n", [OldPayloadType]),
			{Rtp, State}
	end;
transcode(#rtp{payload_type = OldPayloadType} = Rtp, State = #state{decoder = false}) ->
	case codec:start_link(rtp_utils:get_codec_from_payload(OldPayloadType)) of
		{stop,unsupported} ->
			error_logger:error_msg("Cannot start decoder for payload ~b~n", [OldPayloadType]),
			{Rtp, State};
		{ok, Decoder} ->
			transcode(Rtp, State#state{decoder = {OldPayloadType, Decoder}})
	end;
transcode(Pkt, State) ->
	{Pkt, State}.

send(gen_udp, _, _, _, null, _, null) ->
	ok;
send(gen_udp, Fd, Pkt, Ip, Port, null, null) ->
	prim_inet:sendto(Fd, Ip, Port, Pkt);
send(gen_udp, Fd, Pkt, _, _, Ip, Port) ->
	prim_inet:sendto(Fd, Ip, Port, Pkt);
send(gen_tcp, Fd, Pkt, _, _, _, _) ->
	prim_inet:send(Fd, Pkt, []).

send_subscriber(_, null, _, _, _) ->
		ok;
send_subscriber(Parent, Subscribers, Data, Ip, Port) when is_list(Subscribers) ->
		lists:foreach(fun(X) -> send_subscriber(Parent, X, Data, Ip, Port) end, Subscribers);
send_subscriber(Subscriber, Subscriber, Data, Ip, Port) ->
		Subscriber ! {Data, Ip, Port};
send_subscriber(_, {Type, Fd, Ip, Port}, Data, _, _) ->
		send(Type, Fd, Data, Ip, Port, null, null);
send_subscriber(_, Subscriber, Data, _, _) ->
		Subscriber ! {Data, null, null}.

append_subscriber(null, Subscriber) -> Subscriber;
append_subscriber(Subscribers, Subscriber) when is_list(Subscribers) -> [S || S <- Subscribers, S /= Subscriber] ++ [Subscriber];
append_subscriber(Subscriber, Subscriber) -> Subscriber;
append_subscriber(OldSubscriber, Subscriber) -> [OldSubscriber, Subscriber].
