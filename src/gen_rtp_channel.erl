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
		rtp_subscriber = null,
		rtp,
		ip = null,
		rtpport = null,
		rtcpport = null,
		local,
		peer = null,
		ssrc = null,
		type,
		rxbytes = 0,
		rxpackets = 0,
		txbytes = 0,
		txpackets = 0,
		sr = null,
		rr = null,
		sendrecv,
		zrtp = null,
		ctxI = passthru,
		ctxO = passthru,
		other_ssrc = null,
		process_chain_up = [],
		process_chain_down = [],
		encoder = false,
		decoder = false,
		% If set to true then we'll have another one INTERIM_UPDATE
		% interval to wait for initial data
		keepalive = true,
		timeout = ?INTERIM_UPDATE,
		counter = 0
	}
).

open(Port) ->
	open(Port, []).
open(Port, Params) ->
	gen_server:start_link(?MODULE, [Params ++ [{port, Port}]], []).
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

handle_call(get_stats, _, #state{rtp = Port, ip = Ip, rtpport = RtpPort, rtcpport = RtcpPort, local = Local, sr = Sr, rr = Rr} = State) ->
	<<SSRC:32, Type:8, RxBytes:32, RxPackets:32, TxBytes:32, TxPackets:32, TxBytes2:32, TxPackets2:32>> = port_control(Port, 5, <<>>),
	{reply, {Local, {Ip, RtpPort, RtcpPort}, SSRC, Type, RxBytes, RxPackets, TxBytes, TxPackets, TxBytes2, TxPackets2, Sr, Rr}, State};

handle_call({rtp_subscriber, {set, Subscriber}}, _, #state{peer = null} = State) ->
	{reply, ok, State#state{rtp_subscriber = Subscriber}};
handle_call({rtp_subscriber, {set, null}}, _, State) ->
	{reply, ok, State#state{rtp_subscriber = null}};
handle_call({rtp_subscriber, {set, Subscriber}}, _, #state{peer = {PosixFd, {I0, I1, I2, I3} = Ip, Port}} = State) ->
	gen_server:call(Subscriber, {set_fd, <<PosixFd:32, Port:16, I0:8, I1:8, I2:8, I3:8>>}),
	{reply, ok, State#state{rtp_subscriber = Subscriber}};
handle_call({rtp_subscriber, {add, Subscriber}}, _, #state{rtp_subscriber = OldSubscriber} = State) ->
	{reply, ok, State#state{rtp_subscriber = append_subscriber(OldSubscriber, Subscriber)}};

handle_call(get_phy, _, #state{rtp = Fd, ip = Ip, rtpport = RtpPort, rtcpport = RtcpPort, local = Local} = State) ->
	{reply, {Fd, Local, {Ip, RtpPort, RtcpPort}}, State};

handle_call({set_fd, Bin}, _, #state{rtp = Fd} = State) ->
	port_control(Fd, 4, Bin),
	{reply, ok, State};

handle_call(Request, From, State) ->
	{reply, ok, State}.

%%
%% Other side's RTP handling - we should send it downstream
%%

handle_cast({Pkt, Ip, Port}, #state{rtp = Fd, ip = DefIp, rtpport = DefPort, txbytes = TxBytes, txpackets = TxPackets} = State) when is_binary(Pkt) ->
	% If it's binary then treat it like RTP
	Fd ! {self(), {command, Pkt}},
	{noreply, State#state{txbytes = TxBytes + size(Pkt) - 12, txpackets = TxPackets + 1}};
handle_cast(
	{#rtp{ssrc = OtherSSRC} = Pkt, Ip, Port},
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort, process_chain_down = Chain, other_ssrc = OtherSSRC, txbytes = TxBytes, txpackets = TxPackets} = State
) ->
	{NewPkt, NewState} = process_chain(Chain, Pkt, State),
	Fd ! {self(), {command, Pkt}},
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
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort} = State
) ->
	NewPkt = rtcp:encode(Pkt),
	Fd ! {self(), {command, Pkt}},
	{noreply, State};

%%
%% Other side's ZRTP handling - we should send it downstream
%%

handle_cast(
	{#zrtp{} = Pkt, Ip, Port},
	#state{rtp = Fd, ip = DefIp, rtpport = DefPort} = State
) ->
	Fd ! {self(), {command, Pkt}},
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

terminate(Reason, #state{rtp = Port, encoder = Encoder, decoder = Decoder}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	% FIXME We must send RTCP bye here
	Port == undefined orelse port_close(Port),
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
handle_info({#rtcp{} = Msg, Ip, Port}, State) ->
	handle_cast({Msg, Ip, Port}, State);
handle_info({#zrtp{} = Msg, Ip, Port}, State) ->
	handle_cast({Msg, Ip, Port}, State);
handle_info({Msg, Ip, Port}, State) when is_binary(Msg) ->
	handle_cast({Msg, Ip, Port}, State);

%% Handle incoming RTP message
handle_info({rtp, Fd, Ip, Port, Msg}, #state{rtp_subscriber = Subscriber} = State) ->
	NewState = process_data(Fd, Ip, Port, Msg, State),
	{noreply, NewState};
handle_info({rtcp, Fd, Ip, Port, Msg}, State) ->
	NewState = process_data(Fd, Ip, Port, Msg, State),
	{noreply, NewState};
handle_info({udp, Fd, Ip, Port, Msg}, State) ->
	NewState = process_data(Fd, Ip, Port, Msg, State),
	{noreply, NewState};

handle_info({peer, PosixFd, Ip, Port}, #state{rtp_subscriber = null} = State) ->
	{noreply, State#state{peer = {PosixFd, Ip, Port}}};
handle_info({peer, PosixFd, {I0, I1, I2, I3} = Ip, Port}, #state{rtp_subscriber = Subscriber} = State) ->
	gen_server:call(Subscriber, {set_fd, <<PosixFd:32, Port:16, I0:8, I1:8, I2:8, I3:8>>}),
	{noreply, State#state{peer = {PosixFd, Ip, Port}}};

handle_info({timeout, _Port}, #state{keepalive = false} = State) ->
	error_logger:error_msg("gen_rtp_channel ignore timeout"),
	{noreply, State};
handle_info({timeout, _Port}, State) ->
	{stop, timeout, State};

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
	TimeoutMain = proplists:get_value(timeout, Params, ?INTERIM_UPDATE),
	TimeoutEarly = proplists:get_value(timeout_early, Params, ?INTERIM_UPDATE),

	load_library(rtp_drv),
	Port = open_port({spawn, rtp_drv}, [binary]),
	{I0, I1, I2, I3} = IpAddr,
	erlang:port_control(Port, 1, <<IpPort:16, 4:8, I0:8, I1:8, I2:8, I3:8, TimeoutEarly:32, TimeoutMain:32>>),
	<<I0:8, I1:8, I2:8, I3:8, RtpPort:16, RtcpPort:16>> = port_control(Port, 2, <<>>),
	erlang:port_set_data(Port, inet_udp),

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
	{FunDecode, FunEncode} = case (Ctx /= none) or (Transcoding /= none)  of
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
			rtp_subscriber = null,
			rtp = Port,
			ip = PreIp,
			rtpport = PrePort,
			local = {{I0, I1, I2, I3}, RtpPort, RtcpPort},
%			zrtp = Zrtp,
%			ctxI = CtxI,
%			ctxO = CtxO,
%			ssrc = SSRC,
%			other_ssrc = OtherSSRC,
			process_chain_up = FunDecode,
			process_chain_down = FunTranscode ++ FunEncode,
			encoder = Encoder,
			sendrecv = SendRecvStrategy,
			timeout = ?INTERIM_UPDATE
		}
	};

handle_info(Info, State) ->
	error_logger:error_msg("gen_rtp unmatched info [~p]", [Info]),
	{noreply, State}.

%%
%% Private functions
%%

%% Handle incoming RTP message
process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{rtp_subscriber = Subscriber, sendrecv = SendRecv, process_chain_up = [], rxbytes = RxBytes, rxpackets = RxPackets} = State) when PType =< 34; 96 =< PType ->
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			send_subscriber(Subscriber, Msg, Ip, Port),
			State#state{ip = Ip, rtpport = Port, ssrc = SSRC, type = PType, rxbytes = RxBytes + size(Msg) - 12, rxpackets = RxPackets + 1};
		false ->
			State
	end;
process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{rtp_subscriber = Subscriber, sendrecv = SendRecv, process_chain_up = Chain, rxbytes = RxBytes, rxpackets = RxPackets} = State) when PType =< 34; 96 =< PType ->
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{NewMsg, NewState} = process_chain(Chain, Msg, State),
			send_subscriber(Subscriber, NewMsg, Ip, Port),
			NewState#state{ip = Ip, rtpport = Port, ssrc = SSRC, type = PType, rxbytes = RxBytes + size(Msg) - 12, rxpackets = RxPackets + 1};
		false ->
			State
	end;
%% Handle incoming RTCP message
process_data(Fd, Ip, Port, <<?RTP_VERSION:2, _:7, PType:7, _:48, SSRC:32, _/binary>> = Msg, #state{rtp_subscriber = Subscriber, sendrecv = SendRecv, rr = Rr0, sr = Sr0} = State) when 64 =< PType, PType =< 82 ->
	case SendRecv(Ip, Port, SSRC,  State#state.ip, State#state.rtcpport, State#state.ssrc) of
		true ->
			{ok, #rtcp{payloads = Rtcps} = NewMsg} = rtcp:decode(Msg),
			send_subscriber(Subscriber, NewMsg, Ip, Port),
			% FIXME make a ring buffer
			Sr = rtp_utils:take(Rtcps, sr),
			Rr = rtp_utils:take(Rtcps, rr),
			State#state{ip = Ip, rtcpport = Port, ssrc = SSRC, sr = case Sr of false -> Sr0; _ -> Sr end, rr = case Rr of false -> Rr0; _ -> Rr end};
		false ->
			State
	end;
%% Handle incoming ZRTP message
process_data(Fd, Ip, Port, <<?ZRTP_MARKER:16, _:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, _/binary>> = Msg, #state{rtp_subscriber = Subscriber, sendrecv = SendRecv} = State) ->
	% Treat ZRTP in the same way as RTP
	case SendRecv(Ip, Port, SSRC, State#state.ip, State#state.rtpport, State#state.ssrc) of
		true ->
			{ok, Zrtp} = zrtp:decode(Msg),
			case State#state.zrtp of
				% If we didn't setup ZRTP FSM then we are acting
				% as pass-thru ZRTP proxy
				null -> send_subscriber(Subscriber, Zrtp, Ip, Port);
				ZrtpFsm -> gen_server:cast(self(), {gen_server:call(ZrtpFsm, Zrtp), Ip, Port})
			end,
			State#state{ip = Ip, rtpport = Port, ssrc = SSRC};
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

send_subscriber(null, _, _, _) ->
		ok;
send_subscriber(Subscribers, Data, Ip, Port) when is_list(Subscribers) ->
		lists:foreach(fun(X) -> send_subscriber(X, Data, Ip, Port) end, Subscribers);
%send_subscriber(Subscriber, Data, Ip, Port) ->
%		Subscriber ! {Data, Ip, Port};
send_subscriber({Type, Fd, Ip, Port}, Pkt, _, _) ->
		Fd ! {self(), {command, Pkt}};
send_subscriber(Subscriber, Pkt, _, _) ->
		Subscriber ! {Pkt, null, null}.

append_subscriber(null, Subscriber) -> Subscriber;
append_subscriber(Subscribers, Subscriber) when is_list(Subscribers) -> [S || S <- Subscribers, S /= Subscriber] ++ [Subscriber];
append_subscriber(Subscriber, Subscriber) -> Subscriber;
append_subscriber(OldSubscriber, Subscriber) -> [OldSubscriber, Subscriber].

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
