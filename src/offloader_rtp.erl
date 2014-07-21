-module(offloader_rtp).
-behaviour(gen_server).
-compile(export_all).

-include("../include/offloader_rtp.hrl").

-record(state, {
	port = null,
	tableid = $0
}).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Params) ->
	process_flag(trap_exit, true),

	% Deferred init
	self() ! {init, Params},

	{ok, #state{}}.

handle_call(#mediaproxy_message{} = Msg, _From, #state{port = Fd} = State) ->
	ok = file:write(Fd, message_to_binary(Msg)),
	{reply, ok, State};

handle_call(Call, _From, State) ->
	error_logger:error_msg("offloader_rtp: unmatched call [~p]", [Call]),
	{stop, {error, {unknown_call, Call}}, State}.

handle_cast(#mediaproxy_message{} = Msg, #state{port = Fd} = State) ->
	ok = file:write(Fd, message_to_binary(Msg)),
	{noreply, State};

handle_cast(Cast, State) ->
	error_logger:error_msg("offloader_rtp: unmatched cast [~p]", [Cast]),
	{stop, {error, {unknown_cast, Cast}}, State}.

handle_info({init, Params}, _) ->
	% Open a control socket
	{ok, Td} = file:open("/proc/mediaproxy/control", [write]),

	% Get a Table ID if provided
	TableId = proplists:get_value(tableid, Params, 0) + $0, % Poor-man's integer to char
	% Make a table
	ok = file:write(Td, <<"add ", TableId:8, "\n">>),

	% We don't need a control socket here
	ok = file:close(Td),

	% Open a newly created table
	{ok, Fd} = file:open( <<"/proc/mediaproxy/", TableId:8, "/control">>, [write]),

	% Ping our connection
	ok = file:write(Fd, message_to_binary(#mediaproxy_message{})),

	error_logger:info_msg("~s: started at ~p.~n", [?MODULE, node()]),

	{noreply, #state{port = Fd, tableid = TableId}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{port = Fd}) ->
	{memory, Bytes} = erlang:process_info(self(), memory),
	file:close(Fd),
	error_logger:warning_msg("offloader_rtp: terminated due to reason [~p] (allocated ~b bytes)", [Reason, Bytes]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% gen_server Private Definitions
%% ------------------------------------------------------------------

% 224 bytes
message_to_binary(#mediaproxy_message{cmd = noop, target = Target}) ->
	Binary = target_to_binary(Target),
	<<?MEDIAPROXY_CMD_NOOP:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>;
message_to_binary(#mediaproxy_message{cmd = add, target = Target}) ->
	Binary = target_to_binary(Target),
	<<?MEDIAPROXY_CMD_ADD:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>;
message_to_binary(#mediaproxy_message{cmd = del, target = Target}) ->
	Binary = target_to_binary(Target),
	<<?MEDIAPROXY_CMD_DEL:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>;
message_to_binary(#mediaproxy_message{cmd = update, target = Target}) ->
	Binary = target_to_binary(Target),
	<<?MEDIAPROXY_CMD_UPDATE:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>.

% 216 bytes
target_to_binary(#mediaproxy_target_info{target_port = TP, src_addr = SA, dst_addr = DA, mirror_addr = MA, decrypt = D, encrypt = E, tos = T, rtcp_mux = RM}) ->
	SABin = address_to_binary(SA),
	DABin = address_to_binary(DA),
	MABin = address_to_binary(MA),
	DBin = srtp_to_binary(D),
	EBin = srtp_to_binary(E),
	<<TP:16/little, 0:16, SABin:24/binary, DABin:24/binary, MABin:24/binary, 0:32, DBin:64/binary, EBin:64/binary, T:8/little, 0:24, (case RM of false -> 0; _ -> 1 end):8, 0:24>>;
target_to_binary(null) ->
	<<0:8/little-unsigned-integer-unit:216>>.



% 24 bytes
address_to_binary(#mediaproxy_address{ip = {I0, I1, I2, I3}, port = Port}) ->
	<<?NFPROTO_IPV4:32/little, I0:8, I1:8, I2:8, I3:8, 0:96, Port:16/little, 0:16>>;
address_to_binary(#mediaproxy_address{ip = {I0, I1, I2, I3, I4, I5, I6, I7}, port = Port}) ->
	<<?NFPROTO_IPV6:32/little, I0:16/little, I1:16/little, I2:16/little, I3:16/little, I4:16/little, I5:16/little, I6:16/little, I7:16/little, Port:16/little, 0:16>>;
address_to_binary(#mediaproxy_address{ip = null}) ->
	<<0:8/little-unsigned-integer-unit:24>>;
address_to_binary(null) ->
	<<0:8/little-unsigned-integer-unit:24>>.

% 64 bytes
srtp_to_binary(#mediaproxy_srtp{cipher = C, hmac = H, master_key = MK, master_salt = MS, mki = MKI, last_index = LI, auth_tag_len = ATL, mki_len = MKIL}) ->
	CBin = cipher_to_binary(C),
	HBin = hmac_to_binary(H),
	<<CBin:4/binary, HBin:4/binary, MK:16/binary, MS:14/binary, 0:16, MKI:64/little, LI:64/little, ATL:32/little, MKIL:32/little>>;
srtp_to_binary(null) ->
	<<0:8/little-unsigned-integer-unit:64>>.

cipher_to_binary(invalid) -> <<0:32/little>>;
cipher_to_binary(null) -> <<1:32/little>>;
cipher_to_binary(aes_cm) -> <<2:32/little>>;
cipher_to_binary(aes_f8) -> <<3:32/little>>.

hmac_to_binary(invalid) -> <<0:32/little>>;
hmac_to_binary(null) -> <<1:32/little>>;
hmac_to_binary(sha1) -> <<2:32/little>>.

binary_to_message(<<?MEDIAPROXY_CMD_NOOP:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>) ->
	{ok, Target} = binary_to_target(Binary),
	{ok, #mediaproxy_message{cmd = noop, target = Target}};
binary_to_message(<<?MEDIAPROXY_CMD_ADD:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>) ->
	{ok, Target} = binary_to_target(Binary),
	{ok, #mediaproxy_message{cmd = add, target = Target}};
binary_to_message(<<?MEDIAPROXY_CMD_DEL:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>) ->
	{ok, Target} = binary_to_target(Binary),
	{ok, #mediaproxy_message{cmd = del, target = Target}};
binary_to_message(<<?MEDIAPROXY_CMD_UPDATE:8/little-unsigned-integer-unit:1, 0:56, Binary:216/binary>>) ->
	{ok, Target} = binary_to_target(Binary),
	{ok, #mediaproxy_message{cmd = update, target = Target}}.

binary_to_target(<<0:8/little-unsigned-integer-unit:216>>) ->
	{ok, null};
binary_to_target(<<TP:16/little, 0:16, SABin:24/binary, DABin:24/binary, MABin:24/binary, 0:32, DBin:64/binary, EBin:64/binary, T:8/little, 0:24, RMBin:8, 0:24>>) ->
	{ok, SA} = binary_to_address(SABin),
	{ok, DA} = binary_to_address(DABin),
	{ok, MA} = binary_to_address(MABin),
	{ok, D} = binary_to_srtp(DBin),
	{ok, E} = binary_to_srtp(EBin),
	RM = case RMBin of 0 -> false; _ -> true end,
	{ok, #mediaproxy_target_info{target_port = TP, src_addr = SA, dst_addr = DA, mirror_addr = MA, decrypt = D, encrypt = E, tos = T, rtcp_mux = RM}}.


binary_to_address(<<0:192>>) ->
	% FIXME
%	{ok, #mediaproxy_address{ip = null, port = 0}};
	{ok, null};
binary_to_address(<<?NFPROTO_IPV4:32/little, I0:8, I1:8, I2:8, I3:8, 0:96, Port:16/little, 0:16>>) ->
	{ok, #mediaproxy_address{ip = {I0, I1, I2, I3}, port = Port}};
binary_to_address(<<?NFPROTO_IPV6:32/little, I0:16/little, I1:16/little, I2:16/little, I3:16/little, I4:16/little, I5:16/little, I6:16/little, I7:16/little, Port:16/little, 0:16>>) ->
	{ok, #mediaproxy_address{ip = {I0, I1, I2, I3, I4, I5, I6, I7}, port = Port}}.

binary_to_srtp(<<0:8/little-unsigned-integer-unit:64>>) ->
	{ok, #mediaproxy_srtp{}};
binary_to_srtp(<<CBin:4/binary, HBin:4/binary, MK:16/binary, MS:14/binary, 0:16, MKI:64/little, LI:64/little, ATL:32/little, MKIL:32/little>>) ->
	C = binary_to_cipher(CBin),
	H = binary_to_hmac(HBin),
	{ok, #mediaproxy_srtp{cipher = C, hmac = H, master_key = MK, master_salt = MS, mki = MKI, last_index = LI, auth_tag_len = ATL, mki_len = MKIL}}.

binary_to_cipher(<<0:32/little>>) -> invalid;
binary_to_cipher(<<1:32/little>>) -> null;
binary_to_cipher(<<2:32/little>>) -> aes_cm;
binary_to_cipher(<<3:32/little>>) -> aes_f8.

binary_to_hmac(<<0:32/little>>) -> invalid;
binary_to_hmac(<<1:32/little>>) -> null;
binary_to_hmac(<<2:32/little>>) -> sha1.
