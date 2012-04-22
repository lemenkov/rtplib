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

-module(stun).
-author('lemenkov@gmail.com').

% FIXME - don't forget to remove from final version!
-compile(export_all).

-include("../include/stun.hrl").

decode(<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?STUN_MAGIC_COOKIE:32, TransactionID:96, Rest/binary>> = StunBinary) ->
	Method = case <<M0:5, M1:3, M2:4>> of
		<<1:12>> -> binding;
		<<3:12>> -> allocate; % TURN
		<<4:12>> -> refresh; % TURN
		<<6:12>> -> send; % TURN
		<<7:12>> -> data; % TURN
		<<8:12>> -> createperm; % TURN
		<<9:12>> -> channelbind; % TURN

		<<Other:12>> -> Other
	end,
	Class = case <<C0:1, C1:1>> of
		<<0:1, 0:1>> -> request;
		<<0:1, 1:1>> -> indication;
		<<1:1, 0:1>> -> success;
		<<1:1, 1:1>> -> error
	end,
	{Rest2, Length2, Fingerprint} = case check_fingerprint(StunBinary) of
		{crc, {true, <<_:20/binary, Data/binary>>}} -> {Data, Length - 8, true};
		{crc, {false, _}} -> throw({error, bad_fingerprint});
		{nocrc, _} -> {Rest, Length, false}
	end,
	Attrs = decode_attrs(Rest2, Length2, TransactionID, []),
	{ok, #stun{class = Class, method = Method, transactionid = TransactionID, fingerprint = Fingerprint, attrs = Attrs}}.

encode(#stun{class = Class, method = Method, transactionid = TransactionID, fingerprint = Fingerprint, attrs = Attrs}) ->
	M = case Method of
		binding -> 1;
		allocate -> 3;
		refresh -> 4;
		send -> 6;
		data -> 7;
		createperm -> 8;
		channelbind -> 9;
		Other -> Other
	end,
	<<M0:5, M1:3, M2:4>> = <<M:12>>,
	<<C0:1, C1:1>> = case Class of
		request -><<0:1, 0:1>>;
		indication -> <<0:1, 1:1>>;
		success -> <<1:1, 0:1>>;
		error -> <<1:1, 1:1>>
	end,
	BinAttrs = << <<(encode_bin(encode_attr(T, V, TransactionID)))/binary>> || {T, V} <- Attrs >>,
	Length = size(BinAttrs),

	StunBinary = <<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?STUN_MAGIC_COOKIE:32, TransactionID:96, BinAttrs/binary>>,
	case Fingerprint of
		false -> StunBinary;
		_ ->  insert_fingerprint(StunBinary)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% STUN decoding helpers
%%

decode_attrs(<<>>, 0, _, Attrs) ->
	Attrs;
decode_attrs(<<>>, Length, _, Attrs) ->
	error_logger:warning_msg("STUN TLV wrong length [~p]~n", [Length]),
	Attrs;
decode_attrs(<<Type:16, ItemLength:16, Bin/binary>>, Length, TID, Attrs) ->
	PaddingLength = case ItemLength rem 4 of
		0 -> 0;
		Else ->
			case ItemLength == size(Bin) of
				true -> 0;
				_ -> 4 - Else
			end
	end,
	<<Value:ItemLength/binary, _:PaddingLength/binary, Rest/binary>> = Bin,
	{T,V} = case Type of
		16#0001 -> {'MAPPED-ADDRESS', decode_attr_addr(Value)};
		16#0002 -> {'RESPONSE-ADDRESS', decode_attr_addr(Value)}; % Obsolete
		16#0003 -> {'CHANGE-ADDRESS', decode_attr_addr(Value)}; % Obsolete
		16#0004 -> {'SOURCE-ADDRESS', decode_attr_addr(Value)}; % Obsolete
		16#0005 -> {'CHANGED-ADDRESS', decode_attr_addr(Value)}; % Obsolete
		16#0006 -> {'USERNAME', Value};
		16#0007 -> {'PASSWORD', Value}; % Obsolete
		16#0008 -> {'MESSAGE-INTEGRITY', Value};
		16#0009 -> {'ERROR-CODE', decode_attr_err(Value)};
		16#000a -> {'UNKNOWN-ATTRIBUTES', Value};
		16#000b -> {'REFLECTED-FROM', Value}; % Obsolete
		16#000c -> {'CHANNEL-NUMBER', Value}; % draft-ietf-behave-turn-10
		16#000d -> {'LIFETIME', Value}; % draft-ietf-behave-turn-10
		16#000e -> {'ALTERNATE-SERVER', decode_attr_addr(Value)}; % Obsolete -> 16#8023
		16#000f -> {'MAGIC-COOKIE', Value};
		16#0010 -> {'BANDWIDTH', Value}; % turn-07
		16#0011 -> {'DESTINATION-ADDRESS', decode_attr_addr(Value)};
		16#0012 -> {'XOR-PEER-ADDRESS', decode_attr_xaddr(Value, TID)}; % draft-ietf-behave-turn-10 - was REMOTE-ADDRESS
		16#0013 -> {'DATA', Value}; % draft-ietf-behave-turn-10
		16#0014 -> {'REALM', Value};
		16#0015 -> {'NONCE', Value};
		16#0016 -> {'XOR-RELAYED-ADDRESS', decode_attr_xaddr(Value, TID)}; % draft-ietf-behave-turn-10
		16#0017 -> {'REQUESTED-ADDRESS-TYPE', Value}; % draft-ietf-behave-turn-ipv6-03
		16#0018 -> {'EVEN-PORT', Value}; % draft-ietf-behave-turn-10
		16#0019 -> {'REQUESTED-TRANSPORT', Value}; % draft-ietf-behave-turn-10
		16#001a -> {'DONT-FRAGMENT', Value}; % draft-ietf-behave-turn-10
		16#0020 -> {'XOR-MAPPED-ADDRESS', decode_attr_xaddr(Value, TID)};
		16#0022 -> {'RESERVATION-TOKEN', Value}; % draft-ietf-behave-turn-10
		16#0024 -> {'PRIORITY', Value}; % draft-ietf-mmusic-ice-19
		16#0025 -> {'USE-CANDIDATE', Value}; % draft-ietf-mmusic-ice-19
		16#0026 -> {'PADDING', Value}; % draft-ietf-behave-nat-behavior-discovery-03
		16#0027 -> {'XOR-RESPONSE-TARGET', decode_attr_xaddr(Value, TID)}; % draft-ietf-behave-nat-behavior-discovery-03
		16#0028 -> {'XOR-REFLECTED-FROM', decode_attr_xaddr(Value, TID)}; % draft-ietf-behave-nat-behavior-discovery-03
		16#0030 -> {'ICMP', Value}; % Moved from TURN to a future I-D

		16#8020 -> {'X-VOVIDA-XOR-MAPPED-ADDRESS', decode_attr_xaddr(Value, TID)}; % VOVIDA non-standart
		16#8021 -> {'X-VOVIDA-XOR-ONLY', Value}; % VOVIDA non-standart

		16#8022 -> {'SOFTWARE', rtp_utils:fix_null_terminated(Value)}; % VOVIDA 'SERVER-NAME'
		16#8023 -> {'ALTERNATE-SERVER', decode_attr_addr(Value)};
		16#8027 -> {'CACHE_TIMEOUT', Value}; % draft-ietf-behave-nat-behavior-discovery-03
%		16#8028 -> {'FINGERPRINT', Value};
		16#8029 -> {'ICE-CONTROLLED', Value}; % draft-ietf-mmusic-ice-19
		16#802a -> {'ICE-CONTROLLING', Value}; % draft-ietf-mmusic-ice-19
		16#802b -> {'RESPONSE-ORIGIN', decode_attr_addr(Value)};
		16#802c -> {'OTHER-ADDRESS', decode_attr_addr(Value)};

		16#8050 -> {'X-VOVIDA-SECONDARY-ADDRESS', decode_attr_addr(Value)}; % VOVIDA non-standart

		16#c001 -> {'CONNECTION-REQUEST-BINDING', Value};
		16#c002 -> {'BINDING-CHANGE', Value};

		Other -> {Other, Value}
	end,
	NewLength = Length - (2 + 2 + ItemLength + PaddingLength),
	decode_attrs(Rest, NewLength, TID, Attrs ++ [{T, V}]).

decode_attr_addr(<<0:8, 1:8, Port:16, I0:8, I1:8, I2:8, I3:8>>) ->
	{{I0, I1, I2, I3}, Port};
decode_attr_addr(<<0:8, 2:8, Port:16, I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>) ->
	{{I0, I1, I2, I3, I4, I5, I6, I7}, Port}.

decode_attr_xaddr(<<0:8, 1:8, XPort:16, XAddr:32>>, _) ->
	Port = XPort bxor (?STUN_MAGIC_COOKIE bsr 16),
	<<I0:8, I1:8, I2:8, I3:8>> = <<(XAddr bxor ?STUN_MAGIC_COOKIE):32>>,
	{{I0, I1, I2, I3}, Port};
decode_attr_xaddr(<<0:8, 2:8, XPort:16, XAddr:128>>, TID) ->
	Port = XPort bxor (?STUN_MAGIC_COOKIE bsr 16),
	<<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>> = <<(XAddr bxor ((?STUN_MAGIC_COOKIE bsl 96) bor TID)):128>>,
	{{I0, I1, I2, I3, I4, I5, I6, I7}, Port}.

decode_attr_err(<<_Mbz:20, Class:4, Number:8, Reason/binary>>) ->
	{Class*100 + Number, Reason}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_bin({T, V}) ->
	L = size(V),
	PaddingLength = case L rem 4 of
		0 -> 0;
		Else -> (4 - Else)*8
	end,
	<<T:16, L:16, V:L/binary, 0:PaddingLength>>.

encode_attr('MAPPED-ADDRESS', Value, _) -> {16#0001, encode_attr_addr(Value)};
encode_attr('RESPONSE-ADDRESS', Value, _) -> {16#0002, encode_attr_addr(Value)};
encode_attr('CHANGE-ADDRESS', Value, _) -> {16#0003, encode_attr_addr(Value)};
encode_attr('SOURCE-ADDRESS', Value, _) -> {16#0004, encode_attr_addr(Value)};
encode_attr('CHANGED-ADDRESS', Value, _) -> {16#0005, encode_attr_addr(Value)};
encode_attr('USERNAME', Value, _) -> {16#0006, Value};
encode_attr('PASSWORD', Value, _) -> {16#0007, Value};
encode_attr('MESSAGE-INTEGRITY', Value, _) -> {16#0008, Value};
encode_attr('ERROR-CODE', Value, _) -> {16#0009, encode_attr_err(Value)};
encode_attr('UNKNOWN-ATTRIBUTES', Value, _) -> {16#000a, Value};
encode_attr('REFLECTED-FROM', Value, _) -> {16#000b, Value};
encode_attr('CHANNEL-NUMBER', Value, _) -> {16#000c, Value};
encode_attr('LIFETIME', Value, _) -> {16#000d, Value};
encode_attr('MAGIC-COOKIE', Value, _) -> {16#000f, Value};
encode_attr('BANDWIDTH', Value, _) -> {16#0010, Value};
encode_attr('DESTINATION-ADDRESS', Value, _) -> {16#0011, encode_attr_addr(Value)};
encode_attr('XOR-PEER-ADDRESS', Value, TID) -> {16#0012, encode_attr_xaddr(Value, TID)};
encode_attr('DATA', Value, _) -> {16#0013, Value};
encode_attr('REALM', Value, _) -> {16#0014, Value};
encode_attr('NONCE', Value, _) -> {16#0015, Value};
encode_attr('XOR-RELAYED-ADDRESS', Value, TID) -> {16#0016, encode_attr_xaddr(Value, TID)};
encode_attr('REQUESTED-ADDRESS-TYPE', Value, _) -> {16#0017, Value};
encode_attr('EVEN-PORT', Value, _) -> {16#0018, Value};
encode_attr('REQUESTED-TRANSPORT', Value, _) -> {16#0019, Value};
encode_attr('DONT-FRAGMENT', Value, _) -> {16#001a, Value};
encode_attr('XOR-MAPPED-ADDRESS', Value, TID) -> {16#0020, encode_attr_xaddr(Value, TID)};
encode_attr('RESERVATION-TOKEN', Value, _) -> {16#0022, Value};
encode_attr('PRIORITY', Value, _) -> {16#0024, Value};
encode_attr('USE-CANDIDATE', Value, _) -> {16#0025, Value};
encode_attr('PADDING', Value, _) -> {16#0026, Value};
encode_attr('XOR-RESPONSE-TARGET', Value, TID) -> {16#0027, encode_attr_xaddr(Value, TID)};
encode_attr('XOR-REFLECTED-FROM', Value, TID) -> {16#0028, encode_attr_xaddr(Value, TID)};
encode_attr('PING', Value, _) -> {16#0030, Value};
encode_attr('X-VOVIDA-XOR-MAPPED-ADDRESS', Value, TID) -> {16#8020, encode_attr_xaddr(Value, TID)};
encode_attr('X-VOVIDA-XOR-ONLY', Value, _) -> {16#8021, Value};
encode_attr('SOFTWARE', Value, _) -> {16#8022, Value};
encode_attr('ALTERNATE-SERVER', Value, _) -> {16#8023, encode_attr_addr(Value)};
encode_attr('CACHE_TIMEOUT', Value, _) -> {16#8027, Value};
%encode_attr('FINGERPRINT', Value, _) -> {16#8028, Value};
encode_attr('ICE-CONTROLLED', Value, _) -> {16#8029, Value};
encode_attr('ICE-CONTROLLING', Value, _) -> {16#802a, Value};
encode_attr('RESPONSE-ORIGIN', Value, _) -> {16#802b, encode_attr_addr(Value)};
encode_attr('OTHER-ADDRESS', Value, _) -> {16#802c, encode_attr_addr(Value)};
encode_attr('X-VOVIDA-SECONDARY-ADDRESS', Value, _) -> {16#8050, Value};
encode_attr('CONNECTION-REQUEST-BINDING', Value, _) -> {16#c001, Value};
encode_attr('BINDING-CHANGE', Value, _) -> {16#c002, Value};
encode_attr(Other, Value, _) -> {Other, Value}.

encode_attr_addr({{I0, I1, I2, I3}, Port}) ->
	<<0:8, 1:8, Port:16, I0:8, I1:8, I2:8, I3:8>>;
encode_attr_addr({{I0, I1, I2, I3, I4, I5, I6, I7}, Port}) ->
	<<0:8, 2:8, Port:16, I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>.

encode_attr_xaddr({{I0, I1, I2, I3}, Port}, _) ->
	XPort = Port bxor (?STUN_MAGIC_COOKIE bsr 16),
	<<Addr:32>> = <<I0:8, I1:8, I2:8, I3:8>>,
	XAddr = Addr bxor ?STUN_MAGIC_COOKIE,
	<<0:8, 1:8, XPort:16, XAddr:32>>;
encode_attr_xaddr({{I0, I1, I2, I3, I4, I5, I6, I7}, Port}, TID) ->
	XPort = Port bxor (?STUN_MAGIC_COOKIE bsr 16),
	<<Addr:128>> = <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>,
	XAddr = Addr bxor ((?STUN_MAGIC_COOKIE bsl 96) bor TID),
	<<0:8, 2:8, XPort:16, XAddr:128>>.

encode_attr_err({ErrorCode, Reason}) ->
	Class = ErrorCode div 100,
	Number = ErrorCode rem 100,
	<<0:20, Class:4, Number:8, Reason/binary>>.

%%
%% Fingerprinting and auth
%%

check_fingerprint(StunBinary) ->
	Size = size(StunBinary) - 8,
	case StunBinary of
		<<Message:Size/binary, 16#80:8, 16#28:8, 16#00:8, 16#04:8, CRC:32>> ->
			<<H:16, OldSize:16, Payload/binary>> = Message,
			NewSize = OldSize - 8,
			{crc, {erlang:crc32(Message) bxor 16#5354554e == CRC, <<H:16, NewSize:16, Payload/binary>>}};
		_ ->
			error_logger:warning_msg("No CRC was found in a STUN message."),
			{nocrc, StunBinary}
	end.
insert_fingerprint(StunBinary) ->
	<<H:16, _:16, Message/binary>> = StunBinary,
	Size = size(StunBinary) + 8 - 20,
	CRC = erlang:crc32(Message) bxor 16#5354554e,
	<<H:16, Size:16, Message/binary, 16#80:8, 16#28:8, 16#00:8, 16#04:8, CRC:32>>.
