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

decode(<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?STUN_MAGIC_COOKIE:32, TransactionID:96, Rest/binary>>) ->
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
	Attrs = decode_attrs(Rest, Length, TransactionID, []),
	{ok, #stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}}.

encode(#stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}) ->
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
	<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?STUN_MAGIC_COOKIE:32, TransactionID:96, BinAttrs/binary>>.

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
		16#0014 -> {'REALM', Value};
		16#0015 -> {'NONCE', Value};
		16#0020 -> {'XOR-MAPPED-ADDRESS', decode_attr_xaddr(Value, TID)};

		16#8020 -> {'X-VOVIDA-XOR-MAPPED-ADDRESS', decode_attr_xaddr(Value, TID)}; % VOVIDA non-standart
		16#8021 -> {'X-VOVIDA-XOR-ONLY', Value}; % VOVIDA non-standart

		16#8022 -> {'SOFTWARE', rtp_utils:fix_null_terminated(Value)}; % VOVIDA 'SERVER-NAME'
		16#8023 -> {'ALTERNATE-SERVER', decode_attr_addr(Value)};
		16#8028 -> {'FINGERPRINT', Value};
		16#802B -> {'RESPONSE-ORIGIN', decode_attr_addr(Value)};

		16#8050 -> {'X-VOVIDA-SECONDARY-ADDRESS', decode_attr_addr(Value)}; % VOVIDA non-standart
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
	Attr = <<T:16, L:16, V:L/binary, 0:PaddingLength>>.

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
encode_attr('REALM', Value, _) -> {16#0014, Value};
encode_attr('NONCE', Value, _) -> {16#0015, Value};
encode_attr('XOR-MAPPED-ADDRESS', Value, TID) -> {16#0020, encode_attr_xaddr(Value, TID)};
encode_attr('X-VOVIDA-XOR-MAPPED-ADDRESS', Value, TID) -> {16#8020, encode_attr_xaddr(Value, TID)};
encode_attr('X-VOVIDA-XOR-ONLY', Value, _) -> {16#8021, Value};
encode_attr('SOFTWARE', Value, _) -> {16#8022, Value};
encode_attr('ALTERNATE-SERVER', Value, _) -> {16#8023, encode_attr_addr(Value)};
encode_attr('FINGERPRINT', Value, _) -> {16#8028, Value};
encode_attr('RESPONSE-ORIGIN', Value, _) -> {16#802B, encode_attr_addr(Value)};
encode_attr('X-VOVIDA-SECONDARY-ADDRESS', Value, _) -> {16#8050, Value};
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
	<<Addr:128>> = <<I0:8, I1:8, I2:8, I3:8, I4:8, I5:8, I6:8, I7:8>>,
	XAddr = Addr bxor ((?STUN_MAGIC_COOKIE bsl 96) bor TID),
	<<0:8, 2:8, XPort:16, XAddr:128>>.

encode_attr_err({ErrorCode, Reason}) ->
	Class = ErrorCode div 100,
	Number = ErrorCode rem 100,
	<<0:20, Class:4, Number:8, Reason/binary>>.
