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

decode(<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?MAGIC_COOKIE:32, TransactionID:96, Rest/binary>>) ->
	Method = case <<M0:5, M1:3, M2:4>> of
		<<1:12>> -> binding;
		<<Other:12>> -> Other
	end,
	Class = case <<C0:1, C1:1>> of
		<<0:1, 0:1>> -> request;
		<<0:1, 1:1>> -> indication;
		<<1:1, 0:1>> -> success;
		<<1:1, 1:1>> -> error
	end,
	Attrs = decode_attrs(Rest, Length, []),
	{ok, #stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}}.

encode(#stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}) ->
	M = case Method of
		binding -> 1;
		Other -> Other
	end,
	<<M0:5, M1:3, M2:4>> = <<M:12>>,
	<<C0:1, C1:1>> = case Class of
		request -><<0:1, 0:1>>;
		indication -> <<0:1, 1:1>>;
		success -> <<1:1, 0:1>>;
		error -> <<1:1, 1:1>>
	end,
	BinAttrs = encode_attrs(Attrs, <<>>),
	Length = size(BinAttrs),
	<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?MAGIC_COOKIE:32, TransactionID:96, BinAttrs/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% STUN decoding helpers
%%

decode_attrs(<<>>, 0, Attrs) ->
	Attrs;
decode_attrs(<<>>, Length, Attrs) ->
	error_logger:warning_msg("STUN TLV wrong length [~p]~n", [Length]),
	Attrs;
decode_attrs(<<Type:16, ItemLength:16, Bin/binary>>, Length, Attrs) ->
	PaddingLength = case ItemLength rem 4 of
		0 -> 0;
		Else -> 4 - Else
	end,
	T = case Type of
		16#0001 -> 'MAPPED-ADDRESS';
		16#0002 -> 'RESPONSE-ADDRESS'; % Obsolete
		16#0003 -> 'CHANGE-ADDRESS'; % Obsolete
		16#0004 -> 'SOURCE-ADDRESS'; % Obsolete
		16#0005 -> 'CHANGED-ADDRESS'; % Obsolete
		16#0006 -> 'USERNAME';
		16#0007 -> 'PASSWORD'; % Obsolete
		16#0008 -> 'MESSAGE-INTEGRITY';
		16#0009 -> 'ERROR-CODE';
		16#000a -> 'UNKNOWN-ATTRIBUTES';
		16#000b -> 'REFLECTED-FROM'; % Obsolete
		16#0014 -> 'REALM';
		16#0015 -> 'NONCE';
		16#0020 -> 'XOR-MAPPED-ADDRESS';

		16#8022 -> 'SOFTWARE';
		16#8023 -> 'ALTERNATE-SERVER';
		16#8028 -> 'FINGERPRINT';
		Other -> Other
	end,
	<<Value:ItemLength/binary, _:PaddingLength/binary, Rest/binary>> = Bin,
	NewLength = Length - (2 + 2 + ItemLength + PaddingLength),
	decode_attrs(Rest, NewLength, Attrs ++ [{T, Value}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_attrs([], Attrs) ->
	Attrs;
encode_attrs([{Type, Value}|Rest], Attrs) ->
	% FIXME
	ItemLength = size(Value),
	PaddingLength = case ItemLength rem 4 of
		0 -> 0;
		Else -> (4 - Else)*8
	end,
	T = case Type of
		'MAPPED-ADDRESS' -> 16#0001;
		'RESPONSE-ADDRESS' -> 16#0002;
		'CHANGE-ADDRESS' -> 16#0003;
		'SOURCE-ADDRESS' -> 16#0004;
		'CHANGED-ADDRESS' -> 16#0005;
		'USERNAME' -> 16#0006;
		'PASSWORD' -> 16#0007;
		'MESSAGE-INTEGRITY' -> 16#0008;
		'ERROR-CODE' -> 16#0009;
		'UNKNOWN-ATTRIBUTES' -> 16#000a;
		'REFLECTED-FROM' -> 16#000b;
		'REALM' -> 16#0014;
		'NONCE' -> 16#0015;
		'XOR-MAPPED-ADDRESS' -> 16#0020;

		'SOFTWARE' -> 16#8022;
		'ALTERNATE-SERVER' -> 16#8023;
		'FINGERPRINT' -> 16#8028;
		Other -> Other
	end,
	Attr = <<T:16, ItemLength:16, Value:ItemLength/binary, 0:PaddingLength>>,
	encode_attrs(Rest, <<Attrs/binary, Attr/binary>>).
