%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Peter Lemenkov <lemenkov@gmail.com>
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
%%% * Neither the names of its contributors may be used to endorse or promote
%%% products derived from this software without specific prior written permission.
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

-module(rtp).
-author('lemenkov@gmail.com').

% FIXME - don't forget to remove from final version!
-compile(export_all).

-include("../include/rtp.hrl").
-include("../include/stun.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FIXME 0 <= CC <=15
decode(<<?RTP_VERSION:2, Padding:1, ExtensionFlag:1, CC:4, Marker:1, PayloadType:7, SequenceNumber:16, Timestamp:32, SSRC:32, Rest/binary>>) ->
	{ok, Data0, CSRCs} = decode_csrc(Rest, CC, []),
	{ok, Data1, Extension} = decode_extension(Data0, ExtensionFlag),
	{ok, Payload} = remove_padding(Data1, Padding),
	{ok, #rtp{
		padding = Padding,
		marker = Marker,
		payload_type = PayloadType,
		sequence_number = SequenceNumber,
		timestamp = Timestamp,
		ssrc = SSRC,
		csrcs = CSRCs,
		extension = Extension,
		payload = Payload
	}};

decode(<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?MAGIC_COOKIE:32, TransactionID:96, Rest/binary>>) ->
	<<Method:12>> = <<M0:5, M1:3, M2:4>>,
	Class = case <<C0:1, C1:1>> of
		<<0:0, 0:1>> -> request;
		<<0:0, 1:1>> -> indication;
		<<1:0, 0:1>> -> success;
		<<1:0, 1:1>> -> error
	end,
	Attrs = decode_attrs(Rest, Length, []),
	{ok, #stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_csrc(Data, 0, CSRCs) ->
	{ok, Data, CSRCs};
decode_csrc(<<CSRC:32, Data/binary>>, CC, CSRCs) ->
	decode_csrc(Data, CC-1, CSRCs ++ [CSRC]).

decode_extension(Data, 0) ->
	{ok, Data, null};
decode_extension(<<Type:16, Length:16, Payload:Length/binary, Data/binary>>, 1) ->
	{ok, Data, #extension{type = Type, payload = Payload}}.

decode_rfc2833(<<Event:8, 0:1, _Mbz:1, Volume:6, Duration:16>>) ->
	{ok, #rfc2833{event = Event, eof = false, volume = Volume, duration = Duration}};
decode_rfc2833(<<Event:8, 1:1, _Mbz:1, Volume:6, Duration:16>>) ->
	{ok, #rfc2833{event = Event, eof = true, volume = Volume, duration = Duration}}.

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
	<<Value:ItemLength/binary, _:PaddingLength/binary, Rest/binary>> = Bin,
	NewLength = Length - (2 + 2 + ItemLength + PaddingLength),
	decode_attrs(Rest, NewLength, Attrs ++ [{Type, Value}]).

remove_padding(Data, 0) ->
	{ok, Data};
remove_padding(Data, 1) when Data /= <<>> ->
	L = size(Data) - 1,
	<<_:L/binary, C>> = Data,
	{Payload, _} = split_binary(Data, L+1-C),
	{ok, Payload}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO Padding
encode(#rtp{padding = P, marker = M, payload_type = PT, sequence_number = SN, timestamp = TS, ssrc = SSRC, csrcs = CSRCs, extension = X, payload = Payload}) ->
	CC = length(CSRCs),
	CSRC_Data = list_to_binary([<<CSRC:32>> || CSRC <- CSRCs]),
	{ExtensionFlag, ExtensionData} = encode_extension(X),
	<<?RTP_VERSION:2, P:1, ExtensionFlag:1, CC:4, M:1, PT:7, SN:16, TS:32, SSRC:32, CSRC_Data/binary, ExtensionData/binary, Payload/binary>>;

encode(#stun{class = Class, method = Method, transactionid = TransactionID, attrs = Attrs}) ->
	<<M0:5, M1:3, M2:4>> = <<Method:12>>,
	<<C0:1, C1:1>> = case Class of
		request -><<0:0, 0:1>>;
		indication -> <<0:0, 1:1>>;
		success -> <<1:0, 0:1>>;
		error -> <<1:0, 1:1>>
	end,
	BinAttrs = encode_attrs(Attrs, <<>>),
	Length = size(BinAttrs),
	<<?STUN_MARKER:2, M0:5, C0:1, M1:3, C1:1, M2:4 , Length:16, ?MAGIC_COOKIE:32, TransactionID:96, BinAttrs/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_extension(null) ->
	{0, <<>>};
encode_extension(#extension{type = Type, payload = Payload}) ->
	Length = size(Payload),
	{1, <<Type:16, Length:16, Payload:Length/binary>>}.

encode_2833(#rfc2833{event = Event, eof = false, volume = Volume, duration = Duration}) ->
	<<Event:8, 0:1, 0:1, Volume:6, Duration:16>>;
encode_2833(#rfc2833{event = Event, eof = true, volume = Volume, duration = Duration}) ->
	<<Event:8, 1:1, 0:1, Volume:6, Duration:16>>.

encode_attrs([], Attrs) ->
	Attrs;
encode_attrs([{Type, Value}|Rest], Attrs) ->
	% FIXME
	ItemLength = size(Value),
	PaddingLength = case ItemLength rem 4 of
		0 -> 0;
		Else -> (4 - Else)*8
	end,
	Attr = <<Type:16, ItemLength:16, Value:ItemLength/binary, 0:PaddingLength>>,
	encode_attrs(Rest, <<Attrs/binary, Attr/binary>>).
