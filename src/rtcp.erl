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

% See these RFCs for further details:

% http://www.ietf.org/rfc/rfc2032.txt
% http://www.ietf.org/rfc/rfc3550.txt
% http://www.ietf.org/rfc/rfc3611.txt

-module(rtcp).
-author('lemenkov@gmail.com').

% FIXME - don't forget to remove from final version!
-compile(export_all).

-export([encode/1]).
-export([decode/1]).

-include("../include/rtcp.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(Data) when is_binary(Data) ->
	decode(Data, []).

decode(<<>>, DecodedRtcps) ->
	% No data left, so we simply return list of decoded RTCP-packets
	{ok, DecodedRtcps};

decode(<<1:8, Rest/binary>>, DecodedRtcps) ->
	% FIXME Should we do this at all?
	error_logger:warning_msg("Try to fix wrong RTCP version (0)~n"),
	decode(<<?RTCP_VERSION:2, 0:1, 1:5, Rest/binary>>, DecodedRtcps);
decode(<<1:2, Rest/binary>>, DecodedRtcps) ->
	% FIXME Should we do this at all?
	error_logger:warning_msg("Try to fix wrong RTCP version (1)~n"),
	decode(<<?RTCP_VERSION:2, Rest/binary>>, DecodedRtcps);

% We, currently, decoding only unencrypted RTCP (encryption is in my TODO-list),
% so we suppose, that each packet starts from the standart header

% Length is calculated in 32-bit units, so in order to calculate
% number of bytes we need to multiply it by 4

% There can be multiple RTCP packets stacked, and there is no way to determine
% reliably how many packets we received so we need recursively process them one
% by one

% Full INTRA-frame Request (h.261 specific)
% No padding for these packets, one 32-bit word of payload
decode(<<?RTCP_VERSION:2, ?PADDING_NO:1, _Mbz:5, ?RTCP_FIR:8, 1:16, SSRC:32, Tail/binary>>, DecodedRtcps) ->
	decode(Tail, DecodedRtcps ++ [#fir{ssrc=SSRC}]);

% Negative ACKnowledgements (h.261 specific)
% No padding for these packets, two 32-bit words of payload
decode(<<?RTCP_VERSION:2, ?PADDING_NO:1, _Mbz:5, ?RTCP_NACK:8, 2:16, SSRC:32, FSN:16, BLP:16, Tail/binary>>, DecodedRtcps) ->
	decode(Tail, DecodedRtcps ++ [#nack{ssrc=SSRC, fsn=FSN, blp=BLP}]);

% SMPTE Time-Codes (short form)
decode(<<?RTCP_VERSION:2, PaddingFlag:1, _Mbz:5, ?RTCP_SMPTETC:8, 3:16, SSRC:32, TimeStamp:32, S:1, Hours:5, Minutes:6, Seconds:6, Frames:6, 0:8, Tail/binary>>, DecodedRtcps) ->
	decode(Tail, DecodedRtcps ++ [#smptetc{ssrc=SSRC, timestamp=TimeStamp, sign=S, hours=Hours, minutes=Minutes, seconds=Seconds, frames=Frames}]);
% SMPTE Time-Codes (long form)
decode(<<?RTCP_VERSION:2, PaddingFlag:1, _Mbz:5, ?RTCP_SMPTETC:8, 4:16, SSRC:32, TimeStamp:32, Smpte12m:64, Tail/binary>>, DecodedRtcps) ->
	decode(Tail, DecodedRtcps ++ [#smptetc{ssrc=SSRC, timestamp=TimeStamp, smpte12m=Smpte12m}]);

% Sender Report
% * NTPSec - NTP timestamp, most significant word
% * NTPFrac - NTP timestamp, least significant word
% * TimeStamp - RTP timestamp
% * Packets - sender's packet count
% * Octets - sender's octet count
decode(<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, ?RTCP_SR:8, Length:16, SSRC:32, NTP:64, TimeStamp:32, Packets:32, Octets:32, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length * 4 - (4 * 6),
	<<ReportBlocks:ByteLength/binary, Tail/binary>> = Rest,
	decode(Tail, DecodedRtcps ++ [#sr{ssrc=SSRC, ntp=NTP, timestamp=TimeStamp, packets=Packets, octets=Octets, rblocks = decode_rblocks(ReportBlocks, RC)}]);

% Receiver Report
decode(<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, ?RTCP_RR:8, Length:16, SSRC:32, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length*4 - 4,
	<<ReportBlocks:ByteLength/binary, Tail/binary>> = Rest,
	decode(Tail, DecodedRtcps ++ [#rr{ssrc=SSRC, rblocks = decode_rblocks(ReportBlocks, RC)}]);

% Source DEScription
decode(<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, ?RTCP_SDES:8, Length:16, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length*4,
	<<Payload:ByteLength/binary, Tail/binary>> = Rest,
	% There may be RC number of chunks (we call them Chunks), containing of
	% their own SSRC 32-bit identificator and arbitrary number of SDES-items
	decode(Tail, DecodedRtcps ++ [#sdes{list=decode_sdes_items(Payload, [])}]);

% End of stream (but not necessary the end of communication, since there may be
% many streams within)
decode(<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, ?RTCP_BYE:8, Length:16, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length*4,
	<<Payload:ByteLength/binary, Tail/binary>> = Rest,
	decode(Tail, DecodedRtcps ++ [decode_bye(Payload, RC, [])]);

% Application-specific data
decode(<<?RTCP_VERSION:2, PaddingFlag:1, Subtype:5, ?RTCP_APP:8, Length:16, SSRC:32, Name:4/binary, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length*4 - 8,
	<<Data:ByteLength/binary, Tail/binary>> = Rest,
	decode(Tail, DecodedRtcps ++ [#app{ssrc=SSRC, subtype=Subtype, name=Name, data=Data}]);

% eXtended Report
decode(<<?RTCP_VERSION:2, PaddingFlag:1, _Mbz:5, ?RTCP_XR:8, Length:16, SSRC:32, Rest/binary>>, DecodedRtcps) ->
	ByteLength = Length*4 - 4,
	<<XReportBlocks:ByteLength/binary, Tail/binary>> = Rest,
	decode(Tail, DecodedRtcps ++ [#xr{ssrc=SSRC, xrblocks=decode_xrblocks(XReportBlocks, ByteLength)}]);

decode(<<0:32, Rest/binary>>, DecodedRtcps) ->
	error_logger:warning_msg("RTCP unknown padding [<<0,0,0,0>>]~n"),
	decode(Rest, DecodedRtcps);
decode(Padding, DecodedRtcps) ->
	error_logger:warning_msg("RTCP unknown padding [~p]~n", [Padding]),
	{ok, DecodedRtcps}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We're creating function for decoding ReportBlocks, which present in both
% SenderReport's (SR) and ReceiverReport's (RR) packets
decode_rblocks(Data, RC) ->
	decode_rblocks(Data, RC, []).

% If no data was left, then we ignore the RC value and return what we already
% decoded
decode_rblocks(<<>>, _RC, Rblocks) ->
	Rblocks;

% The packets can contain padding filling space up to 32-bit boundaries
% If RC value (number of ReportBlocks left) = 0, then we return what we already
% decoded
decode_rblocks(Padding, 0, Result) ->
	% We should report about padding since it may be also malformed RTCP packet
	error_logger:warning_msg("ReportBlocks padding [~p]~n", [Padding]),
	Result;

% Create and fill with values new #rblocks{...} structure and proceed with next
% one (decreasing ReportBlocks counted (RC) by 1)
% * SSRC - SSRC of the source
% * FL - fraction lost
% * CNPL - cumulative number of packets lost
% * EHSNR - extended highest sequence number received
% * IJ - interarrival jitter
% * LSR - last SR timestamp
% * DLSR - delay since last SR
decode_rblocks(<<SSRC:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC, Result) ->
	decode_rblocks(Rest, RC-1, Result ++ [#rblock{ssrc=SSRC, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]);

decode_rblocks(Padding, _RC, Result) when size(Padding) < 24 ->
	% We should report about padding since it may be also malformed RTCP packet
	error_logger:warning_msg("ReportBlocks padding [~p]~n", [Padding]),
	Result.

decode_xrblocks(Data, Length) ->
	decode_xrblocks(Data, Length, []).

decode_xrblocks(<<>>, _Length, XRBlocks) ->
	XRBlocks;

% The packets can contain padding filling space up to 32-bit boundaries
% If RC value (number of ReportBlocks left) = 0, then we return what we already
% decoded
decode_xrblocks(Padding, 0, XRBlocks) ->
	% We should report about padding since it may be also malformed RTCP packet
	error_logger:warning_msg("eXtended ReportBlocks padding [~p]~n", [Padding]),
	XRBlocks;

decode_xrblocks(<<BT:8, TS:8, BlockLength:16, Rest/binary>>, Length, Result) ->
	ByteLength = BlockLength * 4,
	<<BlockData:ByteLength/binary, Next/binary>> = Rest,
	decode_xrblocks(Next, Length - (BlockLength * 4 + 4) , Result ++ [#xrblock{type=BT, ts=TS, data=BlockData}]).

% Recursively process each chunk and return list of SDES-items
decode_sdes_items(<<>>, Result) ->
	Result;
% First SDES item is always SSRC
decode_sdes_items(<<SSRC:32, RawData/binary>>, Result) ->
	% Each SDES list is followed by their own SSRC value (they are not
	% necessary the same) and the arbitrary raw data
	{SdesProplist, RawDataRest} = decode_sdes_item(RawData, [{ssrc,SSRC}]),
	% We're processing next possible SDES chunk
	% - We decrease SDES count (SC) by one, since we already proccessed one
	% SDES chunk
	% - We add previously decoded SDES proplist to the list of already
	% processed SDES chunks
	decode_sdes_items(RawDataRest, Result ++ [SdesProplist]).

% All items are ItemID:8_bit, Lenght:8_bit, ItemData:Length_bit
decode_sdes_item(<<?SDES_CNAME:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	% AddPac SIP device sends us wrongly produced CNAME item (with 2-byte
	% arbitrary padding inserted):
	% <<?SDES_CNAME:8, 19:8, ArbitraryPadding:16, "AddPac VoIP Gateway":(19*8)/binary>>
	% I don't think that we need to fix it.
	decode_sdes_item(Tail, Items ++ [{cname, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_NAME:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{name, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_EMAIL:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{email, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_PHONE:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{phone, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_LOC:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{loc, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_TOOL:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{tool, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_NOTE:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items ++ [{note, binary_to_list(V)}]);
decode_sdes_item(<<?SDES_PRIV:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	<<PL:8,  PD:PL/binary, Rest/binary>> = V,
	decode_sdes_item(Tail, Items ++ [{priv, {binary_to_list(PD), Rest}}]);
decode_sdes_item(<<?SDES_NULL:8, Tail/binary>>, Items) ->
	% This is NULL terminator
	% Let's calculate how many bits we need to skip (padding up to 32-bit
	% boundaries)
	R = 8 * (size(Tail) rem 4),
	<<_PaddingBits:R, Rest/binary>> = Tail,
	% mark this SDES chunk as null-terminated properly and return
	{Items ++ [{eof,true}], Rest};
decode_sdes_item(<<_:8, L:8, _:L/binary, Tail/binary>>, Items) ->
	% unknown SDES item - just skip it and proceed to the next one
	decode_sdes_item(Tail, Items);
decode_sdes_item(Rest, Items) ->
	% possibly, next SDES chunk - just stop and return what was already
	% decoded
	{Items, Rest}.

decode_bye(<<>>, _RC, Ret) ->
	% If no data was left, then we should ignore the RC value and return
	% what we already decoded
	#bye{ssrc=Ret};

decode_bye(<<L:8, Text:L/binary, _/binary>>, 0, Ret) ->
	% Text message is always the last data chunk in BYE packet
	#bye{message=binary_to_list(Text), ssrc=Ret};

decode_bye(Padding, 0, Ret) ->
	% No text, no SSRC left, so just returning what we already have
	error_logger:warning_msg("BYE padding [~p]~n", [Padding]),
	#bye{ssrc=Ret};

decode_bye(<<SSRC:32, Tail/binary>>, RC, Ret) when RC>0 ->
	% SSRC of stream, which just ends
	decode_bye(Tail, RC-1, Ret ++ [SSRC]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode(List) when is_list(List) ->
	encode_list(List, <<>>);

encode(#fir{ssrc = SSRC}) ->
	encode_fir(SSRC);

encode(#nack{ssrc = SSRC, fsn = FSN, blp = BLP}) ->
	encode_nack(SSRC, FSN, BLP);

encode(#sr{ssrc = SSRC, ntp = Ntp, timestamp = TimeStamp, packets = Packets, octets = Octets, rblocks = ReportBlocks}) ->
	encode_sr(SSRC, Ntp, TimeStamp, Packets, Octets, ReportBlocks);

encode(#rr{ssrc = SSRC, rblocks = ReportBlocks}) ->
	encode_rr(SSRC, ReportBlocks);

encode(#sdes{list=SdesItemsList}) ->
	encode_sdes(SdesItemsList);

encode(#bye{message = Message, ssrc = SSRCs}) ->
	encode_bye(SSRCs, Message);

encode(#app{subtype = Subtype, ssrc = SSRC, name = Name, data = Data}) ->
	encode_app(Subtype, SSRC, Name, Data);

encode(#xr{ssrc = SSRC, xrblocks = XRBlocks}) ->
	encode_xr(SSRC, XRBlocks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_list([], Data) ->
	Data;
encode_list([Head | Tail], Data) ->
	Enc = encode(Head),
	encode_list(Tail, <<Data/binary, Enc/binary>>).

encode_fir(SSRC) ->
	<<?RTCP_VERSION:2, ?PADDING_NO:1, ?MBZ:5, ?RTCP_FIR:8, 1:16, SSRC:32>>.

encode_nack(SSRC, FSN, BLP) ->
	<<?RTCP_VERSION:2, ?PADDING_NO:1, ?MBZ:5, ?RTCP_NACK:8, 2:16, SSRC:32, FSN:16, BLP:16>>.

% TODO profile-specific extensions
encode_sr(SSRC, Ntp, TimeStamp, Packets, Octets, ReportBlocks) when is_list(ReportBlocks) ->

	% Number of ReportBlocks
	RC = length(ReportBlocks),

	% TODO profile-specific extensions' size
	% sizeof(SSRC) + sizeof(Sender's Info) + RC * sizeof(ReportBlock) in
	% 32-bit words
	Length = 1 + 5 + RC * 6,

	RB = encode_rblocks(ReportBlocks),

	<<?RTCP_VERSION:2, ?PADDING_NO:1, RC:5, ?RTCP_SR:8, Length:16, SSRC:32, Ntp:64, TimeStamp:32, Packets:32, Octets:32, RB/binary>>.

% TODO profile-specific extensions
encode_rr(SSRC, ReportBlocks) when is_list(ReportBlocks) ->
	% Number of ReportBlocks
	RC = length(ReportBlocks),

	% TODO profile-specific extensions' size
	% sizeof(SSRC) + RC * sizeof(ReportBlock) in 32-bit words
	Length = 1 + RC * 6,

	RB = encode_rblocks(ReportBlocks),

	<<?RTCP_VERSION:2, ?PADDING_NO:1, RC:5, ?RTCP_RR:8, Length:16, SSRC:32, RB/binary>>.

encode_sdes(SdesItemsList) when is_list (SdesItemsList) ->
	RC = length(SdesItemsList),

	SdesData = list_to_binary ([encode_sdes_items(X) || X <- SdesItemsList]),

	Length = size(SdesData) div 4,

	% TODO ensure that this list is null-terminated and no null-terminator
	% exists in the middle of the list

	<<?RTCP_VERSION:2, ?PADDING_NO:1, RC:5, ?RTCP_SDES:8, Length:16, SdesData/binary>>.

encode_bye(SSRCsList, []) when is_list(SSRCsList) ->
	SSRCs=list_to_binary([<<S:32>> || S <- SSRCsList]),
	SC = size(SSRCs) div 4,
	<<?RTCP_VERSION:2, ?PADDING_NO:1, SC:5, ?RTCP_BYE:8, SC:16, SSRCs/binary>>;

encode_bye(SSRCsList, MessageList) when is_list(SSRCsList), is_list(MessageList) ->
	Message = list_to_binary(MessageList),
	SSRCs = list_to_binary([<<S:32>> || S <- SSRCsList]),
	SC = size(SSRCs) div 4,
	% FIXME no more than 255 symbols
	TextLength = size(Message),
	case (TextLength + 1) rem 4 of
		0 ->
			<<?RTCP_VERSION:2, ?PADDING_NO:1, SC:5, ?RTCP_BYE:8, (SC + ((TextLength + 1) div 4)):16, SSRCs/binary, TextLength:8, Message/binary>>;
		Pile ->
			Padding = <<0:((4-Pile)*8)>>,
			<<?RTCP_VERSION:2, ?PADDING_YES:1, SC:5, ?RTCP_BYE:8, (SC + ((TextLength + 1 + 4 - Pile) div 4)):16, SSRCs/binary, TextLength:8, Message/binary, Padding/binary>>
	end.

encode_app(Subtype, SSRC, Name, Data) when is_list(Name), is_binary(Data) ->
	encode_app(Subtype, SSRC, list_to_binary(Name), Data);

encode_app(Subtype, SSRC, Name, Data) when is_binary(Name), is_binary(Data) ->
	case {(size(Data) rem 4) == 0, (size(Name) == 4)} of
		{true, true} ->
			% sizeof(SSRC)/4 + sizeof(Name)/4 + sizeof(Data)/4
			Length = 1 + 1 + size(Data) div 4,
			<<?RTCP_VERSION:2, ?PADDING_NO:1, Subtype:5, ?RTCP_APP:8, Length:16, SSRC:32, Name/binary, Data/binary>>;
		_ ->
			{error, bad_data}
	end.

encode_xr(SSRC, XRBlocks) when is_list(XRBlocks) ->
	XRBlocksData = encode_xrblocks(XRBlocks),
	Length = 1 + size(XRBlocksData) div 4,
	<<?RTCP_VERSION:2, ?PADDING_NO:1, ?MBZ:5, ?RTCP_XR:8, Length:16, SSRC:32, XRBlocksData/binary>>.

encode_rblocks(RBlocks) when is_list (RBlocks) ->
	encode_rblocks(RBlocks, []).

encode_rblocks([],  EncodedRBlocks) ->
	list_to_binary(EncodedRBlocks);

encode_rblocks([ RBlock | Rest],  EncodedRBlocks) ->
	encode_rblocks(Rest, EncodedRBlocks ++ [encode_rblock(RBlock)]).

% * SSRC - SSRC of the source
% * FL - fraction lost
% * CNPL - cumulative number of packets lost
% * EHSNR - extended highest sequence number received
% * IJ - interarrival jitter
% * LSR - last SR timestamp
% * DLSR - delay since last SR
encode_rblock(#rblock{ssrc=SSRC, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}) ->
	encode_rblock(SSRC, FL, CNPL, EHSNR, IJ, LSR, DLSR);
encode_rblock({SSRC, FL, CNPL, EHSNR, IJ, LSR, DLSR}) ->
	encode_rblock(SSRC, FL, CNPL, EHSNR, IJ, LSR, DLSR).
encode_rblock(SSRC, FL, CNPL, EHSNR, IJ, LSR, DLSR) ->
	<<SSRC:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32>>.

encode_sdes_items(SdesItems) when is_list (SdesItems) ->
	SdesChunkData = list_to_binary ([ encode_sdes_item(X,Y) || {X,Y} <- SdesItems]),

	PaddingSize = case size(SdesChunkData) rem 4 of
		0 -> 0;
		Rest -> (4 - Rest) * 8
	end,

	Padding = <<0:PaddingSize>>,
	<<SdesChunkData/binary, Padding/binary>>.


encode_sdes_item(eof) ->
	<<?SDES_NULL:8>>.
encode_sdes_item(eof, true) ->
	<<?SDES_NULL:8>>;
encode_sdes_item(_, null) ->
	<<>>;
encode_sdes_item(ssrc, Value) ->
	<<Value:32>>;
encode_sdes_item(cname, Value) ->
	encode_sdes_item(?SDES_CNAME, list_to_binary(Value));
encode_sdes_item(name, Value) ->
	encode_sdes_item(?SDES_NAME, list_to_binary(Value));
encode_sdes_item(email, Value) ->
	encode_sdes_item(?SDES_EMAIL, list_to_binary(Value));
encode_sdes_item(phone, Value) ->
	encode_sdes_item(?SDES_PHONE, list_to_binary(Value));
encode_sdes_item(loc, Value) ->
	encode_sdes_item(?SDES_LOC, list_to_binary(Value));
encode_sdes_item(tool, Value) ->
	encode_sdes_item(?SDES_TOOL, list_to_binary(Value));
encode_sdes_item(note, Value) ->
	encode_sdes_item(?SDES_NOTE, list_to_binary(Value));
encode_sdes_item(priv, {PrivTypeName, Value}) ->
	PrivTypeBin = list_to_binary(PrivTypeName),
	PrivTypeSize = size(PrivTypeBin),
	encode_sdes_item(?SDES_PRIV, <<PrivTypeSize:8, PrivTypeBin/binary, Value/binary>>);
encode_sdes_item(SdesType, Value) when is_binary (Value) ->
	L = size(Value),
	<<SdesType:8, L:8, Value:L/binary>>.

encode_xrblocks(XRBlocks) when is_list (XRBlocks) ->
	encode_xrblocks(XRBlocks, []).

encode_xrblocks([],  EncodedXRBlocks) ->
	list_to_binary(EncodedXRBlocks);
encode_xrblocks([ XRBlock | Rest],  EncodedXRBlocks) ->
	encode_xrblocks(Rest, EncodedXRBlocks ++ [encode_xrblock(XRBlock)]).

encode_xrblock(#xrblock{type = BT, ts = TS, data = Data}) ->
	encode_xrblock(BT, TS,  Data);
encode_xrblock({BT, TS,  Data}) ->
	encode_xrblock(BT, TS,  Data).

encode_xrblock(BT, TS,  Data) ->
	case size(Data) rem 4 of
		0 ->
			BlockLength = size(Data) div 4,
			<<BT:8, TS:8, BlockLength:16, Data/binary>>;
		_ ->
			throw ({error, "Please, normalize data first"})
	end.
