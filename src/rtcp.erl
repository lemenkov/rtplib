%%%----------------------------------------------------------------------
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 3 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

% see this RFC for further details:
% http://www.ietf.org/rfc/rfc3550.txt

-module(rtcp).
-author('lemenkov@gmail.com').

-export([encode/1]).
-export([decode/1]).

-include("rtcp.hrl").

encode(Packets) ->
	% TODO
	ok.

decode(Data) ->
	decode(Data, []).

decode(<<>>, DecodedRtcps) ->
	% No data left, so we simply return list of decoded RTCP-packets
	DecodedRtcps;

% We, currently, decoding only unencrypted RTCP (enclyption is in my TODO-list),
% so we suppose, that each packet starts from the standart header
decode(<<?RTCP_VERSION:2, PaddingFlag:1, RC:5, PacketType:8, Length:16, Tail/binary>>, DecodedRtcps) ->
	% Length is calculated in 32-bit units, so in order to calculate
	% number of bytes we need to multiply it by 4
	ByteLength = Length*4,

	<<Payload:ByteLength/binary, Next/binary>> = Tail,

	Rtcp = case PacketType of

		% Sender Report
		?RTCP_SR ->
			% * NTPSec - NTP timestamp, most significant word
			% * NTPFrac - NTP timestamp, least significant word
			% * TimeStamp - RTP timestamp
			% * Packets - sender's packet count
			% * Octets - sender's octet count
			<<SSRC:32, NTPSec:32, NTPFrac:32, TimeStamp:32, Packets:32, Octets:32, ReportBlocks/binary>> = Payload,

			Ntp2Now = fun(NTPSec1, NTPFrac1) ->
				MegaSecs = NTPSec1 div 1000000,
				Secs = NTPSec1 rem 1000000,
				R = lists:foldl(fun(X, Acc) -> Acc + ((NTPFrac1 bsr (X-1)) band 1)/(2 bsl (32-X)) end, 0, lists:seq(1, 32)),
				MicroSecs = trunc(1000000*R),
				{MegaSecs, Secs, MicroSecs}
			end,
			#sr{ssrc=SSRC, ntp=Ntp2Now(NTPSec, NTPFrac), timestamp=TimeStamp, packets=Packets, octets=Octets, rblocks = decode_rblocks(ReportBlocks, RC)};

		% Receiver Report
		?RTCP_RR ->
			<<SSRC:32, ReportBlocks/binary>> = Payload,
			#rr{ssrc=SSRC, rblocks = decode_rblocks(ReportBlocks, RC)};

		% Source DEScription.
		?RTCP_SDES ->
			% There may be RC number of chunks (we call them Chunks), containing of their own SSRC 32-bit identificator
			% and arbitrary number of SDES-items.
			decode_sdes_items(Payload, RC, []);

		% End of stream (but not necessary the end of communication, since there may be many streams within)
		?RTCP_BYE ->
			decode_bye(Payload, RC, []);

		_ ->
			% FIXME add more RTCP packet types
			{error, unknown_type}
	end,

	% There can be multiple RTCP packets stacked, and there is no way to determine reliably how many packets we received
	% so we need recursively process them one by one
	decode(Next, DecodedRtcps ++ [Rtcp]);

decode(Padding, DecodedRtcps) ->
	error_logger:warning_msg("RTCP unknown padding [~p]~n", [Padding]),
	DecodedRtcps.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% We're creating function for decoding ReportBlocks, which present in both SenderReport's (SR)
% and ReceiverReport's (RR) packets
decode_rblocks(Data, RC) ->
	decode_rblocks(Data, RC, []).

% If no data was left, then we ignore the RC value and return what we already decoded
decode_rblocks(<<>>, _RC, Rblocks) ->
	Rblocks;

% The packets can contain padding filling space up to 32-bit boundaries
% If RC value (number of ReportBlocks left) = 0, then we return what we already decoded
decode_rblocks(Padding, 0, Result) ->
	% We should report about padding since it may be also malformed RTCP packet
	error_logger:warning_msg("ReportBlocks padding [~p]~n", [Padding]),
	Result;

% Create and fill with values new #rblocks{...} structure and proceed with next one (decreasing
% ReportBlocks counted (RC1) by 1)
% * SSRC1 - SSRC of the source
% * FL - fraction lost
% * CNPL - cumulative number of packets lost
% * EHSNR - extended highest sequence number received
% * IJ - interarrival jitter
% * LSR - last SR timestamp
% * DLSR - delay since last SR
decode_rblocks(<<SSRC:32, FL:8, CNPL:24/signed, EHSNR:32, IJ:32, LSR:32, DLSR:32, Rest/binary>>, RC1, Result) ->
	decode_rblocks(Rest, RC1-1, Result ++ [#rblock{ssrc=SSRC, fraction=FL, lost=CNPL, last_seq=EHSNR, jitter=IJ, lsr=LSR, dlsr=DLSR}]).

% Recursively process each chunk and return list of SDES-items
decode_sdes_items(<<>>, _SC, Result) ->
	% Disregard SDES items count (SC) if no data remaining
	% simply construct #sdes{} from the resulting list of
	% SDES-items (Result) and return
	#sdes{list=Result};
decode_sdes_items(Padding, 0, Result) ->
	% SDES may contain padding (should be noted by PaddingFlag)
	% Likewise.
	error_logger:warning_msg("SDES padding [~p]~n", [Padding]),
	#sdes{list=Result};
decode_sdes_items(<<SSRC:32, RawData/binary>>, SC, Result) when SC>0 ->
	% Each SDES-item followed by their own SSRC value (they are not necessary the same)
	% and the arbitrary raw data
	{Items, RawDataRest} = decode_sdes_item(RawData, #sdes_items{ssrc=SSRC}),
	% We're processing next possible SDES chunk
	% - We decrease SDES count (SC) by one, since we already proccessed one SDES chunk
	% - We add previously decoded and pack into #sdes{} SDES-items to the list of
	%   already processed SDES chunks
	decode_sdes_items(RawDataRest, SC-1, Result ++ [Items]).

% All items are ItemID:8_bit, Lenght:8_bit, ItemData:Length_bit
decode_sdes_item(<<?SDES_CNAME:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	% AddPac SIP device sends us wrongly produced CNAME item (with 2-byte arbitrary padding inserted):
	% <<?SDES_CNAME:8, 19:8, ArbitraryPadding:16, "AddPac VoIP Gateway":(19*8)/binary>>
	% I don't think that we need to fix it.
	decode_sdes_item(Tail, Items#sdes_items{cname=binary_to_list(V)});
decode_sdes_item(<<?SDES_NAME:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{name=binary_to_list(V)});
decode_sdes_item(<<?SDES_EMAIL:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{email=binary_to_list(V)});
decode_sdes_item(<<?SDES_PHONE:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{phone=binary_to_list(V)});
decode_sdes_item(<<?SDES_LOC:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{loc=binary_to_list(V)});
decode_sdes_item(<<?SDES_TOOL:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{tool=binary_to_list(V)});
decode_sdes_item(<<?SDES_NOTE:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{note=binary_to_list(V)});
decode_sdes_item(<<?SDES_PRIV:8, L:8, V:L/binary, Tail/binary>>, Items) ->
	decode_sdes_item(Tail, Items#sdes_items{priv=V});
decode_sdes_item(<<?SDES_NULL:8, Tail/binary>>, Items) ->
	% This is NULL terminator
	% Let's calculate how many bits we need to skip (padding up to 32-bit boundaries)
	R = 8 * (size(Tail) rem 4),
	<<_PaddingBits:R, Rest/binary>> = Tail,
	% mark this SDES chunk as null-terminated properly and return
	{Items#sdes_items{eof=true}, Rest};
decode_sdes_item(<<_:8, L:8, _:L/binary, Tail/binary>>, Items) ->
	% unknown SDES item - just skip it and proceed to the next one
	decode_sdes_item(Tail, Items);
decode_sdes_item(Rest, Items) ->
	% possibly, next SDES chunk - just stop and return what was already decoded
	{Items, Rest}.

decode_bye(<<>>, _RC, Ret) ->
	% If no data was left, then we should ignore the RC value and return what we already decoded
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
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_bye(#bye{message = null, ssrc = SSRCs}) ->
	SC = size(SSRCs) div 4,
	<<?RTCP_VERSION:2, ?PADDING_NO:1, SC:5, ?RTCP_BYE:8, SC:16, SSRCs/binary>>;

encode_bye(#bye{message = Message, ssrc = SSRCs}) ->
	SC = size(SSRCs) div 4,
	% FIXME no more than 255 symbols
	TextLength = size(Message),
	case (TextLength + 1) rem 4 of
		0 ->
			<<?RTCP_VERSION:2, ?PADDING_NO:1, SC:5, ?RTCP_BYE:8, (SC + ((TextLength + 1) div 4)):16, SSRCs/binary, TextLength:8, Message/binary>>;
		Pile ->
			Padding = <<0:((4-Pile)*8)>>,
			<<?RTCP_VERSION:2, ?PADDING_YES:1, SC:5, ?RTCP_BYE:8, (SC + ((TextLength + 1 + 4 - Pile) div 4)):16, SSRCs/binary, TextLength:8, Message/binary, Padding/binary>>
	end;

encode_bye(SSRCs) when is_list(SSRCs) ->
	encode_bye(#bye{message=null, ssrc=list_to_binary([<<S:32>> || S <- SSRCs])}).

encode_bye(SSRCs, Message) when is_list(SSRCs), is_list(Message) ->
	encode_bye(#bye{message=list_to_binary(Message), ssrc=list_to_binary([<<S:32>> || S <- SSRCs])}).

