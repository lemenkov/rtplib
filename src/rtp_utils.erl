%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2012 Peter Lemenkov <lemenkov@gmail.com>
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

-module(rtp_utils).
-author('lemenkov@gmail.com').

-export([dump_packet/3]).
-export([get_type/1]).
-export([get_codec_from_payload/1]).
-export([get_payload_from_codec/1]).

-export([ntp2now/1]).
-export([now2ntp/0]).
-export([now2ntp/1]).

-export([mktimestamp/2]).

-export([pp/1]).

-export([fix_null_terminated/1]).

-include("../include/rtcp.hrl").
-include("../include/rtp.hrl").

dump_packet(Node, Pid, Packet) ->
	{H,M,Ms} = os:timestamp(),
	% later we may try to decode these rtcp packets and to fix decoding errors:
	file:write_file("/tmp/rtcp_err." ++ atom_to_list(Node) ++ "." ++ pid_to_list(Pid) ++ "." ++ integer_to_list(H) ++ "_" ++ integer_to_list(M) ++ "_" ++ integer_to_list(Ms) ++ ".bin", Packet).

get_type(#fir{}) -> fir;
get_type(#nack{}) -> nack;
get_type(#sr{}) -> sr;
get_type(#rr{}) -> rr;
get_type(#sdes{}) -> sdes;
get_type(#bye{}) -> bye;
get_type(#app{}) -> app;
get_type(#xr{}) -> xr;
get_type(_) -> unknown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Different helper functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


frac(Int) ->
	frac(trunc((Int*(2 bsl 32))/1000000), 32, 0).
frac(_, 0, Result) ->
	Result;
frac(Int, X, Acc) ->
	Div = Int div (2 bsl X-1),
	Rem = Int rem (2 bsl X-1),
	frac(Rem, X-1, Acc bor (Div bsl X)).

ntp2now (<<NTPSec:32, NTPFrac:32>>) ->
	MegaSecs = (NTPSec - 2208988800) div 1000000,
	Secs = (NTPSec - 2208988800) rem 1000000,
	R = lists:foldl(fun(X, Acc) -> Acc + ((NTPFrac bsr (X-1)) band 1)/(2 bsl (32-X)) end, 0, lists:seq(1, 32)),
	MicroSecs = trunc(1000000*R),
	{MegaSecs, Secs, MicroSecs}.

now2ntp () ->
	now2ntp (os:timestamp()).

now2ntp ({MegaSecs, Secs, MicroSecs}) ->
	% 2208988800 is the number of seconds from 00:00:00 01-01-1900 to 00:00:00 01-01-1970
	NTPSec = MegaSecs*1000000 + Secs + 2208988800,
	NTPFrac = frac(MicroSecs),
	<<NTPSec:32, NTPFrac:32>>.

pp(#rtcp{payloads = Rtcps}) ->
	lists:flatten(lists:map(fun pp/1, Rtcps));

% RTP
pp(#rtp{
		padding = Padding,
		marker = Marker,
		% FIXME - this is just wrong
		payload_type = 101,
		sequence_number = SN,
		timestamp = TS,
		ssrc = SSRC,
		csrcs = CSRCS,
		extension = Extension,
		payload = Payload}
) ->
	{ok, Dtmf} = rtp:decode_dtmf(Payload),
	Event = Dtmf#dtmf.event,
	Eof = Dtmf#dtmf.eof,
	Volume = Dtmf#dtmf.volume,
	Duration = Dtmf#dtmf.duration,
	io_lib:format("
		{	\"type\":\"rtp\",
			\"padding\":~b,
			\"marker\":~b,
			\"payload_type\":\"rtpevent\",
			\"sequence_number\":~b,
			\"timestamp\":~b,
			\"ssrc\":~b,
			\"csrcs\":\"~p\",
			\"extension\":\"~p\",
			\"payload\":[event:~b,eof=~p,volume=~p,duration=~p]}", [Padding, Marker, SN, TS, SSRC, CSRCS, Extension, Event, Eof, Volume, Duration]);
% RTP
pp(#rtp{
		padding = Padding,
		marker = Marker,
		payload_type = PT,
		sequence_number = SN,
		timestamp = TS,
		ssrc = SSRC,
		csrcs = CSRCS,
		extension = Extension,
		payload = Payload}
) ->
	io_lib:format("
		{	\"type\":\"rtp\",
			\"padding\":~b,
			\"marker\":~b,
			\"payload_type\":\"~s\",
			\"sequence_number\":~b,
			\"timestamp\":~b,
			\"ssrc\":~b,
			\"csrcs\":\"~p\",
			\"extension\":\"~p\",
			\"payload\":~p}", [Padding, Marker, print_rtp_payload_type(PT), SN, TS, SSRC, CSRCS, Extension, Payload]);
% RTCP

pp(#fir{ssrc = SSRC}) ->
	io_lib:format("{\"type\":\"fir\",\"ssrc\":~b}", [SSRC]);
pp(#nack{ssrc = SSRC, fsn = Fsn, blp = Blp}) ->
	io_lib:format("{\"type\":\"nack\",\"ssrc\":~b,\"fsn\":~b,\"blp\":~b}",
		[SSRC,Fsn,Blp]);
pp(#sr{ssrc = SSRC, ntp = NTP, timestamp = TS, packets = Packets, octets = Octets, rblocks = Rblocks}) ->
	<<NtpSec:32, NtpFrac:32>> = <<NTP:64>>,
	io_lib:format("{\"type\":\"sr\",\"ssrc\":~b,\"ntpsec\":~b,\"ntpfrac\":~b,\"timestamp\":~b,\"packets\":~b,\"octets\":~b,\"rblocks\":[~s]}",
		[SSRC, NtpSec, NtpFrac, TS, Packets, Octets, pp_rblocks(Rblocks)]);
pp(#rr{ssrc = SSRC, rblocks = Rblocks}) ->
	io_lib:format("{\"type\":\"rr\",\"ssrc\":~b,\"rblocks\":[~s]}",
		[SSRC, pp_rblocks(Rblocks)]);
pp(#sdes{list = SdesList}) ->
	io_lib:format("{\"type\":\"sdes\",\"list\":[~s]}", [pp_sdes(SdesList)]);
pp(#bye{ssrc = SSRCs, message = []}) ->
	io_lib:format("{\"type\":\"bye\",\"ssrc\":~p,\"message\":\"\"}",
		[SSRCs]);
pp(#bye{ssrc = SSRCs, message = Message}) ->
	io_lib:format("{\"type\":\"bye\",\"ssrc\":~p,\"message\":\"~s\"}",
		[SSRCs, fix_null_terminated(Message)]);
pp(#app{ssrc=SSRC, subtype=Subtype, name=Name, data=Data}) ->
	io_lib:format("{\"type\":\"app\",\"ssrc\":~b,\"subtype\":\"~b\",\"name\":\"~s\",\"data\":\"~p\"}",
		[SSRC, Subtype, Name, Data]);
pp(#xr{ssrc = SSRC, xrblocks = Xrblocks}) ->
	io_lib:format("{\"type\":\"xr\",\"ssrc\":~b,\"xrblocks\":\"~s\"}",
		[SSRC, pp_xrblocks(Xrblocks)]);
pp(Whatever) ->
	io_lib:format("{\"type\":\"unknown\",\"rawdata\":\"~p\"}", [Whatever]).

pp_rblocks([]) -> "{}";
pp_rblocks([#rblock{} = R | []]) ->
	io_lib:format("{\"ssrc\":~b,\"fraction\":~b,\"lost\":~b,\"last_seq\":~b,\"jitter\":~b,\"lsr\":~b,\"dlsr\":~b}",
		[R#rblock.ssrc, R#rblock.fraction, R#rblock.lost, R#rblock.last_seq, R#rblock.jitter, R#rblock.lsr, R#rblock.dlsr]);
pp_rblocks([#rblock{} = R | Rest]) ->
	io_lib:format("{\"ssrc\":~b,\"fraction\":~b,\"lost\":~b,\"last_seq\":~b,\"jitter\":~b,\"lsr\":~b,\"dlsr\":~b},",
		[R#rblock.ssrc, R#rblock.fraction, R#rblock.lost, R#rblock.last_seq, R#rblock.jitter, R#rblock.lsr, R#rblock.dlsr]) ++ pp_rblocks(Rest).

pp_xrblocks([]) -> "";
pp_xrblocks([#xrblock{type = Type, ts = Ts, data = Data} | []]) ->
	io_lib:format("{\"type\":~b,\"ts\":~b,\"data\":~p}", [Type, Ts, Data]);
pp_xrblocks([#xrblock{type = Type, ts = Ts, data = Data} | Rest]) ->
	io_lib:format("{\"type\":~b,\"ts\":~b,\"data\":~p},", [Type, Ts, Data]) ++ pp_xrblocks(Rest).

pp_sdes([]) -> "";
pp_sdes([R | Rest]) ->
	lists:flatten([pp_sdes_item(X,Y) || {X,Y} <- R]) ++ pp_sdes(Rest).

pp_sdes_item(ssrc, V) -> io_lib:format("{\"ssrc\":~b,", [V]);
pp_sdes_item(cname, V) -> io_lib:format("\"cname\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(name, V) -> io_lib:format("\"name\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(email, V) -> io_lib:format("\"email\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(phone, V) -> io_lib:format("\"phone\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(loc, V) -> io_lib:format("\"loc\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(tool, V) -> io_lib:format("\"tool\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(note, V) -> io_lib:format("\"note\":\"~s\",", [fix_null_terminated(V)]);
pp_sdes_item(priv, V) -> io_lib:format("\"priv\":~p,", [V]);
pp_sdes_item(eof, true) -> io_lib:format("\"eof\":true}", []);
pp_sdes_item(eof, V) -> io_lib:format("\"eof\"~p}", [V]).

fix_null_terminated(String) when is_list(String) ->
	% FIXME should we print \0 anyway?
%	[ case X of 0 -> "\\0"; _ -> X end || X <- String ].
	[ X || X <- String, X /= 0 ];
fix_null_terminated(Binary) when is_binary(Binary) ->
	<< <<X:8>> || <<X:8>> <= Binary, X /= 0 >>.

% http://www.iana.org/assignments/rtp-parameters
print_rtp_payload_type(?RTP_PAYLOAD_PCMU) -> "PCMU";
print_rtp_payload_type(1) -> "1 (Reserved)";
print_rtp_payload_type(2) -> "2 (Reserved)";
print_rtp_payload_type(?RTP_PAYLOAD_GSM) -> "GSM";
print_rtp_payload_type(?RTP_PAYLOAD_G723) -> "G723";
print_rtp_payload_type(?RTP_PAYLOAD_DVI4_8KHz) -> "DVI4";
print_rtp_payload_type(?RTP_PAYLOAD_DVI4_16KHz) -> "DVI4";
print_rtp_payload_type(?RTP_PAYLOAD_LPC) -> "LPC";
print_rtp_payload_type(?RTP_PAYLOAD_PCMA) -> "PCMA";
print_rtp_payload_type(?RTP_PAYLOAD_G722) -> "G722";
print_rtp_payload_type(?RTP_PAYLOAD_L16_2Ch) -> "L16";
print_rtp_payload_type(?RTP_PAYLOAD_L16_1Ch) -> "L16";
print_rtp_payload_type(?RTP_PAYLOAD_QCELP) -> "QCELP";
print_rtp_payload_type(?RTP_PAYLOAD_CN) -> "CN";
print_rtp_payload_type(?RTP_PAYLOAD_MPA) -> "MPA";
print_rtp_payload_type(?RTP_PAYLOAD_G728) -> "G728";
print_rtp_payload_type(?RTP_PAYLOAD_DVI4_11KHz) -> "DVI4";
print_rtp_payload_type(?RTP_PAYLOAD_DVI4_22KHz) -> "DVI4";
print_rtp_payload_type(?RTP_PAYLOAD_G729) -> "G729";
print_rtp_payload_type(19) -> "19 (Reserved)";
print_rtp_payload_type(20) -> "20 (Unassigned)";
print_rtp_payload_type(21) -> "21 (Unassigned)";
print_rtp_payload_type(22) -> "22 (Unassigned)";
print_rtp_payload_type(23) -> "23 (Unassigned)";
print_rtp_payload_type(24) -> "24 (Unassigned)";
print_rtp_payload_type(?RTP_PAYLOAD_CELB) -> "CelB";
print_rtp_payload_type(?RTP_PAYLOAD_JPEG) -> "JPEG";
print_rtp_payload_type(27) -> "27 (Unassigned)";
print_rtp_payload_type(?RTP_PAYLOAD_NV) -> "nv";
print_rtp_payload_type(29) -> "29 (Unassigned)";
print_rtp_payload_type(30) -> "30 (Unassigned)";
print_rtp_payload_type(?RTP_PAYLOAD_H261) -> "H261";
print_rtp_payload_type(?RTP_PAYLOAD_MPV) -> "MPV";
print_rtp_payload_type(?RTP_PAYLOAD_MP2T) -> "MP2T";
print_rtp_payload_type(?RTP_PAYLOAD_H263) -> "H263";
print_rtp_payload_type(Val) when Val >= 35, Val =< 63 -> io_lib:format("~b (Unassigned)", [Val]);
print_rtp_payload_type(64) -> "64 (Unassigned) (Wrongly decoded RTCP FIR)";
print_rtp_payload_type(65) -> "65 (Unassigned) (Wrongly decoded RTCP NACK)";
print_rtp_payload_type(66) -> "66 (Unassigned) (Wrongly decoded RTCP SMPTETC)";
print_rtp_payload_type(67) -> "67 (Unassigned) (Wrongly decoded RTCP IJ)";
print_rtp_payload_type(68) -> "68 (Unassigned)";
print_rtp_payload_type(69) -> "69 (Unassigned)";
print_rtp_payload_type(70) -> "70 (Unassigned)";
print_rtp_payload_type(71) -> "71 (Unassigned)";
print_rtp_payload_type(72) -> "72 (Wrongly decoded RTCP SR)";
print_rtp_payload_type(73) -> "73 (Wrongly decoded RTCP RR)";
print_rtp_payload_type(74) -> "74 (Wrongly decoded RTCP SDES)";
print_rtp_payload_type(75) -> "75 (Wrongly decoded RTCP BYE)";
print_rtp_payload_type(76) -> "76 (Wrongly decoded RTCP APP)";
print_rtp_payload_type(77) -> "77 (Unassigned) (Wrongly decoded RTCP RTPFB)";
print_rtp_payload_type(78) -> "78 (Unassigned) (Wrongly decoded RTCP PSFB)";
print_rtp_payload_type(79) -> "79 (Unassigned) (Wrongly decoded RTCP XR)";
print_rtp_payload_type(80) -> "80 (Unassigned) (Wrongly decoded RTCP AVB)";
print_rtp_payload_type(81) -> "81 (Unassigned) (Wrongly decoded RTCP RSI)";
print_rtp_payload_type(82) -> "82 (Unassigned) (Wrongly decoded RTCP TOKEN)";
print_rtp_payload_type(Val) when Val >= 83, Val =< 95 -> io_lib:format("~b (Unassigned)", [Val]);
print_rtp_payload_type(Val) when Val >= 96, Val =< 127 -> io_lib:format("~b (Dynamic)", [Val]);
print_rtp_payload_type(_) -> "unknown payload".


% FIXME use more atoms instead of numbers where possible
% grep "a=rtpmap:" /var/log/messages | sed -e 's,.*a=rtpmap:,,g' | sort | uniq | sort -n
% http://www.iana.org/assignments/rtp-parameters
% http://www.iana.org/assignments/media-types/audio/index.html
get_codec_from_payload(0) -> {'PCMU',8000,1};
% 1 and 2 are reserved
get_codec_from_payload(3) -> {'GSM',8000,1};
get_codec_from_payload(4) -> {'G723',8000,1};
get_codec_from_payload(5) -> {'DVI4',8000,1};
get_codec_from_payload(6) -> {'DVI4',16000,1};
get_codec_from_payload(7) -> {'LPC',8000,1};
get_codec_from_payload(8) -> {'PCMA',8000,1};
get_codec_from_payload(9) -> {'G722',8000,1};
get_codec_from_payload(10) -> {'L16',8000,2}; % FIXME 44100 according to RFC3551
get_codec_from_payload(11) -> {'L16',8000,1}; % FIXME 44100 according to RFC3551
get_codec_from_payload(12) -> {'QCELP',8000,1};
get_codec_from_payload(13) -> {'CN',8000,1};
get_codec_from_payload(14) -> {'MPA',90000,0};
get_codec_from_payload(15) -> {'G728',8000,1};
get_codec_from_payload(16) -> {'DVI4',11025,1};
get_codec_from_payload(17) -> {'DVI4',22050,1};
get_codec_from_payload(18) -> {'G729',8000,1};

get_codec_from_payload(31) -> {'H261',90000,0};
get_codec_from_payload(34) -> {'H263',90000,0};

get_codec_from_payload(C) when is_integer(C) -> C.

get_payload_from_codec({'PCMU',8000,1}) -> 0;
get_payload_from_codec({'GSM',8000,1}) -> 3;
get_payload_from_codec({'G723',8000,1}) -> 4;
get_payload_from_codec({'DVI4',8000,1}) -> 5;
get_payload_from_codec({'DVI4',16000,1}) -> 6;
get_payload_from_codec({'LPC',8000,1}) -> 7;
get_payload_from_codec({'PCMA',8000,1}) -> 8;
get_payload_from_codec({'G722',8000,1}) -> 9;
get_payload_from_codec({'L16',44100,2}) -> 10;
get_payload_from_codec({'L16',44100,1}) -> 11;
get_payload_from_codec({'QCELP',8000,1}) -> 12;
get_payload_from_codec({'CN',8000,1}) -> 13;
get_payload_from_codec({'MPA',90000,0}) -> 14;
get_payload_from_codec({'G728',8000,1}) -> 15;
get_payload_from_codec({'DVI4',11025,1}) -> 16;
get_payload_from_codec({'DVI4',22050,1}) -> 17;
get_payload_from_codec({'G729',8000,1}) -> 18;
get_payload_from_codec({'H261',90000,0}) -> 31;
get_payload_from_codec({'H263',90000,0}) -> 34;

get_payload_from_codec(C) when is_integer(C) -> C.

mktimestamp(PayloadType, InitialTime) ->
	{_, Clock, _} = get_codec_from_payload(PayloadType),
	{MegaSecs, Secs, MicroSecs} = os:timestamp(),
	Millisecs = MegaSecs*1000000000 + Secs*1000 + (MicroSecs div 1000) - InitialTime,
	round(Millisecs * (Clock  / 1000)).
