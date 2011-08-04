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

-module(rtp_utils).
-author('lemenkov@gmail.com').

-export([dump_packet/3]).
-export([get_type/1]).

-export([ntp2now/1]).
-export([now2ntp/0]).
-export([now2ntp/1]).

-export([pp/1]).

-include("../include/rtcp.hrl").

dump_packet(Node, Pid, Packet) ->
	{H,M,Ms} = now(),
	% later we may try to decode these rtcp packets and to fix decoding errors:
	% lists:map(fun(X) -> io:format("FILE: ~p~n", [X]), {ok, Rtcp} = file:read_file(X), rtcp:decode(Rtcp) end, filelib:wildcard("/tmp/rtcp_err.*.bin")).
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
	now2ntp (now()).

now2ntp ({MegaSecs, Secs, MicroSecs}) ->
	% 2208988800 is the number of seconds from 00:00:00 01-01-1900 to 00:00:00 01-01-1970
	NTPSec = MegaSecs*1000000 + Secs + 2208988800,
	NTPFrac = frac(MicroSecs),
	<<NTPSec:32, NTPFrac:32>>.

pp(#fir{} = Rec) ->
	io_lib:format("{\"type\":\"fir\",\"ssrc\":~b}", [Rec#fir.ssrc]);
pp(#nack{} = Rec) ->
	io_lib:format("{\"type\":\"nack\",\"ssrc\":~b,\"fsn\":~b,\"blp\":~b}", [Rec#nack.ssrc,Rec#nack.fsn,Rec#nack.blp]);
pp(#sr{} = Rec) ->
	NTP = Rec#sr.ntp,
	<<NtpSec:32, NtpFrac:32>> = <<NTP:64>>,
	io_lib:format("{\"type\":\"sr\",\"ssrc\":~b,\"ntpsec\":~b,\"ntpfrac\":~b,\"timestamp\":~b,\"packets\":~b,\"octets\":~b,\"rblocks\":[~s]}",
		[Rec#sr.ssrc, NtpSec, NtpFrac, Rec#sr.timestamp, Rec#sr.packets, Rec#sr.octets, pp_rblocks(Rec#sr.rblocks)]);
pp(#rr{} = Rec) ->
	io_lib:format("{\"type\":\"rr\",\"ssrc\":~b,\"rblocks\":[~s]}",
		[Rec#rr.ssrc, pp_rblocks(Rec#rr.rblocks)]);
pp(#sdes{} = Rec) ->
	io_lib:format("{\"type\":\"sdes\",\"list\":[~s]}", [pp_sdes(Rec#sdes.list)]);
pp(#bye{} = Rec) ->
	io_lib:format("{\"type\":\"bye\",\"ssrc\":~p,\"message\":\"~s\"}",
		[Rec#bye.ssrc, Rec#bye.message]);
pp(#app{} = Rec) ->
	io_lib:format("{\"type\":\"app\",\"ssrc\":~b,\"name\":\"~s\",\"data\":\"~p\"}",
		[Rec#app.ssrc, Rec#app.name, Rec#app.data]);
pp(#xr{} = Rec) ->
	io_lib:format("{\"type\":\"xr\",\"ssrc\":~b,\"xrblocks\":\"~s\"}",
		[Rec#xr.ssrc, pp_xrblocks(Rec#xr.xrblocks)]);
pp(Whatever) ->
	io_lib:format("{\"type\":\"unknown\",\"rawdata\":\"~p\"}", [Whatever]).

pp_rblocks([]) -> "";
pp_rblocks([#rblock{} = R | Rest]) ->
	io_lib:format("{\"ssrc\":~b,\"fraction\":~b,\"lost\":~b,\"last_seq\":~b,\"jitter\":~b,\"lsr\":~b,\"dlsr\":~b},",
		[R#rblock.ssrc, R#rblock.fraction, R#rblock.lost, R#rblock.last_seq, R#rblock.jitter, R#rblock.lsr, R#rblock.dlsr]) ++ pp_rblocks(Rest).

pp_xrblocks([]) -> "{}";
pp_xrblocks([#xrblock{} = R | Rest]) ->
	io_lib:format("{\"type\":~b,\"ts\":~b,\"data\":~p},",
		[R#xrblock.type, R#xrblock.ts, R#xrblock.data]) ++ pp_xrblocks(Rest).

pp_sdes([]) -> "";
pp_sdes([R | Rest]) ->
	lists:flatten([pp_sdes_item(X,Y) || {X,Y} <- R]) ++ pp_sdes(Rest).

pp_sdes_item(ssrc, V) -> io_lib:format("{\"ssrc\":~b,", [V]);
pp_sdes_item(cname, V) -> io_lib:format("\"cname\":\"~s\",", [V]);
pp_sdes_item(name, V) -> io_lib:format("\"name\":\"~s\",", [V]);
pp_sdes_item(email, V) -> io_lib:format("\"email\":\"~s\",", [V]);
pp_sdes_item(phone, V) -> io_lib:format("\"phone\":\"~s\",", [V]);
pp_sdes_item(loc, V) -> io_lib:format("\"loc\":\"~s\",", [V]);
pp_sdes_item(tool, V) -> io_lib:format("\"tool\":\"~s\",", [V]);
pp_sdes_item(note, V) -> io_lib:format("\"note\":\"~s\",", [V]);
pp_sdes_item(priv, V) -> io_lib:format("\"priv\":~p,", [V]);
pp_sdes_item(eof, true) -> io_lib:format("\"eof\":true}", []);
pp_sdes_item(eof, V) -> io_lib:format("\"eof\"~p}", [V]).
