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

-module(srtp).
-author('lemenkov@gmail.com').

-export([encrypt/2]).
-export([decrypt/2]).

-include("../include/rtcp.hrl").
-include("../include/rtp.hrl").

encrypt(#rtp{} = Rtp, passthru) ->
	rtp:encode(Rtp);
encrypt(#rtcp{encrypted = Data} = Rctp, passthru) ->
	Data;
encrypt(#rtp{} = Rtp, Key) ->
	% TODO - 
	rtp:encode(Rtp);
encrypt(#rtcp{} = Rctp, Key) ->
	% TODO - 
	rtcp:encode(Rctp).

decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, passthru) when PayloadType =< 34; 96 =< PayloadType ->
	rtp:decode(Data);
decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, passthru) when 64 =< PayloadType, PayloadType =< 82 ->
	{ok, #rtcp{encrypted = Data}};
decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, Key) when PayloadType =< 34; 96 =< PayloadType ->
	% TODO
	rtp:decode(Data);
decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, passthru) when 64 =< PayloadType, PayloadType =< 82 ->
	% TODO
	rtcp:decode(Data).
