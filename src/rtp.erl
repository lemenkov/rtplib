
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

-include("rtp.hrl").

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
	#rtp{
		padding = Padding,
		marker = Marker,
		payload_type = PayloadType,
		sequence_number = SequenceNumber,
		timestamp = Timestamp,
		ssrc = SSRC,
		csrcs = CSRCs,
		extension = Extension,
		payload = Payload
	}.

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

% TODO Padding, Extension
encode(#rtp{padding = P, marker = M, payload_type = PT, sequence_number = SN, timestamp = TS, ssrc = SSRC, csrcs = CSRCs, extension = X, payload = P}) ->
	CC = length(CSRCs),
	CSRC_Data = list_to_binary([<<CSRC:32>> || CSRC <- CSRCs]),
	<<?RTP_VERSION:2, P:1, X:1, CC:4, M:1, PT:7, SN:16, TS:32, SSRC:32, CSRC_Data/binary, P/binary>>.
