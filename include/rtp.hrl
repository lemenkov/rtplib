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

-define(RTP_VERSION, 2).

-record(rtp, {	padding = 0,
		marker = 0,
		payload_type,
		sequence_number,
		timestamp,
		ssrc,
		csrcs = [],
		extension = null,
		payload = <<>>}).

-record(extension, {type, payload}).

-record(rfc2833, {event, eof, volume, duration}).

% http://www.iana.org/assignments/rtp-parameters
% http://en.wikipedia.org/wiki/RTP_audio_video_profile
% See these RFCs for further details:

% http://www.ietf.org/rfc/rfc2029.txt
% http://www.ietf.org/rfc/rfc2190.txt
% http://www.ietf.org/rfc/rfc2250.txt
% http://www.ietf.org/rfc/rfc2435.txt
% http://www.ietf.org/rfc/rfc2658.txt
% http://www.ietf.org/rfc/rfc3389.txt
% http://www.ietf.org/rfc/rfc3551.txt
% http://www.ietf.org/rfc/rfc4587.txt
% http://www.cisco.com/en/US/tech/tk652/tk698/technologies_tech_note09186a0080094ae2.shtml

-define(RTP_PAYLOAD_PCMU,	0).
-define(RTP_PAYLOAD_GSM,	3).
-define(RTP_PAYLOAD_G723,	4).
-define(RTP_PAYLOAD_DVI4_8KHz,	5).
-define(RTP_PAYLOAD_DVI4_16KHz,	6).
-define(RTP_PAYLOAD_LPC,	7).
-define(RTP_PAYLOAD_PCMA,	8).
-define(RTP_PAYLOAD_G722,	9).
-define(RTP_PAYLOAD_L16_2Ch,	10).
-define(RTP_PAYLOAD_L16_1Ch,	11).
-define(RTP_PAYLOAD_QCELP,	12).
-define(RTP_PAYLOAD_CN,		13). % RFC 3389
-define(RTP_PAYLOAD_MPA,	14).
-define(RTP_PAYLOAD_G728,	15).
-define(RTP_PAYLOAD_DVI4_11KHz,	16).
-define(RTP_PAYLOAD_DVI4_22KHz,	17).
-define(RTP_PAYLOAD_G729,	18).
-define(RTP_PAYLOAD_CELB,	25). % RFC 2029
-define(RTP_PAYLOAD_JPEG,	26). % RFC 2435
-define(RTP_PAYLOAD_NV,		28).
-define(RTP_PAYLOAD_H261,	31). % RFC 4587
-define(RTP_PAYLOAD_MPV,	32). % RFC 2250
-define(RTP_PAYLOAD_MP2T,	33). % RFC 2250
-define(RTP_PAYLOAD_H263,	34).
