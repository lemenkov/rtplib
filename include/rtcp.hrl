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

% http://www.iana.org/assignments/rtp-parameters

% See these RFCs for further details:

% http://www.ietf.org/rfc/rfc2032.txt
% http://www.ietf.org/rfc/rfc3550.txt
% http://www.ietf.org/rfc/rfc3611.txt
% http://www.ietf.org/rfc/rfc4585.txt
% http://www.ietf.org/rfc/rfc5450.txt
% http://www.ietf.org/rfc/rfc5484.txt

% Version is always 2
-define(RTCP_VERSION, 2).

-define(RTCP_FIR,  192). % RFC 2032
-define(RTCP_NACK, 193). % RFC 2032
-define(RTCP_SMPTETC, 194). % RFC 5484
-define(RTCP_IJ,   195). % RFC 5450
-define(RTCP_SR,   200). % RFC 3550
-define(RTCP_RR,   201). % RFC 3550
-define(RTCP_SDES, 202). % RFC 3550
-define(RTCP_BYE,  203). % RFC 3550
-define(RTCP_APP,  204). % RFC 3550
-define(RTCP_RTPFB,205). % RFC 4585
-define(RTCP_PSFB, 206). % RFC 4585
-define(RTCP_XR,   207). % RFC 3611
-define(RTCP_AVB,  208). % IEEE 1733
%-define(RTCP_RSI,  209). % RFC 5760 FIXME
%-define(RTCP_TOKEN, 210). % RFC 6285 FIXME

-define(SDES_NULL,  0).
-define(SDES_CNAME, 1).
-define(SDES_NAME,  2).
-define(SDES_EMAIL, 3).
-define(SDES_PHONE, 4).
-define(SDES_LOC,   5).
-define(SDES_TOOL,  6).
-define(SDES_NOTE,  7).
-define(SDES_PRIV,  8).
%-define(SDES_H323_CADDR, 9). % FIXME

-define(PADDING_YES, 1).
-define(PADDING_NO, 0).

% A compound structure for RTCP (or multiple RTCP packets stacked together
-record(rtcp, {
		payloads = [],
		encrypted = null
	}).

% Full INTRA-frame Request (h.261 specific)
-record(fir, {ssrc}).
% Negative ACKnowledgements (h.261 specific)
-record(nack, {ssrc, fsn, blp}).
-record(smptetc, {ssrc, timestamp, sign, hours, minutes, seconds, frames, smpte12m}).
% Sender Report
% * NTP - NTP timestamp
% * TimeStamp - RTP timestamp
% * Packets - sender's packet count
% * Octets - sender's octet count
-record(sr, {ssrc, ntp, timestamp, packets, octets, rblocks=[]}).
% Receiver Report and Inter-arrival Jitter (must be placed after a receiver report and MUST have the same value for RC)
-record(rr, {ssrc, rblocks=[], ijs=[]}).
% Source DEScription
-record(sdes, {list}).
% End of stream (but not necessary the end of communication, since there may be
% many streams within)
-record(bye, {message=[], ssrc=[]}).
% Application-specific data
-record(app, {subtype, ssrc=[], name=[], data=null}).
% eXtended Report
-record(xr, {ssrc, xrblocks=[]}).
% Generic NACK
-record(gnack, {ssrc_s, ssrc_m, list}).
% Picture Loss Indication
-record(pli, {ssrc_s, ssrc_m}).
% Slice Loss Indication
-record(sli, {ssrc_s, ssrc_m, slis}).
% Reference Picture Selection Indication
-record(rpsi, {ssrc_s, ssrc_m, type, bitlength, payload}).
% Application Layer Feedback Messages
-record(alfb, {ssrc_s, ssrc_m, data}).
% IEEE 1733 AVB
-record(avb, {ssrc, name, gmtbi, gmid, sid, astime, rtptime}).

% ReportBlocks counted (RC) by 1)
% * SSRC - SSRC of the source
% * FL - fraction lost
% * CNPL - cumulative number of packets lost
% * EHSNR - extended highest sequence number received
% * IJ - interarrival jitter
% * LSR - last SR timestamp
% * DLSR - delay since last SR
-record(rblock, {ssrc, fraction, lost, last_seq, jitter, lsr, dlsr}).
-record(xrblock, {type, ts, data}).
