% http://www.iana.org/assignments/rtp-parameters

% See these RFCs for further details:

% http://www.ietf.org/rfc/rfc2032.txt
% http://www.ietf.org/rfc/rfc3550.txt
% http://www.ietf.org/rfc/rfc3611.txt

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
-define(RTCP_RSI,  209). % RFC 5760

-define(SDES_NULL,  0).
-define(SDES_CNAME, 1).
-define(SDES_NAME,  2).
-define(SDES_EMAIL, 3).
-define(SDES_PHONE, 4).
-define(SDES_LOC,   5).
-define(SDES_TOOL,  6).
-define(SDES_NOTE,  7).
-define(SDES_PRIV,  8).

-define(PADDING_YES, 1).
-define(PADDING_NO, 0).

% Must be zero
-define(MBZ, 0).

% Full INTRA-frame Request (h.261 specific)
-record(fir, {ssrc}).
% Negative ACKnowledgements (h.261 specific)
-record(nack, {ssrc, fsn, blp}).
% Sender Report
% * NTP - NTP timestamp
% * TimeStamp - RTP timestamp
% * Packets - sender's packet count
% * Octets - sender's octet count
-record(sr, {ssrc, ntp, timestamp, packets, octets, rblocks=[]}).
% Receiver Report
-record(rr, {ssrc, rblocks=[]}).
% Source DEScription
-record(sdes, {list}).
% End of stream (but not necessary the end of communication, since there may be
% many streams within)
-record(bye, {message=[], ssrc=[]}).
% Application-specific data
-record(app, {subtype, ssrc=[], name=[], data=null}).
% eXtended Report
-record(xr, {ssrc, xrblocks=[]}).

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
-record(sdes_items, {ssrc, cname=null, name=null, email=null, phone=null, loc=null, tool=null, note=null, priv=null, eof=false}).

