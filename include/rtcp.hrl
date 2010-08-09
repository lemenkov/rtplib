% http://www.iana.org/assignments/rtp-parameters

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

-record(fir, {ssrc}).
-record(nack, {ssrc, fsn, blp}).
-record(sr, {ssrc, ntp, timestamp, packets, octets, rblocks=[]}).
-record(rr, {ssrc, rblocks=[]}).
-record(sdes, {list}).
-record(bye, {message=[], ssrc=[]}).
-record(app, {subtype, ssrc=[], name=[], data=null}).
-record(xr, {ssrc, xrblocks=[]}).

-record(rblock, {ssrc, fraction, lost, last_seq, jitter, lsr, dlsr}).
-record(xrblock, {type, ts, data}).
-record(sdes_items, {ssrc, cname, name=null, email=null, phone=null, loc=null, tool=null, note=null, priv=null, eof=false}).

