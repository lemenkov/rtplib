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

% See these RFCs for further details:

% http://www.ietf.org/rfc/rfc2029.txt
% http://www.ietf.org/rfc/rfc2190.txt
% http://www.ietf.org/rfc/rfc2250.txt
% http://www.ietf.org/rfc/rfc2435.txt
% http://www.ietf.org/rfc/rfc2658.txt
% http://www.ietf.org/rfc/rfc3389.txt
% http://www.ietf.org/rfc/rfc3551.txt
% http://www.ietf.org/rfc/rfc4587.txt

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
