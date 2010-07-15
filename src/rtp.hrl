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
