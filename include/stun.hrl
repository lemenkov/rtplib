-define(STUN_MARKER, 0).
-define(MAGIC_COOKIE, 16#2112A442).

-record(stun, {
		class = null,
		method = 0,
		length = 0,
		transactionid = <<>>
	}).
