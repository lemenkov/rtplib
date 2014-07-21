%%
%% See /usr/include/linux/netfilter.h
%%

-define(NFPROTO_UNSPEC, 0).
-define(NFPROTO_IPV4,   2).
-define(NFPROTO_ARP,    3).
-define(NFPROTO_BRIDGE, 7).
-define(NFPROTO_IPV6,  10).
-define(NFPROTO_DECNET,12).

-record(mediaproxy_address, {
	ip = null :: inet:ip_address(),
	port = 0 :: inet:port_number()
}).

-type mediaproxy_address() :: #mediaproxy_address{}.

-type mediaproxy_cipher() :: invalid | null | aes_cm | aes_f8.

-define(MEDIAPROXY_CIPHER_INVALID, 0).
-define(MEDIAPROXY_CIPHER_NULL, 1).
-define(MEDIAPROXY_CIPHER_AES_CM, 2).
-define(MEDIAPROXY_CIPHER_AES_F8, 3).

-type mediaproxy_hmac() :: invalid | null | sha1.

-define(MEDIAPROXY_HMAC_INVALID, 0).
-define(MEDIAPROXY_HMAC_NULL, 1).
-define(MEDIAPROXY_HMAC_SHA1, 2).

-record(mediaproxy_srtp, {
	cipher = null :: mediaproxy_cipher(),
	hmac = null :: mediaproxy_hmac(),
	master_key = <<0:128>> :: <<_:128>>,
	master_salt = <<0:112>> :: <<_:112>>,
	mki = 0 :: non_neg_integer(),
	last_index = 0 :: non_neg_integer(),
	auth_tag_len = 0 :: 0..255,
	mki_len = 0 :: 0..255
}).

-type mediaproxy_srtp() :: #mediaproxy_srtp{}.

-define(SIZEOF_MEDIAPROXY_TARGET_INFO, 216).

-record(mediaproxy_target_info, {
	target_port = 0 :: inet:port_number(),
	src_addr = null :: null | mediaproxy_address(),
	dst_addr = null :: null | mediaproxy_address(),
	mirror_addr = null :: null | mediaproxy_address(),
	decrypt = #mediaproxy_srtp{} :: null | mediaproxy_srtp(),
	encrypt = #mediaproxy_srtp{} :: null | mediaproxy_srtp(),
	tos = 0 :: 0..255,
	rtcp_mux = false :: boolean()
}).

-type mediaproxy_target_info() :: #mediaproxy_target_info{}.

-type mediaproxy_cmd() :: noop | add | del | update.

-define(MEDIAPROXY_CMD_NOOP,   1).
-define(MEDIAPROXY_CMD_ADD,    2).
-define(MEDIAPROXY_CMD_DEL,    3).
-define(MEDIAPROXY_CMD_UPDATE, 4).

-record(mediaproxy_message, {
	cmd = noop :: mediaproxy_cmd(),
	target = null :: null | mediaproxy_target_info()
}).

-type mediaproxy_message() :: #mediaproxy_message{}.

-record(mediaproxy_stats, {
	packets = 0 :: non_neg_integer(),
	bytes = 0 :: non_neg_integer(),
	errors = 0 :: non_neg_integer()
}).

-type mediaproxy_stats() :: #mediaproxy_stats{}.

-define(SIZEOF_MEDIAPROXY_LIST_ENTRY, 240).

-record(mediaproxy_list_entry, {
	target = null :: null | #mediaproxy_target_info{},
	stats = null :: null | #mediaproxy_stats{}
}).

-type mediaproxy_list_entry() :: #mediaproxy_list_entry{}.
