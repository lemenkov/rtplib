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

% http://tools.ietf.org/html/rfc6189

-define(ZRTP_MARKER, 16#1000).
-define(ZRTP_MAGIC_COOKIE, 16#5a525450). % <<"ZRTP">>, <<90,82,84,80>>

-define(ZRTP_MSG_HELLO,		"Hello   "). % <<72,101,108,108,111,32,32,32>>
-define(ZRTP_MSG_HELLOACK,	"HelloACK").
-define(ZRTP_MSG_COMMIT,	"Commit  ").
-define(ZRTP_MSG_DHPART1,	"DHPart1 ").
-define(ZRTP_MSG_DHPART2,	"DHPart2 ").
-define(ZRTP_MSG_CONFIRM1,	"Confirm1").
-define(ZRTP_MSG_CONFIRM2,	"Confirm2").
-define(ZRTP_MSG_CONF2ACK,	"Conf2ACK").
-define(ZRTP_MSG_ERROR,		"Error   ").
-define(ZRTP_MSG_ERRORACK,	"ErrorACK").
-define(ZRTP_MSG_GOCLEAR,	"GoClear ").
-define(ZRTP_MSG_CLEARACK,	"ClearACK").
-define(ZRTP_MSG_SASRELAY,	"SASrelay").
-define(ZRTP_MSG_RELAYACK,	"RelayACK").
-define(ZRTP_MSG_PING,		"Ping    ").
-define(ZRTP_MSG_PINGACK,	"PingACK ").

-define(ZRTP_HASH_S256, "S256").
-define(ZRTP_HASH_S384, "S384").
-define(ZRTP_HASH_N256, "N256").
-define(ZRTP_HASH_N384, "N384").
-define(ZRTP_HASH_ALL_SUPPORTED, [?ZRTP_HASH_S256, ?ZRTP_HASH_S384]).

-define(ZRTP_CIPHER_AES1, "AES1").
-define(ZRTP_CIPHER_AES2, "AES2").
-define(ZRTP_CIPHER_AES3, "AES3").
-define(ZRTP_CIPHER_2FS1, "2FS1").
-define(ZRTP_CIPHER_2FS2, "2FS2").
-define(ZRTP_CIPHER_2FS3, "2FS3").
-define(ZRTP_CIPHER_ALL_SUPPORTED, [?ZRTP_CIPHER_AES1, ?ZRTP_CIPHER_AES2, ?ZRTP_CIPHER_AES3]).

-define(ZRTP_AUTH_TAG_HS32, "HS32").
-define(ZRTP_AUTH_TAG_HS80, "HS80").
-define(ZRTP_AUTH_TAG_SK32, "SK32").
-define(ZRTP_AUTH_TAG_SK64, "SK64").
-define(ZRTP_AUTH_ALL_SUPPORTED, [?ZRTP_AUTH_TAG_HS32, ?ZRTP_AUTH_TAG_HS80]).

-define(ZRTP_KEY_AGREEMENT_DH2K, "DH2k"). % DH mode with p=2048 bit prime per RFC 3526, Section 3.
-define(ZRTP_KEY_AGREEMENT_DH3K, "DH3k"). % DH mode with p=3072 bit prime per RFC 3526, Section 4.
-define(ZRTP_KEY_AGREEMENT_EC25, "EC25"). % Elliptic Curve DH, P-256 per RFC 5114, Section 2.6
-define(ZRTP_KEY_AGREEMENT_EC38, "EC38"). % Elliptic Curve DH, P-384 per RFC 5114, Section 2.7
-define(ZRTP_KEY_AGREEMENT_EC52, "EC52"). % Elliptic Curve DH, P-521 per RFC 5114, Section 2.8 (deprecated - do not use)
-define(ZRTP_KEY_AGREEMENT_PRSH, "Prsh"). % Preshared Non-DH mode
-define(ZRTP_KEY_AGREEMENT_MULT, "Mult"). % Multistream Non-DH mode
-define(ZRTP_KEY_AGREEMENT_ALL_SUPPORTED, [?ZRTP_KEY_AGREEMENT_DH2K, ?ZRTP_KEY_AGREEMENT_DH3K]).

-define(ZRTP_SAS_TYPE_B32, "B32 ").
-define(ZRTP_SAS_TYPE_B256, "B256").
-define(ZRTP_SAS_TYPE_ALL_SUPPORTED, [?ZRTP_SAS_TYPE_B32, ?ZRTP_SAS_TYPE_B256]).

-define(ZRTP_SIGNATURE_TYPE_PGP, "PGP ").
-define(ZRTP_SIGNATURE_TYPE_X509, "X509").

-define(ZRTP_SIGNATURE_HELLO, 16#505a).

-define(ZRTP_VERSION, "1.10").
-define(ZRTP_SOFTWARE, "Erlang (Z)RTPLIB").

-define(HASH_IMAGE_SIZE, 32).

-define(STR_INITIATOR, "Initiator").
-define(STR_RESPONDER, "Responder").

-define(ZRTP_ERROR_MALFORMED_PACKET, 16#10). % Malformed packet (CRC OK, but wrong structure)
-define(ZRTP_ERROR_SOFTWARE, 16#20). % Critical software error
-define(ZRTP_ERROR_UNSUPPORTED_VERSION, 16#30). % Unsupported ZRTP version
-define(ZRTP_ERROR_HELLO_MISMATCH, 16#40). % Hello components mismatch
-define(ZRTP_ERROR_UNSUPPORTED_HASH, 16#51). % Hash Type not supported
-define(ZRTP_ERROR_UNSUPPORTED_CYPHER, 16#52). % Cipher Type not supported
-define(ZRTP_ERROR_UNSUPPORTED_KEY_EXCHANGE, 16#53). % Public key exchange not supported
-define(ZRTP_ERROR_UNSUPPORTED_AUTH_TAG, 16#54). % SRTP auth tag not supported
-define(ZRTP_ERROR_UNSUPPORTED_SAS, 16#55). % SAS rendering scheme not supported
-define(ZRTP_ERROR_NO_SHARED_SECRETS, 16#56). % No shared secret available, DH mode required
-define(ZRTP_ERROR_DH_BAD_PV, 16#61). % DH Error: bad pvi or pvr ( == 1, 0, or p-1)
-define(ZRTP_ERROR_DH_BAD_HV, 16#62). % DH Error: hvi != hashed data
-define(ZRTP_ERROR_MITM, 16#63). % Received relayed SAS from untrusted MiTM
-define(ZRTP_ERROR_MAC, 16#70). % Auth Error: Bad Confirm pkt MAC
-define(ZRTP_ERROR_NONCE, 16#80). % Nonce reuse
-define(ZRTP_ERROR_ZID, 16#90). % Equal ZIDs in Hello
-define(ZRTP_ERROR_SSRC, 16#91). % SSRC collision
-define(ZRTP_ERROR_UNAVAILABLE, 16#A0). % Service unavailable
-define(ZRTP_ERROR_TIMEOUT, 16#B0). % Protocol timeout error
-define(ZRTP_ERROR_GOCLEAR_NA, 16#100). % GoClear message received, but not allowed

-record(zrtp, {
		sequence = 0,
		ssrc = 0,
		message = null
	}).

-record(hello, {
		clientid,
		h3,
		zid,
		s,
		m,
		p,
		hash = [],
		cipher = [],
		auth = [],
		keyagr = [],
		sas = [],
		mac = <<0,0,0,0,0,0,0,0>>
	}).

-record(commit, {
		h2,
		zid,
		hash,
		cipher,
		auth,
		keyagr,
		sas,
		hvi = null,
		nonce = null,
		keyid = null,
		mac = <<0,0,0,0,0,0,0,0>>
	}).

-record(dhpart1, {
		h1,
		rs1IDr,
		rs2IDr,
		auxsecretIDr,
		pbxsecretIDr,
		pvr,
		mac = <<0,0,0,0,0,0,0,0>>
	}).
-record(dhpart2, {
		h1,
		rs1IDi,
		rs2IDi,
		auxsecretIDi,
		pbxsecretIDi,
		pvi,
		mac = <<0,0,0,0,0,0,0,0>>
	}).

-record(confirm1, {
		conf_mac,
		cfb_init_vect,
		h0 = null,
		pbx_enrollement = null,
		sas_verified = null,
		allow_clear = null,
		disclosure = null,
		cache_exp_interval = null,
		signature = null,
		encrypted_data = null
	}).

-record(confirm2, {
		conf_mac,
		cfb_init_vect,
		h0 = null,
		pbx_enrollement = null,
		sas_verified = null,
		allow_clear = null,
		disclosure = null,
		cache_exp_interval = null,
		signature = null,
		encrypted_data = null
	}).

-record(error, {code}).
-record(goclear, {mac}).

-record(sasrelay, {
		mac,
		cfb_init_vect,
		sas_verified,
		allow_clear,
		disclosure,
		sas_rend_scheme,
		mitm_sash_hash,
		signature = null
	}).

-record(ping, {hash}).

-record(pingack, {
		sender_hash,
		receiver_hash,
		ssrc
	}).

-record(signature, {type, data}).
