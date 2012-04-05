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
-define(ZRTP_MAGIC_COOKIE, 16#5a525450).

-define(ZRTP_MSG_HELLO,		"Hello   ").
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

-define(ZRTP_CIPHER_AES1, "AES1").
-define(ZRTP_CIPHER_AES2, "AES2").
-define(ZRTP_CIPHER_AES3, "AES3").
-define(ZRTP_CIPHER_2FS1, "2FS1").
-define(ZRTP_CIPHER_2FS2, "2FS2").
-define(ZRTP_CIPHER_2FS3, "2FS3").

-define(ZRTP_AUTH_TAG_HS32, "HS32").
-define(ZRTP_AUTH_TAG_HS80, "HS80").
-define(ZRTP_AUTH_TAG_SK32, "SK32").
-define(ZRTP_AUTH_TAG_SK64, "SK64").

-define(ZRTP_KEY_AGREEMENT_DH3K, "DH3k").
-define(ZRTP_KEY_AGREEMENT_DH2K, "DH2k").
-define(ZRTP_KEY_AGREEMENT_EC25, "EC25").
-define(ZRTP_KEY_AGREEMENT_EC38, "EC38").
-define(ZRTP_KEY_AGREEMENT_EC52, "EC52").
-define(ZRTP_KEY_AGREEMENT_PRSH, "Prsh").
-define(ZRTP_KEY_AGREEMENT_MULT, "Mult").

-define(ZRTP_SAS_TYPE_B32, "B32 ").
-define(ZRTP_SAS_TYPE_B256, "B256").

-define(ZRTP_SIGNATURE_TYPE_PGP, "PGP ").
-define(ZRTP_SIGNATURE_TYPE_X509, "X509").

-define(ZRTP_SIGNATURE_HELLO, 16#505a).

-define(ZRTP_VERSION, "1.10").
-define(ZRTP_SOFTWARE, "Erlang (Z)RTPLIB").

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
		mac
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
		mac
	}).

-record(dhpart1, {
		h1,
		rs1IDr,
		rs2IDr,
		auxsecretIDr,
		pbxsecretIDr,
		pvr,
		mac
	}).
-record(dhpart2, {
		h1,
		rs1IDi,
		rs2IDi,
		auxsecretIDi,
		pbxsecretIDi,
		pvi,
		mac
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
