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

-module(zrtp).
-author('lemenkov@gmail.com').

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

% FIXME - don't forget to remove from final version!
-compile(export_all).

-include("../include/zrtp.hrl").

-record(state, {
		parent = null,
		zid,
		ssrc,
		h0,
		h1,
		h2,
		h3,
		iv,
		hash = null,
		cipher = null,
		auth = null,
		keyagr = null,
		sas = null,

		rs1IDi = null,
		rs1IDr = null,
		rs2IDi = null,
		rs2IDr = null,
		auxSecretIDi = null,
		auxSecretIDr = null,
		pbxSecretIDi = null,
		pbxSecretIDr = null,
		dhPriv = null,
		dhPubl = null,
		shared = <<>>,
		s0 = null,

		srtp_key_i = null,
		srtp_salt_i = null,
		srtp_key_r = null,
		srtp_salt_r = null,
		hmac_key_i = null,
		hmac_key_r = null,
		confirm_key_i = null,
		confirm_key_r = null,
		sas_val = null,

		other_zid = null,
		other_ssrc = null,
		other_h0 = null,
		other_h1 = null,
		other_h2 = null,
		other_h3 = null,
		prev_sn = 0,
		storage = null
	}
).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

%% Generate Alice's ZRTP server
init([Parent])->
	init([Parent, null, null]);
init([Parent, ZID, SSRC]) ->
	init([Parent, ZID, SSRC, ?ZRTP_HASH_ALL_SUPPORTED, ?ZRTP_CIPHER_ALL_SUPPORTED, ?ZRTP_AUTH_ALL_SUPPORTED, ?ZRTP_KEY_AGREEMENT_ALL_SUPPORTED, ?ZRTP_SAS_TYPE_ALL_SUPPORTED]);
init([Parent, ZID, SSRC, Hashes, Ciphers, Auths, KeyAgreements, SASTypes] = Params) when is_binary(ZID) ->
	% Deferred init
	self() ! {init, Params},
	{ok, #state{}}.

handle_call(
	init,
	_From,
	#state{
		zid = ZID,
		ssrc = MySSRC,
		h3 = H3,
		h2 = H2,
		storage = Tid
	} = State) ->

	HelloMsg = #hello{
		h3 = H3,
		zid = ZID,
		s = 0, % FIXME allow checking digital signature (see http://zfone.com/docs/ietf/rfc6189bis.html#SignSAS )
		m = 1, % FIXME allow to set to false
		p = 0, % We can send COMMIT messages
		hash = ets:lookup_element(Tid, hash, 2),
		cipher = ets:lookup_element(Tid, cipher, 2),
		auth = ets:lookup_element(Tid, auth, 2),
		keyagr = ets:lookup_element(Tid, keyagr, 2),
		sas = ets:lookup_element(Tid, sas, 2)
	},

	Hello = #zrtp{
		sequence = 1,
		ssrc = MySSRC,
		message = HelloMsg#hello{mac = mkhmac(HelloMsg, H2)}
	},

	% Store full Alice's HELLO message
	ets:insert(Tid, {{alice, hello}, Hello}),

	{reply, Hello, State};

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #hello{
			h3 = HashImageH3,
			zid = ZID,
			s = S,
			m = M,
			p = P,
			hash = Hashes,
			cipher = Ciphers,
			auth = Auths,
			keyagr = KeyAgreements,
			sas = SASTypes
		}
	} = Hello,
	_From,
	#state{
		ssrc = MySSRC,
		storage = Tid
	} = State) ->
	Hash = negotiate(Tid, hash, ?ZRTP_HASH_ALL_SUPPORTED, ?ZRTP_HASH_S256, Hashes),
	Cipher = negotiate(Tid, cipher, ?ZRTP_CIPHER_ALL_SUPPORTED, ?ZRTP_CIPHER_AES1, Ciphers),
	Auth = negotiate(Tid, auth, ?ZRTP_AUTH_ALL_SUPPORTED, ?ZRTP_AUTH_TAG_HS32, Auths),
	KeyAgr = negotiate(Tid, keyagr, ?ZRTP_KEY_AGREEMENT_ALL_SUPPORTED, ?ZRTP_KEY_AGREEMENT_DH3K, KeyAgreements),
	SAS = negotiate(Tid, sas, ?ZRTP_SAS_TYPE_ALL_SUPPORTED, ?ZRTP_SAS_TYPE_B32, SASTypes),

	% Store full Bob's HELLO message
	ets:insert(Tid, {{bob, hello}, Hello}),

	{reply,
		#zrtp{sequence = SN+1, ssrc = MySSRC, message = helloack},
		State#state{
			hash = Hash,
			cipher = Cipher,
			auth = Auth,
			keyagr = KeyAgr,
			sas = SAS,
			other_zid = ZID,
			other_ssrc = SSRC,
			other_h3 = HashImageH3,
			prev_sn = SN}
	};

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = helloack
	} = HelloAck,
	_From,
	#state{
		zid = ZID,
		ssrc = MySSRC,
		h3 = H3,
		h2 = H2,
		h1 = H1,
		h0 = H0,
		hash = Hash,
		cipher = Cipher,
		auth = Auth,
		keyagr = KeyAgr,
		sas = SAS,
		other_ssrc = SSRC,
		storage = Tid
	} = State) ->

	#zrtp{message = HelloMsg} = ets:lookup_element(Tid, {bob, hello}, 2),

	HashFun = get_hashfun(Hash),
	HMacFun = get_hmacfun(Hash),

	% FIXME check for preshared keys instead of regenerating them - should we use Mnesia?
	Rs1 = ets:lookup_element(Tid, rs1, 2),
	Rs2 = ets:lookup_element(Tid, rs2, 2),
	Rs3 = ets:lookup_element(Tid, rs3, 2),
	Rs4 = ets:lookup_element(Tid, rs4, 2),

	<<Rs1IDi:8/binary, _/binary>> = HMacFun(Rs1, ?STR_INITIATOR),
	<<Rs1IDr:8/binary, _/binary>> = HMacFun(Rs1, ?STR_RESPONDER),
	<<Rs2IDi:8/binary, _/binary>> = HMacFun(Rs2, ?STR_INITIATOR),
	<<Rs2IDr:8/binary, _/binary>> = HMacFun(Rs2, ?STR_RESPONDER),
	<<AuxSecretIDi:8/binary, _/binary>> = HMacFun(Rs3, H3),
	<<AuxSecretIDr:8/binary, _/binary>> = HMacFun(Rs3, H3),
	<<PbxSecretIDi:8/binary, _/binary>> = HMacFun(Rs4, ?STR_INITIATOR),
	<<PbxSecretIDr:8/binary, _/binary>> = HMacFun(Rs4, ?STR_RESPONDER),

	{PublicKey, PrivateKey} = ets:lookup_element(Tid, {pki,KeyAgr}, 2),

	% We must generate DHPart2 here
	DHpart2Msg = mkdhpart2(H0, H1, Rs1IDi, Rs2IDi, AuxSecretIDi, PbxSecretIDi, PublicKey),
	ets:insert(Tid, {dhpart2msg, DHpart2Msg}),

	Hvi = calculate_hvi(HelloMsg, DHpart2Msg, HashFun),

	CommitMsg = #commit{
		h2 = H2,
		zid = ZID,
		hash = Hash,
		cipher = Cipher,
		auth = Auth,
		keyagr = KeyAgr,
		sas = SAS,
		hvi = Hvi
	},

	Commit = #zrtp{
		sequence = SN + 1,
		ssrc = MySSRC,
		message = CommitMsg#commit{mac = mkhmac(CommitMsg, H1)}
	},

	% Store full Alice's COMMIT message
	ets:insert(Tid, {{alice, commit}, Commit}),

	{reply, Commit,
		State#state{
			rs1IDi = Rs1IDi,
			rs1IDr = Rs1IDr,
			rs2IDi = Rs2IDi,
			rs2IDr = Rs2IDr,
			auxSecretIDi = AuxSecretIDi,
			auxSecretIDr = AuxSecretIDr,
			pbxSecretIDi = PbxSecretIDi,
			pbxSecretIDr = PbxSecretIDr,
			dhPriv = PrivateKey,
			dhPubl = PublicKey
		}
	};

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #commit{
			h2 = HashImageH2,
			zid = ZID,
			hash = Hash,
			cipher = Cipher,
			auth = Auth,
			keyagr = KeyAgr,
			sas = SAS,
			hvi = Hvi
		}
	} = Commit,
	_From,
	#state{
		ssrc = MySSRC,
		other_ssrc = SSRC,
		other_zid = ZID,
		h1 = H1,
		h0 = H0,
		hash = Hash,
		cipher = Cipher,
		auth = Auth,
		keyagr = KeyAgr,
		sas = SAS,
		rs1IDr = Rs1IDr,
		rs2IDr = Rs2IDr,
		auxSecretIDr = AuxSecretIDr,
		pbxSecretIDr = PbxSecretIDr,
		dhPubl = PublicKey,
		prev_sn = SN0,
		storage = Tid
	} = State) when SN > SN0 ->

	% Lookup Bob's HELLO packet
	Hello = ets:lookup_element(Tid, {bob, hello}, 2),

	case verify_hmac(Hello, HashImageH2) of
		true ->

			% Store full Bob's COMMIT message
			ets:insert(Tid, {{bob, commit}, Commit}),

			% Lookup Alice's COMMIT packet
			#zrtp{message = #commit{hvi = MyHvi}} = ets:lookup_element(Tid, {alice, commit}, 2),

			% Check for lowest Hvi
			case Hvi < MyHvi of
				true ->
					% We're Initiator so do nothing and wait for the DHpart1
					{reply, ok, State#state{other_h2 = HashImageH2, prev_sn = SN}};
				false ->
					DHpart1Msg = mkdhpart1(H0, H1, Rs1IDr, Rs2IDr, AuxSecretIDr, PbxSecretIDr, PublicKey),
					DHpart1 = #zrtp{sequence = SN+1, ssrc = MySSRC, message = DHpart1Msg},

					% Store full Alice's DHpart1 message
					ets:insert(Tid, {{alice, dhpart1}, DHpart1}),

					{reply, DHpart1, State#state{other_h2 = HashImageH2, prev_sn = SN}}
			end;
		false ->
			{reply, #error{code = ?ZRTP_ERROR_HELLO_MISMATCH}, State}
	end;

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #dhpart1{
			h1 = HashImageH1,
			rs1IDr = Rs1IDr,
			rs2IDr = Rs2IDr,
			auxsecretIDr = AuxsecretIDr,
			pbxsecretIDr = PbxsecretIDr,
			pvr = Pvr
		}
	} = DHpart1,
	_From,
	#state{
		zid = ZIDi,
		ssrc = MySSRC,
		other_ssrc = SSRC,
		other_zid = ZIDr,
		h1 = H1,
		h0 = H0,
		hash = Hash,
		cipher = Cipher,
		sas = SAS,
		rs1IDi = Rs1IDi,
		rs2IDi = Rs2IDi,
		auxSecretIDi = AuxSecretIDi,
		pbxSecretIDi = PbxSecretIDi,
		dhPriv = PrivateKey,
		prev_sn = SN0,
		storage = Tid
	} = State) when SN > SN0 ->

	% Lookup Bob's COMMIT packet
	Commit = ets:lookup_element(Tid, {bob, commit}, 2),

	case verify_hmac(Commit, HashImageH1) of
		true ->
			% Store full Bob's DHpart1 message
			ets:insert(Tid, {{bob, dhpart1}, DHpart1}),

			% Calculate ZRTP params
			DHpart2Msg = ets:lookup_element(Tid, dhpart2msg, 2),

			DHpart2 = #zrtp{sequence = SN+1, ssrc = MySSRC, message = DHpart2Msg},

			% Store full Alice's DHpart2 message
			ets:insert(Tid, {{alice, dhpart2}, DHpart2}),

			% Calculate DHresult
			DHresult = zrtp_crypto:mkfinal(Pvr, PrivateKey),

			% Calculate total hash - http://zfone.com/docs/ietf/rfc6189bis.html#DHSecretCalc
			HashFun = get_hashfun(Hash),
			#zrtp{message = HelloMsg} = ets:lookup_element(Tid, {bob, hello}, 2),
			#zrtp{message = CommitMsg} = ets:lookup_element(Tid, {alice, commit}, 2),

			% http://zfone.com/docs/ietf/rfc6189bis.html#SharedSecretDetermination
			TotalHash = HashFun(<< <<(encode_message(X))/binary>> || X <- [HelloMsg, CommitMsg, DHpart1#zrtp.message, DHpart2Msg] >>),
			KDF_Context = <<ZIDi/binary, ZIDr/binary, TotalHash/binary>>,
			% We have to set s1, s2, s3 to null for now - FIXME
			S0 = HashFun(<<1:32, DHresult/binary, "ZRTP-HMAC-KDF", ZIDi/binary, ZIDr/binary, TotalHash/binary, 0:32, 0:32, 0:32 >>),

			% Derive keys
			HLength = get_hashlength(Hash),
			KLength = get_keylength(Cipher),

			% SRTP keys
			<<MasterKeyI:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator SRTP master key">>, KDF_Context),
			<<MasterSaltI:14/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator SRTP master salt">>, KDF_Context),
			<<MasterKeyR:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder SRTP master key">>, KDF_Context),
			<<MasterSaltR:14/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder SRTP master salt">>, KDF_Context),

			<<HMacKeyI:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator HMAC key">>, KDF_Context),
			<<HMacKeyR:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder HMAC key">>, KDF_Context),

			<<ConfirmKeyI:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator ZRTP key">>, KDF_Context),
			<<ConfirmKeyR:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder ZRTP key">>, KDF_Context),

			<<ZRTPSessKey:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"ZRTP Session Key">>, KDF_Context),
			<<ExportedKey:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Exported key">>, KDF_Context),

			% http://zfone.com/docs/ietf/rfc6189bis.html#SASType
			<<SASValue:4/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"SAS">>, KDF_Context),
			SASString = zrtp_crypto:sas(SASValue, SAS),

			{reply, DHpart2,
				State#state{
					other_h1 = HashImageH1,
					prev_sn = SN,
					s0 = S0,
					srtp_key_i = MasterKeyI,
					srtp_salt_i = MasterSaltI,
					srtp_key_r = MasterKeyR,
					srtp_salt_r = MasterSaltR,
					hmac_key_i = HMacKeyI,
					hmac_key_r = HMacKeyR,
					confirm_key_i = ConfirmKeyI,
					confirm_key_r = ConfirmKeyR,
					sas_val = SASString
				}
			};
		false ->
			{reply, #error{code = ?ZRTP_ERROR_HELLO_MISMATCH}, State}
	end;

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #dhpart2{
			h1 = HashImageH1,
			rs1IDi = Rs1IDo,
			rs2IDi = Rs2IDi,
			auxsecretIDi = AuxsecretIDi,
			pbxsecretIDi = PbxsecretIDi,
			pvi = Pvi
		}
	} = DHpart2,
	_From,
	#state{
		zid = ZIDr,
		ssrc = MySSRC,
		dhPriv = PrivateKey,
		other_ssrc = SSRC,
		hash = Hash,
		cipher = Cipher,
		sas = SAS,
		h0 = H0,
		iv = IV,
		other_zid = ZIDi,
		prev_sn = SN0,
		storage = Tid
	} = State) when SN > SN0 ->

	% Lookup Bob's COMMIT packet
	Commit = ets:lookup_element(Tid, {bob, commit}, 2),

	case verify_hmac(Commit, HashImageH1) of
		true ->
			% Store full Bob's DHpart2 message
			ets:insert(Tid, {{bob, dhpart2}, DHpart2}),

			% Calculate DHresult
			DHresult = zrtp_crypto:mkfinal(Pvi, PrivateKey),

			% Calculate total hash - http://zfone.com/docs/ietf/rfc6189bis.html#DHSecretCalc
			HashFun = get_hashfun(Hash),
			#zrtp{message = HelloMsg} = ets:lookup_element(Tid, {alice, hello}, 2),
			#zrtp{message = CommitMsg} = ets:lookup_element(Tid, {bob, commit}, 2),
			#zrtp{message = DHpart1Msg} = ets:lookup_element(Tid, {alice, dhpart1}, 2),

			% http://zfone.com/docs/ietf/rfc6189bis.html#SharedSecretDetermination
			TotalHash = HashFun(<< <<(encode_message(X))/binary>> || X <- [HelloMsg, CommitMsg, DHpart1Msg, DHpart2#zrtp.message] >>),
			KDF_Context = <<ZIDi/binary, ZIDr/binary, TotalHash/binary>>,
			% We have to set s1, s2, s3 to null for now - FIXME
			S0 = HashFun(<<1:32, DHresult/binary, "ZRTP-HMAC-KDF", ZIDi/binary, ZIDr/binary, TotalHash/binary, 0:32, 0:32, 0:32 >>),

			% Derive keys
			HLength = get_hashlength(Hash),
			KLength = get_keylength(Cipher),

			% SRTP keys
			<<MasterKeyI:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator SRTP master key">>, KDF_Context),
			<<MasterSaltI:14/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator SRTP master salt">>, KDF_Context),
			<<MasterKeyR:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder SRTP master key">>, KDF_Context),
			<<MasterSaltR:14/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder SRTP master salt">>, KDF_Context),

			<<HMacKeyI:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator HMAC key">>, KDF_Context),
			<<HMacKeyR:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder HMAC key">>, KDF_Context),

			<<ConfirmKeyI:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Initiator ZRTP key">>, KDF_Context),
			<<ConfirmKeyR:KLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Responder ZRTP key">>, KDF_Context),

			<<ZRTPSessKey:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"ZRTP Session Key">>, KDF_Context),
			<<ExportedKey:HLength/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"Exported key">>, KDF_Context),

			% http://zfone.com/docs/ietf/rfc6189bis.html#SASType
			<<SASValue:4/binary, _/binary>> = zrtp_crypto:kdf(Hash, S0, <<"SAS">>, KDF_Context),
			SASString = zrtp_crypto:sas(SASValue, SAS),

			% FIXME add actual values as well as SAS
			HMacFun = get_hmacfun(Hash),
			EData = crypto:aes_ctr_encrypt(ConfirmKeyR, IV, <<H0/binary, 0:15, 0:9, 0:4, 0:1, 0:1, 1:1, 0:1, 16#FFFFFFFF:32>>),
			ConfMac = HMacFun(HMacKeyR, EData),

			Confirm1Msg = #confirm1{
				conf_mac = ConfMac,
				cfb_init_vect = IV,
				encrypted_data = EData
			},

			{reply, #zrtp{sequence = SN+1, ssrc = MySSRC, message = Confirm1Msg},
				State#state{
					other_h1 = HashImageH1,
					prev_sn = SN,
					s0 = S0,
					srtp_key_i = MasterKeyI,
					srtp_salt_i = MasterSaltI,
					srtp_key_r = MasterKeyR,
					srtp_salt_r = MasterSaltR,
					hmac_key_i = HMacKeyI,
					hmac_key_r = HMacKeyR,
					confirm_key_i = ConfirmKeyI,
					confirm_key_r = ConfirmKeyR,
					sas_val = SASString
				}
			};
		false ->
			{reply, #error{code = ?ZRTP_ERROR_HELLO_MISMATCH}, State}
	end;

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #confirm1{
			conf_mac = ConfMac,
			cfb_init_vect = IV,
			encrypted_data = EData
		}
	} = Confirm1,
	_From,
	#state{
		h0 = H0,
		hash = Hash,
		srtp_key_i = MasterKeyI,
		srtp_salt_i = MasterSaltI,
		srtp_key_r = MasterKeyR,
		srtp_salt_r = MasterSaltR,
		hmac_key_i = HMacKeyI,
		hmac_key_r = HMacKeyR,
		confirm_key_i = ConfirmKeyI,
		confirm_key_r = ConfirmKeyR,
		ssrc = MySSRC,
		other_ssrc = SSRC,
		other_h3 = HashImageH3,
		other_h2 = HashImageH2,
		other_h1 = HashImageH1,
		prev_sn = SN0,
		storage = Tid
	} = State) when SN > SN0 ->

	% Verify HMAC chain
	HMacFun = get_hmacfun(Hash),
	ConfMac = HMacFun(HMacKeyR, EData),
	<<HashImageH0:32/binary, _Mbz:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, Rest/binary>> = crypto:aes_ctr_decrypt(ConfirmKeyR, IV, EData),

%	Signature = case SigLen of
%		0 -> null;
%		_ ->
%			SigLenBytes = (SigLen - 1) * 4,
%			<<SigType:4/binary, SigData:SigLenBytes/binary>> = Rest,
%			#signature{type = SigType, data = SigData}
%	end,

	% Verify HMAC chain
	HashImageH1 = erlsha2:sha256(HashImageH0),
	HashImageH2 = erlsha2:sha256(HashImageH1),
	HashImageH3 = erlsha2:sha256(HashImageH2),

	% Lookup Bob's DHpart1 packet
	DHpart1 = ets:lookup_element(Tid, {bob, dhpart1}, 2),

	case verify_hmac(DHpart1, HashImageH0) of
		true ->
			% Store full Bob's CONFIRM1 message
			ets:insert(Tid, {{bob, confirm1}, Confirm1}),

			% FIXME add actual values as well as SAS
			HMacFun = get_hmacfun(Hash),
			EData2 = crypto:aes_ctr_encrypt(ConfirmKeyI, IV, <<H0/binary, 0:15, 0:9, 0:4, 0:1, 0:1, 1:1, 0:1, 16#FFFFFFFF:32>>),
			ConfMac2 = HMacFun(HMacKeyI, EData2),

			Confirm2Msg = #confirm2{
				conf_mac = ConfMac2,
				cfb_init_vect = IV,
				encrypted_data = EData2
			},

			{reply, #zrtp{sequence = SN+1, ssrc = MySSRC, message = Confirm2Msg}, State#state{other_h0 = HashImageH0, prev_sn = SN}};
		false ->
			{reply, #error{code = ?ZRTP_ERROR_HELLO_MISMATCH}, State}
	end;

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = #confirm2{
			conf_mac = ConfMac,
			cfb_init_vect = IV,
			encrypted_data = EData
		}
	} = Confirm2,
	_From,
	#state{
		parent = Parent,
		h0 = H0,
		hash = Hash,
		cipher = Cipher,
		auth = Auth,
		srtp_key_i = KeyI,
		srtp_salt_i = SaltI,
		srtp_key_r = KeyR,
		srtp_salt_r = SaltR,
		hmac_key_i = HMacKeyI,
		hmac_key_r = HMacKeyR,
		confirm_key_i = ConfirmKeyI,
		confirm_key_r = ConfirmKeyR,
		ssrc = MySSRC,
		other_ssrc = SSRC,
		prev_sn = SN0,
		storage = Tid
	} = State) when SN > SN0 ->

	% Verify HMAC chain
	HMacFun = get_hmacfun(Hash),
	ConfMac = HMacFun(HMacKeyI, EData),
	<<HashImageH0:32/binary, _Mbz:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, Rest/binary>> = crypto:aes_ctr_decrypt(ConfirmKeyI, IV, EData),

%	Signature = case SigLen of
%		0 -> null;
%		_ ->
%			SigLenBytes = (SigLen - 1) * 4,
%			<<SigType:4/binary, SigData:SigLenBytes/binary>> = Rest,
%			#signature{type = SigType, data = SigData}
%	end,

	% Verify HMAC chain
	HashImageH1 = erlsha2:sha256(HashImageH0),
	HashImageH2 = erlsha2:sha256(HashImageH1),
	HashImageH3 = erlsha2:sha256(HashImageH0),

	% Lookup Bob's DHpart2 packet
	DHpart2 = ets:lookup_element(Tid, {bob, dhpart2}, 2),

	case verify_hmac(DHpart2, HashImageH0) of
		true ->
			% We must send blocking request here
			% And we're Responder
			(Parent == null) orelse gen_server:call(Parent, {prepcrypto,
					{SSRC, Cipher, Auth, get_taglength(Auth), KeyI, SaltI},
					{MySSRC, Cipher, Auth, get_taglength(Auth), KeyR, SaltR}}
			),
			{reply, #zrtp{sequence = SN+1, ssrc = MySSRC, message = conf2ack}, State};
		false ->
			{reply, #error{code = ?ZRTP_ERROR_HELLO_MISMATCH}, State}
	end;

handle_call(
	#zrtp{
		sequence = SN,
		ssrc = SSRC,
		message = conf2ack
	} = Conf2Ack,
	_From,
	#state{
		cipher = Cipher,
		auth = Auth,
		ssrc = MySSRC,
		other_ssrc = SSRC,
		parent = Parent,
		srtp_key_i = KeyI,
		srtp_salt_i = SaltI,
		srtp_key_r = KeyR,
		srtp_salt_r = SaltR
	} = State) ->

	% We must send blocking request here
	% And we're Initiator
	(Parent == null) orelse gen_server:call(Parent, {gocrypto,
			{MySSRC, Cipher, Auth, get_taglength(Auth), KeyI, SaltI},
			{SSRC, Cipher, Auth, get_taglength(Auth), KeyR, SaltR}}
	),

	{reply, ok, State};

handle_call(get_keys, _From, State) ->
	{reply,
		{
			State#state.srtp_key_i,
			State#state.srtp_salt_i,
			State#state.srtp_key_r,
			State#state.srtp_salt_i
		},
		State};

handle_call(Other, _From, State) ->
	{reply, error, State}.

handle_cast(Other, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	ok.

handle_info({init, [Parent, ZID, SSRC, Hashes, Ciphers, Auths, KeyAgreements, SASTypes]}, State) ->
	% Intialize NIF libraries if not initialized yet
	crc32c:init(),
	sas:init(),

	Z = case ZID of
		null -> crypto:rand_bytes(96);
		_ -> ZID
	end,

	% First hash is a random set of bytes
	% Th rest are a chain of hashes made with predefined hash function
	H0 = crypto:rand_bytes(32),
	H1 = erlsha2:sha256(H0),
	H2 = erlsha2:sha256(H1),
	H3 = erlsha2:sha256(H2),

	IV = crypto:rand_bytes(16),

	Tid = ets:new(zrtp, [private]),

	ets:insert(Tid, {hash, Hashes}),
	ets:insert(Tid, {cipher, Ciphers}),
	ets:insert(Tid, {auth, Auths}),
	ets:insert(Tid, {keyagr, KeyAgreements}),
	ets:insert(Tid, {sas, SASTypes}),

	% To speedup things later we precompute all keys - we have a plenty of time for that right now
	lists:map(fun(KA) -> {PublicKey, PrivateKey} = zrtp_crypto:mkdh(KA), ets:insert(Tid, {{pki,KA}, {PublicKey, PrivateKey}}) end, KeyAgreements),

	% Likewise - prepare Rs1,Rs2,Rs3,Rs4 values now for further speedups
	lists:map(fun(Atom) -> ets:insert(Tid, {Atom, crypto:rand_bytes(32)}) end, [rs1, rs2, rs3, rs4]),

	{noreply, #state{
			parent = Parent,
			zid = Z,
			ssrc = SSRC,
			h0 = H0,
			h1 = H1,
			h2 = H2,
			h3 = H3,
			iv = IV,
			storage = Tid
		}
	};
handle_info(Other, State) ->
	{noreply, State}.

%%
%% Encoding/Decoding helpers
%%

decode(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, Rest/binary>>) ->
	L = size(Rest) - 4,
	<<BinMessage:L/binary, CRC:32>> = Rest,
	<<CRC:32>> == crc32c:crc32c(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, BinMessage/binary>>),
	{ok, Message} = decode_message(BinMessage),
	{ok, #zrtp{sequence = Sequence, ssrc = SSRC, message = Message}}.

encode(#zrtp{sequence = Sequence, ssrc = SSRC, message = Message}) ->
	BinMessage = encode_message(Message),
	CRC = crc32c:crc32c(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, BinMessage/binary>>),
	<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, BinMessage/binary, CRC/binary>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Decoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_HELLO, ?ZRTP_VERSION, ClientIdentifier:16/binary, HashImageH3:32/binary, ZID:12/binary, 0:1, S:1, M:1, P:1, _Mbz:8, HC:4, CC:4, AC:4, KC:4, SC:4, Rest/binary>>) ->
	[HCs, CCs, ACs, KCs, SCs] = [ X*4 || X <- [HC, CC, AC, KC, SC] ],
	<<HashesBin:HCs/binary, CiphersBin:CCs/binary, AuthsBin:ACs/binary, KeyAgreementsBin:KCs/binary, SASTypesBin:SCs/binary, MAC:8/binary>> = Rest,
	Hashes = [ X || <<X:4/binary>> <= HashesBin ],
	Ciphers = [ X || <<X:4/binary>> <= CiphersBin ],
	Auths = [ X || <<X:4/binary>> <= AuthsBin ],
	KeyAgreements = [ X || <<X:4/binary>> <= KeyAgreementsBin ],
	SASTypes = [ X || <<X:4/binary>> <= SASTypesBin ],
	{ok, #hello{
		clientid = ClientIdentifier,
		h3 = HashImageH3,
		zid = ZID,
		s = S,
		m = M,
		p = P,
		hash = Hashes,
		cipher = Ciphers,
		auth = Auths,
		keyagr = KeyAgreements,
		sas = SASTypes,
		mac = MAC
	}};

decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_HELLOACK>>) ->
	{ok, helloack};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 29:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, KeyAgreement:4/binary, SAS:4/binary, HVI:32/binary, MAC:8/binary>>) ->
	{ok, #commit{
			h2 = HashImageH2,
			zid = ZID,
			hash = Hash,
			cipher = Cipher,
			auth = AuthType,
			keyagr = KeyAgreement,
			sas = SAS,
			hvi = HVI,
			mac = MAC
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 25:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, ?ZRTP_KEY_AGREEMENT_MULT, SAS:4/binary, Nonce:16/binary, MAC:8/binary>>) ->
	{ok, #commit{
			h2 = HashImageH2,
			zid = ZID,
			hash = Hash,
			cipher = Cipher,
			auth = AuthType,
			keyagr = <<"Mult">>,
			sas = SAS,
			nonce = Nonce,
			mac = MAC
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 27:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, ?ZRTP_KEY_AGREEMENT_PRSH, SAS:4/binary, Nonce:16/binary, KeyID:8/binary, MAC:8/binary>>) ->
	{ok, #commit{
			h2 = HashImageH2,
			zid = ZID,
			hash = Hash,
			cipher = Cipher,
			auth = AuthType,
			keyagr = <<"Prsh">>,
			sas = SAS,
			nonce = Nonce,
			keyid = KeyID,
			mac = MAC
		}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_DHPART1, HashImageH1:32/binary, Rs1IDr:8/binary, Rs2IDr:8/binary, AuxsecretIDr:8/binary, PbxsecretIDr:8/binary, Rest/binary>>) ->
	PVRLength = (Length - (1 + 2 + 8 + 2 + 2 + 2 + 2 + 2)) * 4,
	<<PVR:PVRLength/binary, MAC:8/binary>> = Rest,
	{ok, #dhpart1{
		h1 = HashImageH1,
		rs1IDr = Rs1IDr,
		rs2IDr = Rs2IDr,
		auxsecretIDr = AuxsecretIDr,
		pbxsecretIDr = PbxsecretIDr,
		pvr = PVR,
		mac = MAC
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_DHPART2, HashImageH1:32/binary, Rs1IDi:8/binary, Rs2IDi:8/binary, AuxsecretIDi:8/binary, PbxsecretIDi:8/binary, Rest/binary>>) ->
	PVILength = (Length - (1 + 2 + 8 + 2 + 2 + 2 + 2 + 2)) * 4,
	<<PVI:PVILength/binary, MAC:8/binary>> = Rest,
	{ok, #dhpart2{
		h1 = HashImageH1,
		rs1IDi = Rs1IDi,
		rs2IDi = Rs2IDi,
		auxsecretIDi = AuxsecretIDi,
		pbxsecretIDi = PbxsecretIDi,
		pvi = PVI,
		mac = MAC
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM1, ConfMac:8/binary, CFBInitVect:16/binary, EncryptedData/binary>>) ->
	{ok, #confirm1{
			conf_mac = ConfMac,
			cfb_init_vect = CFBInitVect,
			encrypted_data = EncryptedData
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM2, ConfMac:8/binary, CFBInitVect:16/binary, EncryptedData/binary>>) ->
	{ok, #confirm2{
			conf_mac = ConfMac,
			cfb_init_vect = CFBInitVect,
			encrypted_data = EncryptedData
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_CONF2ACK>>) ->
	{ok, conf2ack};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 4:16, ?ZRTP_MSG_ERROR, ErrorCode:32>>) ->
	{ok, #error{code = ErrorCode}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_ERRORACK>>) ->
	{ok, errorack};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 4:16, ?ZRTP_MSG_GOCLEAR, MAC:8/binary>>) ->
	{ok, #goclear{mac = MAC}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_CLEARACK>>) ->
	{ok, clearack};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_SASRELAY, MAC:8/binary, CFBInitVect:16/binary, _Mbz:15, SigLen:9, 0:4, 0:1, V:1, A:1, D:1, RSRSAS:4/binary, MiTMHash:32/binary, Rest/binary>>) ->
	Signature = case SigLen of
		0 -> null;
		_ ->
			SigLenBytes = (SigLen - 1) * 4,
			<<SigType:4/binary, SigData:SigLenBytes/binary>> = Rest,
			#signature{type = SigType, data = SigData}
	end,
	{ok, #sasrelay{
			mac = MAC,
			cfb_init_vect = CFBInitVect,
			sas_verified = V,
			allow_clear = A,
			disclosure = D,
			sas_rend_scheme = RSRSAS,
			mitm_sash_hash = MiTMHash,
			signature = Signature
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_RELAYACK>>) ->
	{ok, relayack};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 6:16, ?ZRTP_MSG_PING, ?ZRTP_VERSION, EndpointHash:8/binary>>) ->
	{ok, #ping{hash = EndpointHash}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, 9:16, ?ZRTP_MSG_PINGACK, ?ZRTP_VERSION, SenderEndpointHash:8/binary, ReceivedEndpointHash:8/binary, SSRC:4/binary>>) ->
	{ok, #pingack{sender_hash = SenderEndpointHash, receiver_hash = ReceivedEndpointHash, ssrc = SSRC}};
decode_message(_) ->
	{error, unknown_msg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Encoding helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_message(#hello{clientid = ClientIdentifier, h3 = HashImageH3, zid = ZID, s = S, m = M, p = P, hash = Hashes, cipher = Ciphers, auth = Auths, keyagr = KeyAgreements, sas = SASTypes, mac = MAC}) ->
	HC = length(Hashes),
	CC = length(Ciphers),
	AC = length(Auths),
	KC = length(KeyAgreements),
	SC = length(SASTypes),
	BinHashes = << <<(to_binary(X))/binary>> || X <- Hashes >>,
	BinCiphers = << <<(to_binary(X))/binary>> || X <- Ciphers >>,
	BinAuths = << <<(to_binary(X))/binary>> || X <- Auths >>,
	BinKeyAgreements = << <<(to_binary(X))/binary>> || X <- KeyAgreements >>,
	BinSASTypes = << <<(to_binary(X))/binary>> || X <- SASTypes >>,
	Rest = <<BinHashes/binary, BinCiphers/binary, BinAuths/binary, BinKeyAgreements/binary, BinSASTypes/binary, MAC/binary>>,
	Length = (2 + 2 + 8 + 4 + 16 + 32 + 12 + 4 + size(Rest)) div 4,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_HELLO, ?ZRTP_VERSION, ClientIdentifier:16/binary, HashImageH3:32/binary, ZID:12/binary, 0:1, S:1, M:1, P:1, 0:8, HC:4, CC:4, AC:4, KC:4, SC:4, Rest/binary>>;
encode_message(helloack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_HELLOACK>>;
encode_message(#commit{h2 = HashImageH2,zid = ZID,hash = Hash,cipher = Cipher,auth = AuthType,keyagr = <<"Mult">>,sas = SAS,nonce = Nonce,mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 25:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, (to_binary(Hash)):4/binary, (to_binary(Cipher)):4/binary, (to_binary(AuthType)):4/binary, ?ZRTP_KEY_AGREEMENT_MULT, (to_binary(SAS)):4/binary, Nonce:16/binary, MAC:8/binary>>;
encode_message(#commit{h2 = HashImageH2, zid = ZID, hash = Hash, cipher = Cipher, auth = AuthType, keyagr = <<"Prsh">>, sas = SAS, nonce = Nonce, keyid = KeyID, mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 27:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, (to_binary(Hash)):4/binary, (to_binary(Cipher)):4/binary, (to_binary(AuthType)):4/binary, ?ZRTP_KEY_AGREEMENT_PRSH, (to_binary(SAS)):4/binary, Nonce:16/binary, KeyID:8/binary, MAC:8/binary>>;
encode_message(#commit{h2 = HashImageH2,zid = ZID,hash = Hash,cipher = Cipher,auth = AuthType,keyagr = KeyAgreement,sas = SAS,hvi = HVI,mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 29:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, (to_binary(Hash)):4/binary, (to_binary(Cipher)):4/binary, (to_binary(AuthType)):4/binary, (to_binary(KeyAgreement)):4/binary, (to_binary(SAS)):4/binary, HVI:32/binary, MAC:8/binary>>;
encode_message(#dhpart1{h1 = HashImageH1,rs1IDr = Rs1IDr,rs2IDr = Rs2IDr,auxsecretIDr = AuxsecretIDr,pbxsecretIDr = PbxsecretIDr,pvr = PVR,mac = MAC}) ->
	Length = 1 + 2 + 8 + 2 + 2 + 2 + 2 + size(PVR) div 4 + 2,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_DHPART1, HashImageH1:32/binary, Rs1IDr:8/binary, Rs2IDr:8/binary, AuxsecretIDr:8/binary, PbxsecretIDr:8/binary, PVR/binary, MAC/binary>>;
encode_message(#dhpart2{h1 = HashImageH1,rs1IDi = Rs1IDi,rs2IDi = Rs2IDi,auxsecretIDi = AuxsecretIDi,pbxsecretIDi = PbxsecretIDi,pvi = PVI,mac = MAC}) ->
	Length = 1 + 2 + 8 + 2 + 2 + 2 + 2 + size(PVI) div 4 + 2,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_DHPART2, HashImageH1:32/binary, Rs1IDi:8/binary, Rs2IDi:8/binary, AuxsecretIDi:8/binary, PbxsecretIDi:8/binary, PVI/binary, MAC/binary>>;
encode_message(#confirm1{conf_mac = ConfMac,cfb_init_vect = CFBInitVect,h0 = HashPreimageH0,pbx_enrollement = E,sas_verified = V,allow_clear = A,disclosure = D,cache_exp_interval = CacheExpInterval,signature = Signature, encrypted_data = null}) ->
	SignatureBin = case Signature of
		null -> <<>>;
		#signature{type = SigType, data = SigData} ->
			<<SigType/binary, SigData/binary>>
	end,
	SigLen = size(SignatureBin) div 4,
	Length = 19 + SigLen,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM1, ConfMac:8/binary, CFBInitVect:16/binary, HashPreimageH0:32/binary, 0:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, SignatureBin/binary>>;
encode_message(#confirm1{conf_mac = ConfMac,cfb_init_vect = CFBInitVect,h0 = null,pbx_enrollement = null,sas_verified = null,allow_clear = null,disclosure = null,cache_exp_interval = null,signature = null, encrypted_data = EncryptedData}) ->
	Length = (36 + size(EncryptedData)) div 4,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM1, ConfMac:8/binary, CFBInitVect:16/binary, EncryptedData/binary>>;
encode_message(#confirm2{conf_mac = ConfMac,cfb_init_vect = CFBInitVect,h0 = HashPreimageH0,pbx_enrollement = E,sas_verified = V,allow_clear = A,disclosure = D,cache_exp_interval = CacheExpInterval,signature = Signature, encrypted_data = null}) ->
	SignatureBin = case Signature of
		null -> <<>>;
		#signature{type = SigType, data = SigData} ->
			<<SigType/binary, SigData/binary>>
	end,
	SigLen = size(SignatureBin) div 4,
	Length = 19 + SigLen,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM2, ConfMac:8/binary, CFBInitVect:16/binary, HashPreimageH0:32/binary, 0:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, SignatureBin/binary>>;
encode_message(#confirm2{conf_mac = ConfMac,cfb_init_vect = CFBInitVect,h0 = null,pbx_enrollement = null,sas_verified = null,allow_clear = null,disclosure = null,cache_exp_interval = null,signature = null, encrypted_data = EncryptedData}) ->
	Length = (36 + size(EncryptedData)) div 4,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM2, ConfMac:8/binary, CFBInitVect:16/binary, EncryptedData/binary>>;
encode_message(conf2ack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_CONF2ACK>>;
encode_message(#error{code = ErrorCode}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 4:16, ?ZRTP_MSG_ERROR, ErrorCode:32>>;
encode_message(errorack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_ERRORACK>>;
encode_message(#goclear{mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 4:16, ?ZRTP_MSG_GOCLEAR, MAC:8/binary>>;
encode_message(clearack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_CLEARACK>>;
encode_message(#sasrelay{mac = MAC,cfb_init_vect = CFBInitVect,sas_verified = V,allow_clear = A,disclosure = D,sas_rend_scheme = RSRSAS,mitm_sash_hash = MiTMHash,signature = Signature}) ->
	SignatureBin = case Signature of
		null -> <<>>;
		#signature{type = SigType, data = SigData} ->
			<<SigType/binary, SigData/binary>>
	end,
	SigLen = size(SignatureBin) div 4,
	Length = 19 + SigLen,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_SASRELAY, MAC:8/binary, CFBInitVect:16/binary, 0:15, SigLen:9, 0:4, 0:1, V:1, A:1, D:1, RSRSAS:4/binary, MiTMHash:32/binary, SignatureBin/binary>>;
encode_message(relayack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_RELAYACK>>;
encode_message(#ping{hash = EndpointHash}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 6:16, ?ZRTP_MSG_PING, ?ZRTP_VERSION, EndpointHash:8/binary>>;
encode_message(#pingack{sender_hash = SenderEndpointHash, receiver_hash = ReceivedEndpointHash, ssrc = SSRC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 9:16, ?ZRTP_MSG_PINGACK, ?ZRTP_VERSION, SenderEndpointHash:8/binary, ReceivedEndpointHash:8/binary, SSRC:4/binary>>;
encode_message(_) ->
	{error, unknown_msg}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Various helpers
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate_hvi(#hello{} = Hello, #dhpart2{} = DHPart2, HashFun) ->
	HelloBin = encode_message(Hello),
	DHPart2Bin = encode_message(DHPart2),
	HashFun(<<DHPart2Bin/binary, HelloBin/binary>>).

verify_hmac(#zrtp{message = #hello{zid = ZID, mac = Mac} = Msg} = Packet, H2) ->
	verify_hmac(Msg, Mac, H2);
verify_hmac(#zrtp{message = #commit{mac = Mac} = Msg} = Packet, H1) ->
	verify_hmac(Msg, Mac, H1);
verify_hmac(#zrtp{message = #dhpart1{mac = Mac} = Msg} = Packet, H0) ->
	verify_hmac(Msg, Mac, H0);
verify_hmac(#zrtp{message = #dhpart2{mac = Mac} = Msg} = Packet, H0) ->
	verify_hmac(Msg, Mac, H0);
verify_hmac(_, _) ->
	false.

mkhmac(Msg, Hash) ->
	Payload = encode_message(Msg),
	Size = size(Payload) - 8,
	<<Data:Size/binary, _/binary>> = Payload,
	<<Mac:8/binary, _/binary>> = hmac:hmac256(Hash, Data),
	Mac.

verify_hmac(_, _, null) ->
	false;
verify_hmac(Msg, Mac, Hash) ->
	Mac == mkhmac(Msg, Hash).

mkdhpart1(H0, H1, Rs1IDr, Rs2IDr, AuxSecretIDr, PbxSecretIDr, PublicKey) ->
	<<_:32, Pvr/binary>> = PublicKey,
	DHpart1 = #dhpart1{
		h1 = H1,
		rs1IDr = Rs1IDr,
		rs2IDr = Rs2IDr,
		auxsecretIDr = AuxSecretIDr,
		pbxsecretIDr = PbxSecretIDr,
		pvr = Pvr
	},
	Mac = mkhmac(DHpart1, H0),
	DHpart1#dhpart1{mac = Mac}.
mkdhpart2(H0, H1, Rs1IDi, Rs2IDi, AuxSecretIDi, PbxSecretIDi, PublicKey) ->
	<<_:32, Pvi/binary>> = PublicKey,
	DHpart2 = #dhpart2{
		h1 = H1,
		rs1IDi = Rs1IDi,
		rs2IDi = Rs2IDi,
		auxsecretIDi = AuxSecretIDi,
		pbxsecretIDi = PbxSecretIDi,
		pvi = Pvi
	},
	Mac = mkhmac(DHpart2, H0),
	DHpart2#dhpart2{mac = Mac}.

negotiate(Tid, RecId, Supported, Default, BobList) ->
	AliceList = ets:lookup_element(Tid, RecId, 2),
	negotiate(Supported, Default, AliceList, BobList).

negotiate(_, Default, [], _) ->
	Default;
negotiate(_, Default, _, []) ->
	Default;
negotiate(Supported, _, AliceList, BobList) ->
	IntersectList = lists:filter(fun(X) -> lists:member(X, AliceList) end, BobList),
	choose(Supported, IntersectList).

choose([], IntersectList) ->
	throw({error,cant_negotiate});
choose([Item | Rest], IntersectList) ->
	case lists:member(Item, IntersectList) of
		true -> Item;
		_ -> choose(Rest, IntersectList)
	end.

get_hashfun(?ZRTP_HASH_S256) -> fun erlsha2:sha256/1;
get_hashfun(?ZRTP_HASH_S384) -> fun erlsha2:sha384/1.
get_hmacfun(?ZRTP_HASH_S256) -> fun hmac:hmac256/2;
get_hmacfun(?ZRTP_HASH_S384) -> fun hmac:hmac384/2.
get_hashlength(?ZRTP_HASH_S256) -> 32;
get_hashlength(?ZRTP_HASH_S384) -> 48.
get_keylength(?ZRTP_CIPHER_AES1) -> 16;
get_keylength(?ZRTP_CIPHER_AES2) -> 24;
get_keylength(?ZRTP_CIPHER_AES3) -> 32.
get_taglength(?ZRTP_AUTH_TAG_HS32) -> 4;
get_taglength(?ZRTP_AUTH_TAG_HS80) -> 10;
get_taglength(?ZRTP_AUTH_TAG_SK32) -> 4;
get_taglength(?ZRTP_AUTH_TAG_SK64) -> 8.

to_binary(B) when is_binary(B) -> B;
to_binary(B) when is_list(B) -> list_to_binary(B).
