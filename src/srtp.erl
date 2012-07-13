%%%----------------------------------------------------------------------
%%% Copyright (c) 2012 Peter Lemenkov <lemenkov@gmail.com>
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

-module(srtp).
-author('lemenkov@gmail.com').

-export([new_ctx/6]).
-export([encrypt/2]).
-export([decrypt/2]).
-compile(export_all).

-include("../include/rtcp.hrl").
-include("../include/rtp.hrl").
-include("../include/srtp.hrl").

new_ctx(SSRC, Ealg, Aalg, MasterKey, MasterSalt, TagLength) ->
	<<K_S:128>> = srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_SALT, 0, 0),
	#srtp_crypto_ctx{
		ssrc = SSRC,
		ealg = Ealg,
		aalg = Aalg,
		masterKey = MasterKey,
		masterSalt = MasterSalt,
		k_a = srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_AUTH, 0, 0),
		k_e = srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_ENCR, 0, 0),
		k_s = K_S,
		tagLength = TagLength
	}.

encrypt(#rtp{} = Rtp, passthru) ->
	{ok, rtp:encode(Rtp), passthru};
encrypt(#rtcp{encrypted = Data} = Rctp, passthru) ->
	{ok, Data, passthru};

encrypt(
	#rtp{sequence_number = SequenceNumber, ssrc = SSRC, payload = Payload} = Rtp,
	#srtp_crypto_ctx{ssrc = SSRC, s_l = OldSequenceNumber, roc = Roc, aalg = Aalg, ealg = Ealg, k_a = KeyA, k_e = KeyE, k_s = Salt, tagLength = TagLength} = Ctx
) ->
	EncryptedPayload = encrypt_payload(Payload, SSRC, guess_index(SequenceNumber, OldSequenceNumber, Roc), Ealg, KeyE, Salt),
	{ok, append_auth(rtp:encode(Rtp#rtp{payload = EncryptedPayload}), <<Roc:32>>, Aalg, KeyA, TagLength), update_ctx(Ctx, SequenceNumber, OldSequenceNumber, Roc)};
encrypt(
	#rtcp{} = Rtcp,
	#srtp_crypto_ctx{ssrc = SSRC, rtcp_idx = Idx, aalg = Aalg, ealg = Ealg, k_a = KeyA, k_e = KeyE, k_s = Salt, tagLength = TagLength} = Ctx
) ->
	<<Header:8/binary, Payload/binary>> = rtcp:encode(Rtcp),
	EncryptedPayload = encrypt_payload(Payload, SSRC, 0, Ealg, KeyE, Salt),
	{ok, append_auth(<<Header:8/binary, EncryptedPayload/binary, 1:1, Idx:31>>, <<>>, Aalg, KeyA, TagLength), Ctx}.

decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, passthru) when PayloadType =< 34; 96 =< PayloadType ->
	{ok, Rtp} = rtp:decode(Data),
	{ok, Rtp, passthru};
decrypt(<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data, passthru) when 64 =< PayloadType, PayloadType =< 82 ->
	{ok, #rtcp{encrypted = Data}, passthru};

decrypt(
	<<?RTP_VERSION:2, _:7, PayloadType:7, SequenceNumber:16, _:32, SSRC:32, Rest/binary>> = Data,
	#srtp_crypto_ctx{ssrc = SSRC, s_l = OldSequenceNumber, roc = Roc, aalg = Aalg, ealg = Ealg, k_a = KeyA, k_e = KeyE, k_s = Salt, tagLength = TagLength} = Ctx
) when PayloadType =< 34; 96 =< PayloadType ->
	<<Header:12/binary, EncryptedPayload/binary>> = check_auth(Data, <<Roc:32>>, Aalg, KeyA, TagLength),
	DecryptedPayload = decrypt_payload(EncryptedPayload, SSRC, guess_index(SequenceNumber, OldSequenceNumber, Roc), Ealg, KeyE, Salt),
	{ok, Rtp} = rtp:decode(<<Header:12/binary, DecryptedPayload/binary>>),
	{ok, Rtp, update_ctx(Ctx, SequenceNumber, OldSequenceNumber, Roc)};
decrypt(
	<<?RTP_VERSION:2, _:7, PayloadType:7, Rest/binary>> = Data,
	#srtp_crypto_ctx{ssrc = SSRC, aalg = Aalg, ealg = Ealg, k_a = KeyA, k_e = KeyE, k_s = Salt, tagLength = TagLength} = Ctx
) when 64 =< PayloadType, PayloadType =< 82 ->
	Size = size(Data) - (TagLength + 8 + 4),
	<<Header:8/binary, EncryptedPayload:Size/binary, E:1, Index:31>> = check_auth(Data, <<>>, Aalg, KeyA, TagLength),
	DecryptedPayload = decrypt_payload(EncryptedPayload, SSRC, 0, Ealg, KeyE, Salt),
	{ok, Rtcp} = rtp:decode(<<Header:8/binary, DecryptedPayload/binary>>),
	{ok, Rtcp, Ctx}.

%%
%% Auth
%%

check_auth(Data, _, srtpAuthenticationNull, _, _) ->
	Data;
check_auth(Data, Roc, srtpAuthenticationSha1Hmac, Key, TagLength) ->
	Size = size(Data) - TagLength,
	<<NewData:Size/binary, Tag:TagLength/binary>> = Data,
	<<Tag:TagLength/binary, _/binary>> = crypto:sha_mac(Key, <<NewData/binary, Roc/binary>>),
	NewData;
check_auth(Data, Roc, srtpAuthenticationSkeinHmac, Key, TagLength) ->
	throw({error, skein_unsupported}).

append_auth(Data, _, srtpAuthenticationNull, _, _) ->
	Data;
append_auth(Data, Roc, srtpAuthenticationSha1Hmac, Key, TagLength) ->
	<<Tag:TagLength/binary, _/binary>> = crypto:sha_mac(Key, <<Data/binary, Roc/binary>>),
	<<Data/binary, Tag/binary>>;
append_auth(Data, Roc, srtpAuthenticationSkeinHmac, Key, TagLength) ->
	throw({error, skein_unsupported}).

encrypt_payload(Data, _, _, srtpEncryptionNull, _, _) ->
	Data;
encrypt_payload(Data, SSRC, Index, srtpEncryptionAESCM, Key, Salt) ->
	crypto:aes_ctr_encrypt(Key, <<((Salt bxor (SSRC bsl 48)) bxor Index):112, 0:16>>, Data);
encrypt_payload(Data, SSRC, Index, srtpEncryptionAESF8, Key, Salt) ->
	throw({error, aesf8_encryption_unsupported});
encrypt_payload(Data, SSRC, Index, srtpEncryptionTWOF8, Key, Salt) ->
	throw({error, twof8_encryption_unsupported}).

decrypt_payload(Data, _, _, srtpEncryptionNull, _, _) ->
	Data;
decrypt_payload(Data, SSRC, Index, srtpEncryptionAESCM, Key, Salt) ->
	crypto:aes_ctr_decrypt(Key, <<((Salt bxor (SSRC bsl 48)) bxor Index):112, 0:16>>, Data);
decrypt_payload(Data, SSRC, Index, srtpEncryptionAESF8, Key, Salt) ->
	throw({error, aesf8_decryption_unsupported});
decrypt_payload(Data, SSRC, Index, srtpEncryptionTWOF8, Key, Salt) ->
	throw({error, twof8_decryption_unsupported}).

%%
%% Crypto-specific functions
%%

computeIV(<<MSH:112, _/binary>> = MasterSalt, Label, Index, 0) ->
	<<(MSH bxor (Label bsl 48)):112, 0:16>>;
computeIV(<<MSH:112, _/binary>> = MasterSalt, Label, Index, KeyDerivationRate) ->
	<<(MSH bxor ((Label bsl 48) bor (Index div KeyDerivationRate))):112, 0:16>>.

derive_key(MasterKey, MasterSalt, Label, Index, KeyDerivationRate) ->
	crypto:aes_ctr_encrypt(MasterKey, computeIV(MasterSalt, Label, Index, KeyDerivationRate), <<0:128>>).

get_ctr_cipher_stream(MasterKey, IV, Step) ->
	get_ctr_cipher_stream(MasterKey, IV, Step, 0).
get_ctr_cipher_stream(MasterKey, <<IV:14/binary, _/binary>>, Step, Step) ->
	crypto:aes_ctr_encrypt(MasterKey, <<IV/binary, Step:16>>, <<0:128>>);
get_ctr_cipher_stream(MasterKey, <<IV:14/binary, _/binary>>, Step, CurrentStep) ->
	get_ctr_cipher_stream(MasterKey, crypto:aes_ctr_encrypt(MasterKey, <<IV/binary, CurrentStep:16>>, <<0:128>>), Step, CurrentStep + 1).

guess_index(SequenceNumber, null, Roc) ->
	guess_index(SequenceNumber, SequenceNumber, Roc);
guess_index(SequenceNumber, OldSequenceNumber, Roc) when OldSequenceNumber < 32768 ->
	GuessedRoc = case (SequenceNumber - OldSequenceNumber) > 32768 of
		true -> Roc - 1;
		false -> Roc
	end,
	GuessedRoc bsl 16 + SequenceNumber;
guess_index(SequenceNumber, OldSequenceNumber, Roc) ->
	GuessedRoc = case (OldSequenceNumber - 32768) > SequenceNumber of
		true -> Roc + 1;
		false -> Roc
	end,
	GuessedRoc bsl 16 + SequenceNumber.

update_ctx(Ctx, SequenceNumber, OldSequenceNumber, Roc) when OldSequenceNumber < 32768 ->
	NewSequenceNumber = erlang:max(SequenceNumber, OldSequenceNumber),
	GuessedRoc = case (SequenceNumber - OldSequenceNumber) > 32768 of
		true -> Roc - 1;
		false -> Roc
	end,
	case GuessedRoc > Roc of
		true -> Ctx#srtp_crypto_ctx{s_l = SequenceNumber, roc = GuessedRoc};
		false -> Ctx#srtp_crypto_ctx{s_l = NewSequenceNumber, roc = Roc}
	end;
update_ctx(Ctx, SequenceNumber, OldSequenceNumber, Roc) ->
	NewSequenceNumber = erlang:max(SequenceNumber, OldSequenceNumber),
	GuessedRoc = case (OldSequenceNumber - 32768) > SequenceNumber of
		true -> Roc + 1;
		false -> Roc
	end,
	case GuessedRoc > Roc of
		true -> Ctx#srtp_crypto_ctx{s_l = SequenceNumber, roc = GuessedRoc};
		false -> Ctx#srtp_crypto_ctx{s_l = NewSequenceNumber, roc = Roc}
	end.
