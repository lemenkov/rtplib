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

% FIXME - don't forget to remove from final version!
-compile(export_all).

-include("../include/zrtp.hrl").

decode(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, Rest/binary>>) ->
	L = size(Rest) - 4,
	<<BinMessage:L/binary, CRC:32>> = Rest,
	true = check_crc32c(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, BinMessage/binary>>, CRC),
	{ok, Message} = decode_message(BinMessage),
	{ok, #zrtp{sequence = Sequence, ssrc = SSRC, message = Message}}.

encode(#zrtp{sequence = Sequence, ssrc = SSRC, message = Message}) ->
	BinMessage = encode_message(Message),
	CRC = make_crc32c(<<?ZRTP_MARKER:16, Sequence:16, ?ZRTP_MAGIC_COOKIE:32, SSRC:32, BinMessage/binary>>),
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
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM1, ConfMac:8/binary, CFBInitVect:16/binary, HashPreimageH0:32/binary, _Mbz:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, Rest/binary>>) ->
	Signature = case SigLen of
		0 -> null;
		_ ->
			SigLenBytes = (SigLen - 1) * 4,
			<<SigType:4/binary, SigData:SigLenBytes/binary>> = Rest,
			#signature{type = SigType, data = SigData}
	end,
	{ok, #confirm1{
			conf_mac = ConfMac,
			cfb_init_vect = CFBInitVect,
			h0 = HashPreimageH0,
			pbx_enrollement = E,
			sas_verified = V,
			allow_clear = A,
			disclosure = D,
			cache_exp_interval = CacheExpInterval,
			signature = Signature
	}};
% We need this for the case when we can't decrypt the payload (full proxy mode)
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM1, ConfMac:8/binary, CFBInitVect:16/binary, EncryptedData/binary>>) ->
	{ok, #confirm1{
			conf_mac = ConfMac,
			cfb_init_vect = CFBInitVect,
			encrypted_data = EncryptedData
	}};
decode_message(<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_CONFIRM2, ConfMac:8/binary, CFBInitVect:16/binary, HashPreimageH0:32/binary, _Mbz:15, SigLen:9, 0:4, E:1, V:1, A:1, D:1, CacheExpInterval:4/binary, Rest/binary>>) ->
	Signature = case SigLen of
		0 -> null;
		_ ->
			SigLenBytes = (SigLen - 1) * 4,
			<<SigType:4/binary, SigData:SigLenBytes/binary>> = Rest,
			#signature{type = SigType, data = SigData}
	end,
	{ok, #confirm2{
			conf_mac = ConfMac,
			cfb_init_vect = CFBInitVect,
			h0 = HashPreimageH0,
			pbx_enrollement = E,
			sas_verified = V,
			allow_clear = A,
			disclosure = D,
			cache_exp_interval = CacheExpInterval,
			signature = Signature
	}};
% We need this for the case when we can't decrypt the payload (full proxy mode)
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
	BinHashes = << <<X/binary>> || <<X/binary>> <- Hashes >>,
	BinCiphers = << <<X/binary>> || <<X/binary>> <- Ciphers >>,
	BinAuths = << <<X/binary>> || <<X/binary>> <- Auths >>,
	BinKeyAgreements = << <<X/binary>> || <<X/binary>> <- KeyAgreements >>,
	BinSASTypes = << <<X/binary>> || <<X/binary>> <- SASTypes >>,
	Rest = <<BinHashes/binary, BinCiphers/binary, BinAuths/binary, BinKeyAgreements/binary, BinSASTypes/binary, MAC/binary>>,
	Length = (2 + 2 + 8 + 4 + 16 + 32 + 12 + 4 + size(Rest)) div 4,
	<<?ZRTP_SIGNATURE_HELLO:16, Length:16, ?ZRTP_MSG_HELLO, ?ZRTP_VERSION, ClientIdentifier:16/binary, HashImageH3:32/binary, ZID:12/binary, 0:1, S:1, M:1, P:1, 0:8, HC:4, CC:4, AC:4, KC:4, SC:4, Rest/binary>>;
encode_message(helloack) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 3:16, ?ZRTP_MSG_HELLOACK>>;
encode_message(#commit{h2 = HashImageH2,zid = ZID,hash = Hash,cipher = Cipher,auth = AuthType,keyagr = <<"Mult">>,sas = SAS,nonce = Nonce,mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 25:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, ?ZRTP_KEY_AGREEMENT_MULT, SAS:4/binary, Nonce:16/binary, MAC:8/binary>>;
encode_message(#commit{h2 = HashImageH2, zid = ZID, hash = Hash, cipher = Cipher, auth = AuthType, keyagr = <<"Prsh">>, sas = SAS, nonce = Nonce, keyid = KeyID, mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 27:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, ?ZRTP_KEY_AGREEMENT_PRSH, SAS:4/binary, Nonce:16/binary, KeyID:8/binary, MAC:8/binary>>;
encode_message(#commit{h2 = HashImageH2,zid = ZID,hash = Hash,cipher = Cipher,auth = AuthType,keyagr = KeyAgreement,sas = SAS,hvi = HVI,mac = MAC}) ->
	<<?ZRTP_SIGNATURE_HELLO:16, 29:16, ?ZRTP_MSG_COMMIT, HashImageH2:32/binary, ZID:12/binary, Hash:4/binary, Cipher:4/binary, AuthType:4/binary, KeyAgreement:4/binary, SAS:4/binary, HVI:32/binary, MAC:8/binary>>;
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

check_crc32c(Message, CRC) ->
	<<NewCRC:32>> = make_crc32c(Message),
	NewCRC == CRC.

make_crc32c(Message) ->
	% FIXME use NIF here instead of that
	load_library(crc32c_drv),
	Port = open_port({spawn, crc32c_drv}, [binary]),
	CRC = port_control(Port, 0, Message),
	port_close(Port),
	CRC.

% FIXME remove this
load_library(Name) ->
	case erl_ddll:load_driver(code:lib_dir(rtplib) ++ "/priv/", Name) of
		ok -> ok;
		{error, already_loaded} -> ok;
		{error, permanent} -> ok;
		{error, Error} ->
			error_logger:error_msg("Can't load ~p library: ~s~n", [Name, erl_ddll:format_error(Error)]),
			{error, Error}
	end.
