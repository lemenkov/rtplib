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

-module(srtp_test).

% All data within this test is taken from Appendix A. RFC 3711
-include("srtp.hrl").
-include("rtp.hrl").
-include_lib("eunit/include/eunit.hrl").

srtp_aesf8_test_() ->
	Header = <<128,110,92,186,80,104,29,229,92,98,21,153>>, % 16#806e5cba50681de55c621599
	Payload = <<"pseudorandomness is the next best thing">>, % 70736575646f72616e646f6d6e657373 20697320746865206e65787420626573 74207468696e67
	EncryptedPayload = <<16#019ce7a26e7854014a6366aa95d4eefd:128, 16#1ad4172a14f9faf455b7f1d4b62bd08f:128, 16#562c0eef7c4802:56>>,
	Rtp = #rtp{
		padding = 0,
		marker = 0,
		payload_type = 110,
		sequence_number = 23738,
		timestamp = 1349000677,
		ssrc = 1549931929,
		csrcs = [],
		extension = null,
		payload = Payload
	},
	Roc = 16#d462564a,
	Key = 16#234829008467be186c3de14aae72d62c,
	Salt = 16#32f2870d,
%	{ok, FdA} = gen_udp:open(0, [{active, false}, binary]),
%	{ok, FdB} = gen_udp:open(0, [{active, false}, binary]),
%	{setup,
%		fun() -> true end,
%		fun (_) -> gen_udp:close(FdA),  gen_udp:close(FdB) end,
%		[]
%	}.
	[].

srtp_aescm_test_() ->
	MasterKey = <<16#E1F97A0D3E018BE0D64FA32C06DE4139:128>>,
	MasterSalt = <<16#0EC675AD498AFEEBB6960B3AABE6:112>>,
	Header = <<128,110,92,186,80,104,29,229,92,98,21,153>>, % 16#806e5cba50681de55c621599
	Payload = <<"pseudorandomness is the next best thing">>, % 70736575646f72616e646f6d6e657373 20697320746865206e65787420626573 74207468696e67
	EncryptedPayload = <<199,114,106,144,88,86,211,227,9,159,237,135,145,51,120,85,148,197,254,229,134,149,179,2,69,112,79,144,239,69,167,137,21,150,11,149,7,141,52>>,
	SSRC = 1549931929,
	Rtp = #rtp{
		padding = 0,
		marker = 0,
		payload_type = 110,
		sequence_number = 23738,
		timestamp = 1349000677,
		ssrc = SSRC,
		csrcs = [],
		extension = null,
		payload = Payload
	},
	EncRtp = #rtp{
		padding = 0,
		marker = 0,
		payload_type = 110,
		sequence_number = 23738,
		timestamp = 1349000677,
		ssrc = SSRC,
		csrcs = [],
		extension = null,
		payload = EncryptedPayload
	},
	EncRtpBin = rtp:encode(EncRtp),
	Ctx = srtp:new_ctx(SSRC, srtpEncryptionAESCM, srtpAuthenticationNull, MasterKey, MasterSalt, 0),
	[
		{"Test correct AES-CM encryption",
			fun() -> ?assertMatch({ok, EncRtpBin, _}, srtp:encrypt(Rtp, Ctx)) end
		},
		{"Test correct AES-CM decryption",
			fun() -> ?assertMatch({ok, Rtp, _}, srtp:decrypt(EncRtpBin, Ctx)) end
		}
	].

srtp_computeIV_test_() ->
	MasterSalt = <<16#0EC675AD498AFEEBB6960B3AABE6:112>>,
	[

		{"Test IV generation (label #0 - RTP session encryption key)",
			fun() -> ?assertEqual(<<16#0EC675AD498AFEEBB6960B3AABE6:112>>,
						srtp:computeIV(MasterSalt, ?SRTP_LABEL_RTP_ENCR, 0, 0)) end
		},
		{"Test IV generation (label #1 - RTP session authentication key)",
			fun() -> ?assertEqual(<<16#0EC675AD498AFEEAB6960B3AABE6:112>>,
						srtp:computeIV(MasterSalt, ?SRTP_LABEL_RTP_AUTH, 0, 0)) end
		},
		{"Test IV generation (label #2 - RTP session salt)",
			fun() -> ?assertEqual(<<16#0EC675AD498AFEE9B6960B3AABE6:112>>,
						srtp:computeIV(MasterSalt, ?SRTP_LABEL_RTP_SALT, 0, 0)) end
		}

	].

%% See RFC 3711 B.2
srtp_aes_cm_get_ctr_cipher_stream_test_() ->
	SessionKey = <<16#2B7E151628AED2A6ABF7158809CF4F3C:128>>,
	SessionSalt = <<16#F0F1F2F3F4F5F6F7F8F9FAFBFCFD:112>>,
	Index = 0, % Sequence Number
	SSRC = 0,
	Label = ?SRTP_LABEL_RTP_ENCR,
	KeyDerivationRate = 0,

	[
		{"Test AES-CM keystream generation",
			fun() -> ?assertEqual(
						[
							<<16#E03EAD0935C95E80E166B16DD92B4EB4:128>>,
							<<16#D23513162B02D0F72A43A2FE4A5F97AB:128>>,
							<<16#41E95B3BB0A2E8DD477901E4FCA894C0:128>>,
							<<16#EC8CDF7398607CB0F2D21675EA9EA1E4:128>>,
							<<16#362B7C3C6773516318A077D7FC5073AE:128>>,
							<<16#6A2CC3787889374FBEB4C81B17BA6C44:128>>
						],
						lists:map(
							fun(Step) -> srtp:get_ctr_cipher_stream(SessionKey, SessionSalt, Label, Index, KeyDerivationRate, Step) end,
							[0, 1, 2, 16#FEFF, 16#FF00, 16#FF01]
						)
					) end
		}
	].




%% See RFC 3711 B.3
srtp_derive_key_test_() ->
	MasterKey = <<16#E1F97A0D3E018BE0D64FA32C06DE4139:128>>,
	MasterSalt = <<16#0EC675AD498AFEEBB6960B3AABE6:112>>,

	Cipher0 = <<16#C61E7A93744F39EE10734AFE3FF7A087:128>>,
	Cipher1 = <<16#CEBE321F6FF7716B6FD4AB49AF256A15:128>>,
	Cipher2 = <<16#30CBBC08863D8C85D49DB34A9AE17AC6:128>>,
	%<<CipherSalt:112, _/binary>> = Cipher2,
	[
		{"Test RTP session encryption key generation (Label #0)",
			fun() -> ?assertEqual(Cipher0, srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_ENCR, 0, 0)) end
		},
		{"Test RTP session authentication key generation (Label #1)",
			fun() -> ?assertEqual(Cipher1, srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_AUTH, 0, 0)) end
		},
		{"Test RTP session salt key generation (Label #2)",
			fun() -> ?assertEqual(Cipher2, srtp:derive_key(MasterKey, MasterSalt, ?SRTP_LABEL_RTP_SALT, 0, 0)) end
		},
		{"Simple RTP session auth key generation #0",
			fun() -> ?assertEqual(<<16#CEBE321F6FF7716B6FD4AB49AF256A15:128>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60000:128>>, <<0:128>>)) end
		},
		{"Simple RTP session auth key generation #1",
			fun() -> ?assertEqual(<<16#6D38BAA48F0A0ACF3C34E2359E6CDBCE:128>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60001:128>>, <<0:128>>)) end
		},
		{"Simple RTP session auth key generation #2",
			fun() -> ?assertEqual(<<16#E049646C43D9327AD175578EF7227098:128>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60002:128>>, <<0:128>>)) end
		},
		{"Simple RTP session auth key generation #3",
			fun() -> ?assertEqual(<<16#6371C10C9A369AC2F94A8C5FBCDDDC25:128>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60003:128>>, <<0:128>>)) end
		},
		{"Simple RTP session auth key generation #4",
			fun() -> ?assertEqual(<<16#6D6E919A48B610EF17C2041E47403576:128>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60004:128>>, <<0:128>>)) end
		},
		{"Simple RTP session auth key generation #5",
			fun() -> ?assertMatch(<<16#6B68642C59BBFC2F34DB60DBDFB2:112, _/binary>>,
						crypto:aes_ctr_encrypt(MasterKey, <<16#0EC675AD498AFEEAB6960B3AABE60005:128>>, <<0:128>>)) end
		}
	].
