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

-module(zrtp_test).

-include("zrtp.hrl").
-include_lib("eunit/include/eunit.hrl").

zrtp_test_() ->
	ZrtpMarker = <<16,0>>, % 16#1000
	MagicCookie = <<90,82,84,80>>,

	% HELLO data

	HelloSequence = <<0,1>>, % 1
	HelloSSRC = <<12,97,171,169>>, % 16#0c61aba9
	HelloPayload = <<80,90,0,38,72,101,108,108,111,32,32,32,49,46,49,48,71,
	78,85,32,90,82,84,80,52,74,32,50,46,49,46,48,36,100,96,195,67,116,141,
	243,85,119,185,231,219,133,160,71,155,124,115,6,65,67,48,136,85,73,84,
	138,65,222,226,134,250,192,159,99,210,55,218,87,82,138,170,107,0,2,68,
	81,83,51,56,52,83,50,53,54,50,70,83,51,65,69,83,51,50,70,83,49,65,69,83,
	49,83,75,51,50,83,75,54,52,72,83,51,50,72,83,56,48,69,67,50,53,68,72,51,
	107,69,67,51,56,68,72,50,107,77,117,108,116,66,51,50,32,166,153,199,74,
	77,108,243,56>>,
	HelloCRC = <<115,174,9,180>>, % 16#73ae09b4
	HelloZrtpBin = <<ZrtpMarker/binary, HelloSequence/binary, MagicCookie/binary, HelloSSRC/binary, HelloPayload/binary, HelloCRC/binary>>,

	HelloMessage = #hello{
		clientid = <<"GNU ZRTP4J 2.1.0">>,
		h3 = <<36,100,96,195,67,116,141,243,85,119,185,231,219,133,160,71,155,124,115,6,65,67,48,136,85,73,84,138,65,222,226,134>>,
		zid = <<250,192,159,99,210,55,218,87,82,138,170,107>>,
		s = 0,
		m = 0,
		p = 0,
		hash = [<<"S384">>,<<"S256">>],
		cipher = [<<"2FS3">>,<<"AES3">>,<<"2FS1">>,<<"AES1">>],
		auth = [<<"SK32">>,<<"SK64">>,<<"HS32">>,<<"HS80">>],
		keyagr = [<<"EC25">>,<<"DH3k">>,<<"EC38">>,<<"DH2k">>,<<"Mult">>],
		sas = [<<"B32 ">>],
		mac = <<166,153,199,74,77,108,243,56>>
	},
	HelloZrtp = #zrtp{sequence = 1, ssrc = 16#0c61aba9, message = HelloMessage},

	% HELLOACK data

	HelloACKSequence = <<0,2>>, % 2
	HelloACKSSRC = <<131,2,99,21>>,
	HelloACKPayload = <<80,90,0,3,72,101,108, 108,111,65,67,75>>,
	HelloACKCRC = <<19,33,158,48>>,
	HelloACKZrtpBin = <<ZrtpMarker/binary, HelloACKSequence/binary, MagicCookie/binary, HelloACKSSRC/binary, HelloACKPayload/binary, HelloACKCRC/binary>>,
	HelloACKZrtp = #zrtp{sequence = 2, ssrc = 2197971733, message = helloack},

	% COMMIT data

	CommitSequence = <<0,3>>, % 3
	CommitSSRC = <<131,2,99,21>>,
	CommitPayload = <<80,90,0,29,67,111,109,109,105,116,32,32,30,245,116,43,
	215,16,39,7,110,252,252,176,151,231,112,74,81,230,239,191,27,210,241,
	137,170,241,115,159,54,127,146,20,12,18,241,209,181,195,98,133,85,240,
	101,201,83,50,53,54,65,69,83,49,72,83,51,50,68,72,51,107,66,51,50,32,47,
	192,193,234,25,118,126,42,0,241,125,53,60,122,157,5,211,171,245,63,113,
	147,58,107,12,143,154,68,86,117,187,40,25,0,51,120,101,107,177,124>>,
	CommitCRC = <<166,61,245,82>>,
	CommitZrtpBin = <<ZrtpMarker/binary, CommitSequence/binary, MagicCookie/binary, CommitSSRC/binary, CommitPayload/binary, CommitCRC/binary>>,

	CommitMessage = #commit{
		h2 = <<30,245,116,43,215,16,39,7,110,252,252,176,151,231,112,74,81,230,239,191,27,210,241,137,170,241,115,159,54,127,146,20>>,
		zid = <<12,18,241,209,181,195,98,133,85,240,101,201>>,
		hash = <<"S256">>,
		cipher = <<"AES1">>,
		auth = <<"HS32">>,
		keyagr = <<"DH3k">>,
		sas = <<"B32 ">>,
		hvi = <<47,192,193,234,25,118,126,42,0,241,125,53,60,122,157,5,211,171,245,63,113,147,58,107,12,143,154,68,86,117,187,40>>,
		nonce = null,
		keyid = null,
		mac = <<25,0,51,120,101,107,177,124>>
	},
	CommitZrtp = #zrtp{sequence = 3, ssrc = 2197971733, message = CommitMessage},

	% DHPART1 data

	Dhpart1Sequence = <<0,4>>, % 4
	Dhpart1SSRC = <<131,2,99,21>>,
	Dhpart1Payload = <<80,90,0,117,68,72,80,97,114,116,49,32,152,154,62,17,
	91,59,159,56,94,85,78,21,208,79,160,223,4,98,78,102,241,157,227,166,215,
	133,35,99,115,241,252,34,215,185,25,48,117,116,71,135,42,250,167,106,
	252,240,36,102,32,157,88,186,193,118,143,181,120,202,237,239,152,31,168,
	188,51,28,210,248,155,9,96,96,53,139,185,169,116,164,82,161,35,76,129,
	197,68,142,64,101,89,165,179,192,26,206,151,32,130,142,243,234,187,123,
	188,3,124,10,24,248,98,115,160,140,58,93,117,137,230,77,202,52,218,218,
	224,5,97,227,23,3,168,104,196,130,3,146,243,252,59,184,103,171,254,234,
	149,248,158,89,23,5,25,63,195,138,77,168,192,129,179,83,111,255,28,255,
	82,151,52,25,27,36,236,46,13,143,196,9,178,206,181,251,52,14,141,7,28,
	29,91,219,218,218,111,78,186,27,8,30,150,73,5,194,23,108,136,98,21,193,
	27,24,141,212,160,79,21,228,13,60,40,158,155,147,147,98,191,73,243,171,
	109,108,210,52,44,12,106,147,83,189,227,121,97,129,231,35,135,234,58,
	213,95,136,39,134,54,72,101,123,117,104,144,101,71,196,187,171,159,191,
	104,193,130,170,47,203,132,66,109,75,233,29,146,58,61,77,2,139,118,38,
	154,61,54,125,45,108,160,125,18,235,175,116,177,82,145,119,78,172,64,
	166,199,237,253,105,234,237,137,10,143,125,238,68,56,128,14,245,2,79,1,
	161,152,131,128,181,250,218,95,19,196,231,98,41,8,69,115,215,91,212,249,
	238,122,86,242,153,84,141,241,82,138,110,153,187,163,228,9,116,228,47,
	53,113,145,110,47,193,11,66,186,179,135,177,192,124,58,249,107,59,115,
	104,76,136,164,25,20,78,246,22,60,166,7,124,64,44,16,34,69,32,42,120,
	244,214,117,120,129,174,87,92,229,69,225,40,22,159,84,43,2,173,85,194,
	107,0,194,88,134,26,84,141,19,203,125,228,183,138,25,246,233,212,9,8,
	127,217,180,230,31,183,135,17,197,175,0,22,38,238,34,55>>,
	Dhpart1CRC = <<7,55,38,47>>,
	Dhpart1ZrtpBin = <<ZrtpMarker/binary, Dhpart1Sequence/binary, MagicCookie/binary, Dhpart1SSRC/binary, Dhpart1Payload/binary, Dhpart1CRC/binary>>,

	Dhpart1Message = #dhpart1{
		h1 = <<152,154,62,17,91,59,159,56,94,85,78,21,208,79,160,223,4,98,78,102,241,157,227,166,215,133,35,99,115,241,252,34>>,
		rs1IDr = <<215,185,25,48,117,116,71,135>>,
		rs2IDr = <<42,250,167,106,252,240,36,102>>,
		auxsecretIDr = <<32,157,88,186,193,118,143,181>>,
		pbxsecretIDr = <<120,202,237,239,152,31,168,188>>,
		pvr = <<51,28,210,248,155,9,96,96,53,139,185,169,116,164,82,161,
		35,76,129,197,68,142,64,101,89,165,179,192,26,206,151,32,130,
		142,243,234,187,123,188,3,124,10,24,248,98,115,160,140,58,93,
		117,137,230,77,202,52,218,218,224,5,97,227,23,3,168,104,196,130,
		3,146,243,252,59,184,103,171,254,234,149,248,158,89,23,5,25,63,
		195,138,77,168,192,129,179,83,111,255,28,255,82,151,52,25,27,36,
		236,46,13,143,196,9,178,206,181,251,52,14,141,7,28,29,91,219,
		218,218,111,78,186,27,8,30,150,73,5,194,23,108,136,98,21,193,27,
		24,141,212,160,79,21,228,13,60,40,158,155,147,147,98,191,73,243,
		171,109,108,210,52,44,12,106,147,83,189,227,121,97,129,231,35,
		135,234,58,213,95,136,39,134,54,72,101,123,117,104,144,101,71,
		196,187,171,159,191,104,193,130,170,47,203,132,66,109,75,233,29,
		146,58,61,77,2,139,118,38,154,61,54,125,45,108,160,125,18,235,
		175,116,177,82,145,119,78,172,64,166,199,237,253,105,234,237,
		137,10,143,125,238,68,56,128,14,245,2,79,1,161,152,131,128,181,
		250,218,95,19,196,231,98,41,8,69,115,215,91,212,249,238,122,86,
		242,153,84,141,241,82,138,110,153,187,163,228,9,116,228,47,53,
		113,145,110,47,193,11,66,186,179,135,177,192,124,58,249,107,59,
		115,104,76,136,164,25,20,78,246,22,60,166,7,124,64,44,16,34,69,
		32,42,120,244,214,117,120,129,174,87,92,229,69,225,40,22,159,84,
		43,2,173,85,194,107,0,194,88,134,26,84,141,19,203,125,228,183,
		138,25,246,233,212,9,8,127,217,180,230,31,183,135,17>>,
		mac = <<197,175,0,22,38,238,34,55>>
	},
	Dhpart1Zrtp = #zrtp{sequence = 4, ssrc = 2197971733, message = Dhpart1Message},

	% DHPART2 data

	Dhpart2Sequence = <<0,4>>, % 4
	Dhpart2SSRC = <<131,2,101,199>>,
	Dhpart2Payload = <<80,90,0,117,68,72,80,97,114,116,50,32,50,11,151,90,
	234,110,70,186,161,209,67,93,197,140,2,30,65,221,70,65,130,98,118,42,
	132,229,100,162,236,93,114,241,22,38,97,78,137,5,28,82,237,173,46,63,
	159,117,201,77,8,33,19,160,92,32,19,238,81,81,204,168,177,80,21,72,51,
	136,103,93,43,31,149,20,162,217,218,62,88,101,149,229,173,75,10,209,64,
	209,207,170,156,99,3,21,251,52,93,134,150,125,85,30,94,181,245,212,235,
	9,21,56,248,90,122,101,76,27,255,207,88,196,56,184,22,17,120,147,60,236,
	255,55,18,184,246,254,98,201,146,93,78,8,119,140,104,67,44,156,224,223,
	59,158,104,143,109,150,177,192,206,48,190,71,213,116,222,113,226,222,
	235,227,200,9,234,196,111,7,30,130,234,70,194,255,224,44,143,61,55,121,
	82,127,241,231,111,146,51,118,23,138,153,19,200,243,91,137,73,167,84,33,
	239,78,122,240,86,14,29,59,86,70,209,177,245,64,10,73,129,179,255,90,13,
	200,177,101,97,60,250,238,31,163,110,232,199,87,218,223,146,74,221,246,
	208,135,77,79,162,102,96,81,248,19,126,23,245,233,122,131,183,74,218,
	200,138,16,144,223,246,109,215,165,53,62,115,244,92,206,83,27,120,195,
	231,68,241,45,37,85,91,51,170,99,142,174,6,130,209,109,104,202,38,116,
	214,105,20,113,26,32,96,207,147,180,70,142,247,103,101,240,61,223,35,69,
	25,123,22,132,104,245,186,58,51,200,193,233,160,230,143,22,150,136,196,
	55,194,184,205,24,88,146,251,116,163,191,28,169,73,111,165,235,253,58,
	235,102,29,238,254,227,151,130,205,135,227,164,169,64,121,7,8,3,225,207,
	160,8,210,243,131,164,163,172,2,132,155,199,224,253,238,51,167,54,214,
	150,146,31,122,33,93,130,211,53,7,212,80,199,214,234,199,150,34,132,61,
	88,63,225,175,135,214,61,6,58,213,174,26,236,27,74,69,23,231,249,124,
	130,127,134,235,175,26,109,81,82,136,186,70,225,126,85,81,75>>,
	Dhpart2CRC = <<198,17,69,244>>,
	Dhpart2ZrtpBin = <<ZrtpMarker/binary, Dhpart2Sequence/binary, MagicCookie/binary, Dhpart2SSRC/binary, Dhpart2Payload/binary, Dhpart2CRC/binary>>,

	Dhpart2Message = #dhpart2{
		h1 = <<50,11,151,90,234,110,70,186,161,209,67,93,197,140,2,30,65,221,70,65,130,98,118,42,132,229,100,162,236,93,114,241>>,
		rs1IDi = <<22,38,97,78,137,5,28,82>>,
		rs2IDi = <<237,173,46,63,159,117,201,77>>,
		auxsecretIDi = <<8,33,19,160,92,32,19,238>>,
		pbxsecretIDi = <<81,81,204,168,177,80,21,72>>,
		pvi = <<51,136,103,93,43,31,149,20,162,217,218,62,88,101,149,
		229,173,75,10,209,64,209,207,170,156,99,3,21,251,52,93,134,150,
		125,85,30,94,181,245,212,235,9,21,56,248,90,122,101,76,27,255,
		207,88,196,56,184,22,17,120,147,60,236,255,55,18,184,246,254,98,
		201,146,93,78,8,119,140,104,67,44,156,224,223,59,158,104,143,
		109,150,177,192,206,48,190,71,213,116,222,113,226,222,235,227,
		200,9,234,196,111,7,30,130,234,70,194,255,224,44,143,61,55,121,
		82,127,241,231,111,146,51,118,23,138,153,19,200,243,91,137,73,
		167,84,33,239,78,122,240,86,14,29,59,86,70,209,177,245,64,10,73,
		129,179,255,90,13,200,177,101,97,60,250,238,31,163,110,232,199,
		87,218,223,146,74,221,246,208,135,77,79,162,102,96,81,248,19,
		126,23,245,233,122,131,183,74,218,200,138,16,144,223,246,109,
		215,165,53,62,115,244,92,206,83,27,120,195,231,68,241,45,37,85,
		91,51,170,99,142,174,6,130,209,109,104,202,38,116,214,105,20,
		113,26,32,96,207,147,180,70,142,247,103,101,240,61,223,35,69,25,
		123,22,132,104,245,186,58,51,200,193,233,160,230,143,22,150,136,
		196,55,194,184,205,24,88,146,251,116,163,191,28,169,73,111,165,
		235,253,58,235,102,29,238,254,227,151,130,205,135,227,164,169,
		64,121,7,8,3,225,207,160,8,210,243,131,164,163,172,2,132,155,
		199,224,253,238,51,167,54,214,150,146,31,122,33,93,130,211,53,7,
		212,80,199,214,234,199,150,34,132,61,88,63,225,175,135,214,61,6,
		58,213,174,26,236,27,74,69,23,231,249,124,130,127,134,235,175,
		26,109,81,82>>,
		mac = <<136,186,70,225,126,85,81,75>>
	},
	Dhpart2Zrtp = #zrtp{sequence = 4, ssrc = 2197972423, message = Dhpart2Message},

	% tests

	[
		{"Simple decoding of ZRTP HELLO message payload",
			fun() -> ?assertEqual({ok, HelloMessage}, zrtp:decode_message(HelloPayload)) end
		},
		{"Simple encoding of ZRTP HELLO message to binary",
			fun() -> ?assertEqual(HelloPayload, zrtp:encode_message(HelloMessage)) end
		},
		{"Simple decoding of ZRTP HELLO data packet",
			fun() -> ?assertEqual({ok, HelloZrtp}, rtp:decode(HelloZrtpBin)) end
		},
		{"Check that we can reproduce original HELLO data stream from HELLO record",
			fun() -> ?assertEqual(HelloZrtpBin, rtp:encode(HelloZrtp)) end
		},

		{"Simple decoding of ZRTP HELLOACK message payload",
			fun() -> ?assertEqual({ok, helloack}, zrtp:decode_message(HelloACKPayload)) end
		},
		{"Simple encoding of ZRTP HELLOACK message to binary",
			fun() -> ?assertEqual(HelloACKPayload, zrtp:encode_message(helloack)) end
		},
		{"Simple decoding of ZRTP HELLO ACK data packet",
			fun() -> ?assertEqual({ok, HelloACKZrtp}, rtp:decode(HelloACKZrtpBin)) end
		},
		{"Check that we can reproduce original HELLOACK data stream from HELLOACK record",
			fun() -> ?assertEqual(HelloACKZrtpBin, rtp:encode(HelloACKZrtp)) end
		},

		{"Simple decoding of ZRTP COMMIT message payload",
			fun() -> ?assertEqual({ok, CommitMessage}, zrtp:decode_message(CommitPayload)) end
		},
		{"Simple encoding of ZRTP COMMIT message to binary",
			fun() -> ?assertEqual(CommitPayload, zrtp:encode_message(CommitMessage)) end
		},
		{"Simple decoding of ZRTP COMMIT data packet",
			fun() -> ?assertEqual({ok, CommitZrtp}, rtp:decode(CommitZrtpBin)) end
		},
		{"Check that we can reproduce original COMMIT data stream from COMMIT record",
			fun() -> ?assertEqual(CommitZrtpBin, rtp:encode(CommitZrtp)) end
		},

		{"Simple decoding of ZRTP DHPART1 message payload",
			fun() -> ?assertEqual({ok, Dhpart1Message}, zrtp:decode_message(Dhpart1Payload)) end
		},
		{"Simple encoding of ZRTP DHPART1 message to binary",
			fun() -> ?assertEqual(Dhpart1Payload, zrtp:encode_message(Dhpart1Message)) end
		},
		{"Simple decoding of ZRTP DHPART1 data packet",
			fun() -> ?assertEqual({ok, Dhpart1Zrtp}, rtp:decode(Dhpart1ZrtpBin)) end
		},
		{"Check that we can reproduce original DHPART1 data stream from DHPART1 record",
			fun() -> ?assertEqual(Dhpart1ZrtpBin, rtp:encode(Dhpart1Zrtp)) end
		},

		{"Simple decoding of ZRTP DHPART2 message payload",
			fun() -> ?assertEqual({ok, Dhpart2Message}, zrtp:decode_message(Dhpart2Payload)) end
		},
		{"Simple encoding of ZRTP DHPART2 message to binary",
			fun() -> ?assertEqual(Dhpart2Payload, zrtp:encode_message(Dhpart2Message)) end
		},
		{"Simple decoding of ZRTP DHPART2 data packet",
			fun() -> ?assertEqual({ok, Dhpart2Zrtp}, rtp:decode(Dhpart2ZrtpBin)) end
		},
		{"Check that we can reproduce original DHPART2 data stream from DHPART2 record",
			fun() -> ?assertEqual(Dhpart2ZrtpBin, rtp:encode(Dhpart2Zrtp)) end
		}
	].
