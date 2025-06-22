-module(otc_util).

-export([decode_tbcd/1,
         encode_tbcd/1,
         decode_tbcd/2,
         encode_tbcd/2,
         decode_common_mcc_mnc/1,
         encode_common_mcc_mnc/1,
         dec_tbcd_digit/1,
         enc_tbcd_digit/1,
         decode_network_name/1,
         encode_network_name/1,
         decode_gsm_packed/1,
         encode_gsm_packed/1,
         zero_to_allowed/1,
         allowed_to_zero/1,
         one_to_true/1,
         true_to_one/1,
         zero_to_true/1,
         true_to_zero/1,
         write_to_pcap/2
        ]).

-include_lib("eunit/include/eunit.hrl").

%% "telephony binary coded decimal"
%% TS 29.002, type TBCD-STRING
%% "123" -> <<2:4, 1:4, 15:4, 3:4>>.
decode_tbcd(asn1_NOVALUE) ->
    undefined;
decode_tbcd(Bin) ->
    lists:reverse(decode_tbcd(Bin, [])).

decode_tbcd(<<>>, Acc) ->
    Acc;
decode_tbcd(<<2#1111:4, D1:4>>, Acc) ->
    [dec_tbcd_digit(D1) | Acc];
decode_tbcd(<<D1:4, D2:4, Ds/binary>>, Acc) ->
    decode_tbcd(Ds, [dec_tbcd_digit(D1), dec_tbcd_digit(D2) | Acc]).

dec_tbcd_digit(D) when D >= 0, D =< 9 -> D + $0;
dec_tbcd_digit(2#1010) -> $*;
dec_tbcd_digit(2#1011) -> $#;
dec_tbcd_digit(2#1100) -> $a;
dec_tbcd_digit(2#1101) -> $b;
dec_tbcd_digit(2#1110) -> $c;
dec_tbcd_digit(2#1111) -> $f.

encode_tbcd(Str) ->
    encode_tbcd(Str, <<>>).

encode_tbcd([D1, D2 | Ds], Acc) ->
    encode_tbcd(Ds, enc_tbcd_digits(D1, D2, Acc));
encode_tbcd([D], Acc) ->
    enc_tbcd_digits(D, $f, Acc);
encode_tbcd([], Acc) ->
    Acc.

enc_tbcd_digits(D1, D2, Acc) ->
    <<Acc/binary, (enc_tbcd_digit(D2)):4, (enc_tbcd_digit(D1)):4>>.

enc_tbcd_digit(D) when D >= $0, D =< $9   -> D - $0;
enc_tbcd_digit($*)                        -> 2#1010;
enc_tbcd_digit($#)                        -> 2#1011;
enc_tbcd_digit(A) when A =:= $A; A =:= $a -> 2#1100;
enc_tbcd_digit(B) when B =:= $B; B =:= $b -> 2#1101;
enc_tbcd_digit(C) when C =:= $C; C =:= $c -> 2#1110;
enc_tbcd_digit(F) when F =:= $F; F =:= $f -> 2#1111.

%% 3GPP TS 24.008
decode_common_mcc_mnc(<<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2]);
decode_common_mcc_mnc(<<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3]).

encode_common_mcc_mnc(MCCMNC) when length(MCCMNC) == 5 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>;
encode_common_mcc_mnc(MCCMNC) when length(MCCMNC) == 6 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>.

%% 3GPP TS 24.008 Figure 10.5.80, and Table 10.5.94
decode_network_name(<<1:1, CS:3, AddCI:1, _SpareBits:3, Str/binary>>) ->
    case CS of
        2#000 ->
            %% defined in 3GPP TS 23.038 [8b]
            #{coding_scheme => cell_broadcast,
              text_string => decode_gsm_packed(Str),
              add_country_initials => one_to_true(AddCI)
             };
        2#001 ->
            ucs2
    end.

encode_network_name(#{coding_scheme := cell_broadcast,
                      text_string := Str,
                      add_country_initials := CI}) ->
    SpareBits = length(Str) rem 8,
    TextBin = encode_gsm_packed(Str),
    AddCI = true_to_one(CI),
    <<1:1, 2#000:3, AddCI:1, SpareBits:3, TextBin/binary>>.

decode_gsm_packed(Bin) ->
    Spares = 8-((byte_size(Bin)*7) rem 8),
    decode_gsm_packed(Bin, Spares).

decode_gsm_packed(Bin, SpareBits) ->
    <<0:SpareBits, R/bitstring>> = binary_reverse(Bin),
    lists:reverse([S || <<S:7>> <= R]).

encode_gsm_packed(Str) ->
    Spares = 8-((length(Str)*7) rem 8),
    encode_gsm_packed(Str, Spares).

encode_gsm_packed(Str, SpareBits) ->
    R = <<<<S:7>> || S <- lists:reverse(Str)>>,
    binary_reverse(<<0:SpareBits, R/bitstring>>).

binary_reverse(Bin) ->
    binary:encode_unsigned(binary:decode_unsigned(Bin, little)).

zero_to_allowed(0) -> allowed;
zero_to_allowed(1) -> not_allowed.

allowed_to_zero(allowed) -> 0;
allowed_to_zero(not_allowed) -> 1.

one_to_true(1) -> true;
one_to_true(0) -> false.

true_to_one(true) -> 1;
true_to_one(false) -> 0.

zero_to_true(0) -> true;
zero_to_true(1) -> false.

true_to_zero(true) -> 0;
true_to_zero(false) -> 1.

gsm_packed_test() ->
    ?assertEqual("abcdef", decode_gsm_packed(<<97,241,152,92,54,3>>)),
    ?assertEqual(<<97,241,152,92,54,3>>, encode_gsm_packed("abcdef")),
    ?assertEqual("gsm7enc", decode_gsm_packed(binary:decode_hex(<<"E779FB56768F01">>))),
    ?assertEqual(binary:decode_hex(<<"E779FB56768F01">>), encode_gsm_packed("gsm7enc")),
    ?assertEqual(<<79,234,16>>, encode_gsm_packed("OTC")).

write_to_pcap(Filename, {[LowestLayer|_], _} = Tuple) ->
  {ok, Bin} = otc:encode(Tuple),
  PPI = otc_sctp_ppi:codec(maps:get(protocol, LowestLayer), #{}),
  write_to_pcap(Filename, PPI, Bin);
write_to_pcap(Filename, [LowestLayer|_] = List) ->
  {ok, Bin} = otc:encode(List),
  PPI = otc_sctp_ppi:codec(maps:get(protocol, LowestLayer), #{}),
  write_to_pcap(Filename, PPI, Bin).

write_to_pcap(Filename, PPI, Bin) ->
  DataChunk = <<16#28024345:32, %% TSN
                0:16, 0:16, %% Stream identifier, sequence number
                PPI:32, %% PPI
                Bin/binary>>,
  ChunkLen = byte_size(DataChunk) + 4, %% Chunk in bytes including the Type, Flags, Length, and Value.
  ChunkLenRest = case ChunkLen rem 4 of
                   0 ->
                     0;
                   R ->
                     (4 - R)
                 end,
  Chunk1 = <<0, 2#0000_0111, %% Chunk type=DATA, flags
             ChunkLen:16, %% include TSN, Stream ID,Number, PPI
             DataChunk/binary,
             0:(ChunkLenRest*8)>>, %% Possible padding
  SPort = 16384,
  DPort = 2905,
  SCTPChunk = <<SPort:16, DPort:16,
                16#deadbeef:32, %% VTag
                0:32, %% CRC32c checksum
                Chunk1/binary>>,
  SCTPLen = byte_size(SCTPChunk),
  SIP = <<10, 28, 6, 43>>,
  DIP = <<10, 28, 6, 44>>,
  IPPkt = <<16#45, %% version, header length (in 32 bit words)
            16#00, %% TypeOfService
            (SCTPLen+20):16, %% length in octets including internet header and data.
            16#141c:16, %% Identification
            0:16, %% Flags, Fragment Offset
            16#3b, %% TTL
            16#84, %% Protocol=SCTP
            16#dead:16, %% Checksum
            SIP:4/binary, DIP:4/binary,
            SCTPChunk/binary>>,

  DMac = binary:decode_hex(<<"8000207A3F3E">>),
  OMac = binary:decode_hex(<<"800020203AAE">>),
  EthPkt = <<DMac:6/binary, OMac:6/binary,
             16#0800:16, %% EtherType=IPv4
             IPPkt/binary,
             0:32>>, %% CRC

  PktSize = byte_size(EthPkt),
  FileBin = <<16#a1b2c3d4:32, %% Magic Number
              16#0002:16, 16#0004:16, %% PCAP Version
              0:32, 0:32, %% Reserved 1, 2
              16#ffff:32, %% SnapLen (i.e. max size per packet)
              0:16, 16#0001:16, %% Flags, LinkType=Ethernet
              16#665E29BE:32, 0:32, %% Timestamp S, MS
              PktSize:32, PktSize:32, %% Captured,Original packet lengths
              EthPkt/binary>>,
  file:write_file(Filename, FileBin).
