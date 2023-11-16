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
         true_to_zero/1
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

decode_gsm_packed(Str) ->
    decode_gsm_packed(Str, <<>>).

decode_gsm_packed(<<>>, _) ->
    [];
decode_gsm_packed(Bin, Acc) when byte_size(Bin) > 0 ->
    L = 7-bit_size(Acc),
    <<B:(8-L), A:L, Rest/binary>> = Bin,
    <<S:8>> = <<0:1, A:L, Acc/bitstring>>,
    [S|decode_gsm_packed(Rest, <<B:(8-L)>>)].

encode_gsm_packed(Str) ->
    list_to_bitstring(encode_gsm_packed(Str, <<>>)).

encode_gsm_packed([], Acc) ->
    L = bit_size(Acc),
    [<<0:(8-L), Acc/bitstring>>];
encode_gsm_packed([A|Rs], Acc) ->
    P = (8-bit_size(Acc)) rem 8,
    K = (7+bit_size(Acc)) rem 8,
    <<_:1, B1:K, B0:P>> = <<A:8>>,
    [<<B0:P, Acc/bitstring>>|encode_gsm_packed(Rs, <<B1:K>>)].

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

