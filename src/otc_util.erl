-module(otc_util).

-export([
    decode_tbcd/1,
    encode_tbcd/1,
    decode_mcc_mnc/1,
    encode_mcc_mnc/1
]).


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

decode_mcc_mnc(<<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2]);
decode_mcc_mnc(<<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>) ->
    lists:map(fun dec_tbcd_digit/1, [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3]).

encode_mcc_mnc(MCCMNC) when length(MCCMNC) == 5 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, 2#1111:4, MCC3:4, MNC2:4, MNC1:4>>;
encode_mcc_mnc(MCCMNC) when length(MCCMNC) == 6 ->
    [MCC1, MCC2, MCC3, MNC1, MNC2, MNC3] = lists:map(fun enc_tbcd_digit/1, MCCMNC),
    <<MCC2:4, MCC1:4, MNC3:4, MCC3:4, MNC2:4, MNC1:4>>.