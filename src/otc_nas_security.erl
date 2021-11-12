%% 3GPP TS 33.401(EPS) & TS 33.501(5G)
-module(otc_nas_security).

-export([encrypt/6,
         encrypt/7,
         decrypt/6,
         decrypt/7,
         mac/6,
         mac/7,
         kdf/0]).

%% Ciphering

encrypt(Alg, Key, Count, Bearer, Direction, Text) ->
    encrypt(Alg, Key, Count, Bearer, Direction, Text, bit_size(Text)).

encrypt(eea1_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea1_128(Key, Count, Bearer, Direction, Text, Length);
encrypt(eea2_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length);
encrypt(eea3_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea3_128(Key, Count, Bearer, Direction, Text, Length);
encrypt(nea1_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea1_128(Key, Count, Bearer, Direction, Text, Length);
encrypt(nea2_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length);
encrypt(nea3_128, Key, Count, Bearer, Direction, Text, Length) ->
    encrypt_eea3_128(Key, Count, Bearer, Direction, Text, Length).

encrypt_eea1_128(_Key, _Count, _Bearer, _Direction, _Text, _Length) ->
    %% 128-EEA1 is based on SNOW 3G and is identical to UEA2 as
    %% specified in TS 35.215. The used IV is constructed the same
    %% way as in subclause 3.4 of that TS.
    unimplemented.

encrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length) ->
    eea2(Key, Count, Bearer, Direction, Text, Length).

encrypt_eea3_128(_Key, _Count, _Bearer, _Direction, _Text, _Length) ->
    %% 128-EEA3 is based on ZUC and specified in TS 35.221.
    unimplemented.

decrypt(Alg, Key, Count, Bearer, Direction, Text) ->
    decrypt(Alg, Key, Count, Bearer, Direction, Text, bit_size(Text)).

decrypt(eea1_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea1_128(Key, Count, Bearer, Direction, Text, Length);
decrypt(eea2_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length);
decrypt(eea3_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea3_128(Key, Count, Bearer, Direction, Text, Length);
decrypt(nea1_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea1_128(Key, Count, Bearer, Direction, Text, Length);
decrypt(nea2_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length);
decrypt(nea3_128, Key, Count, Bearer, Direction, Text, Length) ->
    decrypt_eea3_128(Key, Count, Bearer, Direction, Text, Length).

decrypt_eea1_128(_Key, _Count, _Bearer, _Direction, _Text, _Length) ->
    %% 128-EEA1 is based on SNOW 3G and is identical to UEA2 as
    %% specified in TS 35.215. The used IV is constructed the same
    %% way as in subclause 3.4 of that TS.
    unimplemented.

decrypt_eea2_128(Key, Count, Bearer, Direction, Text, Length) ->
    eea2(Key, Count, Bearer, Direction, Text, Length).

decrypt_eea3_128(_Key, _Count, _Bearer, _Direction, _Text, _Length) ->
    %% 128-EEA3 is based on ZUC and specified in TS 35.221.
    unimplemented.

eea2(Key, Count, Bearer, Direction, Text, Length) ->
    %% 128-EEA2 is based on 128-bit AES in CTR mode.
    X = ((Bearer band 2#11111) bsl 3) bor ((Direction band 2#1) bsl 2),
    IV = <<Count/binary, X/integer, 0:88>>,
    Out = crypto:crypto_one_time(aes_128_ctr, Key, IV, Text, true),
    Rest = bit_size(Text) - Length,
    <<O:Length, _:Rest>> = Out,
    <<O:Length, 0:Rest>>.


%% Integrity Protection

mac(Alg, Key, Count, Bearer, Direction, Message) ->
    mac(Alg, Key, Count, Bearer, Direction, Message, bit_size(Message)).

mac(eia1_128, Key, Count, Bearer, Direction, Message, Length) ->
    mac_eia1_128(Key, Count, Bearer, Direction, Message, Length);
mac(eia2_128, Key, Count, Bearer, Direction, Message, Length) ->
    mac_eia2_128(Key, Count, Bearer, Direction, Message, Length);
mac(eia3_128, Key, Count, Bearer, Direction, Message, Length) ->
    mac_eia3_128(Key, Count, Bearer, Direction, Message, Length).

mac_eia1_128(_Key, _Count, _Bearer, _Direction, _Message, _Length) ->
    unimplemented.
mac_eia2_128(Key, Count, Bearer, Direction, Message, Length) ->
    eia2(Key, Count, Bearer, Direction, Message, Length).
mac_eia3_128(_Key, _Count, _Bearer, _Direction, _Message, _Length) ->
    unimplemented.

eia2(Key, Count, Bearer, Direction, Message, Length) ->
    %% 128-EIA2 is based on 128-bit AES in CMAC mode.
    %% NOTE: Message not aligned to 8-bit boundary is not supported.
    X = ((Bearer band 2#11111) bsl 3) bor ((Direction band 2#1) bsl 2),

    %% TODO: Pad in right way as described in §2.4, RFC 4493
    %%  In step 4, M_last is calculated by exclusive-OR'ing M_n and one of
    %%  the previously calculated subkeys.  If the last block is a complete
    %%  block (true), then M_last is the exclusive-OR of M_n and K1.
    %%  Otherwise, M_last is the exclusive-OR of padding(M_n) and K2.
    Rest = bit_size(Message) - Length,
    <<Msg:Length, _:Rest>> = Message,
    Input = <<Count/binary, X/integer, 0:24, Msg:Length, 0:Rest>>,
    crypto:macN(cmac, aes_128_cbc, Key, Input, 4).


%% Key derivation

kdf() -> unimplemented.
