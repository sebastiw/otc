%% 3GPP TS 33.401(EPS) & TS 33.501(5G)
-module(erlumts_nas_security).

-export([encrypt/6,
         encrypt/7,
         decrypt/6,
         decrypt/7,
         mac/1,
         verify_mac/1,
         kdf/0]).

%% Ciphering

encrypt(Alg, Key, Count, Bearer, Dir, Text) ->
    encrypt(Alg, Key, Count, Bearer, Dir, Text, bit_size(Text)).

encrypt(eea1_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea1_128(Key, Count, Bearer, Dir, Text, Length);
encrypt(eea2_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length);
encrypt(eea3_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea3_128(Key, Count, Bearer, Dir, Text, Length);
encrypt(nea1_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea1_128(Key, Count, Bearer, Dir, Text, Length);
encrypt(nea2_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length);
encrypt(nea3_128, Key, Count, Bearer, Dir, Text, Length) ->
    encrypt_eea3_128(Key, Count, Bearer, Dir, Text, Length).

encrypt_eea1_128(_Key, _Count, _Bearer, _Dir, _Text, _Length) ->
    %% 128-EEA1 is based on SNOW 3G and is identical to UEA2 as
    %% specified in TS 35.215. The used IV is constructed the same
    %% way as in subclause 3.4 of that TS.
    unimplemented.

encrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length) ->
    eea2(Key, Count, Bearer, Dir, Text, Length).

encrypt_eea3_128(_Key, _Count, _Bearer, _Dir, _Text, _Length) ->
    %% 128-EEA3 is based on ZUC and specified in TS 35.221.
    unimplemented.

decrypt(Alg, Key, Count, Bearer, Dir, Text) ->
    decrypt(Alg, Key, Count, Bearer, Dir, Text, bit_size(Text)).

decrypt(eea1_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea1_128(Key, Count, Bearer, Dir, Text, Length);
decrypt(eea2_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length);
decrypt(eea3_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea3_128(Key, Count, Bearer, Dir, Text, Length);
decrypt(nea1_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea1_128(Key, Count, Bearer, Dir, Text, Length);
decrypt(nea2_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length);
decrypt(nea3_128, Key, Count, Bearer, Dir, Text, Length) ->
    decrypt_eea3_128(Key, Count, Bearer, Dir, Text, Length).

decrypt_eea1_128(_Key, _Count, _Bearer, _Dir, _Text, _Length) ->
    %% 128-EEA1 is based on SNOW 3G and is identical to UEA2 as
    %% specified in TS 35.215. The used IV is constructed the same
    %% way as in subclause 3.4 of that TS.
    unimplemented.

decrypt_eea2_128(Key, Count, Bearer, Dir, Text, Length) ->
    eea2(Key, Count, Bearer, Dir, Text, Length).

decrypt_eea3_128(_Key, _Count, _Bearer, _Dir, _Text, _Length) ->
    %% 128-EEA3 is based on ZUC and specified in TS 35.221.
    unimplemented.

eea2(Key, Count, Bearer, Dir, Text, Length) ->
    %% 128-EEA2 is based on 128-bit AES in CTR mode.
    X = ((Bearer band 2#11111) bsl 3) bor ((Dir band 2#1) bsl 2),
    IV = <<Count/binary, <<X>>/binary, 0:88>>,
    Out = crypto:crypto_one_time(aes_128_ctr, Key, IV, Text, true),
    Rest = bit_size(Text) - Length,
    <<O:Length, _:Rest>> = Out,
    <<O:Length, 0:Rest>>.

%% Integrity Protection

mac(eia1_128) -> mac_eia1_128();
mac(eia2_128) -> mac_eia2_128();
mac(eia3_128) -> mac_eia3_128().

mac_eia1_128() -> unimplemented.
mac_eia2_128() -> unimplemented.
mac_eia3_128() -> unimplemented.

verify_mac(eia1) -> verify_eia1_128();
verify_mac(eia2) -> verify_eia2_128();
verify_mac(eia3) -> verify_eia3_128();
verify_mac(nia1) -> verify_eia1_128();
verify_mac(nia2) -> verify_eia2_128();
verify_mac(nia3) -> verify_eia3_128().

verify_eia1_128() -> unimplemented.
verify_eia2_128() -> unimplemented.
verify_eia3_128() -> unimplemented.

%% Key derivation

kdf() -> unimplemented.
