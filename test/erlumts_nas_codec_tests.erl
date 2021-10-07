-module(erlumts_nas_codec_tests).


-include_lib("eunit/include/eunit.hrl").

-include("../include/nas.hrl").

codec_test() ->
    Msgs = [{attach_request,
             hexstream_to_binary(<<"0741710809101000001000100620200000000000040201d011">>),
             #{security_header_type => 0,
               protocol_discriminator => 7,
               attach_request_message_identity => <<"A">>,
               nas_key_set_identifier => 7,
               eps_attach_type => 1,
               eps_mobile_identity => <<9,16,16,0,0,16,0,16>>,
               esm_message_container => <<2,1,208,17>>,
               ue_network_capability => <<32,32,0,0,0,0>>}},
            {authentication_request,
             hexstream_to_binary(<<"075206a73180283e95708d1c6141a545b68a45100aa0a855812680002863ebdc835cec7c">>),
             #{security_header_type => 7,
               protocol_discriminator => 0,
               authentication_request_message_type => <<"R">>,
               spare_half_octet => 6,
               nas_key_set_identifierasme => 0,
               authentication_parameter_rand_eps_challenge => <<167,49,128,40,62,149,112,141,28,97,65,165,69,182,138,69>>,
               authentication_parameter_autn_eps_challenge => <<10,160,168,85,129,38,128,0,40,99,235,220,131,92,236,124>>}},
            {invalid_message, hexstream_to_binary(<<"deadbeef">>), unsupported}],
    %?debugMsg("testing encoding..."),
    %encode(Msgs),
    ?debugMsg("testing decoding..."),
    decode(Msgs).

encode([]) ->
    ok;
encode([{Name, Expected, Parsed} | Rest]) ->
    Encoded = erlumts_nas_codec:encode(Parsed),
    ?debugFmt("testing ~s ~s: ~s", [encode, Name, ?assertMatch(Expected, Encoded)]),
    encode(Rest).

decode([]) ->
    ok;
decode([{Name, Bin, Expected} | Rest]) ->
    Decoded = erlumts_nas_codec:decode(Bin),
    ?debugFmt("testing ~s ~s: ~s", [decode, Name, ?assertMatch(Expected, Decoded)]),
    decode(Rest).

hexstream_to_binary(In) ->
    list_to_binary([binary_to_integer(<<A, B>>, 16) || <<A, B>> <= In]).
