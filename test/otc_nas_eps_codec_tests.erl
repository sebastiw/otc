-module(otc_nas_eps_codec_tests).

-include_lib("eunit/include/eunit.hrl").

plain_attach_request_test_() ->
    Bin = hexstream_to_binary(<<"0741710809101000001000100620200000000000040201d011">>),
    Map = #{security_header_type => plain_nas_message,
            protocol_discriminator => eps_mobility_management_messages,
            message_type => attach_request,
            nas_key_set_identifier => 7,
            eps_attach_type => 1,
            eps_mobile_identity => <<9,16,16,0,0,16,0,16>>,
            esm_message_container =>
                #{eps_bearer_identity => 0,
                  message_type => pdn_connectivity_request,
                  pdn_type => 1,
                  procedure_transaction_identity => <<1>>,
                  protocol_discriminator =>
                      eps_session_management_messages,
                  request_type => 1},
            ue_network_capability => <<32,32,0,0,0,0>>},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

integrity_attach_request_test_() ->
    Bin = hexstream_to_binary(<<"17be27ab2e3a0741610bf644f05180000ac0000c0305f"
                                "070c0401100270233d011d12720808021100100001081"
                                "0600000000830600000000000d00000a00000500001000"
                                "5244f05100015c0a003103e5e03e11034f18a640080402"
                                "600000021f005d0103e0c1">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected,
            sequence_number => <<58>>,
            message_authentication_code => hexstream_to_binary(<<"be27ab2e">>),
            nas_message =>
                #{drx_parameter => <<10,0>>,
                  eps_attach_type => 1,
                  eps_mobile_identity =>
                      <<246,68,240,81,128,0,10,192,0,12,3>>,
                  esm_message_container =>
                      #{eps_bearer_identity => 0,
                        esm_information_transfer_flag => <<1:4>>,
                        message_type => pdn_connectivity_request,
                        pdn_type => 1,
                        procedure_transaction_identity => <<51>>,
                        protocol_configuration_options =>
                            <<128,128,33,16,1,0,0,16,129,6,0,0,0,0,131,6,0,0,
                              0,0,0,13,0,0,10,0,0,5,0,0,16,0>>,
                        protocol_discriminator =>
                            eps_session_management_messages,
                        request_type => 1},
                  last_visited_registered_tai => <<68,240,81,0,1>>,
                  message_type => attach_request,
                  mobile_station_classmark_2 => <<79,24,166>>,
                  ms_network_capability => <<"åà>">>,
                  ms_network_feature_support => <<1:4>>,
                  nas_key_set_identifier => 6,
                  old_guti_type => <<0:4>>,
                  protocol_discriminator => eps_mobility_management_messages,
                  security_header_type => plain_nas_message,
                  supported_codecs => <<4,2,96,0,0,2,31,0>>,
                  ue_network_capability => <<240,112,192,64,17>>,
                  voice_domain_preference_and_ues_usage_setting => <<3>>}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

plain_authentication_request_test_() ->
    Bin = hexstream_to_binary(<<"075206a73180283e95708d1c6141a545b68a"
                                "45100aa0a855812680002863ebdc835cec7c">>),
    Map = #{security_header_type => plain_nas_message,
            protocol_discriminator => eps_mobility_management_messages,
            message_type => authentication_request,
            nas_key_set_identifierasme => 6,
            authentication_parameter_rand_eps_challenge =>
                hexstream_to_binary(<<"a73180283e95708d1c6141a545b68a45">>),
            authentication_parameter_autn_eps_challenge =>
                hexstream_to_binary(<<"0aa0a855812680002863ebdc835cec7c">>)},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

plain_authentication_response_test_() ->
    Bin = hexstream_to_binary(<<"17f908e12d3c075308373f4dcdb2e7769b">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected,
            sequence_number => <<"<">>,
            message_authentication_code => <<"ù\bá-">>,
            nas_message =>
                #{authentication_response_parameter =>
                      <<55,63,77,205,178,231,118,155>>,
                  message_type => authentication_response,
                  protocol_discriminator => eps_mobility_management_messages,
                  security_header_type => plain_nas_message}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

security_mode_command_test_() ->
    Bin = hexstream_to_binary(<<"37685cc2d900075d010605f070c04070">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_eps_security,
            sequence_number => <<0>>,
            message_authentication_code => hexstream_to_binary(<<"685cc2d9">>),
            nas_message =>
                #{message_type => security_mode_command,
                  nas_key_set_identifier => 6,
                  protocol_discriminator => eps_mobility_management_messages,
                  replayed_ue_security_capabilities => hexstream_to_binary(<<"f070c04070">>),
                  security_header_type => plain_nas_message,
                  selected_nas_security_algorithms => <<1>>}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

security_mode_complete_test_() ->
    Bin = hexstream_to_binary(<<"47cd4d049b00075e">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type =>
                integrity_protected_ciphered_eps_security,
            sequence_number => <<0>>,
            message_authentication_code => hexstream_to_binary(<<"cd4d049b">>),
            nas_message =>
                #{message_type => security_mode_complete,
                  protocol_discriminator => eps_mobility_management_messages,
                  security_header_type => plain_nas_message}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

esm_information_request_test_() ->
    Bin = hexstream_to_binary(<<"2745d8f29b010233d9">>),
    Map = #{message_authentication_code => hexstream_to_binary(<<"45d8f29b0">>),
            protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<1>>,
            nas_message =>
                #{eps_bearer_identity => 0,
                  message_type => esm_information_request,
                  procedure_transaction_identity => <<"3">>,
                  protocol_discriminator => eps_session_management_messages}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

esm_information_response_test_() ->
    Bin = hexstream_to_binary(<<"270bef195a010233da280908696e7465726e6574">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<1>>,
            message_authentication_code => hexstream_to_binary(<<"0bef195a">>),
            nas_message =>
                #{access_point_name => <<"\binternet">>,
                  eps_bearer_identity => 0,
                  message_type => esm_information_response,
                  procedure_transaction_identity => <<"3">>,
                  protocol_discriminator => eps_session_management_messages}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

attach_accept_test_() ->
    Bin = hexstream_to_binary(<<"27118d83930207420149062044f051000100725233c10"
                                "1091c08696e7465726e6574066d6e63303135066d6363"
                                "343430046770727305010a0802615d0100301023911f9"
                                "396fefe764bffff00f700f7003203843401005e04fefe"
                                "f7f7272780000d0401010101000d04010000018021100"
                                "300001081060101010183060100000100100205dc500b"
                                "f644f05180000ac0000e0359496401015e0106">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<2>>,
            message_authentication_code => <<17,141,131,147>>,
            nas_message =>
                #{eps_attach_result => 1,
                  eps_network_feature_support => <<1>>,
                  esm_message_container =>
                      #{access_point_name =>
                            <<8,105,110,116,101,114,110,101,116,6,109,110,99,
                              48,49,53,6,109,99,99,52,52,48,4,103,112,114,115>>,
                        apn_ambr => <<"þþ÷÷">>,
                        eps_bearer_identity => 5,
                        eps_qos => <<"\t">>,
                        message_type =>
                            activate_default_eps_bearer_context_request,
                        negotiated_llc_sapi => <<3>>,
                        negotiated_qos =>
                            <<35,145,31,147,150,254,254,118,75,255,255,0,247,
                              0,247,0>>,
                        packet_flow_identifier => <<0>>,
                        pdn_address => <<1,10,8,2,97>>,
                        procedure_transaction_identity => <<"3">>,
                        protocol_configuration_options =>
                            <<128,0,13,4,1,1,1,1,0,13,4,1,0,0,1,128,33,16,3,0,
                              0,16,129,6,1,1,1,1,131,6,1,0,0,1,0,16,2,5,220>>,
                        protocol_discriminator =>
                            eps_session_management_messages,
                        radio_priority => <<4:4>>,
                        transaction_identifier => <<0>>},
                  guti => <<246,68,240,81,128,0,10,192,0,14,3>>,
                  message_type => attach_accept,
                  protocol_discriminator => eps_mobility_management_messages,
                  security_header_type => plain_nas_message,
                  t3412_extended_value => <<6>>,
                  t3412_value => <<"I">>,t3423_value => <<"I">>,
                  tai_list => <<32,68,240,81,0,1>>}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

activate_default_eps_bearer_context_accept_test_() ->
    Bin = hexstream_to_binary(<<"2714573c4602074300035200c2">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<2>>,
            message_authentication_code => <<20,87,60,70>>,
            nas_message =>
                #{esm_message_container =>
                      #{eps_bearer_identity => 5,
                        message_type =>
                            activate_default_eps_bearer_context_accept,
                        procedure_transaction_identity => <<0>>,
                        protocol_discriminator =>
                            eps_session_management_messages},
                  message_type => attach_complete,
                  protocol_discriminator => eps_mobility_management_messages,
                  security_header_type => plain_nas_message}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

pdn_connectivity_request_test_() ->
    Bin = hexstream_to_binary(<<"27ed17dacc030234d031280403696d732729808021100"
                                "1000010810600000000830600000000000d0000030000"
                                "0100000c00000a00000500001000">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<3>>,
            message_authentication_code => <<237,23,218,204>>,
            nas_message =>
                #{access_point_name => <<3,105,109,115>>,
                  eps_bearer_identity => 0,
                  message_type => pdn_connectivity_request,
                  pdn_type => 3,
                  procedure_transaction_identity => <<"4">>,
                  protocol_configuration_options =>
                      <<128,128,33,16,1,0,0,16,129,6,0,0,0,0,131,6,0,0,0,0,0,
                        13,0,0,3,0,0,1,0,0,12,0,0,10,0,0,5,0,0,16,0>>,
                  protocol_discriminator => eps_session_management_messages,
                  request_type => 1}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

pdn_connectivity_reject_test_() ->
    Bin = hexstream_to_binary(<<"27b31470a6030234d11a3701b6">>),
    Map = #{protocol_discriminator => eps_mobility_management_messages,
            security_header_type => integrity_protected_ciphered,
            sequence_number => <<3>>,
            message_authentication_code => <<179,20,112,166>>,
            nas_message =>
                #{back_off_timer_value => <<"¶">>,
                  eps_bearer_identity => 0,
                  esm_cause => <<26>>,
                  message_type => pdn_connectivity_reject,
                  procedure_transaction_identity => <<"4">>,
                  protocol_discriminator => eps_session_management_messages}},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

service_request_test_() ->
    Bin = hexstream_to_binary(<<"c7c4c952">>),
    Map = #{ksi_and_sequence_number => <<"Ä">>,
            message_authentication_code_short => <<"ÉR">>,
            message_type => service_request,
            protocol_discriminator => eps_mobility_management_messages,
            security_header_type => service_request},
    [?_assertEqual(Map, otc_nas_eps_codec:decode(Bin)),
     ?_assertEqual(Bin, otc_nas_eps_codec:encode(Map))
    ].

invalid_message_test() ->
    Bin = hexstream_to_binary(<<"deadbeef">>),
    ?assertMatch({unsupported, {protocol_discriminator, _}}, otc_nas_eps_codec:decode(Bin)).

hexstream_to_binary(In) ->
    list_to_binary([binary_to_integer(<<A, B>>, 16) || <<A, B>> <= In]).
