-module(otc_tests).

-include_lib("eunit/include/eunit.hrl").

decode_m3ua_no_payload_test() ->
    Bin = <<16#01,16#00,16#02,16#01,16#00,16#00,16#00,16#10,
            16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    Msg = otc:decode(m3ua, Bin),
    ?assertMatch({ok, [#{protocol := m3ua}]}, Msg),
    NewBin = otc:encode(element(2, Msg)),
    ?assertEqual({ok, Bin}, NewBin).

decode_m3ua_sccp_payload_test() ->
    TcapBin = <<16#62,16#26,16#48,16#04,
                16#72,16#10,16#01,16#b3,
                16#6b,16#1e,16#28,16#1c,
                16#06,16#07,16#00,16#11,
                16#86,16#05,16#01,16#01,
                16#01,16#a0,16#11,16#60,
                16#0f,16#80,16#02,16#07,
                16#80,16#a1,16#09,16#06,
                16#07,16#04,16#00,16#00,
                16#01,16#00,16#15,16#03>>,
    SccpBin = <<16#11,16#80,16#0F,16#04,
                16#0F,16#1A,16#00,16#0B,
                16#12,16#08,16#00,16#11,
                16#04,16#64,16#77,16#77,
                16#77,16#77,16#07,16#0B,
                16#12,16#08,16#00,16#11,
                16#04,16#64,16#77,16#77,
                16#77,16#77,16#07,16#28,
                TcapBin/binary>>,

    M3uaBin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#60,
                16#02,16#10,16#00,16#58,16#00,16#00,16#05,16#04,
                16#00,16#00,16#35,16#a7,16#03,16#03,16#00,16#08,
                SccpBin/binary>>,

    M3ua = #{protocol => m3ua,
             message_type => data,
             message_class => transfer,
             protocol_data =>
                 #{service_indicator => sccp,
                   originating_point_code => <<0,0,5,4>>,
                   destination_point_code => <<0,0,53,167>>,
                   network_indicator => national_spare,
                   message_priority => 0,
                   signalling_link_selection => 8}},
    Sccp = #{protocol => sccp,
             message_type => xudt,
             called_party_address =>
                 #{subsystem_number => msc,
                   routing_indicator => global_title,
                   global_title =>
                       #{address => "46777777777",
                         nature_of_address_indicator => 4,
                         numbering_plan => 1,
                         translation_type => 0,
                         odd_even_indicator => odd,
                         encoding_scheme => bcd},
                   national_use_indicator => false,
                   point_code => undefined},
             calling_party_address =>
                 #{subsystem_number => msc,
                   routing_indicator => global_title,
                   global_title =>
                       #{address => "46777777777",
                         nature_of_address_indicator => 4,
                         numbering_plan => 1,
                         translation_type => 0,
                         odd_even_indicator => odd,
                         encoding_scheme => bcd},
                   national_use_indicator => false,
                   point_code => undefined},
             protocol_class => #{options => return_on_error,class => 0},
             hop_counter => 15},
    Tcap = #{protocol => tcap,
             type => 'begin',
             components => [],
             dialogue =>
                 #{type => dialogueRequest,
                   application_context_family => map,
                   user_information => undefined,
                   application_context_name => 'shortMsgMO-RelayContext-v3',
                   supported_versions => [version1]},
             otid => <<"721001B3">>},
    Msg = otc:decode(m3ua, M3uaBin),
    ?assertMatch({ok, [M3ua, Sccp, Tcap]}, Msg),
    NewBin = otc:encode({[M3ua, Sccp], TcapBin}),
    ?assertEqual({ok, M3uaBin}, NewBin).

udt_scmg_test() ->
    CdPA = <<16#42, %% AddressIndicator
             16#01>>, %% SSN
    CgPA = <<16#42, %% AddressIndicator
             16#01>>, %% SSN
    ScMgBin = <<16#03, %% SCMG MsgType
                16#06, %% Affected SSN
                16#12, 16#34, %% Affected PC
                16#00>>, %% Subsystem multiplicity indicator
    SccpBin = <<16#09, %% SCCP MsgType,
                16#80, %% ProtocolClass
                16#03, 16#05, 16#07, %% Pointers
                16#02, CdPA/binary,
                16#02, CgPA/binary,
                16#05, ScMgBin/binary>>,
    Sccp = #{protocol => sccp,
             called_party_address =>
                 #{global_title => undefined,
                   national_use_indicator => false,
                   point_code => undefined,
                   routing_indicator => subsystem_number,
                   subsystem_number => management},
             calling_party_address =>
                 #{global_title => undefined,
                   national_use_indicator => false,
                   point_code => undefined,
                   routing_indicator => subsystem_number,
                   subsystem_number => management},
             message_type => udt,
             protocol_class => #{class => 0, options => return_on_error}},
    ScMg = #{protocol => sccp_mgmt,
             format_identifier => status_test,
             affected_point_code => <<18,52>>,
             affected_subsystem_number => hlr,
             subsystem_multiplicity_indicator => 0
            },
    Val = otc:decode(sccp, SccpBin),
    ?assertEqual({ok, [Sccp, ScMg]}, Val),
    NewBin = otc:encode([Sccp, ScMg]),
    ?assertEqual({ok, SccpBin}, NewBin).


decode_nas_eps_pdn_connectivity_reject_test() ->
    Bin = <<16#27,16#b3,16#14,16#70,
            16#a6,16#03,16#02,16#34,
            16#d1,16#1a,16#37,16#01,16#b6>>,
    Map1 = #{protocol => nas_eps,
             protocol_discriminator => eps_mobility_management_messages,
             security_header_type => integrity_protected_ciphered},
    Map2 = #{protocol => nas_eps_emm,
             sequence_number => <<3>>,
             message_authentication_code => <<179,20,112,166>>},
    Map3 = #{protocol => nas_eps,
             eps_bearer_identity => 0,
             protocol_discriminator => eps_session_management_messages},
    Map4 = #{protocol => nas_eps_esm,
             back_off_timer_value => <<"¶">>,
             esm_cause => <<26>>,
             message_type => pdn_connectivity_reject,
             procedure_transaction_identity => <<"4">>},
    Msg = otc:decode(nas_eps, Bin),
    ?assertMatch({ok, [Map1, Map2, Map3, Map4]}, Msg),
    NewBin = otc:encode(element(2, Msg)),
    ?assertEqual({ok, Bin}, NewBin).
