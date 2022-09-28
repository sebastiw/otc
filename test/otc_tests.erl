-module(otc_tests).

-include_lib("eunit/include/eunit.hrl").

decode_m3ua_no_payload_test() ->
    Bin = <<16#01,16#00,16#02,16#01,16#00,16#00,16#00,16#10,
            16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    Msg = otc:decode(m3ua, Bin),
    ?assertMatch({ok, [#{protocol := m3ua}]}, Msg),
    NewBin = otc:encode(element(2, Msg)),
    ?assertEqual(Bin, NewBin).

decode_m3ua_sccp_payload_test() ->
    UserData = <<16#62,16#26,16#48,16#04,
                 16#72,16#10,16#01,16#b3,
                 16#6b,16#1e,16#28,16#1c,
                 16#06,16#07,16#00,16#11,
                 16#86,16#05,16#01,16#01,
                 16#01,16#a0,16#11,16#60,
                 16#0f,16#80,16#02,16#07,
                 16#80,16#a1,16#09,16#06,
                 16#07,16#04,16#00,16#00,
                 16#01,16#00,16#15,16#03>>,
    SccpBin = <<16#11,16#80,16#0f,16#39,
                16#03,16#0e,16#00,16#0b,
                16#12,16#08,16#00,16#11,
                16#04,16#64,16#77,16#77,
                16#77,16#77,16#07,16#28,
                UserData/binary,
                16#0b,16#12,16#08,16#00,
                16#11,16#04,16#64,16#77,
                16#77,16#77,16#77,16#07>>,

    M3uaBin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#60,
                16#02,16#10,16#00,16#58,16#00,16#00,16#05,16#04,
                16#00,16#00,16#35,16#a7,16#03,16#03,16#00,16#08,
                SccpBin/binary>>,
    Msg = otc:decode(m3ua, M3uaBin),
    ?assertMatch({ok, {[#{protocol := m3ua}, #{protocol := sccp}], UserData}}, Msg),
    %% Both M3UA and SCCP has the payload backed in the packet.
    {ok, {[#{protocol_data := _PD} = M3UA, _SCCP], _Payload}} = Msg,
    NewBin = otc:encode(M3UA),
    ?assertEqual(M3uaBin, NewBin).

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
    ?assertEqual(Bin, NewBin).
