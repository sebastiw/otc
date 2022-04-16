-module(otc_m3ua_codec_tests).

-include_lib("eunit/include/eunit.hrl").

m3ua_ssnm_duna_test() ->
    Bin = <<16#01,16#00,16#02,16#01,16#00,16#00,16#00,16#10,
            16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    Exp = #{affected_point_code => [{<<0>>,<<0,5,4>>}],
            message_class => ssnm,
            message_type => duna,
            protocol => m3ua},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_ssnm_dava_test() ->
    Bin = <<16#01,16#00,16#02,16#02,16#00,16#00,16#00,16#10,
            16#00,16#12,16#00,16#08,16#00,16#00,16#05,16#04>>,
    Exp = #{affected_point_code => [{<<0>>,<<0,5,4>>}],
            message_class => ssnm,
            message_type => dava,
            protocol => m3ua},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_aspsm_aspup_test() ->
    Bin = <<16#01,16#00,16#03,16#01,16#00,16#00,16#00,16#08>>,
    Exp = #{message_class => aspsm,
            message_type => aspup,
            protocol => m3ua},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_aspsm_aspup_ack_test() ->
    Bin = <<16#01,16#00,16#03,16#04,16#00,16#00,16#00,16#08>>,
    Exp = #{message_class => aspsm,
            message_type => aspup_ack,
            protocol => m3ua},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_mgmt_ntfy_test() ->
    Bin = <<16#01,16#00,16#00,16#01,16#00,16#00,16#00,16#10,
            16#00,16#0d,16#00,16#08,16#00,16#01,16#00,16#02>>,
    Exp = #{message_class => mgmt,
            message_type => ntfy,
            protocol => m3ua,
            status =>
                #{status_information => application_server_inactive,
                  status_type =>
                      application_server_state_change}},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_asptm_aspac_test() ->
    Bin = <<16#01,16#00,16#04,16#01,16#00,16#00,16#00,16#08>>,
    Exp = #{message_class => asptm,
            message_type => aspac,
            protocol => m3ua},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_asptm_aspac_ack_test() ->
    Bin = <<16#01,16#00,16#04,16#03,16#00,16#00,16#00,16#10,
            16#00,16#0b,16#00,16#08,16#00,16#00,16#00,16#02>>,
    Exp = #{message_class => asptm,
            message_type => aspac_ack,
            protocol => m3ua,
            traffic_mode_type => loadshare},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_tm_data_tcap_empty_begin_test() ->
    D = <<16#11,16#80,16#0f,16#39,16#03,16#0e,16#00,16#0b,16#12,
          16#08,16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#28,
          16#62,16#26,16#48,16#04,16#72,16#10,16#01,16#b3,16#6b,16#1e,16#28,
          16#1c,16#06,16#07,16#00,16#11,16#86,16#05,16#01,16#01,16#01,16#a0,
          16#11,16#60,16#0f,16#80,16#02,16#07,16#80,16#a1,16#09,16#06,16#07,
          16#04,16#00,16#00,16#01,16#00,16#15,16#03,16#0b,16#12,16#08,16#00,
          16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07>>,

    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#60,16#02,16#10,16#00,
            16#58,16#00,16#00,16#05,16#04,16#00,16#00,16#35,16#a7,16#03,16#03,
            16#00,16#08, D/binary>>,
    Exp = #{message_class => tm,
            message_type => data,
            protocol => m3ua,
            protocol_data =>
                #{destination_point_code => <<0,0,53,167>>,
                  message_priority => 0,
                  network_indicator => 3,
                  originating_point_code => <<0,0,5,4>>,
                  service_indicator => sccp,
                  signalling_link_selection => 8,
                  user_protocol_data => D
                 }},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_tm_data_tcap_empty_continue_test() ->
    D = <<16#09,16#81,16#03,16#0e,16#19,16#0b,16#12,16#08,16#00,
          16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#0b,16#12,16#08,
          16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#07,16#36,16#65,
          16#34,16#48,16#04,16#5f,16#f3,16#71,16#68,16#49,16#04,16#72,16#10,
          16#01,16#b3,16#6b,16#26,16#28,16#24,16#06,16#07,16#00,16#11,16#86,
          16#05,16#01,16#01,16#01,16#a0,16#19,16#61,16#17,16#a1,16#09,16#06,
          16#07,16#04,16#00,16#00,16#01,16#00,16#15,16#03,16#a2,16#03,16#02,
          16#01,16#00,16#a3,16#05,16#a1,16#03,16#02,16#01,16#00>>,

    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#6c,16#02,16#10,16#00,
            16#64,16#00,16#00,16#35,16#a7,16#00,16#00,16#05,16#04,16#03,16#03,
            16#00,16#c4,D/binary>>,
    Exp = #{message_class => tm,
            message_type => data,
            protocol => m3ua,
            protocol_data =>
                #{destination_point_code => <<0,0,5,4>>,
                  message_priority => 0,
                  network_indicator => 3,
                  originating_point_code => <<0,0,53,167>>,
                  service_indicator => sccp,
                  signalling_link_selection => 196,
                  user_protocol_data => D
                 }},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

m3ua_tm_data_tcap_abort_test() ->
    D = <<16#09,16#01,16#03,16#0e,16#19,16#0b,16#12,16#08,16#00,
          16#11,16#04,16#64,16#07,16#77,16#77,16#77,16#77,16#0b,16#12,16#08,
          16#00,16#11,16#04,16#64,16#77,16#77,16#77,16#77,16#77,16#0b,16#67,
          16#09,16#49,16#04,16#23,16#03,16#7b,16#22,16#4a,16#01,16#01>>,

    Bin = <<16#01,16#00,16#01,16#01,16#00,16#00,16#00,16#44,16#02,16#10,16#00,
            16#39,16#00,16#00,16#35,16#a7,16#00,16#00,16#05,16#04,16#03,16#03,
            16#00,16#ee,D/binary,16#00,16#00,16#00>>,
    Exp = #{message_class => tm,
            message_type => data,
            protocol => m3ua,
            protocol_data =>
                #{destination_point_code => <<0,0,5,4>>,
                  message_priority => 0,
                  network_indicator => 3,
                  originating_point_code => <<0,0,53,167>>,
                  service_indicator => sccp,
                  signalling_link_selection => 238,
                  user_protocol_data => D
                 }},
    Val = otc_m3ua_codec:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m3ua_codec:encode(Val),
    ?assertEqual(Bin, NewBin).
