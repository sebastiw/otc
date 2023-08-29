-module(otc_gtpv1c_codec_tests).

-include_lib("eunit/include/eunit.hrl").

sgsn_context_response_test() ->
    Bin = <<"3233002c09fe4b60850e0000018010d8fde1aa113aeb040a850004c0a8a8f58500"
            "04c0a8a8f5ff000bd7cf030020060103070180">>,
    Map = #{message_type => sgsn_context_response,
            message_group => mobility_management,
            teid => 167660384,
            sequence_number => 34062,
            extension_headers => #{},
            cause => request_accepted,
            tunnel_endpoint_identifier_control_plane => 988480522,
            sgsn_address_for_control_plane =>
                #{ipv4 => {192,168,168,245}},
            alternative_ggsn_address_for_control_plane =>
                #{ipv4 => {192,168,168,245}},
            private_extension =>
                #{identifier => 55247,
                  value => binary:decode_hex(<<"030020060103070180">>)}
           },
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

create_pdp_context_request_test() ->
    Bin = <<"321000a600000000a170ff000242900901020304f50342f089fffeff0ffc100015"
            "78aa11bc391c691405800002f12183000908696e7465726e657484002380802110"
            "01000010810600000000830600000000000d00000a000005000010000011008500"
            "04d9ae4c22850004d9ae4c38860007916427851633f787000f0223921f9396fdfe"
            "74fcffff00640094000140970001019800080142f0890408acf099000240009a00"
            "083175113171327203">>,
    Map = #{message_group => tunnel_management,
            message_type => create_pdp_context_request,
            teid => 0,
            sequence_number => 41328,
            extension_headers => #{},
            imsi => "240990102030405",
            routeing_area_identity =>
                #{mcc => "240", mnc => "98",
                  location_area_code => 65534,
                  routing_area_code => 255},
            selection_mode => verified,
            tunnel_endpoint_identifier_data_i => binary:decode_hex(<<"001578aa">>),
            tunnel_endpoint_identifier_control_plane => 3157859433,
            nsapi => 5,
            end_user_address => ipv4,
            access_point_name => "internet",
            protocol_configuration_options =>
                [#{id => 32801,
                   content => binary:decode_hex(<<"01000010810600000000830600000000">>)},
                 #{id => 13},
                 #{id => 10},
                 #{id => 5},
                 #{id => 16},
                 #{id => 17}],
            sgsn_address_for_signalling => #{ipv4 => {217,174,76,34}},
            sgsn_address_for_user_traffic => #{ipv4 => {217,174,76,56}},
            msisdn => "1946725861337",
            quality_of_service_profile =>
                #{allocation_retention_priority => <<2>>,
                  qos_profile_data =>
                      binary:decode_hex(<<"23921f9396fdfe74fcffff006400">>)},
            common_flags =>
                #{dual_address_bearer_flag => 0,
                  upgrade_qos_supported => 1,
                  nrsn => 0,
                  no_qos_negotiation => 0,
                  mbms_counting_information => 0,
                  ran_procedures_ready => 0,
                  mbms_service_type => 0,
                  prohibit_payload_compression => 0},
            rat_type => utran,
            user_location_information =>
                #{mcc => "240", mnc => "98",
                  location_area_code => 1032,
                  service_area_code => 44272},
            ms_time_zone =>
                #{time_zone => 60,
                  daylight_saving_time => no_adjustment},
            imeisv => #{imei => "135711131723273",
                        sv => "0"}},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

create_pdp_context_response_test() ->
    Bin = <<"32110072bc391c69a1700000018008fe1080c6c00511850ee0057f016580db8000"
            "06f1210a077c7c84002b80000d0408080808000d04080804048021100300001081"
            "06080808088306080804040005010200100205dc85000454d89e0385000454d89e"
            "0387000f0223921f9396fdfe74fcffff006400b8000101">>,
    Map = #{message_group => tunnel_management,
            message_type => create_pdp_context_response,
            extension_headers => #{},
            teid => 3157859433,
            sequence_number => 41328,
            cause => request_accepted,
            reordering_required => false,
            tunnel_endpoint_identifier_data_i => binary:decode_hex(<<"80c6c005">>),
            tunnel_endpoint_identifier_control_plane => 2232344581,
            charging_id => 23429339,
            end_user_address =>
                #{ipv4 => {10,7,124,124}},
            protocol_configuration_options =>
                [#{id => 13,
                   content => <<8,8,8,8>>},
                 #{id => 13,
                   content => <<8,8,4,4>>},
                 #{id => 32801,
                   content => binary:decode_hex(<<"03000010810608080808830608080404">>)},
                 #{id => 5,
                   content => binary:decode_hex(<<"02">>)},
                 #{id => 16,
                   content => binary:decode_hex(<<"05dc">>)}],
            ggsn_address_for_control_plane =>
                #{ipv4 => {84,216,158,3}},
            ggsn_address_for_user_traffic =>
                #{ipv4 => {84,216,158,3}},
            quality_of_service_profile =>
                #{allocation_retention_priority => <<2>>,
                  qos_profile_data =>
                      binary:decode_hex(<<"23921f9396fdfe74fcffff006400">>)},
            bearer_control_mode => ms_nw},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

update_pdp_context_request_test() ->
    Bin = <<"3612005c89f6c00a8c10000201ffff000242900901020304f50342f27911c0640e"
            "041041e077a21112fb83401405850004ddb105e1850004ddb104e787000d021b62"
            "1f71965858744bffff0094000120970001029800080042f27911c0b18699000223"
            "00">>,
    Map = #{message_group => tunnel_management,
            message_type => update_pdp_context_request,
            teid => 2314649610,
            sequence_number => 35856,
            extension_headers =>
                #{ms_info_change_reporting_support_indication => true},
            imsi => "240990102030405",
            routeing_area_identity =>
                #{mcc => "242", mnc => "97",
                  location_area_code => 4544,
                  routing_area_code => 100},
            recovery => 4,
            tunnel_endpoint_identifier_data_i => binary:decode_hex(<<"41e077a2">>),
            tunnel_endpoint_identifier_control_plane => 318473024,
            nsapi => 5,
            sgsn_address_for_control_plane =>
                #{ipv4 => {221,177,5,225}},
            sgsn_address_for_user_traffic =>
                #{ipv4 => {221,177,4,231}},
            quality_of_service_profile =>
                #{allocation_retention_priority => <<2>>,
                  qos_profile_data =>
                      binary:decode_hex(<<"1b621f71965858744bffff00">>)},
            common_flags =>
                #{dual_address_bearer_flag => 0,
                  upgrade_qos_supported => 0,
                  nrsn => 1,
                  no_qos_negotiation => 0,
                  mbms_counting_information => 0,
                  ran_procedures_ready => 0,
                  mbms_service_type => 0,
                  prohibit_payload_compression => 0},
            rat_type => geran,
            user_location_information =>
                #{mcc => "242", mnc => "97",
                  location_area_code => 4544,
                  cell_identity => 45446},
            ms_time_zone =>
                #{time_zone => 480,
                  daylight_saving_time => no_adjustment}},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

update_pdp_context_response_test() ->
    Bin = <<"3213003912fb83408c10000001800e3c1089f6c00a1189f6c00a7f031403468500"
            "04b9273759850004b927375987000d021b621f71965858744bffff00b5000100">>,
    Map = #{message_group => tunnel_management,
            message_type => update_pdp_context_response,
            teid => 318473024,
            sequence_number => 35856,
            extension_headers => #{},
            cause => request_accepted,
            recovery => 60,
            tunnel_endpoint_identifier_data_i => binary:decode_hex(<<"89f6c00a">>),
            tunnel_endpoint_identifier_control_plane => 2314649610,
            charging_id => 51643206,
            ggsn_address_for_control_plane => #{ipv4 => {185,39,55,89}},
            ggsn_address_for_user_traffic => #{ipv4 => {185,39,55,89}},
            quality_of_service_profile =>
                #{allocation_retention_priority => <<2>>,
                  qos_profile_data =>
                      binary:decode_hex(<<"1b621f71965858744bffff00">>)},
            ms_info_change_reporting_action => stop_reporting},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

delete_pdp_context_request_test() ->
    Bin = <<"32140008850ee005b804ff0013ff1405">>,
    Map = #{message_group => tunnel_management,
            message_type => delete_pdp_context_request,
            teid => 2232344581,
            sequence_number => 47108,
            extension_headers => #{},
            nsapi => 5,
            teardown_ind => true},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

delete_pdp_contest_response_test() ->
    Bin = <<"32150006bc391c69b80400000180">>,
    Map = #{message_group => tunnel_management,
            message_type => delete_pdp_context_response,
            teid => 3157859433,
            sequence_number => 47108,
            extension_headers => #{},
            cause => request_accepted},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

echo_request_test() ->
    Bin = <<"3201000400000000b9650000">>,
    Map = #{message_group => path_management,
            message_type => echo_request,
            teid => 0,
            sequence_number => 47461,
            extension_headers => #{}},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

echo_response_test() ->
    Bin = <<"3202000600000000b96500000e0a">>,
    Map = #{message_type => echo_response,
            message_group => path_management,
            teid => 0,
            sequence_number => 47461,
            extension_headers => #{},
            recovery => 10},
    Msg = otc_gtpv1c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

