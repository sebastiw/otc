-module(otc_gtpv2c_codec_tests).

-include_lib("eunit/include/eunit.hrl").

create_session_request_test() ->
    Bin = <<"482001860000000037B2EE000100080042829978563412F14C0006007409036606"
            "454B000800733272311171350156000D001842F08928E742F08900041701530003"
            "0042F089520001000A570009008A67B8031EAC10436F570009018700000000AC10"
            "0899470026000F696E7465726E74696E7465726E6574027636066D6E6330393806"
            "6D63633234300467707273800001000163000100014F00050001000000007F0001"
            "000048000800003D0900003D09004E006F0080C2231501000015106C7357576C73"
            "57576C7357576C735757C2233102000031104F58DE7396EDCF6266F4B6CF369C47"
            "A96A544274444B55476D736E474863774D436F32516E6378467A46464280211001"
            "000010810600000000830600000000000D00000300000A000005000010005D001F"
            "004900010005500016006809000000000000000000000000000000000000000003"
            "00010044720002006300A9001A0007090102030405060708091112131415160521"
            "2223242542F089CC0014000000006400000000000000C8E02B90CC01020304">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            piggy_backed => false,
            message_priority => false,
            message_type => create_session_request,
            teid => 0,
            sequence_number => 3650286,
            imsi => "242899876543211",
            msisdn => "479030666054",
            mei => #{imei => "372327131117531", sv => "0"},
            uli => #{tai => #{mcc => "240",
                              mnc => "98",
                              tracking_area_code => 16#28e7},
                     ecgi => #{mcc => "240",
                               mnc => "98",
                               eutran_cell_identifier => 16#0041701}},
            serving_network => #{mcc => "240",
                                 mnc => "98"},
            rat_type => nr,
            sender_f_teid => #{teid_gre_key => 16#67b8031e,
                               ipv4 => {172,16,67,111},
                               interface_type => s11_mme_gtpc},
            pgw_s5s8_address => #{teid_gre_key => 16#00000000,
                                  ipv4 => {172,16,8,153},
                                  interface_type => s5s8_pgw_gtpc},
            apn => "interntinternet.v6.mnc098.mcc240.gprs",
            selection_mode =>
                #{provided_by => ms,
                  verified => false},
            pdn_type => ipv4,
            pdn_address_allocation => #{ipv4 => {0,0,0,0}},
            maximum_apn_restriction => 0,
            apn_ambr => #{downlink => 4_000_000,
                          uplink => 4_000_000},
            protocol_config_opts => [#{id => 49699,
                                       content =>
                                           binary:decode_hex(<<"01000015106C735"
                                                               "7576C7357576C73"
                                                               "57576C735757">>)
                                      },
                                     #{id => 49699,
                                       content =>
                                           binary:decode_hex(<<"02000031104F58D"
                                                               "E7396EDCF6266F4"
                                                               "B6CF369C47A96A5"
                                                               "44274444B55476D"
                                                               "736E474863774D4"
                                                               "36F32516E637846"
                                                               "7A464642">>)
                                      },
                                     #{id => 32801,
                                       content =>
                                           binary:decode_hex(<<"010000108106000"
                                                               "000008306000000"
                                                               "00">>)
                                      },
                                     #{id => 13},
                                     #{id => 3},
                                     #{id => 10},
                                     #{id => 5},
                                     #{id => 16}
                                    ],
            bearer_contexts_to_be_created =>
                #{eps_bearer_id => 5,
                  bearer_level_qos =>
                      #{guaranteed_bitrate_downlink => 0,
                        guaranteed_bitrate_uplink => 0,
                        maximum_bitrate_downlink => 0,
                        maximum_bitrate_uplink => 0,
                        pre_emption_capability => false,
                        pre_emption_vulnerability => true,
                        priority_level => 10,
                        qci => 9}},
            recovery => 68,
            ue_time_zone =>
                #{daylight_saving_time => no_adjustment,
                  time_zone => 9*60},
            twan_identifier =>
                #{ssid => "010203040506070809",
                  bss_id => "111213141516",
                  civic_address => "2122232425",
                  twan_plmn_id => #{mcc => "240", mnc => "98"}},
            apn_rate_control_status =>
                %% Mar  7, 2019 13:45:00.003936947 CET
                #{uplink_packets_allowed => 100,
                  additional_exception_reports => 0,
                  downlink_packets_allowed => 200,
                  validity_time => {{2019,3,7},{12,45,0}},
                  validity_time_fractions_raw => 16_909_060}},
    ?assertEqual(3_936_947, otc_gtpv2c:fraction_to_ns(16_909_060)),
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

create_session_response_test() ->
    Bin = <<"4821008405415EA10000620002000200100057000901878701000AB92737594F00"
            "0500010A83AC117F000100004800080000000100000001004E00270080000D0408"
            "080808000D04080804048021100300001081060808080883060808040400100205"
            "DC5D002000490001000502000200100057000902858701000AB92737595E000400"
            "03145955">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => create_session_response,
            piggy_backed => false,
            message_priority => false,
            teid => 88170145,
            sequence_number => 98,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node},
            pgw_s5s8s2as2b_f_teid =>
                #{ipv4 => {185,39,55,89},
                  teid_gre_key => 2264989706,
                  interface_type => s5s8_pgw_gtpc},
            pdn_address_allocation => #{ipv4 => {10,131,172,17}},
            apn_restriction => 0,
            apn_ambr => #{downlink => 256,
                          uplink => 256},
            protocol_config_opts =>
                [#{id => 13,
                   content => binary:decode_hex(<<"08080808">>)},
                 #{id => 13,
                   content => binary:decode_hex(<<"08080404">>)},
                 #{id => 32801,
                   content =>
                       binary:decode_hex(<<"03000010810608080808830608080404">>)},
                 #{id => 16,
                   content => binary:decode_hex(<<"05dc">>)}],
            bearer_contexts_created =>
                #{eps_bearer_id => 5,
                  cause =>
                      #{cause => request_accepted,
                        cause_source => originated_by_remote_node},
                  s5s8u_pgw_f_teid =>
                      #{ipv4 => {185,39,55,89},
                        interface_type => s5s8_pgw_gtpu,
                        teid_gre_key => 2264989706},
                  charging_id => <<3,20,89,85>>}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

create_bearer_request_test() ->
    Bin = <<"485F011004B7FF761615720049000100065D00F10049000100005400AB00241080"
            "2920240753002000103B0000000000000004FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
            "FF301140C35B509F43217F2620240753002000103B0000000000000004FFFFFFFF"
            "FFFFFFFFFFFFFFFFFFFFFFFF3011509F43123F2920240753002000103B00000000"
            "00000004FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF301140C35A509F42233E262024"
            "0753002000103B0000000000000004FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF3011"
            "509F4257000900812F8586C60AF01FCD57000901858B28B47BCAB3C7F150001600"
            "2501000000002A000000002A000000002A000000002ACB00050003000A00054D00"
            "0A000100000000001C000000">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            piggy_backed => false,
            message_priority => false,
            message_type => create_bearer_request,
            teid => 79167350,
            sequence_number => 1447282,
            linked_eps_bearer_id => 6,
            bearer_contexts =>
                #{eps_bearer_id => 0,
                  tft =>
                      #{operation => create_new,
                        packet_filters =>
                            [#{direction => downlink,
                               identifier => 0,
                               evaluation_precedence => 128,
                               content => [{remote, #{ipv6 => aton("2407:5300:2000:103b::4"),
                                                      mask => aton("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")}},
                                           {protocol_identifier, 16#11},
                                           {local, #{port => 50011}},
                                           {remote, #{port => 40771}}
                                          ]
                              },
                             #{direction => uplink,
                               identifier => 1,
                               evaluation_precedence => 127,
                               content => [{remote, #{ipv6 => aton("2407:5300:2000:103b::4"),
                                                      mask => aton("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")}},
                                           {protocol_identifier, 16#11},
                                           {remote, #{port => 40771}}
                                          ]
                              },
                             #{direction => downlink,
                               identifier => 2,
                               evaluation_precedence => 63,
                               content => [{remote, #{ipv6 => aton("2407:5300:2000:103b::4"),
                                                      mask => aton("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")}},
                                           {protocol_identifier, 16#11},
                                           {local, #{port => 50010}},
                                           {remote, #{port => 40770}}
                                          ]
                              },
                             #{direction => uplink,
                               identifier => 3,
                               evaluation_precedence => 62,
                               content => [{remote, #{ipv6 => aton("2407:5300:2000:103b::4"),
                                                      mask => aton("ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")}},
                                           {protocol_identifier, 16#11},
                                           {remote, #{port => 40770}}
                                          ]
                              }
                            ]},
                  bearer_level_qos =>
                      #{pre_emption_capability => true,
                        priority_level => 9,
                        pre_emption_vulnerability => false,
                        qci => 1,
                        maximum_bitrate_uplink => 42,
                        maximum_bitrate_downlink => 42,
                        guaranteed_bitrate_uplink => 42,
                        guaranteed_bitrate_downlink => 42},
                  s5s8u_pgw_f_teid =>
                      #{interface_type => s5s8_pgw_gtpu,
                        ipv4 => {202,179,199,241},
                        teid_gre_key => 16#8B28B47B},
                  s1u_sgw_f_teid =>
                      #{interface_type => s1u_sgw_gtpu,
                        ipv4 => {10,240,31,205},
                        teid_gre_key => 16#2F8586C6},
                  maximum_packet_loss_rate =>
                      #{maximum_packet_loss_rate_ul => 10,
                        maximum_packet_loss_rate_dl => 5}
                 },
            indication_flags =>
                #{'5g_srvcc_ho_indication' => 0,
                  '5gc_not_restricted_indication' => 0,
                  '5gc_not_restricted_support' => 0,
                  '5gs_interworking_indication' => 0,
                  '5gs_interworking_without_n26_indication' => 0,
                  abnormal_release_of_radio_link => 0,
                  associate_oci_with_pgw_nodes_identity => 0,
                  associate_oci_with_sgw_nodes_identity => 0,
                  buffered_dl_data_waiting_indication => 0,
                  change_f_teid_support_indication => 0,
                  change_of_location_information_indication => 0,
                  change_of_presence_reporting_area_information_indication => 0,
                  change_reporting_support_indication => 0,
                  control_plane_only_pdn_connection_indication => 0,
                  create_session_request_message_forwarded_indication => 0,
                  cs_to_ps_srvcc_indication => 0,
                  csfb_indication => 0,
                  csg_change_reporting_support_indication => 0,
                  delay_tolerant_connection_indication => 0,
                  direct_forwarding_indication => 0,
                  direct_tunnel_flag => 0,
                  dual_address_bearer_flag => 0,
                  emergency_pdu_session_indication => 0,
                  enb_change_reporting_support_indication => 0,
                  ethernet_pdn_support_indication => 0,
                  extended_ebi_value_range_support_indication => 1,
                  extended_pco_support_indication => 0,
                  handover_indication => 0,
                  idle_mode_signalling_reduction_activation_indication => 0,
                  idle_mode_signalling_reduction_supported_indication => 0,
                  indirect_data_forwarding_with_upf_indication => 0,
                  isr_is_activated_for_the_ue => 0,
                  lte_m_rat_type_reporting_to_pgw_indication => 1,
                  lte_m_satellite_access_indication => 0,
                  lte_m_ue_indication => 1,
                  management_based_mdt_allowed_flag => 0,
                  mt_edt_applicable => 0,s11_u_tunnel_flag => 0,
                  mt_edt_not_applicable => 0,
                  nbifom_support_indication => 0,
                  no_5gs_n26_mobility_indication => 0,
                  notify_source_enodeb_indication => 0,
                  notify_start_pause_of_charging_via_user_plane_support_indication => 0,
                  operation_indication => 0,
                  p_cscf_restoration_indication => 0,
                  pdn_pause_off_indication => 0,
                  pdn_pause_on_enabled_indication => 0,
                  pdn_pause_support_indication => 0,
                  pending_mt_short_message_indication => 0,
                  pending_network_initiated_pdn_connection_signalling_indication => 0,
                  pending_subscription_change_indication => 0,
                  pgw_change_indication => 0,
                  pgw_redirection_due_to_mismatch_with_network_slice_subscribed_by_ue_support_indication => 0,
                  piggybacking_supported => 0,ms_validated => 0,
                  propagate_bbai_information_change => 0,
                  release_over_any_access_indication => 0,
                  restoration_of_pdn_connections_after_an_pgw_c_smf_change_support_indication => 0,
                  retrieve_location_indication_flag => 0,
                  return_preferred_indication => 0,
                  s5s8_protocol_type => 0,
                  same_iwk_scef_selected_for_monitoring_event_indication => 0,
                  satellite_rat_type_reporting_to_pgw_indication => 0,
                  scope_indication => 0,
                  sgw_change_indication => 1,
                  sgw_restoration_needed_indication => 0,
                  static_ipv4_address_flag => 0,
                  static_ipv6_address_flag => 0,
                  subscribed_qos_change_indication => 0,
                  triggering_sgsn_initiated_pdp_context_creation_modification_indication => 0,
                  ue_available_for_signaling_indication => 0,
                  ue_not_authorised_cause_code_support_indication => 0,
                  unauthenticated_imsi => 0,
                  user_plane_integrity_protection_support_indication => 0,
                  wlcp_pdn_connection_modification_support_indication => 0}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

forward_relocation_request_1_test() ->
    Bin = <<"4885032E00000000000001000100080042829978563412F1570009008E00025A9C"
            "9C17040C6D008F0047001E000A45564552595748455245066D6E63303032066D63"
            "6332353504677072737F000100FF4A0004000A014A8149000100055700090087B9"
            "1CEFFA95FEC6F25D003E0049000100055700090081000000970A667FA157000901"
            "85B918EFFA95FEC6F2500016006C08000000000000000000000000000000000000"
            "00008900010080480008000000EA600000EA60570009018B061000A9AC1316C988"
            "00180005544F504F4E03533131035347570955475730303157564E6B00E3009E02"
            "A200000A00002A8DF90D5F6DC051386F9A0DA929E701F6D1BE424757ACDE711680"
            "9902010F479C0A0031FEAEDEA4CA074F7ABBA12D72041863F72468C44B0700307C"
            "05572B8832B7F203000186A0000186A00000EA600000EA6004E0E0C04003E58024"
            "0831751131713272230001020000010202FFFF0000660034001E0A455645525957"
            "48455245066D6E63303032066D6363323535046770727300001000000000010000"
            "8000E02B91CC10203040002E00180441504E31066D6E63303938066D6363323430"
            "0467707273000100000000001000080000E02B92CC112233440000760149000340"
            "81200A3027C5200004001041860244F8FFF13E3FFC4F8FFF13E3FFC4F8FFEFE1FF"
            "901103004870CA74A9020020F33035359A46014046F651021E2424031B1A53032A"
            "B71556621E40008DD8C63310F2000468C4B19887900022150002140328B2F00222"
            "F00C140140C7886119024284ADEC047E6700C1D8E047E6700C9D80FA81B4076154"
            "50742F3E82059967FF44AE618531B38020114270380E0A1A000120080C01102ABB"
            "400000091A0C2549E062C40303041210308F341C31186F212BA035869CE2D00102"
            "00554E772CB550971C581A0CA36220096102F6802BA91A85590980890C823DB6FB"
            "DA335DB730423DEB2F08BFE3C922F469FD65531BB0C02F6800C1232F2F2ED9B736"
            "E103140585E880EB3B112774DC1268C5E2CA1E35B7D7B6DF03B300004E40024500"
            "0052F52002F680110052F5200316201100000C0052F52002C0C02100001179000C"
            "000552F52016003123453A2A4F7700020000107800030052F520">>,


    Map = #{version => 2,
            message_group => mobility_management,
            piggy_backed => false,
            message_priority => false,
            message_type => forward_relocation_request,
            teid => 0,
            sequence_number => 1,
            imsi => "242899876543211",
            sender_f_teid =>
                #{interface_type => s3_sgsn_gtpc,
                  ipv4 => {156,23,4,12},
                  teid_gre_key => 154268},
            mmesgsnamf_ue_eps_pdn_connections =>
                #{apn => "EVERYWHERE.mnc002.mcc255.gprs",
                  apn_ambr => #{downlink => 60000,
                                uplink => 60000},
                  apn_restriction => 255,
                  bearer_contexts =>
                      #{bearer_level_qos =>
                            #{guaranteed_bitrate_downlink => 0,
                              guaranteed_bitrate_uplink => 0,
                              maximum_bitrate_downlink => 0,
                              maximum_bitrate_uplink => 0,
                              pre_emption_capability => false,
                              pre_emption_vulnerability => true,
                              priority_level => 11,
                              qci => 8},
                        eps_bearer_id => 5,
                        pgw_s5s8_ip_address_and_teid_for_user_plane =>
                            #{interface_type => s5s8_pgw_gtpu,
                              ipv4 => {149,254,198,242},
                              teid_gre_key => 3105419258},
                        sgw_s1s4s12_ip_address_and_teid_for_user_plane =>
                            #{interface_type => s1u_sgw_gtpu,
                              ipv4 => {10,102,127,161},
                              teid_gre_key => 151},
                        transaction_identifier => <<128>>},
                  ipv4_address => {10,1,74,129},
                  linked_eps_bearer_id => 5,
                  pgw_s5s8_ip_address_for_control_plane_or_pmip =>
                      #{interface_type => s5s8_pgw_gtpc,
                        ipv4 => {149,254,198,242},
                        teid_gre_key => 3105681402}},
            sgw_s11s4_f_teid =>
                #{interface_type => s11s4_sgw_gtpc,
                  ipv4 => {172,19,22,201},
                  teid_gre_key => 101712041},
            sgw_node_name => "TOPON.S11.SGW.UGW001WVN",
            mmesgsnamf_ue_mm_context =>
                #{type =>
                      mm_context_eps_security_context_quadruplets_and_quintuplets,
                  security_mode => 4,
                  ksi => 6,
                  used_nas_integrity_protection_algorithm => 2,
                  used_nas_cipher => 2,
                  nas_downlink_count => 10,
                  nas_uplink_count => 42,
                  kasme =>
                      binary:decode_hex(<<"8DF90D5F6DC051386F9A0DA929E701F6D1BE"
                                          "424757ACDE7116809902010F479C">>),
                  quadruplets => [],
                  quintuplets => [],
                  drx_parameter => 2560,
                  next_hop =>
                      binary:decode_hex(<<"31FEAEDEA4CA074F7ABBA12D72041863F724"
                                          "68C44B0700307C05572B8832B7F2">>),
                  next_hop_chaining_count => 3,
                  subscribed_ue_ambr => #{uplink => 100000,
                                          downlink => 100000},
                  used_ue_ambr => #{uplink => 60000,
                                    downlink => 60000},
                  ue_network_capability => <<2#1110_0000, 2#1110_0000, 2#1100_0000, 2#0100_0000>>,
                  ms_network_capability => <<2#1110_0101, 2#1000_0000, 2#0010_0100>>,
                  mei => #{imei => "135711131723273",
                           sv => "2"},
                  access_restriction_data =>
                      #{utran => allowed,
                        geran => allowed,
                        gan => allowed,
                        i_hspa_evolution => allowed,
                        wb_e_utran => allowed,
                        nb_iot => allowed,
                        enhanced_coverage => allowed,
                        ho_to_non_3gpp_access => allowed},
                  voice_domain_preference_and_ues_usage_setting => <<16#02>>,
                  ue_radio_capability_for_paging_information => <<>>,
                  extended_access_restriction_data =>
                      #{nr_u_in_5gs => allowed,
                        new_radio_unlicensed_as_secondary_rat => allowed,
                        nr_in_5gs => allowed,
                        unlicensed_spectrum_lwa_lwip_as_secondary_rat => not_allowed,
                        nr_as_secondary_rat => allowed},
                  ue_additional_security_capability => <<16#FF, 16#FF>>,
                  ue_nr_security_capability => <<>>,
                  apn_rate_control_statuses =>
                      [#{additional_exception_reports => 1,
                         apn => "EVERYWHERE.mnc002.mcc255.gprs",
                         downlink_packets_allowed => 32768,
                         uplink_packets_allowed => 4096,
                         validity_time => {{2019,3,7},{12,49,16}},
                         validity_time_fractions_raw => 270544960},
                       #{additional_exception_reports => 16,
                         apn => "APN1.mnc098.mcc240.gprs",
                         downlink_packets_allowed => 524288,
                         uplink_packets_allowed => 65536,
                         validity_time => {{2019,3,7},{12,53,32}},
                         validity_time_fractions_raw => 287454020}],
                  core_network_restrictions => <<>>,
                  ue_radio_capability_id => <<>>},
            e_utran_transparent_container =>
                #{eutran_transparent_container =>
                      binary:decode_hex(<<"4081200A3027C5200004001041860244F8FF"
                                          "F13E3FFC4F8FFF13E3FFC4F8FFEFE1FF9011"
                                          "03004870CA74A9020020F33035359A460140"
                                          "46F651021E2424031B1A53032AB71556621E"
                                          "40008DD8C63310F2000468C4B19887900022"
                                          "150002140328B2F00222F00C140140C78861"
                                          "19024284ADEC047E6700C1D8E047E6700C9D"
                                          "80FA81B407615450742F3E82059967FF44AE"
                                          "618531B38020114270380E0A1A000120080C"
                                          "01102ABB400000091A0C2549E062C4030304"
                                          "1210308F341C31186F212BA035869CE2D001"
                                          "0200554E772CB550971C581A0CA362200961"
                                          "02F6802BA91A85590980890C823DB6FBDA33"
                                          "5DB730423DEB2F08BFE3C922F469FD65531B"
                                          "B0C02F6800C1232F2F2ED9B736E103140585"
                                          "E880EB3B112774DC1268C5E2CA1E35B7D7B6"
                                          "DF03B300004E400245000052F52002F68011"
                                          "0052F5200316201100000C0052F52002C0C0"
                                          "21000011">>)},
            target_identification =>
                #{mcc => "255",
                  mnc => "02",
                  gnodeb_id => 3220293,
                  id_length => 22,
                  '5gs_tracking_area_code' => <<":*O">>},
            s1_ap_cause => #{radio_network_layer_cause => <<16>>},
            selected_plmn_id => #{mcc => "255",
                                  mnc => "02"}},
    ?assertEqual(62991157, otc_gtpv2c:fraction_to_ns(270544960)),
    ?assertEqual(66928104, otc_gtpv2c:fraction_to_ns(287454020)),
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

forward_relocation_request_2_test() ->
    Bin = <<"488503440000000000C156000100080042900901020304F5570009008C31EB70CD"
            "71D5828B6D00E30047001A0006706172726F74066D6E63303938066D6363323430"
            "046770727380000100004A0004000A25A331490001000557000900878CF7DFC075"
            "2E64F488004C0005746F706F6E0570677773351473656261737469772D69732D64"
            "61726B6E6573730474656C6503636F6D03657063066D6E63303938066D63633234"
            "300B336770706E6574776F726B036F72675D00460049000100055700090081C242"
            "32620AF0171857000901858CF7DFC0752E62EC500016007C090000000000000000"
            "00000000000000000000000076000400020708B4890001000048000800000124F8"
            "000493E057000901975870CF5EC0A8AB1888004C0005746F706F6E057367777335"
            "1473656261737469772D69732D6461726B6E6573730474656C6503636F6D036570"
            "63066D6E63303938066D63633234300B336770706E6574776F726B036F72676B00"
            "79009802A200002800002C166018D55F4272DCD6B8B74EA7BF1175A54B476074D9"
            "3BF1632201DF8787B9FC0A00FEC5322C68CE46CFDC6F2BA665B3CB9ED71A6FC2BB"
            "BB9D61097CD34A5D368CF700000124F8000493E0000124F8000493E004E060C040"
            "03E5E02408733272311171355100000000000000000000004D000A001000000000"
            "00000000007600DA00034080B10B102FC518004001072248244F93FFC4F93FFC4F"
            "93FFC4F93FFC4F93FFAE07EE40441401190A70CA74A9220205800000000015D800"
            "0000E13C200280255970811623A1B880D013E1B8800004011004C1037B01FFA9C0"
            "B81FFA9C083EA26A03D7538A1D884F9806EF32CD73F9563FF454D618721B228611"
            "2680B4000000A20082810464D9005040020103089846F8064ADFB24550AC073D00"
            "10000803C0A752ABCAF720074070A3CDB0215881040B8ED40000004E4002450000"
            "42F08926FAED410042F089326C80200000160042F08926FAED4180000079000A00"
            "0742F089836FAB1A30317700020000217800030042F08903000100857200020063"
            "005300030042F0899F000600035758A600004C000600740903660645CD00310042"
            "F0899876540C000000000000000000000000020000001400000000000000000000"
            "00000000000000000000040A0B0C0D">>,

    Map = #{version => 2,
            piggy_backed => false,
            message_priority => false,
            message_group => mobility_management,
            message_type => forward_relocation_request,
            teid => 0,
            sequence_number => 49494,
            imsi => "240990102030405",
            sender_f_teid =>
                #{interface_type => s10n26_mme_gtpc,
                  teid_gre_key => 837513421,
                  ipv4 => {113,213,130,139}},
            mmesgsnamf_ue_eps_pdn_connections =>
                #{apn => "parrot.mnc098.mcc240.gprs",
                  linked_eps_bearer_id => 5,
                  ipv4_address => {10,37,163,49},
                  selection_mode =>
                      #{provided_by => ms_or_network,
                        verified => true},
                  pgw_s5s8_ip_address_for_control_plane_or_pmip =>
                      #{interface_type => s5s8_pgw_gtpc,
                        ipv4 => {117,46,100,244},
                        teid_gre_key => 2365054912},
                  pgw_node_name =>
                      "topon.pgws5.sebastiw-is-darkness.tele.com.epc.mnc098.mcc240.3gppnetwork.org",
                  bearer_contexts =>
                      #{eps_bearer_id => 5,
                        sgw_s1s4s12_ip_address_and_teid_for_user_plane =>
                            #{interface_type => s1u_sgw_gtpu,
                              ipv4 => {10,240,23,24},
                              teid_gre_key => 3259118178},
                        pgw_s5s8_ip_address_and_teid_for_user_plane =>
                            #{interface_type => s5s8_pgw_gtpu,
                              ipv4 => {117,46,98,236},
                              teid_gre_key => 2365054912},
                        bearer_level_qos =>
                            #{guaranteed_bitrate_downlink => 0,
                              guaranteed_bitrate_uplink => 0,
                              maximum_bitrate_downlink => 0,
                              maximum_bitrate_uplink => 0,
                              pre_emption_capability => false,
                              pre_emption_vulnerability => true,
                              priority_level => 15,
                              qci => 9},
                        bss_container => #{bss_container => binary:decode_hex(<<"0708B4">>)},
                        transaction_identifier => <<0>>},
                  apn_ambr => #{downlink => 300000,
                                uplink => 75000}},
            sgw_s11s4_f_teid =>
                #{interface_type => sgw_upf_gtpu_dl_data_forwarding,
                  ipv4 => {192,168,171,24},
                  teid_gre_key => 1483788126},
            sgw_node_name =>
                "topon.sgws5.sebastiw-is-darkness.tele.com.epc.mnc098.mcc240.3gppnetwork.org",
            mmesgsnamf_ue_mm_context =>
                #{type =>
                      mm_context_eps_security_context_quadruplets_and_quintuplets,
                  security_mode => 4,
                  used_nas_integrity_protection_algorithm => 2,
                  used_nas_cipher => 2,
                  nas_downlink_count => 40,
                  nas_uplink_count => 44,
                  kasme =>
                      binary:decode_hex(<<"166018D55F4272DCD6B8B74EA7BF1175A54B"
                                          "476074D93BF1632201DF8787B9FC">>),
                  quadruplets => [],
                  quintuplets => [],
                  drx_parameter => 2560,
                  next_hop =>
                      binary:decode_hex(<<"FEC5322C68CE46CFDC6F2BA665B3CB9ED71A"
                                          "6FC2BBBB9D61097CD34A5D368CF7">>),
                  next_hop_chaining_count => 0,
                  subscribed_ue_ambr => #{uplink => 75000,
                                          downlink => 300000},
                  used_ue_ambr => #{uplink => 75000,
                                    downlink => 300000},
                  ue_network_capability => binary:decode_hex(<<"E060C040">>),
                  ms_network_capability => binary:decode_hex(<<"E5E024">>),
                  mei => #{imei => "372327131117531", sv => "5"},
                  access_restriction_data =>
                      #{enhanced_coverage => allowed,
                        gan => allowed,
                        geran => allowed,
                        ho_to_non_3gpp_access => allowed,
                        i_hspa_evolution => allowed,
                        nb_iot => allowed,
                        utran => allowed,
                        wb_e_utran => allowed},
                  apn_rate_control_statuses => [],
                  extended_access_restriction_data => #{},
                  core_network_restrictions => <<>>,
                  ue_radio_capability_id => <<>>,
                  ksi => 0,
                  ue_additional_security_capability => <<>>,
                  ue_nr_security_capability => <<>>,
                  ue_radio_capability_for_paging_information => <<>>,
                  voice_domain_preference_and_ues_usage_setting => <<>>},
            indication_flags =>
                #{management_based_mdt_allowed_flag => 0,
                  create_session_request_message_forwarded_indication => 0,
                  pdn_pause_support_indication => 0,
                  abnormal_release_of_radio_link => 0,
                  extended_ebi_value_range_support_indication => 0,
                  scope_indication => 0,
                  static_ipv4_address_flag => 0,
                  restoration_of_pdn_connections_after_an_pgw_c_smf_change_support_indication => 0,
                  change_f_teid_support_indication => 0,
                  s11_u_tunnel_flag => 0,
                  piggybacking_supported => 0,
                  change_of_presence_reporting_area_information_indication => 0,
                  associate_oci_with_pgw_nodes_identity => 0,
                  ue_available_for_signaling_indication => 0,
                  propagate_bbai_information_change => 0,
                  ms_validated => 0,
                  delay_tolerant_connection_indication => 0,
                  enb_change_reporting_support_indication => 0,
                  same_iwk_scef_selected_for_monitoring_event_indication => 0,
                  notify_start_pause_of_charging_via_user_plane_support_indication => 0,
                  dual_address_bearer_flag => 0,
                  '5gc_not_restricted_support' => 0,
                  cs_to_ps_srvcc_indication => 0,
                  pdn_pause_on_enabled_indication => 0,
                  extended_pco_support_indication => 0,
                  sgw_change_indication => 0,
                  pending_mt_short_message_indication => 0,
                  '5g_srvcc_ho_indication' => 0,
                  associate_oci_with_sgw_nodes_identity => 0,
                  operation_indication => 0,
                  handover_indication => 0,
                  '5gc_not_restricted_indication' => 0,
                  ue_not_authorised_cause_code_support_indication => 0,
                  idle_mode_signalling_reduction_activation_indication => 0,
                  direct_tunnel_flag => 0,
                  isr_is_activated_for_the_ue => 0,
                  sgw_restoration_needed_indication => 0,
                  return_preferred_indication => 0,
                  csg_change_reporting_support_indication => 0,
                  static_ipv6_address_flag => 0,
                  mt_edt_applicable => 0,
                  lte_m_satellite_access_indication => 0,
                  no_5gs_n26_mobility_indication => 0,
                  control_plane_only_pdn_connection_indication => 0,
                  notify_source_enodeb_indication => 0,
                  s5s8_protocol_type => 0,
                  csfb_indication => 0,
                  pending_subscription_change_indication => 0,
                  lte_m_rat_type_reporting_to_pgw_indication => 0,
                  p_cscf_restoration_indication => 0,
                  indirect_data_forwarding_with_upf_indication => 0,
                  direct_forwarding_indication => 1,
                  buffered_dl_data_waiting_indication => 0,
                  unauthenticated_imsi => 0,
                  ethernet_pdn_support_indication => 0,
                  change_reporting_support_indication => 0,
                  satellite_rat_type_reporting_to_pgw_indication => 0,
                  mt_edt_not_applicable => 0,
                  user_plane_integrity_protection_support_indication => 0,
                  '5gs_interworking_without_n26_indication' => 0,
                  wlcp_pdn_connection_modification_support_indication => 0,
                  nbifom_support_indication => 0,
                  change_of_location_information_indication => 0,
                  lte_m_ue_indication => 0,
                  idle_mode_signalling_reduction_supported_indication => 0,
                  pgw_change_indication => 0,
                  release_over_any_access_indication => 0,
                  pdn_pause_off_indication => 0,
                  '5gs_interworking_indication' => 0,
                  emergency_pdu_session_indication => 0,
                  subscribed_qos_change_indication => 0,
                  pgw_redirection_due_to_mismatch_with_network_slice_subscribed_by_ue_support_indication => 0,
                  retrieve_location_indication_flag => 0,
                  pending_network_initiated_pdn_connection_signalling_indication => 0,
                  triggering_sgsn_initiated_pdp_context_creation_modification_indication => 0},
            e_utran_transparent_container =>
                #{eutran_transparent_container =>
                      binary:decode_hex(<<"4080B10B102FC518004001072248244F93FF"
                                          "C4F93FFC4F93FFC4F93FFC4F93FFAE07EE40"
                                          "441401190A70CA74A9220205800000000015"
                                          "D8000000E13C200280255970811623A1B880"
                                          "D013E1B8800004011004C1037B01FFA9C0B8"
                                          "1FFA9C083EA26A03D7538A1D884F9806EF32"
                                          "CD73F9563FF454D618721B2286112680B400"
                                          "0000A20082810464D9005040020103089846"
                                          "F8064ADFB24550AC073D0010000803C0A752"
                                          "ABCAF720074070A3CDB0215881040B8ED400"
                                          "00004E400245000042F08926FAED410042F0"
                                          "89326C80200000160042F08926FAED418000"
                                          "00">>)
                 },
            target_identification =>
                #{'5gs_tracking_area_code' => <<26,48,49>>,
                  extended_ng_enodeb_id =>
                      #{short_macro_enodeb_id => <<219,234,3:2>>},
                  mcc => "240",
                  mnc => "98"},
            s1_ap_cause => #{radio_network_layer_cause => <<"!">>},
            selected_plmn_id => #{mcc => "240",
                                  mnc => "98"},
            serving_network => #{mcc => "240",
                                 mnc => "98"},
            additional_mm_context_for_srvcc =>
                #{mobile_station_classmark_2 => binary:decode_hex(<<"5758A6">>),
                  mobile_station_classmark_3 => <<>>,
                  supported_codec_list => <<>>},
            c_msisdn => "479030666054",
            recovery => 133,
            ue_time_zone =>
                #{daylight_saving_time => no_adjustment,
                  time_zone => 9*60},
            extended_trace_information =>
                #{ip_address_of_trace_collection_entity => binary:decode_hex(<<"0A0B0C0D">>),
                  list_interfaces =>
                      <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>,
                  list_ne_type => <<0,0>>,
                  mcc => "240",
                  mnc => "98",
                  session_trace_depth => 0,
                  trace_id => <<152,118,84>>,
                  triggering_events => <<0,0,0,0,0,0,0,0,0,0,0,0>>}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

identification_request_test() ->
    Bin = <<"4880005B0000000000D5F90075000A0042F089E28140E079F62D74003A00001744"
            "80FDEF180741120BF642F089E28140E079F62D04E060C04000050201D011D15244"
            "F002008A5C0A003103E5E0249011035758A65D0100E05300030042F089">>,
    Map = #{version => 2,
            piggy_backed => false,
            message_priority => false,
            message_group => mobility_management,
            message_type => identification_request,
            teid => 0,
            sequence_number => 54777,
            guti => #{mcc => "240",
                      mnc => "98",
                      m_tmsi => binary:decode_hex(<<"E079F62D">>),
                      mme_group_id => 57985,
                      mme_code => 64},
            complete_attach_request_message =>
                #{complete_attach_request_message =>
                      binary:decode_hex(<<"174480FDEF180741120BF642F089E28140E0"
                                          "79F62D04E060C04000050201D011D15244F0"
                                          "02008A5C0A003103E5E0249011035758A65D"
                                          "0100E0">>)},
            target_plmn_id => #{mcc => "240",
                                mnc => "98"}
           },
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

identification_response_test() ->
    Bin = <<"4881009C0000000000D5F9000200020010000100080042829978563412F16B007E"
            "008103A2000004000018224014064F109DC057BDDBE2DBDC37B6FECB4FD71AB176"
            "AA23BFFBCB7E2E4CCB000124F8000493E0000124F8000493E004E060C04003E5E0"
            "240873327231117135110008223014164F209DC067BDCBE2DBCC37B6FECB4FD71A"
            "B176BA23BFFBBB7E2E4CBB0000000103048F008F0002555500000000">>,

    Map = #{version => 2,
            piggy_backed => false,
            message_priority => false,
            message_group => mobility_management,
            message_type => identification_response,
            teid => 0,
            sequence_number => 54777,
            mmesgsn_ue_mm_context =>
                #{type =>
                      mm_context_eps_security_context_quadruplets_and_quintuplets,
                  security_mode => 4,
                  ksi => 1,
                  used_nas_integrity_protection_algorithm => 2,
                  used_nas_cipher => 2,
                  voice_domain_preference_and_ues_usage_setting =>
                      <<>>,
                  ue_radio_capability_for_paging_information =>
                      <<>>,
                  apn_rate_control_statuses => [],
                  nas_downlink_count => 4,
                  nas_uplink_count => 24,
                  kasme =>
                      binary:decode_hex(<<"224014064F109DC057BDDBE2DBDC37B6FECB"
                                          "4FD71AB176AA23BFFBCB7E2E4CCB">>),
                  quadruplets => [],
                  quintuplets => [],
                  subscribed_ue_ambr => #{uplink => 75000,
                                          downlink => 300000},
                  used_ue_ambr => #{uplink => 75000,
                                    downlink => 300000},
                  ue_network_capability => <<"à`À@">>,
                  ms_network_capability => <<"åà$">>,
                  mei => #{imei => "372327131117531", sv => "1"},
                  access_restriction_data =>
                      #{utran => allowed,
                        geran => allowed,
                        gan => allowed,
                        i_hspa_evolution => allowed,
                        wb_e_utran => allowed,
                        nb_iot => allowed,
                        enhanced_coverage => allowed,
                        ho_to_non_3gpp_access => allowed},
                  old_security_container =>
                      #{
                        kasme_old =>
                            binary:decode_hex(<<"223014164F209DC067BDCBE2DBCC37"
                                                "B6FECB4FD71AB176BA23BFFBBB7E2E"
                                                "4CBB">>),
                        ksi_old => 1,
                        ncc_old => 0,
                        rlos => 0},
                  extended_access_restriction_data =>
                      #{nr_u_in_5gs => allowed,
                        new_radio_unlicensed_as_secondary_rat =>
                            allowed,
                        nr_in_5gs => allowed,
                        unlicensed_spectrum_lwa_lwip_as_secondary_rat =>
                            not_allowed,
                        nr_as_secondary_rat => not_allowed},
                  ue_additional_security_capability =>
                      binary:decode_hex(<<"8F008F00">>),
                  ue_nr_security_capability => <<"UU">>,
                  core_network_restrictions => <<>>,
                  ue_radio_capability_id => <<>>
                 },
            imsi => "242899876543211",
            cause => #{cause => request_accepted,
                       cause_source => originated_by_remote_node}
           },
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

bearer_resource_command_test() ->
    Bin = <<"4844001E801C20079825E900490001000C640001000855000300A220114900010106">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => bearer_resource_command,
            teid => 2149326855,
            sequence_number => 9971177,
            piggy_backed => false,
            message_priority => false,
            linked_eps_bearer_id => 12,
            procedure_transaction_id => 8,
            traffic_aggregate_description => <<162,32,17>>,
            eps_bearer_id => 6},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

bearer_resource_failure_indication_test() ->
    Bin = <<"4845001CE49EBF329825E90002000600450049000000490001000C6400010008">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => bearer_resource_failure_indication,
            piggy_backed => false,
            message_priority => false,
            teid => 3835608882,
            sequence_number => 9971177,
            cause =>
                #{cause => mandatory_ie_incorrect,
                  cause_source => originated_by_remote_node,
                  offending_iei => 73,
                  offending_ie_instance => 0},
            linked_eps_bearer_id => 12,
            procedure_transaction_id => 8},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

create_bearer_request2_test() ->
    Bin = <<"485F00723BC9E2604A5E050049000100065D006100490001000054002900222030"
            "1110947AAD58FFFFFFFF301140BFE0508CE011311110947AAD58FFFFFFFF301140"
            "BFE0508CE057000901858AC0800554D89E03500016000801000000004000000000"
            "40000000004000000000405E000400016588CE">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => create_bearer_request,
            piggy_backed => false,
            message_priority => false,
            teid => 1003086432,
            sequence_number => 4873733,
            linked_eps_bearer_id => 6,
            bearer_contexts =>
                #{eps_bearer_id => 0,
                  tft =>
                      #{operation => create_new,
                        packet_filters =>
                            [#{direction => uplink,
                               identifier => 0,
                               evaluation_precedence => 48,
                               content =>
                                   [{remote, #{ipv4 => {148,122,173,88},
                                               mask => {255,255,255,255}}},
                                    {protocol_identifier, 17},
                                    {local, #{port => 49120}},
                                    {remote, #{port => 36064}}
                                   ]},
                             #{direction => downlink,
                               identifier => 1,
                               evaluation_precedence => 49,
                               content =>
                                   [{remote, #{ipv4 => {148,122,173,88},
                                               mask => {255,255,255,255}}},
                                    {protocol_identifier, 17},
                                    {local, #{port => 49120}},
                                    {remote, #{port => 36064}}
                                   ]}]},
                  s5s8u_pgw_f_teid =>
                      #{ipv4 => {84,216,158,3},
                        interface_type => s5s8_pgw_gtpu,
                        teid_gre_key => 2327871493},
                  bearer_level_qos =>
                      #{priority_level => 2,
                        pre_emption_capability => true,
                        pre_emption_vulnerability => true,
                        qci => 1,maximum_bitrate_uplink => 64,
                        maximum_bitrate_downlink => 64,
                        guaranteed_bitrate_uplink => 64,
                        guaranteed_bitrate_downlink => 64},
                  charging_id => <<1,101,136,206>>}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

create_bearer_response_test() ->
    Bin = <<"48600048861780054A5E05000200020010005D0025004900010007020002001000"
            "57000902843BC9E26AD994906F57000903858AC0800554D89E0356000D001842F2"
            "797BD542F279010F3801">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => create_bearer_response,
            piggy_backed => false,
            message_priority => false,
            teid => 2249687045,
            sequence_number => 4873733,
            uli =>
                #{tai =>
                      #{mcc => "242", mnc => "97",
                        tracking_area_code => 31701},
                  ecgi =>
                      #{mcc => "242", mnc => "97",
                        eutran_cell_identifier => 17774593}},
            bearer_contexts =>
                #{eps_bearer_id => 7,
                  cause =>
                      #{cause => request_accepted,
                        cause_source => originated_by_remote_node},
                  s5s8u_sgw_f_teid =>
                      #{ipv4 => {217,148,144,111},
                        teid_gre_key => 1003086442,
                        interface_type => s5s8_sgw_gtpu},
                  s5s8u_pgw_f_teid =>
                      #{ipv4 => {84,216,158,3},
                        teid_gre_key => 2327871493,
                        interface_type => s5s8_pgw_gtpu}},
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

delete_bearer_request_test() ->
    Bin = <<"4863000D3BC9E2601A8205004900010107">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => delete_bearer_request,
            piggy_backed => false,
            message_priority => false,
            teid => 1003086432,
            sequence_number => 1737221,
            eps_bearer_ids => 7},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

delete_bearer_response_test() ->
    Bin = <<"4864003C861780051A8205000200020010005D000B004900010007020002001000"
            "72000200400056000D001842F2797BD542F27901210408AA000400E7978381">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => delete_bearer_response,
            piggy_backed => false,
            message_priority => false,
            teid => 2249687045,
            sequence_number => 1737221,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node},
            uli =>
                #{tai =>
                      #{mcc => "242", mnc => "97",
                        tracking_area_code => 31701},
                  ecgi =>
                      #{mcc => "242", mnc => "97",
                        eutran_cell_identifier => 18940936}},
            ue_time_zone =>
                #{daylight_saving_time => no_adjustment,
                  time_zone => 60},
            uli_timestamp => {{2023,2,15}, {16,14,57}},
            bearer_contexts =>
                #{eps_bearer_id => 7,
                  cause =>
                      #{cause => request_accepted,
                        cause_source => originated_by_remote_node}}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

failed_delete_bearer_command_test() ->
    Bin = <<"4842002F855DE00A92633F00560008001042F08901FFDA6FAA000400E7977F2D72"
            "000200400057000900860D2E3220D9AE45C0">>,
    ?assertThrow({error, {missing_mandatory_tlivs, [bearer_contexts]}},
                 otc:decapsulate(gtpv2c, binary:decode_hex(Bin))).

failed_delete_bearer_failure_indication_test() ->
    Bin = <<"484300120D2E322092633F000200060046005D000000">>,
    ?assertThrow({error, {missing_mandatory_tlivs, [bearer_context]}},
                 otc:decapsulate(gtpv2c, binary:decode_hex(Bin))).

suspend_notification_test() ->
    Bin = <<"48A200088159000344ECB200">>,
    Map = #{version => 2,
            message_group => cs_fallback_and_srvcc_related,
            message_type => suspend_notification,
            message_priority => false,
            piggy_backed => false,
            teid => 2170093571,
            sequence_number => 4517042},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

suspend_acknowledgement_test() ->
    Bin = <<"48A3000E0256804044ECB200020002001000">>,
    Map = #{version => 2,
            message_group => cs_fallback_and_srvcc_related,
            message_type => suspend_acknowledge,
            piggy_backed => false,
            message_priority => false,
            teid => 39223360,
            sequence_number => 4517042,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

resume_notification_test() ->
    Bin = <<"48A400148159000344ECC4000100080042829978563412F1">>,
    Map = #{version => 2,
            message_group => cs_fallback_and_srvcc_related,
            message_type => resume_notification,
            piggy_backed => false,
            message_priority => false,
            teid => 2170093571,
            sequence_number => 4517060,
            imsi => "242899876543211"},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

resume_acknowledgement_test() ->
    Bin = <<"48A5000E0256804044ECC400020002001000">>,
    Map = #{version => 2,
            message_group => cs_fallback_and_srvcc_related,
            message_type => resume_acknowledge,
            piggy_backed => false,
            message_priority => false,
            teid => 39223360,
            sequence_number => 4517060,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

delete_session_request_test() ->
    Bin = <<"482400338154C002311C7C00490001000656000D001842F089859F42F06904CAE3"
            "0157000900862DCB0310D9AE45C0AA000400E7977ECD">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => delete_session_request,
            piggy_backed => false,
            message_priority => false,
            teid => 2169815042,
            sequence_number => 3218556,
            linked_eps_bearer_id => 6,
            uli =>
                #{tai =>
                      #{mcc => "240", mnc => "98",
                        tracking_area_code => 34207},
                  ecgi =>
                      #{mcc => "240", mnc => "96",
                        eutran_cell_identifier => 80405249}},
            uli_timestamp => {{2023,2,15}, {15,54,53}},
            sender_f_teid =>
                #{ipv4 => {217,174,69,192},
                  teid_gre_key => 768279312,
                  interface_type => s5s8_sgw_gtpc}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

delete_session_response_test() ->
    Bin = <<"4825000e2dcb0310311c7c00020002001000">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => delete_session_response,
            piggy_backed => false,
            message_priority => false,
            teid => 768279312,
            sequence_number => 3218556,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

echo_request_test() ->
    Bin = <<"4001000900000000030001001A">>,
    Map = #{version => 2,
            message_group => path_management,
            message_type => echo_request,
            sequence_number => 0,
            piggy_backed => false,
            message_priority => false,
            recovery => 26},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

echo_response_test() ->
    Bin = <<"4002000900000000030001000A">>,
    Map = #{version => 2,
            message_group => path_management,
            message_type => echo_response,
            sequence_number => 0,
            piggy_backed => false,
            message_priority => false,
            recovery => 10},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

modify_bearer_request_test() ->
    Bin = <<"4822007985118009400E91004B000800317511317132726356000D001842F08986"
            "C742F069050CB4165300030042F08952000100064D000A00001000080000000000"
            "00570009008613334440D9AE45C048000800000021C0000021C05D001200490001"
            "0005570009018413334445D9AE45CA0300010016720002004000">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => modify_bearer_request,
            piggy_backed => false,
            message_priority => false,
            teid => 2232516617,
            sequence_number => 4198033,
            mei => #{imei => "135711131723273", sv => "6"},
            rat_type => eutran_wb_eutran,
            uli =>
                #{tai =>
                      #{mcc => "240", mnc => "98",
                        tracking_area_code => 34503},
                  ecgi =>
                      #{mcc => "240", mnc => "96",
                        eutran_cell_identifier => 84718614}},
            serving_network => #{mcc => "240", mnc => "98"},
            indication_flags =>
                #{change_of_location_information_indication => 0,
                  operation_indication => 0,
                  associate_oci_with_pgw_nodes_identity => 0,
                  user_plane_integrity_protection_support_indication =>
                      0,
                  '5g_srvcc_ho_indication' => 0,
                  change_f_teid_support_indication => 0,
                  wlcp_pdn_connection_modification_support_indication =>
                      0,
                  idle_mode_signalling_reduction_supported_indication =>
                      0,
                  no_5gs_n26_mobility_indication => 0,
                  retrieve_location_indication_flag => 0,
                  restoration_of_pdn_connections_after_an_pgw_c_smf_change_support_indication =>
                      0,
                  satellite_rat_type_reporting_to_pgw_indication =>
                      0,
                  idle_mode_signalling_reduction_activation_indication =>
                      0,
                  isr_is_activated_for_the_ue => 0,
                  notify_start_pause_of_charging_via_user_plane_support_indication =>
                      0,
                  abnormal_release_of_radio_link => 0,
                  direct_forwarding_indication => 0,
                  release_over_any_access_indication => 0,
                  csfb_indication => 0,
                  indirect_data_forwarding_with_upf_indication =>
                      0,
                  scope_indication => 0,
                  triggering_sgsn_initiated_pdp_context_creation_modification_indication =>
                      0,
                  cs_to_ps_srvcc_indication => 0,
                  sgw_restoration_needed_indication => 0,
                  pending_mt_short_message_indication => 0,
                  sgw_change_indication => 0,
                  mt_edt_applicable => 0,lte_m_ue_indication => 0,
                  propagate_bbai_information_change => 0,
                  ms_validated => 0,
                  change_reporting_support_indication => 1,
                  lte_m_rat_type_reporting_to_pgw_indication => 0,
                  lte_m_satellite_access_indication => 0,
                  pdn_pause_off_indication => 0,
                  extended_pco_support_indication => 0,
                  control_plane_only_pdn_connection_indication =>
                      0,
                  '5gc_not_restricted_indication' => 0,
                  same_iwk_scef_selected_for_monitoring_event_indication =>
                      0,
                  handover_indication => 0,
                  '5gs_interworking_indication' => 0,
                  pgw_redirection_due_to_mismatch_with_network_slice_subscribed_by_ue_support_indication =>
                      0,
                  csg_change_reporting_support_indication => 0,
                  direct_tunnel_flag => 0,
                  mt_edt_not_applicable => 0,
                  static_ipv4_address_flag => 0,
                  s5s8_protocol_type => 0,
                  static_ipv6_address_flag => 0,
                  p_cscf_restoration_indication => 0,
                  s11_u_tunnel_flag => 0,
                  dual_address_bearer_flag => 0,
                  pdn_pause_support_indication => 1,
                  delay_tolerant_connection_indication => 0,
                  pending_network_initiated_pdn_connection_signalling_indication =>
                      0,
                  ue_available_for_signaling_indication => 0,
                  associate_oci_with_sgw_nodes_identity => 0,
                  return_preferred_indication => 0,
                  '5gc_not_restricted_support' => 0,
                  management_based_mdt_allowed_flag => 0,
                  notify_source_enodeb_indication => 0,
                  emergency_pdu_session_indication => 0,
                  change_of_presence_reporting_area_information_indication =>
                      0,
                  pdn_pause_on_enabled_indication => 0,
                  ethernet_pdn_support_indication => 0,
                  unauthenticated_imsi => 0,
                  enb_change_reporting_support_indication => 0,
                  '5gs_interworking_without_n26_indication' => 0,
                  extended_ebi_value_range_support_indication => 0,
                  create_session_request_message_forwarded_indication =>
                      0,
                  piggybacking_supported => 0,
                  subscribed_qos_change_indication => 0,
                  nbifom_support_indication => 0,
                  buffered_dl_data_waiting_indication => 0,
                  ue_not_authorised_cause_code_support_indication =>
                      0,
                  pgw_change_indication => 0,
                  pending_subscription_change_indication => 0},
            sender_f_teid =>
                #{ipv4 => {217,174,69,192},
                  teid_gre_key => 322126912,
                  interface_type => s5s8_sgw_gtpc},
            apn_ambr => #{downlink => 8640,
                          uplink => 8640},
            recovery => 22,
            ue_time_zone =>
                #{daylight_saving_time => no_adjustment,
                  time_zone => 60},
            bearer_contexts_to_be_modified =>
                #{eps_bearer_id => 5,
                  s5s8u_sgw_f_teid =>
                      #{ipv4 => {217,174,69,202},
                        teid_gre_key => 322126917,
                        interface_type => s5s8_sgw_gtpu}}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

modify_bearer_response_test() ->
    Bin = <<"4823003913334440400E91000200020010004C0006006427851633F74900010005"
            "7F000100005D00130049000100050200020010005E00040002F9FCEB">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => modify_bearer_response,
            piggy_backed => false,
            message_priority => false,
            teid => 322126912,
            sequence_number => 4198033,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node},
            msisdn => "46725861337",
            linked_eps_bearer_id => 5,
            apn_restriction => 0,
            bearer_contexts_modified =>
                #{cause =>
                      #{cause => request_accepted,
                        cause_source => originated_by_remote_node},
                  eps_bearer_id => 5,
                  charging_id => <<2,249,252,235>>}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

modify_bearer_command_test() ->
    Bin = <<"4840004485118009C00E920048000800000493E0000493E05D001F004900010005"
            "500016005809000000000000000000000000000000000000000057000900861333"
            "4440D9AE45C0">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => modify_bearer_command,
            piggy_backed => false,
            message_priority => false,
            teid => 2232516617,
            sequence_number => 12586642,
            apn_ambr => #{downlink => 300000,
                          uplink => 300000},
            bearer_context =>
                #{eps_bearer_id => 5,
                  bearer_level_qos =>
                      #{guaranteed_bitrate_downlink => 0,
                        guaranteed_bitrate_uplink => 0,
                        maximum_bitrate_downlink => 0,
                        maximum_bitrate_uplink => 0,
                        pre_emption_capability => false,
                        pre_emption_vulnerability => true,
                        priority_level => 6,
                        qci => 9}},
            sender_f_teid =>
                #{ipv4 => {217,174,69,192},
                  teid_gre_key => 322126912,
                  interface_type => s5s8_sgw_gtpc}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

update_bearer_request_test() ->
    Bin = <<"4861001D13334440C00E92005D000500490001000548000800000493E0000493E0">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => update_bearer_request,
            piggy_backed => false,
            message_priority => false,
            teid => 322126912,
            sequence_number => 12586642,
            bearer_contexts => #{eps_bearer_id => 5},
            apn_ambr => #{downlink => 300000,
                          uplink => 300000}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

update_bearer_response_test() ->
    Bin = <<"4862002E85118009C00E92000200020010005D000B004900010005020002001000"
            "56000D001842F08986C742F069050CB416">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => update_bearer_response,
            piggy_backed => false,
            message_priority => false,
            teid => 2232516617,
            sequence_number => 12586642,
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node},
            uli =>
                #{tai =>
                      #{mcc => "240", mnc => "98",
                        tracking_area_code => 34503},
                  ecgi =>
                      #{mcc => "240", mnc => "96",
                        eutran_cell_identifier => 84718614}},
            bearer_contexts =>
                #{cause =>
                      #{cause => request_accepted,
                        cause_source => originated_by_remote_node},
                  eps_bearer_id => 5}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

modify_bearer_command2_test() ->
    Bin = <<"4840003788AF2003DF41860048000800000493E0000493E05D001F004900010005"
            "5000160058090000000000000000000000000000000000000000">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => modify_bearer_command,
            piggy_backed => false,
            message_priority => false,
            teid => 2293178371,
            sequence_number => 14631302,
            apn_ambr => #{downlink => 300000,
                          uplink => 300000},
            bearer_context =>
                #{eps_bearer_id => 5,
                  bearer_level_qos =>
                      #{guaranteed_bitrate_downlink => 0,
                        guaranteed_bitrate_uplink => 0,
                        maximum_bitrate_downlink => 0,
                        maximum_bitrate_uplink => 0,
                        pre_emption_capability => false,
                        pre_emption_vulnerability => true,
                        priority_level => 6,
                        qci => 9}}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

modify_bearer_failure_indication_test() ->
    Bin = <<"4841000e00000000df418600020002004000">>,
    Map = #{version => 2,
            message_group => tunnel_management,
            message_type => modify_bearer_failure_indication,
            piggy_backed => false,
            message_priority => false,
            teid => 0,
            sequence_number => 14631302,
            cause =>
                #{cause => context_not_found,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg).

change_notification_request_test() ->
    Bin = <<"4826005A8701000A000064000100080042900901020304F54B0008003175113171"
            "327223520001000656000D001842F27911C042F2790D9B19FC4900010005C9001B"
            "00030005E680965CE680966100000000000000000000000000000004">>,
    Map = #{version => 2,
            message_group => mobility_management,
            message_type => change_notification_request,
            piggy_backed => false,
            message_priority => false,
            teid => 2264989706,
            sequence_number => 100,
            imsi => "240990102030405",
            mei => #{imei => "135711131723273", sv => "2"},
            rat_type => eutran_wb_eutran,
            uli =>
                #{tai =>
                      #{mcc => "242", mnc => "97",
                        tracking_area_code => 4544},
                  ecgi =>
                      #{mcc => "242", mnc => "97",
                        eutran_cell_identifier => 228268540}},
            secondary_rat_usage_data_report =>
                #{eps_bearer_id => 5,
                  intended_receiver_pgw => forward,
                  intended_receiver_sgw => store,
                  secondary_rat_type => nr,
                  end_timestamp => {{2022,7,19}, {2,33,5}},
                  start_timestamp => {{2022,7,19}, {2,33,0}},
                  usage_data_dl => 0,
                  usage_data_ul => 4},
            lbi => 5},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

change_notification_response_test() ->
    Bin = <<"4827001A00000000000064000100080042900901020304F5020002001000">>,
    Map = #{version => 2,
            message_group => mobility_management,
            message_type => change_notification_response,
            piggy_backed => false,
            message_priority => false,
            teid => 0,
            sequence_number => 100,
            imsi => "240990102030405",
            cause =>
                #{cause => request_accepted,
                  cause_source => originated_by_remote_node}},
    Msg = otc_gtpv2c:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_gtpv2c:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

%% -----------------------------------------------------------------------------

aton(IPString) ->
    {ok, A} = inet:parse_address(IPString),
    A.

