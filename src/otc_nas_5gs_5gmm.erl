-module(otc_nas_5gs_5gmm).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1]).

spec() ->
    "3GPP TS 24.501 version 16.10.0".

codec({SecurityHeaderType, Bin}) when is_binary(Bin) ->
    decode({SecurityHeaderType, Bin});
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) -> {ok, nas_5gs}.

decode({plain_5gs_nas_message, <<MT:8/big, OIE/binary>>}) ->
    MsgType = otc_nas_eps:parse_msg_type(MT),
    Msg = decode_5gmm_msg(MsgType, OIE),
    Msg#{message_type => MsgType};
decode({service_request, Bin}) ->
    Msg = decode_5gmm_msg(service_request, Bin),
    Msg#{message_type => service_request};
decode({_SHT, <<MAC:4/binary, SN:1/binary, NMSG/binary>>}) ->
    {#{message_authentication_code => MAC,
       sequence_number => SN
      }, NMSG}.

encode(#{message_authentication_code := MAC, sequence_number := SN}) ->
    <<MAC:4/binary, SN:1/binary>>;
encode(#{message_type := service_request} = Msg) ->
    encode_5gmm_msg(service_request, Msg);
encode(#{message_type := MsgType} = Msg) ->
    Bin = encode_5gmm_msg(MsgType, Msg),
    MT = otc_nas_5gs:compose_msg_type(MsgType),
    <<MT:8/big, Bin/binary>>.

decode_5gmm_msg(authentication_request, Bin0) ->
    {Ngksi, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {Abba, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{authentication_parameter_rand_5g_authentication_challenge, 16#21, tv, 17},
            {authentication_parameter_autn_5g_authentication_challenge, 16#20, tlv, 18},
            {eap_message, 16#78, tlve, {7, 150}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{ngksi => Ngksi,
               abba => Abba
              };
decode_5gmm_msg(authentication_response, Bin0) ->
    Opts = [{authentication_response_parameter, 16#2D, tlv, 18},
            {eap_message, 16#78, tlve, {7, 1503}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(authentication_result, Bin0) ->
    {Ngksi, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {EapMessage, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [{abba, 16#38, tlv, {4, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{ngksi => Ngksi,
               eap_message => EapMessage
              };
decode_5gmm_msg(authentication_failure, Bin0) ->
    {MmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(authentication_reject, Bin0) ->
    Opts = [{eap_message, 16#78, tlve, {7, 1503}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(registration_request, Bin0) ->
    {SRegistrationType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {SMobileIdentity, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [{non_current_native_nas_key_set_identifier, 16#C, tv, 1},
            {'5gmm_capability', 16#10, tlv, {3, 15}},
            {ue_security_capability, 16#2E, tlv, {4, 10}},
            {requested_nssai, 16#2F, tlv, {4, 74}},
            {last_visited_registered_tai, 16#52, tv, 7},
            {s1_ue_network_capability, 16#17, tlv, {4, 15}},
            {uplink_data_status, 16#40, tlv, {4, 34}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {mico_indication, 16#B, tv, 1},
            {ue_status, 16#2B, tlv, 3},
            {additional_guti, 16#77, tlve, 14},
            {allowed_pdu_session_status, 16#25, tlv, {4, 34}},
            {ues_usage_setting, 16#18, tlv, 3},
            {requested_drx_parameters, 16#51, tlv, 3},
            {eps_nas_message_container, 16#70, tlve, {4, n}},
            {ladn_indication, 16#74, tlve, {3, 811}},
            {payload_container_type, 16#8, tv, 1},
            {payload_container, 16#7B, tlve, {4, 65538}},
            {network_slicing_indication, 16#9, tv, 1},
            {'5gs_update_type', 16#53, tlv, 3},
            {mobile_station_classmark_2, 16#41, tlv, 5},
            {supported_codecs, 16#42, tlv, {5, n}},
            {nas_message_container, 16#71, tlve, {4, n}},
            {eps_bearer_context_status, 16#60, tlv, 4},
            {requested_extended_drx_parameters, 16#6E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {requested_mapped_nssai, 16#35, tlv, {3, 42}},
            {additional_information_requested, 16#48, tlv, 3},
            {requested_wus_assistance_information, 16#1A, tlv, {3, n}},
            {n5gc_indication, 16#A, t, 1},
            {requested_nb_n1_mode_drx_parameters, 16#30, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{'5gs_registration_type' => SRegistrationType,
               ngksi => Ngksi,
               '5gs_mobile_identity' => SMobileIdentity
              };
decode_5gmm_msg(registration_accept, Bin0) ->
    {SRegistrationResult, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [{'5g_guti', 16#77, tlve, 14},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {tai_list, 16#54, tlv, {9, 114}},
            {allowed_nssai, 16#15, tlv, {4, 74}},
            {rejected_nssai, 16#11, tlv, {4, 42}},
            {configured_nssai, 16#31, tlv, {4, 146}},
            {'5gs_network_feature_support', 16#21, tlv, {3, 5}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {pdu_session_reactivation_result, 16#26, tlv, {4, 34}},
            {pdu_session_reactivation_result_error_cause, 16#72, tlve, {5, 515}},
            {ladn_information, 16#79, tlve, {12, 1715}},
            {mico_indication, 16#B, tv, 1},
            {network_slicing_indication, 16#9, tv, 1},
            {service_area_list, 16#27, tlv, {6, 114}},
            {t3512_value, 16#5E, tlv, 3},
            {non_3gpp_de_registration_timer_value, 16#5D, tlv, 3},
            {t3502_value, 16#16, tlv, 3},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {sor_transparent_container, 16#73, tlve, {20, n}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {nssai_inclusion_mode, 16#A, tv, 1},
            {operator_defined_access_category_definitions, 16#76, tlve, {3, n}},
            {negotiated_drx_parameters, 16#51, tlv, 3},
            {non_3gpp_nw_policies, 16#D, tv, 1},
            {eps_bearer_context_status, 16#60, tlv, 4},
            {negotiated_extended_drx_parameters, 16#6E, tlv, 3},
            {t3447_value, 16#6C, tlv, 3},
            {t3448_value, 16#6B, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#E, tv, 1},
            {pending_nssai, 16#39, tlv, {4, 146}},
            {ciphering_key_data, 16#74, tlve, {34, n}},
            {cag_information_list, 16#75, tlve, {3, n}},
            {truncated_5g_s_tmsi_configuration, 16#1B, tlv, 3},
            {negotiated_wus_assistance_information, 16#1C, tlv, {3, n}},
            {negotiated_nb_n1_mode_drx_parameters, 16#29, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gs_registration_result' => SRegistrationResult
              };
decode_5gmm_msg(registration_complete, Bin0) ->
    Opts = [{sor_transparent_container, 16#73, tlve, 20}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(registration_reject, Bin0) ->
    {MmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3502_value, 16#16, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {rejected_nssai, 16#69, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(ul_nas_transport, Bin0) ->
    {PayloadContainerType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {PayloadContainer, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [{old_pdu_session_id, 16#59, tv, 2},
            {request_type, 16#8, tv, 1},
            {s_nssai, 16#22, tlv, {3, 10}},
            {dnn, 16#25, tlv, {3, 102}},
            {additional_information, 16#24, tlv, {3, n}},
            {ma_pdu_session_information, 16#A, tv, 1},
            {release_assistance_indication, 16#F, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{payload_container_type => PayloadContainerType,
               payload_container => PayloadContainer
              };
decode_5gmm_msg(dl_nas_transport, Bin0) ->
    {PayloadContainerType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {PayloadContainer, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [{additional_information, 16#24, tlv, {3, n}},
            {'5gmm_cause', 16#58, tv, 2},
            {back_off_timer_value, 16#37, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{payload_container_type => PayloadContainerType,
               payload_container => PayloadContainer
              };
decode_5gmm_msg(deregistration_request_ue_originating_deregistration, Bin0) ->
    {DeRegistrationType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {SMobileIdentity, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{de_registration_type => DeRegistrationType,
               ngksi => Ngksi,
               '5gs_mobile_identity' => SMobileIdentity
              };
decode_5gmm_msg(deregistration_accept_ue_originating_deregistration, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(deregistration_request_ue_terminated_deregistration, Bin0) ->
    {DeRegistrationType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{'5gmm_cause', 16#58, tv, 2},
            {t3346_value, 16#5F, tlv, 3},
            {rejected_nssai, 16#6D, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{de_registration_type => DeRegistrationType
              };
decode_5gmm_msg(deregistration_accept_ue_terminated_deregistration, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(service_request, Bin0) ->
    {Ngksi, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {ServiceType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {STmsi, Bin3} = otc_l3_codec:decode_lve(Bin2),
    Opts = [{uplink_data_status, 16#40, tlv, {4, 34}},
            {allowed_pdu_session_status, 16#25, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{ngksi => Ngksi,
               service_type => ServiceType,
               '5g_s_tmsi' => STmsi
              };
decode_5gmm_msg(service_accept, Bin0) ->
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {pdu_session_reactivation_result, 16#26, tlv, {4, 34}},
            {pdu_session_reactivation_result_error_cause, 16#72, tlve, {5, 515}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(service_reject, Bin0) ->
    {MmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {t3346_value, 16#5F, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(configuration_update_command, Bin0) ->
    Opts = [{configuration_update_indication, 16#D, tv, 1},
            {'5g_guti', 16#77, tlve, 14},
            {tai_list, 16#54, tlv, {9, 114}},
            {allowed_nssai, 16#15, tlv, {4, 74}},
            {service_area_list, 16#27, tlv, {6, 114}},
            {full_name_for_network, 16#43, tlv, {3, n}},
            {short_name_for_network, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_local_time_zone, 16#47, tv, 8},
            {network_daylight_saving_time, 16#49, tlv, 3},
            {ladn_information, 16#79, tlve, {3, 1715}},
            {mico_indication, 16#B, tv, 1},
            {network_slicing_indication, 16#9, tv, 1},
            {configured_nssai, 16#31, tlv, {4, 146}},
            {rejected_nssai, 16#11, tlv, {4, 42}},
            {operator_defined_access_category_definitions, 16#76, tlve, {3, n}},
            {sms_indication, 16#F, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {cag_information_list, 16#75, tlve, {3, n}},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#A, tv, 1},
            {'5gs_registration_result', 16#44, tlv, 3},
            {truncated_5g_s_tmsi_configuration, 16#1B, tlv, 3},
            {additional_configuration_indication, 16#C, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(configuration_update_complete, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(identity_request, Bin0) ->
    {IdentityType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{identity_type => IdentityType
              };
decode_5gmm_msg(identity_respones, Bin0) ->
    {MobileIdentity, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{mobile_identity => MobileIdentity
              };
decode_5gmm_msg(notification, Bin0) ->
    {AccessType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{access_type => AccessType
              };
decode_5gmm_msg(notification_response, Bin0) ->
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(security_mode_command, Bin0) ->
    {SelectedNasSecurityAlgorithms, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {Ngksi, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = otc_l3_codec:decode_v(Bin2, half),
    {ReplayedUeSecurityCapabilities, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{imeisv_request, 16#E, tv, 1},
            {selected_eps_nas_security_algorithms, 16#57, tv, 2},
            {additional_5g_security_information, 16#36, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {abba, 16#38, tlv, {4, n}},
            {replayed_s1_ue_security_capabilities, 16#19, tlv, {4, 7}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{selected_nas_security_algorithms => SelectedNasSecurityAlgorithms,
               ngksi => Ngksi,
               replayed_ue_security_capabilities => ReplayedUeSecurityCapabilities
              };
decode_5gmm_msg(security_mode_complete, Bin0) ->
    Opts = [{imeisv, 16#77, tlve, 12},
            {nas_message_container, 16#71, tlve, {4, n}},
            {non_imeisv_pei, 16#78, tlve, {7, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gmm_msg(security_mode_reject, Bin0) ->
    {MmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(security_protected_5gs_nas_message, Bin0) ->
    {MessageAuthenticationCode, Bin1} = otc_l3_codec:decode_v(Bin0, 4),
    {SequenceNumber, Bin2} = otc_l3_codec:decode_v(Bin1, 1),
    {Plain5gsNasMessage, Bin3} = otc_l3_codec:decode_v(Bin2, {3, n}),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_authentication_code => MessageAuthenticationCode,
               sequence_number => SequenceNumber,
               plain_5gs_nas_message => Plain5gsNasMessage
              };
decode_5gmm_msg('5gmm_status', Bin0) ->
    {MmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(control_plane_service_request, Bin0) ->
    {ControlPlaneServiceType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{ciot_small_data_container, 16#6F, tlv, {4, 257}},
            {payload_container_type, 16#8, tv, 1},
            {payload_container, 16#7B, tlve, {4, 65538}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {release_assistance_indication, 16#F, tv, 1},
            {uplink_data_status, 16#40, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}},
            {additional_information, 16#24, tlv, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{control_plane_service_type => ControlPlaneServiceType,
               ngksi => Ngksi
              };
decode_5gmm_msg(network_slice_specific_authentication_command, Bin0) ->
    {SNssai, Bin1} = otc_l3_codec:decode_lv(Bin0),
    {EapMessage, Bin2} = otc_l3_codec:decode_lve(Bin1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              };
decode_5gmm_msg(network_slice_specific_authentication_complete, Bin0) ->
    {SNssai, Bin1} = otc_l3_codec:decode_lv(Bin0),
    {EapMessage, Bin2} = otc_l3_codec:decode_lve(Bin1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              };
decode_5gmm_msg(network_slice_specific_authentication_result, Bin0) ->
    {SNssai, Bin1} = otc_l3_codec:decode_lv(Bin0),
    {EapMessage, Bin2} = otc_l3_codec:decode_lve(Bin1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              }.

encode_5gmm_msg(authentication_request, #{ngksi := Ngksi, abba := Abba} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(Abba, <<>>),
    Opts = [{authentication_parameter_rand_5g_authentication_challenge, 16#21, tv, 17},
            {authentication_parameter_autn_5g_authentication_challenge, 16#20, tlv, 18},
            {eap_message, 16#78, tlve, {7, 150}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(authentication_response, #{} = Msg) ->
    Opts = [{authentication_response_parameter, 16#2D, tlv, 18},
            {eap_message, 16#78, tlve, {7, 1503}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(authentication_result, #{ngksi := Ngksi,
                                         eap_message := EapMessage} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [{abba, 16#38, tlv, {4, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(authentication_failure, #{'5gmm_cause' := MmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MmCause, 1, <<>>),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(authentication_reject, #{} = Msg) ->
    Opts = [{eap_message, 16#78, tlve, {7, 1503}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(registration_request, #{'5gs_registration_type' := SRegistrationType,
                                        ngksi := Ngksi,
                                        '5gs_mobile_identity' := SMobileIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(SRegistrationType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(SMobileIdentity, <<>>),
    Opts = [{non_current_native_nas_key_set_identifier, 16#C, tv, 1},
            {'5gmm_capability', 16#10, tlv, {3, 15}},
            {ue_security_capability, 16#2E, tlv, {4, 10}},
            {requested_nssai, 16#2F, tlv, {4, 74}},
            {last_visited_registered_tai, 16#52, tv, 7},
            {s1_ue_network_capability, 16#17, tlv, {4, 15}},
            {uplink_data_status, 16#40, tlv, {4, 34}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {mico_indication, 16#B, tv, 1},
            {ue_status, 16#2B, tlv, 3},
            {additional_guti, 16#77, tlve, 14},
            {allowed_pdu_session_status, 16#25, tlv, {4, 34}},
            {ues_usage_setting, 16#18, tlv, 3},
            {requested_drx_parameters, 16#51, tlv, 3},
            {eps_nas_message_container, 16#70, tlve, {4, n}},
            {ladn_indication, 16#74, tlve, {3, 811}},
            {payload_container_type, 16#8, tv, 1},
            {payload_container, 16#7B, tlve, {4, 65538}},
            {network_slicing_indication, 16#9, tv, 1},
            {'5gs_update_type', 16#53, tlv, 3},
            {mobile_station_classmark_2, 16#41, tlv, 5},
            {supported_codecs, 16#42, tlv, {5, n}},
            {nas_message_container, 16#71, tlve, {4, n}},
            {eps_bearer_context_status, 16#60, tlv, 4},
            {requested_extended_drx_parameters, 16#6E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {requested_mapped_nssai, 16#35, tlv, {3, 42}},
            {additional_information_requested, 16#48, tlv, 3},
            {requested_wus_assistance_information, 16#1A, tlv, {3, n}},
            {n5gc_indication, 16#A, t, 1},
            {requested_nb_n1_mode_drx_parameters, 16#30, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(registration_accept, #{'5gs_registration_result' := SRegistrationResult} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(SRegistrationResult, <<>>),
    Opts = [{'5g_guti', 16#77, tlve, 14},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {tai_list, 16#54, tlv, {9, 114}},
            {allowed_nssai, 16#15, tlv, {4, 74}},
            {rejected_nssai, 16#11, tlv, {4, 42}},
            {configured_nssai, 16#31, tlv, {4, 146}},
            {'5gs_network_feature_support', 16#21, tlv, {3, 5}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {pdu_session_reactivation_result, 16#26, tlv, {4, 34}},
            {pdu_session_reactivation_result_error_cause, 16#72, tlve, {5, 515}},
            {ladn_information, 16#79, tlve, {12, 1715}},
            {mico_indication, 16#B, tv, 1},
            {network_slicing_indication, 16#9, tv, 1},
            {service_area_list, 16#27, tlv, {6, 114}},
            {t3512_value, 16#5E, tlv, 3},
            {non_3gpp_de_registration_timer_value, 16#5D, tlv, 3},
            {t3502_value, 16#16, tlv, 3},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {sor_transparent_container, 16#73, tlve, {20, n}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {nssai_inclusion_mode, 16#A, tv, 1},
            {operator_defined_access_category_definitions, 16#76, tlve, {3, n}},
            {negotiated_drx_parameters, 16#51, tlv, 3},
            {non_3gpp_nw_policies, 16#D, tv, 1},
            {eps_bearer_context_status, 16#60, tlv, 4},
            {negotiated_extended_drx_parameters, 16#6E, tlv, 3},
            {t3447_value, 16#6C, tlv, 3},
            {t3448_value, 16#6B, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#E, tv, 1},
            {pending_nssai, 16#39, tlv, {4, 146}},
            {ciphering_key_data, 16#74, tlve, {34, n}},
            {cag_information_list, 16#75, tlve, {3, n}},
            {truncated_5g_s_tmsi_configuration, 16#1B, tlv, 3},
            {negotiated_wus_assistance_information, 16#1C, tlv, {3, n}},
            {negotiated_nb_n1_mode_drx_parameters, 16#29, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(registration_complete, #{} = Msg) ->
    Opts = [{sor_transparent_container, 16#73, tlve, 20}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(registration_reject, #{'5gmm_cause' := MmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MmCause, 1, <<>>),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3502_value, 16#16, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {rejected_nssai, 16#69, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(ul_nas_transport, #{payload_container_type := PayloadContainerType,
                                    payload_container := PayloadContainer} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(PayloadContainerType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(PayloadContainer, <<>>),
    Opts = [{old_pdu_session_id, 16#59, tv, 2},
            {request_type, 16#8, tv, 1},
            {s_nssai, 16#22, tlv, {3, 10}},
            {dnn, 16#25, tlv, {3, 102}},
            {additional_information, 16#24, tlv, {3, n}},
            {ma_pdu_session_information, 16#A, tv, 1},
            {release_assistance_indication, 16#F, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(dl_nas_transport, #{payload_container_type := PayloadContainerType,
                                    payload_container := PayloadContainer} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(PayloadContainerType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(PayloadContainer, <<>>),
    Opts = [{additional_information, 16#24, tlv, {3, n}},
            {'5gmm_cause', 16#58, tv, 2},
            {back_off_timer_value, 16#37, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(deregistration_request_ue_originating_deregistration, #{de_registration_type := DeRegistrationType,
                                                                        ngksi := Ngksi,
                                                                        '5gs_mobile_identity' := SMobileIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(DeRegistrationType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(SMobileIdentity, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(deregistration_accept_ue_originating_deregistration, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(deregistration_request_ue_terminated_deregistration, #{de_registration_type := DeRegistrationType} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(DeRegistrationType, half, <<>>),
    Opts = [{'5gmm_cause', 16#58, tv, 2},
            {t3346_value, 16#5F, tlv, 3},
            {rejected_nssai, 16#6D, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(deregistration_accept_ue_terminated_deregistration, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(service_request, #{ngksi := Ngksi,
                                   service_type := ServiceType,
                                   '5g_s_tmsi' := STmsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(ServiceType, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(STmsi, <<>>),
    Opts = [{uplink_data_status, 16#40, tlv, {4, 34}},
            {allowed_pdu_session_status, 16#25, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg(service_accept, #{} = Msg) ->
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {pdu_session_reactivation_result, 16#26, tlv, {4, 34}},
            {pdu_session_reactivation_result_error_cause, 16#72, tlve, {5, 515}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(service_reject, #{'5gmm_cause' := MmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MmCause, 1, <<>>),
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {t3346_value, 16#5F, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3},
            {cag_information_list, 16#75, tlve, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(configuration_update_command, #{} = Msg) ->
    Opts = [{configuration_update_indication, 16#D, tv, 1},
            {'5g_guti', 16#77, tlve, 14},
            {tai_list, 16#54, tlv, {9, 114}},
            {allowed_nssai, 16#15, tlv, {4, 74}},
            {service_area_list, 16#27, tlv, {6, 114}},
            {full_name_for_network, 16#43, tlv, {3, n}},
            {short_name_for_network, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_local_time_zone, 16#47, tv, 8},
            {network_daylight_saving_time, 16#49, tlv, 3},
            {ladn_information, 16#79, tlve, {3, 1715}},
            {mico_indication, 16#B, tv, 1},
            {network_slicing_indication, 16#9, tv, 1},
            {configured_nssai, 16#31, tlv, {4, 146}},
            {rejected_nssai, 16#11, tlv, {4, 42}},
            {operator_defined_access_category_definitions, 16#76, tlve, {3, n}},
            {sms_indication, 16#F, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {cag_information_list, 16#75, tlve, {3, n}},
            {ue_radio_capability_id, 16#67, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#A, tv, 1},
            {'5gs_registration_result', 16#44, tlv, 3},
            {truncated_5g_s_tmsi_configuration, 16#1B, tlv, 3},
            {additional_configuration_indication, 16#C, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(configuration_update_complete, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(identity_request, #{identity_type := IdentityType} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(IdentityType, half, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(identity_respones, #{mobile_identity := MobileIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(MobileIdentity, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(notification, #{access_type := AccessType} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(AccessType, half, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(notification_response, #{} = Msg) ->
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(security_mode_command, #{selected_nas_security_algorithms := SelectedNasSecurityAlgorithms,
                                         ngksi := Ngksi,
                                         replayed_ue_security_capabilities := ReplayedUeSecurityCapabilities} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SelectedNasSecurityAlgorithms, 1, <<>>),
    Bin2 = <<0:4>>,  % spare_half_octet
    Bin3 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin4 = otc_l3_codec:encode_lv(ReplayedUeSecurityCapabilities, <<>>),
    Opts = [{imeisv_request, 16#E, tv, 1},
            {selected_eps_nas_security_algorithms, 16#57, tv, 2},
            {additional_5g_security_information, 16#36, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {abba, 16#38, tlv, {4, n}},
            {replayed_s1_ue_security_capabilities, 16#19, tlv, {4, 7}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_5gmm_msg(security_mode_complete, #{} = Msg) ->
    Opts = [{imeisv, 16#77, tlve, 12},
            {nas_message_container, 16#71, tlve, {4, n}},
            {non_imeisv_pei, 16#78, tlve, {7, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gmm_msg(security_mode_reject, #{'5gmm_cause' := MmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(security_protected_5gs_nas_message, #{message_authentication_code := MessageAuthenticationCode,
                                                      sequence_number := SequenceNumber,
                                                      plain_5gs_nas_message := Plain5gsNasMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MessageAuthenticationCode, 4, <<>>),
    Bin2 = otc_l3_codec:encode_v(SequenceNumber, 1, <<>>),
    Bin3 = otc_l3_codec:encode_v(Plain5gsNasMessage, {3, n}, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_5gmm_msg('5gmm_status', #{'5gmm_cause' := MmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gmm_msg(control_plane_service_request, #{control_plane_service_type := ControlPlaneServiceType,
                                                 ngksi := Ngksi} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(Ngksi, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(ControlPlaneServiceType, half, <<>>),
    Opts = [{ciot_small_data_container, 16#6F, tlv, {4, 257}},
            {payload_container_type, 16#8, tv, 1},
            {payload_container, 16#7B, tlve, {4, 65538}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {release_assistance_indication, 16#F, tv, 1},
            {uplink_data_status, 16#40, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}},
            {additional_information, 16#24, tlv, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(network_slicespecific_authentication_command, #{s_nssai := SNssai,
                                                                eap_message := EapMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(SNssai, <<>>),
    Bin2 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(network_slicespecific_authentication_complete, #{s_nssai := SNssai,
                                                                 eap_message := EapMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(SNssai, <<>>),
    Bin2 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_5gmm_msg(network_slicespecific_authentication_result, #{s_nssai := SNssai,
                                                               eap_message := EapMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(SNssai, <<>>),
    Bin2 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>.

