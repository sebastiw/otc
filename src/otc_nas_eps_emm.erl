-module(otc_nas_eps_emm).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         encode/1]).

spec() ->
    "3GPP TS 24.301 version 16.8.0".

codec({SecurityHeaderType, Bin}, _Opts) when is_atom(SecurityHeaderType), is_binary(Bin) ->
    decode({SecurityHeaderType, Bin});
codec(Map, _Opts) when is_map(Map) ->
    encode({Map, <<>>});
codec({Map, PDU}, _Opts) when is_map(Map) ->
    encode({Map, PDU}).

next(_) -> {ok, nas_eps}.

decode({plain_nas_message, <<MT:8/big, OIE/binary>>}) ->
    MsgType = otc_nas_eps:parse_msg_type(MT),
    Msg = decode_emm_msg(MsgType, OIE),
    Msg#{message_type => MsgType};
decode({service_request, Bin}) ->
    Msg = decode_emm_msg(service_request, Bin),
    Msg#{message_type => service_request};
decode({_SHT, <<MAC:4/binary, SN:1/binary, NMSG/binary>>}) ->
    {#{message_authentication_code => MAC,
       sequence_number => SN
      }, NMSG}.

encode({#{message_authentication_code := MAC, sequence_number := SN}, NMSG}) ->
    <<MAC:4/binary, SN:1/binary, NMSG/binary>>;
encode({#{message_type := service_request} = Msg, _}) ->
    encode_emm_msg(service_request, Msg);
encode({#{message_type := MsgType} = Msg, _}) ->
    Bin = encode_emm_msg(MsgType, Msg),
    MT = otc_nas_eps:compose_msg_type(MsgType),
    <<MT:8/big, Bin/binary>>.

decode_emm_msg(attach_accept, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsAttachResult, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {T3412Value, Bin3} = otc_l3_codec:decode_v(Bin2, 1),
    {TaiList, Bin4} = otc_l3_codec:decode_lv(Bin3),
    {EsmMessageContainer, Bin5} = otc_l3_codec:decode_lve(Bin4),
    Opts = [{guti, 16#50, tlv, 13},
            {location_area_identification, 16#13, tv, 6},
            {ms_identity, 16#23, tlv, {7, 10}},
            {emm_cause, 16#53, tv, 2},
            {t3402_value, 16#17, tv, 2},
            {t3423_value, 16#59, tv, 2},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {eps_network_feature_support, 16#64, tlv, {3, 4}},
            {additional_update_result, 16#F, tv, 1},
            {t3412_extended_value, 16#5E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {dcn_id, 16#65, tlv, 4},
            {sms_services_status, 16#E, tv, 1},
            {non_3gpp_nw_provided_policies, 16#D, tv, 1},
            {t3448_value, 16#6B, tlv, 3},
            {network_policy, 16#C, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {ciphering_key_data, 16#7C, tlve, {35, 2291}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1},
            {negotiated_wus_assistance_information, 16#35, tlv, {3, n}},
            {negotiated_drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{eps_attach_result => EpsAttachResult,
               t3412_value => T3412Value,
               tai_list => TaiList,
               esm_message_container => EsmMessageContainer
              };
decode_emm_msg(attach_complete, Bin0) ->
    {EsmMessageContainer, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_message_container => EsmMessageContainer
              };
decode_emm_msg(attach_reject, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{esm_message_container, 16#78, tlve, {6, n}},
            {t3346_value, 16#5F, tlv, 3},
            {t3402_value, 16#16, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(attach_request, Bin0) ->
    {NasKeySetIdentifier, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsAttachType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {EpsMobileIdentity, Bin3} = otc_l3_codec:decode_lv(Bin2),
    {UeNetworkCapability, Bin4} = otc_l3_codec:decode_lv(Bin3),
    {EsmMessageContainer, Bin5} = otc_l3_codec:decode_lve(Bin4),
    Opts = [{old_p_tmsi_signature, 16#19, tv, 4},
            {additional_guti, 16#50, tlv, 13},
            {last_visited_registered_tai, 16#52, tv, 6},
            {drx_parameter, 16#5C, tv, 3},
            {ms_network_capability, 16#31, tlv, {4, 10}},
            {old_location_area_identification, 16#13, tv, 6},
            {tmsi_status, 16#9, tv, 1},
            {mobile_station_classmark_2, 16#11, tlv, 5},
            {mobile_station_classmark_3, 16#20, tlv, {2, 34}},
            {supported_codecs, 16#40, tlv, {5, n}},
            {additional_update_type, 16#F, tv, 1},
            {voice_domain_preference_and_ues_usage_setting, 16#5D, tlv, 3},
            {device_properties, 16#D, tv, 1},
            {old_guti_type, 16#E, tv, 1},
            {ms_network_feature_support, 16#C, tv, 1},
            {tmsi_based_nri_container, 16#10, tlv, 4},
            {t3324_value, 16#6A, tlv, 3},
            {t3412_extended_value, 16#5E, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_status, 16#6D, tlv, 3},
            {additional_information_requested, 16#17, tv, 2},
            {n1_ue_network_capability, 16#32, tlv, {3, 15}},
            {ue_radio_capability_id_availability, 16#34, tlv, 3},
            {requested_wus_assistance_information, 16#35, tlv, {3, n}},
            {drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{eps_attach_type => EpsAttachType,
               nas_key_set_identifier => NasKeySetIdentifier,
               eps_mobile_identity => EpsMobileIdentity,
               ue_network_capability => UeNetworkCapability,
               esm_message_container => EsmMessageContainer
              };
decode_emm_msg(authentication_failure, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(authentication_reject, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(authentication_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {NasKeySetIdentifierasme, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {AuthenticationParameterRandEpsChallenge, Bin3} = otc_l3_codec:decode_v(Bin2, 16),
    {AuthenticationParameterAutnEpsChallenge, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{nas_key_set_identifierasme => NasKeySetIdentifierasme,
               authentication_parameter_rand_eps_challenge => AuthenticationParameterRandEpsChallenge,
               authentication_parameter_autn_eps_challenge => AuthenticationParameterAutnEpsChallenge
              };
decode_emm_msg(authentication_response, Bin0) ->
    {AuthenticationResponseParameter, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{authentication_response_parameter => AuthenticationResponseParameter
              };
decode_emm_msg(cs_service_notification, Bin0) ->
    {PagingIdentity, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{cli, 16#60, tlv, {3, 14}},
            {ss_code, 16#61, tv, 2},
            {lcs_indicator, 16#62, tv, 2},
            {lcs_client_identity, 16#63, tlv, {3, 257}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{paging_identity => PagingIdentity
              };
decode_emm_msg(detach_accept_ue_originating_detach, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(detach_accept_ue_terminated_detach, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(detach_request_ue_originating_detach, Bin0) ->
    {NasKeySetIdentifier, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {DetachType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {EpsMobileIdentity, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{detach_type => DetachType,
               nas_key_set_identifier => NasKeySetIdentifier,
               eps_mobile_identity => EpsMobileIdentity
              };
decode_emm_msg(detach_request_ue_terminated_detach, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {DetachType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{emm_cause, 16#53, tv, 2}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{detach_type => DetachType
              };
decode_emm_msg(downlink_nas_transport, Bin0) ->
    {NasMessageContainer, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{nas_message_container => NasMessageContainer
              };
decode_emm_msg(emm_information, Bin0) ->
    Opts = [{full_name_for_network, 16#43, tlv, {3, n}},
            {short_name_for_network, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_local_time_zone, 16#47, tv, 8},
            {network_daylight_saving_time, 16#49, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(emm_status, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(extended_service_request, Bin0) ->
    {NasKeySetIdentifier, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {ServiceType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {MTmsi, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{service_type => ServiceType,
               nas_key_set_identifier => NasKeySetIdentifier,
               m_tmsi => MTmsi
              };
decode_emm_msg(guti_reallocation_command, Bin0) ->
    {Guti, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [{tai_list, 16#54, tlv, {8, 98}},
            {dcn_id, 16#65, tlv, 4},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{guti => Guti
              };
decode_emm_msg(guti_reallocation_complete, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(identity_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {IdentityType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{identity_type => IdentityType
              };
decode_emm_msg(identity_response, Bin0) ->
    {IdentityResponseMessage, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {MobileIdentity, Bin2} = otc_l3_codec:decode_lv(Bin1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{identity_response_message => IdentityResponseMessage,
               mobile_identity => MobileIdentity
              };
decode_emm_msg(security_mode_command, Bin0) ->
    {SelectedNasSecurityAlgorithms, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {_, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {NasKeySetIdentifier, Bin3} = otc_l3_codec:decode_v(Bin2, half),
    {ReplayedUeSecurityCapabilities, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{imeisv_request, 16#C, tv, 1},
            {replayed_nonceue, 16#55, tv, 5},
            {noncemme, 16#56, tv, 5},
            {hashmme, 16#4F, tlv, 10},
            {replayed_ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_radio_capability_id_request, 16#37, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{selected_nas_security_algorithms => SelectedNasSecurityAlgorithms,
               nas_key_set_identifier => NasKeySetIdentifier,
               replayed_ue_security_capabilities => ReplayedUeSecurityCapabilities
              };
decode_emm_msg(security_mode_complete, Bin0) ->
    Opts = [{imeisv, 16#23, tlv, 11},
            {replayed_nas_message_container, 16#79, tlve, {3, n}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(security_mode_reject, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(security_protected_nas_message, Bin0) ->
    {MessageAuthenticationCode, Bin1} = otc_l3_codec:decode_v(Bin0, 4),
    {SequenceNumber, Bin2} = otc_l3_codec:decode_v(Bin1, 1),
    {NasMessage, Bin3} = otc_l3_codec:decode_v(Bin2, {1, n}),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_authentication_code => MessageAuthenticationCode,
               sequence_number => SequenceNumber,
               nas_message => NasMessage
              };
decode_emm_msg(service_reject, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(service_request, Bin0) ->
    {KsiAndSequenceNumber, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {MessageAuthenticationCodeShort, Bin2} = otc_l3_codec:decode_v(Bin1, 2),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{ksi_and_sequence_number => KsiAndSequenceNumber,
               message_authentication_code_short => MessageAuthenticationCodeShort
              };
decode_emm_msg(tracking_area_update_accept, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsUpdateResult, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{t3412_value, 16#5A, tv, 2},
            {guti, 16#50, tlv, 13},
            {tai_list, 16#54, tlv, {8, 98}},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {location_area_identification, 16#13, tv, 6},
            {ms_identity, 16#23, tlv, {7, 10}},
            {emm_cause, 16#53, tv, 2},
            {t3402_value, 16#17, tv, 2},
            {t3423_value, 16#59, tv, 2},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {eps_network_feature_support, 16#64, tlv, {3, 4}},
            {additional_update_result, 16#F, tv, 1},
            {t3412_extended_value, 16#5E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {header_compression_configuration_status, 16#68, tlv, 4},
            {dcn_id, 16#65, tlv, 4},
            {sms_services_status, 16#E, tv, 1},
            {non_3gpp_nw_policies, 16#D, tv, 1},
            {t3448_value, 16#6B, tlv, 3},
            {network_policy, 16#C, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {ciphering_key_data, 16#7C, tlve, {35, 2291}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1},
            {negotiated_wus_assistance_information, 16#35, tlv, {3, n}},
            {negotiated_drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{eps_update_result => EpsUpdateResult
              };
decode_emm_msg(tracking_area_update_complete, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_emm_msg(tracking_area_update_reject, Bin0) ->
    {EmmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{emm_cause => EmmCause
              };
decode_emm_msg(tracking_area_update_request, Bin0) ->
    {NasKeySetIdentifier, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsUpdateType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {OldGuti, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{non_current_native_nas_key_set_identifier, 16#B, tv, 1},
            {gprs_ciphering_key_sequence_number, 16#8, tv, 1},
            {old_p_tmsi_signature, 16#19, tv, 4},
            {additional_guti, 16#50, tlv, 13},
            {nonceue, 16#55, tv, 5},
            {ue_network_capability, 16#58, tlv, {4, 15}},
            {last_visited_registered_tai, 16#52, tv, 6},
            {drx_parameter, 16#5C, tv, 3},
            {ue_radio_capability_information_update_needed, 16#A, tv, 1},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {ms_network_capability, 16#31, tlv, {4, 10}},
            {old_location_area_identification, 16#13, tv, 6},
            {tmsi_status, 16#9, tv, 1},
            {mobile_station_classmark_2, 16#11, tlv, 5},
            {mobile_station_classmark_3, 16#20, tlv, {2, 34}},
            {supported_codecs, 16#40, tlv, {5, n}},
            {additional_update_type, 16#F, tv, 1},
            {voice_domain_preference_and_ues_usage_setting, 16#5D, tlv, 3},
            {old_guti_type, 16#E, tv, 1},
            {device_properties, 16#D, tv, 1},
            {ms_network_feature_support, 16#C, tv, 1},
            {tmsi_based_nri_container, 16#10, tlv, 4},
            {t3324_value, 16#6A, tlv, 3},
            {t3412_extended_value, 16#5E, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_status, 16#6D, tlv, 3},
            {additional_information_requested, 16#17, tv, 2},
            {n1_ue_network_capability, 16#32, tlv, {3, 15}},
            {ue_radio_capability_id_availability, 16#34, tlv, 3},
            {requested_wus_assistance_information, 16#35, tlv, {3, n}},
            {drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{eps_update_type => EpsUpdateType,
               nas_key_set_identifier => NasKeySetIdentifier,
               old_guti => OldGuti
              };
decode_emm_msg(uplink_nas_transport, Bin0) ->
    {NasMessageContainer, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{nas_message_container => NasMessageContainer
              };
decode_emm_msg(downlink_generic_nas_transport, Bin0) ->
    {GenericMessageContainerType, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {GenericMessageContainer, Bin2} = otc_l3_codec:decode_lve(Bin1),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{generic_message_container_type => GenericMessageContainerType,
               generic_message_container => GenericMessageContainer
              };
decode_emm_msg(uplink_generic_nas_transport, Bin0) ->
    {GenericMessageContainerType, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    {GenericMessageContainer, Bin2} = otc_l3_codec:decode_lve(Bin1),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{generic_message_container_type => GenericMessageContainerType,
               generic_message_container => GenericMessageContainer
              };
decode_emm_msg(control_plane_service_request, Bin0) ->
    {NasKeySetIdentifier, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {ControlPlaneServiceType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{esm_message_container, 16#78, tlve, {3, n}},
            {nas_message_container, 16#67, tlv, {4, 253}},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{control_plane_service_type => ControlPlaneServiceType,
               nas_key_set_identifier => NasKeySetIdentifier
              };
decode_emm_msg(service_accept, Bin0) ->
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals.

encode_emm_msg(attach_accept, #{eps_attach_result := EpsAttachResult,
                                t3412_value := T3412Value,
                                tai_list := TaiList,
                                esm_message_container := EsmMessageContainer} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(EpsAttachResult, half, <<>>),
    Bin3 = otc_l3_codec:encode_v(T3412Value, 1, <<>>),
    Bin4 = otc_l3_codec:encode_lv(TaiList, <<>>),
    Bin5 = otc_l3_codec:encode_lve(EsmMessageContainer, <<>>),
    Opts = [{guti, 16#50, tlv, 13},
            {location_area_identification, 16#13, tv, 6},
            {ms_identity, 16#23, tlv, {7, 10}},
            {emm_cause, 16#53, tv, 2},
            {t3402_value, 16#17, tv, 2},
            {t3423_value, 16#59, tv, 2},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {eps_network_feature_support, 16#64, tlv, {3, 4}},
            {additional_update_result, 16#F, tv, 1},
            {t3412_extended_value, 16#5E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {dcn_id, 16#65, tlv, 4},
            {sms_services_status, 16#E, tv, 1},
            {non_3gpp_nw_provided_policies, 16#D, tv, 1},
            {t3448_value, 16#6B, tlv, 3},
            {network_policy, 16#C, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {ciphering_key_data, 16#7C, tlve, {35, 2291}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1},
            {negotiated_wus_assistance_information, 16#35, tlv, {3, n}},
            {negotiated_drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, Bin5/bitstring, OptBin/binary>>;
encode_emm_msg(attach_complete, #{esm_message_container := EsmMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(EsmMessageContainer, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(attach_reject, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [{esm_message_container, 16#78, tlve, {6, n}},
            {t3346_value, 16#5F, tlv, 3},
            {t3402_value, 16#16, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(attach_request, #{eps_attach_type := EpsAttachType,
                                 nas_key_set_identifier := NasKeySetIdentifier,
                                 eps_mobile_identity := EpsMobileIdentity,
                                 ue_network_capability := UeNetworkCapability,
                                 esm_message_container := EsmMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(EpsAttachType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(EpsMobileIdentity, <<>>),
    Bin4 = otc_l3_codec:encode_lv(UeNetworkCapability, <<>>),
    Bin5 = otc_l3_codec:encode_lve(EsmMessageContainer, <<>>),
    Opts = [{old_p_tmsi_signature, 16#19, tv, 4},
            {additional_guti, 16#50, tlv, 13},
            {last_visited_registered_tai, 16#52, tv, 6},
            {drx_parameter, 16#5C, tv, 3},
            {ms_network_capability, 16#31, tlv, {4, 10}},
            {old_location_area_identification, 16#13, tv, 6},
            {tmsi_status, 16#9, tv, 1},
            {mobile_station_classmark_2, 16#11, tlv, 5},
            {mobile_station_classmark_3, 16#20, tlv, {2, 34}},
            {supported_codecs, 16#40, tlv, {5, n}},
            {additional_update_type, 16#F, tv, 1},
            {voice_domain_preference_and_ues_usage_setting, 16#5D, tlv, 3},
            {device_properties, 16#D, tv, 1},
            {old_guti_type, 16#E, tv, 1},
            {ms_network_feature_support, 16#C, tv, 1},
            {tmsi_based_nri_container, 16#10, tlv, 4},
            {t3324_value, 16#6A, tlv, 3},
            {t3412_extended_value, 16#5E, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_status, 16#6D, tlv, 3},
            {additional_information_requested, 16#17, tv, 2},
            {n1_ue_network_capability, 16#32, tlv, {3, 15}},
            {ue_radio_capability_id_availability, 16#34, tlv, 3},
            {requested_wus_assistance_information, 16#35, tlv, {3, n}},
            {drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, Bin5/bitstring, OptBin/binary>>;
encode_emm_msg(authentication_failure, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(authentication_reject, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(authentication_request, #{nas_key_set_identifierasme := NasKeySetIdentifierasme,
                                         authentication_parameter_rand_eps_challenge := AuthenticationParameterRandEpsChallenge,
                                         authentication_parameter_autn_eps_challenge := AuthenticationParameterAutnEpsChallenge} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(NasKeySetIdentifierasme, half, <<>>),
    Bin3 = otc_l3_codec:encode_v(AuthenticationParameterRandEpsChallenge, 16, <<>>),
    Bin4 = otc_l3_codec:encode_lv(AuthenticationParameterAutnEpsChallenge, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_emm_msg(authentication_response, #{authentication_response_parameter := AuthenticationResponseParameter} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(AuthenticationResponseParameter, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(cs_service_notification, #{paging_identity := PagingIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(PagingIdentity, 1, <<>>),
    Opts = [{cli, 16#60, tlv, {3, 14}},
            {ss_code, 16#61, tv, 2},
            {lcs_indicator, 16#62, tv, 2},
            {lcs_client_identity, 16#63, tlv, {3, 257}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(detach_accept_ue_originating_detach, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(detach_accept_ue_terminated_detach, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(detach_request_ue_originating_detach, #{detach_type := DetachType,
                                                       nas_key_set_identifier := NasKeySetIdentifier,
                                                       eps_mobile_identity := EpsMobileIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(DetachType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(EpsMobileIdentity, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_emm_msg(detach_request_ue_terminated_detach, #{detach_type := DetachType} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(DetachType, half, <<>>),
    Opts = [{emm_cause, 16#53, tv, 2}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(downlink_nas_transport, #{nas_message_container := NasMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(NasMessageContainer, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(emm_information, #{} = Msg) ->
    Opts = [{full_name_for_network, 16#43, tlv, {3, n}},
            {short_name_for_network, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_local_time_zone, 16#47, tv, 8},
            {network_daylight_saving_time, 16#49, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(emm_status, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(extended_service_request, #{service_type := ServiceType,
                                           nas_key_set_identifier := NasKeySetIdentifier,
                                           m_tmsi := MTmsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(ServiceType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(MTmsi, <<>>),
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_emm_msg(guti_reallocation_command, #{guti := Guti} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(Guti, <<>>),
    Opts = [{tai_list, 16#54, tlv, {8, 98}},
            {dcn_id, 16#65, tlv, 4},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(guti_reallocation_complete, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(identity_request, #{identity_type := IdentityType} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(IdentityType, half, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(identity_response, #{identity_response_message := IdentityResponseMessage,
                                    mobile_identity := MobileIdentity} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(IdentityResponseMessage, 1, <<>>),
    Bin2 = otc_l3_codec:encode_lv(MobileIdentity, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(security_mode_command, #{selected_nas_security_algorithms := SelectedNasSecurityAlgorithms,
                                        nas_key_set_identifier := NasKeySetIdentifier,
                                        replayed_ue_security_capabilities := ReplayedUeSecurityCapabilities} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SelectedNasSecurityAlgorithms, 1, <<>>),
    Bin2 = <<0:4>>,  % spare_half_octet
    Bin3 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin4 = otc_l3_codec:encode_lv(ReplayedUeSecurityCapabilities, <<>>),
    Opts = [{imeisv_request, 16#C, tv, 1},
            {replayed_nonceue, 16#55, tv, 5},
            {noncemme, 16#56, tv, 5},
            {hashmme, 16#4F, tlv, 10},
            {replayed_ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_radio_capability_id_request, 16#37, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_emm_msg(security_mode_complete, #{} = Msg) ->
    Opts = [{imeisv, 16#23, tlv, 11},
            {replayed_nas_message_container, 16#79, tlve, {3, n}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(security_mode_reject, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(security_protected_nas_message, #{message_authentication_code := MessageAuthenticationCode,
                                                 sequence_number := SequenceNumber,
                                                 nas_message := NasMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(MessageAuthenticationCode, 4, <<>>),
    Bin2 = otc_l3_codec:encode_v(SequenceNumber, 1, <<>>),
    Bin3 = otc_l3_codec:encode_v(NasMessage, {1, n}, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_emm_msg(service_reject, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3448_value, 16#6B, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(service_request, #{ksi_and_sequence_number := KsiAndSequenceNumber,
                                  message_authentication_code_short := MessageAuthenticationCodeShort} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(KsiAndSequenceNumber, 1, <<>>),
    Bin2 = otc_l3_codec:encode_v(MessageAuthenticationCodeShort, 2, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(tracking_area_update_accept, #{eps_update_result := EpsUpdateResult} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(EpsUpdateResult, half, <<>>),
    Opts = [{t3412_value, 16#5A, tv, 2},
            {guti, 16#50, tlv, 13},
            {tai_list, 16#54, tlv, {8, 98}},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {location_area_identification, 16#13, tv, 6},
            {ms_identity, 16#23, tlv, {7, 10}},
            {emm_cause, 16#53, tv, 2},
            {t3402_value, 16#17, tv, 2},
            {t3423_value, 16#59, tv, 2},
            {equivalent_plmns, 16#4A, tlv, {5, 47}},
            {emergency_number_list, 16#34, tlv, {5, 50}},
            {eps_network_feature_support, 16#64, tlv, {3, 4}},
            {additional_update_result, 16#F, tv, 1},
            {t3412_extended_value, 16#5E, tlv, 3},
            {t3324_value, 16#6A, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {header_compression_configuration_status, 16#68, tlv, 4},
            {dcn_id, 16#65, tlv, 4},
            {sms_services_status, 16#E, tv, 1},
            {non_3gpp_nw_policies, 16#D, tv, 1},
            {t3448_value, 16#6B, tlv, 3},
            {network_policy, 16#C, tv, 1},
            {t3447_value, 16#6C, tlv, 3},
            {extended_emergency_number_list, 16#7A, tlve, {7, 65538}},
            {ciphering_key_data, 16#7C, tlve, {35, 2291}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1},
            {negotiated_wus_assistance_information, 16#35, tlv, {3, n}},
            {negotiated_drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(tracking_area_update_complete, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_emm_msg(tracking_area_update_reject, #{emm_cause := EmmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EmmCause, 1, <<>>),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(tracking_area_update_request, #{eps_update_type := EpsUpdateType,
                                               nas_key_set_identifier := NasKeySetIdentifier,
                                               old_guti := OldGuti} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(EpsUpdateType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(OldGuti, <<>>),
    Opts = [{non_current_native_nas_key_set_identifier, 16#B, tv, 1},
            {gprs_ciphering_key_sequence_number, 16#8, tv, 1},
            {old_p_tmsi_signature, 16#19, tv, 4},
            {additional_guti, 16#50, tlv, 13},
            {nonceue, 16#55, tv, 5},
            {ue_network_capability, 16#58, tlv, {4, 15}},
            {last_visited_registered_tai, 16#52, tv, 6},
            {drx_parameter, 16#5C, tv, 3},
            {ue_radio_capability_information_update_needed, 16#A, tv, 1},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {ms_network_capability, 16#31, tlv, {4, 10}},
            {old_location_area_identification, 16#13, tv, 6},
            {tmsi_status, 16#9, tv, 1},
            {mobile_station_classmark_2, 16#11, tlv, 5},
            {mobile_station_classmark_3, 16#20, tlv, {2, 34}},
            {supported_codecs, 16#40, tlv, {5, n}},
            {additional_update_type, 16#F, tv, 1},
            {voice_domain_preference_and_ues_usage_setting, 16#5D, tlv, 3},
            {old_guti_type, 16#E, tv, 1},
            {device_properties, 16#D, tv, 1},
            {ms_network_feature_support, 16#C, tv, 1},
            {tmsi_based_nri_container, 16#10, tlv, 4},
            {t3324_value, 16#6A, tlv, 3},
            {t3412_extended_value, 16#5E, tlv, 3},
            {extended_drx_parameters, 16#6E, tlv, 3},
            {ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_status, 16#6D, tlv, 3},
            {additional_information_requested, 16#17, tv, 2},
            {n1_ue_network_capability, 16#32, tlv, {3, 15}},
            {ue_radio_capability_id_availability, 16#34, tlv, 3},
            {requested_wus_assistance_information, 16#35, tlv, {3, n}},
            {drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_emm_msg(uplink_nas_transport, #{nas_message_container := NasMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(NasMessageContainer, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_emm_msg(downlink_generic_nas_transport, #{generic_message_container_type := GenericMessageContainerType,
                                                 generic_message_container := GenericMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(GenericMessageContainerType, 1, <<>>),
    Bin2 = otc_l3_codec:encode_lve(GenericMessageContainer, <<>>),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(uplink_generic_nas_transport, #{generic_message_container_type := GenericMessageContainerType,
                                               generic_message_container := GenericMessageContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(GenericMessageContainerType, 1, <<>>),
    Bin2 = otc_l3_codec:encode_lve(GenericMessageContainer, <<>>),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(control_plane_service_request, #{control_plane_service_type := ControlPlaneServiceType,
                                                nas_key_set_identifier := NasKeySetIdentifier} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(NasKeySetIdentifier, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(ControlPlaneServiceType, half, <<>>),
    Opts = [{esm_message_container, 16#78, tlve, {3, n}},
            {nas_message_container, 16#67, tlv, {4, 253}},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_emm_msg(service_accept, #{} = Msg) ->
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {t3448_value, 16#6B, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin.
