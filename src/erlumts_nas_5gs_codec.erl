%% 3GPP TS 24.501 version 16.10.0
-module(erlumts_nas_5gs_codec).

-include("include/nas_5gs.hrl").
-include("include/l3.hrl").

-export([decode/1, encode/1]).

-spec decode(binary()) -> map() | unsupported.
decode(<<EPD:8, Bin/binary>>) ->
    ProtocolDiscriminator = erlumts_l3_codec:parse_protocol_discriminator(EPD),
    case ProtocolDiscriminator of
        '5gs_mobility_management_messages' -> %% security protected
            %% Security Header Type
            <<SHT:4, _:4, Rest/binary>> = Bin,
            SecurityHeaderType = parse_security_header_type(SHT),
            case decode_5gmm_content(SecurityHeaderType, Rest) of
                unsupported ->
                    unsupported;
                Msg ->
                    Msg#{security_header_type => SecurityHeaderType,
                         protocol_discriminator => ProtocolDiscriminator
                        }
            end;
        '5gs_session_management_messages' ->
            %% PDU session identity
            <<PSI:8, Rest/binary>> = Bin,
            case decode_5gsm_content(Rest) of
                unsupported ->
                    unsupported;
                Msg ->
                    Msg#{pdu_session_identity => PSI,
                         protocol_discriminator => ProtocolDiscriminator
                        }
            end;
        _ ->
            unsupported
    end.

-spec encode(map()) -> binary().
encode(Nas) ->
    <<>>.

decode_5gmm_content(plain_nas_message, <<MT:1/binary, OIE/binary>>) ->
    MsgType = parse_msg_type(MT),
    case decode_5gmm_msg(MsgType, OIE) of
        unsupported ->
            unsupported;
        Msg ->
            Msg#{message_type => MsgType}
    end;
decode_5gmm_content(_, <<MAC:3/binary, SN:1/binary, NMSG/binary>>) ->
    unsupported;
decode_5gmm_content(_, _) ->
    unsupported.

decode_5gsm_content(<<PTI:1/binary, MT:1/binary, OIE/binary>>) ->
    MsgType = parse_msg_type(MT),
    case decode_5gsm_msg(MsgType, OIE) of
        unsupported ->
            unsupported;
        Msg ->
            Msg#{procedure_transaction_identity => PTI,
                 message_type => MsgType}
    end;
decode_5gsm_content(_) ->
    unsupported.

%% 9.3.1 Security header type
parse_security_header_type(?NAS_SHT_PLAIN_5GS_NAS_MESSAGE) -> plain_5gs_nas_message;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED) -> integrity_protected;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED) -> integrity_protected_ciphered;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_5GS_SECURITY) -> integrity_protected_5gs_security;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_5GS_SECURITY) -> integrity_protected_ciphered_5gs_security.

%% 9.7 Message type
parse_msg_type(?NAS_MSGT_REGISTRATION_REQUEST) -> registration_request;
parse_msg_type(?NAS_MSGT_REGISTRATION_ACCEPT) -> registration_accept;
parse_msg_type(?NAS_MSGT_REGISTRATION_COMPLETE) -> registration_complete;
parse_msg_type(?NAS_MSGT_REGISTRATION_REJECT) -> registration_reject;
parse_msg_type(?NAS_MSGT_DEREGISTRATION_REQUEST_UE_ORIGINATING) -> deregistration_request_ue_originating;
parse_msg_type(?NAS_MSGT_DEREGISTRATION_ACCEPT_UE_ORIGINATING) -> deregistration_accept_ue_originating;
parse_msg_type(?NAS_MSGT_DEREGISTRATION_REQUEST_UE_TERMINATED) -> deregistration_request_ue_terminated;
parse_msg_type(?NAS_MSGT_DEREGISTRATION_ACCEPT_UE_TERMINATED) -> deregistration_accept_ue_terminated;
parse_msg_type(?NAS_MSGT_SERVICE_REQUEST) -> service_request;
parse_msg_type(?NAS_MSGT_SERVICE_REJECT) -> service_reject;
parse_msg_type(?NAS_MSGT_SERVICE_ACCEPT) -> service_accept;
parse_msg_type(?NAS_MSGT_CONTROL_PLANE_SERVICE_REQUEST) -> control_plane_service_request;
parse_msg_type(?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_COMMAND) -> network_slice_specific_authentication_command;
parse_msg_type(?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_COMPLETE) -> network_slice_specific_authentication_complete;
parse_msg_type(?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_RESULT) -> network_slice_specific_authentication_result;
parse_msg_type(?NAS_MSGT_CONFIGURATION_UPDATE_COMMAND) -> configuration_update_command;
parse_msg_type(?NAS_MSGT_CONFIGURATION_UPDATE_COMPLETE) -> configuration_update_complete;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_REQUEST) -> authentication_request;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_RESPONSE) -> authentication_response;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_REJECT) -> authentication_reject;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_FAILURE) -> authentication_failure;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_RESULT) -> authentication_result;
parse_msg_type(?NAS_MSGT_IDENTITY_REQUEST) -> identity_request;
parse_msg_type(?NAS_MSGT_IDENTITY_RESPONSE) -> identity_response;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_COMMAND) -> security_mode_command;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_COMPLETE) -> security_mode_complete;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_REJECT) -> security_mode_reject;
parse_msg_type(?NAS_MSGT_5GMM_STATUS) -> '5gmm_status';
parse_msg_type(?NAS_MSGT_NOTIFICATION) -> notification;
parse_msg_type(?NAS_MSGT_NOTIFICATION_RESPONSE) -> notification_response;
parse_msg_type(?NAS_MSGT_UL_NAS_TRANSPORT) -> ul_nas_transport;
parse_msg_type(?NAS_MSGT_DL_NAS_TRANSPORT) -> dl_nas_transport;
parse_msg_type(?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_REQUEST) -> pdu_session_establishment_request;
parse_msg_type(?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_ACCEPT) -> pdu_session_establishment_accept;
parse_msg_type(?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_REJECT) -> pdu_session_establishment_reject;
parse_msg_type(?NAS_MSGT_PDU_SESSION_AUTHENTICATION_COMMAND) -> pdu_session_authentication_command;
parse_msg_type(?NAS_MSGT_PDU_SESSION_AUTHENTICATION_COMPLETE) -> pdu_session_authentication_complete;
parse_msg_type(?NAS_MSGT_PDU_SESSION_AUTHENTICATION_RESULT) -> pdu_session_authentication_result;
parse_msg_type(?NAS_MSGT_PDU_SESSION_MODIFICATION_REQUEST) -> pdu_session_modification_request;
parse_msg_type(?NAS_MSGT_PDU_SESSION_MODIFICATION_REJECT) -> pdu_session_modification_reject;
parse_msg_type(?NAS_MSGT_PDU_SESSION_MODIFICATION_COMMAND) -> pdu_session_modification_command;
parse_msg_type(?NAS_MSGT_PDU_SESSION_MODIFICATION_COMPLETE) -> pdu_session_modification_complete;
parse_msg_type(?NAS_MSGT_PDU_SESSION_MODIFICATION_COMMAND_REJECT) -> pdu_session_modification_command_reject;
parse_msg_type(?NAS_MSGT_PDU_SESSION_RELEASE_REQUEST) -> pdu_session_release_request;
parse_msg_type(?NAS_MSGT_PDU_SESSION_RELEASE_REJECT) -> pdu_session_release_reject;
parse_msg_type(?NAS_MSGT_PDU_SESSION_RELEASE_COMMAND) -> pdu_session_release_command;
parse_msg_type(?NAS_MSGT_PDU_SESSION_RELEASE_COMPLETE) -> pdu_session_release_complete;
parse_msg_type(?NAS_MSGT_5GSM_STATUS) -> '5gsm_status'.

compose_msg_type(registration_request) -> ?NAS_MSGT_REGISTRATION_REQUEST;
compose_msg_type(registration_accept) -> ?NAS_MSGT_REGISTRATION_ACCEPT;
compose_msg_type(registration_complete) -> ?NAS_MSGT_REGISTRATION_COMPLETE;
compose_msg_type(registration_reject) -> ?NAS_MSGT_REGISTRATION_REJECT;
compose_msg_type(deregistration_request_ue_originating) -> ?NAS_MSGT_DEREGISTRATION_REQUEST_UE_ORIGINATING;
compose_msg_type(deregistration_accept_ue_originating) -> ?NAS_MSGT_DEREGISTRATION_ACCEPT_UE_ORIGINATING;
compose_msg_type(deregistration_request_ue_terminated) -> ?NAS_MSGT_DEREGISTRATION_REQUEST_UE_TERMINATED;
compose_msg_type(deregistration_accept_ue_terminated) -> ?NAS_MSGT_DEREGISTRATION_ACCEPT_UE_TERMINATED;
compose_msg_type(service_request) -> ?NAS_MSGT_SERVICE_REQUEST;
compose_msg_type(service_reject) -> ?NAS_MSGT_SERVICE_REJECT;
compose_msg_type(service_accept) -> ?NAS_MSGT_SERVICE_ACCEPT;
compose_msg_type(control_plane_service_request) -> ?NAS_MSGT_CONTROL_PLANE_SERVICE_REQUEST;
compose_msg_type(network_slice_specific_authentication_command) -> ?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_COMMAND;
compose_msg_type(network_slice_specific_authentication_complete) -> ?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_COMPLETE;
compose_msg_type(network_slice_specific_authentication_result) -> ?NAS_MSGT_NETWORK_SLICE_SPECIFIC_AUTHENTICATION_RESULT;
compose_msg_type(configuration_update_command) -> ?NAS_MSGT_CONFIGURATION_UPDATE_COMMAND;
compose_msg_type(configuration_update_complete) -> ?NAS_MSGT_CONFIGURATION_UPDATE_COMPLETE;
compose_msg_type(authentication_request) -> ?NAS_MSGT_AUTHENTICATION_REQUEST;
compose_msg_type(authentication_response) -> ?NAS_MSGT_AUTHENTICATION_RESPONSE;
compose_msg_type(authentication_reject) -> ?NAS_MSGT_AUTHENTICATION_REJECT;
compose_msg_type(authentication_failure) -> ?NAS_MSGT_AUTHENTICATION_FAILURE;
compose_msg_type(authentication_result) -> ?NAS_MSGT_AUTHENTICATION_RESULT;
compose_msg_type(identity_request) -> ?NAS_MSGT_IDENTITY_REQUEST;
compose_msg_type(identity_response) -> ?NAS_MSGT_IDENTITY_RESPONSE;
compose_msg_type(security_mode_command) -> ?NAS_MSGT_SECURITY_MODE_COMMAND;
compose_msg_type(security_mode_complete) -> ?NAS_MSGT_SECURITY_MODE_COMPLETE;
compose_msg_type(security_mode_reject) -> ?NAS_MSGT_SECURITY_MODE_REJECT;
compose_msg_type('5gmm_status') -> ?NAS_MSGT_5GMM_STATUS;
compose_msg_type(notification) -> ?NAS_MSGT_NOTIFICATION;
compose_msg_type(notification_response) -> ?NAS_MSGT_NOTIFICATION_RESPONSE;
compose_msg_type(ul_nas_transport) -> ?NAS_MSGT_UL_NAS_TRANSPORT;
compose_msg_type(dl_nas_transport) -> ?NAS_MSGT_DL_NAS_TRANSPORT;
compose_msg_type(pdu_session_establishment_request) -> ?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_REQUEST;
compose_msg_type(pdu_session_establishment_accept) -> ?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_ACCEPT;
compose_msg_type(pdu_session_establishment_reject) -> ?NAS_MSGT_PDU_SESSION_ESTABLISHMENT_REJECT;
compose_msg_type(pdu_session_authentication_command) -> ?NAS_MSGT_PDU_SESSION_AUTHENTICATION_COMMAND;
compose_msg_type(pdu_session_authentication_complete) -> ?NAS_MSGT_PDU_SESSION_AUTHENTICATION_COMPLETE;
compose_msg_type(pdu_session_authentication_result) -> ?NAS_MSGT_PDU_SESSION_AUTHENTICATION_RESULT;
compose_msg_type(pdu_session_modification_request) -> ?NAS_MSGT_PDU_SESSION_MODIFICATION_REQUEST;
compose_msg_type(pdu_session_modification_reject) -> ?NAS_MSGT_PDU_SESSION_MODIFICATION_REJECT;
compose_msg_type(pdu_session_modification_command) -> ?NAS_MSGT_PDU_SESSION_MODIFICATION_COMMAND;
compose_msg_type(pdu_session_modification_complete) -> ?NAS_MSGT_PDU_SESSION_MODIFICATION_COMPLETE;
compose_msg_type(pdu_session_modification_command_reject) -> ?NAS_MSGT_PDU_SESSION_MODIFICATION_COMMAND_REJECT;
compose_msg_type(pdu_session_release_request) -> ?NAS_MSGT_PDU_SESSION_RELEASE_REQUEST;
compose_msg_type(pdu_session_release_reject) -> ?NAS_MSGT_PDU_SESSION_RELEASE_REJECT;
compose_msg_type(pdu_session_release_command) -> ?NAS_MSGT_PDU_SESSION_RELEASE_COMMAND;
compose_msg_type(pdu_session_release_complete) -> ?NAS_MSGT_PDU_SESSION_RELEASE_COMPLETE;
compose_msg_type('5gsm_status') -> ?NAS_MSGT_5GSM_STATUS.

decode_5gmm_msg(authentication_request, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {Abba, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [{authentication_parameter_rand_5g_authentication_challenge, 16#21, tv, 17},
            {authentication_parameter_autn_5g_authentication_challenge, 16#20, tlv, 18},
            {eap_message, 16#78, tlve, {7, 150}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{ngksi => Ngksi,
               abba => Abba
              };
decode_5gmm_msg(authentication_response, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{authentication_response_parameter, 16#2D, tlv, 18},
            {eap_message, 16#78, tlve, {7, 1503}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(authentication_result, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {EapMessage, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [{abba, 16#38, tlv, {4, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{ngksi => Ngksi,
               eap_message => EapMessage
              };
decode_5gmm_msg(authentication_failure, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MmCause, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(authentication_reject, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{eap_message, 16#78, tlve, {7, 1503}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(registration_request, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SRegistrationType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {Ngksi, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {SMobileIdentity, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{'5gs_registration_type' => SRegistrationType,
               ngksi => Ngksi,
               '5gs_mobile_identity' => SMobileIdentity
              };
decode_5gmm_msg(registration_accept, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SRegistrationResult, Bin2} = erlumts_l3_codec:decode_lv(Bin1),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gs_registration_result' => SRegistrationResult
              };
decode_5gmm_msg(registration_complete, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{sor_transparent_container, 16#73, tlve, 20}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(registration_reject, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MmCause, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3502_value, 16#16, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {rejected_nssai, 16#69, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(ul_nas_transport, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {PayloadContainerType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {PayloadContainer, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [{old_pdu_session_id, 16#59, tv, 2},
            {request_type, 16#8, tv, 1},
            {s_nssai, 16#22, tlv, {3, 10}},
            {dnn, 16#25, tlv, {3, 102}},
            {additional_information, 16#24, tlv, {3, n}},
            {ma_pdu_session_information, 16#A, tv, 1},
            {release_assistance_indication, 16#F, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{payload_container_type => PayloadContainerType,
               payload_container => PayloadContainer
              };
decode_5gmm_msg(dl_nas_transport, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {PayloadContainerType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {PayloadContainer, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [{additional_information, 16#24, tlv, {3, n}},
            {'5gmm_cause', 16#58, tv, 2},
            {back_off_timer_value, 16#37, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{payload_container_type => PayloadContainerType,
               payload_container => PayloadContainer
              };
decode_5gmm_msg(deregistration_request_ue_originating_deregistration, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {DeRegistrationType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {Ngksi, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {SMobileIdentity, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{de_registration_type => DeRegistrationType,
               ngksi => Ngksi,
               '5gs_mobile_identity' => SMobileIdentity
              };
decode_5gmm_msg(deregistration_accept_ue_originating_deregistration, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(deregistration_request_ue_terminated_deregistration, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {DeRegistrationType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    Opts = [{'5gmm_cause', 16#58, tv, 2},
            {t3346_value, 16#5F, tlv, 3},
            {rejected_nssai, 16#6D, tlv, {4, 42}},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{de_registration_type => DeRegistrationType
              };
decode_5gmm_msg(deregistration_accept_ue_terminated_deregistration, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(service_request, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {Ngksi, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ServiceType, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {STmsi, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [{uplink_data_status, 16#40, tlv, {4, 34}},
            {allowed_pdu_session_status, 16#25, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{ngksi => Ngksi,
               service_type => ServiceType,
               '5g_s_tmsi' => STmsi
              };
decode_5gmm_msg(service_accept, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {pdu_session_reactivation_result, 16#26, tlv, {4, 34}},
            {pdu_session_reactivation_result_error_cause, 16#72, tlve, {5, 515}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(service_reject, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MmCause, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}},
            {t3346_value, 16#5F, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {t3448_value, 16#6B, tlv, 3},
            {cag_information_list, 16#75, tlve, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(configuration_update_command, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(configuration_update_complete, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(identity_request, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {IdentityType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{identity_type => IdentityType
              };
decode_5gmm_msg(identity_respones, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MobileIdentity, Bin2} = erlumts_l3_codec:decode_lve(Bin1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{mobile_identity => MobileIdentity
              };
decode_5gmm_msg(notification, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {AccessType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {_, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{access_type => AccessType
              };
decode_5gmm_msg(notification_response, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{pdu_session_status, 16#50, tlv, {4, 34}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(security_mode_command, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SelectedNasSecurityAlgorithms, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    {Ngksi, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    {_, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {ReplayedUeSecurityCapabilities, Bin5} = erlumts_l3_codec:decode_lv(Bin4),
    Opts = [{imeisv_request, 16#E, tv, 1},
            {selected_eps_nas_security_algorithms, 16#57, tv, 2},
            {additional_5g_security_information, 16#36, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {abba, 16#38, tlv, {4, n}},
            {replayed_s1_ue_security_capabilities, 16#19, tlv, {4, 7}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{selected_nas_security_algorithms => SelectedNasSecurityAlgorithms,
               ngksi => Ngksi,
               replayed_ue_security_capabilities => ReplayedUeSecurityCapabilities
              };
decode_5gmm_msg(security_mode_complete, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    Opts = [{imeisv, 16#77, tlve, 12},
            {nas_message_container, 16#71, tlve, {4, n}},
            {non_imeisv_pei, 16#78, tlve, {7, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals;
decode_5gmm_msg(security_mode_reject, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MmCause, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(security_protected_5gs_nas_message, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MessageAuthenticationCode, Bin2} = erlumts_l3_codec:decode_v(Bin1, 4),
    {SequenceNumber, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {Plain5gsNasMessage, Bin4} = erlumts_l3_codec:decode_v(Bin3, {3, n}),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{message_authentication_code => MessageAuthenticationCode,
               sequence_number => SequenceNumber,
               plain_5gs_nas_message => Plain5gsNasMessage
              };
decode_5gmm_msg('5gmm_status', Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {MmCause, Bin2} = erlumts_l3_codec:decode_v(Bin1, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{'5gmm_cause' => MmCause
              };
decode_5gmm_msg(control_plane_service_request, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {ControlPlaneServiceType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {Ngksi, Bin3} = erlumts_l3_codec:decode_v(Bin2, half),
    Opts = [{ciot_small_data_container, 16#6F, tlv, {4, 257}},
            {payload_container_type, 16#8, tv, 1},
            {payload_container, 16#7B, tlve, {4, 65538}},
            {pdu_session_status, 16#50, tlv, {4, 34}},
            {release_assistance_indication, 16#F, tv, 1},
            {uplink_data_status, 16#40, tlv, {4, 34}},
            {nas_message_container, 16#71, tlve, {4, n}},
            {additional_information, 16#24, tlv, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{control_plane_service_type => ControlPlaneServiceType,
               ngksi => Ngksi
              };
decode_5gmm_msg(network_slicespecific_authentication_command, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SNssai, Bin2} = erlumts_l3_codec:decode_lv(Bin1),
    {EapMessage, Bin3} = erlumts_l3_codec:decode_lve(Bin2),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              };
decode_5gmm_msg(network_slicespecific_authentication_complete, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SNssai, Bin2} = erlumts_l3_codec:decode_lv(Bin1),
    {EapMessage, Bin3} = erlumts_l3_codec:decode_lve(Bin2),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              };
decode_5gmm_msg(network_slicespecific_authentication_result, Bin0) ->
    {_, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SNssai, Bin2} = erlumts_l3_codec:decode_lv(Bin1),
    {EapMessage, Bin3} = erlumts_l3_codec:decode_lve(Bin2),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{s_nssai => SNssai,
               eap_message => EapMessage
              };
decode_5gmm_msg(_, _) ->
    unsupported.

decode_5gsm_msg(pdu_session_establishment_request, Bin0) ->
    {IntegrityProtectionMaximumDataRate, Bin1} = erlumts_l3_codec:decode_v(Bin0, 2),
    Opts = [{pdu_session_type, 16#9, tv, 1},
            {ssc_mode, 16#A, tv, 1},
            {'5gsm_capability', 16#28, tlv, {3, 15}},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {sm_pdu_dn_request_container, 16#39, tlv, {3, 255}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ds_tt_ethernet_port_mac_address, 16#6E, tlv, 8},
            {ue_ds_tt_residence_time, 16#6F, tlv, 10},
            {port_management_information_container, 16#74, tlve, {8, 65538}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3},
            {suggested_interface_identifier, 16#29, tlv, 11}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{integrity_protection_maximum_data_rate => IntegrityProtectionMaximumDataRate
              };
decode_5gsm_msg(pdu_session_establishment_accept, Bin0) ->
    {SelectedPduSessionType, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SelectedSscMode, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AuthorizedQosRules, Bin3} = erlumts_l3_codec:decode_lve(Bin2),
    {SessionAmbr, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {pdu_address, 16#29, tlv, [7, 11, 15, 27, 31]},
            {rq_timer_value, 16#56, tv, 2},
            {s_nssai, 16#22, tlv, {3, 10}},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {dnn, 16#25, tlv, {3, 102}},
            {'5gsm_network_feature_support', 16#17, tlv, {3, 15}},
            {serving_plmn_rate_control, 16#18, tlv, 4},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {control_plane_only_indication, 16#C, tv, 1},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{selected_pdu_session_type => SelectedPduSessionType,
               selected_ssc_mode => SelectedSscMode,
               authorized_qos_rules => AuthorizedQosRules,
               session_ambr => SessionAmbr
              };
decode_5gsm_msg(pdu_session_establishment_reject, Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {allowed_ssc_mode, 16#F, tv, 1},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_authentication_command, Bin0) ->
    {EapMessage, Bin1} = erlumts_l3_codec:decode_lve(Bin0),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{eap_message => EapMessage
              };
decode_5gsm_msg(pdu_session_authentication_complete, Bin0) ->
    {EapMessage, Bin1} = erlumts_l3_codec:decode_lve(Bin0),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{eap_message => EapMessage
              };
decode_5gsm_msg(pdu_session_authentication_result, Bin0) ->
    Opts = [{eap_message, 16#78, tlve, {7, 1503}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_request, Bin0) ->
    Opts = [{'5gsm_capability', 16#28, tlv, {3, 15}},
            {'5gsm_cause', 16#59, tv, 2},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {integrity_protection_maximum_data_rate, 16#13, tv, 3},
            {requested_qos_rules, 16#7A, tlve, {7, 65538}},
            {requested_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_reject, Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_modification_command, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {session_ambr, 16#2A, tlv, 8},
            {rq_timer_value, 16#56, tv, 2},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {authorized_qos_rules, 16#7A, tlve, {7, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#1E, tlv, 4},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_complete, Bin0) ->
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_command_reject, Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_request, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_release_reject, Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_command, Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {access_type, 16#D, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_complete, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg('5gsm_status', Bin0) ->
    {SmCause, Bin1} = erlumts_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(_, _) ->
    unsupported.