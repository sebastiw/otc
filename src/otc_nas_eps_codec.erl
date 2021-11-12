%% 3GPP TS 24.301 version 16.8.0
-module(otc_nas_eps_codec).

-include("include/nas_eps.hrl").
-include("include/l3.hrl").

-export([decode/1, encode/1]).

-spec decode(binary()) -> map() | {unsupported, term()}.
decode(<<EBI_SHT:4, PD:4, Rest/binary>>) ->
    ProtocolDiscriminator = otc_l3_codec:parse_protocol_discriminator(PD),
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            %% Security Header Type
            SecurityHeaderType = parse_security_header_type(EBI_SHT),
            case decode_emm_content(SecurityHeaderType, Rest) of
                {unsupported, Reason} ->
                    {unsupported, Reason};
                Msg ->
                    Msg#{security_header_type => SecurityHeaderType,
                         protocol_discriminator => ProtocolDiscriminator
                        }
            end;
        eps_session_management_messages ->
            %% EPS Bearer Identity
            case decode_esm_content(Rest) of
                {unsupported, R} ->
                    {unsupported, R};
                Msg ->
                    Msg#{eps_bearer_identity => EBI_SHT,
                         protocol_discriminator => ProtocolDiscriminator
                        }
            end;
        _ ->
            {unsupported, {protocol_discriminator, ProtocolDiscriminator}}
    end.

-spec encode(map()) -> binary() | {unsupported, term()}.
encode(#{protocol_discriminator := ProtocolDiscriminator} = Msg) ->
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            SecurityHeaderType = maps:get(security_header_type, Msg, undefined),
            case encode_emm_content(SecurityHeaderType, Msg) of
                {unsupported, Reason} ->
                    {unsupported, Reason};
                Bin ->
                    PD = otc_l3_codec:compose_protocol_discriminator(ProtocolDiscriminator),
                    SHT = compose_security_header_type(SecurityHeaderType),
                    <<SHT:4, PD:4, Bin/binary>>
            end;
        eps_session_management_messages ->
            EBI = maps:get(eps_bearer_identity, Msg, undefined),
            case encode_esm_content(Msg) of
                {unsupported, Reason} ->
                    {unsupported, Reason};
                Bin ->
                    PD = otc_l3_codec:compose_protocol_discriminator(ProtocolDiscriminator),
                    <<EBI:4, PD:4, Bin/binary>>
            end;
        _ ->
            {unsupported, {protocol_discriminator, ProtocolDiscriminator}}

    end;
encode(Msg) ->
    {unsupported, Msg}.


decode_emm_content(plain_nas_message, <<MT:8/big, OIE/binary>>) ->
    MsgType = parse_msg_type(MT),
    case decode_emm_msg(MsgType, OIE) of
        unsupported ->
            {unsupported, {plain_nas_message, emm_msg, MT}};
        Msg ->
            Msg#{message_type => MsgType}
    end;
decode_emm_content(service_request, Bin) ->
    Msg = decode_emm_msg(service_request, Bin),
    Msg#{message_type => service_request};
decode_emm_content(_SHT, <<MAC:4/binary, SN:1/binary, NMSG/binary>>) ->
    case decode(NMSG) of
        {unsupported, Reason} ->
            {unsupported, Reason};
        Msg ->
            #{message_authentication_code => MAC,
              sequence_number => SN,
              nas_message => Msg
             }
    end;
decode_emm_content(_, _) ->
    {unsupported, binary}.

encode_emm_content(plain_nas_message, #{message_type := MsgType} = Msg) ->
    case encode_emm_msg(MsgType, Msg) of
        {unsupported, not_implemented} ->
            {unsupported, {plain_nas_message, emm_msg, MsgType}};
        Bin ->
            MT = compose_msg_type(MsgType),
            <<MT:8/big, Bin/binary>>
    end;
encode_emm_content(service_request, Msg) ->
    encode_emm_msg(service_request, Msg);
encode_emm_content(Unknown_SHT, #{message_authentication_code := MAC, sequence_number := SN, nas_message := Msg}) ->
    case encode(Msg) of
        {unsupported, not_implemented} ->
            {unsupported, {unknown_msg_type, Unknown_SHT}};
        Bin ->
            <<MAC:4/binary, SN:1/binary, Bin/binary>>
    end;
encode_emm_content(_, _) ->
    {unsupported, msg}.

decode_esm_content(<<PTI:1/binary, MT:8/big, OIE/binary>>) ->
    MsgType = parse_msg_type(MT),
    case decode_esm_msg(MsgType, OIE) of
        unsupported ->
            {unsupported, {esm_msg, MT}};
        Msg ->
            Msg#{procedure_transaction_identity => PTI,
                 message_type => MsgType}
    end;
decode_esm_content(_) ->
    {unsupported, binary}.

encode_esm_content(#{message_type := MsgType} = Msg) ->
    case encode_esm_msg(MsgType, Msg) of
        {unsupported, not_implemented} ->
            {unsupported, {esm_msg, MsgType}};
        Bin ->
            MT = compose_msg_type(MsgType),
            PTI = maps:get(procedure_transaction_identity, Msg),
            <<PTI:1/binary, MT:8/big, Bin/binary>>
    end;
encode_esm_content(_) ->
    {unsupported, msg}.

%% 9.3.1 Security header type
parse_security_header_type(?NAS_SHT_PLAIN_NAS_MESSAGE) -> plain_nas_message;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED) -> integrity_protected;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED) -> integrity_protected_ciphered;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_EPS_SECURITY) -> integrity_protected_eps_security;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_EPS_SECURITY) -> integrity_protected_ciphered_eps_security;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_PARTIALLY_CIPHERED_NAS_MESSAGE) -> integrity_protected_partially_ciphered_nas_message;
parse_security_header_type(?NAS_SHT_SERVICE_REQUEST) -> service_request;
parse_security_header_type(_) ->
    %% not used and should be interpreted as 1100
    service_request.

compose_security_header_type(plain_nas_message) -> ?NAS_SHT_PLAIN_NAS_MESSAGE;
compose_security_header_type(integrity_protected) -> ?NAS_SHT_INTEGRITY_PROTECTED;
compose_security_header_type(integrity_protected_ciphered) -> ?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED;
compose_security_header_type(integrity_protected_eps_security) -> ?NAS_SHT_INTEGRITY_PROTECTED_EPS_SECURITY;
compose_security_header_type(integrity_protected_ciphered_eps_security) -> ?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_EPS_SECURITY;
compose_security_header_type(integrity_protected_partially_ciphered_nas_message) -> ?NAS_SHT_INTEGRITY_PROTECTED_PARTIALLY_CIPHERED_NAS_MESSAGE;
compose_security_header_type(service_request) -> ?NAS_SHT_SERVICE_REQUEST;
compose_security_header_type(_) -> ?NAS_SHT_SERVICE_REQUEST.

%% 9.8 Message type
parse_msg_type(?NAS_MSGT_ATTACH_REQUEST) -> attach_request;
parse_msg_type(?NAS_MSGT_ATTACH_ACCEPT) -> attach_accept;
parse_msg_type(?NAS_MSGT_ATTACH_COMPLETE) -> attach_complete;
parse_msg_type(?NAS_MSGT_ATTACH_REJECT) -> attach_reject;
parse_msg_type(?NAS_MSGT_DETACH_REQUEST) -> detach_request;
parse_msg_type(?NAS_MSGT_DETACH_ACCEPT) -> detach_accept;
parse_msg_type(?NAS_MSGT_TRACKING_AREA_UPDATE_REQUEST) -> tracking_area_update_request;
parse_msg_type(?NAS_MSGT_TRACKING_AREA_UPDATE_ACCEPT) -> tracking_area_update_accept;
parse_msg_type(?NAS_MSGT_TRACKING_AREA_UPDATE_COMPLETE) -> tracking_area_update_complete;
parse_msg_type(?NAS_MSGT_TRACKING_AREA_UPDATE_REJECT) -> tracking_area_update_reject;
parse_msg_type(?NAS_MSGT_EXTENDED_SERVICE_REQUEST) -> extended_service_request;
parse_msg_type(?NAS_MSGT_CONTROL_PLANE_SERVICE_REQUEST) -> control_plane_service_request;
parse_msg_type(?NAS_MSGT_SERVICE_REJECT) -> service_reject;
parse_msg_type(?NAS_MSGT_SERVICE_ACCEPT) -> service_accept;
parse_msg_type(?NAS_MSGT_GUTI_REALLOCATION_COMMAND) -> guti_reallocation_command;
parse_msg_type(?NAS_MSGT_GUTI_REALLOCATION_COMPLETE) -> guti_reallocation_complete;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_REQUEST) -> authentication_request;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_RESPONSE) -> authentication_response;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_REJECT) -> authentication_reject;
parse_msg_type(?NAS_MSGT_AUTHENTICATION_FAILURE) -> authentication_failure;
parse_msg_type(?NAS_MSGT_IDENTITY_REQUEST) -> identity_request;
parse_msg_type(?NAS_MSGT_IDENTITY_RESPONSE) -> identity_response;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_COMMAND) -> security_mode_command;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_COMPLETE) -> security_mode_complete;
parse_msg_type(?NAS_MSGT_SECURITY_MODE_REJECT) -> security_mode_reject;
parse_msg_type(?NAS_MSGT_EMM_STATUS) -> emm_status;
parse_msg_type(?NAS_MSGT_EMM_INFORMATION) -> emm_information;
parse_msg_type(?NAS_MSGT_DOWNLINK_NAS_TRANSPORT) -> downlink_nas_transport;
parse_msg_type(?NAS_MSGT_UPLINK_NAS_TRANSPORT) -> uplink_nas_transport;
parse_msg_type(?NAS_MSGT_CS_SERVICE_NOTIFICATION) -> cs_service_notification;
parse_msg_type(?NAS_MSGT_DOWNLINK_GENERIC_NAS_TRANSPORT) -> downlink_generic_nas_transport;
parse_msg_type(?NAS_MSGT_UPLINK_GENERIC_NAS_TRANSPORT) -> uplink_generic_nas_transport;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REQUEST) -> activate_default_eps_bearer_context_request;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_ACCEPT) -> activate_default_eps_bearer_context_accept;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REJECT) -> activate_default_eps_bearer_context_reject;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REQUEST) -> activate_dedicated_eps_bearer_context_request;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_ACCEPT) -> activate_dedicated_eps_bearer_context_accept;
parse_msg_type(?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REJECT) -> activate_dedicated_eps_bearer_context_reject;
parse_msg_type(?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REQUEST) -> modify_eps_bearer_context_request;
parse_msg_type(?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_ACCEPT) -> modify_eps_bearer_context_accept;
parse_msg_type(?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REJECT) -> modify_eps_bearer_context_reject;
parse_msg_type(?NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_REQUEST) -> deactivate_eps_bearer_context_request;
parse_msg_type(?NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_ACCEPT) -> deactivate_eps_bearer_context_accept;
parse_msg_type(?NAS_MSGT_PDN_CONNECTIVITY_REQUEST) -> pdn_connectivity_request;
parse_msg_type(?NAS_MSGT_PDN_CONNECTIVITY_REJECT) -> pdn_connectivity_reject;
parse_msg_type(?NAS_MSGT_PDN_DISCONNECT_REQUEST) -> pdn_disconnect_request;
parse_msg_type(?NAS_MSGT_PDN_DISCONNECT_REJECT) -> pdn_disconnect_reject;
parse_msg_type(?NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REQUEST) -> bearer_resource_allocation_request;
parse_msg_type(?NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REJECT) -> bearer_resource_allocation_reject;
parse_msg_type(?NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REQUEST) -> bearer_resource_modification_request;
parse_msg_type(?NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REJECT) -> bearer_resource_modification_reject;
parse_msg_type(?NAS_MSGT_ESM_INFORMATION_REQUEST) -> esm_information_request;
parse_msg_type(?NAS_MSGT_ESM_INFORMATION_RESPONSE) -> esm_information_response;
parse_msg_type(?NAS_MSGT_NOTIFICATION) -> notification;
parse_msg_type(?NAS_MSGT_ESM_DUMMY_MESSAGE) -> esm_dummy_message;
parse_msg_type(?NAS_MSGT_ESM_STATUS) -> esm_status;
parse_msg_type(?NAS_MSGT_REMOTE_UE_REPORT) -> remote_ue_report;
parse_msg_type(?NAS_MSGT_REMOTE_UE_REPORT_RESPONSE) -> remote_ue_report_response;
parse_msg_type(?NAS_MSGT_ESM_DATA_TRANSPORT) -> esm_data_transport;
parse_msg_type(_) ->
    unsupported.

compose_msg_type(attach_request) -> ?NAS_MSGT_ATTACH_REQUEST;
compose_msg_type(attach_accept) -> ?NAS_MSGT_ATTACH_ACCEPT;
compose_msg_type(attach_complete) -> ?NAS_MSGT_ATTACH_COMPLETE;
compose_msg_type(attach_reject) -> ?NAS_MSGT_ATTACH_REJECT;
compose_msg_type(detach_request) -> ?NAS_MSGT_DETACH_REQUEST;
compose_msg_type(detach_accept) -> ?NAS_MSGT_DETACH_ACCEPT;
compose_msg_type(tracking_area_update_request) -> ?NAS_MSGT_TRACKING_AREA_UPDATE_REQUEST;
compose_msg_type(tracking_area_update_accept) -> ?NAS_MSGT_TRACKING_AREA_UPDATE_ACCEPT;
compose_msg_type(tracking_area_update_complete) -> ?NAS_MSGT_TRACKING_AREA_UPDATE_COMPLETE;
compose_msg_type(tracking_area_update_reject) -> ?NAS_MSGT_TRACKING_AREA_UPDATE_REJECT;
compose_msg_type(extended_service_request) -> ?NAS_MSGT_EXTENDED_SERVICE_REQUEST;
compose_msg_type(control_plane_service_request) -> ?NAS_MSGT_CONTROL_PLANE_SERVICE_REQUEST;
compose_msg_type(service_reject) -> ?NAS_MSGT_SERVICE_REJECT;
compose_msg_type(service_accept) -> ?NAS_MSGT_SERVICE_ACCEPT;
compose_msg_type(guti_reallocation_command) -> ?NAS_MSGT_GUTI_REALLOCATION_COMMAND;
compose_msg_type(guti_reallocation_complete) -> ?NAS_MSGT_GUTI_REALLOCATION_COMPLETE;
compose_msg_type(authentication_request) -> ?NAS_MSGT_AUTHENTICATION_REQUEST;
compose_msg_type(authentication_response) -> ?NAS_MSGT_AUTHENTICATION_RESPONSE;
compose_msg_type(authentication_reject) -> ?NAS_MSGT_AUTHENTICATION_REJECT;
compose_msg_type(authentication_failure) -> ?NAS_MSGT_AUTHENTICATION_FAILURE;
compose_msg_type(identity_request) -> ?NAS_MSGT_IDENTITY_REQUEST;
compose_msg_type(identity_response) -> ?NAS_MSGT_IDENTITY_RESPONSE;
compose_msg_type(security_mode_command) -> ?NAS_MSGT_SECURITY_MODE_COMMAND;
compose_msg_type(security_mode_complete) -> ?NAS_MSGT_SECURITY_MODE_COMPLETE;
compose_msg_type(security_mode_reject) -> ?NAS_MSGT_SECURITY_MODE_REJECT;
compose_msg_type(emm_status) -> ?NAS_MSGT_EMM_STATUS;
compose_msg_type(emm_information) -> ?NAS_MSGT_EMM_INFORMATION;
compose_msg_type(downlink_nas_transport) -> ?NAS_MSGT_DOWNLINK_NAS_TRANSPORT;
compose_msg_type(uplink_nas_transport) -> ?NAS_MSGT_UPLINK_NAS_TRANSPORT;
compose_msg_type(cs_service_notification) -> ?NAS_MSGT_CS_SERVICE_NOTIFICATION;
compose_msg_type(downlink_generic_nas_transport) -> ?NAS_MSGT_DOWNLINK_GENERIC_NAS_TRANSPORT;
compose_msg_type(uplink_generic_nas_transport) -> ?NAS_MSGT_UPLINK_GENERIC_NAS_TRANSPORT;
compose_msg_type(activate_default_eps_bearer_context_request) -> ?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REQUEST;
compose_msg_type(activate_default_eps_bearer_context_accept) -> ?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_ACCEPT;
compose_msg_type(activate_default_eps_bearer_context_reject) -> ?NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REJECT;
compose_msg_type(activate_dedicated_eps_bearer_context_request) -> ?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REQUEST;
compose_msg_type(activate_dedicated_eps_bearer_context_accept) -> ?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_ACCEPT;
compose_msg_type(activate_dedicated_eps_bearer_context_reject) -> ?NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REJECT;
compose_msg_type(modify_eps_bearer_context_request) -> ?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REQUEST;
compose_msg_type(modify_eps_bearer_context_accept) -> ?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_ACCEPT;
compose_msg_type(modify_eps_bearer_context_reject) -> ?NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REJECT;
compose_msg_type(deactivate_eps_bearer_context_request) -> ?NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_REQUEST;
compose_msg_type(deactivate_eps_bearer_context_accept) -> ?NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_ACCEPT;
compose_msg_type(pdn_connectivity_request) -> ?NAS_MSGT_PDN_CONNECTIVITY_REQUEST;
compose_msg_type(pdn_connectivity_reject) -> ?NAS_MSGT_PDN_CONNECTIVITY_REJECT;
compose_msg_type(pdn_disconnect_request) -> ?NAS_MSGT_PDN_DISCONNECT_REQUEST;
compose_msg_type(pdn_disconnect_reject) -> ?NAS_MSGT_PDN_DISCONNECT_REJECT;
compose_msg_type(bearer_resource_allocation_request) -> ?NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REQUEST;
compose_msg_type(bearer_resource_allocation_reject) -> ?NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REJECT;
compose_msg_type(bearer_resource_modification_request) -> ?NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REQUEST;
compose_msg_type(bearer_resource_modification_reject) -> ?NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REJECT;
compose_msg_type(esm_information_request) -> ?NAS_MSGT_ESM_INFORMATION_REQUEST;
compose_msg_type(esm_information_response) -> ?NAS_MSGT_ESM_INFORMATION_RESPONSE;
compose_msg_type(notification) -> ?NAS_MSGT_NOTIFICATION;
compose_msg_type(esm_dummy_message) -> ?NAS_MSGT_ESM_DUMMY_MESSAGE;
compose_msg_type(esm_status) -> ?NAS_MSGT_ESM_STATUS;
compose_msg_type(remote_ue_report) -> ?NAS_MSGT_REMOTE_UE_REPORT;
compose_msg_type(remote_ue_report_response) -> ?NAS_MSGT_REMOTE_UE_REPORT_RESPONSE;
compose_msg_type(esm_data_transport) -> ?NAS_MSGT_ESM_DATA_TRANSPORT.

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
               esm_message_container => decode(EsmMessageContainer)
              };
decode_emm_msg(attach_complete, Bin0) ->
    {EsmMessageContainer, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_message_container => decode(EsmMessageContainer)
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
               esm_message_container => decode(EsmMessageContainer)
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
    Optionals;
decode_emm_msg(_, _) ->
    unsupported.

encode_emm_msg(attach_accept, #{eps_attach_result := EpsAttachResult,
                                t3412_value := T3412Value,
                                tai_list := TaiList,
                                esm_message_container := EsmMessageContainer} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(EpsAttachResult, half, <<>>),
    Bin3 = otc_l3_codec:encode_v(T3412Value, 1, <<>>),
    Bin4 = otc_l3_codec:encode_lv(TaiList, <<>>),
    Bin5 = otc_l3_codec:encode_lve(encode(EsmMessageContainer), <<>>),
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
    Bin1 = otc_l3_codec:encode_lve(encode(EsmMessageContainer), <<>>),
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
    Bin5 = otc_l3_codec:encode_lve(encode(EsmMessageContainer), <<>>),
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
    OptBin;
encode_emm_msg(_, _) ->
    {unsupported, not_implemented}.


decode_esm_msg(activate_dedicated_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(activate_dedicated_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(activate_dedicated_eps_bearer_context_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {EpsQos, Bin3} = otc_l3_codec:decode_lv(Bin2),
    {Tft, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               eps_qos => EpsQos,
               tft => Tft
              };
decode_esm_msg(activate_default_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(activate_default_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(activate_default_eps_bearer_context_request, Bin0) ->
    {EpsQos, Bin1} = otc_l3_codec:decode_lv(Bin0),
    {AccessPointName, Bin2} = otc_l3_codec:decode_lv(Bin1),
    {PdnAddress, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {connectivity_type, 16#B, tv, 1},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {control_plane_only_indication, 16#9, tv, 1},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#6E, tlv, 4},
            {extended_apn_ambr, 16#5F, tlv, 8}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{eps_qos => EpsQos,
               access_point_name => AccessPointName,
               pdn_address => PdnAddress
              };
decode_esm_msg(bearer_resource_allocation_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_allocation_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {TrafficFlowAggregate, Bin3} = otc_l3_codec:decode_lv(Bin2),
    {RequiredTrafficFlowQos, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               traffic_flow_aggregate => TrafficFlowAggregate,
               required_traffic_flow_qos => RequiredTrafficFlowQos
              };
decode_esm_msg(bearer_resource_modification_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_modification_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentityForPacketFilter, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {TrafficFlowAggregate, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{required_traffic_flow_qos, 16#5B, tlv, {3, 15}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{eps_bearer_identity_for_packet_filter => EpsBearerIdentityForPacketFilter,
               traffic_flow_aggregate => TrafficFlowAggregate
              };
decode_esm_msg(deactivate_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(deactivate_eps_bearer_context_request, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {t3396_value, 16#37, tlv, 3},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(esm_dummy_message, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_information_request, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_information_response, Bin0) ->
    Opts = [{access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_status, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(modify_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_request, Bin0) ->
    Opts = [{new_eps_qos, 16#5B, tlv, {3, 15}},
            {tft, 16#36, tlv, {3, 257}},
            {new_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_apn_ambr, 16#5F, tlv, 8},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(notification, Bin0) ->
    {NotificationIndicator, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{notification_indicator => NotificationIndicator
              };
decode_esm_msg(pdn_connectivity_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(pdn_connectivity_request, Bin0) ->
    {PdnType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {RequestType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{esm_information_transfer_flag, 16#D, tv, 1},
            {access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{request_type => RequestType,
               pdn_type => PdnType
              };
decode_esm_msg(pdn_disconnect_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(pdn_disconnect_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity
              };
decode_esm_msg(remote_ue_report, Bin0) ->
    Opts = [{remote_ue_context_connected, 16#79, tlve, {3, 65538}},
            {remote_ue_context_disconnected, 16#7A, tlve, {3, 65538}},
            {prose_key_management_function_address, 16#6F, tlv, {3, 19}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(remote_ue_report_response, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_data_transport, Bin0) ->
    {UserDataContainer, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [{release_assistance_indication, 16#F, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{user_data_container => UserDataContainer
              };
decode_esm_msg(_, _) ->
    unsupported.

encode_esm_msg(activate_dedicated_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(activate_dedicated_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(activate_dedicated_eps_bearer_context_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity, eps_qos := EpsQos, tft := Tft} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(EpsQos, <<>>),
    Bin4 = otc_l3_codec:encode_lv(Tft, <<>>),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_esm_msg(activate_default_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(activate_default_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(activate_default_eps_bearer_context_request, #{eps_qos := EpsQos, access_point_name := AccessPointName, pdn_address := PdnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(EpsQos, <<>>),
    Bin2 = otc_l3_codec:encode_lv(AccessPointName, <<>>),
    Bin3 = otc_l3_codec:encode_lv(PdnAddress, <<>>),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {connectivity_type, 16#B, tv, 1},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {control_plane_only_indication, 16#9, tv, 1},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#6E, tlv, 4},
            {extended_apn_ambr, 16#5F, tlv, 8}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_allocation_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_allocation_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity, traffic_flow_aggregate := TrafficFlowAggregate, required_traffic_flow_qos := RequiredTrafficFlowQos} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(TrafficFlowAggregate, <<>>),
    Bin4 = otc_l3_codec:encode_lv(RequiredTrafficFlowQos, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_modification_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_modification_request, #{eps_bearer_identity_for_packet_filter := EpsBearerIdentityForPacketFilter, traffic_flow_aggregate := TrafficFlowAggregate} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(EpsBearerIdentityForPacketFilter, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(TrafficFlowAggregate, <<>>),
    Opts = [{required_traffic_flow_qos, 16#5B, tlv, {3, 15}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_esm_msg(deactivate_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(deactivate_eps_bearer_context_request, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {t3396_value, 16#37, tlv, 3},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(esm_dummy_message, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_information_request, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_information_response, #{} = Msg) ->
    Opts = [{access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_status, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(modify_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(modify_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(modify_eps_bearer_context_request, #{} = Msg) ->
    Opts = [{new_eps_qos, 16#5B, tlv, {3, 15}},
            {tft, 16#36, tlv, {3, 257}},
            {new_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_apn_ambr, 16#5F, tlv, 8},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(notification, #{notification_indicator := NotificationIndicator} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(NotificationIndicator, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_connectivity_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_connectivity_request, #{request_type := RequestType, pdn_type := PdnType} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(PdnType, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(RequestType, half, <<>>),
    Opts = [{esm_information_transfer_flag, 16#D, tv, 1},
            {access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_disconnect_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_disconnect_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_esm_msg(remote_ue_report, #{} = Msg) ->
    Opts = [{remote_ue_context_connected, 16#79, tlve, {3, 65538}},
            {remote_ue_context_disconnected, 16#7A, tlve, {3, 65538}},
            {prose_key_management_function_address, 16#6F, tlv, {3, 19}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(remote_ue_report_response, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_data_transport, #{user_data_container := UserDataContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(UserDataContainer, <<>>),
    Opts = [{release_assistance_indication, 16#F, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(_, _) ->
    {unsupported, not_implemented}.
