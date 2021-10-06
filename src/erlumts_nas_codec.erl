%% 3GPP TS 24.301 version 16.8.0
-module(erlumts_nas_codec).

-include("include/nas.hrl").
-include("include/l3.hrl").

-export([decode/1, encode/1]).

-spec decode(binary()) -> map() | unsupported | undefined.
decode(<<EBI_SHT:4, PD:4, Rest/binary>>) ->
    ProtocolDiscriminator = erlumts_l3_codec:parse_protocol_discriminator(PD),
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            %% Security Header Type
            SecurityHeaderType = parse_security_header_type(EBI_SHT),
            decode_emm_content(SecurityHeaderType, Rest);
        eps_session_management_messages ->
            %% EPS Bearer Identity
            decode_esm_content(EBI_SHT, Rest);
        _ ->
            unsupported
    end.

-spec encode(map()) -> binary().
encode(Nas) ->
    <<>>.

decode_emm_content(_, <<>>) ->
    undefined;
decode_emm_content(plain_nas_message, Bin) ->
    <<MT:1/binary, OIE/binary>> = Bin,
    MsgType = parse_msg_type(MT),
    decode_emm_msg(MsgType, OIE);
decode_emm_content(_, Bin) ->
    <<MAC:3/binary, SN:1/binary, NMSG/binary>> = Bin,
    undefined.

decode_esm_content(EPSBearerIdentity, Bin) ->
    <<PTI:1/binary, MT:1/binary, OIE/binary>> = Bin,
    MsgType = parse_msg_type(MT),
    decode_esm_msg(MsgType, OIE).

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
parse_msg_type(?NAS_MSGT_ESM_DATA_TRANSPORT) -> esm_data_transport.

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
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AttachAcceptMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EpsAttachResult, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {SpareHalfOctet, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {T3412Value, Bin6} = erlumts_l3_codec:decode_v(Bin5, 1),
    {TaiList, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    {EsmMessageContainer, Bin8} = erlumts_l3_codec:decode_lve(Bin7),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin8, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               attach_accept_message_identity => AttachAcceptMessageIdentity,
               eps_attach_result => EpsAttachResult,
               spare_half_octet => SpareHalfOctet,
               t3412_value => T3412Value,
               tai_list => TaiList,
               esm_message_container => EsmMessageContainer
              };
decode_emm_msg(attach_complete, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AttachCompleteMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmMessageContainer, Bin4} = erlumts_l3_codec:decode_lve(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               attach_complete_message_identity => AttachCompleteMessageIdentity,
               esm_message_container => EsmMessageContainer
              };
decode_emm_msg(attach_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AttachRejectMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{esm_message_container, 16#78, tlve, {6, n}},
            {t3346_value, 16#5F, tlv, 3},
            {t3402_value, 16#16, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               attach_reject_message_identity => AttachRejectMessageIdentity,
               emm_cause => EmmCause
              };
decode_emm_msg(attach_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AttachRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EpsAttachType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {EpsMobileIdentity, Bin6} = erlumts_l3_codec:decode_lv(Bin5),
    {UeNetworkCapability, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    {EsmMessageContainer, Bin8} = erlumts_l3_codec:decode_lve(Bin7),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin8, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               attach_request_message_identity => AttachRequestMessageIdentity,
               eps_attach_type => EpsAttachType,
               nas_key_set_identifier => NasKeySetIdentifier,
               eps_mobile_identity => EpsMobileIdentity,
               ue_network_capability => UeNetworkCapability,
               esm_message_container => EsmMessageContainer
              };
decode_emm_msg(authentication_failure, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AuthenticationFailureMessageType, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{authentication_failure_parameter, 16#30, tlv, 16}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               authentication_failure_message_type => AuthenticationFailureMessageType,
               emm_cause => EmmCause
              };
decode_emm_msg(authentication_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AuthenticationRejectMessageType, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               authentication_reject_message_type => AuthenticationRejectMessageType
              };
decode_emm_msg(authentication_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AuthenticationRequestMessageType, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {NasKeySetIdentifierasme, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {SpareHalfOctet, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {AuthenticationParameterRandEpsChallenge, Bin6} = erlumts_l3_codec:decode_v(Bin5, 16),
    {AuthenticationParameterAutnEpsChallenge, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin7, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               authentication_request_message_type => AuthenticationRequestMessageType,
               nas_key_set_identifierasme => NasKeySetIdentifierasme,
               spare_half_octet => SpareHalfOctet,
               authentication_parameter_rand_eps_challenge => AuthenticationParameterRandEpsChallenge,
               authentication_parameter_autn_eps_challenge => AuthenticationParameterAutnEpsChallenge
              };
decode_emm_msg(authentication_response, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {AuthenticationResponseMessageType, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {AuthenticationResponseParameter, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               authentication_response_message_type => AuthenticationResponseMessageType,
               authentication_response_parameter => AuthenticationResponseParameter
              };
decode_emm_msg(cs_service_notification, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {CsServiceNotificationMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {PagingIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{cli, 16#60, tlv, {3, 14}},
            {ss_code, 16#61, tv, 2},
            {lcs_indicator, 16#62, tv, 2},
            {lcs_client_identity, 16#63, tlv, {3, 257}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               cs_service_notification_message_identity => CsServiceNotificationMessageIdentity,
               paging_identity => PagingIdentity
              };
decode_emm_msg(detach_accept_ue_originating_detach, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DetachAcceptMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               detach_accept_message_identity => DetachAcceptMessageIdentity
              };
decode_emm_msg(detach_accept_ue_terminated_detach, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DetachAcceptMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               detach_accept_message_identity => DetachAcceptMessageIdentity
              };
decode_emm_msg(detach_request_ue_originating_detach, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DetachRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {DetachType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {EpsMobileIdentity, Bin6} = erlumts_l3_codec:decode_lv(Bin5),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               detach_request_message_identity => DetachRequestMessageIdentity,
               detach_type => DetachType,
               nas_key_set_identifier => NasKeySetIdentifier,
               eps_mobile_identity => EpsMobileIdentity
              };
decode_emm_msg(detach_request_ue_terminated_detach, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DetachRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {DetachType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {SpareHalfOctet, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    Opts = [{emm_cause, 16#53, tv, 2}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               detach_request_message_identity => DetachRequestMessageIdentity,
               detach_type => DetachType,
               spare_half_octet => SpareHalfOctet
              };
decode_emm_msg(downlink_nas_transport, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DownlinkNasTransportMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {NasMessageContainer, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               downlink_nas_transport_message_identity => DownlinkNasTransportMessageIdentity,
               nas_message_container => NasMessageContainer
              };
decode_emm_msg(emm_information, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {EmmInformationMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [{full_name_for_network, 16#43, tlv, {3, n}},
            {short_name_for_network, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_local_time_zone, 16#47, tv, 8},
            {network_daylight_saving_time, 16#49, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               emm_information_message_identity => EmmInformationMessageIdentity
              };
decode_emm_msg(emm_status, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {EmmStatusMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               emm_status_message_identity => EmmStatusMessageIdentity,
               emm_cause => EmmCause
              };
decode_emm_msg(extended_service_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ExtendedServiceRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ServiceType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {MTmsi, Bin6} = erlumts_l3_codec:decode_lv(Bin5),
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               extended_service_request_message_identity => ExtendedServiceRequestMessageIdentity,
               service_type => ServiceType,
               nas_key_set_identifier => NasKeySetIdentifier,
               m_tmsi => MTmsi
              };
decode_emm_msg(guti_reallocation_command, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {GutiReallocationCommandMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {Guti, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [{tai_list, 16#54, tlv, {8, 98}},
            {dcn_id, 16#65, tlv, 4},
            {ue_radio_capability_id, 16#66, tlv, {3, n}},
            {ue_radio_capability_id_deletion_indication, 16#B, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               guti_reallocation_command_message_identity => GutiReallocationCommandMessageIdentity,
               guti => Guti
              };
decode_emm_msg(guti_reallocation_complete, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {GutiReallocationCompleteMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               guti_reallocation_complete_message_identity => GutiReallocationCompleteMessageIdentity
              };
decode_emm_msg(identity_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {IdentityRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {IdentityType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {SpareHalfOctet, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               identity_request_message_identity => IdentityRequestMessageIdentity,
               identity_type => IdentityType,
               spare_half_octet => SpareHalfOctet
              };
decode_emm_msg(identity_response, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {IdentityResponseMessage, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {MobileIdentity, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               identity_response_message => IdentityResponseMessage,
               mobile_identity => MobileIdentity
              };
decode_emm_msg(security_mode_command, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {SecurityModeCommandMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {SelectedNasSecurityAlgorithms, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {SpareHalfOctet, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    {ReplayedUeSecurityCapabilities, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    Opts = [{imeisv_request, 16#C, tv, 1},
            {replayed_nonceue, 16#55, tv, 5},
            {noncemme, 16#56, tv, 5},
            {hashmme, 16#4F, tlv, 10},
            {replayed_ue_additional_security_capability, 16#6F, tlv, 6},
            {ue_radio_capability_id_request, 16#37, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin7, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               security_mode_command_message_identity => SecurityModeCommandMessageIdentity,
               selected_nas_security_algorithms => SelectedNasSecurityAlgorithms,
               nas_key_set_identifier => NasKeySetIdentifier,
               spare_half_octet => SpareHalfOctet,
               replayed_ue_security_capabilities => ReplayedUeSecurityCapabilities
              };
decode_emm_msg(security_mode_complete, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {SecurityModeCompleteMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [{imeisv, 16#23, tlv, 11},
            {replayed_nas_message_container, 16#79, tlve, {3, n}},
            {ue_radio_capability_id, 16#66, tlv, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               security_mode_complete_message_identity => SecurityModeCompleteMessageIdentity
              };
decode_emm_msg(security_mode_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {SecurityModeRejectMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               security_mode_reject_message_identity => SecurityModeRejectMessageIdentity,
               emm_cause => EmmCause
              };
decode_emm_msg(security_protected_nas_message, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {MessageAuthenticationCode, Bin3} = erlumts_l3_codec:decode_v(Bin2, 4),
    {SequenceNumber, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {NasMessage, Bin5} = erlumts_l3_codec:decode_v(Bin4, {1, n}),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               message_authentication_code => MessageAuthenticationCode,
               sequence_number => SequenceNumber,
               nas_message => NasMessage
              };
decode_emm_msg(service_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ServiceRejectMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               service_reject_message_identity => ServiceRejectMessageIdentity,
               emm_cause => EmmCause
              };
decode_emm_msg(service_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {KsiAndSequenceNumber, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {MessageAuthenticationCodeShort, Bin4} = erlumts_l3_codec:decode_v(Bin3, 2),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               ksi_and_sequence_number => KsiAndSequenceNumber,
               message_authentication_code_short => MessageAuthenticationCodeShort
              };
decode_emm_msg(tracking_area_update_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {TrackingAreaUpdateAcceptMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EpsUpdateResult, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {SpareHalfOctet, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               tracking_area_update_accept_message_identity => TrackingAreaUpdateAcceptMessageIdentity,
               eps_update_result => EpsUpdateResult,
               spare_half_octet => SpareHalfOctet
              };
decode_emm_msg(tracking_area_update_complete, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {TrackingAreaUpdateCompleteMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               tracking_area_update_complete_message_identity => TrackingAreaUpdateCompleteMessageIdentity
              };
decode_emm_msg(tracking_area_update_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {TrackingAreaUpdateRejectMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EmmCause, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{t3346_value, 16#5F, tlv, 3},
            {extended_emm_cause, 16#A, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               tracking_area_update_reject_message_identity => TrackingAreaUpdateRejectMessageIdentity,
               emm_cause => EmmCause
              };
decode_emm_msg(tracking_area_update_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {TrackingAreaUpdateRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EpsUpdateType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {OldGuti, Bin6} = erlumts_l3_codec:decode_lv(Bin5),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               tracking_area_update_request_message_identity => TrackingAreaUpdateRequestMessageIdentity,
               eps_update_type => EpsUpdateType,
               nas_key_set_identifier => NasKeySetIdentifier,
               old_guti => OldGuti
              };
decode_emm_msg(uplink_nas_transport, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {UplinkNasTransportMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {NasMessageContainer, Bin4} = erlumts_l3_codec:decode_lv(Bin3),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               uplink_nas_transport_message_identity => UplinkNasTransportMessageIdentity,
               nas_message_container => NasMessageContainer
              };
decode_emm_msg(downlink_generic_nas_transport, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {DownlinkGenericNasTransportMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {GenericMessageContainerType, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {GenericMessageContainer, Bin5} = erlumts_l3_codec:decode_lve(Bin4),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               downlink_generic_nas_transport_message_identity => DownlinkGenericNasTransportMessageIdentity,
               generic_message_container_type => GenericMessageContainerType,
               generic_message_container => GenericMessageContainer
              };
decode_emm_msg(uplink_generic_nas_transport, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {UplinkGenericNasTransportMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {GenericMessageContainerType, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {GenericMessageContainer, Bin5} = erlumts_l3_codec:decode_lve(Bin4),
    Opts = [{additional_information, 16#65, tlv, {3, n}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               uplink_generic_nas_transport_message_identity => UplinkGenericNasTransportMessageIdentity,
               generic_message_container_type => GenericMessageContainerType,
               generic_message_container => GenericMessageContainer
              };
decode_emm_msg(control_plane_service_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ControlPlaneServiceRequestMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ControlPlaneServiceType, Bin4} = erlumts_l3_codec:decode_v(Bin3, half),
    {NasKeySetIdentifier, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    Opts = [{esm_message_container, 16#78, tlve, {3, n}},
            {nas_message_container, 16#67, tlv, {4, 253}},
            {eps_bearer_context_status, 16#57, tlv, 4},
            {device_properties, 16#D, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               control_plane_service_request_message_identity => ControlPlaneServiceRequestMessageIdentity,
               control_plane_service_type => ControlPlaneServiceType,
               nas_key_set_identifier => NasKeySetIdentifier
              };
decode_emm_msg(service_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {SecurityHeaderType, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ServiceAcceptMessageIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    Opts = [{eps_bearer_context_status, 16#57, tlv, 4},
            {t3448_value, 16#6B, tlv, 3}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               security_header_type => SecurityHeaderType,
               service_accept_message_identity => ServiceAcceptMessageIdentity
              }.

decode_esm_msg(activate_dedicated_eps_bearer_context_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDedicatedEpsBearerContextAcceptMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_dedicated_eps_bearer_context_accept_message_identity => ActivateDedicatedEpsBearerContextAcceptMessageIdentity
              };
decode_esm_msg(activate_dedicated_eps_bearer_context_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDedicatedEpsBearerContextRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_dedicated_eps_bearer_context_reject_message_identity => ActivateDedicatedEpsBearerContextRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(activate_dedicated_eps_bearer_context_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDedicatedEpsBearerContextRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {LinkedEpsBearerIdentity, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {SpareHalfOctet, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    {EpsQos, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    {Tft, Bin8} = erlumts_l3_codec:decode_lv(Bin7),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin8, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_dedicated_eps_bearer_context_request_message_identity => ActivateDedicatedEpsBearerContextRequestMessageIdentity,
               linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               spare_half_octet => SpareHalfOctet,
               eps_qos => EpsQos,
               tft => Tft
              };
decode_esm_msg(activate_default_eps_bearer_context_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDefaultEpsBearerContextAcceptMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_default_eps_bearer_context_accept_message_identity => ActivateDefaultEpsBearerContextAcceptMessageIdentity
              };
decode_esm_msg(activate_default_eps_bearer_context_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDefaultEpsBearerContextRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_default_eps_bearer_context_reject_message_identity => ActivateDefaultEpsBearerContextRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(activate_default_eps_bearer_context_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ActivateDefaultEpsBearerContextRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EpsQos, Bin5} = erlumts_l3_codec:decode_lv(Bin4),
    {AccessPointName, Bin6} = erlumts_l3_codec:decode_lv(Bin5),
    {PdnAddress, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin7, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               activate_default_eps_bearer_context_request_message_identity => ActivateDefaultEpsBearerContextRequestMessageIdentity,
               eps_qos => EpsQos,
               access_point_name => AccessPointName,
               pdn_address => PdnAddress
              };
decode_esm_msg(bearer_resource_allocation_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {BearerResourceAllocationRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               bearer_resource_allocation_reject_message_identity => BearerResourceAllocationRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_allocation_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {BearerResourceAllocationRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {LinkedEpsBearerIdentity, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {SpareHalfOctet, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    {TrafficFlowAggregate, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    {RequiredTrafficFlowQos, Bin8} = erlumts_l3_codec:decode_lv(Bin7),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin8, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               bearer_resource_allocation_request_message_identity => BearerResourceAllocationRequestMessageIdentity,
               linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               spare_half_octet => SpareHalfOctet,
               traffic_flow_aggregate => TrafficFlowAggregate,
               required_traffic_flow_qos => RequiredTrafficFlowQos
              };
decode_esm_msg(bearer_resource_modification_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {BearerResourceModificationRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               bearer_resource_modification_reject_message_identity => BearerResourceModificationRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_modification_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {BearerResourceModificationRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EpsBearerIdentityForPacketFilter, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {SpareHalfOctet, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    {TrafficFlowAggregate, Bin7} = erlumts_l3_codec:decode_lv(Bin6),
    Opts = [{required_traffic_flow_qos, 16#5B, tlv, {3, 15}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin7, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               bearer_resource_modification_request_message_identity => BearerResourceModificationRequestMessageIdentity,
               eps_bearer_identity_for_packet_filter => EpsBearerIdentityForPacketFilter,
               spare_half_octet => SpareHalfOctet,
               traffic_flow_aggregate => TrafficFlowAggregate
              };
decode_esm_msg(deactivate_eps_bearer_context_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {DeactivateEpsBearerContextAcceptMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               deactivate_eps_bearer_context_accept_message_identity => DeactivateEpsBearerContextAcceptMessageIdentity
              };
decode_esm_msg(deactivate_eps_bearer_context_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {DeactivateEpsBearerContextRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {t3396_value, 16#37, tlv, 3},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               deactivate_eps_bearer_context_request_message_identity => DeactivateEpsBearerContextRequestMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(esm_dummy_message, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmDummyMessageMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               esm_dummy_message_message_identity => EsmDummyMessageMessageIdentity
              };
decode_esm_msg(esm_information_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmInformationRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               esm_information_request_message_identity => EsmInformationRequestMessageIdentity
              };
decode_esm_msg(esm_information_response, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmInformationResponseMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               esm_information_response_message_identity => EsmInformationResponseMessageIdentity
              };
decode_esm_msg(esm_status, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmStatusMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               esm_status_message_identity => EsmStatusMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_accept, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ModifyEpsBearerContextAcceptMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               modify_eps_bearer_context_accept_message_identity => ModifyEpsBearerContextAcceptMessageIdentity
              };
decode_esm_msg(modify_eps_bearer_context_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ModifyEpsBearerContextRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               modify_eps_bearer_context_reject_message_identity => ModifyEpsBearerContextRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {ModifyEpsBearerContextRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
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
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               modify_eps_bearer_context_request_message_identity => ModifyEpsBearerContextRequestMessageIdentity
              };
decode_esm_msg(notification, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {NotificationMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {NotificationIndicator, Bin5} = erlumts_l3_codec:decode_lv(Bin4),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               notification_message_identity => NotificationMessageIdentity,
               notification_indicator => NotificationIndicator
              };
decode_esm_msg(pdn_connectivity_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {PdnConnectivityRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               pdn_connectivity_reject_message_identity => PdnConnectivityRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(pdn_connectivity_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {PdnConnectivityRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {RequestType, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {PdnType, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    Opts = [{esm_information_transfer_flag, 16#D, tv, 1},
            {access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               pdn_connectivity_request_message_identity => PdnConnectivityRequestMessageIdentity,
               request_type => RequestType,
               pdn_type => PdnType
              };
decode_esm_msg(pdn_disconnect_reject, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {PdnDisconnectRejectMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {EsmCause, Bin5} = erlumts_l3_codec:decode_v(Bin4, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               pdn_disconnect_reject_message_identity => PdnDisconnectRejectMessageIdentity,
               esm_cause => EsmCause
              };
decode_esm_msg(pdn_disconnect_request, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {PdnDisconnectRequestMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {LinkedEpsBearerIdentity, Bin5} = erlumts_l3_codec:decode_v(Bin4, half),
    {SpareHalfOctet, Bin6} = erlumts_l3_codec:decode_v(Bin5, half),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               pdn_disconnect_request_message_identity => PdnDisconnectRequestMessageIdentity,
               linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               spare_half_octet => SpareHalfOctet
              };
decode_esm_msg(remote_ue_report, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {RemoteUeReportMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [{remote_ue_context_connected, 16#79, tlve, {3, 65538}},
            {remote_ue_context_disconnected, 16#7A, tlve, {3, 65538}},
            {prose_key_management_function_address, 16#6F, tlv, {3, 19}}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               remote_ue_report_message_identity => RemoteUeReportMessageIdentity
              };
decode_esm_msg(remote_ue_report_response, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {RemoteUeReportResponseMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    Opts = [],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               remote_ue_report_response_message_identity => RemoteUeReportResponseMessageIdentity
              };
decode_esm_msg(esm_data_transport, Bin0) ->
    {ProtocolDiscriminator, Bin1} = erlumts_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentity, Bin2} = erlumts_l3_codec:decode_v(Bin1, half),
    {ProcedureTransactionIdentity, Bin3} = erlumts_l3_codec:decode_v(Bin2, 1),
    {EsmDataTransportMessageIdentity, Bin4} = erlumts_l3_codec:decode_v(Bin3, 1),
    {UserDataContainer, Bin5} = erlumts_l3_codec:decode_lve(Bin4),
    Opts = [{release_assistance_indication, 16#F, tv, 1}],
    {Optionals, _Unknown} = erlumts_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{protocol_discriminator => ProtocolDiscriminator,
               eps_bearer_identity => EpsBearerIdentity,
               procedure_transaction_identity => ProcedureTransactionIdentity,
               esm_data_transport_message_identity => EsmDataTransportMessageIdentity,
               user_data_container => UserDataContainer
              }.
