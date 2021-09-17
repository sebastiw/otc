%% 3GPP TS 24.301 version 16.8.0
-module(sigerl_nas_codec).

-include("include/nas.hrl").
-include("include/l3.hrl").

-export([decode/1, encode/1]).

decode(<<EBI_SHT:4, PD:4, Rest/binary>>) ->
    ProtocolDiscriminator = sigerl_l3_codec:parse_protocol_discriminator(PD),
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            % Security Header Type
            SecurityHeaderType = parse_security_header_type(EBI_SHT),
            parse_emm_content(SecurityHeaderType, Rest);
        eps_session_management_messages ->
            % EPS Bearer Identity
            parse_esm_content(EBI_SHT, Rest);
        _ ->
            undefined
    end.

encode(Nas) ->
    Nas.

parse_emm_content(_, <<>>) ->
    undefined;
parse_emm_content(plain_nas_message, Bin) ->
    <<MT:1/binary, OIE/binary>> = Bin,
    MsgType = parse_msg_type(MT),
    decode_emm_msg(MsgType, OIE);
parse_emm_content(_, Bin) ->
    <<MAC:3/binary, SN:1/binary, NMSG/binary>> = Bin,
    ok.

parse_esm_content(EPSBearerIdentity, Bin) ->
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
    {{AttachRes, Spare}, Bin1} = ossie_l3_codec:parse_v(both, Bin0),
    {T3412, Bin1} = ossie_l3_codec:parse_v(1, Bin0),
    {TaiList, Bin2} = ossie_l3_codec:parse_lv(Bin1),
    {ESMMC, Bin3} = ossie_l3_codec:parse_lve(Bin2),
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
            {negotiated_drx_parameter_in_nb_s1_mode, 16#36, tlv, 3}
           ],
    {Optionals, _Unknown} = sigerl_l3_codec:decode_iei_list(Bin3, Opts),
    #nas_msg_attach_accept{
       eps_attach_result = AttachRes,
       spare_half_octet = Spare,
       t3412_value = T3412,
       tai_list = TaiList,
       esm_message_container = ESMMC,
       guti = maps:get(guti, Optionals, undefined),
       location_area_identification = maps:get(location_area_identification, Optionals, undefined),
       ms_identity = maps:get(ms_identity, Optionals, undefined),
       emm_cause = maps:get(emm_cause, Optionals, undefined),
       t3402_value = maps:get(t3402_value, Optionals, undefined),
       t3423_value = maps:get(t3423_value, Optionals, undefined),
       equivalent_plmns = maps:get(equivalent_plmns, Optionals, undefined),
       emergency_number_list = maps:get(emergency_number_list, Optionals, undefined),
       eps_network_feature_support = maps:get(eps_network_feature_support, Optionals, undefined),
       additional_update_result = maps:get(additional_update_result, Optionals, undefined),
       t3412_extended_value = maps:get(t3412_extended_value, Optionals, undefined),
       t3324_value = maps:get(t3324_value, Optionals, undefined),
       extended_drx_parameters = maps:get(extended_drx_parameters, Optionals, undefined),
       dcn_id = maps:get(dcn_id, Optionals, undefined),
       sms_services_status = maps:get(sms_services_status, Optionals, undefined),
       non_3gpp_nw_provided_policies = maps:get(non_3gpp_nw_provided_policies, Optionals, undefined),
       t3448_value = maps:get(t3448_value, Optionals, undefined),
       network_policy = maps:get(network_policy, Optionals, undefined),
       t3447_value = maps:get(t3447_value, Optionals, undefined),
       extended_emergency_number_list = maps:get(extended_emergency_number_list, Optionals, undefined),
       ciphering_key_data = maps:get(ciphering_key_data, Optionals, undefined),
       ue_radio_capability_id = maps:get(ue_radio_capability_id, Optionals, undefined),
       ue_radio_capability_id_deletion_indication = maps:get(ue_radio_capability_id_deletion_indication, Optionals, undefined),
       negotiated_wus_assistance_information = maps:get(negotiated_wus_assistance_information, Optionals, undefined),
       negotiated_drx_parameter_in_nb_s1_mode = maps:get(negotiated_drx_parameter_in_nb_s1_mode, Optionals, undefined)
      }.
decode_esm_msg(_MsgType, _Bin) ->
    ok.
