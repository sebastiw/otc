-module(otc_nas_eps).
-behaviour(otc_codec).

-include("include/nas_eps.hrl").
-include("include/l3.hrl").

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1]).

-export([parse_msg_type/1,
         compose_msg_type/1
        ]).

spec() ->
    "3GPP TS 24.301 version 16.8.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode({Map, <<>>});
codec({Map, PDU}) when is_map(Map), is_binary(PDU) ->
    encode({Map, PDU}).

-spec next(map()) -> '$stop' | {ok, nas_eps | nas_eps_emm | nas_eps_esm}.
next(#{protocol_discriminator := eps_mobility_management_messages}) -> {ok, nas_eps_emm};
next(#{protocol_discriminator := eps_session_management_messages}) -> {ok, nas_eps_esm};
next(_) -> '$stop'.

decode(<<EBI_SHT:4, PD:4, Rest/binary>>) ->
    ProtocolDiscriminator = otc_l3_codec:parse_protocol_discriminator(PD),
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            %% Security Header Type
            SecurityHeaderType = parse_security_header_type(EBI_SHT),
            Msg = #{security_header_type => SecurityHeaderType,
                    protocol_discriminator => ProtocolDiscriminator
                   },
            {Msg, {SecurityHeaderType, Rest}};
        eps_session_management_messages ->
            Msg = #{eps_bearer_identity => EBI_SHT,
                    protocol_discriminator => ProtocolDiscriminator
                   },
            {Msg, Rest}
    end.

encode({#{protocol_discriminator := ProtocolDiscriminator} = Msg, PDU}) ->
    PD = otc_l3_codec:compose_protocol_discriminator(ProtocolDiscriminator),
    case ProtocolDiscriminator of
        eps_mobility_management_messages -> %% security protected
            SecurityHeaderType = maps:get(security_header_type, Msg, undefined),
            SHT = compose_security_header_type(SecurityHeaderType),
            <<SHT:4, PD:4, PDU/binary>>;
        eps_session_management_messages ->
            EBI = maps:get(eps_bearer_identity, Msg, undefined),
            <<EBI:4, PD:4, PDU/binary>>
    end.

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

