-module(otc_nas_5gs).
-behaviour(otc_codec).

-include("include/nas_5gs.hrl").
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
    "3GPP TS 24.501 version 16.10.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

-spec next(map()) -> '$stop' | {ok, nas_5gs | nas_5gs_5gmm | nas_5gs_5gsm}.
next(#{protocol_discriminator := '5gs_mobility_management_messages'}) -> {ok, nas_5gs_5gmm};
next(#{protocol_discriminator := '5gs_session_management_messages'}) -> {ok, nas_5gs_5gsm};
next(_) -> '$stop'.

decode(<<EPD:8, Bin/binary>>) ->
    ProtocolDiscriminator = otc_l3_codec:parse_protocol_discriminator(EPD),
    case ProtocolDiscriminator of
        '5gs_mobility_management_messages' -> %% security protected
            %% Security Header Type
            <<_:4, SHT:4, Rest/binary>> = Bin,
            SecurityHeaderType = parse_security_header_type(SHT),
            Msg = #{security_header_type => SecurityHeaderType,
                    protocol_discriminator => ProtocolDiscriminator
                   },
            {Msg, {SecurityHeaderType, Rest}};
        '5gs_session_management_messages' ->
            %% PDU session identity
            <<PSI:8, Rest/binary>> = Bin,
            Msg = #{pdu_session_identity => PSI,
                    protocol_discriminator => ProtocolDiscriminator
                   },
            {Msg, Rest}
    end.

encode(#{protocol_discriminator := ProtocolDiscriminator} = Msg) ->
    EPD = otc_l3_codec:compose_protocol_discriminator(ProtocolDiscriminator),
    case ProtocolDiscriminator of
        '5gs_mobility_management_messages' -> %% security protected
            SecurityHeaderType = maps:get(security_header_type, Msg, undefined),
            SHT = compose_security_header_type(SecurityHeaderType),
            <<EPD:8, 0:4, SHT:4>>;
        '5gs_session_management_messages' ->
            PSI = maps:get(pdu_session_identity, Msg, undefined),
            <<EPD:8, PSI:8>>
    end.

%% 9.3.1 Security header type
parse_security_header_type(?NAS_SHT_PLAIN_5GS_NAS_MESSAGE) -> plain_5gs_nas_message;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED) -> integrity_protected;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED) -> integrity_protected_ciphered;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_5GS_SECURITY) -> integrity_protected_5gs_security;
parse_security_header_type(?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_5GS_SECURITY) -> integrity_protected_ciphered_5gs_security.

compose_security_header_type(plain_5gs_nas_message) -> ?NAS_SHT_PLAIN_5GS_NAS_MESSAGE;
compose_security_header_type(integrity_protected) -> ?NAS_SHT_INTEGRITY_PROTECTED;
compose_security_header_type(integrity_protected_ciphered) -> ?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED;
compose_security_header_type(integrity_protected_5gs_security) -> ?NAS_SHT_INTEGRITY_PROTECTED_5GS_SECURITY;
compose_security_header_type(integrity_protected_ciphered_5gs_security) -> ?NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_5GS_SECURITY.


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
