-module(otc_gtpv2).
-behaviour(otc_codec).

-include("include/gtpv2.hrl").
-include("include/l3.hrl").

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

spec() ->
    "3GPP TS 29.274 v17.7.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) ->
    '$stop'.

decode(<<2:3, P:1, T:1, MP:1, _:2, MT:8, Len:16, Rest0/binary>>) ->
    <<GTP0:Len/binary, Rest1/binary>> = Rest0,
    MessageType = parse_message_type(MT),
    {MsgFields, GTP1} = decode_msg_fields(P, T, MP, GTP0),
    Msg0 = decode_msg(MessageType, GTP1),
    Msg1 = maps:merge(Msg0, MsgFields),
    Msg2 = Msg1#{message_type => MessageType},
    {Msg2, Rest1}.

encode(Map) ->
    #{message_type := MessageType} = Map,
    MT = compose_message_type(MessageType),
    {[P, T, MP], MsgFields} = encode_msg_fields(Map),
    MsgBin = encode_msg(MessageType, Map),
    Len = byte_size(MsgBin),
    <<2:3, P:1, T:1, MP:1, 0:2, MT:8, Len:16, MsgFields/binary, MsgBin/binary>>.

parse_message_type(?GTPv2_MSG_TYPE_ECHO_REQUEST) ->
    echo_request;
parse_message_type(?GTPv2_MSG_TYPE_ECHO_RESPONSE) ->
    echo_response;
parse_message_type(?GTPv2_MSG_TYPE_VERSION_NOT_SUPPORTED_INDICATION) ->
    version_not_supported_indication;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_SESSION_REQUEST) ->
    create_session_request;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_SESSION_RESPONSE) ->
    create_session_response;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_SESSION_REQUEST) ->
    delete_session_request;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_SESSION_RESPONSE) ->
    delete_session_response;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_BEARER_REQUEST) ->
    modify_bearer_request;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_BEARER_RESPONSE) ->
    modify_bearer_response;
parse_message_type(?GTPv2_MSG_TYPE_REMOTE_UE_REPORT_NOTIFICATION) ->
    remote_ue_report_notification;
parse_message_type(?GTPv2_MSG_TYPE_REMOTE_UE_REPORT_ACKNOWLEDGE) ->
    remote_ue_report_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_CHANGE_NOTIFICATION_REQUEST) ->
    change_notification_request;
parse_message_type(?GTPv2_MSG_TYPE_CHANGE_NOTIFICATION_RESPONSE) ->
    change_notification_response;
parse_message_type(?GTPv2_MSG_TYPE_RESUME_NOTIFICATION) ->
    resume_notification;
parse_message_type(?GTPv2_MSG_TYPE_RESUME_ACKNOWLEDGE) ->
    resume_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_BEARER_COMMAND) ->
    modify_bearer_command;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_BEARER_FAILURE_INDICATION) ->
    modify_bearer_failure_indication;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_BEARER_COMMAND) ->
    delete_bearer_command;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_BEARER_FAILURE_INDICATION) ->
    delete_bearer_failure_indication;
parse_message_type(?GTPv2_MSG_TYPE_BEARER_RESOURCE_COMMAND) ->
    bearer_resource_command;
parse_message_type(?GTPv2_MSG_TYPE_BEARER_RESOURCE_FAILURE_INDICATION) ->
    bearer_resource_failure_indication;
parse_message_type(?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION_FAILURE_INDICATION) ->
    downlink_data_notification_failure_indication;
parse_message_type(?GTPv2_MSG_TYPE_TRACE_SESSION_ACTIVATION) ->
    trace_session_activation;
parse_message_type(?GTPv2_MSG_TYPE_TRACE_SESSION_DEACTIVATION) ->
    trace_session_deactivation;
parse_message_type(?GTPv2_MSG_TYPE_STOP_PAGING_INDICATION) ->
    stop_paging_indication;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_BEARER_REQUEST) ->
    create_bearer_request;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_BEARER_RESPONSE) ->
    create_bearer_response;
parse_message_type(?GTPv2_MSG_TYPE_UPDATE_BEARER_REQUEST) ->
    update_bearer_request;
parse_message_type(?GTPv2_MSG_TYPE_UPDATE_BEARER_RESPONSE) ->
    update_bearer_response;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_BEARER_REQUEST) ->
    delete_bearer_request;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_BEARER_RESPONSE) ->
    delete_bearer_response;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_PDN_CONNECTION_SET_REQUEST) ->
    delete_pdn_connection_set_request;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_PDN_CONNECTION_SET_RESPONSE) ->
    delete_pdn_connection_set_response;
parse_message_type(?GTPv2_MSG_TYPE_PGW_DOWNLINK_TRIGGERING_NOTIFICATION) ->
    pgw_downlink_triggering_notification;
parse_message_type(?GTPv2_MSG_TYPE_PGW_DOWNLINK_TRIGGERING_ACKNOWLEDGE) ->
    pgw_downlink_triggering_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_IDENTIFICATION_REQUEST) ->
    identification_request;
parse_message_type(?GTPv2_MSG_TYPE_IDENTIFICATION_RESPONSE) ->
    identification_response;
parse_message_type(?GTPv2_MSG_TYPE_CONTEXT_REQUEST) ->
    context_request;
parse_message_type(?GTPv2_MSG_TYPE_CONTEXT_RESPONSE) ->
    context_response;
parse_message_type(?GTPv2_MSG_TYPE_CONTEXT_ACKNOWLEDGE) ->
    context_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_RELOCATION_REQUEST) ->
    forward_relocation_request;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_RELOCATION_RESPONSE) ->
    forward_relocation_response;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_NOTIFICATION) ->
    forward_relocation_complete_notification;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_ACKNOWLEDGE) ->
    forward_relocation_complete_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_ACCESS_CONTEXT_NOTIFICATION) ->
    forward_access_context_notification;
parse_message_type(?GTPv2_MSG_TYPE_FORWARD_ACCESS_CONTEXT_ACKNOWLEDGE) ->
    forward_access_context_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_RELOCATION_CANCEL_REQUEST) ->
    relocation_cancel_request;
parse_message_type(?GTPv2_MSG_TYPE_RELOCATION_CANCEL_RESPONSE) ->
    relocation_cancel_response;
parse_message_type(?GTPv2_MSG_TYPE_CONFIGURATION_TRANSFER_TUNNEL) ->
    configuration_transfer_tunnel;
parse_message_type(?GTPv2_MSG_TYPE_RAN_INFORMATION_RELAY) ->
    ran_information_relay;
parse_message_type(?GTPv2_MSG_TYPE_DETACH_NOTIFICATION) ->
    detach_notification;
parse_message_type(?GTPv2_MSG_TYPE_DETACH_ACKNOWLEDGE) ->
    detach_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_CS_PAGING_INDICATION) ->
    cs_paging_indication;
parse_message_type(?GTPv2_MSG_TYPE_ALERT_MME_NOTIFICATION) ->
    alert_mme_notification;
parse_message_type(?GTPv2_MSG_TYPE_ALERT_MME_ACKNOWLEDGE) ->
    alert_mme_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_UE_ACTIVITY_NOTIFICATION) ->
    ue_activity_notification;
parse_message_type(?GTPv2_MSG_TYPE_UE_ACTIVITY_ACKNOWLEDGE) ->
    ue_activity_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_ISR_STATUS_INDICATION) ->
    isr_status_indication;
parse_message_type(?GTPv2_MSG_TYPE_UE_REGISTRATION_QUERY_REQUEST) ->
    ue_registration_query_request;
parse_message_type(?GTPv2_MSG_TYPE_UE_REGISTRATION_QUERY_RESPONSE) ->
    ue_registration_query_response;
parse_message_type(?GTPv2_MSG_TYPE_SUSPEND_NOTIFICATION) ->
    suspend_notification;
parse_message_type(?GTPv2_MSG_TYPE_SUSPEND_ACKNOWLEDGE) ->
    suspend_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_FORWARDING_TUNNEL_REQUEST) ->
    create_forwarding_tunnel_request;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_FORWARDING_TUNNEL_RESPONSE) ->
    create_forwarding_tunnel_response;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_INDIRECT_DATA_FORWARDING_TUNNEL_REQUEST) ->
    create_indirect_data_forwarding_tunnel_request;
parse_message_type(?GTPv2_MSG_TYPE_CREATE_INDIRECT_DATA_FORWARDING_TUNNEL_RESPONSE) ->
    create_indirect_data_forwarding_tunnel_response;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_INDIRECT_DATA_FORWARDING_TUNNEL_REQUEST) ->
    delete_indirect_data_forwarding_tunnel_request;
parse_message_type(?GTPv2_MSG_TYPE_DELETE_INDIRECT_DATA_FORWARDING_TUNNEL_RESPONSE) ->
    delete_indirect_data_forwarding_tunnel_response;
parse_message_type(?GTPv2_MSG_TYPE_RELEASE_ACCESS_BEARERS_REQUEST) ->
    release_access_bearers_request;
parse_message_type(?GTPv2_MSG_TYPE_RELEASE_ACCESS_BEARERS_RESPONSE) ->
    release_access_bearers_response;
parse_message_type(?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION) ->
    downlink_data_notification;
parse_message_type(?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION_ACKNOWLEDGE) ->
    downlink_data_notification_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_PGW_RESTART_NOTIFICATION) ->
    pgw_restart_notification;
parse_message_type(?GTPv2_MSG_TYPE_PGW_RESTART_NOTIFICATION_ACKNOWLEDGE) ->
    pgw_restart_notification_acknowledge;
parse_message_type(?GTPv2_MSG_TYPE_UPDATE_PDN_CONNECTION_SET_REQUEST) ->
    update_pdn_connection_set_request;
parse_message_type(?GTPv2_MSG_TYPE_UPDATE_PDN_CONNECTION_SET_RESPONSE) ->
    update_pdn_connection_set_response;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_ACCESS_BEARERS_REQUEST) ->
    modify_access_bearers_request;
parse_message_type(?GTPv2_MSG_TYPE_MODIFY_ACCESS_BEARERS_RESPONSE) ->
    modify_access_bearers_response;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_START_REQUEST) ->
    mbms_session_start_request;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_START_RESPONSE) ->
    mbms_session_start_response;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_UPDATE_REQUEST) ->
    mbms_session_update_request;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_UPDATE_RESPONSE) ->
    mbms_session_update_response;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_STOP_REQUEST) ->
    mbms_session_stop_request;
parse_message_type(?GTPv2_MSG_TYPE_MBMS_SESSION_STOP_RESPONSE) ->
    mbms_session_stop_response.

compose_message_type(echo_request) ->
    ?GTPv2_MSG_TYPE_ECHO_REQUEST;
compose_message_type(echo_response) ->
    ?GTPv2_MSG_TYPE_ECHO_RESPONSE;
compose_message_type(version_not_supported_indication) ->
    ?GTPv2_MSG_TYPE_VERSION_NOT_SUPPORTED_INDICATION;
compose_message_type(create_session_request) ->
    ?GTPv2_MSG_TYPE_CREATE_SESSION_REQUEST;
compose_message_type(create_session_response) ->
    ?GTPv2_MSG_TYPE_CREATE_SESSION_RESPONSE;
compose_message_type(delete_session_request) ->
    ?GTPv2_MSG_TYPE_DELETE_SESSION_REQUEST;
compose_message_type(delete_session_response) ->
    ?GTPv2_MSG_TYPE_DELETE_SESSION_RESPONSE;
compose_message_type(modify_bearer_request) ->
    ?GTPv2_MSG_TYPE_MODIFY_BEARER_REQUEST;
compose_message_type(modify_bearer_response) ->
    ?GTPv2_MSG_TYPE_MODIFY_BEARER_RESPONSE;
compose_message_type(remote_ue_report_notification) ->
    ?GTPv2_MSG_TYPE_REMOTE_UE_REPORT_NOTIFICATION;
compose_message_type(remote_ue_report_acknowledge) ->
    ?GTPv2_MSG_TYPE_REMOTE_UE_REPORT_ACKNOWLEDGE;
compose_message_type(change_notification_request) ->
    ?GTPv2_MSG_TYPE_CHANGE_NOTIFICATION_REQUEST;
compose_message_type(change_notification_response) ->
    ?GTPv2_MSG_TYPE_CHANGE_NOTIFICATION_RESPONSE;
compose_message_type(resume_notification) ->
    ?GTPv2_MSG_TYPE_RESUME_NOTIFICATION;
compose_message_type(resume_acknowledge) ->
    ?GTPv2_MSG_TYPE_RESUME_ACKNOWLEDGE;
compose_message_type(modify_bearer_command) ->
    ?GTPv2_MSG_TYPE_MODIFY_BEARER_COMMAND;
compose_message_type(modify_bearer_failure_indication) ->
    ?GTPv2_MSG_TYPE_MODIFY_BEARER_FAILURE_INDICATION;
compose_message_type(delete_bearer_command) ->
    ?GTPv2_MSG_TYPE_DELETE_BEARER_COMMAND;
compose_message_type(delete_bearer_failure_indication) ->
    ?GTPv2_MSG_TYPE_DELETE_BEARER_FAILURE_INDICATION;
compose_message_type(bearer_resource_command) ->
    ?GTPv2_MSG_TYPE_BEARER_RESOURCE_COMMAND;
compose_message_type(bearer_resource_failure_indication) ->
    ?GTPv2_MSG_TYPE_BEARER_RESOURCE_FAILURE_INDICATION;
compose_message_type(downlink_data_notification_failure_indication) ->
    ?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION_FAILURE_INDICATION;
compose_message_type(trace_session_activation) ->
    ?GTPv2_MSG_TYPE_TRACE_SESSION_ACTIVATION;
compose_message_type(trace_session_deactivation) ->
    ?GTPv2_MSG_TYPE_TRACE_SESSION_DEACTIVATION;
compose_message_type(stop_paging_indication) ->
    ?GTPv2_MSG_TYPE_STOP_PAGING_INDICATION;
compose_message_type(create_bearer_request) ->
    ?GTPv2_MSG_TYPE_CREATE_BEARER_REQUEST;
compose_message_type(create_bearer_response) ->
    ?GTPv2_MSG_TYPE_CREATE_BEARER_RESPONSE;
compose_message_type(update_bearer_request) ->
    ?GTPv2_MSG_TYPE_UPDATE_BEARER_REQUEST;
compose_message_type(update_bearer_response) ->
    ?GTPv2_MSG_TYPE_UPDATE_BEARER_RESPONSE;
compose_message_type(delete_bearer_request) ->
    ?GTPv2_MSG_TYPE_DELETE_BEARER_REQUEST;
compose_message_type(delete_bearer_response) ->
    ?GTPv2_MSG_TYPE_DELETE_BEARER_RESPONSE;
compose_message_type(delete_pdn_connection_set_request) ->
    ?GTPv2_MSG_TYPE_DELETE_PDN_CONNECTION_SET_REQUEST;
compose_message_type(delete_pdn_connection_set_response) ->
    ?GTPv2_MSG_TYPE_DELETE_PDN_CONNECTION_SET_RESPONSE;
compose_message_type(pgw_downlink_triggering_notification) ->
    ?GTPv2_MSG_TYPE_PGW_DOWNLINK_TRIGGERING_NOTIFICATION;
compose_message_type(pgw_downlink_triggering_acknowledge) ->
    ?GTPv2_MSG_TYPE_PGW_DOWNLINK_TRIGGERING_ACKNOWLEDGE;
compose_message_type(identification_request) ->
    ?GTPv2_MSG_TYPE_IDENTIFICATION_REQUEST;
compose_message_type(identification_response) ->
    ?GTPv2_MSG_TYPE_IDENTIFICATION_RESPONSE;
compose_message_type(context_request) ->
    ?GTPv2_MSG_TYPE_CONTEXT_REQUEST;
compose_message_type(context_response) ->
    ?GTPv2_MSG_TYPE_CONTEXT_RESPONSE;
compose_message_type(context_acknowledge) ->
    ?GTPv2_MSG_TYPE_CONTEXT_ACKNOWLEDGE;
compose_message_type(forward_relocation_request) ->
    ?GTPv2_MSG_TYPE_FORWARD_RELOCATION_REQUEST;
compose_message_type(forward_relocation_response) ->
    ?GTPv2_MSG_TYPE_FORWARD_RELOCATION_RESPONSE;
compose_message_type(forward_relocation_complete_notification) ->
    ?GTPv2_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_NOTIFICATION;
compose_message_type(forward_relocation_complete_acknowledge) ->
    ?GTPv2_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_ACKNOWLEDGE;
compose_message_type(forward_access_context_notification) ->
    ?GTPv2_MSG_TYPE_FORWARD_ACCESS_CONTEXT_NOTIFICATION;
compose_message_type(forward_access_context_acknowledge) ->
    ?GTPv2_MSG_TYPE_FORWARD_ACCESS_CONTEXT_ACKNOWLEDGE;
compose_message_type(relocation_cancel_request) ->
    ?GTPv2_MSG_TYPE_RELOCATION_CANCEL_REQUEST;
compose_message_type(relocation_cancel_response) ->
    ?GTPv2_MSG_TYPE_RELOCATION_CANCEL_RESPONSE;
compose_message_type(configuration_transfer_tunnel) ->
    ?GTPv2_MSG_TYPE_CONFIGURATION_TRANSFER_TUNNEL;
compose_message_type(ran_information_relay) ->
    ?GTPv2_MSG_TYPE_RAN_INFORMATION_RELAY;
compose_message_type(detach_notification) ->
    ?GTPv2_MSG_TYPE_DETACH_NOTIFICATION;
compose_message_type(detach_acknowledge) ->
    ?GTPv2_MSG_TYPE_DETACH_ACKNOWLEDGE;
compose_message_type(cs_paging_indication) ->
    ?GTPv2_MSG_TYPE_CS_PAGING_INDICATION;
compose_message_type(alert_mme_notification) ->
    ?GTPv2_MSG_TYPE_ALERT_MME_NOTIFICATION;
compose_message_type(alert_mme_acknowledge) ->
    ?GTPv2_MSG_TYPE_ALERT_MME_ACKNOWLEDGE;
compose_message_type(ue_activity_notification) ->
    ?GTPv2_MSG_TYPE_UE_ACTIVITY_NOTIFICATION;
compose_message_type(ue_activity_acknowledge) ->
    ?GTPv2_MSG_TYPE_UE_ACTIVITY_ACKNOWLEDGE;
compose_message_type(isr_status_indication) ->
    ?GTPv2_MSG_TYPE_ISR_STATUS_INDICATION;
compose_message_type(ue_registration_query_request) ->
    ?GTPv2_MSG_TYPE_UE_REGISTRATION_QUERY_REQUEST;
compose_message_type(ue_registration_query_response) ->
    ?GTPv2_MSG_TYPE_UE_REGISTRATION_QUERY_RESPONSE;
compose_message_type(suspend_notification) ->
    ?GTPv2_MSG_TYPE_SUSPEND_NOTIFICATION;
compose_message_type(suspend_acknowledge) ->
    ?GTPv2_MSG_TYPE_SUSPEND_ACKNOWLEDGE;
compose_message_type(create_forwarding_tunnel_request) ->
    ?GTPv2_MSG_TYPE_CREATE_FORWARDING_TUNNEL_REQUEST;
compose_message_type(create_forwarding_tunnel_response) ->
    ?GTPv2_MSG_TYPE_CREATE_FORWARDING_TUNNEL_RESPONSE;
compose_message_type(create_indirect_data_forwarding_tunnel_request) ->
    ?GTPv2_MSG_TYPE_CREATE_INDIRECT_DATA_FORWARDING_TUNNEL_REQUEST;
compose_message_type(create_indirect_data_forwarding_tunnel_response) ->
    ?GTPv2_MSG_TYPE_CREATE_INDIRECT_DATA_FORWARDING_TUNNEL_RESPONSE;
compose_message_type(delete_indirect_data_forwarding_tunnel_request) ->
    ?GTPv2_MSG_TYPE_DELETE_INDIRECT_DATA_FORWARDING_TUNNEL_REQUEST;
compose_message_type(delete_indirect_data_forwarding_tunnel_response) ->
    ?GTPv2_MSG_TYPE_DELETE_INDIRECT_DATA_FORWARDING_TUNNEL_RESPONSE;
compose_message_type(release_access_bearers_request) ->
    ?GTPv2_MSG_TYPE_RELEASE_ACCESS_BEARERS_REQUEST;
compose_message_type(release_access_bearers_response) ->
    ?GTPv2_MSG_TYPE_RELEASE_ACCESS_BEARERS_RESPONSE;
compose_message_type(downlink_data_notification) ->
    ?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION;
compose_message_type(downlink_data_notification_acknowledge) ->
    ?GTPv2_MSG_TYPE_DOWNLINK_DATA_NOTIFICATION_ACKNOWLEDGE;
compose_message_type(pgw_restart_notification) ->
    ?GTPv2_MSG_TYPE_PGW_RESTART_NOTIFICATION;
compose_message_type(pgw_restart_notification_acknowledge) ->
    ?GTPv2_MSG_TYPE_PGW_RESTART_NOTIFICATION_ACKNOWLEDGE;
compose_message_type(update_pdn_connection_set_request) ->
    ?GTPv2_MSG_TYPE_UPDATE_PDN_CONNECTION_SET_REQUEST;
compose_message_type(update_pdn_connection_set_response) ->
    ?GTPv2_MSG_TYPE_UPDATE_PDN_CONNECTION_SET_RESPONSE;
compose_message_type(modify_access_bearers_request) ->
    ?GTPv2_MSG_TYPE_MODIFY_ACCESS_BEARERS_REQUEST;
compose_message_type(modify_access_bearers_response) ->
    ?GTPv2_MSG_TYPE_MODIFY_ACCESS_BEARERS_RESPONSE;
compose_message_type(mbms_session_start_request) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_START_REQUEST;
compose_message_type(mbms_session_start_response) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_START_RESPONSE;
compose_message_type(mbms_session_update_request) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_UPDATE_REQUEST;
compose_message_type(mbms_session_update_response) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_UPDATE_RESPONSE;
compose_message_type(mbms_session_stop_request) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_STOP_REQUEST;
compose_message_type(mbms_session_stop_response) ->
    ?GTPv2_MSG_TYPE_MBMS_SESSION_STOP_RESPONSE.

decode_msg_fields(P, T, MP, GTP0) ->
    PB = case P of
             1 -> true;
             0 -> false
         end,
    Fs0 = [{piggy_backed, PB}],
    {Fs1, Rest1} = case T of
                       1 ->
                           <<TEID:32, SN:24, Rest0/binary>> = GTP0,
                           {[{teid, TEID}, {sequence_number, SN}], Rest0};
                       0 ->
                           <<SeqNo:24, Rest0/binary>> = GTP0,
                           {[{sequence_number, SeqNo}], Rest0}
                   end,

    {Fs2, Rest3} = case MP of
                       1 ->
                           <<MP:4, _:4, Rest2/binary>> = Rest1,
                           {[{message_priority, MP}], Rest2};
                       0 ->
                           <<_:8, Rest2/binary>> = Rest1,
                           {[], Rest2}
                   end,
    {maps:from_list(Fs0 ++ Fs1 ++ Fs2), Rest3}.

encode_msg_fields(Map) ->
    PB = maps:get(piggy_backed, Map, false),
    TEID = maps:get(teid, Map, undefined),
    MP = maps:get(message_priority, Map, 0),
    SN = maps:get(sequence_number, Map, 0),

    Indicators = [case PB of true -> 1; _ -> 0 end,
                  case TEID of undefined -> 0; _ -> 1 end,
                  case MP of 0 -> 0; _ -> 1 end],
    Bin = case TEID of
              undefined ->
                  <<SN:24, MP:4, 0:4>>;
              _ ->
                  <<TEID:32, SN:24, MP:4, 0:4>>
          end,
    {Indicators, Bin}.
