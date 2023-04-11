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

decode_parameters(_, [], Acc) ->
    Acc;
decode_parameters(<<>>, _, Acc) ->
    Acc;
decode_parameters(<<IEI:8, Len:16, _:4, Instance:4, Bin0/binary>>, Os, Acc) ->
    <<V:Len/binary, Rest/binary>> = Bin0,
    Param = parse_iei(IEI),
    case lists:keytake(Param, 1, Os) of
        {value, {Name, _, _}, NOs} ->
            Par = decode_parameter(Name, V),
            decode_parameters(Rest, NOs, Acc#{Name => Par});
        false ->
            decode_parameters(Rest, Os, Acc)
    end.

parse_iei(?GTPv2_IEI_IMSI) ->
    imsi;
parse_iei(?GTPv2_IEI_CAUSE) ->
    cause;
parse_iei(?GTPv2_IEI_RECOVERY_RESTART_COUNTER) ->
    recovery_restart_counter;
parse_iei(?GTPv2_IEI_STN_SR) ->
    stn_sr;
parse_iei(?GTPv2_IEI_SRVCC_CAUSE) ->
    srvcc_cause;
parse_iei(?GTPv2_IEI_APN) ->
    apn;
parse_iei(?GTPv2_IEI_AMBR) ->
    ambr;
parse_iei(?GTPv2_IEI_EPS_BEARER_ID) ->
    eps_bearer_id;
parse_iei(?GTPv2_IEI_IP_ADDRESS) ->
    ip_address;
parse_iei(?GTPv2_IEI_MEI) ->
    mei;
parse_iei(?GTPv2_IEI_MSISDN) ->
    msisdn;
parse_iei(?GTPv2_IEI_INDICATION) ->
    indication;
parse_iei(?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS) ->
    protocol_configuration_options;
parse_iei(?GTPv2_IEI_PDN_ADDRESS_ALLOCATION) ->
    pdn_address_allocation;
parse_iei(?GTPv2_IEI_BEARER_QOS) ->
    bearer_qos;
parse_iei(?GTPv2_IEI_FLOW_QOS) ->
    flow_qos;
parse_iei(?GTPv2_IEI_RAT_TYPE) ->
    rat_type;
parse_iei(?GTPv2_IEI_SERVING_NETWORK) ->
    serving_network;
parse_iei(?GTPv2_IEI_BEARER_TFT) ->
    bearer_tft;
parse_iei(?GTPv2_IEI_TRAFFIC_AGGREGATION_DESCRIPTION) ->
    traffic_aggregation_description;
parse_iei(?GTPv2_IEI_USER_LOCATION_INFORMATION) ->
    user_location_information;
parse_iei(?GTPv2_IEI_F_TEID) ->
    f_teid;
parse_iei(?GTPv2_IEI_TMSI) ->
    tmsi;
parse_iei(?GTPv2_IEI_GLOBAL_CN_ID) ->
    global_cn_id;
parse_iei(?GTPv2_IEI_S103_PDN_DATA_FORWARDING_INFO) ->
    s103_pdn_data_forwarding_info;
parse_iei(?GTPv2_IEI_S1_U_DATA_FORWARDING_INFO) ->
    s1_u_data_forwarding_info;
parse_iei(?GTPv2_IEI_DELAY_VALUE) ->
    delay_value;
parse_iei(?GTPv2_IEI_BEARER_CONTEXT) ->
    bearer_context;
parse_iei(?GTPv2_IEI_CHARGING_ID) ->
    charging_id;
parse_iei(?GTPv2_IEI_CHARGING_CHARACTERISTICS) ->
    charging_characteristics;
parse_iei(?GTPv2_IEI_TRACE_INFORMATION) ->
    trace_information;
parse_iei(?GTPv2_IEI_BEARER_FLAGS) ->
    bearer_flags;
parse_iei(?GTPv2_IEI_PDN_TYPE) ->
    pdn_type;
parse_iei(?GTPv2_IEI_PROCEDURE_TRANSACTION_ID) ->
    procedure_transaction_id;
parse_iei(?GTPv2_IEI_MM_CONTEXT_GSM_KEY_AND_TRIPLETS) ->
    mm_context_gsm_key_and_triplets;
parse_iei(?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_CIPHER_AND_QUINTUPLETS) ->
    mm_context_umts_key_cipher_and_quintuplets;
parse_iei(?GTPv2_IEI_MM_CONTEXT_GSM_KEY_CIPHER_AND_QUINTUPLETS) ->
    mm_context_gsm_key_cipher_and_quintuplets;
parse_iei(?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_AND_QUINTUPLETS) ->
    mm_context_umts_key_and_quintuplets;
parse_iei(?GTPv2_IEI_MM_CONTEXT_EPS_SECURITY_CONTEXT_QUADRUPLETS_AND_QUINTUPLETS) ->
    mm_context_eps_security_context_quadruplets_and_quintuplets;
parse_iei(?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_QUADRUPLETS_AND_QUINTUPLETS) ->
    mm_context_umts_key_quadruplets_and_quintuplets;
parse_iei(?GTPv2_IEI_PDN_CONNECTION) ->
    pdn_connection;
parse_iei(?GTPv2_IEI_PDU_NUMBERS) ->
    pdu_numbers;
parse_iei(?GTPv2_IEI_P_TMSI) ->
    p_tmsi;
parse_iei(?GTPv2_IEI_P_TMSI_SIGNATURE) ->
    p_tmsi_signature;
parse_iei(?GTPv2_IEI_HOP_COUNTER) ->
    hop_counter;
parse_iei(?GTPv2_IEI_UE_TIME_ZONE) ->
    ue_time_zone;
parse_iei(?GTPv2_IEI_TRACE_REFERENCE) ->
    trace_reference;
parse_iei(?GTPv2_IEI_COMPLETE_REQUEST_MESSAGE) ->
    complete_request_message;
parse_iei(?GTPv2_IEI_GUTI) ->
    guti;
parse_iei(?GTPv2_IEI_F_CONTAINER) ->
    f_container;
parse_iei(?GTPv2_IEI_F_CAUSE) ->
    f_cause;
parse_iei(?GTPv2_IEI_PLMN_ID) ->
    plmn_id;
parse_iei(?GTPv2_IEI_TARGET_IDENTIFICATION) ->
    target_identification;
parse_iei(?GTPv2_IEI_PACKET_FLOW_ID) ->
    packet_flow_id;
parse_iei(?GTPv2_IEI_RAB_CONTEXT) ->
    rab_context;
parse_iei(?GTPv2_IEI_SOURCE_RNC_PDCP_CONTEXT_INFO) ->
    source_rnc_pdcp_context_info;
parse_iei(?GTPv2_IEI_PORT_NUMBER) ->
    port_number;
parse_iei(?GTPv2_IEI_APN_RESTRICTION) ->
    apn_restriction;
parse_iei(?GTPv2_IEI_SELECTION_MODE) ->
    selection_mode;
parse_iei(?GTPv2_IEI_SOURCE_IDENTIFICATION) ->
    source_identification;
parse_iei(?GTPv2_IEI_CHANGE_REPORTING_ACTION) ->
    change_reporting_action;
parse_iei(?GTPv2_IEI_FQ_CSID) ->
    fq_csid;
parse_iei(?GTPv2_IEI_CHANNEL_NEEDED) ->
    channel_needed;
parse_iei(?GTPv2_IEI_EMLPP_PRIORITY) ->
    emlpp_priority;
parse_iei(?GTPv2_IEI_NODE_TYPE) ->
    node_type;
parse_iei(?GTPv2_IEI_FQDN) ->
    fqdn;
parse_iei(?GTPv2_IEI_TRANSACTION_IDENTIFIER) ->
    transaction_identifier;
parse_iei(?GTPv2_IEI_MBMS_SESSION_DURATION) ->
    mbms_session_duration;
parse_iei(?GTPv2_IEI_MBMS_SERVICE_AREA) ->
    mbms_service_area;
parse_iei(?GTPv2_IEI_MBMS_SESSION_IDENTIFIER) ->
    mbms_session_identifier;
parse_iei(?GTPv2_IEI_MBMS_FLOW_IDENTIFIER) ->
    mbms_flow_identifier;
parse_iei(?GTPv2_IEI_MBMS_IP_MULTICAST_DISTRIBUTION) ->
    mbms_ip_multicast_distribution;
parse_iei(?GTPv2_IEI_MBMS_DISTRIBUTION_ACKNOWLEDGE) ->
    mbms_distribution_acknowledge;
parse_iei(?GTPv2_IEI_RFSP_INDEX) ->
    rfsp_index;
parse_iei(?GTPv2_IEI_USER_CSG_INFORMATION) ->
    user_csg_information;
parse_iei(?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION) ->
    csg_information_reporting_action;
parse_iei(?GTPv2_IEI_CSG_ID) ->
    csg_id;
parse_iei(?GTPv2_IEI_CSG_MEMBERSHIP_INDICATION) ->
    csg_membership_indication;
parse_iei(?GTPv2_IEI_SERVICE_INDICATOR) ->
    service_indicator;
parse_iei(?GTPv2_IEI_DETACH_TYPE) ->
    detach_type;
parse_iei(?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME) ->
    local_distiguished_name;
parse_iei(?GTPv2_IEI_NODE_FEATURES) ->
    node_features;
parse_iei(?GTPv2_IEI_MBMS_TIME_TO_DATA_TRANSFER) ->
    mbms_time_to_data_transfer;
parse_iei(?GTPv2_IEI_THROTTLING) ->
    throttling;
parse_iei(?GTPv2_IEI_ALLOCATIONRETENTION_PRIORITY) ->
    allocationretention_priority;
parse_iei(?GTPv2_IEI_EPC_TIMER) ->
    epc_timer;
parse_iei(?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION) ->
    signalling_priority_indication;
parse_iei(?GTPv2_IEI_TEMPORARY_MOBILE_GROUP_IDENTITY) ->
    temporary_mobile_group_identity;
parse_iei(?GTPv2_IEI_ADDITIONAL_MM_CONTEXT_FOR_SRVCC) ->
    additional_mm_context_for_srvcc;
parse_iei(?GTPv2_IEI_ADDITIONAL_FLAGS_FOR_SRVCC) ->
    additional_flags_for_srvcc;
parse_iei(?GTPv2_IEI_MDT_CONFIGURATION) ->
    mdt_configuration;
parse_iei(?GTPv2_IEI_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS) ->
    additional_protocol_configuration_options;
parse_iei(?GTPv2_IEI_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER) ->
    absolute_time_of_mbms_data_transfer;
parse_iei(?GTPv2_IEI_HENB_INFORMATION_REPORTING) ->
    henb_information_reporting;
parse_iei(?GTPv2_IEI_IPV4_CONFIGURATION_PARAMETERS) ->
    ipv4_configuration_parameters;
parse_iei(?GTPv2_IEI_CHANGE_TO_REPORT_FLAGS) ->
    change_to_report_flags;
parse_iei(?GTPv2_IEI_ACTION_INDICATION) ->
    action_indication;
parse_iei(?GTPv2_IEI_TWAN_IDENTIFIER) ->
    twan_identifier;
parse_iei(?GTPv2_IEI_ULI_TIMESTAMP) ->
    uli_timestamp;
parse_iei(?GTPv2_IEI_MBMS_FLAGS) ->
    mbms_flags;
parse_iei(?GTPv2_IEI_RANNAS_CAUSE) ->
    rannas_cause;
parse_iei(?GTPv2_IEI_CN_OPERATOR_SELECTION_ENTITY) ->
    cn_operator_selection_entity;
parse_iei(?GTPv2_IEI_TRUSTED_WLAN_MODE_INDICATION) ->
    trusted_wlan_mode_indication;
parse_iei(?GTPv2_IEI_NODE_NUMBER) ->
    node_number;
parse_iei(?GTPv2_IEI_NODE_IDENTIFIER) ->
    node_identifier;
parse_iei(?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION) ->
    presence_reporting_area_action;
parse_iei(?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION) ->
    presence_reporting_area_information;
parse_iei(?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP) ->
    twan_identifier_timestamp;
parse_iei(?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION) ->
    overload_control_information;
parse_iei(?GTPv2_IEI_LOAD_CONTROL_INFORMATION) ->
    load_control_information;
parse_iei(?GTPv2_IEI_METRIC) ->
    metric;
parse_iei(?GTPv2_IEI_SEQUENCE_NUMBER) ->
    sequence_number;
parse_iei(?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY) ->
    apn_and_relative_capacity;
parse_iei(?GTPv2_IEI_WLAN_OFFLOADABILITY_INDICATION) ->
    wlan_offloadability_indication;
parse_iei(?GTPv2_IEI_PAGING_AND_SERVICE_INFORMATION) ->
    paging_and_service_information;
parse_iei(?GTPv2_IEI_INTEGER_NUMBER) ->
    integer_number;
parse_iei(?GTPv2_IEI_MILLISECOND_TIME_STAMP) ->
    millisecond_time_stamp;
parse_iei(?GTPv2_IEI_MONITORING_EVENT_INFORMATION) ->
    monitoring_event_information;
parse_iei(?GTPv2_IEI_ECGI_LIST) ->
    ecgi_list;
parse_iei(?GTPv2_IEI_REMOTE_UE_CONTEXT) ->
    remote_ue_context;
parse_iei(?GTPv2_IEI_REMOTE_USER_ID) ->
    remote_user_id;
parse_iei(?GTPv2_IEI_REMOTE_UE_IP_INFORMATION) ->
    remote_ue_ip_information;
parse_iei(?GTPv2_IEI_CIOT_OPTIMIZATIONS_SUPPORT_INDICATION) ->
    ciot_optimizations_support_indication;
parse_iei(?GTPv2_IEI_SCEF_PDN_CONNECTION) ->
    scef_pdn_connection;
parse_iei(?GTPv2_IEI_HEADER_COMPRESSION_CONFIGURATION) ->
    header_compression_configuration;
parse_iei(?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS) ->
    extended_protocol_configuration_options;
parse_iei(?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL) ->
    serving_plmn_rate_control;
parse_iei(?GTPv2_IEI_COUNTER) ->
    counter;
parse_iei(?GTPv2_IEI_MAPPED_UE_USAGE_TYPE) ->
    mapped_ue_usage_type;
parse_iei(?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT) ->
    secondary_rat_usage_data_report;
parse_iei(?GTPv2_IEI_UP_FUNCTION_SELECTION_INDICATION_FLAGS) ->
    up_function_selection_indication_flags;
parse_iei(?GTPv2_IEI_MAXIMUM_PACKET_LOSS_RATE) ->
    maximum_packet_loss_rate;
parse_iei(?GTPv2_IEI_APN_RATE_CONTROL_STATUS) ->
    apn_rate_control_status;
parse_iei(?GTPv2_IEI_EXTENDED_TRACE_INFORMATION) ->
    extended_trace_information;
parse_iei(?GTPv2_IEI_MONITORING_EVENT_EXTENSION_INFORMATION) ->
    monitoring_event_extension_information;
parse_iei(?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX) ->
    additional_rrm_policy_index;
parse_iei(?GTPv2_IEI_V2X_CONTEXT) ->
    v2x_context;
parse_iei(?GTPv2_IEI_PC5_QOS_PARAMETERS) ->
    pc5_qos_parameters;
parse_iei(?GTPv2_IEI_SERVICES_AUTHORIZED) ->
    services_authorized;
parse_iei(?GTPv2_IEI_BIT_RATE) ->
    bit_rate;
parse_iei(?GTPv2_IEI_PC5_QOS_FLOW) ->
    pc5_qos_flow;
parse_iei(?GTPv2_IEI_SGI_PTP_TUNNEL_ADDRESS) ->
    sgi_ptp_tunnel_address;
parse_iei(?GTPv2_IEI_PGW_CHANGE_INFO) ->
    pgw_change_info;
parse_iei(?GTPv2_IEI_PGW_FQDN) ->
    pgw_fqdn;
parse_iei(?GTPv2_IEI_GROUP_ID) ->
    group_id;
parse_iei(?GTPv2_IEI_PSCELL_ID) ->
    pscell_id;
parse_iei(?GTPv2_IEI_UP_SECURITY_POLICY) ->
    up_security_policy;
parse_iei(?GTPv2_IEI_ALTERNATIVE_IMSI) ->
    alternative_imsi;
parse_iei(?GTPv2_IEI_SPECIAL_IE_TYPE_FOR_IE_TYPE_EXTENSION) ->
    special_ie_type_for_ie_type_extension;
parse_iei(?GTPv2_IEI_PRIVATE_EXTENSION) ->
    private_extension.

compose_iei(imsi) ->
    ?GTPv2_IEI_IMSI;
compose_iei(cause) ->
    ?GTPv2_IEI_CAUSE;
compose_iei(recovery_restart_counter) ->
    ?GTPv2_IEI_RECOVERY_RESTART_COUNTER;
compose_iei(stn_sr) ->
    ?GTPv2_IEI_STN_SR;
compose_iei(srvcc_cause) ->
    ?GTPv2_IEI_SRVCC_CAUSE;
compose_iei(apn) ->
    ?GTPv2_IEI_APN;
compose_iei(ambr) ->
    ?GTPv2_IEI_AMBR;
compose_iei(eps_bearer_id) ->
    ?GTPv2_IEI_EPS_BEARER_ID;
compose_iei(ip_address) ->
    ?GTPv2_IEI_IP_ADDRESS;
compose_iei(mei) ->
    ?GTPv2_IEI_MEI;
compose_iei(msisdn) ->
    ?GTPv2_IEI_MSISDN;
compose_iei(indication) ->
    ?GTPv2_IEI_INDICATION;
compose_iei(protocol_configuration_options) ->
    ?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS;
compose_iei(pdn_address_allocation) ->
    ?GTPv2_IEI_PDN_ADDRESS_ALLOCATION;
compose_iei(bearer_qos) ->
    ?GTPv2_IEI_BEARER_QOS;
compose_iei(flow_qos) ->
    ?GTPv2_IEI_FLOW_QOS;
compose_iei(rat_type) ->
    ?GTPv2_IEI_RAT_TYPE;
compose_iei(serving_network) ->
    ?GTPv2_IEI_SERVING_NETWORK;
compose_iei(bearer_tft) ->
    ?GTPv2_IEI_BEARER_TFT;
compose_iei(traffic_aggregation_description) ->
    ?GTPv2_IEI_TRAFFIC_AGGREGATION_DESCRIPTION;
compose_iei(user_location_information) ->
    ?GTPv2_IEI_USER_LOCATION_INFORMATION;
compose_iei(f_teid) ->
    ?GTPv2_IEI_F_TEID;
compose_iei(tmsi) ->
    ?GTPv2_IEI_TMSI;
compose_iei(global_cn_id) ->
    ?GTPv2_IEI_GLOBAL_CN_ID;
compose_iei(s103_pdn_data_forwarding_info) ->
    ?GTPv2_IEI_S103_PDN_DATA_FORWARDING_INFO;
compose_iei(s1_u_data_forwarding_info) ->
    ?GTPv2_IEI_S1_U_DATA_FORWARDING_INFO;
compose_iei(delay_value) ->
    ?GTPv2_IEI_DELAY_VALUE;
compose_iei(bearer_context) ->
    ?GTPv2_IEI_BEARER_CONTEXT;
compose_iei(charging_id) ->
    ?GTPv2_IEI_CHARGING_ID;
compose_iei(charging_characteristics) ->
    ?GTPv2_IEI_CHARGING_CHARACTERISTICS;
compose_iei(trace_information) ->
    ?GTPv2_IEI_TRACE_INFORMATION;
compose_iei(bearer_flags) ->
    ?GTPv2_IEI_BEARER_FLAGS;
compose_iei(pdn_type) ->
    ?GTPv2_IEI_PDN_TYPE;
compose_iei(procedure_transaction_id) ->
    ?GTPv2_IEI_PROCEDURE_TRANSACTION_ID;
compose_iei(mm_context_gsm_key_and_triplets) ->
    ?GTPv2_IEI_MM_CONTEXT_GSM_KEY_AND_TRIPLETS;
compose_iei(mm_context_umts_key_cipher_and_quintuplets) ->
    ?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_CIPHER_AND_QUINTUPLETS;
compose_iei(mm_context_gsm_key_cipher_and_quintuplets) ->
    ?GTPv2_IEI_MM_CONTEXT_GSM_KEY_CIPHER_AND_QUINTUPLETS;
compose_iei(mm_context_umts_key_and_quintuplets) ->
    ?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_AND_QUINTUPLETS;
compose_iei(mm_context_eps_security_context_quadruplets_and_quintuplets) ->
    ?GTPv2_IEI_MM_CONTEXT_EPS_SECURITY_CONTEXT_QUADRUPLETS_AND_QUINTUPLETS;
compose_iei(mm_context_umts_key_quadruplets_and_quintuplets) ->
    ?GTPv2_IEI_MM_CONTEXT_UMTS_KEY_QUADRUPLETS_AND_QUINTUPLETS;
compose_iei(pdn_connection) ->
    ?GTPv2_IEI_PDN_CONNECTION;
compose_iei(pdu_numbers) ->
    ?GTPv2_IEI_PDU_NUMBERS;
compose_iei(p_tmsi) ->
    ?GTPv2_IEI_P_TMSI;
compose_iei(p_tmsi_signature) ->
    ?GTPv2_IEI_P_TMSI_SIGNATURE;
compose_iei(hop_counter) ->
    ?GTPv2_IEI_HOP_COUNTER;
compose_iei(ue_time_zone) ->
    ?GTPv2_IEI_UE_TIME_ZONE;
compose_iei(trace_reference) ->
    ?GTPv2_IEI_TRACE_REFERENCE;
compose_iei(complete_request_message) ->
    ?GTPv2_IEI_COMPLETE_REQUEST_MESSAGE;
compose_iei(guti) ->
    ?GTPv2_IEI_GUTI;
compose_iei(f_container) ->
    ?GTPv2_IEI_F_CONTAINER;
compose_iei(f_cause) ->
    ?GTPv2_IEI_F_CAUSE;
compose_iei(plmn_id) ->
    ?GTPv2_IEI_PLMN_ID;
compose_iei(target_identification) ->
    ?GTPv2_IEI_TARGET_IDENTIFICATION;
compose_iei(packet_flow_id) ->
    ?GTPv2_IEI_PACKET_FLOW_ID;
compose_iei(rab_context) ->
    ?GTPv2_IEI_RAB_CONTEXT;
compose_iei(source_rnc_pdcp_context_info) ->
    ?GTPv2_IEI_SOURCE_RNC_PDCP_CONTEXT_INFO;
compose_iei(port_number) ->
    ?GTPv2_IEI_PORT_NUMBER;
compose_iei(apn_restriction) ->
    ?GTPv2_IEI_APN_RESTRICTION;
compose_iei(selection_mode) ->
    ?GTPv2_IEI_SELECTION_MODE;
compose_iei(source_identification) ->
    ?GTPv2_IEI_SOURCE_IDENTIFICATION;
compose_iei(change_reporting_action) ->
    ?GTPv2_IEI_CHANGE_REPORTING_ACTION;
compose_iei(fq_csid) ->
    ?GTPv2_IEI_FQ_CSID;
compose_iei(channel_needed) ->
    ?GTPv2_IEI_CHANNEL_NEEDED;
compose_iei(emlpp_priority) ->
    ?GTPv2_IEI_EMLPP_PRIORITY;
compose_iei(node_type) ->
    ?GTPv2_IEI_NODE_TYPE;
compose_iei(fqdn) ->
    ?GTPv2_IEI_FQDN;
compose_iei(transaction_identifier) ->
    ?GTPv2_IEI_TRANSACTION_IDENTIFIER;
compose_iei(mbms_session_duration) ->
    ?GTPv2_IEI_MBMS_SESSION_DURATION;
compose_iei(mbms_service_area) ->
    ?GTPv2_IEI_MBMS_SERVICE_AREA;
compose_iei(mbms_session_identifier) ->
    ?GTPv2_IEI_MBMS_SESSION_IDENTIFIER;
compose_iei(mbms_flow_identifier) ->
    ?GTPv2_IEI_MBMS_FLOW_IDENTIFIER;
compose_iei(mbms_ip_multicast_distribution) ->
    ?GTPv2_IEI_MBMS_IP_MULTICAST_DISTRIBUTION;
compose_iei(mbms_distribution_acknowledge) ->
    ?GTPv2_IEI_MBMS_DISTRIBUTION_ACKNOWLEDGE;
compose_iei(rfsp_index) ->
    ?GTPv2_IEI_RFSP_INDEX;
compose_iei(user_csg_information) ->
    ?GTPv2_IEI_USER_CSG_INFORMATION;
compose_iei(csg_information_reporting_action) ->
    ?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION;
compose_iei(csg_id) ->
    ?GTPv2_IEI_CSG_ID;
compose_iei(csg_membership_indication) ->
    ?GTPv2_IEI_CSG_MEMBERSHIP_INDICATION;
compose_iei(service_indicator) ->
    ?GTPv2_IEI_SERVICE_INDICATOR;
compose_iei(detach_type) ->
    ?GTPv2_IEI_DETACH_TYPE;
compose_iei(local_distiguished_name) ->
    ?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME;
compose_iei(node_features) ->
    ?GTPv2_IEI_NODE_FEATURES;
compose_iei(mbms_time_to_data_transfer) ->
    ?GTPv2_IEI_MBMS_TIME_TO_DATA_TRANSFER;
compose_iei(throttling) ->
    ?GTPv2_IEI_THROTTLING;
compose_iei(allocationretention_priority) ->
    ?GTPv2_IEI_ALLOCATIONRETENTION_PRIORITY;
compose_iei(epc_timer) ->
    ?GTPv2_IEI_EPC_TIMER;
compose_iei(signalling_priority_indication) ->
    ?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION;
compose_iei(temporary_mobile_group_identity) ->
    ?GTPv2_IEI_TEMPORARY_MOBILE_GROUP_IDENTITY;
compose_iei(additional_mm_context_for_srvcc) ->
    ?GTPv2_IEI_ADDITIONAL_MM_CONTEXT_FOR_SRVCC;
compose_iei(additional_flags_for_srvcc) ->
    ?GTPv2_IEI_ADDITIONAL_FLAGS_FOR_SRVCC;
compose_iei(mdt_configuration) ->
    ?GTPv2_IEI_MDT_CONFIGURATION;
compose_iei(additional_protocol_configuration_options) ->
    ?GTPv2_IEI_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS;
compose_iei(absolute_time_of_mbms_data_transfer) ->
    ?GTPv2_IEI_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER;
compose_iei(henb_information_reporting) ->
    ?GTPv2_IEI_HENB_INFORMATION_REPORTING;
compose_iei(ipv4_configuration_parameters) ->
    ?GTPv2_IEI_IPV4_CONFIGURATION_PARAMETERS;
compose_iei(change_to_report_flags) ->
    ?GTPv2_IEI_CHANGE_TO_REPORT_FLAGS;
compose_iei(action_indication) ->
    ?GTPv2_IEI_ACTION_INDICATION;
compose_iei(twan_identifier) ->
    ?GTPv2_IEI_TWAN_IDENTIFIER;
compose_iei(uli_timestamp) ->
    ?GTPv2_IEI_ULI_TIMESTAMP;
compose_iei(mbms_flags) ->
    ?GTPv2_IEI_MBMS_FLAGS;
compose_iei(rannas_cause) ->
    ?GTPv2_IEI_RANNAS_CAUSE;
compose_iei(cn_operator_selection_entity) ->
    ?GTPv2_IEI_CN_OPERATOR_SELECTION_ENTITY;
compose_iei(trusted_wlan_mode_indication) ->
    ?GTPv2_IEI_TRUSTED_WLAN_MODE_INDICATION;
compose_iei(node_number) ->
    ?GTPv2_IEI_NODE_NUMBER;
compose_iei(node_identifier) ->
    ?GTPv2_IEI_NODE_IDENTIFIER;
compose_iei(presence_reporting_area_action) ->
    ?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION;
compose_iei(presence_reporting_area_information) ->
    ?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION;
compose_iei(twan_identifier_timestamp) ->
    ?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP;
compose_iei(overload_control_information) ->
    ?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION;
compose_iei(load_control_information) ->
    ?GTPv2_IEI_LOAD_CONTROL_INFORMATION;
compose_iei(metric) ->
    ?GTPv2_IEI_METRIC;
compose_iei(sequence_number) ->
    ?GTPv2_IEI_SEQUENCE_NUMBER;
compose_iei(apn_and_relative_capacity) ->
    ?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY;
compose_iei(wlan_offloadability_indication) ->
    ?GTPv2_IEI_WLAN_OFFLOADABILITY_INDICATION;
compose_iei(paging_and_service_information) ->
    ?GTPv2_IEI_PAGING_AND_SERVICE_INFORMATION;
compose_iei(integer_number) ->
    ?GTPv2_IEI_INTEGER_NUMBER;
compose_iei(millisecond_time_stamp) ->
    ?GTPv2_IEI_MILLISECOND_TIME_STAMP;
compose_iei(monitoring_event_information) ->
    ?GTPv2_IEI_MONITORING_EVENT_INFORMATION;
compose_iei(ecgi_list) ->
    ?GTPv2_IEI_ECGI_LIST;
compose_iei(remote_ue_context) ->
    ?GTPv2_IEI_REMOTE_UE_CONTEXT;
compose_iei(remote_user_id) ->
    ?GTPv2_IEI_REMOTE_USER_ID;
compose_iei(remote_ue_ip_information) ->
    ?GTPv2_IEI_REMOTE_UE_IP_INFORMATION;
compose_iei(ciot_optimizations_support_indication) ->
    ?GTPv2_IEI_CIOT_OPTIMIZATIONS_SUPPORT_INDICATION;
compose_iei(scef_pdn_connection) ->
    ?GTPv2_IEI_SCEF_PDN_CONNECTION;
compose_iei(header_compression_configuration) ->
    ?GTPv2_IEI_HEADER_COMPRESSION_CONFIGURATION;
compose_iei(extended_protocol_configuration_options) ->
    ?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS;
compose_iei(serving_plmn_rate_control) ->
    ?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL;
compose_iei(counter) ->
    ?GTPv2_IEI_COUNTER;
compose_iei(mapped_ue_usage_type) ->
    ?GTPv2_IEI_MAPPED_UE_USAGE_TYPE;
compose_iei(secondary_rat_usage_data_report) ->
    ?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT;
compose_iei(up_function_selection_indication_flags) ->
    ?GTPv2_IEI_UP_FUNCTION_SELECTION_INDICATION_FLAGS;
compose_iei(maximum_packet_loss_rate) ->
    ?GTPv2_IEI_MAXIMUM_PACKET_LOSS_RATE;
compose_iei(apn_rate_control_status) ->
    ?GTPv2_IEI_APN_RATE_CONTROL_STATUS;
compose_iei(extended_trace_information) ->
    ?GTPv2_IEI_EXTENDED_TRACE_INFORMATION;
compose_iei(monitoring_event_extension_information) ->
    ?GTPv2_IEI_MONITORING_EVENT_EXTENSION_INFORMATION;
compose_iei(additional_rrm_policy_index) ->
    ?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX;
compose_iei(v2x_context) ->
    ?GTPv2_IEI_V2X_CONTEXT;
compose_iei(pc5_qos_parameters) ->
    ?GTPv2_IEI_PC5_QOS_PARAMETERS;
compose_iei(services_authorized) ->
    ?GTPv2_IEI_SERVICES_AUTHORIZED;
compose_iei(bit_rate) ->
    ?GTPv2_IEI_BIT_RATE;
compose_iei(pc5_qos_flow) ->
    ?GTPv2_IEI_PC5_QOS_FLOW;
compose_iei(sgi_ptp_tunnel_address) ->
    ?GTPv2_IEI_SGI_PTP_TUNNEL_ADDRESS;
compose_iei(pgw_change_info) ->
    ?GTPv2_IEI_PGW_CHANGE_INFO;
compose_iei(pgw_fqdn) ->
    ?GTPv2_IEI_PGW_FQDN;
compose_iei(group_id) ->
    ?GTPv2_IEI_GROUP_ID;
compose_iei(pscell_id) ->
    ?GTPv2_IEI_PSCELL_ID;
compose_iei(up_security_policy) ->
    ?GTPv2_IEI_UP_SECURITY_POLICY;
compose_iei(alternative_imsi) ->
    ?GTPv2_IEI_ALTERNATIVE_IMSI;
compose_iei(special_ie_type_for_ie_type_extension) ->
    ?GTPv2_IEI_SPECIAL_IE_TYPE_FOR_IE_TYPE_EXTENSION;
compose_iei(private_extension) ->
    ?GTPv2_IEI_PRIVATE_EXTENSION.



decode_parameter(imsi, V) ->
    %% ITU-T Rec E.212 TBCD digits
    V;
decode_parameter(cause, V) ->
    <<C:8, _S:5, PCE:1, BCE:1, CS:1, Rest0/binary>> = V,
    Is = case C >= 16 andalso C =< 63 of
             true ->
                 #{};
             false ->
                 BEs = [{bearer_context_ie_error, true} || 1 == BCE],
                 PEs = [{pdn_connection_ie_error, true} || 1 == PCE],
                 maps:from_list(BEs ++ PEs)
         end,
    Cause = parse_cause_type(C),
    Base = Is#{cause => Cause,
               cause_source => case CS of
                                   0 -> originated_by_remote_node;
                                   1 -> originated_by_sending_node
                               end},
    case Rest0 of
        <<>> ->
            Base;
        <<IEI:8, _:20, Inst:4, _/binary>> ->
            Base#{offending_iei => IEI,
                  offending_ie_instance => Inst}
    end;
decode_parameter(recovery_restart_counter, V) ->
    V;
decode_parameter(apn, V) ->
    %% The encoding the APN field follows 3GPP TS 23.003 [2] clause
    %% 9.1. The content of the APN field shall be the full APN with
    %% both the APN Network Identifier and APN Operator Identifier
    %% being present as specified in 3GPP TS 23.003 [2] clauses 9.1.1
    %% and 9.1.2, 3GPP TS 23.060 [35] Annex A and 3GPP TS 23.401 [3]
    %% clauses 4.3.8.1.
    V;
decode_parameter(ambr, V) ->
    <<Uplink:32, Downlink:32>> = V,
    #{uplink => Uplink,
      downlink => Downlink};
decode_parameter(ebi, V) ->
    <<_:4, EBI:4, _/binary>> = V,
    EBI;
decode_parameter(ip_address, V) ->
    inet:parse_address(V);
decode_parameter(mei, V) ->
    %% The ME Identity field contains either the IMEI or the IMEISV as
    %% defined in clause 6.2 of 3GPP TS 23.003 [2]. It is encoded as
    %% specified in clause 7.7.53 of 3GPP TS 29.060 [4], beginning
    %% with octet 4 of Figure 7.7.53.1.
    %% The IMEI(SV) digits are encoded using BCD coding where IMEI is
    %% 15 BCD digits and IMEISV is 16 BCD digits. For IMEI, bits 5 to
    %% 8 of the last octet shall be filled with an end mark coded as
    %% '1111'.
    V;
decode_parameter(_, V) ->
    V.

parse_cause_type(?GTPv2_CAUSE_TYPE_LOCAL_DETACH) ->
    local_detach;
parse_cause_type(?GTPv2_CAUSE_TYPE_COMPLETE_DETACH) ->
    complete_detach;
parse_cause_type(?GTPv2_CAUSE_TYPE_RAT_CHANGED_FROM_3GPP_TO_NON_3GPP) ->
    rat_changed_from_3gpp_to_non_3gpp;
parse_cause_type(?GTPv2_CAUSE_TYPE_ISR_DEACTIVATION) ->
    isr_deactivation;
parse_cause_type(?GTPv2_CAUSE_TYPE_ERROR_INDICATION_RECEIVED_FROM_RNCENODEBS4_SGSNMME) ->
    error_indication_received_from_rncenodebs4_sgsnmme;
parse_cause_type(?GTPv2_CAUSE_TYPE_IMSI_DETACH_ONLY) ->
    imsi_detach_only;
parse_cause_type(?GTPv2_CAUSE_TYPE_REACTIVATION_REQUESTED) ->
    reactivation_requested;
parse_cause_type(?GTPv2_CAUSE_TYPE_PDN_RECONNECTION_TO_THIS_APN_DISALLOWED) ->
    pdn_reconnection_to_this_apn_disallowed;
parse_cause_type(?GTPv2_CAUSE_TYPE_ACCESS_CHANGED_FROM_NON_3GPP_TO_3GPP) ->
    access_changed_from_non_3gpp_to_3gpp;
parse_cause_type(?GTPv2_CAUSE_TYPE_PDN_CONNECTION_INACTIVITY_TIMER_EXPIRES) ->
    pdn_connection_inactivity_timer_expires;
parse_cause_type(?GTPv2_CAUSE_TYPE_PGW_NOT_RESPONDING) ->
    pgw_not_responding;
parse_cause_type(?GTPv2_CAUSE_TYPE_NETWORK_FAILURE) ->
    network_failure;
parse_cause_type(?GTPv2_CAUSE_TYPE_QOS_PARAMETER_MISMATCH) ->
    qos_parameter_mismatch;
parse_cause_type(?GTPv2_CAUSE_TYPE_EPS_TO_5GS_MOBILITY) ->
    eps_to_5gs_mobility;
parse_cause_type(?GTPv2_CAUSE_TYPE_REQUEST_ACCEPTED) ->
    request_accepted;
parse_cause_type(?GTPv2_CAUSE_TYPE_REQUEST_ACCEPTED_PARTIALLY) ->
    request_accepted_partially;
parse_cause_type(?GTPv2_CAUSE_TYPE_NEW_PDN_TYPE_DUE_TO_NETWORK_PREFERENCE) ->
    new_pdn_type_due_to_network_preference;
parse_cause_type(?GTPv2_CAUSE_TYPE_NEW_PDN_TYPE_DUE_TO_SINGLE_ADDRESS_BEARER_ONLY) ->
    new_pdn_type_due_to_single_address_bearer_only;
parse_cause_type(?GTPv2_CAUSE_TYPE_CONTEXT_NOT_FOUND) ->
    context_not_found;
parse_cause_type(?GTPv2_CAUSE_TYPE_INVALID_MESSAGE_FORMAT) ->
    invalid_message_format;
parse_cause_type(?GTPv2_CAUSE_TYPE_VERSION_NOT_SUPPORTED_BY_NEXT_PEER) ->
    version_not_supported_by_next_peer;
parse_cause_type(?GTPv2_CAUSE_TYPE_INVALID_LENGTH) ->
    invalid_length;
parse_cause_type(?GTPv2_CAUSE_TYPE_SERVICE_NOT_SUPPORTED) ->
    service_not_supported;
parse_cause_type(?GTPv2_CAUSE_TYPE_MANDATORY_IE_INCORRECT) ->
    mandatory_ie_incorrect;
parse_cause_type(?GTPv2_CAUSE_TYPE_MANDATORY_IE_MISSING) ->
    mandatory_ie_missing;
parse_cause_type(?GTPv2_CAUSE_TYPE_SYSTEM_FAILURE) ->
    system_failure;
parse_cause_type(?GTPv2_CAUSE_TYPE_NO_RESOURCES_AVAILABLE) ->
    no_resources_available;
parse_cause_type(?GTPv2_CAUSE_TYPE_SEMANTIC_ERROR_IN_THE_TFT_OPERATION) ->
    semantic_error_in_the_tft_operation;
parse_cause_type(?GTPv2_CAUSE_TYPE_SYNTACTIC_ERROR_IN_THE_TFT_OPERATION) ->
    syntactic_error_in_the_tft_operation;
parse_cause_type(?GTPv2_CAUSE_TYPE_SEMANTIC_ERRORS_IN_PACKET_FILTERS) ->
    semantic_errors_in_packet_filters;
parse_cause_type(?GTPv2_CAUSE_TYPE_SYNTACTIC_ERRORS_IN_PACKET_FILTERS) ->
    syntactic_errors_in_packet_filters;
parse_cause_type(?GTPv2_CAUSE_TYPE_MISSING_OR_UNKNOWN_APN) ->
    missing_or_unknown_apn;
parse_cause_type(?GTPv2_CAUSE_TYPE_GRE_KEY_NOT_FOUND) ->
    gre_key_not_found;
parse_cause_type(?GTPv2_CAUSE_TYPE_RELOCATION_FAILURE) ->
    relocation_failure;
parse_cause_type(?GTPv2_CAUSE_TYPE_DENIED_IN_RAT) ->
    denied_in_rat;
parse_cause_type(?GTPv2_CAUSE_TYPE_PREFERRED_PDN_TYPE_NOT_SUPPORTED) ->
    preferred_pdn_type_not_supported;
parse_cause_type(?GTPv2_CAUSE_TYPE_ALL_DYNAMIC_ADDRESSES_ARE_OCCUPIED) ->
    all_dynamic_addresses_are_occupied;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_CONTEXT_WITHOUT_TFT_ALREADY_ACTIVATED) ->
    ue_context_without_tft_already_activated;
parse_cause_type(?GTPv2_CAUSE_TYPE_PROTOCOL_TYPE_NOT_SUPPORTED) ->
    protocol_type_not_supported;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_NOT_RESPONDING) ->
    ue_not_responding;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_REFUSES) ->
    ue_refuses;
parse_cause_type(?GTPv2_CAUSE_TYPE_SERVICE_DENIED) ->
    service_denied;
parse_cause_type(?GTPv2_CAUSE_TYPE_UNABLE_TO_PAGE_UE) ->
    unable_to_page_ue;
parse_cause_type(?GTPv2_CAUSE_TYPE_NO_MEMORY_AVAILABLE) ->
    no_memory_available;
parse_cause_type(?GTPv2_CAUSE_TYPE_USER_AUTHENTICATION_FAILED) ->
    user_authentication_failed;
parse_cause_type(?GTPv2_CAUSE_TYPE_APN_ACCESS_DENIED_NO_SUBSCRIPTION) ->
    apn_access_denied_no_subscription;
parse_cause_type(?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_REASON_NOT_SPECIFIED) ->
    request_rejected_reason_not_specified;
parse_cause_type(?GTPv2_CAUSE_TYPE_P_TMSI_SIGNATURE_MISMATCH) ->
    p_tmsi_signature_mismatch;
parse_cause_type(?GTPv2_CAUSE_TYPE_IMSIIMEI_NOT_KNOWN) ->
    imsiimei_not_known;
parse_cause_type(?GTPv2_CAUSE_TYPE_SEMANTIC_ERROR_IN_THE_TAD_OPERATION) ->
    semantic_error_in_the_tad_operation;
parse_cause_type(?GTPv2_CAUSE_TYPE_SYNTACTIC_ERROR_IN_THE_TAD_OPERATION) ->
    syntactic_error_in_the_tad_operation;
parse_cause_type(?GTPv2_CAUSE_TYPE_REMOTE_PEER_NOT_RESPONDING) ->
    remote_peer_not_responding;
parse_cause_type(?GTPv2_CAUSE_TYPE_COLLISION_WITH_NETWORK_INITIATED_REQUEST) ->
    collision_with_network_initiated_request;
parse_cause_type(?GTPv2_CAUSE_TYPE_UNABLE_TO_PAGE_UE_DUE_TO_SUSPENSION) ->
    unable_to_page_ue_due_to_suspension;
parse_cause_type(?GTPv2_CAUSE_TYPE_CONDITIONAL_IE_MISSING) ->
    conditional_ie_missing;
parse_cause_type(?GTPv2_CAUSE_TYPE_APN_RESTRICTION_TYPE_INCOMPATIBLE_WITH_CURRENTLY_ACTIVE_PDN_CONNECTION) ->
    apn_restriction_type_incompatible_with_currently_active_pdn_connection;
parse_cause_type(?GTPv2_CAUSE_TYPE_INVALID_OVERALL_LENGTH_OF_THE_TRIGGERED_RESPONSE_MESSAGE_AND_A_PIGGYBACKED_INITIAL_MESSAGE) ->
    invalid_overall_length_of_the_triggered_response_message_and_a_piggybacked_initial_message;
parse_cause_type(?GTPv2_CAUSE_TYPE_DATA_FORWARDING_NOT_SUPPORTED) ->
    data_forwarding_not_supported;
parse_cause_type(?GTPv2_CAUSE_TYPE_INVALID_REPLY_FROM_REMOTE_PEER) ->
    invalid_reply_from_remote_peer;
parse_cause_type(?GTPv2_CAUSE_TYPE_FALLBACK_TO_GTPV1) ->
    fallback_to_gtpv1;
parse_cause_type(?GTPv2_CAUSE_TYPE_INVALID_PEER) ->
    invalid_peer;
parse_cause_type(?GTPv2_CAUSE_TYPE_TEMPORARILY_REJECTED_DUE_TO_HANDOVERTAURAU_PROCEDURE_IN_PROGRESS) ->
    temporarily_rejected_due_to_handovertaurau_procedure_in_progress;
parse_cause_type(?GTPv2_CAUSE_TYPE_MODIFICATIONS_NOT_LIMITED_TO_S1_U_BEARERS) ->
    modifications_not_limited_to_s1_u_bearers;
parse_cause_type(?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_FOR_A_PMIPV6_REASON) ->
    request_rejected_for_a_pmipv6_reason;
parse_cause_type(?GTPv2_CAUSE_TYPE_APN_CONGESTION) ->
    apn_congestion;
parse_cause_type(?GTPv2_CAUSE_TYPE_BEARER_HANDLING_NOT_SUPPORTED) ->
    bearer_handling_not_supported;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_ALREADY_RE_ATTACHED) ->
    ue_already_re_attached;
parse_cause_type(?GTPv2_CAUSE_TYPE_MULTIPLE_PDN_CONNECTIONS_FOR_A_GIVEN_APN_NOT_ALLOWED) ->
    multiple_pdn_connections_for_a_given_apn_not_allowed;
parse_cause_type(?GTPv2_CAUSE_TYPE_TARGET_ACCESS_RESTRICTED_FOR_THE_SUBSCRIBER) ->
    target_access_restricted_for_the_subscriber;
parse_cause_type(?GTPv2_CAUSE_TYPE_MMESGSN_REFUSES_DUE_TO_VPLMN_POLICY) ->
    mmesgsn_refuses_due_to_vplmn_policy;
parse_cause_type(?GTPv2_CAUSE_TYPE_GTP_C_ENTITY_CONGESTION) ->
    gtp_c_entity_congestion;
parse_cause_type(?GTPv2_CAUSE_TYPE_LATE_OVERLAPPING_REQUEST) ->
    late_overlapping_request;
parse_cause_type(?GTPv2_CAUSE_TYPE_TIMED_OUT_REQUEST) ->
    timed_out_request;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_IS_TEMPORARILY_NOT_REACHABLE_DUE_TO_POWER_SAVING) ->
    ue_is_temporarily_not_reachable_due_to_power_saving;
parse_cause_type(?GTPv2_CAUSE_TYPE_RELOCATION_FAILURE_DUE_TO_NAS_MESSAGE_REDIRECTION) ->
    relocation_failure_due_to_nas_message_redirection;
parse_cause_type(?GTPv2_CAUSE_TYPE_UE_NOT_AUTHORISED_BY_OCS_OR_EXTERNAL_AAA_SERVER) ->
    ue_not_authorised_by_ocs_or_external_aaa_server;
parse_cause_type(?GTPv2_CAUSE_TYPE_MULTIPLE_ACCESSES_TO_A_PDN_CONNECTION_NOT_ALLOWED) ->
    multiple_accesses_to_a_pdn_connection_not_allowed;
parse_cause_type(?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_DUE_TO_UE_CAPABILITY) ->
    request_rejected_due_to_ue_capability;
parse_cause_type(?GTPv2_CAUSE_TYPE_S1_U_PATH_FAILURE) ->
    s1_u_path_failure;
parse_cause_type(?GTPv2_CAUSE_TYPE_5GC_NOT_ALLOWED) ->
    '5gc_not_allowed';
parse_cause_type(?GTPv2_CAUSE_TYPE_PGW_MISMATCH_WITH_NETWORK_SLICE_SUBSCRIBED_BY_THE_UE) ->
    pgw_mismatch_with_network_slice_subscribed_by_the_ue;
parse_cause_type(?GTPv2_CAUSE_TYPE_REJECTION_DUE_TO_PAGING_RESTRICTION) ->
    rejection_due_to_paging_restriction.

compose_cause_type(local_detach) ->
    ?GTPv2_CAUSE_TYPE_LOCAL_DETACH;
compose_cause_type(complete_detach) ->
    ?GTPv2_CAUSE_TYPE_COMPLETE_DETACH;
compose_cause_type(rat_changed_from_3gpp_to_non_3gpp) ->
    ?GTPv2_CAUSE_TYPE_RAT_CHANGED_FROM_3GPP_TO_NON_3GPP;
compose_cause_type(isr_deactivation) ->
    ?GTPv2_CAUSE_TYPE_ISR_DEACTIVATION;
compose_cause_type(error_indication_received_from_rncenodebs4_sgsnmme) ->
    ?GTPv2_CAUSE_TYPE_ERROR_INDICATION_RECEIVED_FROM_RNCENODEBS4_SGSNMME;
compose_cause_type(imsi_detach_only) ->
    ?GTPv2_CAUSE_TYPE_IMSI_DETACH_ONLY;
compose_cause_type(reactivation_requested) ->
    ?GTPv2_CAUSE_TYPE_REACTIVATION_REQUESTED;
compose_cause_type(pdn_reconnection_to_this_apn_disallowed) ->
    ?GTPv2_CAUSE_TYPE_PDN_RECONNECTION_TO_THIS_APN_DISALLOWED;
compose_cause_type(access_changed_from_non_3gpp_to_3gpp) ->
    ?GTPv2_CAUSE_TYPE_ACCESS_CHANGED_FROM_NON_3GPP_TO_3GPP;
compose_cause_type(pdn_connection_inactivity_timer_expires) ->
    ?GTPv2_CAUSE_TYPE_PDN_CONNECTION_INACTIVITY_TIMER_EXPIRES;
compose_cause_type(pgw_not_responding) ->
    ?GTPv2_CAUSE_TYPE_PGW_NOT_RESPONDING;
compose_cause_type(network_failure) ->
    ?GTPv2_CAUSE_TYPE_NETWORK_FAILURE;
compose_cause_type(qos_parameter_mismatch) ->
    ?GTPv2_CAUSE_TYPE_QOS_PARAMETER_MISMATCH;
compose_cause_type(eps_to_5gs_mobility) ->
    ?GTPv2_CAUSE_TYPE_EPS_TO_5GS_MOBILITY;
compose_cause_type(request_accepted) ->
    ?GTPv2_CAUSE_TYPE_REQUEST_ACCEPTED;
compose_cause_type(request_accepted_partially) ->
    ?GTPv2_CAUSE_TYPE_REQUEST_ACCEPTED_PARTIALLY;
compose_cause_type(new_pdn_type_due_to_network_preference) ->
    ?GTPv2_CAUSE_TYPE_NEW_PDN_TYPE_DUE_TO_NETWORK_PREFERENCE;
compose_cause_type(new_pdn_type_due_to_single_address_bearer_only) ->
    ?GTPv2_CAUSE_TYPE_NEW_PDN_TYPE_DUE_TO_SINGLE_ADDRESS_BEARER_ONLY;
compose_cause_type(context_not_found) ->
    ?GTPv2_CAUSE_TYPE_CONTEXT_NOT_FOUND;
compose_cause_type(invalid_message_format) ->
    ?GTPv2_CAUSE_TYPE_INVALID_MESSAGE_FORMAT;
compose_cause_type(version_not_supported_by_next_peer) ->
    ?GTPv2_CAUSE_TYPE_VERSION_NOT_SUPPORTED_BY_NEXT_PEER;
compose_cause_type(invalid_length) ->
    ?GTPv2_CAUSE_TYPE_INVALID_LENGTH;
compose_cause_type(service_not_supported) ->
    ?GTPv2_CAUSE_TYPE_SERVICE_NOT_SUPPORTED;
compose_cause_type(mandatory_ie_incorrect) ->
    ?GTPv2_CAUSE_TYPE_MANDATORY_IE_INCORRECT;
compose_cause_type(mandatory_ie_missing) ->
    ?GTPv2_CAUSE_TYPE_MANDATORY_IE_MISSING;
compose_cause_type(system_failure) ->
    ?GTPv2_CAUSE_TYPE_SYSTEM_FAILURE;
compose_cause_type(no_resources_available) ->
    ?GTPv2_CAUSE_TYPE_NO_RESOURCES_AVAILABLE;
compose_cause_type(semantic_error_in_the_tft_operation) ->
    ?GTPv2_CAUSE_TYPE_SEMANTIC_ERROR_IN_THE_TFT_OPERATION;
compose_cause_type(syntactic_error_in_the_tft_operation) ->
    ?GTPv2_CAUSE_TYPE_SYNTACTIC_ERROR_IN_THE_TFT_OPERATION;
compose_cause_type(semantic_errors_in_packet_filters) ->
    ?GTPv2_CAUSE_TYPE_SEMANTIC_ERRORS_IN_PACKET_FILTERS;
compose_cause_type(syntactic_errors_in_packet_filters) ->
    ?GTPv2_CAUSE_TYPE_SYNTACTIC_ERRORS_IN_PACKET_FILTERS;
compose_cause_type(missing_or_unknown_apn) ->
    ?GTPv2_CAUSE_TYPE_MISSING_OR_UNKNOWN_APN;
compose_cause_type(gre_key_not_found) ->
    ?GTPv2_CAUSE_TYPE_GRE_KEY_NOT_FOUND;
compose_cause_type(relocation_failure) ->
    ?GTPv2_CAUSE_TYPE_RELOCATION_FAILURE;
compose_cause_type(denied_in_rat) ->
    ?GTPv2_CAUSE_TYPE_DENIED_IN_RAT;
compose_cause_type(preferred_pdn_type_not_supported) ->
    ?GTPv2_CAUSE_TYPE_PREFERRED_PDN_TYPE_NOT_SUPPORTED;
compose_cause_type(all_dynamic_addresses_are_occupied) ->
    ?GTPv2_CAUSE_TYPE_ALL_DYNAMIC_ADDRESSES_ARE_OCCUPIED;
compose_cause_type(ue_context_without_tft_already_activated) ->
    ?GTPv2_CAUSE_TYPE_UE_CONTEXT_WITHOUT_TFT_ALREADY_ACTIVATED;
compose_cause_type(protocol_type_not_supported) ->
    ?GTPv2_CAUSE_TYPE_PROTOCOL_TYPE_NOT_SUPPORTED;
compose_cause_type(ue_not_responding) ->
    ?GTPv2_CAUSE_TYPE_UE_NOT_RESPONDING;
compose_cause_type(ue_refuses) ->
    ?GTPv2_CAUSE_TYPE_UE_REFUSES;
compose_cause_type(service_denied) ->
    ?GTPv2_CAUSE_TYPE_SERVICE_DENIED;
compose_cause_type(unable_to_page_ue) ->
    ?GTPv2_CAUSE_TYPE_UNABLE_TO_PAGE_UE;
compose_cause_type(no_memory_available) ->
    ?GTPv2_CAUSE_TYPE_NO_MEMORY_AVAILABLE;
compose_cause_type(user_authentication_failed) ->
    ?GTPv2_CAUSE_TYPE_USER_AUTHENTICATION_FAILED;
compose_cause_type(apn_access_denied_no_subscription) ->
    ?GTPv2_CAUSE_TYPE_APN_ACCESS_DENIED_NO_SUBSCRIPTION;
compose_cause_type(request_rejected_reason_not_specified) ->
    ?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_REASON_NOT_SPECIFIED;
compose_cause_type(p_tmsi_signature_mismatch) ->
    ?GTPv2_CAUSE_TYPE_P_TMSI_SIGNATURE_MISMATCH;
compose_cause_type(imsiimei_not_known) ->
    ?GTPv2_CAUSE_TYPE_IMSIIMEI_NOT_KNOWN;
compose_cause_type(semantic_error_in_the_tad_operation) ->
    ?GTPv2_CAUSE_TYPE_SEMANTIC_ERROR_IN_THE_TAD_OPERATION;
compose_cause_type(syntactic_error_in_the_tad_operation) ->
    ?GTPv2_CAUSE_TYPE_SYNTACTIC_ERROR_IN_THE_TAD_OPERATION;
compose_cause_type(remote_peer_not_responding) ->
    ?GTPv2_CAUSE_TYPE_REMOTE_PEER_NOT_RESPONDING;
compose_cause_type(collision_with_network_initiated_request) ->
    ?GTPv2_CAUSE_TYPE_COLLISION_WITH_NETWORK_INITIATED_REQUEST;
compose_cause_type(unable_to_page_ue_due_to_suspension) ->
    ?GTPv2_CAUSE_TYPE_UNABLE_TO_PAGE_UE_DUE_TO_SUSPENSION;
compose_cause_type(conditional_ie_missing) ->
    ?GTPv2_CAUSE_TYPE_CONDITIONAL_IE_MISSING;
compose_cause_type(apn_restriction_type_incompatible_with_currently_active_pdn_connection) ->
    ?GTPv2_CAUSE_TYPE_APN_RESTRICTION_TYPE_INCOMPATIBLE_WITH_CURRENTLY_ACTIVE_PDN_CONNECTION;
compose_cause_type(invalid_overall_length_of_the_triggered_response_message_and_a_piggybacked_initial_message) ->
    ?GTPv2_CAUSE_TYPE_INVALID_OVERALL_LENGTH_OF_THE_TRIGGERED_RESPONSE_MESSAGE_AND_A_PIGGYBACKED_INITIAL_MESSAGE;
compose_cause_type(data_forwarding_not_supported) ->
    ?GTPv2_CAUSE_TYPE_DATA_FORWARDING_NOT_SUPPORTED;
compose_cause_type(invalid_reply_from_remote_peer) ->
    ?GTPv2_CAUSE_TYPE_INVALID_REPLY_FROM_REMOTE_PEER;
compose_cause_type(fallback_to_gtpv1) ->
    ?GTPv2_CAUSE_TYPE_FALLBACK_TO_GTPV1;
compose_cause_type(invalid_peer) ->
    ?GTPv2_CAUSE_TYPE_INVALID_PEER;
compose_cause_type(temporarily_rejected_due_to_handovertaurau_procedure_in_progress) ->
    ?GTPv2_CAUSE_TYPE_TEMPORARILY_REJECTED_DUE_TO_HANDOVERTAURAU_PROCEDURE_IN_PROGRESS;
compose_cause_type(modifications_not_limited_to_s1_u_bearers) ->
    ?GTPv2_CAUSE_TYPE_MODIFICATIONS_NOT_LIMITED_TO_S1_U_BEARERS;
compose_cause_type(request_rejected_for_a_pmipv6_reason) ->
    ?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_FOR_A_PMIPV6_REASON;
compose_cause_type(apn_congestion) ->
    ?GTPv2_CAUSE_TYPE_APN_CONGESTION;
compose_cause_type(bearer_handling_not_supported) ->
    ?GTPv2_CAUSE_TYPE_BEARER_HANDLING_NOT_SUPPORTED;
compose_cause_type(ue_already_re_attached) ->
    ?GTPv2_CAUSE_TYPE_UE_ALREADY_RE_ATTACHED;
compose_cause_type(multiple_pdn_connections_for_a_given_apn_not_allowed) ->
    ?GTPv2_CAUSE_TYPE_MULTIPLE_PDN_CONNECTIONS_FOR_A_GIVEN_APN_NOT_ALLOWED;
compose_cause_type(target_access_restricted_for_the_subscriber) ->
    ?GTPv2_CAUSE_TYPE_TARGET_ACCESS_RESTRICTED_FOR_THE_SUBSCRIBER;
compose_cause_type(mmesgsn_refuses_due_to_vplmn_policy) ->
    ?GTPv2_CAUSE_TYPE_MMESGSN_REFUSES_DUE_TO_VPLMN_POLICY;
compose_cause_type(gtp_c_entity_congestion) ->
    ?GTPv2_CAUSE_TYPE_GTP_C_ENTITY_CONGESTION;
compose_cause_type(late_overlapping_request) ->
    ?GTPv2_CAUSE_TYPE_LATE_OVERLAPPING_REQUEST;
compose_cause_type(timed_out_request) ->
    ?GTPv2_CAUSE_TYPE_TIMED_OUT_REQUEST;
compose_cause_type(ue_is_temporarily_not_reachable_due_to_power_saving) ->
    ?GTPv2_CAUSE_TYPE_UE_IS_TEMPORARILY_NOT_REACHABLE_DUE_TO_POWER_SAVING;
compose_cause_type(relocation_failure_due_to_nas_message_redirection) ->
    ?GTPv2_CAUSE_TYPE_RELOCATION_FAILURE_DUE_TO_NAS_MESSAGE_REDIRECTION;
compose_cause_type(ue_not_authorised_by_ocs_or_external_aaa_server) ->
    ?GTPv2_CAUSE_TYPE_UE_NOT_AUTHORISED_BY_OCS_OR_EXTERNAL_AAA_SERVER;
compose_cause_type(multiple_accesses_to_a_pdn_connection_not_allowed) ->
    ?GTPv2_CAUSE_TYPE_MULTIPLE_ACCESSES_TO_A_PDN_CONNECTION_NOT_ALLOWED;
compose_cause_type(request_rejected_due_to_ue_capability) ->
    ?GTPv2_CAUSE_TYPE_REQUEST_REJECTED_DUE_TO_UE_CAPABILITY;
compose_cause_type(s1_u_path_failure) ->
    ?GTPv2_CAUSE_TYPE_S1_U_PATH_FAILURE;
compose_cause_type('5gc_not_allowed') ->
    ?GTPv2_CAUSE_TYPE_5GC_NOT_ALLOWED;
compose_cause_type(pgw_mismatch_with_network_slice_subscribed_by_the_ue) ->
    ?GTPv2_CAUSE_TYPE_PGW_MISMATCH_WITH_NETWORK_SLICE_SUBSCRIBED_BY_THE_UE;
compose_cause_type(rejection_due_to_paging_restriction) ->
    ?GTPv2_CAUSE_TYPE_REJECTION_DUE_TO_PAGING_RESTRICTION.
