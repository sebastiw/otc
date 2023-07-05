-module(otc_gtpv2).
-behaviour(otc_codec).

-include_lib("kernel/include/logger.hrl").
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

decode(<<2:3, P:1, T:1, MP:1, _:2, MT:8, Len:16, GTP0:Len/binary>>) ->
    MessageType = parse_message_type(MT),
    {MsgFields, GTP1} = decode_msg_fields(P, T, MP, GTP0),
    Msg0 = decode_msg(MessageType, GTP1),
    Msg1 = maps:merge(Msg0, MsgFields),
    Msg2 = Msg1#{message_type => MessageType,
                 version => 2},
    Msg2.

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

decode_msg_fields(P, T, M, GTP0) ->
    PB = case P of
             1 -> true;
             0 -> false
         end,
    Fs0 = [{piggy_backed, PB}],
    {Fs1, Rest1} = case T of
                       1 ->
                           <<TEID:32, SeqNo:24, Rest0/binary>> = GTP0,
                           {[{teid, TEID}, {sequence_number, SeqNo}], Rest0};
                       0 ->
                           <<SeqNo:24, Rest0/binary>> = GTP0,
                           {[{sequence_number, SeqNo}], Rest0}
                   end,

    {Fs2, Rest3} = case M of
                       1 ->
                           <<MP:4, _:4, Rest2/binary>> = Rest1,
                           {[{message_priority, MP}], Rest2};
                       0 ->
                           <<_:8, Rest2/binary>> = Rest1,
                           {[{message_priority, false}], Rest2}
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
parse_iei(?GTPv2_IEI_TRAFFIC_AGGREGATE_DESCRIPTION) ->
    traffic_aggregate_description;
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
parse_iei(?GTPv2_IEI_ALLOCATION_RETENTION_PRIORITY) ->
    allocation_retention_priority;
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
parse_iei(?GTPv2_IEI_RAN_NAS_CAUSE) ->
    ran_nas_cause;
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
compose_iei(traffic_aggregate_description) ->
    ?GTPv2_IEI_TRAFFIC_AGGREGATE_DESCRIPTION;
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
compose_iei(allocation_retention_priority) ->
    ?GTPv2_IEI_ALLOCATION_RETENTION_PRIORITY;
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
compose_iei(ran_nas_cause) ->
    ?GTPv2_IEI_RAN_NAS_CAUSE;
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

decode_parameter(imsi, V, _) ->
    %% ITU-T Rec E.212 TBCD digits
    tcbd_decode(V);
decode_parameter(cause, V, _) ->
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
decode_parameter(recovery_restart_counter, V, _) ->
    <<RestartCounter:8>> = V,
    RestartCounter;
decode_parameter(apn, V, _) ->
    decode_apn(V);
decode_parameter(ambr, V, _) ->
    <<Uplink:32, Downlink:32>> = V,
    #{uplink => Uplink,
      downlink => Downlink};
decode_parameter(eps_bearer_id, V, _) ->
    <<_:4, EBI:4, _/binary>> = V,
    EBI;
decode_parameter(ip_address, V, _) ->
    bin_to_ip_addr(V);
decode_parameter(mei, V, _) ->
    %% The ME Identity field contains either the IMEI or the IMEISV as
    %% defined in clause 6.2 of 3GPP TS 23.003 [2]. It is encoded as
    %% specified in clause 7.7.53 of 3GPP TS 29.060 [4], beginning
    %% with octet 4 of Figure 7.7.53.1.
    %% The IMEI(SV) digits are encoded using BCD coding where IMEI is
    %% 15 BCD digits and IMEISV is 16 BCD digits. For IMEI, bits 5 to
    %% 8 of the last octet shall be filled with an end mark coded as
    %% '1111'.
    IMEISV = tcbd_decode(V),
    case length(IMEISV) of
        15 ->
            #{imei => IMEISV};
        16 ->
            {IMEI, SV} = lists:split(15, IMEISV),
            #{imei => IMEI, sv => SV}
    end;
decode_parameter(msisdn, V, _) ->
    tcbd_decode(V);
decode_parameter(indication, V0, _) ->
    V = <<V0/binary, 0:(80-bit_size(V0))>>,
    <<DAF:1, DTF:1, HI:1, DFI:1, OI:1, ISRSI:1, ISRAI:1, SGWCI:1,
      SQCI:1, UIMSI:1, CFSI:1, CRSI:1, P:1, PT:1, SI:1, MSV:1,
      RetLoc:1, PBIC:1, SRNI:1, S6AF:1, S4AF:1, MBMDT:1, ISRAU:1, CCRSI:1,
      CPRAI:1, AARL:1, PPOF:1, PPONPPEI:1, PPSI:1, CSFBI:1, CLII:1, CPSR:1,
      NSI:1, UASI:1, DTCI:1, BDWI:1, PSCI:1, PCRI:1, AOSI:1, AOPI:1,
      ROAAI:1, EPCOSI:1, CPOPCI:1, PMTSMI:1, S11TF:1, PNSI:1, UNACCSI:1, WPMSI:1,
      FGSNN26:1, REPREFI:1, FGSIWK:1, EEVRSI:1, LTEMUI:1, LTEMPI:1, ENBCRSI:1, TSPCMI:1,

      CSRMFI:1, MTEDTN:1, MTEDTA:1, N5GNMI:1, FGCNRS:1, FGCNRI:1, FSRHOI:1, ETHPDN:1,
      NSPUSI:1, PGWRNSI:1, RPPCSI:1, PGWCHI:1, SISSME:1, NSENBI:1, IDFUPF:1, EMCI:1,
      _Spare:5, LTEMSAI:1, SRTPI:1,  UPIPSI:1,
      _/binary>> = V,

    #{dual_address_bearer_flag => DAF,
      direct_tunnel_flag => DTF,
      handover_indication => HI,
      direct_forwarding_indication => DFI,
      operation_indication => OI,
      idle_mode_signalling_reduction_supported_indication => ISRSI,
      idle_mode_signalling_reduction_activation_indication => ISRAI,
      sgw_change_indication => SGWCI,
      subscribed_qos_change_indication => SQCI,
      unauthenticated_imsi => UIMSI,
      change_f_teid_support_indication => CFSI,
      change_reporting_support_indication => CRSI,
      piggybacking_supported => P,
      s5s8_protocol_type => PT,
      scope_indication => SI,
      ms_validated => MSV,
      retrieve_location_indication_flag => RetLoc,
      propagate_bbai_information_change => PBIC,
      sgw_restoration_needed_indication => SRNI,
      static_ipv6_address_flag => S6AF,
      static_ipv4_address_flag => S4AF,
      management_based_mdt_allowed_flag => MBMDT,
      isr_is_activated_for_the_ue => ISRAU,
      csg_change_reporting_support_indication => CCRSI,
      change_of_presence_reporting_area_information_indication => CPRAI,
      abnormal_release_of_radio_link => AARL,
      pdn_pause_off_indication => PPOF,
      pdn_pause_on_enabled_indication => PPONPPEI,
      pdn_pause_support_indication => PPSI,
      csfb_indication => CSFBI,
      change_of_location_information_indication => CLII,
      cs_to_ps_srvcc_indication => CPSR,
      nbifom_support_indication => NSI,
      ue_available_for_signaling_indication => UASI,
      delay_tolerant_connection_indication => DTCI,
      buffered_dl_data_waiting_indication => BDWI,
      pending_subscription_change_indication => PSCI,
      p_cscf_restoration_indication => PCRI,
      associate_oci_with_sgw_nodes_identity => AOSI,
      associate_oci_with_pgw_nodes_identity => AOPI,
      release_over_any_access_indication => ROAAI,
      extended_pco_support_indication => EPCOSI,
      control_plane_only_pdn_connection_indication => CPOPCI,
      pending_mt_short_message_indication => PMTSMI,
      s11_u_tunnel_flag => S11TF,
      pending_network_initiated_pdn_connection_signalling_indication => PNSI,
      ue_not_authorised_cause_code_support_indication => UNACCSI,
      wlcp_pdn_connection_modification_support_indication => WPMSI,
      '5gs_interworking_without_n26_indication' => FGSNN26,
      return_preferred_indication => REPREFI,
      '5gs_interworking_indication' => FGSIWK,
      extended_ebi_value_range_support_indication => EEVRSI,
      lte_m_ue_indication => LTEMUI,
      lte_m_rat_type_reporting_to_pgw_indication => LTEMPI,
      enb_change_reporting_support_indication => ENBCRSI,
      triggering_sgsn_initiated_pdp_context_creation_modification_indication => TSPCMI,
      create_session_request_message_forwarded_indication => CSRMFI,
      mt_edt_not_applicable => MTEDTN,
      mt_edt_applicable => MTEDTA,
      no_5gs_n26_mobility_indication => N5GNMI,
      '5gc_not_restricted_support' => FGCNRS,
      '5gc_not_restricted_indication' => FGCNRI,
      '5g_srvcc_ho_indication' => FSRHOI,
      ethernet_pdn_support_indication => ETHPDN,
      notify_start_pause_of_charging_via_user_plane_support_indication => NSPUSI,
      pgw_redirection_due_to_mismatch_with_network_slice_subscribed_by_ue_support_indication => PGWRNSI,
      restoration_of_pdn_connections_after_an_pgw_c_smf_change_support_indication => RPPCSI,
      pgw_change_indication => PGWCHI,
      same_iwk_scef_selected_for_monitoring_event_indication => SISSME,
      notify_source_enodeb_indication => NSENBI,
      indirect_data_forwarding_with_upf_indication => IDFUPF,
      emergency_pdu_session_indication => EMCI,
      lte_m_satellite_access_indication => LTEMSAI,
      satellite_rat_type_reporting_to_pgw_indication => SRTPI,
      user_plane_integrity_protection_support_indication => UPIPSI};
decode_parameter(protocol_configuration_options, V, _) ->
    %% Specified as per clause 10.5.6.3 of 3GPP TS 24.008
    <<_Ext:1, 0:4, CP:3, R0/binary>> = V,
    0 = CP,
    decode_pco(R0);
decode_parameter(pdn_address_allocation, V, _) ->
    <<PDN:1/binary, R0/binary>> = V,
    case pdn_type(PDN) of
        ipv4 ->
            <<IPv4:4/binary, _/binary>> = R0,
            #{ipv4 => bin_to_ip_addr(IPv4)};
        ipv6 ->
            <<IPv6:16/binary, _/binary>> = R0,
            #{ipv6 => bin_to_ip_addr(IPv6)};
        ipv4v6 ->
            <<IPv4:4/binary, IPv6:16/binary, _/binary>> = R0,
            #{ipv4 => bin_to_ip_addr(IPv4),
              ipv6 => bin_to_ip_addr(IPv6)};
        T ->
            T
    end;
decode_parameter(bearer_qos, V, _) ->
    <<ARPBin:1/binary,
      QoSBin:21/binary>> = V,
    ARP = decode_allocation_retention_priority(ARPBin),
    QOS = decode_qos(QoSBin),
    maps_merge_all([ARP, QOS]);
decode_parameter(flow_qos, V, _) ->
    decode_qos(V);
decode_parameter(rat_type, V, _) ->
    <<RATType:8>> = V,
    case RATType of
        ?GTPv2_RAT_TYPE_UTRAN -> utran;
        ?GTPv2_RAT_TYPE_GERAN -> geran;
        ?GTPv2_RAT_TYPE_WLAN -> wlan;
        ?GTPv2_RAT_TYPE_GAN -> gan;
        ?GTPv2_RAT_TYPE_HSPA_EVOLUTION -> hspa_evolution;
        ?GTPv2_RAT_TYPE_EUTRAN_WB_EUTRAN -> eutran_wb_eutran;
        ?GTPv2_RAT_TYPE_VIRTUAL -> virtual;
        ?GTPv2_RAT_TYPE_EUTRAN_NB_IOT -> eutran_nb_iot;
        ?GTPv2_RAT_TYPE_LTE_M -> lte_m;
        ?GTPv2_RAT_TYPE_NR -> nr;
        ?GTPv2_RAT_TYPE_WB_EUTRAN_LEO -> wb_eutran_leo;
        ?GTPv2_RAT_TYPE_WB_EUTRAN_MEO -> wb_eutran_meo;
        ?GTPv2_RAT_TYPE_WB_EUTRAN_GEO -> wb_eutran_geo;
        ?GTPv2_RAT_TYPE_WB_EUTRAN_OTHERSAT -> wb_eutran_othersat;
        ?GTPv2_RAT_TYPE_EUTRAN_NB_IOT_LEO -> eutran_nb_iot_leo;
        ?GTPv2_RAT_TYPE_EUTRAN_NB_IOT_MEO -> eutran_nb_iot_meo;
        ?GTPv2_RAT_TYPE_EUTRAN_NB_IOT_GEO -> eutran_nb_iot_geo;
        ?GTPv2_RAT_TYPE_EUTRAN_NB_IOT_OTHERSAT -> eutran_nb_iot_othersat;
        ?GTPv2_RAT_TYPE_LTE_M_LEO -> lte_m_leo;
        ?GTPv2_RAT_TYPE_LTE_M_MEO -> lte_m_meo;
        ?GTPv2_RAT_TYPE_LTE_M_GEO -> lte_m_geo;
        ?GTPv2_RAT_TYPE_LTE_M_OTHERSAT -> lte_m_othersat
    end;
decode_parameter(serving_network, V, _) ->
    decode_mcc_mnc(V);
decode_parameter(bearer_tft, V, _) ->
    %% Specified in 3GPP TS 24.008
    <<OpCode:3, Ebit:1, PacketFiltersNum:4, R0/binary>> = V,
    {PktFilters, R2} = case OpCode of
                           2#000 ->
                               %% Ignore this IE
                               0 = PacketFiltersNum,
                               {#{packet_filters => ignore}, R0};
                           2#001 ->
                               %% Create new TFT
                               {PFs, R1} = decode_tft_packet_filters(PacketFiltersNum, R0),
                               {#{packet_filters => PFs}, R1};
                           2#010 ->
                               %% Delete existing TFT
                               0 = PacketFiltersNum,
                               {#{}, R0};
                           2#011 ->
                               %% Add packet filters to existing TFT
                               {PFs, R1} = decode_tft_packet_filters(PacketFiltersNum, R0),
                               {#{packet_filters => PFs}, R1};
                           2#100 ->
                               %% Replace packet filters in existing TFT
                               {PFs, R1} = decode_tft_packet_filters(PacketFiltersNum, R0),
                               {#{packet_filters => PFs}, R1};
                           2#101 ->
                               %% Delete packet filters from existing TFT
                               <<PacketFilters:PacketFiltersNum/binary, R1/binary>> = R0,
                               PFs = [#{identifier => PFID} || <<_:4, PFID:4>> <= PacketFilters],
                               {#{packet_filters => PFs}, R1};
                           2#110 ->
                               %% No TFT operation
                               0 = PacketFiltersNum,
                               {#{}, R0}
                       end,
    {ParamList, _} = case Ebit of
                         0 ->
                             %% parameters list is not included
                             {#{}, <<>>};
                         1 ->
                             %% parameters list is included
                             Params = decode_tft_parameters_list(R2),
                             {#{parameters_list => Params}, <<>>}
                     end,
    maps_merge_all([PktFilters, ParamList]);
decode_parameter(traffic_aggregate_description, V, _) ->
    %% Specified in 3GPP TS 24.008
    V;
decode_parameter(user_location_information, V, _) ->
    <<EMIDI:1, MIDI:1, LAII:1, ECGII:1, TAII:1, RAII:1, SAII:1, CGII:1,
      CGIBin:(CGII*7)/binary,
      SAIBin:(SAII*7)/binary,
      RAIBin:(RAII*7)/binary,
      TAIBin:(TAII*5)/binary,
      ECGIBin:(ECGII*7)/binary,
      LAIBin:(LAII*5)/binary,
      MIDBin:(MIDI*6)/binary,
      EMIDBin:(EMIDI*6)/binary,
      _Rest/binary>> = V,
    Fields = [{cgi, CGII, fun decode_cgi/1, CGIBin},
              {sai, SAII, fun decode_sai/1, SAIBin},
              {rai, RAII, fun decode_rai/1, RAIBin},
              {tai, TAII, fun decode_tai/1, TAIBin},
              {ecgi, ECGII, fun decode_ecgi/1, ECGIBin},
              {lai, LAII, fun decode_lai/1, LAIBin},
              {macro_enodeb_id, MIDI, fun decode_macro_enodeb_id/1, MIDBin},
              {extended_macro_enodeb_id, EMIDI, fun decode_extended_macro_enodeb_id/1, EMIDBin}],
    lists:foldl(fun ({_Name, 0, _, _}, Acc) ->
                        Acc;
                    ({Name, 1, DFun, Bin}, Acc) ->
                        Acc#{Name => DFun(Bin)}
                end,
                #{},
                Fields);
decode_parameter(f_teid, V, _) ->
    <<V4:1, V6:1, IT:6, TEIDGRE:32,
      IPv4:(V4*4)/binary, IPv6:(V6*16)/binary>> = V,
    InterfaceType = case IT of
                        0 -> s1u_enodeb_gtpu;
                        1 -> s1u_sgw_gtpu;
                        2 -> s12_rnc_gtpu;
                        3 -> s12_sgw_gtpu;
                        4 -> s5s8_sgw_gtpu;
                        5 -> s5s8_pgw_gtpu;
                        6 -> s5s8_sgw_gtpc;
                        7 -> s5s8_pgw_gtpc;
                        8 -> s5s8_sgw_pmipv6;
                        9 -> s5s8_pgw_pmipv6;
                        10 -> s11_mme_gtpc;
                        11 -> s11s4_sgw_gtpc;
                        12 -> s10n26_mme_gtpc;
                        13 -> s3_mme_gtpc;
                        14 -> s3_sgsn_gtpc;
                        15 -> s4_sgsn_gtpu;
                        16 -> s4_sgw_gtpu;
                        17 -> s4_sgsn_gtpc;
                        18 -> s16_sgsn_gtpc;
                        19 -> enodeb_gnodeb_gtpu_dl_data_forwarding;
                        20 -> enodeb_gtpu_ul_data_forwarding;
                        21 -> rnc_gtpu_data_forwarding;
                        22 -> sgsn_gtpu_data_forwarding;
                        23 -> sgw_upf_gtpu_dl_data_forwarding;
                        24 -> sm_mbms_gw_gtpc;
                        25 -> sn_mbms_gw_gtpc;
                        26 -> sm_mme_gtpc;
                        27 -> sn_sgsn_gtpc;
                        28 -> sgw_gtpu_ul_data_forwarding;
                        29 -> sn_sgsn_gtpu;
                        30 -> s2b_epdg_gtpc;
                        31 -> s2bu_epdg_gtpu;
                        32 -> s2b_pgw_gtpc;
                        33 -> s2bu_pgw_gtpu;
                        34 -> s2a_twan_gtpu;
                        35 -> s2a_twan_gtpc;
                        36 -> s2a_pgw_gtpc;
                        37 -> s2a_pgw_gtpu;
                        38 -> s11_mme_gtpu;
                        39 -> s11_sgw_gtpu;
                        40 -> n26_amf_gtpc;
                        41 -> n19mb_upf_gtpu
                    end,
    case {V4, V6} of
        {1, 0} ->
            #{interface_type => InterfaceType,
              teid_gre_key => TEIDGRE,
              ipv4 => bin_to_ip_addr(IPv4)};
        {0, 1} ->
            #{interface_type => InterfaceType,
              teid_gre_key => TEIDGRE,
              ipv6 => bin_to_ip_addr(IPv6)};
        {1, 1} ->
            #{interface_type => InterfaceType,
              teid_gre_key => TEIDGRE,
              ipv4 => bin_to_ip_addr(IPv4),
              ipv6 => bin_to_ip_addr(IPv6)}
    end;
decode_parameter(tmsi, V, _) ->
    %% Defined in 3GPP TS 23.003
    V;
decode_parameter(global_cn_id, V, _) ->
    <<MCCMNCBin:3/binary, CNID:4/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{cn_id => CNID};
decode_parameter(s103_pdn_data_forwarding_info, V, _) ->
    <<Len:8, HSGWAddr:Len/binary, GREKey:4/binary,
      EPSNum:8, EPSBearers/binary>> = V,
    EPSBearerIDs = [ID || <<_:4, ID:4>> <= EPSBearers],
    EPSNum = length(EPSBearerIDs),
    #{hsgw_address => HSGWAddr,
      gre_key => GREKey,
      eps_bearer_ids => EPSBearerIDs};
decode_parameter(s1_u_data_forwarding_info, V, _) ->
    <<_:4, EPSBearerId:4, Len:8, SGWAddr:Len/binary, Teid:4/binary>> = V,
    #{eps_bearer_id => EPSBearerId,
      sgw_address => SGWAddr,
      sgw_s1u_teid => Teid};
decode_parameter(delay_value, V, _) ->
    <<Multiples:8/binary>> = V,
    50*Multiples;
decode_parameter(bearer_context, V, Opts) ->
    %% Bearer Context Grouped Type
    {BearerContext, _} = decode_tliv_list(V, Opts),
    BearerContext;
decode_parameter(charging_id, V, _) ->
    %% Defined in 3GPP TS 32.251
    <<ChargingId:4/binary>> = V,
    ChargingId;
decode_parameter(charging_characteristics, V, _) ->
    %% Defined in 3GPP TS 32.251
    <<ChargingCharacteristics:2/binary>> = V,
    ChargingCharacteristics;
decode_parameter(trace_information, V, _) ->
    <<MCCMNCBin:3/binary,
      TraceId:8,
      TriggeringEvents:8,
      NETypes:2/binary,
      SessionTraceDepth:1/binary,
      ListOfInterfaces:2/binary,
      IPAddr:4/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{%% Defined in clause 5.6 of 3GPP TS 32.422
            trace_id => TraceId,
            %% Encoded as the first 9 octets in clause 5.1 of 3GPP TS 32.422
            triggering_events => TriggeringEvents,
            %% Specified in 3GPP TS 32.422
            ne_types => NETypes,
            %% Specified in 3GPP TS 32.422
            session_trace_depth => SessionTraceDepth,
            %% Encoded as the first 12 octets in clause 5.5 of 3GPP TS 32.422
            list_of_interfaces => ListOfInterfaces,
            %% Specified in 3GPP TS 32.422
            ip_address => bin_to_ip_addr(IPAddr)};
decode_parameter(bearer_flags, V, _) ->
    <<_:4, ASI:1, Vind:1, VB:1, PPC:1>> = V,
    #{prohibit_payload_compression => PPC,
      voice_bearer => VB,
      vsrvcc_indicator => Vind,
      activity_status_indicator => ASI};
decode_parameter(pdn_type, V, _) ->
    pdn_type(V);
decode_parameter(procedure_transaction_id, V, _) ->
    <<PTI:8>> = V,
    PTI;
decode_parameter(mm_context_gsm_key_and_triplets, V, _) ->
    <<SecurityMode:3, _:1, DRXI:1, CKSN:3,
      NumTriplets:3, _:3, UAMBRI:1, SAMBRI:1,
      _:5, UsedCipher:3,
      Kc:8/binary,
      R0/binary>> = V,

    {Triplets, R1} = triplets(NumTriplets, R0),

    {CommonMM, _R11} = common_mm_context(DRXI, 0, SAMBRI, UAMBRI, 0, R1),

    Base = #{security_mode => SecurityMode,
             cksn => CKSN,
             used_cipher => UsedCipher,
             kc => Kc,
             triplets => Triplets},
    maps_merge_all([Base, CommonMM]);
decode_parameter(mm_context_umts_key_cipher_and_quintuplets, V, _) ->
    <<SecurityMode:3, _:1, DRXI:1, CKSN:3,
      NumQuintuplets:3, IOVI:1, GUPII:1, UGIPAI:1, UAMBRI:1, SAMBRI:1,
      _:2, UsedGPRSIntegrityProtectionAlgorithm:3, UsedCipher:3,
      CK:16/binary,
      IK:16/binary,
      R0/binary>> = V,

    {Quintuplets, R1} = quintuplets(NumQuintuplets, R0),

    {CommonMM, R11} = common_mm_context(DRXI, 0, SAMBRI, UAMBRI, 0, R1),

    {HigherBitratesThan16MbpsFlag, R12} = otc_l3_codec:decode_lv(R11),

    IOVFun = fun (R) ->
                     <<IOVUpdatesCounter:8, Rp0/binary>> = R,
                     {#{iov_updates_counter => IOVUpdatesCounter}, Rp0}
             end,
    {IOV, _R11} = maybe_decode(IOVI, IOVFun, R12, #{}),

    Base = #{security_mode => SecurityMode,
             cksn => CKSN,
             gupii => GUPII,
             ugipai => UGIPAI,
             used_gprs_integrity_protection_algorithm => UsedGPRSIntegrityProtectionAlgorithm,
             used_cipher => UsedCipher,
             ck => CK,
             ik => IK,
             quintuplets => Quintuplets,
             higher_bitrates_than_16_mbps_flag => HigherBitratesThan16MbpsFlag},
    maps_merge_all([Base, CommonMM, IOV]);
decode_parameter(mm_context_gsm_key_cipher_and_quintuplets, V, _) ->
    <<SecurityMode:3, _:1, DRXI:1, CKSN:3,
      NumQuintuplets:3, _:3, UAMBRI:1, SAMBRI:1,
      _:5, UsedCipher:3,
      Kc:8/binary,
      R0/binary>> = V,

    {Quintuplets, R1} = quintuplets(NumQuintuplets, R0),

    {CommonMM, R11} = common_mm_context(DRXI, 0, SAMBRI, UAMBRI, 0, R1),

    {HigherBitratesThan16MbpsFlag, _R12} = otc_l3_codec:decode_lv(R11),

    Base = #{security_mode => SecurityMode,
             cksn => CKSN,
             used_cipher => UsedCipher,
             kc => Kc,
             quintuplets => Quintuplets,
             higher_bitrates_than_16_mbps_flag => HigherBitratesThan16MbpsFlag},
    maps_merge_all([Base, CommonMM]);
decode_parameter(mm_context_umts_key_and_quintuplets, V, _) ->
    <<SecurityMode:3, _:1, DRXI:1, KSI:3,
      NumQuintuplets:3, IOVI:1, GUPII:1, UGIPAI:1, UAMBRI:1, SAMBRI:1,
      _:5, UsedGPRSIntegrityProtectionAlgorithm:3,
      CK:16/binary,
      IK:16/binary,
      R0/binary>> = V,

    {Quintuplets, R1} = quintuplets(NumQuintuplets, R0),

    {CommonMM, R11} = common_mm_context(DRXI, 0, SAMBRI, UAMBRI, 0, R1),

    {HigherBitratesThan16MbpsFlag, R12} = otc_l3_codec:decode_lv(R11),

    IOVFun = fun (R) ->
                     <<IOVUpdatesCounter:8, Rp0/binary>> = R,
                     {#{iov_updates_counter => IOVUpdatesCounter}, Rp0}
             end,
    {IOV, R13} = maybe_decode(IOVI, IOVFun, R12, #{}),

    {ExtendedAccessRestrictionData, _R14} = otc_l3_codec:decode_lv(R13),

    Base = #{security_mode => SecurityMode,
             ksi => KSI,
             gupii => GUPII,
             ugipai => UGIPAI,
             used_gprs_integrity_protection_algorithm => UsedGPRSIntegrityProtectionAlgorithm,
             ck => CK,
             ik => IK,
             quintuplets => Quintuplets,
             higher_bitrates_than_16_mbps_flag => HigherBitratesThan16MbpsFlag,
             extended_access_restriction_data => ExtendedAccessRestrictionData},
    maps_merge_all([Base, CommonMM, IOV]);
decode_parameter(mm_context_eps_security_context_quadruplets_and_quintuplets, V, _) ->
    <<SecurityMode:3, NHI:1, DRXI:1, KSI:3,
      NumQuintuplets:3, NumQuadruplets:3, UAMBRI:1, OSCI:1,
      SAMBRI:1, UsedNASIntegrityProtectionAlgorithm:3, UsedNASCipher:4,
      NASDownlinkCount:24,
      NASUplinkCount:24,
      KASME:32/binary, R0/binary>> = V,

    {Quadruplets, R1} = quadruplets(NumQuadruplets, R0),
    {Quintuplets, R2} = quintuplets(NumQuintuplets, R1),

    {CommonMM, R18} = common_mm_context(DRXI, NHI, SAMBRI, UAMBRI, OSCI, R2),

    {UERadioCapabilityForPagingInformation, R19} = maybe_decode(byte_size(R18), fun otc_l3_codec:decode_lve/1, R18, <<>>),
    {ExtendedAccessRestrictionData, R20} = maybe_decode(byte_size(R19), fun otc_l3_codec:decode_lv/1, R19, <<>>),

    ExtARDFun = fun (R) ->
                        <<_:3, NRUNA:1, NRUSRNA:1, NRNA:1, USSRNA:1, NRSRNA:1, _/binary>> = R,
                        #{nr_as_secondary_rat => is_allowed(NRSRNA),
                          unlicensed_spectrum_lwa_lwip_as_secondary_rat => is_allowed(USSRNA),
                          nr_in_5gs => is_allowed(NRNA),
                          new_radio_unlicensed_as_secondary_rat => is_allowed(NRUSRNA),
                          nr_u_in_5gs => is_allowed(NRUNA)}
                end,
    ExtARD = maybe_decode(ExtARDFun, ExtendedAccessRestrictionData, #{}),

    {UEAdditionalSecurityCapability, R21} = maybe_decode(byte_size(R20), fun otc_l3_codec:decode_lv/1, R20, <<>>),
    {UENRSecurityCapability, R22} = maybe_decode(byte_size(R21), fun otc_l3_codec:decode_lv/1, R21, <<>>),
    {APNRateControlStatusesBin, R23} = maybe_decode(byte_size(R22), fun otc_l3_codec:decode_lve/1, R22, <<>>),
    APNRateControlStatuses = apn_rate_control_statuses(APNRateControlStatusesBin),

    CNFun = fun (R) ->
                    {CoreNetworkRestrictions, Rp0} = otc_l3_codec:decode_lv(R),
                    {UERadioCapabilityID, Rp1} = otc_l3_codec:decode_lv(Rp0),
                    <<_:6, ENSCT:2>> = Rp1,
                    #{core_network_restrictions => CoreNetworkRestrictions,
                      ue_radio_capability_id => UERadioCapabilityID,
                      eps_nas_security_context_type => case ENSCT of
                                                           2#00 -> not_supported;
                                                           2#01 -> native;
                                                           2#10 -> mapped
                                                       end
                     }
            end,
    CN = maybe_decode(CNFun, R23, #{}),

    Base = #{security_mode => SecurityMode,
             ksi => KSI,
             used_nas_integrity_protection_algorithm => UsedNASIntegrityProtectionAlgorithm,
             used_nas_cipher => UsedNASCipher,
             nas_downlink_count => NASDownlinkCount,
             nas_uplink_count => NASUplinkCount,
             kasme => KASME,
             quadruplets => Quadruplets,
             quintuplets => Quintuplets,
             ue_radio_capability_for_paging_information => UERadioCapabilityForPagingInformation,
             ue_additional_security_capability => UEAdditionalSecurityCapability,
             ue_nr_security_capability => UENRSecurityCapability,
             apn_rate_control_statuses => APNRateControlStatuses,
             extended_access_restriction_data => ExtARD},
    maps_merge_all([Base, CommonMM, CN]);
decode_parameter(mm_context_umts_key_quadruplets_and_quintuplets, V, _) ->
    <<SecurityMode:3, _:1, DRXI:1, KSI:3,
      NumQuintuplets:3, NumQuadruplets:3, UAMBRI:1, SAMBRI:1,
      _:8, CK:16/binary, IK:16/binary,
      R0/binary>> = V,

    {Quadruplets, R1} = quadruplets(NumQuadruplets, R0),
    {Quintuplets, R2} = quintuplets(NumQuintuplets, R1),

    {CommonMM, R11} = common_mm_context(DRXI, 0, SAMBRI, UAMBRI, 0, R2),

    {APNRateControlStatusesBin, _R12} = otc_l3_codec:decode_lve(R11),
    APNRateControlStatuses = apn_rate_control_statuses(APNRateControlStatusesBin),

    Base = #{security_mode => SecurityMode,
             ksi => KSI,
             ck => CK,
             ik => IK,
             quadruplets => Quadruplets,
             quintuplets => Quintuplets,
             apn_rate_control_statuses => APNRateControlStatuses},
    maps_merge_all([Base, CommonMM]);
decode_parameter(pdn_connection, V, Opts) ->
    %% PDN Connection Grouped Type
    {PDNs, _} = decode_tliv_list(V, Opts),
    PDNs;
decode_parameter(pdn_numbers, V, _) ->
    <<0:4, NSAPI:4,
      DLGTPUSequenceNumber:16,
      ULGTPUSequenceNumber:16,
      SendNPDUNumber:16,
      ReceiveNPDUNumber:16/binary>> = V,
    #{nsapi => NSAPI,
      dl_gtpu_sequence_number => DLGTPUSequenceNumber,
      ul_gtpu_sequence_number => ULGTPUSequenceNumber,
      send_npdu_number => SendNPDUNumber,
      receive_npdu_number => ReceiveNPDUNumber};
decode_parameter(p_tmsi, V, _) ->
    %% Defined in 3GPP TS 23.003
    V;
decode_parameter(p_tmsi_signature, V, _) ->
    %% Defined in 3GPP TS 24.008
    V;
decode_parameter(hop_counter, V, _) ->
    <<HopCounter:8/binary>> = V,
    HopCounter;
decode_parameter(ue_time_zone, V, _) ->
    <<TZa:4, S:1, TZb:3,
      _:6, DaylightSavingTime:2>> = V,
    DST = case DaylightSavingTime of
              0 -> no_adjustment;
              1 -> plus_one_hour;
              2 -> plus_two_hours
          end,
    %% 3GPP TS 23.040
    %%
    %% The Time Zone indicates the difference, expressed in quarters
    %% of an hour, between the local time and GMT. In the first of the
    %% two semi‑octets, the first bit (bit 3 of the seventh octet of
    %% the TP‑Service‑Centre‑Time‑Stamp field) represents the
    %% algebraic sign of this difference (0: positive, 1: negative).
    %%
    %% ..Semi-octet as the representation given in 3GPP TS 24.008 [12]
    %% under "called BCD number"..
    %%
    %% E.g.
    %%
    %% 3GPP-MS-TimeZone: 8001
    %% Timezone: GMT + 2 hours 0 minutes
    %% .... ..01 = Adjustment: +1 hour adjustment for Daylight Saving Time (1)
    %% 01 is DST (+1 hour), as defined in 29.274
    %% 0x80 = 1000 0000
    %% as it is BCD, swap "semi-octets"
    %% 0000 1000
    %% 1st bit is the + - indicator (0 = +, 1 = -)
    %% so after BCD decode,  it is dec 8 here, 8 / 4 (quarters) =  2 hours
    %%
    %% 3GPP-MS-TimeZone: 2300
    %% Timezone: GMT + 8 hours 0 minutes
    %% .... ..00 = Adjustment: No adjustment (0)
    %% 0x23 = 0010 0011
    %% 0011 0010 = BCD 32
    %% 32 / 4 = 8 hours
    Tz = case S of
             1 -> -1 * (TZb * 10 + TZa);
             0 ->       TZb * 10 + TZa
         end,
    #{time_zone => Tz*15,
      daylight_saving_time => DST};
decode_parameter(trace_reference, V, _) ->
    %% TraceId defined in 3GPP TS 32.422
    <<MCCMNCBin:3/binary, TraceId:3/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{trace_id => TraceId};
decode_parameter(complete_request_message, V, _) ->
    <<CRMType:8, CompleteRequestMessage/binary>> = V,
    case CRMType of
        0 ->
            #{complete_attach_request_message => CompleteRequestMessage};
        1 ->
            #{complete_tau_request_message => CompleteRequestMessage}
    end;
decode_parameter(guti, V, _) ->
    %% Specified in 3GPP TS 23.003
    <<MCCMNCBin:3/binary, MMEGroupID:16, MMECode:8, M_TMSI/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{mme_group_id => MMEGroupID,
            mme_code => MMECode,
            m_tmsi => M_TMSI};
decode_parameter(f_container, V, _) ->
    <<_:4, ContainerType:4, Container/binary>> = V,
    case ContainerType of
        1 -> #{utran_transparent_container => Container};
        2 -> #{bss_container => Container};
        3 -> #{eutran_transparent_container => Container};
        4 -> #{nbifom_container => Container};
        5 -> #{endc_container => Container};
        6 -> #{inter_system_son_container => Container}
    end;
decode_parameter(f_cause, V, _) ->
    <<_:4, CauseType:4, CauseValue/binary>> = V,
    case CauseType of
        0 -> #{radio_network_layer_cause => CauseValue};
        1 -> #{transport_layer_cause => CauseValue};
        2 -> #{nas_cause => CauseValue};
        3 -> #{protocol_cause => CauseValue};
        4 -> #{miscellaneous_cause => CauseValue}
    end;
decode_parameter(plmn_id, V, _) ->
    %% Different MCC/MNC encoding than rest of the protocol
    <<MCC2:4/big, MCC1:4/big,
      MNC1:4/big, MCC3:4/big,
      MNC3:4/big, MNC2:4/big>> = V,
    MCC = [MCC1+$0, MCC2+$0, MCC3+$0],
    MNC = case MNC1 of
              2#1111 ->
                  [MNC2+$0, MNC3+$0];
              _ ->
                  [MNC1+$0, MNC2+$0, MNC3+$0]
          end,
    #{mcc => MCC, mnc => MNC};
decode_parameter(target_identification, V, _) ->
    <<TargetType:8, TargetID/binary>> = V,
    case TargetType of
        0 ->
            %% RNC-ID
            <<RAIBin:7/binary, RNCID:2, ERNCID/binary>> = TargetID,
            RAI = decode_rai(RAIBin),
            RAI#{rnc_id => RNCID,
                 extended_rnc_id => ERNCID};
        1 ->
            %% Macro eNB-ID
            <<MENBBin:6/binary, TAC:2/binary>> = TargetID,
            MENB = decode_macro_enodeb_id(MENBBin),
            MENB#{tracking_area_code => TAC};
        2 ->
            %% Cell Identifier
            #{cell_id => TargetID};
        3 ->
            %% Home eNB-ID
            <<MCCMNCBin:3/binary, _:4, HID:20, TAC:2/binary>> = TargetID,
            MCCMNC = decode_mcc_mnc(MCCMNCBin),
            MCCMNC#{home_enodeb_id => HID,
                    tracking_area_code => TAC};
        4 ->
            %% Extended Macro eNB-ID
            <<EENBBin:6/binary, TAC:2/binary, _:8>> = TargetID,
            EENB = decode_extended_macro_enodeb_id(EENBBin),
            #{extended_macro_enodeb_id => EENB,
              tracking_area_code => TAC};
        5 ->
            %% gNB-ID
            <<MCCMNCBin:3/binary, _:2, GnodeBLen:6, GnodeBBin:4/binary, TAC:3/binary>> = TargetID,
            <<_:(32-GnodeBLen), GnodeB:GnodeBLen>> = GnodeBBin,
            MCCMNC = decode_mcc_mnc(MCCMNCBin),
            MCCMNC#{gnodeb_id => GnodeB,
                    tracking_area_code => TAC};
        6 ->
            %% Macro ng-eNB-ID
            <<MCCMNCBin:3/binary, _:4, MacroNg:20, TAC:3/binary>> = TargetID,
            MCCMNC = decode_mcc_mnc(MCCMNCBin),
            MCCMNC#{macro_ng_enodeb_id => MacroNg,
                    '5gs_tracking_area_code' => TAC};
        7 ->
            %% Extended ng-eNB-ID
            <<MCCMNCBin:3/binary, EID:3/bytes, TAC:3/binary>> = TargetID,
            MCCMNC = decode_mcc_mnc(MCCMNCBin),
            EMID = case EID of
                       <<0:1, _:2, ID:21/bits>> ->
                           #{long_macro_enodeb_id => ID};
                       <<1:1, _:2, _:3, ID:18/bits>> ->
                           #{short_macro_enodeb_id => ID}
                   end,
            MCCMNC#{extended_ng_enodeb_id => EMID,
                    '5gs_tracking_area_code' => TAC};
        8 ->
            %% en-gNB-ID
            <<MCCMNCBin:3/binary, TAC5I:1, TACI:1, ENGNBLen:6, ID:4/binary,
              T0:(TAC5I*3)/binary,
              T1:(TACI*3)/binary>> = TargetID,
            <<_:(32-ENGNBLen), ENGNBID:ENGNBLen>> = ID,
            MCCMNC = decode_mcc_mnc(MCCMNCBin),
            TAC5 = case TAC5I of
                       0 ->
                           undefined;
                       1 ->
                           T0
                   end,
            TAC = case TACI of
                      0 ->
                          undefined;
                      1 ->
                          T1
                  end,
            MCCMNC#{engnb_id => ENGNBID,
                    tac5gs => TAC5,
                    tac => TAC}
    end;
decode_parameter(packet_flow_id, V, _) ->
    <<_:4, EBI:4, PacketFlowId/binary>> = V,
    #{ebi => EBI, packet_flow_id => PacketFlowId};
decode_parameter(rab_context, V, _) ->
    <<ULPSI:1, DLPSI:1, ULGSI:1, DLGSI:1, NSAPI:4,
      DLGTPUSeq:16/big, ULGTPUSeq:16/big,
      DLPDCPSeq:16/big, ULPDCPSeq:16/big>> = V,
    #{nsapi => NSAPI,
      dl_gtpu_sequence_number => case DLGSI of 0 -> 0; 1 -> DLGTPUSeq end,
      ul_gtpu_sequence_number => case ULGSI of 0 -> 0; 1 -> ULGTPUSeq end,
      dl_pdcp_sequence_number => case DLPSI of 0 -> 0; 1 -> DLPDCPSeq end,
      ul_pdcp_sequence_number => case ULPSI of 0 -> 0; 1 -> ULPDCPSeq end};
decode_parameter(source_rnc_pdcp_context_info, V, _) ->
    V;
decode_parameter(port_number, V, _) ->
    <<Port:16/big>> = V,
    Port;
decode_parameter(apn_restriction, V, _) ->
    <<RestrictionType:8>> = V,
    RestrictionType;
decode_parameter(selection_mode, V, _) ->
    <<_:6, SelectionMode:2>> = V,
    case SelectionMode of
        0 ->
            #{provided_by => ms_or_network,
              verified => true};
        1 ->
            #{provided_by => ms,
              verified => false};
        2 ->
            #{provided_by => network,
              verified => false};
        3 ->
            #{provided_by => network,
              verified => false}
    end;
decode_parameter(source_identification, V, _) ->
    %% Target Cell ID defined in 3GPP TS 48.018
    <<TargetCellID:8/binary, SourceType:8, SourceID/binary>> = V,
    ST = case SourceType of
             0 ->
                 cell_id;
             1 ->
                 rnc_id
         end,
    #{target_cell_id => TargetCellID,
      source_id => #{ST => SourceID}};
decode_parameter(change_reporting_action, V, _) ->
    S = 8*byte_size(V),
    <<Action:S/big>> = V,
    case Action of
        0 ->
            stop_reporting;
        1 ->
            {start_reporting, [cgi, sai]};
        2 ->
            {start_reporting, [rai]};
        3 ->
            {start_reporting, [tai]};
        4 ->
            {start_reporting, [ecgi]};
        5 ->
            {start_reporting, [cgi, sai, rai]};
        6 ->
            {start_reporting, [tai, ecgi]};
        7 ->
            {start_reporting, [macro_enodeb_id, extended_macro_enodeb_id]};
        8 ->
            {start_reporting, [tai, macro_enodeb_id, extended_macro_enodeb_id]}
    end;
decode_parameter(fq_csid, V, _) ->
    <<NodeIDType:4, NumberOfCSIDs:4,
      R0/binary>> = V,
    {NodeId, R2} = case NodeIDType of
                       0 ->
                           <<NID:4/binary, R1/binary>> = R0,
                           {#{ipv4 => bin_to_ip_addr(NID)}, R1};
                       1 ->
                           <<NID:16/binary, R1/binary>> = R0,
                           {#{ipv6 => bin_to_ip_addr(NID)}, R1};
                       2 ->
                           <<MCCMNC:20, NID:12, R1/binary>> = R0,
                           {MCC, MNC} = lists:split(3, integer_to_list(MCCMNC)),
                           {#{mcc => list_to_integer(MCC),
                              mnc => list_to_integer(MNC),
                              node_id => NID}, R1}
                   end,
    CSIDs = [CSID || <<CSID:2/binary>> <= R2],
    #{node_id => NodeId,
      csids => NumberOfCSIDs =:= length(CSIDs) andalso CSIDs};
decode_parameter(channel_needed, V, _) ->
    V;
decode_parameter(emlpp_priority, V, _) ->
    V;
decode_parameter(node_type, V, _) ->
    <<NodeType:8>> = V,
    case NodeType of
        0 ->
            mme;
        1 ->
            sgsn
    end;
decode_parameter(fqdn, V, _) ->
    decode_apn(V);
decode_parameter(private_extension, V, _) ->
    <<EnterpriseID:16, ProprietaryValue/binary>> = V,
    #{enterprise_id => EnterpriseID,
      proprietary_value => ProprietaryValue};
decode_parameter(transaction_identifier, V, _) ->
    %% Specified in 3GPP TS 24.007
    V;
decode_parameter(mbms_session_duration, V, _) ->
    <<Duration:3/binary>> = V,
    Duration;
decode_parameter(mbms_service_area, V, _) ->
    %% Defined in 3GPP TS 23.246
    V;
decode_parameter(mbms_session_identifier, V, _) ->
    %% Defined in 3GPP TS 29.061
    <<MBMSSessionIdentifier:1/binary>> = V,
    MBMSSessionIdentifier;
decode_parameter(mbms_flow_identifier, V, _) ->
    %% Defined in 3GPP TS 23.246
    <<MBMSFlowIdentifier:2/binary>> = V,
    MBMSFlowIdentifier;
decode_parameter(mbms_ip_multicast_distribution, V, _) ->
    <<CTEID:32,
      DAType:2, DALen:6, IPMD:DALen/binary,
      SAType:2, SALen:6, IPMS:SALen/binary,
      MBMSHCIndicator:1/binary>> = V,
    IPMulticastDistributionAddress = case {DAType, DALen} of
                                         {0, 4} ->
                                             %% ipv4
                                             #{ipv4 => bin_to_ip_addr(IPMD)};
                                         {1, 16} ->
                                             %% ipv6
                                             #{ipv6 => bin_to_ip_addr(IPMD)}
                                     end,
    IPMulticastSourceAddress = case {SAType, SALen} of
                                   {0, 4} ->
                                       %% ipv4
                                       #{ipv4 => bin_to_ip_addr(IPMS)};
                                   {1, 16} ->
                                       %% ipv6
                                       #{ipv6 => bin_to_ip_addr(IPMS)}
                               end,
    #{cteid => CTEID,
      ip_multicast_distribution_address => IPMulticastDistributionAddress,
      ip_multicast_source_address => IPMulticastSourceAddress,
      mbms_hc_indicator => MBMSHCIndicator};
decode_parameter(mbms_distribution_acknowledge, V, _) ->
    <<_:6, MBMSDistributionAcknowledge:2>> = V,
    case MBMSDistributionAcknowledge of
        0 ->
            no_rnc_accepted;
        1 ->
            all_rnc_accepted;
        2 ->
            some_rnc_accepted
    end;
decode_parameter(user_csg_information, V, _) ->
    <<MCCMNCBin:3/binary,
      CSGIDBin:4/binary,
      AccessMode:2, _:4, LCSG:1, CMI:1>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    CSGID = decode_csg_id(CSGIDBin),
    Base = maps_merge_all([MCCMNC, CSGID]),
    Base#{access_mode => case AccessMode of 0 -> closed; 1 -> hybrid end,
          leave_csg_flag => case LCSG of 0 -> false; 1 -> true end,
          csg_membership_indication => case CMI of 0 -> false; 1 -> true end};
decode_parameter(csg_information_reporting_action, V, _) ->
    <<_:5, UCIUHC:1, UCISHC:1, UCICSG:1>> = V,
    case UCIUHC + UCISHC + UCICSG of
        0 ->
            stop_reporting;
        _ ->
            #{unsubscribed_hybrid_cell => case UCIUHC of 0 -> false; 1 -> true end,
              subscribed_hybrid_cell => case UCISHC of 0 -> false; 1 -> true end,
              csg_cell => case UCICSG of 0 -> false; 1 -> true end}
    end;
decode_parameter(rfsp_index, V, _) ->
    %% Specified in 3GPP TS 36.413
    <<RFSPIndex:16>> = V,
    RFSPIndex;
decode_parameter(csg_id, V, _) ->
    <<CSGIDBin:4/binary>> = V,
    decode_csg_id(CSGIDBin);
decode_parameter(csg_membership_indication, V, _) ->
    <<_:7, CMI:1>> = V,
    case CMI of 0 -> true; 1 -> false end;
decode_parameter(service_indicator, V, _) ->
    <<SI:8>> = V,
    case SI of 1 -> cs_call; 2 -> sms end;
decode_parameter(detach_type, V, _) ->
    <<DT:8>> = V,
    case DT of 1 -> ps; 2 -> combined_ps_cs end;
decode_parameter(local_distinguished_name, V, _) ->
    V;
decode_parameter(node_features, V, _) ->
    <<_:1, MTEDT:1, ETH:1, S1UN:1, CIOT:1, NTSR:1, MABR:1, PRN:1>> = V,
    SupportedFeatures = [{pgw_restart_notification, PRN},
                         {modify_access_bearers_request, MABR},
                         {network_triggered_service_restoration_procedure, NTSR},
                         {cellular_internet_of_things, CIOT},
                         {s1u_path_failure_notification_feature, S1UN},
                         {ethernet_pdn_type, ETH},
                         {support_of_mt_edt, MTEDT}],
    [N || {N, 1} <- SupportedFeatures];
decode_parameter(mbms_time_to_data_transfer, V, _) ->
    <<MBMSTimeToDataTransfer:8>> = V,
    MBMSTimeToDataTransfer;
decode_parameter(throttling, V, _) ->
    <<DelayUnit:3, DelayValue:5, Factor:8>> = V,
    DUMs = case DelayUnit of
               2#000 ->
                   %% 2 seconds
                   timer:seconds(2);
               2#001 ->
                   %% 1 minute
                   timer:minutes(1);
               2#010 ->
                   %% 10 minutes
                   timer:minutes(10);
               2#011 ->
                   %% 1 hour
                   timer:hours(1);
               2#100 ->
                   %% 10 hours
                   timer:hours(10);
               2#111 ->
                   %% deactivated
                   0;
               _ ->
                   %% 1 minute
                   timer:minutes(1)
           end,
    F = case 0 < Factor andalso Factor =< 100 of
            true ->
                Factor;
            false ->
                0
        end,
    #{value => DelayValue,
      unit_ms => DUMs,
      factor => F};
decode_parameter(allocation_retention_priority, V, _) ->
    decode_allocation_retention_priority(V);
decode_parameter(epc_timer, V, _) ->
    <<TimerUnit:3, TimerValue:5>> = V,
    TUMs = case TimerUnit of
               2#000 ->
                   %% 2 seconds
                   timer:seconds(2);
               2#001 ->
                   %% 1 minute
                   timer:minutes(1);
               2#010 ->
                   %% 10 minutes
                   timer:minutes(10);
               2#011 ->
                   %% 1 hour
                   timer:hours(1);
               2#100 ->
                   %% 10 hours
                   timer:hours(10);
               2#111 ->
                   %% infinite
                   0;
               _ ->
                   %% 1 minute
                   timer:minutes(1)
           end,
    #{unit_ms => TUMs,
      value => TimerValue};
decode_parameter(signalling_priority_indication, V, _) ->
    <<_:7, LAPI:1>> = V,
    #{low_access_priority => case LAPI of 0 -> false; 1 -> true end};
decode_parameter(temporary_mobile_group_identity, V, _) ->
    %% 3GPP TS 29.061 and 3GPP TS 24.008
    <<TMGI:5/binary>> = V,
    TMGI;
decode_parameter(additional_mm_context_for_srvcc, V, _) ->
    <<MSC2Len:8, MSC2:MSC2Len/binary,
      MSC3Len:8, MSC3:MSC3Len/binary,
      SCLLen:8, SCL:SCLLen/binary>> = V,
    #{mobile_station_classmark_2 => MSC2,
      mobile_station_classmark_3 => MSC3,
      supported_codec_list => SCL};
decode_parameter(additional_flags_for_srvcc, V, _) ->
    <<_:6, VF:1, ICS:1>> = V,
    #{ims_centralized_service => case ICS of 0 -> false; 1 -> true end,
      vsrvcc_flag => case VF of 0 -> false; 1 -> true end};
decode_parameter(mdt_configuration, V, _) ->
    <<JobType:8,
      ListOfMeasurements:4/binary,
      ReportingTrigger:8,
      ReportInterval:8,
      ReportAmount:8,
      EventThresholdRSRP:8,
      EventThresholdRSRQ:8,
      AreaScopeLen:8, AreaScope:AreaScopeLen/binary,
      _:4, PLI:1, PMI:1, MPI:1, CRRMI:1,
      CollectionPeriod:(CRRMI*8),
      MeasurementPeriod:(MPI*8),
      PositioningMethod:(PMI*8),
      NumberOfMDTPLMNs:(PLI*8), MDTPLMNList:(NumberOfMDTPLMNs*3)/binary,
      _/binary>> = V,
    CollP = case CRRMI of
                0 ->
                    #{};
                1 ->
                    #{collection_period_for_rrm_measurements_lte => CollectionPeriod}
            end,
    MeasP = case MPI of
                0 ->
                    #{};
                1 ->
                    #{measurement_period_lte => MeasurementPeriod}
            end,
    PosM = case PMI of
               0 ->
                   #{};
               1 ->
                   #{positioning_method => PositioningMethod}
           end,
    MDTPLMNs = case PLI of
                   0 ->
                       #{};
                   1 ->
                       #{mdt_plmn_list => [M || <<M:3/binary>> <= MDTPLMNList]}
               end,
    Base = maps_merge_all([CollP, MeasP, PosM, MDTPLMNs]),
    Base#{job_type => JobType,
          list_of_measurements => ListOfMeasurements,
          reporting_trigger => ReportingTrigger,
          report_interval => ReportInterval,
          report_amount => ReportAmount,
          event_threshold_rsrp => EventThresholdRSRP,
          event_threshold_rsrq => EventThresholdRSRQ,
          area_scope => AreaScope};
decode_parameter(additional_protocol_configuration_options, V, _) ->
    %% Specified in 3GPP TS 29.275
    V;
decode_parameter(absolute_time_of_mbms_data_transfer, V, _) ->
    %% Octets are coded as the time in seconds relative to
    %% 00:00:00 on 1 January 1900 (calculated as continuous time
    %% without leap seconds and traceable to a common time reference)
    %% where binary encoding of the integer part is in the 32 most
    %% significant bits and binary encoding of the fraction part in
    %% the 32 least significant bits. The fraction part is expressed
    %% with a granularity of 1 /2**32 second.
    <<Seconds:32, Fraction:32>> = V,
    #{date_time => datetime_from_epoch(Seconds),
      fraction_raw => Fraction,
      fraction_ns => fraction_to_ns(Fraction)};
decode_parameter(henb_information_reporting, V, _) ->
    <<_:7, FTI:1>> = V,
    case FTI of
        0 -> stop_reporting;
        1 -> start_reporting
    end;
decode_parameter(ipv4_configuration_parameters, V, _) ->
    <<SubnetPrefixLen:8, IP1:8, IP2:8, IP3:8, IP4:8>> = V,
    #{subnet_prefix_length => SubnetPrefixLen,
      ipv4_address => {IP1, IP2, IP3, IP4}};
decode_parameter(change_to_report_flags, V, _) ->
    <<_:6, TZCR:1, SNCR:1>> = V,
    #{time_zone => case TZCR of 0 -> false; 1 -> true end,
      serving_network => case SNCR of 0 -> false; 1 -> true end};
decode_parameter(action_indication, V, _) ->
    <<_:5, AI:3>> = V,
    case AI of
        0 -> no_action;
        1 -> deactivation;
        2 -> paging;
        3 -> paging_stop
    end;
decode_parameter(twan_identifier, V, _) ->
    %% 3GPP TS 23.402
    <<_:3, LAII:1, OPNAI:1, PLMNI:1, CIVAI:1, BSSIDI:1,
      SSIDLen:8, SSID:SSIDLen/binary,
      BSS:(BSSIDI*6)/binary,
      CivicAddressLen:(CIVAI*8), CivA:CivicAddressLen/binary,
      PLMN:(PLMNI*3)/binary,
      TWANOpNameLen:(OPNAI*8), OpName:TWANOpNameLen/binary,
      R11/binary>> = V,
    BSSID = case BSSIDI of
                0 ->
                    #{};
                1 ->
                    #{bss_id => hex_string(BSS)}
            end,
    CIVAddr = case CIVAI of
                  0 ->
                      #{};
                  1 ->
                      %% IETF RFC 4776 section 3.1 excluding
                      %% first 3 octets
                      #{civic_address => hex_string(CivA)}
              end,
    TWANPLMN = case PLMNI of
                   0 ->
                       #{};
                   1 ->
                       #{twan_plmn_id => decode_mcc_mnc(PLMN)}
               end,
    TWANOpName = case OPNAI of
                     0 ->
                         #{};
                     1 ->
                         #{twan_operator_name => OpName}
                 end,
    LAId = case LAII of
               0 ->
                   #{};
               1 ->
                   <<RelayIdType:8,
                     RelayIdLen:8, RI:RelayIdLen/binary,
                     CircuitIdLen:8, CId:CircuitIdLen/binary, _R14/binary>> = R11,
                   case {RelayIdType, RelayIdLen} of
                       {0, 4} ->
                           %% IPv4
                           #{ipv4 => bin_to_ip_addr(RI), circuit_id => CId};
                       {0, 16} ->
                           %% IPv6
                           #{ipv6 => bin_to_ip_addr(RI), circuit_id => CId};
                       {1, _} ->
                           %% FQDN
                           #{fqdn => RI, circuit_id => CId}
                   end
           end,
    Base = maps_merge_all([BSSID, CIVAddr, TWANPLMN, TWANOpName, LAId]),
    Base#{ssid => hex_string(SSID)};
decode_parameter(uli_timestamp, V, _) ->
    %% Defined in IETF RFC 5905
    <<Seconds:32>> = V,
    datetime_from_epoch(Seconds);
decode_parameter(mbms_flags, V, _) ->
    <<_:6, LMRI:1, MSRI:1>> = V,
    #{mbms_session_re_establishment => case MSRI of 0 -> false; 1 -> true end,
      local_mbms_bearer_context_release => case LMRI of 0 -> false; 1 -> true end};
decode_parameter(ran_nas_cause, V, _) ->
    <<ProtoType:4, CauseType:4, R0/binary>> = V,
    case ProtoType of
        0 ->
            %% S1AP
            %% Defined in clause 9.2.1.3 in 3GPP TS 36.413
            <<Val:1/binary>> = R0,
            #{protocol_type => s1ap,
              cause_type => case CauseType of
                                0 -> radio_network_layer;
                                1 -> transport_layer;
                                2 -> nas;
                                3 -> protocol;
                                4 -> miscellaneous
                            end,
              cause_value => Val};
        1 ->
            %% EMM
            <<Val:1/binary>> = R0,
            #{protocol_type => emm,
              cause_value => Val};
        2 ->
            %% ESM
            <<Val:1/binary>> = R0,
            #{protocol_type => esm,
              cause_value => Val};
        3 ->
            %% Diameter
            <<Val:2/binary>> = R0,
            #{protocol_type => diameter,
              cause_value => Val};
        4 ->
            %% IKEv2
            <<Val:2/binary>> = R0,
            #{protocol_type => ikev2,
              cause_value => Val}
    end;
decode_parameter(cn_operator_selection_entity, V, _) ->
    <<_:6, SelectionEntity:2>> = V,
    case SelectionEntity of
        0 -> ue;
        1 -> network;
        _ -> network
    end;
decode_parameter(trusted_wlan_mode_indication, V, _) ->
    <<_:6, MCM:1, SCM:1>> = V,
    #{multi_connection_mode => case MCM of 0 -> false; 1 -> true end,
      single_connection_mode => case SCM of 0 -> false; 1 -> true end};
decode_parameter(node_number, V, _) ->
    %% ISDN-number of SGSN (3GPP TS 23.003), MME (3GPP TS 29.002), or
    %% MSC (3GPP TS 29.002)
    <<NodeNumLen:8, NodeNum:NodeNumLen/binary>> = V,
    NodeNum;
decode_parameter(node_identifier, V, _) ->
    <<NodeNameLen:8, NodeName:NodeNameLen/binary,
      NodeRealmLen:8, NodeRealm:NodeRealmLen/binary>> = V,
    #{node_name => NodeName,
      node_realm => NodeRealm};
decode_parameter(presence_reporting_area_action, V, _) ->
    <<_:4, INAPRA:1, A:3, R0/binary>> = V,
    {Action, R2} = case A of
                       1 ->
                           <<PresenceReportingAreaIdentifier:3/binary, R1/binary>> = R0,
                           {#{presence_reporting_area_identifier => PresenceReportingAreaIdentifier,
                              action => start}, R1};
                       2 ->
                           <<PresenceReportingAreaIdentifier:3/binary, R1/binary>> = R0,
                           {#{presence_reporting_area_identifier => PresenceReportingAreaIdentifier,
                              action => stop}, R1};
                       3 ->
                           <<PresenceReportingAreaIdentifier:3/binary, R1/binary>> = R0,
                           {#{presence_reporting_area_identifier => PresenceReportingAreaIdentifier,
                              action => modify}, R1};
                       _ ->
                           {#{}, R0}
                   end,
    <<TAINum:4, RAINum:4,
      _:3, MacroENBNum:5,
      _:3, HomeENBNum:5,
      _:3, ECGINum:5,
      _:3, SAINum:5,
      _:3, CGINum:5,
      TAIs:(5*TAINum)/binary,
      RAIs:(7*RAINum)/binary,
      MacroENBs:(6*MacroENBNum)/binary,
      HomeENBs:(6*HomeENBNum)/binary,
      ECGIs:(7*ECGINum)/binary,
      SAIs:(7*SAINum)/binary,
      CGIs:(7*CGINum)/binary,
      _:3, ExtendedMacroENBNum:5,
      ExtendedMacroENBs:(6*ExtendedMacroENBNum)/binary>> = R2,
    Action#{flag => case INAPRA of 0 -> active; 1 -> inactive end,
            tais => [decode_tai(T) || <<T:5>> <= TAIs],
            rais => [decode_rai(R) || <<R:7>> <= RAIs],
            macro_enbs => [decode_macro_enodeb_id(M) || <<M:6>> <= MacroENBs],
            home_enbs => [decode_macro_enodeb_id(H) || <<H:6>> <= HomeENBs],
            ecgis => [decode_ecgi(E) || <<E:7>> <= ECGIs],
            sais => [decode_sai(S) || <<S:7>> <= SAIs],
            cgis => [decode_cgi(C) || <<C:7>> <= CGIs],
            extended_macro_enbs => [decode_extended_macro_enodeb_id(Ex) || <<Ex:6>> <= ExtendedMacroENBs]};
decode_parameter(presence_reporting_area_information, V, _) ->
    <<PRAI:3/binary,
      _:4, INAPRA:1, APRA:1, OPRA:1, IPRA:1,
      R0/binary>> = V,
    Base = #{presence_reporting_area_identifier => PRAI},
    PRA = case {INAPRA, OPRA, IPRA} of
              {0, 0, 0} ->
                  Base;
              {0, 0, 1} ->
                  Base#{flag => inside};
              {0, 1, 0} ->
                  Base#{flag => outside};
              {1, 0, 0} ->
                  Base#{flag => inactive}
          end,
    APRAs = decode_additional_pras(APRA, R0),
    PRA#{additional_presence_reporting_areas => APRAs};
decode_parameter(twan_identifier_timestamp, V, _) ->
    <<Seconds:32>> = V,
    datetime_from_epoch(Seconds);
decode_parameter(overload_control_information, V, Opts) ->
    %% Overload Control Information Grouped Type
    {OverloadControlInfo, _} = decode_tliv_list(V, Opts),
    OverloadControlInfo;
decode_parameter(load_control_information, V, Opts) ->
    %% Load Control Information Grouped Type
    {LoadControlInfo, _} = decode_tliv_list(V, Opts),
    LoadControlInfo;
decode_parameter(metric, V, _) ->
    <<Metric:8>> = V,
    case Metric >= 0 andalso Metric =< 100 of
        true  ->
            Metric;
        false ->
            0
    end;
decode_parameter(sequence_number, V, _) ->
    <<SeqNum:32>> = V,
    SeqNum;
decode_parameter(apn_and_relative_capacity, V, _) ->
    <<RC:8,
      APNLen:8, APN:APNLen/binary>> = V,
    RelativeCapacity = case RC >= 1 andalso RC =< 100 of
                           true ->
                               RC;
                           false ->
                               0
                       end,
    #{relative_capacity => RelativeCapacity,
      apn => APN};
decode_parameter(wlan_offloadability_indication, V, _) ->
    <<_:6, EI:1, UI:1>> = V,
    #{eutran_offloadability => case EI of 0 -> false; 1 -> true end,
      utran_offloadability => case UI of 0 -> false; 1 -> true end};
decode_parameter(paging_and_service_information, V, _) ->
    <<0:4, EBI:4, _:7, PPI:1, R0/binary>> = V,
    Base = #{eps_bearer_id => EBI},
    case PPI of
        0 ->
            Base;
        1 ->
            <<_:2, PPIValue:6>> = R0,
            Base#{paging_policy_indication => PPIValue}
    end;
decode_parameter(integer_number, V, _) ->
    V;
decode_parameter(millisecond_time_stamp, V, _) ->
    <<Milliseconds:48>> = V,
    Milliseconds;
decode_parameter(monitoring_event_information, V, _) ->
    <<SCEFRefID:4/binary,
      SCEFIDLen:8, SCEFID:SCEFIDLen/binary,
      RemainingReports:2/binary>> = V,
    #{scef_reference_id => SCEFRefID,
      scef_id => SCEFID,
      remaining_number_of_reports => RemainingReports};
decode_parameter(ecgi_list, V, _) ->
    <<_ECGINum:2/binary,
      R0/binary>> = V,
    [decode_ecgi(E) || <<E:7>> <= R0];
decode_parameter(remote_ue_context, V, Opts) ->
    %% Remote UE Context Grouped Type
    {RemoteUEContext, _} = decode_tliv_list(V, Opts),
    RemoteUEContext;
decode_parameter(remote_user_id, V, _) ->
    <<0:6, IMEIF:1, MSISDNF:1,
      IMSILen:8, IMSI:IMSILen/binary,
      MSISDNLen:(MSISDNF*8), M:MSISDNLen/binary,
      IMEILen:(IMEIF*8), I:IMEILen/binary, _/binary>> = V,
    MSISDN = case MSISDNF of
                 0 ->
                     #{};
                 1 ->
                     #{msisdn => M}
             end,
    IMEI = case IMEIF of
               0 ->
                   #{};
               1 ->
                   #{imei => I}
           end,
    maps_merge_all([MSISDN, IMEI, #{imsi => IMSI}]);
decode_parameter(remote_ue_ip_information, V, _) ->
    V;
decode_parameter(ciot_optimizations_support_indication, V, _) ->
    <<_:4, IHCSI:1, AWOPDN:1, SCNIPDN:1, SGNIPDN:1>> = V,
    #{ip_header_compression_support => case IHCSI of 0 -> false; 1 -> true end,
      attach_without_pdn_support => case AWOPDN of 0 -> false; 1 -> true end,
      scef_non_ip_pdn_support => case SCNIPDN of 0 -> false; 1 -> true end,
      sgi_non_ip_pdn_support => case SGNIPDN of 0 -> false; 1 -> true end};
decode_parameter(scef_pdn_connection, V, Opts) ->
    %% PDN Connection Grouped Type
    {PDNConnection, _} = decode_tliv_list(V, Opts),
    PDNConnection;
decode_parameter(header_compression_configuration, V, _) ->
    <<I2:1, I3:1, I4:1, I6:1, I102:1, I103:1, I104:1, _:9, MAXCID:16>> = V,
    Identifiers = [{I2, 16#0002}, {I3, 16#0003}, {I4, 16#0004}, {I6, 16#0006},
                   {I102, 16#0102}, {I103, 16#0103}, {I104, 16#0104}],
    #{max_cid_value => MAXCID,
      profile_identifiers => [P || {1, P} <- Identifiers]};
decode_parameter(extended_protocol_configuration_options, V, _) ->
    V;
decode_parameter(serving_plmn_rate_control, V, _) ->
    %% 5 to 6 Uplink Rate Limit
    %% 7 to 8 Downlink Rate Limit
    <<UplinkRateLimit:16, DownlinkRateLimit:16>> = V,
    #{uplink_rate_limit => UplinkRateLimit,
      downlink_rate_limit => DownlinkRateLimit};
decode_parameter(counter, V, _) ->
    %% IETF RFC 5905
    <<Timestamp:4/binary, Counter:32>> = V,
    #{timestamp => Timestamp,
      counter => Counter};
decode_parameter(mapped_ue_usage_type, V, _) ->
    <<MappedUEUsageType:16>> = V,
    MappedUEUsageType;
decode_parameter(secondary_rat_usage_data_report, V, _) ->
    <<_:5, SRUDN:1, IRSGW:1, IRPGW:1,
      SecRATType:8,
      0:4, EBI:4,
      TimeUsage:24/binary,
      R0/binary>> = V,
    SecondaryRATType = case SecRATType of
                           0 -> nr;
                           1 -> unlicensed_spectrum
                       end,
    IntendedReceiverSGW = case IRSGW of 0 -> dont_store; 1 -> store end,
    IntendedReceiverPGW = case IRPGW of 0 -> dont_forward; 1 -> forward end,
    Base = #{intended_receiver_sgw => IntendedReceiverSGW,
             intended_receiver_pgw => IntendedReceiverPGW,
             secondary_rat_type => SecondaryRATType,
             eps_bearer_id => EBI},
    case {SRUDN, IRPGW} of
        {1, 1} ->
            <<0:(24*8)>> = TimeUsage,
            <<LengthOfSRDURT:8, SRDURT:LengthOfSRDURT/binary>> = R0,
            Base#{secondary_rat_data_usage_report_transfer => SRDURT};
        {0, _} ->
            <<StartTimestamp:32,
              EndTimestamp:32,
              UsageDataDL:64,
              UsageDataUL:64>> = TimeUsage,
            <<>> = R0,
            Base#{start_timestamp => datetime_from_epoch(StartTimestamp),
                  end_timestamp => datetime_from_epoch(EndTimestamp),
                  usage_data_dl => UsageDataDL,
                  usage_data_ul => UsageDataUL}
    end;
decode_parameter(up_function_selection_indication_flags, V, _) ->
    <<_:7, DCNR:1>> = V,
    case DCNR of
        0 ->
            undefined;
        1 ->
            dual_connectivity
    end;
decode_parameter(maximum_packet_loss_rate, V, _) ->
    <<_:6, DL:1, UL:1,
      ULMaxPacketLossRate:(UL*16),
      DLMaxPacketLossRate:(DL*16)>> = V,
    case {DL, UL} of
        {0, 0} ->
            #{};
        {0, 1} ->
            #{maximum_packet_loss_rate_ul => ULMaxPacketLossRate};
        {1, 0} ->
            #{maximum_packet_loss_rate_dl => DLMaxPacketLossRate};
        {1, 1} ->
            #{maximum_packet_loss_rate_ul => ULMaxPacketLossRate,
              maximum_packet_loss_rate_dl => DLMaxPacketLossRate}
    end;
decode_parameter(apn_rate_control_status, V, _) ->
    <<UplinkPacketsAllowed:32,
      AdditionalExceptionReports:32,
      DownlinkPacketsAllowed:32,
      ValidityTimeS:32,
      ValidityTimeF:32>> = V,
    #{uplink_packets_allowed => UplinkPacketsAllowed,
      additional_exception_reports => AdditionalExceptionReports,
      downlink_packets_allowed => DownlinkPacketsAllowed,
      validity_time => datetime_from_epoch(ValidityTimeS),
      validity_time_fractions_raw => ValidityTimeF,
      validity_time_fractions_ns => fraction_to_ns(ValidityTimeF)};
decode_parameter(extended_trace_information, V, _) ->
    <<MCCMNCBin:3/binary,
      TraceId:3/binary,
      LengthOfTriggeringEvents:8, TriggeringEvents:LengthOfTriggeringEvents/binary,
      LengthOfListNEType:8, ListNEType:LengthOfListNEType/binary,
      SessionTraceDepth:8,
      LengthOfListInterfaces:8, ListInterfaces:LengthOfListInterfaces/binary,
      LengthOfIPsOfTCE:8, IPsOfTraceCollectionEntity:LengthOfIPsOfTCE/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{trace_id => TraceId,
            triggering_events => TriggeringEvents,
            list_ne_type => ListNEType,
            session_trace_depth => SessionTraceDepth,
            list_interfaces => ListInterfaces,
            ip_address_of_trace_collection_entity => IPsOfTraceCollectionEntity};
decode_parameter(monitoring_event_extension_information, V, _) ->
%% 5 Spare LRTP
%% 6 to 9 SCEF Reference ID
%% 10 SCEF ID Length
%% 11 to k SCEF ID
%% (h) to (h+3) Remaining Minimum Periodic Location Reporting Time
    <<_:7, LRTP:1,
      SCEFRefID:4/binary,
      SCEFIDLen:8, SCEFID:SCEFIDLen/binary,
      RemainingTime:(LRTP*4)/binary,
      _R1/binary>> = V,
    Base = #{scef_reference_id => SCEFRefID,
             scef_id => SCEFID},
    case LRTP of
        0 ->
            Base;
        1 ->
            Base#{remaining_minimum_periodic_location_reporting_time => RemainingTime}
    end;
decode_parameter(additional_rrm_policy_index, V, _) ->
    <<AdditionalRRMPolicyIndex:32>> = V,
    AdditionalRRMPolicyIndex;
decode_parameter(v2x_context, V, Opts) ->
    %% V2X Context Grouped Type
    {V2XContext, _} = decode_tliv_list(V, Opts),
    V2XContext;
decode_parameter(pc5_qos_parameters, V, Opts) ->
    %% PC5 QoS Parameters Grouped Type
    {PC5QoSParams, _} = decode_tliv_list(V, Opts),
    PC5QoSParams;
decode_parameter(services_authorized, V, _) ->
    <<VA:8, PA:8>> = V,
    #{vehicle => case VA of 0 -> authorized; 1 -> unauthorized end,
      pedestrian => case PA of 0 -> authorized; 1 -> unauthorized end};
decode_parameter(bit_rate, V, _) ->
    <<BitRate:32>> = V,
    BitRate;
decode_parameter(pc5_qos_flow, V, _) ->
    <<_:7, R:1,
      PQI:8,
      GFBR:32,
      MFBR:32,
      R0/binary>> = V,
    Base = #{pqi => PQI,
             guaranteed_flow_bit_rate => GFBR,
             maximum_flow_bit_rate => MFBR},
    case R of
        0 ->
            Base;
        1 ->
            <<Range:1/binary>> = R0,
            Base#{range => Range}
    end;
decode_parameter(sgi_ptp_tunnel_address, V, _) ->
    <<_:5, P:1, V6:1, V4:1, R0/binary>> = V,
    case {V4, V6, P} of
        {0, 0, 0} ->
            #{};
        {0, 0, 1} ->
            <<Port:16>> = R0,
            #{port => Port};
        {0, 1, 0} ->
            <<IPv6:16/binary>> = R0,
            #{ipv6 => bin_to_ip_addr(IPv6)};
        {0, 1, 1} ->
            <<IPv6:16/binary, Port:16>> = R0,
            #{ipv6 => bin_to_ip_addr(IPv6),
              port => Port};
        {1, 0, 0} ->
            <<IPv4:4/binary>> = R0,
            #{ipv4 => bin_to_ip_addr(IPv4)};
        {1, 0, 1} ->
            <<IPv4:4/binary, Port:16>> = R0,
            #{ipv4 => bin_to_ip_addr(IPv4),
              port => Port}
    end;
decode_parameter(pgw_change_info, V, Opts) ->
    %% PGW Change Info Grouped Type
    {PGWChangeInfo, _} = decode_tliv_list(V, Opts),
    PGWChangeInfo;
decode_parameter(pgw_fqdn, V, _) ->
    <<_:8, FQDN/binary>> = V,
    FQDN;
decode_parameter(group_id, V, _) ->
    %% Specified in 3GPP TS 29.244
    V;
decode_parameter(pscell_id, V, _) ->
    <<MCCMNCBin:3/binary,
      _:4, NRCGI:36>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{nr_cell_identity => NRCGI};
decode_parameter(up_security_policy, V, _) ->
    <<_:6, UPIPPolicy:2>> = V,
    case UPIPPolicy of
        0 ->
            #{user_plane_integrity => not_needed};
        1 ->
            #{user_plane_integrity => preferred};
        2 ->
            #{user_plane_integrity => required}
    end;
decode_parameter(alternative_imsi, V, _) ->
    %% ITU-T Rec E.212 TBCD digits
    tcbd_decode(V);
decode_parameter(_, V, _) ->
    V.

%% The encoding the APN field follows 3GPP TS 23.003 [2] clause
%% 9.1. The content of the APN field shall be the full APN with
%% both the APN Network Identifier and APN Operator Identifier
%% being present as specified in 3GPP TS 23.003 [2] clauses 9.1.1
%% and 9.1.2, 3GPP TS 23.060 [35] Annex A and 3GPP TS 23.401 [3]
%% clauses 4.3.8.1.
decode_apn(Bin) ->
    decode_apn(Bin, []).

decode_apn(<<>>, Acc) ->
    lists:flatten(lists:join(".", lists:reverse(Acc)));
decode_apn(<<A1L, A1:A1L/binary, A2/binary>>, Acc) ->
    decode_apn(A2, [binary_to_list(A1) | Acc]).

decode_mcc_mnc(V) ->
    <<MCC2:4/big, MCC1:4/big,
      MNC3:4/big, MCC3:4/big,
      MNC2:4/big, MNC1:4/big>> = V,
    MCC = [MCC1+$0, MCC2+$0, MCC3+$0],
    MNC = case MNC3 of
              2#1111 ->
                  [MNC1+$0, MNC2+$0];
              _ ->
                  [MNC1+$0, MNC2+$0, MNC3+$0]
          end,
    #{mcc => MCC, mnc => MNC}.

bin_to_ip_addr(IP) ->
    L = case byte_size(IP) of
            4  -> 8;
            16 -> 16
        end,
    IPParts = [A || <<A:L>> <= IP],
    list_to_tuple(IPParts).

bin_to_mac_addr(IP) ->
    MACParts = [binary_to_list(A) || <<A:1/binary>> <= IP],
    lists:join(":", MACParts).

pdn_type(<<_:5, 1:3>>) -> ipv4;
pdn_type(<<_:5, 2:3>>) -> ipv6;
pdn_type(<<_:5, 3:3>>) -> ipv4v6;
pdn_type(<<_:5, 4:3>>) -> non_ip;
pdn_type(<<_:5, 5:3>>) -> ethernet.

triplets(NumTriplets, R0) ->
    triplets(NumTriplets, R0, []).

triplets(0, R0, Acc) ->
    {lists:reverse(Acc), R0};
triplets(NumTriplets, R0, Acc) ->
    <<RAND:16/binary, SRES:4/binary, Kc0:8/binary, R1/binary>> = R0,
    T = #{rand => RAND, sres => SRES, kc => Kc0},
    triplets(NumTriplets - 1, R1, [T | Acc]).

quadruplets(NumQuadruplets, R0) ->
    quadruplets(NumQuadruplets, R0, []).

quadruplets(0, R0, Acc) ->
    {lists:reverse(Acc), R0};
quadruplets(NumQuadruplets, R0, Acc) ->
    <<RAND:16/binary,
      XRESLen:8, XRES:XRESLen/binary,
      AUTNLen:8, AUTN:AUTNLen/binary,
      Kasme:32/binary, R3/binary>> = R0,
    Q = #{rand => RAND, xres => XRES, autn => AUTN, kasme => Kasme},
    quadruplets(NumQuadruplets - 1, R3, [Q | Acc]).

quintuplets(NumQuintuplets, R0) ->
    quintuplets(NumQuintuplets, R0, []).

quintuplets(0, R0, Acc) ->
    {lists:reverse(Acc), R0};
quintuplets(NumQuintuplets, R0, Acc) ->
    <<RAND:16/binary,
      XRESLen:8, XRES:XRESLen/binary,
      CK:16/binary, IK:16/binary,
      AUTNLen:8, AUTN:AUTNLen/binary, R3/binary>> = R0,
    Q = #{rand => RAND, xres => XRES, ck => CK, ik => IK, autn => AUTN},
    quintuplets(NumQuintuplets - 1, R3, [Q | Acc]).

common_mm_context(DRXI, NHI, SAMBRI, UAMBRI, OSCI, R2) ->
    DRXFun = fun (R) ->
                     <<DRXParam:16, Rp0/binary>> = R,
                     {#{drx_parameter => DRXParam}, Rp0}
             end,
    {DRX, R3} = maybe_decode(DRXI, DRXFun, R2, #{}),

    NHFun = fun (R) ->
                    <<NHBin:32/binary,
                      _:5, NCC:3, Rp0/binary>> = R,
                    {#{next_hop => NHBin, next_hop_chaining_count => NCC}, Rp0}
            end,
    {NH, R4} = maybe_decode(NHI, NHFun, R3, #{}),

    SAMBRFun = fun (R) ->
                       <<UplinkSubscribedUEAMBR:32,
                         DownlinkSubscribedUEAMBR:32,
                         Rp0/binary>> = R,
                       {#{uplink_subscribed_ue_ambr => UplinkSubscribedUEAMBR,
                          downlink_subscribed_ue_ambr => DownlinkSubscribedUEAMBR}, Rp0}
               end,
    {SAMBR, R5} = maybe_decode(SAMBRI, SAMBRFun, R4, #{}),

    UAMBRFun = fun (R) ->
                       <<UplinkUsedUEAMBR:32,
                         DownlinkUsedUEAMBR:32,
                         Rp0/binary>> = R,
                       {#{uplink_used_ue_ambr => UplinkUsedUEAMBR,
                          downlink_used_ue_ambr => DownlinkUsedUEAMBR}, Rp0}
               end,
    {UAMBR, R6} = maybe_decode(UAMBRI, UAMBRFun, R5, #{}),

    {UENetworkCapability, R7} = otc_l3_codec:decode_lv(R6),
    {MSNetworkCapability, R8} = otc_l3_codec:decode_lv(R7),
    {MEI, R9} = otc_l3_codec:decode_lv(R8),

    <<ECNA:1, NBNA:1, HNNA:1, ENA:1, INA:1, GANA:1, GENA:1, UNA:1,
      R10/binary>> = R9,
    ARD = #{utran => is_allowed(UNA),
            geran => is_allowed(GENA),
            gan => is_allowed(GANA),
            i_hspa_evolution => is_allowed(INA),
            wb_e_utran => is_allowed(ENA),
            nb_iot => is_allowed(NBNA),
            enhanced_coverage => is_allowed(ECNA),
            ho_to_non_3gpp_access => is_allowed(HNNA)},

    OSCFun = fun (R) ->
                     <<NHI_old:1, RLOS:1, KSI_old:3, NCC_old:3,
                       KASME_old:32/binary, Rp0/binary>> = R,
                     BaseOld = #{rlos => RLOS,
                                 ksi_old => KSI_old,
                                 ncc_old => NCC_old,
                                 kasme_old => KASME_old},
                     NHOldFun = fun (P0) ->
                                        <<NHOldBin:32/binary, P1/binary>> = P0,
                                        {#{next_hop_old => NHOldBin}, P1}
                                end,
                     {NHOld, Rp1} = maybe_decode(NHI_old, NHOldFun, Rp0, #{}),
                     OSCM = maps_merge_all([BaseOld, NHOld]),
                     {#{old_security_container => OSCM}, Rp1}
             end,
    {OSC, R17} = maybe_decode(OSCI, OSCFun, R10, #{}),

    {VoiceDomainPreferenceAndUEsUsageSetting, R18} = maybe_decode(byte_size(R17), fun otc_l3_codec:decode_lv/1, R17, <<>>),

    B = #{ue_network_capability => UENetworkCapability,
          ms_network_capability => MSNetworkCapability,
          mei => decode_parameter(mei, MEI, []),
          access_restriction_data => ARD,
          voice_domain_preference_and_ues_usage_setting => VoiceDomainPreferenceAndUEsUsageSetting},

    {maps_merge_all([B, DRX, NH, SAMBR, UAMBR, OSC]), R18}.

is_allowed(0) -> allowed;
is_allowed(1) -> not_allowed.

maybe_decode(_, <<>>, Default) ->
    Default;
maybe_decode(DecodeFun, Bin, _) ->
    DecodeFun(Bin).

maybe_decode(Indicator, DecodeFun, Bin, Default) ->
    case Indicator of
        0 ->
            {Default, Bin};
        _ ->
            DecodeFun(Bin)
    end.

apn_rate_control_statuses(APNRateControlStatusesBin) ->
    apn_rate_control_statuses(APNRateControlStatusesBin, []).

apn_rate_control_statuses(<<>>, Acc) ->
    lists:reverse(Acc);
apn_rate_control_statuses(APNRateControlStatusesBin, Acc) ->
    <<_FullLen:16, APNLen:16,
      APN:APNLen/binary,
      APNRateControlStatus:20/binary, R1/binary>> = APNRateControlStatusesBin,
    APNRCS = decode_parameter(apn_rate_control_status, APNRateControlStatus, []),
    S = APNRCS#{apn => decode_apn(APN)},
    apn_rate_control_statuses(R1, [S | Acc]).

decode_cgi(V) ->
    <<MCCMNCBin:3/binary, LAC:16, CI:16>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{location_area_code => LAC,
            cell_identity => CI}.

decode_sai(V) ->
    <<MCCMNCBin:3/binary, LAC:16, SAC:2/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{location_area_code => LAC,
            service_area_code => SAC}.

decode_rai(V) ->
    <<MCCMNCBin:3/binary, LAC:16, RAC:16>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{location_area_code => LAC,
            routing_area_code => RAC}.

decode_tai(V) ->
    <<MCCMNCBin:3/binary, TAC:16>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{tracking_area_code => TAC}.

decode_ecgi(V) ->
    <<MCCMNCBin:3/binary, _:4, ECI:28>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{eutran_cell_identifier => ECI}.

decode_lai(V) ->
    <<MCCMNCBin:3/binary, LAC:16>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{location_area_code => LAC}.

decode_macro_enodeb_id(V) ->
    <<MCCMNCBin:3/binary, _:4, ID:20>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    MCCMNC#{macro_enodeb_id => ID}.

decode_extended_macro_enodeb_id(V) ->
    <<MCCMNCBin:3/binary, EID:3/binary>> = V,
    MCCMNC = decode_mcc_mnc(MCCMNCBin),
    case EID of
        <<0:1, _:2, ID:21/bitstring>> ->
            MCCMNC#{long_macro_enodeb_id => ID};
        <<1:1, _:2, _:3, ID:18/bitstring>> ->
            MCCMNC#{short_macro_enodeb_id => ID}
    end.

decode_csg_id(CSGIDBin) ->
    <<_:5, CSGID:27>> = CSGIDBin,
    CSGID.

datetime_from_epoch(Seconds) ->
    Epoch = calendar:datetime_to_gregorian_seconds({{1900, 1, 1}, {0, 0, 0}}),
    calendar:gregorian_seconds_to_datetime(Epoch + Seconds).

fraction_to_ns(Fraction) ->
    erlang:floor(Fraction / math:pow(2, 32)*erlang:convert_time_unit(1, second, nanosecond)).

decode_additional_pras(0, _) ->
    [];
decode_additional_pras(1, V) ->
    <<PRAI:3/binary,
      _:5, APRA:1, OPRA:1, IPRA:1,
      R0/binary>> = V,
    Base = #{presence_reporting_area_identifier => PRAI},
    PRA = case {OPRA, IPRA} of
              {0, 0} ->
                  Base;
              {0, 1} ->
                  Base#{flag => inside};
              {1, 0} ->
                  Base#{flag => outside}
          end,
    [PRA | decode_additional_pras(APRA, R0)].

decode_pco(<<>>) ->
    [];
decode_pco(V) ->
    <<ID:16, R0/binary>> = V,
    LenLen =
        case ID of
            16#0023 ->
                %% QoS rules with the length of two octets
                16;
            16#0024 ->
                %% QoS flow descriptions with the length of two octets);
                16;
            16#0030 ->
                %% ATSSS response with the length of two octets);
                16;
            16#0031 ->
                %% DNS server security information with length of two octets);
                16;
            16#0032 ->
                %% ECS address with the length of two octets);or
                16;
            16#0041 ->
                %% Service-level-AA container with the length of two octets);
                16;
            _ ->
                8
        end,
    <<ContentLen:LenLen, Content:ContentLen/binary, R2/binary>> = R0,
    PCO = case ContentLen of
              0 ->
                  #{id => ID};
              _ ->
                  #{id => ID, content => Content}
          end,
    [PCO | decode_pco(R2)].

decode_allocation_retention_priority(V) ->
    <<_:1, PCI:1, PL:4, _:1, PVI:1>> = V,
    #{priority_level => PL,
      pre_emption_capability => case PCI of
                                    0 -> false;
                                    1 -> true
                                end,
      pre_emption_vulnerability => case PVI of
                                       0 -> false;
                                       1 -> true
                                   end}.

decode_qos(V) ->
    <<QCI:8, MBRU:40, MBRD:40, GBRU:40, GBRD:40>> = V,
    #{qci => QCI,
      maximum_bitrate_uplink => MBRU,
      maximum_bitrate_downlink => MBRD,
      guaranteed_bitrate_uplink => GBRU,
      guaranteed_bitrate_downlink => GBRD}.

decode_tft_packet_filters(Num, V) ->
    decode_tft_packet_filters(Num, V, []).

decode_tft_packet_filters(0, R, Acc) ->
    {lists:reverse(Acc), R};
decode_tft_packet_filters(N, R0, Acc) ->
    <<_:2, Dir:2, PFID:4, EvalPrec:8, PFLen:8,
      PFBin:PFLen/binary, R2/binary>> = R0,
    PF = #{identifier => PFID,
           direction => case Dir of
                            2#00 -> pre_rel7;
                            2#01 -> downlink;
                            2#10 -> uplink;
                            2#11 -> bidirectional
                        end,
           evaluation_precedence => EvalPrec,
           content => decode_tft_packet_filter_content(PFBin, #{})
          },
    decode_tft_packet_filters(N-1, R2, [PF|Acc]).

decode_tft_packet_filter_content(<<>>, Acc) ->
    Acc;
decode_tft_packet_filter_content(<<2#0001_0000:8, R0/binary>>, Acc) ->
    %% IPv4 remote address type
    <<IPv4:4/binary, R1/binary>> = R0,
    Rem = maps:get(remote, Acc, #{}),
    V = Rem#{ipv4 => bin_to_ip_addr(IPv4)},
    decode_tft_packet_filter_content(R1, Acc#{remote => V});
decode_tft_packet_filter_content(<<2#0001_0001:8, R0/binary>>, Acc) ->
    %% IPv4 local address type
    <<IPv4:4/binary, R1/binary>> = R0,
    Loc = maps:get(local, Acc, #{}),
    V = Loc#{ipv4 => bin_to_ip_addr(IPv4)},
    decode_tft_packet_filter_content(R1, Acc#{local => V});
decode_tft_packet_filter_content(<<2#0010_0000:8, R0/binary>>, Acc) ->
    %% IPv6 remote address type
    <<IPv6:16/binary, Mask:16/binary, R1/binary>> = R0,
    Rem = maps:get(remote, Acc, #{}),
    V = Rem#{ipv6 => bin_to_ip_addr(IPv6),
             mask => bin_to_ip_addr(Mask)},
    decode_tft_packet_filter_content(R1, Acc#{remote => V});
decode_tft_packet_filter_content(<<2#0010_0001:8, R0/binary>>, Acc) ->
    %% IPv6 remote address/prefix length type
    <<IPv6:16/binary, PrefixLen:1/binary, R1/binary>> = R0,
    Rem = maps:get(remote, Acc, #{}),
    V = Rem#{ipv6 => bin_to_ip_addr(IPv6),
             prefix_length => PrefixLen},
    decode_tft_packet_filter_content(R1, Acc#{remote => V});
decode_tft_packet_filter_content(<<2#0010_0011:8, R0/binary>>, Acc) ->
    %% IPv6 local address/prefix length type
    <<IPv6:16/binary, PrefixLen:1/binary, R1/binary>> = R0,
    Loc = maps:get(local, Acc, #{}),
    V = Loc#{ipv6 => bin_to_ip_addr(IPv6),
             prefix_length => PrefixLen},
    decode_tft_packet_filter_content(R1, Acc#{local => V});
decode_tft_packet_filter_content(<<2#0011_0000:8, R0/binary>>, Acc) ->
    %% Protocol identifier/Next header type
    <<ProtocolId:8, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{protocol_identifier => ProtocolId});
decode_tft_packet_filter_content(<<2#0100_0000:8, R0/binary>>, Acc) ->
    %% Single local port type
    <<Port:16, R1/binary>> = R0,
    Loc = maps:get(local, Acc, #{}),
    V = Loc#{port => Port},
    decode_tft_packet_filter_content(R1, Acc#{local => V});
decode_tft_packet_filter_content(<<2#0100_0001:8, R0/binary>>, Acc) ->
    %% Local port range type
    <<LowPort:16, HighPort:16, R1/binary>> = R0,
    Port = #{low => LowPort, high => HighPort},
    Loc = maps:get(local, Acc, #{}),
    V = Loc#{port => Port},
    decode_tft_packet_filter_content(R1, Acc#{local => V});
decode_tft_packet_filter_content(<<2#0101_0000:8, R0/binary>>, Acc) ->
    %% Single remote port type
    <<Port:16, R1/binary>> = R0,
    Rem = maps:get(remote, Acc, #{}),
    V = Rem#{port => Port},
    decode_tft_packet_filter_content(R1, Acc#{remote => V});
decode_tft_packet_filter_content(<<2#0101_0001:8, R0/binary>>, Acc) ->
    %% Remote port range type
    <<LowPort:16, HighPort:16, R1/binary>> = R0,
    Port = #{low => LowPort, high => HighPort},
    Rem = maps:get(remote, Acc, #{}),
    V = Rem#{port => Port},
    decode_tft_packet_filter_content(R1, Acc#{remote => V});
decode_tft_packet_filter_content(<<2#0110_0000:8, R0/binary>>, Acc) ->
    %% Security parameter index type
    <<SecurityParamIndex:4/binary, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{security_parameter_index => SecurityParamIndex});
decode_tft_packet_filter_content(<<2#0111_0000:8, R0/binary>>, Acc) ->
    %% Type of service/Traffic class type
    <<ToS:1/binary, Mask:1/binary, R1/binary>> = R0,
    V = #{type => ToS, mask => Mask},
    decode_tft_packet_filter_content(R1, Acc#{service => V});
decode_tft_packet_filter_content(<<2#1000_0000:8, R0/binary>>, Acc) ->
    %% Flow label type
    <<_:4, FlowLabel:20, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{flow_label => FlowLabel});
decode_tft_packet_filter_content(<<2#1000_0001:8, R0/binary>>, Acc) ->
    %% Destination MAC address type
    <<MAC:6/binary, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{destination_mac => bin_to_mac_addr(MAC)});
decode_tft_packet_filter_content(<<2#1000_0010:8, R0/binary>>, Acc) ->
    %% Source MAC address type
    <<MAC:6/binary, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{source_mac => bin_to_mac_addr(MAC)});
decode_tft_packet_filter_content(<<2#1000_0011:8, R0/binary>>, Acc) ->
    %% 802.1Q C-TAG VID type
    <<_:4, VID:12, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{q_ctag_vid => VID});
decode_tft_packet_filter_content(<<2#1000_0100:8, R0/binary>>, Acc) ->
    %% 802.1Q S-TAG VID type
    <<_:4, VID:12, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{q_stag_vid => VID});
decode_tft_packet_filter_content(<<2#1000_0101:8, R0/binary>>, Acc) ->
    %% 802.1Q C-TAG PCP/DEI type
    <<_:4, PCP:3, DEI:1, R1/binary>> = R0,
    V = #{pcp => PCP, dei => DEI},
    decode_tft_packet_filter_content(R1, Acc#{q_ctag_pcp => V});
decode_tft_packet_filter_content(<<2#1000_0110:8, R0/binary>>, Acc) ->
    %% 802.1Q S-TAG PCP/DEI type
    <<_:4, PCP:3, DEI:1, R1/binary>> = R0,
    V = #{pcp => PCP, dei => DEI},
    decode_tft_packet_filter_content(R1, Acc#{q_stag_pcp => V});
decode_tft_packet_filter_content(<<2#1000_0111:8, R0/binary>>, Acc) ->
    %% Ethertype type
    <<Ethertype:16, R1/binary>> = R0,
    decode_tft_packet_filter_content(R1, Acc#{ethertype => Ethertype}).

decode_tft_parameters_list(R) ->
    decode_tft_parameters_list(R, []).

decode_tft_parameters_list(<<>>, Acc) ->
    lists:reverse(Acc);
decode_tft_parameters_list(R0, Acc) ->
    <<PID:8, PLen:8, PCon:PLen, R2/binary>> = R0,
    P = #{identifier => PID, content => PCon},
    decode_tft_parameters_list(R2, [P|Acc]).


hex_string(BSS) ->
    binary_to_list(binary:encode_hex(BSS)).

maps_merge_all([]) ->
    #{};
maps_merge_all([H|T]) ->
    maps:merge(H, maps_merge_all(T)).

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

decode_tliv(<<254:8, Len:16, _Spare:4, Instance:4, Type:16, Bin0/binary>>) ->
    <<Data:Len/binary, Rest/binary>> = Bin0,
    {Type, Instance, Data, Rest};
decode_tliv(<<Type:8, Len:16, _Spare:4, Instance:4, Bin0/binary>>) ->
    <<Data:Len/binary, Rest/binary>> = Bin0,
    {Type, Instance, Data, Rest}.

decode_tliv_list(Bin, List) ->
    decode_tliv_list(Bin, List, #{}).

decode_tliv_list(<<>>, List, Acc) ->
    %% All mandatory TLIVs are present
    case [element(1, L) || L <- List, mandatory =:= element(3, L)] of
        [] ->
            {Acc, <<>>};
        T ->
            {error, {missing_mandatory_tlivs, T}}
    end;
decode_tliv_list(Bin, [], Acc) ->
    {Acc, Bin};
decode_tliv_list(Bin, List, Acc) ->
    %% TODO: support for Private Extension instance=VS
    {Type, Instance, DataBin, Rest} = decode_tliv(Bin),
    {value, H, NewList} = tlivtake(Type, Instance, List),
    Name = element(1, H),
    Opts = case H of
               {_, _, _, O} -> O;
               _ -> []
           end,
    Data = decode_parameter(parse_iei(Type), DataBin, Opts),
    decode_tliv_list(Rest, NewList, Acc#{Name => Data}).

tlivtake(Type, Instance, List) ->
    tlivtake(Type, Instance, List, []).
tlivtake(_Type, _Instance, [], _Acc) ->
    false;
tlivtake(Type, Instance, [H|T], Acc) ->
    %% {name, {type, instance}, mandatory|optional}
    %% {mm_context, {?GTPv2_IEI_MM_CONTEXT, 0}, mandatory},
    case element(2, H) of
        {Types, Instance} when is_list(Types) ->
            case lists:member(Type, Types) of
                true ->
                    {value, H, lists:reverse(Acc) ++ T};
                false ->
                    tlivtake(Type, Instance, T, [H|Acc])
            end;
        {Type, Instance} ->
            {value, H, lists:reverse(Acc) ++ T};
        _ ->
            tlivtake(Type, Instance, T, [H|Acc])
    end.

decode_msg(echo_request, Bin0) ->
    Fields = [{recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, mandatory},
              {sending_node_features, {?GTPv2_IEI_NODE_FEATURES, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => path_management
        };
decode_msg(echo_response, Bin0) ->
    Fields = [{recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, mandatory},
              {sending_node_features, {?GTPv2_IEI_NODE_FEATURES, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => path_management
        };
decode_msg(version_not_supported, Bin0) ->
    Fields = [],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => path_management
        };
decode_msg(create_session_request, Bin0) ->
    BearerContextCreated = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {tft, {?GTPv2_IEI_BEARER_TFT, 0}, optional},
                            {s1u_enodeb_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                            {s4u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                            {s5s8u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional},
                            {s5s8u_pgw_f_teid, {?GTPv2_IEI_F_TEID, 3}, conditional},
                            {s12_rnc_f_teid, {?GTPv2_IEI_F_TEID, 4}, conditional_optional},
                            {s2bu_epdg_f_teid, {?GTPv2_IEI_F_TEID, 5}, conditional},
                            {s2au_twan_f_teid, {?GTPv2_IEI_F_TEID, 6}, conditional},
                            {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
                            {s11u_mme_f_teid, {?GTPv2_IEI_F_TEID, 7}, conditional_optional}
                           ],
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {s4u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional}
                           ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    RemoteUEContextConnected = [{remote_user_id, {?GTPv2_IEI_REMOTE_USER_ID, 0}, mandatory},
                                {remote_ue_ip_information, {?GTPv2_IEI_REMOTE_UE_IP_INFORMATION, 0}, mandatory}
                               ],
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {msisdn, {?GTPv2_IEI_MSISDN, 0}, conditional},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {serving_network, {?GTPv2_IEI_SERVING_NETWORK, 0}, conditional},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, mandatory},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, mandatory},
              {pgw_s5s8_address, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {apn, {?GTPv2_IEI_APN, 0}, mandatory},
              {selection_mode, {?GTPv2_IEI_SELECTION_MODE, 0}, conditional},
              {pdn_type, {?GTPv2_IEI_PDN_TYPE, 0}, conditional},
              {pdn_address_allocation, {?GTPv2_IEI_PDN_ADDRESS_ALLOCATION, 0}, conditional},
              {maximum_apn_restriction, {?GTPv2_IEI_APN_RESTRICTION, 0}, conditional},
              {apn_ambr, {?GTPv2_IEI_AMBR, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {trusted_wlan_mode_indication, {?GTPv2_IEI_TRUSTED_WLAN_MODE_INDICATION, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {bearer_contexts_to_be_created, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContextCreated},
              {bearer_contexts_to_be_removed, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {trace_information, {?GTPv2_IEI_TRACE_INFORMATION, 0}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {epdg_fq_csid, {?GTPv2_IEI_FQ_CSID, 2}, conditional},
              {twan_fq_csid, {?GTPv2_IEI_FQ_CSID, 3}, conditional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional},
              {user_csg_information, {?GTPv2_IEI_USER_CSG_INFORMATION, 0}, conditional},
              {charging_characteristics, {?GTPv2_IEI_CHARGING_CHARACTERISTICS, 0}, conditional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {sgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 1}, optional},
              {epdg_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 2}, optional},
              {twan_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 3}, optional},
              {signalling_priority_indication, {?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION, 0}, conditional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional},
              {additional_protocol_config_opts, {?GTPv2_IEI_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {henb_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
              {henb_udp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 2}, conditional},
              {twan_identifier, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional},
              {epdg_ip_address, {?GTPv2_IEI_IP_ADDRESS, 3}, optional},
              {cn_operator_selection_entity, {?GTPv2_IEI_CN_OPERATOR_SELECTION_ENTITY, 0}, conditional},
              {presence_reporting_area_information, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION, 0}, conditional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {origination_time_stamp, {?GTPv2_IEI_MILLISECOND_TIME_STAMP, 0}, conditional},
              {maximum_wait_time, {?GTPv2_IEI_INTEGER_NUMBER, 0}, conditional},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 1}, conditional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 0}, conditional},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional},
              {remote_ue_context_connected, {?GTPv2_IEI_REMOTE_UE_CONTEXT, 0}, conditional, RemoteUEContextConnected},
              {tgpp_aaa_server_identifier, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, optional},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {serving_plmn_rate_control, {?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL, 0}, conditional},
              {mo_exception_data_counter, {?GTPv2_IEI_COUNTER, 0}, conditional},
              {ue_tcp_port, {?GTPv2_IEI_PORT_NUMBER, 2}, conditional},
              {mapped_ue_usage_type, {?GTPv2_IEI_MAPPED_UE_USAGE_TYPE, 0}, conditional},
              {uli_for_sgw, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 1}, conditional},
              {sgw_u_node_name, {?GTPv2_IEI_FQDN, 0}, conditional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional},
              {up_function_selection_indication_flags, {?GTPv2_IEI_UP_FUNCTION_SELECTION_INDICATION_FLAGS, 0}, conditional},
              {apn_rate_control_status, {?GTPv2_IEI_APN_RATE_CONTROL_STATUS, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(create_session_response, Bin0) ->
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory}
                           ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, conditional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, conditional},
                     {group_id, {?GTPv2_IEI_GROUP_ID, 0}, optional}
                    ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
              {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional},
              {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {pgw_s5s8s2as2b_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {pdn_address_allocation, {?GTPv2_IEI_PDN_ADDRESS_ALLOCATION, 0}, conditional},
              {apn_restriction, {?GTPv2_IEI_APN_RESTRICTION, 0}, conditional},
              {apn_ambr, {?GTPv2_IEI_AMBR, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {bearer_contexts_created, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory},
              {bearer_contexts_marked_for_removal, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {charging_gateway_name, {?GTPv2_IEI_FQDN, 0}, conditional},
              {charging_gateway_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {sgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {pgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 1}, optional},
              {pgw_back_off_time, {?GTPv2_IEI_EPC_TIMER, 0}, optional},
              {additional_protocol_config_opts, {?GTPv2_IEI_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {trusted_wlan_ipv4_parameters, {?GTPv2_IEI_IPV4_CONFIGURATION_PARAMETERS, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional},
              {pdn_connection_charging_id, {?GTPv2_IEI_CHARGING_ID, 0}, conditional},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {pgw_node_name, {?GTPv2_IEI_FQDN, 1}, conditional},
              {sgi_ptp_tunnel_address, {?GTPv2_IEI_SGI_PTP_TUNNEL_ADDRESS, 0}, conditional},
              {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional, PGWChangeInfo},
              {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_FQDN, 3}, optional},
              {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, optional},
              {up_security_policy, {?GTPv2_IEI_UP_SECURITY_POLICY, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(create_bearer_request, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {tft, {?GTPv2_IEI_BEARER_TFT, 0}, mandatory},
                     {s1u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {s5s8u_pgw_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                     {s12_sgw_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional},
                     {s4u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 3}, conditional},
                     {s2bu_pgw_f_teid, {?GTPv2_IEI_F_TEID, 4}, conditional},
                     {s2au_pgw_f_teid, {?GTPv2_IEI_F_TEID, 5}, conditional},
                     {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
                     {charging_id, {?GTPv2_IEI_CHARGING_ID, 0}, conditional_optional},
                     {bearer_flags, {?GTPv2_IEI_BEARER_FLAGS, 0}, optional},
                     {protocol_configuration_options, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, optional},
                     {extended_protocol_configuration_options, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, optional},
                     {maximum_packet_loss_rate, {?GTPv2_IEI_MAXIMUM_PACKET_LOSS_RATE, 0}, conditional_optional}
                    ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, optional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, optional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, optional},
                     {new_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
                     {new_sgwc_ip_address, {?GTPv2_IEI_IP_ADDRESS, 2}, optional},
                     {pgw_csmf_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
                     {group_id, {?GTPv2_IEI_GROUP_ID, 0}, conditional},
                     {pgw_control_plane_ip_address, {?GTPv2_IEI_IP_ADDRESS, 2}, conditional},
                     {new_group_id, {?GTPv2_IEI_GROUP_ID, 1}, optional}
                    ],
    Fields = [{procedure_transaction_id, {?GTPv2_IEI_PROCEDURE_TRANSACTION_ID, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, optional},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
              {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
              {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional_optional},
              {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional_optional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional_optional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(create_bearer_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                     {s1u_enodeb_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {s1u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                     {s5s8u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional},
                     {s5s8u_pgw_f_teid, {?GTPv2_IEI_F_TEID, 3}, conditional},
                     {s12_rnc_f_teid, {?GTPv2_IEI_F_TEID, 4}, conditional},
                     {s12_sgw_f_teid, {?GTPv2_IEI_F_TEID, 5}, conditional},
                     {s4u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 6}, conditional},
                     {s4u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 7}, conditional},
                     {s2bu_epdg_f_teid, {?GTPv2_IEI_F_TEID, 8}, conditional},
                     {s2bu_pgw_f_teid, {?GTPv2_IEI_F_TEID, 9}, conditional},
                     {s2au_twan_f_teid, {?GTPv2_IEI_F_TEID, 10}, conditional},
                     {s2au_pgw_f_teid, {?GTPv2_IEI_F_TEID, 11}, conditional},
                     {protocol_configuration_options, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {ran_nas_cause, {?GTPv2_IEI_RAN_NAS_CAUSE, 0}, conditional_optional},
                     {extended_protocol_configuration_options, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {epdg_fq_csid, {?GTPv2_IEI_FQ_CSID, 2}, conditional},
              {twan_fq_csid, {?GTPv2_IEI_FQ_CSID, 3}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional_optional},
              {twan_identifier, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {presence_reporting_area_information, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION, 0}, conditional_optional},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 1}, conditional_optional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 1}, conditional_optional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {ue_tcp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(bearer_resource_command, Bin0) ->
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
              {procedure_transaction_id, {?GTPv2_IEI_PROCEDURE_TRANSACTION_ID, 0}, mandatory},
              {flow_qos, {?GTPv2_IEI_FLOW_QOS, 0}, conditional},
              {traffic_aggregate_description, {?GTPv2_IEI_TRAFFIC_AGGREGATE_DESCRIPTION, 0}, conditional},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, conditional},
              {serving_network, {?GTPv2_IEI_SERVING_NETWORK, 0}, optional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, optional},
              {eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 1}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {s4_u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {s12_rnc_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, optional},
              {signalling_priority_indication, {?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, optional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(bearer_resource_failure_indication, Bin0) ->
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
              {procedure_transaction_id, {?GTPv2_IEI_PROCEDURE_TRANSACTION_ID, 0}, mandatory},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, optional},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_bearer_request, Bin0) ->
    BearerContextModified = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                             {s1_enodeb_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                             {s5s8u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                             {s12_rnc_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional},
                             {s4u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 3}, conditional},
                             {s11u_mme_f_teid, {?GTPv2_IEI_F_TEID, 4}, conditional_optional}
                            ],
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory}
                           ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {serving_network, {?GTPv2_IEI_SERVING_NETWORK, 0}, conditional_optional},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {apn_ambr, {?GTPv2_IEI_AMBR, 0}, conditional},
              {delay_downlink_packet_notification_request, {?GTPv2_IEI_DELAY_VALUE, 0}, conditional},
              {bearer_contexts_to_be_modified, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContextModified},
              {bearer_contexts_to_be_removed, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {user_csg_information, {?GTPv2_IEI_USER_CSG_INFORMATION, 0}, conditional_optional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional_optional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional_optional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {sgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 1}, optional},
              {henb_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {henb_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 2}, conditional_optional},
              {cn_operator_selection_entity, {?GTPv2_IEI_CN_OPERATOR_SELECTION_ENTITY, 0}, conditional_optional},
              {presence_reporting_area_information, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {epdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {serving_plmn_rate_control, {?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL, 0}, conditional_optional},
              {mo_exception_data_counter, {?GTPv2_IEI_COUNTER, 0}, conditional_optional},
              {imsi, {?GTPv2_IEI_IMSI, 0}, optional},
              {uli_for_sgw, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 1}, conditional_optional},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional_optional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 0}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_bearer_response, Bin0) ->
    BearerContextModified = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                             {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                             {s1u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                             {s12_sgw_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                             {s4U_sgw_f_teid, {?GTPv2_IEI_F_TEID, 2}, conditional},
                             {charging_id, {?GTPv2_IEI_CHARGING_ID, 0}, conditional},
                             {bearer_flags, {?GTPv2_IEI_BEARER_FLAGS, 0}, conditional_optional},
                             {s11u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 3}, conditional}
                            ],
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory}
                           ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, conditional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, conditional},
                     {group_id, {?GTPv2_IEI_GROUP_ID, 0}, optional}
                    ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {msisdn, {?GTPv2_IEI_MSISDN, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {apn_restriction, {?GTPv2_IEI_APN_RESTRICTION, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {bearer_contexts_modified, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContextModified},
              {bearer_contexts_marked_for_removal, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
              {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
              {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional_optional},
              {charging_gateway_name, {?GTPv2_IEI_FQDN, 0}, conditional},
              {charging_gateway_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {sgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {pgw_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 1}, optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional_optional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional_optional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {pdn_connection_charging_id, {?GTPv2_IEI_CHARGING_ID, 0}, conditional_optional},
              {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_session_request, Bin0) ->
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {originating_node, {?GTPv2_IEI_NODE_TYPE, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {uli_timestamp, {?GTPv2_IEI_ULI_TIMESTAMP, 0}, conditional_optional},
              {rannas_release_cause, {?GTPv2_IEI_RAN_NAS_CAUSE, 0}, conditional_optional},
              {twan_identifier, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional_optional},
              {twan_identifier_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 1}, conditional_optional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 1}, conditional_optional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
              {ue_tcp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_bearer_request, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory}
                    ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, optional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, optional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, optional},
                     {new_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
                     {new_sgwc_ip_address, {?GTPv2_IEI_IP_ADDRESS, 3}, optional},
                     {pgw_csmf_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
                     {group_id, {?GTPv2_IEI_GROUP_ID, 0}, conditional},
                     {pgw_control_plane_ip_address, {?GTPv2_IEI_IP_ADDRESS, 2}, conditional},
                     {new_group_id, {?GTPv2_IEI_GROUP_ID, 1}, optional}
                    ],
    Fields = [{linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {eps_bearer_ids, {?GTPv2_IEI_EPS_BEARER_ID, 1}, conditional},
              {failed_bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, optional, BearerContext},
              {procedure_transaction_id, {?GTPv2_IEI_PROCEDURE_TRANSACTION_ID, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {cause, {?GTPv2_IEI_CAUSE, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional_optional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional_optional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {apn_rate_control_status, {?GTPv2_IEI_APN_RATE_CONTROL_STATUS, 0}, conditional_optional},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
              {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_session_response, Bin0) ->
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional_optional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional_optional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {extended_protocol_config_opts, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
              {apn_rate_control_status, {?GTPv2_IEI_APN_RATE_CONTROL_STATUS, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_bearer_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                     {protocol_configuration_options, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {ran_nas_cause, {?GTPv2_IEI_RAN_NAS_CAUSE, 0}, conditional_optional},
                     {extended_protocol_configuration_options, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContext},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {epdg_fq_csid, {?GTPv2_IEI_FQ_CSID, 2}, conditional},
              {twan_fq_csid, {?GTPv2_IEI_FQ_CSID, 3}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional_optional},
              {uli_timestamp, {?GTPv2_IEI_ULI_TIMESTAMP, 0}, conditional_optional},
              {twan_identifier, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional_optional},
              {twan_identifier_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 1}, conditional_optional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 1}, conditional_optional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {ue_tcp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(downlink_data_notification, Bin0) ->
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, conditional_optional},
              {eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional_optional},
              {allocation_retention_priority, {?GTPv2_IEI_ALLOCATION_RETENTION_PRIORITY, 0}, conditional_optional},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional_optional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, optional, LCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, optional, OCI},
              {paging_and_service_information, {?GTPv2_IEI_PAGING_AND_SERVICE_INFORMATION, 0}, conditional_optional},
              {dl_data_packets_size, {?GTPv2_IEI_INTEGER_NUMBER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(downlink_data_notification_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {data_notification_delay, {?GTPv2_IEI_DELAY_VALUE, 0}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {dl_low_priority_traffic_throttling, {?GTPv2_IEI_THROTTLING, 0}, optional},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional_optional},
              {dl_buffering_duration, {?GTPv2_IEI_EPC_TIMER, 0}, conditional_optional},
              {dl_buffering_suggested_packet_count, {?GTPv2_IEI_INTEGER_NUMBER, 0}, optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(downlink_data_notification_failure_indication, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {originating_node, {?GTPv2_IEI_NODE_TYPE, 0}, conditional_optional},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_indirect_data_forwarding_tunnel_request, Bin0) ->
    Fields = [{private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_indirect_data_forwarding_tunnel_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_bearer_command, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, conditional_optional}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{apn_ambr, {?GTPv2_IEI_AMBR, 0}, mandatory},
              {bearer_context, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_bearer_failure_indication, Bin0) ->
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(update_bearer_request, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {tft, {?GTPv2_IEI_BEARER_TFT, 0}, conditional},
                     {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, conditional},
                     {bearer_flags, {?GTPv2_IEI_BEARER_FLAGS, 0}, optional},
                     {protocol_configuration_options, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {additional_protocol_configuration_options, {?GTPv2_IEI_ADDITIONAL_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {extended_protocol_configuration_options, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {maximum_packet_loss_rate, {?GTPv2_IEI_MAXIMUM_PACKET_LOSS_RATE, 0}, conditional_optional}
                    ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {apns_and_relative_capacity, {?GTPv2_IEI_APN_AND_RELATIVE_CAPACITY, 0}, conditional_optional}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, optional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, optional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, optional},
                     {new_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
                     {new_sgwc_ip_address, {?GTPv2_IEI_IP_ADDRESS, 3}, optional},
                     {pgw_csmf_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
                     {group_id, {?GTPv2_IEI_GROUP_ID, 0}, conditional},
                     {pgw_control_plane_ip_address, {?GTPv2_IEI_IP_ADDRESS, 2}, conditional},
                     {new_group_id, {?GTPv2_IEI_GROUP_ID, 1}, optional}
                    ],
    Fields = [{bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {procedure_transaction_id, {?GTPv2_IEI_PROCEDURE_TRANSACTION_ID, 0}, conditional},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional},
              {apn_ambr, {?GTPv2_IEI_AMBR, 0}, mandatory},
              {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
              {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
              {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional_optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
              {pgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, conditional_optional, LCI},
              {pgws_apn_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 1}, conditional_optional, LCI},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 2}, optional, LCI},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(update_bearer_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                     {s4u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {s12_rnc_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
                     {protocol_configuration_options, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
                     {ran_nas_cause, {?GTPv2_IEI_RAN_NAS_CAUSE, 0}, conditional_optional},
                     {extended_protocol_configuration_options, {?GTPv2_IEI_EXTENDED_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {protocol_config_opts, {?GTPv2_IEI_PROTOCOL_CONFIGURATION_OPTIONS, 0}, conditional_optional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {epdg_fq_csid, {?GTPv2_IEI_FQ_CSID, 2}, conditional},
              {twan_fq_csid, {?GTPv2_IEI_FQ_CSID, 3}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional_optional},
              {twan_identifier, {?GTPv2_IEI_TWAN_IDENTIFIER, 0}, conditional_optional},
              {mmes4_sgsns_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {presence_reporting_area_information, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION, 0}, conditional_optional},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {twanepdgs_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 2}, optional, OCI},
              {wlan_location_information, {?GTPv2_IEI_TWAN_IDENTIFIER, 1}, conditional_optional},
              {wlan_location_timestamp, {?GTPv2_IEI_TWAN_IDENTIFIER_TIMESTAMP, 1}, conditional_optional},
              {ue_local_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {ue_udp_port, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {nbifom_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
              {ue_tcp_port, {?GTPv2_IEI_PORT_NUMBER, 1}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_bearer_command, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {bearer_flags, {?GTPv2_IEI_BEARER_FLAGS, 0}, conditional_optional},
                     {ran_nas_cause, {?GTPv2_IEI_RAN_NAS_CAUSE, 0}, conditional_optional}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional_optional},
              {uli_timestamp, {?GTPv2_IEI_ULI_TIMESTAMP, 0}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(delete_bearer_failure_indication, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory}
                    ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory},
           {apns, {?GTPv2_IEI_APN, 0}, conditional_optional}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {bearer_context, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {pgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, conditional_optional, OCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 1}, optional, OCI},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(create_indirect_data_forwarding_tunnel_request, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {enodeb_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
                     {sgw_upf_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 1}, conditional_optional},
                     {sgsn_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 2}, conditional_optional},
                     {rnc_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 3}, conditional_optional},
                     {enodeb_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 4}, optional},
                     {sgw_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 5}, optional},
                     {mme_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 6}, conditional_optional}
                    ],
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(create_indirect_data_forwarding_tunnel_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                     {s1u_sgw_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {s12_sgw_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 1}, conditional},
                     {s4u_sgw_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 2}, conditional},
                     {sgw_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 3}, conditional},
                     {s1u_sgw_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 4}, optional},
                     {sgw_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 5}, optional}
                    ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(release_access_bearers_request, Bin0) ->
    Fields = [{list_of_rabs, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
              {originating_node, {?GTPv2_IEI_NODE_TYPE, 0}, conditional_optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(release_access_bearers_response, Bin0) ->
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, optional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, optional, LCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, optional, OCI},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(stop_paging_indication, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_access_bearers_request, Bin0) ->
    BearerContextModified = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                             {s1u_enodeb_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                             {s11u_mme_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional_optional}
                            ],
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory}
                           ],
    Fields = [{indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {delay_downlink_packet_notification_request, {?GTPv2_IEI_DELAY_VALUE, 0}, conditional},
              {bearer_contexts_to_be_modified, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContextModified},
              {bearer_contexts_to_be_removed, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(modify_access_bearers_response, Bin0) ->
    BearerContextModified = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                             {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
                             {s1u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
                             {s11u_sgw_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional}
                            ],
    BearerContextRemoved = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory}
                           ],
    LCI = [{load_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {load_metric, {?GTPv2_IEI_METRIC, 0}, mandatory}
          ],
    OCI = [{overload_control_sequence_number, {?GTPv2_IEI_SEQUENCE_NUMBER, 0}, mandatory},
           {overload_reduction_metric, {?GTPv2_IEI_METRIC, 0}, mandatory},
           {period_of_validity, {?GTPv2_IEI_EPC_TIMER, 0}, mandatory}
          ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {bearer_contexts_modified, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContextModified},
              {bearer_contexts_marked_for_removal, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContextRemoved},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {sgws_node_level_load_control_information, {?GTPv2_IEI_LOAD_CONTROL_INFORMATION, 0}, optional, LCI},
              {sgws_overload_control_information, {?GTPv2_IEI_OVERLOAD_CONTROL_INFORMATION, 0}, optional, OCI},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(remote_ue_report_notification, Bin0) ->
    RemoteUEContextConnected = [{remote_user_id, {?GTPv2_IEI_REMOTE_USER_ID, 0}, mandatory},
                                {remote_ue_ip_information, {?GTPv2_IEI_REMOTE_UE_IP_INFORMATION, 0}, mandatory}
                               ],
    RemoteUEContextDisconnected = [{remote_user_id, {?GTPv2_IEI_REMOTE_USER_ID, 0}, mandatory}
                                  ],
    Fields = [{remote_ue_context_connected, {?GTPv2_IEI_REMOTE_UE_CONTEXT, 0}, conditional, RemoteUEContextConnected},
              {remote_ue_context_disconnected, {?GTPv2_IEI_REMOTE_UE_CONTEXT, 1}, conditional, RemoteUEContextDisconnected},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(remote_ue_report_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => tunnel_management
        };
decode_msg(forward_relocation_request, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {tft, {?GTPv2_IEI_BEARER_TFT, 0}, conditional},
                     {sgw_s1s4s12_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 0}, mandatory},
                     {pgw_s5s8_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 1}, conditional_optional},
                     {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
                     {bss_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
                     {transaction_identifier, {?GTPv2_IEI_TRANSACTION_IDENTIFIER, 0}, conditional},
                     {bearer_flags, {?GTPv2_IEI_BEARER_FLAGS, 0}, conditional_optional},
                     {sgw_s11_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 2}, conditional_optional}
                    ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, conditional_optional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, conditional_optional}
                    ],
    RemoteUEContextConnected = [{remote_user_id, {?GTPv2_IEI_REMOTE_USER_ID, 0}, mandatory},
                                {remote_ue_ip_information, {?GTPv2_IEI_REMOTE_UE_IP_INFORMATION, 0}, mandatory}
                               ],
    EPSPDNConnections = [{apn, {?GTPv2_IEI_APN, 0}, mandatory},
                         {apn_restriction, {?GTPv2_IEI_APN_RESTRICTION, 0}, conditional},
                         {selection_mode, {?GTPv2_IEI_SELECTION_MODE, 0}, conditional_optional},
                         {ipv4_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
                         {ipv6_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
                         {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                         {pgw_s5s8_ip_address_for_control_plane_or_pmip, {?GTPv2_IEI_F_TEID, 0}, mandatory},
                         {pgw_node_name, {?GTPv2_IEI_FQDN, 0}, conditional_optional},
                         {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContext},
                         {apn_ambr, {?GTPv2_IEI_AMBR, 0}, mandatory},
                         {charging_characteristics, {?GTPv2_IEI_CHARGING_CHARACTERISTICS, 0}, conditional},
                         {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
                         {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
                         {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional_optional},
                         {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
                         {signalling_priority_indication, {?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION, 0}, conditional_optional},
                         {change_to_report_flags, {?GTPv2_IEI_CHANGE_TO_REPORT_FLAGS, 0}, conditional_optional},
                         {local_home_network_id, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
                         {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
                         {wlan_offloadability_indication, {?GTPv2_IEI_WLAN_OFFLOADABILITY_INDICATION, 0}, conditional_optional},
                         {remote_ue_context_connected, {?GTPv2_IEI_REMOTE_UE_CONTEXT, 0}, conditional_optional, RemoteUEContextConnected},
                         {pdn_type, {?GTPv2_IEI_PDN_TYPE, 0}, conditional_optional},
                         {header_compression_configuration, {?GTPv2_IEI_HEADER_COMPRESSION_CONFIGURATION, 0}, conditional_optional},
                         {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
                         {up_security_policy, {?GTPv2_IEI_UP_SECURITY_POLICY, 0}, conditional_optional}
                        ],
    UESCEFPDNConnections = [{apn, {?GTPv2_IEI_APN, 0}, mandatory},
                            {default_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {scef_id, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, mandatory}
                           ],
    PC5QoSParameters = [{pc5_qos_flows, {?GTPv2_IEI_PC5_QOS_FLOW, 0}, mandatory},
                        {pc5_link_aggregated_bit_rates, {?GTPv2_IEI_BIT_RATE, 0}, optional}
                       ],
    SubscribedV2XInformation = [{lte_v2x_services_authorized, {?GTPv2_IEI_SERVICES_AUTHORIZED, 0}, conditional},
                                {nr_v2x_services_authorized, {?GTPv2_IEI_SERVICES_AUTHORIZED, 1}, conditional},
                                {lte_ue_sidelink_ambr, {?GTPv2_IEI_BIT_RATE, 0}, conditional},
                                {nr_ue_sidelink_ambr, {?GTPv2_IEI_BIT_RATE, 1}, conditonal},
                                {pc5_qos_parameters, {?GTPv2_IEI_PC5_QOS_PARAMETERS, 0}, conditional, PC5QoSParameters}
                               ],
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, mandatory},
              {mmesgsnamf_ue_eps_pdn_connections, {?GTPv2_IEI_PDN_CONNECTION, 0}, conditional, EPSPDNConnections},
              {sgw_s11s4_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {sgw_node_name, {?GTPv2_IEI_FQDN, 0}, conditional},
              {mmesgsnamf_ue_mm_context, {?GTPv2_IEI_MM_CONTEXTS, 0}, mandatory},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {e_utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional},
              {utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 1}, conditional},
              {bss_container, {?GTPv2_IEI_F_CONTAINER, 2}, conditional},
              {target_identification, {?GTPv2_IEI_TARGET_IDENTIFICATION, 0}, conditional},
              {hrpd_access_node_s101_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {iws_1x_s102_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
              {s1_ap_cause, {?GTPv2_IEI_F_CAUSE, 0}, conditional},
              {ranap_cause, {?GTPv2_IEI_F_CAUSE, 1}, conditional},
              {bssgp_cause, {?GTPv2_IEI_F_CAUSE, 2}, conditional},
              {source_identification, {?GTPv2_IEI_SOURCE_IDENTIFICATION, 0}, conditional},
              {selected_plmn_id, {?GTPv2_IEI_PLMN_ID, 0}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {trace_information, {?GTPv2_IEI_TRACE_INFORMATION, 0}, conditional},
              {subscribed_rfsp_index, {?GTPv2_IEI_RFSP_INDEX, 0}, conditional_optional},
              {rfsp_index_in_use, {?GTPv2_IEI_RFSP_INDEX, 1}, conditional_optional},
              {csg_id, {?GTPv2_IEI_CSG_ID, 0}, conditional_optional},
              {csg_membership_indication, {?GTPv2_IEI_CSG_MEMBERSHIP_INDICATION, 0}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {serving_network, {?GTPv2_IEI_SERVING_NETWORK, 0}, conditional_optional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {additional_mm_context_for_srvcc, {?GTPv2_IEI_ADDITIONAL_MM_CONTEXT_FOR_SRVCC, 0}, conditional_optional},
              {additional_flags_for_srvcc, {?GTPv2_IEI_ADDITIONAL_FLAGS_FOR_SRVCC, 0}, conditional_optional},
              {stn_sr, {?GTPv2_IEI_STN_SR, 0}, conditional_optional},
              {c_msisdn, {?GTPv2_IEI_MSISDN, 0}, conditional_optional},
              {mdt_configuration, {?GTPv2_IEI_MDT_CONFIGURATION, 0}, conditional_optional},
              {sgsn_node_name, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
              {mme_node_name, {?GTPv2_IEI_FQDN, 2}, conditional_optional},
              {user_csg_information, {?GTPv2_IEI_USER_CSG_INFORMATION, 0}, conditional_optional},
              {monitoring_event_information, {?GTPv2_IEI_MONITORING_EVENT_INFORMATION, 0}, conditional_optional},
              {monitoring_event_extension_information, {?GTPv2_IEI_MONITORING_EVENT_EXTENSION_INFORMATION, 0}, conditional_optional},
              {ue_usage_type, {?GTPv2_IEI_INTEGER_NUMBER, 0}, conditional_optional},
              {mmesgsn_ue_scef_pdn_connections, {?GTPv2_IEI_SCEF_PDN_CONNECTION, 0}, conditional_optional, UESCEFPDNConnections},
              {msisdn, {?GTPv2_IEI_MSISDN, 1}, conditional_optional},
              {source_udp_port_number, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {serving_plmn_rate_control, {?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL, 0}, conditional_optional},
              {extended_trace_information, {?GTPv2_IEI_EXTENDED_TRACE_INFORMATION, 0}, conditional},
              {subscribed_additional_rrm_policy_index, {?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX, 0}, conditional_optional},
              {additional_rrm_policy_index_in_use, {?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX, 1}, conditional_optional},
              {subscribed_v2x_information, {?GTPv2_IEI_V2X_CONTEXT, 0}, conditional_optional, SubscribedV2XInformation},
              {iwk_scef_id_for_monitoring_event, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, conditional_optional},
              {alternative_imsi, {?GTPv2_IEI_ALTERNATIVE_IMSI, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(forward_relocation_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional},
                     {packet_flow_id, {?GTPv2_IEI_PACKET_FLOW_ID, 0}, conditional},
                     {enodeb_gnodeb_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {enodeb_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 1}, optional},
                     {sgw_upf_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 2}, conditional_optional},
                     {rnc_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 3}, conditional},
                     {sgsn_f_teid_for_dl_data_forwarding, {?GTPv2_IEI_F_TEID, 4}, conditional},
                     {sgw_f_teid_for_ul_data_forwarding, {?GTPv2_IEI_F_TEID, 5}, optional}
                    ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {list_of_set_up_bearers, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional, BearerContext},
              {list_of_set_up_rabs, {?GTPv2_IEI_BEARER_CONTEXT, 1}, conditional, BearerContext},
              {list_of_set_up_pfcs, {?GTPv2_IEI_BEARER_CONTEXT, 2}, optional, BearerContext},
              {s1_ap_cause, {?GTPv2_IEI_F_CAUSE, 0}, conditional},
              {ranap_cause, {?GTPv2_IEI_F_CAUSE, 1}, conditional},
              {bssgp_cause, {?GTPv2_IEI_F_CAUSE, 2}, conditional},
              {e_utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional},
              {utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 1}, conditional},
              {bss_container, {?GTPv2_IEI_F_CONTAINER, 2}, conditional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {sgsn_node_name, {?GTPv2_IEI_FQDN, 0}, conditional_optional},
              {mme_node_name, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
              {sgsn_number, {?GTPv2_IEI_NODE_NUMBER, 0}, conditional_optional},
              {sgsn_identifier, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, optional},
              {mme_identifier, {?GTPv2_IEI_NODE_IDENTIFIER, 1}, optional},
              {mme_number_for_mt_sms, {?GTPv2_IEI_NODE_NUMBER, 1}, conditional_optional},
              {sgsn_identifier_for_mt_sms, {?GTPv2_IEI_NODE_IDENTIFIER, 2}, conditional_optional},
              {mme_identifier_for_mt_sms, {?GTPv2_IEI_NODE_IDENTIFIER, 3}, conditional_optional},
              {list_of_set_up_bearers_for_scef_pdn_connections, {?GTPv2_IEI_BEARER_CONTEXT, 3}, conditional_optional, BearerContext},
              {vsrvcc_rejected_cause, {?GTPv2_IEI_SRVCC_CAUSE, 0}, conditional_optional},
              {msc_number, {?GTPv2_IEI_NODE_NUMBER, 2}, optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(forward_relocation_complete_notification, Bin0) ->
    Fields = [{indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(forward_relocation_complete_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {secondary_rat_usage_data_report_from_ng_ran, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 1}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(context_request, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {guti, {?GTPv2_IEI_GUTI, 0}, conditional},
              {rai, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {p_tmsi, {?GTPv2_IEI_P_TMSI, 0}, conditional},
              {p_tmsi_signature, {?GTPv2_IEI_P_TMSI_SIGNATURE, 0}, conditional},
              {complete_tau_request_message, {?GTPv2_IEI_COMPLETE_REQUEST_MESSAGE, 0}, conditional},
              {s3s16s10n26_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {udp_source_port_number, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, conditional},
              {indication, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {hop_counter, {?GTPv2_IEI_HOP_COUNTER, 0}, optional},
              {target_plmn_id, {?GTPv2_IEI_SERVING_NETWORK, 0}, conditional_optional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {sgsn_node_name, {?GTPv2_IEI_FQDN, 0}, conditional_optional},
              {mme_node_name, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
              {sgsn_number, {?GTPv2_IEI_NODE_NUMBER, 0}, optional},
              {sgsn_identifier, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, optional},
              {mme_identifier, {?GTPv2_IEI_NODE_IDENTIFIER, 1}, optional},
              {ciot_optimizations_support_indication, {?GTPv2_IEI_CIOT_OPTIMIZATIONS_SUPPORT_INDICATION, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(context_response, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {tft, {?GTPv2_IEI_BEARER_TFT, 0}, conditional},
                     {sgw_s1s4s12s11_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 0}, conditional},
                     {pgw_s5s8_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 1}, conditional_optional},
                     {bearer_level_qos, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
                     {bss_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional_optional},
                     {transaction_identifier, {?GTPv2_IEI_TRANSACTION_IDENTIFIER, 0}, conditional},
                     {sgw_s11_ip_address_and_teid_for_user_plane, {?GTPv2_IEI_F_TEID, 2}, conditional_optional}
                    ],
    RemoteUEContextConnected = [{remote_user_id, {?GTPv2_IEI_REMOTE_USER_ID, 0}, mandatory},
                                {remote_ue_ip_information, {?GTPv2_IEI_REMOTE_UE_IP_INFORMATION, 0}, mandatory}
                               ],
    PGWChangeInfo = [{pgw_set_fqdn, {?GTPv2_IEI_PGW_FQDN, 0}, conditional_optional},
                     {alternative_pgw_csmf_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
                     {alternative_pgw_csmf_fqdn, {?GTPv2_IEI_PGW_FQDN, 1}, conditional_optional}
                    ],
    EPSPDNConnections = [{apn, {?GTPv2_IEI_APN, 0}, mandatory},
                         {apn_restriction, {?GTPv2_IEI_APN_RESTRICTION, 0}, conditional},
                         {selection_mode, {?GTPv2_IEI_SELECTION_MODE, 0}, conditional_optional},
                         {ipv4_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
                         {ipv6_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
                         {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                         {pgw_s5s8_ip_address_for_control_plane_or_pmip, {?GTPv2_IEI_F_TEID, 0}, mandatory},
                         {pgw_node_name, {?GTPv2_IEI_FQDN, 0}, conditional_optional},
                         {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, mandatory, BearerContext},
                         {apn_ambr, {?GTPv2_IEI_AMBR, 0}, mandatory},
                         {charging_characteristics, {?GTPv2_IEI_CHARGING_CHARACTERISTICS, 0}, conditional},
                         {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
                         {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
                         {henb_information_reporting, {?GTPv2_IEI_HENB_INFORMATION_REPORTING, 0}, conditional_optional},
                         {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
                         {signalling_priority_indication, {?GTPv2_IEI_SIGNALLING_PRIORITY_INDICATION, 0}, conditional_optional},
                         {change_to_report_flags, {?GTPv2_IEI_CHANGE_TO_REPORT_FLAGS, 0}, conditional_optional},
                         {local_home_network_id, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
                         {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
                         {wlan_offloadability_indication, {?GTPv2_IEI_WLAN_OFFLOADABILITY_INDICATION, 0}, conditional_optional},
                         {remote_ue_context_connected, {?GTPv2_IEI_REMOTE_UE_CONTEXT, 0}, conditional_optional, RemoteUEContextConnected},
                         {pdn_type, {?GTPv2_IEI_PDN_TYPE, 0}, conditional_optional},
                         {header_compression_configuration, {?GTPv2_IEI_HEADER_COMPRESSION_CONFIGURATION, 0}, conditional_optional},
                         {pgw_change_info, {?GTPv2_IEI_PGW_CHANGE_INFO, 0}, conditional_optional, PGWChangeInfo},
                         {up_security_policy, {?GTPv2_IEI_UP_SECURITY_POLICY, 0}, conditional_optional}
                        ],
    UESCEFPDNConnections = [{apn, {?GTPv2_IEI_APN, 0}, mandatory},
                            {default_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                            {scef_id, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, mandatory}
                           ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mmesgsnamf_ue_mm_context, {?GTPv2_IEI_MM_CONTEXTS, 0}, conditional},
              {mmesgsnamf_ue_eps_pdn_connections, {?GTPv2_IEI_PDN_CONNECTION, 0}, conditional, EPSPDNConnections},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {sgw_s11s4_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {sgw_node_name, {?GTPv2_IEI_FQDN, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {trace_information, {?GTPv2_IEI_TRACE_INFORMATION, 0}, conditional},
              {hrpd_access_node_s101_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {iws_1x_s102_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, conditional},
              {subscribed_rfsp_index, {?GTPv2_IEI_RFSP_INDEX, 0}, conditional_optional},
              {rfsp_index_in_use, {?GTPv2_IEI_RFSP_INDEX, 1}, conditional_optional},
              {ue_time_zone, {?GTPv2_IEI_UE_TIME_ZONE, 0}, conditional_optional},
              {mmes4_sgsn_ldn, {?GTPv2_IEI_LOCAL_DISTIGUISHED_NAME, 0}, optional},
              {mdt_configuration, {?GTPv2_IEI_MDT_CONFIGURATION, 0}, conditional_optional},
              {sgsn_node_name, {?GTPv2_IEI_FQDN, 1}, conditional_optional},
              {mme_node_name, {?GTPv2_IEI_FQDN, 2}, conditional_optional},
              {user_csg_information, {?GTPv2_IEI_USER_CSG_INFORMATION, 0}, conditional_optional},
              {monitoring_event_information, {?GTPv2_IEI_MONITORING_EVENT_INFORMATION, 0}, conditional_optional},
              {monitoring_event_extension_information, {?GTPv2_IEI_MONITORING_EVENT_EXTENSION_INFORMATION, 0}, conditional_optional},
              {ue_usage_type, {?GTPv2_IEI_INTEGER_NUMBER, 0}, conditional_optional},
              {mmesgsn_ue_scef_pdn_connections, {?GTPv2_IEI_SCEF_PDN_CONNECTION, 0}, conditional, UESCEFPDNConnections},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, conditional_optional},
              {serving_plmn_rate_control, {?GTPv2_IEI_SERVING_PLMN_RATE_CONTROL, 0}, conditional_optional},
              {mo_exception_data_counter, {?GTPv2_IEI_COUNTER, 0}, conditional_optional},
              {remaining_running_service_gap_timer, {?GTPv2_IEI_INTEGER_NUMBER, 1}, conditional_optional},
              {extended_trace_information, {?GTPv2_IEI_EXTENDED_TRACE_INFORMATION, 0}, conditional},
              {subscribed_additional_rrm_policy_index, {?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX, 0}, conditional_optional},
              {additional_rrm_policy_index_in_use, {?GTPv2_IEI_ADDITIONAL_RRM_POLICY_INDEX, 1}, conditional_optional},
              {iwk_scef_id_for_monitoring_event, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, conditional_optional},
              {alternative_imsi, {?GTPv2_IEI_ALTERNATIVE_IMSI, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(context_acknowledge, Bin0) ->
    BearerContext = [{eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, mandatory},
                     {forwarding_f_teid, {?GTPv2_IEI_F_TEID, 0}, mandatory}
                    ],
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional},
              {forwarding_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {bearer_contexts, {?GTPv2_IEI_BEARER_CONTEXT, 0}, conditional_optional, BearerContext},
              {sgsn_number, {?GTPv2_IEI_NODE_NUMBER, 0}, conditional_optional},
              {mme_number_for_mt_sms, {?GTPv2_IEI_NODE_NUMBER, 1}, conditional_optional},
              {sgsn_identifier_for_mt_sms, {?GTPv2_IEI_NODE_IDENTIFIER, 0}, conditional_optional},
              {mme_identifier_for_mt_sms, {?GTPv2_IEI_NODE_IDENTIFIER, 1}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(identification_request, Bin0) ->
    Fields = [{guti, {?GTPv2_IEI_GUTI, 0}, conditional},
              {rai, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {p_tmsi, {?GTPv2_IEI_P_TMSI, 0}, conditional},
              {p_tmsi_signature, {?GTPv2_IEI_P_TMSI_SIGNATURE, 0}, conditional},
              {complete_attach_request_message, {?GTPv2_IEI_COMPLETE_REQUEST_MESSAGE, 0}, conditional},
              {address, {?GTPv2_IEI_IP_ADDRESS, 0}, optional},
              {udp_source_port_number, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional},
              {hop_counter, {?GTPv2_IEI_HOP_COUNTER, 0}, optional},
              {target_plmn_id, {?GTPv2_IEI_SERVING_NETWORK, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(identification_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mmesgsn_ue_mm_context, {?GTPv2_IEI_MM_CONTEXTS, 0}, conditional},
              {trace_information, {?GTPv2_IEI_TRACE_INFORMATION, 0}, conditional_optional},
              {ue_usage_type, {?GTPv2_IEI_INTEGER_NUMBER, 0}, conditional_optional},
              {monitoring_event_information, {?GTPv2_IEI_MONITORING_EVENT_INFORMATION, 0}, conditional_optional},
              {monitoring_event_extension_information, {?GTPv2_IEI_MONITORING_EVENT_EXTENSION_INFORMATION, 0}, conditional_optional},
              {extended_trace_information, {?GTPv2_IEI_EXTENDED_TRACE_INFORMATION, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(forward_access_context_notification, Bin0) ->
    Fields = [{rab_contexts, {?GTPv2_IEI_RAB_CONTEXT, 0}, conditional},
              {source_rnc_pdcp_context_info, {?GTPv2_IEI_SOURCE_RNC_PDCP_CONTEXT_INFO, 0}, conditional},
              {pdu_numbers, {?GTPv2_IEI_PDU_NUMBERS, 0}, conditional},
              {e_utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 0}, conditional},
              {e_utran_transparent_container, {?GTPv2_IEI_F_CONTAINER, 1}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(forward_access_context_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(detach_notification, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {detach_type, {?GTPv2_IEI_DETACH_TYPE, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(detach_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(change_notification_request, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {rat_type, {?GTPv2_IEI_RAT_TYPE, 0}, mandatory},
              {uli, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {user_csg_information, {?GTPv2_IEI_USER_CSG_INFORMATION, 0}, conditional_optional},
              {pgw_s5s8_gtp_c_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {lbi, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional_optional},
              {presence_reporting_area_information, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_INFORMATION, 0}, conditional_optional},
              {mo_exception_data_counter, {?GTPv2_IEI_COUNTER, 0}, conditional_optional},
              {secondary_rat_usage_data_report, {?GTPv2_IEI_SECONDARY_RAT_USAGE_DATA_REPORT, 0}, conditional_optional},
              {pscell_id, {?GTPv2_IEI_PSCELL_ID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(change_notification_response, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {change_reporting_action, {?GTPv2_IEI_CHANGE_REPORTING_ACTION, 0}, conditional},
              {csg_information_reporting_action, {?GTPv2_IEI_CSG_INFORMATION_REPORTING_ACTION, 0}, conditional_optional},
              {presence_reporting_area_action, {?GTPv2_IEI_PRESENCE_REPORTING_AREA_ACTION, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(relocation_cancel_request, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional},
              {indication_flags, {?GTPv2_IEI_INDICATION, 0}, conditional_optional},
              {ranap_cause, {?GTPv2_IEI_F_CAUSE, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(relocation_cancel_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(configuration_transfer_tunnel, Bin0) ->
    Fields = [{son_container, {?GTPv2_IEI_F_CONTAINER, 0}, mandatory},
              {target_node_id, {?GTPv2_IEI_TARGET_IDENTIFICATION, 0}, mandatory},
              {connected_target_enodeb_id, {?GTPv2_IEI_TARGET_IDENTIFICATION, 1}, conditional_optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(ran_information_relay, Bin0) ->
    Fields = [{bss_container, {?GTPv2_IEI_F_CONTAINER, 0}, mandatory},
              {rim_routing_address, {?GTPv2_IEI_TARGET_IDENTIFICATION, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(isr_status_indication, Bin0) ->
    Fields = [{action_indication, {?GTPv2_IEI_ACTION_INDICATION, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(ue_registration_query_request, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(ue_registration_query_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {imsi, {?GTPv2_IEI_IMSI, 0}, mandatory},
              {selected_core_network_operator_identifier, {?GTPv2_IEI_PLMN_ID, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mobility_management
        };
decode_msg(suspend_notification, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {rai, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, conditional},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional_optional},
              {p_tmsi, {?GTPv2_IEI_P_TMSI, 0}, conditional},
              {originating_node, {?GTPv2_IEI_NODE_TYPE, 0}, conditional_optional},
              {address, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional_optional},
              {udp_source_port_number, {?GTPv2_IEI_PORT_NUMBER, 0}, conditional_optional},
              {hop_counter, {?GTPv2_IEI_HOP_COUNTER, 0}, optional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(suspend_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(resume_notification, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, mandatory},
              {linked_eps_bearer_id, {?GTPv2_IEI_EPS_BEARER_ID, 0}, conditional_optional},
              {originating_node, {?GTPv2_IEI_NODE_TYPE, 0}, conditional_optional},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(resume_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(cs_paging_indication, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, mandatory},
              {vlr_name, {?GTPv2_IEI_FQDN, 0}, mandatory},
              {tmsi, {?GTPv2_IEI_TMSI, 0}, optional},
              {location_area_identifier, {?GTPv2_IEI_USER_LOCATION_INFORMATION, 0}, optional},
              {global_cn_id, {?GTPv2_IEI_GLOBAL_CN_ID, 0}, optional},
              {channel_needed, {?GTPv2_IEI_CHANNEL_NEEDED, 0}, optional},
              {emlpp_priority, {?GTPv2_IEI_EMLPP_PRIORITY, 0}, optional},
              {service_indicator, {?GTPv2_IEI_SERVICE_INDICATOR, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(alert_mme_notification, Bin0) ->
    Fields = [{private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(alert_mme_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(ue_activity_notification, Bin0) ->
    Fields = [{private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(ue_activity_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => cs_fallback_and_srvcc_related
        };
decode_msg(create_forwarding_tunnel_request, Bin0) ->
    Fields = [{s103_pdn_data_forwarding_info, {?GTPv2_IEI_S103_PDN_DATA_FORWARDING_INFO, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => non_3gpp_access_related
        };
decode_msg(create_forwarding_tunnel_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {s1_u_data_forwarding_info, {?GTPv2_IEI_S1_U_DATA_FORWARDING_INFO, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => non_3gpp_access_related
        };
decode_msg(delete_pdn_connection_set_request, Bin0) ->
    Fields = [{mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 2}, conditional},
              {epdg_fq_csid, {?GTPv2_IEI_FQ_CSID, 3}, conditional},
              {twan_fq_csid, {?GTPv2_IEI_FQ_CSID, 4}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(delete_pdn_connection_set_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(update_pdn_connection_set_request, Bin0) ->
    Fields = [{mme_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {sgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 1}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(update_pdn_connection_set_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {pgw_fq_csid, {?GTPv2_IEI_FQ_CSID, 0}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(pgw_restart_notification, Bin0) ->
    Fields = [{pgw_s5s8_ip_address, {?GTPv2_IEI_IP_ADDRESS, 0}, mandatory},
              {sgw_s11s4_ip_address, {?GTPv2_IEI_IP_ADDRESS, 1}, mandatory},
              {cause, {?GTPv2_IEI_CAUSE, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(pgw_restart_notification_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(pgw_downlink_triggering_notification, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, mandatory},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {pgw_s5_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(pgw_downlink_triggering_acknowledge, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {mmes4_sgsn_identifier, {?GTPv2_IEI_IP_ADDRESS, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => restoration_and_recovery
        };
decode_msg(trace_session_activation, Bin0) ->
    Fields = [{imsi, {?GTPv2_IEI_IMSI, 0}, conditional},
              {trace_information, {?GTPv2_IEI_TRACE_INFORMATION, 0}, mandatory},
              {mei, {?GTPv2_IEI_MEI, 0}, conditional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => trace_management
        };
decode_msg(trace_session_deactivation, Bin0) ->
    Fields = [{trace_reference, {?GTPv2_IEI_TRACE_REFERENCE, 0}, mandatory}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => trace_management
        };
decode_msg(mbms_session_start_request, Bin0) ->
    Fields = [{sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, mandatory},
              {tmgi, {?GTPv2_IEI_TEMPORARY_MOBILE_GROUP_IDENTITY, 0}, mandatory},
              {mbms_session_duration, {?GTPv2_IEI_MBMS_SESSION_DURATION, 0}, mandatory},
              {mbms_service_area, {?GTPv2_IEI_MBMS_SERVICE_AREA, 0}, mandatory},
              {mbms_session_identifier, {?GTPv2_IEI_MBMS_SESSION_IDENTIFIER, 0}, conditional},
              {mbms_flow_identifier, {?GTPv2_IEI_MBMS_FLOW_IDENTIFIER, 0}, conditional},
              {qos_profile, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
              {mbms_ip_multicast_distribution, {?GTPv2_IEI_MBMS_IP_MULTICAST_DISTRIBUTION, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {mbms_time_to_data_transfer, {?GTPv2_IEI_MBMS_TIME_TO_DATA_TRANSFER, 0}, conditional_optional},
              {mbms_data_transfer, {?GTPv2_IEI_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER, 0}, conditional_optional},
              {mbms_flags, {?GTPv2_IEI_MBMS_FLAGS, 0}, conditional_optional},
              {mbms_alternative_ip_multicast_distribution, {?GTPv2_IEI_MBMS_IP_MULTICAST_DISTRIBUTION, 1}, conditional_optional},
              {mbms_cell_list, {?GTPv2_IEI_ECGI_LIST, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        };
decode_msg(mbms_session_start_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, mandatory},
              {mbms_distribution_acknowledge, {?GTPv2_IEI_MBMS_DISTRIBUTION_ACKNOWLEDGE, 0}, conditional},
              {sn_u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 1}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        };
decode_msg(mbms_session_update_request, Bin0) ->
    Fields = [{mbms_service_area, {?GTPv2_IEI_MBMS_SERVICE_AREA, 0}, conditional},
              {tmgi, {?GTPv2_IEI_TEMPORARY_MOBILE_GROUP_IDENTITY, 0}, mandatory},
              {sender_f_teid, {?GTPv2_IEI_F_TEID, 0}, optional},
              {mbms_session_duration, {?GTPv2_IEI_MBMS_SESSION_DURATION, 0}, mandatory},
              {qos_profile, {?GTPv2_IEI_BEARER_QOS, 0}, mandatory},
              {mbms_session_identifier, {?GTPv2_IEI_MBMS_SESSION_IDENTIFIER, 0}, conditional},
              {mbms_flow_identifier, {?GTPv2_IEI_MBMS_FLOW_IDENTIFIER, 0}, conditional},
              {mbms_time_to_data_transfer, {?GTPv2_IEI_MBMS_TIME_TO_DATA_TRANSFER, 0}, conditional_optional},
              {mbms_data_transfer, {?GTPv2_IEI_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER, 0}, conditional_optional},
              {mbms_cell_list, {?GTPv2_IEI_ECGI_LIST, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        };
decode_msg(mbms_session_update_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {mbms_distribution_acknowledge, {?GTPv2_IEI_MBMS_DISTRIBUTION_ACKNOWLEDGE, 0}, conditional},
              {sn_u_sgsn_f_teid, {?GTPv2_IEI_F_TEID, 0}, conditional},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        };
decode_msg(mbms_session_stop_request, Bin0) ->
    Fields = [{mbms_flow_identifier, {?GTPv2_IEI_MBMS_FLOW_IDENTIFIER, 0}, conditional},
              {mbms_data_transfer, {?GTPv2_IEI_ABSOLUTE_TIME_OF_MBMS_DATA_TRANSFER, 0}, conditional_optional},
              {mbms_flags, {?GTPv2_IEI_MBMS_FLAGS, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        };
decode_msg(mbms_session_stop_response, Bin0) ->
    Fields = [{cause, {?GTPv2_IEI_CAUSE, 0}, mandatory},
              {recovery, {?GTPv2_IEI_RECOVERY_RESTART_COUNTER, 0}, conditional_optional},
              {private_extension, {?GTPv2_IEI_PRIVATE_EXTENSION, vs}, optional}],
    {Msg, _Unknown} = decode_tliv_list(Bin0, Fields),
    Msg#{message_group => mbms
        }.

tcbd_decode(<<>>) ->
    [];
tcbd_decode(<<2#1111:4, A:4>>) ->
    [tbcd_decode_num(A)];
tcbd_decode(<<B:4, A:4, Rest/binary>>) ->
    [tbcd_decode_num(A), tbcd_decode_num(B) | tcbd_decode(Rest)].

tbcd_decode_num(2#0000) -> $0;
tbcd_decode_num(2#0001) -> $1;
tbcd_decode_num(2#0010) -> $2;
tbcd_decode_num(2#0011) -> $3;
tbcd_decode_num(2#0100) -> $4;
tbcd_decode_num(2#0101) -> $5;
tbcd_decode_num(2#0110) -> $6;
tbcd_decode_num(2#0111) -> $7;
tbcd_decode_num(2#1000) -> $8;
tbcd_decode_num(2#1001) -> $9;
tbcd_decode_num(2#1010) -> $A;
tbcd_decode_num(2#1011) -> $B;
tbcd_decode_num(2#1100) -> $C;
tbcd_decode_num(2#1101) -> $D;
tbcd_decode_num(2#1110) -> $E;
tbcd_decode_num(2#1111) -> $F.

encode_msg(_, _Msg) ->
    <<>>.
