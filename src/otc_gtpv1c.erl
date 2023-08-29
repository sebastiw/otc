-module(otc_gtpv1c).
-behaviour(otc_codec).

-include("include/gtpv1c.hrl").
-include("include/l3.hrl").

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

spec() ->
    "3GPP TS 29.060 v17.3.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map);
codec({Map, <<>>}) ->
    encode(Map).

next(_) ->
    '$stop'.

decode(<<1:3, 1:1, _:1, E:1, S:1, PN:1, MT:8, Len:16, TEID:32, GTP0:Len/binary>>) ->
    MessageType = parse_message_type(MT),
    {MsgFields, GTP1} = decode_msg_fields(E, S, PN, GTP0),
    IEIs = decode_ieis(GTP1),
    Msg0 = decode_msg(MessageType, IEIs),
    Msg1 = maps:merge(Msg0, MsgFields),
    Msg2 = Msg1#{message_type => MessageType,
                 teid => TEID},
    Msg2.

encode(Map) ->
    #{message_type := MessageType,
      teid := TEID} = Map,
    MT = compose_message_type(MessageType),
    {[E,S,PN], MsgFields} = encode_msg_fields(Map),
    MsgBin = encode_msg(MessageType, Map),
    Len = byte_size(MsgBin),
    <<1:3, 1:1, 0:1, E:1, S:1, PN:1, MT:8, Len:16, TEID:32, MsgFields/binary, MsgBin/binary>>.

parse_message_type(?GTPv1C_MSG_TYPE_ECHO_REQUEST) ->
    echo_request;
parse_message_type(?GTPv1C_MSG_TYPE_ECHO_RESPONSE) ->
    echo_response;
parse_message_type(?GTPv1C_MSG_TYPE_VERSION_NOT_SUPPORTED) ->
    version_not_supported;
parse_message_type(?GTPv1C_MSG_TYPE_NODE_ALIVE_REQUEST) ->
    node_alive_request;
parse_message_type(?GTPv1C_MSG_TYPE_NODE_ALIVE_RESPONSE) ->
    node_alive_response;
parse_message_type(?GTPv1C_MSG_TYPE_REDIRECTION_REQUEST) ->
    redirection_request;
parse_message_type(?GTPv1C_MSG_TYPE_REDIRECTION_RESPONSE) ->
    redirection_response;
parse_message_type(?GTPv1C_MSG_TYPE_CREATE_PDP_CONTEXT_REQUEST) ->
    create_pdp_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_CREATE_PDP_CONTEXT_RESPONSE) ->
    create_pdp_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_UPDATE_PDP_CONTEXT_REQUEST) ->
    update_pdp_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_UPDATE_PDP_CONTEXT_RESPONSE) ->
    update_pdp_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_DELETE_PDP_CONTEXT_REQUEST) ->
    delete_pdp_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_DELETE_PDP_CONTEXT_RESPONSE) ->
    delete_pdp_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_INITIATE_PDP_CONTEXT_ACTIVATION_REQUEST) ->
    initiate_pdp_context_activation_request;
parse_message_type(?GTPv1C_MSG_TYPE_INITIATE_PDP_CONTEXT_ACTIVATION_RESPONSE) ->
    initiate_pdp_context_activation_response;
parse_message_type(?GTPv1C_MSG_TYPE_ERROR_INDICATION) ->
    error_indication;
parse_message_type(?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REQUEST) ->
    pdu_notification_request;
parse_message_type(?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_RESPONSE) ->
    pdu_notification_response;
parse_message_type(?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REJECT_REQUEST) ->
    pdu_notification_reject_request;
parse_message_type(?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REJECT_RESPONSE) ->
    pdu_notification_reject_response;
parse_message_type(?GTPv1C_MSG_TYPE_SUPPORTED_EXTENSION_HEADERS_NOTIFICATION) ->
    supported_extension_headers_notification;
parse_message_type(?GTPv1C_MSG_TYPE_SEND_ROUTEING_INFORMATION_FOR_GPRS_REQUEST) ->
    send_routeing_information_for_gprs_request;
parse_message_type(?GTPv1C_MSG_TYPE_SEND_ROUTEING_INFORMATION_FOR_GPRS_RESPONSE) ->
    send_routeing_information_for_gprs_response;
parse_message_type(?GTPv1C_MSG_TYPE_FAILURE_REPORT_REQUEST) ->
    failure_report_request;
parse_message_type(?GTPv1C_MSG_TYPE_FAILURE_REPORT_RESPONSE) ->
    failure_report_response;
parse_message_type(?GTPv1C_MSG_TYPE_NOTE_MS_GPRS_PRESENT_REQUEST) ->
    note_ms_gprs_present_request;
parse_message_type(?GTPv1C_MSG_TYPE_NOTE_MS_GPRS_PRESENT_RESPONSE) ->
    note_ms_gprs_present_response;
parse_message_type(?GTPv1C_MSG_TYPE_IDENTIFICATION_REQUEST) ->
    identification_request;
parse_message_type(?GTPv1C_MSG_TYPE_IDENTIFICATION_RESPONSE) ->
    identification_response;
parse_message_type(?GTPv1C_MSG_TYPE_SGSN_CONTEXT_REQUEST) ->
    sgsn_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_SGSN_CONTEXT_RESPONSE) ->
    sgsn_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_SGSN_CONTEXT_ACKNOWLEDGE) ->
    sgsn_context_acknowledge;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_REQUEST) ->
    forward_relocation_request;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_RESPONSE) ->
    forward_relocation_response;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_COMPLETE) ->
    forward_relocation_complete;
parse_message_type(?GTPv1C_MSG_TYPE_RELOCATION_CANCEL_REQUEST) ->
    relocation_cancel_request;
parse_message_type(?GTPv1C_MSG_TYPE_RELOCATION_CANCEL_RESPONSE) ->
    relocation_cancel_response;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_SRNS_CONTEXT) ->
    forward_srns_context;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_ACKNOWLEDGE) ->
    forward_relocation_complete_acknowledge;
parse_message_type(?GTPv1C_MSG_TYPE_FORWARD_SRNS_CONTEXT_ACKNOWLEDGE) ->
    forward_srns_context_acknowledge;
parse_message_type(?GTPv1C_MSG_TYPE_UE_REGISTRATION_QUERY_REQUEST) ->
    ue_registration_query_request;
parse_message_type(?GTPv1C_MSG_TYPE_UE_REGISTRATION_QUERY_RESPONSE) ->
    ue_registration_query_response;
parse_message_type(?GTPv1C_MSG_TYPE_RAN_INFORMATION_RELAY) ->
    ran_information_relay;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REQUEST) ->
    mbms_notification_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_RESPONSE) ->
    mbms_notification_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REJECT_REQUEST) ->
    mbms_notification_reject_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REJECT_RESPONSE) ->
    mbms_notification_reject_response;
parse_message_type(?GTPv1C_MSG_TYPE_CREATE_MBMS_CONTEXT_REQUEST) ->
    create_mbms_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_CREATE_MBMS_CONTEXT_RESPONSE) ->
    create_mbms_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_UPDATE_MBMS_CONTEXT_REQUEST) ->
    update_mbms_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_UPDATE_MBMS_CONTEXT_RESPONSE) ->
    update_mbms_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_DELETE_MBMS_CONTEXT_REQUEST) ->
    delete_mbms_context_request;
parse_message_type(?GTPv1C_MSG_TYPE_DELETE_MBMS_CONTEXT_RESPONSE) ->
    delete_mbms_context_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_REGISTRATION_REQUEST) ->
    mbms_registration_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_REGISTRATION_RESPONSE) ->
    mbms_registration_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_DE_REGISTRATION_REQUEST) ->
    mbms_de_registration_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_DE_REGISTRATION_RESPONSE) ->
    mbms_de_registration_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_START_REQUEST) ->
    mbms_session_start_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_START_RESPONSE) ->
    mbms_session_start_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_STOP_REQUEST) ->
    mbms_session_stop_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_STOP_RESPONSE) ->
    mbms_session_stop_response;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_UPDATE_REQUEST) ->
    mbms_session_update_request;
parse_message_type(?GTPv1C_MSG_TYPE_MBMS_SESSION_UPDATE_RESPONSE) ->
    mbms_session_update_response;
parse_message_type(?GTPv1C_MSG_TYPE_MS_INFO_CHANGE_NOTIFICATION_REQUEST) ->
    ms_info_change_notification_request;
parse_message_type(?GTPv1C_MSG_TYPE_MS_INFO_CHANGE_NOTIFICATION_RESPONSE) ->
    ms_info_change_notification_response;
parse_message_type(?GTPv1C_MSG_TYPE_DATA_RECORD_TRANSFER_REQUEST) ->
    data_record_transfer_request;
parse_message_type(?GTPv1C_MSG_TYPE_DATA_RECORD_TRANSFER_RESPONSE) ->
    data_record_transfer_response;
parse_message_type(?GTPv1C_MSG_TYPE_TUNNEL_STATUS) ->
    tunnel_status;
parse_message_type(?GTPv1C_MSG_TYPE_END_MARKER) ->
    end_marker;
parse_message_type(?GTPv1C_MSG_TYPE_G_PDU) ->
    g_pdu.

compose_message_type(echo_request) ->
    ?GTPv1C_MSG_TYPE_ECHO_REQUEST;
compose_message_type(echo_response) ->
    ?GTPv1C_MSG_TYPE_ECHO_RESPONSE;
compose_message_type(version_not_supported) ->
    ?GTPv1C_MSG_TYPE_VERSION_NOT_SUPPORTED;
compose_message_type(node_alive_request) ->
    ?GTPv1C_MSG_TYPE_NODE_ALIVE_REQUEST;
compose_message_type(node_alive_response) ->
    ?GTPv1C_MSG_TYPE_NODE_ALIVE_RESPONSE;
compose_message_type(redirection_request) ->
    ?GTPv1C_MSG_TYPE_REDIRECTION_REQUEST;
compose_message_type(redirection_response) ->
    ?GTPv1C_MSG_TYPE_REDIRECTION_RESPONSE;
compose_message_type(create_pdp_context_request) ->
    ?GTPv1C_MSG_TYPE_CREATE_PDP_CONTEXT_REQUEST;
compose_message_type(create_pdp_context_response) ->
    ?GTPv1C_MSG_TYPE_CREATE_PDP_CONTEXT_RESPONSE;
compose_message_type(update_pdp_context_request) ->
    ?GTPv1C_MSG_TYPE_UPDATE_PDP_CONTEXT_REQUEST;
compose_message_type(update_pdp_context_response) ->
    ?GTPv1C_MSG_TYPE_UPDATE_PDP_CONTEXT_RESPONSE;
compose_message_type(delete_pdp_context_request) ->
    ?GTPv1C_MSG_TYPE_DELETE_PDP_CONTEXT_REQUEST;
compose_message_type(delete_pdp_context_response) ->
    ?GTPv1C_MSG_TYPE_DELETE_PDP_CONTEXT_RESPONSE;
compose_message_type(initiate_pdp_context_activation_request) ->
    ?GTPv1C_MSG_TYPE_INITIATE_PDP_CONTEXT_ACTIVATION_REQUEST;
compose_message_type(initiate_pdp_context_activation_response) ->
    ?GTPv1C_MSG_TYPE_INITIATE_PDP_CONTEXT_ACTIVATION_RESPONSE;
compose_message_type(error_indication) ->
    ?GTPv1C_MSG_TYPE_ERROR_INDICATION;
compose_message_type(pdu_notification_request) ->
    ?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REQUEST;
compose_message_type(pdu_notification_response) ->
    ?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_RESPONSE;
compose_message_type(pdu_notification_reject_request) ->
    ?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REJECT_REQUEST;
compose_message_type(pdu_notification_reject_response) ->
    ?GTPv1C_MSG_TYPE_PDU_NOTIFICATION_REJECT_RESPONSE;
compose_message_type(supported_extension_headers_notification) ->
    ?GTPv1C_MSG_TYPE_SUPPORTED_EXTENSION_HEADERS_NOTIFICATION;
compose_message_type(send_routeing_information_for_gprs_request) ->
    ?GTPv1C_MSG_TYPE_SEND_ROUTEING_INFORMATION_FOR_GPRS_REQUEST;
compose_message_type(send_routeing_information_for_gprs_response) ->
    ?GTPv1C_MSG_TYPE_SEND_ROUTEING_INFORMATION_FOR_GPRS_RESPONSE;
compose_message_type(failure_report_request) ->
    ?GTPv1C_MSG_TYPE_FAILURE_REPORT_REQUEST;
compose_message_type(failure_report_response) ->
    ?GTPv1C_MSG_TYPE_FAILURE_REPORT_RESPONSE;
compose_message_type(note_ms_gprs_present_request) ->
    ?GTPv1C_MSG_TYPE_NOTE_MS_GPRS_PRESENT_REQUEST;
compose_message_type(note_ms_gprs_present_response) ->
    ?GTPv1C_MSG_TYPE_NOTE_MS_GPRS_PRESENT_RESPONSE;
compose_message_type(identification_request) ->
    ?GTPv1C_MSG_TYPE_IDENTIFICATION_REQUEST;
compose_message_type(identification_response) ->
    ?GTPv1C_MSG_TYPE_IDENTIFICATION_RESPONSE;
compose_message_type(sgsn_context_request) ->
    ?GTPv1C_MSG_TYPE_SGSN_CONTEXT_REQUEST;
compose_message_type(sgsn_context_response) ->
    ?GTPv1C_MSG_TYPE_SGSN_CONTEXT_RESPONSE;
compose_message_type(sgsn_context_acknowledge) ->
    ?GTPv1C_MSG_TYPE_SGSN_CONTEXT_ACKNOWLEDGE;
compose_message_type(forward_relocation_request) ->
    ?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_REQUEST;
compose_message_type(forward_relocation_response) ->
    ?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_RESPONSE;
compose_message_type(forward_relocation_complete) ->
    ?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_COMPLETE;
compose_message_type(relocation_cancel_request) ->
    ?GTPv1C_MSG_TYPE_RELOCATION_CANCEL_REQUEST;
compose_message_type(relocation_cancel_response) ->
    ?GTPv1C_MSG_TYPE_RELOCATION_CANCEL_RESPONSE;
compose_message_type(forward_srns_context) ->
    ?GTPv1C_MSG_TYPE_FORWARD_SRNS_CONTEXT;
compose_message_type(forward_relocation_complete_acknowledge) ->
    ?GTPv1C_MSG_TYPE_FORWARD_RELOCATION_COMPLETE_ACKNOWLEDGE;
compose_message_type(forward_srns_context_acknowledge) ->
    ?GTPv1C_MSG_TYPE_FORWARD_SRNS_CONTEXT_ACKNOWLEDGE;
compose_message_type(ue_registration_query_request) ->
    ?GTPv1C_MSG_TYPE_UE_REGISTRATION_QUERY_REQUEST;
compose_message_type(ue_registration_query_response) ->
    ?GTPv1C_MSG_TYPE_UE_REGISTRATION_QUERY_RESPONSE;
compose_message_type(ran_information_relay) ->
    ?GTPv1C_MSG_TYPE_RAN_INFORMATION_RELAY;
compose_message_type(mbms_notification_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REQUEST;
compose_message_type(mbms_notification_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_RESPONSE;
compose_message_type(mbms_notification_reject_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REJECT_REQUEST;
compose_message_type(mbms_notification_reject_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_NOTIFICATION_REJECT_RESPONSE;
compose_message_type(create_mbms_context_request) ->
    ?GTPv1C_MSG_TYPE_CREATE_MBMS_CONTEXT_REQUEST;
compose_message_type(create_mbms_context_response) ->
    ?GTPv1C_MSG_TYPE_CREATE_MBMS_CONTEXT_RESPONSE;
compose_message_type(update_mbms_context_request) ->
    ?GTPv1C_MSG_TYPE_UPDATE_MBMS_CONTEXT_REQUEST;
compose_message_type(update_mbms_context_response) ->
    ?GTPv1C_MSG_TYPE_UPDATE_MBMS_CONTEXT_RESPONSE;
compose_message_type(delete_mbms_context_request) ->
    ?GTPv1C_MSG_TYPE_DELETE_MBMS_CONTEXT_REQUEST;
compose_message_type(delete_mbms_context_response) ->
    ?GTPv1C_MSG_TYPE_DELETE_MBMS_CONTEXT_RESPONSE;
compose_message_type(mbms_registration_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_REGISTRATION_REQUEST;
compose_message_type(mbms_registration_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_REGISTRATION_RESPONSE;
compose_message_type(mbms_de_registration_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_DE_REGISTRATION_REQUEST;
compose_message_type(mbms_de_registration_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_DE_REGISTRATION_RESPONSE;
compose_message_type(mbms_session_start_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_START_REQUEST;
compose_message_type(mbms_session_start_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_START_RESPONSE;
compose_message_type(mbms_session_stop_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_STOP_REQUEST;
compose_message_type(mbms_session_stop_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_STOP_RESPONSE;
compose_message_type(mbms_session_update_request) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_UPDATE_REQUEST;
compose_message_type(mbms_session_update_response) ->
    ?GTPv1C_MSG_TYPE_MBMS_SESSION_UPDATE_RESPONSE;
compose_message_type(ms_info_change_notification_request) ->
    ?GTPv1C_MSG_TYPE_MS_INFO_CHANGE_NOTIFICATION_REQUEST;
compose_message_type(ms_info_change_notification_response) ->
    ?GTPv1C_MSG_TYPE_MS_INFO_CHANGE_NOTIFICATION_RESPONSE;
compose_message_type(data_record_transfer_request) ->
    ?GTPv1C_MSG_TYPE_DATA_RECORD_TRANSFER_REQUEST;
compose_message_type(data_record_transfer_response) ->
    ?GTPv1C_MSG_TYPE_DATA_RECORD_TRANSFER_RESPONSE;
compose_message_type(tunnel_status) ->
    ?GTPv1C_MSG_TYPE_TUNNEL_STATUS;
compose_message_type(end_marker) ->
    ?GTPv1C_MSG_TYPE_END_MARKER;
compose_message_type(g_pdu) ->
    ?GTPv1C_MSG_TYPE_G_PDU.

decode_msg_fields(E, S, PN, GTP0) ->
    case E+S+PN > 0 of
        true ->
            <<SequenceNumber:16, NPDUNumber:8, Rest0/binary>> = GTP0,
            Fields =
                [{sequence_number, SequenceNumber} || 1 == S] ++
                [{npdu_number, NPDUNumber} || 1 == PN],
            F = maps:from_list(Fields),
            {Extensions, Rest} = parse_extension_headers(E, Rest0),
            {F#{extension_headers => Extensions}, Rest};
        false ->
            {#{}, GTP0}
    end.

encode_msg_fields(Map) ->
    EH = maps:get(extension_headers, Map, undefined),
    SN = maps:get(sequence_number, Map, undefined),
    NN = maps:get(npdu_number, Map, undefined),

    ExtHeaders = case EH of
                     undefined -> <<0:8>>;
                     _ -> compose_extension_headers(EH)
                 end,
    SequenceNum = case SN of
                      undefined -> <<0:16>>;
                      _ -> <<SN:16>>
                  end,
    NPDUNum = case NN of
                  undefined -> <<0:8>>;
                  _ -> <<NN:8>>
              end,

    Indicators = [case I of undefined -> 0; _ -> 1 end || I <- [EH, SN, NN]],
    Bin = case lists:sum(Indicators) of
              0 -> <<>>;
              _ -> <<SequenceNum/binary, NPDUNum/binary, ExtHeaders/binary>>
          end,

    {Indicators, Bin}.

parse_extension_headers(1, Rest0) ->
    parse_next_extension_headers(Rest0, #{});
parse_extension_headers(0, <<_:8, Rest0/binary>>) ->
    {#{}, Rest0}.

parse_next_extension_headers(<<2#0000_0000:8, Rest0/binary>>, Acc) ->
    {Acc, Rest0};
parse_next_extension_headers(<<2#0000_0001:8, Rest0/binary>>, Acc0) ->
    <<1:8, 16#FF:8, 16#FF:8, Rest1/binary>> = Rest0,
    Acc = Acc0#{mbms_support_indication => true},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#0000_0010:8, Rest0/binary>>, Acc0) ->
    <<1:8, 16#FF:8, 16#FF:8, Rest1/binary>> = Rest0,
    Acc = Acc0#{ms_info_change_reporting_support_indication => true},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#0010_0000:8, Rest0/binary>>, Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    <<1:8, ServiceClassIndicator:8, _Spare:8, Rest1/binary>> = Rest0,
    Acc = Acc0#{service_class_indicator => ServiceClassIndicator},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#0100_0000:8, Rest0/binary>>, Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    <<1:8, UDPPort:16, Rest1/binary>> = Rest0,
    Acc = Acc0#{udp_port => UDPPort},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#1000_0001:8, Rest0/binary>>, Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    %% Specified in in 3GPP TS 36.425
    <<Len:8, Rest1/binary>> = Rest0,
    <<RanContainer:(4*Len-1)/binary, Rest2/binary>> = Rest1,
    Acc = Acc0#{ran_container => RanContainer},
    parse_next_extension_headers(Rest2, Acc);
parse_next_extension_headers(<<2#1100_0000:8, Rest0/binary>>, Acc0) ->
    <<1:8, PDCPNumber:16, Rest1/binary>> = Rest0,
    Acc = Acc0#{pdpc_number => PDCPNumber},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#1100_0001:8, Rest0/binary>>, Acc0) ->
    <<1:8, 16#FF:8, 16#FF:8, Rest1/binary>> = Rest0,
    Acc = Acc0#{suspend_request => true},
    parse_next_extension_headers(Rest1, Acc);
parse_next_extension_headers(<<2#1100_0010:8, Rest0/binary>>, Acc0) ->
    <<1:8, 16#FF:8, 16#FF:8, Rest1/binary>> = Rest0,
    Acc = Acc0#{suspend_response => true},
    parse_next_extension_headers(Rest1, Acc).

compose_extension_headers(Map) ->
    compose_next_extension_headers(maps:to_list(Map), <<2#0000_0000:8>>).
compose_next_extension_headers([], Acc) ->
    Acc;
compose_next_extension_headers([{mbms_support_indication, true}|T], Acc0) ->
    Acc = <<2#0000_0001:8, 1:8, 16#FF:8, 16#FF:8, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{ms_info_change_reporting_support_indication, true}|T], Acc0) ->
    Acc = <<2#0000_0010:8, 1:8, 16#FF:8, 16#FF:8, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{service_class_indicator, ServiceClassIndicator}|T], Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    Acc = <<2#0010_0000:8, 1:8, ServiceClassIndicator:8, 0:8, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{udp_port, UDPPort}|T], Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    Acc = <<2#0100_0000:8, 1:8, UDPPort:16, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{ran_container, RanContainer}|T], Acc0) ->
    %% Reserved for GTP-U. See 3GPP TS 29.281 [41].
    %% Specified in in 3GPP TS 36.425
    Len = byte_size(RanContainer) div 4,
    Acc = <<2#1000_0001:8, (Len+1):8, RanContainer/binary, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{pdpc_number, PDCPNumber}|T], Acc0) ->
    Acc = <<2#1100_0000:8, 1:8, PDCPNumber:16, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{suspend_request, true}|T], Acc0) ->
    Acc = <<2#1100_0001:8, 1:8, 16#FF:8, 16#FF:8, Acc0/binary>>,
    compose_next_extension_headers(T, Acc);
compose_next_extension_headers([{suspend_response, true}|T], Acc0) ->
    Acc = <<2#1100_0010:8, 1:8, 16#FF:8, 16#FF:8, Acc0/binary>>,
    compose_next_extension_headers(T, Acc).

decode_ieis(<<>>) ->
    [];
decode_ieis(<<?GTPv1C_IEI_CAUSE:8, Value:1/binary, Rest/binary>>) ->
    <<V:8>> = Value,
    C = parse_iei_cause(V),
    [{cause, C}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_IMSI:8, Value:8/binary, Rest/binary>>) ->
    IMSI = otc_gtpv2c:tbcd_decode(Value),
    [{imsi, IMSI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RAI:8, Value:6/binary, Rest/binary>>) ->
    RAI = otc_gtpv2c:decode_rai(Value),
    [{rai, RAI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TLLI:8, Value:4/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.003
    [{tlli, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_P_TMSI:8, Value:4/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.003
    [{p_tmsi, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_REORDERING_REQUIRED:8, Value:1/binary, Rest/binary>>) ->
    <<_:7, RR:1>> = Value,
    Reord = case RR of
                0 -> false;
                1 -> true
            end,
    [{reordering_required, Reord}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_AUTHENTICATION_TRIPLET:8, Value:28/binary, Rest/binary>>) ->
    <<RAND:16/binary, SRES:4/binary, Kc:8/binary>> = Value,
    AuthTriplet = #{rand => RAND,
                    sres => SRES,
                    kc => Kc},
    [{authentication_triplet, AuthTriplet}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MAP_CAUSE:8, Value:1/binary, Rest/binary>>) ->
    %% MAP-causes in 3GPP TS 29.002
    <<V:8>> = Value,
    [{map_cause, V}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_P_TMSI_SIGNATURE:8, Value:3/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 24.008
    [{p_tmsi_signature, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MS_VALIDATED:8, Value:1/binary, Rest/binary>>) ->
    <<_:7, MV:1>> = Value,
    MsValidated = case MV of
                      0 -> false;
                      1 -> true
                  end,
    [{ms_validated, MsValidated}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RECOVERY:8, Value:1/binary, Rest/binary>>) ->
    <<V:8>> = Value,
    [{recovery, V}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SELECTION_MODE:8, Value:1/binary, Rest/binary>>) ->
    <<_:6, SM:2>> = Value,
    SelectionMode = case SM of
                        0 -> verified;
                        1 -> unverified_ms_provided;
                        _ -> unverified_network_provided
                    end,
    [{selection_mode, SelectionMode}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I:8, Value:4/binary, Rest/binary>>) ->
    [{tunnel_endpoint_identifier_data_i, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE:8, Value:4/binary, Rest/binary>>) ->
    <<TEID:32>> = Value,
    [{tunnel_endpoint_identifier_control_plane, TEID}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_II:8, Value:5/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, DBin/binary>> = Value,
    DATA_II = #{nsapi => NSAPI,
                data => DBin},
    [{tunnel_endpoint_identifier_data_ii, DATA_II}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TEARDOWN_IND:8, Value:1/binary, Rest/binary>>) ->
    <<_:7, TI:1>> = Value,
    TearDownInd = case TI of
                      0 -> false;
                      1 -> true
                  end,
    [{teardown_ind, TearDownInd}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_NSAPI:8, Value:1/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4>> = Value,
    [{nsapi, NSAPI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RANAP_CAUSE:8, Value:1/binary, Rest/binary>>) ->
    %% RANAP-causes defined in 3GPP TS 25.413
    <<V:8>> = Value,
    [{ranap_cause, V}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RAB_CONTEXT:8, Value:9/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, DL_GTPU_SEQ:16, UL_GTPU_SEQ:16, DL_PDCP_SEQ:16, UL_PDCP_SEQ:16>> = Value,
    RABContext = #{nsapi => NSAPI,
                   dl_gtpu_sequence => DL_GTPU_SEQ,
                   ul_gtpu_sequence => UL_GTPU_SEQ,
                   dl_pdcp_sequence => DL_PDCP_SEQ,
                   ul_pdcp_sequence => UL_PDCP_SEQ
                  },
    [{rab_context, RABContext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RADIO_PRIORITY_SMS:8, Value:1/binary, Rest/binary>>) ->
    <<_:5, RPS:3>> = Value,
    [{radio_priority_sms, RPS}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RADIO_PRIORITY:8, Value:1/binary, Rest/binary>>) ->
    <<NSAPI:4, _:1, RP:3>> = Value,
    RadioPrio = #{nsapi => NSAPI,
                  radio_priority => RP},
    [{radio_priority, RadioPrio}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PACKET_FLOW_ID:8, Value:2/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, PFI:8>> = Value,
    PacketFlowId =  #{nsapi => NSAPI,
                      packet_flow_id => PFI},
    [{packet_flow_id, PacketFlowId}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CHARGING_CHARACTERISTICS:8, Value:2/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 32.251 and 3GPP TS 32.298
    [{charging_characteristics, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TRACE_REFERENCE:8, Value:2/binary, Rest/binary>>) ->
    [{trace_reference, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TRACE_TYPE:8, Value:2/binary, Rest/binary>>) ->
    %% Defined in GSM 12.08
    [{trace_type, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MS_NOT_REACHABLE_REASON:8, Value:1/binary, Rest/binary>>) ->
    %% Reason for Absence defined in 3GPP TS 23.040
    <<V:8>> = Value,
    [{ms_not_reachable_reason, V}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CHARGING_ID:8, Value:4/binary, Rest/binary>>) ->
    <<CID:32>> = Value,
    [{charging_id, CID}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_END_USER_ADDRESS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, Org:4, Type:8, Addr/binary>> = Value,
    EndUserAddress = pdp_address(Org, Type, Addr),
    [{end_user_address, EndUserAddress}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MM_CONTEXT:8, L:16, Value:L/binary, Rest/binary>>) ->
    MM = case Value of
             <<_:5, CKSN:3, 1:2, NV:3, C:3, Rest/binary>> ->
                 %% GSM Key and Triplets
                 <<KC:8/binary, Triplets:NV/binary, DRX:2/binary,
                   MSNetCapLen:8, MSNetCap:MSNetCapLen/binary,
                   ContLen:16, Cont:ContLen/binary>> = Rest,
                 #{cipher_key_sequence_number => CKSN,
                   used_cipher => C,
                   kc => KC,
                   triplets => triplets(Triplets),
                   drx_parameter => DRX,
                   ms_network_capability => MSNetCap,
                   container => Cont};
             <<_:5, CKSN:3, 3:2, NV:3, C:3, Rest/binary>> ->
                 %% GSM Keys and UMTS Quintuplets
                 <<KC:8/binary, QLen:16, Quintuplets:QLen/binary,
                   DRX:2/binary,
                   MSNetCapLen:8, MSNetCap:MSNetCapLen/binary,
                   ContLen:16, Cont:ContLen/binary,
                   ARDLen:8, ARD:ARDLen/binary>> = Rest,
                 NRSRNA = case ARD of
                              <<>> ->
                                  undefined;
                              <<_:7, NR:1>> ->
                                  NR =:= 1
                          end,
                 #{cipher_key_sequence_number => CKSN,
                   used_cipher => C,
                   kc => KC,
                   quintuplets => quintuplets(NV, Quintuplets),
                   drx_parameter => DRX,
                   ms_network_capability => MSNetCap,
                   container => Cont,
                   nr_as_secondary_rat_not_allowed => NRSRNA};
             <<GUPII:1, UGIPAI:1, IPA:3, KSI:3, 2:2, NV:3, _:3, Rest/binary>> ->
                 %% UMTS Keys and Quintuplets
                 <<CK:16/binary, IK:16/binary,
                   QLen:16, Quintuplets:QLen/binary,
                   DRX:2/binary,
                   MSNetCapLen:8, MSNetCap:MSNetCapLen/binary,
                   ContLen:16, Cont:ContLen/binary,
                   ARDLen:8, ARD:ARDLen/binary>> = Rest,
                 NRSRNA = case ARD of
                              <<>> ->
                                  undefined;
                              <<_:7, NR:1>> ->
                                  NR =:= 1
                          end,
                 #{gprs_user_plane_integrity_indicator => GUPII,
                   used_gprs_integrity_protection_algorithm_indicator => UGIPAI,
                   used_gprs_integrity_protection_algorithm => IPA,
                   key_set_identifier => KSI,
                   cipher_key => CK,
                   integrity_key => IK,
                   quintuplets => quintuplets(NV, Quintuplets),
                   drx_parameter => DRX,
                   ms_network_capability => MSNetCap,
                   container => Cont,
                   nr_as_secondary_rat_not_allowed => NRSRNA};
             <<GUPII:1, UGIPAI:1, IPA:3, CKSN_KSI:3, 0:2, NV:3, C:3, Rest/binary>> ->
                 %% Used Cipher value, UMTS Keys and Quintuplets
                 <<CK:16/binary, IK:16/binary,
                   QLen:16, Quintuplets:QLen/binary,
                   DRX:2/binary,
                   MSNetCapLen:8, MSNetCap:MSNetCapLen/binary,
                   ContLen:16, Cont:ContLen/binary,
                   ARDLen:8, ARD:ARDLen/binary>> = Rest,
                 NRSRNA = case ARD of
                              <<>> ->
                                  undefined;
                              <<_:7, NR:1>> ->
                                  NR =:= 1
                          end,
                 #{gprs_user_plane_integrity_indicator => GUPII,
                   used_gprs_integrity_protection_algorithm_indicator => UGIPAI,
                   used_gprs_integrity_protection_algorithm => IPA,
                   cipher_key_sequence_number => CKSN_KSI,
                   used_cipher => C,
                   cipher_key => CK,
                   integrity_key => IK,
                   quintuplets => quintuplets(NV, Quintuplets),
                   drx_parameter => DRX,
                   ms_network_capability => MSNetCap,
                   container => Cont,
                   nr_as_secondary_rat_not_allowed => NRSRNA}
         end,
    [{mm_context, MM}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PDP_CONTEXT:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<EA:1, VAA:1, ASI:1, Order:1, NSAPI:4, _:4, SAPI:4,
      QoSSubLen:8, QoSSub:QoSSubLen/binary,
      QoSReqLen:8, QoSReq:QoSReqLen/binary,
      QoSNegLen:8, QoSNeg:QoSNegLen/binary,
      SND:16, SNU:16, SNN:8, RNN:8,
      UTEICP:4/binary, UTEIDI:4/binary,
      PDPContId:16, _:4, PTO:4, PTN:8, PALen:8, PAddr:PALen/binary,
      GACPLen:8, GACP:GACPLen/binary,
      GAUTLen:8, GAUT:GAUTLen/binary,
      APNLen:8, APN:APNLen/binary,
      _:4, TI:12,
      E/binary>> = Value,
    PDPAddr = case EA of
                  0 ->
                      pdp_address(PTO, PTN, PAddr);
                  1 ->
                      <<EPTN:8, EPALen:8, EPAddr:EPALen/binary>> = E,
                      P = pdp_address(PTO, PTN, PAddr),
                      E = pdp_address(PTO, EPTN, EPAddr),
                      maps:merge(P, E)
              end,
    PDPContext = #{vplmn_address_allowed => case VAA of
                                                 0 ->
                                                     false;
                                                 1 ->
                                                     true
                                             end,
                   activity_status_indicator => case ASI of
                                                    0 ->
                                                        true;
                                                    1 ->
                                                        false
                                                end,
                   reordering_required => case Order of
                                               0 ->
                                                   false;
                                               1 ->
                                                   true
                                           end,
                   pdp_context_id => PDPContId,
                   nsapi => NSAPI,
                   sapi => SAPI,
                   qos_subscribed => qos(QoSSub),
                   qos_requested => qos(QoSReq),
                   qos_negotiated => qos(QoSNeg),
                   sequence_number_down => SND,
                   sequence_number_up => SNU,
                   send_npdu_number => SNN,
                   receive_npdu_number => RNN,
                   uplink_tunnel_endpoint_identifier_control_plane => UTEICP,
                   uplink_tunnel_endpoint_identifier_data_i => UTEIDI,
                   pdp_address => PDPAddr,
                   ggsn_address_for_control_plane => GACP,
                   ggsn_address_for_user_traffic => GAUT,
                   access_point_name => APN,
                   trace_reference => TI},
    [{pdp_context, PDPContext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ACCESS_POINT_NAME:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.060
    APN = otc_gtpv2c:decode_apn(Value),
    [{access_point_name, APN}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PROTOCOL_CONFIGURATION_OPTIONS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_Ext:1, 0:4, CP:3, R0/binary>> = Value,
    0 = CP,
    PCO = otc_gtpv2c:decode_pco(R0),
    [{protocol_configuration_options, PCO}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_GSN_ADDRESS:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.003
    A = case L of
            4 ->
                #{ipv4 => otc_gtpv2c:bin_to_ip_addr(Value)};
            16 ->
                #{ipv6 => otc_gtpv2c:bin_to_ip_addr(Value)}
        end,
    [{gsn_address, A}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MSISDN:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% The MS international ISDN numbers are allocated from the ITU-T
    %% Recommendation E.164 numbering plan, see 3GPP TS 23.003. The
    %% MSISDN is coded according to the contents of ISDN-AddressString
    %% data type defined in 3GPP TS 29.002. The MSISDN shall be in
    %% international format and the "nature of address indicator"
    %% shall indicate "international number".
    MSISDN = otc_gtpv2c:tbcd_decode(Value),
    [{msisdn, MSISDN}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE:8, L:16, Value:L/binary, Rest/binary>>) ->
    QoS = qos(Value),
    [{quality_of_service_profile, QoS}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_AUTHENTICATION_QUINTUPLET:8, L:16, Value:L/binary, Rest/binary>>) ->
    Quintuplet = quintuplets(1, Value),
    [{authentication_quintuplet, Quintuplet}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TRAFFIC_FLOW_TEMPLATE:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% The TFT value part is described in 3GPP TS 23.060 and defined
    %% in 3GPP TS 24.008
    [{traffic_flow_template, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TARGET_IDENTIFICATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<RAIBin:7/binary, RNCID:2/binary, ERNCID/binary>> = Value,
    RAI = otc_gtpv2c:decode_rai(RAIBin),
    TI = RAI#{rnc_id => RNCID,
              extended_rnc_id => ERNCID},
    [{target_identification, TI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UTRAN_TRANSPARENT_CONTAINER:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{utran_transparent_container, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RAB_SETUP_INFORMATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, R/binary>> = Value,
    RABInfo = case R of
                  <<>> ->
                      #{nsapi => NSAPI};
                  <<DATA:4/binary, RNCIP/binary>> ->
                      #{nsapi => NSAPI,
                        data => DATA,
                        rnc_ip => otc_gtpv2c:bin_to_ip_addr(RNCIP)}
              end,
    [{rab_setup_information, RABInfo}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EXTENSION_HEADER_TYPE_LIST:8, L:16, Value:L/binary, Rest/binary>>) ->
    TypeList = [T || <<T:8>> <= Value],
    [{extension_header_type_list, TypeList}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TRIGGER_ID:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{trigger_id, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_OMC_IDENTITY:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{omc_identity, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RAN_TRANSPARENT_CONTAINER:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{ran_transparent_container, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PDP_CONTEXT_PRIORITIZATION:8, L:16, _Value:L/binary, Rest/binary>>) ->
    [{pdp_context_prioritization, true}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ADDITIONAL_RAB_SETUP_INFORMATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, R/binary>> = Value,
    RABInfo = case R of
                  <<>> ->
                      #{nsapi => NSAPI};
                  <<DATA:4/binary, RNCIP/binary>> ->
                      #{nsapi => NSAPI,
                        data => DATA,
                        rnc_ip => otc_gtpv2c:bin_to_ip_addr(RNCIP)}
              end,
    [{additional_rab_setup_information, RABInfo}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SGSN_NUMBER:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.003
    [{sgsn_number, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_COMMON_FLAGS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<DABF:1, UQS:1, NRSN:1, NQN:1, MCI:1, RPR:1, MST:1, PPC:1, _/binary>> = Value,
    Flags = #{dual_address_bearer_flag => DABF,
              upgrade_qos_supported => UQS,
              nrsn => NRSN,
              no_qos_negotiation => NQN,
              mbms_counting_information => MCI,
              ran_procedures_ready => RPR,
              mbms_service_type => MST,
              prohibit_payload_compression => PPC
             },
    [{common_flags, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_APN_RESTRICTION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<R:8, _/binary>> = Value,
    [{apn_restriction, R}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RADIO_PRIORITY_LCS:8, L:16, Value0:L/binary, Rest/binary>>) when L > 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    <<_:5, RP:3>> = Value,
    [{radio_priority_lcs, RP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RADIO_PRIORITY_LCS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:5, RP:3>> = Value,
    [{radio_priority_lcs, RP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RAT_TYPE:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<RAT:8, _/binary>> = Value,
    RATType = case RAT of
                    1 -> utran;
                    2 -> geran;
                    3 -> wlan;
                    4 -> gan;
                    5 -> hspa_evolution;
                    6 -> eutran;
                    _ -> undefined
                end,
    [{rat_type, RATType}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_USER_LOCATION_INFORMATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<GLType:8, GL/binary>> = Value,
    ULI = case GLType of
              0 ->
                  otc_gtpv2c:decode_cgi(GL);
              1 ->
                  otc_gtpv2c:decode_sai(GL);
              2 ->
                  otc_gtpv2c:decode_rai(GL)
          end,
    [{user_location_information, ULI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MS_TIME_ZONE:8, L:16, Value:L/binary, Rest/binary>>) ->
    TimeZone = otc_gtpv2c:decode_timezone(Value),
    [{ms_time_zone, TimeZone}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_IMEISV:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 8 ->
    <<Value:8/binary, _/binary>> = Value0,
    IMEISV = otc_gtpv2c:decode_imeisv(Value),
    [{imeisv, IMEISV}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CAMEL_CHARGING_INFORMATION_CONTAINER:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% The CAMELInformationPDP IE within an S-CDR is defined in 3GPP TS 32.298
    [{camel_charging_information_container, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_UE_CONTEXT:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<LinkedNSAPI:4, _:4,
      UTEIDCP:32,
      ENSAPI:8,
      _:4, PTO:4, PTN:8, PAddrLen:8, PAddr:PAddrLen/binary,
      GACPLen:8, GACP:GACPLen/binary,
      APNL:8, APN:APNL/binary,
      _:4, TI:12>> = Value,
    UEContext = #{linked_nsapi => LinkedNSAPI,
                  uplink_tunnel_endpoint_identifier_control_plane => UTEIDCP,
                  enhanced_nsapi => ENSAPI,
                  pdp_address => pdp_address(PTO, PTN, PAddr),
                  ggsn_address_for_control_plane => GACP,
                  apn => otc_gtpv2c:decode_apn(APN),
                  transaction_identifier => TI
                 },
    [{mbms_ue_context, UEContext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_TMGI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 6 ->
    <<Value:6/binary, _/binary>> = Value0,
    [{tmgi, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RIM_ROUTING_ADDRESS:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Coded according to 3GPP TS 48.018
    [{rim_routing_address, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_PROTOCOL_CONFIGURATION_OPTIONS:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 24.008
    [{mbms_protocol_configuration_options, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_SERVICE_AREA:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.246
    [{mbms_service_area, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SOURCE_RNC_PDCP_CONTEXT_INFO:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{source_rnc_pdcp_context_info, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ADDITIONAL_TRACE_INFO:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 9 ->
    <<Value:9/binary, _/binary>> = Value0,
    <<TR2:24, TRSR:16, TE:8, TD:8, LI:8, TAC:8>> = Value,
    TraceInfo = #{trace_reference2 => TR2,
                  trace_recording_session_reference => TRSR,
                  triggering_events => TE,
                  trace_depth => TD,
                  list_of_interfaces => LI,
                  trace_activity_control => TAC
                 },
    [{additional_trace_info, TraceInfo}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_HOP_COUNTER:8, L:16, Value0:L/binary, Rest/binary>>) ->
    <<Value:8, _/binary>> = Value0,
    [{hop_counter, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SELECTED_PLMN_ID:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 3 ->
    %% Specified in 3GPP TS 25.413
    <<Value:3/binary>> = Value0,
    PLMNId = otc_gtpv2c:decode_plmn_id(Value),
    [{selected_plmn_id, PLMNId}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_SESSION_IDENTIFIER:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:8, _/binary>> = Value0,
    SessId = case Value of
                 0 -> only_2g;
                 1 -> only_3g;
                 2 -> both_2g_3g
             end,
    [{mbms_session_identifier, SessId}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_2G3G_INDICATOR:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary>> = Value0,
    [{mbms_2g3g_indicator, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_2G3G_INDICATOR:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{mbms_2g3g_indicator, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ENHANCED_NSAPI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    %% Defined in 3GPP TS 24.008
    <<Value:1/binary, _/binary>> = Value0,
    [{enhanced_nsapi, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_SESSION_DURATION:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 3 ->
    %% Defined in 3GPP TS 23.246
    <<Value:3/binary, _/binary>> = Value0,
    [{mbms_session_duration, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ADDITIONAL_MBMS_TRACE_INFO:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 8 ->
    %% Defined in 3GPP TS 32.422
    <<Value:8/binary, _/binary>> = Value0,
    <<TR2:24, TRSR:0, TE:16, TD:8, LI:8, TAC:8>> = Value,
    TraceInfo = #{trace_reference2 => TR2,
                  trace_recording_session_reference => TRSR,
                  triggering_events => TE,
                  trace_depth => TD,
                  list_of_interfaces => LI,
                  trace_activity_control => TAC
                 },
    [{additional_mbms_trace_info, TraceInfo}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_SESSION_REPETITION_NUMBER:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary>> = Value0,
    [{mbms_session_repetition_number, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_TIME_TO_DATA_TRANSFER:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary>> = Value0,
    [{mbms_time_to_data_transfer, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_BSS_CONTAINER:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 48.018
    [{bss_container, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CELL_IDENTIFICATION:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 17 ->
    <<Value:17/binary, _/binary>> = Value0,
    <<TargetCellId:8/binary, SourceType:8, SourceCellId/binary>> = Value,
    SourceKey = case SourceType of
                    0 -> source_cell_id;
                    1 -> source_rnc_id
                end,
    CellId = #{target_cell_id => TargetCellId,
               SourceKey => SourceCellId
              },
    [{cell_identification, CellId}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PDU_NUMBERS:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 9 ->
    <<Value:9/binary, _/binary>> = Value0,
    <<_:4, NSAPI:4,
      SND:16, SNU:16, SNN:16, RNN:16>> = Value,
    PDUNums = #{nsapi => NSAPI,
                sequence_number_down => SND,
                sequence_number_up => SNU,
                send_npdu_number => SNN,
                receive_npdu_number => RNN},
    [{pdu_numbers, PDUNums}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_BSSGP_CAUSE:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    %% Defined in 3GPP TS 48.018
    <<Value:1/binary>> = Value0,
    [{bssgp_cause, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_REQUIRED_MBMS_BEARER_CAPABILITIES:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.246
    [{required_mbms_bearer_capabilities, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RIM_ROUTING_ADDRESS_DISCRIMINATOR:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    %% Coded according to 3GPP TS 48.018
    <<Value:1/binary, _/binary>> = Value0,
    <<_:4, RIM:4>> = Value,
    [{rim_routing_address_discriminator, RIM}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_LIST_OF_SET_UP_PFCS:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 48.018
    [{list_of_set_up_pfcs, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PS_HANDOVER_XID_PARAMETERS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, SAPI:4, XIDLen:8, XID:XIDLen/binary>> = Value,
    XIDPar = #{sapi => SAPI,
               xid_parameters => XID},
    [{ps_handover_xid_parameters, XIDPar}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MS_INFO_CHANGE_REPORTING_ACTION:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:8, _/binary>> = Value0,
    Action = case Value of
                 0 -> stop_reporting;
                 1 -> start_reporting_cgi_sai;
                 2 -> start_reporting_rai
             end,
    [{ms_info_change_reporting_action, Action}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_DIRECT_TUNNEL_FLAGS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:5, EI:1, GCSI:1, DTI:1, _/binary>> = Value,
    Flags = #{error_indication => EI,
              gprs_csi => GCSI,
              direct_tunneling_indication => DTI},
    [{direct_tunnel_flags, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CORRELATION_ID:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    [{correlation_id, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_BEARER_CONTROL_MODE:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:8, _/binary>> = Value0,
    Mode = case Value of
               0 -> ms_only;
               1 -> ms_nw
           end,
    [{bearer_control_mode, Mode}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_FLOW_IDENTIFIER:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{mbms_flow_identifier, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_IP_MULTICAST_DISTRIBUTION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<CData:4/binary,
      DAddrType:2, DAddrLen:6, DAddr:DAddrLen/binary,
      SAddrType:2, SAddrLen:6, SAddr:SAddrLen/binary,
      HCInd:8>> = Value,
    DAddress = case {DAddrType, DAddrLen} of
                   {0, 4} ->
                       #{ipv4 => otc_gtpv2c:bin_to_ip_address(DAddr)};
                   {1, 16} ->
                       #{ipv6 => otc_gtpv2c:bin_to_ip_address(DAddr)}
               end,
    SAddress = case {SAddrType, SAddrLen} of
                   {0, 4} ->
                       #{ipv4 => otc_gtpv2c:bin_to_ip_address(SAddr)};
                   {1, 16} ->
                       #{ipv6 => otc_gtpv2c:bin_to_ip_address(SAddr)}
               end,
    MultiDist = #{common_tunnel_endpoint_identifier_data => CData,
                  distribution_address => DAddress,
                  source_address => SAddress,
                  mbms_hc_indicator => HCInd},
    [{mbms_ip_multicast_distribution, MultiDist}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MBMS_DISTRIBUTION_ACKNOWLEDGEMENT:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    <<_:6, D:2>> = Value,
    Dist = #{distribution_indication => case D of
                                            0 -> none;
                                            1 -> all;
                                            2 -> some
                                        end},
    [{mbms_distribution_acknowledgement, Dist}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RELIABLE_INTER_RAT_HANDOVER_INFO:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    [{reliable_inter_rat_handover_info, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RFSP_INDEX:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 2 ->
    <<Value:16, _/binary>> = Value0,
    [{rfsp_index, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_FQDN:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Identical to the encoding in IETF RFC 1035
    FQDN = otc_gtpv2c:decode_apn(Value),
    [{fqdn, FQDN}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EVOLVED_ALLOCATION_RETENTION_PRIORITY_I:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    Priority = decode_allocation_retention_priority(Value),
    [{evolved_allocation_retention_priority_i, Priority}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EVOLVED_ALLOCATION_RETENTION_PRIORITY_II:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 2 ->
    <<Value:2/binary>> = Value0,
    <<_:4, NSAPI:4, PL/binary>> = Value,
    Priority = decode_allocation_retention_priority(PL),
    Prio = Priority#{nsapi => NSAPI},
    [{evolved_allocation_retention_priority_ii, Prio}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EXTENDED_COMMON_FLAGS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<UASI:1, BDWI:1, PCRI:1, VB:1, RetLoc:1, CPSR:1, CCRSI:1, UnauthenticatedIMSI:1>> = Value,
    Flags = #{ue_available_for_signaling_indication => UASI,
              buffered_dl_data_waiting_indication => BDWI,
              pcscf_restoration_indication => PCRI,
              voice_bearer => VB,
              retrieve_location => RetLoc,
              cs_to_ps_srvcc_indication => CPSR,
              csg_change_reporting_support_indication => CCRSI,
              unauthenticated_imsi => UnauthenticatedIMSI
             },
    [{extended_common_flags, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UCI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 8 ->
    <<Value:8/binary, _/binary>> = Value0,
    <<MCCMNCBin:3/binary,
      CSGIDBin:4/binary,
      AccessMode:2, _:5, CMI:1>> = Value,
    MCCMNC = otc_gtpv2c:decode_mcc_mnc(MCCMNCBin),
    CSGID = otc_gtpv2c:decode_csg_id(CSGIDBin),
    Base = maps:merge(MCCMNC, CSGID),
    UCI = Base#{access_mode => case AccessMode of
                                   0 -> closed;
                                   1 -> hybrid
                               end,
                %% CMI reverse of GTPv2-C
                csg_membership_indication => case CMI of
                                                 0 -> true;
                                                 1 -> false
                                             end},
    [{uci, UCI}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CSG_INFORMATION_REPORTING_ACTION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:5, UCIUHC:1, UCISHC:1, UCICSG:1, _/binary>> = Value,
    Actions = case UCIUHC+UCISHC+UCICSG > 0 of
                  true ->
                      [start_reporting_unsubscribed_cell || UCIUHC =:= 1] ++
                          [start_reporting_subscribed_hybrid_cell || UCISHC =:= 1] ++
                          [start_reporting_csg_cell || UCICSG =:= 1];
                  false ->
                      [stop_reporting]
              end,
    [{csg_information_reporting_action, Actions}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CSG_ID:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 4 ->
    <<Value:4/binary, _/binary>> = Value0,
    CSGId = otc_gtpv2c:decode_csg_id(Value),
    [{csg_id, CSGId}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CMI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:1/binary, _/binary>> = Value0,
    <<_:7, CMI:1>> = Value,
    CM = case CMI of
             0 -> true;
             1 -> false
         end,
    [{cmi, CM}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_AMBR:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 8 ->
    <<Value:8/binary, _/binary>> = Value0,
    <<UL:32, DL:32>> = Value,
    AMBR = #{uplink => UL,
             downlink => DL
            },
    [{ambr, AMBR}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UE_NETWORK_CAPABILITY:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 24.301
    [{ue_network_capability, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UE_AMBR:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<SUL:32, SDL:32, AuthorizedAMBR/binary>> = Value,
    AMBR = case AuthorizedAMBR of
               <<AUL:32, ADL:32>> ->
                   #{subscribed => #{uplink => SUL,
                                     downlink => SDL},
                     authorized => #{uplink => AUL,
                                     downlink => ADL}};
               <<>> ->
                   #{subscribed => #{uplink => SUL,
                                     downlink => SDL}}
           end,
    [{ue_ambr, AMBR}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_APN_AMBR_WITH_NSAPI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 9 ->
    <<Value:9/binary, _/binary>> = Value0,
    <<_:4, NSAPI:4, AUL:32, ADL:32>> = Value,
    AMBR = #{nsapi => NSAPI,
             authorized => #{uplink => AUL,
                             downlink => ADL}},
    [{apn_ambr_with_nsapi, AMBR}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_GGSN_BACK_OFF_TIME:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<Unit:3, Val:5, _/binary>> = Value,
    UMs = case Unit of
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
    Timer = #{value => Val,
              unit_ms => UMs},
    [{ggsn_back_off_time, Timer}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SIGNALLING_PRIORITY_INDICATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:7, LAPI:1, _/binary>> = Value,
    Prio = #{low_access_priority_indication => LAPI
            },
    [{signalling_priority_indication, Prio}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SIGNALLING_PRIORITY_INDICATION_WITH_NSAPI:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, _:7, LAPI:1, _/binary>> = Value,
    Prio = #{nsapi => NSAPI,
             low_access_priority_indication => LAPI
            },
    [{signalling_priority_indication_with_nsapi, Prio}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_HIGHER_BITRATES_THAN_16_MBPS_FLAG:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:8, _/binary>> = Value0,
    Flag = case Value of
               0 -> not_allowed;
               1 -> allowed
           end,
    [{higher_bitrates_than_16_mbps_flag, Flag}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ADDITIONAL_MM_CONTEXT_FOR_SRVCC:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.216
    <<MSC2Len:8, MSC2:MSC2Len/binary,
      MSC3Len:8, MSC3:MSC3Len/binary,
      SCLLen:8, SCL:SCLLen/binary,
      _/binary>> = Value,
    MMContext = #{mobile_station_classmark_2 => MSC2,
                  mobile_station_classmark_3 => MSC3,
                  supported_codec_list => SCL
                 },
    [{additional_mm_context_for_srvcc, MMContext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ADDITIONAL_FLAGS_FOR_SRVCC:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:7, ICS:1, _/binary>> = Value,
    Flags = #{ims_centralized_service => ICS},
    [{additional_flags_for_srvcc, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_STN_SR:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 23.003
    Addr = decode_address(Value),
    [{stn_sr, Addr}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_C_MSISDN:8, L:16, Value:L/binary, Rest/binary>>) ->
    [{c_msisdn, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EXTENDED_RANAP_CAUSE:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% RANAP-cause is defined in 3GPP TS 25.413
    <<Cause:16, _/binary>> = Value,
    [{extended_ranap_cause, Cause}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ENODEB_ID:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<Type:8, ID/binary>> = Value,
    ENodeBID = case Type of
                   0 ->
                       <<MCCMNCBin:3/binary, _:4, MID:20, TAC:16>> = ID,
                       MCCMNC = otc_gtpv2c:decode_mcc_mnc(MCCMNCBin),
                       MCCMNC#{macro_enodeb_id => MID,
                               tracking_area_code => TAC};
                   1 ->
                       <<MCCMNCBin:3/binary, _:4, HID:28, TAC:16>> = ID,
                       MCCMNC = otc_gtpv2c:decode_mcc_mnc(MCCMNCBin),
                       MCCMNC#{home_enodeb_id => HID,
                              tracking_area_code => TAC}
               end,
    [{enodeb_id, ENodeBID}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SELECTION_MODE_WITH_NSAPI:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 2 ->
    <<Value:2/binary, _/binary>> = Value0,
    <<_:4, NSAPI:4, _:7, SLV:1>> = Value,
    Mode = #{nsapi => NSAPI,
             selection_mode => case SLV of
                                   0 -> verified;
                                   1 -> unverified_ms_provided;
                                   _ -> unverified_network_provided
                               end},
    [{selection_mode_with_nsapi, Mode}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_ULI_TIMESTAMP:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in IETF RFC 5905
    <<TS:32, _/binary>> = Value,
    [{uli_timestamp, TS}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_LHN_ID_WITH_NSAPI:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:4, NSAPI:4, LHNID/binary>> = Value,
    LHN = #{nsapi => NSAPI,
            local_home_network_id => LHNID},
    [{lhn_id_with_nsapi, LHN}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CN_OPERATOR_SELECTION_ENTITY:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:6, SE:2, _/binary>> = Value,
    SelEnt = case SE of
                 0 -> ue;
                 _ -> network
             end,
    [{cn_operator_selection_entity, SelEnt}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UE_USAGE_TYPE:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 29.272
    <<UsageType:32>> = Value,
    [{ue_usage_type, UsageType}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_EXTENDED_COMMON_FLAGS_II:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:5, PMTSMI:1, DTCI:1, PNSI:1, _/binary>> = Value,
    Flags = #{pending_mt_short_message_indication => PMTSMI,
              delay_tolerant_connection_indication => DTCI,
              pending_network_initiated_pdn_connection_signalling_indication => PNSI},
    [{extended_common_flags_ii, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_NODE_IDENTIFIER:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Specified in 3GPP TS 29.274
    [{node_identifier, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CIOT_OPTIMIZATIONS_SUPPORT_INDICATION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:6, SCNI:1, SGNI:1, _/binary>> = Value,
    Indication = #{scef_non_ip_pdn_support_indication => SCNI,
                   gi_non_ip_pdn_support_indication => SGNI},
    [{ciot_optimizations_support_indication, Indication}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SCEF_PDN_CONNECTION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<APNLen:8, APNBin:APNLen/binary,
      _:4, NSAPI:4,
      SCEFLen:8, SCEFBin:SCEFLen/binary,
      _/binary>> = Value,
    APN = otc_gtpv2c:decode_apn(APNBin),
    SCEF = #{apn => APN,
             nsapi => NSAPI,
             scef_id => SCEFBin},
    [{scef_pdn_connection, SCEF}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_IOV_UPDATES_COUNTER:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 1 ->
    <<Value:8, _/binary>> = Value0,
    [{iov_updates_counter, Value}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_MAPPED_UE_USAGE_TYPE:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% Defined in 3GPP TS 29.303
    <<UsageType:16, _/binary>> = Value,
    [{mapped_ue_usage_type, UsageType}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_UP_FUNCTION_SELECTION_INDICATION_FLAGS:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<_:7, DCNR:1, _/binary>> = Value,
    Flags = #{dual_connectivity_with_nr => DCNR},
    [{up_function_selection_indication_flags, Flags}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_SPECIAL_IE_TYPE_FOR_IE_TYPE_EXTENSION:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<ExtId:16, ExtVal/binary>> = Value,
    Ext = #{extension_identifier => ExtId,
            extension_value => ExtVal},
    [{special_ie_type_for_ie_type_extension, Ext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CHARGING_GATEWAY_ADDRESS:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 16 ->
    <<Value:16/binary, _/binary>> = Value0,
    IP = #{ipv6 => otc_gtpv2c:bin_to_ip_address(Value)},
    [{charging_gateway_address, IP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_CHARGING_GATEWAY_ADDRESS:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 4 ->
    <<Value:4/binary, _/binary>> = Value0,
    IP = #{ipv4 => otc_gtpv2c:bin_to_ip_address(Value)},
    [{charging_gateway_address, IP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_PRIVATE_EXTENSION:8, L:16, Value:L/binary, Rest/binary>>) ->
    %% The Extension Identifier is a value defined in the Private
    %% Enterprise number list in the most recent "Assigned Numbers"
    %% RFC (RFC 3232 [14] or later).
    %% See https://www.iana.org/assignments/enterprise-numbers
    <<ExtId:16, ExtVal/binary>> = Value,
    Ext = #{identifier => ExtId, value => ExtVal},
    [{private_extension, Ext}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_GTP_U_PEER_ADDRESS:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 16 ->
    <<Value:16/binary, _/binary>> = Value0,
    IP = #{ipv6 => otc_gtpv2c:bin_to_ip_address(Value)},
    [{gtp_u_peer_address, IP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_GTP_U_PEER_ADDRESS:8, L:16, Value0:L/binary, Rest/binary>>) when L >= 4 ->
    <<Value:4/binary, _/binary>> = Value0,
    IP = #{ipv4 => otc_gtpv2c:bin_to_ip_address(Value)},
    [{gtp_u_peer_address, IP}|decode_ieis(Rest)];
decode_ieis(<<?GTPv1C_IEI_RECOVERY_TIME_STAMP:8, L:16, Value:L/binary, Rest/binary>>) ->
    <<Seconds:32, Fractions:32, _/binary>> = Value,
    S = #{seconds => Seconds,
          fractions => Fractions},
    [{recovery_time_stamp, S}|decode_ieis(Rest)];
decode_ieis(_) ->
    [].

decode_msg(echo_request, IEIs) ->
    Fields = [{private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => path_management};
decode_msg(echo_response, IEIs) ->
    Fields = [{recovery, recovery, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => path_management};
decode_msg(version_not_supported, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => path_management};
decode_msg(supported_extension_headers_notification, IEIs) ->
    Fields = [{extension_header_type_list, extension_header_type_list, mandatory}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => path_management};
decode_msg(create_pdp_context_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {routeing_area_identity, rai, optional},
              {recovery, recovery, optional},
              {selection_mode, selection_mode, conditional},
              {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {nsapi, nsapi, mandatory},
              {linked_nsapi, nsapi, conditional},
              {charging_characteristics, charging_characteristics, conditional},
              {trace_reference, trace_reference, optional},
              {trace_type, trace_type, optional},
              {end_user_address, end_user_address, conditional},
              {access_point_name, access_point_name, conditional},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {sgsn_address_for_signalling, gsn_address, mandatory},
              {sgsn_address_for_user_traffic, gsn_address, mandatory},
              {msisdn, msisdn, conditional},
              {quality_of_service_profile, quality_of_service_profile, mandatory},
              {tft, traffic_flow_template, conditional},
              {trigger_id, trigger_id, optional},
              {omc_identity, omc_identity, optional},
              {common_flags, common_flags, optional},
              {apn_restriction, apn_restriction, optional},
              {rat_type, rat_type, optional},
              {user_location_information, user_location_information, optional},
              {ms_time_zone, ms_time_zone, optional},
              {imeisv, imeisv, conditional},
              {camel_charging_information_container, camel_charging_information_container, optional},
              {additional_trace_info, additional_trace_info, optional},
              {correlation_id, correlation_id, optional},
              {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
              {extended_common_flags, extended_common_flags, optional},
              {user_csg_information, uci, optional},
              {apn_ambr, ambr, optional},
              {signalling_priority_indication, signalling_priority_indication, optional},
              {cn_operator_selection_entity, cn_operator_selection_entity, optional},
              {mapped_ue_usage_type, mapped_ue_usage_type, optional},
              {up_function_selection_indication_flags, up_function_selection_indication_flags, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(create_pdp_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {reordering_required, reordering_required, conditional},
              {recovery, recovery, optional},
              {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {nsapi, nsapi, optional},
              {charging_id, charging_id, conditional},
              {end_user_address, end_user_address, conditional},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {ggsn_address_for_control_plane, gsn_address, conditional},
              {ggsn_address_for_user_traffic, gsn_address, conditional},
              {alternative_ggsn_address_for_control_plane, gsn_address, conditional},
              {alternative_ggsn_address_for_user_traffic, gsn_address, conditional},
              {quality_of_service_profile, quality_of_service_profile, conditional},
              {charging_gateway_address, charging_gateway_address, optional},
              {alternative_charging_gateway_address, charging_gateway_address, optional},
              {common_flags, common_flags, optional},
              {apn_restriction, apn_restriction, optional},
              {ms_info_change_reporting_action, ms_info_change_reporting_action, optional},
              {bearer_control_mode, bearer_control_mode, optional},
              {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
              {extended_common_flag, extended_common_flags, optional},
              {csg_information_reporting_action, csg_information_reporting_action, optional},
              {apn_ambr, ambr, optional},
              {ggsn_back_off_time, ggsn_back_off_time, optional},
              {extended_common_flags_ii, extended_common_flags_ii, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(update_pdp_context_request, IEIs) ->
    SGSNFields = [{imsi, imsi, optional},
                  {routeing_area_identity, rai, optional},
                  {recovery, recovery, optional},
                  {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, mandatory},
                  {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
                  {nsapi, nsapi, mandatory},
                  {trace_reference, trace_reference, optional},
                  {trace_type, trace_type, optional},
                  {protocol_configuration_options, protocol_configuration_options, optional},
                  {sgsn_address_for_control_plane, gsn_address, mandatory},
                  {sgsn_address_for_user_traffic, gsn_address, mandatory},
                  {alternative_sgsn_address_for_control_plane, gsn_address, conditional},
                  {alternative_sgsn_address_for_user_traffic, gsn_address, conditional},
                  {quality_of_service_profile, quality_of_service_profile, mandatory},
                  {tft, traffic_flow_template, optional},
                  {trigger_id, trigger_id, optional},
                  {omc_identity, omc_identity, optional},
                  {common_flags, common_flags, optional},
                  {rat_type, rat_type, optional},
                  {user_location_information, user_location_information, optional},
                  {ms_time_zone, ms_time_zone, optional},
                  {additonal_trace_info, additional_trace_info, optional},
                  {direct_tunnel_flags, direct_tunnel_flags, optional},
                  {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
                  {extended_common_flags, extended_common_flags, optional},
                  {user_csg_information, uci, optional},
                  {apn_ambr, ambr, optional},
                  {signalling_priority_indication, signalling_priority_indication, optional},
                  {cn_operator_selection_entity, cn_operator_selection_entity, optional},
                  {imeisv, imeisv, optional},
                  {private_extension, private_extension, optional}],
    GGSNFields = [{imsi, imsi, optional},
                  {recovery, recovery, optional},
                  {nsapi, nsapi, mandatory},
                  {end_user_address, end_user_address, optional},
                  {protocol_configuration_options, protocol_configuration_options, optional},
                  {quality_of_service_profile, quality_of_service_profile, optional},
                  {tft, traffic_flow_template, optional},
                  {common_flags, common_flags, optional},
                  {apn_restriction, apn_restriction, optional},
                  {ms_info_change_reporting_action, ms_info_change_reporting_action, optional},
                  {direct_tunnel_flags, direct_tunnel_flags, optional},
                  {bearer_control_mode, bearer_control_mode, optional},
                  {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
                  {extended_common_flags, extended_common_flags, optional},
                  {csg_information_reporting_action, csg_information_reporting_action, optional},
                  {apn_ambr, ambr, optional},
                  {private_extension, private_extension, optional}],
    SGSNCP = lists:keymember(sgsn_address_for_control_plane, 1, IEIs),
    SGSNUP = lists:keymember(sgsn_address_for_user_traffic, 1, IEIs),
    Fields = case SGSNCP orelse SGSNUP of
                 true ->
                     SGSNFields;
                 false ->
                     SGSNFields
             end,
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(update_pdp_context_response, IEIs) ->
    SGSNFields = [{cause, cause, mandatory},
                  {recovery, recovery, optional},
                  {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, optional},
                  {protocol_configuration_options, protocol_configuration_options, optional},
                  {sgsn_address_for_user_traffic, gsn_address, optional},
                  {quality_of_service_profile, quality_of_service_profile, conditional},
                  {user_location_information, user_location_information, optional},
                  {ms_time_zone, ms_time_zone, optional},
                  {direct_tunnel_flags, direct_tunnel_flags, optional},
                  {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
                  {apn_ambr, ambr, optional},
                  {private_extension, private_extension, optional}],
    GGSNFields = [{cause, cause, mandatory},
                  {recovery, recovery, optional},
                  {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, conditional},
                  {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
                  {charging_id, charging_id, conditional},
                  {protocol_configuration_options, protocol_configuration_options, optional},
                  {ggsn_address_for_control_plane, gsn_address, conditional},
                  {ggsn_address_for_user_traffic, gsn_address, conditional},
                  {alternative_ggsn_address_for_control_plane, gsn_address, conditional},
                  {alternative_ggsn_address_for_user_traffic, gsn_address, conditional},
                  {quality_of_service_profile, quality_of_service_profile, conditional},
                  {charging_gateway_address, charging_gateway_address, optional},
                  {alternative_charging_gateway_address, charging_gateway_address, optional},
                  {common_flags, common_flags, optional},
                  {apn_restriction, apn_restriction, optional},
                  {bearer_control_mode, bearer_control_mode, optional},
                  {ms_info_change_reporting_action, ms_info_change_reporting_action, optional},
                  {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
                  {csg_information_reporting_action, csg_information_reporting_action, optional},
                  {apn_ambr, ambr, optional},
                  {private_extension, private_extension, optional}],

    GGSNKeys = [K || {K, _, _} <- GGSNFields] -- [K || {K, _, _} <- SGSNFields],
    IsGGSN = lists:any(fun (K) -> lists:keymember(K, 1, IEIs) end, GGSNKeys),
    Fields = case IsGGSN of
                 false ->
                     SGSNFields;
                 true ->
                     GGSNFields
             end,
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(delete_pdp_context_request, IEIs) ->
    Fields = [{cause, cause, optional},
              {teardown_ind, teardown_ind, conditional},
              {nsapi, nsapi, mandatory},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {user_location_information, user_location_information, optional},
              {ms_time_zone, ms_time_zone, optional},
              {extended_common_flags, extended_common_flags, optional},
              {uli_timestamp, uli_timestamp, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(delete_pdp_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {user_location_information, user_location_information, optional},
              {ms_time_zone, ms_time_zone, optional},
              {uli_timestamp, uli_timestamp, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(pdu_notification_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {ggsn_address_for_control_plane, gsn_address, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(pdu_notification_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(pdu_notification_reject_request, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(pdu_notification_reject_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(initiate_pdp_context_activation_request, IEIs) ->
    Fields = [{linked_nsapi, nsapi, mandatory},
              {protocol_configuration_options, protocol_configuration_options, optional},
              {quality_of_service_profile, quality_of_service_profile, mandatory},
              {tft, traffic_flow_template, conditional},
              {correlation_id, correlation_id, mandatory},
              {evolved_allocation_retention_priority_i, evolved_allocation_retention_priority_i, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(initiate_pdp_context_activation_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {protocol_configuration_options, protocol_configuration_options, conditional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(send_routeing_information_for_gprs_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(send_routeing_information_for_gprs_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {imsi, imsi, mandatory},
              {map_cause, map_cause, optional},
              {ms_not_reachable_reason, ms_not_reachable_reason, optional},
              {gsn_address, gsn_address, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(failure_report_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(failure_report_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {map_cause, map_cause, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(note_ms_gprs_present_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {gsn_address, gsn_address, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(note_ms_gprs_present_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => location_management};
decode_msg(identification_request, IEIs) ->
    Fields = [{routeing_area_identity, rai, mandatory},
              {packet_tmsi, p_tmsi, mandatory},
              {p_tmsi_signature, p_tmsi_signature, conditional},
              {sgsn_address_for_control_plane, gsn_address, optional},
              {hop_counter, hop_counter, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(identification_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {imsi, imsi, conditional},
              {authentication_triplet, authentication_triplet, conditional},
              {authentication_quintuplet, authentication_quintuplet, conditional},
              {ue_usage_type, ue_usage_type, optional},
              {iov_updates_counter, iov_updates_counter, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(sgsn_context_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {routeing_area_identity, rai, mandatory},
              {temporary_logical_link_identifier_tlli, tlli, conditional},
              {packet_tmsi_p_tmsi, p_tmsi, conditional},
              {p_tmsi_signature, p_tmsi_signature, conditional},
              {ms_validated, ms_validated, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {sgsn_address_for_control_plane, gsn_address, mandatory},
              {alternative_sgsn_address_for_control_plane, gsn_address, optional},
              {sgsn_number, sgsn_number, optional},
              {rat_type, rat_type, optional},
              {hop_counter, hop_counter, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(sgsn_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {imsi, imsi, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {rab_context, rab_context, conditional},
              {radio_priority_sms, radio_priority_sms, optional},
              {radio_priority, radio_priority, optional},
              {packet_flow_id, packet_flow_id, optional},
              {charging_characteristics, charging_characteristics, optional},
              {radio_priority_lcs, radio_priority_lcs, optional},
              {mm_context, mm_context, conditional},
              {pdp_context, pdp_context, conditional},
              {sgsn_address_for_control_plane, gsn_address, conditional},
              {alternative_ggsn_address_for_control_plane, gsn_address, optional},
              {alternative_ggsn_address_for_user_traffic, gsn_address, optional},
              {pdp_context_prioritization, pdp_context_prioritization, optional},
              {mbms_ue_context, mbms_ue_context, optional},
              {subscribed_rfsp_index, rfsp_index, optional},
              {rfsp_index_in_use, rfsp_index, optional},
              {co_located_ggsn_pgw_fqdn, fqdn, optional},
              {evolved_allocation_retention_priority_ii, evolved_allocation_retention_priority_ii, optional},
              {extended_common_flags, extended_common_flags, optional},
              {ue_network_capability, ue_network_capability, optional},
              {ue_ambr, ue_ambr, optional},
              {apn_ambr_with_nsapi, apn_ambr_with_nsapi, optional},
              {signalling_priority_indication_with_nsapi, signalling_priority_indication_with_nsapi, optional},
              {higher_bitrates_than_16_mbps_flag, higher_bitrates_than_16_mbps_flag, optional},
              {selection_mode_with_nsapi, selection_mode_with_nsapi, optional},
              {local_home_network_id_with_nsapi, lhn_id_with_nsapi, optional},
              {ue_usage_type, ue_usage_type, optional},
              {extended_common_flags_ii, extended_common_flags_ii, optional},
              {ue_scef_pdn_connection, scef_pdn_connection, optional},
              {iov_updates_counter, iov_updates_counter, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(sgsn_context_acknowledge, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_data_ii, tunnel_endpoint_identifier_data_ii, conditional},
              {sgsn_address_for_user_traffic, gsn_address, conditional},
              {sgsn_number, sgsn_number, optional},
              {node_identifier, node_identifier, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_relocation_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {ranap_cause, ranap_cause, mandatory},
              {packet_flow_id, packet_flow_id, optional},
              {charging_characteristics, charging_characteristics, optional},
              {mm_context, mm_context, mandatory},
              {pdp_context, pdp_context, conditional},
              {sgsn_address_for_control_plane, gsn_address, mandatory},
              {alternative_ggsn_address_for_control_plane, gsn_address, optional},
              {alternative_ggsn_address_for_user_traffic, gsn_address, optional},
              {target_identification, target_identification, mandatory},
              {utran_transparent_container, utran_transparent_container, mandatory},
              {pdp_context_prioritization, pdp_context_prioritization, optional},
              {mbms_ue_context, mbms_ue_context, optional},
              {selected_plmn_id, selected_plmn_id, optional},
              {bss_container, bss_container, optional},
              {cell_identification, cell_identification, optional},
              {bssgp_cause, bssgp_cause, optional},
              {ps_handover_xid_parameters, ps_handover_xid_parameters, optional},
              {direct_tunnel_flags, direct_tunnel_flags, optional},
              {reliable_inter_rat_handover_info, reliable_inter_rat_handover_info, optional},
              {subscribed_rfsp_index, rfsp_index, optional},
              {rfsp_index_in_use, rfsp_index, optional},
              {co_located_ggsn_pgw_fqdn, fqdn, optional},
              {evolved_allocation_retention_priority_ii, evolved_allocation_retention_priority_ii, optional},
              {extended_common_flags, extended_common_flags, optional},
              {csg_id, csg_id, optional},
              {csg_membership_indication, cmi, optional},
              {ue_network_capability, ue_network_capability, optional},
              {ue_ambr, ue_ambr, optional},
              {apn_ambr_with_nsapi, apn_ambr_with_nsapi, optional},
              {signalling_priority_indication_with_nsapi, signalling_priority_indication_with_nsapi, optional},
              {higher_bitrates_than_16_mbps_flag, higher_bitrates_than_16_mbps_flag, optional},
              {additional_mm_context_for_srvcc, additional_mm_context_for_srvcc, optional},
              {additional_flags_for_srvcc, additional_flags_for_srvcc, optional},
              {stn_sr, stn_sr, optional},
              {c_msisdn, c_msisdn, optional},
              {extended_ranap_cause, extended_ranap_cause, optional},
              {enodeb_id, enodeb_id, optional},
              {selection_mode_with_nsapi, selection_mode_with_nsapi, optional},
              {ue_usage_type, ue_usage_type, optional},
              {extended_common_flags_ii, extended_common_flags_ii, optional},
              {ue_scef_pdn_connection, scef_pdn_connection, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_relocation_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {tunnel_endpoint_identifier_data_ii, tunnel_endpoint_identifier_data_ii, optional},
              {ranap_cause, ranap_cause, conditional},
              {sgsn_address_for_control_plane, gsn_address, conditional},
              {sgsn_address_for_user_traffic, gsn_address, optional},
              {utran_transparent_container, utran_transparent_container, optional},
              {rab_setup_information, rab_setup_information, conditional},
              {additional_rab_setup_information, additional_rab_setup_information, conditional},
              {sgsn_number, sgsn_number, optional},
              {bss_container, bss_container, optional},
              {bssgp_cause, bssgp_cause, optional},
              {list_of_set_up_pfcs, list_of_set_up_pfcs, optional},
              {extended_ranap_cause, extended_ranap_cause, optional},
              {node_identfiier, node_identifier, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_relocation_complete, IEIs) ->
    Fields = [{private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(relocation_cancel_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {imeisv, imeisv, conditional},
              {extended_common_flags, extended_common_flags, optional},
              {extended_ranap_cause, extended_ranap_cause, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(relocation_cancel_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_relocation_complete_acknowledge, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, charging_id, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_srns_context_acknowledge, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, charging_id, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(forward_srns_context, IEIs) ->
    Fields = [{rab_context, rab_context, mandatory},
              {source_rnc_pdcp_context_info, source_rnc_pdcp_context_info, optional},
              {pdu_numbers, pdu_numbers, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(ran_information_relay, IEIs) ->
    Fields = [{ran_transparent_container, ran_transparent_container, mandatory},
              {rim_routing_address, rim_routing_address, optional},
              {rim_routing_address_discriminator, rim_routing_address_discriminator, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(ue_registration_query_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(ue_registration_query_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {imsi, imsi, mandatory},
              {selected_plmn_id, selected_plmn_id, conditional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mobility_management};
decode_msg(mbms_notification_request, IEIs) ->
    Fields = [{imsi, imsi, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {nsapi, nsapi, mandatory},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {ggsn_address_for_control_plane, gsn_address, mandatory},
              {mbms_protocol_configuration_options, mbms_protocol_configuration_options, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_notification_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_notification_reject_request, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, mandatory},
              {nsapi, nsapi, mandatory},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {sgsn_address_for_control_plane, gsn_address, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_notification_reject_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(create_mbms_context_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {routeing_area_identity, rai, mandatory},
              {recovery, recovery, optional},
              {selection_mode, selection_mode, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {trace_reference, trace_reference, optional},
              {trace_type, trace_type, optional},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {sgsn_address_for_signalling, gsn_address, mandatory},
              {msisdn, msisdn, conditional},
              {trigger_id, trigger_id, optional},
              {omc_identity, omc_identity, optional},
              {rat_type, rat_type, optional},
              {user_location_information, user_location_information, optional},
              {ms_time_zone, ms_time_zone, optional},
              {imeisv, imeisv, optional},
              {mbms_protocol_configuration_options, mbms_protocol_configuration_options, optional},
              {additonal_trace_info, additional_trace_info, optional},
              {enhanced_nsapi, enhanced_nsapi, mandatory},
              {additional_mbms_trace_info, additional_mbms_trace_info, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(create_mbms_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {recovery, recovery, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {charging_id, charging_id, conditional},
              {ggsn_address_for_control_plane, gsn_address, conditional},
              {alternative_ggsn_address_for_control_plane, gsn_address, conditional},
              {charging_gateway_address, charging_gateway_address, optional},
              {alternative_charging_gateway_address, charging_gateway_address, optional},
              {mbms_protocol_configuration_options, mbms_protocol_configuration_options, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(update_mbms_context_request, IEIs) ->
    Fields = [{routeing_area_identity, rai, mandatory},
              {recovery, recovery, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {trace_reference, trace_reference, optional},
              {trace_type, trace_type, optional},
              {sgsn_address_for_control_plane, gsn_address, mandatory},
              {alternative_sgsn_address_for_control_plane, gsn_address, conditional},
              {trigger_id, trigger_id, optional},
              {omc_identity, omc_identity, optional},
              {rat_type, rat_type, optional},
              {user_location_information, user_location_information, optional},
              {ms_time_zone, ms_time_zone, optional},
              {additional_trace_info, additional_trace_info, optional},
              {enhanced_nsapi, enhanced_nsapi, mandatory},
              {additional_mbms_trace_info, additional_mbms_trace_info, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(update_mbms_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {recovery, recovery, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {charging_id, charging_id, conditional},
              {ggsn_address_for_control_plane, gsn_address, conditional},
              {alternative_ggsn_address_for_control_plane, gsn_address, conditional},
              {charging_gateway_address, charging_gateway_address, optional},
              {alternative_charging_gateway_address, charging_gateway_address, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(delete_mbms_context_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {end_user_address, end_user_address, conditional},
              {access_point_name, access_point_name, conditional},
              {mbms_protocol_configuration_options, mbms_protocol_configuration_options, optional},
              {enhanced_nsapi, enhanced_nsapi, conditional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(delete_mbms_context_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {mbms_protocol_configuration_options, mbms_protocol_configuration_options, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_registration_request, IEIs) ->
    Fields = [{tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {sgsn_address_for_control_plane, gsn_address, conditional},
              {alternative_sgsn_address_for_control_plane, gsn_address, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_registration_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {ggsn_address_for_control_plane, gsn_address, conditional},
              {temporary_mobile_group_identity_tmgi, tmgi, conditional},
              {required_mbms_bearer_capabilities, required_mbms_bearer_capabilities, conditional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_deregistration_request, IEIs) ->
    Fields = [{end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_deregistration_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_start_request, IEIs) ->
    Fields = [{recovery, recovery, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {ggsn_address_for_control_plane, gsn_address, conditional},
              {alternative_ggsn_address_for_control_plane, gsn_address, optional},
              {quality_of_service_profile, quality_of_service_profile, mandatory},
              {common_flags, common_flags, mandatory},
              {temporary_mobile_group_identity_tmgi, tmgi, mandatory},
              {mbms_service_area, mbms_service_area, mandatory},
              {mbms_session_identifier, mbms_session_identifier, optional},
              {mbms_2g3g_indicator, mbms_2g3g_indicator, mandatory},
              {mbms_session_duration, mbms_session_duration, mandatory},
              {mbms_session_repetition_number, mbms_session_repetition_number, optional},
              {mbms_time_to_data_transfer, mbms_time_to_data_transfer, mandatory},
              {mbms_flow_identifier, mbms_flow_identifier, optional},
              {mbms_ip_multicast_distribution, mbms_ip_multicast_distribution, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_start_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {recovery, recovery, optional},
              {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, conditional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, conditional},
              {sgsn_address_for_control_plane, gsn_address, conditional},
              {sgsn_address_for_user_traffic, gsn_address, conditional},
              {alternative_sgsn_address_for_user_traffic, gsn_address, optional},
              {mbms_distribution_acknowledgement, mbms_distribution_acknowledgement, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_stop_request, IEIs) ->
    Fields = [{end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {mbms_flow_identifier, mbms_flow_identifier, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_stop_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_update_request, IEIs) ->
    Fields = [{tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, optional},
              {end_user_address, end_user_address, mandatory},
              {access_point_name, access_point_name, mandatory},
              {ggsn_address_for_control_plane, gsn_address, optional},
              {temporary_mobile_group_identity_tmgi, tmgi, mandatory},
              {mbms_session_duration, mbms_session_duration, mandatory},
              {mbms_service_area, mbms_service_area, mandatory},
              {mbms_session_identifier, mbms_session_identifier, optional},
              {mbms_session_repetition_number, mbms_session_repetition_number, optional},
              {mbms_flow_identifier, mbms_flow_identifier, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(mbms_session_update_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, optional},
              {tunnel_endpoint_identifier_control_plane, tunnel_endpoint_identifier_control_plane, optional},
              {sgsn_address_for_data_i, gsn_address, optional},
              {sgsn_address_for_control_plane, gsn_address, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(ms_info_change_notification_request, IEIs) ->
    Fields = [{imsi, imsi, conditional},
              {linked_nsapi, nsapi, optional},
              {rat_type, rat_type, mandatory},
              {user_location_information, user_location_information, conditional},
              {imeisv, imeisv, conditional},
              {extended_common_flags, extended_common_flags, optional},
              {user_csg_information, uci, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
decode_msg(ms_info_change_notification_response, IEIs) ->
    Fields = [{cause, cause, mandatory},
              {imsi, imsi, conditional},
              {linked_nsapi, nsapi, optional},
              {imeisv, imeisv, conditional},
              {ms_info_change_reporting_action, ms_info_change_reporting_action, optional},
              {csg_information_reporting_action, csg_information_reporting_action, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => mbms};
%% 3GPP TS 32.295
decode_msg(data_record_transfer_request, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{};
decode_msg(data_record_transfer_response, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{};
%% 3GPP TS 29.281
decode_msg(error_indication, IEIs) ->
    Fields = [{tunnel_endpoint_identifier_data_i, tunnel_endpoint_identifier_data_i, mandatory},
              {gtp_u_peer_address, gtp_u_peer_address, mandatory},
              {recovery_time_stamp, recovery_time_stamp, optional},
              {private_extension, private_extension, optional}],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{message_group => tunnel_management};
decode_msg(tunnel_status, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{};
decode_msg(end_marker, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{};
decode_msg(g_pdu, IEIs) ->
    Fields = [],
    Msg = decode_msg_fields(Fields, IEIs),
    Msg#{}.

encode_msg(_, _Msg) ->
    <<>>.

decode_msg_fields(Fields, IEIs) ->
    decode_msg_fields(Fields, IEIs, #{}).

decode_msg_fields([], _IEIs, Acc) ->
    Acc;
decode_msg_fields([{N, I, mandatory}|Fields], IEIs, Acc) ->
    {value, {_, V}, IEIs1} = lists:keytake(I, 1, IEIs),
    decode_msg_fields(Fields, IEIs1, Acc#{N => V});
decode_msg_fields([{N, I, _}|Fields], IEIs, Acc) ->
    case lists:keytake(I, 1, IEIs) of
        {value, {_, V}, IEIs1} ->
            decode_msg_fields(Fields, IEIs1, Acc#{N => V});
        false ->
            decode_msg_fields(Fields, IEIs, Acc)
    end.

parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_REQUEST_IMSI) -> request_imsi;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_REQUEST_IMEI) -> request_imei;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_REQUEST_IMSI_AND_IMEI) -> request_imsi_and_imei;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_NO_IDENTITY_NEEDED) -> no_identity_needed;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_MS_REFUSES) -> req_ms_refuses;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_MS_IS_NOT_GPRS_RESPONDING) -> req_ms_is_not_gprs_responding;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_REACTIVATION_REQUESTED) -> reactivation_requested;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_PDP_ADDRESS_INACTIVITY_TIMER_EXPIRES) -> pdp_address_inactivity_timer_expires;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_NETWORK_FAILURE) -> network_failure;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REQ_QOS_PARAMETER_MISMATCH) -> qos_parameter_mismatch;
parse_iei_cause(?GTPv1C_IEI_CAUSE_ACC_REQUEST_ACCEPTED) -> request_accepted;
parse_iei_cause(?GTPv1C_IEI_CAUSE_ACC_NEW_PDP_TYPE_DUE_TO_NETWORK_PREFERENCE) -> new_pdp_type_due_to_network_preference;
parse_iei_cause(?GTPv1C_IEI_CAUSE_ACC_NEW_PDP_TYPE_DUE_TO_SINGLE_ADDRESS_BEARER_ONLY) -> new_pdp_type_due_to_single_address_bearer_only;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_NON_EXISTENT) -> non_existent;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_INVALID_MESSAGE_FORMAT) -> invalid_message_format;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_IMSI_IMEI_NOT_KNOWN) -> imsi_imei_not_known;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MS_IS_GPRS_DETACHED) -> ms_is_gprs_detached;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MS_IS_NOT_GPRS_RESPONDING) -> rej_ms_is_not_gprs_responding;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MS_REFUSES) -> rej_ms_refuses;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_VERSION_NOT_SUPPORTED) -> version_not_supported;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_NO_RESOURCES_AVAILABLE) -> no_resources_available;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SERVICE_NOT_SUPPORTED) -> service_not_supported;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MANDATORY_IE_INCORRECT) -> mandatory_ie_incorrect;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MANDATORY_IE_MISSING) -> mandatory_ie_missing;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_OPTIONAL_IE_INCORRECT) -> optional_ie_incorrect;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SYSTEM_FAILURE) -> system_failure;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_ROAMING_RESTRICTION) -> roaming_restriction;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_P_TMSI_SIGNATURE_MISMATCH) -> p_tmsi_signature_mismatch;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_GPRS_CONNECTION_SUSPENDED) -> gprs_connection_suspended;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_AUTHENTICATION_FAILURE) -> authentication_failure;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_USER_AUTHENTICATION_FAILED) -> user_authentication_failed;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_CONTEXT_NOT_FOUND) -> context_not_found;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_ALL_DYNAMIC_PDP_ADDRESSES_ARE_OCCUPIED) -> all_dynamic_pdp_addresses_are_occupied;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_NO_MEMORY_IS_AVAILABLE) -> no_memory_is_available;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_RELOCATION_FAILURE) -> relocation_failure;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_UNKNOWN_MANDATORY_EXTENSION_HEADER) -> unknown_mandatory_extension_header;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SEMANTIC_ERROR_IN_THE_TFT_OPERATION) -> semantic_error_in_the_tft_operation;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SYNTACTIC_ERROR_IN_THE_TFT_OPERATION) -> syntactic_error_in_the_tft_operation;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SEMANTIC_ERRORS_IN_PACKET_FILTER) -> semantic_errors_in_packet_filter;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_SYNTACTIC_ERRORS_IN_PACKET_FILTER) -> syntactic_errors_in_packet_filter;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MISSING_OR_UNKNOWN_APN) -> missing_or_unknown_apn;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_UNKNOWN_PDP_ADDRESS_OR_PDP_TYPE) -> unknown_pdp_address_or_pdp_type;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_PDP_CONTEXT_WITHOUT_TFT_ALREADY_ACTIVATED) -> pdp_context_without_tft_already_activated;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_APN_ACCESS_DENIED_NO_SUBSCRIPTION) -> apn_access_denied_no_subscription;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_APN_RESTRICTION_TYPE_INCOMPATIBILITY_WITH_CURRENTLY_ACTIVE_PDP_CONTEXTS) -> apn_restriction_type_incompatibility_with_currently_active_pdp_contexts;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MS_MBMS_CAPABILITIES_INSUFFICIENT) -> ms_mbms_capabilities_insufficient;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_INVALID_CORRELATION_ID) -> invalid_correlation_id;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_MBMS_BEARER_CONTEXT_SUPERSEDED) -> mbms_bearer_context_superseded;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_BEARER_CONTROL_MODE_VIOLATION) -> bearer_control_mode_violation;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_COLLISION_WITH_NETWORK_INITIATED_REQUEST) -> collision_with_network_initiated_request;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_APN_CONGESTION) -> apn_congestion;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_TARGET_ACCESS_RESTRICTED_FOR_THE_SUBSCRIBER) -> target_access_restricted_for_the_subscriber;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_UE_IS_TEMPORARILY_NOT_REACHABLE_DUE_TO_POWER_SAVING) -> ue_is_temporarily_not_reachable_due_to_power_saving;
parse_iei_cause(?GTPv1C_IEI_CAUSE_REJ_RELOCATION_FAILURE_DUE_TO_NAS_MESSAGE_REDIRECTION) -> relocation_failure_due_to_nas_message_redirection.

triplets(<<>>) ->
    [];
triplets(R0) ->
    <<RAND:16/binary, SRES:4/binary, Kc0:8/binary, R1/binary>> = R0,
    T = #{rand => RAND, sres => SRES, kc => Kc0},
    [T | triplets(R1)].

quintuplets(0, _) ->
    [];
quintuplets(N, R0) ->
    <<RAND:16/binary,
      XRESLen:8, XRES:XRESLen/binary,
      CK:16/binary, IK:16/binary,
      AUTNLen:8, AUTN:AUTNLen/binary, R3/binary>> = R0,
    Q = #{rand => RAND, xres => XRES, ck => CK, ik => IK, autn => AUTN},
    [Q | quintuplets(N-1, R3)].

qos(Bin) ->
    %% allocation/retention priority defined in 3GPP TS 23.107
    %% QoS Profile Data 3GPP TS 24.008
    <<ARP:1/binary, PD/binary>> = Bin,
    #{allocation_retention_priority => ARP,
      qos_profile_data => PD}.

parse_pdp_address_org(0) ->
    etsi;
parse_pdp_address_org(1) ->
    ietf.

parse_pdp_address_type(etsi, 1) ->
    ppp;
parse_pdp_address_type(etsi, 2) ->
    non_ip;
parse_pdp_address_type(ietf, 16#21) ->
    ipv4;
parse_pdp_address_type(ietf, 16#57) ->
    ipv6;
parse_pdp_address_type(ietf, 16#8D) ->
    ipv4v6.

pdp_address(Org, Type, <<>>) ->
    Organization = parse_pdp_address_org(Org),
    PDPType = parse_pdp_address_type(Organization, Type),
    PDPType;
pdp_address(Org, Type, Addr) ->
    Organization = parse_pdp_address_org(Org),
    PDPType = parse_pdp_address_type(Organization, Type),
    Address = case PDPType of
                  ppp ->
                      #{ppp => Addr};
                  non_ip ->
                      #{non_ip => Addr};
                  ipv4 ->
                      #{ipv4 => otc_gtpv2c:bin_to_ip_addr(Addr)};
                  ipv6 ->
                      #{ipv6 => otc_gtpv2c:bin_to_ip_addr(Addr)};
                  ipv4v6 ->
                      <<IPv4:4/binary, IPv6:16/binary, _/binary>> = Addr,
                      #{ipv4 => otc_gtpv2c:bin_to_ip_addr(IPv4),
                        ipv6 => otc_gtpv2c:bin_to_ip_addr(IPv6)}
              end,
    Address.

decode_allocation_retention_priority(Value) ->
    <<_:1, PCI:1, PL:4, _:1, PVI:1>> = Value,
    #{preemption_capability => PCI,
      priority_level => PL,
      preemption_vulnerability => PVI}.

decode_address(Bin) ->
    <<ExtI:1, NoAI:3, NPI:4, Addr/binary>> = Bin,
    NoA = case NoAI of
              2#000 -> unknown;
              2#001 -> international;
              2#010 -> national;
              2#011 -> network;
              2#100 -> subscriber;
              2#101 -> reserved;
              2#110 -> abbreviated;
              2#111 -> reserved_extension
          end,
    NP = case NPI of
             2#0000 -> unknown;
             2#0001 -> e164;
             2#0010 -> {spare, NPI};
             2#0011 -> x121;
             2#0100 -> f69;
             2#0101 -> {spare, NPI};
             2#0110 -> e212;
             2#0111 -> {spare, NPI};
             2#1000 -> national;
             2#1001 -> private;
             2#1111 -> reserved_extension;
             _ -> {reserved, NPI}
         end,
    #{extension_indicator => ExtI,
      nature_of_address => NoA,
      numbering_plan => NP,
      address => otc_gtpv2c:decode_tbcd(Addr)}.
