-module(otc_gtp).
-behaviour(otc_codec).

-include("include/gtp.hrl").
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

decode(<<1:3, 1:1, _:1, E:1, S:1, PN:1, MT:8, Len:16, TEID:32, Rest0/binary>>) ->
    <<GTP0:Len/binary, Rest1/binary>> = Rest0,
    MessageType = parse_message_type(MT),
    {MsgFields, GTP1} = decode_msg_fields(E, S, PN, GTP0),
    Msg0 = decode_msg(MessageType, GTP1),
    Msg1 = maps:merge(Msg0, MsgFields),
    Msg2 = Msg1#{message_type => MessageType,
                 teid => TEID},
    {Msg2, Rest1}.

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
parse_extension_headers(0, Rest0) ->
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

decode_msg(echo_request, Bin0) ->
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals#{message_group => path_management
              };
decode_msg(echo_response, Bin0) ->
    {?GTPv1C_IEI_RECOVERY, Recovery, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => path_management,
               recovery => Recovery
              };
decode_msg(version_not_supported, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals#{message_group => path_management
              };
decode_msg(supported_extension_headers_notification, Bin0) ->
    {?GTPv1C_IEI_EXTENSION_HEADER_TYPE_LIST, ExtensionHeaderTypeList, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => path_management,
               extension_header_type_list => ExtensionHeaderTypeList
              };
decode_msg(create_pdp_context_request, Bin0) ->
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, Bin1} = otc_l3_codec:decode_tv(Bin0, 4),
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin2} = otc_l3_codec:decode_tv(Bin1, 1),
    {?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    Opts = [{rai, 3, tv, 6},
            {recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {camel_charging_information_container, 155, tlv, {4, n}},
            {additional_trace_info, 162, tlv, 9},
            {correlation_id, 183, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {ambr, 198, tlv, 8},
            {signalling_priority_indication, 203, tlv, 1},
            {cn_operator_selection_entity, 216, tlv, 1},
            {mapped_ue_usage_type, 223, tlv, 2},
            {up_function_selection_indication_flags, 224, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_group => tunnel_management,
               tunnel_endpoint_identifier_data_i => TunnelEndpointIdentifierDataI,
               nsapi => Nsapi,
               quality_of_service_profile => QualityOfServiceProfile
              };
decode_msg(create_pdp_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {nsapi, 20, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {bearer_control_mode, 184, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {ggsn_back_off_time, 202, tlv, 1},
            {extended_common_flags_ii, 218, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(update_pdp_context_request_sgsn, Bin0) ->
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, Bin1} = otc_l3_codec:decode_tv(Bin0, 4),
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin2} = otc_l3_codec:decode_tv(Bin1, 1),
    {?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    Opts = [{imsi, 2, tv, 8},
            {rai, 3, tv, 6},
            {recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {traffic_flow_template, 137, tlv, {4, n}},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {additional_trace_info, 162, tlv, 9},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {ambr, 198, tlv, 8},
            {signalling_priority_indication, 203, tlv, 1},
            {cn_operator_selection_entity, 216, tlv, 1},
            {imeisv, 154, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_group => tunnel_management,
               tunnel_endpoint_identifier_data_i => TunnelEndpointIdentifierDataI,
               nsapi => Nsapi,
               quality_of_service_profile => QualityOfServiceProfile
              };
decode_msg(update_pdp_context_request_ggsn, Bin0) ->
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{imsi, 2, tv, 8},
            {recovery, 14, tv, 1},
            {end_user_address, 128, tlv, {6, n}},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {quality_of_service_profile, 135, tlv, {5, n}},
            {traffic_flow_template, 137, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {bearer_control_mode, 184, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               nsapi => Nsapi
              };
decode_msg(update_pdp_context_response_sgsn, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {tunnel_endpoint_identifier_data_i, 16, tv, 4},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(update_pdp_context_response_ggsn, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {bearer_control_mode, 184, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(delete_pdp_context_request, Bin0) ->
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{cause, 1, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uli_timestamp, 214, tlv, 4},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               nsapi => Nsapi
              };
decode_msg(delete_pdp_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {uli_timestamp, 214, tlv, 4},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(error_indication, Bin0) ->
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, Bin1} = otc_l3_codec:decode_tv(Bin0, 4),
    {?GTPv1C_IEI_GTP_U_PEER_ADDRESS, GtpUPeerAddress, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    Opts = [{recovery_time_stamp, aa, tlv, {8, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => tunnel_management,
               tunnel_endpoint_identifier_data_i => TunnelEndpointIdentifierDataI,
               gtp_u_peer_address => GtpUPeerAddress
              };
decode_msg(pdu_notification_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{message_group => tunnel_management,
               imsi => Imsi,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName,
               gsn_address => GsnAddress
              };
decode_msg(pdu_notification_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(pdu_notification_reject_request, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName
              };
decode_msg(pdu_notification_reject_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(initiate_pdp_context_activation_request, Bin0) ->
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    {?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    {?GTPv1C_IEI_CORRELATION_ID, CorrelationId, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_group => tunnel_management,
               nsapi => Nsapi,
               quality_of_service_profile => QualityOfServiceProfile,
               correlation_id => CorrelationId
              };
decode_msg(initiate_pdp_context_activation_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => tunnel_management,
               cause => Cause
              };
decode_msg(send_routeing_information_for_gprs_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => location_management,
               imsi => Imsi
              };
decode_msg(send_routeing_information_for_gprs_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    {?GTPv1C_IEI_IMSI, Imsi, Bin2} = otc_l3_codec:decode_tv(Bin1, 8),
    Opts = [{map_cause, 11, tv, 1},
            {ms_not_reachable_reason, 29, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => location_management,
               cause => Cause,
               imsi => Imsi
              };
decode_msg(failure_report_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => location_management,
               imsi => Imsi
              };
decode_msg(failure_report_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{map_cause, 11, tv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => location_management,
               cause => Cause
              };
decode_msg(note_ms_gprs_present_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => location_management,
               imsi => Imsi,
               gsn_address => GsnAddress
              };
decode_msg(note_ms_gprs_present_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => location_management,
               cause => Cause
              };
decode_msg(identification_request, Bin0) ->
    {?GTPv1C_IEI_RAI, Rai, Bin1} = otc_l3_codec:decode_tv(Bin0, 6),
    {?GTPv1C_IEI_P_TMSI, PTmsi, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {hop_counter, 163, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => mobility_management,
               rai => Rai,
               p_tmsi => PTmsi
              };
decode_msg(identification_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{ue_usage_type, 217, tlv, 7},
            {iov_updates_counter, 222, tlv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(sgsn_context_request, Bin0) ->
    {?GTPv1C_IEI_RAI, Rai, Bin1} = otc_l3_codec:decode_tv(Bin0, 6),
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    Opts = [{ms_validated, 13, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {sgsn_number, 147, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {hop_counter, 163, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_group => mobility_management,
               rai => Rai,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               gsn_address => GsnAddress
              };
decode_msg(sgsn_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{radio_priority_sms, 23, tv, 1},
            {radio_priority, 24, tv, 1},
            {packet_flow_id, 25, tv, 2},
            {charging_characteristics, 26, tv, 2},
            {radio_priority_lcs, 150, tlv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {pdp_context_prioritization, 145, tlv, 0},
            {mbms_ue_context, 156, tlv, {13, n}},
            {rfsp_index, 189, tlv, 2},
            {rfsp_index, 189, tlv, 2},
            {fqdn, 190, tlv, {4, n}},
            {evolved_allocationretention_priority_ii, 192, tlv, 2},
            {extended_common_flags, 193, tlv, {4, n}},
            {ue_network_capability, 199, tlv, {4, n}},
            {ue_ambr, 200, tlv, {11, n}},
            {apn_ambr_with_nsapi, 201, tlv, 9},
            {signalling_priority_indication_with_nsapi, 204, tlv, 2},
            {higher_bitrates_than_16_mbps_flag, 205, tlv, 1},
            {selection_mode_with_nsapi, 213, tlv, 2},
            {lhn_id_with_nsapi, 215, tlv, {5, n}},
            {ue_usage_type, 217, tlv, 7},
            {extended_common_flags_ii, 218, tlv, 1},
            {scef_pdn_connection, 221, tlv, {5, n}},
            {iov_updates_counter, 222, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(sgsn_context_acknowledge, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{sgsn_number, 147, tlv, {4, n}},
            {node_identifier, 219, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(forward_relocation_request, Bin0) ->
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin1} = otc_l3_codec:decode_tv(Bin0, 4),
    {?GTPv1C_IEI_RANAP_CAUSE, RanapCause, Bin2} = otc_l3_codec:decode_tv(Bin1, 1),
    {?GTPv1C_IEI_MM_CONTEXT, MmContext, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_TARGET_IDENTIFICATION, TargetIdentification, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    {?GTPv1C_IEI_UTRAN_TRANSPARENT_CONTAINER, UtranTransparentContainer, Bin6} = otc_l3_codec:decode_tlv(Bin5),
    Opts = [{packet_flow_id, 25, tv, 2},
            {charging_characteristics, 26, tv, 2},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {pdp_context_prioritization, 145, tlv, 0},
            {mbms_ue_context, 156, tlv, {13, n}},
            {selected_plmn_id, 164, tlv, 3},
            {bss_container, 173, tlv, {4, n}},
            {cell_identification, 174, tlv, 17},
            {bssgp_cause, 176, tlv, 1},
            {ps_handover_xid_parameters, 180, tlv, {6, n}},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {reliable_inter_rat_handover_info, 188, tlv, 1},
            {rfsp_index, 189, tlv, 2},
            {rfsp_index, 189, tlv, 2},
            {fqdn, 190, tlv, {4, n}},
            {evolved_allocationretention_priority_ii, 192, tlv, 2},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_id, 196, tlv, 4},
            {cmi, 197, tlv, 1},
            {ue_network_capability, 199, tlv, {4, n}},
            {ue_ambr, 200, tlv, {11, n}},
            {apn_ambr_with_nsapi, 201, tlv, 9},
            {signalling_priority_indication_with_nsapi, 204, tlv, 2},
            {higher_bitrates_than_16_mbps_flag, 205, tlv, 1},
            {additional_mm_context_for_srvcc, 207, tlv, {5, n}},
            {additional_flags_for_srvcc, 208, tlv, 1},
            {stn_sr, 209, tlv, {5, n}},
            {c_msisdn, 210, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {enodeb_id, 212, tlv, {5, 13}},
            {selection_mode_with_nsapi, 213, tlv, 2},
            {ue_usage_type, 217, tlv, 7},
            {extended_common_flags_ii, 218, tlv, 1},
            {scef_pdn_connection, 221, tlv, {5, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{message_group => mobility_management,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               ranap_cause => RanapCause,
               mm_context => MmContext,
               gsn_address => GsnAddress,
               target_identification => TargetIdentification,
               utran_transparent_container => UtranTransparentContainer
              };
decode_msg(forward_relocation_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{tunnel_endpoint_identifier_data_ii, 18, tv, 5},
            {gsn_address, 133, tlv, {4, n}},
            {utran_transparent_container, 139, tlv, {4, n}},
            {sgsn_number, 147, tlv, {4, n}},
            {bss_container, 173, tlv, {4, n}},
            {bssgp_cause, 176, tlv, 1},
            {list_of_set_up_pfcs, 179, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {node_identifier, 219, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(forward_relocation_complete, Bin0) ->
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals#{message_group => mobility_management
              };
decode_msg(relocation_cancel_request, Bin0) ->
    Opts = [{extended_common_flags, 193, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals#{message_group => mobility_management
              };
decode_msg(relocation_cancel_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(forward_relocation_complete_acknowledge, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{charging_id, 127, tv, 4}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(forward_srns_context_acknowledge, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{charging_id, 127, tv, 4}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause
              };
decode_msg(forward_srns_context, Bin0) ->
    {?GTPv1C_IEI_RAB_CONTEXT, RabContext, Bin1} = otc_l3_codec:decode_tv(Bin0, 9),
    Opts = [{source_rnc_pdcp_context_info, 161, tlv, {4, n}},
            {pdu_numbers, 175, tlv, 9},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               rab_context => RabContext
              };
decode_msg(ran_information_relay, Bin0) ->
    {?GTPv1C_IEI_RAN_TRANSPARENT_CONTAINER, RanTransparentContainer, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    Opts = [{rim_routing_address, 158, tlv, {4, n}},
            {rim_routing_address_discriminator, 178, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               ran_transparent_container => RanTransparentContainer
              };
decode_msg(ue_registration_query_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mobility_management,
               imsi => Imsi
              };
decode_msg(ue_registration_query_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    {?GTPv1C_IEI_IMSI, Imsi, Bin2} = otc_l3_codec:decode_tv(Bin1, 8),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => mobility_management,
               cause => Cause,
               imsi => Imsi
              };
decode_msg(mbms_notification_request, Bin0) ->
    {?GTPv1C_IEI_IMSI, Imsi, Bin1} = otc_l3_codec:decode_tv(Bin0, 8),
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin3} = otc_l3_codec:decode_tv(Bin2, 1),
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin6} = otc_l3_codec:decode_tlv(Bin5),
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin6, Opts),
    Optionals#{message_group => mbms,
               imsi => Imsi,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               nsapi => Nsapi,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName,
               gsn_address => GsnAddress
              };
decode_msg(mbms_notification_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_notification_reject_request, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    {?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, Bin2} = otc_l3_codec:decode_tv(Bin1, 4),
    {?GTPv1C_IEI_NSAPI, Nsapi, Bin3} = otc_l3_codec:decode_tv(Bin2, 1),
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{message_group => mbms,
               cause => Cause,
               tunnel_endpoint_identifier_control_plane => TunnelEndpointIdentifierControlPlane,
               nsapi => Nsapi,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName
              };
decode_msg(mbms_notification_reject_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(create_mbms_context_request, Bin0) ->
    {?GTPv1C_IEI_RAI, Rai, Bin1} = otc_l3_codec:decode_tv(Bin0, 6),
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_ENHANCED_NSAPI, EnhancedNsapi, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    Opts = [{recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {imeisv, 154, tlv, 8},
            {mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {additional_trace_info, 162, tlv, 9},
            {additional_mbms_trace_info, 169, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{message_group => mbms,
               rai => Rai,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName,
               gsn_address => GsnAddress,
               enhanced_nsapi => EnhancedNsapi
              };
decode_msg(create_mbms_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(update_mbms_context_request, Bin0) ->
    {?GTPv1C_IEI_RAI, Rai, Bin1} = otc_l3_codec:decode_tv(Bin0, 6),
    {?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    {?GTPv1C_IEI_ENHANCED_NSAPI, EnhancedNsapi, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    Opts = [{recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {additional_trace_info, 162, tlv, 9},
            {additional_mbms_trace_info, 169, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{message_group => mbms,
               rai => Rai,
               gsn_address => GsnAddress,
               enhanced_nsapi => EnhancedNsapi
              };
decode_msg(update_mbms_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(delete_mbms_context_request, Bin0) ->
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals#{message_group => mbms
              };
decode_msg(delete_mbms_context_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_registration_request, Bin0) ->
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => mbms,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName
              };
decode_msg(mbms_registration_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_deregistration_request, Bin0) ->
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => mbms,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName
              };
decode_msg(mbms_deregistration_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_session_start_request, Bin0) ->
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    {?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_COMMON_FLAGS, CommonFlags, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_TMGI, Tmgi, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    {?GTPv1C_IEI_MBMS_SERVICE_AREA, MbmsServiceArea, Bin6} = otc_l3_codec:decode_tlv(Bin5),
    {?GTPv1C_IEI_MBMS_2G3G_INDICATOR, Mbms2g3gIndicator, Bin7} = otc_l3_codec:decode_tlv(Bin6),
    {?GTPv1C_IEI_MBMS_SESSION_DURATION, MbmsSessionDuration, Bin8} = otc_l3_codec:decode_tlv(Bin7),
    {?GTPv1C_IEI_MBMS_TIME_TO_DATA_TRANSFER, MbmsTimeToDataTransfer, Bin9} = otc_l3_codec:decode_tlv(Bin8),
    Opts = [{recovery, 14, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_session_identifier, 165, tlv, 1},
            {mbms_session_repetition_number, 170, tlv, 1},
            {mbms_flow_identifier, 185, tlv, {4, n}},
            {mbms_ip_multicast_distribution, 186, tlv, {9, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin9, Opts),
    Optionals#{message_group => mbms,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName,
               quality_of_service_profile => QualityOfServiceProfile,
               common_flags => CommonFlags,
               tmgi => Tmgi,
               mbms_service_area => MbmsServiceArea,
               mbms_2g3g_indicator => Mbms2g3gIndicator,
               mbms_session_duration => MbmsSessionDuration,
               mbms_time_to_data_transfer => MbmsTimeToDataTransfer
              };
decode_msg(mbms_session_start_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{recovery, 14, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_distribution_acknowledgement, 187, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_session_stop_request, Bin0) ->
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    Opts = [{mbms_flow_identifier, 185, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{message_group => mbms,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName
              };
decode_msg(mbms_session_stop_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(mbms_session_update_request, Bin0) ->
    {?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    {?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, Bin2} = otc_l3_codec:decode_tlv(Bin1),
    {?GTPv1C_IEI_TMGI, Tmgi, Bin3} = otc_l3_codec:decode_tlv(Bin2),
    {?GTPv1C_IEI_MBMS_SESSION_DURATION, MbmsSessionDuration, Bin4} = otc_l3_codec:decode_tlv(Bin3),
    {?GTPv1C_IEI_MBMS_SERVICE_AREA, MbmsServiceArea, Bin5} = otc_l3_codec:decode_tlv(Bin4),
    Opts = [{tunnel_endpoint_identifier_control_plane, 17, tv, 4},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_session_identifier, 165, tlv, 1},
            {mbms_session_repetition_number, 170, tlv, 1},
            {mbms_flow_identifier, 185, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin5, Opts),
    Optionals#{message_group => mbms,
               end_user_address => EndUserAddress,
               access_point_name => AccessPointName,
               tmgi => Tmgi,
               mbms_session_duration => MbmsSessionDuration,
               mbms_service_area => MbmsServiceArea
              };
decode_msg(mbms_session_update_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{tunnel_endpoint_identifier_data_i, 16, tv, 4},
            {tunnel_endpoint_identifier_control_plane, 17, tv, 4},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              };
decode_msg(ms_info_change_notification_request, Bin0) ->
    {?GTPv1C_IEI_RAT_TYPE, RatType, Bin1} = otc_l3_codec:decode_tlv(Bin0),
    Opts = [{nsapi, 20, tv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               rat_type => RatType
              };
decode_msg(ms_info_change_notification_response, Bin0) ->
    {?GTPv1C_IEI_CAUSE, Cause, Bin1} = otc_l3_codec:decode_tv(Bin0, 1),
    Opts = [{nsapi, 20, tv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{message_group => mbms,
               cause => Cause
              }.

encode_msg(echo_request, #{} = Msg) ->
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_msg(echo_response, #{recovery := Recovery} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RECOVERY, Recovery, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(version_not_supported, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_msg(supported_extension_headers_notification, #{extension_header_type_list := ExtensionHeaderTypeList} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_EXTENSION_HEADER_TYPE_LIST, ExtensionHeaderTypeList, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(create_pdp_context_request, #{tunnel_endpoint_identifier_data_i := TunnelEndpointIdentifierDataI,
                                         nsapi := Nsapi,
                                         quality_of_service_profile := QualityOfServiceProfile} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, 4, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, <<>>),
    Opts = [{rai, 3, tv, 6},
            {recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {camel_charging_information_container, 155, tlv, {4, n}},
            {additional_trace_info, 162, tlv, 9},
            {correlation_id, 183, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {ambr, 198, tlv, 8},
            {signalling_priority_indication, 203, tlv, 1},
            {cn_operator_selection_entity, 216, tlv, 1},
            {mapped_ue_usage_type, 223, tlv, 2},
            {up_function_selection_indication_flags, 224, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, OptBin/binary>>;
encode_msg(create_pdp_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {nsapi, 20, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {bearer_control_mode, 184, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {ggsn_back_off_time, 202, tlv, 1},
            {extended_common_flags_ii, 218, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(update_pdp_context_request_sgsn, #{tunnel_endpoint_identifier_data_i := TunnelEndpointIdentifierDataI,
                                              nsapi := Nsapi,
                                              quality_of_service_profile := QualityOfServiceProfile} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, 4, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, <<>>),
    Opts = [{imsi, 2, tv, 8},
            {rai, 3, tv, 6},
            {recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {traffic_flow_template, 137, tlv, {4, n}},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {additional_trace_info, 162, tlv, 9},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {ambr, 198, tlv, 8},
            {signalling_priority_indication, 203, tlv, 1},
            {cn_operator_selection_entity, 216, tlv, 1},
            {imeisv, 154, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, OptBin/binary>>;
encode_msg(update_pdp_context_request_ggsn, #{nsapi := Nsapi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Opts = [{imsi, 2, tv, 8},
            {recovery, 14, tv, 1},
            {end_user_address, 128, tlv, {6, n}},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {quality_of_service_profile, 135, tlv, {5, n}},
            {traffic_flow_template, 137, tlv, {4, n}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {bearer_control_mode, 184, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(update_pdp_context_response_sgsn, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {tunnel_endpoint_identifier_data_i, 16, tv, 4},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(update_pdp_context_response_ggsn, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {common_flags, 148, tlv, 1},
            {apn_restriction, 149, tlv, 1},
            {bearer_control_mode, 184, tlv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {ambr, 198, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(delete_pdp_context_request, #{nsapi := Nsapi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Opts = [{cause, 1, tv, 1},
            {protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uli_timestamp, 214, tlv, 4},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(delete_pdp_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {uli_timestamp, 214, tlv, 4},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(error_indication, #{tunnel_endpoint_identifier_data_i := TunnelEndpointIdentifierDataI,
                               gtp_u_peer_address := GtpUPeerAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_DATA_I, TunnelEndpointIdentifierDataI, 4, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GTP_U_PEER_ADDRESS, GtpUPeerAddress, <<>>),
    Opts = [{recovery_time_stamp, aa, tlv, {8, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(pdu_notification_request, #{imsi := Imsi,
                                       tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                       end_user_address := EndUserAddress,
                                       access_point_name := AccessPointName,
                                       gsn_address := GsnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, OptBin/binary>>;
encode_msg(pdu_notification_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(pdu_notification_reject_request, #{cause := Cause,
                                              tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                              end_user_address := EndUserAddress,
                                              access_point_name := AccessPointName} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, OptBin/binary>>;
encode_msg(pdu_notification_reject_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(initiate_pdp_context_activation_request, #{nsapi := Nsapi,
                                                      quality_of_service_profile := QualityOfServiceProfile,
                                                      correlation_id := CorrelationId} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_CORRELATION_ID, CorrelationId, <<>>),
    Opts = [{protocol_configuration_options, 132, tlv, {4, n}},
            {evolved_allocationretention_priority_i, 191, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, OptBin/binary>>;
encode_msg(initiate_pdp_context_activation_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(send_routeing_information_for_gprs_request, #{imsi := Imsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(send_routeing_information_for_gprs_response, #{cause := Cause,
                                                          imsi := Imsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Opts = [{map_cause, 11, tv, 1},
            {ms_not_reachable_reason, 29, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(failure_report_request, #{imsi := Imsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(failure_report_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{map_cause, 11, tv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(note_ms_gprs_present_request, #{imsi := Imsi,
                                           gsn_address := GsnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(note_ms_gprs_present_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(identification_request, #{rai := Rai,
                                     p_tmsi := PTmsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RAI, Rai, 6, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_P_TMSI, PTmsi, 4, <<>>),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {hop_counter, 163, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(identification_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{ue_usage_type, 217, tlv, 7},
            {iov_updates_counter, 222, tlv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(sgsn_context_request, #{rai := Rai,
                                   tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                   gsn_address := GsnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RAI, Rai, 6, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Opts = [{ms_validated, 13, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {sgsn_number, 147, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {hop_counter, 163, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, OptBin/binary>>;
encode_msg(sgsn_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{radio_priority_sms, 23, tv, 1},
            {radio_priority, 24, tv, 1},
            {packet_flow_id, 25, tv, 2},
            {charging_characteristics, 26, tv, 2},
            {radio_priority_lcs, 150, tlv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {pdp_context_prioritization, 145, tlv, 0},
            {mbms_ue_context, 156, tlv, {13, n}},
            {rfsp_index, 189, tlv, 2},
            {rfsp_index, 189, tlv, 2},
            {fqdn, 190, tlv, {4, n}},
            {evolved_allocationretention_priority_ii, 192, tlv, 2},
            {extended_common_flags, 193, tlv, {4, n}},
            {ue_network_capability, 199, tlv, {4, n}},
            {ue_ambr, 200, tlv, {11, n}},
            {apn_ambr_with_nsapi, 201, tlv, 9},
            {signalling_priority_indication_with_nsapi, 204, tlv, 2},
            {higher_bitrates_than_16_mbps_flag, 205, tlv, 1},
            {selection_mode_with_nsapi, 213, tlv, 2},
            {lhn_id_with_nsapi, 215, tlv, {5, n}},
            {ue_usage_type, 217, tlv, 7},
            {extended_common_flags_ii, 218, tlv, 1},
            {scef_pdn_connection, 221, tlv, {5, n}},
            {iov_updates_counter, 222, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(sgsn_context_acknowledge, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{sgsn_number, 147, tlv, {4, n}},
            {node_identifier, 219, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(forward_relocation_request, #{tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                         ranap_cause := RanapCause,
                                         mm_context := MmContext,
                                         gsn_address := GsnAddress,
                                         target_identification := TargetIdentification,
                                         utran_transparent_container := UtranTransparentContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RANAP_CAUSE, RanapCause, 1, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MM_CONTEXT, MmContext, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_TARGET_IDENTIFICATION, TargetIdentification, <<>>),
    Bin6 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_UTRAN_TRANSPARENT_CONTAINER, UtranTransparentContainer, <<>>),
    Opts = [{packet_flow_id, 25, tv, 2},
            {charging_characteristics, 26, tv, 2},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {pdp_context_prioritization, 145, tlv, 0},
            {mbms_ue_context, 156, tlv, {13, n}},
            {selected_plmn_id, 164, tlv, 3},
            {bss_container, 173, tlv, {4, n}},
            {cell_identification, 174, tlv, 17},
            {bssgp_cause, 176, tlv, 1},
            {ps_handover_xid_parameters, 180, tlv, {6, n}},
            {direct_tunnel_flags, 182, tlv, {4, n}},
            {reliable_inter_rat_handover_info, 188, tlv, 1},
            {rfsp_index, 189, tlv, 2},
            {rfsp_index, 189, tlv, 2},
            {fqdn, 190, tlv, {4, n}},
            {evolved_allocationretention_priority_ii, 192, tlv, 2},
            {extended_common_flags, 193, tlv, {4, n}},
            {csg_id, 196, tlv, 4},
            {cmi, 197, tlv, 1},
            {ue_network_capability, 199, tlv, {4, n}},
            {ue_ambr, 200, tlv, {11, n}},
            {apn_ambr_with_nsapi, 201, tlv, 9},
            {signalling_priority_indication_with_nsapi, 204, tlv, 2},
            {higher_bitrates_than_16_mbps_flag, 205, tlv, 1},
            {additional_mm_context_for_srvcc, 207, tlv, {5, n}},
            {additional_flags_for_srvcc, 208, tlv, 1},
            {stn_sr, 209, tlv, {5, n}},
            {c_msisdn, 210, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {enodeb_id, 212, tlv, {5, 13}},
            {selection_mode_with_nsapi, 213, tlv, 2},
            {ue_usage_type, 217, tlv, 7},
            {extended_common_flags_ii, 218, tlv, 1},
            {scef_pdn_connection, 221, tlv, {5, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, Bin6/binary, OptBin/binary>>;
encode_msg(forward_relocation_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{tunnel_endpoint_identifier_data_ii, 18, tv, 5},
            {gsn_address, 133, tlv, {4, n}},
            {utran_transparent_container, 139, tlv, {4, n}},
            {sgsn_number, 147, tlv, {4, n}},
            {bss_container, 173, tlv, {4, n}},
            {bssgp_cause, 176, tlv, 1},
            {list_of_set_up_pfcs, 179, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {node_identifier, 219, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(forward_relocation_complete, #{} = Msg) ->
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_msg(relocation_cancel_request, #{} = Msg) ->
    Opts = [{extended_common_flags, 193, tlv, {4, n}},
            {extended_ranap_cause, 211, tlv, 2},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_msg(relocation_cancel_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(forward_relocation_complete_acknowledge, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{charging_id, 127, tv, 4}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(forward_srns_context_acknowledge, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{charging_id, 127, tv, 4}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(forward_srns_context, #{rab_context := RabContext} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RAB_CONTEXT, RabContext, 9, <<>>),
    Opts = [{source_rnc_pdcp_context_info, 161, tlv, {4, n}},
            {pdu_numbers, 175, tlv, 9},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(ran_information_relay, #{ran_transparent_container := RanTransparentContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_RAN_TRANSPARENT_CONTAINER, RanTransparentContainer, <<>>),
    Opts = [{rim_routing_address, 158, tlv, {4, n}},
            {rim_routing_address_discriminator, 178, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(ue_registration_query_request, #{imsi := Imsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(ue_registration_query_response, #{cause := Cause,
                                             imsi := Imsi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(mbms_notification_request, #{imsi := Imsi,
                                        tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                        nsapi := Nsapi,
                                        end_user_address := EndUserAddress,
                                        access_point_name := AccessPointName,
                                        gsn_address := GsnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_IMSI, Imsi, 8, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin3 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Bin6 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, Bin6/binary, OptBin/binary>>;
encode_msg(mbms_notification_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_notification_reject_request, #{cause := Cause,
                                               tunnel_endpoint_identifier_control_plane := TunnelEndpointIdentifierControlPlane,
                                               nsapi := Nsapi,
                                               end_user_address := EndUserAddress,
                                               access_point_name := AccessPointName} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Bin2 = otc_l3_codec:encode_tv(?GTPv1C_IEI_TUNNEL_ENDPOINT_IDENTIFIER_CONTROL_PLANE, TunnelEndpointIdentifierControlPlane, 4, <<>>),
    Bin3 = otc_l3_codec:encode_tv(?GTPv1C_IEI_NSAPI, Nsapi, 1, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, OptBin/binary>>;
encode_msg(mbms_notification_reject_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(create_mbms_context_request, #{rai := Rai,
                                          end_user_address := EndUserAddress,
                                          access_point_name := AccessPointName,
                                          gsn_address := GsnAddress,
                                          enhanced_nsapi := EnhancedNsapi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RAI, Rai, 6, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ENHANCED_NSAPI, EnhancedNsapi, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {imeisv, 154, tlv, 8},
            {mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {additional_trace_info, 162, tlv, 9},
            {additional_mbms_trace_info, 169, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, OptBin/binary>>;
encode_msg(create_mbms_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(update_mbms_context_request, #{rai := Rai,
                                          gsn_address := GsnAddress,
                                          enhanced_nsapi := EnhancedNsapi} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_RAI, Rai, 6, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_GSN_ADDRESS, GsnAddress, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ENHANCED_NSAPI, EnhancedNsapi, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {trace_reference, 27, tv, 2},
            {trace_type, 28, tv, 2},
            {trigger_id, 142, tlv, {4, n}},
            {omc_identity, 143, tlv, {4, n}},
            {rat_type, 151, tlv, 1},
            {user_location_information, 152, tlv, {5, n}},
            {ms_time_zone, 153, tlv, 1},
            {additional_trace_info, 162, tlv, 9},
            {additional_mbms_trace_info, 169, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, OptBin/binary>>;
encode_msg(update_mbms_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {charging_gateway_address, 251, tlv, {4, 16}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(delete_mbms_context_request, #{} = Msg) ->
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_msg(delete_mbms_context_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{mbms_protocol_configuration_options, 159, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_registration_request, #{end_user_address := EndUserAddress,
                                        access_point_name := AccessPointName} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Opts = [{gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(mbms_registration_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_deregistration_request, #{end_user_address := EndUserAddress,
                                          access_point_name := AccessPointName} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(mbms_deregistration_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_session_start_request, #{end_user_address := EndUserAddress,
                                         access_point_name := AccessPointName,
                                         quality_of_service_profile := QualityOfServiceProfile,
                                         common_flags := CommonFlags,
                                         tmgi := Tmgi,
                                         mbms_service_area := MbmsServiceArea,
                                         mbms_2g3g_indicator := Mbms2g3gIndicator,
                                         mbms_session_duration := MbmsSessionDuration,
                                         mbms_time_to_data_transfer := MbmsTimeToDataTransfer} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_QUALITY_OF_SERVICE_PROFILE, QualityOfServiceProfile, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_COMMON_FLAGS, CommonFlags, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_TMGI, Tmgi, <<>>),
    Bin6 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_SERVICE_AREA, MbmsServiceArea, <<>>),
    Bin7 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_2G3G_INDICATOR, Mbms2g3gIndicator, <<>>),
    Bin8 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_SESSION_DURATION, MbmsSessionDuration, <<>>),
    Bin9 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_TIME_TO_DATA_TRANSFER, MbmsTimeToDataTransfer, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_session_identifier, 165, tlv, 1},
            {mbms_session_repetition_number, 170, tlv, 1},
            {mbms_flow_identifier, 185, tlv, {4, n}},
            {mbms_ip_multicast_distribution, 186, tlv, {9, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, Bin6/binary, Bin7/binary, Bin8/binary, Bin9/binary, OptBin/binary>>;
encode_msg(mbms_session_start_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{recovery, 14, tv, 1},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_distribution_acknowledgement, 187, tlv, 1},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_session_stop_request, #{end_user_address := EndUserAddress,
                                        access_point_name := AccessPointName} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Opts = [{mbms_flow_identifier, 185, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, OptBin/binary>>;
encode_msg(mbms_session_stop_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(mbms_session_update_request, #{end_user_address := EndUserAddress,
                                          access_point_name := AccessPointName,
                                          tmgi := Tmgi,
                                          mbms_session_duration := MbmsSessionDuration,
                                          mbms_service_area := MbmsServiceArea} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_END_USER_ADDRESS, EndUserAddress, <<>>),
    Bin2 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_ACCESS_POINT_NAME, AccessPointName, <<>>),
    Bin3 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_TMGI, Tmgi, <<>>),
    Bin4 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_SESSION_DURATION, MbmsSessionDuration, <<>>),
    Bin5 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_MBMS_SERVICE_AREA, MbmsServiceArea, <<>>),
    Opts = [{tunnel_endpoint_identifier_control_plane, 17, tv, 4},
            {gsn_address, 133, tlv, {4, n}},
            {mbms_session_identifier, 165, tlv, 1},
            {mbms_session_repetition_number, 170, tlv, 1},
            {mbms_flow_identifier, 185, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, OptBin/binary>>;
encode_msg(mbms_session_update_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{tunnel_endpoint_identifier_data_i, 16, tv, 4},
            {tunnel_endpoint_identifier_control_plane, 17, tv, 4},
            {gsn_address, 133, tlv, {4, n}},
            {gsn_address, 133, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(ms_info_change_notification_request, #{rat_type := RatType} = Msg) ->
    Bin1 = otc_l3_codec:encode_tlv(?GTPv1C_IEI_RAT_TYPE, RatType, <<>>),
    Opts = [{nsapi, 20, tv, 1},
            {extended_common_flags, 193, tlv, {4, n}},
            {uci, 194, tlv, 8},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>;
encode_msg(ms_info_change_notification_response, #{cause := Cause} = Msg) ->
    Bin1 = otc_l3_codec:encode_tv(?GTPv1C_IEI_CAUSE, Cause, 1, <<>>),
    Opts = [{nsapi, 20, tv, 1},
            {ms_info_change_reporting_action, 181, tlv, 1},
            {csg_information_reporting_action, 195, tlv, {4, n}},
            {private_extension, 255, tlv, {6, n}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/binary, OptBin/binary>>.
