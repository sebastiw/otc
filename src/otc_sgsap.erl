-module(otc_sgsap).
-behaviour(otc_codec).

-include("include/sgsap.hrl").
-include("include/l3.hrl").

-export([spec/0,
         next/1,
         codec/2,
         decode/1,
         encode/1
        ]).

spec() ->
    "3GPP TS 29.118 V17.0.0".

next(_) ->
    '$stop'.

codec(Bin, _Opts) when is_binary(Bin) ->
    decode(Bin);
codec(Msg, _Opts) when is_map(Msg) ->
    encode({Msg, <<>>});
codec({Msg, PDU}, _Opts) ->
    encode({Msg, PDU}).

decode(<<MT:8, Bin1/binary>>) ->
    IEIs = decode_ieis(Bin1),
    MessageType = parse_msg_type(MT),
    M = decode_msg(MessageType, IEIs),
    M#{message_type => MessageType
      }.

encode({Msg, PDU}) ->
    M = encode(Msg),
    <<M/binary, PDU/binary>>;
encode(#{message_type := MessageType} = Msg) ->
    MT = compose_msg_type(MessageType),
    M = encode_msg(MessageType, Msg),
    <<MT:8, M/binary>>.

parse_msg_type(?SGSAP_MSGT_PAGING_REQUEST) -> paging_request;
parse_msg_type(?SGSAP_MSGT_PAGING_REJECT) -> paging_reject;
parse_msg_type(?SGSAP_MSGT_SERVICE_REQUEST) -> service_request;
parse_msg_type(?SGSAP_MSGT_DOWNLINK_UNITDATA) -> downlink_unitdata;
parse_msg_type(?SGSAP_MSGT_UPLINK_UNITDATA) -> uplink_unitdata;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_REQUEST) -> location_update_request;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_ACCEPT) -> location_update_accept;
parse_msg_type(?SGSAP_MSGT_LOCATION_UPDATE_REJECT) -> location_update_reject;
parse_msg_type(?SGSAP_MSGT_TMSI_REALLOCATION_COMPLETE) -> tmsi_reallocation_complete;
parse_msg_type(?SGSAP_MSGT_ALERT_REQUEST) -> alert_request;
parse_msg_type(?SGSAP_MSGT_ALERT_ACK) -> alert_ack;
parse_msg_type(?SGSAP_MSGT_ALERT_REJECT) -> alert_reject;
parse_msg_type(?SGSAP_MSGT_UE_ACTIVITY_INDICATION) -> ue_activity_indication;
parse_msg_type(?SGSAP_MSGT_EPS_DETACH_INDICATION) -> eps_detach_indication;
parse_msg_type(?SGSAP_MSGT_EPS_DETACH_ACK) -> eps_detach_ack;
parse_msg_type(?SGSAP_MSGT_IMSI_DETACH_INDICATION) -> imsi_detach_indication;
parse_msg_type(?SGSAP_MSGT_IMSI_DETACH_ACK) -> imsi_detach_ack;
parse_msg_type(?SGSAP_MSGT_RESET_INDICATION) -> reset_indication;
parse_msg_type(?SGSAP_MSGT_RESET_ACK) -> reset_ack;
parse_msg_type(?SGSAP_MSGT_SERVICE_ABORT_REQUEST) -> service_abort_request;
parse_msg_type(?SGSAP_MSGT_MO_CSFB_INDICATION) -> mo_csfb_indication;
parse_msg_type(?SGSAP_MSGT_MM_INFORMATION_REQUEST) -> mm_information_request;
parse_msg_type(?SGSAP_MSGT_RELEASE_REQUEST) -> release_request;
parse_msg_type(?SGSAP_MSGT_STATUS) -> status;
parse_msg_type(?SGSAP_MSGT_UE_UNREACHABLE) -> ue_unreachable.

compose_msg_type(paging_request) -> ?SGSAP_MSGT_PAGING_REQUEST;
compose_msg_type(paging_reject) -> ?SGSAP_MSGT_PAGING_REJECT;
compose_msg_type(service_request) -> ?SGSAP_MSGT_SERVICE_REQUEST;
compose_msg_type(downlink_unitdata) -> ?SGSAP_MSGT_DOWNLINK_UNITDATA;
compose_msg_type(uplink_unitdata) -> ?SGSAP_MSGT_UPLINK_UNITDATA;
compose_msg_type(location_update_request) -> ?SGSAP_MSGT_LOCATION_UPDATE_REQUEST;
compose_msg_type(location_update_accept) -> ?SGSAP_MSGT_LOCATION_UPDATE_ACCEPT;
compose_msg_type(location_update_reject) -> ?SGSAP_MSGT_LOCATION_UPDATE_REJECT;
compose_msg_type(tmsi_reallocation_complete) -> ?SGSAP_MSGT_TMSI_REALLOCATION_COMPLETE;
compose_msg_type(alert_request) -> ?SGSAP_MSGT_ALERT_REQUEST;
compose_msg_type(alert_ack) -> ?SGSAP_MSGT_ALERT_ACK;
compose_msg_type(alert_reject) -> ?SGSAP_MSGT_ALERT_REJECT;
compose_msg_type(ue_activity_indication) -> ?SGSAP_MSGT_UE_ACTIVITY_INDICATION;
compose_msg_type(eps_detach_indication) -> ?SGSAP_MSGT_EPS_DETACH_INDICATION;
compose_msg_type(eps_detach_ack) -> ?SGSAP_MSGT_EPS_DETACH_ACK;
compose_msg_type(imsi_detach_indication) -> ?SGSAP_MSGT_IMSI_DETACH_INDICATION;
compose_msg_type(imsi_detach_ack) -> ?SGSAP_MSGT_IMSI_DETACH_ACK;
compose_msg_type(reset_indication) -> ?SGSAP_MSGT_RESET_INDICATION;
compose_msg_type(reset_ack) -> ?SGSAP_MSGT_RESET_ACK;
compose_msg_type(service_abort_request) -> ?SGSAP_MSGT_SERVICE_ABORT_REQUEST;
compose_msg_type(mo_csfb_indication) -> ?SGSAP_MSGT_MO_CSFB_INDICATION;
compose_msg_type(mm_information_request) -> ?SGSAP_MSGT_MM_INFORMATION_REQUEST;
compose_msg_type(release_request) -> ?SGSAP_MSGT_RELEASE_REQUEST;
compose_msg_type(status) -> ?SGSAP_MSGT_STATUS;
compose_msg_type(ue_unreachable) -> ?SGSAP_MSGT_UE_UNREACHABLE.

decode_ieis(<<>>) ->
    [];
decode_ieis(<<IEI:8, L:8, V:L/binary, R/binary>>) ->
    [{IEI, L, decode_iei(IEI, V)}|decode_ieis(R)].

decode_iei(?SGSNAP_IEI_IMSI, V) ->
    %% 3GPP TS 29.018
    {imsi, Ds} = decode_mobile_identity(V),
    Ds;
decode_iei(?SGSNAP_IEI_VLR_NAME, V) ->
    %% 3GPP TS 23.003
    otc_gtpv2c:decode_apn(V);
decode_iei(?SGSNAP_IEI_TMSI, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, V) ->
    %% 3GPP TS 24.008
    otc_gtpv2c:decode_lai(V);
decode_iei(?SGSNAP_IEI_CHANNEL_NEEDED, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_EMLPP_PRIORITY, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_TMSI_STATUS, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_SGS_CAUSE, V) ->
    case V of
        <<2#0000_0001>> -> imsi_detached_for_eps_services;
        <<2#0000_0010>> -> imsi_detached_for_eps_and_non_eps_services;
        <<2#0000_0011>> -> imsi_unknown;
        <<2#0000_0100>> -> imsi_detached_for_non_eps_services;
        <<2#0000_0101>> -> imsi_implicitly_detached_for_non_eps_services;
        <<2#0000_0110>> -> ue_unreachable;
        <<2#0000_0111>> -> message_not_compatible_with_the_protocol_state;
        <<2#0000_1000>> -> missing_mandatory_information_element;
        <<2#0000_1001>> -> invalid_mandatory_information;
        <<2#0000_1010>> -> conditional_information_element_error;
        <<2#0000_1011>> -> semantically_incorrect_message;
        <<2#0000_1100>> -> message_unknown;
        <<2#0000_1101>> -> mobile_terminating_cs_fallback_call_rejected_by_the_user;
        <<2#0000_1110>> -> ue_temporarily_unreachable;
        _ -> {normal, V}
    end;
decode_iei(?SGSNAP_IEI_MME_NAME, V) ->
    %% 3GPP TS 23.003
    otc_gtpv2c:decode_apn(V);
decode_iei(?SGSNAP_IEI_EPS_LOCATION_UPDATE_TYPE, V) ->
    case V of
        <<2#0000_0001>> -> imsi_attach;
        _ -> normal_location_update
    end;
decode_iei(?SGSNAP_IEI_GLOBAL_CNID, V) ->
    V;
decode_iei(?SGSNAP_IEI_MOBILE_IDENTITY, V) ->
    %% 3GPP TS 29.018
    decode_mobile_identity(V);
decode_iei(?SGSNAP_IEI_REJECT_CAUSE, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE, V) ->
    case V of
        <<2#0000_0001>> -> network_initiated;
        <<2#0000_0010>> -> ue_initiated;
        <<2#0000_0011>> -> eps_not_allowed
    end;
decode_iei(?SGSNAP_IEI_IMSI_DETACH_FROM_NONEPS_SERVICE_TYPE, V) ->
    case V of
        <<2#0000_0001>> -> explicit_ue_initiated;
        <<2#0000_0010>> -> combined_ue_initiated;
        <<2#0000_0011>> -> implicit_network_initiated
    end;
decode_iei(?SGSNAP_IEI_IMEISV, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_NAS_MESSAGE_CONTAINER, V) ->
    %% 3GPP TS 24.011 (CP-DATA, CP-ACK or CP-ERROR)
    decode_nas_message(V);
decode_iei(?SGSNAP_IEI_MM_INFORMATION, V) ->
    %% 3GPP TS 29.018
    IEIs = [{full_name, 16#43, tlv, {3, n}},
            {short_name, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_time_zone, 16#47, tv, 8},
            {lsa_identity, 16#48, tlv, {2, 5}},
            {daylight_saving_time, 16#49, tlv, 3}
           ],
    {Dec, <<>>} = otc_l3_codec:decode_iei_list(V, IEIs),
    maps:fold(fun (full_name = K, Val, Acc) ->
                      Acc#{K => otc_util:decode_network_name(Val)};
                  (short_name = K, Val, Acc) ->
                      Acc#{K => otc_util:decode_network_name(Val)};
                  (universal_time_and_time_zone = K, Val, Acc) ->
                      [Y1,Y2,Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2,S1,S2,Tz1,Tz2] = otc_util:decode_tbcd(Val),
                      Y = list_to_integer([Y1,Y2]),
                      Mo = list_to_integer([Mo1,Mo2]),
                      D = list_to_integer([D1,D2]),
                      H = list_to_integer([H1,H2]),
                      Mi = list_to_integer([Mi1,Mi2]),
                      S = list_to_integer([S1,S2]),
                      Tz = list_to_integer([Tz1,Tz2]),
                      Acc#{K => #{datetime => {{Y+2000, Mo, D}, {H, Mi, S}},
                                  timezone => Tz}};
                  (K, Val, Acc) ->
                      Acc#{K => Val}
              end, #{}, Dec);
decode_iei(?SGSNAP_IEI_ERRONEOUS_MESSAGE, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_CLI, V) ->
    %% 3GPP TS 24.008
    V;
decode_iei(?SGSNAP_IEI_LCS_CLIENT_IDENTITY, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_LCS_INDICATOR, V) ->
    case V of
        <<2#0000_0001>> -> mt_lr;
        _ -> {normal, V}
    end;
decode_iei(?SGSNAP_IEI_SS_CODE, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_SERVICE_INDICATOR, V) ->
    case V of
        <<2#0000_0010>> -> sms;
        _ -> cs_call
    end;
decode_iei(?SGSNAP_IEI_UE_TIME_ZONE, V) ->
    %% 3GPP TS 24.008
    V;
decode_iei(?SGSNAP_IEI_MOBILE_STATION_CLASSMARK_2, V) ->
    %% 3GPP TS 24.008
    V;
decode_iei(?SGSNAP_IEI_TRACKING_AREA_IDENTITY, V) ->
    %% 3GPP TS 24.301
    V;
decode_iei(?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, V) ->
    %% 3GPP TS TS 29.018
    V;
decode_iei(?SGSNAP_IEI_UE_EMM_MODE, V) ->
    case V of
        <<2#0000_0000>> -> emm_idle;
        <<2#0000_0001>> -> emm_connected
    end;
decode_iei(?SGSNAP_IEI_ADDITIONAL_PAGING_INDICATORS, V) ->
    case V of
        <<2#0000_0000>> -> csri_not_set;
        <<2#0000_0001>> -> csri_set
    end;
decode_iei(?SGSNAP_IEI_TMSI_BASED_NRI_CONTAINER, V) ->
    %% 3GPP TS 29.018
    V;
decode_iei(?SGSNAP_IEI_SELECTED_CS_DOMAIN_OPERATOR, V) ->
    %% 3GPP TS 24.008
    V;
decode_iei(?SGSNAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_SM_DELIVERY_TIMER, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_SM_DELIVERY_START_TIME, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS, V) ->
    case V of
        <<2#0000_0000>> -> smbri_not_set;
        <<2#0000_0001>> -> smbri_set
    end;
decode_iei(?SGSNAP_IEI_MAXIMUM_RETRANSMISSION_TIME, V) ->
    %% 3GPP TS 29.002
    V;
decode_iei(?SGSNAP_IEI_REQUESTED_RETRANSMISSION_TIME, V) ->
    %% 3GPP TS 29.002
    V.

encode_iei(?SGSNAP_IEI_IMSI, V) ->
    %% 3GPP TS 29.018
    encode_mobile_identity({imsi, V});
encode_iei(?SGSNAP_IEI_VLR_NAME, V) ->
    %% 3GPP TS 23.003
    otc_gtpv2c:encode_apn(V);
encode_iei(?SGSNAP_IEI_TMSI, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, V) ->
    %% 3GPP TS 24.008
    otc_gtpv2c:encode_lai(V);
encode_iei(?SGSNAP_IEI_CHANNEL_NEEDED, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_EMLPP_PRIORITY, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_TMSI_STATUS, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_SGS_CAUSE, V) ->
    case V of
        imsi_detached_for_eps_services -> <<2#0000_0001>>;
        imsi_detached_for_eps_and_non_eps_services -> <<2#0000_0010>>;
        imsi_unknown -> <<2#0000_0011>>;
        imsi_detached_for_non_eps_services -> <<2#0000_0100>>;
        imsi_implicitly_detached_for_non_eps_services -> <<2#0000_0101>>;
        ue_unreachable -> <<2#0000_0110>>;
        message_not_compatible_with_the_protocol_state -> <<2#0000_0111>>;
        missing_mandatory_information_element -> <<2#0000_1000>>;
        invalid_mandatory_information -> <<2#0000_1001>>;
        conditional_information_element_error -> <<2#0000_1010>>;
        semantically_incorrect_message -> <<2#0000_1011>>;
        message_unknown -> <<2#0000_1100>>;
        mobile_terminating_cs_fallback_call_rejected_by_the_user -> <<2#0000_1101>>;
        ue_temporarily_unreachable -> <<2#0000_1110>>;
        {normal, B} -> B
    end;
encode_iei(?SGSNAP_IEI_MME_NAME, V) ->
    %% 3GPP TS 23.003
    otc_gtpv2c:encode_apn(V);
encode_iei(?SGSNAP_IEI_EPS_LOCATION_UPDATE_TYPE, V) ->
    case V of
        imsi_attach -> <<2#0000_0001>>;
        normal_location_update -> <<2#0000_0010>>
    end;
encode_iei(?SGSNAP_IEI_GLOBAL_CNID, V) ->
    V;
encode_iei(?SGSNAP_IEI_MOBILE_IDENTITY, V) ->
    %% 3GPP TS 29.018
    encode_mobile_identity(V);
encode_iei(?SGSNAP_IEI_REJECT_CAUSE, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE, V) ->
    case V of
        network_initiated -> <<2#0000_0001>>;
        ue_initiated -> <<2#0000_0010>>;
        eps_not_allowed -> <<2#0000_0011>>
    end;
encode_iei(?SGSNAP_IEI_IMSI_DETACH_FROM_NONEPS_SERVICE_TYPE, V) ->
    case V of
        explicit_ue_initiated -> <<2#0000_0001>>;
        combined_ue_initiated -> <<2#0000_0010>>;
        implicit_network_initiated -> <<2#0000_0011>>
    end;
encode_iei(?SGSNAP_IEI_IMEISV, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_NAS_MESSAGE_CONTAINER, V) ->
    %% 3GPP TS 24.011
    encode_nas_message(V);
encode_iei(?SGSNAP_IEI_MM_INFORMATION, V) ->
    %% 3GPP TS 29.018
    IEIs = [{full_name, 16#43, tlv, {3, n}},
            {short_name, 16#45, tlv, {3, n}},
            {local_time_zone, 16#46, tv, 2},
            {universal_time_and_time_zone, 16#47, tv, 8},
            {lsa_identity, 16#48, tlv, {2, 5}},
            {daylight_saving_time, 16#49, tlv, 3}
           ],
    M = maps:fold(fun (full_name = K, Val, Acc) ->
                          Acc#{K => otc_util:encode_network_name(Val)};
                      (short_name = K, Val, Acc) ->
                          Acc#{K => otc_util:encode_network_name(Val)};
                      (universal_time_and_time_zone = K, Map, Acc) ->
                          #{datetime := {{Y, Mo, D}, {H, Mi, S}},
                            timezone := Tz} = Map,
                          [Y1,Y2,Mo1,Mo2,D1,D2] = lists:flatten(io_lib:format("~2..0w~2..0w~2..0w",[Y-2000,Mo,D])),
                          [H1,H2,Mi1,Mi2,S1,S2] = lists:flatten(io_lib:format("~2..0w~2..0w~2..0w",[H,Mi,S])),
                          [Tz1,Tz2] = lists:flatten(io_lib:format("~2..0w",[Tz])),
                          Val = otc_util:encode_tbcd([Y1,Y2,Mo1,Mo2,D1,D2,H1,H2,Mi1,Mi2,S1,S2,Tz1,Tz2]),
                          Acc#{K => Val};
                      (K, Val, Acc) ->
                          Acc#{K => Val}
                  end, #{}, V),
    otc_l3_codec:encode_iei_list(M, IEIs);
encode_iei(?SGSNAP_IEI_ERRONEOUS_MESSAGE, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_CLI, V) ->
    %% 3GPP TS 24.008
    V;
encode_iei(?SGSNAP_IEI_LCS_CLIENT_IDENTITY, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_LCS_INDICATOR, V) ->
    case V of
        mt_lr -> <<2#0000_0001>>;
        {normal, B} -> B
    end;
encode_iei(?SGSNAP_IEI_SS_CODE, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_SERVICE_INDICATOR, V) ->
    case V of
        sms -> <<2#0000_0010>>;
        cs_call -> <<2#0000_0001>>
    end;
encode_iei(?SGSNAP_IEI_UE_TIME_ZONE, V) ->
    %% 3GPP TS 24.008
    V;
encode_iei(?SGSNAP_IEI_MOBILE_STATION_CLASSMARK_2, V) ->
    %% 3GPP TS 24.008
    V;
encode_iei(?SGSNAP_IEI_TRACKING_AREA_IDENTITY, V) ->
    %% 3GPP TS 24.301
    V;
encode_iei(?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, V) ->
    %% 3GPP TS TS 29.018
    V;
encode_iei(?SGSNAP_IEI_UE_EMM_MODE, V) ->
    case V of
        emm_idle -> <<2#0000_0000>>;
        emm_connected -> <<2#0000_0001>>
    end;
encode_iei(?SGSNAP_IEI_ADDITIONAL_PAGING_INDICATORS, V) ->
    case V of
        csri_not_set -> <<2#0000_0000>>;
        csri_set -> <<2#0000_0001>>
    end;
encode_iei(?SGSNAP_IEI_TMSI_BASED_NRI_CONTAINER, V) ->
    %% 3GPP TS 29.018
    V;
encode_iei(?SGSNAP_IEI_SELECTED_CS_DOMAIN_OPERATOR, V) ->
    %% 3GPP TS 24.008
    V;
encode_iei(?SGSNAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_SM_DELIVERY_TIMER, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_SM_DELIVERY_START_TIME, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS, V) ->
    case V of
        smbri_not_set -> <<2#0000_0000>>;
        smbri_set -> <<2#0000_0001>>
    end;
encode_iei(?SGSNAP_IEI_MAXIMUM_RETRANSMISSION_TIME, V) ->
    %% 3GPP TS 29.002
    V;
encode_iei(?SGSNAP_IEI_REQUESTED_RETRANSMISSION_TIME, V) ->
    %% 3GPP TS 29.002
    V.

allowed_ieis(downlink_unitdata) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {nas_message_container, ?SGSNAP_IEI_NAS_MESSAGE_CONTAINER, mandatory, {2, 251}}];
allowed_ieis(release_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {sgs_cause, ?SGSNAP_IEI_SGS_CAUSE, optional, 1}];
allowed_ieis(uplink_unitdata) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {nas_message_container, ?SGSNAP_IEI_NAS_MESSAGE_CONTAINER, mandatory, {2, 251}},
     {imeisv, ?SGSNAP_IEI_IMEISV, optional, 8},
     {ue_time_zone, ?SGSNAP_IEI_UE_TIME_ZONE, optional, 1},
     {mobile_station_classmark_2, ?SGSNAP_IEI_MOBILE_STATION_CLASSMARK_2, optional, 3},
     {tai, ?SGSNAP_IEI_TRACKING_AREA_IDENTITY, optional, 5},
     {ecgi, ?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, optional, 7}];
allowed_ieis(ue_unreachable) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {sgs_cause, ?SGSNAP_IEI_SGS_CAUSE, mandatory, 1},
     {requested_retransmission_time, ?SGSNAP_IEI_REQUESTED_RETRANSMISSION_TIME, optional, 4},
     {additional_ue_unreachable_indicators, ?SGSNAP_IEI_ADDITIONAL_UE_UNREACHABLE_INDICATORS, optional, 1}];
allowed_ieis(ue_activity_indication) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {maximum_ue_availability_time, ?SGSNAP_IEI_MAXIMUM_UE_AVAILABILITY_TIME, optional, 4}];
allowed_ieis(status) ->
    [{imsi, ?SGSNAP_IEI_IMSI, optional, {4, 8}},
     {sgs_cause, ?SGSNAP_IEI_SGS_CAUSE, mandatory, 1},
     {erroneous_message, ?SGSNAP_IEI_ERRONEOUS_MESSAGE, mandatory, {1, n}}];
allowed_ieis(location_update_accept) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {location_area_identifier, ?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, mandatory, 5},
     {new_tmsi_or_imsi, ?SGSNAP_IEI_MOBILE_IDENTITY, optional, {4, 8}}];
allowed_ieis(imsi_detach_ack) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(tmsi_reallocation_complete) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(eps_detach_indication) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {mme_name, ?SGSNAP_IEI_MME_NAME, mandatory, 55},
     {imsi_detach_from_eps_service_type, ?SGSNAP_IEI_IMSI_DETACH_FROM_EPS_SERVICE_TYPE, mandatory, 1}];
allowed_ieis(service_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {service_indicator, ?SGSNAP_IEI_SERVICE_INDICATOR, mandatory, 1},
     {imeisv, ?SGSNAP_IEI_IMEISV, optional, 8},
     {ue_time_zone, ?SGSNAP_IEI_UE_TIME_ZONE, optional, 1},
     {mobile_station_classmark_2, ?SGSNAP_IEI_MOBILE_STATION_CLASSMARK_2, optional, 3},
     {tai_tracking, ?SGSNAP_IEI_TRACKING_AREA_IDENTITY, optional, 5},
     {ecgi_e_utran, ?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, optional, 7},
     {ue_emm_mode, ?SGSNAP_IEI_UE_EMM_MODE, optional, 1}];
allowed_ieis(eps_detach_ack) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(alert_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(alert_ack) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(service_abort_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}}];
allowed_ieis(location_update_reject) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {reject_cause, ?SGSNAP_IEI_REJECT_CAUSE, mandatory, 1},
     {location_area_identifier, ?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, mandatory, 5}];
allowed_ieis(alert_reject) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {sgs_cause, ?SGSNAP_IEI_SGS_CAUSE, mandatory, 1}];
allowed_ieis(paging_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {vlr_name, ?SGSNAP_IEI_VLR_NAME, mandatory, {1, n}},
     {service_indicator, ?SGSNAP_IEI_SERVICE_INDICATOR, mandatory, 1},
     {tmsi, ?SGSNAP_IEI_TMSI, optional, 4},
     {cli, ?SGSNAP_IEI_CLI, optional, {1, 12}},
     {location_area_identifier, ?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, optional, 5},
     {global_cnid, ?SGSNAP_IEI_GLOBAL_CNID, optional, 5},
     {ss_code, ?SGSNAP_IEI_SS_CODE, optional, 1},
     {lcs_indicator, ?SGSNAP_IEI_LCS_INDICATOR, optional, 1},
     {lcs_client_identity, ?SGSNAP_IEI_LCS_CLIENT_IDENTITY, optional, {1, n}},
     {channel_needed, ?SGSNAP_IEI_CHANNEL_NEEDED, optional, 1},
     {emlpp_priority, ?SGSNAP_IEI_EMLPP_PRIORITY, optional, 1},
     {additional_paging_indicators, ?SGSNAP_IEI_ADDITIONAL_PAGING_INDICATORS, optional, 1},
     {sm_delivery_timer, ?SGSNAP_IEI_SM_DELIVERY_TIMER, optional, 2},
     {sm_delivery_start_time, ?SGSNAP_IEI_SM_DELIVERY_START_TIME, optional, 4},
     {maximum_retransmission_time, ?SGSNAP_IEI_MAXIMUM_RETRANSMISSION_TIME, optional, 4}];
allowed_ieis(paging_reject) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {sgs_cause, ?SGSNAP_IEI_SGS_CAUSE, mandatory, 1}];
allowed_ieis(mm_information_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {mm_information, ?SGSNAP_IEI_MM_INFORMATION, mandatory, {1, n}}];
allowed_ieis(reset_indication) ->
    [{mme_name, ?SGSNAP_IEI_MME_NAME, conditional, 55},
     {vlr_name, ?SGSNAP_IEI_VLR_NAME, conditional, {1, n}}];
allowed_ieis(location_update_request) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {mme_name, ?SGSNAP_IEI_MME_NAME, mandatory, 55},
     {eps_location_update_type, ?SGSNAP_IEI_EPS_LOCATION_UPDATE_TYPE, mandatory, 1},
     {new_location_area_identifier, ?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, mandatory, 5},
     {old_location_area_identifier, ?SGSNAP_IEI_LOCATION_AREA_IDENTIFIER, optional, 5},
     {tmsi_status, ?SGSNAP_IEI_TMSI_STATUS, optional, 1},
     {imeisv, ?SGSNAP_IEI_IMEISV, optional, 8},
     {tai, ?SGSNAP_IEI_TRACKING_AREA_IDENTITY, optional, 5},
     {ecgi, ?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, optional, 7},
     {tmsi_based_nri_container, ?SGSNAP_IEI_TMSI_BASED_NRI_CONTAINER, optional, 2},
     {selected_cs_domain_operator, ?SGSNAP_IEI_SELECTED_CS_DOMAIN_OPERATOR, optional, 3}];
allowed_ieis(mo_csfb_indication) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {tai, ?SGSNAP_IEI_TRACKING_AREA_IDENTITY, optional, 5},
     {ecgi, ?SGSNAP_IEI_EUTRAN_CELL_GLOBAL_IDENTITY, optional, 7}];
allowed_ieis(reset_ack) ->
    [{mme_name, ?SGSNAP_IEI_MME_NAME, conditional, 55},
     {vlr_name, ?SGSNAP_IEI_VLR_NAME, conditional, {1, n}}];
allowed_ieis(imsi_detach_indication) ->
    [{imsi, ?SGSNAP_IEI_IMSI, mandatory, {4, 8}},
     {mme_name, ?SGSNAP_IEI_MME_NAME, mandatory, 55},
     {imsi_detach_from_noneps_service_type, ?SGSNAP_IEI_IMSI_DETACH_FROM_NONEPS_SERVICE_TYPE, mandatory, 1}].

decode_msg(MT, IEIs) ->
    Allowed = allowed_ieis(MT),
    parse_ieis(Allowed, IEIs).

encode_msg(MT, Msg) ->
    Allowed = allowed_ieis(MT),
    compose_ieis(Allowed, Msg).

parse_ieis(Allowed, IEIs) ->
    parse_ieis(Allowed, IEIs, #{}).

parse_ieis([], _IEIs, Acc) ->
    Acc;
parse_ieis(Allowed, [], Acc) ->
    [] = [N || {N, _, mandatory, _} <- Allowed],
    Acc;
parse_ieis(Allowed, [{IEI, L, V}|R], Acc) ->
    case lists:keytake(IEI, 2, Allowed) of
        false ->
            parse_ieis(Allowed, R, Acc);
        {value, {Name, _, _, {MinL, MaxL}}, All} when L < MaxL; L > MinL ->
            parse_ieis(All, R, Acc#{Name => V});
        {value, {Name, _, _, L}, All} ->
            parse_ieis(All, R, Acc#{Name => V})
    end.

compose_ieis(Allowed, Msg) ->
    compose_ieis(lists:reverse(Allowed), Msg, <<>>).

compose_ieis([], _Msg, Acc) ->
    Acc;
compose_ieis([{N, IEI, _, _}|R], Msg, Acc) when is_map_key(N, Msg) ->
    #{N := Value} = Msg,
    V = encode_iei(IEI, Value),
    L = byte_size(V),
    I = <<IEI:8, L:8, V/binary>>,
    compose_ieis(R, Msg, <<I/binary, Acc/binary>>);
compose_ieis([_|R], Msg, Acc) ->
    compose_ieis(R, Msg, Acc).

decode_nas_message(<<PD:4, TI:4, 2#0000_0001:8, D/binary>>) ->
    #{protocol_discriminator => otc_l3_codec:parse_protocol_discriminator(PD),
      transaction_identifier => TI,
      message_type => cp_data,
      user_payload => D
     };
decode_nas_message(<<PD:4, TI:4, 2#0000_0100:8>>) ->
    #{protocol_discriminator => otc_l3_codec:parse_protocol_discriminator(PD),
      transaction_identifier => TI,
      message_type => cp_ack
     };
decode_nas_message(<<PD:4, TI:4, 2#0001_0000:8, C/binary>>) ->
    #{protocol_discriminator => otc_l3_codec:parse_protocol_discriminator(PD),
      transaction_identifier => TI,
      message_type => cp_error,
      cause => C
     }.

encode_nas_message(#{message_type := cp_data} = V) ->
    #{protocol_discriminator := ProtoDisc,
      transaction_identifier := TI,
      user_payload := D
     } = V,
    PD = otc_l3_codec:compose_protocol_discriminator(ProtoDisc),
    <<PD:4, TI:4, 2#0000_0001:8, D/binary>>;
encode_nas_message(#{message_type := cp_ack} = V) ->
    #{protocol_discriminator := ProtoDisc,
      transaction_identifier := TI
     } = V,
    PD = otc_l3_codec:compose_protocol_discriminator(ProtoDisc),
    <<PD:4, TI:4, 2#0000_0100:8>>;
encode_nas_message(#{message_type := cp_error} = V) ->
    #{protocol_discriminator := ProtoDisc,
      transaction_identifier := TI,
      cause := C
     } = V,
    PD = otc_l3_codec:compose_protocol_discriminator(ProtoDisc),
    <<PD:4, TI:4, 2#0001_0000:8, C/binary>>.

decode_mobile_identity(<<D1:4, _P:1, ToI:3, Ds/binary>>) ->
    %% 3GPP TS 24.008 Table 10.5.4: Mobile Identity information element
    case parse_type_of_identity(ToI) of
        tmgi ->
            <<ServId:3/binary, Rest0/binary>> = Ds,
            M = case D1 of
                    2#00 ->
                        #{mbms_service_identity => ServId};
                    2#01 ->
                        MCCMNC = otc_gtpv2c:decode_mcc_mnc(Rest0),
                        MCCMNC#{mbms_service_identity => ServId};
                    2#10 ->
                        #{mbms_service_identity => ServId,
                          mbms_session_identity => Rest0
                         };
                    2#11 ->
                        <<MC:3/binary, Rest/binary>> = Rest0,
                        MCCMNC = otc_gtpv2c:decode_mcc_mnc(MC),
                        MCCMNC#{mbms_service_identity => ServId,
                                mbms_session_identity => Rest
                               }
                end,
            {tmgi, M};
        tmsi ->
            {tmsi, Ds};
        T ->
            Digits = otc_util:decode_tbcd(Ds, [otc_util:dec_tbcd_digit(D1)]),
            {T, lists:reverse(Digits)}
    end.

encode_mobile_identity({T, V}) ->
    %% 3GPP TS 24.008 Table 10.5.4: Mobile Identity information element
    ToI = compose_type_of_identity(T),
    case T of
        tmgi ->
            #{mbms_service_identity := ServId} = V,
            case V of
                #{mcc := _MCC, mnc := _MNC,
                  mbms_session_identity := Rest
                 } ->
                    MC = otc_gtpv2c:encode_mcc_mnc(V),
                    <<2#00:2, 2#11:2, 0:1, ToI:3, ServId:3/binary, MC:3/binary, Rest/binary>>;
                #{mbms_session_identity := Rest} ->
                    <<2#00:2, 2#10:2, 0:1, ToI:3, ServId:3/binary, Rest/binary>>;
                #{mcc := _MCC, mnc := _MNC} ->
                    MC = otc_gtpv2c:encode_mcc_mnc(V),
                    <<2#00:2, 2#01:2, 0:1, ToI:3, ServId:3/binary, MC:3/binary>>;
                _ ->
                    <<2#00:2, 2#00:2, 0:1, ToI:3, ServId:3/binary>>
            end;
        tmsi ->
            P = byte_size(V) rem 2,
            <<2#1111:4, P:1, ToI:3, V/binary>>;
        _ ->
            [I1|Ds] = V,
            P = length(V) rem 2,
            D1 = otc_util:enc_tbcd_digit(I1),
            otc_util:encode_tbcd(Ds, <<D1:4, P:1, ToI:3>>)
    end.

parse_type_of_identity(2#001) -> imsi;
parse_type_of_identity(2#010) -> imei;
parse_type_of_identity(2#011) -> imeisv;
parse_type_of_identity(2#100) -> tmsi;
parse_type_of_identity(2#101) -> tmgi;
parse_type_of_identity(2#000) -> no_identity.

compose_type_of_identity(imsi) -> 2#001;
compose_type_of_identity(imei) -> 2#010;
compose_type_of_identity(imeisv) -> 2#011;
compose_type_of_identity(tmsi) -> 2#100;
compose_type_of_identity(tmgi) -> 2#101;
compose_type_of_identity(no_identity) -> 2#000.

