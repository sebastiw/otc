-module(otc_m3ua).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         encode/1,
         decode_point_code/1,
         encode_point_code/1]).

-include("include/m3ua.hrl").
-include("include/mtp3.hrl").

spec() ->
    "IETF RFC 4666 September 2006".

codec(Bin, _Opts) when is_binary(Bin) ->
    decode(Bin);
codec(Map, _Opts) when is_map(Map) ->
    encode({Map, <<>>});
codec({Map, PDU}, _Opts) when is_map(Map), is_binary(PDU) ->
    encode({Map, PDU}).

-type subproto() :: sccp | tup | isup | broadband_isup |
                    satellite_isup |
                    aal_type_2_signalling |
                    bearer_independent_call_control |
                    gateway_control_protocol.

-spec next(map()) -> '$stop' | {ok, subproto()}.
next(#{protocol_data := #{service_indicator := SI}}) -> {ok, SI};
next(_) -> '$stop'.

-type itu_point_code() :: {itu, integer(), integer(), integer()}.
-type ansi_point_code() :: {ansi, integer(), integer(), integer()}.

-spec decode(binary()) -> map().
decode(<<1:8, _:8, MessageClass:8, MessageType:8, Len:32/big, Remain/binary>>) ->
    %% 0                   1                   2                   3
    %% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |    Version    |   Reserved    | Message Class | Message Type  |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |                        Message Length                         |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% \                                                               \
    %% /                                                               /
    MC = parse_message_class(MessageClass),
    MT = parse_message_type(MC, MessageType),
    ValLen = Len - 8,                 % Remove version, reserved, mc, mt, length
    <<Bin:ValLen/binary>> = Remain,
    {ok, Msg} = decode_msg(MC, MT, Bin),
    case Msg#{message_type => MT, message_class => MC} of
        #{protocol_data := PD} = Msg2 ->
            {UPD, PD2} = maps:take(user_protocol_data, PD),
            {Msg2#{protocol_data => PD2}, UPD};
        Msg2 ->
            Msg2
    end.

encode({#{protocol_data := PD} = Msg, UDP}) ->
    encode(Msg#{protocol_data => PD#{user_protocol_data => UDP}});
encode({Msg, _UDP}) ->
    encode(Msg);
encode(#{message_type := MessageType, message_class := MessageClass} = Msg) ->
    MC = compose_message_class(MessageClass),
    MT = compose_message_type(MessageClass, MessageType),
    Bin = encode_msg(MessageClass, MessageType, Msg),
    Len = byte_size(Bin) + 8,
    <<1:8, 0:8, MC:8, MT:8, Len:32/big, Bin/binary>>.

parse_message_class(?M3UA_MSG_CLASS_MGMT) -> mgmt;
parse_message_class(?M3UA_MSG_CLASS_TRANSFER) -> transfer;
parse_message_class(?M3UA_MSG_CLASS_SSNM) -> ssnm;
parse_message_class(?M3UA_MSG_CLASS_ASPSM) -> aspsm;
parse_message_class(?M3UA_MSG_CLASS_ASPTM) -> asptm;
parse_message_class(?M3UA_MSG_CLASS_RKM) -> rkm.

compose_message_class(mgmt) -> ?M3UA_MSG_CLASS_MGMT;
compose_message_class(transfer) -> ?M3UA_MSG_CLASS_TRANSFER;
compose_message_class(ssnm) -> ?M3UA_MSG_CLASS_SSNM;
compose_message_class(aspsm) -> ?M3UA_MSG_CLASS_ASPSM;
compose_message_class(asptm) -> ?M3UA_MSG_CLASS_ASPTM;
compose_message_class(rkm) -> ?M3UA_MSG_CLASS_RKM.

parse_message_type(mgmt, ?M3UA_MGMT_TYPE_ERR) -> err;
parse_message_type(mgmt, ?M3UA_MGMT_TYPE_NTFY) -> ntfy;
parse_message_type(transfer, ?M3UA_TRANSFER_TYPE_DATA) -> data;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_DUNA) -> duna;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_DAVA) -> dava;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_DAUD) -> daud;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_SCON) -> scon;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_DUPU) -> dupu;
parse_message_type(ssnm, ?M3UA_SSNM_TYPE_DRST) -> drst;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_ASPUP) -> aspup;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_ASPDN) -> aspdn;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_BEAT) -> beat;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_ASPUP_ACK) -> aspup_ack;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_ASPDN_ACK) -> aspdn_ack;
parse_message_type(aspsm, ?M3UA_ASPSM_TYPE_BEAT_ACK) -> beat_ack;
parse_message_type(asptm, ?M3UA_ASPTM_TYPE_ASPAC) -> aspac;
parse_message_type(asptm, ?M3UA_ASPTM_TYPE_ASPIA) -> aspia;
parse_message_type(asptm, ?M3UA_ASPTM_TYPE_ASPAC_ACK) -> aspac_ack;
parse_message_type(asptm, ?M3UA_ASPTM_TYPE_ASPIA_ACK) -> aspia_ack;
parse_message_type(rkm, ?M3UA_RKM_TYPE_REG_REQ) -> reg_req;
parse_message_type(rkm, ?M3UA_RKM_TYPE_REG_RSP) -> reg_rsp;
parse_message_type(rkm, ?M3UA_RKM_TYPE_DEREG_REQ) -> dereg_req;
parse_message_type(rkm, ?M3UA_RKM_TYPE_DEREG_RSP) -> dereg_rsp.

compose_message_type(mgmt, err) -> ?M3UA_MGMT_TYPE_ERR;
compose_message_type(mgmt, ntfy) -> ?M3UA_MGMT_TYPE_NTFY;
compose_message_type(transfer, data) -> ?M3UA_TRANSFER_TYPE_DATA;
compose_message_type(ssnm, duna) -> ?M3UA_SSNM_TYPE_DUNA;
compose_message_type(ssnm, dava) -> ?M3UA_SSNM_TYPE_DAVA;
compose_message_type(ssnm, daud) -> ?M3UA_SSNM_TYPE_DAUD;
compose_message_type(ssnm, scon) -> ?M3UA_SSNM_TYPE_SCON;
compose_message_type(ssnm, dupu) -> ?M3UA_SSNM_TYPE_DUPU;
compose_message_type(ssnm, drst) -> ?M3UA_SSNM_TYPE_DRST;
compose_message_type(aspsm, aspup) -> ?M3UA_ASPSM_TYPE_ASPUP;
compose_message_type(aspsm, aspdn) -> ?M3UA_ASPSM_TYPE_ASPDN;
compose_message_type(aspsm, beat) -> ?M3UA_ASPSM_TYPE_BEAT;
compose_message_type(aspsm, aspup_ack) -> ?M3UA_ASPSM_TYPE_ASPUP_ACK;
compose_message_type(aspsm, aspdn_ack) -> ?M3UA_ASPSM_TYPE_ASPDN_ACK;
compose_message_type(aspsm, beat_ack) -> ?M3UA_ASPSM_TYPE_BEAT_ACK;
compose_message_type(asptm, aspac) -> ?M3UA_ASPTM_TYPE_ASPAC;
compose_message_type(asptm, aspia) -> ?M3UA_ASPTM_TYPE_ASPIA;
compose_message_type(asptm, aspac_ack) -> ?M3UA_ASPTM_TYPE_ASPAC_ACK;
compose_message_type(asptm, aspia_ack) -> ?M3UA_ASPTM_TYPE_ASPIA_ACK;
compose_message_type(rkm, reg_req) -> ?M3UA_RKM_TYPE_REG_REQ;
compose_message_type(rkm, reg_rsp) -> ?M3UA_RKM_TYPE_REG_RSP;
compose_message_type(rkm, dereg_req) -> ?M3UA_RKM_TYPE_DEREG_REQ;
compose_message_type(rkm, dereg_rsp) -> ?M3UA_RKM_TYPE_DEREG_RSP.

decode_msg(transfer, data, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {protocol_data, 16#0210, mandatory, single},
                         {correlation_id, 16#0013, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, duna, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, dava, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, daud, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, scon, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {concerned_destination, 16#0206, optional, single},
                         {congestion_indications, 16#0205, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, dupu, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {user_and_cause, 16#0204, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ssnm, drst, Bin) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, aspup, Bin) ->
    AllowedParameters = [{asp_identifier, 16#0011, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, aspup_ack, Bin) ->
    AllowedParameters = [{asp_identifier, 16#0011, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, aspdn, Bin) ->
    AllowedParameters = [{info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, aspdn_ack, Bin) ->
    AllowedParameters = [{info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, beat, Bin) ->
    AllowedParameters = [{heartbeat_data, 16#0009, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(aspsm, beat_ack, Bin) ->
    AllowedParameters = [{heartbeat_data, 16#0009, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rkm, reg_req, Bin) ->
    AllowedParameters = [{routing_key, 16#0207, mandatory, multiple}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rkm, reg_rsp, Bin) ->
    AllowedParameters = [{registration_result, 16#0208, mandatory, multiple}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rkm, dereg_req, Bin) ->
    AllowedParameters = [{routing_context, 16#0006, mandatory, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rkm, dereg_rsp, Bin) ->
    AllowedParameters = [{deregistration_result, 16#0209, mandatory, multiple}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(asptm, aspac, Bin) ->
    AllowedParameters = [{traffic_mode_type, 16#000b, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(asptm, aspac_ack, Bin) ->
    AllowedParameters = [{traffic_mode_type, 16#000b, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(asptm, aspia, Bin) ->
    AllowedParameters = [{routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(asptm, aspia_ack, Bin) ->
    AllowedParameters = [{routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(mgmt, err, Bin) ->
    AllowedParameters = [{error_code, 16#000c, mandatory, single},
                         {routing_context, 16#0006, conditional, single},
                         {network_appearance, 16#0200, conditional, single},
                         {affected_point_code, 16#0012, conditional, single},
                         {diagnostic_information, 16#0007, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(mgmt, ntfy, Bin) ->
    AllowedParameters = [{status, 16#000d, mandatory, single},
                         {asp_identifier, 16#0011, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters).

encode_msg(transfer, data, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {protocol_data, 16#0210, mandatory, single},
                         {correlation_id, 16#0013, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, duna, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, dava, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, daud, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, scon, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {concerned_destination, 16#0206, optional, single},
                         {congestion_indications, 16#0205, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, dupu, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {user_and_cause, 16#0204, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ssnm, drst, Msg) ->
    AllowedParameters = [{network_appearance, 16#0200, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, aspup, Msg) ->
    AllowedParameters = [{asp_identifier, 16#0011, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, aspup_ack, Msg) ->
    AllowedParameters = [{asp_identifier, 16#0011, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, aspdn, Msg) ->
    AllowedParameters = [{info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, aspdn_ack, Msg) ->
    AllowedParameters = [{info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, beat, Msg) ->
    AllowedParameters = [{heartbeat_data, 16#0009, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(aspsm, beat_ack, Msg) ->
    AllowedParameters = [{heartbeat_data, 16#0009, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rkm, reg_req, Msg) ->
    AllowedParameters = [{routing_key, 16#0207, mandatory, multiple}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rkm, reg_rsp, Msg) ->
    AllowedParameters = [{registration_result, 16#0208, mandatory, multiple}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rkm, dereg_req, Msg) ->
    AllowedParameters = [{routing_context, 16#0006, mandatory, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rkm, dereg_rsp, Msg) ->
    AllowedParameters = [{deregistration_result, 16#0209, mandatory, multiple}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(asptm, aspac, Msg) ->
    AllowedParameters = [{traffic_mode_type, 16#000b, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(asptm, aspac_ack, Msg) ->
    AllowedParameters = [{traffic_mode_type, 16#000b, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(asptm, aspia, Msg) ->
    AllowedParameters = [{routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(asptm, aspia_ack, Msg) ->
    AllowedParameters = [{routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(mgmt, err, Msg) ->
    AllowedParameters = [{error_code, 16#000c, mandatory, single},
                         {routing_context, 16#0006, conditional, single},
                         {network_appearance, 16#0200, conditional, single},
                         {affected_point_code, 16#0012, conditional, single},
                         {diagnostic_information, 16#0007, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters);
encode_msg(mgmt, ntfy, Msg) ->
    AllowedParameters = [{status, 16#000d, mandatory, single},
                         {asp_identifier, 16#0011, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    encode_parameters(Msg, AllowedParameters).

decode_parameters(Bin, AllowedParameters) ->
    Ps = decode_parameters(Bin, AllowedParameters, #{}),
    verify_mandatory_exist(Ps, AllowedParameters).

decode_parameters(<<>>, _, Acc) ->
    Acc;
decode_parameters(Bin, AllowedParameters, Acc) ->
    {Tag, BinValue, Rest} = take_parameter(Bin),

    case lists:keytake(Tag, 2, AllowedParameters) of
        {value, {_, _, _, single} = T, APs} ->
            NAcc = decode_parameter(T, BinValue, Acc),
            decode_parameters(Rest, APs, NAcc);
        {value, {_, _, _, multiple} = T, APs} ->
            NAcc = decode_parameter(T, BinValue, Acc),
            decode_parameters(Rest, [T|APs], NAcc)
    end.

verify_mandatory_exist(Ps, []) ->
    {ok, Ps};
verify_mandatory_exist(Ps, [{N, _, mandatory, _}|_]) when not is_map_key(N, Ps) ->
    throw({mandatory_missing, N});
verify_mandatory_exist(Ps, [_|Rest]) ->
    verify_mandatory_exist(Ps, Rest).

encode_parameters(Msg, AllowedParameters) ->
    encode_parameters(Msg, lists:reverse(AllowedParameters), <<>>).

encode_parameters(_, [], Acc) ->
    Acc;
encode_parameters(Msg, [Param|AllowedParameters], Acc) ->
    case Param of
        {Name, Tag, _, single} when is_map_key(Name, Msg) ->
            #{Name := V} = Msg,
            Value = encode_parameter(Name, V),
            NAcc = wrap_param(Tag, Value, Acc),
            encode_parameters(Msg, AllowedParameters, NAcc);
        {Name, Tag, _, multiple} when is_map_key(Name, Msg) ->
            #{Name := Vs} = Msg,
            Bin = lists:foldl(fun (V, Acc2) ->
                                      Value = encode_parameter(Name, V),
                                      wrap_param(Tag, Value, Acc2)
                              end, <<>>, Vs),
            encode_parameters(Msg, AllowedParameters, Bin);
        {Name, _, mandatory, _} when not is_map_key(Name, Msg) ->
            throw({mandatory_missing, Name});
         _ ->
            encode_parameters(Msg, AllowedParameters, Acc)
    end.

wrap_param(Tag, Value, Acc) ->
    ValLen = byte_size(Value),
    PadLen = (4 - (ValLen rem 4)) rem 4,
    Len = ValLen + 4,
    <<Tag:16/big, Len:16/big, Value:ValLen/binary, 0:PadLen/integer-unit:8, Acc/binary>>.


take_parameter(<<Tag:16/big, Len:16/big, Bin1/binary>>) ->
    %% If the length of the parameter is not a multiple of 4 octets,
    %% the sender pads the Parameter at the end (i.e., after the
    %% Parameter Value field) with all zero octets.
    PadLen = (4 - (Len rem 4)) rem 4,
    %%  0                   1                   2                   3
    %%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |          Parameter Tag        |       Parameter Length        |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% \                                                               \
    %% /                       Parameter Value                         /
    %% \                                                               \
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    ValLen = Len - 4,         % Remove parameter tag + length
    <<Value:ValLen/binary, 0:PadLen/integer-unit:8, Rest/binary>> = Bin1,
    {Tag, Value, Rest}.

decode_parameter({network_appearance = Name, _, _, SM}, <<NA:32/big>>, Acc) ->
    update_params(Name, SM, NA, Acc);
decode_parameter({routing_context = Name, _, _, SM}, <<RC:32/big>>, Acc) ->
    update_params(Name, SM, RC, Acc);
decode_parameter({routing_context = Name, _, _, SM}, Bin, Acc) ->
    RCs = [RC || <<RC:32/big>> <= Bin],
    update_params(Name, SM, RCs, Acc);
decode_parameter({protocol_data = Name, _, _, SM}, <<PDH:12/binary, UPD/binary>>, Acc) ->
    <<OPC:4/binary, DPC:4/binary, SI:8/big, NI:8/big, MP:8/big, SLS:8/big>> = PDH,
    PD = #{originating_point_code => OPC,
           destination_point_code => DPC,
           service_indicator => parse_user_identity(SI),
           network_indicator => parse_network_indicator(NI),
           message_priority => MP,
           signalling_link_selection => SLS,
           user_protocol_data => UPD
          },
    update_params(Name, SM, PD, Acc);
decode_parameter({correlation_id = Name, _, _, SM}, <<CI:32/big>>, Acc) ->
    update_params(Name, SM, CI, Acc);
decode_parameter({affected_point_code = Name, _, _, SM}, Bin, Acc) ->
    APCs = [{Mask, PC} || <<Mask:1/binary, PC:3/binary>> <= Bin],
    update_params(Name, SM, APCs, Acc);
decode_parameter({info_string = Name, _, _, SM}, Bin, Acc) ->
    update_params(Name, SM, Bin, Acc);
decode_parameter({concerned_destination = Name, _, _, SM}, <<_R:1/binary, PC:3/binary>>, Acc) ->
    update_params(Name, SM, PC, Acc);
decode_parameter({congestion_indications = Name, _, _, SM}, <<_R:3/binary, CL:8/big>>, Acc) ->
    update_params(Name, SM, CL, Acc);
decode_parameter({user_and_cause = Name, _, _, SM}, <<Cause:16/big, User:16/big>>, Acc) ->
    C = case Cause of
            1 -> unequipped_remote_user;
            2 -> inaccessible_remote_user;
            _ -> unknown                    % treat as 0
        end,
    U = parse_user_identity(User),
    V = #{unavailability_cause => C,
          mtp3_user_identity => U},
    update_params(Name, SM, V, Acc);
decode_parameter({asp_identifier = Name, _, _, SM}, <<ID:32/big>>, Acc) ->
    update_params(Name, SM, ID, Acc);
decode_parameter({heartbeat_data = Name, _, _, SM}, Bin, Acc) ->
    update_params(Name, SM, Bin, Acc);
decode_parameter({routing_key = Name, _, _, SM}, Bin, Acc) ->
    AllowedParameters = [{local_rk_identifier, 16#020a, mandatory, single},
                         {routing_context, 16#0006, optional, single},
                         {traffic_mode_type, 16#000b, optional, single},
                         {destination_point_code, 16#020b, mandatory, multiple},
                         {network_appearance, 16#0200, optional, single},
                         {service_indicators, 16#020c, optional, multiple},
                         {originating_point_code_list, 16#020e, optional, multiple}
                        ],
    V = decode_parameters(Bin, AllowedParameters, #{}),
    update_params(Name, SM, V, Acc);
decode_parameter({local_rk_identifier = Name, _, _, SM}, <<LRKI:32/big>>, Acc) ->
    update_params(Name, SM, LRKI, Acc);
decode_parameter({traffic_mode_type = Name, _, _, SM}, <<TMT:32/big>>, Acc) ->
    V = case TMT of
            1 -> override;
            2 -> loadshare;
            3 -> broadcast;
            _ -> unsupported
        end,
    update_params(Name, SM, V, Acc);
decode_parameter({destination_point_code = Name, _, _, SM}, <<Mask:1/binary, PC:3/binary>>, Acc) ->
    update_params(Name, SM, {Mask, PC}, Acc);
decode_parameter({service_indicators = Name, _, _, SM}, Bin, Acc) ->
    SIs = [parse_user_identity(SI) || <<SI:8/big>> <= Bin],
    V = lists:dropwhile(fun ({reserved, 0}) -> true; (_) -> false end, lists:reverse(SIs)),
    update_params(Name, SM, lists:reverse(V), Acc);
decode_parameter({originating_point_code_list = Name, _, _, SM}, Bin, Acc) ->
    OPCs = [{Mask, PC} || <<Mask:1/binary, PC:3/binary>> <= Bin],
    update_params(Name, SM, OPCs, Acc);
decode_parameter({registration_result = Name, _, _, SM}, Bin, Acc) ->
    AllowedParameters = [{local_rk_identifier, 16#020a, mandatory, single},
                         {registration_status, 16#0212, mandatory, single},
                         {routing_context, 16#0006, optional, single}
                        ],
    V = decode_parameters(Bin, AllowedParameters, #{}),
    update_params(Name, SM, V, Acc);
decode_parameter({registration_status = Name, _, _, SM}, <<S:32/big>>, Acc) ->
    V = case S of
            0 -> successfully_registered;
            1 -> {error, unknown};
            2 -> {error, invalid_dpc};
            3 -> {error, invalid_network_appearance};
            4 -> {error, invalid_routing_key};
            5 -> {error, permission_denied};
            6 -> {error, cannot_support_unique_routing};
            7 -> {error, routing_key_not_currently_provisioned};
            8 -> {error, insufficient_resources};
            9 -> {error, unsupported_rk_parameter_field};
            10 -> {error, unsupported_or_invalid_traffic_handling_mode};
            11 -> {error, routing_key_change_refused};
            12 -> {error, routing_key_already_registered};
            _ -> unsupported
        end,
    update_params(Name, SM, V, Acc);
decode_parameter({deregistration_result = Name, _, _, SM}, Bin, Acc) ->
    AllowedParameters = [{routing_context, 16#0006, mandatory, single},
                         {deregistration_status, 16#0213, mandatory, single}
                        ],
    V = decode_parameters(Bin, AllowedParameters, #{}),
    update_params(Name, SM, V, Acc);
decode_parameter({deregistration_status = Name, _, _, SM}, <<S:32/big>>, Acc) ->
    V = case S of
            0 -> successfully_deregistered;
            1 -> {error, unknown};
            2 -> {error, invalid_routing_context};
            3 -> {error, permission_denied};
            4 -> {error, not_registered};
            5 -> {error, asp_currently_active_for_routing_context}
        end,
    update_params(Name, SM, V, Acc);
decode_parameter({error_code = Name, _, _, SM}, <<E:32/big>>, Acc) ->
    V = case E of
            16#01 -> invalid_version;
            16#03 -> unsupported_message_class;
            16#04 -> unsupported_message_type;
            16#05 -> unsupported_traffic_mode_type;
            16#06 -> unexpected_message;
            16#07 -> protocol_error;
            16#09 -> invalid_stream_identifier;
            16#0d -> refused_management_blocking;
            16#0e -> asp_identifier_required;
            16#0f -> invalid_asp_identifier;
            16#11 -> invalid_parameter_value;
            16#12 -> parameter_field_error;
            16#13 -> unexpected_parameter;
            16#14 -> destination_status_unknown;
            16#15 -> invalid_network_appearance;
            16#16 -> missing_parameter;
            16#19 -> invalid_routing_context;
            16#1a -> no_configured_as_for_asp;
            _ -> unsupported
        end,
    update_params(Name, SM, V, Acc);
decode_parameter({diagnostic_information = Name, _, _, SM}, Bin, Acc) ->
    update_params(Name, SM, Bin, Acc);
decode_parameter({status = Name, _, _, SM}, <<ST:16/big, SI:16/big>>, Acc) ->
    V = case ST of
            1 ->
                I = case SI of
                        1 -> reserved;
                        2 -> application_server_inactive;
                        3 -> application_server_active;
                        4 -> application_server_pending;
                        _ -> unsupported
                    end,
                #{status_type => application_server_state_change,
                  status_information => I};
            _ -> % treat all as other
                I = case SI of
                        1 -> insufficient_asp_resources_active_in_as;
                        2 -> alternate_asp_active;
                        3 -> asp_failure;
                        _ -> unsupported
                    end,
                #{status_type => other,
                  status_information => I}
        end,
    update_params(Name, SM, V, Acc).

update_params(Name, single, V, Acc) ->
    Acc#{Name => V};
update_params(Name, multiple, V, Acc) ->
    maps:update_with(Name, fun (Vs) -> [V|Vs] end, [V], Acc).


encode_parameter(network_appearance, NA) ->
    <<NA:32/big>>;
encode_parameter(routing_context, RCs) when is_list(RCs) ->
    V = [<<RC:32/big>> || RC <- RCs],
    binary:list_to_bin(V);
encode_parameter(routing_context, RC) ->
    <<RC:32/big>>;
encode_parameter(protocol_data, V) ->
   #{originating_point_code := OPC,
     destination_point_code := DPC,
     service_indicator := ServiceInd,
     network_indicator := NetworkInd,
     message_priority := MP,
     signalling_link_selection := SLS,
     user_protocol_data := UPD
    } = V,
    SI = compose_user_identity(ServiceInd),
    NI = compose_network_indicator(NetworkInd),
    PDH = <<OPC:4/binary, DPC:4/binary, SI:8/big, NI:8/big, MP:8/big, SLS:8/big>>,
    <<PDH:12/binary, UPD/binary>>;
encode_parameter(correlation_id, CI) ->
    <<CI:32/big>>;
encode_parameter(affected_point_code, APCs) ->
    V = [<<Mask:1/binary, PC:3/binary>> || {Mask, PC} <- APCs],
    binary:list_to_bin(V);
encode_parameter(info_string, V) ->
    V;
encode_parameter(concerned_destination, PC) ->
    <<0:8, PC:3/binary>>;
encode_parameter(congestion_indications, CL) ->
    <<0:24, CL:8/big>>;
encode_parameter(user_and_cause, V) ->
    #{unavailability_cause := Cause,
      mtp3_user_identity := User
     } = V,
    C = case Cause of
            unequipped_remote_user -> 1;
            inaccessible_remote_user -> 2;
            _ -> 0                    % treat as 0
        end,
    U = compose_user_identity(User),
    <<C:16/big, U:16/big>>;
encode_parameter(asp_identifier, ID) ->
    <<ID:32/big>>;
encode_parameter(heartbeat_data, V) ->
    V;
encode_parameter(routing_key, V) ->
    AllowedParameters = [{local_rk_identifier, 16#020a, mandatory, single},
                         {routing_context, 16#0006, optional, single},
                         {traffic_mode_type, 16#000b, optional, single},
                         {destination_point_code, 16#020b, mandatory, multiple},
                         {network_appearance, 16#0200, optional, single},
                         {service_indicators, 16#020c, optional, multiple},
                         {originating_point_code_list, 16#020e, optional, multiple}
                        ],
    encode_parameters(V, AllowedParameters);
encode_parameter(local_rk_identifier, LRKI) ->
    <<LRKI:32/big>>;
encode_parameter(traffic_mode_type, V) ->
    TMT = case V of
              override -> 1;
              loadshare -> 2;
              broadcast -> 3
          end,
    <<TMT:32/big>>;
encode_parameter(destination_point_code, {Mask, PC}) ->
    <<Mask:1/binary, PC:3/binary>>;
encode_parameter(service_indicators, SIs) ->
    V = [<<(compose_user_identity(SI)):8/big>> || SI <- SIs],
    binary:list_to_bin(V);
encode_parameter(originating_point_code_list, OPCs) ->
    V = [<<Mask:1/binary, PC:3/binary>> || {Mask, PC} <- OPCs],
    binary:list_to_bin(V);
encode_parameter(registration_result, V) ->
    AllowedParameters = [{local_rk_identifier, 16#020a, mandatory, single},
                         {registration_status, 16#0212, mandatory, single},
                         {routing_context, 16#0006, optional, single}
                        ],
    encode_parameters(V, AllowedParameters);
encode_parameter(registration_status, V) ->
    S = case V of
            successfully_registered ->                                0;
            {error, unknown} ->                                       1;
            {error, invalid_dpc} ->                                   2;
            {error, invalid_network_appearance} ->                    3;
            {error, invalid_routing_key} ->                           4;
            {error, permission_denied} ->                             5;
            {error, cannot_support_unique_routing} ->                 6;
            {error, routing_key_not_currently_provisioned} ->         7;
            {error, insufficient_resources} ->                        8;
            {error, unsupported_rk_parameter_field} ->                9;
            {error, unsupported_or_invalid_traffic_handling_mode} ->  10;
            {error, routing_key_change_refused} ->                    11;
            {error, routing_key_already_registered} ->                12
        end,
    <<S:32/big>>;
encode_parameter(deregistration_result, V) ->
    AllowedParameters = [{routing_context, 16#0006, mandatory, single},
                         {deregistration_status, 16#0213, mandatory, single}
                        ],
    encode_parameters(V, AllowedParameters);
encode_parameter(deregistration_status, V) ->
    S = case V of
            successfully_deregistered ->                         0;
            {error, unknown} ->                                  1;
            {error, invalid_routing_context} ->                  2;
            {error, permission_denied} ->                        3;
            {error, not_registered} ->                           4;
            {error, asp_currently_active_for_routing_context} -> 5
        end,
    <<S:32/big>>;
encode_parameter(error_code, V) ->
    E = case V of
            invalid_version ->               16#01;
            unsupported_message_class ->     16#03;
            unsupported_message_type ->      16#04;
            unsupported_traffic_mode_type -> 16#05;
            unexpected_message ->            16#06;
            protocol_error ->                16#07;
            invalid_stream_identifier ->     16#09;
            refused_management_blocking ->   16#0d;
            asp_identifier_required ->       16#0e;
            invalid_asp_identifier ->        16#0f;
            invalid_parameter_value ->       16#11;
            parameter_field_error ->         16#12;
            unexpected_parameter ->          16#13;
            destination_status_unknown ->    16#14;
            invalid_network_appearance ->    16#15;
            missing_parameter ->             16#16;
            invalid_routing_context ->       16#19;
            no_configured_as_for_asp ->      16#1a
        end,
    <<E:32/big>>;
encode_parameter(diagnostic_information, V) ->
    V;
encode_parameter(status, V) ->
    ST = case V of
             #{status_type := application_server_state_change} ->
                 1;
             _ ->
                 2
         end,
    SI = case V of
             #{status_type := application_server_state_change,
               status_information := I} ->
                 case I of
                     reserved ->                    1;
                     application_server_inactive -> 2;
                     application_server_active ->   3;
                     application_server_pending ->  4
                 end;
             #{status_information := I} -> % treat all as other
                 case I of
                     insufficient_asp_resources_active_in_as -> 1;
                     alternate_asp_active ->                    2;
                     asp_failure ->                             3
                 end
         end,
    <<ST:16/big, SI:16/big>>.


parse_user_identity(User) ->
    case User of
        3 -> sccp;
        4 -> tup;
        5 -> isup;
        9 -> broadband_isup;
        10 -> satellite_isup;
        12 -> aal_type_2_signalling;
        13 -> bearer_independent_call_control;
        14 -> gateway_control_protocol;
        R -> {reserved, R}
    end.

compose_user_identity(User) ->
    case User of
        sccp -> 3;
        tup -> 4;
        isup -> 5;
        broadband_isup -> 9;
        satellite_isup -> 10;
        aal_type_2_signalling -> 12;
        bearer_independent_call_control -> 13;
        {reserved, R} -> R
    end.

parse_network_indicator(?MTP3_NETIND_INTERNATIONAL) -> international;
parse_network_indicator(?MTP3_NETIND_INTERNATIONAL_SPARE) -> international_spare;
parse_network_indicator(?MTP3_NETIND_NATIONAL) -> national;
parse_network_indicator(?MTP3_NETIND_NATIONAL_SPARE) -> national_spare;
parse_network_indicator(R) ->  {reserved, R}.

compose_network_indicator(international) -> ?MTP3_NETIND_INTERNATIONAL;
compose_network_indicator(international_spare) -> ?MTP3_NETIND_INTERNATIONAL_SPARE;
compose_network_indicator(national) -> ?MTP3_NETIND_NATIONAL;
compose_network_indicator(national_spare) -> ?MTP3_NETIND_NATIONAL_SPARE;
compose_network_indicator({reserved, R}) -> R.


-spec decode_point_code({binary(), binary()}) -> [itu_point_code() | ansi_point_code()].
decode_point_code({<<Mask:8/big>>, <<PCbin:24/big>>}) ->
    MaskBits = trunc(math:pow(2, Mask) - 1),
    LowPC = PCbin band (trunc(math:pow(2, 24) - 1) bxor MaskBits),
    HighPC = PCbin bor MaskBits,
    [decode_pc(<<PC:24/big>>) || PC <- lists:seq(LowPC, HighPC)].

decode_pc(<<0:10, Zone:3, Region:8, SP:3>>) ->         % ITU 14-bits
    {itu, Zone, Region, SP};
decode_pc(<<Network:8, Cluster:8, Member:8>>) ->       % ANSI 24-bits
    {ansi, Network, Cluster, Member}.

-spec encode_point_code(itu_point_code() | ansi_point_code()) -> binary().
encode_point_code(PC) when is_tuple(PC) ->
    encode_point_code(0, PC).

encode_point_code(Mask, {itu, Zone, Region, SP}) ->
    <<Mask:8, 0:10, Zone:3, Region:8, SP:3>>;
encode_point_code(Mask, {ansi, Network, Cluster, Member}) ->
    Mask = 0,
    <<Mask:8, Network:8, Cluster:8, Member:8>>.
