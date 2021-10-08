-module(erlumts_m3ua_codec).
%% IETF RFC 4666 September 2006

-export([decode/1,
         decode_point_code/1]).

-include("include/m3ua.hrl").

-type itu_point_code() :: {itu, integer(), integer(), integer()}.
-type ansi_point_code() :: {ansi, integer(), integer(), integer()}.

-spec decode(binary()) -> unsupported | map().
decode(<<1:8, 0:8, MessageClass:8, MessageType:8, Bin/binary>>) ->
    %% 0                   1                   2                   3
    %% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |    Version    |   Reserved    | Message Class | Message Type  |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |                        Message Length                         |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% \                                                               \
    %% /                                                               /
    MT = parse_message_type(MessageType),
    MC = parse_message_class(MT, MessageClass),
    case decode_msg(MT, MC, Bin) of
        unsupported ->
            unsupported;
        Msg ->
            Msg#{message_type => MT,
                 message_class => MC}
    end;
decode(_) ->
    unsupported.

parse_message_type(?M3UA_MSG_CLASS_MGMT) -> mgmt;
parse_message_type(?M3UA_MSG_CLASS_TM) -> tm;
parse_message_type(?M3UA_MSG_CLASS_SSNM) -> ssnm;
parse_message_type(?M3UA_MSG_CLASS_ASPSM) -> aspsm;
parse_message_type(?M3UA_MSG_CLASS_ASPTM) -> asptm;
parse_message_type(?M3UA_MSG_CLASS_RKM) -> rkm;
parse_message_type(_) ->
    unsupported.

parse_message_class(mgmt, ?M3UA_MGMT_TYPE_ERR) -> err;
parse_message_class(mgmt, ?M3UA_MGMT_TYPE_NTFY) -> ntfy;
parse_message_class(tm, ?M3UA_TM_TYPE_DATA) -> data;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_DUNA) -> duna;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_DAVA) -> dava;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_DAUD) -> daud;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_SCON) -> scon;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_DUPU) -> dupu;
parse_message_class(ssnm, ?M3UA_SSNM_TYPE_DRST) -> drst;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_ASPUP) -> aspup;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_ASPDN) -> aspdn;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_BEAT) -> beat;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_ASPUP_ACK) -> aspup_ack;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_ASPDN_ACK) -> aspdn_ack;
parse_message_class(aspsm, ?M3UA_ASPSM_TYPE_BEAT_ACK) -> beat_ack;
parse_message_class(asptm, ?M3UA_ASPTM_TYPE_ASPAC) -> aspac;
parse_message_class(asptm, ?M3UA_ASPTM_TYPE_ASPIA) -> aspia;
parse_message_class(asptm, ?M3UA_ASPTM_TYPE_ASPAC_ACK) -> aspac_ack;
parse_message_class(asptm, ?M3UA_ASPTM_TYPE_ASPIA_ACK) -> aspia_ack;
parse_message_class(rkm, ?M3UA_RKM_TYPE_REG_REQ) -> reg_req;
parse_message_class(rkm, ?M3UA_RKM_TYPE_REG_RSP) -> reg_rsp;
parse_message_class(rkm, ?M3UA_RKM_TYPE_DEREG_REQ) -> dereg_req;
parse_message_class(rkm, ?M3UA_RKM_TYPE_DEREG_RSP) -> dereg_rsp;
parse_message_class(_, _) ->
    unsupported.

decode_msg(tm, data, Bin) ->
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
                         {routing_context, 16#0006, mandatory, single},
                         {network_appearance, 16#0200, mandatory, single},
                         {affected_point_code, 16#0012, mandatory, single},
                         {diagnostic_information, 16#0007, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(mgmt, ntfy, Bin) ->
    AllowedParameters = [{status, 16#000d, mandatory, single},
                         {asp_identifier, 16#0011, optional, single},
                         {routing_context, 16#0006, optional, single},
                         {info_string, 16#0004, optional, single}
                        ],
    decode_parameters(Bin, AllowedParameters);
decode_msg(_, _, _) ->
    unsupported.

decode_parameters(Bin, AllowedParameters) ->
    Ps = decode_parameters(Bin, AllowedParameters, #{}),
    verify_mandatory_exist(Ps, AllowedParameters).

verify_mandatory_exist(Ps, AllowedParameters) ->
    MandatoryParameters = lists:filter(fun ({_, _, mandatory, _}) -> true; (_) -> false end, AllowedParameters),
    case lists:all(fun (P) -> is_map_key(P, Ps) end, MandatoryParameters) of
        true ->
            Ps;
        false ->
            unsupported
    end.

decode_parameters(<<>>, _, Acc) ->
    Acc;
decode_parameters(Bin, AllowedParameters, Acc) ->
    case take_parameter(Bin) of
        {Tag, BinValue, Rest} ->
            case lists:keytake(Tag, 2, AllowedParameters) of
                false ->
                    unsupported;
                {value, {_, _, _, single} = T, APs} ->
                    NAcc = decode_parameter(T, BinValue, Acc),
                    decode_parameters(Rest, APs, NAcc);
                {value, {_, _, _, multiple} = T, APs} ->
                    NAcc = decode_parameter(T, BinValue, Acc),
                    decode_parameters(Rest, [T|APs], NAcc)
            end;
        unsupported ->
            unsupported
    end.

take_parameter(<<Tag:2/binary, Len:16/big, Bin1/binary>>) ->
    %% If the length of the parameter is not a multiple of 4 octets,
    %% the sender pads the Parameter at the end (i.e., after the
    %% Parameter Value field) with all zero octets.
    PadLen = ((4 - Len) rem 4) rem 4,
    %%  0                   1                   2                   3
    %%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |          Parameter Tag        |       Parameter Length        |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% \                                                               \
    %% /                       Parameter Value                         /
    %% \                                                               \
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    <<Value:Len/binary, 0:PadLen/integer-unit:8, Rest/binary>> = Bin1,
    {Tag, Value, Rest};
take_parameter(_) ->
    unsupported.

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
           service_indicator => SI,
           network_indicator => NI,
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
    U = user_identity(User),
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
    SIs = [user_identity(SI) || <<SI:8/big>> <= Bin],
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
    update_params(Name, SM, V, Acc);
decode_parameter(_, _, _) ->
    unsupported.

update_params(Name, single, V, Acc) ->
    Acc#{Name => V};
update_params(Name, multiple, V, Acc) ->
    maps:update_with(Name, fun (Vs) -> [V|Vs] end, [V], Acc).


user_identity(User) ->
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

-spec decode_point_code({binary(), binary()}) -> [itu_point_code() | ansi_point_code()].
decode_point_code({<<Mask:8/big>>, <<PCbin:24/big>>}) ->
    MaskBits = trunc(math:pow(2, Mask) - 1),
    LowPC = PCbin band (trunc(math:pow(2, 24) - 1) bxor MaskBits),
    HighPC = PCbin bor MaskBits,
    DivPCFun = fun(PC) ->
                       case PC of
                           <<0:10, Zone:3, Region:8, SP:3>> ->     % ITU 14-bits
                               {itu, Zone, Region, SP};
                           <<Network:8, Cluster:8, Member:8>> ->   % ANSI 24-bits
                               {ansi, Network, Cluster, Member}
                       end
               end,
    [DivPCFun(<<PC:24/big>>) || PC <- lists:seq(LowPC, HighPC)].
