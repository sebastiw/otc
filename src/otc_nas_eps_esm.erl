-module(otc_nas_eps_esm).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1]).

spec() ->
    "3GPP TS 24.301 version 16.8.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) -> {ok, nas_eps}.

decode(<<PTI:1/binary, MT:8/big, OIE/binary>>) ->
    MsgType = otc_nas_eps:parse_msg_type(MT),
    Msg = decode_esm_msg(MsgType, OIE),
    Msg#{procedure_transaction_identity => PTI,
         message_type => MsgType}.

encode(#{message_type := MsgType} = Msg) ->
    Bin = encode_esm_msg(MsgType, Msg),
    MT = otc_nas_eps:compose_msg_type(MsgType),
    PTI = maps:get(procedure_transaction_identity, Msg),
    <<PTI:1/binary, MT:8/big, Bin/binary>>.

decode_esm_msg(activate_dedicated_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(activate_dedicated_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(activate_dedicated_eps_bearer_context_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {EpsQos, Bin3} = otc_l3_codec:decode_lv(Bin2),
    {Tft, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               eps_qos => EpsQos,
               tft => Tft
              };
decode_esm_msg(activate_default_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(activate_default_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(activate_default_eps_bearer_context_request, Bin0) ->
    {EpsQos, Bin1} = otc_l3_codec:decode_lv(Bin0),
    {AccessPointName, Bin2} = otc_l3_codec:decode_lv(Bin1),
    {PdnAddress, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {connectivity_type, 16#B, tv, 1},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {control_plane_only_indication, 16#9, tv, 1},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#6E, tlv, 4},
            {extended_apn_ambr, 16#5F, tlv, 8}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{eps_qos => EpsQos,
               access_point_name => AccessPointName,
               pdn_address => PdnAddress
              };
decode_esm_msg(bearer_resource_allocation_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_allocation_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {TrafficFlowAggregate, Bin3} = otc_l3_codec:decode_lv(Bin2),
    {RequiredTrafficFlowQos, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity,
               traffic_flow_aggregate => TrafficFlowAggregate,
               required_traffic_flow_qos => RequiredTrafficFlowQos
              };
decode_esm_msg(bearer_resource_modification_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(bearer_resource_modification_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {EpsBearerIdentityForPacketFilter, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {TrafficFlowAggregate, Bin3} = otc_l3_codec:decode_lv(Bin2),
    Opts = [{required_traffic_flow_qos, 16#5B, tlv, {3, 15}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin3, Opts),
    Optionals#{eps_bearer_identity_for_packet_filter => EpsBearerIdentityForPacketFilter,
               traffic_flow_aggregate => TrafficFlowAggregate
              };
decode_esm_msg(deactivate_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(deactivate_eps_bearer_context_request, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {t3396_value, 16#37, tlv, 3},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(esm_dummy_message, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_information_request, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_information_response, Bin0) ->
    Opts = [{access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_status, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_accept, Bin0) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(modify_eps_bearer_context_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(modify_eps_bearer_context_request, Bin0) ->
    Opts = [{new_eps_qos, 16#5B, tlv, {3, 15}},
            {tft, 16#36, tlv, {3, 257}},
            {new_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_apn_ambr, 16#5F, tlv, 8},
            {extended_eps_qos, 16#5C, tlv, 12}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(notification, Bin0) ->
    {NotificationIndicator, Bin1} = otc_l3_codec:decode_lv(Bin0),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{notification_indicator => NotificationIndicator
              };
decode_esm_msg(pdn_connectivity_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(pdn_connectivity_request, Bin0) ->
    {PdnType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {RequestType, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{esm_information_transfer_flag, 16#D, tv, 1},
            {access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{request_type => RequestType,
               pdn_type => PdnType
              };
decode_esm_msg(pdn_disconnect_reject, Bin0) ->
    {EsmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{esm_cause => EsmCause
              };
decode_esm_msg(pdn_disconnect_request, Bin0) ->
    {_, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {LinkedEpsBearerIdentity, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin2, Opts),
    Optionals#{linked_eps_bearer_identity => LinkedEpsBearerIdentity
              };
decode_esm_msg(remote_ue_report, Bin0) ->
    Opts = [{remote_ue_context_connected, 16#79, tlve, {3, 65538}},
            {remote_ue_context_disconnected, 16#7A, tlve, {3, 65538}},
            {prose_key_management_function_address, 16#6F, tlv, {3, 19}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(remote_ue_report_response, Bin0) ->
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_esm_msg(esm_data_transport, Bin0) ->
    {UserDataContainer, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [{release_assistance_indication, 16#F, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{user_data_container => UserDataContainer
              }.

encode_esm_msg(activate_dedicated_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(activate_dedicated_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(activate_dedicated_eps_bearer_context_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity, eps_qos := EpsQos, tft := Tft} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(EpsQos, <<>>),
    Bin4 = otc_l3_codec:encode_lv(Tft, <<>>),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_esm_msg(activate_default_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(activate_default_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(activate_default_eps_bearer_context_request, #{eps_qos := EpsQos, access_point_name := AccessPointName, pdn_address := PdnAddress} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(EpsQos, <<>>),
    Bin2 = otc_l3_codec:encode_lv(AccessPointName, <<>>),
    Bin3 = otc_l3_codec:encode_lv(PdnAddress, <<>>),
    Opts = [{transaction_identifier, 16#5D, tlv, {3, 4}},
            {negotiated_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {connectivity_type, 16#B, tv, 1},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {control_plane_only_indication, 16#9, tv, 1},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#6E, tlv, 4},
            {extended_apn_ambr, 16#5F, tlv, 8}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_allocation_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_allocation_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity, traffic_flow_aggregate := TrafficFlowAggregate, required_traffic_flow_qos := RequiredTrafficFlowQos} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(TrafficFlowAggregate, <<>>),
    Bin4 = otc_l3_codec:encode_lv(RequiredTrafficFlowQos, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_modification_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(bearer_resource_modification_request, #{eps_bearer_identity_for_packet_filter := EpsBearerIdentityForPacketFilter, traffic_flow_aggregate := TrafficFlowAggregate} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(EpsBearerIdentityForPacketFilter, half, <<>>),
    Bin3 = otc_l3_codec:encode_lv(TrafficFlowAggregate, <<>>),
    Opts = [{required_traffic_flow_qos, 16#5B, tlv, {3, 15}},
            {esm_cause, 16#58, tv, 2},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, OptBin/binary>>;
encode_esm_msg(deactivate_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(deactivate_eps_bearer_context_request, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {t3396_value, 16#37, tlv, 3},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(esm_dummy_message, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_information_request, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_information_response, #{} = Msg) ->
    Opts = [{access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_status, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(modify_eps_bearer_context_accept, #{} = Msg) ->
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(modify_eps_bearer_context_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(modify_eps_bearer_context_request, #{} = Msg) ->
    Opts = [{new_eps_qos, 16#5B, tlv, {3, 15}},
            {tft, 16#36, tlv, {3, 257}},
            {new_qos, 16#30, tlv, {14, 22}},
            {negotiated_llc_sapi, 16#32, tv, 2},
            {radio_priority, 16#8, tv, 1},
            {packet_flow_identifier, 16#34, tlv, 3},
            {apn_ambr, 16#5E, tlv, {4, 8}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {wlan_offload_indication, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {extended_apn_ambr, 16#5F, tlv, 8},
            {extended_eps_qos, 16#5C, tlv, 12}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(notification, #{notification_indicator := NotificationIndicator} = Msg) ->
    Bin1 = otc_l3_codec:encode_lv(NotificationIndicator, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_connectivity_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {back_off_timer_value, 16#37, tlv, 3},
            {re_attempt_indicator, 16#6B, tlv, 3},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_connectivity_request, #{request_type := RequestType, pdn_type := PdnType} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(PdnType, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(RequestType, half, <<>>),
    Opts = [{esm_information_transfer_flag, 16#D, tv, 1},
            {access_point_name, 16#28, tlv, {3, 102}},
            {protocol_configuration_options, 16#27, tlv, {3, 253}},
            {device_properties, 16#C, tv, 1},
            {nbifom_container, 16#33, tlv, {3, 257}},
            {header_compression_configuration, 16#66, tlv, {5, 257}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_disconnect_reject, #{esm_cause := EsmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(EsmCause, 1, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_esm_msg(pdn_disconnect_request, #{linked_eps_bearer_identity := LinkedEpsBearerIdentity} = Msg) ->
    Bin1 = <<0:4>>,  % spare_half_octet
    Bin2 = otc_l3_codec:encode_v(LinkedEpsBearerIdentity, half, <<>>),
    Opts = [{protocol_configuration_options, 16#27, tlv, {3, 253}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, OptBin/binary>>;
encode_esm_msg(remote_ue_report, #{} = Msg) ->
    Opts = [{remote_ue_context_connected, 16#79, tlve, {3, 65538}},
            {remote_ue_context_disconnected, 16#7A, tlve, {3, 65538}},
            {prose_key_management_function_address, 16#6F, tlv, {3, 19}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(remote_ue_report_response, #{} = Msg) ->
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_esm_msg(esm_data_transport, #{user_data_container := UserDataContainer} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(UserDataContainer, <<>>),
    Opts = [{release_assistance_indication, 16#F, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>.
