-module(otc_nas_5gs_5gsm).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1]).

spec() ->
    "3GPP TS 24.501 version 16.10.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) -> {ok, nas_5gs}.

decode(<<PTI:1/binary, MT:8/big, OIE/binary>>) ->
    MsgType = otc_nas_5gs:parse_msg_type(MT),
    Msg = decode_5gsm_msg(MsgType, OIE),
    Msg#{procedure_transaction_identity => PTI,
         message_type => MsgType}.

encode(#{message_type := MsgType} = Msg) ->
    Bin = encode_5gsm_msg(MsgType, Msg),
    MT = otc_nas_5gs:compose_msg_type(MsgType),
    PTI = maps:get(procedure_transaction_identity, Msg),
    <<PTI:1/binary, MT:8/big, Bin/binary>>.

decode_5gsm_msg(pdu_session_establishment_request, Bin0) ->
    {IntegrityProtectionMaximumDataRate, Bin1} = otc_l3_codec:decode_v(Bin0, 2),
    Opts = [{pdu_session_type, 16#9, tv, 1},
            {ssc_mode, 16#A, tv, 1},
            {'5gsm_capability', 16#28, tlv, {3, 15}},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {sm_pdu_dn_request_container, 16#39, tlv, {3, 255}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ds_tt_ethernet_port_mac_address, 16#6E, tlv, 8},
            {ue_ds_tt_residence_time, 16#6F, tlv, 10},
            {port_management_information_container, 16#74, tlve, {8, 65538}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3},
            {suggested_interface_identifier, 16#29, tlv, 11}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{integrity_protection_maximum_data_rate => IntegrityProtectionMaximumDataRate
              };
decode_5gsm_msg(pdu_session_establishment_accept, Bin0) ->
    {SelectedPduSessionType, Bin1} = otc_l3_codec:decode_v(Bin0, half),
    {SelectedSscMode, Bin2} = otc_l3_codec:decode_v(Bin1, half),
    {AuthorizedQosRules, Bin3} = otc_l3_codec:decode_lve(Bin2),
    {SessionAmbr, Bin4} = otc_l3_codec:decode_lv(Bin3),
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {pdu_address, 16#29, tlv, [7, 11, 15, 27, 31]},
            {rq_timer_value, 16#56, tv, 2},
            {s_nssai, 16#22, tlv, {3, 10}},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {dnn, 16#25, tlv, {3, 102}},
            {'5gsm_network_feature_support', 16#17, tlv, {3, 15}},
            {serving_plmn_rate_control, 16#18, tlv, 4},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {control_plane_only_indication, 16#C, tv, 1},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin4, Opts),
    Optionals#{selected_pdu_session_type => SelectedPduSessionType,
               selected_ssc_mode => SelectedSscMode,
               authorized_qos_rules => AuthorizedQosRules,
               session_ambr => SessionAmbr
              };
decode_5gsm_msg(pdu_session_establishment_reject, Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {allowed_ssc_mode, 16#F, tv, 1},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_authentication_command, Bin0) ->
    {EapMessage, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{eap_message => EapMessage
              };
decode_5gsm_msg(pdu_session_authentication_complete, Bin0) ->
    {EapMessage, Bin1} = otc_l3_codec:decode_lve(Bin0),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{eap_message => EapMessage
              };
decode_5gsm_msg(pdu_session_authentication_result, Bin0) ->
    Opts = [{eap_message, 16#78, tlve, {7, 1503}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_request, Bin0) ->
    Opts = [{'5gsm_capability', 16#28, tlv, {3, 15}},
            {'5gsm_cause', 16#59, tv, 2},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {integrity_protection_maximum_data_rate, 16#13, tv, 3},
            {requested_qos_rules, 16#7A, tlve, {7, 65538}},
            {requested_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_reject, Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_modification_command, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {session_ambr, 16#2A, tlv, 8},
            {rq_timer_value, 16#56, tv, 2},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {authorized_qos_rules, 16#7A, tlve, {7, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#1E, tlv, 4},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_complete, Bin0) ->
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_modification_command_reject, Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_request, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg(pdu_session_release_reject, Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_command, Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {access_type, 16#D, tv, 1}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              };
decode_5gsm_msg(pdu_session_release_complete, Bin0) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin0, Opts),
    Optionals;
decode_5gsm_msg('5gsm_status', Bin0) ->
    {SmCause, Bin1} = otc_l3_codec:decode_v(Bin0, 1),
    Opts = [],
    {Optionals, _Unknown} = otc_l3_codec:decode_iei_list(Bin1, Opts),
    Optionals#{'5gsm_cause' => SmCause
              }.

encode_5gsm_msg(pdu_session_establishment_request, #{integrity_protection_maximum_data_rate := IntegrityProtectionMaximumDataRate} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(IntegrityProtectionMaximumDataRate, 2, <<>>),
    Opts = [{pdu_session_type, 16#9, tv, 1},
            {ssc_mode, 16#A, tv, 1},
            {'5gsm_capability', 16#28, tlv, {3, 15}},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {sm_pdu_dn_request_container, 16#39, tlv, {3, 255}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ds_tt_ethernet_port_mac_address, 16#6E, tlv, 8},
            {ue_ds_tt_residence_time, 16#6F, tlv, 10},
            {port_management_information_container, 16#74, tlve, {8, 65538}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3},
            {suggested_interface_identifier, 16#29, tlv, 11}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_establishment_accept, #{selected_pdu_session_type := SelectedPduSessionType,
                                                    selected_ssc_mode := SelectedSscMode,
                                                    authorized_qos_rules := AuthorizedQosRules,
                                                    session_ambr := SessionAmbr} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SelectedSscMode, half, <<>>),
    Bin2 = otc_l3_codec:encode_v(SelectedPduSessionType, half, <<>>),
    Bin3 = otc_l3_codec:encode_lve(AuthorizedQosRules, <<>>),
    Bin4 = otc_l3_codec:encode_lv(SessionAmbr, <<>>),
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {pdu_address, 16#29, tlv, [7, 11, 15, 27, 31]},
            {rq_timer_value, 16#56, tv, 2},
            {s_nssai, 16#22, tlv, {3, 10}},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {eap_message, 16#78, tlve, {7, 1503}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {dnn, 16#25, tlv, {3, 102}},
            {'5gsm_network_feature_support', 16#17, tlv, {3, 15}},
            {serving_plmn_rate_control, 16#18, tlv, 4},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {control_plane_only_indication, 16#C, tv, 1},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, Bin2/bitstring, Bin3/bitstring, Bin4/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_establishment_reject, #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {allowed_ssc_mode, 16#F, tv, 1},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_authentication_command, #{eap_message := EapMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_authentication_complete, #{eap_message := EapMessage} = Msg) ->
    Bin1 = otc_l3_codec:encode_lve(EapMessage, <<>>),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_authentication_result, #{} = Msg) ->
    Opts = [{eap_message, 16#78, tlve, {7, 1503}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg(pdu_session_modification_request, #{} = Msg) ->
    Opts = [{'5gsm_capability', 16#28, tlv, {3, 15}},
            {'5gsm_cause', 16#59, tv, 2},
            {maximum_number_of_supported_packet_filters, 16#55, tv, 3},
            {always_on_pdu_session_requested, 16#B, tv, 1},
            {integrity_protection_maximum_data_rate, 16#13, tv, 3},
            {requested_qos_rules, 16#7A, tlve, {7, 65538}},
            {requested_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg(pdu_session_modification_reject, #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {re_attempt_indicator, 16#1D, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_modification_command, #{} = Msg) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {session_ambr, 16#2A, tlv, 8},
            {rq_timer_value, 16#56, tv, 2},
            {always_on_pdu_session_indication, 16#8, tv, 1},
            {authorized_qos_rules, 16#7A, tlve, {7, 65538}},
            {mapped_eps_bearer_contexts, 16#75, tlve, {7, 65538}},
            {authorized_qos_flow_descriptions, 16#79, tlve, {6, 65538}},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {atsss_container, 16#77, tlve, {3, 65538}},
            {ip_header_compression_configuration, 16#66, tlv, {5, 257}},
            {port_management_information_container, 16#74, tlve, {4, 65538}},
            {serving_plmn_rate_control, 16#1E, tlv, 4},
            {ethernet_header_compression_configuration, 16#1F, tlv, 3}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg(pdu_session_modification_complete, #{} = Msg) ->
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {port_management_information_container, 16#74, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg(pdu_session_modification_command_reject, #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_release_request, #{} = Msg) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg(pdu_session_release_reject, #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [{extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_release_command, #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [{back_off_timer_value, 16#37, tlv, 3},
            {eap_message, 16#78, tlve, {7, 1503}},
            {'5gsm_congestion_re_attempt_indicator', 16#61, tlv, 3},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}},
            {access_type, 16#D, tv, 1}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>;
encode_5gsm_msg(pdu_session_release_complete, #{} = Msg) ->
    Opts = [{'5gsm_cause', 16#59, tv, 2},
            {extended_protocol_configuration_options, 16#7B, tlve, {4, 65538}}],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    OptBin;
encode_5gsm_msg('5gsm_status', #{'5gsm_cause' := SmCause} = Msg) ->
    Bin1 = otc_l3_codec:encode_v(SmCause, 1, <<>>),
    Opts = [],
    OptBin = otc_l3_codec:encode_iei_list(Msg, Opts),
    <<Bin1/bitstring, OptBin/binary>>.

