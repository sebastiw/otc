-module(otc_mtp3).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

-include("include/mtp3.hrl").

spec() ->
    "ITU-T Q.704 (07/96) July 1996".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map);
codec({Map, PDU}) when is_map(Map) ->
    encode({Map, PDU}).

-type subproto() :: sccp | tup | isup |
                    dup_call | dup_reg | mtp_test |
                    broadband_isup | satellite_isup.

-spec next(map()) -> '$stop' | {ok, subproto()}.
next(#{service_indicator := SI} = V)
  when not is_map_key(payload, V) -> {ok, SI};
next(_) -> '$stop'.

decode(<<NI:2/big, NU:2/big, SI:4/big, RL0:32/little, Rest/binary>>) ->
    NetworkInd = parse_network_indicator(NI),
    ServiceInd = parse_service_indicator(SI),
    %% Routing Label is in reverse byte order
    RL = <<RL0:32/big>>,
    <<SLS:4/big, OPC:14/big, DPC:14/big>> = RL,
    Msg0 = decode_msg(ServiceInd, Rest),
    Msg = Msg0#{network_indicator => NetworkInd,
                national_use_spare => NU,
                service_indicator => ServiceInd,
                signalling_link_selection => SLS,
                originating_point_code => OPC,
                destination_point_code => DPC
               },
    case Msg of
        #{payload := D} when is_binary(D) ->
            {maps:without([payload], Msg), D};
        _ ->
            Msg
    end.

encode({Map, PDU}) when byte_size(PDU) > 0 ->
    encode(Map#{payload => PDU});
encode(#{network_indicator := NetworkInd, service_indicator := ServiceInd} = Msg) ->
    #{signalling_link_selection := SLS,
      originating_point_code := OPC,
      destination_point_code := DPC
     } = Msg,
    NI = compose_network_indicator(NetworkInd),
    NU = maps:get(national_use_spare, Msg, 2#00),
    SI = compose_service_indicator(ServiceInd),
    RL0 = <<SLS:4/big, OPC:14/big, DPC:14/big>>,
    %% Routing Label is in reverse byte order
    <<RL:32/little>> = RL0,
    Bin = encode_msg(ServiceInd, Msg),
    <<NI:2/big, NU:2/big, SI:4/big, RL:32/big, Bin/binary>>.

parse_service_indicator(?MTP3_SERVIND_MGMT) -> mgmt;
parse_service_indicator(?MTP3_SERVIND_MAINT) -> maint;
parse_service_indicator(?MTP3_SERVIND_SCCP) -> sccp;
parse_service_indicator(?MTP3_SERVIND_TUP) -> tup;
parse_service_indicator(?MTP3_SERVIND_ISUP) -> isup;
parse_service_indicator(?MTP3_SERVIND_DUP_CALL) -> dup_call;
parse_service_indicator(?MTP3_SERVIND_DUP_REG) -> dup_reg;
parse_service_indicator(?MTP3_SERVIND_MTP_TEST) -> mtp_test;
parse_service_indicator(?MTP3_SERVIND_BROADBAND_ISUP) -> broadband_isup;
parse_service_indicator(?MTP3_SERVIND_SATELLITE_ISUP) -> satellite_isup.

compose_service_indicator(mgmt) -> ?MTP3_SERVIND_MGMT;
compose_service_indicator(maint) -> ?MTP3_SERVIND_MAINT;
compose_service_indicator(sccp) -> ?MTP3_SERVIND_SCCP;
compose_service_indicator(tup) -> ?MTP3_SERVIND_TUP;
compose_service_indicator(isup) -> ?MTP3_SERVIND_ISUP;
compose_service_indicator(dup_call) -> ?MTP3_SERVIND_DUP_CALL;
compose_service_indicator(dup_reg) -> ?MTP3_SERVIND_DUP_REG;
compose_service_indicator(mtp_test) -> ?MTP3_SERVIND_MTP_TEST;
compose_service_indicator(broadband_isup) -> ?MTP3_SERVIND_BROADBAND_ISUP;
compose_service_indicator(satellite_isup) -> ?MTP3_SERVIND_SATELLITE_ISUP.

parse_network_indicator(?MTP3_NETIND_INTERNATIONAL) -> international;
parse_network_indicator(?MTP3_NETIND_INTERNATIONAL_SPARE) -> international_spare;
parse_network_indicator(?MTP3_NETIND_NATIONAL) -> national;
parse_network_indicator(?MTP3_NETIND_NATIONAL_SPARE) -> national_spare.

compose_network_indicator(international) -> ?MTP3_NETIND_INTERNATIONAL;
compose_network_indicator(international_spare) -> ?MTP3_NETIND_INTERNATIONAL_SPARE;
compose_network_indicator(national) -> ?MTP3_NETIND_NATIONAL;
compose_network_indicator(national_spare) -> ?MTP3_NETIND_NATIONAL_SPARE.

decode_msg(mgmt, Bin) -> %% Q.704
    #{payload => decode_mgmt(Bin)};
decode_msg(maint, Bin) -> %% Q.707
    #{payload => decode_maint(Bin)};
decode_msg(_, Bin) ->
    #{payload => Bin}.

encode_msg(mgmt, #{payload := Msg}) -> %% Q.704
    encode_mgmt(Msg);
encode_msg(maint, #{payload := Msg}) -> %% Q.707
    encode_maint(Msg);
encode_msg(_, #{payload := Bin}) ->
    Bin.

decode_mgmt(<<?MTP3_MGMT_H1_CHM_COO:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<FSN:7, _:1>> = Bin,
    #{message_type => changeover_order,
      forward_sequence_number => FSN};
decode_mgmt(<<?MTP3_MGMT_H1_CHM_COA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<FSN:7, _:1>> = Bin,
    #{message_type => changeover_ack,
      forward_sequence_number => FSN};
decode_mgmt(<<?MTP3_MGMT_H1_CHM_XCO:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<FSN:24>> = Bin,
    #{message_type => extended_changeover_order,
      forward_sequence_number => FSN};
decode_mgmt(<<?MTP3_MGMT_H1_CHM_XCA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<FSN:24>> = Bin,
    #{message_type => extended_changeover_ack,
      forward_sequence_number => FSN};
decode_mgmt(<<?MTP3_MGMT_H1_CHM_CBD:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<CBC:8>> = Bin,
    #{message_type => changeback_declaration,
      changeback_code => CBC};
decode_mgmt(<<?MTP3_MGMT_H1_CHM_CBA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>) ->
    <<CBC:8>> = Bin,
    #{message_type => changeback_ack,
      changeback_code => CBC};
decode_mgmt(<<?MTP3_MGMT_H1_ECM_ECO:4, ?MTP3_MGMT_H0_ECM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => emergency_changeover};
decode_mgmt(<<?MTP3_MGMT_H1_ECM_ECA:4, ?MTP3_MGMT_H0_ECM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => emergency_changeover_ack};
decode_mgmt(<<?MTP3_MGMT_H1_FCM_RCT:4, ?MTP3_MGMT_H0_FCM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => route_congestion_test};
decode_mgmt(<<?MTP3_MGMT_H1_FCM_TFC:4, ?MTP3_MGMT_H0_FCM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => transfer_controlled};
decode_mgmt(<<?MTP3_MGMT_H1_TFM_TFP:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>) ->
    <<DPC:14, _:2>> = Bin,
    #{message_type => transfer_prohibited,
      destination_point_code => DPC};
decode_mgmt(<<?MTP3_MGMT_H1_TFM_TFR:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>) ->
    <<DPC:14, _:2>> = Bin,
    #{message_type => transfer_restricted,
      destination_point_code => DPC};
decode_mgmt(<<?MTP3_MGMT_H1_TFM_TFA:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>) ->
    <<DPC:14, _:2>> = Bin,
    #{message_type => transfer_allowed,
      destination_point_code => DPC};
decode_mgmt(<<?MTP3_MGMT_H1_RSM_RST:4, ?MTP3_MGMT_H0_RSM:4, Bin/binary>>) ->
    <<DPC:14, _:2>> = Bin,
    #{message_type => route_set_test_prohibited,
      destination_point_code => DPC};
decode_mgmt(<<?MTP3_MGMT_H1_RSM_RSR:4, ?MTP3_MGMT_H0_RSM:4, Bin/binary>>) ->
    <<DPC:14, _:2>> = Bin,
    #{message_type => route_set_test_restricted,
      destination_point_code => DPC};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LIN:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_inhibit};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LUN:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_uninhibit};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LIA:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_inhibit_ack};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LUA:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_uninhibit_ack};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LID:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_inhibit_denied};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LFU:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_force_uninhibit};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LLT:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_local_inhibit_test};
decode_mgmt(<<?MTP3_MGMT_H1_MIM_LRT:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => link_remote_inhibit_test};
decode_mgmt(<<?MTP3_MGMT_H1_TRM_TRA:4, ?MTP3_MGMT_H0_TRM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => traffic_restart_allowed};
decode_mgmt(<<?MTP3_MGMT_H1_DLM_DLC:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>) ->
    <<SDLID:12, _:4>> = Bin,
    #{message_type => data_link_connection_order,
      signalling_data_link_id => SDLID};
decode_mgmt(<<?MTP3_MGMT_H1_DLM_CSS:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => connection_successful};
decode_mgmt(<<?MTP3_MGMT_H1_DLM_CNS:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => connection_not_successful};
decode_mgmt(<<?MTP3_MGMT_H1_DLM_CNP:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>) ->
    <<>> = Bin,
    #{message_type => connection_not_possible};
decode_mgmt(<<?MTP3_MGMT_H1_UFC_UPU:4, ?MTP3_MGMT_H0_UFC:4, Bin/binary>>) ->
    <<DPC:14, _:2, UPID:4, UC:4>> = Bin,
    UnavailabilityCause = parse_unavailability_cause(UC),
    #{message_type => user_part_unavailable,
      destination_point_code => DPC,
      user_part_id => UPID,
      unavailability_cause => UnavailabilityCause
     }.

encode_mgmt(#{message_type := changeover_order,
              forward_sequence_number := FSN}) ->
    Bin = <<FSN:7, 0:1>>,
    <<?MTP3_MGMT_H1_CHM_COO:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := changeover_ack,
              forward_sequence_number := FSN}) ->
    Bin = <<FSN:7, 0:1>>,
    <<?MTP3_MGMT_H1_CHM_COA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := extended_changeover_order,
              forward_sequence_number := FSN}) ->
    Bin = <<FSN:24>>,
    <<?MTP3_MGMT_H1_CHM_XCO:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := extended_changeover_ack,
              forward_sequence_number := FSN}) ->
    Bin = <<FSN:24>>,
    <<?MTP3_MGMT_H1_CHM_XCA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := changeback_declaration,
              changeback_code := CBC}) ->
    Bin = <<CBC:8>>,
    <<?MTP3_MGMT_H1_CHM_CBD:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := changeback_ack,
              changeback_code := CBC}) ->
    Bin = <<CBC:8>>,
    <<?MTP3_MGMT_H1_CHM_CBA:4, ?MTP3_MGMT_H0_CHM:4, Bin/binary>>;
encode_mgmt(#{message_type := emergency_changeover}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_ECM_ECO:4, ?MTP3_MGMT_H0_ECM:4, Bin/binary>>;
encode_mgmt(#{message_type := emergency_changeover_ack}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_ECM_ECA:4, ?MTP3_MGMT_H0_ECM:4, Bin/binary>>;
encode_mgmt(#{message_type := route_congestion_test}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_FCM_RCT:4, ?MTP3_MGMT_H0_FCM:4, Bin/binary>>;
encode_mgmt(#{message_type := transfer_controlled}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_FCM_TFC:4, ?MTP3_MGMT_H0_FCM:4, Bin/binary>>;
encode_mgmt(#{message_type := transfer_prohibited,
              destination_point_code := DPC}) ->
    Bin = <<DPC:14, 0:2>>,
    <<?MTP3_MGMT_H1_TFM_TFP:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>;
encode_mgmt(#{message_type := transfer_restricted,
              destination_point_code := DPC}) ->
    Bin = <<DPC:14, 0:2>>,
    <<?MTP3_MGMT_H1_TFM_TFR:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>;
encode_mgmt(#{message_type := transfer_allowed,
              destination_point_code := DPC}) ->
    Bin = <<DPC:14, 0:2>>,
    <<?MTP3_MGMT_H1_TFM_TFA:4, ?MTP3_MGMT_H0_TFM:4, Bin/binary>>;
encode_mgmt(#{message_type := route_set_test_prohibited,
              destination_point_code := DPC}) ->
    Bin = <<DPC:14, 0:2>>,
    <<?MTP3_MGMT_H1_RSM_RST:4, ?MTP3_MGMT_H0_RSM:4, Bin/binary>>;
encode_mgmt(#{message_type := route_set_test_restricted,
              destination_point_code := DPC}) ->
    Bin = <<DPC:14, 0:2>>,
    <<?MTP3_MGMT_H1_RSM_RSR:4, ?MTP3_MGMT_H0_RSM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_inhibit}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LIN:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_uninhibit}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LUN:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_inhibit_ack}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LIA:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_uninhibit_ack}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LUA:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_inhibit_denied}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LID:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_force_uninhibit}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LFU:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_local_inhibit_test}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LLT:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := link_remote_inhibit_test}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_MIM_LRT:4, ?MTP3_MGMT_H0_MIM:4, Bin/binary>>;
encode_mgmt(#{message_type := traffic_restart_allowed}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_TRM_TRA:4, ?MTP3_MGMT_H0_TRM:4, Bin/binary>>;
encode_mgmt(#{message_type := data_link_connection_order,
              signalling_data_link_id := SDLID}) ->
    Bin = <<SDLID:12, 0:4>>,
    <<?MTP3_MGMT_H1_DLM_DLC:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>;
encode_mgmt(#{message_type := connection_successful}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_DLM_CSS:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>;
encode_mgmt(#{message_type := connection_not_successful}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_DLM_CNS:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>;
encode_mgmt(#{message_type := connection_not_possible}) ->
    Bin = <<>>,
    <<?MTP3_MGMT_H1_DLM_CNP:4, ?MTP3_MGMT_H0_DLM:4, Bin/binary>>;
encode_mgmt(#{message_type := user_part_unavailable,
              destination_point_code := DPC,
              user_part_id := UPID,
              unavailability_cause := UnavailabilityCause
             }) ->
    UC = compose_unavailability_cause(UnavailabilityCause),
    Bin = <<DPC:14, 0:2, UPID:4, UC:4>>,
    <<?MTP3_MGMT_H1_UFC_UPU:4, ?MTP3_MGMT_H0_UFC:4, Bin/binary>>.

parse_unavailability_cause(?MTP3_UPU_CAUSE_UNKNOWN) -> unknown;
parse_unavailability_cause(?MTP3_UPU_CAUSE_UNEQUIPPED) -> unequipped;
parse_unavailability_cause(?MTP3_UPU_CAUSE_INACCESSIBLE) -> inaccessible.

compose_unavailability_cause(unknown) -> ?MTP3_UPU_CAUSE_UNKNOWN;
compose_unavailability_cause(unequipped) -> ?MTP3_UPU_CAUSE_UNEQUIPPED;
compose_unavailability_cause(inaccessible) -> ?MTP3_UPU_CAUSE_INACCESSIBLE.

decode_maint(<<?MTP3_MAINT_H1_TEST_SLTM:4, ?MTP3_MAINT_H0_TEST:4, Bin/binary>>) ->
    <<L:4, _:4, Rest/binary>> = Bin,
    <<TP:L/binary>> = Rest,
    #{message_type => signalling_link_test,
      test_pattern => TP};
decode_maint(<<?MTP3_MAINT_H1_TEST_SLTA:4, ?MTP3_MAINT_H0_TEST:4, Bin/binary>>) ->
    <<L:4, _:4, Rest/binary>> = Bin,
    <<TP:L/binary>> = Rest,
    #{message_type => signalling_link_test_ack,
      test_pattern => TP}.

encode_maint(#{message_type := signalling_link_test,
               test_pattern := TP}) ->
    L = byte_size(TP),
    Bin = <<L:4, 0:4, TP/binary>>,
    <<?MTP3_MAINT_H1_TEST_SLTM:4, ?MTP3_MAINT_H0_TEST:4, Bin/binary>>;
encode_maint(#{message_type := signalling_link_test_ack,
               test_pattern := TP}) ->
    L = byte_size(TP),
    Bin = <<L:4, 0:4, TP/binary>>,
    <<?MTP3_MAINT_H1_TEST_SLTA:4, ?MTP3_MAINT_H0_TEST:4, Bin/binary>>.

