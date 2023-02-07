-module(otc_sccp).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

-include("include/sccp.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "ITU-T Q.713 (03/2001)".

codec(Bin) when is_binary(Bin) ->
    case decode(Bin) of
        #{long_data := LD} = Msg2
          when is_map(LD) ->
            Msg2;
        #{long_data := LD, message_type := MessageType} = Msg2
          when ludt =:= MessageType;
               ludts =:= MessageType ->
            {Msg2, LD};
        #{data := D} = Msg2
          when is_map(D) ->
            Msg2;
        #{data := D, message_type := MessageType} = Msg2
          when cr =:= MessageType;
               cc =:= MessageType;
               cref =:= MessageType;
               rlsd =:= MessageType;
               dt1 =:= MessageType;
               dt2 =:= MessageType;
               udt =:= MessageType;
               udts =:= MessageType;
               ed =:= MessageType;
               xudt =:= MessageType;
               xudts =:= MessageType ->
            {Msg2, D};
        Msg2 ->
            Msg2
    end;
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) -> '$stop'.

decode(<<MT:8/big, Rest/binary>>) ->
    MessageType = parse_message_type(MT),
    Msg = decode_msg(MessageType, Rest),
    Msg#{message_type => MessageType}.

encode(#{message_type := MessageType} = Msg) ->
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg),
    <<MT:8/big, Bin/binary>>.

parse_message_type(?SCCP_MSG_TYPE_CR) -> cr;
parse_message_type(?SCCP_MSG_TYPE_CC) -> cc;
parse_message_type(?SCCP_MSG_TYPE_CREF) -> cref;
parse_message_type(?SCCP_MSG_TYPE_RLSD) -> rlsd;
parse_message_type(?SCCP_MSG_TYPE_RLC) -> rlc;
parse_message_type(?SCCP_MSG_TYPE_DT1) -> dt1;
parse_message_type(?SCCP_MSG_TYPE_DT2) -> dt2;
parse_message_type(?SCCP_MSG_TYPE_AK) -> ak;
parse_message_type(?SCCP_MSG_TYPE_UDT) -> udt;
parse_message_type(?SCCP_MSG_TYPE_UDTS) -> udts;
parse_message_type(?SCCP_MSG_TYPE_ED) -> ed;
parse_message_type(?SCCP_MSG_TYPE_EA) -> ea;
parse_message_type(?SCCP_MSG_TYPE_RSR) -> rsr;
parse_message_type(?SCCP_MSG_TYPE_RSC) -> rsc;
parse_message_type(?SCCP_MSG_TYPE_ERR) -> err;
parse_message_type(?SCCP_MSG_TYPE_IT) -> it;
parse_message_type(?SCCP_MSG_TYPE_XUDT) -> xudt;
parse_message_type(?SCCP_MSG_TYPE_XUDTS) -> xudts;
parse_message_type(?SCCP_MSG_TYPE_LUDT) -> ludt;
parse_message_type(?SCCP_MSG_TYPE_LUDTS) -> ludts.

compose_message_type(cr) -> ?SCCP_MSG_TYPE_CR;
compose_message_type(cc) -> ?SCCP_MSG_TYPE_CC;
compose_message_type(cref) -> ?SCCP_MSG_TYPE_CREF;
compose_message_type(rlsd) -> ?SCCP_MSG_TYPE_RLSD;
compose_message_type(rlc) -> ?SCCP_MSG_TYPE_RLC;
compose_message_type(dt1) -> ?SCCP_MSG_TYPE_DT1;
compose_message_type(dt2) -> ?SCCP_MSG_TYPE_DT2;
compose_message_type(ak) -> ?SCCP_MSG_TYPE_AK;
compose_message_type(udt) -> ?SCCP_MSG_TYPE_UDT;
compose_message_type(udts) -> ?SCCP_MSG_TYPE_UDTS;
compose_message_type(ed) -> ?SCCP_MSG_TYPE_ED;
compose_message_type(ea) -> ?SCCP_MSG_TYPE_EA;
compose_message_type(rsr) -> ?SCCP_MSG_TYPE_RSR;
compose_message_type(rsc) -> ?SCCP_MSG_TYPE_RSC;
compose_message_type(err) -> ?SCCP_MSG_TYPE_ERR;
compose_message_type(it) -> ?SCCP_MSG_TYPE_IT;
compose_message_type(xudt) -> ?SCCP_MSG_TYPE_XUDT;
compose_message_type(xudts) -> ?SCCP_MSG_TYPE_XUDTS;
compose_message_type(ludt) -> ?SCCP_MSG_TYPE_LUDT;
compose_message_type(ludts) -> ?SCCP_MSG_TYPE_LUDTS.

decode_msg(cr, Bin) ->
    NumPointers = 2,
    <<SLR:3/binary, PC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{credit, 3},
                         {calling_party_address, {4, n}},
                         {data, {3, 130}},
                         {hop_counter, 3},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{source_local_reference => decode_parameter(source_local_reference, SLR),
               protocol_class => decode_parameter(protocol_class, PC),
               called_party_address => decode_parameter(called_party_address, CdPA)};
decode_msg(cc, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, SLR:3/binary, PC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{credit, 3},
                         {called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR),
               protocol_class => decode_parameter(protocol_class, PC)};
decode_msg(cref, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, RC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               refusal_cause => decode_parameter(refusal_cause, RC)};
decode_msg(rlsd, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, SLR:3/binary, RC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR),
               release_cause => decode_parameter(release_cause, RC)};
decode_msg(rlc, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR)};
decode_msg(dt1, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, SR:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               segmenting_reassembling => decode_parameter(segmenting_reassembling, SR),
               data => decode_parameter(data, D)};
decode_msg(dt2, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, SS:2/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               sequencing_segmenting => decode_parameter(sequencing_segmenting, SS),
               data => decode_parameter(data, D)};
decode_msg(ak, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, RSN:1/binary, C:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               receive_sequence_number => decode_parameter(receive_sequence_number, RSN),
               credit => decode_parameter(credit, C)};
decode_msg(udt, Bin) ->
    NumPointers = 3,
    <<PC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D] = separate_fields(Pointers, Bin1),
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = #{},
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => decode_data(data, D, CalledPartyAddress, CallingPartyAddress)};
decode_msg(udts, Bin) ->
    NumPointers = 3,
    <<RC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D] = separate_fields(Pointers, Bin1),
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = #{},
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => decode_data(data, D, CalledPartyAddress, CallingPartyAddress)};
decode_msg(ed, Bin) ->
    NumPointers = 1,
    <<DLR:3/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               data => decode_parameter(data, D)};
decode_msg(ea, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR)};
decode_msg(rsr, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, RC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR),
               reset_cause => decode_parameter(reset_cause, RC)};
decode_msg(rsc, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR)};
decode_msg(err, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, EC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               error_cause => decode_parameter(error_cause, EC)};
decode_msg(it, Bin) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, PC:1/binary, SS:2/binary, C:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR),
               source_local_reference => decode_parameter(source_local_reference, SLR),
               protocol_class => decode_parameter(protocol_class, PC),
               sequencing_segmenting => decode_parameter(sequencing_segmenting, SS),
               credit => decode_parameter(credit, C)};
decode_msg(xudt, Bin) ->
    NumPointers = 4,
    <<PC:1/binary, HC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D, OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => decode_data(data, D, CalledPartyAddress, CallingPartyAddress)};
decode_msg(xudts, Bin) ->
    NumPointers = 4,
    <<RC:1/binary, HC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D, OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => decode_data(data, D, CalledPartyAddress, CallingPartyAddress)};
decode_msg(ludt, Bin) ->
    NumPointers = 4,
    <<PC:1/binary, HC:1/binary, Pointers:(2*NumPointers)/binary, Bin1/binary>> = Bin,
    <<CdPAP:16/big, CgPAP:16/big, LDP:16/big, OptBinP:16/big>> = Pointers,
    CdPAL = binary:at(Bin1, CdPAP),
    CdPA = binary:part(Bin1, CdPAP+1, CdPAL-1),
    CgPAL = binary:at(Bin1, CgPAP),
    CgPA = binary:part(Bin1, CgPAP+1, CgPAL-1),
    <<LDL:16/big>> = binary:part(Bin1, LDP, 2),
    LD = binary:part(Bin1, LDP+1, LDL-1),
    OptBin = binary:part(Bin1, byte_size(Bin1), -OptBinP),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               long_data => decode_data(long_data, LD, CalledPartyAddress, CallingPartyAddress)};
decode_msg(ludts, Bin) ->
    NumPointers = 4,
    <<RC:1/binary, HC:1/binary, Pointers:(2*NumPointers)/binary, Bin1/binary>> = Bin,
    <<CdPAP:16/big, CgPAP:16/big, LDP:16/big, OptBinP:16/big>> = Pointers,
    CdPAL = binary:at(Bin1, CdPAP),
    CdPA = binary:part(Bin1, CdPAP+1, CdPAL-1),
    CgPAL = binary:at(Bin1, CgPAP),
    CgPA = binary:part(Bin1, CgPAP+1, CgPAL-1),
    <<LDL:16/big>> = binary:part(Bin1, LDP, 2),
    LD = binary:part(Bin1, LDP+1, LDL-1),
    OptBin = binary:part(Bin1, byte_size(Bin1), -OptBinP),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA),
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               long_data => decode_data(long_data, LD, CalledPartyAddress, CallingPartyAddress)}.

separate_fields_test() ->
    %% Example:
    %% Expected order: [CdPA, CgPA, LD, OptBin]
    %% PointerBin: <<94 = PointCdPA, 3 = PointCgPA, 14 = PointLD, 0 = PointOpt>>
    %% Bin: <<CgPABin1, LDBin2, CdPABin3, OptBin4>>
    CdPA = <<4, "CdPA">>,
    CgPA = <<4, "CgPA">>,
    D = <<1, "D">>,
    Bin = <<CgPA/binary, D/binary, CdPA/binary>>,
    Vs = separate_fields(<<94, 3, 14, 0>>, Bin),
    ?assertEqual([<<"CdPA">>, <<"CgPA">>, <<"D">>, <<>>], Vs).

separate_fields(PointerBin, Bin) ->
    %% Split and add appearance counter
    {Pointers, Vs} = separate_fields(PointerBin, Bin, {1, [], []}),
    %% Sort pointers (with optional last)
    Ordered = case lists:sort(Pointers) of
                  [{0, _} = O|Rest] ->
                      %% No optionals, put it last
                      Rest ++ [O];
                  R -> R
              end,
    %% Zip with values and order on the appearance counter
    Zipped = lists:zip(Ordered, Vs),
    ReOrdered = lists:sort(fun ({{_, A}, _}, {{_, B}, _}) -> A < B end, Zipped),
    {_, Vals} = lists:unzip(ReOrdered),
    Vals.

separate_fields(<<>>, _, {_, PAcc, BAcc}) ->
    {PAcc, lists:reverse(BAcc)};
separate_fields(<<0:8/big>>, _, {N, PAcc, BAcc}) ->
    {[{0, N}|PAcc], lists:reverse([<<>>|BAcc])};
separate_fields(<<P:8/big, R/binary>>, <<L:8/big, Bin/binary>> = Field, {N, PAcc, BAcc}) ->
    case byte_size(Bin) < L of
        true ->
            %% When Length is longer then rest of binary
            %% pass it to decode_parameters/2
            separate_fields(<<>>, <<>>, {N+1, [{P, N}|PAcc], [Field | BAcc]});
        false ->
            <<Val:L/binary, Rest/binary>> = Bin,
            separate_fields(R, Rest, {N+1, [{P, N}|PAcc], [Val|BAcc]})
    end.

-define(IS_SCCP_MGMT,
        #{routing_indicator := subsystem_number, subsystem_number := management}).

decode_data(_, D, ?IS_SCCP_MGMT, ?IS_SCCP_MGMT) ->
    decode_mgmt_data(D);
decode_data(Type, D, _, _) ->
    decode_parameter(Type, D).

encode_data(_, D, ?IS_SCCP_MGMT, ?IS_SCCP_MGMT) ->
    encode_mgmt_data(D);
encode_data(Type, D, _, _) ->
    encode_parameter(Type, D).

decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => allowed,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => prohibited,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => status_test,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => out_of_service_request,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => out_of_service_grant,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode_mgmt_data(<<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, ASSN:8, APC:2/binary, SMI:8, CL:8>>) ->
    #{format_identifier => congested,
      affected_subsystem_number => parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI,
      congestion_level => CL}.

encode_mgmt_data(#{format_identifier := allowed,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode_mgmt_data(#{format_identifier := prohibited,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode_mgmt_data(#{format_identifier := status_test,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode_mgmt_data(#{format_identifier := out_of_service_request,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode_mgmt_data(#{format_identifier := out_of_service_grant,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode_mgmt_data(#{format_identifier := congested,
                   affected_subsystem_number := ASSN,
                   affected_point_code := APC,
                   subsystem_multiplicity_indicator := SMI,
                   congestion_level := CL}) ->
    <<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, (compose_ssn(ASSN)):8, APC:2/binary, SMI:8, CL:8>>.

encode_msg(cr,
           #{source_local_reference := SourceLocalReference,
             protocol_class := ProtocolClass,
             called_party_address := CalledPartyAddress} = Msg) ->
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    PC = encode_parameter(protocol_class, ProtocolClass),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    AllowedParameters = [{credit, 3},
                         {calling_party_address, {4, n}},
                         {data, {3, 130}},
                         {hop_counter, 3},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         CdPALen+2;
                     false ->
                         0
                 end,
    Pointers = <<(2):8/big, OptPointer:8/big>>,
    <<SLR/binary, PC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary,
      OptBin/binary>>;
encode_msg(cc,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             protocol_class := ProtocolClass} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    PC = encode_parameter(protocol_class, ProtocolClass),
    AllowedParameters = [{credit, 3},
                         {called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         1;
                     false ->
                         0
                 end,
    Pointers = <<OptPointer:8/big>>,
    <<DLR/binary, SLR/binary, PC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(cref,
           #{destination_local_reference := DestinationLocalReference,
             refusal_cause := RefusalCause} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    RC = encode_parameter(refusal_cause, RefusalCause),
    AllowedParameters = [{called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         1;
                     false ->
                         0
                 end,
    Pointers = <<OptPointer:8/big>>,
    <<DLR/binary, RC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rlsd,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             release_cause := ReleaseCause} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    RC = encode_parameter(release_cause, ReleaseCause),
    AllowedParameters = [{data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         1;
                     false ->
                         0
                 end,
    Pointers = <<OptPointer:8/big>>,
    <<DLR/binary, SLR/binary, RC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rlc,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(dt1,
           #{destination_local_reference := DestinationLocalReference,
             segmenting_reassembling := SegmentingReassembling,
             data := Data} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SR = encode_parameter(segmenting_reassembling, SegmentingReassembling),
    D = encode_parameter(data, Data),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, SR/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(dt2,
           #{destination_local_reference := DestinationLocalReference,
             sequencing_segmenting := SequencingSegmenting,
             data := Data} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SS = encode_parameter(sequencing_segmenting, SequencingSegmenting),
    D = encode_parameter(data, Data),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, SS/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ak,
           #{destination_local_reference := DestinationLocalReference,
             receive_sequence_number := ReceiveSequenceNumber,
             credit := Credit} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    RSN = encode_parameter(receive_sequence_number, ReceiveSequenceNumber),
    C = encode_parameter(credit, Credit),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, RSN/binary, C/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(udt,
           #{protocol_class := ProtocolClass,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := Data} = Msg) ->
    PC = encode_parameter(protocol_class, ProtocolClass),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    D = encode_data(data, Data, CalledPartyAddress, CallingPartyAddress),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<(3):8/big, (CdPALen+3):8/big, (CgPALen+CdPALen+3):8/big>>,
    <<PC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(udts,
           #{return_cause := ReturnCause,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := Data} = Msg) ->
    RC = encode_parameter(return_cause, ReturnCause),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    D = encode_data(data, Data, CalledPartyAddress, CallingPartyAddress),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<(3):8/big, (CdPALen+3):8/big, (CgPALen+CdPALen+3):8/big>>,
    <<RC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ed,
           #{destination_local_reference := DestinationLocalReference,
             data := Data} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    D = encode_parameter(data, Data),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ea,
           #{destination_local_reference := DestinationLocalReference} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rsr,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             reset_cause := ResetCause} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    RC = encode_parameter(reset_cause, ResetCause),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, RC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rsc,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(err,
           #{destination_local_reference := DestinationLocalReference,
             error_cause := ErrorCause} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    EC = encode_parameter(error_cause, ErrorCause),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, EC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(it,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             protocol_class := ProtocolClass,
             sequencing_segmenting := SequencingSegmenting,
             credit := Credit} = Msg) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference),
    SLR = encode_parameter(source_local_reference, SourceLocalReference),
    PC = encode_parameter(protocol_class, ProtocolClass),
    SS = encode_parameter(sequencing_segmenting, SequencingSegmenting),
    C = encode_parameter(credit, Credit),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, PC/binary, SS/binary, C/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(xudt,
           #{protocol_class := ProtocolClass,
             hop_counter := HopCounter,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := Data} = Msg) ->
    PC = encode_parameter(protocol_class, ProtocolClass),
    HC = encode_parameter(hop_counter, HopCounter),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    D = encode_data(data, Data, CalledPartyAddress, CallingPartyAddress),
    DLen = byte_size(D),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         DLen+CgPALen+CdPALen+4;
                     false ->
                         0
                 end,
    Pointers = <<(4):8/big, (CdPALen+4):8/big, (CgPALen+CdPALen+4):8/big, OptPointer:8/big>>,
    <<PC/binary, HC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(xudts,
           #{return_cause := ReturnCause,
             hop_counter := HopCounter,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := Data} = Msg) ->
    RC = encode_parameter(return_cause, ReturnCause),
    HC = encode_parameter(hop_counter, HopCounter),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    D = encode_data(data, Data, CalledPartyAddress, CallingPartyAddress),
    DLen = byte_size(D),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         DLen+CgPALen+CdPALen+4;
                     false ->
                         0
                 end,
    Pointers = <<(4):8/big, (CdPALen+4):8/big, (CgPALen+CdPALen+4):8/big, OptPointer:8/big>>,
    <<RC/binary, HC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ludt,
           #{protocol_class := ProtocolClass,
             hop_counter := HopCounter,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             long_data := LongData} = Msg) ->
    PC = encode_parameter(protocol_class, ProtocolClass),
    HC = encode_parameter(hop_counter, HopCounter),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    LD = encode_data(long_data, LongData, CalledPartyAddress, CallingPartyAddress),
    LDLen = byte_size(LD),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         LDLen+CgPALen+CdPALen+4;
                     false ->
                         0
                 end,
    Pointers = <<(4):16/big, (CdPALen+4):16/big, (CgPALen+CdPALen+4):16/big, OptPointer:16/big>>,
    <<PC/binary, HC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, LDLen:16/big, LD/binary,
      OptBin/binary>>;
encode_msg(ludts,
           #{return_cause := ReturnCause,
             hop_counter := HopCounter,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             long_data := LongData} = Msg) ->
    RC = encode_parameter(return_cause, ReturnCause),
    HC = encode_parameter(hop_counter, HopCounter),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress),
    CgPALen = byte_size(CgPA),
    LD = encode_data(long_data, LongData, CalledPartyAddress, CallingPartyAddress),
    LDLen = byte_size(LD),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters),
    OptPointer = case byte_size(OptBin) > 0 of
                     true ->
                         LDLen+CgPALen+CdPALen+4;
                     false ->
                         0
                 end,
    Pointers = <<(4):16/big, (CdPALen+4):16/big, (CgPALen+CdPALen+4):16/big, OptPointer:16/big>>,
    <<RC/binary, HC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, LDLen:16/big, LD/binary,
      OptBin/binary>>.

decode_parameters(Bin, Parameters) ->
    decode_parameters(Bin, Parameters, #{}).

decode_parameters(_, [], Acc) ->
    Acc;
decode_parameters(<<>>, _, Acc) ->
    Acc;
decode_parameters(<<?SCCP_IEI_END_OF_OPTIONAL_PARAMETERS>>, _, Acc) ->
    Acc;
decode_parameters(<<IEI:8/big, Len:8/big, Bin0/binary>>, Os, Acc) ->
    <<V:Len/binary, Rest/binary>> = Bin0,
    Param = parse_iei(IEI),
    case lists:keytake(Param, 1, Os) of
        {value, {Name, _, _}, NOs} ->
            Par = decode_parameter(Name, V),
            decode_parameters(Rest, NOs, Acc#{Name => Par});
        {value, {Name, _}, NOs} ->
            Par = decode_parameter(Name, V),
            decode_parameters(Rest, NOs, Acc#{Name => Par});
        false ->
            decode_parameters(Rest, Os, Acc)
    end.

encode_parameters(Msg, Parameters) ->
    encode_parameters(Msg, Parameters, <<>>).

encode_parameters(_, [], Acc) ->
    Acc;
encode_parameters(_, [{end_of_optional_parameters, _}|_], Acc) when byte_size(Acc) > 0 ->
    <<Acc/binary, ?SCCP_IEI_END_OF_OPTIONAL_PARAMETERS:8/big>>;
encode_parameters(Msg, [{Name, _}|Os], Acc) when is_map_key(Name, Msg) ->
    V = maps:get(Name, Msg),
    Bin = encode_parameter(Name, V),
    IEI = compose_iei(Name),
    Len = byte_size(Bin),
    NAcc = <<IEI:8/big, Len:8/big, Bin/binary, Acc/binary>>,
    encode_parameters(Msg, Os, NAcc);
encode_parameters(Msg, [_|Os], Acc) ->
    encode_parameters(Msg, Os, Acc).

%% Mandatory, taken care of elsewhere:
%% parse_iei(?SCCP_IEI_DESTINATION_LOCAL_REFERENCE) -> destination_local_reference;
%% parse_iei(?SCCP_IEI_SOURCE_LOCAL_REFERENCE) -> source_local_reference;
%% parse_iei(?SCCP_IEI_PROTOCOL_CLASS) -> protocol_class;
%% parse_iei(?SCCP_IEI_SEGMENTING_REASSEMBLING) -> segmenting_reassembling;
%% parse_iei(?SCCP_IEI_RECEIVE_SEQUENCE_NUMBER) -> receive_sequence_number;
%% parse_iei(?SCCP_IEI_SEQUENCING_SEGMENTING) -> sequencing_segmenting;
%% parse_iei(?SCCP_IEI_RELEASE_CAUSE) -> release_cause;
%% parse_iei(?SCCP_IEI_RETURN_CAUSE) -> return_cause;
%% parse_iei(?SCCP_IEI_RESET_CAUSE) -> reset_cause;
%% parse_iei(?SCCP_IEI_ERROR_CAUSE) -> error_cause;
%% parse_iei(?SCCP_IEI_REFUSAL_CAUSE) -> refusal_cause;
%% parse_iei(?SCCP_IEI_LONG_DATA) -> long_data;
%% Optional:
parse_iei(?SCCP_IEI_CALLED_PARTY_ADDRESS) -> called_party_address;
parse_iei(?SCCP_IEI_CALLING_PARTY_ADDRESS) -> calling_party_address;
parse_iei(?SCCP_IEI_CREDIT) -> credit;
parse_iei(?SCCP_IEI_DATA) -> data;
parse_iei(?SCCP_IEI_SEGMENTATION) -> segmentation;
parse_iei(?SCCP_IEI_HOP_COUNTER) -> hop_counter;
parse_iei(?SCCP_IEI_IMPORTANCE) -> importance.

%% compose_iei(destination_local_reference) -> ?SCCP_IEI_DESTINATION_LOCAL_REFERENCE;
%% compose_iei(source_local_reference) -> ?SCCP_IEI_SOURCE_LOCAL_REFERENCE;
%% compose_iei(protocol_class) -> ?SCCP_IEI_PROTOCOL_CLASS;
%% compose_iei(segmenting_reassembling) -> ?SCCP_IEI_SEGMENTING_REASSEMBLING;
%% compose_iei(receive_sequence_number) -> ?SCCP_IEI_RECEIVE_SEQUENCE_NUMBER;
%% compose_iei(sequencing_segmenting) -> ?SCCP_IEI_SEQUENCING_SEGMENTING;
%% compose_iei(release_cause) -> ?SCCP_IEI_RELEASE_CAUSE;
%% compose_iei(return_cause) -> ?SCCP_IEI_RETURN_CAUSE;
%% compose_iei(reset_cause) -> ?SCCP_IEI_RESET_CAUSE;
%% compose_iei(error_cause) -> ?SCCP_IEI_ERROR_CAUSE;
%% compose_iei(refusal_cause) -> ?SCCP_IEI_REFUSAL_CAUSE;
%% compose_iei(long_data) -> ?SCCP_IEI_LONG_DATA;
compose_iei(called_party_address) -> ?SCCP_IEI_CALLED_PARTY_ADDRESS;
compose_iei(calling_party_address) -> ?SCCP_IEI_CALLING_PARTY_ADDRESS;
compose_iei(credit) -> ?SCCP_IEI_CREDIT;
compose_iei(data) -> ?SCCP_IEI_DATA;
compose_iei(segmentation) -> ?SCCP_IEI_SEGMENTATION;
compose_iei(hop_counter) -> ?SCCP_IEI_HOP_COUNTER;
compose_iei(importance) -> ?SCCP_IEI_IMPORTANCE.

decode_parameter(destination_local_reference, Bin) ->
    Bin;
decode_parameter(source_local_reference, Bin) ->
    Bin;
decode_parameter(called_party_address, Bin) ->
    decode_address(Bin);
decode_parameter(calling_party_address, Bin) ->
    decode_address(Bin);
decode_parameter(protocol_class, Bin) ->
    case Bin of
        <<O:4, 2#0000:4>> ->
            Opts = parse_protocol_class_options(O),
            Opts#{class => 0};          % Connection-less
        <<O:4, 2#0001:4>> ->
            Opts = parse_protocol_class_options(O),
            Opts#{class => 1};          % Connection-less
        <<S:4, 2#0010:4>> ->
            #{class => 2, spare => S};  % Connection-oriented
        <<S:4, 2#0011:4>> ->
            #{class => 3, spare => S}   % Connection-oriented
    end;
decode_parameter(segmenting_reassembling, Bin) ->
    <<_Spare:7, M:1>> = Bin,
    case M of
        0 -> no_more_data;
        1 -> more_data
    end;
decode_parameter(receive_sequence_number, Bin) ->
    <<PR:7, _Spare:1>> = Bin,
    PR;
decode_parameter(sequencing_segmenting, Bin) ->
    <<PS:7, _Spare:1, PR:7, M:1>> = Bin,
    #{send_sequence_number => PS,
      receive_sequence_number => PR,
      more_data => 1 =:= M
     };
decode_parameter(credit, Bin) ->
    Bin;
decode_parameter(release_cause, Bin) ->
    <<RC:8>> = Bin,
    parse_release_cause(RC);
decode_parameter(return_cause, Bin) ->
    <<RC:8>> = Bin,
    parse_return_cause(RC);
decode_parameter(reset_cause, Bin) ->
    <<RC:8>> = Bin,
    parse_reset_cause(RC);
decode_parameter(error_cause, Bin) ->
    <<EC:8>> = Bin,
    parse_error_cause(EC);
decode_parameter(refusal_cause, Bin) ->
    <<RC:8>> = Bin,
    parse_refusal_cause(RC);
decode_parameter(data, Bin) ->
    Bin;
decode_parameter(segmentation, Bin) ->
    <<F:1, C:1, _Spare:2, Rem:4, LocalRef:3/binary>> = Bin,
    #{first_segment_indication => 0 == F,
      class => C,
      remaining_segments => Rem,
      local_reference => LocalRef};
decode_parameter(hop_counter, Bin) ->
    <<HC:8>> = Bin,
    HC;
decode_parameter(importance, Bin) ->
    <<_Spare:5, Importance:3>> = Bin,
    Importance;
decode_parameter(long_data, Bin) ->
    Bin;
decode_parameter(_, _) ->
    undefined.

parse_protocol_class_options(2#0000) ->
    #{options => no_options};
parse_protocol_class_options(2#1000) ->
    #{options => return_on_error};
parse_protocol_class_options(_O) ->
    #{}.

compose_protocol_class_options(#{options := no_options}) ->
    2#0000;
compose_protocol_class_options(#{options := return_on_error}) ->
    2#1000;
compose_protocol_class_options(_) ->
    0.

parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_END_USER_ORIGINATED) -> end_user_originated;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_END_USER_CONGESTION) -> end_user_congestion;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_END_USER_FAILURE) -> end_user_failure;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_SCCP_USER_ORIGINATED) -> sccp_user_originated;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_REMOTE_PROCEDURE_ERROR) -> remote_procedure_error;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_INCONSISTENT_CONNECTION_DATA) -> inconsistent_connection_data;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_ACCESS_FAILURE) -> access_failure;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_ACCESS_CONGESTION) -> access_congestion;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_SUBSYSTEM_FAILURE) -> subsystem_failure;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_SUBSYSTEM_CONGESTION) -> subsystem_congestion;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_MTP_FAILURE) -> mtp_failure;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_NETWORK_CONGESTION) -> network_congestion;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_EXPIRATION_OF_RESET_TIMER) -> expiration_of_reset_timer;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_EXPIRATION_OF_RECEIVE_INACTIVITY_TIMER) -> expiration_of_receive_inactivity_timer;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_RESERVED) -> reserved;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_UNQUALIFIED) -> unqualified;
parse_release_cause(?SCCP_IEI_RELEASE_CAUSE_SCCP_FAILURE) -> sccp_failure;
parse_release_cause(V) when V >= 2#0001_0001; V =< 2#1111_0011 -> {reserved_international, V};
parse_release_cause(V) when V >= 2#1111_0100; V =< 2#1111_1110 -> {reserved_national, V};
parse_release_cause(V) -> {reserved, V}.

compose_release_cause(end_user_originated) -> ?SCCP_IEI_RELEASE_CAUSE_END_USER_ORIGINATED;
compose_release_cause(end_user_congestion) -> ?SCCP_IEI_RELEASE_CAUSE_END_USER_CONGESTION;
compose_release_cause(end_user_failure) -> ?SCCP_IEI_RELEASE_CAUSE_END_USER_FAILURE;
compose_release_cause(sccp_user_originated) -> ?SCCP_IEI_RELEASE_CAUSE_SCCP_USER_ORIGINATED;
compose_release_cause(remote_procedure_error) -> ?SCCP_IEI_RELEASE_CAUSE_REMOTE_PROCEDURE_ERROR;
compose_release_cause(inconsistent_connection_data) -> ?SCCP_IEI_RELEASE_CAUSE_INCONSISTENT_CONNECTION_DATA;
compose_release_cause(access_failure) -> ?SCCP_IEI_RELEASE_CAUSE_ACCESS_FAILURE;
compose_release_cause(access_congestion) -> ?SCCP_IEI_RELEASE_CAUSE_ACCESS_CONGESTION;
compose_release_cause(subsystem_failure) -> ?SCCP_IEI_RELEASE_CAUSE_SUBSYSTEM_FAILURE;
compose_release_cause(subsystem_congestion) -> ?SCCP_IEI_RELEASE_CAUSE_SUBSYSTEM_CONGESTION;
compose_release_cause(mtp_failure) -> ?SCCP_IEI_RELEASE_CAUSE_MTP_FAILURE;
compose_release_cause(network_congestion) -> ?SCCP_IEI_RELEASE_CAUSE_NETWORK_CONGESTION;
compose_release_cause(expiration_of_reset_timer) -> ?SCCP_IEI_RELEASE_CAUSE_EXPIRATION_OF_RESET_TIMER;
compose_release_cause(expiration_of_receive_inactivity_timer) -> ?SCCP_IEI_RELEASE_CAUSE_EXPIRATION_OF_RECEIVE_INACTIVITY_TIMER;
compose_release_cause(reserved) -> ?SCCP_IEI_RELEASE_CAUSE_RESERVED;
compose_release_cause(unqualified) -> ?SCCP_IEI_RELEASE_CAUSE_UNQUALIFIED;
compose_release_cause(sccp_failure) -> ?SCCP_IEI_RELEASE_CAUSE_SCCP_FAILURE;
compose_release_cause({reserved_international, V}) -> V;
compose_release_cause({reserved_national, V}) -> V;
compose_release_cause({reserved, V}) -> V.

parse_return_cause(?SCCP_IEI_RETURN_CAUSE_NO_TRANSLATION_FOR_AN_ADDRESS_OF_SUCH_NATURE) -> no_translation_for_an_address_of_such_nature;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_NO_TRANSLATION_FOR_THIS_SPECIFIC_ADDRESS) -> no_translation_for_this_specific_address;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_SUBSYSTEM_CONGESTION) -> subsystem_congestion;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_SUBSYSTEM_FAILURE) -> subsystem_failure;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_UNEQUIPPED_USER) -> unequipped_user;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_MTP_FAILURE) -> mtp_failure;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_NETWORK_CONGESTION) -> network_congestion;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_UNQUALIFIED) -> unqualified;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_ERROR_IN_MESSAGE_TRANSPORT) -> error_in_message_transport;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_ERROR_IN_LOCAL_PROCESSING) -> error_in_local_processing;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_DESTINATION_CANNOT_PERFORM_REASSEMBLY) -> destination_cannot_perform_reassembly;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_SCCP_FAILURE) -> sccp_failure;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_HOP_COUNTER_VIOLATION) -> hop_counter_violation;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_SEGMENTATION_NOT_SUPPORTED) -> segmentation_not_supported;
parse_return_cause(?SCCP_IEI_RETURN_CAUSE_SEGMENTATION_FAILURE) -> segmentation_failure;
parse_return_cause(V) when V >= 2#0000_1111; V =< 2#1110_0100 -> {reserved_international, V};
parse_return_cause(V) when V >= 2#1110_0101; V =< 2#1111_1110 -> {reserved_national, V};
parse_return_cause(V) -> {reserved, V}.

compose_return_cause(no_translation_for_an_address_of_such_nature) -> ?SCCP_IEI_RETURN_CAUSE_NO_TRANSLATION_FOR_AN_ADDRESS_OF_SUCH_NATURE;
compose_return_cause(no_translation_for_this_specific_address) -> ?SCCP_IEI_RETURN_CAUSE_NO_TRANSLATION_FOR_THIS_SPECIFIC_ADDRESS;
compose_return_cause(subsystem_congestion) -> ?SCCP_IEI_RETURN_CAUSE_SUBSYSTEM_CONGESTION;
compose_return_cause(subsystem_failure) -> ?SCCP_IEI_RETURN_CAUSE_SUBSYSTEM_FAILURE;
compose_return_cause(unequipped_user) -> ?SCCP_IEI_RETURN_CAUSE_UNEQUIPPED_USER;
compose_return_cause(mtp_failure) -> ?SCCP_IEI_RETURN_CAUSE_MTP_FAILURE;
compose_return_cause(network_congestion) -> ?SCCP_IEI_RETURN_CAUSE_NETWORK_CONGESTION;
compose_return_cause(unqualified) -> ?SCCP_IEI_RETURN_CAUSE_UNQUALIFIED;
compose_return_cause(error_in_message_transport) -> ?SCCP_IEI_RETURN_CAUSE_ERROR_IN_MESSAGE_TRANSPORT;
compose_return_cause(error_in_local_processing) -> ?SCCP_IEI_RETURN_CAUSE_ERROR_IN_LOCAL_PROCESSING;
compose_return_cause(destination_cannot_perform_reassembly) -> ?SCCP_IEI_RETURN_CAUSE_DESTINATION_CANNOT_PERFORM_REASSEMBLY;
compose_return_cause(sccp_failure) -> ?SCCP_IEI_RETURN_CAUSE_SCCP_FAILURE;
compose_return_cause(hop_counter_violation) -> ?SCCP_IEI_RETURN_CAUSE_HOP_COUNTER_VIOLATION;
compose_return_cause(segmentation_not_supported) -> ?SCCP_IEI_RETURN_CAUSE_SEGMENTATION_NOT_SUPPORTED;
compose_return_cause(segmentation_failure) -> ?SCCP_IEI_RETURN_CAUSE_SEGMENTATION_FAILURE;
compose_return_cause({reserved_international, V}) -> V;
compose_return_cause({reserved_national, V}) -> V;
compose_return_cause({reserved, V}) -> V.

parse_reset_cause(?SCCP_IEI_RESET_CAUSE_END_USER_ORIGINATED) -> end_user_originated;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_SCCP_USER_ORIGINATED) -> sccp_user_originated;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_MESSAGE_OUT_OF_ORDER_INCORRECT_PS) -> message_out_of_order_incorrect_ps;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_MESSAGE_OUT_OF_ORDER_INCORRECT_PR) -> message_out_of_order_incorrect_pr;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_MESSAGE_OUT_OF_WINDOW) -> remote_procedure_error_message_out_of_window;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_INCORRECT_PS_AFTER_REINITIALIZATION) -> remote_procedure_error_incorrect_ps_after_reinitialization;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_GENERAL) -> remote_procedure_error_general;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_REMOTE_END_USER_OPERATIONAL) -> remote_end_user_operational;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_NETWORK_OPERATIONAL) -> network_operational;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_ACCESS_OPERATIONAL) -> access_operational;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_NETWORK_CONGESTION) -> network_congestion;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_RESERVED) -> reserved;
parse_reset_cause(?SCCP_IEI_RESET_CAUSE_UNQUALIFIED) -> unqualified;
parse_reset_cause(V) when V >= 2#0000_1101; V =< 2#1111_0011 -> {reserved_international, V};
parse_reset_cause(V) when V >= 2#1111_0100; V =< 2#1111_1110 -> {reserved_national, V};
parse_reset_cause(V) -> {reserved, V}.

compose_reset_cause(end_user_originated) -> ?SCCP_IEI_RESET_CAUSE_END_USER_ORIGINATED;
compose_reset_cause(sccp_user_originated) -> ?SCCP_IEI_RESET_CAUSE_SCCP_USER_ORIGINATED;
compose_reset_cause(message_out_of_order_incorrect_ps) -> ?SCCP_IEI_RESET_CAUSE_MESSAGE_OUT_OF_ORDER_INCORRECT_PS;
compose_reset_cause(message_out_of_order_incorrect_pr) -> ?SCCP_IEI_RESET_CAUSE_MESSAGE_OUT_OF_ORDER_INCORRECT_PR;
compose_reset_cause(remote_procedure_error_message_out_of_window) -> ?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_MESSAGE_OUT_OF_WINDOW;
compose_reset_cause(remote_procedure_error_incorrect_ps_after_reinitialization) -> ?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_INCORRECT_PS_AFTER_REINITIALIZATION;
compose_reset_cause(remote_procedure_error_general) -> ?SCCP_IEI_RESET_CAUSE_REMOTE_PROCEDURE_ERROR_GENERAL;
compose_reset_cause(remote_end_user_operational) -> ?SCCP_IEI_RESET_CAUSE_REMOTE_END_USER_OPERATIONAL;
compose_reset_cause(network_operational) -> ?SCCP_IEI_RESET_CAUSE_NETWORK_OPERATIONAL;
compose_reset_cause(access_operational) -> ?SCCP_IEI_RESET_CAUSE_ACCESS_OPERATIONAL;
compose_reset_cause(network_congestion) -> ?SCCP_IEI_RESET_CAUSE_NETWORK_CONGESTION;
compose_reset_cause(reserved) -> ?SCCP_IEI_RESET_CAUSE_RESERVED;
compose_reset_cause(unqualified) -> ?SCCP_IEI_RESET_CAUSE_UNQUALIFIED;
compose_reset_cause({reserved_international, V}) -> V;
compose_reset_cause({reserved_national, V}) -> V;
compose_reset_cause({reserved, V}) -> V.

parse_error_cause(?SCCP_IEI_ERROR_CAUSE_LOCAL_REFERENCE_NUMBER_MISMATCH_UNASSIGNED_DESTINATION) -> local_reference_number_mismatch_unassigned_destination;
parse_error_cause(?SCCP_IEI_ERROR_CAUSE_LOCAL_REFERENCE_NUMBER_MISMATCH_INCONSISTENT_SOURCE) -> local_reference_number_mismatch_inconsistent_source;
parse_error_cause(?SCCP_IEI_ERROR_CAUSE_POINT_CODE_MISMATCH) -> point_code_mismatch;
parse_error_cause(?SCCP_IEI_ERROR_CAUSE_SERVICE_CLASS_MISMATCH) -> service_class_mismatch;
parse_error_cause(?SCCP_IEI_ERROR_CAUSE_UNQUALIFIED) -> unqualified;
parse_error_cause(V) when V >= 2#0000_0101; V =< 2#1111_0011 -> {reserved_international, V};
parse_error_cause(V) when V >= 2#1111_0100; V =< 2#1111_1110 -> {reserved_national, V};
parse_error_cause(V) -> {reserved, V}.

compose_error_cause(local_reference_number_mismatch_unassigned_destination) -> ?SCCP_IEI_ERROR_CAUSE_LOCAL_REFERENCE_NUMBER_MISMATCH_UNASSIGNED_DESTINATION;
compose_error_cause(local_reference_number_mismatch_inconsistent_source) -> ?SCCP_IEI_ERROR_CAUSE_LOCAL_REFERENCE_NUMBER_MISMATCH_INCONSISTENT_SOURCE;
compose_error_cause(point_code_mismatch) -> ?SCCP_IEI_ERROR_CAUSE_POINT_CODE_MISMATCH;
compose_error_cause(service_class_mismatch) -> ?SCCP_IEI_ERROR_CAUSE_SERVICE_CLASS_MISMATCH;
compose_error_cause(unqualified) -> ?SCCP_IEI_ERROR_CAUSE_UNQUALIFIED;
compose_error_cause({reserved_international, V}) -> V;
compose_error_cause({reserved_national, V}) -> V;
compose_error_cause({reserved, V}) -> V.

parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_END_USER_ORIGINATED) -> end_user_originated;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_END_USER_CONGESTION) -> end_user_congestion;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_END_USER_FAILURE) -> end_user_failure;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_SCCP_USER_ORIGINATED) -> sccp_user_originated;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_DESTINATION_ADDRESS_UNKNOWN) -> destination_address_unknown;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_DESTINATION_INACCESSIBLE) -> destination_inaccessible;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_NETWORK_RESOURCE_QOS_NOT_AVAILABLE_NON_TRANSIENT) -> network_resource_qos_not_available_non_transient;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_NETWORK_RESOURCE_QOS_NOT_AVAILABLE_TRANSIENT) -> network_resource_qos_not_available_transient;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_ACCESS_FAILURE) -> access_failure;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_ACCESS_CONGESTION) -> access_congestion;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_SUBSYSTEM_FAILURE) -> subsystem_failure;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_SUBSYSTEM_CONGESTION) -> subsystem_congestion;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_EXPIRATION_OF_THE_CONNECTION_ESTABLISHMENT_TIMER) -> expiration_of_the_connection_establishment_timer;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_INCOMPATIBLE_USER_DATA) -> incompatible_user_data;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_RESERVED) -> reserved;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_UNQUALIFIED) -> unqualified;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_HOP_COUNTER_VIOLATION) -> hop_counter_violation;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_SCCP_FAILURE) -> sccp_failure;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_NO_TRANSLATION_FOR_AN_ADDRESS_OF_SUCH_NATURE) -> no_translation_for_an_address_of_such_nature;
parse_refusal_cause(?SCCP_IEI_REFUSAL_CAUSE_UNEQUIPPED_USER) -> unequipped_user;
parse_refusal_cause(V) when V >= 2#0001_0100; V =< 2#1111_0011 -> {reserved_international, V};
parse_refusal_cause(V) when V >= 2#1111_0100; V =< 2#1111_1110 -> {reserved_national, V};
parse_refusal_cause(V) -> {reserved, V}.

compose_refusal_cause(end_user_originated) -> ?SCCP_IEI_REFUSAL_CAUSE_END_USER_ORIGINATED;
compose_refusal_cause(end_user_congestion) -> ?SCCP_IEI_REFUSAL_CAUSE_END_USER_CONGESTION;
compose_refusal_cause(end_user_failure) -> ?SCCP_IEI_REFUSAL_CAUSE_END_USER_FAILURE;
compose_refusal_cause(sccp_user_originated) -> ?SCCP_IEI_REFUSAL_CAUSE_SCCP_USER_ORIGINATED;
compose_refusal_cause(destination_address_unknown) -> ?SCCP_IEI_REFUSAL_CAUSE_DESTINATION_ADDRESS_UNKNOWN;
compose_refusal_cause(destination_inaccessible) -> ?SCCP_IEI_REFUSAL_CAUSE_DESTINATION_INACCESSIBLE;
compose_refusal_cause(network_resource_qos_not_available_non_transient) -> ?SCCP_IEI_REFUSAL_CAUSE_NETWORK_RESOURCE_QOS_NOT_AVAILABLE_NON_TRANSIENT;
compose_refusal_cause(network_resource_qos_not_available_transient) -> ?SCCP_IEI_REFUSAL_CAUSE_NETWORK_RESOURCE_QOS_NOT_AVAILABLE_TRANSIENT;
compose_refusal_cause(access_failure) -> ?SCCP_IEI_REFUSAL_CAUSE_ACCESS_FAILURE;
compose_refusal_cause(access_congestion) -> ?SCCP_IEI_REFUSAL_CAUSE_ACCESS_CONGESTION;
compose_refusal_cause(subsystem_failure) -> ?SCCP_IEI_REFUSAL_CAUSE_SUBSYSTEM_FAILURE;
compose_refusal_cause(subsystem_congestion) -> ?SCCP_IEI_REFUSAL_CAUSE_SUBSYSTEM_CONGESTION;
compose_refusal_cause(expiration_of_the_connection_establishment_timer) -> ?SCCP_IEI_REFUSAL_CAUSE_EXPIRATION_OF_THE_CONNECTION_ESTABLISHMENT_TIMER;
compose_refusal_cause(incompatible_user_data) -> ?SCCP_IEI_REFUSAL_CAUSE_INCOMPATIBLE_USER_DATA;
compose_refusal_cause(reserved) -> ?SCCP_IEI_REFUSAL_CAUSE_RESERVED;
compose_refusal_cause(unqualified) -> ?SCCP_IEI_REFUSAL_CAUSE_UNQUALIFIED;
compose_refusal_cause(hop_counter_violation) -> ?SCCP_IEI_REFUSAL_CAUSE_HOP_COUNTER_VIOLATION;
compose_refusal_cause(sccp_failure) -> ?SCCP_IEI_REFUSAL_CAUSE_SCCP_FAILURE;
compose_refusal_cause(no_translation_for_an_address_of_such_nature) -> ?SCCP_IEI_REFUSAL_CAUSE_NO_TRANSLATION_FOR_AN_ADDRESS_OF_SUCH_NATURE;
compose_refusal_cause(unequipped_user) -> ?SCCP_IEI_REFUSAL_CAUSE_UNEQUIPPED_USER;
compose_refusal_cause({reserved_international, V}) -> V;
compose_refusal_cause({reserved_national, V}) -> V;
compose_refusal_cause({reserved, V}) -> V.

encode_parameter(destination_local_reference, Bin) ->
    Bin;
encode_parameter(source_local_reference, Bin) ->
    Bin;
encode_parameter(called_party_address, Bin) ->
    encode_address(Bin);
encode_parameter(calling_party_address, Bin) ->
    encode_address(Bin);
encode_parameter(protocol_class, Val) ->
    case Val of
        #{class := 0} ->
            O = compose_protocol_class_options(Val),
            <<O:4, 2#0000:4>>;
        #{class := 1} ->
            O = compose_protocol_class_options(Val),
            <<O:4, 2#0001:4>>;
        #{class := 2} ->
            <<0:4, 2#0010:4>>;
        #{class := 3} ->
            <<0:4, 2#0011:4>>
    end;
encode_parameter(segmenting_reassembling, Bin) ->
    Bin;
encode_parameter(receive_sequence_number, Bin) ->
    Bin;
encode_parameter(sequencing_segmenting, Bin) ->
    Bin;
encode_parameter(credit, Bin) ->
    Bin;
encode_parameter(release_cause, RC) ->
    V = compose_release_cause(RC),
    <<V:8>>;
encode_parameter(return_cause, RC) ->
    V = compose_return_cause(RC),
    <<V:8>>;
encode_parameter(reset_cause, RC) ->
    V = compose_reset_cause(RC),
    <<V:8>>;
encode_parameter(error_cause, EC) ->
    V = compose_error_cause(EC),
    <<V:8>>;
encode_parameter(refusal_cause, RC) ->
    V = compose_refusal_cause(RC),
    <<V:8>>;
encode_parameter(data, Bin) ->
    Bin;
encode_parameter(segmentation, V) ->
    F = case maps:get(first_segment_indication, V, true) of 
            true ->
                0;
            _ ->
                1
        end,
    C = maps:get(class, V, 0),
    Rem = maps:get(remaining_segments, V, 0),
    LR = rand:uniform(2#1111)-1,
    LocalRef = maps:get(local_reference, V, LR),
    <<F:1, C:1, 0:2, Rem:4, LocalRef:3/binary>>;
encode_parameter(hop_counter, HC) ->
    <<HC:8>>;
encode_parameter(importance, Importance) ->
    <<0:5, Importance:3>>;
encode_parameter(long_data, Bin) ->
    Bin.

decode_address(<<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, Bin0/binary>>) ->
    {PC, Bin1} = case PCI of
                     0 -> {undefined, Bin0};
                     1 -> <<LSB:8, 0:2, MSB:6, Rest0/binary>> = Bin0,
                          {<<MSB:6, LSB:8>>, Rest0}
                 end,
    {SSN, Bin2} = case SSNI of
                      0 -> {undefined, Bin1};
                      1 -> <<SSN0:8/big, Rest1/binary>> = Bin1,
                           {parse_ssn(SSN0), Rest1}
                  end,
    GT = case GTI of
             0 ->
                 undefined;
             2#0001 ->
                 %% global title includes nature of address
                 %% indicator only
                 <<OE:1, NI:7, GT0/binary>> = Bin2,
                 OEI = case OE of
                           0 -> even;
                           1 -> odd
                       end,
                 GT1 = decode_bcd(OEI, GT0),
                 #{odd_even_indicator => OEI,
                   nature_of_address_indicator => NI,
                   address => GT1};
             2#0010 ->
                 %% global title includes translation type
                 %% only
                 <<TT:8/big, GT0/binary>> = Bin2,
                 #{translation_type => TT,
                   address => GT0};
             2#0011 ->
                 %% global title includes translation type,
                 %% numbering plan and encoding scheme
                 <<TT:8/big, NP:4, ES:4, GT0/binary>> = Bin2,
                 GT1 = decode_gt_part(ES, GT0),
                 #{translation_type => TT,
                   numbering_plan => NP,
                   address => GT1};
             2#0100 ->
                 %% global title includes translation type,
                 %% numbering plan, encoding scheme and nature
                 %% of address indicator
                 <<TT:8/big, NP:4, ES:4, 0:1, NI:7, GT0/binary>> = Bin2,
                 GT1 = decode_gt_part(ES, GT0),
                 GT1#{translation_type => TT,
                      numbering_plan => NP,
                      nature_of_address_indicator => NI}
         end,
    RoutingInd = case RI of
                     1 -> subsystem_number;
                     0 -> global_title
                 end,
    #{national_use_indicator => NR,
      routing_indicator => RoutingInd,
      global_title_indicator => GTI,
      global_title => GT,
      subsystem_number => SSN,
      point_code => PC
     }.

decode_gt_part(2#0000, GT) ->
    #{encoding_scheme => unknown,
      address => GT};
decode_gt_part(2#0001, GT) ->
    #{encoding_scheme => bcd,
      odd_even_indicator => odd,
      address => decode_bcd(odd, GT)};
decode_gt_part(2#0010, GT) ->
    #{encoding_scheme => bcd,
      odd_even_indicator => even,
      address => decode_bcd(even, GT)};
decode_gt_part(2#0100, GT) ->
    #{encoding_scheme => national,
      address => GT}.

encode_gt_part(#{encoding_scheme := unknown,
                 address := GT}) ->
    GT;
encode_gt_part(#{encoding_scheme := bcd,
                 address := GT}) ->
    encode_bcd(GT);
encode_gt_part(#{encoding_scheme := national,
                 address := GT}) ->
    GT.

encode_address(#{national_use_indicator := NR,
                 routing_indicator := RoutingInd,
                 global_title_indicator := GTI
                } = Address) ->
    {PCI, PCBin} = case maps:get(point_code, Address, undefined) of
                       undefined -> {0, <<>>};
                       PC -> {1, PC}
                   end,
    {SSNI, SSNBin} = case maps:get(subsystem_number, Address, undefined) of
                         undefined -> {0, <<>>};
                         SSN -> {1, <<(compose_ssn(SSN)):8/big>>}
                     end,
    RI = case RoutingInd of
             subsystem_number -> 1;
             global_title -> 0
         end,
    GlobalTitle = maps:get(global_title, Address, #{}),
    GT = case GTI of
             2#0100 ->
                 #{translation_type := TT,
                   numbering_plan := NP,
                   encoding_scheme := EncodingScheme,
                   nature_of_address_indicator := NI
                  } = GlobalTitle,
                 GT1 = encode_gt_part(GlobalTitle),
                 ES = compose_encoding_scheme(EncodingScheme, GlobalTitle),
                 <<TT:8/big, NP:4, ES:4, 0:1, NI:7, GT1/binary>>;
             2#0011 ->
                 #{translation_type := TT,
                   numbering_plan := NP,
                   encoding_scheme := EncodingScheme
                  } = GlobalTitle,
                 GT1 = encode_gt_part(GlobalTitle),
                 ES = compose_encoding_scheme(EncodingScheme, GlobalTitle),
                 <<TT:8/big, NP:4, ES:4, GT1/binary>>;
             2#0010 ->
                 #{translation_type := TT
                  } = GlobalTitle,
                 GT1 = encode_gt_part(GlobalTitle),
                 <<TT:8/big, GT1/binary>>;
             2#0001 ->
                 #{odd_even_indicator := OE,
                   nature_of_address_indicator := NI
                  } = GlobalTitle,
                 GT1 = encode_gt_part(GlobalTitle),
                 <<OE:1, NI:7, GT1/binary>>;
             2#0000 ->
                 <<>>
         end,
    <<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, SSNBin/binary, PCBin/binary, GT/binary>>.

compose_encoding_scheme(unknown, _) ->
    2#0000;
compose_encoding_scheme(bcd, #{odd_even_indicator := odd}) ->
    2#0001;
compose_encoding_scheme(bcd, #{odd_even_indicator := even}) ->
    2#0010;
compose_encoding_scheme(national, _) ->
    2#0100.

parse_ssn(?SCCP_SSN_UNKNOWN) -> unknown;
parse_ssn(?SCCP_SSN_MGMT) -> management;
parse_ssn(?SCCP_SSN_ITU_RESERVED) -> itu_reserved;
parse_ssn(?SCCP_SSN_ISUP) -> isup;
parse_ssn(?SCCP_SSN_OMAP) -> omap;
parse_ssn(?SCCP_SSN_MAP) -> map;
parse_ssn(?SCCP_SSN_HLR) -> hlr;
parse_ssn(?SCCP_SSN_VLR) -> vlr;
parse_ssn(?SCCP_SSN_MSC) -> msc;
parse_ssn(?SCCP_SSN_EIC) -> eic;
parse_ssn(?SCCP_SSN_AUC) -> auc;
parse_ssn(?SCCP_SSN_ISSS) -> isss;
parse_ssn(?SCCP_SSN_BROADBAND) -> broadband_isdn_edge_to_edge_applications;
parse_ssn(?SCCP_SSN_TC_TEST_RESPONDER) -> tc_test_responder;

parse_ssn(?SCCP_SSN_NAT_CSS) -> css;
parse_ssn(?SCCP_SSN_NAT_PCAP) -> pcap;
parse_ssn(?SCCP_SSN_NAT_BSS) -> bss;
parse_ssn(?SCCP_SSN_NAT_MSC) -> msc;
parse_ssn(?SCCP_SSN_NAT_SMLC) -> smlc;
parse_ssn(?SCCP_SSN_NAT_BSS_OM) -> bss_om;
parse_ssn(?SCCP_SSN_NAT_BSSAP) -> bssap;
parse_ssn(?SCCP_SSN_NAT_RANAP) -> ranap;
parse_ssn(?SCCP_SSN_NAT_RNSAP) -> rnsap;
parse_ssn(?SCCP_SSN_NAT_GMLC) -> gmlc;
parse_ssn(?SCCP_SSN_NAT_CAP) -> cap;
parse_ssn(?SCCP_SSN_NAT_SCF) -> scf;
parse_ssn(?SCCP_SSN_NAT_SIWF) -> siwf;
parse_ssn(?SCCP_SSN_NAT_SGSN) -> sgsn;
parse_ssn(?SCCP_SSN_NAT_GGSN) -> ggsn;

parse_ssn(SSN) when SSN == 2#00001100 -> {international, SSN};
parse_ssn(SSN) when SSN >= 2#00001111, SSN =< 2#00011111 -> {international, SSN};
parse_ssn(SSN) when SSN >= 2#00100000, SSN =< 2#11111110 -> {national, SSN};
parse_ssn(SSN) when SSN == 2#11111111 -> expansion.

compose_ssn(unknown) -> ?SCCP_SSN_UNKNOWN;
compose_ssn(management) -> ?SCCP_SSN_MGMT;
compose_ssn(itu_reserved) -> ?SCCP_SSN_ITU_RESERVED;
compose_ssn(isup) -> ?SCCP_SSN_ISUP;
compose_ssn(omap) -> ?SCCP_SSN_OMAP;
compose_ssn(map) -> ?SCCP_SSN_MAP;
compose_ssn(hlr) -> ?SCCP_SSN_HLR;
compose_ssn(vlr) -> ?SCCP_SSN_VLR;
compose_ssn(msc) -> ?SCCP_SSN_MSC;
compose_ssn(eic) -> ?SCCP_SSN_EIC;
compose_ssn(auc) -> ?SCCP_SSN_AUC;
compose_ssn(isss) -> ?SCCP_SSN_ISSS;
compose_ssn(broadband_isdn_edge_to_edge_applications) -> ?SCCP_SSN_BROADBAND;
compose_ssn(tc_test_responder) -> ?SCCP_SSN_TC_TEST_RESPONDER;
compose_ssn({international, SSN}) -> SSN;
compose_ssn({national, SSN}) -> SSN;
compose_ssn(expansion) -> 2#11111111.

decode_bcd(even, <<>>) ->
    [];
decode_bcd(odd, <<2#0000:4, A:4>>) ->
    [decode_bcd_digit(A)];
decode_bcd(OE, <<B:4, A:4, Rest/binary>>) ->
    [decode_bcd_digit(A), decode_bcd_digit(B)|decode_bcd(OE, Rest)].

encode_bcd([]) ->
    <<>>;
encode_bcd([A]) ->
    <<2#0000:4, (encode_bcd_digit(A)):4>>;
encode_bcd([A, B|Rest]) ->
    <<(encode_bcd_digit(B)):4, (encode_bcd_digit(A)):4, (encode_bcd(Rest))/binary>>.

decode_bcd_digit(2#1010) -> $a;
decode_bcd_digit(2#1011) -> $b;
decode_bcd_digit(2#1100) -> $c;
decode_bcd_digit(2#1101) -> $d;
decode_bcd_digit(2#1110) -> $e;
decode_bcd_digit(2#1111) -> $f;
decode_bcd_digit(B) -> $0+B.

encode_bcd_digit($a) -> 2#1010;
encode_bcd_digit($b) -> 2#1011;
encode_bcd_digit($c) -> 2#1100;
encode_bcd_digit($d) -> 2#1101;
encode_bcd_digit($e) -> 2#1110;
encode_bcd_digit($f) -> 2#1111;
encode_bcd_digit(B) -> B-$0.
