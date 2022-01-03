-module(otc_sccp_codec).

-export([decode/1,
         encode/1
        ]).

-include("include/sccp.hrl").
-include_lib("eunit/include/eunit.hrl").

decode(<<MT:8/big, Rest/binary>>) ->
    MessageType = parse_message_type(MT),
    case decode_msg(MessageType, Rest) of
        unsupported ->
            unsupported;
        Msg ->
            Msg#{message_type => MessageType}
    end.

encode(#{message_type := MessageType} = Msg) ->
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg),
    <<MT:8/big, Bin/binary>>;
encode(_) ->
    unsupported.

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
parse_message_type(?SCCP_MSG_TYPE_LUDTS) -> ludts;
parse_message_type(_) ->
    unsupported.

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
    Optionals = #{},
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               data => decode_parameter(data, D)};
decode_msg(udts, Bin) ->
    NumPointers = 3,
    <<RC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D] = separate_fields(Pointers, Bin1),
    Optionals = #{},
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               data => decode_parameter(data, D)};
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
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               data => decode_parameter(data, D)};
decode_msg(xudts, Bin) ->
    NumPointers = 4,
    <<RC:1/binary, HC:1/binary, Pointers:NumPointers/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D, OptBin] = separate_fields(Pointers, Bin1),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               data => decode_parameter(data, D)};
decode_msg(ludt, Bin) ->
    NumPointers = 4,
    <<PC:1/binary, HC:1/binary, Pointers:(2*NumPointers)/binary, Bin1/binary>> = Bin,
    <<CdPAP:16/big, CgPAP:16/big, LDP:16/big, OptBinP:16/big>> = Pointers,
    <<CdPAL:8/big>> = binary:at(Bin1, CdPAP),
    CdPA = binary:part(Bin1, CdPAP+1, CdPAL-1),
    <<CgPAL:8/big>> = binary:at(Bin1, CgPAP),
    CgPA = binary:part(Bin1, CgPAP+1, CgPAL-1),
    <<LDL:16/big>> = binary:part(Bin1, LDP, 2),
    LD = binary:part(Bin1, LDP+1, LDL-1),
    OptBin = binary:part(Bin1, byte_size(Bin1), -OptBinP),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               long_data => decode_parameter(long_data, LD)};
decode_msg(ludts, Bin) ->
    NumPointers = 4,
    <<RC:1/binary, HC:1/binary, Pointers:(2*NumPointers)/binary, Bin1/binary>> = Bin,
    <<CdPAP:16/big, CgPAP:16/big, LDP:16/big, OptBinP:16/big>> = Pointers,
    <<CdPAL:8/big>> = binary:at(Bin1, CdPAP),
    CdPA = binary:part(Bin1, CdPAP+1, CdPAL-1),
    <<CgPAL:8/big>> = binary:at(Bin1, CgPAP),
    CgPA = binary:part(Bin1, CgPAP+1, CgPAL-1),
    <<LDL:16/big>> = binary:part(Bin1, LDP, 2),
    LD = binary:part(Bin1, LDP+1, LDL-1),
    OptBin = binary:part(Bin1, byte_size(Bin1), -OptBinP),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters),
    Optionals#{return_cause => decode_parameter(return_cause, RC),
               hop_counter => decode_parameter(hop_counter, HC),
               called_party_address => decode_parameter(called_party_address, CdPA),
               calling_party_address => decode_parameter(calling_party_address, CgPA),
               long_data => decode_parameter(long_data, LD)};
decode_msg(_, _) ->
    unsupported.

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
separate_fields(<<P:8/big, R/binary>>, <<L:8/big, Bin/binary>>, {N, PAcc, BAcc}) ->
    <<Val:L/binary, Rest/binary>> = Bin,
    separate_fields(R, Rest, {N+1, [{P, N}|PAcc], [Val|BAcc]}).



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
    D = encode_parameter(data, Data),
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
    D = encode_parameter(data, Data),
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
    D = encode_parameter(data, Data),
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
    D = encode_parameter(data, Data),
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
    LD = encode_parameter(long_data, LongData),
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
    LD = encode_parameter(long_data, LongData),
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

parse_iei(?SCCP_IEI_DESTINATION_LOCAL_REFERENCE) -> destination_local_reference;
parse_iei(?SCCP_IEI_SOURCE_LOCAL_REFERENCE) -> source_local_reference;
parse_iei(?SCCP_IEI_CALLED_PARTY_ADDRESS) -> called_party_address;
parse_iei(?SCCP_IEI_CALLING_PARTY_ADDRESS) -> calling_party_address;
parse_iei(?SCCP_IEI_PROTOCOL_CLASS) -> protocol_class;
parse_iei(?SCCP_IEI_SEGMENTING_REASSEMBLING) -> segmenting_reassembling;
parse_iei(?SCCP_IEI_RECEIVE_SEQUENCE_NUMBER) -> receive_sequence_number;
parse_iei(?SCCP_IEI_SEQUENCING_SEGMENTING) -> sequencing_segmenting;
parse_iei(?SCCP_IEI_CREDIT) -> credit;
parse_iei(?SCCP_IEI_RELEASE_CAUSE) -> release_cause;
parse_iei(?SCCP_IEI_RETURN_CAUSE) -> return_cause;
parse_iei(?SCCP_IEI_RESET_CAUSE) -> reset_cause;
parse_iei(?SCCP_IEI_ERROR_CAUSE) -> error_cause;
parse_iei(?SCCP_IEI_REFUSAL_CAUSE) -> refusal_cause;
parse_iei(?SCCP_IEI_DATA) -> data;
parse_iei(?SCCP_IEI_SEGMENTATION) -> segmentation;
parse_iei(?SCCP_IEI_HOP_COUNTER) -> hop_counter;
parse_iei(?SCCP_IEI_IMPORTANCE) -> importance;
parse_iei(?SCCP_IEI_LONG_DATA) -> long_data.

compose_iei(destination_local_reference) -> ?SCCP_IEI_DESTINATION_LOCAL_REFERENCE;
compose_iei(source_local_reference) -> ?SCCP_IEI_SOURCE_LOCAL_REFERENCE;
compose_iei(called_party_address) -> ?SCCP_IEI_CALLED_PARTY_ADDRESS;
compose_iei(calling_party_address) -> ?SCCP_IEI_CALLING_PARTY_ADDRESS;
compose_iei(protocol_class) -> ?SCCP_IEI_PROTOCOL_CLASS;
compose_iei(segmenting_reassembling) -> ?SCCP_IEI_SEGMENTING_REASSEMBLING;
compose_iei(receive_sequence_number) -> ?SCCP_IEI_RECEIVE_SEQUENCE_NUMBER;
compose_iei(sequencing_segmenting) -> ?SCCP_IEI_SEQUENCING_SEGMENTING;
compose_iei(credit) -> ?SCCP_IEI_CREDIT;
compose_iei(release_cause) -> ?SCCP_IEI_RELEASE_CAUSE;
compose_iei(return_cause) -> ?SCCP_IEI_RETURN_CAUSE;
compose_iei(reset_cause) -> ?SCCP_IEI_RESET_CAUSE;
compose_iei(error_cause) -> ?SCCP_IEI_ERROR_CAUSE;
compose_iei(refusal_cause) -> ?SCCP_IEI_REFUSAL_CAUSE;
compose_iei(data) -> ?SCCP_IEI_DATA;
compose_iei(segmentation) -> ?SCCP_IEI_SEGMENTATION;
compose_iei(hop_counter) -> ?SCCP_IEI_HOP_COUNTER;
compose_iei(importance) -> ?SCCP_IEI_IMPORTANCE;
compose_iei(long_data) -> ?SCCP_IEI_LONG_DATA.

decode_parameter(destination_local_reference, Bin) ->
    Bin;
decode_parameter(source_local_reference, Bin) ->
    Bin;
decode_parameter(called_party_address, Bin) ->
    decode_gt(Bin);
decode_parameter(calling_party_address, Bin) ->
    decode_gt(Bin);
decode_parameter(protocol_class, Bin) ->
    Bin;
decode_parameter(segmenting_reassembling, Bin) ->
    Bin;
decode_parameter(receive_sequence_number, Bin) ->
    Bin;
decode_parameter(sequencing_segmenting, Bin) ->
    Bin;
decode_parameter(credit, Bin) ->
    Bin;
decode_parameter(release_cause, Bin) ->
    Bin;
decode_parameter(return_cause, Bin) ->
    Bin;
decode_parameter(reset_cause, Bin) ->
    Bin;
decode_parameter(error_cause, Bin) ->
    Bin;
decode_parameter(refusal_cause, Bin) ->
    Bin;
decode_parameter(data, Bin) ->
    Bin;
decode_parameter(segmentation, Bin) ->
    Bin;
decode_parameter(hop_counter, Bin) ->
    Bin;
decode_parameter(importance, Bin) ->
    Bin;
decode_parameter(long_data, Bin) ->
    Bin;
decode_parameter(_, _) ->
    undefined.

encode_parameter(destination_local_reference, Bin) ->
    Bin;
encode_parameter(source_local_reference, Bin) ->
    Bin;
encode_parameter(called_party_address, Bin) ->
    encode_gt(Bin);
encode_parameter(calling_party_address, Bin) ->
    encode_gt(Bin);
encode_parameter(protocol_class, Bin) ->
    Bin;
encode_parameter(segmenting_reassembling, Bin) ->
    Bin;
encode_parameter(receive_sequence_number, Bin) ->
    Bin;
encode_parameter(sequencing_segmenting, Bin) ->
    Bin;
encode_parameter(credit, Bin) ->
    Bin;
encode_parameter(release_cause, Bin) ->
    Bin;
encode_parameter(return_cause, Bin) ->
    Bin;
encode_parameter(reset_cause, Bin) ->
    Bin;
encode_parameter(error_cause, Bin) ->
    Bin;
encode_parameter(refusal_cause, Bin) ->
    Bin;
encode_parameter(data, Bin) ->
    Bin;
encode_parameter(segmentation, Bin) ->
    Bin;
encode_parameter(hop_counter, Bin) ->
    Bin;
encode_parameter(importance, Bin) ->
    Bin;
encode_parameter(long_data, Bin) ->
    Bin.

decode_gt(<<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, Bin0/binary>>) ->
    {PC, Bin1} = case PCI of
                     0 -> {undefined, Bin0};
                     1 -> <<PC0:1/binary, Rest0/binary>> = Bin0,
                          {PC0, Rest0}
                 end,
    {SSN, Bin2} = case SSNI of
                      0 -> {undefined, Bin1};
                      1 -> <<SSN0:1/binary, Rest1/binary>> = Bin1,
                           {SSN0, Rest1}
                  end,
    GT = case GTI of
             0 ->
                 undefined;
             2#0001 ->
                 %% global title includes nature of address
                 %% indicator only
                 <<OE:1, NI:7, GT0/binary>> = Bin2,
                 GT1 = decode_bcd(GT0),
                 #{odd_even_indicator => OE,
                   nature_of_address_indicator => NI,
                   global_title => GT1};
             2#0010 ->
                 %% global title includes translation type
                 %% only
                 <<TT:8/big, GT0/binary>> = Bin2,
                 GT1 = decode_bcd(GT0),
                 #{translation_type => TT,
                   global_title => GT1};
             2#0011 ->
                 %% global title includes translation type,
                 %% numbering plan and encoding scheme
                 <<TT:8/big, NP:4, ES:4, GT0/binary>> = Bin2,
                 GT1 = decode_bcd(GT0),
                 #{translation_type => TT,
                   numbering_plan => NP,
                   encoding_scheme => ES,
                   global_title => GT1};
             2#0100 ->
                 %% global title includes translation type,
                 %% numbering plan, encoding scheme and nature
                 %% of address indicator
                 <<TT:8/big, NP:4, ES:4, OE:1, NI:7, GT0/binary>> = Bin2,
                 GT1 = decode_bcd(GT0),
                 #{translation_type => TT,
                   numbering_plan => NP,
                   encoding_scheme => ES,
                   odd_even_indicator => OE,
                   nature_of_address_indicator => NI,
                   global_title => GT1}
         end,
    RoutingInd = case RI of
                     1 -> ssn;
                     0 -> gt
                 end,
    #{national_use_indicator => NR,
      routing_indicator => RoutingInd,
      global_title_indicator => GTI,
      address => GT,
      subsystem_number => SSN,
      point_code => PC
     }.

encode_gt(#{national_use_indicator := NR,
            routing_indicator := RoutingInd,
            global_title_indicator := GTI,
            address := GT,
            subsystem_number := SSN,
            point_code := PC
           }) ->
    {PCI, PCBin} = case PC of
                       undefined -> {0, <<>>};
                       _ -> {1, PC}
                   end,
    {SSNI, SSNBin} = case SSN of
                         undefined -> {0, <<>>};
                         _ -> {1, SSN}
                     end,
    RI = case RoutingInd of
             ssn -> 1;
             gt -> 0
         end,
    Address = case GTI of
                  2#0100 ->
                      #{translation_type := TT,
                        numbering_plan := NP,
                        encoding_scheme := ES,
                        odd_even_indicator := OE,
                        nature_of_address_indicator := NI,
                        global_title := GT0} = GT,
                      GT1 = encode_bcd(GT0),
                      <<TT:8/big, NP:4, ES:4, OE:1, NI:7, GT1/binary>>;
                  2#0011 ->
                      #{translation_type := TT,
                        numbering_plan := NP,
                        encoding_scheme := ES,
                        global_title := GT0} = GT,
                      GT1 = encode_bcd(GT0),
                      <<TT:8/big, NP:4, ES:4, GT1/binary>>;
                  2#0010 ->
                      #{translation_type := TT,
                        global_title := GT0} = GT,
                      GT1 = encode_bcd(GT0),
                      <<TT:8/big, GT1/binary>>;
                  2#0001 ->
                      #{odd_even_indicator := OE,
                        nature_of_address_indicator := NI,
                        global_title := GT0} = GT,
                      GT1 = encode_bcd(GT0),
                      <<OE:1, NI:7, GT1/binary>>
              end,
    <<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, SSNBin/binary, PCBin/binary, Address/binary>>.

decode_bcd(<<>>) ->
    [];
decode_bcd(<<2#1111:4, A:4>>) ->
    [decode_bcd_digit(A)];
decode_bcd(<<B:4, A:4, Rest/binary>>) ->
    [decode_bcd_digit(A), decode_bcd_digit(B)|decode_bcd(Rest)].

encode_bcd([]) ->
    <<>>;
encode_bcd([A]) ->
    <<2#1111:4, (encode_bcd_digit(A)):4>>;
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
