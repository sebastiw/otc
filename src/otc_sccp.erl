-module(otc_sccp).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         decode/2,
         encode/1,
         encode/2
        ]).

-deprecated([{decode, 1, "Use decode/2 instead."}]).

-export([parse_ssn/1,
         compose_ssn/1
        ]).

-include("include/sccp.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "ITU-T Q.713 (03/2001)".

codec(Bin, Opts) when is_binary(Bin) ->
    decode(Bin, Opts);
codec(Map, Opts) when is_map(Map) ->
    encode({Map, <<>>}, Opts);
codec({Map, PDU}, Opts) ->
    encode({Map, PDU}, Opts).

-define(IS_SCCP_MGMT,
        #{routing_indicator := subsystem_number, subsystem_number := management}).
-define(IS_CONNECTIONLESS(M),
        0 =:= map_get(class, map_get(protocol_class, M));
        1 =:= map_get(class, map_get(protocol_class, M))).

%% ITU-T Q.713 (03/2001) Chapter 5
next(#{calling_party_address := ?IS_SCCP_MGMT,
       called_party_address := ?IS_SCCP_MGMT}) ->
    {ok, sccp_mgmt};
%% TS 29.002 v17.2.0 Chapter 6
next(#{called_party_address := #{subsystem_number := hlr}} = M)
  when ?IS_CONNECTIONLESS(M) ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := vlr}} = M)
  when ?IS_CONNECTIONLESS(M),
       hlr =:= CgSSN; vlr =:= CgSSN; msc =:= CgSSN; css =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := msc}} = M)
  when ?IS_CONNECTIONLESS(M),
       msc =:= CgSSN; sgsn =:= CgSSN; gmlc =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := eir}} = M)
  when ?IS_CONNECTIONLESS(M),
       msc =:= CgSSN; sgsn =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := gsmSCF}} = M)
  when ?IS_CONNECTIONLESS(M),
       hlr =:= CgSSN; vlr =:= CgSSN; msc =:= CgSSN; sgsn =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := sgsn}} = M)
  when ?IS_CONNECTIONLESS(M),
       hlr =:= CgSSN; msc =:= CgSSN; gmlc =:= CgSSN; css =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := hlr},
       called_party_address := #{subsystem_number := ggsn}} = M)
  when ?IS_CONNECTIONLESS(M) ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := css}} = M)
  when ?IS_CONNECTIONLESS(M),
       vlr =:= CgSSN; sgsn =:= CgSSN ->
    {ok, tcap};
next(#{calling_party_address := #{subsystem_number := CgSSN},
       called_party_address := #{subsystem_number := glmc}} = M)
  when ?IS_CONNECTIONLESS(M),
       msc =:= CgSSN; gsmSCF =:= CgSSN; sgsn =:= CgSSN ->
    {ok, tcap};
%% TS 29.078 v17.0.0 Chapter 14.2.2
next(#{calling_party_address := #{subsystem_number := cap},
       called_party_address := #{subsystem_number := cap}} = M)
  when ?IS_CONNECTIONLESS(M) ->
    {ok, tcap};
%% TODO: ITU-T Q.1400 (03/93)
next(_) -> '$stop'.

decode(Bin) ->
    decode(Bin, #{}).

decode(<<MT:8/big, Rest/binary>>, Opts) ->
    MessageType = parse_message_type(MT),
    Msg = decode_msg(MessageType, Rest, Opts),
    case Msg#{message_type => MessageType} of
        #{long_data := LD} = Msg2 ->
            {maps:without([long_data], Msg2), LD};
        #{data := D} = Msg2 ->
            {maps:without([data], Msg2), D};
        Msg2 ->
            Msg2
    end.

encode(Msg) ->
    encode(Msg, #{}).

encode({#{message_type := MessageType} = Msg, PDU}, Opts)
  when ludt =:= MessageType;
       ludts =:= MessageType ->
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg#{long_data => PDU}, Opts),
    <<MT:8/big, Bin/binary>>;
encode({#{message_type := MessageType} = Msg, PDU}, Opts)
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
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg#{data => PDU}, Opts),
    <<MT:8/big, Bin/binary>>;
encode({#{message_type := MessageType} = Msg, _}, Opts) ->
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg, Opts),
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

decode_msg(cr, Bin, Opts) ->
    NumPointers = 2,
    <<SLR:3/binary, PC:1/binary, Bin1/binary>> = Bin,
    [CdPA, OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{credit, 3},
                         {calling_party_address, {4, n}},
                         {data, {3, 130}},
                         {hop_counter, 3},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{source_local_reference => decode_parameter(source_local_reference, SLR, Opts),
               protocol_class => decode_parameter(protocol_class, PC, Opts),
               called_party_address => decode_parameter(called_party_address, CdPA, Opts)};
decode_msg(cc, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, SLR:3/binary, PC:1/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{credit, 3},
                         {called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts),
               protocol_class => decode_parameter(protocol_class, PC, Opts)};
decode_msg(cref, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, RC:1/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               refusal_cause => decode_parameter(refusal_cause, RC, Opts)};
decode_msg(rlsd, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, SLR:3/binary, RC:1/binary, Bin1/binary>> = Bin,
    [OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts),
               release_cause => decode_parameter(release_cause, RC, Opts)};
decode_msg(rlc, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts)};
decode_msg(dt1, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, SR:1/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               segmenting_reassembling => decode_parameter(segmenting_reassembling, SR, Opts),
               data => decode_parameter(data, D, Opts)};
decode_msg(dt2, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, SS:2/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               sequencing_segmenting => decode_parameter(sequencing_segmenting, SS, Opts),
               data => decode_parameter(data, D, Opts)};
decode_msg(ak, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, RSN:1/binary, C:1/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               receive_sequence_number => decode_parameter(receive_sequence_number, RSN, Opts),
               credit => decode_parameter(credit, C, Opts)};
decode_msg(udt, Bin, Opts) ->
    NumPointers = 3,
    <<PC:1/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = #{},
    Optionals#{protocol_class => decode_parameter(protocol_class, PC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => D};
decode_msg(udts, Bin, Opts) ->
    NumPointers = 3,
    <<RC:1/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = #{},
    Optionals#{return_cause => decode_parameter(return_cause, RC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => D};
decode_msg(ed, Bin, Opts) ->
    NumPointers = 1,
    <<DLR:3/binary, Bin1/binary>> = Bin,
    [D] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               data => decode_parameter(data, D, Opts)};
decode_msg(ea, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts)};
decode_msg(rsr, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, RC:1/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts),
               reset_cause => decode_parameter(reset_cause, RC, Opts)};
decode_msg(rsc, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts)};
decode_msg(err, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, EC:1/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               error_cause => decode_parameter(error_cause, EC, Opts)};
decode_msg(it, Bin, Opts) ->
    NumPointers = 0,
    <<DLR:3/binary, SLR:3/binary, PC:1/binary, SS:2/binary, C:1/binary, Bin1/binary>> = Bin,
    [] = separate_fields(absolute_pointers(Bin1, NumPointers)),
    Optionals = #{},
    Optionals#{destination_local_reference => decode_parameter(destination_local_reference, DLR, Opts),
               source_local_reference => decode_parameter(source_local_reference, SLR, Opts),
               protocol_class => decode_parameter(protocol_class, PC, Opts),
               sequencing_segmenting => decode_parameter(sequencing_segmenting, SS, Opts),
               credit => decode_parameter(credit, C, Opts)};
decode_msg(xudt, Bin, Opts) ->
    NumPointers = 4,
    <<PC:1/binary, HC:1/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D, OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(Opts)],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC, Opts),
               hop_counter => decode_parameter(hop_counter, HC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => D};
decode_msg(xudts, Bin, Opts) ->
    NumPointers = 4,
    <<RC:1/binary, HC:1/binary, Bin1/binary>> = Bin,
    [CdPA, CgPA, D, OptBin] = separate_fields(absolute_pointers(Bin1, NumPointers, true)),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(Opts)],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{return_cause => decode_parameter(return_cause, RC, Opts),
               hop_counter => decode_parameter(hop_counter, HC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               data => D};
decode_msg(ludt, Bin, Opts) ->
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
                         {importance, 3}
                        |ansi_parameters(Opts)],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{protocol_class => decode_parameter(protocol_class, PC, Opts),
               hop_counter => decode_parameter(hop_counter, HC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               long_data => LD};
decode_msg(ludts, Bin, Opts) ->
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
                         {importance, 3}
                        |ansi_parameters(Opts)],
    CalledPartyAddress = decode_parameter(called_party_address, CdPA, Opts),
    CallingPartyAddress = decode_parameter(calling_party_address, CgPA, Opts),
    Optionals = decode_parameters(OptBin, AllowedParameters, Opts),
    Optionals#{return_cause => decode_parameter(return_cause, RC, Opts),
               hop_counter => decode_parameter(hop_counter, HC, Opts),
               called_party_address => CalledPartyAddress,
               calling_party_address => CallingPartyAddress,
               long_data => LD}.

absolute_pointers(Bin, NumPointers) ->
    absolute_pointers(Bin, NumPointers, false).

absolute_pointers(Bin, NumPointers, IncOptional) ->
    absolute_pointers(Bin, NumPointers, IncOptional, 1).

absolute_pointers(Bin, NumPointers, IncOptional, PointSize) ->
    absolute_pointers(Bin, NumPointers, IncOptional, PointSize, 1).

absolute_pointers(Bin, NumPointers, IncOptional, PointSize, LengthFieldLength) ->
    %% Extract all pointers
    <<PointerBin:(NumPointers*PointSize)/binary, Rest/binary>> = Bin,
    RelativePointers = [P || <<P:(8*PointSize)/big>> <= PointerBin],
    %% Zip with the order and length of length-field
    RPs = lists:zipwith(fun (N, P) -> {N, P-(NumPointers-N)-1, LengthFieldLength} end,
                        lists:seq(1, NumPointers), RelativePointers),
    %% If optional is included, set length to rest of binary
    case IncOptional of
        true ->
            {value, {NumPointers, Pos, _}, RPs0} = lists:keytake(NumPointers, 1, RPs),
            case Pos of
                -1 ->
                    %% If optional pointer is set to 0, set pointer to end of binary
                    RPs1 = RPs0 ++ [{NumPointers, byte_size(Rest)-1, 0}],
                    {RPs1, Rest};
                _ ->
                    %% optional pointers don't have length
                    RPs1 = RPs0 ++ [{NumPointers, Pos-1, 0}],
                    {RPs1, Rest}
            end;
        false ->
            {RPs, Rest}
    end.

absolute_pointers_test_() ->
    %% Example: <<PointerBin:3/b, Bin/b>>
    %% Expected order: [CdPA, CgPA, LD, OptBin]
    %% PointerBin: <<10 = PointCdPA, 3 = PointCgPA, 8 = PointLD, 0 = PointOpt>>
    %% Bin: <<CgPABin1:5/b, LDBin2:2/b, CdPABin3:5/b, OptBin4>>
    CdPA = <<4, "CdPA">>,
    CgPA = <<4, "CgPA">>,
    D = <<1, "D">>,
    Data = <<CgPA/binary, D/binary, CdPA/binary>>,
    Seg = <<16#10, 16#04, 16#01, 16#02, 16#03, 16#04>>,
    [{"Pointers in sorted order + optional",
      ?_assertEqual({[{1,0,1}, {2,5,1}, {3,7,1}, {4,10,0}], <<Data/binary, Seg/binary>>},
                    absolute_pointers(<<4, 8, 9, 12, Data/binary, Seg/binary>>, 4, true))},
     {"Pointers in sorted order + blank optional",
      ?_assertEqual({[{1,0,1}, {2,5,1}, {3,7,1}, {4,10,0}], Data},
                    absolute_pointers(<<4, 8, 9, 12, Data/binary>>, 4, true))},
     {"Pointers in sorted order + empty optional",
      ?_assertEqual({[{1,0,1}, {2,5,1}, {3,7,1}, {4,11,0}], Data},
                    absolute_pointers(<<4, 8, 9, 0, Data/binary>>, 4, true))},
     {"Pointers in non-sorted order + optional",
      ?_assertEqual({[{1,7,1}, {2,0,1}, {3,5,1}, {4,10,0}], <<Data/binary, Seg/binary>>},
                    absolute_pointers(<<11, 3, 7, 12, Data/binary, Seg/binary>>, 4, true))},
     {"Pointers in non-sorted order + blank optional",
      ?_assertEqual({[{1,7,1}, {2,0,1}, {3,5,1}, {4,10,0}], Data},
                    absolute_pointers(<<11, 3, 7, 12, Data/binary>>, 4, true))},
     {"Pointers in non-sorted order + empty optional",
      ?_assertEqual({[{1,7,1}, {2,0,1}, {3,5,1}, {4,11,0}], Data},
                    absolute_pointers(<<11, 3, 7, 0, Data/binary>>, 4, true))},
     {"Pointers in reverse order + optional",
      ?_assertEqual({[{1,7,1}, {2,5,1}, {3,0,1}, {4,10,0}], <<Data/binary, Seg/binary>>},
                    absolute_pointers(<<11, 8, 2, 12, Data/binary, Seg/binary>>, 4, true))},
     {"Pointers in reverse order + blank optional",
      ?_assertEqual({[{1,7,1}, {2,5,1}, {3,0,1}, {4,10,0}], Data},
                    absolute_pointers(<<11, 8, 2, 12, Data/binary>>, 4, true))},
     {"Pointers in reverse order + empty optional",
      ?_assertEqual({[{1,7,1}, {2,5,1}, {3,0,1}, {4,11,0}], Data},
                    absolute_pointers(<<11, 8, 2, 0, Data/binary>>, 4, true))},
     {"Pointers in sorted order",
      ?_assertEqual({[{1,0,1}, {2,5,1}, {3,7,1}], Data},
                    absolute_pointers(<<3, 7, 8, Data/binary>>, 3))},
     {"Pointers in non-sorted order",
      ?_assertEqual({[{1,7,1}, {2,0,1}, {3,5,1}], Data},
                    absolute_pointers(<<10, 2, 6, Data/binary>>, 3))},
     {"Pointers in reverse order",
      ?_assertEqual({[{1,7,1}, {2,5,1}, {3,0,1}], Data},
                    absolute_pointers(<<10, 7, 1, Data/binary>>, 3))}].

separate_fields({RPs, Bin}) ->
    separate_fields(RPs, Bin).

separate_fields(Pointers, Bin) ->
    separate_fields(Pointers, Bin, []).

separate_fields([], _, Acc) ->
    lists:reverse(Acc);
separate_fields([{_, P, 0}|Ps], Bin, Acc) ->
    %% Just take rest of binary
    B = binary:part(Bin, P+1, byte_size(Bin)-(P+1)),
    separate_fields(Ps, Bin, [B|Acc]);
separate_fields([{_, P, L}|Ps], Bin, Acc) ->
    <<Len:(8*L)/big>> = binary:part(Bin, P, L),
    B = binary:part(Bin, P+1, Len),
    separate_fields(Ps, Bin, [B|Acc]).

separate_fields_test_() ->
    %% Example: <<PointerBin:3/b, Bin/b>>
    %% Expected order: [CdPA, CgPA, LD, OptBin]
    %% PointerBin: <<10 = PointCdPA, 3 = PointCgPA, 8 = PointLD, 0 = PointOpt>>
    %% Bin: <<CgPABin1:5/b, LDBin2:2/b, CdPABin3:5/b, OptBin4:0/b>>
    CdPA = <<4, "CdPA">>,
    CgPA = <<4, "CgPA">>,
    D = <<1, "D">>,
    Bin = <<CgPA/binary, D/binary, CdPA/binary>>,
    Seg = <<16#10, 16#04, 16#01, 16#02, 16#03, 16#04>>,
    [{"Pointers in sorted order + optional",
      ?_assertEqual([<<"CgPA">>,<<"D">>,<<"CdPA">>,Seg],
                    separate_fields(absolute_pointers(<<4, 8, 9, 13, Bin/binary, Seg/binary>>, 4, true)))},
     {"Pointers in sorted order + blank optional",
      ?_assertEqual([<<"CgPA">>,<<"D">>,<<"CdPA">>,<<>>],
                    separate_fields(absolute_pointers(<<4, 8, 9, 13, Bin/binary>>, 4, true)))},
     {"Pointers in sorted order + empty optional",
      ?_assertEqual([<<"CgPA">>,<<"D">>,<<"CdPA">>,<<>>],
                    separate_fields(absolute_pointers(<<4, 8, 9, 0, Bin/binary>>, 4, true)))},
     {"Pointers in non-sorted order + blank optional",
      ?_assertEqual([<<"CdPA">>,<<"CgPA">>,<<"D">>,<<>>],
                    separate_fields(absolute_pointers(<<11, 3, 7, 13, Bin/binary>>, 4, true)))},
     {"Pointers in non-sorted order + empty optional",
      ?_assertEqual([<<"CdPA">>,<<"CgPA">>,<<"D">>,<<>>],
                    separate_fields(absolute_pointers(<<11, 3, 7, 0, Bin/binary>>, 4, true)))},
     {"Pointers in reverse order + blank optional",
      ?_assertEqual([<<"CdPA">>,<<"D">>,<<"CgPA">>,<<>>],
                    separate_fields(absolute_pointers(<<11, 8, 2, 13, Bin/binary>>, 4, true)))},
     {"Pointers in reverse order + empty optional",
      ?_assertEqual([<<"CdPA">>,<<"D">>,<<"CgPA">>,<<>>],
                    separate_fields(absolute_pointers(<<11, 8, 2, 0, Bin/binary>>, 4, true)))},
     {"Pointers in sorted order",
      ?_assertEqual([<<"CgPA">>,<<"D">>,<<"CdPA">>],
                    separate_fields(absolute_pointers(<<3, 7, 8, Bin/binary>>, 3)))},
     {"Pointers in non-sorted order",
      ?_assertEqual([<<"CdPA">>,<<"CgPA">>,<<"D">>],
                    separate_fields(absolute_pointers(<<10, 2, 6, Bin/binary>>, 3)))},
     {"Pointers in reverse order",
      ?_assertEqual([<<"CdPA">>,<<"D">>,<<"CgPA">>],
                    separate_fields(absolute_pointers(<<10, 7, 1, Bin/binary>>, 3)))}].

encode_msg(cr,
           #{source_local_reference := SourceLocalReference,
             protocol_class := ProtocolClass,
             called_party_address := CalledPartyAddress} = Msg, Opts) ->
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    AllowedParameters = [{credit, 3},
                         {calling_party_address, {4, n}},
                         {data, {3, 130}},
                         {hop_counter, 3},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             protocol_class := ProtocolClass} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    AllowedParameters = [{credit, 3},
                         {called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             refusal_cause := RefusalCause} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    RC = encode_parameter(refusal_cause, RefusalCause, Opts),
    AllowedParameters = [{called_party_address, {4, n}},
                         {data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             release_cause := ReleaseCause} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    RC = encode_parameter(release_cause, ReleaseCause, Opts),
    AllowedParameters = [{data, {3, 130}},
                         {importance, 3},
                         {end_of_optional_parameters, 1}],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             source_local_reference := SourceLocalReference} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(dt1,
           #{destination_local_reference := DestinationLocalReference,
             segmenting_reassembling := SegmentingReassembling,
             data := Data} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SR = encode_parameter(segmenting_reassembling, SegmentingReassembling, Opts),
    D = encode_parameter(data, Data, Opts),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, SR/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(dt2,
           #{destination_local_reference := DestinationLocalReference,
             sequencing_segmenting := SequencingSegmenting,
             data := Data} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SS = encode_parameter(sequencing_segmenting, SequencingSegmenting, Opts),
    D = encode_parameter(data, Data, Opts),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, SS/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ak,
           #{destination_local_reference := DestinationLocalReference,
             receive_sequence_number := ReceiveSequenceNumber,
             credit := Credit} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    RSN = encode_parameter(receive_sequence_number, ReceiveSequenceNumber, Opts),
    C = encode_parameter(credit, Credit, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, RSN/binary, C/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(udt,
           #{protocol_class := ProtocolClass,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := D} = Msg, Opts) ->
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<(3):8/big, (CdPALen+3):8/big, (CgPALen+CdPALen+3):8/big>>,
    <<PC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(udts,
           #{return_cause := ReturnCause,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := D} = Msg, Opts) ->
    RC = encode_parameter(return_cause, ReturnCause, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<(3):8/big, (CdPALen+3):8/big, (CgPALen+CdPALen+3):8/big>>,
    <<RC/binary, Pointers/binary,
      CdPALen:8/big, CdPA/binary, CgPALen:8/big, CgPA/binary, DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ed,
           #{destination_local_reference := DestinationLocalReference,
             data := Data} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    D = encode_parameter(data, Data, Opts),
    DLen = byte_size(D),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<(1):8/big>>,
    <<DLR/binary, Pointers/binary,
      DLen:8/big, D/binary,
      OptBin/binary>>;
encode_msg(ea,
           #{destination_local_reference := DestinationLocalReference} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rsr,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             reset_cause := ResetCause} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    RC = encode_parameter(reset_cause, ResetCause, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, RC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(rsc,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(err,
           #{destination_local_reference := DestinationLocalReference,
             error_cause := ErrorCause} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    EC = encode_parameter(error_cause, ErrorCause, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, EC/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(it,
           #{destination_local_reference := DestinationLocalReference,
             source_local_reference := SourceLocalReference,
             protocol_class := ProtocolClass,
             sequencing_segmenting := SequencingSegmenting,
             credit := Credit} = Msg, Opts) ->
    DLR = encode_parameter(destination_local_reference, DestinationLocalReference, Opts),
    SLR = encode_parameter(source_local_reference, SourceLocalReference, Opts),
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    SS = encode_parameter(sequencing_segmenting, SequencingSegmenting, Opts),
    C = encode_parameter(credit, Credit, Opts),
    AllowedParameters = [],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
    Pointers = <<>>,
    <<DLR/binary, SLR/binary, PC/binary, SS/binary, C/binary, Pointers/binary,
      OptBin/binary>>;
encode_msg(xudt,
           #{protocol_class := ProtocolClass,
             hop_counter := HopCounter,
             called_party_address := CalledPartyAddress,
             calling_party_address := CallingPartyAddress,
             data := D} = Msg, Opts) ->
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    HC = encode_parameter(hop_counter, HopCounter, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    DLen = byte_size(D),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(#{address_type => ansi})],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             data := D} = Msg, Opts) ->
    RC = encode_parameter(return_cause, ReturnCause, Opts),
    HC = encode_parameter(hop_counter, HopCounter, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    DLen = byte_size(D),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(#{address_type => ansi})],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             long_data := LD} = Msg, Opts) ->
    PC = encode_parameter(protocol_class, ProtocolClass, Opts),
    HC = encode_parameter(hop_counter, HopCounter, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    LDLen = byte_size(LD),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(#{address_type => ansi})],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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
             long_data := LD} = Msg, Opts) ->
    RC = encode_parameter(return_cause, ReturnCause, Opts),
    HC = encode_parameter(hop_counter, HopCounter, Opts),
    CdPA = encode_parameter(called_party_address, CalledPartyAddress, Opts),
    CdPALen = byte_size(CdPA),
    CgPA = encode_parameter(calling_party_address, CallingPartyAddress, Opts),
    CgPALen = byte_size(CgPA),
    LDLen = byte_size(LD),
    AllowedParameters = [{segmentation, 6},
                         {importance, 3}
                        |ansi_parameters(#{address_type => ansi})],
    OptBin = encode_parameters(Msg, AllowedParameters, Opts),
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

ansi_parameters(#{address_type := ansi}) ->
    [{intermediate_signaling_network_identification, {3, 18}},
     {intermediate_network_selection, {5, 7}},
     {message_type_interworking, 2},
     {end_of_optional_parameters, 1}
    ];
ansi_parameters(_) ->
    [{end_of_optional_parameters, 1}].

decode_parameters(Bin, Parameters, Opts) ->
    decode_parameters(Bin, Parameters, #{}, Opts).

decode_parameters(_, [], Acc, _Opts) ->
    Acc;
decode_parameters(<<>>, _, Acc, _Opts) ->
    Acc;
decode_parameters(<<?SCCP_IEI_END_OF_OPTIONAL_PARAMETERS:8/big>>, _, Acc, _Opts) ->
    Acc;
decode_parameters(<<IEI:8/big, Len:8/big, Bin0/binary>>, Os, Acc, Opts) ->
    <<V:Len/binary, Rest/binary>> = Bin0,
    Param = parse_iei(IEI),
    case lists:keytake(Param, 1, Os) of
        {value, {Name, _}, NOs} ->
            Par = decode_parameter(Name, V, Opts),
            decode_parameters(Rest, NOs, Acc#{Name => Par}, Opts);
        false ->
            decode_parameters(Rest, Os, Acc, Opts)
    end.

encode_parameters(Msg, Parameters, Opts) ->
    encode_parameters(Msg, Parameters, Opts, <<>>).

encode_parameters(_, [], _Opts, Acc) ->
    Acc;
encode_parameters(_, [{end_of_optional_parameters, _}|_], _Opts, Acc) when byte_size(Acc) > 0 ->
    <<Acc/binary, ?SCCP_IEI_END_OF_OPTIONAL_PARAMETERS:8/big>>;
encode_parameters(Msg, [{Name, _}|Os], Opts, Acc) when is_map_key(Name, Msg) ->
    V = maps:get(Name, Msg),
    Bin = encode_parameter(Name, V, Opts),
    IEI = compose_iei(Name),
    Len = byte_size(Bin),
    NAcc = <<IEI:8/big, Len:8/big, Bin/binary, Acc/binary>>,
    encode_parameters(Msg, Os, Opts, NAcc);
encode_parameters(Msg, [_|Os], Opts, Acc) ->
    encode_parameters(Msg, Os, Opts, Acc).

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
parse_iei(?SCCP_IEI_IMPORTANCE) -> importance;
parse_iei(?SCCP_IEI_ANSI_INTERMEDIATE_SIGNALING_NETWORK_IDENTIFICATION) -> intermediate_signaling_network_identification;
parse_iei(?SCCP_IEI_ANSI_INTERMEDIATE_NETWORK_SELECTION) -> intermediate_network_selection;
parse_iei(?SCCP_IEI_ANSI_MESSAGE_TYPE_INTERWORKING) -> message_type_interworking.

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
compose_iei(importance) -> ?SCCP_IEI_IMPORTANCE;
compose_iei(intermediate_signaling_network_identification) -> ?SCCP_IEI_ANSI_INTERMEDIATE_SIGNALING_NETWORK_IDENTIFICATION;
compose_iei(intermediate_network_selection) -> ?SCCP_IEI_ANSI_INTERMEDIATE_NETWORK_SELECTION;
compose_iei(message_type_interworking) -> ?SCCP_IEI_ANSI_MESSAGE_TYPE_INTERWORKING.

decode_parameter(destination_local_reference, Bin, _Opts) ->
    Bin;
decode_parameter(source_local_reference, Bin, _Opts) ->
    Bin;
decode_parameter(called_party_address, Bin, Opts) ->
    decode_address(Bin, Opts);
decode_parameter(calling_party_address, Bin, Opts) ->
    decode_address(Bin, Opts);
decode_parameter(protocol_class, Bin, _Opts) ->
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
decode_parameter(segmenting_reassembling, Bin, _Opts) ->
    <<_Spare:7, M:1>> = Bin,
    case M of
        0 -> no_more_data;
        1 -> more_data
    end;
decode_parameter(receive_sequence_number, Bin, _Opts) ->
    <<PR:7, _Spare:1>> = Bin,
    PR;
decode_parameter(sequencing_segmenting, Bin, _Opts) ->
    <<PS:7, _Spare:1, PR:7, M:1>> = Bin,
    #{send_sequence_number => PS,
      receive_sequence_number => PR,
      more_data => 1 =:= M
     };
decode_parameter(credit, Bin, _Opts) ->
    Bin;
decode_parameter(release_cause, Bin, _Opts) ->
    <<RC:8>> = Bin,
    parse_release_cause(RC);
decode_parameter(return_cause, Bin, _Opts) ->
    <<RC:8>> = Bin,
    parse_return_cause(RC);
decode_parameter(reset_cause, Bin, _Opts) ->
    <<RC:8>> = Bin,
    parse_reset_cause(RC);
decode_parameter(error_cause, Bin, _Opts) ->
    <<EC:8>> = Bin,
    parse_error_cause(EC);
decode_parameter(refusal_cause, Bin, _Opts) ->
    <<RC:8>> = Bin,
    parse_refusal_cause(RC);
decode_parameter(data, Bin, _Opts) ->
    Bin;
decode_parameter(segmentation, Bin, _Opts) ->
    <<F:1, C:1, _Spare:2, Rem:4, LocalRef:3/binary>> = Bin,
    #{first_segment_indication => 0 == F,
      class => C,
      remaining_segments => Rem,
      local_reference => LocalRef};
decode_parameter(hop_counter, Bin, _Opts) ->
    <<HC:8>> = Bin,
    HC;
decode_parameter(importance, Bin, _Opts) ->
    <<_Spare:5, Importance:3>> = Bin,
    Importance;
decode_parameter(long_data, Bin, _Opts) ->
    Bin;
decode_parameter(intermediate_signaling_network_identification, Bin, _Opts) ->
    <<Counter:3, TI:1, _Res:1, IRI:2, MI:1, RCE:TI/binary, NIDs/binary>> = Bin,
    NS = case RCE of
             <<>> ->
                 #{};
             <<_:6, NetSpec:2>> ->
                 #{network_specific => NetSpec}
         end,
    NetworkIDs = [NI || <<NI:2/binary>> <= NIDs],
    NS#{counter => Counter,
        network_identifiers => NetworkIDs,
        isni_routing_identificator => case IRI of
                                          2#00 -> neither_constrained_nor_suggested;
                                          2#01 -> constrained;
                                          2#10 -> reserved;
                                          2#11 -> spare
                                      end,
        identify_networks => 1 =:= MI};
decode_parameter(intermediate_network_selection, Bin, _Opts) ->
    <<Counter:2, _Res:2, ToR:2, IT:2, NIDs/binary>> = Bin,
    NetworkIDs = case byte_size(NIDs) of
                     4 ->
                         <<NI1:2/binary, NI2:2/binary>> = NIDs,
                         [NI1, NI2];
                     2 ->
                         [NIDs]
                 end,
    #{counter => Counter,
      network_identifiers => NetworkIDs,
      type_of_routing => case ToR of
                             2#00 -> neither_constrained_nor_suggested;
                             2#01 -> constrained;
                             2#10 -> suggested;
                             2#11 -> reserved
                         end,
      information_type => case IT of
                              2#00 -> ss7;
                              2#01 -> reserved;
                              _ -> {network_specific, IT}
                          end
     };
decode_parameter(message_type_interworking, Bin, _Opts) ->
    <<_Res:1, Drop:1, _Spare:3, OMT:3>> = Bin,
    #{parameter_can_be_dropped => 1 =:= Drop,
      original_message_type => case OMT of
                                   2#000 -> unqualified;
                                   2#001 -> udt;
                                   2#010 -> xudt;
                                   2#011 -> ludt;
                                   _ -> spare
                               end
     };
decode_parameter(_, _, _Opts) ->
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
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_MESSAGE_CHANGE_FAILURE) -> message_change_failure;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_INVALID_INS_ROUTING_REQUEST) -> invalid_ins_routing_request;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_INVALID_ISNI_ROUTING_REQUEST) -> invalid_isni_routing_request;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_UNAUTHORIZED_MESSAGE) -> unauthorized_message;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_MESSAGE_INCOMPATIBILITY) -> message_incompatibility;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_ISNI_CONSTRAINED_ROUTING) -> insi_constrained_routing;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_REDUNDANT_ISNI_CONSTRAINED_ROUTING_INFORMATION) -> redundant_isni_constrained_routing_information;
parse_return_cause(?SCCP_IEI_ANSI_RETURN_CAUSE_ISNI_IDENTIFICATION) -> isni_identification;
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
compose_return_cause(message_change_failure) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_MESSAGE_CHANGE_FAILURE;
compose_return_cause(invalid_ins_routing_request) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_INVALID_INS_ROUTING_REQUEST;
compose_return_cause(invalid_isni_routing_request) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_INVALID_ISNI_ROUTING_REQUEST;
compose_return_cause(unauthorized_message) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_UNAUTHORIZED_MESSAGE;
compose_return_cause(message_incompatibility) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_MESSAGE_INCOMPATIBILITY;
compose_return_cause(insi_constrained_routing) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_ISNI_CONSTRAINED_ROUTING;
compose_return_cause(redundant_isni_constrained_routing_information) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_REDUNDANT_ISNI_CONSTRAINED_ROUTING_INFORMATION;
compose_return_cause(isni_identification) -> ?SCCP_IEI_ANSI_RETURN_CAUSE_ISNI_IDENTIFICATION;
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

encode_parameter(destination_local_reference, Bin, _Opts) ->
    Bin;
encode_parameter(source_local_reference, Bin, _Opts) ->
    Bin;
encode_parameter(called_party_address, Bin, Opts) ->
    encode_address(Bin, Opts);
encode_parameter(calling_party_address, Bin, Opts) ->
    encode_address(Bin, Opts);
encode_parameter(protocol_class, Val, _Opts) ->
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
encode_parameter(segmenting_reassembling, Bin, _Opts) ->
    Bin;
encode_parameter(receive_sequence_number, Bin, _Opts) ->
    Bin;
encode_parameter(sequencing_segmenting, Bin, _Opts) ->
    Bin;
encode_parameter(credit, Bin, _Opts) ->
    Bin;
encode_parameter(release_cause, RC, _Opts) ->
    V = compose_release_cause(RC),
    <<V:8>>;
encode_parameter(return_cause, RC, _Opts) ->
    V = compose_return_cause(RC),
    <<V:8>>;
encode_parameter(reset_cause, RC, _Opts) ->
    V = compose_reset_cause(RC),
    <<V:8>>;
encode_parameter(error_cause, EC, _Opts) ->
    V = compose_error_cause(EC),
    <<V:8>>;
encode_parameter(refusal_cause, RC, _Opts) ->
    V = compose_refusal_cause(RC),
    <<V:8>>;
encode_parameter(data, Bin, _Opts) ->
    Bin;
encode_parameter(segmentation, V, _Opts) ->
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
encode_parameter(hop_counter, HC, _Opts) ->
    <<HC:8>>;
encode_parameter(importance, Importance, _Opts) ->
    <<0:5, Importance:3>>;
encode_parameter(long_data, Bin, _Opts) ->
    Bin;
encode_parameter(intermediate_signaling_network_identification, V, _Opts) ->
    #{counter := Counter,
      network_identifiers := NetworkIDs,
      isni_routing_identificator := RoutingID,
      identify_networks := IN} = V,
    IRI = case RoutingID of
              neither_constrained_nor_suggested -> 2#00;
              constrained -> 2#01;
              reserved -> 2#10;
              spare -> 2#11
          end,

    RCE = case maps:get(network_specific, V, undefined) of
              undefined ->
                  <<>>;
              NetSpec ->
                  <<0:6, NetSpec:2>>
          end,
    TI = byte_size(RCE),
    NIDs = binary:list_to_bin(NetworkIDs),
    MI = otc_util:true_to_one(IN),
    <<Counter:3, TI:1, 0:1, IRI:2, MI:1, RCE:TI/binary, NIDs/binary>>;
encode_parameter(intermediate_network_selection, V, _Opts) ->
    #{counter := Counter,
      network_identifiers := NetworkIDs,
      type_of_routing := TypeOfRouting,
      information_type := InformationType
     } = V,
    ToR = case TypeOfRouting of
              neither_constrained_nor_suggested -> 2#00;
              constrained -> 2#01;
              suggested -> 2#10;
              reserved -> 2#11
          end,
    IT = case InformationType of
             ss7 -> 2#00;
             reserved -> 2#01;
             {network_specific, I} -> I
         end,
    NIDs = binary:list_to_bin(NetworkIDs),
    <<Counter:2, 0:2, ToR:2, IT:2, NIDs/binary>>;
encode_parameter(message_type_interworking, V, _Opts) ->
    #{parameter_can_be_dropped := D,
      original_message_type := OriginalMessageType
     } = V,
    Drop = otc_util:true_to_one(D),
    OMT = case OriginalMessageType of
              unqualified -> 2#000;
              udt -> 2#001;
              xudt -> 2#010;
              ludt -> 2#011;
              spare -> 2#111
          end,
    <<0:1, Drop:1, 0:3, OMT:3>>.

decode_address(Bin, #{address_type := ansi}) ->
    decode_ansi_address(Bin);
decode_address(Bin, _Opts) ->
    decode_itu_address(Bin).

%% T1.112.3
decode_ansi_address(<<NR:1, RI:1, GTI:4, PCI:1, SSNI:1, Bin0/binary>>) ->
    {SSN, Bin1} = case SSNI of
                      0 -> {undefined, Bin0};
                      1 -> <<SSN0:8/big, Rest0/binary>> = Bin0,
                           {parse_ssn(SSN0), Rest0}
                  end,
    {PC, Bin2} = case PCI of
                     0 -> {undefined, Bin1};
                     1 -> <<NCM:8, NC:8, NI:8, Rest1/binary>> = Bin1,
                          {<<NI:8, NC:8, NCM:8>>, Rest1}
                 end,
    GT = case GTI of
             2#0000 ->
                 undefined;
             2#0001 ->
                 %% global title includes translation type,
                 %% numbering plan and encoding scheme
                 <<TT:8/big, NP:4, ES:4, GT0/binary>> = Bin2,
                 GT1 = decode_gt_part(ES, GT0),
                 GT1#{translation_type => TT,
                      numbering_plan => NP};
             2#0010 ->
                 %% global title includes translation type
                 %% only
                 <<TT:8/big, GT0/binary>> = Bin2,
                 #{translation_type => TT,
                   address => GT0}
         end,
    RoutingInd = case RI of
                     1 -> subsystem_number;
                     0 -> global_title
                 end,
    #{national_use_indicator => 1 =:= NR,
      routing_indicator => RoutingInd,
      global_title => GT,
      subsystem_number => SSN,
      point_code => PC
     }.

decode_itu_address(<<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, Bin0/binary>>) ->
    {PC, Bin1} = case PCI of
                     0 -> {undefined, Bin0};
                     1 -> <<LSB:8, 0:2, MSB:6, Rest0/binary>> = Bin0,
                          {<<0:2, MSB:6, LSB:8>>, Rest0}
                 end,
    {SSN, Bin2} = case SSNI of
                      0 -> {undefined, Bin1};
                      1 -> <<SSN0:8/big, Rest1/binary>> = Bin1,
                           {parse_ssn(SSN0), Rest1}
                  end,
    GT = case GTI of
             2#0000 ->
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
                 GT1#{translation_type => TT,
                      numbering_plan => NP};
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
    #{national_use_indicator => 1 =:= NR,
      routing_indicator => RoutingInd,
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

encode_address(Address, #{address_type := ansi}) ->
    encode_ansi_address(Address);
encode_address(Address, _Opts) ->
    encode_itu_address(Address).

encode_ansi_address(#{routing_indicator := RoutingInd} = Address) ->
    NR = case maps:get(national_use_indicator, Address, false) of
             true -> 1;
             _ -> 0
         end,
    {PCI, PCBin} = case maps:get(point_code, Address, undefined) of
                       undefined ->
                           {0, <<>>};
                       PC ->
                           <<NI:8, NC:8, NCM:8>> = PC,
                           {1, <<NCM:8, NC:8, NI:8>>}
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
    {GTI, GTBin} = case GlobalTitle of
                       undefined ->
                           {2#0000, <<>>};
                       #{translation_type := TT,
                         address := GT0} ->
                           {2#0010, <<TT:8/big, GT0/binary>>};
                       #{translation_type := TT,
                         numbering_plan := NP} ->
                           GT1 = encode_gt_part(GlobalTitle),
                           EncodingScheme = maps:get(encoding_scheme, GlobalTitle, unknown),
                           ES = compose_encoding_scheme(EncodingScheme, GlobalTitle),
                           {2#0001, <<TT:8/big, NP:4, ES:4, GT1/binary>>}
                   end,
     <<NR:1, RI:1, GTI:4, PCI:1, SSNI:1, PCBin/binary, SSNBin/binary, GTBin/binary>>.

encode_itu_address(#{routing_indicator := RoutingInd} = Address) ->
    NR = case maps:get(national_use_indicator, Address, false) of
             true -> 1;
             _ -> 0
         end,
    {PCI, PCBin} = case maps:get(point_code, Address, undefined) of
                       undefined ->
                           {0, <<>>};
                       PC ->
                           <<0:2, MSB:6, LSB:8>> = PC,
                           {1, <<LSB:8, 0:2, MSB:6>>}
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
    {GTI, GTBin} = case GlobalTitle of
                       #{translation_type := TT,
                         numbering_plan := NP,
                         encoding_scheme := EncodingScheme,
                         nature_of_address_indicator := NI
                        } ->
                           GT1 = encode_gt_part(GlobalTitle),
                           ES = compose_encoding_scheme(EncodingScheme, GlobalTitle),
                           {2#0100, <<TT:8/big, NP:4, ES:4, 0:1, NI:7, GT1/binary>>};
                       #{translation_type := TT,
                         numbering_plan := NP,
                         encoding_scheme := EncodingScheme
                        } ->
                           GT1 = encode_gt_part(GlobalTitle),
                           ES = compose_encoding_scheme(EncodingScheme, GlobalTitle),
                           {2#0011, <<TT:8/big, NP:4, ES:4, GT1/binary>>};
                       #{translation_type := TT
                        } ->
                           GT1 = encode_gt_part(GlobalTitle),
                           {2#0010, <<TT:8/big, GT1/binary>>};
                       #{odd_even_indicator := OE,
                         nature_of_address_indicator := NI,
                         address := GT0
                        } ->
                           OEI = case OE of
                                     even -> 0;
                                     odd -> 1
                                 end,
                           GT1 = encode_bcd(GT0),
                           {2#0001, <<OEI:1, NI:7, GT1/binary>>};
                       _ ->
                           {2#0000, <<>>}
                   end,
    <<NR:1, RI:1, GTI:4, SSNI:1, PCI:1, PCBin/binary, SSNBin/binary, GTBin/binary>>.

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
parse_ssn(?SCCP_SSN_GSM_UMTS_CSS) -> css;
parse_ssn(?SCCP_SSN_GSM_UMTS_PCAP) -> pcap;
parse_ssn(?SCCP_SSN_GSM_UMTS_BSC) -> bssap_bsc;
parse_ssn(?SCCP_SSN_GSM_UMTS_MSC) -> bssap_msc;
parse_ssn(?SCCP_SSN_GSM_UMTS_SMLC) -> bssap_smlc;
parse_ssn(?SCCP_SSN_GSM_UMTS_BSS_OM) -> bss_om;
parse_ssn(?SCCP_SSN_GSM_UMTS_BSSAP) -> bssap;
parse_ssn(?SCCP_SSN_GSM_UMTS_RANAP) -> ranap;
parse_ssn(?SCCP_SSN_GSM_UMTS_RNSAP) -> rnsap;
parse_ssn(?SCCP_SSN_GSM_UMTS_GMLC) -> gmlc;
parse_ssn(?SCCP_SSN_GSM_UMTS_CAP) -> cap;
parse_ssn(?SCCP_SSN_GSM_UMTS_GSMSCF) -> gsmSCF;
parse_ssn(?SCCP_SSN_GSM_UMTS_SIWF) -> siwf;
parse_ssn(?SCCP_SSN_GSM_UMTS_SGSN) -> sgsn;
parse_ssn(?SCCP_SSN_GSM_UMTS_GGSN) -> ggsn;
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
compose_ssn(css) -> ?SCCP_SSN_GSM_UMTS_CSS;
compose_ssn(pcap) -> ?SCCP_SSN_GSM_UMTS_PCAP;
compose_ssn(bssap_bsc) -> ?SCCP_SSN_GSM_UMTS_BSC;
compose_ssn(bssap_msc) -> ?SCCP_SSN_GSM_UMTS_MSC;
compose_ssn(bssap_smlc) -> ?SCCP_SSN_GSM_UMTS_SMLC;
compose_ssn(bss_om) -> ?SCCP_SSN_GSM_UMTS_BSS_OM;
compose_ssn(bssap) -> ?SCCP_SSN_GSM_UMTS_BSSAP;
compose_ssn(ranap) -> ?SCCP_SSN_GSM_UMTS_RANAP;
compose_ssn(rnsap) -> ?SCCP_SSN_GSM_UMTS_RNSAP;
compose_ssn(gmlc) -> ?SCCP_SSN_GSM_UMTS_GMLC;
compose_ssn(cap) -> ?SCCP_SSN_GSM_UMTS_CAP;
compose_ssn(gsmSCF) -> ?SCCP_SSN_GSM_UMTS_GSMSCF;
compose_ssn(siwf) -> ?SCCP_SSN_GSM_UMTS_SIWF;
compose_ssn(sgsn) -> ?SCCP_SSN_GSM_UMTS_SGSN;
compose_ssn(ggsn) -> ?SCCP_SSN_GSM_UMTS_GGSN;
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

%% ITU-T Q.713 Paragraph 3.4.2.3.1 Global title indicator = 0001
%% Each address signal is coded as follows:
%% 0000 - digit 0
%% 0001 - digit 1
%% 0010 - digit 2
%% 0011 - digit 3
%% 0100 - digit 4
%% 0101 - digit 5
%% 0110 - digit 6
%% 0111 - digit 7
%% 1000 - digit 8
%% 1001 - digit 9
%% 1010 - spare
%% 1011 - code 11
%% 1100 - code 12
%% 1101 - spare
%% 1110 - spare
%% 1111 - ST
%%
%% ITU-T Q.101
%% Code 11/12 are used in "Facilities provided in international semi
%% automatic working" (read internation exchanges).
%% It is a way for the outgoing exchange operator (controlling
%% operator) to obtain the incoming operator (code 11), or the delay
%% operator (code 12).
%%
%% ITU-T Q.763
%% ST is the end of pulsing signal (Stop Sending/Transmitting?) and is
%% only defined for called party addresses. It is not in use in IP networks.
decode_bcd_digit(2#1010) -> $a;
decode_bcd_digit(2#1011) -> $b;
decode_bcd_digit(2#1100) -> $c;
decode_bcd_digit(2#1101) -> $d;
decode_bcd_digit(2#1110) -> $e;
decode_bcd_digit(2#1111) -> $f;
decode_bcd_digit(B) -> $0+B.

encode_bcd_digit(A) when A =:= $A; A =:= $a -> 2#1010;
encode_bcd_digit(B) when B =:= $B; B =:= $b -> 2#1011;
encode_bcd_digit(C) when C =:= $C; C =:= $c -> 2#1100;
encode_bcd_digit(D) when D =:= $D; D =:= $d -> 2#1101;
encode_bcd_digit(E) when E =:= $E; E =:= $e -> 2#1110;
encode_bcd_digit(F) when F =:= $F; F =:= $f -> 2#1111;
encode_bcd_digit(D) when D >= $0, D =< $9   -> D - $0.
