-module(otc_sccp_codec).

-export([decode/1,
         encode/1
        ]).

-include("include/sccp.hrl").

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
    AllowedParameters = [{source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {credit, optional, 3},
                         {calling_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {hop_counter, optional, 3},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(cc, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {credit, optional, 3},
                         {called_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(cref, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {refusal_cause, fixed, 1},
                         {called_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rlsd, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {release_cause, fixed, 1},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rlc, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(dt1, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {segmenting_reassembling, fixed, 1},
                         {data, variable, {2, 256}}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(dt2, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {sequencing_segmenting, fixed, 2},
                         {data, variable, {2, 256}}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ak, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {receive_sequence_number, fixed, 1},
                         {credit, fixed, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(udt, Bin) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(udts, Bin) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ed, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {data, variable, {2, 33}}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ea, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rsr, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {reset_cause, fixed, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(rsc, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(err, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {error_cause, fixed, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(it, Bin) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {sequencing_segmenting, fixed, 2},
                         {credit, fixed, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(xudt, Bin) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(xudts, Bin) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ludt, Bin) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {long_data, variable, {3, 3954}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(ludts, Bin) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {long_data, variable, {3, 3954}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    decode_parameters(Bin, AllowedParameters);
decode_msg(_, _) ->
    unsupported.

encode_msg(cr, Msg) ->
    AllowedParameters = [{source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {credit, optional, 3},
                         {calling_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {hop_counter, optional, 3},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(cc, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {credit, optional, 3},
                         {called_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(cref, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {refusal_cause, fixed, 1},
                         {called_party_address, optional, {4, n}},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rlsd, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {release_cause, fixed, 1},
                         {data, optional, {3, 130}},
                         {importance, optional, 3},
                         {end_of_optional_parameter, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rlc, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(dt1, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {segmenting_reassembling, fixed, 1},
                         {data, variable, {2, 256}}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(dt2, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {sequencing_segmenting, fixed, 2},
                         {data, variable, {2, 256}}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ak, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {receive_sequence_number, fixed, 1},
                         {credit, fixed, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(udt, Msg) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(udts, Msg) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ed, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {data, variable, {2, 33}}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ea, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rsr, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {reset_cause, fixed, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(rsc, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(err, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {error_cause, fixed, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(it, Msg) ->
    AllowedParameters = [{destination_local_reference, fixed, 3},
                         {source_local_reference, fixed, 3},
                         {protocol_class, fixed, 1},
                         {sequencing_segmenting, fixed, 2},
                         {credit, fixed, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(xudt, Msg) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(xudts, Msg) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {data, variable, {2, n}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ludt, Msg) ->
    AllowedParameters = [{protocol_class, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {long_data, variable, {3, 3954}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    encode_parameters(Msg, AllowedParameters);
encode_msg(ludts, Msg) ->
    AllowedParameters = [{return_cause, fixed, 1},
                         {hop_counter, fixed, 1},
                         {called_party_address, variable, {3, n}},
                         {calling_party_address, variable, {3, n}},
                         {long_data, variable, {3, 3954}},
                         {segmentation, optional, 6},
                         {importance, optional, 3},
                         {end_of_optional_parameters, optional, 1}],
    encode_parameters(Msg, AllowedParameters).

decode_parameters(Bin0, Parameters) ->
    {Fs, Vs, Os} = split_parameters(Parameters),
    {Fixed, Bin1} = decode_fixed(Bin0, Fs),
    <<_Pointers:(length(Vs)+1)/binary, Bin2/binary>> = Bin1,
    {Variabled, Bin3} = decode_variabled(Bin2, Vs, Fixed),
    {Optionals, _} = decode_optionals(Bin3, Os, Variabled),
    maps:merge(maps:merge(Optionals, Variabled), Fixed).

encode_parameters(Msg, Parameters) ->
    {Fs, Vs, Os} = split_parameters(Parameters),
    <<>>.

split_parameters(Parameters0) ->
    {Fs, Parameters1} = lists:splitwith(fun ({_, T, _}) -> fixed == T end, Parameters0),
    {Vs, Os} = lists:splitwith(fun ({_, T, _}) -> variable == T end, Parameters1),
    {Fs, Vs, Os}.

decode_fixed(Bin, Fs) ->
    decode_fixed(Bin, Fs, #{}).

decode_fixed(_, [], Acc) ->
    Acc;
decode_fixed(Bin, [{Name, _, Len}|Fs], Acc) ->
    <<V:Len/binary, Rest/binary>> = Bin,
    NAcc = decode_parameter(Name, V, Acc),
    decode_fixed(Rest, Fs, NAcc).

decode_variabled(_, [], Acc) ->
    Acc;
decode_variabled(Bin0, [{Name, _, Len}|Fs], Acc) ->
    <<Len:8/big, Bin1/binary>> = Bin0,
    <<V:Len/binary, Rest/binary>> = Bin1,
    NAcc = decode_parameter(Name, V, Acc),
    decode_variabled(Rest, Fs, NAcc).

decode_optionals(_, [], Acc) ->
    Acc;
decode_optionals(<<?SCCP_IEI_END_OF_OPTIONAL_PARAMETERS>>, _, Acc) ->
    Acc;
decode_optionals(<<IEI:8/big, Len:8/big, Bin0/binary>>, Os, Acc) ->
    <<V:Len/binary, Rest/binary>> = Bin0,
    Param = parse_iei(IEI),
    case lists:keytake(Param, 1, Os) of
        {value, {Name, _, _}, NOs} ->
            NAcc = decode_parameter(Name, V, Acc),
            decode_optionals(Rest, NOs, NAcc);
        false ->
            decode_optionals(Rest, Os, Acc)
    end.

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

decode_parameter(destination_local_reference = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(source_local_reference = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(called_party_address = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(calling_party_address = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(protocol_class = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(segmenting_reassembling = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(receive_sequence_number = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(sequencing_segmenting = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(credit = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(release_cause = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(return_cause = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(reset_cause = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(error_cause = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(refusal_cause = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(data = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(segmentation = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(hop_counter = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(importance = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(long_data = Name, Bin, Acc) ->
    Acc#{Name => Bin};
decode_parameter(_, _, Acc) ->
    Acc.
