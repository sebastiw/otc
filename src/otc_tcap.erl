-module(otc_tcap).

-behaviour(otc_codec).

-export([spec/0,
         next/1,
         codec/1,
         decode/1,
         encode/1
        ]).

spec() ->
    "ITU-T Q.773 (06/97)".

next(#{dialoguePortion :=
           #{'application-context-name' := {0,4,0,0,1,0,21,3},
             'protocol-version' := [version1]}}) -> {ok, map};
next(_) -> '$stop'.

codec(Bin) when is_binary(Bin) ->
    decode(Bin);

codec(Map) when is_map(Map) ->
    encode(Map).

decode(Bin) ->
    {ok, {Type, #{dialoguePortion := Dialogue} = TC0}} = 'TCAPMessages-simple':decode('TCMessage', Bin),
    {ok, {DiaType, Dia0}} = decode_dialogue(Dialogue),
    TC1 = maps:without([dialoguePortion], TC0),
    TC = parse_transaction_ids(TC1),
    Dia = parse_application_context_name(Dia0),
    TC#{message_type => Type,
        dialogue_type => DiaType,
        dialogue_portion => Dia
       }.

encode(#{message_type := Type} = Map) ->
    Dia = encode_dialogue(Map),
    Msg = compose_transaction_ids(Dia),
    'TCAPMessages-simple':encode('TCMessage', {Type, Msg}).

parse_transaction_ids(#{dtid := DT, otid := OT} = M) ->
    M#{dtid => binary:encode_hex(DT),
       otid => binary:encode_hex(OT)};
parse_transaction_ids(#{dtid := DT} = M) ->
    M#{dtid => binary:encode_hex(DT)};
parse_transaction_ids(#{otid := OT} = M) ->
    M#{otid => binary:encode_hex(OT)};
parse_transaction_ids(M) ->
    M.

compose_transaction_ids(#{dtid := DT, otid := OT} = M) ->
    M#{dtid => binary:decode_hex(DT),
       otid => binary:decode_hex(OT)};
compose_transaction_ids(#{dtid := DT} = M) ->
    M#{dtid => binary:decode_hex(DT)};
compose_transaction_ids(#{otid := OT} = M) ->
    M#{otid => binary:decode_hex(OT)};
compose_transaction_ids(M) ->
    M#{}.

decode_dialogue(#{'direct-reference' := DR, encoding := {'single-ASN1-type', Enc}}) ->
    UniID = 'UnidialoguePDUs':'uniDialogue-as-id'(),
    DiaID = 'DialoguePDUs':'dialogue-as-id'(),
    case DR of
        UniID ->
            'UnidialoguePDUs':decode('UniDialoguePDU', Enc);
        DiaID ->
            'DialoguePDUs':decode('DialoguePDU', Enc)
    end.

encode_dialogue(#{dialogue_type := DiaType, dialogue_portion := Dia} = Map) ->
    case DiaType of
        dialogueRequest ->
            DiaID = 'DialoguePDUs':'dialogue-as-id'(),
            {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', Dia),
            Map#{dialoguePortion =>
                     #{'direct-reference' => DiaID,
                       encoding => {'single-ASN1-type', Enc}}}
    end.

parse_application_context_name(#{'application-context-name' := ACN}) ->
    MAP = tuple_to_list('MAP-ApplicationContexts':'map-ac'()),
    case lists:prefix(MAP, tuple_to_list(ACN)) of
        true ->
            parse_map_application_context_name(ACN)
    end.

parse_map_application_context_name(ACN) ->
    ACNs = [{'MAP-ApplicationContexts':F(), F} || {F, 0} <- 'MAP-ApplicationContexts':module_info(exports)],
    case lists:keyfind(ACN, 1, ACNs) of
        false -> ACN;
        {_, F} -> F
    end.
