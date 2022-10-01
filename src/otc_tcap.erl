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
           #{'application-context-name' := {0,4,0,0,1,0,21,3}}}) -> {ok, map};
next(_) -> '$stop'.

codec(Bin) when is_binary(Bin) ->
    decode(Bin);

codec(Map) when is_map(Map) ->
    encode(Map).

decode(Bin) ->
    {ok, D} = 'TCAPMessages-simple':decode('TCMessage', Bin),
    decode_message(D).

encode(Map) ->
    TC = encode_message(Map),
    {ok, Bin} = 'TCAPMessages-simple':encode('TCMessage', TC),
    Bin.

decode_message({'begin', #{otid := OT} = TC}) ->
    TCDialogue = decode_dialogue(TC),
    TCDialogue#{type => 'begin',
                otid => binary:encode_hex(OT)};
decode_message({'continue', #{otid := OT, dtid := DT} = TC}) ->
    TCDialogue = decode_dialogue(TC),
    TCDialogue#{type => 'continue',
                otid => binary:encode_hex(OT),
                dtid => binary:encode_hex(DT)};
decode_message({'end', #{dtid := DT} = TC}) ->
    TCDialogue = decode_dialogue(TC),
    TCDialogue#{type => 'end',
                dtid => binary:encode_hex(DT)};
decode_message({'abort', #{dtid := DT} = TC}) ->
    TC#{type => 'abort',
        dtid => binary:encode_hex(DT)}.

encode_message(#{type := 'begin', otid := OT} = Map) ->
    Dialogue = encode_dialogue(Map),
    {'begin', #{dialoguePortion => Dialogue,
                otid => binary:decode_hex(OT)}};
encode_message(#{type := 'continue', otid := OT, dtid := DT} = Map) ->
    Dialogue = encode_dialogue(Map),
    {'continue', #{dialoguePortion => Dialogue,
                   otid => binary:decode_hex(OT),
                   dtid => binary:decode_hex(DT)}};
encode_message(#{type := 'end', dtid := DT} = Map) ->
    Dialogue = encode_dialogue(Map),
    {'end', #{dialoguePortion => Dialogue,
              dtid => binary:decode_hex(DT)}};
encode_message(#{type := 'abort', dtid := DT} = Map) ->
    Dialogue = encode_dialogue(Map),
    {'abort', #{dialoguePortion => Dialogue,
                   dtid => binary:decode_hex(DT)}}.

decode_dialogue(#{dialoguePortion := #{'direct-reference' := DR, encoding := {'single-ASN1-type', Enc}}}) ->
    UniID = 'UnidialoguePDUs':'uniDialogue-as-id'(),
    DiaID = 'DialoguePDUs':'dialogue-as-id'(),
    {ok, {DiaType, Dia0}} = case DR of
                                UniID ->
                                    'UnidialoguePDUs':decode('UniDialoguePDU', Enc);
                                DiaID ->
                                    'DialoguePDUs':decode('DialoguePDU', Enc)
                            end,
    ACN = parse_application_context_name(Dia0),
    #{dialogue_type => DiaType,
      application_context_name => ACN}.

encode_dialogue(#{dialogue_type := DiaType} = Map) ->
    ACN = compose_application_context_name(Map),
    DiaID = 'DialoguePDUs':'dialogue-as-id'(),
    UniID = 'UnidialoguePDUs':'uniDialogue-as-id'(),
    case DiaType of
        dialogueRequest ->
            Dia = #{'protocol-version' => [version1],
                    'application-context-name' => ACN
                    %% 'user-information' =>
                   },
            {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {DiaType, Dia}),
            #{'direct-reference' => DiaID,
              encoding => {'single-ASN1-type', Enc}};
        dialogueResponse ->
            Dia = #{'protocol-version' => [version1],
                    'application-context-name' => ACN
                    %% 'result' => ,
                    %% 'result-source-diagnostic' => ,
                    %% 'user-information' =>
                   },
            {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {DiaType, Dia}),
            #{'direct-reference' => DiaID,
              encoding => {'single-ASN1-type', Enc}};
        dialogueAbort ->
            Dia = #{%% 'abort-source' => ,
                    %% 'user-information' =>
                   },
            {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {DiaType, Dia}),
            #{'direct-reference' => DiaID,
              encoding => {'single-ASN1-type', Enc}};
        unidialoguePDU ->
            Dia = #{'protocol-version' => [version1],
                    'application-context-name' => ACN
                    %% 'user-information' =>
                   },
            {ok, Enc} = 'UnidialoguePDUs':encode('UnidialoguePDU', {DiaType, Dia}),
            #{'direct-reference' => UniID,
              encoding => {'single-ASN1-type', Enc}}
    end.

parse_application_context_name(#{'application-context-name' := ACN}) ->
    ACNs = [{'MAP-ApplicationContexts':F(), F}
            || {F, 0} <- 'MAP-ApplicationContexts':module_info(exports)],
    case lists:keyfind(ACN, 1, ACNs) of
        false -> ACN;
        {_, F} -> F
    end.

compose_application_context_name(#{application_context_name := ACN}) ->
    ACNs = [{'MAP-ApplicationContexts':F(), F}
            || {F, 0} <- 'MAP-ApplicationContexts':module_info(exports)],
    case lists:keyfind(ACN, 2, ACNs) of
        false -> ACN;
        {F, _} -> F
    end.
