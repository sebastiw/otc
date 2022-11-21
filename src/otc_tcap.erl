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

next(#{dialogue :=
           #{application_context_family := M}}) -> {ok, M};
next(_) -> '$stop'.

codec(Bin) when is_binary(Bin) ->
    case decode(Bin) of
        #{dialogue := #{application_context_family := map} = Dialogue,
          components := [_|_] = Cs} = TC -> {TC, {Dialogue, Cs}};
        TC -> TC
    end;

codec(Map) when is_map(Map) ->
    encode(Map).

decode(Bin) ->
    {ok, D} = 'TCAP-OTC':decode('TCMessage', Bin),
    decode_message(D).

encode(Map) ->
    TC = encode_message(Map),
    {ok, Bin} = 'TCAP-OTC':encode('TCMessage', TC),
    Bin.

decode_message({'unidirectional', TC}) ->
    Components = decode_components(maps:get(components, TC, [])),
    TCDialogue = decode_dialogue(TC),
    #{type => 'unidirectional',
      dialogue => TCDialogue,
      components => Components};
decode_message({'begin', #{otid := OT} = TC}) ->
    Components = decode_components(maps:get(components, TC, [])),
    TCDialogue = decode_dialogue(TC),
    #{type => 'begin',
      otid => binary:encode_hex(OT),
      dialogue => TCDialogue,
      components => Components};
decode_message({'continue', #{otid := OT, dtid := DT} = TC}) ->
    Components = decode_components(maps:get(components, TC, [])),
    TCDialogue = decode_dialogue(TC),
    #{type => 'continue',
      otid => binary:encode_hex(OT),
      dtid => binary:encode_hex(DT),
      dialogue => TCDialogue,
      components => Components};
decode_message({'end', #{dtid := DT} = TC}) ->
    Components = decode_components(maps:get(components, TC, [])),
    TCDialogue = decode_dialogue(TC),
    #{type => 'end',
      dtid => binary:encode_hex(DT),
      dialogue => TCDialogue,
      components => Components};
decode_message({'abort', #{dtid := DT} = TC}) ->
    Reason = case maps:get(reason, TC, undefined) of
                 undefined -> undefined;
                 #{'p-abortCause' := P} -> P;
                 #{'u-abortCause' := U} -> U
             end,
    #{type => 'abort',
      dtid => binary:encode_hex(DT),
      reason => Reason
     }.

encode_message(#{type := 'unidirectional'} = Map) ->
    Components = encode_components(maps:get(components, Map, [])),
    Dialogue = encode_dialogue(maps:get(dialogue, Map, undefined)),
    Base = #{dialoguePortion => Dialogue,
             components => Components},
    {'unidirectional', remove_undefined_field(dialoguePortion, Base)};
encode_message(#{type := 'begin', otid := OT} = Map) ->
    Components = encode_components(maps:get(components, Map, [])),
    Dialogue = encode_dialogue(maps:get(dialogue, Map, undefined)),
    Base = #{otid => binary:decode_hex(OT),
             dialoguePortion => Dialogue,
             components => Components},
    {'begin', remove_undefined_fields([dialoguePortion], Base)};
encode_message(#{type := 'continue', otid := OT, dtid := DT} = Map) ->
    Components = encode_components(maps:get(components, Map, [])),
    Dialogue = encode_dialogue(maps:get(dialogue, Map, undefined)),
    Base = #{otid => binary:decode_hex(OT),
             dtid => binary:decode_hex(DT),
             dialoguePortion => Dialogue,
             components => Components},
    {'continue', remove_undefined_fields([dialoguePortion, components], Base)};
encode_message(#{type := 'end', dtid := DT} = Map) ->
    Components = encode_components(maps:get(components, Map, [])),
    Dialogue = encode_dialogue(maps:get(dialogue, Map, undefined)),
    Base = #{dtid => binary:decode_hex(DT),
             dialoguePortion => Dialogue,
             components => Components},
    {'end', remove_undefined_fields([dialoguePortion, components], Base)};
encode_message(#{type := 'abort', dtid := DT} = Map) ->
    Base = #{dtid => binary:decode_hex(DT),
             reason => maps:get(reason, Map, undefined)
            },
    {'abort', remove_undefined_field(reason, Base)}.

decode_dialogue(#{dialoguePortion := #{'direct-reference' := DR, encoding := {'single-ASN1-type', Enc}}}) ->
    UniID = 'UnidialoguePDUs':'uniDialogue-as-id'(),
    DiaID = 'DialoguePDUs':'dialogue-as-id'(),
    {ok, {DiaType, Dia0}} = case DR of
                                UniID ->
                                    'UnidialoguePDUs':decode('UniDialoguePDU', Enc);
                                DiaID ->
                                    'DialoguePDUs':decode('DialoguePDU', Enc)
                            end,
    Base = #{type => DiaType,
             user_information => maps:get('user-information', Dia0, undefined)
            },
    case DiaType of
        dialogueRequest ->
            {AC, ACN} = parse_application_context_name(Dia0),
            Base#{application_context_family => AC,
                  application_context_name => ACN,
                  supported_versions => maps:get('protocol-version', Dia0),
                  user_information => maps:get('user-information', Dia0, undefined)
                 };
        dialogueResponse ->
            {AC, ACN} = parse_application_context_name(Dia0),
            Base#{application_context_family => AC,
                  application_context_name => ACN,
                  supported_versions => maps:get('protocol-version', Dia0),
                  result => maps:get('result', Dia0),
                  result_source_diagnostic => maps:get('result-source-diagnostic', Dia0),
                  user_information => maps:get('user-information', Dia0, undefined)
                 };
        dialogueAbort ->
            Base#{abort_source => maps:get('abort-source', Dia0),
                  user_information => maps:get('user-information', Dia0, undefined)
                 };
        unidialoguePDU ->
            {AC, ACN} = parse_application_context_name(Dia0),
            Base#{application_context_family => AC,
                  application_context_name => ACN,
                  supported_versions => maps:get('protocol-version', Dia0),
                  user_information => maps:get('user-information', Dia0, undefined)
                 }
    end;
decode_dialogue(_) ->
    #{}.

encode_dialogue(#{type := dialogueRequest} = Map) ->
    ACN = compose_application_context_name(Map),
    Dia = #{'protocol-version' => maps:get(supported_versions, Map, [version1]),
            'application-context-name' => ACN,
            'user-information' => maps:get(user_information, Map, undefined)
           },
    Dialogue = remove_undefined_field('user-information', Dia),
    {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {dialogueRequest, Dialogue}),
    #{'direct-reference' => 'DialoguePDUs':'dialogue-as-id'(),
      encoding => {'single-ASN1-type', Enc}};
encode_dialogue(#{type := dialogueResponse} = Map) ->
    ACN = compose_application_context_name(Map),
    Dia = #{'protocol-version' => maps:get(supported_versions, Map, [version1]),
            'application-context-name' => ACN,
            'result' => maps:get(result, Map),
            'result-source-diagnostic' => maps:get(result_source_diagnostic, Map),
            'user-information' => maps:get(user_information, Map, undefined)
           },
    Dialogue = remove_undefined_field('user-information', Dia),
    {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {dialogueResponse, Dialogue}),
    #{'direct-reference' => 'DialoguePDUs':'dialogue-as-id'(),
      encoding => {'single-ASN1-type', Enc}};
encode_dialogue(#{type := dialogueAbort} = Map) ->
    Dia = #{'abort-source' => maps:get(abort_source, Map, undefined),
            'user-information' => maps:get(user_information, Map, undefined)
           },
    Dialogue = remove_undefined_field('user-information', Dia),
    {ok, Enc} = 'DialoguePDUs':encode('DialoguePDU', {dialogueAbort, Dialogue}),
    #{'direct-reference' => 'DialoguePDUs':'dialogue-as-id'(),
      encoding => {'single-ASN1-type', Enc}};
encode_dialogue(#{type := unidialoguePDU} = Map) ->
    ACN = compose_application_context_name(Map),
    Dia = #{'protocol-version' => maps:get(supported_versions, Map, [version1]),
            'application-context-name' => ACN,
            'user-information' => maps:get(user_information, Map, undefined)
           },
    Dialogue = remove_undefined_field('user-information', Dia),
    {ok, Enc} = 'UnidialoguePDUs':encode('UnidialoguePDU', {unidialoguePDU, Dialogue}),
    #{'direct-reference' => 'UnidialoguePDUs':'uniDialogue-as-id'(),
      encoding => {'single-ASN1-type', Enc}};
encode_dialogue(_) ->
    #{}.

parse_application_context_name(#{'application-context-name' := ACN}) when is_tuple(ACN) ->
    MAPAC = tuple_to_list('MAP-ApplicationContexts':'map-ac'()),
    IsMAP = lists:prefix(MAPAC, tuple_to_list(ACN)),
    if IsMAP ->
            {map, otc_map:parse_application_context(ACN)};
       true ->
            {unknown, ACN}
    end.

compose_application_context_name(#{application_context_family := map} = D) ->
    ACN = maps:get(application_context_name, D),
    otc_map:compose_application_context(ACN);
compose_application_context_name(#{application_context_family := unknown} = D) ->
    maps:get(application_context_name, D).

decode_components([]) ->
    [];
decode_components([C|Cs]) ->
    [decode_component(C)|decode_components(Cs)].

encode_components([]) ->
    [];
encode_components([C|Cs]) ->
    [encode_component(C)|encode_components(Cs)].

decode_component({invoke, C}) ->
    InvokeId = parse_invoke_id(maps:get(invokeId, C, absent)),
    LinkedId = parse_invoke_id(maps:get(linkedId, C, absent)),
    C#{component_type => invoke,
       invokeId => InvokeId,
       linkedId => LinkedId
      };
decode_component({R, C}) when returnResultLast =:= R; returnResultNotLast =:= R ->
    InvokeId = parse_invoke_id(maps:get(invokeId, C, absent)),
    Base = C#{component_type => R,
              invokeId => InvokeId
             },
    case C of
        #{result := Result} ->
            #{opcode := OpCode,
              result := Res} = Result,
            Base#{opcode => OpCode, result => Res};
        _ ->
            Base
    end;
decode_component({returnError, C}) ->
    InvokeId = parse_invoke_id(maps:get(invokeId, C, absent)),
    C#{component_type => returnError,
       invokeId => InvokeId
      };
decode_component({reject, C}) ->
    InvokeId = parse_invoke_id(maps:get(invokeId, C, absent)),
    {ProblemType, Problem} = maps:get(problem, C),
    C#{component_type => returnError,
       invokeId => InvokeId,
       problem_type => ProblemType,
       problem => Problem
      }.

encode_component(#{component_type := invoke} = C) ->
    InvokeId = compose_invoke_id(maps:get(invokeId, C, absent)),
    LinkedId = compose_invoke_id(maps:get(linkedId, C, absent)),
    {invoke, C#{invokeId => InvokeId,
                linkedId => LinkedId}};
encode_component(#{component_type := returnResultLast} = C) ->
    InvokeId = compose_invoke_id(maps:get(invokeId, C, absent)),
    LinkedId = compose_invoke_id(maps:get(linkedId, C, absent)),
    Base = C#{invokeId => InvokeId,
              linkedId => LinkedId},
    case C of
        #{opcode := OpCode, result := Res} ->
            Comp = Base#{result => #{opcode => OpCode, result => Res}},
            {returnResultLast, Comp};
        _ ->
            {returnResultLast, Base}
    end;
encode_component(#{component_type := CType} = C) ->
    {CType, C}.

parse_invoke_id({present, I}) ->
    I;
parse_invoke_id(absent) ->
    absent.

compose_invoke_id(I) when is_integer(I) ->
    {present, I};
compose_invoke_id(absent) ->
    absent.

remove_undefined_fields([], Map) ->
    Map;
remove_undefined_fields([F|Fs], Map) ->
    remove_undefined_fields(Fs, remove_undefined_field(F, Map)).

remove_undefined_field(Field, Map) when undefined =:= erlang:map_get(Field, Map);
                                        [] =:= erlang:map_get(Field, Map) ->
    maps:remove(Field, Map);
remove_undefined_field(_, Map) ->
    Map.
