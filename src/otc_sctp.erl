-module(otc_sctp).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         encode/1]).

-export([decode_parameters/2]).

spec() ->
    "IETF RFC 2960 October 2000".

next(_) -> '$stop'.

codec(Bin, _Opts) when is_binary(Bin) ->
    decode(Bin);
codec(Map, _Opts) when is_map(Map) ->
    encode(Map).

decode(<<SPort:16, DPort:16, VTag:4/binary, Checksum:4/binary, Chunks/binary>>) ->
    Cs = decode_chunks(Chunks),
    #{source_port => SPort,
      destination_port => DPort,
      verification_tag => VTag,
      checksum => Checksum,
      chunks => Cs}.

encode(Msg) ->
    #{source_port := SPort,
      destination_port := DPort,
      verification_tag := VTag,
      checksum := Checksum,
      chunks := Cs} = Msg,
    Chunks = encode_chunks(Cs),
    <<SPort:16, DPort:16, VTag:4/binary, Checksum:4/binary, Chunks/binary>>.

decode_chunks(<<>>) ->
    [];
decode_chunks(<<0:8, _:5, U:1, BE:2, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<TSN:32, SID:16, SSN:16, PPI:32, Data/binary>> = V,
    Desc = case BE of
               2 -> first_fragment;
               0 -> middle_fragment;
               1 -> last_fragment;
               3 -> unfragmented
           end,
    C = #{chunk_type => data,
          tsn => TSN,
          stream_identifier => SID,
          stream_sequence_number =>
              case U of
                  0 -> SSN;
                  1 -> unordered
              end,
          payload_protocol_identifier => PPI,
          description => Desc,
          data => Data},
    [C|decode_chunks(R)];
decode_chunks(<<1:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<Tag:32, ARWND:32, OS:16, MIS:16, TSN:32, Parameters/binary>> = V,
    C = #{chunk_type => init,
          initiate_tag => Tag,
          advertised_receiver_window_credit => ARWND,
          number_of_outbound_streams => OS,
          number_of_inbound_streams => MIS,
          initial_tsn => TSN,
          parameters => decode_parameters(init, Parameters)
         },
    [C|decode_chunks(R)];
decode_chunks(<<2:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<Tag:32, ARWND:32, OS:16, MIS:16, TSN:32, Parameters/binary>> = V,
    C = #{chunk_type => init_ack,
          initiate_tag => Tag,
          advertised_receiver_window_credit => ARWND,
          number_of_outbound_streams => OS,
          number_of_inbound_streams => MIS,
          initial_tsn => TSN,
          parameters => decode_parameters(init_ack, Parameters)
         },
    [C|decode_chunks(R)];
decode_chunks(<<3:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<CumTSN:32, ARWND:32, GapACKs:16, DupTSN:16, GACKs:(4*GapACKs)/binary, DTSNs:(4*DupTSN)/binary>> = V,
    G = [#{start_ack => S, end_ack => E} || <<S:16, E:16>> <= GACKs],
    D = [T || <<T:32>> <= DTSNs],
    C = #{chunk_type => sack,
          cumulative_tsn => CumTSN,
          advertised_receiver_window_credit => ARWND,
          gap_ack_blocks => G,
          duplicate_tsn => D
         },
    [C|decode_chunks(R)];
decode_chunks(<<4:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<1:16, HBLen:16, HBInfo:(HBLen-4)/binary>> = V,
    C = #{chunk_type => heartbeat,
          info => HBInfo
         },
    [C|decode_chunks(R)];
decode_chunks(<<5:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    <<1:16, HBLen:16, HBInfo:(HBLen-4)/binary>> = V,
    C = #{chunk_type => heartbeat_ack,
          info => HBInfo
         },
    [C|decode_chunks(R)];
decode_chunks(<<6:8, _:7, T:1, Len:16, V:(Len-4)/binary, R/binary>>) ->
    Causes = decode_error_causes(V),
    C = #{chunk_type => abort,
          tcb_destroyed => 0 =:= T,
          error_causes => Causes
         },
    [C|decode_chunks(R)];
decode_chunks(<<7:8, _:8, 8:16, CumTSN:32, R/binary>>) ->
    C = #{chunk_type => shutdown,
          cumulative_tsn_ack => CumTSN
         },
    [C|decode_chunks(R)];
decode_chunks(<<8:8, _:8, 4:16, R/binary>>) ->
    C = #{chunk_type => shutdown_ack
         },
    [C|decode_chunks(R)];
decode_chunks(<<9:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    Causes = decode_error_causes(V),
    C = #{chunk_type => error,
          error_causes => Causes
         },
    [C|decode_chunks(R)];
decode_chunks(<<10:8, _:8, Len:16, V:(Len-4)/binary, R/binary>>) ->
    C = #{chunk_type => cookie_echo,
          cookie => V
         },
    [C|decode_chunks(R)];
decode_chunks(<<11:8, _:8, 4:16, R/binary>>) ->
    C = #{chunk_type => cookie_ack
         },
    [C|decode_chunks(R)];
decode_chunks(<<12:8, _:8, 8:16, LowTSN:32, R/binary>>) ->
    C = #{chunk_type => ecn_echo,
          lowest_tsn => LowTSN
         },
    [C|decode_chunks(R)];
decode_chunks(<<13:8, _:8, 8:16, LowTSN:32, R/binary>>) ->
    C = #{chunk_type => cwr,
          lowest_tsn => LowTSN
         },
    [C|decode_chunks(R)];
decode_chunks(<<14:8, _:7, T:1, 4:16, R/binary>>) ->
    C = #{chunk_type => shutdown_complete,
          tcb_destroyed => 0 =:= T
         },
    [C|decode_chunks(R)].

encode_chunks([]) ->
    <<>>;
encode_chunks([#{chunk_type := data} = Chunk|R]) ->
    #{tsn := TSN,
      stream_identifier := SID,
      stream_sequence_number := SSN0,
      payload_protocol_identifier := PPI,
      description := Desc,
      data := Data} = Chunk,
    {U, SSN} = case SSN0 of
                   unordered -> {1, 0};
                   _ -> {0, SSN0}
               end,
    BE = case Desc of
             first_fragment -> 2;
             middle_fragment -> 0;
             last_fragment -> 1;
             unfragmented -> 3
         end,
    V = <<TSN:32, SID:16, SSN:16, PPI:32, Data/binary>>,
    Len = byte_size(V) + 4,
    <<0:8, 0:5, U:1, BE:2, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := init} = Chunk|R]) ->
    #{initiate_tag := Tag,
      advertised_receiver_window_credit := ARWND,
      number_of_outbound_streams := OS,
      number_of_inbound_streams := MIS,
      initial_tsn := TSN,
      parameters := Parameters
     } = Chunk,
    Ps = encode_parameters(Parameters),
    V = <<Tag:32, ARWND:32, OS:16, MIS:16, TSN:32, Ps/binary>>,
    Len = byte_size(V) + 4,
    <<1:8, 0:8, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := init_ack} = Chunk|R]) ->
    #{initiate_tag := Tag,
      advertised_receiver_window_credit := ARWND,
      number_of_outbound_streams := OS,
      number_of_inbound_streams := MIS,
      initial_tsn := TSN,
      parameters := Parameters
     } = Chunk,
    Ps = encode_parameters(Parameters),
    V = <<Tag:32, ARWND:32, OS:16, MIS:16, TSN:32, Ps/binary>>,
    Len = byte_size(V) + 4,
    <<2:8, 0:8, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := sack} = Chunk|R]) ->
    #{cumulative_tsn := CumTSN,
      advertised_receiver_window_credit := ARWND,
      gap_ack_blocks := G,
      duplicate_tsn := D
     } = Chunk,
    DTSNs = <<<<T:32>> || T <- D>>,
    DupTSN = length(D),
    GACKs = <<<<S:16, E:16>> || #{start_ack := S, end_ack := E} <- G>>,
    GapACKs = length(G),
    V = <<CumTSN:32, ARWND:32, GapACKs:16, DupTSN:16, GACKs:(4*GapACKs)/binary, DTSNs:(4*DupTSN)/binary>>,
    Len = byte_size(V) + 4,
    <<3:8, 0:8, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := heartbeat} = Chunk|R]) ->
    #{info := HBInfo
     } = Chunk,
    HBLen = byte_size(HBInfo) + 4,
    V = <<1:16, HBLen:16, HBInfo/binary>>,
    Len = byte_size(V) + 4,
    <<4:8, 0:8, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := heartbeat_ack} = Chunk|R]) ->
    #{info := HBInfo
     } = Chunk,
    HBLen = byte_size(HBInfo) + 4,
    V = <<1:16, HBLen:16, HBInfo/binary>>,
    Len = byte_size(V) + 4,
    <<5:8, 0:8, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := abort} = Chunk|R]) ->
    #{tcb_destroyed := TCB,
      error_causes := Causes
     } = Chunk,
    T = case TCB of
            true -> 0;
            false -> 1
        end,
    V = encode_error_causes(Causes),
    Len = byte_size(V) + 4,
    <<6:8, 0:7, T:1, Len:16, V/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := shutdown} = Chunk|R]) ->
    #{cumulative_tsn_ack := CumTSN
     } = Chunk,
    <<7:8, 0:8, 8:16, CumTSN:32, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := shutdown_ack} = _Chunk|R]) ->
    <<8:8, 0:8, 4:16, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := error} = Chunk|R]) ->
    #{error_causes := Causes
     } = Chunk,
    V = encode_error_causes(Causes),
    Len = byte_size(V) + 4,
    <<9:8, 0:8, Len:16, V:(Len-4)/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := cookie_echo} = Chunk|R]) ->
    #{cookie := V
     } = Chunk,
    Len = byte_size(V) + 4,
    <<10:8, 0:8, Len:16, V:(Len-4)/binary, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := cookie_ack} = _Chunk|R]) ->
    <<11:8, 0:8, 4:16, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := ecn_echo} = Chunk|R]) ->
    #{lowest_tsn := LowTSN
     } = Chunk,
    <<12:8, 0:8, 8:16, LowTSN:32, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := cwr} = Chunk|R]) ->
    #{lowest_tsn := LowTSN
     } = Chunk,
    <<13:8, 0:8, 8:16, LowTSN:32, (encode_chunks(R))/binary>>;
encode_chunks([#{chunk_type := shutdown_complete} = Chunk|R]) ->
    #{tcb_destroyed := TCB
     } = Chunk,
    T = case TCB of
            true -> 0;
            false -> 1
        end,
    <<14:8, 0:7, T:1, 4:16, (encode_chunks(R))/binary>>.

decode_parameters(T, Bin) ->
    decode_parameters(T, Bin, []).

decode_parameters(_T, <<>>, Acc) ->
    lists:reverse(Acc);
decode_parameters(T, <<PT:16, PL:16, R/binary>>, Acc) ->
    Pad = case (PL rem 4) of
              0 -> 0;
              P -> 4 - P
          end,
    <<PV:(PL-4)/binary, _:Pad/binary, Rest/binary>> = R,
    Parameter = decode_parameter(T, PT, PL, PV),
    decode_parameters(T, Rest, [Parameter|Acc]).

decode_parameter(T, 5, 8, V) when init =:= T; init_ack =:= T ->
    {ok, IP} = inet:parse_ipv4_address(binary_to_list(V)),
    {ipv4, IP};
decode_parameter(T, 6, 20, V) when init =:= T; init_ack =:= T ->
    {ok, IP} = inet:parse_ipv6_address(binary_to_list(V)),
    {ipv6, IP};
decode_parameter(init_ack, 7, _, V) ->
    {state_cookie, V};
decode_parameter(init_ack, 8, _Len, V) ->
    {unrecognized_parameters, V};
decode_parameter(init, 9, 8, V) ->
    <<MS:32/binary>> = V,
    {cookie_preservative, MS};
decode_parameter(T, 11, _Len, V) when init =:= T; init_ack =:= T ->
    %% "host name syntax" per RFC1123 Section 2.1
    {hostname, V};
decode_parameter(init, 12, _Len, V) ->
    ATs = decode_address_types(V),
    {supported_address_types, ATs};
decode_parameter(T, 16#8000, 4, <<>>) when init =:= T; init_ack =:= T ->
    {ecn_capable, true};
decode_parameter(T, 16#C000, 4, <<>>) when init =:= T; init_ack =:= T ->
    %% IETF RFC3758
    {forward_tsn_supported, true};
decode_parameter(_T, P, _Len, V) ->
    {unrecognised, {P, V}}.

encode_parameters(Params) ->
    encode_parameters(lists:reverse(Params), <<>>).

encode_parameters([], Acc) ->
    Acc;
encode_parameters([Parameter|Rest], Acc) ->
    {PT, PL, PV} = encode_parameter(Parameter),
    Pad = case (PL rem 4) of
              0 ->
                  0;
              P ->
                  4 - P
          end,
    NewAcc = <<PT:16, (PL+4):16, PV/binary, 0:(Pad*8), Acc/binary>>,
    encode_parameters(Rest, NewAcc).

encode_parameter({ipv4, V}) ->
    {5, 8, inet:ntoa(V)};
encode_parameter({ipv6, V}) ->
    {6, 20, inet:ntoa(V)};
encode_parameter({state_cookie, V}) ->
    {7, byte_size(V), V};
encode_parameter({unrecognized_parameters, V}) ->
    {8, byte_size(V), V};
encode_parameter({cookie_preservative, V}) ->
    {9, 8, <<V:32>>};
encode_parameter({hostname, V}) ->
    {11, byte_size(V), V};
encode_parameter({supported_address_types, V0}) ->
    V = encode_address_types(V0),
    {12, byte_size(V), V};
encode_parameter({ecn_capable, true}) ->
    {16#8000, 0, <<>>};
encode_parameter({forward_tsn_supported, true}) ->
    {16#C000, 0, <<>>};
encode_parameter({unrecognised, {T, V}}) ->
    {T, byte_size(V), V}.

decode_address_types(<<>>) ->
    [];
decode_address_types(<<0:16>>) ->
    [];
decode_address_types(<<5:16, R/binary>>) ->
    [ipv4|decode_address_types(R)];
decode_address_types(<<6:16, R/binary>>) ->
    [ipv6|decode_address_types(R)];
decode_address_types(<<11:16, R/binary>>) ->
    [hostname|decode_address_types(R)].

encode_address_types(Ts) ->
    encode_address_types(lists:reverse(Ts), <<>>).

encode_address_types([], Acc) ->
    Acc;
encode_address_types([ipv4|R], Acc) ->
    encode_address_types(R, <<5:16, Acc/binary>>);
encode_address_types([ipv6|R], Acc) ->
    encode_address_types(R, <<6:16, Acc/binary>>);
encode_address_types([hostname|R], Acc) ->
    encode_address_types(R, <<11:16, Acc/binary>>).

decode_error_causes(<<>>) ->
    [];
decode_error_causes(<<1:16, 8:16, SID:16, _:16, R/binary>>) ->
    C = #{cause_code => invalid_stream_identifier,
          stream_identifier => SID},
    [C|decode_error_causes(R)];
decode_error_causes(<<2:16, Len:16, MP:32, V:(Len-8)/binary, R/binary>>) ->
    Params = [P || <<P:16>> <= V],
    C = #{cause_code => missing_mandatory_parameter,
          missing_parameters => lists:sublist(Params, MP)},
    [C|decode_error_causes(R)];
decode_error_causes(<<3:16, 8:16, V:32, R/binary>>) ->
    C = #{cause_code => stale_cookie_error,
          measure_of_staleness => V},
    [C|decode_error_causes(R)];
decode_error_causes(<<4:16, 4:16, R/binary>>) ->
    C = #{cause_code => out_of_resource},
    [C|decode_error_causes(R)];
decode_error_causes(<<5:16, Len:16, V:(Len-4)/binary, R/binary>>) ->
    C = #{cause_code => unresolvable_address,
          unresolvable_address => V},
    [C|decode_error_causes(R)];
decode_error_causes(<<6:16, Len:16, V:(Len-4)/binary, R/binary>>) ->
    C = #{cause_code => unrecognized_chunk_type,
          unrecognized_chunk => V},
    [C|decode_error_causes(R)];
decode_error_causes(<<7:16, 4:16, R/binary>>) ->
    C = #{cause_code => invalid_mandatory_parameter},
    [C|decode_error_causes(R)];
decode_error_causes(<<8:16, Len:16, V:(Len-4)/binary, R/binary>>) ->
    C = #{cause_code => unrecognized_parameters,
          unrecognized_parameters => V},
    [C|decode_error_causes(R)];
decode_error_causes(<<9:16, 8:16, TSN:32, R/binary>>) ->
    C = #{cause_code => no_user_data,
          tsn => TSN},
    [C|decode_error_causes(R)];
decode_error_causes(<<10:16, 4:16, R/binary>>) ->
    C = #{cause_code => cookie_received_while_shutting_down},
    [C|decode_error_causes(R)].

encode_error_causes([]) ->
    <<>>;
encode_error_causes([#{cause_code := invalid_stream_identifier} = Cause|R]) ->
    #{stream_identifier := SID} = Cause,
    <<1:16, 8:16, SID:16, 0:16, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := missing_mandatory_parameter} = Cause|R]) ->
    #{missing_parameters := Params} = Cause,
    V = <<<<P:16>> || P <- Params>>,
    MP = length(Params),
    Len = byte_size(V) + 8,
    <<2:16, Len:16, MP:32, V:(Len-8)/binary, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := stale_cookie_error} = Cause|R]) ->
    #{measure_of_staleness := V} = Cause,
    <<3:16, 8:16, V:32, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := out_of_resource} = _Cause|R]) ->
    <<4:16, 4:16, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := unresolvable_address} = Cause|R]) ->
    #{unresolvable_address := V} = Cause,
    Len = byte_size(V) + 4,
    <<5:16, Len:16, V/binary, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := unrecognized_chunk_type} = Cause|R]) ->
    #{unrecognized_chunk := V} = Cause,
    Len = byte_size(V) + 4,
    <<6:16, Len:16, V/binary, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := invalid_mandatory_parameter} = _Cause|R]) ->
    <<7:16, 4:16, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := unrecognized_parameters} = Cause|R]) ->
    #{unrecognized_parameters := V} = Cause,
    Len = byte_size(V) + 4,
    <<8:16, Len:16, V/binary, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := no_user_data} = Cause|R]) ->
    #{tsn := TSN} = Cause,
    <<9:16, 8:16, TSN:32, (encode_error_causes(R))/binary>>;
encode_error_causes([#{cause_code := cookie_received_while_shutting_down} = _Cause|R]) ->
    <<10:16, 4:16, (encode_error_causes(R))/binary>>.

