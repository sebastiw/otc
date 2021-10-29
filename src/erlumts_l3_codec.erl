%% ETSI TS 124 007 V16.5.0
-module(erlumts_l3_codec).

-include("include/l3.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([parse_protocol_discriminator/1,
         compose_protocol_discriminator/1
        ]).
-export([decode_v/2,
         decode_lv/1,
         decode_lve/1,
         decode_iei_list/2,
         encode_v/3,
         encode_lv/2,
         encode_lve/2,
         encode_iei_list/2
        ]).

-type iei_type() :: t | v | tv | lv | tlv | lve | tlve.
-type iei_fixed_length() :: half | pos_integer().
-type iei_length() :: iei_fixed_length() | {Min :: pos_integer(), Max :: pos_integer() | n}.
-type iei_tuple() :: {Name :: atom(), IEI :: integer(), Type :: iei_type(), Length :: iei_length()}.
-type iei_list() :: list(iei_tuple()).

%% 11.2.3.1.1 Protocol discriminator
parse_protocol_discriminator(?L3_PD_GROUP_CALL_CONTROL) -> group_call_control;
parse_protocol_discriminator(?L3_PD_BROADCAST_CALL_CONTROL) -> broadcast_call_control;
parse_protocol_discriminator(?L3_PD_EPS_SESSION_MANAGEMENT_MESSAGES) -> eps_session_management_messages;
parse_protocol_discriminator(?L3_PD_CALL_CONTROL) -> call_control;
parse_protocol_discriminator(?L3_PD_GPRS_TRANSPARENT_TRANSPORT_PROTOCOL) -> gprs_transparent_transport_protocol;
parse_protocol_discriminator(?L3_PD_MOBILITY_MANAGEMENT_MESSAGES) -> mobility_management_messages;
parse_protocol_discriminator(?L3_PD_RADIO_RESOURCES_MANAGEMENT_MESSAGES) -> radio_resources_management_messages;
parse_protocol_discriminator(?L3_PD_EPS_MOBILITY_MANAGEMENT_MESSAGES) -> eps_mobility_management_messages;
parse_protocol_discriminator(?L3_PD_GPRS_MOBILITY_MANAGEMENT_MESSAGES) -> gprs_mobility_management_messages;
parse_protocol_discriminator(?L3_PD_SMS_MESSAGES) -> sms_messages;
parse_protocol_discriminator(?L3_PD_GPRS_SESSION_MANAGEMENT_MESSAGES) -> gprs_session_management_messages;
parse_protocol_discriminator(?L3_PD_NON_CALL_RELATED_SS_MESSAGES) -> non_call_related_ss_messages;
parse_protocol_discriminator(?L3_PD_LOCATION_SERVICES) -> location_services; % 3GPP TS 44.071
parse_protocol_discriminator(?L3_PD_EXTENSION_OF_PD) -> extension_of_PD;
parse_protocol_discriminator(?L3_PD_TESTS_PROCEDURES) -> tests_procedures; % 3GPP TS 44.014

%% 11.2.3.1.1A Extended protocol discriminator (EPD)
parse_protocol_discriminator(?L3_EPD_5GS_SESSION_MANAGEMENT_MESSAGES) -> '5gs_session_management_messages';
parse_protocol_discriminator(?L3_EPD_5GS_MOBILITY_MANAGEMENT_MESSAGES) -> '5gs_mobility_management_messages';
parse_protocol_discriminator(PD) ->
    {undefined, PD}.

%% 11.2.3.1.1 Protocol discriminator
compose_protocol_discriminator(group_call_control) -> ?L3_PD_GROUP_CALL_CONTROL;
compose_protocol_discriminator(broadcast_call_control) -> ?L3_PD_BROADCAST_CALL_CONTROL;
compose_protocol_discriminator(eps_session_management_messages) -> ?L3_PD_EPS_SESSION_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(call_control) -> ?L3_PD_CALL_CONTROL;
compose_protocol_discriminator(gprs_transparent_transport_protocol) -> ?L3_PD_GPRS_TRANSPARENT_TRANSPORT_PROTOCOL;
compose_protocol_discriminator(mobility_management_messages) -> ?L3_PD_MOBILITY_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(radio_resources_management_messages) -> ?L3_PD_RADIO_RESOURCES_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(eps_mobility_management_messages) -> ?L3_PD_EPS_MOBILITY_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(gprs_mobility_management_messages) -> ?L3_PD_GPRS_MOBILITY_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(sms_messages) -> ?L3_PD_SMS_MESSAGES;
compose_protocol_discriminator(gprs_session_management_messages) -> ?L3_PD_GPRS_SESSION_MANAGEMENT_MESSAGES;
compose_protocol_discriminator(non_call_related_ss_messages) -> ?L3_PD_NON_CALL_RELATED_SS_MESSAGES;
compose_protocol_discriminator(location_services) -> ?L3_PD_LOCATION_SERVICES; % 3GPP TS 44.071
compose_protocol_discriminator(extension_of_PD) -> ?L3_PD_EXTENSION_OF_PD;
compose_protocol_discriminator(tests_procedures) -> ?L3_PD_TESTS_PROCEDURES; % 3GPP TS 44.014

%% 11.2.3.1.1A Extended protocol discriminator (EPD)
compose_protocol_discriminator('5gs_session_management_messages') -> ?L3_EPD_5GS_SESSION_MANAGEMENT_MESSAGES;
compose_protocol_discriminator('5gs_mobility_management_messages') -> ?L3_EPD_5GS_MOBILITY_MANAGEMENT_MESSAGES;
compose_protocol_discriminator({undefined, PD}) -> PD.

-spec decode_v(binary(), iei_length()) -> {integer() | binary(), bitstring()}.
decode_v(<<V:4/big, Rest/bitstring>>, half) ->
    {V, Rest};
decode_v(Bin, L) when is_integer(L) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest};
decode_v(Bin, {_, n}) ->
    {Bin, <<>>}.

-spec encode_v(integer() | binary(), iei_fixed_length(), binary()) -> binary().
encode_v(V, half, Acc) ->
    <<V:4/big, Acc/bitstring>>;
encode_v(V, L, Acc) when is_integer(L) ->
    <<V:L/binary, Acc/binary>>.

-spec decode_lv(binary()) -> {binary(), binary()}.
decode_lv(<<L:8/big, Bin/bitstring>>) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest}.

-spec encode_lv(binary(), binary()) -> binary().
encode_lv(Value, Acc) ->
    L = byte_size(Value),
    encode_lv(Value, L+1, Acc).

-spec encode_lv(binary(), iei_length(), binary()) -> binary().
encode_lv(Value, L0, Acc) ->
    Length = get_length(Value, L0, 1),
    <<Length:8/big, Value:Length/binary, Acc/bitstring>>.

-spec decode_lve(binary()) -> {binary(), binary()}.
decode_lve(<<L:16/big, Bin/bitstring>>) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest}.

-spec encode_lve(binary(), binary()) -> binary().
encode_lve(Value, Acc) ->
    L = byte_size(Value),
    encode_lve(Value, L+2, Acc).

-spec encode_lve(binary(), iei_length(), binary()) -> binary().
encode_lve(Value, L0, Acc) ->
    Length = get_length(Value, L0, 2),
    <<Length:16/big, Value:Length/binary, Acc/bitstring>>.


-spec decode_iei_list(binary(), iei_list()) -> {Decoded :: map(), Remaining :: bitstring()}.
decode_iei_list(Bin, Opts) ->
    decode_iei_list(Bin, Opts, #{}).

decode_iei_list(<<>>, _Opts, Acc) ->
    {Acc, <<>>};
decode_iei_list(Bin, [], Acc) ->
    {Acc, Bin};
decode_iei_list(<<_V1:4/big, V2:4/big, Rest/binary>>, [{Name, _IEI, v, half}|Opts], Acc) ->
    %% Type 1
    decode_iei_list(Rest, Opts, Acc#{Name => <<V2:4/big>>});
decode_iei_list(Bin, [{Name, _IEI, v, L}|Opts], Acc) when is_binary(Bin); L > 0 ->
    %% Type 3
    <<V:L/binary, Rest/binary>> = Bin,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(<<L:8, Bin/binary>>, [{Name, _IEI, lv, _L2}|Opts], Acc) when is_binary(Bin); L > 0 ->
    %% Type 4
    <<V:L/binary, Rest/binary>> = Bin,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(<<IEI:8/big, Rest/binary>>, [{Name, IEI, t, 1}|Opts], Acc) ->
    %% Type 2
    decode_iei_list(Rest, Opts, Acc#{Name => <<>>});
decode_iei_list(<<IEI:4/big, V:4/big, Rest/binary>>, [{Name, IEI, tv, 1}|Opts], Acc) ->
    %% Type 1
    decode_iei_list(Rest, Opts, Acc#{Name => <<V:4/big>>});
decode_iei_list(<<IEI:8/big, R/binary>>, [{Name, IEI, tv, L}|Opts], Acc) ->
    %% Type 3
    <<V:(L-1)/binary, Rest/binary>> = R,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(<<IEI:8/big, L2:8/big, R/binary>>, [{Name, IEI, tlv, _L}|Opts], Acc) ->
    %% Type 4
    <<V:L2/binary, Rest/binary>> = R,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(<<L:16, Bin/binary>>, [{Name, _IEI, lve, _L2}|Opts], Acc) when is_binary(Bin); L > 0 ->
    %% Type 6
    <<V:L/binary, Rest/binary>> = Bin,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(<<IEI:8/big, L2:16/big, R/binary>>, [{Name, IEI, tlve, _L}|Opts], Acc) ->
    %% Type 6
    <<V:L2/binary, Rest/binary>> = R,
    decode_iei_list(Rest, Opts, Acc#{Name => V});
decode_iei_list(Bin, [_T|Ts], Acc) ->
    decode_iei_list(Bin, Ts, Acc).


decode_iei_list_test_() ->
    [{"Shouldn't return any values",
      [{"Empty everything returns empty",
        ?_assertEqual({#{}, <<>>}, decode_iei_list(<<>>, []))},
       {"No more message",
        ?_assertEqual({#{}, <<>>}, decode_iei_list(<<>>, [{iei_1, 4, v, half}]))},
       {"No more options",
        ?_assertEqual({#{}, <<0:48>>}, decode_iei_list(<<0:48>>, []))}
      ]},
     {"Basic Type 1",
      [?_assertEqual({#{iei_1 => <<7:4>>}, <<>>}, decode_iei_list(<<8:4, 7:4>>, [{iei_1, 8, v, half}])),
       ?_assertEqual({#{iei_1 => <<24:8>>}, <<>>}, decode_iei_list(<<24:8>>, [{iei_1, 24, v, 1}])),
       ?_assertEqual({#{iei_1 => <<7:4>>}, <<>>}, decode_iei_list(<<8:4, 7:4>>, [{iei_1, 8, tv, 1}]))
      ]},
     {"Basic Type 2",
      [?_assertEqual({#{iei_1 => <<>>}, <<>>}, decode_iei_list(<<24:8>>, [{iei_1, 24, t, 1}]))
      ]},
     {"Basic Type 3",
      [?_assertEqual({#{iei_1 => <<24:8, 55:8, 44:8>>}, <<>>}, decode_iei_list(<<24:8, 55:8, 44:8>>, [{iei_1, 24, v, 3}])),
       ?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<24:8, 55:8, 44:8>>, [{iei_1, 24, tv, 3}]))
      ]},
     {"Basic Type 4",
      [?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<2:8, 55:8, 44:8>>, [{iei_1, 24, lv, 3}])),
       ?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<24:8, 2:8, 55:8, 44:8>>, [{iei_1, 24, tlv, 4}]))
      ]},
     {"Basic Type 6",
      [?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<2:16, 55:8, 44:8>>, [{iei_1, 24, lve, 4}])),
       ?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<24:8, 2:16, 55:8, 44:8>>, [{iei_1, 24, tlve, 5}]))
      ]},
     {"Multivalues",
      [?_assertEqual({#{iei_1 => <<55:8, 44:8>>, iei_2 => <<55:8, 44:8>>}, <<>>},
                     decode_iei_list(<<2:16, 55:8, 44:8, 24:8, 2:16, 55:8, 44:8>>,
                                     [{iei_1, 24, lve, 4}, {iei_2, 24, tlve, 5}]))
      ]}
    ].

-spec encode_iei_list(map(), iei_list()) -> bitstring().
encode_iei_list(Msg, Opts) ->
    encode_iei_list(Msg, lists:reverse(Opts), <<>>).

encode_iei_list(_Msg, [], Acc) ->
    Acc;
encode_iei_list(Msg, [{Name, _IEI, v, half}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 1
    V = maps:get(Name, Msg),
    encode_iei_list(Msg, Opts, <<V:4/bitstring, Acc/binary>>);
encode_iei_list(Msg, [{Name, _IEI, v, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 3
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 0),
    encode_iei_list(Msg, Opts, <<V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [{Name, _IEI, lv, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 4
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 1),
    encode_iei_list(Msg, Opts, <<L:8, V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [{Name, IEI, t, 1}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 2
    _V = maps:get(Name, Msg),
    encode_iei_list(Msg, Opts, <<IEI:8/big, Acc/binary>>);
encode_iei_list(Msg, [{Name, IEI, tv, 1}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 1
    V = maps:get(Name, Msg),
    encode_iei_list(Msg, Opts, <<IEI:4/big, V:4/bitstring, Acc/binary>>);
encode_iei_list(Msg, [{Name, IEI, tv, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 3
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 1),
    encode_iei_list(Msg, Opts, <<IEI:8/big, V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [{Name, IEI, tlv, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 4
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 2),
    encode_iei_list(Msg, Opts, <<IEI:8/big, L:8/big, V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [{Name, _IEI, lve, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 6
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 2),
    encode_iei_list(Msg, Opts, <<L:16, V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [{Name, IEI, tlve, L0}|Opts], Acc) when is_map_key(Name, Msg) ->
    %% Type 6
    V = maps:get(Name, Msg),
    L = get_length(V, L0, 3),
    encode_iei_list(Msg, Opts, <<IEI:8/big, L:16/big, V:L/binary, Acc/binary>>);
encode_iei_list(Msg, [_T|Ts], Acc) ->
    encode_iei_list(Msg, Ts, Acc).

get_length(V, {MinLen, n}, ToRemove) ->
    max(byte_size(V), MinLen-ToRemove);
get_length(V, {MinLen, MaxLen}, ToRemove) ->
    min(max(byte_size(V), MinLen-ToRemove), MaxLen-ToRemove);
get_length(_V, L, ToRemove) when is_integer(L) ->
    L-ToRemove.

encode_iei_list_test_() ->
    [{"Shouldn't return any values",
      [{"Empty everything returns empty",
        ?_assertEqual(<<>>, encode_iei_list(#{}, []))},
       {"No more message",
        ?_assertEqual(<<>>, encode_iei_list(#{}, [{iei_1, 4, v, half}]))},
       {"No more options",
        ?_assertEqual(<<>>, encode_iei_list(#{iei_1 => <<0:48>>}, []))}
      ]},
     {"Basic Type 1",
      [?_assertEqual(<<7:4>>, encode_iei_list(#{iei_1 => <<7:4>>}, [{iei_1, 8, v, half}])),
       ?_assertEqual(<<24:8>>, encode_iei_list(#{iei_1 => <<24:8>>}, [{iei_1, 24, v, 1}])),
       ?_assertEqual(<<8:4, 7:4>>, encode_iei_list(#{iei_1 => <<7:4>>}, [{iei_1, 8, tv, 1}]))
      ]},
     {"Basic Type 2",
      [?_assertEqual(<<24:8>>, encode_iei_list(#{iei_1 => <<>>}, [{iei_1, 24, t, 1}]))
      ]},
     {"Basic Type 3",
      [?_assertEqual(<<24:8, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<24:8, 55:8, 44:8>>}, [{iei_1, 24, v, 3}])),
       ?_assertEqual(<<24:8, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<55:8, 44:8>>}, [{iei_1, 24, tv, 3}]))
      ]},
     {"Basic Type 4",
      [?_assertEqual(<<2:8, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<55:8, 44:8>>}, [{iei_1, 24, lv, 3}])),
       ?_assertEqual(<<24:8, 2:8, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<55:8, 44:8>>}, [{iei_1, 24, tlv, 4}]))
      ]},
     {"Basic Type 6",
      [?_assertEqual(<<2:16, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<55:8, 44:8>>}, [{iei_1, 24, lve, 4}])),
       ?_assertEqual(<<24:8, 2:16, 55:8, 44:8>>, encode_iei_list(#{iei_1 => <<55:8, 44:8>>}, [{iei_1, 24, tlve, 5}]))
      ]},
     {"Multivalues",
      [?_assertEqual(<<2:16, 55:8, 44:8, 24:8, 2:16, 55:8, 44:8>>,
                     encode_iei_list(#{iei_1 => <<55:8, 44:8>>, iei_2 => <<55:8, 44:8>>},
                                     [{iei_1, 24, lve, 4}, {iei_2, 24, tlve, 5}]))
      ]}
    ].
