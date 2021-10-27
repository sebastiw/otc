%% ETSI TS 124 007 V16.5.0
-module(erlumts_l3_codec).

-include("include/l3.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([parse_protocol_discriminator/1]).
-export([decode_v/2, decode_lv/1, decode_lve/1, decode_iei_list/2]).

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



-spec decode_v(binary(), iei_fixed_length()) -> {integer() | binary(), bitstring()}.
decode_v(<<V:4/big, Rest/bitstring>>, half) ->
    {V, Rest};
decode_v(Bin, L) when is_integer(L) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest}.

-spec decode_lv(binary()) -> {binary(), binary()}.
decode_lv(<<L:8/big, Bin/bitstring>>) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest}.

-spec decode_lve(binary()) -> {binary(), binary()}.
decode_lve(<<L:16/big, Bin/bitstring>>) ->
    <<V:L/binary, Rest/binary>> = Bin,
    {V, Rest}.


-spec decode_iei_list(binary(), iei_list()) -> {Decoded :: [{Name :: atom(), Value :: binary()}],
                                                Remaining :: bitstring()}.
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
decode_iei_list(Bin, [T|Ts], Acc) ->
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
      [?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<2:16, 55:8, 44:8>>, [{iei_1, 24, lve, 3}])),
       ?_assertEqual({#{iei_1 => <<55:8, 44:8>>}, <<>>}, decode_iei_list(<<24:8, 2:16, 55:8, 44:8>>, [{iei_1, 24, tlve, 4}]))
      ]},
     {"Multivalues",
      [?_assertEqual({#{iei_1 => <<55:8, 44:8>>, iei_2 => <<55:8, 44:8>>}, <<>>},
                     decode_iei_list(<<2:16, 55:8, 44:8, 24:8, 2:16, 55:8, 44:8>>,
                                     [{iei_1, 24, lve, 3}, {iei_2, 24, tlve, 4}]))
      ]}
    ].
