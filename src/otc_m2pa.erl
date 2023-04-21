-module(otc_m2pa).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

-include("include/m2pa.hrl").

spec() ->
    "IETF RFC 4165 September 2005".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

next(#{message_type := user_data}) -> {ok, mtp3};
next(_) -> '$stop'.

decode(<<1:8, _:8, ?M2PA_MSG_CLASS_MSGS:8, MessageType:8, Len:32/big, Remain/binary>>) ->
    %%  0                   1                   2                   3
    %%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |    Version    |     Spare     | Message Class | Message Type  |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |                        Message Length                         |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    MT = parse_message_type(MessageType),
    ValLen = Len - 8,                 % Remove version, spare, mc, mt, length
    <<Bin:ValLen/binary>> = Remain,
    %%  0                   1                   2                   3
    %%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |     unused    |                      BSN                      |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    %% |     unused    |                      FSN                      |
    %% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    <<_:8, BSN:24/big, _:8, FSN:24/big, Rest/binary>> = Bin,
    Base = #{protocol => m2pa,
             message_type => MT,
             backward_sequence_number => BSN,
             forward_sequence_number => FSN
            },
    case decode_msg(MT, Rest) of
        {Msg, Data} ->
            {maps:merge(Msg, Base), Data};
        Msg ->
            maps:merge(Msg, Base)
    end.

encode(#{protocol := m2pa, message_type := MessageType} = Msg) ->
    MT = compose_message_type(MessageType),
    Bin = encode_msg(MessageType, Msg),
    BSN = maps:get(backward_sequence_number, Msg, 0),
    FSN = maps:get(forward_sequence_number, Msg, 0),
    Len = byte_size(Bin) + 8 + 8,
    <<1:8, 0:8, ?M2PA_MSG_CLASS_MSGS:8, MT:8, Len:32/big, 0:8, BSN:24/big, 0:8, FSN:24/big, Bin/binary>>.

parse_message_type(?M2PA_MSG_TYPE_USER_DATA) -> user_data;
parse_message_type(?M2PA_MSG_TYPE_LINK_STATUS) -> link_status.

compose_message_type(user_data) -> ?M2PA_MSG_TYPE_USER_DATA;
compose_message_type(link_status) -> ?M2PA_MSG_TYPE_LINK_STATUS.


decode_msg(user_data, Data) ->
    %% 2.3.1 User data
    case Data of
        <<>> ->
            #{};
        <<PRI:2, _:6, Payload/binary>> ->
            %% PRI - Priority used only in national MTP defined in [JT-Q703] and
            %% [JT-Q704].  These bits are spare for other MTP versions.
            {#{priority => PRI}, Payload}
    end;
decode_msg(link_status, <<State:32/big>>) ->
    #{link_status => parse_link_status(State)};
decode_msg(link_status, <<State:32/big, Filler/binary>>) ->
    proving_normal = parse_link_status(State),
    #{link_status => proving_normal,
      link_status_filler => Filler}.

encode_msg(user_data, Msg) ->
    case Msg of
        #{priority := PRI} ->
            <<PRI:2, 0:6>>;
        _ ->
            <<>>
    end;
encode_msg(link_status, Msg) ->
    LinkStatus = maps:get(link_status, Msg),
    LS = compose_link_status(LinkStatus),
    case LinkStatus of
        proving_normal ->
            Filler = maps:get(link_status_filler, Msg, <<>>),
            <<LS:32/big, Filler/binary>>;
        _ ->
            <<LS:32/big>>
    end.

parse_link_status(?LINK_STATUS_STATE_ALIGNMENT) -> alignment;
parse_link_status(?LINK_STATUS_STATE_PROVING_NORMAL) -> proving_normal;
parse_link_status(?LINK_STATUS_STATE_PROVING_EMERGENCY) -> proving_emergency;
parse_link_status(?LINK_STATUS_STATE_READY) -> ready;
parse_link_status(?LINK_STATUS_STATE_PROCESSOR_OUTAGE) -> processor_outage;
parse_link_status(?LINK_STATUS_STATE_PROCESSOR_RECOVERED) -> processor_recovered;
parse_link_status(?LINK_STATUS_STATE_BUSY) -> busy;
parse_link_status(?LINK_STATUS_STATE_BUSY_ENDED) -> busy_ended;
parse_link_status(?LINK_STATUS_STATE_OUT_OF_SERVICE) -> out_of_service.

compose_link_status(alignment) -> ?LINK_STATUS_STATE_ALIGNMENT;
compose_link_status(proving_normal) -> ?LINK_STATUS_STATE_PROVING_NORMAL;
compose_link_status(proving_emergency) -> ?LINK_STATUS_STATE_PROVING_EMERGENCY;
compose_link_status(ready) -> ?LINK_STATUS_STATE_READY;
compose_link_status(processor_outage) -> ?LINK_STATUS_STATE_PROCESSOR_OUTAGE;
compose_link_status(processor_recovered) -> ?LINK_STATUS_STATE_PROCESSOR_RECOVERED;
compose_link_status(busy) -> ?LINK_STATUS_STATE_BUSY;
compose_link_status(busy_ended) -> ?LINK_STATUS_STATE_BUSY_ENDED;
compose_link_status(out_of_service) -> ?LINK_STATUS_STATE_OUT_OF_SERVICE.
