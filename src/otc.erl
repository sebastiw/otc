-module(otc).
%% Inspired by [pkt](https://github.com/msantos/pkt) and [ossie](https://github.com/massemanet/ossie)

-export([decapsulate/1,
         decapsulate/2,
         decode/1,
         decode/2,
         encode/1,
         sctp_ppi/1,
         sctp/1,
         m2pa/1,
         mtp3/1,
         m3ua/1,
         sccp/1,
         nas_eps/1,
         nas_eps_emm/1,
         nas_eps_esm/1
        ]).

-include_lib("kernel/include/logger.hrl").

-spec otc:decapsulate(_Data) -> _Packet.
-spec otc:decapsulate(_Proto, _Data) -> _Packet.
%% decapsulate/1,2 works on valid packets. If the packet is malformed
%% or unsupported, decapsulate/1 will crash.
decapsulate(Proto, Data) ->
    decapsulate({Proto, Data}).

decapsulate({PPI, Data}) when is_integer(PPI) ->
    decapsulate_next({sctp_ppi(PPI), Data}, []);
decapsulate({Proto, Data}) when is_atom(Proto) ->
    decapsulate_next({Proto, Data}, []);
decapsulate(Data) when is_binary(Data) ->
    decapsulate_next({sctp, Data}, []).

decapsulate_next({'$stop', Data}, Headers) ->
    lists:reverse([Data|Headers]);
decapsulate_next({Proto, Data}, Headers) ->
    {Header, Payload} = ?MODULE:Proto(Data),
    decapsulate_next({next({Proto, Header}), Payload}, [Header|Headers]).

-type data() :: binary().
-type protocol() :: m3ua | sccp.
-spec otc:decode(data()) ->
          {ok, _Packet} |
          {error, _SoFar, {_FailedProto, binary()}}.
-spec otc:decode(protocol(), data()) ->
          {ok, _Packet} |
          {error, _SoFar, {_FailedProto, binary()}}.
%% Similar to decapsulate/1 but, on error, returns any part of the
%% packet that has been successfully converted to Erlang term format.
decode(Data) when is_binary(Data) ->
    decode(sctp, Data).
decode(Proto, Data) ->
    decode_next({Proto, Data}, []).

decode_next({Proto, Data}, Headers) ->
    try ?MODULE:Proto(Data) of
        {ok, {Header, Payload}} ->
            case next({Proto, Header}) of
                '$stop' ->
                    {ok, {lists:reverse([Header#{protocol => Proto}|Headers]), Payload}};
                {ok, Next} ->
                    decode_next({Next, Payload}, [Header#{protocol => Proto}|Headers])
            end;
        {ok, Header} ->
            {ok, lists:reverse([Header#{protocol => Proto}|Headers])};
        {error, _, _} = Error ->
            Error
    catch E:R:S ->
            ?LOG_ERROR({E, R, S}),
            {error, lists:reverse(Headers), {unsupported, Data}}
    end.


next({sctp_ppi, V}) -> otc_sctp_ppi:next(V);
%% next({sctp, V}) -> otc_sctp:next(V);
next({m2pa, V}) -> otc_m3ua:next(V);
next({m3ua, V}) -> otc_m3ua:next(V);
next({mtp3, V}) -> otc_mtp3:next(V);
next({sccp, V}) -> otc_sccp:next(V);
next({nas_eps, V}) -> otc_nas_eps:next(V);
next({nas_eps_emm, V}) -> otc_nas_eps_emm:next(V);
next({nas_eps_esm, V}) -> otc_nas_eps_esm:next(V).

sctp_ppi(PPI) -> otc_sctp_ppi:codec(PPI).
sctp(D) -> otc_sctp:codec(D).
m2pa(D) -> otc_m2pa:codec(D).
mtp3(D) -> otc_mtp3:codec(D).
m3ua(D) -> otc_m3ua:codec(D).
sccp(D) -> otc_sccp:codec(D).
nas_eps(D) -> otc_nas_eps:codec(D).
nas_eps_emm(D) -> otc_nas_eps_emm:codec(D).
nas_eps_esm(D) -> otc_nas_eps_esm:codec(D).


encode(#{protocol := Proto} = Pdu) ->
    ?MODULE:Proto(Pdu);
encode(Pdus) when is_list(Pdus) ->
    encode({Pdus, <<>>});
encode({Pdus, Payload}) when is_list(Pdus) or is_tuple(Pdus), is_binary(Payload) ->
    lists:foldr(fun (P, Acc) ->
                        B = encode(P),
                        <<B/binary, Acc/binary>>
                end, Payload, Pdus);
encode({#{protocol := Proto} = Pdu, Payload}) ->
    ?MODULE:Proto({Pdu, Payload});
encode({Proto, Data}) when is_atom(Proto) ->
    ?MODULE:Proto(Data);
encode(Data) when is_binary(Data) ->
    Data.
