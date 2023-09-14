-module(otc).
%% Inspired by [pkt](https://github.com/msantos/pkt) and [ossie](https://github.com/massemanet/ossie)

%% Supported protocol
-export([sctp_ppi/1,
         %% sctp/1,
         m2pa/1,
         mtp3/1,
         m3ua/1,
         sccp/1,
         sccp_mgmt/1,
         tcap/1,
         map/1,
         nas_eps/1,
         nas_eps_emm/1,
         nas_eps_esm/1,
         nas_5gs/1,
         nas_5gs_5gmm/1,
         nas_5gs_5gsm/1,
         gtpv1c/1,
         gtpv2c/1
        ]).

%% General functions
-export([%% decapsulate/1,
         decapsulate/2,
         decapsulate/3,
         %% decode/1,
         decode/2,
         decode/3,
         encode/1]).

-include_lib("kernel/include/logger.hrl").

%% Supported protocols ---------------------------------------------------------

-type protocol() :: sctp_ppi | m3ua | m2pa | mtp3 | sccp | tcap | map | nas_eps | nas_eps_emm | nas_eps_esm | gtpv1c | gtpv2c.

next({sctp_ppi, V}) -> otc_sctp_ppi:next(V);
%% next({sctp, V}) -> otc_sctp:next(V);
next({m2pa, V}) -> otc_m2pa:next(V);
next({m3ua, V}) -> otc_m3ua:next(V);
next({mtp3, V}) -> otc_mtp3:next(V);
next({sccp, V}) -> otc_sccp:next(V);
next({sccp_mgmt, V}) -> otc_sccp_mgmt:next(V);
next({tcap, V}) -> otc_tcap:next(V);
next({map, V}) -> otc_map:next(V);
next({nas_eps, V}) -> otc_nas_eps:next(V);
next({nas_eps_emm, V}) -> otc_nas_eps_emm:next(V);
next({nas_eps_esm, V}) -> otc_nas_eps_esm:next(V);
next({nas_5gs, V}) -> otc_nas_5gs:next(V);
next({nas_5gs_5gmm, V}) -> otc_nas_5gs_5gmm:next(V);
next({nas_5gs_5gsm, V}) -> otc_nas_5gs_5gsm:next(V);
next({gtpv1c, V}) -> otc_gtpv1c:next(V);
next({gtpv2c, V}) -> otc_gtpv2c:next(V).

sctp_ppi(PPI) -> otc_sctp_ppi:codec(PPI).
%% sctp(D) -> otc_sctp:codec(D).
m2pa(D) -> otc_m2pa:codec(D).
mtp3(D) -> otc_mtp3:codec(D).
m3ua(D) -> otc_m3ua:codec(D).
sccp(D) -> otc_sccp:codec(D).
sccp_mgmt(D) -> otc_sccp_mgmt:codec(D).
tcap(D) -> otc_tcap:codec(D).
map(D) -> otc_map:codec(D).
nas_eps(D) -> otc_nas_eps:codec(D).
nas_eps_emm(D) -> otc_nas_eps_emm:codec(D).
nas_eps_esm(D) -> otc_nas_eps_esm:codec(D).
nas_5gs(D) -> otc_nas_5gs:codec(D).
nas_5gs_5gmm(D) -> otc_nas_5gs_5gmm:codec(D).
nas_5gs_5gsm(D) -> otc_nas_5gs_5gsm:codec(D).
gtpv1c(D) -> otc_gtpv1c:codec(D).
gtpv2c(D) -> otc_gtpv2c:codec(D).

%% General functions -----------------------------------------------------------

-type options() :: #{stop_after => protocol()}.
-type data() :: binary() | non_neg_integer().
-type header() :: map().
-type headers() :: [header()].
-type packet() :: headers() | Decoded :: {headers(), data()} | Decapsulated :: [header() | binary()].

-spec otc:decapsulate(protocol(), data()) -> packet().
-spec otc:decapsulate(protocol(), data(), options()) -> packet().
%% decapsulate/1,2 works on valid packets. If the packet is malformed
%% or unsupported, decapsulate/1 will crash.
decapsulate(Proto, Data) ->
    decapsulate(Proto, Data, #{}).

decapsulate(PPI, Data, Opts) when is_integer(PPI) ->
    decapsulate_next(sctp_ppi(PPI), Data, [], Opts);
decapsulate(Proto, Data, Opts) when is_atom(Proto) ->
    decapsulate_next(Proto, Data, [], Opts).
%% decapsulate(Data) when is_binary(Data) ->
%%     decapsulate_next({sctp, Data}, []).

decapsulate_next('$stop', Data, Headers, _Opts) ->
    lists:reverse([Data|Headers]);
decapsulate_next(Proto, Data, Headers, #{stop_after := Proto}) ->
    lists:reverse([Data|Headers]);
decapsulate_next(Proto, Data, Headers, Opts) ->
    {Header, Payload} = ?MODULE:Proto(Data),
    decapsulate_next(next({Proto, Header}, Opts), Payload, [Header|Headers], Opts).

%% -spec otc:decode(data()) ->
%%           {ok, packet()} |
%%           {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
-spec otc:decode(protocol(), data()) ->
          {ok, packet()} |
          {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
-spec otc:decode(protocol(), data(), options()) ->
          {ok, packet()} |
          {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
%% Similar to decapsulate/1 but, on error, returns any part of the
%% packet that has been successfully converted to Erlang term format.
%% decode(Data) when is_binary(Data) ->
%%     decode(sctp, Data).
decode(P, Data) ->
    decode(P, Data, #{}).

decode(PPI, Data, Opts) when is_integer(PPI) ->
    decode(sctp_ppi(PPI), Data, Opts);
decode(Proto, Data, Opts) when is_atom(Proto) ->
    decode_next({Proto, Data}, [], Opts).

decode_next({Proto, Data}, Headers, Opts) ->
    try ?MODULE:Proto(Data) of
        {Header, Payload} when is_map(Header) ->
            case next({Proto, Header}, Opts) of
                '$stop' ->
                    {ok, {lists:reverse([Header#{protocol => Proto}|Headers]), Payload}};
                {ok, Next} ->
                    decode_next({Next, Payload}, [Header#{protocol => Proto}|Headers], Opts)
            end;
        Header when is_map(Header) ->
            {ok, lists:reverse([Header#{protocol => Proto}|Headers])}
    catch E:R:S ->
            ?LOG_ERROR(#{E => R, stack => S}),
            {error, lists:reverse(Headers), Data}
    end.

next({Proto, _Header}, #{stop_after := Proto}) ->
    '$stop';
next({Proto, Header}, _) ->
    next({Proto, Header}).

encode(#{protocol := Proto} = Pdu) ->
    ?MODULE:Proto(Pdu);
encode(Pdus) when is_list(Pdus) ->
    encode({Pdus, <<>>});
encode({Pdus, Payload}) when is_list(Pdus), is_binary(Payload) ->
    lists:foldr(fun (P, Acc) ->
                        encode({P, Acc})
                end, Payload, Pdus);
encode({#{protocol := Proto} = Pdu, Payload}) ->
    ?MODULE:Proto({Pdu, Payload});
encode({Proto, Data}) when is_atom(Proto) ->
    ?MODULE:Proto(Data);
encode(Data) when is_binary(Data) ->
    Data.
