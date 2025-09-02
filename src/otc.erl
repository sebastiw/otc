-module(otc).
%% Inspired by [pkt](https://github.com/msantos/pkt) and [ossie](https://github.com/massemanet/ossie)

%% Supported protocol
-export([sctp_ppi/2,
         sctp/2,
         m2pa/2,
         mtp3/2,
         m3ua/2,
         sccp/2,
         sccp_mgmt/2,
         tcap/2,
         map/2,
         nas_eps/2,
         nas_eps_emm/2,
         nas_eps_esm/2,
         nas_5gs/2,
         nas_5gs_5gmm/2,
         nas_5gs_5gsm/2,
         gtpv1c/2,
         gtpv2c/2,
         sgsap/2
        ]).

%% Deprecated
-deprecated([{sctp_ppi, 1, "Use sctp_ppi/2 instead"},
             {sctp, 1, "Use sctp/2 instead"},
             {m2pa, 1, "use m2pa/2 instead"},
             {mtp3, 1, "use mtp3/2 instead"},
             {m3ua, 1, "use m3ua/2 instead"},
             {sccp, 1, "use sccp/2 instead"},
             {sccp_mgmt, 1, "use sccp_mgmt/2 instead"},
             {tcap, 1, "use tcap/2 instead"},
             {map, 1, "use map/2 instead"},
             {nas_eps, 1, "use nas_eps/2 instead"},
             {nas_eps_emm, 1, "use nas_eps_emm/2 instead"},
             {nas_eps_esm, 1, "use nas_eps_esm/2 instead"},
             {nas_5gs, 1, "use nas_5gs/2 instead"},
             {nas_5gs_5gmm, 1, "use nas_5gs_5gmm/2 instead"},
             {nas_5gs_5gsm, 1, "use nas_5gs_5gsm/2 instead"},
             {gtpv1c, 1, "use gtpv1c/2 instead"},
             {gtpv2c, 1, "use gtpv2c/2 instead"},
             {sgsap, 1, "use sgsap/2 instead"}
            ]).
-export([sctp_ppi/1,
         sctp/1,
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
         gtpv2c/1,
         sgsap/1
        ]).

%% General functions
-export([decapsulate/1,
         decapsulate/2,
         decapsulate/3,
         decode/1,
         decode/2,
         decode/3,
         encapsulate/1,
         encode/1,
         encode/2
        ]).

-include_lib("kernel/include/logger.hrl").

%% Supported protocols ---------------------------------------------------------

-type protocol() :: sctp_ppi | sctp | m3ua | m2pa | mtp3 | sccp | sccp_mgmt | tcap | map
                  | nas_eps | nas_eps_emm | nas_eps_esm
                  | gtpv1c | gtpv2c | sgsap.

callback_module(sctp_ppi) -> otc_sctp_ppi;
callback_module(sctp) -> otc_sctp;
callback_module(m2pa) -> otc_m2pa;
callback_module(m3ua) -> otc_m3ua;
callback_module(mtp3) -> otc_mtp3;
callback_module(sccp) -> otc_sccp;
callback_module(sccp_mgmt) -> otc_sccp_mgmt;
callback_module(tcap) -> otc_tcap;
callback_module(map) -> otc_map;
callback_module(nas_eps) -> otc_nas_eps;
callback_module(nas_eps_emm) -> otc_nas_eps_emm;
callback_module(nas_eps_esm) -> otc_nas_eps_esm;
callback_module(nas_5gs) -> otc_nas_5gs;
callback_module(nas_5gs_5gmm) -> otc_nas_5gs_5gmm;
callback_module(nas_5gs_5gsm) -> otc_nas_5gs_5gsm;
callback_module(gtpv1c) -> otc_gtpv1c;
callback_module(gtpv2c) -> otc_gtpv2c;
callback_module(sgsap) -> otc_sgsap.

sctp_ppi(PPI) -> sctp_ppi(PPI, #{}).
sctp(D) -> sctp(D, #{}).
m2pa(D) -> m2pa(D, #{}).
mtp3(D) -> mtp3(D, #{}).
m3ua(D) -> m3ua(D, #{}).
sccp(D) -> sccp(D, #{}).
sccp_mgmt(D) -> sccp_mgmt(D, #{}).
tcap(D) -> tcap(D, #{}).
map(D) -> map(D, #{}).
nas_eps(D) -> nas_eps(D, #{}).
nas_eps_emm(D) -> nas_eps_emm(D, #{}).
nas_eps_esm(D) -> nas_eps_esm(D, #{}).
nas_5gs(D) -> nas_5gs(D, #{}).
nas_5gs_5gmm(D) -> nas_5gs_5gmm(D, #{}).
nas_5gs_5gsm(D) -> nas_5gs_5gsm(D, #{}).
gtpv1c(D) -> gtpv1c(D, #{}).
gtpv2c(D) -> gtpv2c(D, #{}).
sgsap(D) -> sgsap(D, #{}).

sctp_ppi(PPI, Opts) -> otc_sctp_ppi:codec(PPI, Opts).
sctp(D, Opts) -> otc_sctp:codec(D, Opts).
m2pa(D, Opts) -> otc_m2pa:codec(D, Opts).
mtp3(D, Opts) -> otc_mtp3:codec(D, Opts).
m3ua(D, Opts) -> otc_m3ua:codec(D, Opts).
sccp(D, Opts) -> otc_sccp:codec(D, Opts).
sccp_mgmt(D, Opts) -> otc_sccp_mgmt:codec(D, Opts).
tcap(D, Opts) -> otc_tcap:codec(D, Opts).
map(D, Opts) -> otc_map:codec(D, Opts).
nas_eps(D, Opts) -> otc_nas_eps:codec(D, Opts).
nas_eps_emm(D, Opts) -> otc_nas_eps_emm:codec(D, Opts).
nas_eps_esm(D, Opts) -> otc_nas_eps_esm:codec(D, Opts).
nas_5gs(D, Opts) -> otc_nas_5gs:codec(D, Opts).
nas_5gs_5gmm(D, Opts) -> otc_nas_5gs_5gmm:codec(D, Opts).
nas_5gs_5gsm(D, Opts) -> otc_nas_5gs_5gsm:codec(D, Opts).
gtpv1c(D, Opts) -> otc_gtpv1c:codec(D, Opts).
gtpv2c(D, Opts) -> otc_gtpv2c:codec(D, Opts).
sgsap(D, Opts) -> otc_sgsap:codec(D, Opts).

%% General functions -----------------------------------------------------------

-type options() :: #{stop_after => protocol(),
                     %% protocol specific options
                     protocol() => map()}.
-type data() :: binary() | non_neg_integer().
-type header() :: map().
-type headers() :: [header()].
-type packet() :: headers() | Decoded :: {headers(), data()} | Decapsulated :: [header() | binary()].
-type payload() :: header() | headers() | {header() | headers(), data()} | {protocol(), header() | {header(), data()}} | data().

-spec otc:decapsulate(data()) -> packet().
-spec otc:decapsulate(protocol(), data()) -> packet().
-spec otc:decapsulate(protocol(), data(), options()) -> packet().
%% decapsulate/1,2,3 works on valid packets. If the packet is malformed
%% or unsupported, decapsulate/1,2,3 will crash.
decapsulate(Data) when is_binary(Data) ->
    decapsulate(sctp, Data).

decapsulate(Proto, Data) ->
    decapsulate(Proto, Data, #{}).

decapsulate(PPI, Data, Opts) when is_integer(PPI) ->
    decapsulate_next(sctp_ppi(PPI), Data, [], Opts);
decapsulate(Proto, Data, Opts) when is_atom(Proto) ->
    decapsulate_next(Proto, Data, [], Opts).

decapsulate_next('$stop', Data, Headers, _Opts) ->
    lists:reverse([Data|Headers]);
decapsulate_next(Proto, Data, Headers, #{stop_after := Proto}) ->
    lists:reverse([Data|Headers]);
decapsulate_next(Proto, Data, Headers, Opts) ->
    case ?MODULE:Proto(Data, options(Proto, Opts)) of
        {Header, Payload} ->
            decapsulate_next(next({Proto, Header}, Opts), Payload, [Header|Headers], Opts);
        Header when is_map(Header) ->
            lists:reverse([Header#{protocol => Proto}|Headers])
    end.

-spec otc:decode(data()) ->
          {ok, packet()} |
          {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
-spec otc:decode(protocol(), data()) ->
          {ok, packet()} |
          {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
-spec otc:decode(protocol(), data(), options()) ->
          {ok, packet()} |
          {error, SoFar :: headers(), {FailedProto :: protocol(), data()}}.
%% Similar to decapsulate/1,2,3 but, on error, returns any part of the
%% packet that has been successfully converted to Erlang term format.
decode(Data) when is_binary(Data) ->
    decode(sctp, Data).

decode(P, Data) ->
    decode(P, Data, #{}).

decode(PPI, Data, Opts) when is_integer(PPI) ->
    decode(sctp_ppi(PPI), Data, Opts);
decode(Proto, Data, Opts) when is_atom(Proto) ->
    decode_next({Proto, Data}, [], Opts).

decode_next({Proto, Data}, Headers, Opts) ->
    try ?MODULE:Proto(Data, options(Proto, Opts)) of
        {Header, Payload} when is_map(Header) ->
            NewHeader = Header#{protocol => Proto},
            case next({Proto, Header}, Opts) of
                '$stop' ->
                    {ok, {lists:reverse([NewHeader|Headers]), Payload}};
                {ok, Next} ->
                    next_opts({Next, NewHeader}, Opts),
                    decode_next({Next, Payload}, [NewHeader|Headers], Opts)
            end;
        Header when is_map(Header) ->
            NewHeader = Header#{protocol => Proto},
            {ok, lists:reverse([NewHeader|Headers])}
    catch E:R:S ->
            ?LOG_ERROR(#{E => R, stack => S}),
            {error, lists:reverse(Headers), Data}
    end.

next({Proto, _Header}, #{stop_after := Proto}) ->
    '$stop';
next({Proto, Header}, _) ->
    Module = callback_module(Proto),
    Module:next(V).

next_opts({Proto, PrevHeader}, Opts) ->
    UserOpts = options(Proto, Opts),
    propagated_options({Proto, PrevHeader}, UserOpts).

propagated_options({Proto, PrevHeader}, UserOpts) ->
    Module = callback_module(Proto),
    case erlang:function_exported(Module, propagated_options, 2) of
        true ->
            Module:propagated_options(PrevHeader, UserOpts);
        false ->
            UserOpts
    end.

-spec options(protocol(), options()) -> options().
%%% Inherits options for sub protocols/modules if they are not
%%% explicitly specified by the user.
%%% e.g. lets the sccp_mgmt layer inherit the options from sccp layer.
options(Proto, Opts) when is_atom(Proto), is_map(Opts), is_map_key(Proto, Opts) ->
    maps:get(Proto, Opts);
options(sccp_mgmt, Opts) ->
    options(sccp, Opts);
options(nas_eps_emm, Opts) ->
    options(nas_eps, Opts);
options(nas_eps_esm, Opts) ->
    options(nas_eps, Opts);
options(nas_5gs_5gmm, Opts) ->
    options(nas_5gs, Opts);
options(nas_5gs_5gsm, Opts) ->
    options(nas_5gs, Opts);
options(Proto, Opts) when is_atom(Proto), is_map(Opts) ->
    %% No options found
    #{}.

-spec otc:encapsulate(payload()) -> data().
-spec otc:encapsulate(payload(), options()) -> data().
encapsulate(Pdu) ->
    encapsulate(Pdu, #{}).

encapsulate(#{protocol := Proto} = Pdu, Opts) ->
    ?MODULE:Proto(Pdu, options(Proto, Opts));
encapsulate(Pdus, Opts) when is_list(Pdus) ->
    encapsulate({Pdus, <<>>}, Opts);
encapsulate({Pdus, Payload}, Opts) when is_list(Pdus), is_binary(Payload) ->
    lists:foldr(fun (P, Acc) ->
                        encapsulate({P, Acc}, Opts)
                end, Payload, Pdus);
encapsulate({#{protocol := Proto} = Pdu, Payload}, Opts) ->
    ?MODULE:Proto({Pdu, Payload}, options(Proto, Opts));
encapsulate({Proto, Data}, Opts) when is_atom(Proto) ->
    ?MODULE:Proto(Data, options(Proto, Opts));
encapsulate(Data, _Opts) when is_binary(Data) ->
    Data.

-spec otc:encode(payload()) -> {ok, data()} | {error, term()}.
-spec otc:encode(payload(), options()) -> {ok, data()} | {error, term()}.
encode(Pdu) ->
    encode(Pdu, #{}).

encode(#{protocol := Proto} = Pdu, Opts) ->
    enc_safe(Proto, Pdu, Opts);
encode(Pdus, Opts) when is_list(Pdus) ->
    encode({Pdus, <<>>}, Opts);
encode({Pdus, Payload}, Opts) when is_list(Pdus), is_binary(Payload) ->
    lists:foldr(fun (P, {ok, Acc}) ->
                        encode({P, Acc}, Opts);
                    (_, {error, _} = Err) ->
                        Err
                end, {ok, Payload}, Pdus);
encode({#{protocol := Proto} = Pdu, Payload}, Opts) ->
    enc_safe(Proto, {Pdu, Payload}, Opts);
encode({Proto, Data}, Opts) when is_atom(Proto) ->
    enc_safe(Proto, Data, Opts);
encode(Data, _Opts) when is_binary(Data) ->
    {ok, Data}.

enc_safe(Proto, Pdu, Opts) ->
    try ?MODULE:Proto(Pdu, options(Proto, Opts)) of
        B when is_binary(B) ->
            {ok, B}
    catch E:R:S ->
            ?LOG_ERROR(#{E => R, stack => S}),
            {error, {Proto, Pdu}}
    end.
