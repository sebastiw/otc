-module(otc_map).

-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

spec() ->
    "ITU-T Q.713 (03/2001)".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode(Map).

next(_) -> '$stop'.

decode(Bin) ->
    ok.

encode(Map) ->
    ok.
