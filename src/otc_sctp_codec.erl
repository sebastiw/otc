-module(otc_sctp_codec).

-export([decode/2,
         encode/2
        ]).

decode(Payload, _Options) ->
    Payload.

encode(Data, _Options) ->
    Data.
