-module(otc_sctp_ppi).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1]).

-include("include/sctp_ppi.hrl").

spec() ->
    "https://www.iana.org/assignments/sctp-parameters/sctp-parameters.xhtml#sctp-parameters-25".

next(m3ua) -> {ok, m3ua};
next(m2pa) -> {ok, m2pa};
next(_) -> '$stop'.

codec(PPI) when is_integer(PPI) ->
    decode(PPI);
codec(PPI) when is_atom(PPI) ->
    encode(PPI).

decode(?SCTP_PPI_M3UA) -> m3ua;
decode(?SCTP_PPI_M2PA) -> m2pa.

encode(m3ua) -> ?SCTP_PPI_M3UA;
encode(m2pa) -> ?SCTP_PPI_M2PA.
