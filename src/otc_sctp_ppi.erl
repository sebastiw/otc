-module(otc_sctp_ppi).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         encode/1]).

-include("include/sctp_ppi.hrl").

spec() ->
    "https://www.iana.org/assignments/sctp-parameters/sctp-parameters.xhtml#sctp-parameters-25".

next(m3ua) -> {ok, m3ua};
next(m2pa) -> {ok, m2pa};
next(_) -> '$stop'.

codec(PPI, _Opts) when is_integer(PPI) ->
    decode(PPI);
codec(PPI, _Opts) when is_atom(PPI) ->
    encode(PPI).

decode(?SCTP_PPI_M2UA) -> m2ua;
decode(?SCTP_PPI_M3UA) -> m3ua;
decode(?SCTP_PPI_M2PA) -> m2pa;
decode(?SCTP_PPI_S1AP) -> s1ap;
decode(?SCTP_PPI_M2AP) -> m2ap;
decode(?SCTP_PPI_M3AP) -> m3ap;
decode(?SCTP_PPI_SSH) -> ssh;
decode(?SCTP_PPI_DIAMETER) -> diameter;
decode(?SCTP_PPI_DIAMETER_DTLS) -> diameter_dtls;
decode(?SCTP_PPI_NGAP) -> ngap;
decode(?SCTP_PPI_NGAP_DTLS) -> ngap_dtls.

encode(m2ua) -> ?SCTP_PPI_M2UA;
encode(m3ua) -> ?SCTP_PPI_M3UA;
encode(m2pa) -> ?SCTP_PPI_M2PA;
encode(s1ap) -> ?SCTP_PPI_S1AP;
encode(m2ap) -> ?SCTP_PPI_M2AP;
encode(m3ap) -> ?SCTP_PPI_M3AP;
encode(ssh) -> ?SCTP_PPI_SSH;
encode(diameter) -> ?SCTP_PPI_DIAMETER;
encode(diameter_dtls) -> ?SCTP_PPI_DIAMETER_DTLS;
encode(ngap) -> ?SCTP_PPI_NGAP;
encode(ngap_dtls) -> ?SCTP_PPI_NGAP_DTLS.
