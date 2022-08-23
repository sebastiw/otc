-module(otc_sctp_ppi).
%% https://www.iana.org/assignments/sctp-parameters/sctp-parameters.xhtml#sctp-parameters-25

-export([codec/1]).

-define(PPI_M3UA, 3).

codec(?PPI_M3UA) -> m3ua;

codec(m3ua) -> ?PPI_M3UA.
