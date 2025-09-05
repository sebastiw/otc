-module(otc_smpp).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         decode/2,
         encode/1,
         encode/2
        ]).

-include("include/smpp.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "SMS Forum SMPP V5.0".

codec(Bin, Opts) when is_binary(Bin) ->
    decode(Bin, Opts);
codec(Map, Opts) when is_map(Map) ->
    encode({Map, <<>>}, Opts);
codec({Map, PDU}, Opts) ->
    encode({Map, PDU}, Opts).

next(_) ->
    '$stop'.

decode(Bin) ->
    decode(Bin, #{}).

decode(Bin, _Opts) ->
    {Header, Rest} = parse_header(Bin),
    Msg = decode_msg(Header, Rest),
    maps:merge(Header, Msg).

encode(Pdu) ->
    encode(Pdu, #{}).

encode({Map, PDU}, Opts) ->
    M = encode(Map, Opts),
    <<M/binary, PDU/binary>>;
encode(Map, _Opts) ->
    Msg = encode_msg(Map),
    Header = compose_header(Map, Msg),
    <<Header/binary, Msg/binary>>.

parse_header(Bin) ->
    <<CL:32, CID:32, CS:32, SN:32, Rest:(CL-16)/binary>> = Bin,
    Header = #{command => parse_command_id(CID),
               command_status => parse_command_status(CS),
               sequence_number => SN},
    {Header, Rest}.

compose_header(Map, Msg) ->
    #{command := Command,
      command_status := CommandStatus,
      sequence_number := SN} = Map,
    CL = byte_size(Msg) + 16,
    CID = compose_command_id(Command),
    CS = compose_command_status(CommandStatus),
    <<CL:32, CID:32, CS:32, SN:32>>.

parse_command_id(?SMPP_CMD_ID_BIND_RECEIVER) -> bind_receiver;
parse_command_id(?SMPP_CMD_ID_BIND_TRANSMITTER) -> bind_transmitter;
parse_command_id(?SMPP_CMD_ID_QUERY_SM) -> query_sm;
parse_command_id(?SMPP_CMD_ID_SUBMIT_SM) -> submit_sm;
parse_command_id(?SMPP_CMD_ID_DELIVER_SM) -> deliver_sm;
parse_command_id(?SMPP_CMD_ID_UNBIND) -> unbind;
parse_command_id(?SMPP_CMD_ID_REPLACE_SM) -> replace_sm;
parse_command_id(?SMPP_CMD_ID_CANCEL_SM) -> cancel_sm;
parse_command_id(?SMPP_CMD_ID_BIND_TRANSCEIVER) -> bind_transceiver;
parse_command_id(?SMPP_CMD_ID_OUTBIND) -> outbind;
parse_command_id(?SMPP_CMD_ID_ENQUIRE_LINK) -> enquire_link;
parse_command_id(?SMPP_CMD_ID_SUBMIT_MULTI) -> submit_multi;
parse_command_id(?SMPP_CMD_ID_ALERT_NOTIFICATION) -> alert_notification;
parse_command_id(?SMPP_CMD_ID_DATA_SM) -> data_sm;
parse_command_id(?SMPP_CMD_ID_BROADCAST_SM) -> broadcast_sm;
parse_command_id(?SMPP_CMD_ID_QUERY_BROADCAST_SM) -> query_broadcast_sm;
parse_command_id(?SMPP_CMD_ID_CANCEL_BROADCAST_SM) -> cancel_broadcast_sm;
parse_command_id(?SMPP_CMD_ID_GENERIC_NACK) -> generic_nack;
parse_command_id(?SMPP_CMD_ID_BIND_RECEIVER_RESP) -> bind_receiver_resp;
parse_command_id(?SMPP_CMD_ID_BIND_TRANSMITTER_RESP) -> bind_transmitter_resp;
parse_command_id(?SMPP_CMD_ID_QUERY_SM_RESP) -> query_sm_resp;
parse_command_id(?SMPP_CMD_ID_SUBMIT_SM_RESP) -> submit_sm_resp;
parse_command_id(?SMPP_CMD_ID_DELIVER_SM_RESP) -> deliver_sm_resp;
parse_command_id(?SMPP_CMD_ID_UNBIND_RESP) -> unbind_resp;
parse_command_id(?SMPP_CMD_ID_REPLACE_SM_RESP) -> replace_sm_resp;
parse_command_id(?SMPP_CMD_ID_CANCEL_SM_RESP) -> cancel_sm_resp;
parse_command_id(?SMPP_CMD_ID_BIND_TRANSCEIVER_RESP) -> bind_transceiver_resp;
parse_command_id(?SMPP_CMD_ID_ENQUIRE_LINK_RESP) -> enquire_link_resp;
parse_command_id(?SMPP_CMD_ID_SUBMIT_MULTI_RESP) -> submit_multi_resp;
parse_command_id(?SMPP_CMD_ID_DATA_SM_RESP) -> data_sm_resp;
parse_command_id(?SMPP_CMD_ID_BROADCAST_SM_RESP) -> broadcast_sm_resp;
parse_command_id(?SMPP_CMD_ID_QUERY_BROADCAST_SM_RESP) -> query_broadcast_sm_resp;
parse_command_id(?SMPP_CMD_ID_CANCEL_BROADCAST_SM_RESP) -> cancel_broadcast_sm_resp.

compose_command_id(bind_receiver) -> ?SMPP_CMD_ID_BIND_RECEIVER;
compose_command_id(bind_transmitter) -> ?SMPP_CMD_ID_BIND_TRANSMITTER;
compose_command_id(query_sm) -> ?SMPP_CMD_ID_QUERY_SM;
compose_command_id(submit_sm) -> ?SMPP_CMD_ID_SUBMIT_SM;
compose_command_id(deliver_sm) -> ?SMPP_CMD_ID_DELIVER_SM;
compose_command_id(unbind) -> ?SMPP_CMD_ID_UNBIND;
compose_command_id(replace_sm) -> ?SMPP_CMD_ID_REPLACE_SM;
compose_command_id(cancel_sm) -> ?SMPP_CMD_ID_CANCEL_SM;
compose_command_id(bind_transceiver) -> ?SMPP_CMD_ID_BIND_TRANSCEIVER;
compose_command_id(outbind) -> ?SMPP_CMD_ID_OUTBIND;
compose_command_id(enquire_link) -> ?SMPP_CMD_ID_ENQUIRE_LINK;
compose_command_id(submit_multi) -> ?SMPP_CMD_ID_SUBMIT_MULTI;
compose_command_id(alert_notification) -> ?SMPP_CMD_ID_ALERT_NOTIFICATION;
compose_command_id(data_sm) -> ?SMPP_CMD_ID_DATA_SM;
compose_command_id(broadcast_sm) -> ?SMPP_CMD_ID_BROADCAST_SM;
compose_command_id(query_broadcast_sm) -> ?SMPP_CMD_ID_QUERY_BROADCAST_SM;
compose_command_id(cancel_broadcast_sm) -> ?SMPP_CMD_ID_CANCEL_BROADCAST_SM;
compose_command_id(generic_nack) -> ?SMPP_CMD_ID_GENERIC_NACK;
compose_command_id(bind_receiver_resp) -> ?SMPP_CMD_ID_BIND_RECEIVER_RESP;
compose_command_id(bind_transmitter_resp) -> ?SMPP_CMD_ID_BIND_TRANSMITTER_RESP;
compose_command_id(query_sm_resp) -> ?SMPP_CMD_ID_QUERY_SM_RESP;
compose_command_id(submit_sm_resp) -> ?SMPP_CMD_ID_SUBMIT_SM_RESP;
compose_command_id(deliver_sm_resp) -> ?SMPP_CMD_ID_DELIVER_SM_RESP;
compose_command_id(unbind_resp) -> ?SMPP_CMD_ID_UNBIND_RESP;
compose_command_id(replace_sm_resp) -> ?SMPP_CMD_ID_REPLACE_SM_RESP;
compose_command_id(cancel_sm_resp) -> ?SMPP_CMD_ID_CANCEL_SM_RESP;
compose_command_id(bind_transceiver_resp) -> ?SMPP_CMD_ID_BIND_TRANSCEIVER_RESP;
compose_command_id(enquire_link_resp) -> ?SMPP_CMD_ID_ENQUIRE_LINK_RESP;
compose_command_id(submit_multi_resp) -> ?SMPP_CMD_ID_SUBMIT_MULTI_RESP;
compose_command_id(data_sm_resp) -> ?SMPP_CMD_ID_DATA_SM_RESP;
compose_command_id(broadcast_sm_resp) -> ?SMPP_CMD_ID_BROADCAST_SM_RESP;
compose_command_id(query_broadcast_sm_resp) -> ?SMPP_CMD_ID_QUERY_BROADCAST_SM_RESP;
compose_command_id(cancel_broadcast_sm_resp) -> ?SMPP_CMD_ID_CANCEL_BROADCAST_SM_RESP.

parse_command_status(?SMPP_CMD_STATUS_ESME_ROK) ->
    esme_rok;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBNDSTS) ->
    esme_rinvbndsts;
parse_command_status(?SMPP_CMD_STATUS_ESME_RALYBND) ->
    esme_ralybnd;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVPRTFLG) ->
    esme_rinvprtflg;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVREGDLVFLG) ->
    esme_rinvregdlvflg;
parse_command_status(?SMPP_CMD_STATUS_ESME_RSYSERR) ->
    esme_rsyserr;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSRCADR) ->
    esme_rinvsrcadr;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDSTADR) ->
    esme_rinvdstadr;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVMSGID) ->
    esme_rinvmsgid;
parse_command_status(?SMPP_CMD_STATUS_ESME_RBINDFAIL) ->
    esme_rbindfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVPASWD) ->
    esme_rinvpaswd;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSYSID) ->
    esme_rinvsysid;
parse_command_status(?SMPP_CMD_STATUS_ESME_RCANCELFAIL) ->
    esme_rcancelfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RREPLACEFAIL) ->
    esme_rreplacefail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RMSGQFUL) ->
    esme_rmsgqful;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSERTYP) ->
    esme_rinvsertyp;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVNUMDESTS) ->
    esme_rinvnumdests;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDLNAME) ->
    esme_rinvdlname;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDESTFLAG) ->
    esme_rinvdestflag;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSUBREP) ->
    esme_rinvsubrep;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVESMCLASS) ->
    esme_rinvesmclass;
parse_command_status(?SMPP_CMD_STATUS_ESME_RCNTSUBDL) ->
    esme_rcntsubdl;
parse_command_status(?SMPP_CMD_STATUS_ESME_RSUBMITFAIL) ->
    esme_rsubmitfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSRCTON) ->
    esme_rinvsrcton;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSRCNPI) ->
    esme_rinvsrcnpi;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDSTTON) ->
    esme_rinvdstton;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDSTNPI) ->
    esme_rinvdstnpi;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSYSTYP) ->
    esme_rinvsystyp;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVREPFLAG) ->
    esme_rinvrepflag;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVNUMMSGS) ->
    esme_rinvnummsgs;
parse_command_status(?SMPP_CMD_STATUS_ESME_RTHROTTLED) ->
    esme_rthrottled;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSCHED) ->
    esme_rinvsched;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVEXPIRY) ->
    esme_rinvexpiry;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDFTMSGID) ->
    esme_rinvdftmsgid;
parse_command_status(?SMPP_CMD_STATUS_ESME_RX_T_APPN) ->
    esme_rx_t_appn;
parse_command_status(?SMPP_CMD_STATUS_ESME_RX_P_APPN) ->
    esme_rx_p_appn;
parse_command_status(?SMPP_CMD_STATUS_ESME_RX_R_APPN) ->
    esme_rx_r_appn;
parse_command_status(?SMPP_CMD_STATUS_ESME_RQUERYFAIL) ->
    esme_rqueryfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVTLVSTREAM) ->
    esme_rinvtlvstream;
parse_command_status(?SMPP_CMD_STATUS_ESME_RTLVNOTALLWD) ->
    esme_rtlvnotallwd;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVTLVLEN) ->
    esme_rinvtlvlen;
parse_command_status(?SMPP_CMD_STATUS_ESME_RMISSINGTLV) ->
    esme_rmissingtlv;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVTLVVAL) ->
    esme_rinvtlvval;
parse_command_status(?SMPP_CMD_STATUS_ESME_RDELIVERYFAILURE) ->
    esme_rdeliveryfailure;
parse_command_status(?SMPP_CMD_STATUS_ESME_RUNKNOWNERR) ->
    esme_runknownerr;
parse_command_status(?SMPP_CMD_STATUS_ESME_RSERTYPUNAUTH) ->
    esme_rsertypunauth;
parse_command_status(?SMPP_CMD_STATUS_ESME_RPROHIBITED) ->
    esme_rprohibited;
parse_command_status(?SMPP_CMD_STATUS_ESME_RSERTYPUNAVAIL) ->
    esme_rsertypunavail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RSERTYPDENIED) ->
    esme_rsertypdenied;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDCS) ->
    esme_rinvdcs;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVSRCADDRSUBUNIT) ->
    esme_rinvsrcaddrsubunit;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVDSTADDRSUBUNIT) ->
    esme_rinvdstaddrsubunit;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTFREQINT) ->
    esme_rinvbcastfreqint;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTALIAS_NAME) ->
    esme_rinvbcastalias_name;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTAREAFMT) ->
    esme_rinvbcastareafmt;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVNUMBCAST_AREAS) ->
    esme_rinvnumbcast_areas;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTCNTTYPE) ->
    esme_rinvbcastcnttype;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTMSGCLASS) ->
    esme_rinvbcastmsgclass;
parse_command_status(?SMPP_CMD_STATUS_ESME_RBCASTFAIL) ->
    esme_rbcastfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RBCASTQUERYFAIL) ->
    esme_rbcastqueryfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RBCASTCANCELFAIL) ->
    esme_rbcastcancelfail;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCAST_REP) ->
    esme_rinvbcast_rep;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTSRVGRP) ->
    esme_rinvbcastsrvgrp;
parse_command_status(?SMPP_CMD_STATUS_ESME_RINVBCASTCHANIND) ->
    esme_rinvbcastchanind;
parse_command_status(V) when V >= 16#00000400; V =< 16#000004FF ->
    {vendor_specific, V}.

compose_command_status(esme_rok) ->
    ?SMPP_CMD_STATUS_ESME_ROK;
compose_command_status(esme_rinvbndsts) ->
    ?SMPP_CMD_STATUS_ESME_RINVBNDSTS;
compose_command_status(esme_ralybnd) ->
    ?SMPP_CMD_STATUS_ESME_RALYBND;
compose_command_status(esme_rinvprtflg) ->
    ?SMPP_CMD_STATUS_ESME_RINVPRTFLG;
compose_command_status(esme_rinvregdlvflg) ->
    ?SMPP_CMD_STATUS_ESME_RINVREGDLVFLG;
compose_command_status(esme_rsyserr) ->
    ?SMPP_CMD_STATUS_ESME_RSYSERR;
compose_command_status(esme_rinvsrcadr) ->
    ?SMPP_CMD_STATUS_ESME_RINVSRCADR;
compose_command_status(esme_rinvdstadr) ->
    ?SMPP_CMD_STATUS_ESME_RINVDSTADR;
compose_command_status(esme_rinvmsgid) ->
    ?SMPP_CMD_STATUS_ESME_RINVMSGID;
compose_command_status(esme_rbindfail) ->
    ?SMPP_CMD_STATUS_ESME_RBINDFAIL;
compose_command_status(esme_rinvpaswd) ->
    ?SMPP_CMD_STATUS_ESME_RINVPASWD;
compose_command_status(esme_rinvsysid) ->
    ?SMPP_CMD_STATUS_ESME_RINVSYSID;
compose_command_status(esme_rcancelfail) ->
    ?SMPP_CMD_STATUS_ESME_RCANCELFAIL;
compose_command_status(esme_rreplacefail) ->
    ?SMPP_CMD_STATUS_ESME_RREPLACEFAIL;
compose_command_status(esme_rmsgqful) ->
    ?SMPP_CMD_STATUS_ESME_RMSGQFUL;
compose_command_status(esme_rinvsertyp) ->
    ?SMPP_CMD_STATUS_ESME_RINVSERTYP;
compose_command_status(esme_rinvnumdests) ->
    ?SMPP_CMD_STATUS_ESME_RINVNUMDESTS;
compose_command_status(esme_rinvdlname) ->
    ?SMPP_CMD_STATUS_ESME_RINVDLNAME;
compose_command_status(esme_rinvdestflag) ->
    ?SMPP_CMD_STATUS_ESME_RINVDESTFLAG;
compose_command_status(esme_rinvsubrep) ->
    ?SMPP_CMD_STATUS_ESME_RINVSUBREP;
compose_command_status(esme_rinvesmclass) ->
    ?SMPP_CMD_STATUS_ESME_RINVESMCLASS;
compose_command_status(esme_rcntsubdl) ->
    ?SMPP_CMD_STATUS_ESME_RCNTSUBDL;
compose_command_status(esme_rsubmitfail) ->
    ?SMPP_CMD_STATUS_ESME_RSUBMITFAIL;
compose_command_status(esme_rinvsrcton) ->
    ?SMPP_CMD_STATUS_ESME_RINVSRCTON;
compose_command_status(esme_rinvsrcnpi) ->
    ?SMPP_CMD_STATUS_ESME_RINVSRCNPI;
compose_command_status(esme_rinvdstton) ->
    ?SMPP_CMD_STATUS_ESME_RINVDSTTON;
compose_command_status(esme_rinvdstnpi) ->
    ?SMPP_CMD_STATUS_ESME_RINVDSTNPI;
compose_command_status(esme_rinvsystyp) ->
    ?SMPP_CMD_STATUS_ESME_RINVSYSTYP;
compose_command_status(esme_rinvrepflag) ->
    ?SMPP_CMD_STATUS_ESME_RINVREPFLAG;
compose_command_status(esme_rinvnummsgs) ->
    ?SMPP_CMD_STATUS_ESME_RINVNUMMSGS;
compose_command_status(esme_rthrottled) ->
    ?SMPP_CMD_STATUS_ESME_RTHROTTLED;
compose_command_status(esme_rinvsched) ->
    ?SMPP_CMD_STATUS_ESME_RINVSCHED;
compose_command_status(esme_rinvexpiry) ->
    ?SMPP_CMD_STATUS_ESME_RINVEXPIRY;
compose_command_status(esme_rinvdftmsgid) ->
    ?SMPP_CMD_STATUS_ESME_RINVDFTMSGID;
compose_command_status(esme_rx_t_appn) ->
    ?SMPP_CMD_STATUS_ESME_RX_T_APPN;
compose_command_status(esme_rx_p_appn) ->
    ?SMPP_CMD_STATUS_ESME_RX_P_APPN;
compose_command_status(esme_rx_r_appn) ->
    ?SMPP_CMD_STATUS_ESME_RX_R_APPN;
compose_command_status(esme_rqueryfail) ->
    ?SMPP_CMD_STATUS_ESME_RQUERYFAIL;
compose_command_status(esme_rinvtlvstream) ->
    ?SMPP_CMD_STATUS_ESME_RINVTLVSTREAM;
compose_command_status(esme_rtlvnotallwd) ->
    ?SMPP_CMD_STATUS_ESME_RTLVNOTALLWD;
compose_command_status(esme_rinvtlvlen) ->
    ?SMPP_CMD_STATUS_ESME_RINVTLVLEN;
compose_command_status(esme_rmissingtlv) ->
    ?SMPP_CMD_STATUS_ESME_RMISSINGTLV;
compose_command_status(esme_rinvtlvval) ->
    ?SMPP_CMD_STATUS_ESME_RINVTLVVAL;
compose_command_status(esme_rdeliveryfailure) ->
    ?SMPP_CMD_STATUS_ESME_RDELIVERYFAILURE;
compose_command_status(esme_runknownerr) ->
    ?SMPP_CMD_STATUS_ESME_RUNKNOWNERR;
compose_command_status(esme_rsertypunauth) ->
    ?SMPP_CMD_STATUS_ESME_RSERTYPUNAUTH;
compose_command_status(esme_rprohibited) ->
    ?SMPP_CMD_STATUS_ESME_RPROHIBITED;
compose_command_status(esme_rsertypunavail) ->
    ?SMPP_CMD_STATUS_ESME_RSERTYPUNAVAIL;
compose_command_status(esme_rsertypdenied) ->
    ?SMPP_CMD_STATUS_ESME_RSERTYPDENIED;
compose_command_status(esme_rinvdcs) ->
    ?SMPP_CMD_STATUS_ESME_RINVDCS;
compose_command_status(esme_rinvsrcaddrsubunit) ->
    ?SMPP_CMD_STATUS_ESME_RINVSRCADDRSUBUNIT;
compose_command_status(esme_rinvdstaddrsubunit) ->
    ?SMPP_CMD_STATUS_ESME_RINVDSTADDRSUBUNIT;
compose_command_status(esme_rinvbcastfreqint) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTFREQINT;
compose_command_status(esme_rinvbcastalias_name) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTALIAS_NAME;
compose_command_status(esme_rinvbcastareafmt) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTAREAFMT;
compose_command_status(esme_rinvnumbcast_areas) ->
    ?SMPP_CMD_STATUS_ESME_RINVNUMBCAST_AREAS;
compose_command_status(esme_rinvbcastcnttype) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTCNTTYPE;
compose_command_status(esme_rinvbcastmsgclass) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTMSGCLASS;
compose_command_status(esme_rbcastfail) ->
    ?SMPP_CMD_STATUS_ESME_RBCASTFAIL;
compose_command_status(esme_rbcastqueryfail) ->
    ?SMPP_CMD_STATUS_ESME_RBCASTQUERYFAIL;
compose_command_status(esme_rbcastcancelfail) ->
    ?SMPP_CMD_STATUS_ESME_RBCASTCANCELFAIL;
compose_command_status(esme_rinvbcast_rep) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCAST_REP;
compose_command_status(esme_rinvbcastsrvgrp) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTSRVGRP;
compose_command_status(esme_rinvbcastchanind) ->
    ?SMPP_CMD_STATUS_ESME_RINVBCASTCHANIND;
compose_command_status({vendor_specific, V}) ->
    V.

decode_msg(#{command := bind_transmitter}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := bind_transmitter_resp}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := bind_receiver}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := bind_receiver_resp}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := bind_transceiver}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := bind_transceiver_resp}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := outbind}, Bin) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := unbind}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := unbind_resp}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := enquire_link}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := enquire_link_resp}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := alert_notification}, Bin) ->
    AllowedParameters = [{source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {esme_addr_ton, 1, integer},
                         {esme_addr_npi, 1, integer},
                         {esme_addr, {0, 65}, cstring},
                         {ms_availability_status, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := generic_nack}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := submit_sm}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := submit_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := data_sm}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {registered_delivery, 1, integer},
                         {data_coding, 1, integer},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := data_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := submit_multi}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {number_of_dests, 1, integer},
                         {dest_address, {0, 24}, composite},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := submit_multi_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {no_unsuccess, 1, integer},
                         {unsuccess_sme, {0, 27}, composite},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := deliver_sm}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_delivery_request_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := deliver_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_delivery_response_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := broadcast_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {broadcast_response_optional_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := cancel_sm}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := cancel_sm_resp}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := query_sm}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := query_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {final_date, {1,17}, cstring},
                         {message_state, 1, integer},
                         {error_code, 1, integer}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := replace_sm}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_replacement_request_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := replace_sm_resp}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := query_broadcast_sm}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {query_broadcast_request_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := query_broadcast_sm_resp}, Bin) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_state, 1, integer},
                         {broadcast_area_identifier, undefined, tlv},
                         {broadcast_area_success, undefined, tlv},
                         {query_broadcast_response_tlvs, undefined, tlv}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := cancel_broadcast_sm}, Bin) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring}
                        ],
    decode_parameters(AllowedParameters, Bin);
decode_msg(#{command := cancel_broadcast_sm_resp}, Bin) ->
    AllowedParameters = [],
    decode_parameters(AllowedParameters, Bin).

encode_msg(#{command := bind_transmitter} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := bind_transmitter_resp} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := bind_receiver} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := bind_receiver_resp} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := bind_transceiver} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring},
                         {system_type, {0, 13}, cstring},
                         {interface_version, 1, integer},
                         {addr_ton, 1, integer},
                         {addr_npi, 1, integer},
                         {address_range, {0, 41}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := bind_transceiver_resp} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {sc_interface_version, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := outbind} = Map) ->
    AllowedParameters = [{system_id, {0, 16}, cstring},
                         {password, {0, 9}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := unbind} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := unbind_resp} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := enquire_link} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := enquire_link_resp} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := alert_notification} = Map) ->
    AllowedParameters = [{source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {esme_addr_ton, 1, integer},
                         {esme_addr_npi, 1, integer},
                         {esme_addr, {0, 65}, cstring},
                         {ms_availability_status, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := generic_nack} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := submit_sm} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := submit_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := data_sm} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {registered_delivery, 1, integer},
                         {data_coding, 1, integer},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := data_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := submit_multi} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {number_of_dests, 1, integer},
                         {dest_address, {0, 24}, composite},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_submission_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := submit_multi_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {no_unsuccess, 1, integer},
                         {unsuccess_sme, {0, 27}, composite},
                         {message_submission_response_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := deliver_sm} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring},
                         {esm_class, 1, integer},
                         {protocol_id, 1, integer},
                         {priority_flag, 1, integer},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {replace_if_present_flag, 1, integer},
                         {data_coding, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_delivery_request_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := deliver_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_delivery_response_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := broadcast_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {broadcast_response_optional_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := cancel_sm} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {dest_addr_ton, 1, integer},
                         {dest_addr_npi, 1, integer},
                         {destination_addr, {0, 21}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := cancel_sm_resp} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := query_sm} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := query_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {final_date, {1,17}, cstring},
                         {message_state, 1, integer},
                         {error_code, 1, integer}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := replace_sm} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {scheduled_delivery_time, {1,17}, cstring},
                         {validity_period, {1,17}, cstring},
                         {registered_delivery, 1, integer},
                         {sm_default_msg_id, 1, integer},
                         {short_message, {0, 255}, string},
                         {message_replacement_request_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := replace_sm_resp} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := query_broadcast_sm} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring},
                         {query_broadcast_request_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := query_broadcast_sm_resp} = Map) ->
    AllowedParameters = [{message_id, {0, 65}, cstring},
                         {message_state, 1, integer},
                         {broadcast_area_identifier, undefined, tlv},
                         {broadcast_area_success, undefined, tlv},
                         {query_broadcast_response_tlvs, undefined, tlv}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := cancel_broadcast_sm} = Map) ->
    AllowedParameters = [{service_type, {0, 6}, cstring},
                         {message_id, {0, 65}, cstring},
                         {source_addr_ton, 1, integer},
                         {source_addr_npi, 1, integer},
                         {source_addr, {0, 65}, cstring}
                        ],
    encode_parameters(AllowedParameters, Map);
encode_msg(#{command := cancel_broadcast_sm_resp} = Map) ->
    AllowedParameters = [],
    encode_parameters(AllowedParameters, Map).


decode_parameters(Allowed, Bin) ->
    decode_parameters(Allowed, Bin, #{}).

encode_parameters(Allowed, Map) ->
    encode_parameters(lists:reverse(Allowed), Map, <<>>).

decode_parameters([], <<>>, Acc) ->
    Acc;
decode_parameters([], Bin, Acc) ->
    ?LOG_INFO("More to decode, have: ~p, still need: ~p", [Acc, Bin]),
    Acc;
decode_parameters(Allowed, <<>>, Acc) ->
    ?LOG_INFO("Nothing more to decode but still want params, have: ~p, want: ~p", [Acc, Allowed]),
    Acc;
decode_parameters([{Name, Length, integer}|Ts], Bin, Acc) ->
    <<Value:(Length*8), Rest/binary>> = Bin,
    NewAcc = decode_parameter(Name, Value, Acc),
    decode_parameters(Ts, Rest, NewAcc);
decode_parameters([{Name, {MinL, MaxL}, cstring}|Ts], Bin, Acc) ->
    {Value, Rest} = extract_cstring(Bin),
    Length = byte_size(Value)+1,
    true = Length >= MinL andalso Length =< MaxL,
    NewAcc = decode_parameter(Name, Value, Acc),
    decode_parameters(Ts, Rest, NewAcc);
decode_parameters([{Name, {MinL, MaxL}, string}|Ts], Bin, Acc) ->
    <<Length:8, Value:Length/binary, Rest/binary>> = Bin,
    true = Length >= MinL andalso Length =< MaxL,
    NewAcc = decode_parameter(Name, Value, Acc),
    decode_parameters(Ts, Rest, NewAcc);
decode_parameters([{Name, Length, tlv}|Ts], Bin, Acc) ->
    <<Tag:16, Length:16, Value:Length/binary, Rest/binary>> = Bin,
    NewAcc = decode_tlv(Name, Tag, Value, Acc),
    decode_parameters(Ts, Rest, NewAcc).

encode_parameters([], _, Acc) ->
    Acc;
encode_parameters(_, Map, Acc) when map_size(Map) =:= 0 ->
    Acc;
encode_parameters([{Name, Length, integer}|Ts], Map, Acc) ->
    case maps:take(Name, Map) of
        error ->
            encode_parameters(Ts, Map, Acc);
        {Value, NewMap} ->
            NewVal = encode_parameter(Name, Value),
            NewAcc = <<NewVal:(Length*8), Acc/binary>>,
            encode_parameters(Ts, NewMap, NewAcc)
    end;
encode_parameters([{Name, {_MinL, _MaxL}, cstring}|Ts], Map, Acc) ->
    case maps:take(Name, Map) of
        error ->
            encode_parameters(Ts, Map, Acc);
        {Value, NewMap} ->
            NewVal = encode_parameter(Name, Value),
            NewAcc = <<NewVal/binary, 0, Acc/binary>>,
            encode_parameters(Ts, NewMap, NewAcc)
    end;
encode_parameters([{Name, {_MinL, _MaxL}, string}|Ts], Map, Acc) ->
    case maps:take(Name, Map) of
        error ->
            encode_parameters(Ts, Map, Acc);
        {Value, NewMap} ->
            NewVal = encode_parameter(Name, Value),
            Length = byte_size(NewVal),
            NewAcc = <<Length:8, NewVal:Length/binary, Acc/binary>>,
            encode_parameters(Ts, NewMap, NewAcc)
    end;
encode_parameters([{Name, Length, tlv}|Ts], Map, Acc) ->
    case maps:take(Name, Map) of
        error ->
            encode_parameters(Ts, Map, Acc);
        {Tlv, NewMap} ->
            {Tag, Value} = encode_tlv(Tlv),
            NewVal = encode_parameter(Name, Value),
            NewAcc = <<Tag:16, Length:16, NewVal:Length/binary, Acc/binary>>,
            encode_parameters(Ts, NewMap, NewAcc)
    end.

extract_cstring(Bin) ->
    case string:split(Bin, <<0>>) of
        [] ->
            {<<>>, <<>>};
        [A] ->
            {A, <<>>};
        [A, Rest] ->
            {A, Rest}
    end.

decode_parameter(addr_ton, Value, Acc) ->
    Acc#{addr_ton => parse_ton(Value)};
decode_parameter(source_addr_ton, Value, Acc) ->
    Acc#{source_addr_ton => parse_ton(Value)};
decode_parameter(dest_addr_ton, Value, Acc) ->
    Acc#{dest_addr_ton => parse_ton(Value)};
decode_parameter(esme_addr_ton, Value, Acc) ->
    Acc#{esme_addr_ton => parse_ton(Value)};
decode_parameter(addr_npi, Value, Acc) ->
    Acc#{addr_npi => parse_npi(Value)};
decode_parameter(source_addr_npi, Value, Acc) ->
    Acc#{source_addr_npi => parse_npi(Value)};
decode_parameter(dest_addr_npi, Value, Acc) ->
    Acc#{dest_addr_npi => parse_npi(Value)};
decode_parameter(esme_addr_npi, Value, Acc) ->
    Acc#{esme_addr_npi => parse_npi(Value)};

decode_parameter(address_range, Value, Acc) ->
    Acc#{address_range => Value};
decode_parameter(data_coding, Value, Acc) ->
    Acc#{data_coding => parse_data_coding(Value)};
decode_parameter(destination_addr, Value, Acc) ->
    Acc#{destination_addr => Value};
decode_parameter(dest_flag, Value, Acc) ->
    Acc#{dest_flag => parse_dest_flag(Value)};
decode_parameter(dl_name, Value, Acc) ->
    Acc#{dl_name => Value};
decode_parameter(esme_addr, Value, Acc) ->
    Acc#{esme_addr => Value};
decode_parameter(esm_class, Value, Acc) ->
    <<GS:2, MT:4, MM:2>> = <<Value:8>>,
    Acc#{esm_class => #{gsm_specific => parse_esm_class_gsm_specific(GS),
                        message_type => parse_esm_class_message_type(MT),
                        messaging_mode => parse_esm_class_messaging_mode(MM)
                       }
        };
decode_parameter(interface_version, Value, Acc) ->
    <<Ma:8, Mi:8>> = <<Value:16>>,
    Acc#{interface_version => {Ma, Mi}};
decode_parameter(message_id, Value, Acc) ->
    Acc#{message_id => Value};
decode_parameter(message_state, Value, Acc) ->
    Acc#{message_state => parse_message_state(Value)};
decode_parameter(no_unsuccess, Value, Acc) ->
    Acc#{no_unsuccess => Value};
decode_parameter(number_of_dests, Value, Acc) ->
    Acc#{number_of_dests => Value};
decode_parameter(password, Value, Acc) ->
    Acc#{password => Value};
decode_parameter(priority_flag, Value, Acc) ->
    Acc#{priority_flag => Value};
decode_parameter(protocol_id, Value, Acc) ->
    Acc#{protocol_id => Value};
decode_parameter(registered_delivery, Value, Acc) ->
    Acc#{registered_delivery => Value};
decode_parameter(replace_if_present_flag, Value, Acc) ->
    Acc#{replace_if_present_flag => case Value of
                                        0 -> false;
                                        1 -> true
                                    end
        };
decode_parameter(scheduled_delivery_time, Value, Acc) ->
    Acc#{scheduled_delivery_time => Value};
decode_parameter(validity_period, Value, Acc) ->
    Acc#{validity_period => Value};
decode_parameter(final_date, Value, Acc) ->
    Acc#{final_date => Value};
decode_parameter(sequence_number, Value, Acc) ->
    Acc#{sequence_number => Value};
decode_parameter(service_type, Value, Acc) ->
    Acc#{service_type => Value};
decode_parameter(short_message, Value, Acc) ->
    Acc#{short_message => Value};
decode_parameter(sm_default_msg_id, Value, Acc) ->
    Acc#{sm_default_msg_id => Value};
decode_parameter(sm_length, Value, Acc) ->
    Acc#{sm_length => Value};
decode_parameter(source_addr, Value, Acc) ->
    Acc#{source_addr => Value};
decode_parameter(system_id, Value, Acc) ->
    Acc#{system_id => Value};
decode_parameter(system_type, Value, Acc) ->
    Acc#{system_type => Value}.

encode_parameter(addr_ton, Value) ->
    compose_ton(Value);
encode_parameter(source_addr_ton, Value) ->
    compose_ton(Value);
encode_parameter(dest_addr_ton, Value) ->
    compose_ton(Value);
encode_parameter(esme_addr_ton, Value) ->
    compose_ton(Value);
encode_parameter(addr_npi, Value) ->
    compose_npi(Value);
encode_parameter(source_addr_npi, Value) ->
    compose_npi(Value);
encode_parameter(dest_addr_npi, Value) ->
    compose_npi(Value);
encode_parameter(esme_addr_npi, Value) ->
    compose_npi(Value);
encode_parameter(address_range, Value) ->
    Value;
encode_parameter(data_coding, Value) ->
    compose_data_coding(Value);
encode_parameter(destination_addr, Value) ->
    Value;
encode_parameter(dest_flag, Value) ->
    compose_dest_flag(Value);
encode_parameter(dl_name, Value) ->
    Value;
encode_parameter(esme_addr, Value) ->
    Value;
encode_parameter(esm_class, Value) ->
    #{gsm_specific := GsmSpec,
      message_type := MessageType,
      messaging_mode := MessagingMode
     } = Value,
    GS = compose_esm_class_gsm_specific(GsmSpec),
    MT = compose_esm_class_message_type(MessageType),
    MM = compose_esm_class_messaging_mode(MessagingMode),
    <<EsmValue:8>> = <<GS:2, MT:4, MM:2>>,
    EsmValue;
encode_parameter(interface_version, Value) ->
    {Ma, Mi} = Value,
    <<Version:16>> = <<Ma:8, Mi:8>>,
    Version;
encode_parameter(message_id, Value) ->
    Value;
encode_parameter(message_state, Value) ->
    compose_message_state(Value);
encode_parameter(no_unsuccess, Value) ->
    Value;
encode_parameter(number_of_dests, Value) ->
    Value;
encode_parameter(password, Value) ->
    Value;
encode_parameter(priority_flag, Value) ->
    Value;
encode_parameter(protocol_id, Value) ->
    Value;
encode_parameter(registered_delivery, Value) ->
    Value;
encode_parameter(replace_if_present_flag, Value) ->
    case Value of
        false -> 0;
        true -> 1
    end;
encode_parameter(scheduled_delivery_time, Value) ->
    Value;
encode_parameter(validity_period, Value) ->
    Value;
encode_parameter(final_date, Value) ->
    Value;
encode_parameter(sequence_number, Value) ->
    Value;
encode_parameter(service_type, Value) ->
    Value;
encode_parameter(short_message, Value) ->
    Value;
encode_parameter(sm_default_msg_id, Value) ->
    Value;
encode_parameter(sm_length, Value) ->
    Value;
encode_parameter(source_addr, Value) ->
    Value;
encode_parameter(system_id, Value) ->
    Value;
encode_parameter(system_type, Value) ->
    Value.

decode_tlv(Name, Tag, Value, Acc) ->
    Acc#{Name => {Tag, Value}}.

encode_tlv({Tag, Value}) ->
    {Tag, Value}.

parse_ton(2#00000000) ->
    unknown;
parse_ton(2#00000001) ->
    international;
parse_ton(2#00000010) ->
    national;
parse_ton(2#00000011) ->
    network_specific;
parse_ton(2#00000100) ->
    subscriber_number;
parse_ton(2#00000101) ->
    alphanumeric;
parse_ton(2#00000110) ->
    abbreviated.

compose_ton(unknown) ->
    2#00000000;
compose_ton(international) ->
    2#00000001;
compose_ton(national) ->
    2#00000010;
compose_ton(network_specific) ->
    2#00000011;
compose_ton(subscriber_number) ->
    2#00000100;
compose_ton(alphanumeric) ->
    2#00000101;
compose_ton(abbreviated) ->
    2#00000110.

parse_npi(2#00000000) ->
    unknown;
parse_npi(2#00000001) ->
    isdn;
parse_npi(2#00000011) ->
    data;
parse_npi(2#00000100) ->
    telex;
parse_npi(2#00000110) ->
    land_mobile;
parse_npi(2#00001000) ->
    national;
parse_npi(2#00001001) ->
    private;
parse_npi(2#00001010) ->
    ermes;
parse_npi(2#00001110) ->
    internet;
parse_npi(2#00010010) ->
    wap.

compose_npi(unknown) ->
    2#00000000;
compose_npi(isdn) ->
    2#00000001;
compose_npi(data) ->
    2#00000011;
compose_npi(telex) ->
    2#00000100;
compose_npi(land_mobile) ->
    2#00000110;
compose_npi(national) ->
    2#00001000;
compose_npi(private) ->
    2#00001001;
compose_npi(ermes) ->
    2#00001010;
compose_npi(internet) ->
    2#00001110;
compose_npi(wap) ->
    2#00010010.

parse_dest_flag(1) ->
    sme_address;
parse_dest_flag(2) ->
    distribution_list_name.

compose_dest_flag(sme_address) ->
    1;
compose_dest_flag(distribution_list_name) ->
    2.

parse_message_state(0) ->
    scheduled;
parse_message_state(1) ->
    enroute;
parse_message_state(2) ->
    delivered;
parse_message_state(3) ->
    expired;
parse_message_state(4) ->
    deleted;
parse_message_state(5) ->
    undeliverable;
parse_message_state(6) ->
    accepted;
parse_message_state(7) ->
    unknown;
parse_message_state(8) ->
    rejected;
parse_message_state(9) ->
    skipped.

compose_message_state(scheduled) ->
    0;
compose_message_state(enroute) ->
    1;
compose_message_state(delivered) ->
    2;
compose_message_state(expired) ->
    3;
compose_message_state(deleted) ->
    4;
compose_message_state(undeliverable) ->
    5;
compose_message_state(accepted) ->
    6;
compose_message_state(unknown) ->
    7;
compose_message_state(rejected) ->
    8;
compose_message_state(skipped) ->
    9.

parse_data_coding(2#0000_0000) ->
    mc_specific;
parse_data_coding(2#0000_0001) ->
    ia5;
parse_data_coding(2#0000_0010 = C) ->
    {unspecified, C};
parse_data_coding(2#0000_0011) ->
    latin1;
parse_data_coding(2#0000_0100 = C) ->
    {unspecified, C};
parse_data_coding(2#0000_0101) ->
    jis;
parse_data_coding(2#0000_0110) ->
    cyrillic;
parse_data_coding(2#0000_0111) ->
    latin_hebrew;
parse_data_coding(2#0000_1000) ->
    ucs2;
parse_data_coding(2#0000_1001) ->
    pictogram;
parse_data_coding(2#0000_1010) ->
    music;
parse_data_coding(2#0000_1011 = C) ->
    {reserved, C};
parse_data_coding(2#0000_1100 = C) ->
    {reserved, C};
parse_data_coding(2#0000_1101) ->
    extended_kanji_jis;
parse_data_coding(2#0000_1110) ->
    ks_c_5601;
parse_data_coding(C) when C >= 2#0000_1111; C =<2#1011_1111 ->
    {reserved, C};
parse_data_coding(C) when C >= 2#1100_0000; C =< 2#1100_1111 ->
    {gsm_mwi_control, C};
parse_data_coding(C) when C >= 2#1101_0000; C =< 2#1101_1111 ->
    {gsm_mwi_control, C};
parse_data_coding(C) when C >= 2#1110_0000; C =< 2#1110_1111 ->
    {reserved, C};
parse_data_coding(C) when C >= 2#1111_0000; C =< 2#1111_1111 ->
    {gsm_message_class_control, C}.

compose_data_coding(mc_specific) ->
    2#0000_0000;
compose_data_coding(ia5) ->
    2#0000_0001;
compose_data_coding({unspecified, C}) ->
    C;
compose_data_coding(latin1) ->
    2#0000_0011;
compose_data_coding(jis) ->
    2#0000_0101;
compose_data_coding(cyrillic) ->
    2#0000_0110;
compose_data_coding(latin_hebrew) ->
    2#0000_0111;
compose_data_coding(ucs2) ->
    2#0000_1000;
compose_data_coding(pictogram) ->
    2#0000_1001;
compose_data_coding(music) ->
    2#0000_1010;
compose_data_coding({reserved, C}) ->
    C;
compose_data_coding(extended_kanji_jis) ->
    2#0000_1101;
compose_data_coding(ks_c_5601) ->
    2#0000_1110;
compose_data_coding({gsm_mwi_control, C}) ->
    C;
compose_data_coding({gsm_message_class_control, C}) ->
    C.

parse_esm_class_gsm_specific(2#00) ->
    default;
parse_esm_class_gsm_specific(2#01) ->
    datagram;
parse_esm_class_gsm_specific(2#10) ->
    forward;
parse_esm_class_gsm_specific(2#11) ->
    store_forward.

compose_esm_class_gsm_specific(default) ->
    2#00;
compose_esm_class_gsm_specific(datagram) ->
    2#01;
compose_esm_class_gsm_specific(forward) ->
    2#10;
compose_esm_class_gsm_specific(store_forward) ->
    2#11.

parse_esm_class_message_type(2#0000) ->
    default;
parse_esm_class_message_type(2#0001) ->
    mc_delivery_receipt;
parse_esm_class_message_type(2#1000) ->
    intermediate_delivery_notification;
parse_esm_class_message_type(2#0010) ->
    delivery_acknowledgement;
parse_esm_class_message_type(2#0100) ->
    user_acknowledgement;
parse_esm_class_message_type(2#0110) ->
    conversation_abort.

compose_esm_class_message_type(default) ->
    2#0000;
compose_esm_class_message_type(mc_delivery_receipt) ->
    2#0001;
compose_esm_class_message_type(intermediate_delivery_notification) ->
    2#1000;
compose_esm_class_message_type(delivery_acknowledgement) ->
    2#0010;
compose_esm_class_message_type(user_acknowledgement) ->
    2#0100;
compose_esm_class_message_type(conversation_abort) ->
    2#0110.

parse_esm_class_messaging_mode(2#00) ->
    no_features;
parse_esm_class_messaging_mode(2#01) ->
    udhi;
parse_esm_class_messaging_mode(2#10) ->
    reply_path;
parse_esm_class_messaging_mode(2#11) ->
    udhi_reply_path.

compose_esm_class_messaging_mode(no_features) ->
    2#00;
compose_esm_class_messaging_mode(udhi) ->
    2#01;
compose_esm_class_messaging_mode(reply_path) ->
    2#10;
compose_esm_class_messaging_mode(udhi_reply_path) ->
    2#11.
