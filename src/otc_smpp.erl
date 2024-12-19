-module(otc_smpp).
-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

-include("include/smpp.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "SMS Forum SMPP V5.0".

codec(Bin) when is_binary(Bin) ->
    decode(Bin);
codec(Map) when is_map(Map) ->
    encode({Map, <<>>});
codec({Map, PDU}) ->
    encode({Map, PDU}).

next(_) ->
    '$stop'.

decode(Bin) ->
    {Header, Rest} = parse_header(Bin),
    Msg = decode_msg(Header, Rest),
    maps:merge(Header, Msg).

encode({Map, PDU}) ->
    M = encode(Map),
    <<M/binary, PDU/binary>>;
encode(Map) ->
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
                         {schedule_delivery_time, {1,17}, cstring},
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
decode_parameter(message_state, Value, Acc) ->
    Acc#{message_state => parse_message_state(Value)};
decode_parameter(replace_if_present_flag, Value, Acc) ->
    Acc#{replace_if_present_flag => case Value of
                                        0 -> false;
                                        1 -> true
                                    end
        };
decode_parameter(Name, Value, Acc) ->
    Acc#{Name => Value}.

encode_parameter(_Name, Value) ->
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

parse_dest_flag(1) ->
    sme_address;
parse_dest_flag(2) ->
    distribution_list_name.

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

