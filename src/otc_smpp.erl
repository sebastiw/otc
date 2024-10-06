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

encode({_, _}) ->
    <<>>.

parse_header(Bin) ->
    <<CL:32, CID:32, CS:32, SN:32, Rest:(CL-16)/binary>> = Bin,
    Header = #{command => parse_command_id(CID),
               command_status => parse_command_status(CS),
               sequence_number => SN},
    {Header, Rest}.

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
                         {sm_length, 1, integer},
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
                         {sm_length, 1, integer},
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
                         {sm_length, 1, integer},
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
                         {sm_length, 1, integer},
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

decode_parameters(Allowed, Bin) ->
    decode_parameters(Allowed, Bin, #{}).

decode_parameters([], <<>>, Acc) ->
    Acc;
decode_parameters([], Bin, Acc) ->
    ?LOG_INFO("More to decode, have: ~p, still need: ~p", [Acc, Bin]),
    Acc;
decode_parameters(Allowed, <<>>, Acc) ->
    ?LOG_INFO("Nothing more to decode but still want params, have: ~p, want: ~p", [Acc, Allowed]),
    Acc;
decode_parameters([{Name, Length, integer}|Ts], Bin, Acc) ->
    <<Value:(Length*8)/integer, Rest/binary>> = Bin,
    decode_parameters(Ts, Rest, Acc#{Name => Value});
decode_parameters([{Name, {MinL, MaxL}, cstring}|Ts], Bin, Acc) ->
    {Value, Rest} = extract_cstring(Bin, MinL, MaxL),
    Length = byte_size(Value),
    true = Length >= MinL andalso Length =< MaxL,
    decode_parameters(Ts, Rest, Acc#{Name => Value});
decode_parameters([{Name, {MinL, MaxL}, string}|Ts], Bin, Acc) ->
    <<Length:8, Value:Length/binary, Rest/binary>> = Bin,
    true = Length >= MinL andalso Length =< MaxL,
    decode_parameters(Ts, Rest, Acc#{Name => Value});
decode_parameters([{Name, Length, tlv}|Ts], Bin, Acc) ->
    <<Tag:8, Length:8, Value:Length/binary, Rest/binary>> = Bin,
    decode_parameters(Ts, Rest, Acc#{Name => {Tag, Value}}).

extract_cstring(Bin, MinL, MaxL) ->
    case string:split(Bin, <<0>>) of
        [A] when byte_size(A) >= MinL, byte_size(A) =< MaxL ->
            {A, <<>>};
        [A, Rest] when byte_size(A) >= MinL, byte_size(A) =< MaxL ->
            {A, Rest}
    end.
