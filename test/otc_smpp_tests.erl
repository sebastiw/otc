-module(otc_smpp_tests).

-include_lib("eunit/include/eunit.hrl").

bind_transmitter_test() ->
    Bin = <<"00000028"
            "00000002"
            "00000000"
            "00000001"
            "61626869"
            "6B0070617373776F726400534D50500001000000">>,
    Map = #{command => bind_transmitter,
            command_status => esme_rok,
            sequence_number => 1,
            system_id => <<"abhik">>,
            password => <<"password">>,
            system_type => <<"SMPP">>,
            interface_version => 1,
            addr_ton => 0,
            addr_npi => 0,
            address_range => <<>>
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

bind_transmitter_resp_test() ->
    Bin = <<"00000015"
            "80000002"
            "00000000"
            "00000001"
            "534D534300">>,
    Map = #{command => bind_transmitter_resp,
            command_status => esme_rok,
            sequence_number => 1,
            system_id => <<"SMSC">>
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

enquire_link_test() ->
    Bin = <<"00000010"
            "00000015"
            "00000000"
            "00000002">>,
    Map = #{command => enquire_link,
            command_status => esme_rok,
            sequence_number => 2
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

enquire_link_resp_test() ->
    Bin = <<"00000010"
            "80000015"
            "00000000"
            "00000002">>,
    Map = #{command => enquire_link_resp,
            command_status => esme_rok,
            sequence_number => 2
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

submit_sm_test() ->
    Bin = <<"00000032"
            "00000004"
            "00000000"
            "00000003"
            "00000131303000010131323334353637383930000000000000000000000474657374">>,
    Map = #{command => submit_sm,
            command_status => esme_rok,
            sequence_number => 3,
            service_type => <<>>,
            source_addr_ton => 0,
            source_addr_npi => 1,
            source_addr => <<"100">>,
            dest_addr_ton => 1,
            dest_addr_npi => 1,
            destination_addr => <<"1234567890">>,
            esm_class => 0,
            protocol_id => 0,
            priority_flag => 0,
            schedule_delivery_time => <<>>,
            validity_period => <<>>,
            registered_delivery => 0,
            replace_if_present_flag => 0,
            data_coding => 0,
            sm_default_msg_id => 0,
            short_message => <<"test">>
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).


submit_sm_resp_test() ->
    Bin = <<"00000019"
            "80000004"
            "00000000"
            "00000003"
            "343763386337376100">>,
    Map = #{command => submit_sm_resp,
            command_status => esme_rok,
            sequence_number => 3,
            message_id => <<"47c8c77a">>
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

unbind_test() ->
    Bin = <<"00000010"
            "00000006"
            "00000000"
            "00000004">>,
    Map = #{command => unbind,
            command_status => esme_rok,
            sequence_number => 4
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

unbind_resp_test() ->
    Bin = <<"00000010"
            "80000006"
            "00000000"
            "00000004">>,
    Map = #{command => unbind_resp,
            command_status => esme_rok,
            sequence_number => 4
           },
    Msg = otc_smpp:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_smpp:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).


