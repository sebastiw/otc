-module(otc_sgsap_tests).

-include_lib("eunit/include/eunit.hrl").

uplink_unitdata_test() ->
    Bin = <<"080108193254769810325416029204">>,
    Map = #{message_type => uplink_unitdata,
            imsi => "123456789012345",
            nas_message_container =>
                #{protocol_discriminator => sms_messages,
                  transaction_identifier => 2,
                  message_type => cp_ack
                 }
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

release_request_test() ->
    Bin = <<"1B01081932547698103254080103">>,
    Map = #{message_type => release_request,
            imsi => "123456789012345",
            sgs_cause => imsi_unknown
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

location_update_request_test() ->
    Bin = <<"09010819325476981032540937066D6D65633031096D6D65676930303032036D6D6503657063066D6E63303031066D63633030310B336770706E6574776F726B036F72670A0101040500F1100007">>,
    Map = #{message_type => location_update_request,
            imsi => "123456789012345",
            mme_name => "mmec01.mmegi0002.mme.epc.mnc001.mcc001.3gppnetwork.org",
            eps_location_update_type => imsi_attach,
            new_location_area_identifier =>
                #{mcc => "001",
                  mnc => "01",
                  location_area_code => 7
                 }
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

location_update_accept_test() ->
    Bin = <<"0A01081932547698103254040500F11000070E05F402E10289">>,
    Map = #{message_type => location_update_accept,
            imsi => "123456789012345",
            location_area_identifier =>
                #{mcc => "001",
                  mnc => "01",
                  location_area_code => 7
                 },
            new_tmsi_or_imsi =>
                {tmsi, binary:decode_hex(<<"02E10289">>)}
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

tmsi_reallocation_complete_test() ->
    Bin = <<"0C01081932547698103254">>,
    Map = #{message_type => tmsi_reallocation_complete,
            imsi => "123456789012345"
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

mm_information_request_test() ->
    Bin = <<"1A01081932547698103254171C43088741E19058341E0145088761F1985C369F014702500100803100">>,
    Map = #{message_type => mm_information_request,
            imsi => "123456789012345",
            mm_information =>
                #{full_name =>
                      #{coding_scheme => cell_broadcast,
                        add_country_initials => false,
                        text_string => "ABCDEFG"
                       },
                  short_name =>
                      #{coding_scheme => cell_broadcast,
                        add_country_initials => false,
                        text_string => "abcdefg"
                       },
                  universal_time_and_time_zone =>
                      #{datetime => {{2020, 5, 10}, {0, 8, 13}},
                        timezone => 0
                       }
                 }
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

paging_request_test() ->
    Bin = <<"0101081932547698103254022903766C72066D7363303031066D6E63303031066D63633030310B336770706E6574776F726B036F7267200101040500F1100007">>,
    Map = #{message_type => paging_request,
            imsi => "123456789012345",
            vlr_name => "vlr.msc001.mnc001.mcc001.3gppnetwork.org",
            service_indicator => cs_call,
            location_area_identifier =>
                #{mcc => "001",
                  mnc => "01",
                  location_area_code => 7
                 }
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

service_request_test() ->
    Bin = <<"0601081932547698103254200101250101">>,
    Map = #{message_type => service_request,
            imsi => "123456789012345",
            service_indicator => cs_call,
            ue_emm_mode => emm_connected
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

service_abort_request_test() ->
    Bin = <<"1701081932547698103254">>,
    Map = #{message_type => service_abort_request,
            imsi => "123456789012345"
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

ue_unreachable_test() ->
    Bin = <<"1F01081932547698103254080106">>,
    Map = #{message_type => ue_unreachable,
            imsi => "123456789012345",
            sgs_cause => ue_unreachable
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).

mo_csfb_indication_test() ->
    Bin = <<"1801081932547698103254">>,
    Map = #{message_type => mo_csfb_indication,
            imsi => "123456789012345"
           },
    Msg = otc_sgsap:decode(binary:decode_hex(Bin)),
    ?assertMatch(Map, Msg),
    NewBin = otc_sgsap:encode(Map),
    ?assertMatch(Bin, binary:encode_hex(NewBin)).
