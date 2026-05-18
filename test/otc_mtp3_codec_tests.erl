-module(otc_mtp3_codec_tests).

-include("include/point_code.hrl").

-include_lib("eunit/include/eunit.hrl").

mgmt_xco_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#31, 16#01, 16#07, 16#00>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0000 .... .... .... .... .... .... .... = Signalling Link Selector: 0
    %% Message Transfer Part Level 3 Management
    %%     .... 0001 = H0 (Message Group): Changeover and changeback messages (0x1)
    %%     0011 .... = H1 (Message): Extended changeover-order signal (0x3)
    %%     Forward Sequence Number: 1793
    Exp = #{destination_point_code => #itu_pc{zone = 3, region = 79, signalling_point = 1},
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => #itu_pc{zone = 6, region = 8, signalling_point = 3},
            payload =>
                #{forward_sequence_number => 1793,
                  message_type => extended_changeover_order},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin, #{point_code => record}),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_cbd_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#1c, 16#51, 16#0a>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0001 .... .... .... .... .... .... .... = Signalling Link Selector: 1
    %% Message Transfer Part Level 3 Management
    %%     .... 0001 = H0 (Message Group): Changeover and changeback messages (0x1)
    %%     0101 .... = H1 (Message): Changeback-declaration signal (0x5)
    %%     Change Back Code: 10
    Exp = #{destination_point_code => 6777,
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => 12355,
            payload =>
                #{changeback_code => 10,
                  message_type => changeback_declaration},
            service_indicator => mgmt,
            signalling_link_selection => 1},
    Val = otc_mtp3:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_coo_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#11, 16#7b>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0000 .... .... .... .... .... .... .... = Signalling Link Selector: 0
    %% Message Transfer Part Level 3 Management
    %%     .... 0001 = H0 (Message Group): Changeover and changeback messages (0x1)
    %%     0001 .... = H1 (Message): Changeover-order signal (0x1)
    %%     .111 1011 = Forward Sequence Number: 123
    Exp = #{destination_point_code => #itu_pc{zone = 3, region = 79, signalling_point = 1},
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => #itu_pc{zone = 6, region = 8, signalling_point = 3},
            payload =>
                #{forward_sequence_number => 123,
                  message_type => changeover_order},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin, #{point_code => record}),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_tfp_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#14, 16#7d, 16#32>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0000 .... .... .... .... .... .... .... = Signalling Link Selector: 0
    %% Message Transfer Part Level 3 Management
    %%     .... 0100 = H0 (Message Group): Transfer prohibited-allowed-restricted messages (0x4)
    %%     0001 .... = H1 (Message): Transfer-prohibited signal (0x1)
    %%     ..11 0010 0111 1101 = Affected Point Code (ITU): 12925
    Exp = #{destination_point_code => #itu_pc{zone = 3, region = 79, signalling_point = 1},
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => #itu_pc{zone = 6, region = 8, signalling_point = 3},
            payload =>
                #{destination_point_code => #itu_pc{zone = 6, region = 79, signalling_point = 5},
                  message_type => transfer_prohibited},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin, #{point_code => record}),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_dlc_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#18, 16#0a, 16#08>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0000 .... .... .... .... .... .... .... = Signalling Link Selector: 0
    %% Message Transfer Part Level 3 Management
    %%     .... 1000 = H0 (Message Group): Signalling-data-link-connection messages (0x8)
    %%     0001 .... = H1 (Message): Signalling-data-link-connection-order signal (0x1)
    %%     .... 1000 0000 1010 = Link: 2058
    Exp = #{destination_point_code => #itu_pc{zone = 3, region = 79, signalling_point = 1},
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => #itu_pc{zone = 6, region = 8, signalling_point = 3},
            payload =>
                #{signalling_data_link_id => 2058,
                  message_type => data_link_connection_order},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin, #{point_code => record}),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_upu_test() ->
    Bin = <<16#c0, 16#93, 16#35, 16#41, 16#01, 16#1a, 16#5c, 16#11, 16#13>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0000 = Service indicator: Signalling Network Management Message (SNM) (0x0)
    %%     Routing label
    %%         .... .... .... .... ..11 0101 1001 0011 = DPC: 13715
    %%         .... 0001 0100 0001 00.. .... .... .... = OPC: 1284
    %%         0000 .... .... .... .... .... .... .... = Signalling Link Selector: 0
    %% Message Transfer Part Level 3 Management
    %%     .... 1010 = H0 (Message Group): User part flow control messages (0xa)
    %%     0001 .... = H1 (Message): User part unavailable signal (0x1)
    %%     ..01 0001 0101 1100 = Affected Point Code (ITU): 4444
    %%     .... 0011 = User: SCCP (0x3)
    %%     0001 .... = Cause: Unequipped remote user (0x1)
    Exp = #{destination_point_code => #itu_pc{zone = 6, region = 178, signalling_point = 3},
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => #itu_pc{zone = 0, region = 160, signalling_point = 4},
            payload =>
                #{destination_point_code => #itu_pc{zone = 2, region = 43, signalling_point = 4},
                  message_type => user_part_unavailable,
                  user_part_id => 3,
                  unavailability_cause => unequipped},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin, #{point_code => record}),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

maint_sltm_test() ->
    TP = test_pattern(),
    Bin = <<16#c1, 16#79, 16#da, 16#10, 16#1c, 16#11, 16#f0, TP/binary>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0001 = Service indicator: Maintenance Regular Message (MTN) (0x1)
    %%     Routing label
    %%         .... .... .... .... ..01 1010 0111 1001 = DPC: 6777
    %%         .... 1100 0001 0000 11.. .... .... .... = OPC: 12355
    %%         0001 .... .... .... .... .... .... .... = Signalling Link Selector: 1
    %% Message Transfer Part Level 3 Management
    %%     .... 0001 = H0 (Message Group): Test messages (0x1)
    %%     0001 .... = H1 (Message): Signalling link test message (0x1)
    %%     1111 .... = Test length: 15
    %%     Test pattern: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    Exp = #{destination_point_code => 6777,
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => 12355,
            payload =>
                #{test_pattern => TP,
                  message_type => signalling_link_test},
            service_indicator => maint,
            signalling_link_selection => 1},
    Val = otc_mtp3:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

maint_slta_test() ->
    TP = test_pattern(),
    Bin = <<16#c1, 16#43, 16#70, 16#9e, 16#16, 16#21, 16#f0, TP/binary>>,
    %% Message Transfer Part Level 3
    %%     Service information octet
    %%         11.. .... = Network indicator: Reserved for national use (0x3)
    %%         ..00 .... = Spare: 0x0
    %%         .... 0001 = Service indicator: Maintenance Regular Message (MTN) (0x1)
    %%     Routing label
    %%         .... .... .... .... ..11 0000 0100 0011 = DPC: 12355
    %%         .... 0110 1001 1110 01.. .... .... .... = OPC: 6777
    %%         0001 .... .... .... .... .... .... .... = Signalling Link Selector: 1
    %% Message Transfer Part Level 3 Management
    %%     .... 0001 = H0 (Message Group): Test messages (0x1)
    %%     0010 .... = H1 (Message): Signalling link test acknowledgement message (0x2)
    %%     1111 .... = Test length: 15
    %%     Test pattern: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    Exp = #{destination_point_code => 12355,
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => 6777,
            payload =>
                #{test_pattern => TP,
                  message_type => signalling_link_test_ack},
            service_indicator => maint,
            signalling_link_selection => 1},
    Val = otc_mtp3:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

test_pattern() ->
    <<16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa,
      16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa>>.
