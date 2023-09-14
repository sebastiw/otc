-module(otc_mtp3_codec_tests).

-include_lib("eunit/include/eunit.hrl").

mgmt_xco_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#31, 16#00, 16#07,
            16#00>>,
    Exp = #{destination_point_code => 6777,
            national_use_spare => 0,
            network_indicator => national_spare,
            originating_point_code => 12355,
            payload =>
                #{forward_sequence_number => 1792,
                  message_type => extended_changeover_order},
            service_indicator => mgmt,
            signalling_link_selection => 0},
    Val = otc_mtp3:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_mtp3:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_cbd_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#1c, 16#51, 16#0a>>,
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

maint_sltm_test() ->
    TP = test_pattern(),
    Bin = <<16#c1, 16#79, 16#da, 16#10, 16#1c, 16#11, 16#f0, TP/binary>>,
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
