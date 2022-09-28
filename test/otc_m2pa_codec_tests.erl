-module(otc_m2pa_codec_tests).

-include_lib("eunit/include/eunit.hrl").

empty_user_data_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#01, 16#00, 16#00, 16#00, 16#10,
            16#00, 16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#00>>,
    Exp = #{backward_sequence_number => 1,
            forward_sequence_number => 0,
            message_class => m2pa,
            message_type => user_data
           },
    Val = otc_m2pa:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m2pa:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_out_of_service_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#09>>,
    Exp = #{backward_sequence_number => 16777215,
            forward_sequence_number => 16777215,
            link_status => out_of_service,
            message_class => m2pa,
            message_type => link_status
           },
    Val = otc_m2pa:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m2pa:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_alignment_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#01>>,
    Exp = #{backward_sequence_number => 16777215,
            forward_sequence_number => 16777215,
            link_status => alignment,
            message_class => m2pa,
            message_type => link_status
           },
    Val = otc_m2pa:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m2pa:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_proving_normal_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#02>>,
    Exp = #{backward_sequence_number => 16777215,
            forward_sequence_number => 16777215,
            link_status => proving_normal,
            message_class => m2pa,
            message_type => link_status
           },
    Val = otc_m2pa:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m2pa:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_ready_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#04>>,
    Exp = #{backward_sequence_number => 16777215,
            forward_sequence_number => 16777215,
            link_status => ready,
            message_class => m2pa,
            message_type => link_status
           },
    Val = otc_m2pa:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_m2pa:encode(Val),
    ?assertEqual(Bin, NewBin).
