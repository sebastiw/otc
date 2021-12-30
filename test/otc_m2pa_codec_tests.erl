-module(otc_m2pa_codec_tests).

-include_lib("eunit/include/eunit.hrl").

empty_user_data_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#01, 16#00, 16#00, 16#00, 16#10,
            16#00, 16#00, 16#00, 16#01, 16#00, 16#00, 16#00, 16#00>>,
    Val = otc_m2pa_codec:decode(Bin),
    NewBin = otc_m2pa_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_out_of_service_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#09>>,
    Val = otc_m2pa_codec:decode(Bin),
    NewBin = otc_m2pa_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_alignment_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#01>>,
    Val = otc_m2pa_codec:decode(Bin),
    NewBin = otc_m2pa_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_proving_normal_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#02>>,
    Val = otc_m2pa_codec:decode(Bin),
    NewBin = otc_m2pa_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

link_status_ready_test() ->
    Bin = <<16#01, 16#00, 16#0b, 16#02, 16#00, 16#00, 16#00, 16#14,
            16#00, 16#ff, 16#ff, 16#ff, 16#00, 16#ff, 16#ff, 16#ff,
            16#00, 16#00, 16#00, 16#04>>,
    Val = otc_m2pa_codec:decode(Bin),
    NewBin = otc_m2pa_codec:encode(Val),
    ?assertEqual(Bin, NewBin).
