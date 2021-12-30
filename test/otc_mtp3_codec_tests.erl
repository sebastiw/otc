-module(otc_mtp3_codec_tests).

-include_lib("eunit/include/eunit.hrl").

mgmt_xco_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#0c, 16#31, 16#00, 16#07,
            16#00>>,
    Val = otc_mtp3_codec:decode(Bin),
    NewBin = otc_mtp3_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

mgmt_cbd_test() ->
    Bin = <<16#c0, 16#79, 16#da, 16#10, 16#1c, 16#51, 16#0a>>,
    Val = otc_mtp3_codec:decode(Bin),
    NewBin = otc_mtp3_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

maint_sltm_test() ->
    Bin = <<16#c1, 16#79, 16#da, 16#10, 16#1c, 16#11, 16#f0, 16#aa,
            16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa,
            16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa>>,
    Val = otc_mtp3_codec:decode(Bin),
    NewBin = otc_mtp3_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

maint_slta_test() ->
    Bin = <<16#c1, 16#43, 16#70, 16#9e, 16#16, 16#21, 16#f0, 16#aa,
            16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa,
            16#aa, 16#aa, 16#aa, 16#aa, 16#aa, 16#aa>>,
    Val = otc_mtp3_codec:decode(Bin),
    NewBin = otc_mtp3_codec:encode(Val),
    ?assertEqual(Bin, NewBin).
