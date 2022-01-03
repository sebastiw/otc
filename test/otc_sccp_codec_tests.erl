-module(otc_sccp_codec_tests).

-include_lib("eunit/include/eunit.hrl").

udt_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#09, 16#80, 16#03, 16#0e, 16#19,
            16#0b, CdPA/binary,
            16#0b, CgPA/binary,
            16#18, D/binary>>,
    Val = otc_sccp_codec:decode(Bin),
    NewBin = otc_sccp_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

xudt_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#11, 16#80, 16#0f, 16#04, 16#0f, 16#1a, 16#00,
            16#0b, CdPA/binary,
            16#0b, CgPA/binary,
            16#18, D/binary>>,
    Val = otc_sccp_codec:decode(Bin),
    NewBin = otc_sccp_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

xudts_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#12, 16#01, 16#0f, 16#04, 16#0f, 16#1a, 16#00,
            16#0b, CgPA/binary,
            16#0b, CdPA/binary,
            16#18, D/binary>>,
    Val = otc_sccp_codec:decode(Bin),
    NewBin = otc_sccp_codec:encode(Val),
    ?assertEqual(Bin, NewBin).

xudts_arbitrary_pointers_test() ->
    %% parameters in arbitrary order
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#12, 16#07, 16#0f, 16#10, 16#03, 16#1a, 16#00,
            16#0b, CgPA/binary,
            16#0b, CdPA/binary,
            16#18, D/binary>>,
    Expected = <<16#12, 16#07, 16#0f, 16#04, 16#0f, 16#1a, 16#00,
                 16#0b, CdPA/binary,
                 16#0b, CgPA/binary,
                 16#18, D/binary>>,
    Val = otc_sccp_codec:decode(Bin),
    NewBin = otc_sccp_codec:encode(Val),
    ?assertEqual(Expected, NewBin).

called_party_address() ->
    <<16#12, 16#08, 16#00, 16#11, 16#04, 16#64, 16#27, 16#11, 16#22, 16#11, 16#22>>.
calling_party_address() ->
    <<16#12, 16#06, 16#00, 16#12, 16#08, 16#48, 16#76, 16#92, 16#88, 16#77, 16#66>>.
data() ->
    <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08,
      16#09, 16#0a, 16#0b, 16#0c, 16#0d, 16#0e, 16#0f, 16#10,
      16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17, 16#18>>.
