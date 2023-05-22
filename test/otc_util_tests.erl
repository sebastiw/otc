-module(otc_util_tests).

-include_lib("eunit/include/eunit.hrl").

tbcd_decode_test_() ->
    [?_assertEqual("123456", otc_util:decode_tbcd(<<2:4, 1:4, 4:4, 3:4, 6:4, 5:4>>)),
     ?_assertEqual("12345", otc_util:decode_tbcd(<<2:4, 1:4, 4:4, 3:4, 15:4, 5:4>>)),
     ?_assertEqual("46724400000", otc_util:decode_tbcd(<<100,39,68,0,0,240>>))
    ].

tbcd_encode_test_() ->
    [?_assertEqual(<<2:4, 1:4, 4:4, 3:4, 6:4, 5:4>>, otc_util:encode_tbcd("123456")),
     ?_assertEqual(<<2:4, 1:4, 4:4, 3:4, 15:4, 5:4>>, otc_util:encode_tbcd("12345"))
    ].
