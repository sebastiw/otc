-module(otc_map_codec_tests).

-include_lib("eunit/include/eunit.hrl").

sendRoutingInfoForSM_returnResultLast_test() ->
    IMSI = <<16#99,16#00,16#99,16#00,16#99,16#00,16#99,16#F0>>,
    NetworkNodeNumber = <<16#11,16#22,16#33,16#44,16#55,16#66,16#F7>>,
    MAPBin = <<16#30,16#15,16#04,16#08,IMSI/binary,16#A0,16#09,16#81,16#07,
               NetworkNodeNumber/binary>>,

    Component = #{component_type => returnResultLast,
                  invokeId => 0,
                  opcode => {local, 45},
                  result => MAPBin
                 },
    Exp = #{component_type => returnResultLast,
            invokeId => 0,
            operation => sendRoutingInfoForSM,
            result => #{imsi => IMSI,
                        locationInfoWithLMSI =>
                            #{'networkNode-Number' => NetworkNodeNumber}}},
    Val = otc_map:decode(Component),
    ?assertEqual(Exp, Val),
    NewComp = otc_map:encode(Val),
    ?assertEqual(Component, NewComp).
