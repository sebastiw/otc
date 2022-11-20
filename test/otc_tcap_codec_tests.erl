-module(otc_tcap_codec_tests).

-include_lib("eunit/include/eunit.hrl").

decode_empty_begin_empty_user_info_test() ->
    %% Asymetric test due to user-information exist in AARQ, but set
    %% to empty.
    Dialogue = <<16#60,16#0F,16#80,16#02,16#07,16#80,16#A1,16#09,
                 16#06,16#07,16#04,16#00,16#00,16#01,16#00,16#15,
                 16#03>>,
    Bin = <<16#62,16#26,16#48,16#04,16#72,16#10,16#01,16#b3,
            16#6b,16#1e,16#28,16#1c,16#06,16#07,16#00,16#11,
            16#86,16#05,16#01,16#01,16#01,16#a0,16#11,Dialogue/binary>>,
    Exp = #{components => [],
            dialogue => #{application_context_family => map,
                          application_context_name => 'shortMsgMO-RelayContext-v3',
                          supported_versions => [version1],
                          type => dialogueRequest,
                          user_information => undefined
                         },
            otid => <<"721001B3">>,
            type => 'begin'},
    Val = otc_tcap:decode(Bin),
    ?assertEqual(Exp, Val).

empty_begin_no_user_info_test() ->
    EmptyComponents = <<108,0>>,
    Dialogue = <<16#60,16#0B,                        16#A1,16#09,
                 16#06,16#07,16#04,16#00,16#00,16#01,16#00,16#15,
                 16#03>>,
    Bin = <<16#62,16#24,16#48,16#04,16#72,16#10,16#01,16#B3,
            16#6B,16#1A,16#28,16#18,16#06,16#07,16#00,16#11,
            16#86,16#05,16#01,16#01,16#01,16#A0,16#0D,Dialogue/binary,
            EmptyComponents/binary>>,
    Exp = #{components => [],
            dialogue => #{application_context_family => map,
                          application_context_name => 'shortMsgMO-RelayContext-v3',
                          supported_versions => [version1],
                          type => dialogueRequest,
                          user_information => undefined
                         },
            otid => <<"721001B3">>,
            type => 'begin'},
    Val = otc_tcap:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_tcap:encode(Val),
    ?assertEqual(Bin, NewBin).

end_result_map_sm_v2_test() ->
    Dialogue = <<16#61,16#17,
                 16#a1,16#09,16#06,16#07,16#04,16#00,16#00,16#01,
                 16#00,16#14,16#02,16#a2,16#03,16#02,16#01,16#00,
                 16#a3,16#05,16#a1,16#03,16#02,16#01,16#00>>,
    MAPBin = <<16#30,16#15,16#04,16#08,16#99,16#00,16#99,16#00,
               16#99,16#00,16#99,16#F0,16#A0,16#09,16#81,16#07,
               16#11,16#22,16#33,16#44,16#55,16#66,16#F7>>,
    Components = <<16#6C,16#21,16#A2,16#1F,16#02,16#01,16#00,16#30,
                   16#1A,16#02,16#01,16#2D,MAPBin/binary>>,
    Bin = <<16#64,16#50,16#49,16#03,16#5d,16#03,16#c3,16#6b,
            16#26,16#28,16#24,16#06,16#07,16#00,16#11,16#86,
            16#05,16#01,16#01,16#01,16#a0,16#19,Dialogue/binary,
            Components/binary>>,
    EndComponent = #{component_type => returnResultLast,
                     invokeId => 0,
                     opcode => {local, 45},
                     result => MAPBin
                    },
    Exp = #{dialogue => #{application_context_family => map,
                          application_context_name => 'shortMsgGatewayContext-v2',
                          result => accepted,
                          result_source_diagnostic => {'dialogue-service-user', null},
                          supported_versions => [version1],
                          type => dialogueResponse,
                          user_information => undefined
                         },
            dtid => <<"5D03C3">>,
            components => [EndComponent],
            type => 'end'},
    Val = otc_tcap:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_tcap:encode(Val),
    ?assertEqual(Bin, NewBin).
