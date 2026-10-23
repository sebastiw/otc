-module(otc_sccp_mgmt_codec_tests).

-include_lib("eunit/include/eunit.hrl").
-include("include/point_code.hrl").

scmg_sst_test() ->
    Bin = <<16#03, %% SCMG MsgType: SST
            16#06, %% Affected SSN
            16#12, 16#34, %% Affected PC
            16#00>>, %% Subsystem multiplicity indicator
    Exp = #{format_identifier => status_test,
            affected_point_code => #itu_pc{zone = 6, region = 130, signalling_point = 2},
            affected_subsystem_number => hlr,
            subsystem_multiplicity_indicator => 0
           },

    Val = otc_sccp_mgmt:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sccp_mgmt:encode(Val),
    ?assertEqual(Bin, NewBin).


