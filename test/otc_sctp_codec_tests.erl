-module(otc_sctp_codec_tests).

-include_lib("eunit/include/eunit.hrl").

data_test() ->
    Bin = <<16#0b, 16#5b, 16#bc, 16#32,
            16#29, 16#a6, 16#97, 16#12,
            16#7f, 16#ad, 16#03, 16#cb,

            16#00, 16#03, 16#00, 16#10,
            16#00, 16#73, 16#07, 16#3d,
            16#00, 16#08, 16#bc, 16#1b,
            16#00, 16#00, 16#00, 16#03>>,
    Exp = #{source_port => 2907,
            destination_port => 48178,
            verification_tag => <<16#29, 16#a6, 16#97, 16#12>>,
            checksum => <<16#7f, 16#ad, 16#03, 16#cb>>,
            chunks =>
                [#{chunk_type => data,
                   data => <<>>,
                   description => unfragmented,
                   payload_protocol_identifier => 3,
                   stream_identifier => 8,
                   stream_sequence_number => 48155,
                   tsn => 7538493}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

init_test() ->
    Bin = <<16#10, 16#f1, 16#0b, 16#5a,
            16#00, 16#00, 16#00, 16#00,
            16#0a, 16#ef, 16#79, 16#63,

            16#01, 16#00, 16#00, 16#28,
            16#69, 16#e6, 16#b3, 16#74,
            16#00, 16#03, 16#20, 16#00,
            16#00, 16#0a, 16#ff, 16#ff,
            16#a4, 16#52, 16#d9, 16#25,
            16#00, 16#0c, 16#00, 16#06,
            16#00, 16#05, 16#00, 16#00,
            16#80, 16#00, 16#00, 16#04,
            16#c0, 16#00, 16#00, 16#04,
            16#FF, 16#FF, 16#00, 16#04>>,
    Exp = #{destination_port => 2906,
            source_port => 4337,
            verification_tag => <<16#00, 16#00, 16#00, 16#00>>,
            checksum => <<16#0a, 16#ef, 16#79, 16#63>>,
            chunks =>
                [#{advertised_receiver_window_credit => 204800,
                   chunk_type => init,
                   initial_tsn => 2756892965,
                   initiate_tag => 1776726900,
                   number_of_inbound_streams => 65535,
                   number_of_outbound_streams => 10,
                   parameters =>
                       [{supported_address_types, [ipv4]},
                        {ecn_capable, true},
                        {forward_tsn_supported, true},
                        {unrecognised, {16#FFFF, <<>>}}]}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

init_ack_test() ->
    Bin = <<16#10, 16#f1, 16#0b, 16#5a,
            16#83, 16#50, 16#9b, 16#69,
            16#0c, 16#76, 16#a2, 16#31,

            16#02, 16#00, 16#00, 16#28,
            16#77, 16#c7, 16#5b, 16#58,
            16#00, 16#03, 16#20, 16#00,
            16#00, 16#02, 16#00, 16#02,
            16#9a, 16#39, 16#b9, 16#00,
            16#00, 16#07, 16#00, 16#08,
            16#01, 16#02, 16#03, 16#04,
            16#80, 16#00, 16#00, 16#04,
            16#c0, 16#00, 16#00, 16#04,
            16#FF, 16#FF, 16#00, 16#04>>,

    Exp = #{destination_port => 2906,
            source_port => 4337,
            verification_tag => <<16#83, 16#50, 16#9b, 16#69>>,
            checksum => <<16#0c, 16#76, 16#a2, 16#31>>,
            chunks =>
                [#{advertised_receiver_window_credit => 204800,
                   chunk_type => init_ack,
                   initial_tsn => 2587474176,
                   initiate_tag => 2009553752,
                   number_of_inbound_streams => 2,
                   number_of_outbound_streams => 2,
                   parameters =>
                       [{state_cookie, <<1, 2, 3, 4>>},
                        {ecn_capable, true},
                        {forward_tsn_supported, true},
                        {unrecognised, {16#FFFF, <<>>}}]}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

sack_test() ->
    Bin = <<16#0b, 16#5b, 16#bc, 16#32,
            16#29, 16#a6, 16#97, 16#12,
            16#7f, 16#ad, 16#03, 16#cb,

            16#03, 16#00, 16#00, 16#10,
            16#82, 16#46, 16#8d, 16#d7,
            16#00, 16#00, 16#80, 16#00,
            16#00, 16#00, 16#00, 16#00>>,
    Exp = #{destination_port => 48178,
            source_port => 2907,
            verification_tag => <<16#29, 16#a6, 16#97, 16#12>>,
            checksum => <<16#7f, 16#ad, 16#03, 16#cb>>,
            chunks =>
                [#{advertised_receiver_window_credit => 32768,
                   chunk_type => sack,
                   cumulative_tsn => 2185661911,
                   duplicate_tsn => [],
                   gap_ack_blocks => []}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

heartbeat_test() ->
    Bin = <<16#10, 16#f1, 16#0b, 16#5a,
            16#6b, 16#93, 16#09, 16#8b,
            16#53, 16#cc, 16#a2, 16#2e,

            16#04, 16#00, 16#00, 16#0c,
            16#00, 16#01, 16#00, 16#08,
            16#01, 16#02, 16#03, 16#04>>,
    Exp = #{destination_port => 2906,
            source_port => 4337,
            verification_tag => <<16#6b, 16#93, 16#09, 16#8b>>,
            checksum => <<16#53, 16#cc, 16#a2, 16#2e>>,
            chunks =>
                [#{chunk_type => heartbeat,
                   info => <<1, 2, 3, 4>>}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

heartbeat_ack_test() ->
    Bin = <<16#0b, 16#5a, 16#10, 16#f1,
            16#97, 16#ee, 16#b8, 16#f0,
            16#5c, 16#e9, 16#65, 16#83,

            16#05, 16#00, 16#00, 16#0c,
            16#00, 16#01, 16#00, 16#08,
            16#01, 16#02, 16#03, 16#04>>,
    Exp = #{destination_port => 4337,
            source_port => 2906,
            verification_tag => <<16#97, 16#ee, 16#b8, 16#f0>>,
            checksum => <<16#5c, 16#e9, 16#65, 16#83>>,
            chunks =>
                [#{chunk_type => heartbeat_ack,
                   info => <<1, 2, 3, 4>>}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

abort_test() ->
    Bin = <<16#0b, 16#5a, 16#10, 16#f1,
            16#69, 16#e6, 16#b3, 16#74,
            16#17, 16#fd, 16#b5, 16#3c,

            16#06, 16#00, 16#00, 16#04>>,
    Exp = #{destination_port => 4337,
            source_port => 2906,
            verification_tag => <<16#69, 16#e6, 16#b3, 16#74>>,
            checksum => <<16#17, 16#fd, 16#b5, 16#3c>>,
            chunks =>
                [#{chunk_type => abort,
                   error_causes => [],
                   tcb_destroyed => true}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

cookie_echo_test() ->
    Bin = <<16#0b, 16#5a, 16#10, 16#f1,
            16#77, 16#c7, 16#5b, 16#58,
            16#11, 16#17, 16#47, 16#65,

            16#0a, 16#00, 16#00, 16#08,
            16#01, 16#02, 16#03, 16#04>>,
    Exp = #{destination_port => 4337,
            source_port => 2906,
            verification_tag => <<16#77, 16#c7, 16#5b, 16#58>>,
            checksum => <<16#11, 16#17, 16#47, 16#65>>,
            chunks =>
                [#{chunk_type => cookie_echo,
                   cookie => <<1,2,3,4>>}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

cookie_ack_test() ->
    Bin = <<16#10, 16#f1, 16#0b, 16#5a,
            16#83, 16#50, 16#9b, 16#69,
            16#30, 16#5d, 16#3e, 16#e0,

            16#0b, 16#00, 16#00, 16#04>>,
    Exp = #{destination_port => 2906,
            source_port => 4337,
            verification_tag => <<16#83, 16#50, 16#9b, 16#69>>,
            checksum => <<16#30, 16#5d, 16#3e, 16#e0>>,
            chunks => [#{chunk_type => cookie_ack}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).

multichunk_test() ->
    Bin = <<16#10, 16#f1, 16#0b, 16#5a,
            16#83, 16#50, 16#9b, 16#69,
            16#30, 16#5d, 16#3e, 16#e0,

            16#0b, 16#00, 16#00, 16#04,

            16#05, 16#00, 16#00, 16#0c,
            16#00, 16#01, 16#00, 16#08,
            16#01, 16#02, 16#03, 16#04,

            16#05, 16#00, 16#00, 16#0c,
            16#00, 16#01, 16#00, 16#08,
            16#04, 16#03, 16#02, 16#01,

            16#05, 16#00, 16#00, 16#0c,
            16#00, 16#01, 16#00, 16#08,
            16#DE, 16#AD, 16#BE, 16#EF>>,
    Exp = #{destination_port => 2906,
            source_port => 4337,
            verification_tag => <<16#83, 16#50, 16#9b, 16#69>>,
            checksum => <<16#30, 16#5d, 16#3e, 16#e0>>,
            chunks => [#{chunk_type => cookie_ack},
                       #{chunk_type => heartbeat_ack,
                         info => <<1, 2, 3, 4>>},
                       #{chunk_type => heartbeat_ack,
                         info => <<4, 3, 2, 1>>},
                       #{chunk_type => heartbeat_ack,
                         info => <<16#DE, 16#AD, 16#BE, 16#EF>>}]},
    Val = otc_sctp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sctp:encode(Val),
    ?assertEqual(Bin, NewBin).
