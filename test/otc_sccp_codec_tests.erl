-module(otc_sccp_codec_tests).

-include_lib("eunit/include/eunit.hrl").

udt_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#09, %% MsgType,
            16#80, %% ProtocolClass
            16#03, 16#0e, 16#19, %% Pointers
            16#0b, CdPA/binary,
            16#0b, CgPA/binary,
            16#18, D/binary>>,
    Exp = #{called_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            calling_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "46729887766",
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            message_type => udt,
            protocol_class => #{class => 0, options => return_on_error}},

    Val = otc_sccp:decode(Bin),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Bin, NewBin).

xudt_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#11, 16#81, 16#0f,
            16#04, 16#0f, 16#1a, 16#00, %% Pointers
            16#0b, CdPA/binary,
            16#0b, CgPA/binary,
            16#18, D/binary>>,
    Exp = #{called_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            calling_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "46729887766",
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            hop_counter => 15,
            message_type => xudt,
            protocol_class => #{class => 1, options => return_on_error}},

    Val = otc_sccp:decode(Bin),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Bin, NewBin).

xudt_opt_segmentation_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#11, 16#01, 16#0D,
            16#04, 16#0F, 16#1A, 16#32, %% pointers
            16#0B, CdPA/binary,
            16#0B, CgPA/binary,
            16#18, D/binary,
            16#10, 16#04, 16#01, 16#02, 16#03, 16#04, %% segmentation
            16#00>>, %% end of optional parameters

    Exp = #{called_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            calling_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "46729887766",
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            hop_counter => 13,
            message_type => xudt,
            protocol_class => #{class => 1, options => no_options},
            segmentation =>
                #{class => 0,
                  first_segment_indication => true,
                  local_reference => <<16#02, 16#03, 16#04>>,
                  remaining_segments => 1}},
      Val = otc_sccp:decode(Bin),
      ?assertEqual({Exp, D}, Val),
      NewBin = otc_sccp:encode(Val),
      ?assertEqual(Bin, NewBin).

udts_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#0a, 16#01, 16#03, 16#0e, 16#19,
            16#0b, CgPA/binary,
            16#0b, CdPA/binary,
            16#18, D/binary>>,
    Exp = #{called_party_address =>
                #{global_title =>
                      #{address => "46729887766",
                        encoding_scheme => bcd,
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            calling_party_address =>
                #{global_title =>
                      #{address => "467211221122",
                        encoding_scheme => bcd,
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            message_type => udts,
            return_cause => no_translation_for_this_specific_address},

    Val = otc_sccp:decode(Bin),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Bin, NewBin).

xudts_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#12, 16#01, 16#0f, 16#04, 16#0f, 16#1a, 16#00,
            16#0b, CgPA/binary,
            16#0b, CdPA/binary,
            16#18, D/binary>>,

    Exp = #{called_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "46729887766",
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            calling_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            hop_counter => 15,
            message_type => xudts,
            return_cause => no_translation_for_this_specific_address},

    Val = otc_sccp:decode(Bin),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val),
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
    Exp = #{called_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            calling_party_address =>
                #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "46729887766",
                        nature_of_address_indicator => 8,
                        numbering_plan => 1,
                        odd_even_indicator => odd,
                        translation_type => 0},
                  national_use_indicator => false,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            hop_counter => 15,
            message_type => xudts,
            return_cause => unqualified},

    Val = otc_sccp:decode(Bin),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Expected, NewBin).

xudts_ansi_test() ->
    CdPA = <<16#89, 16#07, 16#0E, 16#11, 16#32, 16#54, 16#76, 16#98, 16#00>>,
    CgPA = <<16#89, 16#06, 16#0E, 16#51, 16#34, 16#12, 16#89, 16#67, 16#05>>,

    D = data(),

    Bin = <<16#11, 16#80, 16#05, 16#04, 16#0D, 16#16, 16#00,
            16#09, CgPA/binary,
            16#09, CdPA/binary,
            16#18, D/binary>>,

    Exp = #{message_type => xudt,
            calling_party_address =>
                #{subsystem_number => vlr,
                  routing_indicator => global_title,
                  global_title =>
                      #{address => "11234567890",
                        translation_type => 14},
                  national_use_indicator => true,
                  point_code => undefined},
            called_party_address =>
                #{subsystem_number => hlr,
                  routing_indicator => global_title,
                  global_title =>
                      #{address => "15432198765",
                        translation_type => 14},
                  national_use_indicator => true,
                  point_code => undefined},
            protocol_class => #{options => return_on_error,
                                class => 0},
            hop_counter => 5},

    Val = otc_sccp:decode(Bin, #{address_type => ansi}),
    ?assertEqual({Exp, D}, Val),
    NewBin = otc_sccp:encode(Val, #{address_type => ansi}),
    ?assertEqual(Bin, NewBin).

called_party_address() ->
    <<16#12, %% AddressIndicator
      16#08, %% SSN
      16#00, %% TT
      16#12, %% NP/ES
      16#04, %% NI
      16#64, 16#27, 16#11, 16#22, 16#11, 16#22>>.
calling_party_address() ->
    <<16#12, %% AddressIndicator
      16#06, %% SSN
      16#00, %% TT
      16#11, %% NP/ES
      16#08, %% NI
      16#64, 16#27, 16#89, 16#78, 16#67, 16#06>>.
data() ->
    <<16#01, 16#02, 16#03, 16#04, 16#05, 16#06, 16#07, 16#08,
      16#09, 16#0a, 16#0b, 16#0c, 16#0d, 16#0e, 16#0f, 16#10,
      16#11, 16#12, 16#13, 16#14, 16#15, 16#16, 16#17, 16#18>>.
