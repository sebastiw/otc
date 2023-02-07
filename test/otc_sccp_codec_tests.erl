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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            data => D,
            message_type => udt,
            protocol_class => #{class => 0, options => return_on_error}},

    Val = otc_sccp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Bin, NewBin).

xudt_test() ->
    CdPA = called_party_address(),
    CgPA = calling_party_address(),
    D = data(),

    Bin = <<16#11, 16#81, 16#0f, 16#04, 16#0f, 16#1a, 16#00,
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            data => D,
            hop_counter => 15,
            message_type => xudt,
            protocol_class => #{class => 1, options => return_on_error}},

    Val = otc_sccp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Bin, NewBin).

xudt_opt_segmentation_test() ->
      Bin = <<17,1,13,4,15,26,30,11,18,8,0,18,4,100,39,17,34,17,34,11,18,8,
              0,17,8,100,39,137,120,103,6,4,0,70,0,69,16,4,64,235,77,1,0>>,
      Exp =
        #{called_party_address =>
            #{global_title =>
                      #{encoding_scheme => bcd,
                        address => "467211221122",
                        nature_of_address_indicator => 4,
                        numbering_plan => 1,
                        odd_even_indicator => even,
                        translation_type => 0},
              global_title_indicator => 4,
              national_use_indicator => 0,point_code => undefined,
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
              global_title_indicator => 4,
              national_use_indicator => 0,point_code => undefined,
              routing_indicator => global_title,
              subsystem_number => msc},
        data => <<0,70,0,69>>,
        hop_counter => 13,message_type => xudt,
        protocol_class => #{class => 1,options => no_options},
        segmentation =>
            #{class => 1,first_segment_indication => true,
              local_reference => <<235,77,1>>,
              remaining_segments => 0}},
      Val = otc_sccp:decode(Bin),
      ?assertEqual(Exp, Val),
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => msc},
            data => D,
            hop_counter => 15,
            message_type => xudts,
            return_cause => no_translation_for_this_specific_address},

    Val = otc_sccp:decode(Bin),
    ?assertEqual(Exp, Val),
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
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
                  global_title_indicator => 4,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => global_title,
                  subsystem_number => hlr},
            data => D,
            hop_counter => 15,
            message_type => xudts,
            return_cause => unqualified},

    Val = otc_sccp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sccp:encode(Val),
    ?assertEqual(Expected, NewBin).

udt_scmg_test() ->                              % sccp mgmt sst
    CdPA = <<16#42, %% AddressIndicator
             16#01>>, %% SSN
    CgPA = <<16#42, %% AddressIndicator
             16#01>>, %% SSN
    D = <<16#03, %% SCMG MsgType
          16#06, %% Affected SSN
          16#12, 16#34, %% Affected PC
          16#00>>, %% Subsystem multiplicity indicator

    Bin = <<16#09, %% SCCP MsgType,
            16#80, %% ProtocolClass
            16#03, 16#05, 16#07, %% Pointers
            16#02, CdPA/binary,
            16#02, CgPA/binary,
            16#05, D/binary>>,
    Exp = #{called_party_address =>
                #{global_title => undefined,
                  global_title_indicator => 0,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => subsystem_number,
                  subsystem_number => management},
            calling_party_address =>
                #{global_title => undefined,
                  global_title_indicator => 0,
                  national_use_indicator => 0,
                  point_code => undefined,
                  routing_indicator => subsystem_number,
                  subsystem_number => management},
            data => #{format_identifier => status_test,
                      affected_point_code => <<18,52>>,
                      affected_subsystem_number => hlr,
                      subsystem_multiplicity_indicator => 0
                     },
            message_type => udt,
            protocol_class => #{class => 0, options => return_on_error}},

    Val = otc_sccp:decode(Bin),
    ?assertEqual(Exp, Val),
    NewBin = otc_sccp:encode(Val),
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
