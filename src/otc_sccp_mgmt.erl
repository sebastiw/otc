-module(otc_sccp_mgmt).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         decode/2,
         encode/1,
         encode/2
        ]).

-deprecated([{decode, 1, "Use decode/2 instead."}]).

-include("include/sccp.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "ITU-T Q.713 (03/2001)".

codec(Bin, Opts) when is_binary(Bin) ->
    decode(Bin, Opts);
codec(Map, Opts) when is_map(Map) ->
    encode(Map, Opts);
codec({Map, _}, Opts) ->
    encode(Map, Opts).

next(_) -> '$stop'.

decode(Bin) ->
    decode(Bin, #{}).

decode(<<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => allowed,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, ASSN:8, APC:2/binary, SMI:8>>, Opts) ->
    #{format_identifier => allowed,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => prohibited,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, ASSN:8, APC:2/binary, SMI:8>>, Opts) ->
    #{format_identifier => prohibited,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => status_test,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, ASSN:8, APC:2/binary, SMI:8>>, Opts) ->
    #{format_identifier => status_test,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => out_of_service_request,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, ASSN:8, APC:2/binary, SMI:8>>, Opts) ->
    #{format_identifier => out_of_service_request,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => out_of_service_grant,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, ASSN:8, APC:2/binary, SMI:8>>, Opts) ->
    #{format_identifier => out_of_service_grant,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, ASSN:8, APC:2/binary, SMI:8, CL:8>>, Opts) ->
    #{format_identifier => congested,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI,
      congestion_level => CL};
decode(<<?SCCP_SCMG_ANSI_SUBSYSTEM_BACKUP_ROUTING:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => backup_routing,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_ANSI_SUBSYSTEM_NORMAL_ROUTING:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => normal_routing,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_ANSI_SUBSYSTEM_ROUTING_STATUS_TEST:8, ASSN:8, APC:3/binary, SMI:8>>, #{address_type := ansi} = Opts) ->
    #{format_identifier => routing_status_test,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => otc_sccp:decode_pc(APC, Opts),
      subsystem_multiplicity_indicator => SMI}.

encode(Msg) ->
    encode(Msg, #{}).

encode(#{format_identifier := allowed,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := prohibited,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := status_test,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := out_of_service_request,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := out_of_service_grant,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := congested,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI,
         congestion_level := CL}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, ASSNBin:8, APCBin/binary, SMI:8, CL:8>>;
encode(#{format_identifier := backup_routing,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_ANSI_SUBSYSTEM_BACKUP_ROUTING:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := normal_routing,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_ANSI_SUBSYSTEM_NORMAL_ROUTING:8, ASSNBin:8, APCBin/binary, SMI:8>>;
encode(#{format_identifier := routing_status_test,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}, Opts) ->
    ASSNBin = otc_sccp:compose_ssn(ASSN),
    APCBin = otc_sccp:encode_pc(APC, Opts),
    <<?SCCP_SCMG_ANSI_SUBSYSTEM_ROUTING_STATUS_TEST:8, ASSNBin:8, APCBin/binary, SMI:8>>.
