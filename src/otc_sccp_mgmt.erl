-module(otc_sccp_mgmt).
-behaviour(otc_codec).

-export([spec/0,
         codec/2,
         next/1,
         decode/1,
         encode/1
        ]).

-include("include/sccp.hrl").
-include_lib("eunit/include/eunit.hrl").

spec() ->
    "ITU-T Q.713 (03/2001)".

codec(Bin, _Opts) when is_binary(Bin) ->
    decode(Bin);
codec(Map, _Opts) when is_map(Map) ->
    encode(Map);
codec({Map, _}, _Opts) ->
    encode(Map).

next(_) -> '$stop'.

decode(<<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => allowed,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => prohibited,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => status_test,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => out_of_service_request,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, ASSN:8, APC:2/binary, SMI:8>>) ->
    #{format_identifier => out_of_service_grant,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI};
decode(<<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, ASSN:8, APC:2/binary, SMI:8, CL:8>>) ->
    #{format_identifier => congested,
      affected_subsystem_number => otc_sccp:parse_ssn(ASSN),
      affected_point_code => APC,
      subsystem_multiplicity_indicator => SMI,
      congestion_level => CL}.

encode(#{format_identifier := allowed,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_ALLOWED:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode(#{format_identifier := prohibited,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_PROHIBITED:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode(#{format_identifier := status_test,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_STATUS_TEST:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode(#{format_identifier := out_of_service_request,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_REQUEST:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode(#{format_identifier := out_of_service_grant,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI}) ->
    <<?SCCP_SCMG_SUBSYSTEM_OUT_OF_SERVICE_GRANT:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8>>;
encode(#{format_identifier := congested,
         affected_subsystem_number := ASSN,
         affected_point_code := APC,
         subsystem_multiplicity_indicator := SMI,
         congestion_level := CL}) ->
    <<?SCCP_SCMG_SUBSYSTEM_CONGESTED:8, (otc_sccp:compose_ssn(ASSN)):8, APC:2/binary, SMI:8, CL:8>>.
