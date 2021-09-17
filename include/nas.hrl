%% 3GPP TS TS 24.301 version 17.3.0 Release 17

%% Chapter 8 Message types

%% 9.8.1 Message types for EPS mobility management

%% EPS_MOBILITY_MANAGEMENT_MESSAGES 01------
-define(NAS_MSGT_ATTACH_REQUEST, 2#01000001).
-define(NAS_MSGT_ATTACH_ACCEPT, 2#01000010).
-define(NAS_MSGT_ATTACH_COMPLETE, 2#01000011).
-define(NAS_MSGT_ATTACH_REJECT, 2#01000100).
-define(NAS_MSGT_DETACH_REQUEST, 2#01000101).
-define(NAS_MSGT_DETACH_ACCEPT, 2#01000110).
-define(NAS_MSGT_TRACKING_AREA_UPDATE_REQUEST, 2#01001000).
-define(NAS_MSGT_TRACKING_AREA_UPDATE_ACCEPT, 2#01001001).
-define(NAS_MSGT_TRACKING_AREA_UPDATE_COMPLETE, 2#01001010).
-define(NAS_MSGT_TRACKING_AREA_UPDATE_REJECT, 2#01001011).
-define(NAS_MSGT_EXTENDED_SERVICE_REQUEST, 2#01001100).
-define(NAS_MSGT_CONTROL_PLANE_SERVICE_REQUEST, 2#01001101).
-define(NAS_MSGT_SERVICE_REJECT, 2#01001110).
-define(NAS_MSGT_SERVICE_ACCEPT, 2#01001111).
-define(NAS_MSGT_GUTI_REALLOCATION_COMMAND, 2#01010000).
-define(NAS_MSGT_GUTI_REALLOCATION_COMPLETE, 2#01010001).
-define(NAS_MSGT_AUTHENTICATION_REQUEST, 2#01010010).
-define(NAS_MSGT_AUTHENTICATION_RESPONSE, 2#01010011).
-define(NAS_MSGT_AUTHENTICATION_REJECT, 2#01010100).
-define(NAS_MSGT_AUTHENTICATION_FAILURE, 2#01011100).
-define(NAS_MSGT_IDENTITY_REQUEST, 2#01010101).
-define(NAS_MSGT_IDENTITY_RESPONSE, 2#01010110).
-define(NAS_MSGT_SECURITY_MODE_COMMAND, 2#01011101).
-define(NAS_MSGT_SECURITY_MODE_COMPLETE, 2#01011110).
-define(NAS_MSGT_SECURITY_MODE_REJECT, 2#01011111).
-define(NAS_MSGT_EMM_STATUS, 2#01100000).
-define(NAS_MSGT_EMM_INFORMATION, 2#01100001).
-define(NAS_MSGT_DOWNLINK_NAS_TRANSPORT, 2#01100010).
-define(NAS_MSGT_UPLINK_NAS_TRANSPORT, 2#01100011).
-define(NAS_MSGT_CS_SERVICE_NOTIFICATION, 2#01100100).
-define(NAS_MSGT_DOWNLINK_GENERIC_NAS_TRANSPORT, 2#01101000).
-define(NAS_MSGT_UPLINK_GENERIC_NAS_TRANSPORT, 2#01101001).

%% EPS_SESSION_MANAGEMENT_MESSAGES 11------
-define(NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REQUEST, 2#11000001).
-define(NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_ACCEPT, 2#11000010).
-define(NAS_MSGT_ACTIVATE_DEFAULT_EPS_BEARER_CONTEXT_REJECT, 2#11000011).
-define(NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REQUEST, 2#11000101).
-define(NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_ACCEPT, 2#11000110).
-define(NAS_MSGT_ACTIVATE_DEDICATED_EPS_BEARER_CONTEXT_REJECT, 2#11000111).
-define(NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REQUEST, 2#11001001).
-define(NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_ACCEPT, 2#11001010).
-define(NAS_MSGT_MODIFY_EPS_BEARER_CONTEXT_REJECT, 2#11001011).
-define(NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_REQUEST, 2#11001101).
-define(NAS_MSGT_DEACTIVATE_EPS_BEARER_CONTEXT_ACCEPT, 2#11001110).
-define(NAS_MSGT_PDN_CONNECTIVITY_REQUEST, 2#11010000).
-define(NAS_MSGT_PDN_CONNECTIVITY_REJECT, 2#11010001).
-define(NAS_MSGT_PDN_DISCONNECT_REQUEST, 2#11010010).
-define(NAS_MSGT_PDN_DISCONNECT_REJECT, 2#11010011).
-define(NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REQUEST, 2#11010100).
-define(NAS_MSGT_BEARER_RESOURCE_ALLOCATION_REJECT, 2#11010101).
-define(NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REQUEST, 2#11010110).
-define(NAS_MSGT_BEARER_RESOURCE_MODIFICATION_REJECT, 2#11010111).
-define(NAS_MSGT_ESM_INFORMATION_REQUEST, 2#11011001).
-define(NAS_MSGT_ESM_INFORMATION_RESPONSE, 2#11011010).
-define(NAS_MSGT_NOTIFICATION, 2#11011011).
-define(NAS_MSGT_ESM_DUMMY_MESSAGE, 2#11011100).
-define(NAS_MSGT_ESM_STATUS, 2#11101000).
-define(NAS_MSGT_REMOTE_UE_REPORT, 2#11101001).
-define(NAS_MSGT_REMOTE_UE_REPORT_RESPONSE, 2#11101010).
-define(NAS_MSGT_ESM_DATA_TRANSPORT, 2#11101011).

%% 9.3.1 Security header type 
-define(NAS_SHT_PLAIN_NAS_MESSAGE, 2#0000).
-define(NAS_SHT_INTEGRITY_PROTECTED, 2#0001).
-define(NAS_SHT_INTEGRITY_PROTECTED_CIPHERED, 2#0010).
-define(NAS_SHT_INTEGRITY_PROTECTED_EPS_SECURITY, 2#0011).
-define(NAS_SHT_INTEGRITY_PROTECTED_CIPHERED_EPS_SECURITY, 2#0100).
-define(NAS_SHT_INTEGRITY_PROTECTED_PARTIALLY_CIPHERED_NAS_MESSAGE, 2#0101).
-define(NAS_SHT_SERVICE_REQUEST, 2#1100).

-record(nas_eps_mobility_mgmt_msg,
        {security_header_type,
         msg_auth_code, %% not included in plain
         sequence_number,
         nas_msg %% only in plain
        }).

-record(nas_eps_session_mgmt_msg,
        {eps_bearer_id,
         procedure_transaction_id,
         msg_type,
         other_ies
        }).

-record(nas_msg_attach_accept,
        {protocol_discriminator,
         security_header_type,
         attach_accept_message_identity,
         eps_attach_result,
         spare_half_octet,
         t3412_value,
         tai_list,
         esm_message_container,
         guti,
         location_area_identification,
         ms_identity,
         emm_cause,
         t3402_value,
         t3423_value,
         equivalent_plmns,
         emergency_number_list,
         eps_network_feature_support,
         additional_update_result,
         t3412_extended_value,
         t3324_value,
         extended_drx_parameters,
         dcn_id,
         sms_services_status,
         non_3gpp_nw_provided_policies,
         t3448_value,
         network_policy,
         t3447_value,
         extended_emergency_number_list,
         ciphering_key_data,
         ue_radio_capability_id,
         ue_radio_capability_id_deletion_indication,
         negotiated_wus_assistance_information,
         negotiated_drx_parameter_in_nb_s1_mode,
         negotiated_imsi_offset
        }).
-record(nas_msg_attach_complete,
        {protocol_discriminator,
         security_header_type,
         attach_complete_message_identity,
         esm_message_container
        }).
-record(nas_msg_attach_reject,
        {protocol_discriminator,
         security_header_type,
         attach_reject_message_identity,
         emm_cause,
         esm_message_container,
         t3346_value,
         t3402_value,
         extended_emm_cause
        }).
-record(nas_msg_attach_request,
        {protocol_discriminator,
         security_header_type,
         attach_request_message_identity,
         eps_attach_type,
         nas_key_set_identifier,
         eps_mobile_identity,
         ue_network_capability,
         esm_message_container,
         old_p_tmsi_signature,
         additional_guti,
         last_visited_registered_tai,
         drx_parameter,
         ms_network_capability,
         old_location_area_identification,
         tmsi_status,
         mobile_station_classmark_2,
         mobile_station_classmark_3,
         supported_codecs,
         additional_update_type,
         voice_domain_preference_and_ues_usage_setting,
         device_properties,
         old_guti_type,
         ms_network_feature_support,
         tmsi_based_nri_container,
         t3324_value,
         t3412_extended_value,
         extended_drx_parameters,
         ue_additional_security_capability,
         ue_status,
         additional_information_requested,
         n1_ue_network_capability,
         ue_radio_capability_id_availability,
         requested_wus_assistance_information,
         drx_parameter_in_nb_s1_mode,
         requested_imsi_offset
        }).
-record(nas_msg_authentication_failure,
        {protocol_discriminator,
         security_header_type,
         authentication_failure,
         emm_cause,
         authentication_failure_parameter
        }).
-record(nas_msg_authentication_reject,
        {protocol_discriminator,
         security_header_type,
         authentication_reject_message_type
        }).
-record(nas_msg_authentication_request,
        {protocol_discriminator,
         security_header_type,
         authentication_request_message_type,
         nas_key_set_identifierasme,
         spare_half_octet,
         authentication_parameter_rand__eps_challenge,
         authentication_parameter_autn_eps_challenge
        }).
-record(nas_msg_authentication_response,
        {protocol_discriminator,
         security_header_type,
         authentication_response_message_type,
         authentication_response_parameter
        }).
-record(nas_msg_cs_service_notification,
        {protocol_discriminator,
         security_header_type,
         cs_service_notification_message_identity,
         paging_identity,
         cli,
         ss_code,
         lcs_indicator,
         lcs_client_identity
        }).
-record(nas_msg_detach_accept_mo,
        {protocol_discriminator,
         security_header_type,
         detach_accept_message_identity
        }).
-record(nas_msg_detach_accept_mt,
        {protocol_discriminator,
         security_header_type,
         detach_accept_message_identity
        }).
-record(nas_msg_detach_request_mo,
        {protocol_discriminator,
         security_header_type,
         detach_request_message_identity,
         detach_type,
         nas_key_set_identifier,
         eps_mobile_identity
        }).
-record(nas_msg_detach_request_mt,
        {protocol_discriminator,
         security_header_type,
         detach_request_message_identity,
         detach_type,
         spare_half_octet,
         emm_cause
        }).
-record(nas_msg_downlink_nas_transport,
        {protocol_discriminator,
         security_header_type,
         downlink_nas_transport_message_identity,
         nas_message_container
        }).
-record(nas_msg_emm_information,
        {protocol_discriminator,
         security_header_type,
         emm_information_message_identity,
         full_name_for_network,
         short_name_for_network,
         local_time_zone,
         universal_time_and_local_time_zone,
         network_daylight_saving_time
        }).
-record(nas_msg_emm_status,
        {protocol_discriminator,
         security_header_type,
         emm_status_message_identity,
         emm_cause
        }).
-record(nas_msg_extended_service_request,
        {protocol_discriminator,
         security_header_type,
         extended_service_request_message_identity,
         service_type,
         nas_key_set_identifier,
         m_tmsi,
         csfb_response,
         eps_bearer_context_status,
         device_properties,
         ue_request_type,
         paging_restriction
        }).
-record(nas_msg_guti_reallocation_command,
        {protocol_discriminator,
         security_header_type,
         guti_reallocation_command_message_identity,
         guti,
         tai_list,
         dcn_id,
         ue_radio_capability_id,
         ue_radio_capability_id_deletion_indication
        }).
-record(nas_msg_guti_reallocation_complete,
        {protocol_discriminator,
         security_header_type,
         guti_reallocation_complete_message_identity
        }).
-record(nas_msg_identity_request,
        {protocol_discriminator,
         security_header_type,
         identity_request_message_identity,
         identity_type,
         spare_half_octet
        }).
-record(nas_msg_identity_response,
        {protocol_discriminator,
         security_header_type,
         identity_response_message,
         mobile_identity
        }).
-record(nas_msg_security_mode_command,
        {protocol_discriminator,
         security_header_type,
         security_mode_command_message_identity,
         selected_nas_security_algorithms,
         nas_key_set_identifier,
         spare_half_octet,
         replayed_ue_security_capabilities,
         imeisv_request,
         replayed_nonceue,
         noncemme,
         hashmme,
         replayed_ue_additional_security_capability,
         ue_radio_capability_id_request
        }).
-record(nas_msg_security_mode_complete,
        {protocol_discriminator,
         security_header_type,
         security_mode_complete_message_identity,
         imeisv,
         replayed_nas_message_container,
         ue_radio_capability_id
        }).
-record(nas_msg_security_mode_reject,
        {protocol_discriminator,
         security_header_type,
         security_mode_reject_message_identity,
         emm_cause
        }).
-record(nas_msg_security_protected_nas_message,
        {protocol_discriminator,
         security_header_type,
         message_authentication_code,
         sequence_number,
         nas_message
        }).
-record(nas_msg_service_reject,
        {protocol_discriminator,
         security_header_type,
         service_reject_message_identity,
         emm_cause,
         t3442_value,
         t3346_value,
         t3448_value
        }).
-record(nas_msg_service_request,
        {protocol_discriminator,
         security_header_type,
         ksi_and_sequence_number,
         message_authentication_code_short
        }).
-record(nas_msg_tracking_area_update_accept,
        {protocol_discriminator,
         security_header_type,
         tracking_area_update_accept_message_identity,
         eps_update_result,
         spare_half_octet,
         t3412_value,
         guti,
         tai_list,
         eps_bearer_context_status,
         location_area_identification,
         ms_identity,
         emm_cause,
         t3402_value,
         t3423_value,
         equivalent_plmns,
         emergency_number_list,
         eps_network_feature_support,
         additional_update_result,
         t3412_extended_value,
         t3324_value,
         extended_drx_parameters,
         header_compression_configuration_status,
         dcn_id,
         sms_services_status,
         non_3gpp_nw_policies,
         t3448_value,
         network_policy,
         t3447_value,
         extended_emergency_number_list,
         ciphering_key_data,
         ue_radio_capability_id,
         ue_radio_capability_id_deletion_indication,
         negotiated_wus_assistance_information,
         negotiated_drx_parameter_in_nb_s1_mode,
         negotiated_imsi_offset
        }).
-record(nas_msg_tracking_area_update_complete,
        {protocol_discriminator,
         security_header_type,
         tracking_area_update_complete_message_identity
        }).
-record(nas_msg_tracking_area_update_reject,
        {protocol_discriminator,
         security_header_type,
         tracking_area_update_reject,
         emm_cause,
         t3346_value,
         extended_emm_cause
        }).
-record(nas_msg_tracking_area_update_request,
        {protocol_discriminator,
         security_header_type,
         tracking_area_update_request_message_identity,
         eps_update_type,
         nas_key_set_identifier,
         old_guti,
         non_current_native_nas_key_set_identifier,
         gprs_ciphering_key_sequence_number,
         old_p_tmsi_signature,
         additional_guti,
         nonceue,
         ue_network_capability,
         last_visited_registered_tai,
         drx_parameter,
         ue_radio_capability_information_update_needed,
         eps_bearer_context_status,
         ms_network_capability,
         old_location_area_identification,
         tmsi_status,
         mobile_station_classmark_2,
         mobile_station_classmark_3,
         supported_codecs,
         additional_update_type,
         voice_domain_preference_and_ues_usage_setting,
         old_guti_type,
         device_properties,
         ms_network_feature_support,
         tmsi_based_nri_container,
         t3324_value,
         t3412_extended_value,
         extended_drx_parameters,
         ue_additional_security_capability,
         ue_status,
         additional_information_requested,
         n1_ue_network_capability,
         ue_radio_capability_id_availability,
         requested_wus_assistance_information,
         drx_parameter_in_nb_s1_mode,
         requested_imsi_offset,
         ue_request_type,
         paging_restriction
        }).
-record(nas_msg_uplink_nas_transport,
        {protocol_discriminator,
         security_header_type,
         uplink_nas_transport_message_identity,
         nas_message_container
        }).
-record(nas_msg_downlink_generic_nas_transport,
        {protocol_discriminator,
         security_header_type,
         downlink_generic_nas_transport_message_identity,
         generic_message_container_type,
         generic_message_container,
         additional_information
        }).
-record(nas_msg_uplink_generic_nas_transport,
        {protocol_discriminator,
         security_header_type,
         uplink_generic_nas_transport_message_identity,
         generic_message_container_type,
         generic_message_container,
         additional_information
        }).
-record(nas_msg_control_plane_service_request,
        {protocol_discriminator,
         security_header_type,
         control_plane_service_request_message_identity,
         control_plane_service_type,
         nas_key_set_identifier,
         esm_message_container,
         nas_message_container,
         eps_bearer_context_status,
         device_properties,
         ue_request_type,
         paging_restriction
        }).
-record(nas_msg_service_accept,
        {protocol_discriminator,
         security_header_type,
         service_accept_message_identity,
         eps_bearer_context_status,
         t3448_value
        }).
-record(nas_msg_activate_dedicated_eps_bearer_context_accept,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_dedicated_eps_bearer_context_accept_message_identity,
         protocol_configuration_options,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_activate_dedicated_eps_bearer_context_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_dedicated_eps_bearer_context_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_activate_dedicated_eps_bearer_context_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_dedicated_eps_bearer_context_request_message_identity,
         linked_eps_bearer_identity,
         spare_half_octet,
         eps_qos,
         tft,
         transaction_identifier,
         negotiated_qos,
         negotiated_llc_sapi,
         radio_priority,
         packet_flow_identifier,
         protocol_configuration_options,
         wlan_offload_indication,
         nbifom_container,
         extended_protocol_configuration_options,
         extended_eps_qos
        }).
-record(nas_msg_activate_default_eps_bearer_context_accept,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_default_eps_bearer_context_accept_message_identity,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_activate_default_eps_bearer_context_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_default_eps_bearer_context_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_activate_default_eps_bearer_context_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         activate_default_eps_bearer_context_request_message_identity,
         eps_qos,
         access_point_name,
         pdn_address,
         transaction_identifier,
         negotiated_qos,
         negotiated_llc_sapi,
         radio_priority,
         packet_flow_identifier,
         apn_ambr,
         esm_cause,
         protocol_configuration_options,
         connectivity_type,
         wlan_offload_indication,
         nbifom_container,
         header_compression_configuration,
         control_plane_only_indication,
         extended_protocol_configuration_options,
         serving_plmn_rate_control,
         extended_apn_ambr
        }).
-record(nas_msg_bearer_resource_allocation_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         bearer_resource_allocation_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         back_off_timer_value,
         re_attempt_indicator,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_bearer_resource_allocation_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         bearer_resource_allocation_request_message_identity,
         linked_eps_bearer_identity,
         spare_half_octet,
         traffic_flow_aggregate,
         required_traffic_flow_qos,
         protocol_configuration_options,
         device_properties,
         nbifom_container,
         extended_protocol_configuration_options,
         extended_eps_qos
        }).
-record(nas_msg_bearer_resource_modification_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         bearer_resource_modification_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         back_off_timer_value,
         re_attempt_indicator,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_bearer_resource_modification_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         bearer_resource_modification_request_message_identity,
         eps_bearer_identity_for_packet_filter,
         spare_half_octet,
         traffic_flow_aggregate,
         required_traffic_flow_qos,
         esm_cause,
         protocol_configuration_options,
         device_properties,
         nbifom_container,
         header_compression_configuration,
         extended_protocol_configuration_options,
         extended_eps_qos
        }).
-record(nas_msg_deactivate_eps_bearer_context_accept,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         deactivate_eps_bearer_context_accept_message_identity,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_deactivate_eps_bearer_context_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         deactivate_eps_bearer_context_request_message_identity,
         esm_cause,
         protocol_configuration_options,
         t3396_value,
         wlan_offload_indication,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_esm_dummy_message,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         esm_dummy_message
        }).
-record(nas_msg_esm_information_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         esm_information_request_message_identity
        }).
-record(nas_msg_esm_information_response,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         esm_information_response_message_identity,
         access_point_name,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_esm_status,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         esm_status_message_identity,
         esm_cause
        }).
-record(nas_msg_modify_eps_bearer_context_accept,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         modify_eps_bearer_context_accept_message_identity,
         protocol_configuration_options,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_modify_eps_bearer_context_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         modify_eps_bearer_context_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_modify_eps_bearer_context_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         modify_eps_bearer_context_request_message_identity,
         new_eps_qos,
         tft,
         new_qos,
         negotiated_llc_sapi,
         radio_priority,
         packet_flow_identifier,
         apn_ambr,
         protocol_configuration_options,
         wlan_offload_indication,
         nbifom_container,
         header_compression_configuration,
         extended_protocol_configuration_options,
         extended_apn_ambr,
         extended_eps_qos
        }).
-record(nas_msg_notification,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         notification_message_identity,
         notification_indicator
        }).
-record(nas_msg_pdn_connectivity_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         pdn_connectivity_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         back_off_timer_value,
         re_attempt_indicator,
         nbifom_container,
         extended_protocol_configuration_options
        }).
-record(nas_msg_pdn_connectivity_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         pdn_connectivity_request_message_identity,
         request_type,
         pdn_type,
         esm_information_transfer_flag,
         access_point_name,
         protocol_configuration_options,
         device_properties,
         nbifom_container,
         header_compression_configuration,
         extended_protocol_configuration_options
        }).
-record(nas_msg_pdn_disconnect_reject,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         pdn_disconnect_reject_message_identity,
         esm_cause,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_pdn_disconnect_request,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         pdn_disconnect_request_message_identity,
         linked_eps_bearer_identity,
         spare_half_octet,
         protocol_configuration_options,
         extended_protocol_configuration_options
        }).
-record(nas_msg_remote_ue_report,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         remote_ue_report_message_identity,
         remote_ue_context_connected,
         remote_ue_context_disconnected,
         prose_key_management_function_address
        }).
-record(nas_msg_remote_ue_report_response,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         remote_ue_report_response_message_identity
        }).
-record(nas_msg_esm_data_transport,
        {protocol_discriminator,
         eps_bearer_identity,
         procedure_transaction_identity,
         esm_data_transport_message_identity,
         user_data_container,
         release_assistance_indication
        }).
