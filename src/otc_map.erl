-module(otc_map).

-behaviour(otc_codec).

-export([spec/0,
         codec/1,
         next/1,
         decode/1,
         encode/1
        ]).

-export([parse_application_context/1,
         compose_application_context/1
        ]).

-include("include/asn1_map_ac.hrl").

spec() ->
    "3GPP TS 29.002 v17.2.0".

codec({TCDialogue, #{result := Bin} = Component}) when is_binary(Bin) ->
    decode({TCDialogue, Component});
codec(Component) when is_map(Component) ->
    encode(Component).

next(_) -> '$stop'.

decode(_Bin) ->
    error(not_implemented).

encode(_Map) ->
    error(not_implemented).

parse_application_context(?'map-ac') -> 'map-ac';
parse_application_context(?'networkLocUpContext-v1') -> 'networkLocUpContext-v1';
parse_application_context(?'networkLocUpContext-v2') -> 'networkLocUpContext-v2';
parse_application_context(?'networkLocUpContext-v3') -> 'networkLocUpContext-v3';
parse_application_context(?'locationCancellation-v1') -> 'locationCancellation-v1';
parse_application_context(?'locationCancellation-v2') -> 'locationCancellation-v2';
parse_application_context(?'locationCancellationContext-v3') -> 'locationCancellationContext-v3';
parse_application_context(?'roamingNumberEnquiryContext-v1') -> 'roamingNumberEnquiryContext-v1';
parse_application_context(?'roamingNumberEnquiryContext-v2') -> 'roamingNumberEnquiryContext-v2';
parse_application_context(?'roamingNumberEnquiryContext-v3') -> 'roamingNumberEnquiryContext-v3';
parse_application_context(?'istAlertingContext-v3') -> 'istAlertingContext-v3';
parse_application_context(?'locationInfoRetrievalContext-v1') -> 'locationInfoRetrievalContext-v1';
parse_application_context(?'locationInfoRetrievalContext-v2') -> 'locationInfoRetrievalContext-v2';
parse_application_context(?'locationInfoRetrievalContext-v3') -> 'locationInfoRetrievalContext-v3';
parse_application_context(?'callControlTransferContext-v4') -> 'callControlTransferContext-v4';
parse_application_context(?'reportingContext-v3') -> 'reportingContext-v3';
parse_application_context(?'callCompletionContext-v3') -> 'callCompletionContext-v3';
parse_application_context(?'serviceTerminationContext-v3') -> 'serviceTerminationContext-v3';
parse_application_context(?'resetContext-v1') -> 'resetContext-v1';
parse_application_context(?'resetContext-v2') -> 'resetContext-v2';
parse_application_context(?'resetContext-v3') -> 'resetContext-v3';
parse_application_context(?'handoverControlContext-v1') -> 'handoverControlContext-v1';
parse_application_context(?'handoverControlContext-v2') -> 'handoverControlContext-v2';
parse_application_context(?'handoverControlContext-v3') -> 'handoverControlContext-v3';
parse_application_context(?'equipmentMngtContext-v1') -> 'equipmentMngtContext-v1';
parse_application_context(?'equipmentMngtContext-v2') -> 'equipmentMngtContext-v2';
parse_application_context(?'equipmentMngtContext-v3') -> 'equipmentMngtContext-v3';
parse_application_context(?'infoRetrievalContext-v1') -> 'infoRetrievalContext-v1';
parse_application_context(?'infoRetrievalContext-v2') -> 'infoRetrievalContext-v2';
parse_application_context(?'infoRetrievalContext-v3') -> 'infoRetrievalContext-v3';
parse_application_context(?'interVlrInfoRetrievalContext-v2') -> 'interVlrInfoRetrievalContext-v2';
parse_application_context(?'interVlrInfoRetrievalContext-v3') -> 'interVlrInfoRetrievalContext-v3';
parse_application_context(?'subscriberDataMngtContext-v1') -> 'subscriberDataMngtContext-v1';
parse_application_context(?'subscriberDataMngtContext-v2') -> 'subscriberDataMngtContext-v2';
parse_application_context(?'subscriberDataMngtContext-v3') -> 'subscriberDataMngtContext-v3';
parse_application_context(?'tracingContext-v1') -> 'tracingContext-v1';
parse_application_context(?'tracingContext-v2') -> 'tracingContext-v2';
parse_application_context(?'tracingContext-v3') -> 'tracingContext-v3';
parse_application_context(?'networkFunctionalSsContext-v1') -> 'networkFunctionalSsContext-v1';
parse_application_context(?'networkFunctionalSsContext-v2') -> 'networkFunctionalSsContext-v2';
parse_application_context(?'networkUnstructuredSsContext-v2') -> 'networkUnstructuredSsContext-v2';
parse_application_context(?'shortMsgGatewayContext-v1') -> 'shortMsgGatewayContext-v1';
parse_application_context(?'shortMsgGatewayContext-v2') -> 'shortMsgGatewayContext-v2';
parse_application_context(?'shortMsgGatewayContext-v3') -> 'shortMsgGatewayContext-v3';
parse_application_context(?'shortMsgRelayContext-v1') -> 'shortMsgRelayContext-v1';
parse_application_context(?'shortMsgMO-RelayContext-v2') -> 'shortMsgMO-RelayContext-v2';
parse_application_context(?'shortMsgMO-RelayContext-v3') -> 'shortMsgMO-RelayContext-v3';
parse_application_context(?'subscriberDataModificationNotificationContext-v3') -> 'subscriberDataModificationNotificationContext-v3';
parse_application_context(?'shortMsgAlertContext-v1') -> 'shortMsgAlertContext-v1';
parse_application_context(?'shortMsgAlertContext-v2') -> 'shortMsgAlertContext-v2';
parse_application_context(?'mwdMngtContext-v1') -> 'mwdMngtContext-v1';
parse_application_context(?'mwdMngtContext-v2') -> 'mwdMngtContext-v2';
parse_application_context(?'mwdMngtContext-v3') -> 'mwdMngtContext-v3';
parse_application_context(?'shortMsgMT-RelayContext-v2') -> 'shortMsgMT-RelayContext-v2';
parse_application_context(?'shortMsgMT-RelayContext-v3') -> 'shortMsgMT-RelayContext-v3';
parse_application_context(?'imsiRetrievalContext-v2') -> 'imsiRetrievalContext-v2';
parse_application_context(?'msPurgingContext-v2') -> 'msPurgingContext-v2';
parse_application_context(?'msPurgingContext-v3') -> 'msPurgingContext-v3';
parse_application_context(?'subscriberInfoEnquiryContext-v3') -> 'subscriberInfoEnquiryContext-v3';
parse_application_context(?'anyTimeInfoEnquiryContext-v3') -> 'anyTimeInfoEnquiryContext-v3';
parse_application_context(?'groupCallControlContext-v3') -> 'groupCallControlContext-v3';
parse_application_context(?'gprsLocationUpdateContext-v3') -> 'gprsLocationUpdateContext-v3';
parse_application_context(?'gprsLocationInfoRetrievalContext-v4') -> 'gprsLocationInfoRetrievalContext-v4';
parse_application_context(?'failureReportContext-v3') -> 'failureReportContext-v3';
parse_application_context(?'gprsNotifyContext-v3') -> 'gprsNotifyContext-v3';
parse_application_context(?'ss-InvocationNotificationContext-v3') -> 'ss-InvocationNotificationContext-v3';
parse_application_context(?'locationSvcGatewayContext-v3') -> 'locationSvcGatewayContext-v3';
parse_application_context(?'locationSvcEnquiryContext-v3') -> 'locationSvcEnquiryContext-v3';
parse_application_context(?'authenticationFailureReportContext-v3') -> 'authenticationFailureReportContext-v3';
parse_application_context(?'shortMsgMT-Relay-VGCS-Context-v3') -> 'shortMsgMT-Relay-VGCS-Context-v3';
parse_application_context(?'mm-EventReportingContext-v3') -> 'mm-EventReportingContext-v3';
parse_application_context(?'anyTimeInfoHandlingContext-v3') -> 'anyTimeInfoHandlingContext-v3';
parse_application_context(?'resourceManagementContext-v3') -> 'resourceManagementContext-v3';
parse_application_context(?'groupCallInfoRetrievalContext-v3') -> 'groupCallInfoRetrievalContext-v3';
parse_application_context(?'vcsgLocationUpdateContext-v3') -> 'vcsgLocationUpdateContext-v3';
parse_application_context(?'vcsgLocationCancellationContext-v3') -> 'vcsgLocationCancellationContext-v3'.

compose_application_context('map-ac') -> ?'map-ac';
compose_application_context('networkLocUpContext-v1') -> ?'networkLocUpContext-v1';
compose_application_context('networkLocUpContext-v2') -> ?'networkLocUpContext-v2';
compose_application_context('networkLocUpContext-v3') -> ?'networkLocUpContext-v3';
compose_application_context('locationCancellation-v1') -> ?'locationCancellation-v1';
compose_application_context('locationCancellation-v2') -> ?'locationCancellation-v2';
compose_application_context('locationCancellationContext-v3') -> ?'locationCancellationContext-v3';
compose_application_context('roamingNumberEnquiryContext-v1') -> ?'roamingNumberEnquiryContext-v1';
compose_application_context('roamingNumberEnquiryContext-v2') -> ?'roamingNumberEnquiryContext-v2';
compose_application_context('roamingNumberEnquiryContext-v3') -> ?'roamingNumberEnquiryContext-v3';
compose_application_context('istAlertingContext-v3') -> ?'istAlertingContext-v3';
compose_application_context('locationInfoRetrievalContext-v1') -> ?'locationInfoRetrievalContext-v1';
compose_application_context('locationInfoRetrievalContext-v2') -> ?'locationInfoRetrievalContext-v2';
compose_application_context('locationInfoRetrievalContext-v3') -> ?'locationInfoRetrievalContext-v3';
compose_application_context('callControlTransferContext-v4') -> ?'callControlTransferContext-v4';
compose_application_context('reportingContext-v3') -> ?'reportingContext-v3';
compose_application_context('callCompletionContext-v3') -> ?'callCompletionContext-v3';
compose_application_context('serviceTerminationContext-v3') -> ?'serviceTerminationContext-v3';
compose_application_context('resetContext-v1') -> ?'resetContext-v1';
compose_application_context('resetContext-v2') -> ?'resetContext-v2';
compose_application_context('resetContext-v3') -> ?'resetContext-v3';
compose_application_context('handoverControlContext-v1') -> ?'handoverControlContext-v1';
compose_application_context('handoverControlContext-v2') -> ?'handoverControlContext-v2';
compose_application_context('handoverControlContext-v3') -> ?'handoverControlContext-v3';
compose_application_context('equipmentMngtContext-v1') -> ?'equipmentMngtContext-v1';
compose_application_context('equipmentMngtContext-v2') -> ?'equipmentMngtContext-v2';
compose_application_context('equipmentMngtContext-v3') -> ?'equipmentMngtContext-v3';
compose_application_context('infoRetrievalContext-v1') -> ?'infoRetrievalContext-v1';
compose_application_context('infoRetrievalContext-v2') -> ?'infoRetrievalContext-v2';
compose_application_context('infoRetrievalContext-v3') -> ?'infoRetrievalContext-v3';
compose_application_context('interVlrInfoRetrievalContext-v2') -> ?'interVlrInfoRetrievalContext-v2';
compose_application_context('interVlrInfoRetrievalContext-v3') -> ?'interVlrInfoRetrievalContext-v3';
compose_application_context('subscriberDataMngtContext-v1') -> ?'subscriberDataMngtContext-v1';
compose_application_context('subscriberDataMngtContext-v2') -> ?'subscriberDataMngtContext-v2';
compose_application_context('subscriberDataMngtContext-v3') -> ?'subscriberDataMngtContext-v3';
compose_application_context('tracingContext-v1') -> ?'tracingContext-v1';
compose_application_context('tracingContext-v2') -> ?'tracingContext-v2';
compose_application_context('tracingContext-v3') -> ?'tracingContext-v3';
compose_application_context('networkFunctionalSsContext-v1') -> ?'networkFunctionalSsContext-v1';
compose_application_context('networkFunctionalSsContext-v2') -> ?'networkFunctionalSsContext-v2';
compose_application_context('networkUnstructuredSsContext-v2') -> ?'networkUnstructuredSsContext-v2';
compose_application_context('shortMsgGatewayContext-v1') -> ?'shortMsgGatewayContext-v1';
compose_application_context('shortMsgGatewayContext-v2') -> ?'shortMsgGatewayContext-v2';
compose_application_context('shortMsgGatewayContext-v3') -> ?'shortMsgGatewayContext-v3';
compose_application_context('shortMsgRelayContext-v1') -> ?'shortMsgRelayContext-v1';
compose_application_context('shortMsgMO-RelayContext-v2') -> ?'shortMsgMO-RelayContext-v2';
compose_application_context('shortMsgMO-RelayContext-v3') -> ?'shortMsgMO-RelayContext-v3';
compose_application_context('subscriberDataModificationNotificationContext-v3') -> ?'subscriberDataModificationNotificationContext-v3';
compose_application_context('shortMsgAlertContext-v1') -> ?'shortMsgAlertContext-v1';
compose_application_context('shortMsgAlertContext-v2') -> ?'shortMsgAlertContext-v2';
compose_application_context('mwdMngtContext-v1') -> ?'mwdMngtContext-v1';
compose_application_context('mwdMngtContext-v2') -> ?'mwdMngtContext-v2';
compose_application_context('mwdMngtContext-v3') -> ?'mwdMngtContext-v3';
compose_application_context('shortMsgMT-RelayContext-v2') -> ?'shortMsgMT-RelayContext-v2';
compose_application_context('shortMsgMT-RelayContext-v3') -> ?'shortMsgMT-RelayContext-v3';
compose_application_context('imsiRetrievalContext-v2') -> ?'imsiRetrievalContext-v2';
compose_application_context('msPurgingContext-v2') -> ?'msPurgingContext-v2';
compose_application_context('msPurgingContext-v3') -> ?'msPurgingContext-v3';
compose_application_context('subscriberInfoEnquiryContext-v3') -> ?'subscriberInfoEnquiryContext-v3';
compose_application_context('anyTimeInfoEnquiryContext-v3') -> ?'anyTimeInfoEnquiryContext-v3';
compose_application_context('groupCallControlContext-v3') -> ?'groupCallControlContext-v3';
compose_application_context('gprsLocationUpdateContext-v3') -> ?'gprsLocationUpdateContext-v3';
compose_application_context('gprsLocationInfoRetrievalContext-v4') -> ?'gprsLocationInfoRetrievalContext-v4';
compose_application_context('failureReportContext-v3') -> ?'failureReportContext-v3';
compose_application_context('gprsNotifyContext-v3') -> ?'gprsNotifyContext-v3';
compose_application_context('ss-InvocationNotificationContext-v3') -> ?'ss-InvocationNotificationContext-v3';
compose_application_context('locationSvcGatewayContext-v3') -> ?'locationSvcGatewayContext-v3';
compose_application_context('locationSvcEnquiryContext-v3') -> ?'locationSvcEnquiryContext-v3';
compose_application_context('authenticationFailureReportContext-v3') -> ?'authenticationFailureReportContext-v3';
compose_application_context('shortMsgMT-Relay-VGCS-Context-v3') -> ?'shortMsgMT-Relay-VGCS-Context-v3';
compose_application_context('mm-EventReportingContext-v3') -> ?'mm-EventReportingContext-v3';
compose_application_context('anyTimeInfoHandlingContext-v3') -> ?'anyTimeInfoHandlingContext-v3';
compose_application_context('resourceManagementContext-v3') -> ?'resourceManagementContext-v3';
compose_application_context('groupCallInfoRetrievalContext-v3') -> ?'groupCallInfoRetrievalContext-v3';
compose_application_context('vcsgLocationUpdateContext-v3') -> ?'vcsgLocationUpdateContext-v3';
compose_application_context('vcsgLocationCancellationContext-v3') -> ?'vcsgLocationCancellationContext-v3'.
