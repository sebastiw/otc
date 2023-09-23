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

codec(TCAPBin) when is_binary(TCAPBin) ->
    decode(TCAPBin);
codec({TCDialogue, #{result := Bin} = Component}) when is_binary(Bin) ->
    decode({TCDialogue, Component});
codec(Component) when is_map(Component) ->
    encode(Component).

next(_) -> '$stop'.

decode(TCAPBin) when is_binary(TCAPBin) ->
    %% MAP actually specifies that the full TCAP should be
    %% decoded/encoded with the MAP message, but OTC cannot really
    %% determine if it's MAP or CAP itself without first decoding TCAP
    %% to fetch the ACN. This function clause is therefore left for
    %% the user to manually call.
    {ok, D} = 'MAP-OTC':decode('MapSpecificPDUs', TCAPBin),
    D;
decode(Components) when is_list(Components) ->
    %% Because a TC can contain many components
    [decode(C) || C <- Components];
decode({_TCDialogue, Component}) when is_map(Component) ->
    %% TODO: Probably need to have the TC in order to decide which
    %% operation it is. e.g. ForwardSM (both MT and MO) in V1 uses the
    %% same operation id as MO-ForwardSM in v3 (46).
    decode(Component);
decode(Component) when is_map(Component) ->
    {OpCode, C} = maps:take(opcode, Component),
    Operation = parse_operation(OpCode),
    {T, V, K} = case C of
                    #{component_type := returnResultLast, result := Res} ->
                        {'ResultType', Res, result};
                    #{component_type := returnResultNotLast, result := Res} ->
                        {'ResultType', Res, result};
                    #{component_type := invoke, argument := Arg} ->
                        {'ArgumentType', Arg, argument}
                end,
    O = undefined, %% no options
    R = ('MAP-Protocol':'getdec_Supported-MAP-Operations'(OpCode))(T, V, O),
    C#{operation => Operation,
       K => R}.

encode(Map) ->
    {Operation, C} = maps:take(operation, Map),
    OpCode = compose_operation(Operation),
    {T, V, K} = case C of
                    #{component_type := returnResultLast, result := Res} ->
                        {'ResultType', Res, result};
                    #{component_type := returnResultNotLast, result := Res} ->
                        {'ResultType', Res, result};
                    #{component_type := invoke, argument := Arg} ->
                        {'ArgumentType', Arg, argument}
                end,
    O = undefined, %% no options
    {R, _} = ('MAP-Protocol':'getenc_Supported-MAP-Operations'(OpCode))(T, V, O),
    C#{opcode => OpCode,
       K => iolist_to_binary(R)}.

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

parse_operation({local, 2}) -> updateLocation;
parse_operation({local, 3}) -> cancelLocation;
parse_operation({local, 4}) -> provideRoamingNumber;
parse_operation({local, 5}) -> noteSubscriberDataModified;
parse_operation({local, 6}) -> resumeCallHandling;
parse_operation({local, 7}) -> insertSubscriberData;
parse_operation({local, 8}) -> deleteSubscriberData;
parse_operation({local, 10}) -> registerSS;
parse_operation({local, 11}) -> eraseSS;
parse_operation({local, 12}) -> activateSS;
parse_operation({local, 13}) -> deactivateSS;
parse_operation({local, 14}) -> interrogateSS;
parse_operation({local, 15}) -> authenticationFailureReport;
parse_operation({local, 17}) -> registerPassword;
parse_operation({local, 18}) -> getPassword;
parse_operation({local, 20}) -> releaseResources;
parse_operation({local, 21}) -> 'mt-ForwardSM-VGCS';
parse_operation({local, 22}) -> sendRoutingInfo;
parse_operation({local, 23}) -> updateGprsLocation;
parse_operation({local, 24}) -> sendRoutingInfoForGprs;
parse_operation({local, 25}) -> failureReport;
parse_operation({local, 26}) -> noteMsPresentForGprs;
parse_operation({local, 29}) -> sendEndSignal;
parse_operation({local, 33}) -> processAccessSignalling;
parse_operation({local, 34}) -> forwardAccessSignalling;
parse_operation({local, 36}) -> cancelVcsgLocation;
parse_operation({local, 37}) -> reset;
parse_operation({local, 38}) -> 'forwardCheckSS-Indication';
parse_operation({local, 39}) -> prepareGroupCall;
parse_operation({local, 40}) -> sendGroupCallEndSignal;
parse_operation({local, 41}) -> processGroupCallSignalling;
parse_operation({local, 42}) -> forwardGroupCallSignalling;
parse_operation({local, 43}) -> checkIMEI;
parse_operation({local, 44}) -> 'mt-ForwardSM';
parse_operation({local, 45}) -> sendRoutingInfoForSM;
parse_operation({local, 46}) -> 'mo-ForwardSM';
parse_operation({local, 47}) -> 'reportSM-DeliveryStatus';
parse_operation({local, 50}) -> activateTraceMode;
parse_operation({local, 51}) -> deactivateTraceMode;
parse_operation({local, 53}) -> updateVcsgLocation;
parse_operation({local, 55}) -> sendIdentification;
parse_operation({local, 56}) -> sendAuthenticationInfo;
parse_operation({local, 57}) -> restoreData;
parse_operation({local, 58}) -> sendIMSI;
parse_operation({local, 59}) -> 'processUnstructuredSS-Request';
parse_operation({local, 60}) -> 'unstructuredSS-Request';
parse_operation({local, 61}) -> 'unstructuredSS-Notify';
parse_operation({local, 62}) -> anyTimeSubscriptionInterrogation;
parse_operation({local, 63}) -> informServiceCentre;
parse_operation({local, 64}) -> alertServiceCentre;
parse_operation({local, 65}) -> anyTimeModification;
parse_operation({local, 66}) -> readyForSM;
parse_operation({local, 67}) -> purgeMS;
parse_operation({local, 68}) -> prepareHandover;
parse_operation({local, 69}) -> prepareSubsequentHandover;
parse_operation({local, 70}) -> provideSubscriberInfo;
parse_operation({local, 71}) -> anyTimeInterrogation;
parse_operation({local, 72}) -> 'ss-InvocationNotification';
parse_operation({local, 73}) -> setReportingState;
parse_operation({local, 74}) -> statusReport;
parse_operation({local, 75}) -> remoteUserFree;
parse_operation({local, 76}) -> 'registerCC-Entry';
parse_operation({local, 77}) -> 'eraseCC-Entry';
parse_operation({local, 83}) -> provideSubscriberLocation;
parse_operation({local, 84}) -> sendGroupCallInfo;
parse_operation({local, 85}) -> sendRoutingInfoForLCS;
parse_operation({local, 86}) -> subscriberLocationReport;
parse_operation({local, 87}) -> 'ist-Alert';
parse_operation({local, 88}) -> 'ist-Command';
parse_operation({local, 89}) -> 'noteMM-Event'.

compose_operation(updateLocation) -> {local, 2};
compose_operation(cancelLocation) -> {local, 3};
compose_operation(provideRoamingNumber) -> {local, 4};
compose_operation(noteSubscriberDataModified) -> {local, 5};
compose_operation(resumeCallHandling) -> {local, 6};
compose_operation(insertSubscriberData) -> {local, 7};
compose_operation(deleteSubscriberData) -> {local, 8};
compose_operation(registerSS) -> {local, 10};
compose_operation(eraseSS) -> {local, 11};
compose_operation(activateSS) -> {local, 12};
compose_operation(deactivateSS) -> {local, 13};
compose_operation(interrogateSS) -> {local, 14};
compose_operation(authenticationFailureReport) -> {local, 15};
compose_operation(registerPassword) -> {local, 17};
compose_operation(getPassword) -> {local, 18};
compose_operation(releaseResources) -> {local, 20};
compose_operation('mt-ForwardSM-VGCS') -> {local, 21};
compose_operation(sendRoutingInfo) -> {local, 22};
compose_operation(updateGprsLocation) -> {local, 23};
compose_operation(sendRoutingInfoForGprs) -> {local, 24};
compose_operation(failureReport) -> {local, 25};
compose_operation(noteMsPresentForGprs) -> {local, 26};
compose_operation(sendEndSignal) -> {local, 29};
compose_operation(processAccessSignalling) -> {local, 33};
compose_operation(forwardAccessSignalling) -> {local, 34};
compose_operation(cancelVcsgLocation) -> {local, 36};
compose_operation(reset) -> {local, 37};
compose_operation('forwardCheckSS-Indication') -> {local, 38};
compose_operation(prepareGroupCall) -> {local, 39};
compose_operation(sendGroupCallEndSignal) -> {local, 40};
compose_operation(processGroupCallSignalling) -> {local, 41};
compose_operation(forwardGroupCallSignalling) -> {local, 42};
compose_operation(checkIMEI) -> {local, 43};
compose_operation('mt-ForwardSM') -> {local, 44};
compose_operation(sendRoutingInfoForSM) -> {local, 45};
compose_operation('mo-ForwardSM') -> {local, 46};
compose_operation('reportSM-DeliveryStatus') -> {local, 47};
compose_operation(activateTraceMode) -> {local, 50};
compose_operation(deactivateTraceMode) -> {local, 51};
compose_operation(updateVcsgLocation) -> {local, 53};
compose_operation(sendIdentification) -> {local, 55};
compose_operation(sendAuthenticationInfo) -> {local, 56};
compose_operation(restoreData) -> {local, 57};
compose_operation(sendIMSI) -> {local, 58};
compose_operation('processUnstructuredSS-Request') -> {local, 59};
compose_operation('unstructuredSS-Request') -> {local, 60};
compose_operation('unstructuredSS-Notify') -> {local, 61};
compose_operation(anyTimeSubscriptionInterrogation) -> {local, 62};
compose_operation(informServiceCentre) -> {local, 63};
compose_operation(alertServiceCentre) -> {local, 64};
compose_operation(anyTimeModification) -> {local, 65};
compose_operation(readyForSM) -> {local, 66};
compose_operation(purgeMS) -> {local, 67};
compose_operation(prepareHandover) -> {local, 68};
compose_operation(prepareSubsequentHandover) -> {local, 69};
compose_operation(provideSubscriberInfo) -> {local, 70};
compose_operation(anyTimeInterrogation) -> {local, 71};
compose_operation('ss-InvocationNotification') -> {local, 72};
compose_operation(setReportingState) -> {local, 73};
compose_operation(statusReport) -> {local, 74};
compose_operation(remoteUserFree) -> {local, 75};
compose_operation('registerCC-Entry') -> {local, 76};
compose_operation('eraseCC-Entry') -> {local, 77};
compose_operation(provideSubscriberLocation) -> {local, 83};
compose_operation(sendGroupCallInfo) -> {local, 84};
compose_operation(sendRoutingInfoForLCS) -> {local, 85};
compose_operation(subscriberLocationReport) -> {local, 86};
compose_operation('ist-Alert') -> {local, 87};
compose_operation('ist-Command') -> {local, 88};
compose_operation('noteMM-Event') -> {local, 89}.
