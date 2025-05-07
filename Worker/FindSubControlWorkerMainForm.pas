{
    Copyright (C) 2025 VCC
    creation date: 26 Mar 2024
    initial release date: 19 May 2024

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit FindSubControlWorkerMainForm;

{$IFNDEF IsMCU}
  {$DEFINE IsDesktop}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  InMemFileSystem, PollingFIFO, DynArrays, ClickerFileProviderClient,
  TplZlibUnit, TplLzmaUnit,
  IdGlobal, IdTCPClient, IdHTTPServer, IdCoderMIME, IdSchedulerOfThreadPool,
  IdCustomHTTPServer, IdContext, IdCustomTCPServer, IdHTTP;

type

  { TfrmFindSubControlWorkerMain }

  TfrmFindSubControlWorkerMain = class(TForm)
    btnConnection: TButton;
    btnGetListOfFonts: TButton;
    btnBrowseUIClickerPath: TButton;
    chkExtServerKeepAlive: TCheckBox;
    chkExtServerActive: TCheckBox;
    grpExtServer: TGroupBox;
    grpMQTT: TGroupBox;
    IdDecoderMIME1: TIdDecoderMIME;
    IdHTTP1: TIdHTTP;
    IdHTTPServer1: TIdHTTPServer;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    IdTCPClient1: TIdTCPClient;
    imgFindSubControlBackground: TImage;
    lblBrokerConnectionStatus: TLabel;
    lbeUIClickerPath: TLabeledEdit;
    lbeLatestWork: TLabeledEdit;
    lbeUIClickerPort: TLabeledEdit;
    lblExtServerInfo: TLabel;
    lbeAddress: TLabeledEdit;
    lbeClientID: TLabeledEdit;
    lbePort: TLabeledEdit;
    lbeExtServerPort: TLabeledEdit;
    lblServerInfo: TLabel;
    memLog: TMemo;
    OpenDialog1: TOpenDialog;
    tmrReconnectFileProvider: TTimer;
    tmrConnect: TTimer;
    tmrSubscribe: TTimer;
    tmrProcessLog: TTimer;
    tmrProcessRecData: TTimer;
    tmrStartup: TTimer;
    procedure btnBrowseUIClickerPathClick(Sender: TObject);
    procedure btnConnectionClick(Sender: TObject);
    procedure btnGetListOfFontsClick(Sender: TObject);
    procedure chkExtServerActiveChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServer1Connect(AContext: TIdContext);
    procedure IdHTTPServer1Exception(AContext: TIdContext; AException: Exception
      );
    procedure lbeUIClickerPortChange(Sender: TObject);
    procedure tmrConnectTimer(Sender: TObject);
    procedure tmrProcessLogTimer(Sender: TObject);
    procedure tmrProcessRecDataTimer(Sender: TObject);
    procedure tmrReconnectFileProviderTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure tmrSubscribeTimer(Sender: TObject);
  private
    FMQTTUsername: string;
    FMQTTPassword: string;
    FCredentialsFile: string;
    FWorkerExtraName: string;
    FWorkerExtraCaption: string;

    FLoggingFIFO: TPollingFIFO;
    FRecBufFIFO: TPollingFIFO; //used by the reading thread to pass data to MQTT library
    FInMemFS: TInMemFileSystem;
    FSkipSavingIni: Boolean;
    FThisWorkerTask: string;
    FReportedOS: string;
    FReportedFonts: string;   //this is used only if <> ''
    FPollForMissingServerFilesTh: TPollForMissingServerFiles;

    procedure LogDynArrayOfByte(var AArr: TDynArrayOfByte; ADisplayName: string = '');

    procedure HandleClientOnConnected(Sender: TObject);
    procedure HandleClientOnDisconnected(Sender: TObject);

    procedure HandleOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
    function HandleOnFileExists(const AFileName: string): Boolean;
    procedure HandleOnLogMissingServerFile(AMsg: string);
    procedure HandleOnDenyFile(AFileName: string);

    procedure SendString(AString: string);
    procedure SendDynArrayOfByte(AArr: TDynArrayOfByte);
    procedure SendPacketToServer(ClientInstance: DWord);

    procedure AddToLog(AMsg: string);
    procedure SyncReceivedBuffer(var AReadBuf: TDynArrayOfByte);
    procedure ProcessReceivedBuffer;

    procedure InitHandlers;

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;
    procedure LoadSettingsFromCmd;
    function ResolveTemplatePath(APath: string): string;
    procedure SetFileProviderMainDirs;
    procedure InitFileProvider;

    procedure ShowBrokerIsConnected(ASrcCall: string);
    procedure ShowBrokerIsDisconnected(ASrcCall: string);
  public

  end;

var
  frmFindSubControlWorkerMain: TfrmFindSubControlWorkerMain;

implementation

{$R *.frm}


uses
  MQTTUtils, MQTTClient, MQTTConnectCtrl, MQTTSubscribeCtrl, MQTTUnsubscribeCtrl
  {$IFDEF UsingDynTFT}
    , MemManager
  {$ENDIF}
  , DistFindSubControlCommonConsts, ClickerUtils, Types, ClickerActionProperties,
  ClickerActionsClient, ClickerExtraUtils, MemArchive, ClickerIniFiles;

type
  TResponse = record
    Msg: string; //List of UIClicker variables and one or more archived result images.
    SlotInUse: Boolean; //When true, the array item (a.k.a. slot) is waiting for Publish handler to process it. Another item should be used then.
  end;

  TResponseArr = array of TResponse;

var
  AssignedClientID, TopicWithWorkerName_Background, TopicWithWorkerName_FindSubControl: string;
  Responses: TResponseArr;

const
  CBrokerIsConnectedStatus = 'Status: connected';
  CBrokerIsDisconnectedStatus = 'Status: disconnected';


function GetFirstAvailableResponseSlotIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Responses) - 1 do
    if not Responses[i].SlotInUse then
    begin
      Result := i;
      Break;
    end;
end;


function AddItemToResponses(AMsg: string): Integer;
var
  AvailableIndex: Integer;
begin
  AvailableIndex := GetFirstAvailableResponseSlotIndex;
  if AvailableIndex = -1 then
  begin
    SetLength(Responses, Length(Responses) + 1);
    AvailableIndex := Length(Responses) - 1;
  end;

  Responses[AvailableIndex].Msg := AMsg;
  Responses[AvailableIndex].SlotInUse := True;
  Result := AvailableIndex;
end;


function ProcessResponse(AIndex: Integer): string;
begin
  Result := 'Error processing slot';
  if (AIndex < 0) or (AIndex > Length(Responses)) then
  begin
    frmFindSubControlWorkerMain.AddToLog('Bad slot index: ' + IntToStr(AIndex));
    Exit;
  end;

  if not Responses[AIndex].SlotInUse then
  begin
    frmFindSubControlWorkerMain.AddToLog('Requested to process an unused slot at index: ' + IntToStr(AIndex));
    Exit;
  end;

  Responses[AIndex].SlotInUse := False;
  Result := Responses[AIndex].Msg;
end;


procedure HandleOnMQTTError(ClientInstance: DWord; AErr: Word; APacketType: Byte);
var
  PacketTypeStr: string;
begin
  MQTTPacketToString(APacketType, PacketTypeStr);
  frmFindSubControlWorkerMain.AddToLog('Client: ' + IntToHex(ClientInstance, 8) + '  Err: $' + IntToHex(AErr) + '  PacketType: $' + IntToHex(APacketType) + ' (' + PacketTypeStr + ').');  //The error is made of an upper byte and a lower byte.

  if Hi(AErr) = CMQTT_Reason_NotAuthorized then   // $87
  begin
    frmFindSubControlWorkerMain.AddToLog('Server error: Not authorized.');
    if APacketType = CMQTT_CONNACK then
      frmFindSubControlWorkerMain.AddToLog('             on receiving CONNACK.');
  end;

  if Lo(AErr) = CMQTT_PacketIdentifierNotFound_ClientToServer then   // $CE
    frmFindSubControlWorkerMain.AddToLog('Client error: PacketIdentifierNotFound.');

  if Lo(AErr) = CMQTT_UnhandledPacketType then   // $CA
    frmFindSubControlWorkerMain.AddToLog('Client error: UnhandledPacketType.');  //Usually appears when an incomplete packet is received, so the packet type by is 0.
end;


procedure HandleOnSend_MQTT_Packet(ClientInstance: DWord; APacketType: Byte);
var
  PacketName: string;
begin
  MQTTPacketToString(APacketType, PacketName);
  frmFindSubControlWorkerMain.AddToLog('Sending ' + PacketName + ' packet...');

  try
    frmFindSubControlWorkerMain.SendPacketToServer(ClientInstance);
  except
    on E: Exception do
      frmFindSubControlWorkerMain.AddToLog('Cannot send ' + PacketName + ' packet... Ex: ' + E.Message);
  end;
end;


function HandleOnBeforeMQTT_CONNECT(ClientInstance: DWord;  //The lower byte identifies the client instance (the library is able to implement multiple MQTT clients / device). The higher byte can identify the call in user handlers for various events (e.g. TOnBeforeMQTT_CONNECT).
                                    var AConnectFields: TMQTTConnectFields;                    //user code has to fill-in this parameter
                                    var AConnectProperties: TMQTTConnectProperties;
                                    ACallbackID: Word): Boolean;
var
  TempWillProperties: TMQTTWillProperties;
  UserName, Password: string;
  //ClientId: string;
  //Id: Char;
  ConnectFlags: Byte;
  EnabledProperties: Word;
begin
  Result := True;

  frmFindSubControlWorkerMain.AddToLog('Preparing CONNECT data..');

  //Id := Chr((ClientInstance and $FF) + 48);
  //ClientId := 'MyClient' + Id;
  UserName := frmFindSubControlWorkerMain.FMQTTUsername;
  Password := frmFindSubControlWorkerMain.FMQTTPassword;

  //StringToDynArrayOfByte(ClientId, AConnectFields.PayloadContent.ClientID);
  StringToDynArrayOfByte(UserName, AConnectFields.PayloadContent.UserName);
  StringToDynArrayOfByte(Password, AConnectFields.PayloadContent.Password);

  ConnectFlags := CMQTT_UsernameInConnectFlagsBitMask or
                  CMQTT_PasswordInConnectFlagsBitMask or
                  CMQTT_CleanStartInConnectFlagsBitMask {or
                  CMQTT_WillQoSB1InConnectFlagsBitMask};

  EnabledProperties := CMQTTConnect_EnSessionExpiryInterval or
                       CMQTTConnect_EnRequestResponseInformation or
                       CMQTTConnect_EnRequestProblemInformation {or
                       CMQTTConnect_EnAuthenticationMethod or
                       CMQTTConnect_EnAuthenticationData};

  MQTT_InitWillProperties(TempWillProperties);
  TempWillProperties.WillDelayInterval := 30; //some value
  TempWillProperties.PayloadFormatIndicator := 1;  //0 = do not send.  1 = UTF-8 string
  TempWillProperties.MessageExpiryInterval := 3600;
  StringToDynArrayOfByte('SomeType', TempWillProperties.ContentType);
  StringToDynArrayOfByte('SomeTopicName', TempWillProperties.ResponseTopic);
  StringToDynArrayOfByte('MyCorrelationData', TempWillProperties.CorrelationData);

  {$IFDEF EnUserProperty}
    AddStringToDynOfDynArrayOfByte('Key=Value', TempWillProperties.UserProperty);
    AddStringToDynOfDynArrayOfByte('NewKey=NewValue', TempWillProperties.UserProperty);
  {$ENDIF}

  FillIn_PayloadWillProperties(TempWillProperties, AConnectFields.PayloadContent.WillProperties);
  MQTT_FreeWillProperties(TempWillProperties);
  StringToDynArrayOfByte('WillTopic', AConnectFields.PayloadContent.WillTopic);

  //Please set the Will Flag in ConnectFlags below, then uncomment above code, if "Will" properties are required.
  AConnectFields.ConnectFlags := ConnectFlags;  //bits 7-0:  User Name, Password, Will Retain, Will QoS, Will Flag, Clean Start, Reserved
  AConnectFields.EnabledProperties := EnabledProperties;
  AConnectFields.KeepAlive := 0; //any positive values require pinging the server if no other packet is being sent

  AConnectProperties.SessionExpiryInterval := 3600; //[s]
  AConnectProperties.ReceiveMaximum := 7000;
  AConnectProperties.MaximumPacketSize := 10 * 1024 * 1024;
  AConnectProperties.TopicAliasMaximum := 100;
  AConnectProperties.RequestResponseInformation := 1;
  AConnectProperties.RequestProblemInformation := 1;

  {$IFDEF EnUserProperty}
    AddStringToDynOfDynArrayOfByte('UserProp=Value', AConnectProperties.UserProperty);
  {$ENDIF}

  StringToDynArrayOfByte('SCRAM-SHA-1', AConnectProperties.AuthenticationMethod);       //some example from spec, pag 108   the server may add to its log: "bad AUTH method"
  StringToDynArrayOfByte('client-first-data', AConnectProperties.AuthenticationData);   //some example from spec, pag 108

  frmFindSubControlWorkerMain.AddToLog('Done preparing CONNECT data..');
  frmFindSubControlWorkerMain.AddToLog('');
end;


procedure HandleOnAfterMQTT_CONNACK(ClientInstance: DWord; var AConnAckFields: TMQTTConnAckFields; var AConnAckProperties: TMQTTConnAckProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received CONNACK');

  AssignedClientID := StringReplace(DynArrayOfByteToString(AConnAckProperties.AssignedClientIdentifier), #0, '#0', [rfReplaceAll]);
  frmFindSubControlWorkerMain.lbeClientID.Text := AssignedClientID;

  TopicWithWorkerName_Background := CTopicName_AppToWorker_SendBackground + '_' + AssignedClientID;
  TopicWithWorkerName_FindSubControl := CTopicName_AppToWorker_FindSubControl + '_' + AssignedClientID;

  frmFindSubControlWorkerMain.AddToLog('ConnAckFields.EnabledProperties: ' + IntToStr(AConnAckFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('ConnAckFields.SessionPresentFlag: ' + IntToStr(AConnAckFields.SessionPresentFlag));
  frmFindSubControlWorkerMain.AddToLog('ConnAckFields.ConnectReasonCode: ' + IntToStr(AConnAckFields.ConnectReasonCode));  //should be 0

  frmFindSubControlWorkerMain.AddToLog('SessionExpiryInterval: ' + IntToStr(AConnAckProperties.SessionExpiryInterval));
  frmFindSubControlWorkerMain.AddToLog('ReceiveMaximum: ' + IntToStr(AConnAckProperties.ReceiveMaximum));
  frmFindSubControlWorkerMain.AddToLog('MaximumQoS: ' + IntToStr(AConnAckProperties.MaximumQoS));
  frmFindSubControlWorkerMain.AddToLog('RetainAvailable: ' + IntToStr(AConnAckProperties.RetainAvailable));
  frmFindSubControlWorkerMain.AddToLog('MaximumPacketSize: ' + IntToStr(AConnAckProperties.MaximumPacketSize));
  frmFindSubControlWorkerMain.AddToLog('AssignedClientIdentifier: ' + AssignedClientID);
  frmFindSubControlWorkerMain.AddToLog('TopicAliasMaximum: ' + IntToStr(AConnAckProperties.TopicAliasMaximum));
  frmFindSubControlWorkerMain.AddToLog('ReasonString: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(AConnAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}

  frmFindSubControlWorkerMain.AddToLog('WildcardSubscriptionAvailable: ' + IntToStr(AConnAckProperties.WildcardSubscriptionAvailable));
  frmFindSubControlWorkerMain.AddToLog('SubscriptionIdentifierAvailable: ' + IntToStr(AConnAckProperties.SubscriptionIdentifierAvailable));
  frmFindSubControlWorkerMain.AddToLog('SharedSubscriptionAvailable: ' + IntToStr(AConnAckProperties.SharedSubscriptionAvailable));
  frmFindSubControlWorkerMain.AddToLog('ServerKeepAlive: ' + IntToStr(AConnAckProperties.ServerKeepAlive));
  frmFindSubControlWorkerMain.AddToLog('ResponseInformation: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ResponseInformation), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('ServerReference: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ServerReference), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('AuthenticationMethod: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.AuthenticationMethod), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('AuthenticationData: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.AuthenticationData), #0, '#0', [rfReplaceAll]));

  frmFindSubControlWorkerMain.AddToLog('');

  ///////////////////////////////////////// when the server returns SessionPresentFlag set to 1, the library resends unacknowledged Publish and PubRel packets.
  //AConnAckFields.SessionPresentFlag := 1;
end;


function HandleOnBeforeSendingMQTT_SUBSCRIBE(ClientInstance: DWord;  //The lower word identifies the client instance
                                             var ASubscribeFields: TMQTTSubscribeFields;
                                             var ASubscribeProperties: TMQTTSubscribeProperties;
                                             ACallbackID: Word): Boolean;
var
  Options, QoS: Byte;
  SubId: Word;
begin
  Options := 0;
  QoS := 2;

  Options := Options or QoS; //bits 1 and 0
  //Bit 2 of the Subscription Options represents the No Local option.  - spec pag 73
  //Bit 3 of the Subscription Options represents the Retain As Published option.  - spec pag 73
  //Bits 4 and 5 of the Subscription Options represent the Retain Handling option.  - spec pag 73
  //Bits 6 and 7 of the Subscription Options byte are reserved for future use. - Must be set to 0.  - spec pag 73

                                                                            //Subscription identifiers are not mandatory (per spec).
  SubId := MQTT_CreateClientToServerSubscriptionIdentifier(ClientInstance); //This function has to be called here, in this handler only. The library does not call this function other than for init purposes.
                                                                            //If SubscriptionIdentifiers are used, then user code should free them when resubscribing or when unsubscribing.
  ASubscribeProperties.SubscriptionIdentifier := SubId;  //For now, the user code should keep track of these identifiers and free them on resubscribing or unsubscribing.
  frmFindSubControlWorkerMain.AddToLog('Subscribing with new SubscriptionIdentifier: ' + IntToStr(SubId));

  Result := FillIn_SubscribePayload(CTopicName_AppToWorker_GetCapabilities, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add "GetCapabilities" TopicFilters.');
    Exit;
  end;
                                    //CTopicName_AppToWorker_SendBackground is a common subscription
  Result := FillIn_SubscribePayload(CTopicName_AppToWorker_SendBackground, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add "Background" TopicFilters.');
    Exit;
  end;
                                  //TopicWithWorkerName_Background is an individual subscription
  Result := FillIn_SubscribePayload(TopicWithWorkerName_Background, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add "Background" TopicFilters.');
    Exit;
  end;

  Result := FillIn_SubscribePayload(TopicWithWorkerName_FindSubControl, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add "FindSubControl" TopicFilters.');
    Exit;
  end;

  Result := FillIn_SubscribePayload(CTopicName_AppToWorker_GetListOfFonts, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add "GetListOfFonts" TopicFilters.');
    Exit;
  end;

  //
  //Result := FillIn_SubscribePayload('MoreExtra_' + frmFindSubControlWorkerMain.lbeTopicName.Text, 1, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  //if not Result then
  //begin
  //  frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters.');
  //  Exit;
  //end;
  //
  //Result := FillIn_SubscribePayload('LastExtra_' + frmFindSubControlWorkerMain.lbeTopicName.Text, 0, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  //if not Result then
  //begin
  //  frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters.');
  //  Exit;
  //end;

  //Enable SubscriptionIdentifier only if required (allocated above with CreateClientToServerSubscriptionIdentifier) !!!
  //The library initializes EnabledProperties to 0.
  //A subscription is allowed to be made without a SubscriptionIdentifier.
  ASubscribeFields.EnabledProperties := CMQTTSubscribe_EnSubscriptionIdentifier {or CMQTTSubscribe_EnUserProperty};

  frmFindSubControlWorkerMain.AddToLog('Subscribing with PacketIdentifier: ' + IntToStr(ASubscribeFields.PacketIdentifier));
  frmFindSubControlWorkerMain.AddToLog('Subscribing to: ' + StringReplace(DynArrayOfByteToString(ASubscribeFields.TopicFilters), #0, '#0', [rfReplaceAll]));

  frmFindSubControlWorkerMain.AddToLog('');
end;


procedure HandleOnAfterReceivingMQTT_SUBACK(ClientInstance: DWord; var ASubAckFields: TMQTTSubAckFields; var ASubAckProperties: TMQTTSubAckProperties);
var
  i: Integer;
begin
  frmFindSubControlWorkerMain.AddToLog('Received SUBACK');
  //frmFindSubControlWorkerMain.AddToLog('ASubAckFields.IncludeReasonCode: ' + IntToStr(ASubAckFields.IncludeReasonCode));  //not used
  //frmFindSubControlWorkerMain.AddToLog('ASubAckFields.ReasonCode: ' + IntToStr(ASubAckFields.ReasonCode));              //not used
  frmFindSubControlWorkerMain.AddToLog('ASubAckFields.EnabledProperties: ' + IntToStr(ASubAckFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('ASubAckFields.PacketIdentifier: ' + IntToStr(ASubAckFields.PacketIdentifier));  //This must be the same as sent in SUBSCRIBE packet.

  frmFindSubControlWorkerMain.AddToLog('ASubAckFields.Payload.Len: ' + IntToStr(ASubAckFields.SrcPayload.Len));

  for i := 0 to ASubAckFields.SrcPayload.Len - 1 do         //these are QoS values for each TopicFilter (if ok), or error codes (if not ok).
    frmFindSubControlWorkerMain.AddToLog('ASubAckFields.ReasonCodes[' + IntToStr(i) + ']: ' + IntToStr(ASubAckFields.SrcPayload.Content^[i]));

  frmFindSubControlWorkerMain.AddToLog('ASubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(ASubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('ASubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(ASubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}

  frmFindSubControlWorkerMain.tmrSubscribe.Enabled := False;

  frmFindSubControlWorkerMain.ShowBrokerIsConnected('SUBACK');
  frmFindSubControlWorkerMain.AddToLog('');
end;


function HandleOnBeforeSendingMQTT_UNSUBSCRIBE(ClientInstance: DWord;  //The lower word identifies the client instance
                                               var AUnsubscribeFields: TMQTTUnsubscribeFields;
                                               var AUnsubscribeProperties: TMQTTUnsubscribeProperties;
                                               ACallbackID: Word): Boolean;
begin
  Result := FillIn_UnsubscribePayload(CTopicName_AppToWorker_GetCapabilities, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(CTopicName_AppToWorker_SendBackground, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(TopicWithWorkerName_Background, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(TopicWithWorkerName_FindSubControl, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(CTopicName_AppToWorker_GetListOfFonts, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    frmFindSubControlWorkerMain.AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  frmFindSubControlWorkerMain.AddToLog('Unsubscribing from "' + CTopicName_AppToWorker_GetCapabilities + '" and "' + CTopicName_AppToWorker_SendBackground + '" and "' + TopicWithWorkerName_Background + '" and "' + TopicWithWorkerName_FindSubControl + '"...');

  //the user code should call RemoveClientToServerSubscriptionIdentifier to remove the allocate identifier.
end;


procedure HandleOnAfterReceivingMQTT_UNSUBACK(ClientInstance: DWord; var AUnsubAckFields: TMQTTUnsubAckFields; var AUnsubAckProperties: TMQTTUnsubAckProperties);
var
  i: Integer;
begin
  frmFindSubControlWorkerMain.AddToLog('Received UNSUBACK');
  //frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.IncludeReasonCode: ' + IntToStr(ASubAckFields.IncludeReasonCode));  //not used
  //frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.ReasonCode: ' + IntToStr(ASubAckFields.ReasonCode));              //not used
  frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.EnabledProperties: ' + IntToStr(AUnsubAckFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.PacketIdentifier: ' + IntToStr(AUnsubAckFields.PacketIdentifier));  //This must be the same as sent in SUBSCRIBE packet.

  frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.Payload.Len: ' + IntToStr(AUnsubAckFields.SrcPayload.Len));

  for i := 0 to AUnsubAckFields.SrcPayload.Len - 1 do         //these are QoS values for each TopicFilter (if ok), or error codes (if not ok).
    frmFindSubControlWorkerMain.AddToLog('AUnsubAckFields.ReasonCodes[' + IntToStr(i) + ']: ' + IntToStr(AUnsubAckFields.SrcPayload.Content^[i]));

  frmFindSubControlWorkerMain.AddToLog('AUnsubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(AUnsubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('AUnsubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(AUnsubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}

  frmFindSubControlWorkerMain.ShowBrokerIsDisconnected('UNSUBACK');
  frmFindSubControlWorkerMain.AddToLog('');
end;


//This handler is used when this client publishes a message to broker.
function HandleOnBeforeSendingMQTT_PUBLISH(ClientInstance: DWord;  //The lower word identifies the client instance (the library is able to implement multiple MQTT clients / device). The higher byte can identify the call in user handlers for various events (e.g. TOnBeforeMQTT_CONNECT).
                                           var APublishFields: TMQTTPublishFields;                    //user code has to fill-in this parameter
                                           var APublishProperties: TMQTTPublishProperties;            //user code has to fill-in this parameter
                                           ACallbackID: Word): Boolean;
var
  Msg: string;
  QoS: Byte;
begin
  Result := True;

  case ACallbackID of
    0:
    begin
      Msg :=       CProtocolParam_Name + '=' + AssignedClientID + #13#10;
      Msg := Msg + CProtocolParam_OS + '=' + frmFindSubControlWorkerMain.FReportedOS + #13#10;
      Msg := Msg + CProtocolParam_FileCache + '=' + FastReplace_ReturnTo45(frmFindSubControlWorkerMain.FInMemFS.ListMemFilesWithHashAsString) + #13#10;
      Msg := Msg + CProtocolParam_ExtraName + '=' + frmFindSubControlWorkerMain.FWorkerExtraName; //this is user-controlled
    end;

    1:
    begin
      Msg := CProtocolParam_Name + '=' + AssignedClientID + #13#10 +
             ProcessResponse(ACallbackID shr 8);
    end;

    3:
    begin
      Msg := CProtocolParam_Name + '=' + AssignedClientID + #13#10 +
             ProcessResponse(ACallbackID shr 8);
    end;

    4:
    begin
      Msg := CProtocolParam_Name + '=' + AssignedClientID + #13#10 +
             CProtocolParam_Fonts + '=' + ProcessResponse(ACallbackID shr 8);
    end;

    else
      Msg := 'unknown CallbackID';
  end;

  QoS := (APublishFields.PublishCtrlFlags shr 1) and 3;
  //frmFindSubControlWorkerMain.AddToLog('Publishing "' + Msg + '" at QoS = ' + IntToStr(QoS));   //commented because Msg contains an archive, which can be even more than 2MB (hex encoded)

  Result := Result and StringToDynArrayOfByte(Msg, APublishFields.ApplicationMessage);

  case ACallbackID of
    0: Result := Result and StringToDynArrayOfByte(CTopicName_WorkerToApp_GetCapabilities, APublishFields.TopicName);
    1: Result := Result and StringToDynArrayOfByte(CTopicName_WorkerToApp_SendBackground, APublishFields.TopicName);
    3: Result := Result and StringToDynArrayOfByte(CTopicName_WorkerToApp_FindSubControl, APublishFields.TopicName);
    4: Result := Result and StringToDynArrayOfByte(CTopicName_WorkerToApp_GetListOfFonts, APublishFields.TopicName);
    else
      Result := Result and StringToDynArrayOfByte(CMQTT_Worker_UnhandledRequest, APublishFields.TopicName);
  end;

  frmFindSubControlWorkerMain.AddToLog('');
  //QoS can be overriden here. If users override QoS in this handler, then a a different PacketIdentifier might be allocated (depending on what is available)
end;


//This handler is used when this client publishes a message to broker and the broker responds with PUBACK.
procedure HandleOnBeforeSendingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Acknowledging with PUBACK');
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.EnabledProperties: ' + IntToStr(APubAckFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.IncludeReasonCode: ' + IntToStr(APubAckFields.IncludeReasonCode));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.PacketIdentifier: ' + IntToStr(APubAckFields.PacketIdentifier));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.ReasonCode: ' + IntToStr(APubAckFields.ReasonCode));

  frmFindSubControlWorkerMain.AddToLog('APubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(APubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('APubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(APubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}

  frmFindSubControlWorkerMain.AddToLog('');
  //This handler can be used to override what is being sent to server as a reply to PUBLISH
end;


procedure HandleOnAfterReceivingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received PUBACK');
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.EnabledProperties: ' + IntToStr(APubAckFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.IncludeReasonCode: ' + IntToStr(APubAckFields.IncludeReasonCode));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.PacketIdentifier: ' + IntToStr(APubAckFields.PacketIdentifier));
  frmFindSubControlWorkerMain.AddToLog('APubAckFields.ReasonCode: ' + IntToStr(APubAckFields.ReasonCode));

  frmFindSubControlWorkerMain.AddToLog('APubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(APubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('APubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(APubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}

  frmFindSubControlWorkerMain.AddToLog('');
end;


function GetUIClickerAddr: string;
begin
  Result := 'http://127.0.0.1:' + frmFindSubControlWorkerMain.lbeUIClickerPort.Text + '/';
end;


function SendFileToUIClickerWithLoc(AContent: TMemoryStream; AFilename, InMemLoc: string): string;
begin
  Result := SendFileToServer(GetUIClickerAddr +
                             InMemLoc + '?' +
                             CREParam_FileName + '=' + AFilename,
                             AContent);
end;


function SendFileToUIClicker_SrvInMem(AContent: TMemoryStream; AFilename: string): string;   //used for most files
begin
  Result := SendFileToUIClickerWithLoc(AContent, AFilename, CRECmd_SendFileToServer);
end;


function SendFileToUIClicker_ExtRndInMem(AContent: TMemoryStream; AFilename: string): string;   //used for background
begin
  Result := SendFileToUIClickerWithLoc(AContent, AFilename, CRECmd_SetRenderedFile);
end;


function GetListOfFontsFromUIClicker: string;
var
  SetVarOptions: TClkSetVarOptions;
  ListOfFonts: TStringList;
begin
  SetVarOptions.ListOfVarNames := CGetListOfFontsResultVarName + #13#10;
  SetVarOptions.ListOfVarValues := '$GetListOfFonts()$' + #13#10;
  SetVarOptions.ListOfVarEvalBefore := '1' + #13#10;

  Result := ExecuteSetVarAction(GetUIClickerAddr, SetVarOptions);
  if Pos('$RemoteExecResponse$=1', Result) = 0 then
    Result := ''
  else
  begin
    Result := Copy(Result, Length('$RemoteExecResponse$=1') + 1, MaxInt);
    ListOfFonts := TStringList.Create;
    try
      ListOfFonts.LineBreak := #13#10;
      ListOfFonts.Text := FastReplace_87ToReturn(Result);
      Result := ListOfFonts.Values[CGetListOfFontsResultVarName];
    finally
      ListOfFonts.Free;
    end;
  end;
end;


procedure AddToLog(AMsg: string);
begin
  frmFindSubControlWorkerMain.AddToLog(AMsg);
end;


procedure RemoveFontProfileByIndex(AIndex: Integer; var AFindSubControl: TClkFindSubControlOptions);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > Length(AFindSubControl.MatchBitmapText) - 1) then
    raise Exception.Create('Index out of bounds when removing font profile.');

  for i := AIndex to Length(AFindSubControl.MatchBitmapText) - 2 do
    AFindSubControl.MatchBitmapText[i] := AFindSubControl.MatchBitmapText[i + 1];

  SetLength(AFindSubControl.MatchBitmapText, Length(AFindSubControl.MatchBitmapText) - 1);
end;


procedure FilterOutTasksForOtherWorkers(AThisWorkerTask: string; var AFindSubControl: TClkFindSubControlOptions);
var
  WorkerTask, MatchingFiles: TStringList;
  i: Integer;
begin
  WorkerTask := TStringList.Create;
  MatchingFiles := TStringList.Create;
  try
    WorkerTask.LineBreak := #13#10;
    WorkerTask.Text := StringReplace(AThisWorkerTask, '&', #13#10, [rfReplaceAll]);

    for i := Length(AFindSubControl.MatchBitmapText) - 1 downto 0 do
      if WorkerTask.IndexOf(CWorkerTask_TxtPrefix + IntToStr(i) + '=1') = -1 then
        RemoveFontProfileByIndex(i, AFindSubControl); //delete from that index

    AddToLog('Remaining font profiles after filtering:');
    for i := 0 to Length(AFindSubControl.MatchBitmapText) - 1 do
      AddToLog('    ' + AFindSubControl.MatchBitmapText[i].ProfileName + ' (' + AFindSubControl.MatchBitmapText[i].FontName + ' ' + IntToStr(AFindSubControl.MatchBitmapText[i].FontSize) + ')');

    if Length(AFindSubControl.MatchBitmapText) = 0 then
      AddToLog('');

    MatchingFiles.Text := AFindSubControl.MatchBitmapFiles;
    for i := MatchingFiles.Count - 1 downto 0 do
      if WorkerTask.IndexOf(CWorkerTask_BmpPrefix + IntToStr(i) + '=1') = -1 then
        MatchingFiles.Delete(i); //delete from that index
    AFindSubControl.MatchBitmapFiles := MatchingFiles.Text;

    AddToLog('Remaining bitmap files after filtering:');
    for i := 0 to MatchingFiles.Count - 1 do
      AddToLog('    ' + MatchingFiles.Strings[i]);

    if MatchingFiles.Count = 0 then
      AddToLog('');

    MatchingFiles.Text := AFindSubControl.MatchPrimitiveFiles;
    for i := MatchingFiles.Count - 1 downto 0 do
      if WorkerTask.IndexOf(CWorkerTask_PmtvPrefix + IntToStr(i) + '=1') = -1 then
        MatchingFiles.Delete(i); //delete from that index
    AFindSubControl.MatchPrimitiveFiles := MatchingFiles.Text;

    AddToLog('Remaining pmtv files after filtering:');
    for i := 0 to MatchingFiles.Count - 1 do
      AddToLog('    ' + MatchingFiles.Strings[i]);

    if MatchingFiles.Count = 0 then
      AddToLog('');
  finally
    WorkerTask.Free;
    MatchingFiles.Free;
  end;
end;


function SendExecuteFindSubControlAction(AActionContent, AThisWorkerTask: string): string;
var
  TempFindSubControl: TClkFindSubControlOptions;
  TempActionOptions: TClkActionOptions;
  ActionContentList: TStringList;
  ConversionResult, UIClickerAddr: string;
  FindSubControlTimeout: Integer;
begin
  ActionContentList := TStringList.Create;
  try
    ActionContentList.LineBreak := #13#10;
    ActionContentList.Text := StringReplace(AActionContent, '&', #13#10, [rfReplaceAll]);

    FindSubControlTimeout := StrToIntDef(ActionContentList.Values[CPropertyName_ActionTimeout], -2);
    if FindSubControlTimeout = -2 then
    begin
      FindSubControlTimeout := CMinFindSubControlActionTimeout;
      frmFindSubControlWorkerMain.AddToLog('=============== Did not receive a valid action timeout. Setting to minimum: ' + IntToStr(FindSubControlTimeout) + 'ms.');
    end;

    ConversionResult := SetFindSubControlActionProperties(ActionContentList, @frmFindSubControlWorkerMain.AddToLog, TempFindSubControl, TempActionOptions);
    if ConversionResult <> '' then
    begin
      frmFindSubControlWorkerMain.AddToLog('ConversionResult: ' + ConversionResult);
      Result := ConversionResult;
      Exit;
    end;
  finally
    ActionContentList.Free;
  end;

  TempFindSubControl.ImageSource := isFile;
  TempFindSubControl.SourceFileName := CBackgroundFileNameForUIClicker;
  TempFindSubControl.ImageSourceFileNameLocation := isflMem;

  FilterOutTasksForOtherWorkers(AThisWorkerTask, TempFindSubControl);

  UIClickerAddr := GetUIClickerAddr;
  Result := ExecuteFindSubControlAction(UIClickerAddr,
                                        TempFindSubControl,
                                        'Action_' + DateTimeToStr(Now),
                                        FindSubControlTimeout, //The HTTP client has its own timeout, currently hardcoded to 1s for connection and 1h for response.
                                        CREParam_FileLocation_ValueMem,
                                        True);
end;


function SendGetDebugImageFromServer(AActionContent: string; AUsingCompression: Boolean; ACompressionAlgorithm: string): string;
var
  TempStream, ArchiveStream: TMemoryStream;
  TempMatchBitmapAlgorithm: string;
  Params: TStringList;
  MemArchive: TMemArchive;
  TempArchiveHandlers: TArchiveHandlers;
  PluginSettings: TStringList;
  TempLzmaOptions: TplLzmaOptions;
begin
  Params := TStringList.Create;
  try
    Params.LineBreak := #13#10;
    Params.Text := StringReplace(AActionContent, '&', #13#10, [rfReplaceAll]);
    TempMatchBitmapAlgorithm := Params.Values['MatchBitmapAlgorithm'];
  finally
    Params.Free;
  end;

  TempStream := TMemoryStream.Create;
  try
    Result := GetDebugImageFromServerAsStream(GetUIClickerAddr, 0, TempStream, TempMatchBitmapAlgorithm = '1');

    if TempStream.Size > 0 then
    begin
      AddToLog('Archiving the result image.');

      MemArchive := TMemArchive.Create;
      ArchiveStream := TMemoryStream.Create;
      TempArchiveHandlers := TArchiveHandlers.Create;
      try
        TempArchiveHandlers.OnAddToLogNoObj := @AddToLog;

        MemArchive.OnCompress := @TempArchiveHandlers.HandleOnCompress;
        MemArchive.OnDecompress := @TempArchiveHandlers.HandleOnDecompress;
        MemArchive.OnComputeArchiveHash := @TempArchiveHandlers.HandleOnComputeArchiveHash;

        if AUsingCompression then
        begin
          MemArchive.CompressionLevel := 9;
          TempArchiveHandlers.CompressionAlgorithm := CompressionAlgorithmsStrToType(ACompressionAlgorithm);
        end
        else
          MemArchive.CompressionLevel := 0;

        if TempArchiveHandlers.CompressionAlgorithm = caLzma then
        begin
          PluginSettings := TStringList.Create;
          try
            PluginSettings.LineBreak := #13#10;
            ///////////////////////// get PluginSettings from plugin using FillInLzmaOptionsFromPluginProperties
            TempLzmaOptions := FillInDefaultLzmaOptionsFromPluginProperties;  /////////////////////////this call should be replaced with getting the options from plugin
            TempArchiveHandlers.LzmaOptions := TempLzmaOptions;
          finally
            PluginSettings.Free;
          end;
        end;

        try
          MemArchive.OpenArchive(ArchiveStream, True);
          try
            TempStream.Position := 0;
            MemArchive.AddFromStream(CResultFileNameInArchive, TempStream);   //eventually, there will be multiple results
          finally
            MemArchive.CloseArchive;
          end;

          AddToLog('Computing Archive hash.. Size = ' + IntToStr(ArchiveStream.Size));
          AddToLog('Archive hash: ' + ClickerExtraUtils.ComputeHash(ArchiveStream.Memory, ArchiveStream.Size));
        except
          on E: Exception do
            AddToLog('----------- Ex on archiving result image: "' + E.Message + '"  CompressionAlgorithm = ' + IntToStr(Ord(TempArchiveHandlers.CompressionAlgorithm)) + '  CompressionLevel = ' + IntToStr(MemArchive.CompressionLevel));
        end;


        SetLength(Result, ArchiveStream.Size);
        ArchiveStream.Position := 0;
        ArchiveStream.Read(Result[1], ArchiveStream.Size);
      finally
        MemArchive.Free;
        ArchiveStream.Free;
        TempArchiveHandlers.Free;
      end;
    end;
  finally
    TempStream.Free;
  end;
end;


procedure SaveBmpToInMemFS(AContent: TMemoryStream; ABackgroundFnm: string);
begin
  if not frmFindSubControlWorkerMain.FInMemFS.FileExistsInMem(ABackgroundFnm) then
    frmFindSubControlWorkerMain.FInMemFS.SaveFileToMem(ABackgroundFnm, AContent.Memory, AContent.Size);
end;


procedure SaveBackgroundBmpToInMemFS(AContent: TMemoryStream);
var
  Hash, BackgroundFnm: string;
begin
  Hash := ComputeHash(AContent.Memory, AContent.Size);
  BackgroundFnm := 'Background_' + Hash + '.bmp';

  SaveBmpToInMemFS(AContent, BackgroundFnm);
end;


function SendVarsToWorkers(AMemArchiveWithVars: TMemArchive): string;
var
  TempFileContent: TMemoryStream;
  VarsForWorkers_Names, VarsForWorkers_Values, VarsForWorkers_EvalBefore: string;
  TempSetVarOptions: TClkSetVarOptions;
begin
  TempFileContent := TMemoryStream.Create;
  try
    AMemArchiveWithVars.ExtractToStream(CVarsForWorkersInArchive_Names, TempFileContent);
    TempFileContent.Position := 0;
    SetLength(VarsForWorkers_Names, TempFileContent.Size);
    TempFileContent.Read(VarsForWorkers_Names[1], TempFileContent.Size);

    TempFileContent.Clear;

    AMemArchiveWithVars.ExtractToStream(CVarsForWorkersInArchive_Values, TempFileContent);
    TempFileContent.Position := 0;
    SetLength(VarsForWorkers_Values, TempFileContent.Size);
    TempFileContent.Read(VarsForWorkers_Values[1], TempFileContent.Size);

    TempFileContent.Clear;

    AMemArchiveWithVars.ExtractToStream(CVarsForWorkersInArchive_EvalBefore, TempFileContent);
    TempFileContent.Position := 0;
    SetLength(VarsForWorkers_EvalBefore, TempFileContent.Size);
    TempFileContent.Read(VarsForWorkers_EvalBefore[1], TempFileContent.Size);
  finally
    TempFileContent.Free;
  end;

  TempSetVarOptions.ListOfVarNames := VarsForWorkers_Names;
  TempSetVarOptions.ListOfVarValues := VarsForWorkers_Values;
  TempSetVarOptions.ListOfVarEvalBefore := VarsForWorkers_EvalBefore;
  TempSetVarOptions.FailOnException := False;

  Result := ExecuteSetVarAction(GetUIClickerAddr, TempSetVarOptions, False);
end;


const
  CImageSourceRawContentParam: string = '&' + CProtocolParam_ImageSourceRawContent + '=';


procedure DecodeArchiveFromRequest(var AAppMsg: string; out ABmpStr, ACompressionAlgorithm: string; out AUsingCompression: Boolean);
var
  PosImageSourceRawContent: Integer;
begin
  PosImageSourceRawContent := Pos(CImageSourceRawContentParam, AAppMsg);
  ABmpStr := Copy(AAppMsg, PosImageSourceRawContent + Length(CImageSourceRawContentParam), MaxInt);

  AUsingCompression := Copy(AAppMsg, Pos('&' + CProtocolParam_UsingCompression + '=', AAppMsg) + Length('&' + CProtocolParam_UsingCompression + '='), 1) = '1';
  ACompressionAlgorithm := Copy(AAppMsg, Pos('&' + CProtocolParam_CompressionAlgorithm + '=', AAppMsg) + Length('&' + CProtocolParam_CompressionAlgorithm + '='), 30);  //assumes the name of the algorithm is not longer than 30
  ACompressionAlgorithm := Copy(ACompressionAlgorithm, 1, Pos('&', ACompressionAlgorithm) - 1);

  AAppMsg := Copy(AAppMsg, 1, PosImageSourceRawContent - 1); //discard archive

  //frmFindSubControlWorkerMain.AddToLog('ABmpStr: ' + FastReplace_0To1(ABmpStr));
  //frmFindSubControlWorkerMain.AddToLog('=============== UsingCompression: ' + BoolToStr(AUsingCompression, 'True', 'False'));
  //frmFindSubControlWorkerMain.AddToLog('=============== CompressionAlgorithm: ' + ACompressionAlgorithm);
end;


function ProcessSendBackgroundRequest(AAppMsg: string; out AResponse, AErrMsg: string): Boolean;
var
  BmpStr: string;
  UsingCompression: Boolean;
  CompressionAlgorithm: string;
  MemStream, DecompressedStream: TMemoryStream;
  TempMemArchive: TMemArchive;
  TempArchiveHandlers: TArchiveHandlers;
  tk: QWord;
  CmdResult: string;
begin
  AErrMsg := '';
  AResponse := '$ExecAction_Err$=' + CBackgroundOKResponse;
  Result := True;

  DecodeArchiveFromRequest(AAppMsg, BmpStr, CompressionAlgorithm, UsingCompression);

  MemStream := TMemoryStream.Create;
  DecompressedStream := TMemoryStream.Create;
  try
    MemStream.SetSize(Length(BmpStr));
    MemStream.Write(BmpStr[1], Length(BmpStr));
    MemStream.Position := 0;

    TempMemArchive := TMemArchive.Create;
    TempArchiveHandlers := TArchiveHandlers.Create;
    try
      TempArchiveHandlers.OnAddToLogNoObj := @AddToLog;

      TempMemArchive.OnCompress := @TempArchiveHandlers.HandleOnCompress;
      TempMemArchive.OnDecompress := @TempArchiveHandlers.HandleOnDecompress;
      TempMemArchive.OnComputeArchiveHash := @TempArchiveHandlers.HandleOnComputeArchiveHash;

      if UsingCompression then
      begin
        TempMemArchive.CompressionLevel := 9;
        TempArchiveHandlers.CompressionAlgorithm := CompressionAlgorithmsStrToType(CompressionAlgorithm);
      end
      else
        TempMemArchive.CompressionLevel := 0;

      try
        tk := GetTickCount64;
        TempMemArchive.OpenArchive(MemStream, False);
        tk := GetTickCount64 - tk;
        try
          TempMemArchive.ExtractToStream(CBackgroundFileNameInArchive, DecompressedStream);
          AddToLog('Decompressed archive with background in ' + FloatToStrF(tk / 1000, ffNumber, 15, 5) + 's.  Compressed size: ' + IntToStr(MemStream.Size) + '  Background decompressed size: ' + IntToStr(DecompressedStream.Size));

          SaveBackgroundBmpToInMemFS(DecompressedStream);

          DecompressedStream.Position := 0;
          frmFindSubControlWorkerMain.imgFindSubControlBackground.Picture.Bitmap.LoadFromStream(DecompressedStream);

          CmdResult := SendFileToUIClicker_ExtRndInMem(DecompressedStream, CBackgroundFileNameForUIClicker);
          frmFindSubControlWorkerMain.AddToLog('Sending "' + CBackgroundFileNameInArchive + '" to UIClicker. Response: ' + CmdResult);
          DecompressedStream.Clear;

          if CmdResult = 'Client exception: Connect timed out.' then
          begin
            AResponse := '$ExecAction_Err$=Timeout sending the background bitmap to UIClicker.';
            AErrMsg := 'Cannot respond with FindSubControl result on sending background image to UIClicker.';
            Result := False;
            Exit;
          end;
        finally
          TempMemArchive.CloseArchive;
        end;
      except
        on E: Exception do
        begin
          frmFindSubControlWorkerMain.AddToLog('Error working with received archive: "' + E.Message + '"  MemStream.Size = ' + IntToStr(MemStream.Size));
          /////////////////// Set result to False
        end;
      end;
    finally
      TempArchiveHandlers.Free;
      TempMemArchive.Free;
    end;
  finally
    MemStream.Free;
    DecompressedStream.Free;
  end;
end;


function ProcessFindSubControlRequest(AAppMsg, AThisWorkerTask: string; out AResponse, AErrMsg: string): Boolean;
var
  BmpStr: string;
  UsingCompression: Boolean;
  CompressionAlgorithm: string;
  MemStream, DecompressedStream: TMemoryStream;
  TempMemArchive: TMemArchive;
  TempArchiveHandlers: TArchiveHandlers;
  tk: QWord;
  CmdResult: string;
  i: Integer;
  ListOfArchiveFiles: TStringList;
  TempResponseArchiveStr: string;
begin
  AResponse := '$ExecAction_Err$=Unprocessed FindSubControl request.';
  AErrMsg := '';
  Result := True;

  DecodeArchiveFromRequest(AAppMsg, BmpStr, CompressionAlgorithm, UsingCompression);

  MemStream := TMemoryStream.Create;
  DecompressedStream := TMemoryStream.Create;
  try
    MemStream.SetSize(Length(BmpStr));
    MemStream.Write(BmpStr[1], Length(BmpStr));
    MemStream.Position := 0;

    TempMemArchive := TMemArchive.Create;
    TempArchiveHandlers := TArchiveHandlers.Create;
    try
      TempArchiveHandlers.OnAddToLogNoObj := @AddToLog;

      TempMemArchive.OnCompress := @TempArchiveHandlers.HandleOnCompress;
      TempMemArchive.OnDecompress := @TempArchiveHandlers.HandleOnDecompress;
      TempMemArchive.OnComputeArchiveHash := @TempArchiveHandlers.HandleOnComputeArchiveHash;

      if UsingCompression then
      begin
        TempMemArchive.CompressionLevel := 9;
        TempArchiveHandlers.CompressionAlgorithm := CompressionAlgorithmsStrToType(CompressionAlgorithm);
      end
      else
        TempMemArchive.CompressionLevel := 0;

      try
        tk := GetTickCount64;
        TempMemArchive.OpenArchive(MemStream, False);
        tk := GetTickCount64 - tk;
        try
          AddToLog('Decompressed archive with bitmaps in ' + FloatToStrF(tk / 1000, ffNumber, 15, 5) + 's.  Compressed size: ' + IntToStr(MemStream.Size));

          CmdResult := SendVarsToWorkers(TempMemArchive);
          frmFindSubControlWorkerMain.AddToLog('Sending vars to UIClicker. Response: ' + CmdResult);

          if CmdResult = 'Client exception: Connect timed out.' then
          begin
            AResponse := '$ExecAction_Err$=Timeout sending vars to UIClicker.';
            AErrMsg := 'Cannot respond with FindSubControl result on sending vars to UIClicker.';
            Result := False;
            Exit;
          end;

          ListOfArchiveFiles := TStringList.Create;
          try
            ListOfArchiveFiles.LineBreak := #13#10;
            TempMemArchive.GetListOfFiles(ListOfArchiveFiles);
            for i := 0 to ListOfArchiveFiles.Count - 1 do
              if (ListOfArchiveFiles.Strings[i] <> CBackgroundFileNameInArchive) and
                 (ListOfArchiveFiles.Strings[i] <> CVarsForWorkersInArchive_Names) and
                 (ListOfArchiveFiles.Strings[i] <> CVarsForWorkersInArchive_Values) and
                 (ListOfArchiveFiles.Strings[i] <> CVarsForWorkersInArchive_EvalBefore) then
              begin
                DecompressedStream.Clear;
                TempMemArchive.ExtractToStream(ListOfArchiveFiles.Strings[i], DecompressedStream);
                SaveBmpToInMemFS(DecompressedStream, ListOfArchiveFiles.Strings[i]);

                /////////////////////////////////////////////////////// verify cache here

                if Pos(CExtBmp_PrefixUpperCase, UpperCase(ListOfArchiveFiles.Strings[i])) = 1 then
                  CmdResult := SendFileToUIClicker_ExtRndInMem(DecompressedStream, ListOfArchiveFiles.Strings[i])
                else
                  CmdResult := SendFileToUIClicker_SrvInMem(DecompressedStream, ListOfArchiveFiles.Strings[i]);

                frmFindSubControlWorkerMain.AddToLog('Sending "' + ListOfArchiveFiles.Strings[i] + '" to UIClicker. Response: ' + CmdResult);

                if CmdResult = 'Client exception: Connect timed out.' then
                begin
                  AResponse := '$ExecAction_Err$=Timeout sending bitmap "' + ListOfArchiveFiles.Strings[i] + '" to UIClicker.';
                  AErrMsg := 'Cannot respond with FindSubControl result on sending a bitmap to be searched for, to UIClicker.';
                  Result := False;
                  Exit;
                end;
              end;
          finally
            ListOfArchiveFiles.Free;
          end;
        finally
          TempMemArchive.CloseArchive;
        end;
      except
        on E: Exception do
        begin
          frmFindSubControlWorkerMain.AddToLog('Error working with received archive: "' + E.Message + '"  MemStream.Size = ' + IntToStr(MemStream.Size));
          /////////////////// Set result to False
        end;
      end;
    finally
      TempArchiveHandlers.Free;
      TempMemArchive.Free;
    end;
  finally
    MemStream.Free;
    DecompressedStream.Free;
  end;

  //call CRECmd_ExecuteFindSubControlAction   (later, add support for calling CRECmd_ExecutePlugin)
  frmFindSubControlWorkerMain.AddToLog('Sending FindSubControl request...');
  AResponse := FastReplace_87ToReturn(SendExecuteFindSubControlAction(AAppMsg, AThisWorkerTask));
  frmFindSubControlWorkerMain.AddToLog('FindSubControl result: ' + #13#10 + AResponse);

  AddToLog('Compressing result image = ' + BoolToStr(UsingCompression, 'True', 'False'));
  TempResponseArchiveStr := SendGetDebugImageFromServer(AAppMsg, UsingCompression, CompressionAlgorithm);
  AddToLog('Resulted archive size: ' + IntToStr(Length(TempResponseArchiveStr)));
  //AddToLog('First 10 archive bytes: "' + FastReplace_0To1(Copy(TempResponseArchiveStr, 1, 10)));
  //AddToLog('Last 10 archive bytes: "' + FastReplace_0To1(Copy(TempResponseArchiveStr, Length(TempResponseArchiveStr) - 9, 10)));

  TempResponseArchiveStr := StringToHex(TempResponseArchiveStr);
  AddToLog('Resulted hex archive size: ' + IntToStr(Length(TempResponseArchiveStr)));

  AResponse := AResponse + CProtocolParam_ResponseArchiveSize + '=' + IntToStr(Length(TempResponseArchiveStr)) + #13#10;
  AResponse := AResponse {+ #8#7} + CProtocolParam_ResultImageArchive + '=' + TempResponseArchiveStr;
end;


procedure HandleOnAfterReceivingMQTT_PUBLISH(ClientInstance: DWord; var APublishFields: TMQTTPublishFields; var APublishProperties: TMQTTPublishProperties);
var
  QoS: Byte;
  ID: Word;
  Topic, s, Msg, TempWorkerSpecificTask, ThisWorkerTask, WorkerName: string;
  SetVarRequest: TClkSetVarOptions;
  TempFindSubControlResponse, ProcResponse, ProcErrMsg: string;
  ResponseIndex, i: Integer;
begin
  QoS := (APublishFields.PublishCtrlFlags shr 1) and 3;
  Msg := DynArrayOfByteToString(APublishFields.ApplicationMessage); //StringReplace(DynArrayOfByteToString(APublishFields.ApplicationMessage), #0, '#0', [rfReplaceAll]);
  ID := APublishFields.PacketIdentifier;
  Topic := DynArrayOfByteToString(APublishFields.TopicName); //StringReplace(DynArrayOfByteToString(APublishFields.TopicName), #0, '#0', [rfReplaceAll]);
  TempWorkerSpecificTask := DynArrayOfByteToString(APublishProperties.ContentType);

  WorkerName := frmFindSubControlWorkerMain.lbeClientID.Text;
  ThisWorkerTask := Copy(TempWorkerSpecificTask, Pos(WorkerName + CWorkerTaskAssignmentOperator, TempWorkerSpecificTask) + Length(WorkerName + CWorkerTaskAssignmentOperator), MaxInt);
  ThisWorkerTask := Copy(ThisWorkerTask, 1, Pos(CWorkerTaskLineBreak, ThisWorkerTask) - 1);
  frmFindSubControlWorkerMain.lbeLatestWork.Text := ThisWorkerTask;
  frmFindSubControlWorkerMain.lbeLatestWork.Hint := 'Updated at ' + DateTimeToStr(Now);
  frmFindSubControlWorkerMain.FThisWorkerTask := ThisWorkerTask;

  frmFindSubControlWorkerMain.AddToLog('Received PUBLISH' + #13#10 +
                                       '  ServerPacketIdentifier: ' + IntToStr(ID) + #13#10 +
                                       '  Msg: ' + Copy(StringReplace(Msg, #0, #1, [rfReplaceAll]), 1, 100) + #13#10 +   //Do not display entire content. It may be a bitmap
                                       '  QoS: ' + IntToStr(QoS) + #13#10 +
                                       '  TopicName: ' + Topic + #13#10 +
                                       '  WorkerSpecificTask: ' + TempWorkerSpecificTask + #13#10 +
                                       '  PublishCtrlFlags: ' + IntToStr(APublishFields.PublishCtrlFlags));

  s := '';
  for i := 0 to APublishProperties.SubscriptionIdentifier.Len - 1 do
    s := s + IntToStr(APublishProperties.SubscriptionIdentifier.Content^[i]) + ', ';
  frmFindSubControlWorkerMain.AddToLog('SubscriptionIdentifier(s): ' + s);

  if Topic = CTopicName_AppToWorker_GetCapabilities then
  begin
    ////////////////////////////////// respond with something  (i.e. call MQTT_PUBLISH)
    if not MQTT_PUBLISH(ClientInstance, 0, QoS) then
      frmFindSubControlWorkerMain.AddToLog('Cannot respond with capabilities');
  end;

  if (Topic = TopicWithWorkerName_Background) or (Topic = CTopicName_AppToWorker_SendBackground) then  //common and individual subscriptions
  begin
    frmFindSubControlWorkerMain.AddToLog('Sending background image');
    ProcessSendBackgroundRequest(Msg, ProcResponse, ProcErrMsg);

    frmFindSubControlWorkerMain.AddToLog(ProcErrMsg);

    ResponseIndex := AddItemToResponses(ProcResponse);
    if not MQTT_PUBLISH(ClientInstance, 1 + ResponseIndex shl 8, QoS) then  //ideally, there should be a single MQTT_PUBLISH call like this
    begin
      if ProcErrMsg = '' then
        ProcErrMsg := 'Cannot respond with SendBackground result.';

      frmFindSubControlWorkerMain.AddToLog(ProcErrMsg);
    end;
  end;

  if Topic = TopicWithWorkerName_FindSubControl then
  begin
    ////////////////////////////////// respond with something  (i.e. call MQTT_PUBLISH)    //////////////////// start rendering
    frmFindSubControlWorkerMain.AddToLog('Executing FindSubControl');

    ProcessFindSubControlRequest(Msg, frmFindSubControlWorkerMain.FThisWorkerTask, TempFindSubControlResponse, ProcErrMsg);
    ResponseIndex := AddItemToResponses(TempFindSubControlResponse);
    if not MQTT_PUBLISH(ClientInstance, 3 + ResponseIndex shl 8, QoS) then  //ideally, there should be a single MQTT_PUBLISH call like this
    begin
      if ProcErrMsg = '' then
        ProcErrMsg := 'Cannot respond with FindSubControl result.';

      frmFindSubControlWorkerMain.AddToLog(ProcErrMsg);
    end;

    //call CRECmd_GetResultedDebugImage
  end; //if Topic = TopicWithWorkerName

  if Topic = CTopicName_AppToWorker_GetListOfFonts then
  begin
    frmFindSubControlWorkerMain.AddToLog('Getting the list of fonts.');

    if frmFindSubControlWorkerMain.FReportedFonts <> '' then
      ProcResponse := frmFindSubControlWorkerMain.FReportedFonts
    else
      ProcResponse := GetListOfFontsFromUIClicker;

    ResponseIndex := AddItemToResponses(ProcResponse);
    if not MQTT_PUBLISH(ClientInstance, 4 + ResponseIndex shl 8, QoS) then  //ideally, there should be a single MQTT_PUBLISH call like this
    begin
      if ProcErrMsg = '' then
        ProcErrMsg := 'Cannot respond with FindSubControl result.';

      frmFindSubControlWorkerMain.AddToLog(ProcErrMsg);
    end;
  end;

  frmFindSubControlWorkerMain.AddToLog('');
end;


procedure HandleOnBeforeSending_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Acknowledging with PUBREC for ServerPacketID: ' + IntToStr(ATempPubRecFields.PacketIdentifier));
end;


procedure HandleOnAfterReceiving_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received PUBREC for PacketID: ' + IntToStr(ATempPubRecFields.PacketIdentifier));
end;


//Sending PUBREL after the PUBREC response from server, after the client has sent a PUBLISH packet with QoS=2.
procedure HandleOnBeforeSending_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Acknowledging with PUBREL for PacketID: ' + IntToStr(ATempPubRelFields.PacketIdentifier));
end;


procedure HandleOnAfterReceiving_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received PUBREL for ServerPacketID: ' + IntToStr(ATempPubRelFields.PacketIdentifier));
end;


procedure HandleOnBeforeSending_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Acknowledging with PUBCOMP for PacketID: ' + IntToStr(ATempPubCompFields.PacketIdentifier));
end;


procedure HandleOnAfterReceiving_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received PUBCOMP for ServerPacketID: ' + IntToStr(ATempPubCompFields.PacketIdentifier));
end;


procedure HandleOnAfterReceivingMQTT_PINGRESP(ClientInstance: DWord);
begin
  frmFindSubControlWorkerMain.AddToLog('Received PINGRESP');
end;


procedure HandleOnBeforeSendingMQTT_DISCONNECT(ClientInstance: DWord;  //The lower word identifies the client instance
                                               var ADisconnectFields: TMQTTDisconnectFields;
                                               var ADisconnectProperties: TMQTTDisconnectProperties;
                                               ACallbackID: Word);
begin
  frmFindSubControlWorkerMain.AddToLog('Sending DISCONNECT');
  //ADisconnectFields.EnabledProperties := CMQTTDisconnect_EnSessionExpiryInterval;   //uncomment if needed
  //ADisconnectProperties.SessionExpiryInterval := 1;

  //From spec, pag 89:
  //If the Session Expiry Interval is absent, the Session Expiry Interval in the CONNECT packet is used.
  //If the Session Expiry Interval in the CONNECT packet was zero, then it is a Protocol Error to set a non-
  //zero Session Expiry Interval in the DISCONNECT packet sent by the Client.

  //From spec, pag 89:
  //After sending a DISCONNECT packet the sender
  //  MUST NOT send any more MQTT Control Packets on that Network Connection
  //  MUST close the Network Connection
end;


procedure HandleOnAfterReceivingMQTT_DISCONNECT(ClientInstance: DWord;  //The lower word identifies the client instance
                                                var ADisconnectFields: TMQTTDisconnectFields;
                                                var ADisconnectProperties: TMQTTDisconnectProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received DISCONNECT');

  frmFindSubControlWorkerMain.AddToLog('ADisconnectFields.EnabledProperties' + IntToStr(ADisconnectFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('ADisconnectFields.DisconnectReasonCode' + IntToStr(ADisconnectFields.DisconnectReasonCode));

  frmFindSubControlWorkerMain.AddToLog('ADisconnectProperties.SessionExpiryInterval' + IntToStr(ADisconnectProperties.SessionExpiryInterval));
  frmFindSubControlWorkerMain.AddToLog('ADisconnectProperties.ReasonString' + StringReplace(DynArrayOfByteToString(ADisconnectProperties.ReasonString), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('ADisconnectProperties.ServerReference' + StringReplace(DynArrayOfByteToString(ADisconnectProperties.ServerReference), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('ADisconnectProperties.UserProperty' + StringReplace(DynOfDynArrayOfByteToString(ADisconnectProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}
end;


procedure HandleOnBeforeSendingMQTT_AUTH(ClientInstance: DWord;  //The lower word identifies the client instance
                                         var AAuthFields: TMQTTAuthFields;
                                         var AAuthProperties: TMQTTAuthProperties;
                                         ACallbackID: Word);
begin
  frmFindSubControlWorkerMain.AddToLog('Sending AUTH');
  AAuthFields.AuthReasonCode := $19; //Example: reauth   - see spec, pag 108.

  StringToDynArrayOfByte('SCRAM-SHA-1', AAuthProperties.AuthenticationMethod);       //some example from spec, pag 108
  StringToDynArrayOfByte('client-second-data', AAuthProperties.AuthenticationData);   //some modified example from spec, pag 108
end;


procedure HandleOnAfterReceivingMQTT_AUTH(ClientInstance: DWord;  //The lower word identifies the client instance
                                          var AAuthFields: TMQTTAuthFields;
                                          var AAuthProperties: TMQTTAuthProperties);
begin
  frmFindSubControlWorkerMain.AddToLog('Received AUTH');

  frmFindSubControlWorkerMain.AddToLog('AAuthFields.EnabledProperties' + IntToStr(AAuthFields.EnabledProperties));
  frmFindSubControlWorkerMain.AddToLog('AAuthFields.AuthReasonCode' + IntToStr(AAuthFields.AuthReasonCode));

  frmFindSubControlWorkerMain.AddToLog('AAuthProperties.ReasonString' + StringReplace(DynArrayOfByteToString(AAuthProperties.ReasonString), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('AAuthProperties.ServerReference' + StringReplace(DynArrayOfByteToString(AAuthProperties.AuthenticationMethod), #0, '#0', [rfReplaceAll]));
  frmFindSubControlWorkerMain.AddToLog('AAuthProperties.ServerReference' + StringReplace(DynArrayOfByteToString(AAuthProperties.AuthenticationData), #0, '#0', [rfReplaceAll]));

  {$IFDEF EnUserProperty}
    frmFindSubControlWorkerMain.AddToLog('AAuthProperties.UserProperty' + StringReplace(DynOfDynArrayOfByteToString(AAuthProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  {$ENDIF}
end;

{ TfrmFindSubControlWorkerMain }


procedure TfrmFindSubControlWorkerMain.AddToLog(AMsg: string);  //thread safe
begin
  FLoggingFIFO.Put(AMsg);
end;


procedure TfrmFindSubControlWorkerMain.SyncReceivedBuffer(var AReadBuf: TDynArrayOfByte); //thread safe
begin
  FRecBufFIFO.Put(DynArrayOfByteToString(AReadBuf));
end;


procedure TfrmFindSubControlWorkerMain.ProcessReceivedBuffer;  //called by a timer, to process received data
var
  TempReadBuf: TDynArrayOfByte;
  NewData: string;
begin
  if FRecBufFIFO.Pop(NewData) then
  begin
    //AddToLog('==================Received: ' + StringReplace(NewData, #0, #1, [rfReplaceAll]));
    InitDynArrayToEmpty(TempReadBuf);
    try
      if StringToDynArrayOfByte(NewData, TempReadBuf) then
      begin
        MQTT_PutReceivedBufferToMQTTLib(0, TempReadBuf);
        MQTT_Process(0);
      end
      else
        AddToLog('Out of memory in ProcessReceivedBuffer.');
    finally
      FreeDynArray(TempReadBuf);
    end;
  end;
end;


procedure TfrmFindSubControlWorkerMain.SendString(AString: string);
var
  StrBytes: TIdBytes; //this is array of Byte;
  LenStr: Word;
begin
  LenStr := Length(AString);
  SetLength(StrBytes, LenStr + 2);

  StrBytes[0] := Hi(LenStr);
  StrBytes[1] := Lo(LenStr);

  Move(AString, StrBytes[2], LenStr);

  IdTCPClient1.IOHandler.Write(StrBytes);
end;


procedure TfrmFindSubControlWorkerMain.SendDynArrayOfByte(AArr: TDynArrayOfByte);
var
  TempArr: TIdBytes;
begin
  SetLength(TempArr, AArr.Len);
  Move(AArr.Content^, TempArr[0], AArr.Len);
  IdTCPClient1.IOHandler.Write(TempArr);
end;


procedure TfrmFindSubControlWorkerMain.LogDynArrayOfByte(var AArr: TDynArrayOfByte; ADisplayName: string = '');
var
  i: Integer;
  s: string;
begin
  s := ADisplayName + '  Len: ' + IntToStr(AArr.Len) + '  Data: ';
  for i := 0 to AArr.Len - 1 do
    //s := s + IntToHex(AArr.Content^[i], 2) + ' ';
    s := s + IntToStr(AArr.Content^[i]) + ' ';

  AddToLog(s);
end;


procedure TfrmFindSubControlWorkerMain.InitHandlers;
begin
  {$IFDEF IsDesktop}
    OnMQTTError^ := @HandleOnMQTTError;
    OnSendMQTT_Packet^ := @HandleOnSend_MQTT_Packet;
    OnBeforeMQTT_CONNECT^ := @HandleOnBeforeMQTT_CONNECT;
    OnAfterMQTT_CONNACK^ := @HandleOnAfterMQTT_CONNACK;
    OnBeforeSendingMQTT_PUBLISH^ := @HandleOnBeforeSendingMQTT_PUBLISH;
    OnBeforeSendingMQTT_PUBACK^ := @HandleOnBeforeSendingMQTT_PUBACK;
    OnAfterReceivingMQTT_PUBACK^ := @HandleOnAfterReceivingMQTT_PUBACK;
    OnAfterReceivingMQTT_PUBLISH^ := @HandleOnAfterReceivingMQTT_PUBLISH;
    OnBeforeSendingMQTT_PUBREC^ := @HandleOnBeforeSending_MQTT_PUBREC;
    OnAfterReceivingMQTT_PUBREC^ := @HandleOnAfterReceiving_MQTT_PUBREC;
    OnBeforeSendingMQTT_PUBREL^ := @HandleOnBeforeSending_MQTT_PUBREL;
    OnAfterReceivingMQTT_PUBREL^ := @HandleOnAfterReceiving_MQTT_PUBREL;
    OnBeforeSendingMQTT_PUBCOMP^ := @HandleOnBeforeSending_MQTT_PUBCOMP;
    OnAfterReceivingMQTT_PUBCOMP^ := @HandleOnAfterReceiving_MQTT_PUBCOMP;
    OnBeforeSendingMQTT_SUBSCRIBE^ := @HandleOnBeforeSendingMQTT_SUBSCRIBE;
    OnAfterReceivingMQTT_SUBACK^ := @HandleOnAfterReceivingMQTT_SUBACK;
    OnBeforeSendingMQTT_UNSUBSCRIBE^ := @HandleOnBeforeSendingMQTT_UNSUBSCRIBE;
    OnAfterReceivingMQTT_UNSUBACK^ := @HandleOnAfterReceivingMQTT_UNSUBACK;
    OnAfterReceivingMQTT_PINGRESP^ := @HandleOnAfterReceivingMQTT_PINGRESP;
    OnBeforeSendingMQTT_DISCONNECT^ := @HandleOnBeforeSendingMQTT_DISCONNECT;
    OnAfterReceivingMQTT_DISCONNECT^ := @HandleOnAfterReceivingMQTT_DISCONNECT;
    OnBeforeSendingMQTT_AUTH^ := @HandleOnBeforeSendingMQTT_AUTH;
    OnAfterReceivingMQTT_AUTH^ := @HandleOnAfterReceivingMQTT_AUTH;
  {$ELSE}
    OnMQTTError := @HandleOnMQTTError;
    OnSendMQTT_Packet := @HandleOnSend_MQTT_Packet;
    OnBeforeMQTT_CONNECT := @HandleOnBeforeMQTT_CONNECT;
    OnAfterMQTT_CONNACK := @HandleOnAfterMQTT_CONNACK;
    OnBeforeSendingMQTT_PUBLISH := @HandleOnBeforeSendingMQTT_PUBLISH;
    OnBeforeSendingMQTT_PUBACK := @HandleOnBeforeSendingMQTT_PUBACK;
    OnAfterReceivingMQTT_PUBACK := @HandleOnAfterReceivingMQTT_PUBACK;
    OnAfterReceivingMQTT_PUBLISH := @HandleOnAfterReceivingMQTT_PUBLISH;
    OnBeforeSendingMQTT_PUBREC := @HandleOnBeforeSending_MQTT_PUBREC;
    OnAfterReceivingMQTT_PUBREC := @HandleOnAfterReceiving_MQTT_PUBREC;
    OnBeforeSendingMQTT_PUBREL := @HandleOnBeforeSending_MQTT_PUBREL;
    OnAfterReceivingMQTT_PUBREL := @HandleOnAfterReceiving_MQTT_PUBREL;
    OnBeforeSendingMQTT_PUBCOMP := @HandleOnBeforeSending_MQTT_PUBCOMP;
    OnAfterReceivingMQTT_PUBCOMP := @HandleOnAfterReceiving_MQTT_PUBCOMP;
    OnBeforeSendingMQTT_SUBSCRIBE := @HandleOnBeforeSendingMQTT_SUBSCRIBE;
    OnAfterReceivingMQTT_SUBACK := @HandleOnAfterReceivingMQTT_SUBACK;
    OnBeforeSendingMQTT_UNSUBSCRIBE := @HandleOnBeforeSendingMQTT_UNSUBSCRIBE;
    OnAfterReceivingMQTT_UNSUBACK := @HandleOnAfterReceivingMQTT_UNSUBACK;
    OnAfterReceivingMQTT_PINGRESP := @HandleOnAfterReceivingMQTT_PINGRESP;
    OnBeforeSendingMQTT_DISCONNECT := @HandleOnBeforeSendingMQTT_DISCONNECT;
    OnAfterReceivingMQTT_DISCONNECT := @HandleOnAfterReceivingMQTT_DISCONNECT;
    OnBeforeSendingMQTT_AUTH := @HandleOnBeforeSendingMQTT_AUTH;
    OnAfterReceivingMQTT_AUTH := @HandleOnAfterReceivingMQTT_AUTH;
  {$ENDIF}
end;


procedure TfrmFindSubControlWorkerMain.SendPacketToServer(ClientInstance: DWord);
var
  BufferPointer: PMQTTBuffer;
  Err: Word;
begin
  BufferPointer := MQTT_GetClientToServerBuffer(ClientInstance, Err){$IFnDEF SingleOutputBuffer}^.Content^[0]{$ENDIF};
  SendDynArrayOfByte(BufferPointer^);

  {$IFnDEF SingleOutputBuffer}
    if not MQTT_RemovePacketFromClientToServerBuffer(ClientInstance) then
      AddToLog('Can''t remove latest packet from send buffer.');
  {$ELSE}
    raise Exception.Create('MQTT_RemovePacketFromClientToServerBuffer not implemented for SingleOutputBuffer.');
  {$ENDIF}
end;


type
  TMQTTReceiveThread = class(TThread)
  private
    procedure AddToLog(s: string);
  protected
    procedure Execute; override;
  end;


procedure TMQTTReceiveThread.AddToLog(s: string);
begin
  frmFindSubControlWorkerMain.AddToLog(s);
end;


procedure TMQTTReceiveThread.Execute;
var
  TempReadBuf, ExactPacket: TDynArrayOfByte;
  //ReadCount: Integer;
  TempByte: Byte;
  PacketName: string;
  PacketSize: DWord;
  LoggedDisconnection: Boolean;
  TempArr: TIdBytes;
  SuccessfullyDecoded: Boolean;
  ProcessBufferLengthResult: Word;
begin
  try
    //ReadCount := 0;
    InitDynArrayToEmpty(TempReadBuf);

    try
      LoggedDisconnection := False;
      repeat
        //try
        //  TempByte := frmFindSubControlWorkerMain.IdTCPClient1.IOHandler.ReadByte;
        //  if not AddByteToDynArray(TempByte, TempReadBuf) then
        //  begin
        //    HandleOnMQTTError(0, CMQTT_UserError, CMQTT_UNDEFINED);
        //    AddToLog('Cannot allocate buffer when reading. TempReadBuf.Len = ' + IntToStr(TempReadBuf.Len));
        //    MessageBoxFunction('Cannot allocate buffer when reading.', 'th_', 0);
        //    FreeDynArray(TempReadBuf);
        //  end;
        //except
        //  on E: Exception do      ////////////////// ToDo: switch to EIdReadTimeout
        //  begin
        //    if (E.Message = 'Read timed out.') and (TempReadBuf.Len > 0) then
        //    begin
        //      MQTTPacketToString(TempReadBuf.Content^[0], PacketName);
        //      AddToLog('done receiving packet: ' + E.Message + {'   ReadCount: ' + IntToStr(ReadCount) +} '   E.ClassName: ' + E.ClassName);
        //      AddToLog('Buffer size: ' + IntToStr(TempReadBuf.Len) + '  Packet header: $' + IntToHex(TempReadBuf.Content^[0]) + ' (' + PacketName + ')');
        //
        //      frmFindSubControlWorkerMain.SyncReceivedBuffer(TempReadBuf);
        //
        //      FreeDynArray(TempReadBuf);
        //      //ReadCount := 0; //reset for next packet
        //    end
        //    else
        //      if E.Message = 'Connection Closed Gracefully.' then
        //        if not LoggedDisconnection then
        //        begin
        //          LoggedDisconnection := True;
        //          AddToLog('Disconnected from server. Cannot receive more data. Ex: ' + E.Message);
        //        end;
        //
        //    Sleep(1);
        //  end;
        //end;


        try
          if frmFindSubControlWorkerMain.IdTCPClient1 = nil then //don't mind the race condition, the next iteration will catch the var to be nil
            Exit;

          TempByte := frmFindSubControlWorkerMain.IdTCPClient1.IOHandler.ReadByte;
          if not AddByteToDynArray(TempByte, TempReadBuf) then
          begin
            HandleOnMQTTError(0, CMQTT_UserError, CMQTT_UNDEFINED);
            AddToLog('Cannot allocate buffer when reading. TempReadBuf.Len = ' + IntToStr(TempReadBuf.Len));
            MessageBoxFunction('Cannot allocate buffer when reading.', 'th_', 0);
            FreeDynArray(TempReadBuf);
          end
          else
          begin
            SuccessfullyDecoded := True;                                         //PacketSize should be the expected size, which can be greater than TempReadBuf.Len
            ProcessBufferLengthResult := MQTT_ProcessBufferLength(TempReadBuf, PacketSize);

            //AddToLog('----- PacketSize: ' + IntToStr(PacketSize) + '  Len: ' + IntToStr(TempReadBuf.Len) + '  buffer: ' + FastReplace_0To1(Copy(DynArrayOfByteToString(TempReadBuf), 1, 30)));

            if ProcessBufferLengthResult <> CMQTTDecoderNoErr then
            begin
              SuccessfullyDecoded := False;

              if (ProcessBufferLengthResult = CMQTTDecoderIncompleteBuffer) and (PacketSize > 0) then  //PacketSize is successfully decoded, but the packet is incomplete
              begin
                //to get a complete packet, the number of bytes to be read next is PacketSize - TempReadBuf.Len.
                frmFindSubControlWorkerMain.IdTCPClient1.IOHandler.ReadTimeout := 10;

                SetLength(TempArr, 0);
                frmFindSubControlWorkerMain.IdTCPClient1.IOHandler.ReadBytes(TempArr, PacketSize - TempReadBuf.Len);

                if Length(TempArr) > 0 then //it should be >0, otherwise there should be a read timeout exception
                begin
                  if not AddBufferToDynArrayOfByte(@TempArr[0], Length(TempArr), TempReadBuf) then
                  begin
                    AddToLog('Out of memory on allocating TempReadBuf, for multiple bytes.');
                    MessageBoxFunction('Cannot allocate buffer when reading multiple bytes.', 'th_', 0);
                    FreeDynArray(TempReadBuf);
                  end
                  else
                  begin
                    SetLength(TempArr, 0);
                    ProcessBufferLengthResult := MQTT_ProcessBufferLength(TempReadBuf, PacketSize);
                    SuccessfullyDecoded := ProcessBufferLengthResult = CMQTTDecoderNoErr;
                  end;
                end;

                frmFindSubControlWorkerMain.IdTCPClient1.IOHandler.ReadTimeout := 10; //restore timeout, in case the above is increased
              end;
            end;

            if SuccessfullyDecoded then
            begin
              MQTTPacketToString(TempReadBuf.Content^[0], PacketName);
              AddToLog('done receiving packet');
              AddToLog('Buffer size: ' + IntToStr(TempReadBuf.Len) + '  Packet header: $' + IntToHex(TempReadBuf.Content^[0]) + ' (' + PacketName + ')');

              if PacketSize <> TempReadBuf.Len then
              begin
                if CopyFromDynArray(ExactPacket, TempReadBuf, 0, PacketSize) then
                begin
                  frmFindSubControlWorkerMain.SyncReceivedBuffer(ExactPacket);
                  FreeDynArray(ExactPacket);
                  if not RemoveStartBytesFromDynArray(PacketSize, TempReadBuf) then
                    AddToLog('Cannot remove processed packet from TempReadBuf. Packet type: '+ PacketName);
                end
                else
                  AddToLog('Out of memory on allocating ExactPacket.');
              end
              else
              begin
                frmFindSubControlWorkerMain.SyncReceivedBuffer(TempReadBuf);   //MQTT_Process returns an error for unknown and incomplete packets
                FreeDynArray(TempReadBuf);   //freed here, only when a valid packet is formed
              end;

              Sleep(1);
            end; //SuccessfullyDecoded
          end;
        except
        end;

        //Inc(ReadCount);
      until Terminated;
    finally
      AddToLog('Thread done..');
    end;
  except
    on E: Exception do
      AddToLog('Th ex: ' + E.Message);
  end;
end;


var
  Th: TMQTTReceiveThread;


procedure TfrmFindSubControlWorkerMain.FormCreate(Sender: TObject);
begin
  Th := nil;
  FSkipSavingIni := False;
  FLoggingFIFO := TPollingFIFO.Create;
  FRecBufFIFO := TPollingFIFO.Create;
  FInMemFS := TInMemFileSystem.Create;

  FReportedOS := 'Unknown';
  {$IFDEF Windows}
    FReportedOS := CReportedOS_Win;
  {$ENDIF}
  {$IFDEF UNIX}
    FReportedOS := CReportedOS_Lin;
  {$ENDIF}

  FReportedFonts := '';

  tmrStartup.Enabled := True;
end;


procedure TfrmFindSubControlWorkerMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  tk: QWord;
  ClientToServerBuf: {$IFDEF SingleOutputBuffer} PMQTTBuffer; {$ELSE} PMQTTMultiBuffer; {$ENDIF}
  Err: Word;
begin
  try
    if not FSkipSavingIni then
      SaveSettingsToIni;
  except
  end;

  try
    FPollForMissingServerFilesTh.Terminate;
  except
  end;

  try
    if not MQTT_DISCONNECT(0, 0) then
    begin
      AddToLog('Can''t prepare MQTTDisconnect packet.');
      Exit;
    end;

    tk := GetTickCount64;
    repeat
      ClientToServerBuf := MQTT_GetClientToServerBuffer(0, Err);
      Application.ProcessMessages;
      Sleep(10);
    until (GetTickCount64 - tk > 1500) or ((ClientToServerBuf <> nil) and (ClientToServerBuf^.Len = 0));

    if Th <> nil then
    begin
      Th.Terminate;
      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(10);
      until (GetTickCount64 - tk > 1500) or Th.Terminated;
      FreeAndNil(Th);
    end;

    IdTCPClient1.Disconnect(False);
  finally
    MQTT_DestroyClient(0);
  end;
end;


function GetIniFnm: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'FindSubControlWorker.ini';
end;


procedure TfrmFindSubControlWorkerMain.LoadSettingsFromIni;
var
  Ini: TClkIniReadonlyFile;
  Fnm: string;
begin
  Fnm := GetIniFnm;
  if not FileExists(Fnm) then
    Exit;

  Ini := TClkIniReadonlyFile.Create(Fnm);
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    lbeAddress.Text := Ini.ReadString('Settings', 'Address', lbeAddress.Text);
    lbePort.Text := IntToStr(Ini.ReadInteger('Settings', 'Port', 1883));
    lbeUIClickerPort.Text := IntToStr(Ini.ReadInteger('Settings', 'UIClickerPort', 33444));
    lbeUIClickerPath.Text := Ini.ReadString('Settings', 'UIClickerPath', '');
  finally
    Ini.Free;
  end;
end;


procedure TfrmFindSubControlWorkerMain.SaveSettingsToIni;
var
  Ini: TClkIniFile;
begin
  Ini := TClkIniFile.Create(GetIniFnm);
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteString('Settings', 'Address', lbeAddress.Text);
    Ini.WriteInteger('Settings', 'Port', StrToIntDef(lbePort.Text, 1833));
    Ini.WriteInteger('Settings', 'UIClickerPort', StrToIntDef(lbeUIClickerPort.Text, 33444));
    Ini.WriteString('Settings', 'UIClickerPath', lbeUIClickerPath.Text);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmFindSubControlWorkerMain.LoadSettingsFromCmd;
var
  i: Integer;
begin
  AddToLog('To display cmdline options in log, please run with the --help argument.');

  i := 1;
  repeat
    if ParamStr(i) = '--SetBrokerAddress' then
    begin
      lbeAddress.Text := ParamStr(i + 1);
      AddToLog(ParamStr(i) + ' ' + lbeAddress.Text);
      Inc(i);
    end;

    if ParamStr(i) = '--SetBrokerPort' then
    begin
      lbePort.Text := IntToStr(StrToIntDef(ParamStr(i + 1), 1883));
      AddToLog(ParamStr(i) + ' ' + lbePort.Text);
      Inc(i);
    end;

    if ParamStr(i) = '--SetUIClickerPort' then
    begin
      lbeUIClickerPort.Text := IntToStr(StrToIntDef(ParamStr(i + 1), 33444));
      AddToLog(ParamStr(i) + ' ' + lbeUIClickerPort.Text);
      Inc(i);
    end;

    if ParamStr(i) = '--SetBrokerCredFile' then
    begin
      FCredentialsFile := ParamStr(i + 1);
      AddToLog(ParamStr(i) + ' ' + FCredentialsFile);
      Inc(i);
    end;

    if ParamStr(i) = '--SetWorkerExtraName' then
    begin
      FWorkerExtraName := ParamStr(i + 1);
      AddToLog(ParamStr(i) + ' ' + FWorkerExtraName);
      Inc(i);
    end;

    if ParamStr(i) = '--SetWorkerExtraCaption' then
    begin
      FWorkerExtraCaption := ParamStr(i + 1);
      AddToLog(ParamStr(i) + ' ' + FWorkerExtraCaption);
      Caption := Caption + ' - ' + FWorkerExtraCaption;
      Inc(i);
    end;

    if ParamStr(i) = '--SkipSavingIni' then
    begin
      FSkipSavingIni := True;

      if (ParamStr(i + 1) = 'Yes') or (ParamStr(i + 1) = 'True') then
      begin
        AddToLog(ParamStr(i) + ' ' + ParamStr(i + 1));
        Inc(i);
      end
      else
        if (ParamStr(i + 1) = 'No') or (ParamStr(i + 1) = 'False') then
        begin
          FSkipSavingIni := False;
          AddToLog(ParamStr(i) + ' ' + ParamStr(i + 1));
          Inc(i);
        end
        else
          AddToLog(ParamStr(i));
    end;

    {$IFDEF TestBuild}
      if ParamStr(i) = '--SetReportedOS' then
      begin
        FReportedOS := ParamStr(i + 1);
        AddToLog(ParamStr(i) + ' ' + FReportedOS);
        Inc(i);
      end;

      if ParamStr(i) = '--SetReportedFonts' then
      begin
        FReportedFonts := StringReplace(ParamStr(i + 1), ',', #4#5, [rfReplaceAll]);
        AddToLog(ParamStr(i) + ' ' + FReportedFonts);
        Inc(i);
      end;
    {$ENDIF}

    if UpperCase(ParamStr(i)) = '--HELP' then
    begin
      AddToLog('To set the address the broker is listening on, use  --SetBrokerAddress <Address>');
      AddToLog('To set the port the broker is listening on, use  --SetBrokerPort <Port>');
      AddToLog('To set the port UIClicker is listening on, use  --SetUIClickerPort <Port>');
      AddToLog('To set the full file name with the broker credentials, use  --SetBrokerCredFile <FullPathToFilename>. The file format is ini, with "Username" and "Password" keys under the "Credentials" section (no quotes). Default file is up.txt, near this exe.');
      AddToLog('To skip saving current settings to ini, use  --SkipSavingIni Yes');
      AddToLog('To set the worker extra name, use  --SetWorkerExtraName <Name>. This name is reported in plugin and can be used to further identify the worker. By default, this name is a combination of multiple timestamp and random values.');
      AddToLog('To set the worker extra caption, use  --SetWorkerExtraCaption <Caption>. This caption is concatenated (with a dash) to the existing window caption. It is useful to identify the window, by a master UIClicker, when arranging the windows on desktop.');
      AddToLog('To set the reported operating sytem, use  --SetReportedOS <OS>. This value is sent to the plugin and is available in test builds only.');
      AddToLog('To set the reported fonts, instead of the available sytem fonts, use  --SetReportedFonts <comma-separated list of font names>. This value is sent to the plugin and is available in test builds only.');
    end;

    Inc(i);
  until i >= ParamCount;
end;


procedure TfrmFindSubControlWorkerMain.chkExtServerActiveChange(Sender: TObject);
var
  s: string;
begin
  if chkExtServerActive.Checked then
  begin
    try
      IdHTTPServer1.DefaultPort := StrToIntDef(lbeExtServerPort.Text, 43444);
      IdHTTPServer1.KeepAlive := chkExtServerKeepAlive.Checked;
      IdHTTPServer1.Active := True;

      s := 'Server is listening on port ' + IntToStr(IdHTTPServer1.DefaultPort);

      lblServerInfo.Caption := s;
      lblServerInfo.Font.Color := clGreen;
      lblServerInfo.Hint := '';
    except
      on E: Exception do
      begin
        lblServerInfo.Caption := E.Message;
        lblServerInfo.Font.Color := $000000BB;

        if E.Message = 'Could not bind socket.' then
        begin
          lblServerInfo.Caption := lblServerInfo.Caption + '  (hover for hint)';
          lblServerInfo.Hint := 'Make sure there is no other instance of UIClicker or other application listening on the port.';
          lblServerInfo.Hint := lblServerInfo.Hint + #13#10 + 'If there is another application, started by UIClicker in server mode, with inherited handles, it may keep the socket in use.';
        end;
      end;
    end;
  end
  else
  begin
    IdHTTPServer1.Active := False;
    lblServerInfo.Caption := 'Server module is inactive';
    lblServerInfo.Font.Color := clGray;
    lblServerInfo.Hint := '';
  end;
end;


procedure TfrmFindSubControlWorkerMain.btnConnectionClick(Sender: TObject);
var
  tk: QWord;
  ClientToServerBuf: {$IFDEF SingleOutputBuffer} PMQTTBuffer; {$ELSE} PMQTTMultiBuffer; {$ENDIF}
  Err: Word;
begin
  if btnConnection.Tag = 0 then //Disconnect
  begin
    if Th = nil then
      Exit;

    btnConnection.Enabled := False;
    try
      if not MQTT_UNSUBSCRIBE(0, 0) then
      begin
        AddToLog('Can''t prepare MQTT_UNSUBSCRIBE packet.');
        //Exit;
      end;

      memLog.Lines.Add(DateTimeToStr(Now) + '  Unsubscribing...');

      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
      until (GetTickCount64 - tk > 5000) or (lblBrokerConnectionStatus.Caption = CBrokerIsDisconnectedStatus);

      memLog.Lines.Add(DateTimeToStr(Now) + '  Done waiting for unsubscribe response...');
      memLog.Lines.Add(DateTimeToStr(Now) + '  Disconnecting...');

      if not MQTT_DISCONNECT(0, 0) then
      begin
        AddToLog('Can''t prepare MQTTDisconnect packet.');
        //Exit;
      end;

      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
      until GetTickCount64 - tk > 800;   //wait a bit, so that the last log entries are processed

      //try
      //  tk := GetTickCount64;
      //  repeat
      //    ClientToServerBuf := MQTT_GetClientToServerBuffer(0, Err);
      //    Application.ProcessMessages;
      //    Sleep(10);
      //  until (GetTickCount64 - tk > 1500) or ((ClientToServerBuf <> nil) and (ClientToServerBuf^.Len = 0));
      //except
      //end;

      memLog.Lines.Add(DateTimeToStr(Now) + '  Stopping timers...');
      tmrProcessRecData.Enabled := False;
      tmrProcessLog.Enabled := False;
      Th.Terminate;
      tk := GetTickCount64;
      repeat
        Application.ProcessMessages;
        Sleep(10);
      until (GetTickCount64 - tk > 1500) or Th.Terminated;
      FreeAndNil(Th);

      memLog.Lines.Add(DateTimeToStr(Now) + '  Closing connection...');
      IdTCPClient1.Disconnect(False);
      memLog.Lines.Add(DateTimeToStr(Now) + '  Connection closed...');

      btnConnection.Tag := 1;
      btnConnection.Caption := 'Connect';
      Exit;
    finally
      btnConnection.Enabled := True;
    end;
  end;

  if btnConnection.Tag = 1 then //Connect
  begin
    btnConnection.Enabled := False;  //reenabled by tmrConnect timer
    memLog.Lines.Add('');
    memLog.Lines.Add(DateTimeToStr(Now) + '  Reconnecting...');
    tmrProcessLog.Enabled := True;
    tmrProcessRecData.Enabled := True;
    tmrConnect.Enabled := True;

    btnConnection.Tag := 0;
    btnConnection.Caption := 'Disconnect';
  end;
end;


procedure TfrmFindSubControlWorkerMain.btnBrowseUIClickerPathClick(
  Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;

  lbeUIClickerPath.Text := OpenDialog1.FileName;
  SetFileProviderMainDirs;
end;


procedure TfrmFindSubControlWorkerMain.btnGetListOfFontsClick(Sender: TObject);
var
  i: Integer;
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    TempList.LineBreak := #13#10;
    TempList.Text := FastReplace_45ToReturn(GetListOfFontsFromUIClicker);
    for i := 0 to TempList.Count - 1 do
      AddToLog(TempList.Strings[i]);
  finally
    TempList.Free;
  end;
end;


procedure TfrmFindSubControlWorkerMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLoggingFIFO);
  FreeAndNil(FRecBufFIFO);
  FreeAndNil(Th);
  MQTT_Done;

  try
    FreeAndNil(FPollForMissingServerFilesTh); //Enclosed by try..except, in case the app is closed before executing the tmrStartupTimer timer, which creates this object.
  except
  end;

  FreeAndNil(FInMemFS);
end;


const
  CRECmd_GetImage = 'GetImage';
  CRECmd_Dummy = 'Dummy';


procedure GenerateErrBmp(AFnm: string; ADestStream: TStream);
var
  Bmp: TBitmap;
  Err: string;
  WH: TSize;
begin
  Bmp := TBitmap.Create;
  try
    Err := 'File not found in rendering server: ' + AFnm;
    Bmp.PixelFormat := pf24bit;
    Bmp.Canvas.Font.Size := 10;
    Bmp.Canvas.Font.Color := clYellow;
    Bmp.Canvas.Brush.Color := clBlack;
    WH := Bmp.Canvas.TextExtent(Err);

    Bmp.Width := WH.Width + 10;
    Bmp.Height := WH.Height + 10;
    Bmp.Canvas.TextOut(5, 5, Err);

    Bmp.SaveToStream(ADestStream);
  finally
    Bmp.Free;
  end;
end;


//procedure LocalLoadFileFromMemToStream(AFnm: string; AInMemFS: TInMemFileSystem; ADestStream: TStream);
//begin
//  AInMemFS.LoadFileFromMemToStream(AFnm, ADestStream);
//end;


procedure TfrmFindSubControlWorkerMain.IdHTTPServer1CommandGet(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  Cmd: string;
  s: string;
  Fnm: string;
begin
  Cmd := ARequestInfo.Document;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + CRECmd_Dummy then
  begin
    AResponseInfo.ContentText := '';  //maybe not needed
    AResponseInfo.ContentType := 'text/html';

    if AResponseInfo.ContentStream = nil then
      AResponseInfo.ContentStream := TMemoryStream.Create;

    s := 'Dummy';
    AResponseInfo.ContentStream.Write(s[1], Length(s));
    Exit;
  end;

  if Cmd = '/' + CRECmd_GetImage then
  begin
    AResponseInfo.ContentText := '';

    AResponseInfo.ContentType := 'image/bmp'; //'application/octet-stream';
    AResponseInfo.ContentDisposition := 'inline'; //display it in browser
    AResponseInfo.CharSet := 'US-ASCII';  //this is actually required, to prevent converting ASCII characters from 128-255 to '?'

    AResponseInfo.ContentStream := TMemoryStream.Create;
    try
      Fnm := ARequestInfo.Params.Values['FileName'];
      if FInMemFS.FileExistsInMem(Fnm) then
        FInMemFS.LoadFileFromMemToStream(Fnm, TMemoryStream(AResponseInfo.ContentStream))   ///////////////////// not sure about this typecasting
      else
        GenerateErrBmp(Fnm, AResponseInfo.ContentStream);

      AResponseInfo.ContentLength := AResponseInfo.ContentStream.Size;

      AResponseInfo.WriteHeader;
      AResponseInfo.WriteContent;
    finally
      AResponseInfo.ContentStream.Free;
      AResponseInfo.ContentStream := nil;
    end;

    Exit; //useful if there are other commands after this
  end;
end;


procedure TfrmFindSubControlWorkerMain.IdHTTPServer1Connect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 3600000;   //if no bytes are received in 1h, then close the connection
end;


procedure TfrmFindSubControlWorkerMain.IdHTTPServer1Exception(
  AContext: TIdContext; AException: Exception);
begin
  try
    if AException.Message <> 'Connection Closed Gracefully.' then
      //AddToLogFromThread('Server exception: ' + AException.Message);
  except
  end;
end;


procedure TfrmFindSubControlWorkerMain.lbeUIClickerPortChange(Sender: TObject);
begin
  try
    if FPollForMissingServerFilesTh <> nil then
      if FPollForMissingServerFilesTh.RemoteAddress <> GetUIClickerAddr then
        tmrReconnectFileProvider.Enabled := True;
  except
  end;
end;


procedure TfrmFindSubControlWorkerMain.tmrConnectTimer(Sender: TObject);
var
  tk: QWord;
begin
  AddToLog('Connecting to broker...');
  IdTCPClient1.OnConnected := @HandleClientOnConnected;
  IdTCPClient1.OnDisconnected := @HandleClientOnDisconnected;

  //btnConnect.Enabled := False;   //btnConnection.Enabled := True;
  try
    try
      IdTCPClient1.Connect(lbeAddress.Text, StrToIntDef(lbePort.Text, 1883));
      IdTCPClient1.IOHandler.ReadTimeout := 10;
      //AddToLog('Connected to broker...');

      if Th <> nil then
      begin
        Th.Terminate;
        tk := GetTickCount64;
        repeat
          Application.ProcessMessages;
          Sleep(10);
        until (GetTickCount64 - tk > 1500) or Th.Terminated;
        Th := nil;
      end;

      Th := TMQTTReceiveThread.Create(True);
      Th.FreeOnTerminate := False;
      Th.Start;

      if not MQTT_CONNECT(0, 0) then
      begin
        AddToLog('Can''t prepare MQTTConnect packet.');
        Exit;
      end;

      tmrConnect.Enabled := False;
      tmrSubscribe.Enabled := True;
    except
      on E: Exception do
        AddToLog('Can''t connect.  ' + E.Message + '   Class: ' + E.ClassName);
    end;
  finally
    //btnConnect.Enabled := True;
    btnConnection.Enabled := True;
  end;
end;


procedure TfrmFindSubControlWorkerMain.tmrProcessLogTimer(Sender: TObject);
var
  Msg: string;
begin
  if FLoggingFIFO.Pop(Msg) then
    memLog.Lines.Add(DateTimeToStr(Now) + '  ' + (Msg));
end;


procedure TfrmFindSubControlWorkerMain.tmrProcessRecDataTimer(Sender: TObject);
begin
  ProcessReceivedBuffer;
end;


procedure TfrmFindSubControlWorkerMain.tmrReconnectFileProviderTimer(
  Sender: TObject);
var
  tk: QWord;
begin
  tmrReconnectFileProvider.Enabled := False;

  try
    if (FPollForMissingServerFilesTh <> nil) and (FPollForMissingServerFilesTh.RemoteAddress <> GetUIClickerAddr) then
    begin
      try
        FPollForMissingServerFilesTh.Terminate;
      except
        on E: Exception do
          AddToLog('Stopping FileProvider: ' + E.Message);
      end;

      lbeUIClickerPort.Enabled := False;
      try
        try
          tk := GetTickCount64;
          repeat
            Application.ProcessMessages;
            Sleep(10);

            if FPollForMissingServerFilesTh.Done then
              Break;
          until GetTickCount64 - tk > 1000;

          try
            FreeAndNil(FPollForMissingServerFilesTh); //Enclosed by try..except, in case the app is closed before executing the tmrStartupTimer timer, which creates this object.
          except
          end;
        finally
          InitFileProvider;
        end;
      finally
        lbeUIClickerPort.Enabled := True;
      end;
    end;
  except  //catch all exceptions here, because the worker should display no pop-ups
    on E: Exception do
      AddToLog('Stopping FileProvider ex: ' + E.Message);
  end;
end;


procedure TfrmFindSubControlWorkerMain.SetFileProviderMainDirs;
begin
  FPollForMissingServerFilesTh.FullAppDir := ExtractFileDir(lbeUIClickerPath.Text);
  FPollForMissingServerFilesTh.FullTemplatesDir := FPollForMissingServerFilesTh.FullAppDir + PathDelim + 'ActionTemplates'; //hardcoded for now to 'ActionTemplates'
end;


procedure TfrmFindSubControlWorkerMain.InitFileProvider;
begin
  AddToLog('Starting file provider...');
  FPollForMissingServerFilesTh := TPollForMissingServerFiles.Create(True);
  FPollForMissingServerFilesTh.RemoteAddress := GetUIClickerAddr;

  FPollForMissingServerFilesTh.OnLoadMissingFileContent := @HandleOnLoadMissingFileContent;
  FPollForMissingServerFilesTh.OnFileExists := @HandleOnFileExists;
  FPollForMissingServerFilesTh.OnLogMissingServerFile := @HandleOnLogMissingServerFile;
  FPollForMissingServerFilesTh.OnDenyFile := @HandleOnDenyFile;

  SetFileProviderMainDirs;
  FPollForMissingServerFilesTh.AddListOfAccessibleDirs('$AppDir$' + PathDelim + 'ActionTemplates' + PathDelim);
  FPollForMissingServerFilesTh.AddListOfAccessibleDirs(FPollForMissingServerFilesTh.FullTemplatesDir + PathDelim);
  FPollForMissingServerFilesTh.AddListOfAccessibleDirs(CExtBmp_Prefix + PathDelim);
  FPollForMissingServerFilesTh.AddListOfAccessibleFileExtensions('.clktmpl');
  FPollForMissingServerFilesTh.AddListOfAccessibleFileExtensions('.bmp');
  FPollForMissingServerFilesTh.AddListOfAccessibleFileExtensions('.png');
  FPollForMissingServerFilesTh.AddListOfAccessibleFileExtensions('.pmtv');

  FPollForMissingServerFilesTh.ConnectTimeout := 500;
  FPollForMissingServerFilesTh.Start;
  AddToLog('File provider is initialized. Remote address is set to ' + GetUIClickerAddr);
end;


procedure TfrmFindSubControlWorkerMain.tmrStartupTimer(Sender: TObject);
var
  Content: TClkIniReadonlyFile;
begin
  tmrStartup.Enabled := False;
  FCredentialsFile := ExtractFilePath(ParamStr(0)) + 'up.txt';
  Randomize;
  FWorkerExtraName := DateTimeToStr(Now) + '_' + IntToStr(GetTickCount64) + '_' + IntToStr(Random(MaxInt));

  FMQTTUsername := 'Username';
  FMQTTPassword := '';

  LoadSettingsFromIni;
  LoadSettingsFromCmd;

  tmrProcessLog.Enabled := True;
  tmrProcessRecData.Enabled := True;

  Content := TClkIniReadonlyFile.Create(FCredentialsFile);
  try
    if FileExists(FCredentialsFile) then
    begin
      FMQTTUsername := Content.ReadString('Credentials', 'Username', 'Username');
      FMQTTPassword := Content.ReadString('Credentials', 'Password', '');
    end
    else
      AddToLog('Password file not found. Using empty password..');
  finally
    Content.Free;
  end;

  {$IFDEF UsingDynTFT}
    MM_Init;
  {$ENDIF}

  MQTT_Init;
  if not MQTT_CreateClient then
    AddToLog('Can''t create client...');

  InitHandlers;

  {$IFnDEF Windows}
    Font.Size := 8;
    grpMQTT.Font.Size := Font.Size;
    lbeAddress.Font.Size := Font.Size;
    lbePort.Font.Size := Font.Size;
    lbeClientID.Font.Size := Font.Size;
    lbeUIClickerPort.Font.Size := Font.Size;
    lbeUIClickerPath.Font.Size := Font.Size;

    lbeAddress.EditLabel.Font.Size := Font.Size;
    lbePort.EditLabel.Font.Size := Font.Size;
    lbeClientID.EditLabel.Font.Size := Font.Size;
    lbeUIClickerPort.EditLabel.Font.Size := Font.Size;
    lbeUIClickerPath.EditLabel.Font.Size := Font.Size;

    memLog.Font.Size := Font.Size;
  {$ENDIF}

  InitFileProvider;

  tmrConnect.Enabled := True;
end;


procedure TfrmFindSubControlWorkerMain.tmrSubscribeTimer(Sender: TObject);
begin
  if not MQTT_SUBSCRIBE(0, 0) then
  begin
    AddToLog('Can''t prepare MQTT_SUBSCRIBE packet.');
    Exit;
  end;
end;


function TfrmFindSubControlWorkerMain.ResolveTemplatePath(APath: string): string;
begin
  Result := StringReplace(APath, '$TemplateDir$', FPollForMissingServerFilesTh.FullTemplatesDir, [rfReplaceAll]);
  //Result := StringReplace(Result, '$SelfTemplateDir$', ACustomSelfTemplateDir, [rfReplaceAll]);  //not available here
  Result := StringReplace(Result, '$AppDir$', FPollForMissingServerFilesTh.FullAppDir, [rfReplaceAll]);
end;


procedure TfrmFindSubControlWorkerMain.ShowBrokerIsConnected(ASrcCall: string);
begin
  lblBrokerConnectionStatus.Font.Color := clGreen;
  lblBrokerConnectionStatus.Caption := CBrokerIsConnectedStatus; //used by UIClicker in tests
  AddToLog(lblBrokerConnectionStatus.Caption + ' from "' + ASrcCall + '"');
end;


procedure TfrmFindSubControlWorkerMain.ShowBrokerIsDisconnected(ASrcCall: string);
begin
  lblBrokerConnectionStatus.Font.Color := clMaroon;
  lblBrokerConnectionStatus.Caption := CBrokerIsDisconnectedStatus; //used by UIClicker in tests
  AddToLog(lblBrokerConnectionStatus.Caption + ' from "' + ASrcCall + '"');
end;


procedure TfrmFindSubControlWorkerMain.HandleOnLoadMissingFileContent(AFileName: string; AFileContent: TMemoryStream);
begin
  AFileName := ResolveTemplatePath(AFileName);
  FInMemFS.LoadFileFromMemToStream(AFileName, AFileContent);
end;


function TfrmFindSubControlWorkerMain.HandleOnFileExists(const AFileName: string): Boolean;
var
  TempFileName: string;
begin
  TempFileName := ResolveTemplatePath(AFileName);
  Result := FInMemFS.FileExistsInMem(TempFileName);
end;


procedure TfrmFindSubControlWorkerMain.HandleOnLogMissingServerFile(AMsg: string);
begin
  AddToLog('[FileProvider]: ' + AMsg);
end;


procedure TfrmFindSubControlWorkerMain.HandleOnDenyFile(AFileName: string);
var
  Response: string;
begin                //This handler is executed by a different thread, not the UI one.    -  AddToLog puts an item into a FIFO
  AddToLog('Sending a "' + CRECmd_TerminateWaitingForFileAvailability + '" command to server, because of denied file: "' + AFileName + '".');
  Response := TerminateWaitingForFileAvailability(GetUIClickerAddr, CREParam_TerminateWaitingLoop_ValueAll, 0, False);
  AddToLog('"TerminateWaitingForFileAvailability" response: ' + Response);
end;


procedure TfrmFindSubControlWorkerMain.HandleClientOnConnected(Sender: TObject);
begin
  AddToLog('Connected to broker... on port ' + IntToStr(IdTCPClient1.Port));
end;


procedure TfrmFindSubControlWorkerMain.HandleClientOnDisconnected(Sender: TObject);
begin
  AddToLog('Disconnected from broker...');
  //ShowBrokerIsDisconnected;

  try
    if Th <> nil then
      Th.Terminate;

    FreeAndNil(Th);
  except
  end;
end;


end.

