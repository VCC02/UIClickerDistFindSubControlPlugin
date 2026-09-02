{
    Copyright (C) 2026 VCC
    creation date: 30 Aug 2026  - code moved here from UIClickerDistFindSubControl.ppr
    initial release date: 02 Sep 2026

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


unit DistFindSubControlExec;

{$mode Delphi}

interface

uses
  Windows,
  Interfaces, Classes, SysUtils, Graphics, ExtCtrls, Forms, Math, IdTCPClient,
  DistFindSubControlDM, ImgList, TplZlibUnit, TplLzmaUnit, IntegerList,
  IdGlobal, ClickerUtils, ClickerExtraUtils, ClickerActionPlugins, DllUtils,
  ClickerFileProviderUtils, DynArrays, PollingFIFO, InMemFileSystem,
  DistFindSubControlCommonConsts, ClickerIniFiles, ClickerActionProperties,

  ClickerTemplates, MQTTClient, MQTTUtils, MQTTConnectCtrl, MQTTSubscribeCtrl,
  MQTTUnsubscribeCtrl, MemArchive, DistFindSubControlPluginProperties,
  ClickerPluginInMemFileSystem, ClickerPrimitives, ClickerActionPluginAccess,
  DistFindSubControlPropertyEditorForm, FontSorting, DistFindSubControlFSM;


type
  TOnGetMQTTCredentials = procedure(out AUserName, APassword: string) of object;
  TOnMQTTErrorObj = procedure(ClientInstance: DWord; AErr: Word; APacketType: Byte) of object;
  TOnSyncReceivedBuffer = procedure(var AReadBuf: TDynArrayOfByte) of object;
  TOnAppProcMsg = procedure of object;


  TMQTTReceiveThread = class(TThread)
  private
    FDone: Boolean;
    FIdTCPClient: TIdTCPClient;
    FVerbLevel: Integer;

    FOnAddToLog: TOnAddToLog;
    FOnMQTTError: TOnMQTTErrorObj;
    FOnSyncReceivedBuffer: TOnSyncReceivedBuffer;

    procedure AddMsgToLog(s: string);
    procedure DoOnMQTTError(ClientInstance: DWord; AErr: Word; APacketType: Byte);
    procedure DoOnSyncReceivedBuffer(var AReadBuf: TDynArrayOfByte);

  protected
    procedure Execute; override;

  public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);

    property VerbLevel: Integer read FVerbLevel write FVerbLevel;
    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnMQTTError: TOnMQTTErrorObj write FOnMQTTError;
    property OnSyncReceivedBuffer: TOnSyncReceivedBuffer write FOnSyncReceivedBuffer;
  end;


  TDistExec = class
  private
    FDistFSM: TDistFSM;

    IdTCPClient1: TIdTCPClient;
    FRecBufFIFO: TPollingFIFO; //used by the reading thread to pass data to MQTT library
    //DebugPoint: TDbgPoint;
    //TempPluginMQTTClientHandlers: TPluginMQTTClientHandlers;        ///////////////////////////////////////del

    FAllWorkers: TWorkerArr;
    FAllWorkerBackgroundBmpStr: string;

    FGetAllControls: Boolean;  //will be replaced by a TDistPlugin property

    RequestIDCnt: Integer;
    GetCapabilitiesRequestID: string;
    SendBackgroundToAllRequestID: string;
    GetListOfFontsRequestID: string;

    RecTh: TMQTTReceiveThread;

    FVerbLevel: Integer;        //0 = low-level MQTT logging, 1 = Dist plugin general logging, 2 = Dist plugin important logging.
                                //error messages are displayed regardless of this value

    FBrokerAddress: string;
    FBrokerPort: Word;

    tmrProcessRecData: TTimer;

    FOnAddToLog: TOnAddToLog;
    FOnError: TOnAddToLog;
    FOnGetMQTTCredentials: TOnGetMQTTCredentials;
    FOnAppProcMsg: TOnAppProcMsg;
    FOnSaveWorkerCapabilitiesCache: TOnSaveWorkerCapabilitiesCache;
    FOnLoadWorkerCapabilitiesCache: TOnLoadWorkerCapabilitiesCache;

    procedure SetVerbLevel(Value: Integer);
    function GetAllWorkers(Index: Integer): PWorker;
    procedure SetAllWorkers(Index: Integer; Value: PWorker);
    function GetAllWorkersLength: Integer;

    procedure AddToLog(s: string); //calls DoOnAddToLog, to avoid renaming all instances of AddToLog to DoOnAddToLog
    procedure ProcessReceivedBuffer;  //called by a timer, to process received data
    procedure SendDynArrayOfByte(AArr: TDynArrayOfByte);
    procedure SendPacketToServer(ClientInstance: DWord);

    function ConnectToBroker(AAddress: string; APort: Word): Boolean;
    function DisconnectFromBroker: Boolean;

    function GetAllWorkers_AtLeastOneFound: Boolean;
    function GetWorkerIndexByName(AWorkerName: string): Integer;
    procedure AddNewWorkerToList(AName, AOS, AFileCacheInfoContent, AExtraName: string);
    procedure SetWorkerResponseBitmapStream(AWorkerIndex: Integer; AArchiveAsResponseStr, AArchiveSizeStr: string);

    procedure tmrProcessRecDataTimer(Sender: TObject);
    procedure HandleOnAddToLog(s: string);
    procedure HandleClientOnConnected(Sender: TObject);
    procedure HandleClientOnDisconnected(Sender: TObject);

    procedure DoOnAddToLog(s: string);
    procedure DoOnError(s: string);
    procedure DoOnGetMQTTCredentials(out AUserName, APassword: string);
    procedure DoOnAppProcMsg;
    function DoOnSaveWorkerCapabilitiesCache: Boolean;
    function DoOnLoadWorkerCapabilitiesCache: Boolean;

    procedure HandleOnSyncReceivedBuffer(var AReadBuf: TDynArrayOfByte);
    function HandleOnConnectToBroker: Boolean;
    function HandleOnDisconnectFromBroker: Boolean;
    function HandleOnGetAllWorkersCount: Integer;
    function HandleOnSaveWorkerCapabilitiesCache: Boolean;
    function HandleOnLoadWorkerCapabilitiesCache: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StopProcessRecDataTimer;
    //procedure GetAllWorkersContent(var AAllWorkers: TWorkerArr);
    function GetAllWorkersArr: TWorkerArr;

    //The following handlers are public, because they are called by regular procedures from outside:
    procedure HandleOnMQTTError(ClientInstance: DWord; AErr: Word; APacketType: Byte);
    procedure HandleOnSend_MQTT_Packet(ClientInstance: DWord; APacketType: Byte);
    function HandleOnBeforeMQTT_CONNECT(ClientInstance: DWord; var AConnectFields: TMQTTConnectFields; var AConnectProperties: TMQTTConnectProperties; ACallbackID: Word): Boolean;
    procedure HandleOnAfterMQTT_CONNACK(ClientInstance: DWord; var AConnAckFields: TMQTTConnAckFields; var AConnAckProperties: TMQTTConnAckProperties);
    function HandleOnBeforeSendingMQTT_SUBSCRIBE(ClientInstance: DWord; var ASubscribeFields: TMQTTSubscribeFields; var ASubscribeProperties: TMQTTSubscribeProperties; ACallbackID: Word): Boolean;
    procedure HandleOnAfterReceivingMQTT_SUBACK(ClientInstance: DWord; var ASubAckFields: TMQTTSubAckFields; var ASubAckProperties: TMQTTSubAckProperties);
    function HandleOnBeforeSendingMQTT_UNSUBSCRIBE(ClientInstance: DWord; var AUnsubscribeFields: TMQTTUnsubscribeFields; var AUnsubscribeProperties: TMQTTUnsubscribeProperties; ACallbackID: Word): Boolean;
    procedure HandleOnAfterReceivingMQTT_UNSUBACK(ClientInstance: DWord; var AUnsubAckFields: TMQTTUnsubAckFields; var AUnsubAckProperties: TMQTTUnsubAckProperties);
    function HandleOnBeforeSendingMQTT_PUBLISH(ClientInstance: DWord; var APublishFields: TMQTTPublishFields; var APublishProperties: TMQTTPublishProperties; ACallbackID: Word): Boolean;
    procedure HandleOnBeforeSendingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
    procedure HandleOnAfterReceivingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
    procedure HandleOnAfterReceivingMQTT_PUBLISH(ClientInstance: DWord; var APublishFields: TMQTTPublishFields; var APublishProperties: TMQTTPublishProperties);
    procedure HandleOnBeforeSending_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
    procedure HandleOnAfterReceiving_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
    procedure HandleOnBeforeSending_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
    procedure HandleOnAfterReceiving_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
    procedure HandleOnBeforeSending_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
    procedure HandleOnAfterReceiving_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
    procedure HandleOnAfterReceivingMQTT_PINGRESP(ClientInstance: DWord);
    procedure HandleOnBeforeSendingMQTT_DISCONNECT(ClientInstance: DWord; var ADisconnectFields: TMQTTDisconnectFields; var ADisconnectProperties: TMQTTDisconnectProperties; ACallbackID: Word);
    procedure HandleOnAfterReceivingMQTT_DISCONNECT(ClientInstance: DWord; var ADisconnectFields: TMQTTDisconnectFields; var ADisconnectProperties: TMQTTDisconnectProperties);
    procedure HandleOnBeforeSendingMQTT_AUTH(ClientInstance: DWord; var AAuthFields: TMQTTAuthFields; var AAuthProperties: TMQTTAuthProperties; ACallbackID: Word);
    procedure HandleOnAfterReceivingMQTT_AUTH(ClientInstance: DWord; var AAuthFields: TMQTTAuthFields; var AAuthProperties: TMQTTAuthProperties);

    property VerbLevel: Integer read FVerbLevel write SetVerbLevel;
    property BrokerAddress: string read FBrokerAddress write FBrokerAddress;
    property BrokerPort: Word read FBrokerPort write FBrokerPort;
    property AllWorkers[Index: Integer]: PWorker read GetAllWorkers write SetAllWorkers;
    property AllWorkersLength: Integer read GetAllWorkersLength;
    property AllWorkerBackgroundBmpStr: string write FAllWorkerBackgroundBmpStr;
    property GetAllControls: Boolean read FGetAllControls write FGetAllControls;
    property DistFSM: TDistFSM read FDistFSM; //ToDo: move this object to private, then expose only what is needed

    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnError: TOnAddToLog write FOnError;
    property OnGetMQTTCredentials: TOnGetMQTTCredentials write FOnGetMQTTCredentials;
    property OnAppProcMsg: TOnAppProcMsg write FOnAppProcMsg;
    property OnSaveWorkerCapabilitiesCache: TOnSaveWorkerCapabilitiesCache write FOnSaveWorkerCapabilitiesCache;
    property OnLoadWorkerCapabilitiesCache: TOnLoadWorkerCapabilitiesCache write FOnLoadWorkerCapabilitiesCache;
  end;



implementation


constructor TMQTTReceiveThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FOnAddToLog := nil;
  FOnMQTTError := nil;
  FOnSyncReceivedBuffer := nil;
end;


procedure TMQTTReceiveThread.AddMsgToLog(s: string);
begin
  if Assigned(FOnAddToLog) then
    FOnAddToLog(s)
  else
    raise Exception.Create('OnAddToLog not assigned.');
end;


procedure TMQTTReceiveThread.DoOnMQTTError(ClientInstance: DWord; AErr: Word; APacketType: Byte);
begin
  if Assigned(FOnMQTTError) then
    FOnMQTTError(ClientInstance, AErr, APacketType)
  else
    raise Exception.Create('OnMQTTError not assigned.');
end;


procedure TMQTTReceiveThread.DoOnSyncReceivedBuffer(var AReadBuf: TDynArrayOfByte);
begin
  if Assigned(FOnSyncReceivedBuffer) then
    FOnSyncReceivedBuffer(AReadBuf)
  else
    raise Exception.Create('OnSyncReceivedBuffer not assigned.');
end;


procedure TMQTTReceiveThread.Execute;
var
  TempReadBuf, ExactPacket: TDynArrayOfByte;
  //ReadCount: Integer;
  TempByte: Byte;
  PacketName: string;
  PacketSize: DWord;
  //LoggedDisconnection: Boolean;
  TempArr: TIdBytes;
  SuccessfullyDecoded: Boolean;
  ProcessBufferLengthResult: Word;
begin
  FDone := False;

  try
    //ReadCount := 0;
    InitDynArrayToEmpty(TempReadBuf);

    try
      //LoggedDisconnection := False;
      repeat
        //try
        //  TempByte := IdTCPClient1.IOHandler.ReadByte;
        //  AddByteToDynArray(TempByte, TempReadBuf);
        //except
        //  on E: Exception do      ////////////////// ToDo: switch to EIdReadTimeout
        //  begin
        //    if (E.Message = 'Read timed out.') and (TempReadBuf.Len > 0) then
        //    begin
        //      MQTTPacketToString(TempReadBuf.Content^[0], PacketName);
        //      AddMsgToLog('done receiving packet: ' + E.Message + {'   ReadCount: ' + IntToStr(ReadCount) +} '   E.ClassName: ' + E.ClassName);
        //      AddMsgToLog('Buffer size: ' + IntToStr(TempReadBuf.Len) + '  Packet header: $' + IntToHex(TempReadBuf.Content^[0]) + ' (' + PacketName + ')');
        //
        //      SyncReceivedBuffer(TempReadBuf);
        //
        //      FreeDynArray(TempReadBuf);
        //      //ReadCount := 0; //reset for next packet
        //    end
        //    else
        //      if E.Message = 'Connection Closed Gracefully.' then
        //        if not LoggedDisconnection then
        //        begin
        //          LoggedDisconnection := True;
        //          AddMsgToLog('Disconnected from server. Cannot receive more data. Ex: ' + E.Message);
        //        end;
        //
        //    Sleep(1);
        //  end;
        //end;

        try
          if FIdTCPClient = nil then //don't mind the race condition, the next iteration will catch the var to be nil
          begin
            FDone := True;
            Exit;
          end;

          TempByte := FIdTCPClient.IOHandler.ReadByte;
          if not AddByteToDynArray(TempByte, TempReadBuf) then
          begin
            DoOnMQTTError(0, CMQTT_UserError, CMQTT_UNDEFINED);
            AddMsgToLog('Cannot allocate buffer when reading. TempReadBuf.Len = ' + IntToStr(TempReadBuf.Len));
            MessageBoxFunction('Cannot allocate buffer when reading.', 'th_', 0);
            FreeDynArray(TempReadBuf);
            Sleep(1000);
          end
          else
          begin
            SuccessfullyDecoded := True;                                         //PacketSize should be the expected size, which can be greater than TempReadBuf.Len
            ProcessBufferLengthResult := MQTT_ProcessBufferLength(TempReadBuf, PacketSize);

            if ProcessBufferLengthResult <> CMQTTDecoderNoErr then
            begin
              SuccessfullyDecoded := False;

              if (ProcessBufferLengthResult = CMQTTDecoderIncompleteBuffer) and (PacketSize > 0) then  //PacketSize is successfully decoded, but the packet is incomplete
              begin
                //to get a complete packet, the number of bytes to be read next is PacketSize - TempReadBuf.Len.
                FIdTCPClient.IOHandler.ReadTimeout := 10;

                SetLength(TempArr, 0);
                FIdTCPClient.IOHandler.ReadBytes(TempArr, PacketSize - TempReadBuf.Len);

                if Length(TempArr) > 0 then //it should be >0, otherwise there should be a read timeout exception
                begin
                  if not AddBufferToDynArrayOfByte(@TempArr[0], Length(TempArr), TempReadBuf) then
                  begin
                    AddMsgToLog('Out of memory on allocating TempReadBuf, for multiple bytes.');
                    MessageBoxFunction('Cannot allocate buffer when reading multiple bytes.', 'th_', 0);
                    FreeDynArray(TempReadBuf);
                    Sleep(1000);
                  end
                  else
                  begin
                    SetLength(TempArr, 0);
                    ProcessBufferLengthResult := MQTT_ProcessBufferLength(TempReadBuf, PacketSize);
                    SuccessfullyDecoded := ProcessBufferLengthResult = CMQTTDecoderNoErr;
                  end;
                end;

                FIdTCPClient.IOHandler.ReadTimeout := 10; //restore timeout, in case the above is increased
              end;
            end;

            if SuccessfullyDecoded then
            begin
              MQTTPacketToString(TempReadBuf.Content^[0], PacketName);

              if FVerbLevel < 1 then
              begin
                AddMsgToLog('done receiving packet');
                AddMsgToLog('Buffer size: ' + IntToStr(TempReadBuf.Len) + '  Packet header: $' + IntToHex(TempReadBuf.Content^[0]) + ' (' + PacketName + ')');
              end;

              if PacketSize <> TempReadBuf.Len then
              begin
                if CopyFromDynArray(ExactPacket, TempReadBuf, 0, PacketSize) then
                begin
                  DoOnSyncReceivedBuffer(ExactPacket);
                  FreeDynArray(ExactPacket);
                  if not RemoveStartBytesFromDynArray(PacketSize, TempReadBuf) then
                    AddMsgToLog('Cannot remove processed packet from TempReadBuf. Packet type: '+ PacketName);
                end
                else
                  AddMsgToLog('Out of memory on allocating ExactPacket.');
              end
              else
              begin
                DoOnSyncReceivedBuffer(TempReadBuf);   //MQTT_Process returns an error for unknown and incomplete packets
                FreeDynArray(TempReadBuf);   //freed here, only when a valid packet is formed
              end;

              Sleep(1);
            end; //SuccessfullyDecoded
          end;
        except
          Sleep(1);
        end;

        //Inc(ReadCount);
      until Terminated;
    finally
      AddMsgToLog(DateTimeToStr(Now) + ' (' + IntToStr(GetTickCount64) + ')  Thread done..');
    end;
  except
    on E: Exception do
      AddMsgToLog('Th ex: ' + E.Message);
  end;

  FDone := True;
end;


constructor TDistExec.Create;
begin
  inherited Create;
  SetLength(FAllWorkers, 0);
  FGetAllControls := False;
  GetCapabilitiesRequestID := 'NotSet';
  SendBackgroundToAllRequestID := 'NotSet';
  GetListOfFontsRequestID := 'NotSet';
  RecTh := nil;

  Randomize;
  RequestIDCnt := Random(MaxInt);

  FOnAddToLog := nil;
  FOnError := nil;
  FOnGetMQTTCredentials := nil;
  FOnAppProcMsg := nil;
  FOnSaveWorkerCapabilitiesCache := nil;
  FOnLoadWorkerCapabilitiesCache := nil;

  FRecBufFIFO := TPollingFIFO.Create;

  FDistFSM := TDistFSM.Create;
  FDistFSM.VerbLevel := VerbLevel;
  FDistFSM.WorkerRespondedCountBG := 0;
  FDistFSM.WorkerRespondedCountFS := 0;
  FDistFSM.WorkerRespondedCountLoF := 0;

  //FDistFSM.DistPluginOptions := FDistPluginOptions;  //do not set FDistFSM.DistPluginOptions here, because FDistPluginOptions is not set yet.
  FDistFSM.OnAddToLog := HandleOnAddToLog;
  FDistFSM.OnConnectToBroker := HandleOnConnectToBroker;
  FDistFSM.OnGetAllWorkersCount := HandleOnGetAllWorkersCount;
  FDistFSM.OnSaveWorkerCapabilitiesCache := HandleOnSaveWorkerCapabilitiesCache;
  FDistFSM.OnLoadWorkerCapabilitiesCache := HandleOnLoadWorkerCapabilitiesCache;
  FDistFSM.OnDisconnectFromBroker := HandleOnDisconnectFromBroker;

  IdTCPClient1 := TIdTCPClient.Create;
  IdTCPClient1.OnConnected := HandleClientOnConnected;
  IdTCPClient1.OnDisconnected := HandleClientOnDisconnected;

  tmrProcessRecData := TTimer.Create(nil);
  tmrProcessRecData.Interval := 5;
  tmrProcessRecData.OnTimer := tmrProcessRecDataTimer;
  tmrProcessRecData.Enabled := True;
end;


destructor TDistExec.Destroy;
begin
  if RecTh <> nil then
    FreeAndNil(RecTh);

  FreeAndNil(tmrProcessRecData);
  FreeAndNil(IdTCPClient1);
  FreeAndNil(FDistFSM);
  SetLength(FAllWorkers, 0);
  FreeAndNil(FRecBufFIFO);

  inherited Destroy;
end;


procedure TDistExec.AddToLog(s: string);
begin
  DoOnAddToLog(s);
end;


procedure TDistExec.SetVerbLevel(Value: Integer);
begin
  FVerbLevel := Value;

  if RecTh <> nil then
    RecTh.VerbLevel := Value;
end;


function TDistExec.GetAllWorkers(Index: Integer): PWorker;
begin
  if (Index < 0) or (Index > Length(FAllWorkers)) then
    raise Exception.Create('AllWorkers index out of range.');

  Result := @FAllWorkers[Index];
end;


procedure TDistExec.SetAllWorkers(Index: Integer; Value: PWorker);
begin
  if (Index < 0) or (Index > Length(FAllWorkers)) then
    raise Exception.Create('AllWorkers index out of range.');

  if Value = nil then
    raise Exception.Create('AllWorkers new value is nil.');

  FAllWorkers[Index] := Value^;
end;


function TDistExec.GetAllWorkersLength: Integer;
begin
  Result := Length(FAllWorkers);
end;


procedure TDistExec.tmrProcessRecDataTimer(Sender: TObject);
var
  i: Integer;
begin
  //try
  //  DebugPoint('ProcessReceivedBuffer timer', '');
  //except
  //end;

  try
    ProcessReceivedBuffer;
  except
    on E: Exception do
    begin
      tmrProcessRecData.Enabled := False;  //disable the timer, to allow calling Application.ProcessMessages
      try
        AddToLog('_________________ Ex on processing received data: ' + E.Message);

        for i := 1 to 10 do
        begin
          Sleep(100); //this loop is pretty fast, so slow it to the point where the user can manually stop the process
          DoOnAppProcMsg;
        end;
      finally
        if IdTCPClient1 <> nil then  //re-enable if not closing
          tmrProcessRecData.Enabled := True;
      end;

      //The following code was causing the timer to keep running, even after pressing the Stop button.
      //The Test_EnsureTheDistPluginStopsExecutionOnBadCredentials test verifyes this. The above code, which stops the timer, should fix the bug.
      //AddToLog('_________________ Ex on processing received data: ' + E.Message);
      //Sleep(1000); //this loop is pretty fast, so slow it to the point where the user can manually stop the process
    end;
  end;
end;


procedure TDistExec.HandleOnAddToLog(s: string);
begin
  DoOnAddToLog(s);
end;


procedure TDistExec.HandleClientOnConnected(Sender: TObject);
begin
  AddToLog('Connected to broker... on port ' + IntToStr(IdTCPClient1.Port));
end;


procedure TDistExec.HandleClientOnDisconnected(Sender: TObject);
var
  tk: QWord;
begin
  AddToLog('Disconnected from broker...');

  if RecTh = nil then
  begin
    AddToLog('+++++++++++++++++++ RecTh is nil on disconnecting. Nothing to wait for then. (This is a race condition)'); //This handler should be called when RecTh is still valid.
    Exit;
  end;

  try
    tk := GetTickCount64;
    RecTh.Terminate;
    repeat
      DoOnAppProcMsg;
      Sleep(1);

      if GetTickCount64 - tk > 2000 then
      begin
        AddToLog('+++++++++++++++++++ Timeout waiting for RecTh to terminate.');
        Break;
      end;
    until RecTh.FDone;
  except
    on E: Exception do
      AddToLog('+++++++++++++++++++ Ex on terminating RecTh: ' + E.Message + '   Waiting duration: ' + IntToStr(GetTickCount64 - tk) + '  RecTh = ' + IntToStr(QWord(RecTh)));
  end;
end;


procedure TDistExec.StopProcessRecDataTimer;
begin
  tmrProcessRecData.Enabled := False;
end;


//procedure TDistExec.GetAllWorkersContent(var AAllWorkers: TWorkerArr);
//var
//  i: Integer;
//begin
//  SetLength(AAllWorkers, Length(FAllWorkers));
//
//  for i := 0 to Length(FAllWorkers) - 1 do
//    AAllWorkers[i] := FAllWorkers[i]; //there are pointers in this structure, but they are not used by the caller
//end;


function TDistExec.GetAllWorkersArr: TWorkerArr;
begin
  Result := FAllWorkers;
end;


procedure TDistExec.HandleOnSyncReceivedBuffer(var AReadBuf: TDynArrayOfByte); //thread safe
begin
  FRecBufFIFO.Put(DynArrayOfByteToString(AReadBuf));
end;


function TDistExec.HandleOnConnectToBroker: Boolean;
begin
  Result := ConnectToBroker(FBrokerAddress, FBrokerPort);
end;


function TDistExec.HandleOnDisconnectFromBroker: Boolean;
begin
  Result := DisconnectFromBroker;
end;


function TDistExec.HandleOnGetAllWorkersCount: Integer;
begin
  Result := Length(FAllWorkers);
end;


function TDistExec.HandleOnSaveWorkerCapabilitiesCache: Boolean;
begin
  Result := DoOnSaveWorkerCapabilitiesCache;
end;


function TDistExec.HandleOnLoadWorkerCapabilitiesCache: Boolean;
begin
  Result := DoOnLoadWorkerCapabilitiesCache;
end;


procedure TDistExec.ProcessReceivedBuffer;  //called by a timer, to process received data
var
  TempReadBuf: TDynArrayOfByte;
  NewData: string;
begin
  if FRecBufFIFO.Pop(NewData) then
  begin
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


procedure TDistExec.SendDynArrayOfByte(AArr: TDynArrayOfByte);
var
  TempArr: TIdBytes;
begin
  //AddToLog('============Sending: ' + StringReplace(DynArrayOfByteToString(AArr), #0, #1, [rfReplaceAll]));
  SetLength(TempArr, AArr.Len);
  Move(AArr.Content^, TempArr[0], AArr.Len);
  IdTCPClient1.IOHandler.Write(TempArr);
end;


procedure TDistExec.SendPacketToServer(ClientInstance: DWord);
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
    raise Exception.Create('MQTT_RemovePacketFromClientToServerBuffer no implemented for SingleOutputBuffer.');
  {$ENDIF}
end;


function TDistExec.ConnectToBroker(AAddress: string; APort: Word): Boolean;
begin
  Result := False;
  try
    IdTCPClient1.Connect(AAddress, APort);
    IdTCPClient1.IOHandler.ReadTimeout := 1;

    RecTh := TMQTTReceiveThread.Create(True);
    RecTh.FreeOnTerminate := False;
    RecTh.FIdTCPClient := IdTCPClient1;
    RecTh.VerbLevel := FVerbLevel;
    RecTh.OnAddToLog := HandleOnAddToLog;
    RecTh.OnMQTTError := HandleOnMQTTError;
    RecTh.OnSyncReceivedBuffer := HandleOnSyncReceivedBuffer;
    RecTh.Start;

    if not MQTT_CONNECT(0, 0) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  except
    on E: Exception do
      AddToLog('Can''t connect.  ' + E.Message + '   Class: ' + E.ClassName);
  end; //connect
end;


//procedure DoWaitForAllResponses;
//var
//  tk: QWord;
//begin
//  PublishAckReceived := False;
//  tk := GetTickCount64;
//  repeat
//    Application.ProcessMessages;
//    Sleep(1);
//  until (GetTickCount64 - tk > 1500) or PublishAckReceived;
//end;


function TDistExec.DisconnectFromBroker: Boolean;
var
  tk: QWord;
  ClientToServerBuf: {$IFDEF SingleOutputBuffer} PMQTTBuffer; {$ELSE} PMQTTMultiBuffer; {$ENDIF}
  Err: Word;
  //StillRunning: Boolean;
begin
  Result := False;
  //Disconnecting:
  if not MQTT_DISCONNECT(0, 0) then
  begin
    AddToLog('Can''t disconnect.');
    Exit;
  end;

  tk := GetTickCount64;
  repeat
    ClientToServerBuf := MQTT_GetClientToServerBuffer(0, Err);
    DoOnAppProcMsg;
    Sleep(10);
  until (GetTickCount64 - tk > 1500) or ((ClientToServerBuf <> nil) and (ClientToServerBuf^.Len = 0));

  RecTh.Terminate;
  tk := GetTickCount64;
  repeat
    DoOnAppProcMsg;
    Sleep(1);
  until (GetTickCount64 - tk > 1500) or RecTh.Terminated;

  //StillRunning := not RecTh.Terminated;
  FreeAndNil(RecTh);

  IdTCPClient1.Disconnect(False);
  Result := True;

  //if StillRunning then
  //  MessageBox(0, 'MQTT thread is still running..', 'Dist plugin', 0);
end;


procedure TDistExec.HandleOnMQTTError(ClientInstance: DWord; AErr: Word; APacketType: Byte);
var
  PacketTypeStr: string;
  TempLatestMsg: string;
begin
  MQTTPacketToString(APacketType, PacketTypeStr);
  AddToLog('Client: ' + IntToHex(ClientInstance, 8) + '  Err: $' + IntToHex(AErr) + '  PacketType: $' + IntToHex(APacketType) + ' (' + PacketTypeStr + ').');  //The error is made of an upper byte and a lower byte.

  if Hi(AErr) = CMQTT_Reason_NotAuthorized then   // $87
  begin
    TempLatestMsg := 'MQTT broker error: Not authorized.';
    DoOnError(TempLatestMsg);
    AddToLog(TempLatestMsg);
    if APacketType = CMQTT_CONNACK then
      AddToLog('             on receiving CONNACK.');
  end;

  if Lo(AErr) = CMQTT_PacketIdentifierNotFound_ClientToServer then   // $CE
    AddToLog('Client error: PacketIdentifierNotFound.');
end;


procedure TDistExec.HandleOnSend_MQTT_Packet(ClientInstance: DWord; APacketType: Byte);
var
  PacketName: string;
begin
  MQTTPacketToString(APacketType, PacketName);

  if VerbLevel < 1 then
    AddToLog('Sending ' + PacketName + ' packet...');

  try
    SendPacketToServer(ClientInstance);
  except
    on E: Exception do
      AddToLog('Cannot send ' + PacketName + ' packet... Ex: ' + E.Message);
  end;
end;


procedure TDistExec.DoOnAddToLog(s: string);
begin
  if Assigned(FOnAddToLog) then
    FOnAddToLog(s)
  else
    raise Exception.Create('OnAddToLog not assigned.');
end;


procedure TDistExec.DoOnError(s: string);
begin
  if Assigned(FOnError) then
    FOnError(s)
  else
    raise Exception.Create('OnError not assigned.');
end;


procedure TDistExec.DoOnGetMQTTCredentials(out AUserName, APassword: string);
begin
  if Assigned(FOnGetMQTTCredentials) then
    FOnGetMQTTCredentials(AUserName, APassword)
  else
    raise Exception.Create('OnGetMQTTCredentials not assigned.');
end;


procedure TDistExec.DoOnAppProcMsg;
begin
  if Assigned(FOnAppProcMsg) then
    FOnAppProcMsg()
  else
    raise Exception.Create('OnAppProcMsg not assigned.');
end;


function TDistExec.DoOnSaveWorkerCapabilitiesCache: Boolean;
begin
  if Assigned(FOnSaveWorkerCapabilitiesCache) then
    Result := FOnSaveWorkerCapabilitiesCache()
  else
    raise Exception.Create('OnSaveWorkerCapabilitiesCache not assigned.');
end;


function TDistExec.DoOnLoadWorkerCapabilitiesCache: Boolean;
begin
  if Assigned(FOnLoadWorkerCapabilitiesCache) then
    Result := FOnLoadWorkerCapabilitiesCache()
  else
    raise Exception.Create('OnLoadWorkerCapabilitiesCache not assigned.');
end;


function TDistExec.HandleOnBeforeMQTT_CONNECT(ClientInstance: DWord;  //The lower byte identifies the client instance (the library is able to implement multiple MQTT clients / device). The higher byte can identify the call in user handlers for various events (e.g. TOnBeforeMQTT_CONNECT).
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

  if VerbLevel < 1 then
    AddToLog('Preparing CONNECT data..');

  //Id := Chr((ClientInstance and $FF) + 48);
  //ClientId := 'MyClient' + Id;

  DoOnGetMQTTCredentials(UserName, Password);

  //StringToDynArrayOfByte(ClientId, AConnectFields.PayloadContent.ClientID);
  StringToDynArrayOfByte(UserName, AConnectFields.PayloadContent.UserName);
  StringToDynArrayOfByte(Password, AConnectFields.PayloadContent.Password);

  ConnectFlags := CMQTT_UsernameInConnectFlagsBitMask or
                  CMQTT_PasswordInConnectFlagsBitMask or
                  CMQTT_CleanStartInConnectFlagsBitMask {or
                  CMQTT_WillQoSB1InConnectFlagsBitMask};

  EnabledProperties := CMQTTConnect_EnSessionExpiryInterval or
                       CMQTTConnect_EnMaximumPacketSize or
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
  AddStringToDynOfDynArrayOfByte('Key=Value', TempWillProperties.UserProperty);
  AddStringToDynOfDynArrayOfByte('NewKey=NewValue', TempWillProperties.UserProperty);

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
  AddStringToDynOfDynArrayOfByte('UserProp=Value', AConnectProperties.UserProperty);
  StringToDynArrayOfByte('SCRAM-SHA-1', AConnectProperties.AuthenticationMethod);       //some example from spec, pag 108   the server may add to its log: "bad AUTH method"
  StringToDynArrayOfByte('client-first-data', AConnectProperties.AuthenticationData);   //some example from spec, pag 108

  if VerbLevel < 1 then
  begin
    AddToLog('Done preparing CONNECT data..');
    AddToLog('');
  end;
end;


procedure TDistExec.HandleOnAfterMQTT_CONNACK(ClientInstance: DWord; var AConnAckFields: TMQTTConnAckFields; var AConnAckProperties: TMQTTConnAckProperties);
begin
  FDistFSM.ConAckReceived := True;

  if VerbLevel < 1 then
  begin
    AddToLog('Received CONNACK');

    //AddToLog('ConnAckFields.EnabledProperties: ' + IntToStr(AConnAckFields.EnabledProperties));
    //AddToLog('ConnAckFields.SessionPresentFlag: ' + IntToStr(AConnAckFields.SessionPresentFlag));
    //AddToLog('ConnAckFields.ConnectReasonCode: ' + IntToStr(AConnAckFields.ConnectReasonCode));  //should be 0
    //
    //AddToLog('SessionExpiryInterval: ' + IntToStr(AConnAckProperties.SessionExpiryInterval));
    //AddToLog('ReceiveMaximum: ' + IntToStr(AConnAckProperties.ReceiveMaximum));
    //AddToLog('MaximumQoS: ' + IntToStr(AConnAckProperties.MaximumQoS));
    //AddToLog('RetainAvailable: ' + IntToStr(AConnAckProperties.RetainAvailable));

    if AConnAckFields.EnabledProperties and CMQTTConnAck_EnMaximumPacketSize = CMQTTConnAck_EnMaximumPacketSize then
      AddToLog('MaximumPacketSize received: ' + IntToStr(AConnAckProperties.MaximumPacketSize))
    else
      AddToLog('MaximumPacketSize unknown: ' + IntToStr(AConnAckProperties.MaximumPacketSize));


    //AddToLog('AssignedClientIdentifier: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.AssignedClientIdentifier), #0, '#0', [rfReplaceAll]));
    //AddToLog('TopicAliasMaximum: ' + IntToStr(AConnAckProperties.TopicAliasMaximum));
    //AddToLog('ReasonString: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    //AddToLog('UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(AConnAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
    //AddToLog('WildcardSubscriptionAvailable: ' + IntToStr(AConnAckProperties.WildcardSubscriptionAvailable));
    //AddToLog('SubscriptionIdentifierAvailable: ' + IntToStr(AConnAckProperties.SubscriptionIdentifierAvailable));
    //AddToLog('SharedSubscriptionAvailable: ' + IntToStr(AConnAckProperties.SharedSubscriptionAvailable));
    //AddToLog('ServerKeepAlive: ' + IntToStr(AConnAckProperties.ServerKeepAlive));
    //AddToLog('ResponseInformation: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ResponseInformation), #0, '#0', [rfReplaceAll]));
    //AddToLog('ServerReference: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.ServerReference), #0, '#0', [rfReplaceAll]));
    //AddToLog('AuthenticationMethod: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.AuthenticationMethod), #0, '#0', [rfReplaceAll]));
    //AddToLog('AuthenticationData: ' + StringReplace(DynArrayOfByteToString(AConnAckProperties.AuthenticationData), #0, '#0', [rfReplaceAll]));
    //
  end;

  ///////////////////////////////////////// when the server returns SessionPresentFlag set to 1, the library resends unacknowledged Publish and PubRel packets.
  //AConnAckFields.SessionPresentFlag := 1;
end;


function TDistExec.HandleOnBeforeSendingMQTT_SUBSCRIBE(ClientInstance: DWord;  //The lower word identifies the client instance
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

  if VerbLevel < 1 then
    AddToLog('Subscribing with new SubscriptionIdentifier: ' + IntToStr(SubId));

  Result := FillIn_SubscribePayload(CTopicName_WorkerToApp_GetCapabilities, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters (GetCapabilities).');
    Exit;
  end;

  if VerbLevel < 1 then
    AddToLog('Subscribing to: ' + StringReplace(DynArrayOfByteToString(ASubscribeFields.TopicFilters), #0, '#0', [rfReplaceAll]));

  Result := FillIn_SubscribePayload(CTopicName_WorkerToApp_SendBackground, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters (SendBackground).');
    Exit;
  end;

  Result := FillIn_SubscribePayload(CTopicName_WorkerToApp_FindSubControl, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters (FindSubControl).');
    Exit;
  end;

  Result := FillIn_SubscribePayload(CTopicName_WorkerToApp_GetListOfFonts, Options, ASubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to ASubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_SUBSCRIBE not enough memory to add TopicFilters (GetListOfFonts).');
    Exit;
  end;


  //Enable SubscriptionIdentifier only if required (allocated above with CreateClientToServerSubscriptionIdentifier) !!!
  //The library initializes EnabledProperties to 0.
  //A subscription is allowed to be made without a SubscriptionIdentifier.
  ASubscribeFields.EnabledProperties := CMQTTSubscribe_EnSubscriptionIdentifier {or CMQTTSubscribe_EnUserProperty};

  if VerbLevel < 1 then
  begin
    AddToLog('Subscribing with PacketIdentifier: ' + IntToStr(ASubscribeFields.PacketIdentifier));
    AddToLog('Subscribing to: ' + StringReplace(DynArrayOfByteToString(ASubscribeFields.TopicFilters), #0, '#0', [rfReplaceAll]));

    AddToLog('');
  end;
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_SUBACK(ClientInstance: DWord; var ASubAckFields: TMQTTSubAckFields; var ASubAckProperties: TMQTTSubAckProperties);
var
  i: Integer;
begin
  FDistFSM.SubAckReceived := True;

  if VerbLevel < 1 then
  begin
    AddToLog('Received SUBACK');
    //AddToLog('ASubAckFields.IncludeReasonCode: ' + IntToStr(ASubAckFields.IncludeReasonCode));  //not used
    //AddToLog('ASubAckFields.ReasonCode: ' + IntToStr(ASubAckFields.ReasonCode));              //not used
    AddToLog('ASubAckFields.EnabledProperties: ' + IntToStr(ASubAckFields.EnabledProperties));
    AddToLog('ASubAckFields.PacketIdentifier: ' + IntToStr(ASubAckFields.PacketIdentifier));  //This must be the same as sent in SUBSCRIBE packet.

    AddToLog('ASubAckFields.Payload.Len: ' + IntToStr(ASubAckFields.SrcPayload.Len));

    for i := 0 to ASubAckFields.SrcPayload.Len - 1 do         //these are QoS values for each TopicFilter (if ok), or error codes (if not ok).
      AddToLog('ASubAckFields.ReasonCodes[' + IntToStr(i) + ']: ' + IntToStr(ASubAckFields.SrcPayload.Content^[i]));

    AddToLog('ASubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(ASubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    AddToLog('ASubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(ASubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));

    AddToLog('');
  end;
end;


function TDistExec.HandleOnBeforeSendingMQTT_UNSUBSCRIBE(ClientInstance: DWord;  //The lower word identifies the client instance
                                                         var AUnsubscribeFields: TMQTTUnsubscribeFields;
                                                         var AUnsubscribeProperties: TMQTTUnsubscribeProperties;
                                                         ACallbackID: Word): Boolean;
begin
  Result := FillIn_UnsubscribePayload(CTopicName_WorkerToApp_GetCapabilities, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(CTopicName_WorkerToApp_SendBackground, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(CTopicName_WorkerToApp_FindSubControl, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  Result := FillIn_UnsubscribePayload(CTopicName_WorkerToApp_GetListOfFonts, AUnsubscribeFields.TopicFilters);  //call this again with a different string (i.e. TopicFilter), in order to add it to AUnsubscribeFields.TopicFilters
  if not Result then
  begin
    AddToLog('HandleOnBeforeSendingMQTT_UNSUBSCRIBE not enough memory to add TopicFilters.');
    Exit;
  end;

  if VerbLevel < 1 then
    AddToLog('Unsubscribing from "' + CTopicName_WorkerToApp_GetCapabilities + '" and "' +
                                      CTopicName_WorkerToApp_SendBackground + '" and "' +
                                      CTopicName_WorkerToApp_FindSubControl + '" and "' +
                                      CTopicName_WorkerToApp_GetListOfFonts + '"...'
                                      );

  //the user code should call RemoveClientToServerSubscriptionIdentifier to remove the allocate identifier.
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_UNSUBACK(ClientInstance: DWord; var AUnsubAckFields: TMQTTUnsubAckFields; var AUnsubAckProperties: TMQTTUnsubAckProperties);
var
  i: Integer;
begin
  FDistFSM.UnsubAckReceived := True;

  if VerbLevel < 1 then
  begin
    AddToLog('Received UNSUBACK');
    //AddToLog('AUnsubAckFields.IncludeReasonCode: ' + IntToStr(ASubAckFields.IncludeReasonCode));  //not used
    //AddToLog('AUnsubAckFields.ReasonCode: ' + IntToStr(ASubAckFields.ReasonCode));              //not used
    AddToLog('AUnsubAckFields.EnabledProperties: ' + IntToStr(AUnsubAckFields.EnabledProperties));
    AddToLog('AUnsubAckFields.PacketIdentifier: ' + IntToStr(AUnsubAckFields.PacketIdentifier));  //This must be the same as sent in SUBSCRIBE packet.

    AddToLog('AUnsubAckFields.Payload.Len: ' + IntToStr(AUnsubAckFields.SrcPayload.Len));

    for i := 0 to AUnsubAckFields.SrcPayload.Len - 1 do         //these are QoS values for each TopicFilter (if ok), or error codes (if not ok).
      AddToLog('AUnsubAckFields.ReasonCodes[' + IntToStr(i) + ']: ' + IntToStr(AUnsubAckFields.SrcPayload.Content^[i]));

    AddToLog('AUnsubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(AUnsubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    AddToLog('AUnsubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(AUnsubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));

    AddToLog('');
  end;
end;


//This handler is used when this client publishes a message to broker.
function TDistExec.HandleOnBeforeSendingMQTT_PUBLISH(ClientInstance: DWord;  //The lower word identifies the client instance (the library is able to implement multiple MQTT clients / device). The higher byte can identify the call in user handlers for various events (e.g. TOnBeforeMQTT_CONNECT).
                                                     var APublishFields: TMQTTPublishFields;                    //user code has to fill-in this parameter
                                                     var APublishProperties: TMQTTPublishProperties;            //user code has to fill-in this parameter
                                                     ACallbackID: Word): Boolean;
var
  Msg: string;
  //QoS: Byte;
  TempWorkerSpecificTask: string;
  i: Integer;
  CurrentWorkerIndex: Byte;
begin
  Result := True;
  TempWorkerSpecificTask := 'Task not set.';

  case ACallbackID and $FF of
    CCallbackID_GetCapabilities:
    begin
      GetCapabilitiesRequestID := IntToStr(GetTickCount64) + '_' + IntToStr(RequestIDCnt);
      Inc(RequestIDCnt);

      Msg := CMQTT_AppToWorkerCmd_GetCapabilities + #13#10 + CProtocolParam_RequestID + '=' + GetCapabilitiesRequestID;
      TempWorkerSpecificTask := 'DebuggingString';
      Result := Result and StringToDynArrayOfByte(CTopicName_AppToWorker_GetCapabilities, APublishFields.TopicName);
      APublishFields.EnabledProperties := CMQTTPublish_EnContentType;

      if VerbLevel < 2 then
        AddToLog('Requesting worker capabilities...');
    end;

    CCallbackID_SendBackgroundToAll:
    begin
      SendBackgroundToAllRequestID := IntToStr(GetTickCount64) + '_' + IntToStr(RequestIDCnt);
      Inc(RequestIDCnt);

      Msg := CProtocolParam_RequestID + '=' + SendBackgroundToAllRequestID + '&' + FAllWorkerBackgroundBmpStr;  //FAllWorkerBackgroundBmpStr uses '&' instead of #13#10 and the last string is the background itself
      Result := Result and StringToDynArrayOfByte(CTopicName_AppToWorker_SendBackground, APublishFields.TopicName);

      if VerbLevel < 2 then
        AddToLog('Publishing SendBackground request to all workers...  Topic: ' + CTopicName_AppToWorker_SendBackground + '   Len(Msg): ' + IntToStr(Length(Msg)));

      //List of tasks for all workers. It can be an '&' separated list of WorkerName=TaskInfo, from all workers array.
      //The list separator has to be compatible with MQTT spec about UTF-8 encoding.
      //Every worker extracts its task from this array, based on its name.

      TempWorkerSpecificTask := 'DebuggingName:';
      APublishFields.EnabledProperties := CMQTTPublish_EnContentType;
    end;

    CCallbackID_SendBackgroundToSome:
    begin
      CurrentWorkerIndex := (ACallbackID shr 8) and $FF;  //assuming there are no more than 255 workers
      Msg := FAllWorkerBackgroundBmpStr;
      Result := Result and StringToDynArrayOfByte(CTopicName_AppToWorker_SendBackground + '_' + FAllWorkers[CurrentWorkerIndex].Name, APublishFields.TopicName);

      if VerbLevel < 2 then
        AddToLog('Publishing SendBackground request to worker[' + IntToStr(CurrentWorkerIndex) + ']...');

      //List of tasks for all workers. It can be an '&' separated list of WorkerName=TaskInfo, from all workers array.
      //The list separator has to be compatible with MQTT spec about UTF-8 encoding.
      //Every worker extracts its task from this array, based on its name.

      TempWorkerSpecificTask := 'DebuggingName:';
      APublishFields.EnabledProperties := CMQTTPublish_EnContentType;
    end;

    CCallbackID_SendFindSubControl:
    begin
      CurrentWorkerIndex := (ACallbackID shr 8) and $FF;  //assuming there are no more than 255 workers

      FAllWorkers[CurrentWorkerIndex].FindSubControlRequestID := IntToStr(GetTickCount64) + '_' + IntToStr(RequestIDCnt);
      Inc(RequestIDCnt);

      Msg := CProtocolParam_RequestID + '=' + FAllWorkers[CurrentWorkerIndex].FindSubControlRequestID + '&' + FAllWorkers[CurrentWorkerIndex].WorkerActionContentStr;
      Result := Result and StringToDynArrayOfByte(CTopicName_AppToWorker_FindSubControl + '_' + FAllWorkers[CurrentWorkerIndex].Name, APublishFields.TopicName);
      //AddToLog('---------------------------------- Sending FindSubControl request with ID: ' + FAllWorkers[CurrentWorkerIndex].FindSubControlRequestID);

      if VerbLevel < 2 then
        AddToLog('Publishing FindSubControl request to worker[' + IntToStr(CurrentWorkerIndex) + ']...');

      //List of tasks for all workers. It can be an '&' separated list of WorkerName=TaskInfo, from all workers array.
      //The list separator has to be compatible with MQTT spec about UTF-8 encoding.
      //Every worker extracts its task from this array, based on its name.

      TempWorkerSpecificTask := 'DebuggingName:';
      for i := 0 to Length(FAllWorkers) - 1 do
        TempWorkerSpecificTask := TempWorkerSpecificTask + FAllWorkers[i].Name + {'='}CWorkerTaskAssignmentOperator + FAllWorkers[i].WorkerSpecificTask {+ '&'} + CWorkerTaskLineBreak;

      APublishFields.EnabledProperties := CMQTTPublish_EnContentType;
    end;

    CCallbackID_GetListOfFonts:
    begin
      GetListOfFontsRequestID := IntToStr(GetTickCount64) + '_' + IntToStr(RequestIDCnt);
      Inc(RequestIDCnt);

      Msg := CProtocolParam_RequestID + '=' + GetListOfFontsRequestID;

      Result := Result and StringToDynArrayOfByte(CTopicName_AppToWorker_GetListOfFonts, APublishFields.TopicName);

      if VerbLevel < 2 then
        AddToLog('Requesting GetListOfFonts from worker...');
    end;

    else
      Msg := 'UnhandledRequest';
  end;

  //QoS := (APublishFields.PublishCtrlFlags shr 1) and 3;
  //AddToLog('Publishing "' + Msg + '" at QoS = ' + IntToStr(QoS));  //commented, because Msg may contain bitmaps

  Result := Result and StringToDynArrayOfByte(Msg, APublishFields.ApplicationMessage);
  Result := Result and StringToDynArrayOfByte(TempWorkerSpecificTask, APublishProperties.ContentType);

  if VerbLevel < 2 then
    AddToLog('');
  //QoS can be overriden here. If users override QoS in this handler, then a a different PacketIdentifier might be allocated (depending on what is available)
end;


//This handler is used when this client publishes a message to broker and the broker responds with PUBACK.
procedure TDistExec.HandleOnBeforeSendingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
begin
  if VerbLevel < 1 then
  begin
    AddToLog('Acknowledging with PUBACK');
    AddToLog('APubAckFields.EnabledProperties: ' + IntToStr(APubAckFields.EnabledProperties));
    AddToLog('APubAckFields.IncludeReasonCode: ' + IntToStr(APubAckFields.IncludeReasonCode));
    AddToLog('APubAckFields.PacketIdentifier: ' + IntToStr(APubAckFields.PacketIdentifier));
    AddToLog('APubAckFields.ReasonCode: ' + IntToStr(APubAckFields.ReasonCode));

    AddToLog('APubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(APubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    AddToLog('APubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(APubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));

    AddToLog('');
    //This handler can be used to override what is being sent to server as a reply to PUBLISH
  end;
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_PUBACK(ClientInstance: DWord; var APubAckFields: TMQTTPubAckFields; var APubAckProperties: TMQTTPubAckProperties);
begin
  if VerbLevel < 1 then
  begin
    AddToLog('Received PUBACK');
    //AddToLog('APubAckFields.EnabledProperties: ' + IntToStr(APubAckFields.EnabledProperties));
    //AddToLog('APubAckFields.IncludeReasonCode: ' + IntToStr(APubAckFields.IncludeReasonCode));
    //AddToLog('APubAckFields.PacketIdentifier: ' + IntToStr(APubAckFields.PacketIdentifier));
    //AddToLog('APubAckFields.ReasonCode: ' + IntToStr(APubAckFields.ReasonCode));
    //
    //AddToLog('APubAckProperties.ReasonString: ' + StringReplace(DynArrayOfByteToString(APubAckProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    //AddToLog('APubAckProperties.UserProperty: ' + StringReplace(DynOfDynArrayOfByteToString(APubAckProperties.UserProperty), #0, '#0', [rfReplaceAll]));
    AddToLog('');
  end;
end;


function TDistExec.GetAllWorkers_AtLeastOneFound: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FAllWorkers) - 1 do
    if FAllWorkers[i].ResponseReceived and FAllWorkers[i].FindSubControlFound then
    begin
      Result := True;
      Break;
    end;
end;


function TDistExec.GetWorkerIndexByName(AWorkerName: string): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Length(FAllWorkers) - 1 do
    if FAllWorkers[i].Name = AWorkerName then
    begin
      Result := i;
      Break;
    end;
end;


procedure TDistExec.AddNewWorkerToList(AName, AOS, AFileCacheInfoContent, AExtraName: string);
begin
  SetLength(FAllWorkers, Length(FAllWorkers) + 1);
  FAllWorkers[Length(FAllWorkers) - 1].Name := AName;
  FAllWorkers[Length(FAllWorkers) - 1].OS := AOS;
  FAllWorkers[Length(FAllWorkers) - 1].WorkerSpecificTask := '';
  FAllWorkers[Length(FAllWorkers) - 1].FileCacheInfo := TStringList.Create;
  FAllWorkers[Length(FAllWorkers) - 1].FileCacheInfo.LineBreak := #13#10;
  FAllWorkers[Length(FAllWorkers) - 1].FileCacheInfo.Text := AFileCacheInfoContent;
  FAllWorkers[Length(FAllWorkers) - 1].ExtraName := AExtraName;

  FAllWorkers[Length(FAllWorkers) - 1].FilesToSend := '';
  FAllWorkers[Length(FAllWorkers) - 1].BmpFilesToSend := '';
  FAllWorkers[Length(FAllWorkers) - 1].PmtvFilesToSend := '';
  FAllWorkers[Length(FAllWorkers) - 1].ArchiveStream := nil;   //created when required
  FAllWorkers[Length(FAllWorkers) - 1].Archive := nil;         //created when required

  FAllWorkers[Length(FAllWorkers) - 1].TxtCntW := 0;
  FAllWorkers[Length(FAllWorkers) - 1].BmpCntW := 0;
  FAllWorkers[Length(FAllWorkers) - 1].PmtvCntW := 0;

  FAllWorkers[Length(FAllWorkers) - 1].ResponseReceived := False;
  FAllWorkers[Length(FAllWorkers) - 1].FindSubControlFound := False;
  FAllWorkers[Length(FAllWorkers) - 1].Response := 'unset';

  FAllWorkers[Length(FAllWorkers) - 1].ResponseBitmapStream := TMemoryStream.Create; //created here, destroyed on returning to app

  FAllWorkers[Length(FAllWorkers) - 1].Fonts := '';
  FAllWorkers[Length(FAllWorkers) - 1].FindSubControlRequestID := 'NotSet';
end;


procedure TDistExec.SetWorkerResponseBitmapStream(AWorkerIndex: Integer; AArchiveAsResponseStr, AArchiveSizeStr: string);
var
  ArchiveStream: TMemoryStream;
  Archive: TMemArchive;
  TempArchiveHandlers: TArchiveHandlers;
begin
  if VerbLevel < 2 then
    AddToLog('Decoded response archive size: ' + IntToStr(Length(AArchiveAsResponseStr)));

  AArchiveAsResponseStr := Copy(AArchiveAsResponseStr, 1, StrToInt64Def(AArchiveSizeStr, Length(AArchiveAsResponseStr)));

  if VerbLevel < 2 then
    AddToLog('Received response archive size: ' + AArchiveSizeStr);

  AArchiveAsResponseStr := HexToString(AArchiveAsResponseStr);

  if VerbLevel < 2 then
    AddToLog('Received response raw archive size: ' + IntToStr(Length(AArchiveAsResponseStr)) + '   (* 2 = ' + IntToStr(Length(AArchiveAsResponseStr) shl 1) + ')');

  ArchiveStream := TMemoryStream.Create;
  Archive := TMemArchive.Create;
  TempArchiveHandlers := TArchiveHandlers.Create;
  try
    TempArchiveHandlers.OnAddToLog := DoOnAddToLog;
    ArchiveStream.Write(AArchiveAsResponseStr[1], Length(AArchiveAsResponseStr));

    if VerbLevel < 2 then
    begin
      //AddToLog('First 10 archive bytes: "' + FastReplace_0To1(Copy(AArchiveAsResponseStr, 1, 10)));
      //AddToLog('Last 10 archive bytes: "' + FastReplace_0To1(Copy(AArchiveAsResponseStr, Length(AArchiveAsResponseStr) - 9, 10)));
      AddToLog('Computing Archive hash.. Size = ' + IntToStr(ArchiveStream.Size));
      try
        AddToLog('Archive hash: ' + ClickerExtraUtils.ComputeHash(ArchiveStream.Memory, ArchiveStream.Size));
      except
        on E: Exception do
          AddToLog('Ex on computing archive hash: ' + E.Message);
      end;
    end;

    Archive.OnCompress := TempArchiveHandlers.HandleOnCompress;
    Archive.OnDecompress := TempArchiveHandlers.HandleOnDecompress;
    Archive.OnComputeArchiveHash := TempArchiveHandlers.HandleOnComputeArchiveHash;

    try
      Archive.OpenArchive(ArchiveStream, False);
      try
        Archive.ExtractToStream(CResultFileNameInArchive, FAllWorkers[AWorkerIndex].ResponseBitmapStream);
      finally
        Archive.CloseArchive;
      end;
    except
      on E: Exception do
        AddToLog('Ex on extracting archive: ' + E.Message);
    end;
  finally
    ArchiveStream.Free;
    Archive.Free;
    TempArchiveHandlers.Free;
  end;
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_PUBLISH(ClientInstance: DWord; var APublishFields: TMQTTPublishFields; var APublishProperties: TMQTTPublishProperties);
var
  QoS: Byte;
  ID: Word;
  Topic, s, Msg: string;
  i: Integer;
  Content: TStringList;
  WorkerIndex: Integer;
  TempResponseArchiveStr: string;
  WorkerName: string;
  WorkerRequestID: string;
begin
  QoS := (APublishFields.PublishCtrlFlags shr 1) and 3;
  Msg := StringReplace(DynArrayOfByteToString(APublishFields.ApplicationMessage), #0, '#0', [rfReplaceAll]);
  ID := APublishFields.PacketIdentifier;
  Topic := StringReplace(DynArrayOfByteToString(APublishFields.TopicName), #0, '#0', [rfReplaceAll]);

  if VerbLevel < 1 then
    AddToLog('Received PUBLISH  ServerPacketIdentifier: ' + IntToStr(ID) +
                                                   '  Msg: ' + Copy(Msg, 1, Pos(CProtocolParam_ResultImageArchive + '=', Msg) - 1) +
                                                   '  QoS: ' + IntToStr(QoS) +
                                                   '  TopicName: ' + Topic);

  if VerbLevel < 2 then
  begin
    s := '';
    for i := 0 to APublishProperties.SubscriptionIdentifier.Len - 1 do
      s := s + IntToStr(APublishProperties.SubscriptionIdentifier.Content^[i]) + ', ';
    AddToLog('SubscriptionIdentifier(s): ' + s);
  end;

  Content := TStringList.Create;
  try
    Content.LineBreak := #13#10;
    Content.Text := Msg;
    WorkerName := Content.Values[CProtocolParam_Name];
    WorkerRequestID := Content.Values[CProtocolParam_RequestID];

    if Topic = CTopicName_WorkerToApp_GetCapabilities then
    begin
      if FDistFSM.WaitingForWorkerCapabilities and (WorkerRequestID = GetCapabilitiesRequestID) then //This is True only for a short while (see the state machine below). Any worker which publishes after this, is ignored.
      begin
        AddToLog(DateTimeToStr(Now) + ' (' + IntToStr(GetTickCount64) + ')  Adding worker[' + IntToStr(Length(FAllWorkers)) + '] to list: ' + WorkerName);
        AddNewWorkerToList(WorkerName,
                           Content.Values[CProtocolParam_OS],
                           FastReplace_45ToReturn(Content.Values[CProtocolParam_FileCache]), //#4#5 separated list of files. Each item uses the CDefaultInMemFileNameHashSeparator string between name and hash.
                           Content.Values[CProtocolParam_ExtraName]
                          );
      end
      else
        AddToLog(DateTimeToStr(Now) + ' (' + IntToStr(GetTickCount64) + ')  Worker ' + WorkerName + ' responded too late. It won''t be added to list. WorkerRequestID: ' + WorkerRequestID);
    end;


    if Topic = CTopicName_WorkerToApp_SendBackground then
      if WorkerRequestID = SendBackgroundToAllRequestID then
      begin
        WorkerIndex := GetWorkerIndexByName(WorkerName);
        if WorkerIndex = -1 then
          AddToLog('Error: cannot find worker "' + WorkerName + '" on receiving SendBackground result. This might be a valid worker, which responded too late.')
        else
        begin
          if Content.Values['$ExecAction_Err$'] = CBackgroundOKResponse then
          begin
            FDistFSM.WorkerRespondedCountBG := FDistFSM.WorkerRespondedCountBG + 1;

            if VerbLevel < 2 then
              AddToLog('Worker[' + IntToStr(WorkerIndex) + '] responded with success when sending background image.');
          end
          else
            AddToLog('Worker[' + IntToStr(WorkerIndex) + '] responded with error when sending background image: ' + Content.Values['$ExecAction_Err$']);
        end;
      end;

    if Topic = CTopicName_WorkerToApp_FindSubControl then
    begin
      WorkerIndex := GetWorkerIndexByName(WorkerName);
      if WorkerIndex = -1 then
        AddToLog('Error: cannot find worker "' + WorkerName + '" on receiving FindSubControl result. This might be a valid worker, which responded too late.')
      else
      begin
        if WorkerRequestID = FAllWorkers[WorkerIndex].FindSubControlRequestID then
        begin
          FDistFSM.WorkerRespondedCountFS := FDistFSM.WorkerRespondedCountFS + 1;
          FAllWorkers[WorkerIndex].ResponseReceived := True;
          FAllWorkers[WorkerIndex].Response := Content.Values['$ExecAction_Err$'];  /////////////// there is also CActionPlugin_ExecutionResultErrorVar, but the response is set to $ExecAction_Err$
          FAllWorkers[WorkerIndex].FindSubControlFound := FAllWorkers[WorkerIndex].Response = '';

          FAllWorkers[WorkerIndex].ResponseVars.ControlLeft := Content.Values['$Control_Left$'];
          FAllWorkers[WorkerIndex].ResponseVars.ControlTop := Content.Values['$Control_Top$'];
          FAllWorkers[WorkerIndex].ResponseVars.ControlRight := Content.Values['$Control_Right$'];
          FAllWorkers[WorkerIndex].ResponseVars.ControlBottom := Content.Values['$Control_Bottom$'];
          FAllWorkers[WorkerIndex].ResponseVars.ControlWidth := Content.Values['$Control_Width$'];
          FAllWorkers[WorkerIndex].ResponseVars.ControlHeight := Content.Values['$Control_Height$'];
          FAllWorkers[WorkerIndex].ResponseVars.HalfControlWidth := Content.Values['$Half_Control_Width$'];
          FAllWorkers[WorkerIndex].ResponseVars.HalfControlHeight := Content.Values['$Half_Control_Height$'];
          FAllWorkers[WorkerIndex].ResponseVars.SubCnvXOffset := Content.Values['$DebugVar_SubCnvXOffset$'];
          FAllWorkers[WorkerIndex].ResponseVars.SubCnvYOffset := Content.Values['$DebugVar_SubCnvYOffset$'];

          if FGetAllControls then
          begin
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Handles := Content.Values['$AllControl_Handles$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_XOffsets := Content.Values['$AllControl_XOffsets$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_YOffsets := Content.Values['$AllControl_YOffsets$'];

            FAllWorkers[WorkerIndex].ResponseVars.AllControl_MatchSource := Content.Values['$AllControl_MatchSource$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_DetailedMatchSource := Content.Values['$AllControl_DetailedMatchSource$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_ResultedErrorCount := Content.Values['$AllControl_ResultedErrorCount$'];

            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Lefts := Content.Values['$AllControl_Lefts$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Tops := Content.Values['$AllControl_Lefts$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Rights := Content.Values['$AllControl_Rights$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Bottoms := Content.Values['$AllControl_Bottoms$'];

            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Widths := Content.Values['$AllControl_Widths$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllControl_Heights := Content.Values['$AllControl_Heights$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllHalfControl_Widths := Content.Values['$AllHalf_Control_Widths$'];
            FAllWorkers[WorkerIndex].ResponseVars.AllHalfControl_Heights := Content.Values['$AllHalf_Control_Heights$'];
          end;

          TempResponseArchiveStr := Copy(Msg, Pos(CProtocolParam_ResultImageArchive + '=', Msg) + Length(CProtocolParam_ResultImageArchive + '='), MaxInt);
          SetWorkerResponseBitmapStream(WorkerIndex, TempResponseArchiveStr, Content.Values[CProtocolParam_ResponseArchiveSize]);

          FDistFSM.AtLeastOneWorkerFoundTheSubControl := GetAllWorkers_AtLeastOneFound;   //this should be set after all responses
        end  //WorkerRequestID = FAllWorkers[WorkerIndex].FindSubControlRequestID
        else
          AddToLog('==================== Received FindSubControl response with RequestID = ' + FAllWorkers[WorkerIndex].FindSubControlRequestID);
      end; //WorkerIndex = -1
    end; //Topic

    if Topic = CTopicName_WorkerToApp_GetListOfFonts then
      if FDistFSM.WaitingForWorkerFonts then
      begin
        FDistFSM.WorkerRespondedCountLoF := FDistFSM.WorkerRespondedCountLoF + 1;

        if VerbLevel < 2 then
          AddToLog('Received fonts from worker: ' + WorkerName);

        WorkerIndex := GetWorkerIndexByName(WorkerName);
        if WorkerIndex = -1 then
          AddToLog('Error: cannot find worker "' + WorkerName + '" on receiving GetListOfFonts result. This might be a valid worker, which responded too late.')
        else
          FAllWorkers[WorkerIndex].Fonts := Content.Values[CProtocolParam_Fonts];
      end;

    //other fields which may pass metadata:
    //APublishProperties.UserProperty;
    //APublishProperties.ContentType;
    //APublishProperties.CorrelationData;
    //APublishProperties.ResponseTopic;     //this should match the CTopicName_AppToWorker_FindSubControl topic   (i.e. AppToWorker)
  finally
    Content.Free;
  end;

  if VerbLevel < 1 then
    AddToLog('');
end;


procedure TDistExec.HandleOnBeforeSending_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Acknowledging with PUBREC for ServerPacketID: ' + IntToStr(ATempPubRecFields.PacketIdentifier));
end;


procedure TDistExec.HandleOnAfterReceiving_MQTT_PUBREC(ClientInstance: DWord; var ATempPubRecFields: TMQTTPubRecFields; var ATempPubRecProperties: TMQTTPubRecProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Received PUBREC for PacketID: ' + IntToStr(ATempPubRecFields.PacketIdentifier));
end;


//Sending PUBREL after the PUBREC response from server, after the client has sent a PUBLISH packet with QoS=2.
procedure TDistExec.HandleOnBeforeSending_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Acknowledging with PUBREL for PacketID: ' + IntToStr(ATempPubRelFields.PacketIdentifier));
end;


procedure TDistExec.HandleOnAfterReceiving_MQTT_PUBREL(ClientInstance: DWord; var ATempPubRelFields: TMQTTPubRelFields; var ATempPubRelProperties: TMQTTPubRelProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Received PUBREL for ServerPacketID: ' + IntToStr(ATempPubRelFields.PacketIdentifier));
end;


procedure TDistExec.HandleOnBeforeSending_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Acknowledging with PUBCOMP for PacketID: ' + IntToStr(ATempPubCompFields.PacketIdentifier));
end;


procedure TDistExec.HandleOnAfterReceiving_MQTT_PUBCOMP(ClientInstance: DWord; var ATempPubCompFields: TMQTTPubCompFields; var ATempPubCompProperties: TMQTTPubCompProperties);
begin
  if VerbLevel < 1 then
    AddToLog('Received PUBCOMP for ServerPacketID: ' + IntToStr(ATempPubCompFields.PacketIdentifier));
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_PINGRESP(ClientInstance: DWord);
begin
  if VerbLevel < 1 then
    AddToLog('Received PINGRESP');
end;


procedure TDistExec.HandleOnBeforeSendingMQTT_DISCONNECT(ClientInstance: DWord;  //The lower word identifies the client instance
                                                         var ADisconnectFields: TMQTTDisconnectFields;
                                                         var ADisconnectProperties: TMQTTDisconnectProperties;
                                                         ACallbackID: Word);
begin
  if VerbLevel < 1 then
    AddToLog('Sending DISCONNECT');
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


procedure TDistExec.HandleOnAfterReceivingMQTT_DISCONNECT(ClientInstance: DWord;  //The lower word identifies the client instance
                                                          var ADisconnectFields: TMQTTDisconnectFields;
                                                          var ADisconnectProperties: TMQTTDisconnectProperties);
begin
  if VerbLevel < 1 then
  begin
    AddToLog('Received DISCONNECT');

    AddToLog('ADisconnectFields.EnabledProperties' + IntToStr(ADisconnectFields.EnabledProperties));
    AddToLog('ADisconnectFields.DisconnectReasonCode' + IntToStr(ADisconnectFields.DisconnectReasonCode));

    AddToLog('ADisconnectProperties.SessionExpiryInterval' + IntToStr(ADisconnectProperties.SessionExpiryInterval));
    AddToLog('ADisconnectProperties.ReasonString' + StringReplace(DynArrayOfByteToString(ADisconnectProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    AddToLog('ADisconnectProperties.ServerReference' + StringReplace(DynArrayOfByteToString(ADisconnectProperties.ServerReference), #0, '#0', [rfReplaceAll]));
    AddToLog('ADisconnectProperties.UserProperty' + StringReplace(DynOfDynArrayOfByteToString(ADisconnectProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  end;
end;


procedure TDistExec.HandleOnBeforeSendingMQTT_AUTH(ClientInstance: DWord;  //The lower word identifies the client instance
                                                   var AAuthFields: TMQTTAuthFields;
                                                   var AAuthProperties: TMQTTAuthProperties;
                                                   ACallbackID: Word);
begin
  if VerbLevel < 1 then
    AddToLog('Sending AUTH');

  AAuthFields.AuthReasonCode := $19; //Example: reauth   - see spec, pag 108.

  StringToDynArrayOfByte('SCRAM-SHA-1', AAuthProperties.AuthenticationMethod);       //some example from spec, pag 108
  StringToDynArrayOfByte('client-second-data', AAuthProperties.AuthenticationData);   //some modified example from spec, pag 108
end;


procedure TDistExec.HandleOnAfterReceivingMQTT_AUTH(ClientInstance: DWord;  //The lower word identifies the client instance
                                                    var AAuthFields: TMQTTAuthFields;
                                                    var AAuthProperties: TMQTTAuthProperties);
begin
  if VerbLevel < 1 then
  begin
    AddToLog('Received AUTH');

    AddToLog('AAuthFields.EnabledProperties' + IntToStr(AAuthFields.EnabledProperties));
    AddToLog('AAuthFields.AuthReasonCode' + IntToStr(AAuthFields.AuthReasonCode));

    AddToLog('AAuthProperties.ReasonString' + StringReplace(DynArrayOfByteToString(AAuthProperties.ReasonString), #0, '#0', [rfReplaceAll]));
    AddToLog('AAuthProperties.ServerReference' + StringReplace(DynArrayOfByteToString(AAuthProperties.AuthenticationMethod), #0, '#0', [rfReplaceAll]));
    AddToLog('AAuthProperties.ServerReference' + StringReplace(DynArrayOfByteToString(AAuthProperties.AuthenticationData), #0, '#0', [rfReplaceAll]));
    AddToLog('AAuthProperties.UserProperty' + StringReplace(DynOfDynArrayOfByteToString(AAuthProperties.UserProperty), #0, '#0', [rfReplaceAll]));
  end;
end;


end.

