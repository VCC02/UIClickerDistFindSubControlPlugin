{
    Copyright (C) 2025 VCC
    creation date: 21 May 2025
    initial release date: 24 May 2025

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


unit WorkerPoolManagerMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, VirtualTrees,
  IdSchedulerOfThreadPool, IdHTTPServer, IdCustomHTTPServer, IdContext, IdSync, IdCustomTCPServer,
  WorkerPoolCommonConsts;

type
  TMachineType = (mtBroker, mtWorker, mtBrokerAndWorker); //Not sure if it's a good idea of running both broker and worker on the same machine, but from the network's perspective, it should be better.
  TAppType = (atBroker, atWorker, atUIClicker);
  TAppRunning = (arJustStarted, arConnecting, arFullyRunning, arUnknown);
                 //arJustStarted - the Pool manager just sent the running command.
                 //arConnecting - might be used for workers, between arJustStarted and fully connected to UIClicker.
                 //arFullyRunning - the app is running and it is fully connected.
                 //arUnknown - the app is either not running or not connected (bad state).


  TMachineOS = (mosWin, mosLin, mosUnknown);

  TFSM = (SInit, SMonitorStatus, SStartRemoteApps, SWaitForRemoteApps);

  TRunningApp = record
    //Path: string;       //paths will be different between Win and Lin
    Address: string; //Broker address (usually local address, but can be another address, if the worker is connected to a different machine).
    Port: string; //Brokers (server mode) and workers (client mode) have to use the same port. UIClicker will have different ports (server mode).
    StartedAt: QWord; //Timestamp used for waiting for the app to get into a responding/connected state.
    State: TAppRunning;
    StartCmdResponse: string;
    StartedCount: Integer;
  end;
  TRunningAppArr = array of TRunningApp;

  TMachineRec = record
    Address: string;
    Port: string;
    MachineType: TMachineType;
    MachineOS: TMachineOS;
    PoolUserName: string; //This is allocated at the same time the machine is allocated. If multiple users are allowed / machine, then that's a different story.
    PoolPassWord: string;
    ListOfLinWorkersAllocated: string;  //Used when this is a broker machine and another machine with Lin workers point to this one.

    BrokerUserName: string; //workers may use different user and password     //These credentials will have to be moved to the TRunningApp structure.
    BrokerPassword: string;
    PoolID: string; //Reserved for now. May be implemented later, if required.

    State: TFSM;
    NextState: TFSM;
    TargetBrokerCountPerWinMachine: Integer;  //how many brokers should be running on this machine
    TargetBrokerCountPerLinMachine: Integer;  //how many brokers should be running on this machine
    TargetWorkerCountPerWinMachine: Integer;  //how many workers should be running on this machine
    TargetWorkerCountPerLinMachine: Integer;  //how many workers should be running on this machine

    //For example, on a machine, there should be one broker, four workers and four UIClickers.
    BrokersToBeRunning: TRunningAppArr; //This is the target brokers to be running.
    WorkersToBeRunning: TRunningAppArr; //This is the target workers to be running.
    UIClickersToBeRunning: TRunningAppArr; //This is the target UIClickers to be running.
  end;

  PMachineRec = ^TMachineRec;

  TSyncObj = class(TIdSync)
  private
    FCmd: string;
    FParams: TStrings;
    FPeerIP: string;
    FResult: string;
  protected
    procedure DoSynchronize; override;
  end;


  { TfrmWorkerPoolManagerMain }

  TfrmWorkerPoolManagerMain = class(TForm)
    btnAddMachine: TButton;
    btnStartTwoBrokers: TButton;
    btnSendPoolCredentialsToLocal: TButton;
    grpSettings: TGroupBox;
    IdHTTPServerPlugins: TIdHTTPServer;
    IdHTTPServerResources: TIdHTTPServer;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    IdSchedulerOfThreadPool2: TIdSchedulerOfThreadPool;
    lblMinBrokerPort: TLabel;
    lblBrokerCountPerMachine: TLabel;
    memInfo: TMemo;
    memLog: TMemo;
    spnedtBrokerCountPerMachine: TSpinEdit;
    spnedtMinBrokerPort: TSpinEdit;
    tmrFSM: TTimer;
    tmrStartup: TTimer;
    vstMachines: TVirtualStringTree;
    procedure btnAddMachineClick(Sender: TObject);
    procedure btnSendPoolCredentialsToLocalClick(Sender: TObject);
    procedure btnStartTwoBrokersClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServerPluginsCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServerPluginsConnect(AContext: TIdContext);
    procedure IdHTTPServerPluginsException(AContext: TIdContext; AException: Exception
      );
    procedure IdHTTPServerResourcesCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure IdHTTPServerResourcesConnect(AContext: TIdContext);
    procedure IdHTTPServerResourcesException(AContext: TIdContext; AException: Exception
      );
    procedure tmrFSMTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstMachinesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    function GetCredentialsFile(ANodeData: PMachineRec): string;
    function ProcessCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;

    function StartMQTTBrokerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, ABrokerPort, ABrokerUsername, ABrokerPassword: string): string; //returns exec result
    function StartUIClickerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort: string; AUIClickerPort: Word): string; //returns exec result
    function StartWorkerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AWorkerExtraCaption, ABrokerAddress: string; ABrokerPort, AUIClickerPort: Word): string; //returns exec result

    function GetMQTTAppsOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): string; //returns exec result
    function GetAppsWhichHaveToBeStartedCount(var AMachineRec: TMachineRec): Integer;
    procedure StartApps(var AMachineRec: TMachineRec);
    function FindWorkerStatusConnected(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): Boolean;
    function GetAMachineWithBrokerWhichRequiresLinWorkers(AExcludeMachineAddress: string; AGetAnyValidMachine: Boolean): PMachineRec;

    procedure InitBrokersToBeRunning(AMachineNodeData: PMachineRec);
    procedure InitWorkersToBeRunning(AMachineNodeData: PMachineRec);
    procedure InitUIClickersToBeRunning(AMachineNodeData: PMachineRec);

    procedure RunFSM(var AMachineRec: TMachineRec);
  public
    function GetBrokerInfoForClient(APoolClientUserName: string; AIncludeCredentials: Boolean): string;
    function GetMachineByIP(AMachineIP: string): PVirtualNode;
    function SetMachineOnline(APeerIP, AMachineOS: string): string;

    procedure AddToLog(s: string);
  end;


const
  CMachineTypeStr: array[TMachineType] of string = ('Broker', 'Worker', 'BrokerAndWorker');
  CMachineOSStr: array[TMachineOS] of string = (CWinParam, CLinParam, '???');
  CFSMStr: array[TFSM] of string = ('Init', 'MonitorStatus', 'StartRemoteApps', 'WaitForRemoteApps');

  CUIClickerPortOffset = 20000; //This value is added to a worker port, to get the port UIClicker is listening on.
                                //For example, if a worker listens on 12345, its UIClicker listens on 32345.

var
  frmWorkerPoolManagerMain: TfrmWorkerPoolManagerMain;

implementation

{$R *.frm}


uses
  ClickerUtils, ClickerActionsClient, ClickerActionProperties;


procedure TSyncObj.DoSynchronize;
begin
  FResult := 'OK=OK'; //a default response

  //Commands for Pool server
  if Pos('/' + CGetConfigCmd, FCmd) = 1 then
  begin
    FResult := '=Nothing' + #13#10 +
               frmWorkerPoolManagerMain.GetBrokerInfoForClient(FParams.Values[CPoolClientUserNameParam], FParams.Values[CIncludeCredentialsParam] = '1') + #13#10 +
              'Remaining=';
    Exit;
  end;

  //Commands for broker and worker machines:
  if Pos('/' + CMachineOnline, FCmd) = 1 then
  begin
    FResult := frmWorkerPoolManagerMain.SetMachineOnline(FPeerIP, FParams.Values[CMachineOSParam]);
    //FResult := '=Nothing' + #13#10 +
    //           frmWorkerPoolManagerMain.SetMachineOnline(FPeerIP, FParams.Values[CMachineOSParam]) + #13#10 +
    //          'Remaining=';
    Exit;
  end;
end;


{ TfrmWorkerPoolManagerMain }


procedure TfrmWorkerPoolManagerMain.FormCreate(Sender: TObject);
begin
  vstMachines.NodeDataSize := SizeOf(TMachineRec);
  tmrStartup.Enabled := True;
end;


procedure TfrmWorkerPoolManagerMain.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + s);
end;


function TfrmWorkerPoolManagerMain.GetCredentialsFile(ANodeData: PMachineRec): string;
begin
  Result := '[Credentials]' + #4#5 +
            'Username=' + ANodeData^.BrokerUserName + #4#5 +
            'Password=' + ANodeData^.BrokerPassword + #4#5 +
            'PoolID=' + ANodeData^.PoolID + #4#5;
end;


function TfrmWorkerPoolManagerMain.GetBrokerInfoForClient(APoolClientUserName: string; AIncludeCredentials: Boolean): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    Result := CBrokerAddressKeyName + '=' + CErrPrefix + CUserNotFound + #13#10 +
              CBrokerPortKeyName + '=' + '0' + #13#10;
    Exit;
  end;

  repeat
    NodeData := vstMachines.GetNodeData(Node);
    if Assigned(NodeData) then
      if NodeData^.PoolUserName = APoolClientUserName then
      begin
        Result := CBrokerAddressKeyName + '=' + NodeData^.Address + #13#10 +
                  CBrokerPortKeyName + '=' + NodeData^.Port + #13#10;

        if AIncludeCredentials then
          Result := Result + GetCredentialsFile(NodeData);
      end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


function TfrmWorkerPoolManagerMain.GetMachineByIP(AMachineIP: string): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    Result := nil;
    Exit;
  end;

  repeat
    NodeData := vstMachines.GetNodeData(Node);
    if Assigned(NodeData) then
      if NodeData^.Address = AMachineIP then
      begin
        Result := Node;
        Break;
      end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


function TfrmWorkerPoolManagerMain.GetAMachineWithBrokerWhichRequiresLinWorkers(AExcludeMachineAddress: string; AGetAnyValidMachine: Boolean): PMachineRec;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Result := nil;
  Node := vstMachines.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstMachines.GetNodeData(Node);
    if NodeData <> nil then
      if NodeData^.Address <> AExcludeMachineAddress then
        if NodeData^.MachineType in [mtBroker, mtBrokerAndWorker] then
          if (NodeData^.ListOfLinWorkersAllocated = '') or AGetAnyValidMachine then //Available. Maybe a complex logic can be added here, to allow multiple Lin machines to connect to this one.
          begin
            Result := NodeData;
            Break;
          end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


procedure TfrmWorkerPoolManagerMain.InitBrokersToBeRunning(AMachineNodeData: PMachineRec);
var
  i: Integer;
begin
  for i := 0 to Length(AMachineNodeData^.BrokersToBeRunning) - 1 do     //broker(s)
  begin
    AMachineNodeData^.BrokersToBeRunning[i].Port := IntToStr(spnedtMinBrokerPort.Value {+ 1183} + i); //base port + app index
    AMachineNodeData^.BrokersToBeRunning[i].Address := '127.0.0.1';
    AMachineNodeData^.BrokersToBeRunning[i].State := arUnknown;
    AMachineNodeData^.BrokersToBeRunning[i].StartedAt := 0;
    AMachineNodeData^.BrokersToBeRunning[i].StartedCount := 0;
  end;
end;


procedure TfrmWorkerPoolManagerMain.InitWorkersToBeRunning(AMachineNodeData: PMachineRec);
var
  i, WorkerCountPerBroker: Integer;
  BrokerToConnectToNodeData: PMachineRec;
begin
  BrokerToConnectToNodeData := nil;
  if AMachineNodeData^.MachineOS = mosLin then
  begin
    BrokerToConnectToNodeData := GetAMachineWithBrokerWhichRequiresLinWorkers(AMachineNodeData^.Address, False);  //get an available machine (no workers connected to it)
    if BrokerToConnectToNodeData = nil then
      BrokerToConnectToNodeData := GetAMachineWithBrokerWhichRequiresLinWorkers(AMachineNodeData^.Address, True); //now get any valid broker, just get one!
  end;

  if spnedtBrokerCountPerMachine.Value > 0 then
    WorkerCountPerBroker := Length(AMachineNodeData^.WorkersToBeRunning) div spnedtBrokerCountPerMachine.Value
  else
    WorkerCountPerBroker := Length(AMachineNodeData^.WorkersToBeRunning);

  for i := 0 to Length(AMachineNodeData^.WorkersToBeRunning) - 1 do     //workers
  begin
    if AMachineNodeData^.MachineOS = mosWin then
    begin
      AMachineNodeData^.WorkersToBeRunning[i].Address := '127.0.0.1'; //just connect to the same machine

      if spnedtBrokerCountPerMachine.Value > 0 then
        AMachineNodeData^.WorkersToBeRunning[i].Port := AMachineNodeData^.BrokersToBeRunning[i div WorkerCountPerBroker].Port //0000, 1111 etc
      else
        AMachineNodeData^.WorkersToBeRunning[i].Port := '0'; //not sure if these workers should be left unconnected, instead of being connected to a default, which might not be desired
    end
    else
    begin  //mosLin - find an available machine with broker
      if BrokerToConnectToNodeData <> nil then
      begin
        AMachineNodeData^.WorkersToBeRunning[i].Address := BrokerToConnectToNodeData^.Address; //can be different, if this is a machine with Lin workers, connected to a Win broker
        AMachineNodeData^.WorkersToBeRunning[i].Port := BrokerToConnectToNodeData^.Port;
        BrokerToConnectToNodeData^.ListOfLinWorkersAllocated := BrokerToConnectToNodeData^.ListOfLinWorkersAllocated + AMachineNodeData^.WorkersToBeRunning[i].Address + #13#10;
      end
      else   //No broker available for this machine. Ideally, these Lin machines, if they start before any broker, then they should be connected later.
      begin
        AMachineNodeData^.WorkersToBeRunning[i].Address := ''; //this will have to be set later
        AMachineNodeData^.WorkersToBeRunning[i].Port := '';
      end;
    end;  //mosLin

    AMachineNodeData^.WorkersToBeRunning[i].State := arUnknown;
    AMachineNodeData^.WorkersToBeRunning[i].StartedAt := 0;
    AMachineNodeData^.WorkersToBeRunning[i].StartedCount := 0;
  end;
end;


procedure TfrmWorkerPoolManagerMain.InitUIClickersToBeRunning(AMachineNodeData: PMachineRec);
var
  i: Integer;
begin
  for i := 0 to Length(AMachineNodeData^.UIClickersToBeRunning) - 1 do     //UIClickers
  begin
    AMachineNodeData^.UIClickersToBeRunning[i].Port := IntToStr(StrToIntDef(AMachineNodeData^.WorkersToBeRunning[i].Port, 0) + CUIClickerPortOffset + i);
    AMachineNodeData^.UIClickersToBeRunning[i].Address := '127.0.0.1';
    AMachineNodeData^.UIClickersToBeRunning[i].State := arUnknown;
    AMachineNodeData^.UIClickersToBeRunning[i].StartedAt := 0;
    AMachineNodeData^.UIClickersToBeRunning[i].StartedCount := 0;
  end;
end;


function TfrmWorkerPoolManagerMain.SetMachineOnline(APeerIP, AMachineOS: string): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  ToBeAdded: Boolean;
begin
  Node := GetMachineByIP(APeerIP);
  ToBeAdded := Node = nil;

  if ToBeAdded then
    Node := vstMachines.AddChild(vstMachines.RootNode);

  NodeData := vstMachines.GetNodeData(Node);
  NodeData^.Address := APeerIP;

  if ToBeAdded then
    if NodeData^.MachineType in [mtBroker, mtBrokerAndWorker] then
    begin
      Randomize;
      Sleep(33);
      NodeData^.PoolUserName := 'First';
      NodeData^.PoolPassWord := 'RandomlyGeneratedPassword_' + IntToStr(GetTickCount64) + DateTimeToStr(Now) + IntToStr(Random(MaxInt));

      Randomize;
      Sleep(33);
      NodeData^.BrokerUserName := 'User_' + DateTimeToStr(Now) + IntToStr(Random(MaxInt));
      NodeData^.BrokerPassword := 'Unknown';
      NodeData^.PoolID := DateTimeToStr(Now);
    end;

  if AMachineOS = CWinParam then
  begin
    NodeData^.MachineOS := mosWin;
    if ToBeAdded then
    begin
      NodeData^.TargetBrokerCountPerWinMachine := spnedtBrokerCountPerMachine.Value;
      NodeData^.TargetWorkerCountPerWinMachine := spnedtBrokerCountPerMachine.Value * 4;

      SetLength(NodeData^.BrokersToBeRunning, NodeData^.TargetBrokerCountPerWinMachine);
      SetLength(NodeData^.WorkersToBeRunning, NodeData^.TargetWorkerCountPerWinMachine);
      SetLength(NodeData^.UIClickersToBeRunning, NodeData^.TargetWorkerCountPerWinMachine);
    end;
  end
  else
    if AMachineOS = CLinParam then
    begin
      NodeData^.MachineOS := mosLin;
      if ToBeAdded then
      begin
        NodeData^.TargetBrokerCountPerLinMachine := 0;
        NodeData^.TargetWorkerCountPerLinMachine := spnedtBrokerCountPerMachine.Value * 2;

        SetLength(NodeData^.BrokersToBeRunning, NodeData^.TargetBrokerCountPerLinMachine);
        SetLength(NodeData^.WorkersToBeRunning, NodeData^.TargetWorkerCountPerLinMachine);
        SetLength(NodeData^.UIClickersToBeRunning, NodeData^.TargetWorkerCountPerLinMachine);
      end;
    end
    else
    begin
      NodeData^.MachineOS := mosUnknown;
      //TargetBrokerCountPerWinMachine is not set here, in case the machine already exists and it is updated with bad info
    end;

  if ToBeAdded then
  begin
    NodeData^.State := SInit;
    NodeData^.NextState := SInit;

    InitBrokersToBeRunning(NodeData);
    InitWorkersToBeRunning(NodeData);
    InitUIClickersToBeRunning(NodeData);
  end; //ToBeAdded

  vstMachines.InvalidateNode(Node);
  Result := CMachineSet;
end;


function TfrmWorkerPoolManagerMain.ProcessCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FCmd := ACmd;
    SyncObj.FParams := AParams;
    SyncObj.FPeerIP := APeerIP;
    SyncObj.Synchronize;
    Result := SyncObj.FResult;
  finally
    SyncObj.Free;
  end;
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerPluginsCommandGet(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ARequestInfo.Params.LineBreak := #13#10;
  AResponseInfo.ContentType := 'text/plain';
  AResponseInfo.CharSet := 'US-ASCII';

  AResponseInfo.ResponseText := ProcessCommand(ARequestInfo.Document, ARequestInfo.Params, AContext.Binding.PeerIP);
  Sleep(10);
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerResourcesCommandGet(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ARequestInfo.Params.LineBreak := #13#10;
  AResponseInfo.ContentType := 'text/plain';
  AResponseInfo.CharSet := 'US-ASCII';

  AResponseInfo.ResponseText := ProcessCommand(ARequestInfo.Document, ARequestInfo.Params, AContext.Binding.PeerIP);
  Sleep(10);
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerPluginsConnect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 60000;   //if no bytes are received in 1min, then close the connection
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerResourcesConnect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 60000;   //if no bytes are received in 1min, then close the connection
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerPluginsException(
  AContext: TIdContext; AException: Exception);
begin
  //AddToLogFromThread(AException.Message);
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerResourcesException(
  AContext: TIdContext; AException: Exception);
begin
  //AddToLogFromThread(AException.Message);
end;


function TfrmWorkerPoolManagerMain.GetAppsWhichHaveToBeStartedCount(var AMachineRec: TMachineRec): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(AMachineRec.BrokersToBeRunning) - 1 do
    if AMachineRec.BrokersToBeRunning[i].State = arUnknown then
      if AMachineRec.BrokersToBeRunning[i].StartedCount < 3 then
        Inc(Result);

  for i := 0 to Length(AMachineRec.WorkersToBeRunning) - 1 do
    if AMachineRec.WorkersToBeRunning[i].State = arUnknown then
      if AMachineRec.WorkersToBeRunning[i].StartedCount < 3 then
        Inc(Result);

  for i := 0 to Length(AMachineRec.UIClickersToBeRunning) - 1 do
    if AMachineRec.UIClickersToBeRunning[i].State = arUnknown then
      if AMachineRec.UIClickersToBeRunning[i].StartedCount < 3 then
        Inc(Result);
end;


procedure TfrmWorkerPoolManagerMain.StartApps(var AMachineRec: TMachineRec);
var
  i: Integer;
begin
  for i := 0 to Length(AMachineRec.BrokersToBeRunning) - 1 do
    if (AMachineRec.BrokersToBeRunning[i].StartedCount < 3) and (AMachineRec.BrokersToBeRunning[i].State = arUnknown) then
    begin
      Inc(AMachineRec.BrokersToBeRunning[i].StartedCount);
      AMachineRec.BrokersToBeRunning[i].StartedAt := GetTickCount64;
      AMachineRec.BrokersToBeRunning[i].StartCmdResponse := StartMQTTBrokerOnRemoteMachine(AMachineRec.Address, '5444', AMachineRec.BrokersToBeRunning[i].Port, AMachineRec.BrokerUserName, AMachineRec.BrokerPassword);
      AMachineRec.BrokersToBeRunning[i].State := arJustStarted;

      if Pos(CREResp_RemoteExecResponseVar + '=1', AMachineRec.BrokersToBeRunning[i].StartCmdResponse) = 1 then
        AddToLog('Successfully started broker[' + IntToStr(i) + '] at ' + AMachineRec.BrokersToBeRunning[i].Address + ':' + AMachineRec.BrokersToBeRunning[i].Port + '.  StartedCount = ' + IntToStr(AMachineRec.BrokersToBeRunning[i].StartedCount))
      else
        AddToLog('Error starting broker at ' + AMachineRec.BrokersToBeRunning[i].Address + ':' + AMachineRec.BrokersToBeRunning[i].Port + '. ' + AMachineRec.BrokersToBeRunning[i].StartCmdResponse);
    end;

  for i := 0 to Length(AMachineRec.WorkersToBeRunning) - 1 do
    if (AMachineRec.WorkersToBeRunning[i].StartedCount < 3) and (AMachineRec.WorkersToBeRunning[i].State = arUnknown) then
    begin
      if AMachineRec.WorkersToBeRunning[i].Address <> '' then
      begin
        Inc(AMachineRec.WorkersToBeRunning[i].StartedCount);
        AMachineRec.WorkersToBeRunning[i].StartedAt := GetTickCount64;
        AMachineRec.WorkersToBeRunning[i].StartCmdResponse := StartWorkerOnRemoteMachine(AMachineRec.Address, '5444', IntToStr(i), AMachineRec.WorkersToBeRunning[i].Address, StrToIntDef(AMachineRec.WorkersToBeRunning[i].Port, 1183), StrToIntDef(AMachineRec.UIClickersToBeRunning[i].Port, 0));
        AMachineRec.WorkersToBeRunning[i].State := arJustStarted;

        if Pos(CREResp_RemoteExecResponseVar + '=1', AMachineRec.WorkersToBeRunning[i].StartCmdResponse) = 1 then
          AddToLog('Successfully started worker[' + IntToStr(i) + '] at ' + AMachineRec.WorkersToBeRunning[i].Address + ':' + AMachineRec.WorkersToBeRunning[i].Port + '.  StartedCount = ' + IntToStr(AMachineRec.WorkersToBeRunning[i].StartedCount))
        else
          AddToLog('Error starting worker at ' + AMachineRec.WorkersToBeRunning[i].Address + ':' + AMachineRec.WorkersToBeRunning[i].Port + '. ' + AMachineRec.WorkersToBeRunning[i].StartCmdResponse);
      end
      else
      begin
        //this is a Lin machine with workers, which should wait for another machine with broker(s)
        InitWorkersToBeRunning(@AMachineRec); //this should be enough to be called once (outside the for loop)
        AddToLog('Workers from ' + AMachineRec.Address + ' do not have a broker to connect to.');
      end;
    end;

  for i := 0 to Length(AMachineRec.UIClickersToBeRunning) - 1 do
    if (AMachineRec.UIClickersToBeRunning[i].StartedCount < 3) and (AMachineRec.UIClickersToBeRunning[i].State = arUnknown) then
    begin
      Inc(AMachineRec.UIClickersToBeRunning[i].StartedCount);
      AMachineRec.UIClickersToBeRunning[i].StartedAt := GetTickCount64;
      AMachineRec.UIClickersToBeRunning[i].StartCmdResponse := StartUIClickerOnRemoteMachine(AMachineRec.Address, '5444', StrToIntDef(AMachineRec.UIClickersToBeRunning[i].Port, 1183)); //UIClicker will listen on BrokerPort+20000
      AMachineRec.UIClickersToBeRunning[i].State := arJustStarted;

      if Pos(CREResp_RemoteExecResponseVar + '=1', AMachineRec.UIClickersToBeRunning[i].StartCmdResponse) = 1 then
        AddToLog('Successfully started UIClicker[' + IntToStr(i) + '] at ' + AMachineRec.UIClickersToBeRunning[i].Address + ':' + AMachineRec.UIClickersToBeRunning[i].Port + '.  StartedCount = ' + IntToStr(AMachineRec.UIClickersToBeRunning[i].StartedCount))
      else
        AddToLog('Error starting UIClicker at ' + AMachineRec.UIClickersToBeRunning[i].Address + ':' + AMachineRec.UIClickersToBeRunning[i].Port + '. ' + AMachineRec.UIClickersToBeRunning[i].StartCmdResponse);
    end;
end;


procedure TfrmWorkerPoolManagerMain.RunFSM(var AMachineRec: TMachineRec);
begin
  case AMachineRec.State of
    SInit:
    begin

    end;

    SMonitorStatus:
    begin

    end;

    SStartRemoteApps:
    begin
      StartApps(AMachineRec);
    end;

    SWaitForRemoteApps:
    begin
      //get "connected" status of workers etc.
    end;
  end;

  case AMachineRec.State of
    SInit:
      AMachineRec.NextState := SMonitorStatus;

    SMonitorStatus:
      if GetAppsWhichHaveToBeStartedCount(AMachineRec) > 0 then
        AMachineRec.NextState := SStartRemoteApps
      else
        AMachineRec.NextState := SMonitorStatus; //another state is required here, to wait (e.g. 10s) before the next call to GetListOfAppsWhichHaveToBeStarted

    SStartRemoteApps:
      AMachineRec.NextState := SWaitForRemoteApps;

    SWaitForRemoteApps:
      AMachineRec.NextState := SMonitorStatus;
  end;

  AMachineRec.State := AMachineRec.NextState;
end;


procedure TfrmWorkerPoolManagerMain.tmrFSMTimer(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstMachines.GetNodeData(Node);
    RunFSM(NodeData^);

    Node := Node^.NextSibling;
  until Node = nil;

  vstMachines.Repaint;
end;


procedure TfrmWorkerPoolManagerMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  try
    IdHTTPServerPlugins.Active := True;
    AddToLog('WorkerPoolManager is listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort));
  except
    on E: Exception do
      AddToLog('Can''t listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort) + '  ' + E.Message);
  end;

  try
    IdHTTPServerResources.Active := True;
    AddToLog('WorkerPoolManager is listening on port ' + IntToStr(IdHTTPServerResources.DefaultPort));
  except
    on E: Exception do
      AddToLog('Can''t listening on port ' + IntToStr(IdHTTPServerResources.DefaultPort) + '  ' + E.Message);
  end;
end;


procedure TfrmWorkerPoolManagerMain.vstMachinesGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: PMachineRec;
begin
  try
    NodeData := vstMachines.GetNodeData(Node);

    case Column of
      0: CellText := IntToStr(Node^.Index);
      1: CellText := NodeData^.Address;
      2: CellText := CMachineTypeStr[NodeData^.MachineType];
      3: CellText := CMachineOSStr[NodeData^.MachineOS];
      4: CellText := CFSMStr[NodeData^.State];
    end;
  except
    CellText := '?';
  end;
end;


procedure TfrmWorkerPoolManagerMain.btnAddMachineClick(Sender: TObject);
//var
  //Node: PVirtualNode;
  //NodeData: PMachineRec;
begin
  //manual adding (as an example)
  //Node := vstMachines.AddChild(vstMachines.RootNode);
  //NodeData := vstMachines.GetNodeData(Node);
  //NodeData^.Address := '127.0.0.1';
  //NodeData^.Port := '1883';
  //NodeData^.MachineType := mtBrokerAndWorker;
  //
  //Randomize;
  //Sleep(33);
  //NodeData^.PoolUserName := 'First';
  //NodeData^.PoolPassWord := 'RandomlyGeneratedPassword_' + IntToStr(GetTickCount64) + DateTimeToStr(Now) + IntToStr(Random(MaxInt));
  //
  //Randomize;
  //Sleep(33);
  //NodeData^.BrokerUserName := 'User_' + DateTimeToStr(Now) + IntToStr(Random(MaxInt));
  //NodeData^.BrokerPassword := 'Unknown';
  //NodeData^.PoolID := DateTimeToStr(Now);

  //NodeData^.State := SInit;
  //NodeData^.NextState := SInit;
  //NodeData^.TargetBrokerCountPerWinMachine := 0;
  //NodeData^.TargetBrokerCountPerLinMachine := 0;
  //NodeData^.TargetWorkerCountPerWinMachine := 0;
  //NodeData^.TargetWorkerCountPerLinMachine := 0;
  SetMachineOnline('127.0.0.1', CWinParam);
end;


function SendPoolCredentials(AMachineAdress, ACmdUIClickerPort, APoolUserName, APoolPassWord: string): string;  //This sends the pool credentials to the UIClicker which runs the plugin. This code doesn't have to be run from here.
var
  Link: string;
  Content: string; //pool credentials file
  MemStream: TMemoryStream;
begin                                                              //CRECmd_SetMemPluginFile saves the file in the plugin's InMemFS.   CRECmd_SetRenderedFile and CRECmd_SendFileToServer can't be used here.
  Link := 'http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/' + CRECmd_SetMemPluginFile + '?' + CREParam_FileName + '=' + CPoolCredentialsFileName;

  Content := '';
  Content := Content + '[PoolCredentials]' + #13#10;
  Content := Content + 'PoolUserName=' + APoolUserName + #13#10;
  Content := Content + 'PoolPassWord=' + APoolPassWord + #13#10;

  MemStream := TMemoryStream.Create;
  try
    MemStream.Position := 0;
    MemStream.Write(Content[1], Length(Content));
    Result := SendFileToServer(Link, MemStream, False);
  finally
    MemStream.Free;
  end;
end;


procedure TfrmWorkerPoolManagerMain.btnSendPoolCredentialsToLocalClick(
  Sender: TObject);
var
  Res: string;
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    AddToLog('Please add a machine first, in order to send proper content.');

    Res := SendPoolCredentials('127.0.0.1', '5444', 'Dummy_UserName', 'Dummy_Password');
    AddToLog('Sending dummy pool credentials result: ' + Res);

    Exit;
  end;

  NodeData := vstMachines.GetNodeData(Node);

  Res := SendPoolCredentials('127.0.0.1', '5444', NodeData^.PoolUserName, NodeData^.PoolPassWord);
  AddToLog('Sending pool credentials result: ' + Res);
end;


procedure TfrmWorkerPoolManagerMain.btnStartTwoBrokersClick(Sender: TObject);
begin
  memLog.Lines.Add(StartMQTTBrokerOnRemoteMachine('127.0.0.1', '5444', '21883', 'Username', 'Password'));
  memLog.Lines.Add(StartMQTTBrokerOnRemoteMachine('127.0.0.1', '5444', '21884', 'Username', 'Password'));
  memLog.Lines.Add(GetMQTTAppsOnRemoteMachine('127.0.0.1', '5444', CWinParam));
end;


function TfrmWorkerPoolManagerMain.StartMQTTBrokerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, ABrokerPort, ABrokerUsername, ABrokerPassword: string): string; //returns exec result
var
  SetVarOptions: TClkSetVarOptions;
  ExecAppOptions: TClkExecAppOptions;
  PluginOptions: TClkPluginOptions;
begin
  Result := '';

  //set some vars for plugin
  SetVarOptions.ListOfVarNames := '$BrokerPortNumber$' + #13#10 + '$PasswordFile$' + #13#10 + '$ConfFile$' + #13#10 + '$BrokerUsername$' + #13#10 + '$BrokerPassword$' + #13#10;   //these will not be evaluated by SetVar
  SetVarOptions.ListOfVarValues := ABrokerPort +         #13#10 + '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\pp_' + ABrokerPort + '.txt' + #13#10 + '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\mosquitto' + ABrokerPort + '.conf' + #13#10 + ABrokerUsername + #13#10 + ABrokerPassword + #13#10;
  SetVarOptions.ListOfVarEvalBefore := '0' +             #13#10 + '1'              + #13#10 + '1'              + #13#10 + '0' +             #13#10 + '0' +             #13#10;
  SetVarOptions.FailOnException := False;
  try
    Result := ExecuteSetVarAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', SetVarOptions);
  except
    on E: Exception do
      AddToLog('Ex on setting broker params before starting broker: ' + E.Message);
  end;

  //update pp_' + ABrokerPort + '.txt'
  PluginOptions.FileName := '$AppDir$\..\UIClickerDistFindSubControlPlugin\BrokerParams\lib\$AppBitness$-$OSBitness$\BrokerParams.dll';
  PluginOptions.ListOfPropertiesAndValues := ''; //to be set
  PluginOptions.ListOfPropertiesAndTypes := '';
  try
    Result := ExecutePluginAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', PluginOptions);
  except
    on E: Exception do
      AddToLog('Ex on running plugin for setting broker params before starting broker: ' + E.Message);
  end;

  ExecAppOptions.PathToApp := 'C:\Program Files\mosquitto\mosquitto.exe';
  ExecAppOptions.ListOfParams := '-c' + #4#5 +
                                 '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\mosquitto' + ABrokerPort + '.conf';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  try
    Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;
end;


function TfrmWorkerPoolManagerMain.StartUIClickerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort: string; AUIClickerPort: Word): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin
  Result := '';
  ExecAppOptions.PathToApp := '$AppDir$\UIClicker.exe';
  ExecAppOptions.ListOfParams := '--ExtraCaption' + #4#5 +
                                 IntToStr(AUIClickerPort) + #4#5 +
                                 '--ServerPort' + #4#5 +
                                 IntToStr(AUIClickerPort) + #4#5 +
                                 '--SetExecMode' + #4#5 +
                                 'Server' + #4#5 +
                                 '--SkipSavingSettings' + #4#5 +
                                 'Yes';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  try
    Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;
end;


function TfrmWorkerPoolManagerMain.StartWorkerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AWorkerExtraCaption, ABrokerAddress: string; ABrokerPort, AUIClickerPort: Word): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin                                                                                          //ToDo: '--SetBrokerCredFile'
  Result := '';
  ExecAppOptions.PathToApp := '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\FindSubControlWorker.exe';
  ExecAppOptions.ListOfParams := '--SetBrokerAddress' + #4#5 +
                                 'ABrokerAddress' + #4#5 +
                                 '--SetBrokerPort' + #4#5 +
                                 IntToStr(ABrokerPort) + #4#5 +
                                 '--SetUIClickerPort' + #4#5 +
                                 IntToStr(AUIClickerPort) + #4#5 +
                                 '--SetWorkerExtraCaption' + #4#5 +
                                 AWorkerExtraCaption + #4#5 +
                                 '--SkipSavingIni' + #4#5 +
                                 'Yes';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  try
    Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;
end;


function GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort: string): string;
begin
  Result := 'http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/';
end;


function TfrmWorkerPoolManagerMain.GetMQTTAppsOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
  ListOfVars: TStringList;
begin
  if AMachineOS = CWinParam then
  begin
    ExecAppOptions.PathToApp := 'C:\Windows\system32\WindowsPowerShell\v1.0\powershell.exe';       //Windows only
    ExecAppOptions.ListOfParams := 'ps' + #4#5 + '-Name' + #4#5 + CBrokerProcessName + ',' + CWorkerProcessName;  //'-Name' filters all the other processes
    //By default, the columns are:  Handles, NPM(K), PM(K), WS(K), VM(M), CPU(s), Id, ProcessName    (useful columns: Id, ProcessName)
    //There is an extra line, like: -------  ------  -----  -----  -----  ------  --  -----------
  end
  else
    if AMachineOS = CLinParam then
    begin
      ExecAppOptions.PathToApp := 'ps';
      ExecAppOptions.ListOfParams := '-e'; // '-e' means all processes.
      //By default, the columns are: PID, TTY, TIME, CMD   (useful columns: PID, CMD)
      //It will require extra filtering
    end
    else
    begin
      Result := '';
      Exit;
    end;


  ExecAppOptions.WaitForApp := True;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihYes;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  Result := ExecuteExecAppAction(GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort), ExecAppOptions, 'Run PS', 5000);
  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := FastReplace_87ToReturn(Result);
    Result := ListOfVars.Values['$ExecAction_StdOut$'];
    Result := FastReplace_45ToReturn(Result);
  finally
    ListOfVars.Free;
  end;

  //The ideal output would be a list of "ProcessName"="ProcessID", both for brokers and workers
end;


//It works on Win only machines, because of the UIClicker interaction. Eventually, this may be replaced by a client-server approach.
function TfrmWorkerPoolManagerMain.FindWorkerStatusConnected(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): Boolean;  //code version of FindWorkerStatusConnected.clktmpl
var
  FindControl_FindWorker: TClkFindControlOptions;
  WindowOperations_BringToFront: TClkWindowOperationsOptions;
  FindControl_FindMQTTGroupBox: TClkFindControlOptions;
  FindSubControl_FindStatusConnected: TClkFindSubControlOptions;

  Response: string;
begin
  GetDefaultPropertyValues_FindControl(FindControl_FindWorker);
  FindControl_FindWorker.MatchCriteria.SearchForControlMode := sfcmFindWindow;
  FindControl_FindWorker.MatchText := 'FindSubControl Worker - $ExtraWorkerName$';
  FindControl_FindWorker.MatchClassName := 'Window';
  Response := ExecuteFindControlAction(GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort), FindControl_FindWorker, 'Find Worker', 5000, CREParam_FileLocation_ValueDisk);

  GetDefaultPropertyValues_WindowOperations(WindowOperations_BringToFront);
  Response := ExecuteWindowOperationsAction(GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort), WindowOperations_BringToFront);

  GetDefaultPropertyValues_FindControl(FindControl_FindMQTTGroupBox);
  FindControl_FindMQTTGroupBox.MatchText := 'MQTT';
  FindControl_FindMQTTGroupBox.MatchClassName := 'Button';
  FindControl_FindMQTTGroupBox.InitialRectangle.Right := '$Control_Left$';
  FindControl_FindMQTTGroupBox.InitialRectangle.Bottom := '$Control_Top$';
  FindControl_FindMQTTGroupBox.InitialRectangle.LeftOffset := '16';
  FindControl_FindMQTTGroupBox.InitialRectangle.TopOffset := '212';
  FindControl_FindMQTTGroupBox.InitialRectangle.RightOffset := '121';
  FindControl_FindMQTTGroupBox.InitialRectangle.BottomOffset :='289';
  FindControl_FindMQTTGroupBox.UseWholeScreen := False;
  Response := ExecuteFindControlAction(GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort), FindControl_FindWorker, 'Find MQTT GroupBox', 3000, CREParam_FileLocation_ValueDisk);

  GetDefaultPropertyValues_FindSubControl(FindSubControl_FindStatusConnected);
  FindSubControl_FindStatusConnected.MatchCriteria.WillMatchBitmapText := True;
  FindSubControl_FindStatusConnected.MatchCriteria.WillMatchBitmapFiles := False;
  FindSubControl_FindStatusConnected.MatchCriteria.WillMatchPrimitiveFiles := False;
  FindSubControl_FindStatusConnected.AllowToFail := False;
  FindSubControl_FindStatusConnected.MatchText := 'Status: connected';
  SetLength(FindSubControl_FindStatusConnected.MatchBitmapText, 1);
  FindSubControl_FindStatusConnected.MatchBitmapText[0].ForegroundColor := '008000';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].BackgroundColor := '$Color_BtnFace$';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].FontName := 'DejaVu Sans';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].FontSize := 8;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].Bold := False;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].Italic := False;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].Underline := False;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].StrikeOut := False;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].FontQuality := fqNonAntialiased;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].FontQualityUsesReplacement := False;
  FindSubControl_FindStatusConnected.MatchBitmapText[0].FontQualityReplacement := '';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].ProfileName := 'Profile [0]';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].CropLeft := '0';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].CropTop := '0';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].CropRight := '0';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].CropBottom := '0';
  FindSubControl_FindStatusConnected.MatchBitmapText[0].IgnoreBackgroundColor := False;
  FindSubControl_FindStatusConnected.MatchBitmapFiles := '';
  FindSubControl_FindStatusConnected.MatchBitmapAlgorithm := mbaBruteForce;
  FindSubControl_FindStatusConnected.MatchBitmapAlgorithmSettings.XMultipleOf := 1;
  FindSubControl_FindStatusConnected.MatchBitmapAlgorithmSettings.YMultipleOf := 1;
  FindSubControl_FindStatusConnected.MatchBitmapAlgorithmSettings.XOffset := 0;
  FindSubControl_FindStatusConnected.MatchBitmapAlgorithmSettings.YOffset := 0;
  FindSubControl_FindStatusConnected.InitialRectangle.Left := '$Control_Left$';
  FindSubControl_FindStatusConnected.InitialRectangle.Top := '$Control_Top$';
  FindSubControl_FindStatusConnected.InitialRectangle.Right := '$Control_Right$';
  FindSubControl_FindStatusConnected.InitialRectangle.Bottom := '$Control_Bottom$';
  FindSubControl_FindStatusConnected.InitialRectangle.LeftOffset := '337';
  FindSubControl_FindStatusConnected.InitialRectangle.TopOffset := '10';
  FindSubControl_FindStatusConnected.InitialRectangle.RightOffset := '-5';
  FindSubControl_FindStatusConnected.InitialRectangle.BottomOffset := '-35';
  FindSubControl_FindStatusConnected.UseWholeScreen := False;
  FindSubControl_FindStatusConnected.ColorError := '0';
  FindSubControl_FindStatusConnected.AllowedColorErrorCount := '0';
  FindSubControl_FindStatusConnected.WaitForControlToGoAway := False;
  FindSubControl_FindStatusConnected.StartSearchingWithCachedControl := False;
  FindSubControl_FindStatusConnected.CachedControlLeft := '';
  FindSubControl_FindStatusConnected.CachedControlTop := '';
  FindSubControl_FindStatusConnected.MatchPrimitiveFiles := '';
  Response := ExecuteFindSubControlAction(GetMachineConnectionForUIClicker(AMachineAdress, ACmdUIClickerPort), FindSubControl_FindStatusConnected, 'Find status "Connected"', 10000, CREParam_FileLocation_ValueDisk);
end;


end.

