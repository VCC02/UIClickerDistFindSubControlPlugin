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
  TMachineOS = (mosWin, mosLin, mosUnknown);

  TFSM = (SInit, SMonitorStatus, SStartRemoteApps, SWaitForRemoteApps);

  TMachineRec = record
    Address: string;
    Port: string;
    MachineType: TMachineType;
    MachineOS: TMachineOS;
    PoolUserName: string; //This is allocated at the same time the machine is allocated. If multiple users are allowed / machine, then that's a different story.

    BrokerUserName: string; //workers may use different user and password
    BrokerPassword: string;
    PoolID: string; //Reserved for now. May be implemented later, if required.

    State: TFSM;
    NextState: TFSM;
    TargetBrokerCountPerWinMachine: Integer;
    TargetBrokerCountPerLinMachine: Integer;
    TargetWorkerCountPerWinMachine: Integer;
    TargetWorkerCountPerLinMachine: Integer;
    ListOfAppsWhichHaveToBeStarted: string; //ListOfStrings
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

    function StartMQTTBrokerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, ABrokerPort: string): string; //returns exec result
    function StartUIClickerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort: string; ABrokerPort: Word): string; //returns exec result
    function StartWorkerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AWorkerExtraCaption: string; ABrokerPort: Word): string; //returns exec result

    function GetMQTTAppsOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): string; //returns exec result
    function GetListOfAppsWhichHaveToBeStarted(var AMachineRec: TMachineRec): string;  //returns list of paths

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

var
  frmWorkerPoolManagerMain: TfrmWorkerPoolManagerMain;

implementation

{$R *.frm}


uses
  ClickerUtils, ClickerActionsClient;


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

  if AMachineOS = CWinParam then
  begin
    NodeData^.MachineOS := mosWin;
    if ToBeAdded then
    begin
      NodeData^.TargetBrokerCountPerWinMachine := spnedtBrokerCountPerMachine.Value;
      NodeData^.TargetWorkerCountPerWinMachine := spnedtBrokerCountPerMachine.Value * 4;
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
  end;

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


function TfrmWorkerPoolManagerMain.GetListOfAppsWhichHaveToBeStarted(var AMachineRec: TMachineRec): string;  //returns list of processes
var
  CurrentProcesses: TStringList;
  i, BrokerCount, WorkerCount: Integer;
  TargetBrokerCount, TargetWorkerCount: Integer;
begin
  CurrentProcesses := TStringList.Create;
  try
    CurrentProcesses.LineBreak := #13#10;
    CurrentProcesses.Text := GetMQTTAppsOnRemoteMachine(AMachineRec.Address, '5444', CMachineOSStr[AMachineRec.MachineOS]);
    //in addition to this list, UIClicker from the target machine should also be used to get the "connected" status, displayed on every worker window
    //Also, from starting a worker, to getting the "connected" status, there are about 8s. The FSM has to take this into account.

    BrokerCount := 0;
    for i := 0 to CurrentProcesses.Count - 1 do
      if Pos(CBrokerProcessName, CurrentProcesses.Strings[i]) > 0 then // a better filtering is required here  - after implementing "ProcessName"="ProcessID"
        Inc(BrokerCount);

    WorkerCount := 0;
    for i := 0 to CurrentProcesses.Count - 1 do
      if Pos(CWorkerProcessName, CurrentProcesses.Strings[i]) > 0 then // a better filtering is required here  - after implementing "ProcessName"="ProcessID"
        Inc(WorkerCount);

    case AMachineRec.MachineOS of
      mosWin:
      begin
        TargetBrokerCount := AMachineRec.TargetBrokerCountPerWinMachine;
        TargetWorkerCount := AMachineRec.TargetWorkerCountPerWinMachine;
      end;

      mosLin:
      begin
        TargetBrokerCount := AMachineRec.TargetBrokerCountPerLinMachine;
        TargetWorkerCount := AMachineRec.TargetWorkerCountPerLinMachine;
      end;

      else  //mosUnknown
      begin
        TargetBrokerCount := 0;
        TargetWorkerCount := 0;
      end;
    end;

    Result := '';
    for i := 1 to TargetBrokerCount - BrokerCount do
      Result := Result + CBrokerProcessName + #13#10;

    for i := 1 to TargetWorkerCount - WorkerCount do
      Result := Result + CWorkerProcessName + #13#10;
  finally
    CurrentProcesses.Free;
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
      AMachineRec.ListOfAppsWhichHaveToBeStarted := GetListOfAppsWhichHaveToBeStarted(AMachineRec);
    end;

    SStartRemoteApps:
    begin
      //based on AMachineRec.ListOfAppsWhichHaveToBeStarted, run all the apps
    end;

    SWaitForRemoteApps:
    begin
      //get "connected" status of workers etc.
    end;
  end;

  case AMachineRec.State of
    SInit:
      AMachineRec.NextState := SStartRemoteApps;

    SMonitorStatus:
      if AMachineRec.ListOfAppsWhichHaveToBeStarted > '' then
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
end;


procedure TfrmWorkerPoolManagerMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  try
    IdHTTPServerPlugins.Active := True;
    AddToLog('Listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort));
  except
    on E: Exception do
      AddToLog('Can''t listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort) + '  ' + E.Message);
  end;

  try
    IdHTTPServerResources.Active := True;
    AddToLog('Listening on port ' + IntToStr(IdHTTPServerResources.DefaultPort));
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
    end;
  except
    CellText := '?';
  end;
end;


procedure TfrmWorkerPoolManagerMain.btnAddMachineClick(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  //manual adding (as an example)
  Node := vstMachines.AddChild(vstMachines.RootNode);
  NodeData := vstMachines.GetNodeData(Node);
  NodeData^.Address := '127.0.0.1';
  NodeData^.Port := '1883';
  NodeData^.MachineType := mtBrokerAndWorker;
  NodeData^.PoolUserName := 'First';
  NodeData^.BrokerUserName := 'User_' + DateTimeToStr(Now);
  NodeData^.BrokerPassword := 'Unknown';
  NodeData^.PoolID := DateTimeToStr(Now);

  NodeData^.State := SInit;
  NodeData^.NextState := SInit;
  NodeData^.TargetBrokerCountPerWinMachine := 0;
  NodeData^.TargetBrokerCountPerLinMachine := 0;
  NodeData^.TargetWorkerCountPerWinMachine := 0;
  NodeData^.TargetWorkerCountPerLinMachine := 0;
end;


procedure TfrmWorkerPoolManagerMain.btnStartTwoBrokersClick(Sender: TObject);
begin
  memLog.Lines.Add(StartMQTTBrokerOnRemoteMachine('127.0.0.1', '5444', '21883'));
  memLog.Lines.Add(StartMQTTBrokerOnRemoteMachine('127.0.0.1', '5444', '21884'));
  memLog.Lines.Add(GetMQTTAppsOnRemoteMachine('127.0.0.1', '5444', CWinParam));
end;


function TfrmWorkerPoolManagerMain.StartMQTTBrokerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, ABrokerPort: string): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin
  ExecAppOptions.PathToApp := 'C:\Program Files\mosquitto\mosquitto.exe';
  ExecAppOptions.ListOfParams := '-c' + #4#5 + '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\mosquitto' + ABrokerPort + '.conf';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
end;


function TfrmWorkerPoolManagerMain.StartUIClickerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort: string; ABrokerPort: Word): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin
  ExecAppOptions.PathToApp := '$AppDir$\UIClicker.exe';
  ExecAppOptions.ListOfParams := '--ExtraCaption ' + IntToStr(ABrokerPort) + ' --ServerPort ' + IntToStr(ABrokerPort + 20000) + ' --SetExecMode Server --SkipSavingSettings Yes';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
end;


function TfrmWorkerPoolManagerMain.StartWorkerOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AWorkerExtraCaption: string; ABrokerPort: Word): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin                                                                                          //ToDo: '--SetBrokerCredFile'
  ExecAppOptions.PathToApp := '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\FindSubControlWorker.exe';
  ExecAppOptions.ListOfParams := '--ExtraCaption ' + IntToStr(ABrokerPort) + ' --SetBrokerAddress 127.0.0.1 --SetBrokerPort' + IntToStr(ABrokerPort) + ' --SetUIClickerPort' + IntToStr(ABrokerPort + 20000) + ' --SetWorkerExtraCaption ' + AWorkerExtraCaption + ' --SkipSavingIni Yes';
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000);
end;


function TfrmWorkerPoolManagerMain.GetMQTTAppsOnRemoteMachine(AMachineAdress, ACmdUIClickerPort, AMachineOS: string): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
  ListOfVars: TStringList;
  i: Integer;
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

  Result := ExecuteExecAppAction('http://' + AMachineAdress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run PS', 5000);
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

end.

