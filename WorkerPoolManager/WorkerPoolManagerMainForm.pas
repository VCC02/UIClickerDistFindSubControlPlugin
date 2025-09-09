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
  //TAppRunning = (arJustStarted, arConnecting, arFullyRunning, arUnknown);
  //               //arJustStarted - the Pool manager just sent the running command.
  //               //arConnecting - might be used for workers, between arJustStarted and fully connected to UIClicker.
  //               //arFullyRunning - the app is running and it is fully connected.
  //               //arUnknown - the app is either not running or not connected (bad state).


  TMachineOS = (mosWin, mosLin, mosUnknown);

  TFSM = (SInit, SMonitorStatus, SStartRemoteApps, SAfterStarting, SWaitForRemoteApps);

  TRunningApp = record
    //Path: string;       //paths will be different between Win and Lin
    Address: string; //Broker address (usually local address, but can be another address, if the worker is connected to a different machine).
    Port: string; //Brokers (server mode) and workers (client mode) have to use the same port. UIClicker will have different ports (server mode).
    StartedAt: QWord; //Timestamp used for waiting for the app to get into a responding/connected state.
    //State: TAppRunning;
    StartCmdResponse: string;
    StartedCount: Integer;

    BrokerUserName: string; //workers may use different user and password
    BrokerPassword: string; //these are set for broker apps

    State: TFSM;
    NextState: TFSM;
    //Please update procedures and functions for deleting this type of item, if adding array fields.
  end;

  PRunningApp = ^TRunningApp;
  TRunningAppArr = array of TRunningApp;   //should not be needed anymore

  TWorkerClickerAppPair = record
    Worker: TRunningApp;
    UIClicker: TRunningApp;
  end;

  TWorkerClickerAppPairArr = array of TWorkerClickerAppPair;

  TAppPool = record
    HasBroker: Boolean; //True when this pool is "running" on a machine, which has a broker (currently, limited to Win machines).  False, when only workers and UIClickers are running there (usually a Lin machine).
    Broker: TRunningApp;
    WorkerClickerPairs: TWorkerClickerAppPairArr;   //Array of workers and their UIClickers, running on the same machine as the broker or on a different machine.

    PoolUserName: string; //This is allocated at the same time the machine is allocated. Multiple users (pools) can be allowed / machine.
    PoolPassWord: string;
    PoolID: string; //Reserved for now. May be implemented later, if required.
    DistAddress: string; //Address of machine, where the "Dist" plugin is used.  //One Dist UIClicker (and machine) / pool.
  end;

  TAppPoolArr = array of TAppPool;


  TMachineRec = record
    Address: string;
    Port: string;
    MachineType: TMachineType;
    MachineOS: TMachineOS;

    ListOfLinWorkersAllocated: string;  //Used when this is a broker machine and another machine with Lin workers point to this one.

    TargetBrokerCountPerWinMachine: Integer;  //how many brokers should be running on this machine
    TargetBrokerCountPerLinMachine: Integer;  //how many brokers should be running on this machine
    TargetWorkerCountPerWinMachine: Integer;  //how many workers should be running on this machine
    TargetWorkerCountPerLinMachine: Integer;  //how many workers should be running on this machine

    //For example, on a machine, there should be one broker, four workers and four UIClickers.
    AppsToBeRunning: TAppPoolArr;
  end;

  PMachineRec = ^TMachineRec;

  TPluginSyncObj = class(TIdSync)
  private
    FCmd: string;
    FParams: TStrings;
    FPeerIP: string;
    FResult: string;
  protected
    procedure DoSynchronize; override;
  end;


  TResourceSyncObj = class(TIdSync)
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
    btnSendPoolCredentialsToLocal: TButton;
    btnGetListeningProcesses: TButton;
    grpSettings: TGroupBox;
    IdHTTPServerPlugins: TIdHTTPServer;
    IdHTTPServerResources: TIdHTTPServer;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    IdSchedulerOfThreadPool2: TIdSchedulerOfThreadPool;
    lblWin: TLabel;
    lblMaxWorkerMachineCount: TLabel;
    lblMinBrokerPort: TLabel;
    lblBrokerCountPerMachine: TLabel;
    lblLin: TLabel;
    lblWorkerCountPerMachine: TLabel;
    lblServiceUIClickerPort: TLabel;
    lblDistUIClickerPort: TLabel;
    memInfo: TMemo;
    memLog: TMemo;
    spnedtBrokerCountPerMachine: TSpinEdit;
    spnedtMaxWorkerMachineCount: TSpinEdit;
    spnedtMinBrokerPort: TSpinEdit;
    spnedtWorkerCountPerWinMachine: TSpinEdit;
    spnedtServiceUIClickerPort: TSpinEdit;
    spnedtDistUIClickerPort: TSpinEdit;
    spnedtWorkerCountPerLinMachine: TSpinEdit;
    tmrFSM: TTimer;
    tmrStartup: TTimer;
    vstMachines: TVirtualStringTree;
    procedure btnAddMachineClick(Sender: TObject);
    procedure btnGetListeningProcessesClick(Sender: TObject);
    procedure btnSendPoolCredentialsToLocalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
    procedure spnedtDistUIClickerPortChange(Sender: TObject);
    procedure spnedtServiceUIClickerPortChange(Sender: TObject);
    procedure tmrFSMTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure vstMachinesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    FServiceUIClickerCmdPortNumber: string;
    FDistUIClickerCmdPortNumber: string;

    function GetCredentialsFile(var AAppPool: TAppPool): string;
    function ProcessPluginCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;
    function ProcessResourceCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;

    function IsServiceUIClickerPathOK(AMachineAddress, ACmdUIClickerPort: string): string;
    function StartMQTTBrokerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, ABrokerPort, ABrokerUsername, ABrokerPassword: string; var AWorkerClickerPairs: TWorkerClickerAppPairArr): string; //returns exec result
    function StartUIClickerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort: string; AUIClickerPort: Word): string; //returns exec result
    function StartWorkerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, AWorkerExtraCaption, ABrokerAddress: string; ABrokerPort, AUIClickerPort: Word; ABrokerUser, ABrokerPassword: string): string; //returns exec result

    procedure StartBrokerApp(var AApp: TRunningApp; AMachineAddress: string; var AWorkerClickerPairs: TWorkerClickerAppPairArr);
    procedure StartWorkerApp(var AApp: TRunningApp; AMachineAddress, AWorkerExtraCaption: string; AUIClickerPort: Word);
    procedure StartUIClickerApp(var AApp: TRunningApp; AMachineAddress: string);

    procedure InitBrokersToBeRunning(AMachineNodeData: PMachineRec);
    procedure InitWorkersToBeRunning(AMachineNodeData: PMachineRec);
    procedure InitUIClickersToBeRunning(AMachineNodeData: PMachineRec);
    function SendPoolCredentials(AMachineAddress, ACmdUIClickerPort, APoolUserName, APoolPassWord: string): string;

    function GetListOfListeningAppsOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, AMachineOS: string): string; //returns exec result
    function GetAppsWhichHaveToBeStartedCount(var AMachineRec: TMachineRec): Integer;
    function FindWorkerStatusConnected(AMachineAddress, ACmdUIClickerPort, AMachineOS, AWorkerExtraCaption: string): Boolean;
    function GetAMachineWithBrokerWhichRequiresLinWorkers(AExcludeMachineAddress: string; AGetAnyValidMachine: Boolean): PMachineRec;

    procedure UpdateServiceUIClickerCmdPortNumber;
    procedure UpdateDistUIClickerCmdPortNumber;

    procedure RunFSM(var AApp: TRunningApp; AAppType: TAppType; AMachineAddress, AWorkerExtraCaption: string; AUIClickerPort: Word; var AWorkerClickerPairs: TWorkerClickerAppPairArr);

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;
  public
    function GetBrokerInfoForClient(APoolClientUserName, APoolClientPassword: string; AIncludeCredentials: Boolean): string;
    function GetMachineByAddress(AMachineAddress: string): PVirtualNode;
    function SetMachineOnline(AWorkerMachineAddress, AWorkerMachineOS, ADistMachineAddress: string): string;
    function RemoveWorkerMachine(AWorkerMachineAddress: string): string;
    function RemoveDistMachine(AWorkerMachineAddress, ADistMachineAddress: string): string;
    function GetAppsStatus(AWorkerMachineAddress: string): string;

    procedure AddToLog(s: string);
  end;


const
  CMachineTypeStr: array[TMachineType] of string = ('Broker', 'Worker', 'BrokerAndWorker');
  CMachineOSStr: array[TMachineOS] of string = (CWinParam, CLinParam, '???');
  CFSMStr: array[TFSM] of string = ('Init', 'MonitorStatus', 'StartRemoteApps', 'AfterStarting', 'WaitForRemoteApps');

  CUIClickerPortOffset = 20000; //This value is added to a worker port, to get the port UIClicker is listening on.
                                //For example, if a worker listens on 12345, its UIClicker listens on 32345.
  CWorkerMonitoringOffset = 120;  //This value is added to a UIClicker port, on which a worker is listening on.
                                  //This is used for getting the list of apps listening on ports.
                                  //If more than 120 ports are used from UIClickers, then please increase this number.

var
  frmWorkerPoolManagerMain: TfrmWorkerPoolManagerMain;

implementation

{$R *.frm}


uses
  ClickerUtils, ClickerActionsClient, ClickerActionProperties, ClickerIniFiles;


procedure TPluginSyncObj.DoSynchronize;
begin
  FResult := 'OK=OK'; //a default response

  //Commands for Pool server
  if Pos('/' + CGetConfigCmd, FCmd) = 1 then     //PoolClient gets broker credentials. It authenticates here with Pool credentials.
  begin
    FResult := '=Nothing' + #13#10 +
               frmWorkerPoolManagerMain.GetBrokerInfoForClient(FParams.Values[CPoolClientUserNameParam],
                                                               FParams.Values[CPoolClientPassWordParam],
                                                               FParams.Values[CIncludeCredentialsParam] = '1') + #13#10 +
              'Remaining=';
    Exit;
  end;
end;


procedure TResourceSyncObj.DoSynchronize;
begin
  FResult := 'OK=OK'; //a default response
  //Commands for broker and worker machines:
  if Pos('/' + CMachineOnline, FCmd) = 1 then   //This request is expected to be sent by the tool/app which "creates" VMs, which knows what IP addresses have the two new VMs (one for "Dist" plugin, and the other for workers).
  begin                                         //The request should be sent once the VMs are created and booted up (after their UIClickers should be started from Startup folder or something similar).
    FResult := frmWorkerPoolManagerMain.SetMachineOnline(FParams.Values[CWorkerMachineAddress],
                                                         FParams.Values[CMachineOSParam],
                                                         FParams.Values[CDistPluginMachineAddress]);
    //FResult := '=Nothing' + #13#10 +
    //           frmWorkerPoolManagerMain.SetMachineOnline(FPeerIP, FParams.Values[CMachineOSParam]) + #13#10 +
    //          'Remaining=';

    Exit;
  end;

  if Pos('/' + CRemoveWorkerMachine, FCmd) = 1 then
  begin
    FResult := frmWorkerPoolManagerMain.RemoveWorkerMachine(FParams.Values[CWorkerMachineAddress]);
    Exit;
  end;

  if Pos('/' + CRemoveDistMachine, FCmd) = 1 then
  begin
    FResult := frmWorkerPoolManagerMain.RemoveDistMachine(FParams.Values[CWorkerMachineAddress],
                                                          FParams.Values[CDistPluginMachineAddress]);
    Exit;
  end;

  if Pos('/' + CGetAppsStatus, FCmd) = 1 then
  begin
    FResult := frmWorkerPoolManagerMain.GetAppsStatus(FParams.Values[CWorkerMachineAddress]);
    Exit;
  end;
end;


{ TfrmWorkerPoolManagerMain }

procedure TfrmWorkerPoolManagerMain.UpdateServiceUIClickerCmdPortNumber;
begin
  FServiceUIClickerCmdPortNumber := IntToStr(spnedtServiceUIClickerPort.Value);
  AddToLog('Using port ' + FServiceUIClickerCmdPortNumber + ' for "Service" UIClicker (on broker and workers machine).');
end;


procedure TfrmWorkerPoolManagerMain.UpdateDistUIClickerCmdPortNumber;
begin
  FDistUIClickerCmdPortNumber := IntToStr(spnedtDistUIClickerPort.Value);
  AddToLog('Using port ' + FDistUIClickerCmdPortNumber + ' for "Dist" UIClicker (where the UIClickerDistFindSubControl plugin is used).');
end;


procedure TfrmWorkerPoolManagerMain.FormCreate(Sender: TObject);
begin
  GeneralConnectTimeout := 500; //0.5s should be enough on local host

  vstMachines.NodeDataSize := SizeOf(TMachineRec);
  tmrStartup.Enabled := True;
end;


procedure TfrmWorkerPoolManagerMain.LoadSettingsFromIni;
var
  Ini: TClkIniReadonlyFile;
begin
  Ini := TClkIniReadonlyFile.Create(ExtractFilePath(ParamStr(0)) + 'WorkerPoolManager.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    spnedtBrokerCountPerMachine.Value := Ini.ReadInteger('Settings', 'BrokerCountPerMachine', spnedtBrokerCountPerMachine.Value);
    spnedtMinBrokerPort.Value := Ini.ReadInteger('Settings', 'MinBrokerPort', spnedtMinBrokerPort.Value);
    spnedtServiceUIClickerPort.Value := Ini.ReadInteger('Settings', 'ServiceUIClickerPort', spnedtServiceUIClickerPort.Value);
    spnedtDistUIClickerPort.Value := Ini.ReadInteger('Settings', 'DistUIClickerPort', spnedtDistUIClickerPort.Value);
    spnedtMaxWorkerMachineCount.Value := Ini.ReadInteger('Settings', 'MaxWorkerMachineCount', spnedtMaxWorkerMachineCount.Value);
    spnedtWorkerCountPerWinMachine.Value := Ini.ReadInteger('Settings', 'WorkerCountPerWinMachine', spnedtWorkerCountPerWinMachine.Value);
    spnedtWorkerCountPerLinMachine.Value := Ini.ReadInteger('Settings', 'WorkerCountPerLinMachine', spnedtWorkerCountPerLinMachine.Value);

    UpdateServiceUIClickerCmdPortNumber;
    UpdateDistUIClickerCmdPortNumber;
  finally
    Ini.Free;
  end;
end;


procedure TfrmWorkerPoolManagerMain.SaveSettingsToIni;
var
  Ini: TClkIniFile;
begin
  Ini := TClkIniFile.Create(ExtractFilePath(ParamStr(0)) + 'WorkerPoolManager.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteInteger('Settings', 'BrokerCountPerMachine', spnedtBrokerCountPerMachine.Value);
    Ini.WriteInteger('Settings', 'MinBrokerPort', spnedtMinBrokerPort.Value);
    Ini.WriteInteger('Settings', 'ServiceUIClickerPort', spnedtServiceUIClickerPort.Value);
    Ini.WriteInteger('Settings', 'DistUIClickerPort', spnedtDistUIClickerPort.Value);
    Ini.WriteInteger('Settings', 'MaxWorkerMachineCount', spnedtMaxWorkerMachineCount.Value);
    Ini.WriteInteger('Settings', 'WorkerCountPerWinMachine', spnedtWorkerCountPerWinMachine.Value);
    Ini.WriteInteger('Settings', 'WorkerCountPerLinMachine', spnedtWorkerCountPerLinMachine.Value);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmWorkerPoolManagerMain.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + s);
end;


function TfrmWorkerPoolManagerMain.GetCredentialsFile(var AAppPool: TAppPool): string;
begin
  Result := '[Credentials]' + #4#5 +
            'Username=' + AAppPool.Broker.BrokerUserName + #4#5 +   //It's ok to return these credentials to the client.
            'Password=' + AAppPool.Broker.BrokerPassword + #4#5 +   //Workers will have their own credentials.
            'PoolID=' + AAppPool.PoolID + #4#5;
end;


function GetDummyCredentialsFile: string;
begin
  Result := '[Credentials]' + #4#5 +
            'Username=BadUserName' + #4#5 +   //It's ok to return these credentials to the client.
            'Password=BadPassword' + #4#5 +   //Workers will have their own credentials.
            'PoolID=BadPoolID' + #4#5;
end;


function TfrmWorkerPoolManagerMain.GetBrokerInfoForClient(APoolClientUserName, APoolClientPassword: string; AIncludeCredentials: Boolean): string;
  function GetErrorContent(AErr: string): string;
  begin
    Result := CBrokerAddressKeyName + '=' + CErrPrefix + AErr + #13#10 +
              CBrokerPortKeyName + '=' + '0' + #13#10;

    if AIncludeCredentials then
      Result := Result + CCredentialsFileKeyName + '=' + GetDummyCredentialsFile;
  end;

  function GetValidContent(var APool: TAppPool): string;
  begin
    Result := CBrokerAddressKeyName + '=' + APool.Broker.Address + #13#10 +
              CBrokerPortKeyName + '=' + APool.Broker.Port + #13#10;

    if AIncludeCredentials then
      Result := Result + CCredentialsFileKeyName + '=' + GetCredentialsFile(APool);
  end;

var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  i: Integer;
  UserNameFound: Boolean;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    Result := GetErrorContent(CUserNotFound);
    Exit;
  end;

  UserNameFound := False;
  repeat
    NodeData := vstMachines.GetNodeData(Node);
    if Assigned(NodeData) then
      for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
        if NodeData^.AppsToBeRunning[i].PoolUserName = APoolClientUserName then
        begin
          UserNameFound := True;

          if NodeData^.AppsToBeRunning[i].PoolPassWord = APoolClientPassword then
            Result := GetValidContent(NodeData^.AppsToBeRunning[i])
          else
            Result := GetErrorContent(CWrongPassword);

          Break;
        end;

    if UserNameFound then
      Break;

    Node := Node^.NextSibling;
  until Node = nil;

  if not UserNameFound then
    Result := GetErrorContent(CUserNotFound);
end;


function TfrmWorkerPoolManagerMain.GetMachineByAddress(AMachineAddress: string): PVirtualNode;
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
      if NodeData^.Address = AMachineAddress then
      begin
        Result := Node;
        Break;
      end;

    Node := Node^.NextSibling;
  until Node = nil;
end;


function GetIndexOfDistAddressFromAppPoolArr(var AAppPoolArr: TAppPoolArr; ADistAddress: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(AAppPoolArr) - 1 do
    if AAppPoolArr[i].DistAddress = ADistAddress then
    begin
      Result := i;
      Break;
    end;
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
  for i := 0 to Length(AMachineNodeData^.AppsToBeRunning) - 1 do     //broker(s)
  begin                                  //set all these, even if AMachineNodeData^.AppsToBeRunning[i].HasBroker is False
    AMachineNodeData^.AppsToBeRunning[i].Broker.Port := IntToStr(spnedtMinBrokerPort.Value + i); //base port + app index
    AMachineNodeData^.AppsToBeRunning[i].Broker.Address := AMachineNodeData^.Address;
    AMachineNodeData^.AppsToBeRunning[i].Broker.State := SInit;//arUnknown;
    AMachineNodeData^.AppsToBeRunning[i].Broker.StartedAt := 0;
    AMachineNodeData^.AppsToBeRunning[i].Broker.StartedCount := 0;
  end;
end;


procedure TfrmWorkerPoolManagerMain.InitWorkersToBeRunning(AMachineNodeData: PMachineRec);
var
  i, j, WorkerCountPerBroker: Integer;
  BrokerToConnectToNodeData: PMachineRec;
begin
  BrokerToConnectToNodeData := nil;
  if AMachineNodeData^.MachineOS = mosLin then
  begin
    BrokerToConnectToNodeData := GetAMachineWithBrokerWhichRequiresLinWorkers(AMachineNodeData^.Address, False);  //get an available machine (no workers connected to it)
    if BrokerToConnectToNodeData = nil then
      BrokerToConnectToNodeData := GetAMachineWithBrokerWhichRequiresLinWorkers(AMachineNodeData^.Address, True); //now get any valid broker, just get one!
  end;

  for i := 0 to Length(AMachineNodeData^.AppsToBeRunning) - 1 do //pools
  begin
    WorkerCountPerBroker := Length(AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs);

    for j := 0 to WorkerCountPerBroker - 1 do     //workers
    begin
      if AMachineNodeData^.MachineOS = mosWin then
      begin
        AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Address := AMachineNodeData^.AppsToBeRunning[i].Broker.Address; //just connect to the same machine

        if spnedtBrokerCountPerMachine.Value > 0 then
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Port := AMachineNodeData^.AppsToBeRunning[i].Broker.Port //0000, 1111 etc
        else
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Port := '0'; //not sure if these workers should be left unconnected, instead of being connected to a default, which might not be desired
      end
      else
      begin  //mosLin - find an available machine with broker
        if BrokerToConnectToNodeData <> nil then
        begin
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Address := BrokerToConnectToNodeData^.Address; //can be different, if this is a machine with Lin workers, connected to a Win broker
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Port := BrokerToConnectToNodeData^.Port;
          BrokerToConnectToNodeData^.ListOfLinWorkersAllocated := BrokerToConnectToNodeData^.ListOfLinWorkersAllocated + AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Address + #13#10;
        end
        else   //No broker available for this machine. Ideally, these Lin machines, if they start before any broker, then they should be connected later.
        begin
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Address := ''; //this will have to be set later
          AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Port := '';
        end;
      end;  //mosLin

      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.State := SInit;//arUnknown;
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.StartedAt := 0;
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.StartedCount := 0;
    end; //j
  end;  //i
end;


procedure TfrmWorkerPoolManagerMain.InitUIClickersToBeRunning(AMachineNodeData: PMachineRec);   //this should be called after InitWorkersToBeRunning
var
  i, j: Integer;
begin
  for i := 0 to Length(AMachineNodeData^.AppsToBeRunning) - 1 do  //pools
    for j := 0 to Length(AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do //UIClickers
    begin
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.Port := IntToStr(StrToIntDef(AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.Port, 0) + CUIClickerPortOffset + i * Length(AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs) + j);
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.Address := AMachineNodeData^.AppsToBeRunning[i].Broker.Address;
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.State := SInit;//arUnknown;
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.StartedAt := 0;
      AMachineNodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.StartedCount := 0;
    end;
end;


function TfrmWorkerPoolManagerMain.SetMachineOnline(AWorkerMachineAddress, AWorkerMachineOS, ADistMachineAddress: string): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  ToBeAdded: Boolean;
  i, j, k: Integer;
  Res: string;
  PoolIdx: Integer;
  FoundAvailableDistSlot: Boolean;
begin
  Result := 'OK';
  FoundAvailableDistSlot := False;
  Node := GetMachineByAddress(AWorkerMachineAddress);
  ToBeAdded := Node = nil;

  if ToBeAdded then
  begin
    if Integer(vstMachines.RootNodeCount) < spnedtMaxWorkerMachineCount.Value then
    begin
      Node := vstMachines.AddChild(vstMachines.RootNode);
      vstMachines.Repaint;
    end
    else
    begin
      Result := CTooManyWorkerMachines;
      Exit;
    end;
  end;

  NodeData := vstMachines.GetNodeData(Node);
  NodeData^.Address := AWorkerMachineAddress;

  if AWorkerMachineOS = CWinParam then
  begin
    NodeData^.MachineOS := mosWin;
    if ToBeAdded then
    begin
      NodeData^.TargetBrokerCountPerWinMachine := spnedtBrokerCountPerMachine.Value;
      NodeData^.TargetWorkerCountPerWinMachine := spnedtWorkerCountPerWinMachine.Value;

      SetLength(NodeData^.AppsToBeRunning, NodeData^.TargetBrokerCountPerWinMachine);   //Win

      for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do  //pools
      begin
        NodeData^.AppsToBeRunning[i].HasBroker := True;
        SetLength(NodeData^.AppsToBeRunning[i].WorkerClickerPairs, NodeData^.TargetWorkerCountPerWinMachine);
        NodeData^.AppsToBeRunning[i].DistAddress := ''; //Init with empty. Will update later.
      end;

      for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do  //pools
        if NodeData^.AppsToBeRunning[i].DistAddress = '' then
        begin
          NodeData^.AppsToBeRunning[i].DistAddress := ADistMachineAddress;
          Result := ADistMachineAddress;
          Break;
        end;
    end;
  end
  else
    if AWorkerMachineOS = CLinParam then
    begin
      NodeData^.MachineOS := mosLin;
      if ToBeAdded then
      begin
        NodeData^.TargetBrokerCountPerLinMachine := 0;
        NodeData^.TargetWorkerCountPerLinMachine := spnedtWorkerCountPerWinMachine.Value;

        SetLength(NodeData^.AppsToBeRunning, NodeData^.TargetBrokerCountPerWinMachine);   //still Win     - a new logic is required here (with UI settings), to decide exactly what apps are running on Lin
        for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do  //pools
        begin
          NodeData^.AppsToBeRunning[i].HasBroker := NodeData^.TargetBrokerCountPerLinMachine > 0;
          SetLength(NodeData^.AppsToBeRunning[i].WorkerClickerPairs, NodeData^.TargetWorkerCountPerLinMachine);
          NodeData^.AppsToBeRunning[i].DistAddress := ''; //Init with empty. Will update later.
        end;

        for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do  //pools
          if NodeData^.AppsToBeRunning[i].DistAddress = '' then
          begin
            NodeData^.AppsToBeRunning[i].DistAddress := ADistMachineAddress;
            Result := ADistMachineAddress;
            Break;
          end;
      end;
    end
    else
    begin
      NodeData^.MachineOS := mosUnknown;
      //TargetBrokerCountPerWinMachine is not set here, in case the machine already exists and it is updated with bad info
    end;

  if ToBeAdded then
    if NodeData^.MachineType in [mtBroker, mtBrokerAndWorker] then
      for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do  //pools
      begin
        Randomize;              //More complex names and password will have to be generated here
        Sleep(33);
        NodeData^.AppsToBeRunning[i].PoolUserName := 'RandomlyGeneratedUser_' + IntToStr(GetTickCount64) + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt));
        Randomize;
        Sleep(33);
        NodeData^.AppsToBeRunning[i].PoolPassWord := 'RandomlyGeneratedPassword_' + IntToStr(GetTickCount64) + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt));

        for k := 1 to 7 + Random(3) do
        begin
          Sleep(33);
          Randomize;
          NodeData^.AppsToBeRunning[i].PoolPassWord := NodeData^.AppsToBeRunning[i].PoolPassWord + IntToHex(Random(MaxInt)) + IntToStr(Random(MaxInt));
        end;

        Randomize;
        Sleep(33);
        NodeData^.AppsToBeRunning[i].Broker.BrokerUserName := 'User_' + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt));  //used by clients
        Randomize;
        Sleep(33);
        NodeData^.AppsToBeRunning[i].Broker.BrokerPassword := 'UnknownPassword_' + IntToStr(GetTickCount64) + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt));
        Randomize;
        Sleep(33);
        NodeData^.AppsToBeRunning[i].PoolID := StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + '_' + IntToStr(Random(MaxInt));

        for k := 1 to 7 + Random(3) do
        begin
          Sleep(33);
          Randomize;
          NodeData^.AppsToBeRunning[i].Broker.BrokerPassword := NodeData^.AppsToBeRunning[i].Broker.BrokerPassword + IntToHex(Random(MaxInt)) + IntToStr(Random(MaxInt));
        end;

        for j := 0 to Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
        begin
          Sleep(33);
          Randomize;
          NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.BrokerUserName := 'WorkerUser_' + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt)) + IntToStr(Random(MaxInt));
          Sleep(33);
          Randomize;
          NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.BrokerPassword := 'CustomPassword_' + StringReplace(DateTimeToStr(Now), ':', '_', [rfReplaceAll]) + IntToStr(Random(MaxInt)) + IntToStr(Random(MaxInt));

          for k := 1 to 7 + Random(3) do
          begin
            Sleep(33);
            Randomize;
            NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.BrokerPassword := NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.BrokerPassword + IntToHex(Random(MaxInt)) + IntToStr(Random(MaxInt));
          end;
        end;
      end;

  if ToBeAdded then
  begin
    InitBrokersToBeRunning(NodeData);
    InitWorkersToBeRunning(NodeData);
    InitUIClickersToBeRunning(NodeData);

    if Length(NodeData^.AppsToBeRunning) > 0 then
    begin
      PoolIdx := GetIndexOfDistAddressFromAppPoolArr(NodeData^.AppsToBeRunning, ADistMachineAddress);
      if (PoolIdx = -1) or (ADistMachineAddress = '') then
      begin
        //The number of pools should match the number of Dist machines. If PoolIdx is -1, then the machine might have now a new address.
        //Not sure how to handle this right now.
        AddToLog('Can''t find the pool by the address of Dist machine: ' + ADistMachineAddress);
      end
      else  // NodeData^.AppsToBeRunning[PoolIdx].DistAddress should already be the same as ADistMachineAddress.
      begin
        NodeData^.AppsToBeRunning[PoolIdx].DistAddress := ADistMachineAddress;
        Res := SendPoolCredentials(ADistMachineAddress,
                                   FDistUIClickerCmdPortNumber,
                                   NodeData^.AppsToBeRunning[PoolIdx].PoolUserName,
                                   NodeData^.AppsToBeRunning[PoolIdx].PoolPassWord);
        AddToLog('Sending pool credentials result for dist machine ' + ADistMachineAddress + ': ' + Res);
      end;
    end
    else
      AddToLog('No pools are allocated.');
  end //ToBeAdded
  else
  begin //the function may be called with an existing worker machine address, but a new dist machine address has to be assigned to this worker machine address
    for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
      if NodeData^.AppsToBeRunning[i].DistAddress = '' then //find an available slot
      begin
        FoundAvailableDistSlot := True;
        NodeData^.AppsToBeRunning[i].DistAddress := ADistMachineAddress;
        Res := SendPoolCredentials(ADistMachineAddress,
                                   FDistUIClickerCmdPortNumber,
                                   NodeData^.AppsToBeRunning[i].PoolUserName,
                                   NodeData^.AppsToBeRunning[i].PoolPassWord);
        AddToLog('Sending pool credentials result for dist machine ' + ADistMachineAddress + ': ' + Res);

        Break; //do not update the others
      end;

    if not FoundAvailableDistSlot then
      Result := CTooManyDistMachines;
  end;

  vstMachines.InvalidateNode(Node);

  if Result = 'OK' then
    Result := CMachineSet;
end;


function TfrmWorkerPoolManagerMain.RemoveWorkerMachine(AWorkerMachineAddress: string): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Result := CWorkerMachineNotFound;

  Node := GetMachineByAddress(AWorkerMachineAddress);
  if Node = nil then
    Exit;

  NodeData := vstMachines.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.Address = AWorkerMachineAddress then
  begin
    vstMachines.DeleteNode(Node);
    vstMachines.Repaint;
    Result := CMachineRemoved;
    Exit;
  end;
end;


procedure RemoveDistMachineAtIndex(var APoolArr: TAppPoolArr; AIndex: Integer);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > Length(APoolArr) - 1) then
    Exit;

  for i := AIndex to Length(APoolArr) - 2 do
    APoolArr[i] := APoolArr[i + 1]; //there is a dynamic array field in this structure, but it should be fine to have pointer := pointer.

  SetLength(APoolArr, Length(APoolArr) - 1);
end;


function TfrmWorkerPoolManagerMain.RemoveDistMachine(AWorkerMachineAddress, ADistMachineAddress: string): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  i: Integer;
  DistFound: Boolean;
begin
  Result := CWorkerMachineNotFound;

  Node := GetMachineByAddress(AWorkerMachineAddress);
  if Node = nil then
    Exit;

  NodeData := vstMachines.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  if NodeData^.Address = AWorkerMachineAddress then
  begin
    DistFound := False;
    for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
      if NodeData^.AppsToBeRunning[i].DistAddress = ADistMachineAddress then
      begin
        DistFound := True;
        NodeData^.AppsToBeRunning[i].DistAddress := ''; //simply clear the address, do not call RemoveDistMachineAtIndex(NodeData^.AppsToBeRunning, i);  //the number of items should not be modified
        vstMachines.RepaintNode(Node);
        Result := CMachineRemoved;
        Exit;
      end;

    if not DistFound then
      Result := CDistMachineNotFound;
  end;
end;


function GetAppsRunningCountFromPoll(var APoll: TAppPool): Integer;
var
  j: Integer;
begin
  Result := 0;
  if APoll.HasBroker then
  begin
    if (APoll.Broker.State = SMonitorStatus) and
       (APoll.Broker.StartedCount > 0) then
      Inc(Result);
  end;

  for j := 0 to Length(APoll.WorkerClickerPairs) - 1 do
  begin
    if (APoll.WorkerClickerPairs[j].Worker.State = SMonitorStatus) and
       (APoll.WorkerClickerPairs[j].Worker.StartedCount > 0) and
       (APoll.WorkerClickerPairs[j].UIClicker.State = SMonitorStatus) and
       (APoll.WorkerClickerPairs[j].UIClicker.StartedCount > 0) then
      Inc(Result);
  end;
end;


function GetAppsRunningCountFromAllPoll(var AAllPolls: TAppPoolArr): Integer;
var
  i: Integer;
  FoundCountPerPool: TIntArr;
begin
  SetLength(FoundCountPerPool, Length(AAllPolls));
  for i := 0 to Length(AAllPolls) - 1 do
    FoundCountPerPool[i] := GetAppsRunningCountFromPoll(AAllPolls[i]);

  Result := 0;
  for i := 0 to Length(AAllPolls) - 1 do
    Inc(Result, FoundCountPerPool[i]);
end;


function GetTargetAppCountFromMachine(AMachine: PMachineRec): Integer;
begin
  case AMachine^.MachineOS of
    mosWin:
      Result := AMachine^.TargetBrokerCountPerWinMachine * (AMachine^.TargetWorkerCountPerWinMachine + 1);
    mosLin:
      Result := AMachine^.TargetBrokerCountPerLinMachine * AMachine^.TargetWorkerCountPerLinMachine;
    mosUnknown:
      Result := 0;
  end;
end;


function TfrmWorkerPoolManagerMain.GetAppsStatus(AWorkerMachineAddress: string): string;
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  TotalFoundCount, TargetCountPerMachine: Integer;
begin
  Result := CWorkerMachineNotFound;

  Node := GetMachineByAddress(AWorkerMachineAddress);
  if Node = nil then
    Exit;

  NodeData := vstMachines.GetNodeData(Node);
  if NodeData = nil then
    Exit;

  TotalFoundCount := GetAppsRunningCountFromAllPoll(NodeData^.AppsToBeRunning);
  TargetCountPerMachine := GetTargetAppCountFromMachine(NodeData);

  if TotalFoundCount = 0 then
    Result := CNoAppRunning
  else
    if TotalFoundCount < TargetCountPerMachine then
      Result := CSomeAppsRunning
    else
      Result := CAllAppsRunning;    //If there are Lin workers, connecting to a Win broker, then the number of apps is greater than TargetCount.
end;


function TfrmWorkerPoolManagerMain.ProcessPluginCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;
var
  SyncObj: TPluginSyncObj;
begin
  SyncObj := TPluginSyncObj.Create;
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


function TfrmWorkerPoolManagerMain.ProcessResourceCommand(ACmd: string; AParams: TStrings; APeerIP: string): string;
var
  SyncObj: TResourceSyncObj;
begin
  SyncObj := TResourceSyncObj.Create;
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

  AResponseInfo.ResponseText := ProcessPluginCommand(ARequestInfo.Document, ARequestInfo.Params, AContext.Binding.PeerIP);
  Sleep(10);
end;


procedure TfrmWorkerPoolManagerMain.IdHTTPServerResourcesCommandGet(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
begin
  ARequestInfo.Params.LineBreak := #13#10;
  AResponseInfo.ContentType := 'text/plain';
  AResponseInfo.CharSet := 'US-ASCII';

  AResponseInfo.ResponseText := ProcessResourceCommand(ARequestInfo.Document, ARequestInfo.Params, AContext.Binding.PeerIP);
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


procedure TfrmWorkerPoolManagerMain.spnedtServiceUIClickerPortChange(Sender: TObject);
begin
  UpdateServiceUIClickerCmdPortNumber;
end;


procedure TfrmWorkerPoolManagerMain.spnedtDistUIClickerPortChange(Sender: TObject);
begin
  UpdateDistUIClickerCmdPortNumber;
end;


function TfrmWorkerPoolManagerMain.GetAppsWhichHaveToBeStartedCount(var AMachineRec: TMachineRec): Integer;
var
  i, j: Integer;
begin
  Result := 0;
  for i := 0 to Length(AMachineRec.AppsToBeRunning) - 1 do
    if AMachineRec.AppsToBeRunning[i].Broker.State = {arUnknown}SInit then
      if AMachineRec.AppsToBeRunning[i].Broker.StartedCount < 3 then
        Inc(Result);

  for i := 0 to Length(AMachineRec.AppsToBeRunning) - 1 do
    for j := 0 to Length(AMachineRec.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
    begin
      if AMachineRec.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.State = {arUnknown}SInit then
        if AMachineRec.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.StartedCount < 3 then
          Inc(Result);

      if AMachineRec.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.State = {arUnknown}SInit then
        if AMachineRec.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.StartedCount < 3 then
          Inc(Result);
    end;
end;


procedure TfrmWorkerPoolManagerMain.StartBrokerApp(var AApp: TRunningApp; AMachineAddress: string; var AWorkerClickerPairs: TWorkerClickerAppPairArr);
begin
  Inc(AApp.StartedCount);
  AApp.StartedAt := GetTickCount64;
  AApp.StartCmdResponse := StartMQTTBrokerOnRemoteMachine(AMachineAddress, FServiceUIClickerCmdPortNumber, AApp.Port, AApp.BrokerUserName, AApp.BrokerPassword, AWorkerClickerPairs);
  Sleep(250);
end;


procedure TfrmWorkerPoolManagerMain.StartWorkerApp(var AApp: TRunningApp; AMachineAddress, AWorkerExtraCaption: string; AUIClickerPort: Word);
begin
  if AApp.Address <> '' then
  begin
    Inc(AApp.StartedCount);
    AApp.StartedAt := GetTickCount64;
    AApp.StartCmdResponse := StartWorkerOnRemoteMachine(AMachineAddress,
                                                       FServiceUIClickerCmdPortNumber,
                                                       AWorkerExtraCaption,
                                                       AApp.Address,
                                                       StrToIntDef(AApp.Port, 1183),
                                                       AUIClickerPort,
                                                       AApp.BrokerUserName,
                                                       AApp.BrokerPassword);

    Sleep(250);
  end
  //else
  //begin
  //  //this is a Lin machine with workers, which should wait for another machine with broker(s)
  //  InitWorkersToBeRunning(@AMachineRec); //this should be enough to be called once (outside the for loop)
  //  AddToLog('Workers from ' + AMachineRec.Address + ' do not have a broker to connect to.');
  //end;
end;


procedure TfrmWorkerPoolManagerMain.StartUIClickerApp(var AApp: TRunningApp; AMachineAddress: string);
begin
  Inc(AApp.StartedCount);
  AApp.StartedAt := GetTickCount64;
  AApp.StartCmdResponse := StartUIClickerOnRemoteMachine(AMachineAddress, FServiceUIClickerCmdPortNumber, StrToIntDef(AApp.Port, 1183)); //UIClicker will listen on BrokerPort+20000
  Sleep(250);
end;


procedure TfrmWorkerPoolManagerMain.RunFSM(var AApp: TRunningApp; AAppType: TAppType; AMachineAddress, AWorkerExtraCaption: string; AUIClickerPort: Word; var AWorkerClickerPairs: TWorkerClickerAppPairArr);
begin
  case AApp.State of
    SInit:
    begin

    end;

    SMonitorStatus:
    begin

    end;

    SStartRemoteApps:
    begin
      case AAppType of
        atBroker:
          if AApp.StartedCount < 3 then
            StartBrokerApp(AApp, AMachineAddress, AWorkerClickerPairs);

        atWorker:
          if AApp.StartedCount < 3 then
            StartWorkerApp(AApp, AMachineAddress, AWorkerExtraCaption, AUIClickerPort);

        atUIClicker:
          if AApp.StartedCount < 3 then
            StartUIClickerApp(AApp, AMachineAddress);
      end;
    end;

    SAfterStarting:
    begin
      //used for logging
    end;

    SWaitForRemoteApps:
    begin
      //get "connected" status of workers etc.
    end;
  end;

  case AApp.State of
    SInit:
      AApp.NextState := SMonitorStatus;

    SMonitorStatus:
      if AApp.StartedCount < 1 then         //a new logic is required
      begin
        case AAppType of                    //Call a function, to get the list of running processes. If this app is not running, then run it.
          atBroker:
            AApp.NextState := SStartRemoteApps;

          atWorker:
            AApp.NextState := SStartRemoteApps;

          atUIClicker:
            AApp.NextState := SStartRemoteApps;
        end;
      end
      else
        AApp.NextState := SMonitorStatus;  //another state is required here, to wait (e.g. 10s) before the next call to GetListOfAppsWhichHaveToBeStarted

    SStartRemoteApps:
      AApp.NextState := SAfterStarting;

    SAfterStarting:
      AApp.NextState := SWaitForRemoteApps;

    SWaitForRemoteApps:
      if GetTickCount64 - AApp.StartedAt > 10000 then
      begin
        AApp.NextState := SMonitorStatus;
        //dbg:
        //if AAppType = atWorker then
        //  if FindWorkerStatusConnected(AMachineAddress, FCmdPortNumber, CWinParam, AWorkerExtraCaption) then
        //    ;
      end
      else
        AApp.NextState := SWaitForRemoteApps;
  end;

  AApp.State := AApp.NextState;
end;


procedure TfrmWorkerPoolManagerMain.tmrFSMTimer(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
  i, j: Integer;
  CurrentApp: PRunningApp;
  WorkerExtraCaption: string;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
    Exit;

  repeat
    NodeData := vstMachines.GetNodeData(Node);

    for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
    begin
      CurrentApp := @NodeData^.AppsToBeRunning[i].Broker;
      RunFSM(CurrentApp^,
             atBroker,
             NodeData^.Address,
             '',
             0,
             NodeData^.AppsToBeRunning[i].WorkerClickerPairs);

      if (CurrentApp^.StartCmdResponse <> '') and (CurrentApp^.State = SAfterStarting) then
      begin
        if Pos(CREResp_RemoteExecResponseVar + '=1', CurrentApp^.StartCmdResponse) = 1 then
          AddToLog('Successfully started broker[' + IntToStr(i) + '] at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '.  StartedCount = ' + IntToStr(CurrentApp^.StartedCount))
        else
          AddToLog('Error starting broker[' + IntToStr(i) + '] at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '. ' + CurrentApp^.StartCmdResponse);;
      end;

      for j := 0 to Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
      begin
        WorkerExtraCaption := IntToStr(i * Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) + j);
        CurrentApp := @NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker;
        RunFSM(NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker,
               atWorker,
               NodeData^.Address,
               WorkerExtraCaption,
               StrToIntDef(NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.Port, 0),
               NodeData^.AppsToBeRunning[i].WorkerClickerPairs);

        if (CurrentApp^.StartCmdResponse <> '') and (CurrentApp^.State = SAfterStarting) then
        begin
          if Pos(CREResp_RemoteExecResponseVar + '=1', CurrentApp^.StartCmdResponse) = 1 then
            AddToLog('Successfully started worker[' + IntToStr(i) + '][' + IntToStr(j) + '] = [' + WorkerExtraCaption + '] at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '.  StartedCount = ' + IntToStr(CurrentApp^.StartedCount))
          else
            AddToLog('Error starting worker at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '. ' + CurrentApp^.StartCmdResponse);
        end;
      end;

      for j := 0 to Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
      begin
        CurrentApp := @NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker;
        WorkerExtraCaption := IntToStr(i * Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) + j);
        RunFSM(NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker,
               atUIClicker,
               NodeData^.Address,
               '',
               0,
               NodeData^.AppsToBeRunning[i].WorkerClickerPairs);

        if (CurrentApp^.StartCmdResponse <> '') and (CurrentApp^.State = SAfterStarting) then
        begin
          if Pos(CREResp_RemoteExecResponseVar + '=1', CurrentApp^.StartCmdResponse) = 1 then
            AddToLog('Successfully started UIClicker[' + IntToStr(i) + '][' + IntToStr(j) + '] = [' + WorkerExtraCaption + '] at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '.  StartedCount = ' + IntToStr(CurrentApp^.StartedCount))
          else
            AddToLog('Error starting UIClicker at ' + CurrentApp^.Address + ':' + CurrentApp^.Port + '. ' + CurrentApp^.StartCmdResponse);
        end;
      end;
    end;

    Node := Node^.NextSibling;
  until Node = nil;

  vstMachines.Repaint;
end;


procedure TfrmWorkerPoolManagerMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  LoadSettingsFromIni;

  try
    IdHTTPServerPlugins.Active := True;
    AddToLog('WorkerPoolManager is listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort) + ' for plugin connections.');
  except
    on E: Exception do
      AddToLog('Can''t listening on port ' + IntToStr(IdHTTPServerPlugins.DefaultPort) + '  ' + E.Message);
  end;

  try
    IdHTTPServerResources.Active := True;
    AddToLog('WorkerPoolManager is listening on port ' + IntToStr(IdHTTPServerResources.DefaultPort) + ' for resource management connections.');
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
  i, j: Integer;
begin
  try
    NodeData := vstMachines.GetNodeData(Node);

    case Column of
      0: CellText := IntToStr(Node^.Index);
      1: CellText := NodeData^.Address;
      2: CellText := CMachineTypeStr[NodeData^.MachineType];
      3: CellText := CMachineOSStr[NodeData^.MachineOS];
      4:
      begin
        CellText := '';
        for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
        begin
          CellText := CellText + '[' + NodeData^.AppsToBeRunning[i].DistAddress + ']';
          if i < Length(NodeData^.AppsToBeRunning) - 1 then
            CellText := CellText + ', ';
        end;
      end;

      5:
      begin
        CellText := 'Broker: ';
        for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
        begin
          CellText := CellText + CFSMStr[NodeData^.AppsToBeRunning[i].Broker.State] + '  ';

          CellText := CellText + '  Workers: ';
          for j := 0 to Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
            CellText := CellText + CFSMStr[NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].Worker.State] + '  ';

          CellText := CellText + '  UIClickers: ';
          for j := 0 to Length(NodeData^.AppsToBeRunning[i].WorkerClickerPairs) - 1 do
            CellText := CellText + CFSMStr[NodeData^.AppsToBeRunning[i].WorkerClickerPairs[j].UIClicker.State] + '  ';
        end;
      end;
    end;
  except
    CellText := '?';
  end;
end;


procedure TfrmWorkerPoolManagerMain.btnAddMachineClick(Sender: TObject);
begin
  SetMachineOnline('127.0.0.1', CWinParam, '127.0.0.1');
end;


procedure TfrmWorkerPoolManagerMain.btnGetListeningProcessesClick(
  Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PMachineRec;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    MessageBoxFunction('Please select a machine in list.', PChar(Caption), 0);
    Exit;
  end;

  NodeData := vstMachines.GetNodeData(Node);
  AddToLog(#13#10 + GetListOfListeningAppsOnRemoteMachine(NodeData^.Address, FServiceUIClickerCmdPortNumber, CWinParam));
end;


function TfrmWorkerPoolManagerMain.SendPoolCredentials(AMachineAddress, ACmdUIClickerPort, APoolUserName, APoolPassWord: string): string;  //This sends the pool credentials to the UIClicker which runs the plugin.
var
  Link: string;
  Content: string; //pool credentials file
  MemStream: TMemoryStream;
begin                                                              //CRECmd_SetMemPluginFile saves the file in the plugin's InMemFS.   CRECmd_SetRenderedFile and CRECmd_SendFileToServer can't be used here.
  Link := 'http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/' + CRECmd_SetMemPluginFile + '?' + CREParam_FileName + '=' + CPoolCredentialsFileName;

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
  i: Integer;
begin
  Node := vstMachines.GetFirst;
  if Node = nil then
  begin
    AddToLog('Please add a machine first, in order to send proper content.');

    Res := SendPoolCredentials('127.0.0.1', FDistUIClickerCmdPortNumber, 'Dummy_UserName', 'Dummy_Password');
    AddToLog('Sending dummy pool credentials result: ' + Res);

    Exit;
  end;

  NodeData := vstMachines.GetNodeData(Node);

  if Length(NodeData^.AppsToBeRunning) > 0 then
    for i := 0 to Length(NodeData^.AppsToBeRunning) - 1 do
    begin
      Res := SendPoolCredentials(NodeData^.AppsToBeRunning[i].DistAddress,
                                 FDistUIClickerCmdPortNumber,
                                 NodeData^.AppsToBeRunning[i].PoolUserName,
                                 NodeData^.AppsToBeRunning[i].PoolPassWord);
      AddToLog('Sending pool credentials result: ' + Res);
    end
    else
      AddToLog('No pools are allocated.');
end;


procedure TfrmWorkerPoolManagerMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    SaveSettingsToIni;
  except
    on E: Exception do
      AddToLog('Error saving settings. ' + E.Message);
  end;
end;


function TfrmWorkerPoolManagerMain.IsServiceUIClickerPathOK(AMachineAddress, ACmdUIClickerPort: string): string;
const
  CPathToWPM = '$PathToWPM$';
var
  SetVarOptions: TClkSetVarOptions;
  Response, VarPath: string;
  ListOfResponses: TStringList;
begin
  SetVarOptions.ListOfVarNames := CPathToWPM + #13#10;
  SetVarOptions.ListOfVarValues := '$AppDir$\..\UIClickerDistFindSubControlPlugin\WorkerPoolManager\WorkerPoolManager.exe' + #13#10;
  SetVarOptions.ListOfVarEvalBefore := '1' + #13#10;

  SetVarOptions.FailOnException := False;
  try
    Response := ExecuteSetVarAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', SetVarOptions, False);

    ListOfResponses := TStringList.Create;
    try
      ListOfResponses.Text := FastReplace_87ToReturn(Response);
      VarPath := ExpandFileName(ListOfResponses.Values[CPathToWPM], ExtractFilePath(ParamStr(0)));
      Result := BoolToStr(UpperCase(VarPath) = UpperCase(ParamStr(0)), 'OK.', 'not OK. It causes a mismatch on: "' + VarPath + '" vs. "' + ParamStr(0) + '".');
    finally
      ListOfResponses.Free;
    end;
  except
    on E: Exception do
      AddToLog('Ex on setting broker params before starting broker: ' + E.Message);
  end;
end;


function TfrmWorkerPoolManagerMain.StartMQTTBrokerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, ABrokerPort, ABrokerUsername, ABrokerPassword: string; var AWorkerClickerPairs: TWorkerClickerAppPairArr): string; //returns exec result
var
  SetVarOptions: TClkSetVarOptions;
  ExecAppOptions: TClkExecAppOptions;
  PluginOptions: TClkPluginOptions;
  j: Integer;
  ExecResults: TStringList;
  ErrPos1, ErrPos2: Integer;
begin
  Result := '';

  //set some vars for plugin
  SetVarOptions.ListOfVarNames := '$BrokerPortNumber$' + #13#10 +    //these will not be evaluated by SetVar
                                  '$PasswordFile$' + #13#10 +
                                  '$ConfFile$' + #13#10 +
                                  //'$BrokerUsername$' + #13#10 +   //for debugging only
                                  //'$BrokerPassword$' + #13#10 +   //for debugging only
                                  '$WorkerCount$' + #13#10 +
                                  '$ExecAction_Err$' + #13#10 +
                                  '$PluginError$' + #13#10 +
                                  '$ExecAction_StdOut$' + #13#10;

  SetVarOptions.ListOfVarValues := ABrokerPort + #13#10 +
                                   '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\pp_' + ABrokerPort + '.txt' + #13#10 +
                                   '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\mosquitto' + ABrokerPort + '.conf' + #13#10 +
                                   //ABrokerUsername + #13#10 +   //for debugging only
                                   //ABrokerPassword + #13#10 +   //for debugging only
                                   IntToStr(Length(AWorkerClickerPairs)) + #13#10 +
                                   '' + #13#10 +
                                   '' + #13#10 +
                                   '' + #13#10;

  SetVarOptions.ListOfVarEvalBefore := '0' + #13#10 +
                                       '1' + #13#10 +
                                       '1' + #13#10 +
                                       //'0' + #13#10 +  //for debugging only
                                       //'0' + #13#10 +  //for debugging only
                                       '0' + #13#10 +
                                       '0' + #13#10 +
                                       '0' + #13#10 +
                                       '0' + #13#10;

  //for j := 0 to Length(AWorkerClickerPairs) - 1 do     //for debugging only
  //begin
  //  SetVarOptions.ListOfVarNames := SetVarOptions.ListOfVarNames + '$Worker[' + IntToStr(j) + '].Username$' + #13#10;
  //  SetVarOptions.ListOfVarNames := SetVarOptions.ListOfVarNames + '$Worker[' + IntToStr(j) + '].Password$' + #13#10;
  //
  //  SetVarOptions.ListOfVarValues := SetVarOptions.ListOfVarValues + AWorkerClickerPairs[j].Worker.BrokerUserName + #13#10;
  //  SetVarOptions.ListOfVarValues := SetVarOptions.ListOfVarValues + AWorkerClickerPairs[j].Worker.BrokerPassword + #13#10;
  //
  //  SetVarOptions.ListOfVarEvalBefore := SetVarOptions.ListOfVarEvalBefore + '0' + #13#10;
  //  SetVarOptions.ListOfVarEvalBefore := SetVarOptions.ListOfVarEvalBefore + '0' + #13#10;
  //end;

  SetVarOptions.FailOnException := False;
  try
    Result := ExecuteSetVarAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', SetVarOptions, False);
  except
    on E: Exception do
      AddToLog('Ex on setting broker params before starting broker: ' + E.Message);
  end;


  //generate pp file
  ExecAppOptions.PathToApp := 'C:\Program Files\mosquitto\mosquitto_passwd.exe';
  ExecAppOptions.ListOfParams := '-b' + #4#5 +
                                 '-c' + #4#5 +  //this is needed only for the first call, to clear the file
                                 '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\pp_' + ABrokerPort + '.txt' + #4#5 +
                                 ABrokerUsername + #4#5 +
                                 ABrokerPassword;
  ExecAppOptions.WaitForApp := True;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihYes;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  AddToLog('Service UIClicker path: ' + IsServiceUIClickerPathOK(AMachineAddress, ACmdUIClickerPort));

  try
    Result := ExecuteExecAppAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run passwd for plugin', 5000, False);
    AddToLog('Creating pp file for plugin: ' + Copy(Result, Pos('$ExecAction_StdOut$', Result), MaxInt));

    ErrPos1 := Pos('$ExecAction_StdOut$=Error: Unable to open file', Result);
    if ErrPos1 > 0 then
    begin
      ErrPos2 := Pos('for writing. No such file or directory.', Result);
      if ErrPos2 > ErrPos1 then
        AddToLog('The "Service UIClicker" has to be run from its default directory, not from TestDriver, because the path to pp_' + ABrokerPort + '.txt is derived from it.');
    end;
  except
    on E: Exception do
      AddToLog('Ex on creating pp file for plugin: ' + E.Message);
  end;

  ExecResults := TStringList.Create;
  try
    ExecResults.Text := FastReplace_87ToReturn(Result);
    if ExecResults.Values['$ExecAction_Err$'] <> '' then
    begin
      Result := '$ExecAction_Err$=' + ExecResults.Values['$ExecAction_Err$'];
      Exit;
    end;

    if ExecResults.Values['$PluginError$'] <> '' then
    begin
      Result := '$PluginError$=' + ExecResults.Values['$PluginError$'];
      Exit;
    end;

    if ExecResults.Values['$ExecAction_StdOut$'] <> '' then
    begin
      Result := '$ExecAction_StdOut$=' + ExecResults.Values['$ExecAction_StdOut$'];
      Exit;
    end;
  finally
    ExecResults.Free;
  end;

  //this for loop requires the above setting other fields from the ExecAppOptions structure
  for j := 0 to Length(AWorkerClickerPairs) - 1 do
  begin
    ExecAppOptions.ListOfParams := '-b' + #4#5 +  //-b only
                                 '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\pp_' + ABrokerPort + '.txt' + #4#5 +
                                 AWorkerClickerPairs[j].Worker.BrokerUserName + #4#5 +
                                 AWorkerClickerPairs[j].Worker.BrokerPassword;

    try
      Result := ExecuteExecAppAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run passwd for worker[' + IntToStr(j) + ']', 5000, False);
      AddToLog('Creating pp file for worker[' + IntToStr(j) + ']: ' + Copy(Result, Pos('$ExecAction_StdOut$', Result), MaxInt));
    except
      on E: Exception do
        AddToLog('Ex on creating pp file for worker: ' + E.Message);
    end;

    ExecResults := TStringList.Create;
    try
      ExecResults.Text := FastReplace_87ToReturn(Result);
      if ExecResults.Values['$ExecAction_Err$'] <> '' then
      begin
        Result := '$ExecAction_Err$=' + ExecResults.Values['$ExecAction_Err$'];
        Exit;
      end;

      if ExecResults.Values['$PluginError$'] <> '' then
      begin
        Result := '$PluginError$=' + ExecResults.Values['$PluginError$'];
        Exit;
      end;

      if ExecResults.Values['$ExecAction_StdOut$'] <> '' then
      begin
        Result := '$ExecAction_StdOut$=' + ExecResults.Values['$ExecAction_StdOut$'];
        Exit;
      end;
    finally
      ExecResults.Free;
    end;
  end; //for j

  //update pp_' + ABrokerPort + '.txt'
  PluginOptions.FileName := '$AppDir$\..\UIClickerDistFindSubControlPlugin\BrokerParams\lib\$AppBitness$-$OSBitness$\BrokerParams.dll';
  PluginOptions.ListOfPropertiesAndValues := ''; //to be set
  PluginOptions.ListOfPropertiesAndTypes := '';
  try
    Result := ExecutePluginAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', PluginOptions, False);
  except
    on E: Exception do
      AddToLog('Ex on running plugin for setting broker params before starting broker: ' + E.Message);
  end;

  ExecResults := TStringList.Create;
  try
    ExecResults.Text := FastReplace_87ToReturn(Result);
    if ExecResults.Values['$ExecAction_Err$'] <> '' then
    begin
      Result := '$ExecAction_Err$=' + ExecResults.Values['$ExecAction_Err$'];
      Exit;
    end;

    if ExecResults.Values['$PluginError$'] <> '' then
    begin
      Result := '$PluginError$=' + ExecResults.Values['$PluginError$'];
      Exit;
    end;
  finally
    ExecResults.Free;
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
    Result := ExecuteExecAppAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000, False);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;

  ExecResults := TStringList.Create;
  try
    ExecResults.Text := FastReplace_87ToReturn(Result);
    if ExecResults.Values['$ExecAction_Err$'] <> '' then
    begin
      Result := '$ExecAction_Err$=' + ExecResults.Values['$ExecAction_Err$'];
      Exit;
    end;

    if ExecResults.Values['$ExecAction_StdOut$'] <> '' then
    begin
      Result := '$ExecAction_StdOut$=' + ExecResults.Values['$ExecAction_StdOut$'];
      Exit;
    end;
  finally
    ExecResults.Free;
  end;
end;


function TfrmWorkerPoolManagerMain.StartUIClickerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort: string; AUIClickerPort: Word): string; //returns exec result
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
    Result := ExecuteExecAppAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000, False);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;
end;


function TfrmWorkerPoolManagerMain.StartWorkerOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, AWorkerExtraCaption, ABrokerAddress: string; ABrokerPort, AUIClickerPort: Word; ABrokerUser, ABrokerPassword: string): string; //returns exec result
var
  ExecAppOptions: TClkExecAppOptions;
begin                                                                                          //ToDo: '--SetBrokerCredFile'
  Result := '';
  ExecAppOptions.PathToApp := '$AppDir$\..\UIClickerDistFindSubControlPlugin\Worker\FindSubControlWorker.exe';
  ExecAppOptions.ListOfParams := '--SetBrokerAddress' + #4#5 +
                                 ABrokerAddress + #4#5 +
                                 '--SetBrokerPort' + #4#5 +
                                 IntToStr(ABrokerPort) + #4#5 +
                                 '--SetUIClickerPort' + #4#5 +
                                 IntToStr(AUIClickerPort) + #4#5 +
                                 '--SetMonitoringPort' + #4#5 +
                                 IntToStr(AUIClickerPort + CWorkerMonitoringOffset) + #4#5 +
                                 '--SetWorkerExtraCaption' + #4#5 +
                                 AWorkerExtraCaption + #4#5 +
                                 '--SkipSavingIni' + #4#5 +
                                 'Yes' + #4#5 +
                                 '--SetBrokerCredFile' + #4#5 +
                                 '_:\NonExistent.txt'  + #4#5 + //if setting a non-existent file, the worker can use the credentials passed from cmd line
                                 '--SetBrokerUser' + #4#5 +
                                 ABrokerUser + #4#5 +
                                 '--SetBrokerPassword' + #4#5 +
                                 ABrokerPassword
                                 ;
  ExecAppOptions.WaitForApp := False;
  ExecAppOptions.AppStdIn := '';
  ExecAppOptions.CurrentDir := ExtractFileDir(ExecAppOptions.PathToApp);
  ExecAppOptions.UseInheritHandles := uihNo;
  ExecAppOptions.NoConsole := True; //True means do not display a console

  try
    Result := ExecuteExecAppAction('http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/', ExecAppOptions, 'Run Broker', 5000, False);
  except
    on E: Exception do
      AddToLog('Ex on starting broker: ' + E.Message);
  end;
end;


function GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort: string): string;
begin
  Result := 'http://' + AMachineAddress + ':' + ACmdUIClickerPort + '/';
end;


function TfrmWorkerPoolManagerMain.GetListOfListeningAppsOnRemoteMachine(AMachineAddress, ACmdUIClickerPort, AMachineOS: string): string; //list of listening processes  Port=PID
var
  ExecAppOptions: TClkExecAppOptions;
  ListOfVars, ListOfProcesses, FilteredListOfProcesses: TStringList;
  i: Integer;
  s, Port, PID: string;
begin
  if AMachineOS = CWinParam then
  begin
    //ExecAppOptions.PathToApp := 'C:\Windows\system32\WindowsPowerShell\v1.0\powershell.exe';       //Windows only
    //ExecAppOptions.ListOfParams := 'ps';// + #4#5 + '-Name' + #4#5 + CBrokerProcessName + ',' + CWorkerProcessName;  //'-Name' filters all the other processes   - ps displays an error if there is no item called CBrokerProcessName or CWorkerProcessName
    //By default, the columns are:  Handles, NPM(K), PM(K), WS(K), VM(M), CPU(s), Id, ProcessName    (useful columns: Id, ProcessName)
    //There is an extra line, like: -------  ------  -----  -----  -----  ------  --  -----------

    //To kill a process:  taskkill /PID 2056 /F    (does not require admin)
    //To get list of connections:  netstat -ao     (does not require admin)

    ExecAppOptions.PathToApp := 'C:\Windows\system32\netstat.exe';
    ExecAppOptions.ListOfParams := '-ao';
  end
  else
    if AMachineOS = CLinParam then
    begin
      ExecAppOptions.PathToApp := 'ps';
      ExecAppOptions.ListOfParams := '-e'; // '-e' means all processes.
      //By default, the columns are: PID, TTY, TIME, CMD   (useful columns: PID, CMD)
      //It will require extra filtering

      //To get list of connections:   ss -tlnp       //ToDo  find a way to display the PID
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

  Result := ExecuteExecAppAction(GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort), ExecAppOptions, 'Run PS', 5000);
  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := FastReplace_87ToReturn(Result);
    Result := ListOfVars.Values['$ExecAction_StdOut$'];
    Result := FastReplace_45ToReturn(Result);
  finally
    ListOfVars.Free;
  end;

  //The ideal output would be a list of "ProcessName"="ProcessID", or "PortNumber"="ProcessID",, both for brokers and workers
  ListOfProcesses := TStringList.Create;
  FilteredListOfProcesses := TStringList.Create;
  try
    //ListOfProcesses.LineBreak:=; //do not set, in case WorkerPoolManager is running on Lin
    ListOfProcesses.Text := Result;

    if AMachineOS = CWinParam then
    begin
      for i := 0 to ListOfProcesses.Count - 1 do
      begin
        s := ListOfProcesses.Strings[i];
        if (Pos('TCP    0.0.0.0:', s) > 0) and (Pos(' LISTENING ', s) > 0) then
        begin
          s := Copy(s, Pos(':', s) + 1, MaxInt);
          Port := Copy(s, 1, Pos(' ', s) - 1);
          PID := Copy(s, Pos('LISTENING', s) + Length('LISTENING') + 1, MaxInt);
          PID := Trim(PID);
          FilteredListOfProcesses.Add(Port + '=' + PID);
        end;

        Result := FilteredListOfProcesses.Text;
      end;
    end
    else
      if AMachineOS = CLinParam then
        Result := 'Not implemented yet.';
  finally
    ListOfProcesses.Free;
    FilteredListOfProcesses.Free;
  end;
end;


//It works on Win only machines, because of the UIClicker interaction. Eventually, this may be replaced by a client-server approach.
function TfrmWorkerPoolManagerMain.FindWorkerStatusConnected(AMachineAddress, ACmdUIClickerPort, AMachineOS, AWorkerExtraCaption: string): Boolean;  //code version of FindWorkerStatusConnected.clktmpl
var
  FindControl_FindWorker: TClkFindControlOptions;
  WindowOperations_BringToFront: TClkWindowOperationsOptions;
  FindControl_FindMQTTGroupBox: TClkFindControlOptions;
  FindSubControl_FindStatusConnected: TClkFindSubControlOptions;

  Response: string;
  ResultStr: TStringList;
begin
  GetDefaultPropertyValues_FindControl(FindControl_FindWorker);
  FindControl_FindWorker.MatchCriteria.SearchForControlMode := sfcmEnumWindows;
  FindControl_FindWorker.MatchText := 'FindSubControl Worker - ' + AWorkerExtraCaption;   // $ExtraWorkerName$
  FindControl_FindWorker.MatchClassName := 'Window';
  Response := ExecuteFindControlAction(GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort), FindControl_FindWorker, 'Find Worker', 5000, CREParam_FileLocation_ValueDisk);

  ResultStr := TStringList.Create;
  try
    ResultStr.Text := FastReplace_87ToReturn(Response);
    Result := (StrToIntDef(ResultStr.Values['$Control_Left$'], -1) > 0) and (ResultStr.Values['$ExecAction_Err$'] = '');
    if not Result then
    begin
      AddToLog('Find Worker: ' + ResultStr.Values['$ExecAction_Err$']);
      Exit;
    end;
  finally
    ResultStr.Free;
  end;

  GetDefaultPropertyValues_WindowOperations(WindowOperations_BringToFront);
  Response := ExecuteWindowOperationsAction(GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort), WindowOperations_BringToFront);

  ResultStr := TStringList.Create;
  try
    ResultStr.Text := FastReplace_87ToReturn(Response);
    Result := (StrToIntDef(ResultStr.Values['$Control_Left$'], -1) > 0) and (ResultStr.Values['$ExecAction_Err$'] = '');
    if not Result then
    begin
      AddToLog('Bring to front: ' + ResultStr.Values['$ExecAction_Err$']);
      Exit;
    end;
  finally
    ResultStr.Free;
  end;

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
  Response := ExecuteFindControlAction(GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort), FindControl_FindWorker, 'Find MQTT GroupBox', 3000, CREParam_FileLocation_ValueDisk);

  ResultStr := TStringList.Create;
  try
    ResultStr.Text := FastReplace_87ToReturn(Response);
    Result := (StrToIntDef(ResultStr.Values['$Control_Left$'], -1) > 0) and (ResultStr.Values['$ExecAction_Err$'] = '');
    if not Result then
    begin
      AddToLog('Find MQTT GroupBox: ' + ResultStr.Values['$ExecAction_Err$']);
      Exit;
    end;
  finally
    ResultStr.Free;
  end;

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
  Response := ExecuteFindSubControlAction(GetMachineConnectionForUIClicker(AMachineAddress, ACmdUIClickerPort), FindSubControl_FindStatusConnected, 'Find status "Connected"', 10000, CREParam_FileLocation_ValueDisk);

  ResultStr := TStringList.Create;
  try
    ResultStr.Text := FastReplace_87ToReturn(Response);
    Result := (StrToIntDef(ResultStr.Values['$Control_Left$'], -1) > 0) and (ResultStr.Values['$ExecAction_Err$'] = '');
    if not Result then
    begin
      AddToLog('Find status "Connected": ' + ResultStr.Values['$ExecAction_Err$']);
      Exit;
    end;
  finally
    ResultStr.Free;
  end;
end;


end.

