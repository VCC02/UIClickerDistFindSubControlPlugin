{
    Copyright (C) 2025 VCC
    creation date: 02 Dec 2025
    initial release date: 02 Dec 2025

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


unit DistVisorFSM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{To some extent, depending on how expensive it is to run multiple machines, this is the recommended machine allocation matrix:

- DistVisor runs (preferably) on a Lin machine, where it has all the plugins on disk, that it has to send to the other machines.
  If possible, this machine should be the one which hold the information about which VM is created and which is destroyed.
- For every user, there is a Win VM (Dist machine) with a Dist UCIClicker. As a future design, if a single Dist UIClicker cannot handle user actions,
  then multiple instances should run on the same machine, belonging to the same pool of credentials (i.e. same user).
  In addition to this Dist UIClicker, there has to be two more UIClicker instances, one called Service and the other Monitor.
  These additional instances are used to restart UIClicker and each other, in case of crashing.
- For every user, there is a Win VM (worker machine) with a broker (MQTT), and several workers (worker exes and UIClickers).
  In addition to these, there has to be two more UIClicker instances, Service and Monitor.
  These are also used for restarting UIClicker in case of crashing, but the Service one will receive the BrokerParams plugin (from DistVisor)
  and execute it by WorkerPoolManager, when starting the MQTT broker.
  It is also possible to have multiple pools of credentials on the same worker machine, where every pool belongs to a different user. The AddMachine and RemoveMachine functions don't support this yet.
- WorkerPoolManager requires that a MachineOnline request will have the address of the Dist machine and the address of the paired worker machine.
  That means the mkDist machine must be created before the mkWorker machine, so that the address is already available.
  The pairing is done by DistVisor, based on UserID of the already available mkDist machines in the list.
  This is one more reason to have this machine available, before the mkWorker one.
}

type
  TDistVisorFSM = (SInit,
                   SCheckForMonitoringUIClicker, SStartMonitoringUIClicker, SWaitForMonitoringUIClicker,   //present on all machine types
                   SCheckForServiceUIClicker, SStartServiceUIClicker, SWaitForServiceUIClicker,            //present on all machine types
                   SCheckForDistUIClicker, SStartDistUIClicker, SWaitForDistUIClicker,  //For starters, there is one Dist UIClicker / user, with its own machine.
                   SCheckForWPM, SStartWPM, SWaitForWPM, //WPM = WorkePoolManager   - only a single machine should have WorkePoolManager
                   SSendServicePlugins, SPairWithDist,  //should be used after starting Service on worker machine only
                   SSendDistPlugins    //should be used after starting Dist
                   );

const
  CDistVisorFSMStr: array[TDistVisorFSM] of string = (
                   'SInit',
                   'SCheckForMonitoringUIClicker', 'SStartMonitoringUIClicker', 'SWaitForMonitoringUIClicker',
                   'SCheckForServiceUIClicker', 'SStartServiceUIClicker', 'SWaitForServiceUIClicker',
                   'SCheckForDistUIClicker', 'SStartDistUIClicker', 'SWaitForDistUIClicker',
                   'SCheckForWPM', 'SStartWPM', 'SWaitForWPM',
                   'SSendServicePlugins', 'SPairWithDist',
                   'SSendDistPlugins'
                   );

  CDefaultMonitoringPort = '54400';  //monitoring UIClicker instance
  CDefaultServiceUIClickerPort = '55444';
  CDefaultDistUIClickerPort = '5444';
  CWPMPort = '11884';

  CDefaultDistUIClickerBitness = '32';
  CDefaultServiceUIClickerBitness = '32';

  CMonitoringExtraCaption = 'Monitor';
  CServiceExtraCaption = 'Service';
  CDistExtraCaption = 'Dist';

type
  TMachineKind = (mkDist, mkWorker, mkWPM);

  TTargetMachine = record
    State: TDistVisorFSM;
    NextState: TDistVisorFSM;
    MachineKind: TMachineKind;

    MonitoringUIClicker_tk: QWord;
    ServiceUIClicker_tk: QWord;
    Tool_tk: QWord; //a tool can be the Dist UIClicker, or WPM.

    MonitoringUIClickerIsRunning: Boolean;
    ServiceUIClickerIsRunning: Boolean;
    ToolIsRunning: Boolean; //a tool can be the Dist UIClicker, or WPM.

    StartMonitoringUIClickerResult: string;
    StartServiceUIClickerResult: string;
    StartToolResult: string;

    MachineAddress: string; //same address for all tools
    PairedDistMachineAddress: string; //set for mkWorker machines

    MonitoringUIClickerPort: string;
    ServiceUIClickerPort: string;
    ToolPort: string;  // DistUIClicker or WPM

    ServiceUIClickerBitness: string;
    DistUIClickerBitness: string;

    UserID: string;
  end;

  TTargetMachineArr = array of TTargetMachine;


const
  CMachineKindStr: array[TMachineKind] of string = ('mkDist', 'mkWorker', 'mkWPM');

procedure ExecuteFSM;
procedure AddMachine(AKind: TMachineKind; AMachineAddress, AMonitoringUIClickerPort, AServiceUIClickerPort, AToolPort, AUserID: string);
procedure RemoveMachine(AMachineAddress, AUserID: string);
function SendRemoveWorkerMachineRequestToWPM(AWorkerMachineAddress: string): string;


implementation


uses
  ClickerActionsClient, ClickerUtils, ClickerActionProperties,
  DistPluginSender, WorkerPoolCommonConsts;


var
  Machines: TTargetMachineArr;
  CritSec: TRTLCriticalSection;


function GetMachineIndexByAddress(AMachineAddress, AUserID: string): Integer;  //should be called from CritSec only
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Machines) - 1 do
    if (Machines[i].MachineAddress = AMachineAddress) and (Machines[i].UserID = AUserID) then
    begin
      Result := i;
      Exit;
    end;
end;


function GetDistMachineIndexByUserID(AUserID: string): Integer;   //should be called from CritSec only
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Machines) - 1 do
    if (Machines[i].UserID = AUserID) and (Machines[i].MachineKind = mkDist) then
    begin
      Result := i;
      Exit;
    end;
end;


procedure AddMachine(AKind: TMachineKind; AMachineAddress, AMonitoringUIClickerPort, AServiceUIClickerPort, AToolPort, AUserID: string);
const
  CErr = 'Machine already exists when adding.';
  CPairErr = 'Pair Dist machine does not exist.'; //The Dist machine is not yet in the list (identified by UserID).
var
  n: Integer;
  DistMachineAddressIdx: Integer;
begin
  EnterCriticalSection(CritSec);
  try
    if AKind = mkWPM then
      AUserID := '';       //this allows running a WPM on the same machine as the other tools, because they require a valid user ID.

    if GetMachineIndexByAddress(AMachineAddress, AUserID) > -1 then
    begin
      WriteLn(CErr);
      raise Exception.Create(CErr); //if not handled, this results in HTTP error 500
    end;

    n := Length(Machines);
    SetLength(Machines, n + 1);

    Machines[n].MachineKind := AKind;
    Machines[n].MachineAddress := AMachineAddress;
    Machines[n].MonitoringUIClickerPort := AMonitoringUIClickerPort;
    Machines[n].ServiceUIClickerPort := AServiceUIClickerPort;
    Machines[n].ToolPort := AToolPort;

    Machines[n].ServiceUIClickerBitness := CDefaultServiceUIClickerBitness;
    Machines[n].DistUIClickerBitness := CDefaultDistUIClickerBitness;

    Machines[n].UserID := AUserID;

    Machines[n].State := SInit;
    Machines[n].NextState := SInit;

    if AKind = mkWorker then //do the pairing with a Dist machine
    begin
      DistMachineAddressIdx := GetDistMachineIndexByUserID(AUserID);

      if DistMachineAddressIdx > -1 then
        Machines[n].PairedDistMachineAddress := Machines[DistMachineAddressIdx].MachineAddress
      else
      begin
        WriteLn(CPairErr);
        raise Exception.Create(CPairErr); //if not handled, this results in HTTP error 500
      end
    end;
  finally
    LeaveCriticalSection(CritSec);
  end;
end;


procedure RemoveMachine(AMachineAddress, AUserID: string);
const
  CErr = 'Machine not found when removing.';
var
  Idx, i: Integer;
begin
  EnterCriticalSection(CritSec);
  try
    Idx := GetMachineIndexByAddress(AMachineAddress, AUserID);
    if Idx = -1 then
    begin
      WriteLn(CErr);
      raise Exception.Create(CErr); //if not handled, this results in HTTP error 500
    end;

    for i := Idx to Length(Machines) - 2 do
      Machines[i] := Machines[i + 1];

    SetLength(Machines, Length(Machines) - 1);
  finally
    LeaveCriticalSection(CritSec);
  end;
end;


function GetWPMMachineAddress: string;
var
  i: Integer;
begin
  EnterCriticalSection(CritSec);
  try
    Result := '';
    for i := 0 to Length(Machines) - 1 do
      if Machines[i].MachineKind = mkWPM then
      begin
        Result := Machines[i].MachineAddress;
        Exit;
      end;
  finally
    LeaveCriticalSection(CritSec);
  end;
end;


function Get_ExecAction_Err_FromExecutionResult(ARes: string): string;
var
  ListOfVars: TStringList;
begin
  if Pos('$RemoteExecResponse$', ARes) <> 1 then
  begin
    Result := ARes; //probably a connection error
    Exit;
  end;

  ListOfVars := TStringList.Create;
  try
    ListOfVars.Text := FastReplace_87ToReturn(ARes);
    Result := ListOfVars.Values['$ExecAction_Err$'];
  finally
    ListOfVars.Free
  end;
end;


function StartMonitoringUIClicker(AServiceUIClickerAddress, AServiceUIClickerPort, AMonitoringPort: string): string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\UIClicker.exe';
  ExecApp.ListOfParams := StringReplace('--SetExecMode Server --AutoSwitchToExecTab Yes --ServerPort ' + AMonitoringPort + ' ' + '--ExtraCaption ' + CMonitoringExtraCaption + ' --AddAppArgsToLog Yes --SkipSavingSettings Yes', ' ', #4#5, [rfReplaceAll]);
                                                                        //sending the command to service, to start the monitor
  Res := ExecuteExecAppAction('http://' + AServiceUIClickerAddress + ':' + AServiceUIClickerPort + '/', ExecApp, 'Start Monitoring UIClicker', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


function StartServiceUIClicker(AServiceUIClickerAddress, AServiceUIClickerPort, AMonitoringPort: string): string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\UIClicker.exe';
  ExecApp.ListOfParams := StringReplace('--SetExecMode Server --AutoSwitchToExecTab Yes --ServerPort ' + AServiceUIClickerPort + ' ' + '--ExtraCaption ' + CServiceExtraCaption + ' --AddAppArgsToLog Yes --SkipSavingSettings Yes', ' ', #4#5, [rfReplaceAll]);
                                                                          //sending the command to monitor, to start the service
  Res := ExecuteExecAppAction('http://' + AServiceUIClickerAddress + ':' + AMonitoringPort + '/', ExecApp, 'Start Service UIClicker', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


function StartDistUIClicker(AServiceUIClickerAddress, AServiceUIClickerPort, ADistPort: string): string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\UIClicker.exe';
  ExecApp.ListOfParams := StringReplace('--SetExecMode Server --AutoSwitchToExecTab Yes --ServerPort ' + ADistPort + ' ' + '--ExtraCaption ' + CDistExtraCaption + ' --AddAppArgsToLog Yes --SkipSavingSettings Yes', ' ', #4#5, [rfReplaceAll]);
                                                                        //sending the command to service, to start the dist UIClicker
  Res := ExecuteExecAppAction('http://' + AServiceUIClickerAddress + ':' + AServiceUIClickerPort + '/', ExecApp, 'Start Dist UIClicker', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


function StartWorkerPoolManager(AServiceUIClickerAddress, AServiceUIClickerPort: string): string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\..\UIClickerDistFindSubControlPlugin\WorkerPoolManager\WorkerPoolManager.exe';

  Res := ExecuteExecAppAction('http://' + AServiceUIClickerAddress + ':' + AServiceUIClickerPort + '/', ExecApp, 'Start WorkerPoolManager', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


function SendPairingRequestToWPM(AWorkerMachine: TTargetMachine): string;
var
  WPMMachineAddress: string;
begin
  WPMMachineAddress := GetWPMMachineAddress;
  if WPMMachineAddress = '' then  //if sending an HTTP request with an empty address, then an exception occurs: 'A Host is required'
  begin
    Result := 'No WPM machine found in the list. This has to be added before all the others.';
    Exit;
  end;

  Result := SendTextRequestToServer('http://' + GetWPMMachineAddress + ':' + CWPMPort + '/' + CMachineOnline + '?' +
                                    CWorkerMachineAddress + '=' + AWorkerMachine.MachineAddress + '&' +
                                    CMachineOSParam + '=' + CWinParam + '&' + //Win only for now.
                                    CDistPluginMachineAddress + '=' + AWorkerMachine.PairedDistMachineAddress
                                    , False);
end;


function SendRemoveWorkerMachineRequestToWPM(AWorkerMachineAddress: string): string;
var
  WPMMachineAddress: string;
begin
  WPMMachineAddress := GetWPMMachineAddress;
  if WPMMachineAddress = '' then  //if sending an HTTP request with an empty address, then an exception occurs: 'A Host is required'
  begin
    Result := 'No WPM machine found in the list. This has to be added before all the others.';
    Exit;
  end;

  Result := SendTextRequestToServer('http://' + GetWPMMachineAddress + ':' + CWPMPort + '/' + CRemoveWorkerMachine + '?' +
                                    CWorkerMachineAddress + '=' + AWorkerMachineAddress
                                    , False);
end;


procedure ExecuteFSM_Part1(var ATargetMachine: TTargetMachine);
var
  PairingResult: string;
begin
  case ATargetMachine.State of
    SInit:
    begin
      ATargetMachine.MonitoringUIClickerIsRunning := False;
      ATargetMachine.ServiceUIClickerIsRunning := False;
      ATargetMachine.ToolIsRunning := False;
    end;

    SCheckForMonitoringUIClicker:      //DistVisor expects that ServiceUIClicker is started automatically by the OS or a startup app/script.
    begin
      ATargetMachine.MonitoringUIClickerIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.MonitoringUIClickerPort + '/', False) = CREResp_ConnectionOK;
      if not ATargetMachine.MonitoringUIClickerIsRunning then
        WriteLn('The MonitoringUIClicker is not running. Attempting to start it.');
    end;

    SStartMonitoringUIClicker:
    begin
      ATargetMachine.MonitoringUIClicker_tk := GetTickCount64;
      ATargetMachine.StartMonitoringUIClickerResult := StartMonitoringUIClicker(ATargetMachine.MachineAddress, ATargetMachine.ServiceUIClickerPort, ATargetMachine.MonitoringUIClickerPort); //sends a command to the Service UIClicker, to start the Monitoring one.
    end;

    SWaitForMonitoringUIClicker:
      ATargetMachine.MonitoringUIClickerIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.MonitoringUIClickerPort + '/', False) = CREResp_ConnectionOK;

    SCheckForServiceUIClicker:
    begin
      ATargetMachine.ServiceUIClickerIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.ServiceUIClickerPort + '/', False) = CREResp_ConnectionOK;
      if not ATargetMachine.ServiceUIClickerIsRunning then
        WriteLn('The ServiceUIClicker is not running. Attempting to start it. MachineKind: ' + CMachineKindStr[ATargetMachine.MachineKind] + '.');
    end;

    SStartServiceUIClicker:
    begin
      ATargetMachine.ServiceUIClicker_tk := GetTickCount64;
      ATargetMachine.StartServiceUIClickerResult := StartServiceUIClicker(ATargetMachine.MachineAddress, ATargetMachine.ServiceUIClickerPort, ATargetMachine.MonitoringUIClickerPort); //sends a command to the Monitoring UIClicker, to start the Service one.
    end;

    SWaitForServiceUIClicker:
      ATargetMachine.ServiceUIClickerIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.ServiceUIClickerPort + '/', False) = CREResp_ConnectionOK;

    SCheckForDistUIClicker:
      if ATargetMachine.MachineKind = mkDist then
      begin
        ATargetMachine.ToolIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.ToolPort + '/', False) = CREResp_ConnectionOK;
        if not ATargetMachine.ToolIsRunning then
          WriteLn('The DistUIClicker is not running. Attempting to start it.');
      end;

    SStartDistUIClicker:
    begin
      ATargetMachine.Tool_tk := GetTickCount64;
      ATargetMachine.StartToolResult := StartDistUIClicker(ATargetMachine.MachineAddress, ATargetMachine.ServiceUIClickerPort, ATargetMachine.ToolPort); //sends a command to the Service UIClicker, to start the Dist UIClicker
    end;

    SWaitForDistUIClicker:
      ATargetMachine.ToolIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + ATargetMachine.ToolPort + '/', False) = CREResp_ConnectionOK;

    SCheckForWPM:
      if ATargetMachine.MachineKind = mkWPM then
      begin
        ATargetMachine.ToolIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + CWPMPort + '/', False) = CREResp_ConnectionOK;
        if not ATargetMachine.ToolIsRunning then
          WriteLn('The WorkerPoolManager is not running. Attempting to start it.');
      end;

    SStartWPM:
    begin
      ATargetMachine.Tool_tk := GetTickCount64;
      ATargetMachine.StartToolResult := StartWorkerPoolManager(ATargetMachine.MachineAddress, ATargetMachine.ServiceUIClickerPort); //sends a command to the Service UIClicker, to start the WPM.
    end;

    SWaitForWPM:
      ATargetMachine.ToolIsRunning := TestConnection('http://' + ATargetMachine.MachineAddress + ':' + CWPMPort + '/', False) = CREResp_ConnectionOK;

    SSendServicePlugins:
      if ATargetMachine.MachineKind in [mkWorker, mkDist] then
      begin
        SendAllServicePlugins(ATargetMachine.MachineAddress, ATargetMachine.ServiceUIClickerPort, ATargetMachine.ServiceUIClickerBitness);
        //This plugin is not needed on the mkDist machine, but when testin and having all tools running on the same machine,
        //then there is only one instance of the ServiceUIClicker (usually started for Dist).
        //When verifying again for the mkWorker machine, the Service UIClicker is already running, so it wouldn't get the plugin.
      end;

    SPairWithDist:  //entered after SSendServicePlugins, on a mkWorker machine
      if ATargetMachine.MachineKind = mkWorker then
      begin
        //In case of an error message here, it should be reported somehow to the caller (the one which initiated the AddMachine request).
        PairingResult := SendPairingRequestToWPM(ATargetMachine);
        WriteLn('Pairing an mkWorker machine (' + ATargetMachine.MachineAddress + ') to an mkDist machine (' + ATargetMachine.PairedDistMachineAddress + '): ' + PairingResult);
      end;

    SSendDistPlugins:
      if ATargetMachine.MachineKind = mkDist then
      begin
        SendAllDistPlugins(ATargetMachine.MachineAddress, ATargetMachine.ToolPort, ATargetMachine.DistUIClickerBitness);
        //Currently, WPM implements an HTTP command for calling SendPoolCredentials, but it seems that is can call this automatically.
      end;
  end;
end;


procedure ExecuteFSM_Part2(var ATargetMachine: TTargetMachine);
begin
  case ATargetMachine.State of
    SInit:
      ATargetMachine.NextState := SCheckForMonitoringUIClicker;

    SCheckForMonitoringUIClicker:
      if ATargetMachine.MonitoringUIClickerIsRunning then
        ATargetMachine.NextState := SCheckForServiceUIClicker  //next tool
      else
        ATargetMachine.NextState := SStartMonitoringUIClicker;      //maybe report an error if entering here too often

    SStartMonitoringUIClicker:
    begin
      if ATargetMachine.StartMonitoringUIClickerResult = '' then
        ATargetMachine.NextState := SWaitForMonitoringUIClicker
      else
      begin
        WriteLn('Error starting MonitoringUIClicker: "' + ATargetMachine.StartMonitoringUIClickerResult + '".');
        ATargetMachine.NextState := SCheckForServiceUIClicker;
      end;
    end;

    SWaitForMonitoringUIClicker:
    begin
      if ATargetMachine.MonitoringUIClickerIsRunning then
        ATargetMachine.NextState := SCheckForServiceUIClicker
      else
        if ATargetMachine.MonitoringUIClicker_tk < 10000 then
        begin
          ATargetMachine.NextState := SWaitForMonitoringUIClicker;
          WriteLn('Error: Timeout (' + IntToStr(ATargetMachine.MonitoringUIClicker_tk) + ' ms) waiting for MonitoringUIClicker to become available.');
        end
        else
          ATargetMachine.NextState := SCheckForServiceUIClicker;
    end;

    SCheckForServiceUIClicker:
      if ATargetMachine.ServiceUIClickerIsRunning then
        ATargetMachine.NextState := SCheckForDistUIClicker  //next tool
      else
        ATargetMachine.NextState := SStartServiceUIClicker;      //maybe report an error if entering here too often

    SStartServiceUIClicker:
    begin
      if ATargetMachine.StartServiceUIClickerResult = '' then
        ATargetMachine.NextState := SWaitForServiceUIClicker
      else
      begin
        WriteLn('Error starting ServiceUIClicker: "' + ATargetMachine.StartServiceUIClickerResult + '".');
        ATargetMachine.NextState := SCheckForDistUIClicker;
      end;
    end;

    SWaitForServiceUIClicker:
    begin
      if ATargetMachine.ServiceUIClickerIsRunning then
        ATargetMachine.NextState := SSendServicePlugins   //send plugins if running
      else
        if ATargetMachine.ServiceUIClicker_tk < 10000 then
        begin
          ATargetMachine.NextState := SWaitForServiceUIClicker;
          WriteLn('Error: Timeout (' + IntToStr(ATargetMachine.ServiceUIClicker_tk) + ' ms) waiting for ServiceUIClicker to become available.');
        end
        else
          ATargetMachine.NextState := SCheckForDistUIClicker;
    end;

    SCheckForDistUIClicker:
      if ATargetMachine.ToolIsRunning then
        ATargetMachine.NextState := SCheckForWPM  //next tool
      else
        if ATargetMachine.MachineKind = mkDist then
          ATargetMachine.NextState := SStartDistUIClicker      //maybe report an error if entering here too often
        else
          ATargetMachine.NextState := SCheckForWPM;

    SStartDistUIClicker:
    begin
      if ATargetMachine.StartToolResult = '' then
        ATargetMachine.NextState := SWaitForDistUIClicker
      else
      begin
        WriteLn('Error starting DistUIClicker: "' + ATargetMachine.StartToolResult + '".');
        ATargetMachine.NextState := SCheckForWPM;
      end;
    end;

    SWaitForDistUIClicker:
    begin
      if ATargetMachine.ToolIsRunning then
        ATargetMachine.NextState := SSendDistPlugins //send plugins if running
      else
        if ATargetMachine.Tool_tk < 10000 then
        begin
          ATargetMachine.NextState := SWaitForDistUIClicker;
          WriteLn('Error: Timeout (' + IntToStr(ATargetMachine.Tool_tk) + ' ms) waiting for DistUIClicker to become available.');
        end
        else
          ATargetMachine.NextState := SCheckForWPM;
    end;

    SCheckForWPM:
      if ATargetMachine.ToolIsRunning then
        ATargetMachine.NextState := SCheckForMonitoringUIClicker  //next tool
      else
        if ATargetMachine.MachineKind = mkWPM then
          ATargetMachine.NextState := SStartWPM      //maybe report an error if entering here too often
        else
          ATargetMachine.NextState := SCheckForMonitoringUIClicker;

    SStartWPM:
    begin
      if ATargetMachine.StartToolResult = '' then
        ATargetMachine.NextState := SWaitForWPM
      else
      begin
        WriteLn('Error starting WPM: "' + ATargetMachine.StartToolResult + '".');
        ATargetMachine.NextState := SCheckForMonitoringUIClicker;
      end;
    end;

    SWaitForWPM:
    begin
      if ATargetMachine.ToolIsRunning then
        ATargetMachine.NextState := SCheckForMonitoringUIClicker
      else
        if ATargetMachine.Tool_tk < 10000 then
        begin
          ATargetMachine.NextState := SWaitForWPM;
          WriteLn('Error: Timeout (' + IntToStr(ATargetMachine.Tool_tk) + ' ms) waiting for WorkerPoolManager to become available.');
        end
        else
          ATargetMachine.NextState := SCheckForMonitoringUIClicker;
    end;               //somewhere, enter AutosendPluginsOnStartup

    SSendServicePlugins:
      ATargetMachine.NextState := SPairWithDist;

    SPairWithDist:
      ATargetMachine.NextState := SCheckForDistUIClicker;

    SSendDistPlugins:
     ATargetMachine.NextState := SCheckForWPM;
  end;
end;


procedure ExecuteFSM;
var
  i: Integer;
begin
  EnterCriticalSection(CritSec);
  try
    for i := 0 to Length(Machines) - 1 do
    begin
      ExecuteFSM_Part1(Machines[i]);           //if there are requests, which take a long time to respond, then this affects the server's response time
      ExecuteFSM_Part2(Machines[i]);
      Machines[i].State := Machines[i].NextState;
    end;
  finally
    LeaveCriticalSection(CritSec);
  end;
end;


initialization
  InitCriticalSection(CritSec);
  SetLength(Machines, 0);

finalization
  DoneCriticalSection(CritSec);  //by now, the server module should be destroyed
  SetLength(Machines, 0);

end.

