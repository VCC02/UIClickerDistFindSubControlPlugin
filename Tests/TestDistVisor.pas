{
    Copyright (C) 2025 VCC
    creation date: 14 Dec 2025
    initial release date: 14 Dec 2025

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


unit TestDistVisor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AsyncProcess, TestDistPlugin, testregistry,
  IdIPWatch;

type

  TTestDistVisor_Resources = class(TTestDistPlugin)
  private
    function StartDistVisor: TAsyncProcess;
    function StartMonitoringUIClicker: TAsyncProcess;
    procedure AddMachineToDistVisor(AMachineKind, AMachineAddress, AUserID: string);
    procedure ExpectStatusFromMachine(AMachineKind, AMachineAddress, AUserId, AExpectedStatus: string; ATimeout: Integer = 70000);
    procedure WaitForMkWorkerServiceUIClicker(AServiceUIClickerAddressToWaitFor: string);

  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_HappyFlow;

    procedure AfterAll_AlwaysExecute;
  end;


implementation


uses
  UITestUtils, ClickerActionsClient, ClickerUtils, ClickerActionProperties,
  DistVisorCommands, DistVisorFSM,
  Expectations, Forms, PitstopTestRunner, TestHTTPAPI;


var
  DistVisor_Proc: TAsyncProcess;
  MonitoringUIClicker_Proc: TAsyncProcess;

const
  CTestDistVisorUserID = '123abc';

var
  FMachineKind: string;
  FMachineAddress: string;
  FUserId: string;

  FIdIPWatch: TIdIPWatch;
  FLocalIP: string;

function GetAppsStatusCallback: string;
var
  Link: string;
  i: Integer;
begin
  Link := 'http://127.0.0.1:54000/' + CDVCmd_GetMachineStatus +  //127.0.0.1 is the address of DistVisor
          '?' + CDVCmdMachineKindParam + '=' + FMachineKind +
          '&' + CDVCmdMachineAddressParam + '=' + FMachineAddress +
          '&' + CDVCmdUserIDParam + '=' + FUserId;

  Result := SendTextRequestToServer(Link);

  for i := 1 to 40 do
  begin
    Sleep(25);
    Application.ProcessMessages;
  end;
end;


procedure TTestDistVisor_Resources.ExpectStatusFromMachine(AMachineKind, AMachineAddress, AUserId, AExpectedStatus: string; ATimeout: Integer = 70000);
begin
  FMachineAddress := AMachineAddress;
  FMachineKind := AMachineKind;
  FUserId := AUserId;
  LoopedExpect(@GetAppsStatusCallback, ATimeout).ToBe(AExpectedStatus, 'AMachineKind: ' + AMachineKind + '  AMachineAddress: ' + AMachineAddress);
end;


procedure TTestDistVisor_Resources.AddMachineToDistVisor(AMachineKind, AMachineAddress, AUserID: string);
var
  Link: string;
begin
  Link := 'http://127.0.0.1:54000/' + CDVCmd_AddMachine +  //127.0.0.1 is the address of DistVisor
          '?' + CDVCmdMachineKindParam + '=' + AMachineKind +
          '&' + CDVCmdMachineAddressParam + '=' + AMachineAddress +
          '&' + CDVCmdMonitoringUIClickerPortParam + '=' + CDefaultMonitoringPort +
          '&' + CDVCmdServiceUIClickerPortParam + '=' + CDefaultServiceUIClickerPort +
          '&' + CDVCmdToolPortParam + '=' + CDefaultDistUIClickerPort +
          '&' + CDVCmdUserIDParam + '=' + AUserId;

  Expect(SendTextRequestToServer(Link)).ToBe('Done', 'when adding machine to DistVisor, at address: ' + AMachineAddress + ', and kind: ' + AMachineKind + '.');
end;


function TTestDistVisor_Resources.StartDistVisor: TAsyncProcess;
var
  PathToDistVisor: string;
begin
  PathToDistVisor := ExtractFilePath(ParamStr(0)) + '..\DistVisor\DistVisor.exe';
  Result := CreateUIClickerProcess(PathToDistVisor, '');
  Sleep(500);
end;


function TTestDistVisor_Resources.StartMonitoringUIClicker: TAsyncProcess;
var
  PathToDistVisor: string;
begin
  PathToDistVisor := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker\UIClicker.exe';
  Result := CreateUIClickerProcess(PathToDistVisor, '--SetExecMode Server --AutoSwitchToExecTab Yes --ServerPort ' + CDefaultMonitoringPort + ' ' + '--ExtraCaption ' + CMonitoringExtraCaption + ' --AddAppArgsToLog Yes --SkipSavingSettings Yes');
  Sleep(500);
end;


procedure TTestDistVisor_Resources.BeforeAll_AlwaysExecute;
var
  tk: QWord;
begin
  //inherited BeforeAll_AlwaysExecute;
  FIdIPWatch := TIdIPWatch.Create;
  FIdIPWatch.HistoryEnabled := False;
  FIdIPWatch.Active := True;
  FIdIPWatch.WatchInterval := 400;

  DistVisor_Proc := nil;
  MonitoringUIClicker_Proc := nil;

  tk := GetTickCount64;
  repeat
    Application.ProcessMessages;

    FLocalIP := FIdIPWatch.LocalIP;
    if FLocalIP <> '' then
      Break;
  until GetTickCount64 - tk > 2000;

  Expect(FLocalIP > '').ToBe(True, 'A valid local IP address has to exist for this test batch. Please enable a network adapter.');
  frmPitstopTestRunner.AddToLog('Available IP address: ' + FLocalIP);

  DistVisor_Proc := StartDistVisor;
  MonitoringUIClicker_Proc := StartMonitoringUIClicker;
end;


procedure TTestDistVisor_Resources.WaitForMkWorkerServiceUIClicker(AServiceUIClickerAddressToWaitFor: string);
var
  tk: QWord;
  FindControlOptions: TClkFindControlOptions;
  WindowOperationsOptions: TClkWindowOperationsOptions;
  AttemptCount: Integer;
begin
  frmPitstopTestRunner.AddToLog('Waiting for mkWorker ServiceUIClicker...');
  Application.ProcessMessages;

  GetDefaultPropertyValues_FindControl(FindControlOptions);
  FindControlOptions.MatchClassName := 'Window';
  FindControlOptions.MatchCriteria.SearchForControlMode := sfcmFindWindow;

  GetDefaultPropertyValues_WindowOperations(WindowOperationsOptions);
  WindowOperationsOptions.Operation := woClose;

  Application.ProcessMessages;
  tk := GetTickCount64;
  AttemptCount := 0;
  repeat
    try
      FindControlOptions.MatchText := 'UI Clicker Main - ' + CServiceExtraCaption + '_' + CMachineKindStr[mkDist];
      ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindControlAction('http://127.0.0.1:' + CDefaultMonitoringPort + '/', FindControlOptions, 'Find mkDist', 500, CREParam_FileLocation_ValueDisk)));
      ExecuteWindowOperationsAction('http://127.0.0.1:' + CDefaultMonitoringPort + '/', WindowOperationsOptions);
      frmPitstopTestRunner.AddToLog('Closed ' + FindControlOptions.MatchText);
    except
      //if the UIClicker is not running, then this action fails
      frmPitstopTestRunner.AddToLog(FindControlOptions.MatchText + ' not found.');
    end;

    try
      FindControlOptions.MatchText := 'UI Clicker Main - ' + CServiceExtraCaption + '_' + CMachineKindStr[mkWPM];
      ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteFindControlAction('http://127.0.0.1:' + CDefaultMonitoringPort + '/', FindControlOptions, 'Find mkWPM', 500, CREParam_FileLocation_ValueDisk)));
      ExecuteWindowOperationsAction('http://127.0.0.1:' + CDefaultMonitoringPort + '/', WindowOperationsOptions);
      frmPitstopTestRunner.AddToLog('Closed ' + FindControlOptions.MatchText);
    except
      //if the UIClicker is not running, then this action fails
      frmPitstopTestRunner.AddToLog(FindControlOptions.MatchText + ' not found.');
    end;

    try
      ExpectStatusFromMachine('1', AServiceUIClickerAddressToWaitFor, CTestDistVisorUserID, CMachineStatusResponseParam + '=' + CMachineStatus_Online, 500);  //mkWorker    - using a small timeout
      frmPitstopTestRunner.AddToLog('The mkWorker service UIClicker was finally found. Exiting..');
      Break;
    except
      Inc(AttemptCount);
      frmPitstopTestRunner.AddToLog('The service UIClicker was not found. AttemptCount = ' + IntToStr(AttemptCount));
    end;

    Application.ProcessMessages;
    Sleep(100);
  until GetTickCount64 - tk > 3 * 60000; //Waiting 3min, because WPM has to start multiple workers. DistVisor waits for all of those.
end;


procedure TTestDistVisor_Resources.Test_HappyFlow;
begin
  frmPitstopTestRunner.AddToLog('Starting WPM...');
  AddMachineToDistVisor('2', '127.0.0.1', '');   //WPM  has to be added without a user ID
  frmPitstopTestRunner.AddToLog('Waiting for WPM...');
  Application.ProcessMessages;
  ExpectStatusFromMachine('2', '127.0.0.1', '', CMachineStatusResponseParam + '=' + CMachineStatus_Online);
  frmPitstopTestRunner.AddToLog('WPM is running.');

  frmPitstopTestRunner.AddToLog('Starting Dist UIClicker...');
  AddMachineToDistVisor('0', '127.0.0.1', CTestDistVisorUserID);  //Dist
  frmPitstopTestRunner.AddToLog('Waiting for Dist UIClicker...');
  Application.ProcessMessages;
  ExpectStatusFromMachine('0', '127.0.0.1', CTestDistVisorUserID, CMachineStatusResponseParam + '=' + CMachineStatus_Online);
  frmPitstopTestRunner.AddToLog('Dist UIClicker is running.');

  //At this point, the running Service UIClicker should belong to the WPM instance of the visor FSM.
  //It has to be closed. Also, another instance, belonging to Dist, will be started. This also has to be closed.
  //The remaining instance is the Worker one, which should be left running, so it can enter the "SSendServicePlugins" and "SPairWithDist" states.
  frmPitstopTestRunner.AddToLog('Starting Service UIClicker on ' + FLocalIP + '...');
  AddMachineToDistVisor('1', FLocalIP, CTestDistVisorUserID);  //worker
  WaitForMkWorkerServiceUIClicker(FLocalIP);

  //verify again, to fail the test if the Service UIClicker instance is not running
  frmPitstopTestRunner.AddToLog('Verifying again if the expected Service UIClicker is running...');
  Application.ProcessMessages;
  ExpectStatusFromMachine('1', FLocalIP, CTestDistVisorUserID, CMachineStatusResponseParam + '=' + CMachineStatus_Online, 1000);  //mkWorker    - using a small timeout
end;


procedure TTestDistVisor_Resources.AfterAll_AlwaysExecute;
begin
  if DistVisor_Proc <> nil then
  begin
    DistVisor_Proc.Terminate(0);
    DistVisor_Proc.Free;
  end;

  if MonitoringUIClicker_Proc <> nil then
  begin
    MonitoringUIClicker_Proc.Terminate(0);
    MonitoringUIClicker_Proc.Free;
  end;

  FIdIPWatch.Free;
  //inherited AfterAll_AlwaysExecute;
end;


initialization
  FIdIPWatch := nil;
  FLocalIP := 'not set';

  RegisterTest(TTestDistVisor_Resources);

end.

