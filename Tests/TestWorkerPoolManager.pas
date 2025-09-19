{
    Copyright (C) 2025 VCC
    creation date: 19 Sep 2025
    initial release date: 19 Sep 2025

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


unit TestWorkerPoolManager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AsyncProcess, TestDistPlugin, testregistry;

type
  TTestWorkerPoolManager = class(TTestDistPlugin)
  protected
    procedure StartMainUIClickerInstances; //overload;
    function StartWorkerPoolManager: TAsyncProcess;
    procedure ArrangeMainUIClickerWindowsForWorkerPoolManager;
    procedure SetDistUIClickerPortOnWorkerPoolManager;
  public
    constructor Create; override;
    procedure BeforeAll;
    procedure AfterAll;

  published
    procedure BeforeAll_AlwaysExecute;

    procedure Test_AddSelfMachineToList;
    procedure Test_AddTwoDifferentMachinesToList;

    procedure AfterAll_AlwaysExecute;
  end;


implementation


uses
  Forms, ClickerActionsClient, UITestUtils, Expectations, WorkerPoolCommonConsts;

const
  CServiceUIClickerPort = '55444';

var
  FTestDriverForClient_Proc: TAsyncProcess;
  FServiceUIClicker_Proc: TAsyncProcess;
  FClientAppUnderTest_Proc: TAsyncProcess;
  CommonFonts_Proc: TAsyncProcess;
  WorkerPoolManager_Proc: TAsyncProcess;


constructor TTestWorkerPoolManager.Create;
begin
  inherited Create;  //this will set FIsWine

end;


procedure TTestWorkerPoolManager.StartMainUIClickerInstances;
begin
  FTestDriverForClient_Proc := StartDriverUIClicker;
  FClientAppUnderTest_Proc := StartMainUIClicker('Server', CClientUnderTestServerPort, 'ClientUnderTest');  //'Server', '5444', 'Dist'  //The string 'ClientUnderTest' may be used in driver templates
  FServiceUIClicker_Proc := StartMainUIClicker('Server', CServiceUIClickerPort, 'Service');
end;


function TTestWorkerPoolManager.StartWorkerPoolManager: TAsyncProcess;
var
  PathToWorkerPoolManager: string;
begin
  PathToWorkerPoolManager := ExtractFilePath(ParamStr(0)) + '..\WorkerPoolManager\WorkerPoolManager.exe';
  Result := CreateUIClickerProcess(PathToWorkerPoolManager, '');
  Sleep(500);
end;


procedure TTestWorkerPoolManager.ArrangeMainUIClickerWindowsForWorkerPoolManager;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\ArrangeMainUIClickerWindowsForWorkerPoolManager.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.SetDistUIClickerPortOnWorkerPoolManager;
begin
  SetVariable(CTestDriverServerAddress_Client, '$DistPort$', CClientUnderTestServerPort, 0);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\SetDistUIClickerPortOnWorkerPoolManager.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.BeforeAll;
begin
  StartMainUIClickerInstances;
  CommonFonts_Proc := StartTestUtilities;
  WorkerPoolManager_Proc := StartWorkerPoolManager;

  WaitForDriverStartup;

  try
    if IsWine then
    begin
      GeneralConnectTimeout := 10000;
      SetVariableOnTestDriverClient('$IsAdminOnWine$', '  [Is admin]');
      Application.MainForm.Caption := Application.MainForm.Caption  + '  $IsAdminOnWine$';
    end
    else
      SetVariableOnTestDriverClient('$IsAdminOnWine$', ''); // a single #13#10 results in an empty string item in a TStringList. Still, better send '', to allow the expectation to match ''. UIClicker should convert this one '', into a new line.
  except
    on E: Exception do
      raise Exception.Create('Please verify if UIClicker is built for testing (including the test driver). ' + E.Message);
  end;

  ArrangeMainUIClickerWindowsForWorkerPoolManager;;      //Setting window position from ini file, works on Wine. Setting from UIClicker does not (yet).
  Sleep(500);                       //these sleep calls should be replaced by some waiting loops
  ArrangeUIClickerActionWindows;
  Sleep(500);
  SetDistUIClickerPortOnWorkerPoolManager;
  Sleep(500);
  //ArrangeWorkerWindows;   //Move to a test, then uncomment after modifying ArrangeWorkerWindows.clktmpl, to accept different window captions, or implement this feature in WorkerPoolManager.
  //Sleep(500);

  TemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker\TestDriver\ActionTemplates\';

  PrepareClickerUnderTestToReadItsVars; //or write..
  SetVariableOnClickerUnderTest('$TestFilesDir$', ExpandFileName(ExtractFilePath(ParamStr(0)) + 'TestFiles'));
end;


procedure TTestWorkerPoolManager.AfterAll;
begin
  //the following instances should be terminated in this specific order:
  FClientAppUnderTest_Proc.Terminate(0);
  FTestDriverForClient_Proc.Terminate(0);
  FServiceUIClicker_Proc.Terminate(0);
  WorkerPoolManager_Proc.Terminate(0);

  CommonFonts_Proc.Terminate(0);

  FreeAndNil(FClientAppUnderTest_Proc);
  FreeAndNil(FTestDriverForClient_Proc);
  FreeAndNil(FServiceUIClicker_Proc);
  FreeAndNil(WorkerPoolManager_Proc);

  FreeAndNil(CommonFonts_Proc);
end;


procedure TTestWorkerPoolManager.BeforeAll_AlwaysExecute;
begin
  BeforeAll;
end;


procedure TTestWorkerPoolManager.Test_AddSelfMachineToList;
var
  Link: string;
begin
  //Send a request to WorkerPoolManager, to add this machine to list
  Link := 'http://127.0.0.1:11884/' + CMachineOnline +
          '?' + CWorkerMachineAddress + '=' + '127.0.0.1' +
          '&' + CMachineOSParam + '=' + CWinParam +  //start workers on Win
          '&' + CDistPluginMachineAddress + '=' + '127.0.0.1';

  Expect(SendTextRequestToServer(Link)).ToBe('127.0.0.1');
  //this will have to get the process IDs of all workers and worker-UIClickers and stop them when done
end;


procedure TTestWorkerPoolManager.Test_AddTwoDifferentMachinesToList;
var
  Link: string;
begin                                               //maybe it should verify if there are already the following machines, then remove them
  //Send a request to WorkerPoolManager, to add this machine to list
  Link := 'http://127.0.0.1:11884/' + CMachineOnline +
          '?' + CWorkerMachineAddress + '=' + '127.0.0.1' +
          '&' + CMachineOSParam + '=' + CWinParam +  //start workers on Win
          '&' + CDistPluginMachineAddress + '=' + '127.0.0.1';

  Expect(SendTextRequestToServer(Link)).ToBe('127.0.0.1');

  ////////////////////// ToDo: set broker count / machine to 2
  Link := 'http://127.0.0.1:11884/' + CMachineOnline +
          '?' + CWorkerMachineAddress + '=' + '127.0.0.1' +
          '&' + CMachineOSParam + '=' + CWinParam +  //start workers on Win
          '&' + CDistPluginMachineAddress + '=' + '192.168.1.100';

  Expect(SendTextRequestToServer(Link)).ToBe(CMachineSet);   //'192.168.1.100' ?

  //this will have to get the process IDs of all workers and worker-UIClickers and stop them when done
end;


procedure TTestWorkerPoolManager.AfterAll_AlwaysExecute;
begin
  AfterAll;
end;


initialization

  RegisterTest(TTestWorkerPoolManager);

end.

