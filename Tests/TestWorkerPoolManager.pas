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
    procedure SetBrokerCountOnWorkerPoolManager(ACount: Integer);
    procedure SetWorkerCountOnWorkerPoolManager(AWinCount, ALinCount: Integer);
    procedure CloseAllWorkers;
    procedure CloseAllWorkerUIClickers;

    procedure AddMachineToList(AWorkerMachineAddress, ADistUIClickerMachineAddress, AExpectedResponse: string);
    procedure RemoveMachineFromList(AWorkerMachineAddress: string);

    procedure ExpectAppsStatusFromMachine(AWorkerMachineAddress, AExpectedStatus: string; ATimeout: Integer = 100000);
  public
    constructor Create; override;
    procedure BeforeAll;
    procedure AfterAll;

  //published
    procedure BeforeAll_AlwaysExecute; virtual;
    procedure AfterAll_AlwaysExecute; virtual;
  end;


  TTestWorkerPoolManager_Resources = class(TTestWorkerPoolManager)
  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AddSelfMachineToList;
    procedure Test_AddTwoDifferentMachinesToList;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestWorkerPoolManager_DistFindSubControl = class(TTestWorkerPoolManager)
  private
    procedure SetEvaluateFileNameBeforeSendingInPluginAction(AEvaluateFileNameBeforeSending: Boolean);
  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_HappyFlow_DoNotEvaluateFileNameBeforeSending;
    procedure Test_HappyFlow_EvaluateFileNameBeforeSending;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestWorkerPoolManager_DistFindSubControl_NoWorker = class(TTestWorkerPoolManager)
  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_EnsureTheDistPluginStopsExecutionOnBadCredentials;

    procedure AfterAll_AlwaysExecute; override;
  end;


implementation


uses
  Forms, ClickerActionsClient, UITestUtils, Expectations, WorkerPoolCommonConsts,
  ClickerUtils, ClickerActionProperties;

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


procedure TTestWorkerPoolManager.SetBrokerCountOnWorkerPoolManager(ACount: Integer);
begin
  SetVariable(CTestDriverServerAddress_Client, '$BrokerCountPerMachine$', IntToStr(ACount), 0);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\SetBrokerCountOnWorkerPoolManager.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.SetWorkerCountOnWorkerPoolManager(AWinCount, ALinCount: Integer);
begin
  SetVariable(CTestDriverServerAddress_Client, '$Win_WorkerCountPerMachine$', IntToStr(AWinCount), 0);
  SetVariable(CTestDriverServerAddress_Client, '$Lin_WorkerCountPerMachine$', IntToStr(ALinCount), 0);
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\SetWorkerCountOnWorkerPoolManager.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.CloseAllWorkers;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\CloseAllWorkers.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.CloseAllWorkerUIClickers;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\CloseAllWorkerUIClickers.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestWorkerPoolManager.AddMachineToList(AWorkerMachineAddress, ADistUIClickerMachineAddress, AExpectedResponse: string);
var
  Link: string;
begin
  Link := 'http://127.0.0.1:11884/' + CMachineOnline +  //127.0.0.1 is the address of WorkerPoolManager
          '?' + CWorkerMachineAddress + '=' + AWorkerMachineAddress +
          '&' + CMachineOSParam + '=' + CWinParam +  //start workers on Win
          '&' + CDistPluginMachineAddress + '=' + ADistUIClickerMachineAddress;

  Expect(SendTextRequestToServer(Link)).ToBe(AExpectedResponse);
end;


procedure TTestWorkerPoolManager.RemoveMachineFromList(AWorkerMachineAddress: string);
var
  Link, Response: string;
begin
  Link := 'http://127.0.0.1:11884/' + CRemoveWorkerMachine +  //127.0.0.1 is the address of WorkerPoolManager
          '?' + CWorkerMachineAddress + '=' + AWorkerMachineAddress;

  Response := SendTextRequestToServer(Link);

  try
    Expect(Response).ToBe(CMachineRemoved);
  except
    Expect(Response).ToBe(CWorkerMachineNotFound);
  end;
end;


var
  FWorkerMachineAddress: string;

function GetAppsStatusCallback: string;
var
  Link: string;
  i: Integer;
begin
  Link := 'http://127.0.0.1:11884/' + CGetAppsStatus +  //127.0.0.1 is the address of WorkerPoolManager
          '?' + CWorkerMachineAddress + '=' + FWorkerMachineAddress;

  Result := SendTextRequestToServer(Link);

  for i := 1 to 40 do
  begin
    Sleep(25);
    Application.ProcessMessages;
  end;
end;


procedure TTestWorkerPoolManager.ExpectAppsStatusFromMachine(AWorkerMachineAddress, AExpectedStatus: string; ATimeout: Integer = 100000);
begin
  FWorkerMachineAddress := AWorkerMachineAddress;
  LoopedExpect(@GetAppsStatusCallback, ATimeout).ToBe(AExpectedStatus);
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

  ArrangeMainUIClickerWindowsForWorkerPoolManager;      //Setting window position from ini file, works on Wine. Setting from UIClicker does not (yet).
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


procedure TTestWorkerPoolManager.AfterAll_AlwaysExecute;
begin
  AfterAll;
end;


procedure TTestWorkerPoolManager_Resources.BeforeAll_AlwaysExecute;
begin
  inherited BeforeAll_AlwaysExecute;
end;


procedure TTestWorkerPoolManager_Resources.Test_AddSelfMachineToList;
begin
  SetBrokerCountOnWorkerPoolManager(1);
  SetWorkerCountOnWorkerPoolManager(4, 3);
  RemoveMachineFromList('127.0.0.1');

  AddMachineToList('127.0.0.1', '127.0.0.1', '127.0.0.1');
  ExpectAppsStatusFromMachine('127.0.0.1', CAllAppsRunning);

  CloseAllWorkers;
  CloseAllWorkerUIClickers;
end;


procedure TTestWorkerPoolManager_Resources.Test_AddTwoDifferentMachinesToList;
begin
  SetBrokerCountOnWorkerPoolManager(2);
  SetWorkerCountOnWorkerPoolManager(4, 3);
  RemoveMachineFromList('127.0.0.1');

  AddMachineToList('127.0.0.1', '127.0.0.1', '127.0.0.1');
  AddMachineToList('127.0.0.1', '192.168.1.100', CMachineSet);
  ExpectAppsStatusFromMachine('127.0.0.1', CAllAppsRunning, 130000);

  CloseAllWorkers;
  CloseAllWorkerUIClickers;
end;


procedure TTestWorkerPoolManager_Resources.AfterAll_AlwaysExecute;
begin
  inherited AfterAll_AlwaysExecute;
end;


function SendPlugin(ASenderApp, APluginToBeSent, APluginToBeSentDir, ATXKey, ATXIV, ANextKey, ANextIV: string; ADecryptionPluginName: string = ''): string;
var
  ExecApp: TClkExecAppOptions;
  DistPath: string;
  ExecResults: TStringList;
begin
  DistPath := ExtractFilePath(ParamStr(0)) + '..\';
  if (APluginToBeSentDir > '') and (APluginToBeSentDir[Length(APluginToBeSentDir)] <> '\') then
    APluginToBeSentDir := APluginToBeSentDir + '\';

  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := DistPath + ASenderApp + '\' + ASenderApp + '.exe';
  ExecApp.ListOfParams := '--ClickerClient' + #4#5 + DistPath + '..\UIClicker\ClickerClient\ClickerClient.dll' + #4#5 +
                          '--PluginToBeSent' + #4#5 + DistPath + APluginToBeSentDir + 'lib\i386-win32\' + APluginToBeSent + '.dll' + #4#5 +
                          '--PluginToBeSentDestName' + #4#5 + APluginToBeSent + '.dll' + #4#5 +
                          '--UIClickerAddress' + #4#5 + '127.0.0.1' + #4#5 +
                          '--UIClickerPort' + #4#5 + CClientUnderTestServerPort;

  if ADecryptionPluginName <> '' then
    ExecApp.ListOfParams := ExecApp.ListOfParams + #4#5 + '--DecryptionPluginName' + #4#5 + ADecryptionPluginName + '.dllarc|Mem:\' + ADecryptionPluginName + '.dll';

  ExecApp.WaitForApp := True;
  ExecApp.AppStdIn := ATXKey + #4#5 +      //This is provided when DistInitialEnc.exe prints "Trasmission Key" on StdOut.
                      ATXIV + #4#5 +       //This is provided when DistInitialEnc.exe prints "Trasmission IV" on StdOut.
                      ANextKey + #4#5 +    //This is provided when DistInitialEnc.exe prints "Subsequent Key" on StdOut.
                      ANextIV + #4#5;      //This is provided when DistInitialEnc.exe prints "Subsequent IV" on StdOut.
  ExecApp.CurrentDir := ExtractFileDir(ExecApp.PathToApp);
  ExecApp.UseInheritHandles := uihYes;
  ExecApp.VerifyFileExistence := True;
  ExecApp.WaitForApp := True;
  ExecApp.NoConsole := True;

  try
    Result := ExecuteExecAppAction('http://127.0.0.1' + ':' + CServiceUIClickerPort + '/', ExecApp, 'Send DistInitialDec', 5000, False);
  except
    on E: Exception do
      Result := 'Ex on starting broker: ' + E.Message;
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


var
  InitialTransmissionKey: string = 'dummy_key';
  InitialTrasmissionIV: string = 'dummy_iv';
  InitialSubsequentKey: string = 'ABCDEF';
  InitialSubsequentIV: string = 'IV.IV';
  DistDecSubsequentKey: string = 'ABCDEFDistDec';
  DistDecSubsequentIV: string = 'IVDistDec.IV';
  FindSubControlKey: string = 'ABCDEFFindSubControl';
  FindSubControlIV: string = 'IVFindSubControl';

function Send_DistInitialDecDll_Via_DistInitialEnc: string;
begin
  Result := SendPlugin('DistInitialEnc', 'DistInitialDec', 'DistInitialDec', InitialTransmissionKey, InitialTrasmissionIV, InitialSubsequentKey, InitialSubsequentIV);
end;


function Send_DistDecDll_Via_DistInitialEnc: string;
begin
  Result := SendPlugin('DistInitialEnc', 'DistDec', 'DistDec', InitialSubsequentKey, InitialSubsequentIV, DistDecSubsequentKey, DistDecSubsequentIV);
end;


function Send_UIClickerDistFindSubControlDll_Via_DistEnc: string;
begin
  Result := SendPlugin('DistEnc', 'UIClickerDistFindSubControl', '', DistDecSubsequentKey, DistDecSubsequentIV, FindSubControlKey, FindSubControlIV, 'DistDec');
end;


function Send_PoolClientDll_Via_DistEnc: string;
begin
  Result := SendPlugin('DistEnc', 'PoolClient', 'PoolClient', DistDecSubsequentKey, DistDecSubsequentIV, FindSubControlKey, FindSubControlIV, 'DistDec');
end;


procedure SendAllDistPlugins;
const
  CPrefix = '$ExecAction_StdOut$=Trasmission Key=Trasmission IV=Subsequent Key=Subsequent IV=';
  COperation = 'Sending plugin to http://127.0.0.1:' + CClientUnderTestServerPort + '/';
  CExpectedResponse_Unencrypted = CPrefix + 'Sending unencrypted...' + COperation + 'Response: OK';
  CExpectedResponse_Encrypted = CPrefix + COperation + 'Using archive encryption..Response: OK';
begin
  Expect(Send_DistInitialDecDll_Via_DistInitialEnc).ToBe(CExpectedResponse_Unencrypted);
  Expect(Send_DistDecDll_Via_DistInitialEnc).ToBe(CExpectedResponse_Encrypted);
  Expect(Send_UIClickerDistFindSubControlDll_Via_DistEnc).ToBe(CExpectedResponse_Encrypted);
  Expect(Send_PoolClientDll_Via_DistEnc).ToBe(CExpectedResponse_Encrypted);
end;


procedure SetKeys;
begin
  InitialTransmissionKey := 'A new key every time ' + IntToHex(GetTickCount64);
  Sleep(33);
  InitialTrasmissionIV := 'A new IV every time ' + IntToHex(GetTickCount64);
  Sleep(33);
  InitialSubsequentKey := 'A new subsequent key every time ' + IntToHex(GetTickCount64);
  Sleep(33);
  InitialSubsequentIV := 'A new subsequent IV every time ' + IntToHex(GetTickCount64);
end;


procedure TTestWorkerPoolManager_DistFindSubControl.SetEvaluateFileNameBeforeSendingInPluginAction(AEvaluateFileNameBeforeSending: Boolean);
var
  EditTemplateOptions: TClkEditTemplateOptions;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);

  EditTemplateOptions.Operation := etoSetProperty;
  EditTemplateOptions.WhichTemplate := etwtSelf;
  EditTemplateOptions.ListOfEditedProperties := 'FileName=$AppDir$\..\UIClickerDistFindSubControlPlugin\lib\$AppBitness$-$OSBitness$\UIClickerDistFindSubControl.dllListOfPropertiesAndValues=FindSubControlAction=CredentialsFullFileName=Address=127.0.0.1Port=1883WorkerQoS=1GetWorkerCapabilitiesTimeout=500FindSubControlWorkerTimeout=3000FindSubControlTimeoutDiff=2500WorkerCapabilitiesSource=wcsReqCapAndGetFontsAndFindSubControlLoadWorkerCapabilitiesCacheAction=SaveWorkerCapabilitiesCacheAction=TextRenderingOS=Win+LinListOfMultiValuePropertyNames=UseCompression=TrueCompressionAlgorithm=LzmaLzmaEndOfStream=FalseLzmaAlgorithm=2LzmaNumBenchMarkPasses=10LzmaDictionarySize=1048576LzmaMatchFinder=1LzmaLiteralContext=3LzmaLiteralPosBits=0LzmaPosBits=0LzmaFastBytes=5VariablesForWorkers=$Control_Handle$,$Control_Left$,$Control_Top$,$Control_Right$,$Control_Bottom$,$Control_Width$,$Control_Height$ExtraDebuggingInfo=TrueEvaluateFileNameBeforeSending=' + BoolToStr(AEvaluateFileNameBeforeSending, True) + '';
  EditTemplateOptions.ListOfEnabledProperties := 'EvaluateFileNameBeforeSending';
  EditTemplateOptions.EditedActionName := '"Plugin"';
  EditTemplateOptions.EditedActionType := acPlugin;

  PrepareClickerUnderTestToReadItsVars;
  //TestServerAddress := CClientUnderTestServerPort;
  //try
  //  //connect to ClientUnderTest instance, which is now running in server mode
    ExecuteEditTemplateAction(CTestClientAddress, EditTemplateOptions); //ExpectSuccessfulAction(FastReplace_87ToReturn(ExecuteEditTemplateAction(CTestClientAddress, EditTemplateOptions)));
  //finally
  //  TestServerAddress := CTestDriverServerAddress_Client; //restore
  //end;
end;


procedure TTestWorkerPoolManager_DistFindSubControl.BeforeAll_AlwaysExecute;
begin
  inherited BeforeAll_AlwaysExecute;

  SetBrokerCountOnWorkerPoolManager(1);
  SetWorkerCountOnWorkerPoolManager(4, 3);

  RemoveMachineFromList('127.0.0.1');
  AddMachineToList('127.0.0.1', '127.0.0.1', '127.0.0.1');
  ExpectAppsStatusFromMachine('127.0.0.1', CAllAppsRunning);

  SetKeys;
  SendAllDistPlugins;
end;


procedure TTestWorkerPoolManager_DistFindSubControl.Test_HappyFlow_DoNotEvaluateFileNameBeforeSending;
begin
  //SetEvaluateFileNameBeforeSendingInPluginAction(False); //this would require loading the template, executing SetEvaluate.. then running the template
  ExecutePluginTestTemplate_FullPath('..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\BasicDistFindSubControl.clktmpl');
  ExpectVarFromClientUnderTest('$LastAction_Status$', 'Successful', 'Plugin execution should succeed.');
  ExpectVarFromClientUnderTest('$PluginError$', '', 'No plugin error expected.');
end;


procedure TTestWorkerPoolManager_DistFindSubControl.Test_HappyFlow_EvaluateFileNameBeforeSending;
begin
  //SetEvaluateFileNameBeforeSendingInPluginAction(True);   //this would require loading the template, executing SetEvaluate.. then running the template
  ExecutePluginTestTemplate_FullPath('..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\BasicDistFindSubControl.clktmpl');
  ExpectVarFromClientUnderTest('$LastAction_Status$', 'Successful', 'Plugin execution should succeed.');
  ExpectVarFromClientUnderTest('$PluginError$', '', 'No plugin error expected.');
end;


procedure TTestWorkerPoolManager_DistFindSubControl.AfterAll_AlwaysExecute;
begin
  RemoveMachineFromList('127.0.0.1');
  CloseAllWorkers;
  CloseAllWorkerUIClickers;
  inherited AfterAll_AlwaysExecute;
end;


procedure TTestWorkerPoolManager_DistFindSubControl_NoWorker.BeforeAll_AlwaysExecute;
begin
  inherited BeforeAll_AlwaysExecute;

  SetKeys;
  SendAllDistPlugins;
end;


procedure TTestWorkerPoolManager_DistFindSubControl_NoWorker.Test_EnsureTheDistPluginStopsExecutionOnBadCredentials;
begin
  //This test verifies if the tmrProcessRecData timer, from Dist plugin, can be stopped. In case of an exception, the timer would add a log entry, wait for 1s, then exit.
  //When the plugin had bad/unset credentials, this timer would continue to run, even after the stop button has been pressed.
  //The fix was to disable the timer on Exception, and reenable it if the plugin is still supposed to be running (e.g., when the stop button is not pressed).

  ExecutePluginTestTemplate_FullPath('..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\ErrorsDistFindSubControl.clktmpl');
  ExpectVarFromClientUnderTest('$LastAction_Status$', 'Failed', 'Plugin execution should fail.');
  ExpectVarFromClientUnderTest('$PluginError$', 'Plugin exception: "Access violation".', 'Plugin error should be set.');
end;


procedure TTestWorkerPoolManager_DistFindSubControl_NoWorker.AfterAll_AlwaysExecute;
begin
  inherited AfterAll_AlwaysExecute;
end;


initialization

  RegisterTest(TTestWorkerPoolManager_Resources);
  RegisterTest(TTestWorkerPoolManager_DistFindSubControl);
  RegisterTest(TTestWorkerPoolManager_DistFindSubControl_NoWorker);
end.

