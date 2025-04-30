{
    Copyright (C) 2025 VCC
    creation date: 13 Apr 2025
    initial release date: 13 Apr 2025

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


unit TestDistPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TestHTTPAPI, fpcunit, testregistry, //UIActionsStuff,
  ClickerUtils;

type
  TStringArr = array of string;

  TTestDistPlugin = class(TTestHTTPAPI)
  private
    FReportedOSes: TStringArr;
    FReportedFonts: TStringArr;
    FPluginUsedOS: string; //TextRenderingOS property
    FCalledAction: string;
  protected
    procedure StartAllUIClickerInstances;
    procedure StartAllWorkerInstances(const AReportedOSes, AReportedFonts: TStringArr);  //at least those from this machine
    procedure StartTestUtilities;
    procedure ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
    procedure ArrangeMainUIClickerWindows;
    procedure ArrangeUIClickerActionWindows;
    procedure ArrangeWorkerWindows;

    procedure PrepareClickerUnderTestToReadItsVars;
    procedure PrepareClickerUnderTestToLocalMode;
    procedure LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
    procedure ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');
    procedure ExpectWorkAtWorkerSide(const AWork: TStringArr; AExpectedUnreceivedWorkCount: Integer; const ATaskAllocationCountInfo: TStringArr; const ATaskAllocationCountCount: TIntArr);
    procedure ExpectWorkAtPluginSide(const AWork: TStringArr; AExpectedUnreceivedWorkCount: Integer; const ATaskAllocationCountInfo: TStringArr; const ATaskAllocationCountCount: TIntArr);
    procedure ExecutePluginTestTemplate_FullPath(ATemplatePath: string);

    procedure BeforeAll(const AReportedOSes, AReportedFonts: TStringArr);
    procedure AfterAll;
  public
    constructor Create; override;
    procedure SetReportedOSes(const AReportedOSes: TStringArr);
    procedure SetReportedFonts(const AReportedFonts: TStringArr);
    procedure SetTextRenderingOSInPluginAction(AOSName: string);
    procedure SetTextRenderingPmtvActionInPluginAction(APmtvAction: string);
    procedure SetPluginUsedOS(AOSName: string);
  end;


  TTestDistPluginFullOSes = class(TTestDistPlugin)
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute; virtual;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; virtual;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; virtual;

    procedure AfterAll_AlwaysExecute; virtual;
  end;


  TTestDistPluginWinDefaultFonts = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginLinDefaultFonts = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinLinDefaultFonts = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinLinDefaultFonts_WinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinLinDefaultFonts_LinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinDefaultFonts_LinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginLinDefaultFonts_WinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinLinCustomFonts_WinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  TTestDistPluginWinLinCustomFonts_LinPlugin = class(TTestDistPluginFullOSes)
  public
    constructor Create; override;

  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfZeroFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfOneFontProfile_WinFontsOnly; override;
    procedure Test_AllocationOfTwoFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfThreeFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFourFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfFiveFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSixFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfSevenFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfEightFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfNineFontProfiles_WinFontsOnly; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


  //


  TTestDistPlugin_PmtvText = class(TTestDistPlugin)
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute; virtual;

    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_LinFontsOnly; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_WinLinFonts; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_WinLinFonts; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_LinFontsOnly_WinLinFonts; virtual;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly_WinLinFonts; virtual;

    procedure AfterAll_AlwaysExecute; virtual;
  end;


  TTestDistPlugin_PmtvText_WinLinWorkers = class(TTestDistPlugin_PmtvText)
  public
    constructor Create; override;
  published
    procedure BeforeAll_AlwaysExecute; override;

    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly; override;
    procedure Test_AllocationOfPmtvFontProfiles_LinFontsOnly; override;
    procedure Test_AllocationOfPmtvFontProfiles_WinLinFonts; override;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly; override;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_WinLinFonts; override;
    procedure Test_AllocationOfPmtvFontProfiles_LinFontsOnly_WinLinFonts; override;
    procedure Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly_WinLinFonts; override;

    procedure AfterAll_AlwaysExecute; override;
  end;


implementation

uses
  UITestUtils, AsyncProcess, Forms, ClickerActionsClient, Expectations,
  DistFindSubControlCommonConsts, ClickerActionProperties;


const
  CTestDriver_ServerPort_ForClientUnderTest = '25444';
  CClientUnderTestServerPort = '35444'; //this is a temporary mode, while UIClickerUITest reads the test results, then sets it back to client mode

  CWorkerClickerServerPort1 = '34444';
  CWorkerClickerServerPort2 = '44444';
  CWorkerClickerServerPort3 = '54444';
  CWorkerClickerServerPort4 = '24444';

  CTestClientAddress = 'http://127.0.0.1:' + CClientUnderTestServerPort + '/';                                //UIClicker-under-test client in server mode
  CTestDriverServerAddress_Client = 'http://127.0.0.1:' + CTestDriver_ServerPort_ForClientUnderTest + '/';    //UIClicker driver

  //CWorkerClickerServerAddress1 = 'http://127.0.0.1:' + CWorkerClickerServerPort1 + '/';
  //CWorkerClickerServerAddress2 = 'http://127.0.0.1:' + CWorkerClickerServerPort2 + '/';
  //CWorkerClickerServerAddress3 = 'http://127.0.0.1:' + CWorkerClickerServerPort3 + '/';
  //CWorkerClickerServerAddress4 = 'http://127.0.0.1:' + CWorkerClickerServerPort4 + '/';


  CSkipSavingWorkerSettings: string = ' --SkipSavingIni Yes';

var
  FIsWine: Boolean;
  FTestDriverForClient_Proc, FClientAppUnderTest_Proc: TAsyncProcess;
  FWorker1_Proc, FWorker2_Proc, FWorker3_Proc, FWorker4_Proc: TAsyncProcess;
  FServerForWorker1_Proc, FServerForWorker2_Proc, FServerForWorker3_Proc, FServerForWorker4_Proc: TAsyncProcess;
  CommonFonts_Proc: TAsyncProcess;
  FTemplatesDir: string;


constructor TTestDistPlugin.Create;
begin
  inherited Create;
  TestServerAddress := CTestServerAddress;
  SetLength(FReportedOSes, 0);
  SetLength(FReportedFonts, 0);
  FPluginUsedOS := '';
  FCalledAction := '';
end;


procedure TTestDistPlugin.SetReportedOSes(const AReportedOSes: TStringArr);
var
  i: Integer;
begin
  SetLength(FReportedOSes, Length(AReportedOSes));
  for i := 0 to Length(AReportedOSes) - 1 do
    FReportedOSes[i] := AReportedOSes[i];
end;


procedure TTestDistPlugin.SetReportedFonts(const AReportedFonts: TStringArr);
var
  i: Integer;
begin
  SetLength(FReportedFonts, Length(AReportedFonts));
  for i := 0 to Length(AReportedFonts) - 1 do
    FReportedFonts[i] := AReportedFonts[i];
end;


procedure TTestDistPlugin.SetTextRenderingOSInPluginAction(AOSName: string);
var
  EditTemplateOptions: TClkEditTemplateOptions;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);

  EditTemplateOptions.Operation := etoSetProperty;
  EditTemplateOptions.WhichTemplate := etwtSelf;
  EditTemplateOptions.ListOfEditedProperties := 'FileName=$AppDir$\..\UIClickerDistFindSubControlPlugin\lib\$AppBitness$-$OSBitness$\UIClickerDistFindSubControl.dllListOfPropertiesAndValues=FindSubControlAction=CredentialsFullFileName=Address=127.0.0.1Port=1883WorkerQoS=1GetWorkerCapabilitiesTimeout=500FindSubControlWorkerTimeout=3000FindSubControlTimeoutDiff=2500WorkerCapabilitiesSource=wcsReqCapAndGetFontsAndFindSubControlLoadWorkerCapabilitiesCacheAction=SaveWorkerCapabilitiesCacheAction=TextRenderingOS=' + AOSName + 'ListOfMultiValuePropertyNames=UseCompression=TrueCompressionAlgorithm=LzmaLzmaEndOfStream=FalseLzmaAlgorithm=2LzmaNumBenchMarkPasses=10LzmaDictionarySize=1048576LzmaMatchFinder=1LzmaLiteralContext=3LzmaLiteralPosBits=0LzmaPosBits=0LzmaFastBytes=5VariablesForWorkers=$Control_Handle$,$Control_Left$,$Control_Top$,$Control_Right$,$Control_Bottom$,$Control_Width$,$Control_Height$ExtraDebuggingInfo=True';
  EditTemplateOptions.ListOfEnabledProperties := 'TextRenderingOS';
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


procedure TTestDistPlugin.SetTextRenderingPmtvActionInPluginAction(APmtvAction: string);
var
  EditTemplateOptions: TClkEditTemplateOptions;
begin
  GetDefaultPropertyValues_EditTemplate(EditTemplateOptions);

  EditTemplateOptions.Operation := etoSetProperty;
  EditTemplateOptions.WhichTemplate := etwtSelf;
  EditTemplateOptions.ListOfEditedProperties := 'FileName=$AppDir$\..\UIClickerDistFindSubControlPlugin\lib\$AppBitness$-$OSBitness$\UIClickerDistFindSubControl.dllListOfPropertiesAndValues=FindSubControlAction=' + APmtvAction + 'CredentialsFullFileName=Address=127.0.0.1Port=1883WorkerQoS=1GetWorkerCapabilitiesTimeout=500FindSubControlWorkerTimeout=3000FindSubControlTimeoutDiff=2500WorkerCapabilitiesSource=wcsReqCapAndGetFontsAndFindSubControlLoadWorkerCapabilitiesCacheAction=SaveWorkerCapabilitiesCacheAction=TextRenderingOS=Win+LinListOfMultiValuePropertyNames=UseCompression=TrueCompressionAlgorithm=LzmaLzmaEndOfStream=FalseLzmaAlgorithm=2LzmaNumBenchMarkPasses=10LzmaDictionarySize=1048576LzmaMatchFinder=1LzmaLiteralContext=3LzmaLiteralPosBits=0LzmaPosBits=0LzmaFastBytes=5VariablesForWorkers=$Control_Handle$,$Control_Left$,$Control_Top$,$Control_Right$,$Control_Bottom$,$Control_Width$,$Control_Height$ExtraDebuggingInfo=True';
  EditTemplateOptions.ListOfEnabledProperties := 'FindSubControlAction';
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


procedure TTestDistPlugin.SetPluginUsedOS(AOSName: string);
begin
  FPluginUsedOS := AOSName;
end;


procedure TTestDistPlugin.StartAllUIClickerInstances;
const
  CDisplayTabsOptions: string = ' --AutoSwitchToExecTab Yes --AutoEnableSwitchTabsOnDebugging Yes';
var
  PathToTestDriver, PathToAppUnderTest: string;
  DriverParams, AppUnderTestClientParams, ServerForWorkerParams: string;
begin
  PathToTestDriver := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker\TestDriver\UIClicker.exe'; //this should be a stable version of UIClicker
  PathToAppUnderTest := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker\UIClicker.exe';

  {$IFDEF UNIX}
    FIsWine := False;
  {$ELSE}
    FIsWine := {DirectoryExists('Z:\home') and} DirectoryExists('Z:\media') and DirectoryExists('Z:\etc'); //assume this is running on Wine
  {$ENDIF}

  DriverParams := '--SetExecMode Server --ExtraCaption Driver.Client --ServerPort ' + CTestDriver_ServerPort_ForClientUnderTest + CDisplayTabsOptions;
  AppUnderTestClientParams := '--SetExecMode Local --ExtraCaption ClientUnderTest' + CSkipSavingSettings + CDisplayTabsOptions;
  ServerForWorkerParams := '--SetExecMode Server Worker --ServerPort ';

  if FIsWine then
  begin
    SetUIClickerWindowPosition(ExtractFilePath(PathToTestDriver) + 'Clicker.ini', 1040, 50, 1000, 300);
    Sleep(100);
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, DriverParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);

    SetUIClickerWindowPosition(ExtractFilePath(PathToAppUnderTest) + 'Clicker.ini', 360, 50, 30, 490);
    Sleep(100);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams + ' --UseWideStringsOnGetControlText Yes');
    Sleep(1000);
  end
  else
  begin
    FTestDriverForClient_Proc := CreateUIClickerProcess(PathToTestDriver, DriverParams);
    FClientAppUnderTest_Proc := CreateUIClickerProcess(PathToAppUnderTest, AppUnderTestClientParams);

    FServerForWorker1_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort1 + ' --ExtraCaption Worker1');
    FServerForWorker2_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort2 + ' --ExtraCaption Worker2');
    FServerForWorker3_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort3 + ' --ExtraCaption Worker3');
    FServerForWorker4_Proc := CreateUIClickerProcess(PathToAppUnderTest, ServerForWorkerParams + CWorkerClickerServerPort4 + ' --ExtraCaption Worker4');
  end;
end;


procedure TTestDistPlugin.StartAllWorkerInstances(const AReportedOSes, AReportedFonts: TStringArr);  //at least those from this machine
var
  PathToDistWorker: string;
  ReportedFonts: TStringArr;
  i: Integer;
begin
  Expect(Length(AReportedOSes)).ToBe(4, 'Only 4 workers are supported for now (OSes).');   //Each AReportedOSes item matches a worker.
  Expect(Length(AReportedFonts)).ToBe(4, 'Only 4 workers are supported for now (Fonts).'); //Each AReportedFonts item matches a worker. If an item is empty, the "reported fonts" feature is not used, so the worker gets the list of fonts from its UIClicker.

  //Other params: '--SetBrokerCredFile', '--SetBrokerAddress', '--SetBrokerPort'
  PathToDistWorker := ExtractFilePath(ParamStr(0)) + '..\Worker\FindSubControlWorker.exe';

  SetLength(ReportedFonts, Length(AReportedFonts));
  for i := 0 to Length(ReportedFonts) - 1 do
    if AReportedFonts[i] = '' then
      ReportedFonts[i] := ''
    else
      ReportedFonts[i] := ' --SetReportedFonts "' + AReportedFonts[i] + '"'; //using double quotes, because of the command line

  FWorker1_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOSes[0] + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort1 + ' --SetWorkerExtraName First --SetWorkerExtraCaption First' + ReportedFonts[0]);
  Sleep(500);
  FWorker2_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOSes[1] + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort2 + ' --SetWorkerExtraName Second --SetWorkerExtraCaption Second' + ReportedFonts[1]);
  Sleep(500);
  FWorker3_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOSes[2] + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort3 + ' --SetWorkerExtraName Third --SetWorkerExtraCaption Third' + ReportedFonts[2]);
  Sleep(500);
  FWorker4_Proc := CreateUIClickerProcess(PathToDistWorker, '--SetReportedOS ' + AReportedOSes[3] + CSkipSavingWorkerSettings + ' --SetUIClickerPort ' + CWorkerClickerServerPort4 + ' --SetWorkerExtraName Fourth --SetWorkerExtraCaption Fourth' + ReportedFonts[3]);
  Sleep(500);
end;


procedure TTestDistPlugin.StartTestUtilities;
var
  PathToCommonFonts: string;
begin
  PathToCommonFonts := ExtractFilePath(ParamStr(0)) + 'CommonFonts\CommonFonts.exe';
  CommonFonts_Proc := CreateUIClickerProcess(PathToCommonFonts, '');
  Sleep(500);
end;


procedure TTestDistPlugin.ExecuteTemplateOnTestDriver(ATemplatePath, AFileLocation: string; AAdditionalExpectedVar: string = ''; AAdditionalExpectedValue: string = '');
begin
  ExecuteTemplateOnCustomTestDriver(CTestDriverServerAddress_Client, ATemplatePath, AFileLocation, AAdditionalExpectedVar, AAdditionalExpectedValue);
end;


procedure TTestDistPlugin.ArrangeMainUIClickerWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\ArrangeMainUIClickerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestDistPlugin.ArrangeUIClickerActionWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\ArrangeActionWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestDistPlugin.ArrangeWorkerWindows;
begin
  ExecuteTemplateOnTestDriver(ExtractFilePath(ParamStr(0)) + 'TestDriverFiles\ArrangeWorkerWindows.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure SetVariableOnTestDriverClient(AVarName, AVarValue: string; AEvalVarBefore: Boolean = False);
begin
  SetVariableOnCustomTestDriverClient(CTestDriverServerAddress_Client, AVarName, AVarValue, AEvalVarBefore);
end;


procedure WaitForDriverStartup;
begin
  WaitForCustomDriverStartup(CTestDriverServerAddress_Client);
end;


procedure TTestDistPlugin.PrepareClickerUnderTestToReadItsVars;
begin
  PrepareCustomClickerUnderTestToReadItsVars(CTestDriverServerAddress_Client, CClientUnderTestServerPort, FTemplatesDir);
end;


procedure TTestDistPlugin.PrepareClickerUnderTestToLocalMode;
begin
  PrepareCustomClickerUnderTestToLocalMode(CTestDriverServerAddress_Client, FTemplatesDir);
end;


procedure TTestDistPlugin.LoadTestTemplateInClickerUnderTest_FullPath(ATestTemplate: string);
begin
  SetVariableOnTestDriverClient('$TemplateToLoad$', ATestTemplate);
  ExecuteTemplateOnTestDriver(FTemplatesDir + 'LoadCallerTemplateIntoAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
end;


procedure TTestDistPlugin.ExpectVarFromClientUnderTest(AVarName, AExpectedValue: string; AExtraComment: string = '');
begin
  TestServerAddress := CTestClientAddress;
  try
    //connect to ClientUnderTest instance, which is now running in server mode and read its variables
    Expect(GetVarValueFromServer(AVarName)).ToBe(AExpectedValue, AExtraComment);
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
  end;
end;


type
  TBooleanArr = array of Boolean;

const
  CEmptyWorkTask = 'TxtCnt=0&BmpCnt=0&PmtvCnt=0&';
  COneFontProfileTask = 'TxtCnt=1&BmpCnt=0&PmtvCnt=0&';
  CTwoFontProfilesTask = 'TxtCnt=2&BmpCnt=0&PmtvCnt=0&';
  CThreeFontProfilesTask = 'TxtCnt=3&BmpCnt=0&PmtvCnt=0&';
  CFourFontProfilesTask = 'TxtCnt=4&BmpCnt=0&PmtvCnt=0&';
  CFiveFontProfilesTask = 'TxtCnt=5&BmpCnt=0&PmtvCnt=0&';
  CSixFontProfilesTask = 'TxtCnt=6&BmpCnt=0&PmtvCnt=0&';
  CSevenFontProfilesTask = 'TxtCnt=7&BmpCnt=0&PmtvCnt=0&';
  CEightFontProfilesTask = 'TxtCnt=8&BmpCnt=0&PmtvCnt=0&';
  CNineFontProfilesTask = 'TxtCnt=9&BmpCnt=0&PmtvCnt=0&';

procedure ExpectWork(var AWorkersDbgInfo: TStringArray; const AWork: TStringArr; AExpectedUnreceivedWorkCount: Integer; const ATaskAllocationCountInfo: TStringArr; const ATaskAllocationCountCount: TIntArr);
var
  FoundArr: TBooleanArr;
  FoundCountInfo: TIntArr;
  FoundUnAllocatedCount, i, j: Integer;
begin
  SetLength(FoundArr, Length(AWork));
  SetLength(FoundCountInfo, Length(ATaskAllocationCountInfo));

  for i := 0 to Length(FoundArr) - 1 do
    FoundArr[i] := False;

  for j := 0 to Length(FoundCountInfo) - 1 do
    FoundCountInfo[j] := 0;

  FoundUnAllocatedCount := 0;
  for i := 0 to Length(AWorkersDbgInfo) - 1 do
  begin
    for j := 0 to Length(AWork) - 1 do
      if Pos(AWork[j], AWorkersDbgInfo[i]) > 0 then
        FoundArr[j] := True;

    for j := 0 to Length(ATaskAllocationCountInfo) - 1 do
      if Pos(ATaskAllocationCountInfo[j], AWorkersDbgInfo[i]) > 0 then
        Inc(FoundCountInfo[j]);

    if AWorkersDbgInfo[i] = CEmptyWorkTask then
      Inc(FoundUnAllocatedCount);
  end;

  Expect(FoundUnAllocatedCount).ToBe(AExpectedUnreceivedWorkCount, 'The number of workers, which received work, does not match the expected count: ' + IntToStr(AExpectedUnreceivedWorkCount) + '.');

  for j := 0 to Length(FoundArr) - 1 do
    Expect(FoundArr[j]).ToBe(True, 'A worker should get work for the task [' + IntToStr(j) + '].');

  Expect(Length(ATaskAllocationCountInfo)).ToBe(Length(ATaskAllocationCountCount), 'The lengths of the two arrays should match: ATaskAllocationCountInfo vs. ATaskAllocationCountCount.');
  for j := 0 to Length(FoundCountInfo) - 1 do
    Expect(FoundCountInfo[j]).ToBe(ATaskAllocationCountCount[j], 'Task info [' + IntToStr(j) + '] is expected to be found ' + IntToStr(ATaskAllocationCountCount[j]) + ' time(s).');
end;


procedure TTestDistPlugin.ExpectWorkAtWorkerSide(const AWork: TStringArr; AExpectedUnreceivedWorkCount: Integer; const ATaskAllocationCountInfo: TStringArr; const ATaskAllocationCountCount: TIntArr);
var
  WorkersDbgInfo: TStringArray;
begin
  ExecuteTemplateOnTestDriver(FTemplatesDir + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestDriverFiles\GetAllocatedWorkFromAllWorkers.clktmpl', CREParam_FileLocation_ValueDisk);

  SetLength(WorkersDbgInfo, 4);
  WorkersDbgInfo[0] := GetVarValueFromServer('$Worker_First$');       //TestServerAddress is already set to CTestDriverServerAddress_Client;
  WorkersDbgInfo[1] := GetVarValueFromServer('$Worker_Second$');
  WorkersDbgInfo[2] := GetVarValueFromServer('$Worker_Third$');
  WorkersDbgInfo[3] := GetVarValueFromServer('$Worker_Fourth$');

  ExpectWork(WorkersDbgInfo, AWork, AExpectedUnreceivedWorkCount, ATaskAllocationCountInfo, ATaskAllocationCountCount);
end;


procedure TTestDistPlugin.ExpectWorkAtPluginSide(const AWork: TStringArr; AExpectedUnreceivedWorkCount: Integer; const ATaskAllocationCountInfo: TStringArr; const ATaskAllocationCountCount: TIntArr);
var
  WorkersDbgInfo: TStringArray;
  i: Integer;
begin
  TestServerAddress := CTestClientAddress;
  try
    SetLength(WorkersDbgInfo, 4); //4 workers
    for i := 0 to Length(WorkersDbgInfo) - 1 do
      WorkersDbgInfo[i] := GetVarValueFromServer('$Worker[' + IntToStr(i) + '].WorkerSpecificTask$');
  finally
    TestServerAddress := CTestDriverServerAddress_Client; //restore
  end;

  ExpectWork(WorkersDbgInfo, AWork, AExpectedUnreceivedWorkCount, ATaskAllocationCountInfo, ATaskAllocationCountCount);
end;


procedure TTestDistPlugin.BeforeAll(const AReportedOSes, AReportedFonts: TStringArr);
begin
  StartAllUIClickerInstances;
  StartAllWorkerInstances(AReportedOSes, AReportedFonts);
  StartTestUtilities;

  WaitForDriverStartup;

  try
    if FIsWine then
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

  ArrangeMainUIClickerWindows;      //Setting window position from ini file, works on Wine. Setting from UIClicker does not (yet).
  Sleep(500);                       //these sleep calls should be replaced by some waiting loops
  ArrangeUIClickerActionWindows;
  Sleep(500);
  ArrangeWorkerWindows;
  Sleep(500);

  FTemplatesDir := ExtractFilePath(ParamStr(0)) + '..\..\UIClicker\TestDriver\ActionTemplates\';
end;


procedure TTestDistPlugin.AfterAll;
begin
  //the following instances should be terminated in this specific order:
  FClientAppUnderTest_Proc.Terminate(0);
  FTestDriverForClient_Proc.Terminate(0);

  FWorker1_Proc.Terminate(0);
  FWorker2_Proc.Terminate(0);
  FWorker3_Proc.Terminate(0);
  FWorker4_Proc.Terminate(0);

  FServerForWorker1_Proc.Terminate(0);
  FServerForWorker2_Proc.Terminate(0);
  FServerForWorker3_Proc.Terminate(0);
  FServerForWorker4_Proc.Terminate(0);

  CommonFonts_Proc.Terminate(0);

  FreeAndNil(FClientAppUnderTest_Proc);
  FreeAndNil(FTestDriverForClient_Proc);

  FreeAndNil(FWorker1_Proc);
  FreeAndNil(FWorker2_Proc);
  FreeAndNil(FWorker3_Proc);
  FreeAndNil(FWorker4_Proc);

  FreeAndNil(FServerForWorker1_Proc);
  FreeAndNil(FServerForWorker2_Proc);
  FreeAndNil(FServerForWorker3_Proc);
  FreeAndNil(FServerForWorker4_Proc);

  FreeAndNil(CommonFonts_Proc);
end;


procedure TTestDistPlugin.ExecutePluginTestTemplate_FullPath(ATemplatePath: string);
begin
  PrepareClickerUnderTestToLocalMode;

  TestServerAddress := CTestDriverServerAddress_Client;
  LoadTestTemplateInClickerUnderTest_FullPath(ATemplatePath);

  if FPluginUsedOS <> '' then
  begin
    SetTextRenderingOSInPluginAction(FPluginUsedOS);
    PrepareClickerUnderTestToLocalMode;
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'GoToActionPlayer.clktmpl', CREParam_FileLocation_ValueDisk);
  end;

  if FCalledAction <> '' then
  begin
    SetTextRenderingPmtvActionInPluginAction(FCalledAction);
    PrepareClickerUnderTestToLocalMode;
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'GoToActionPlayer.clktmpl', CREParam_FileLocation_ValueDisk);
  end;

  ExecuteTemplateOnTestDriver(FTemplatesDir + 'PlayAllActionsFromAppUnderTest.clktmpl', CREParam_FileLocation_ValueDisk);
  PrepareClickerUnderTestToReadItsVars;

  if (FPluginUsedOS <> '') or (FCalledAction <> '') then
  begin
    ExecuteTemplateOnTestDriver(FTemplatesDir + 'GoToActionPlayer.clktmpl', CREParam_FileLocation_ValueDisk);
    ExecuteTemplateOnTestDriver(FTemplatesDir + '..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestDriverFiles\CloseModifiedTemplatePrompt.clktmpl', CREParam_FileLocation_ValueDisk);
  end;
end;


constructor TTestDistPluginFullOSes.Create;
begin
  inherited Create;
end;


procedure TTestDistPluginFullOSes.BeforeAll_AlwaysExecute;
begin
  BeforeAll(FReportedOSes, FReportedFonts);
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateZeroFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateOneFontProfile.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateTwoFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateThreeFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateFourFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateFiveFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateSixFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateSevenFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateEightFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocateNineFontProfiles.clktmpl');
end;


procedure TTestDistPluginFullOSes.AfterAll_AlwaysExecute;
begin
  AfterAll;
end;


//

constructor TTestDistPluginWinDefaultFonts.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Win, CReportedOS_Win]);
  SetReportedFonts(['', '', '', '']);
end;


procedure TTestDistPluginWinDefaultFonts.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
end;


procedure TTestDistPluginWinDefaultFonts.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinDefaultFonts.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginLinDefaultFonts.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Lin, CReportedOS_Lin, CReportedOS_Lin, CReportedOS_Lin]);
  SetReportedFonts(['', '', '', '']);
end;


procedure TTestDistPluginLinDefaultFonts.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
end;


procedure TTestDistPluginLinDefaultFonts.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginLinDefaultFonts.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginWinLinDefaultFonts.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);       //two Win, two Lin
  SetReportedFonts(['', '', '', '']);
end;


procedure TTestDistPluginWinLinDefaultFonts.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 1, [COneFontProfileTask], [3]);
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 0, [COneFontProfileTask], [4]);
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [3, 1]);  //COneFontProfileTask should be found 3 times.  CTwoFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [2, 2]);  //COneFontProfileTask should be found 2 times.  CTwoFontProfilesTask should be found 2 times.
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [1, 3]);  //COneFontProfileTask should be found 1 time.  CTwoFontProfilesTask should be found 3 times.
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask], [0, 4]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 4 times.
end;


procedure TTestDistPluginWinLinDefaultFonts.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 0, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 3, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 3 times.  CThreeFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinLinDefaultFonts.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginWinLinDefaultFonts_WinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);       //two Win, two Lin
  SetReportedFonts(['', '', '', '']);
  SetPluginUsedOS(CReportedOS_Win);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 2, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 2, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask], [0, 2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask], [0, 2]);
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 1, 1]);  //COneFontProfileTask should be found 0 time.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 1, 1]);  //COneFontProfileTask should be found 0 time.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.  CThreeFontProfilesTask should be found 0 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.  CThreeFontProfilesTask should be found 0 time.
end;


procedure TTestDistPluginWinLinDefaultFonts_WinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginWinLinDefaultFonts_LinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);       //two Win, two Lin
  SetReportedFonts(['', '', '', '']);
  SetPluginUsedOS(CReportedOS_Lin);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 2, [COneFontProfileTask], [2]);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 2, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 2, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask], [0, 2]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask], [0, 2]);
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 1 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 1 time.
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 1, 1]);  //COneFontProfileTask should be found 0 time.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 1, 1]);  //COneFontProfileTask should be found 0 time.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 2]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.  CThreeFontProfilesTask should be found 0 time.
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 2, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 1, 1]);  //COneFontProfileTask should be found 0 times.  CTwoFontProfilesTask should be found 0 times.  CThreeFontProfilesTask should be found 0 time.
end;


procedure TTestDistPluginWinLinDefaultFonts_LinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginWinDefaultFonts_LinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Win, CReportedOS_Win]);       //all Win, no Lin
  SetReportedFonts(['', '', '', '']);
  SetPluginUsedOS(CReportedOS_Lin);                                                            //plugin requests to execute all tasks on Lin workers
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinDefaultFonts_LinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginLinDefaultFonts_WinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Lin, CReportedOS_Lin, CReportedOS_Lin, CReportedOS_Lin]);       //all Lin, no Win
  SetReportedFonts(['', '', '', '']);
  SetPluginUsedOS(CReportedOS_Win);                                                            //plugin requests to execute all tasks on Win workers
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginLinDefaultFonts_WinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;



//


constructor TTestDistPluginWinLinCustomFonts_WinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);       //two Win, two Lin. Only one Win worker "implements" the used font.
  SetReportedFonts(['DejaVu Sans,DejaVu Serif', 'Courier New,Tahoma', 'DejaVu Sans,DejaVu Sans Mono', 'Liberation Sans']);
  SetPluginUsedOS(CReportedOS_Win);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask], [0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask], [0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask], [0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask], [0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask], [0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask], [0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask, CNineFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask, CNineFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_WinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPluginWinLinCustomFonts_LinPlugin.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);       //two Win, two Lin. Only one Lin worker "implements" the used font.
  SetReportedFonts(['DejaVu Sans,DejaVu Serif', 'Courier New,Tahoma', 'DejaVu Sans,DejaVu Sans Mono', 'Liberation Sans']);
  SetPluginUsedOS(CReportedOS_Lin);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.BeforeAll_AlwaysExecute;
begin
  inherited;
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfZeroFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide([], 4, [], []);
  ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfOneFontProfile_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&'], 3, [COneFontProfileTask], [1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfTwoFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask], [0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask], [0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfThreeFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask], [0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfFourFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask], [0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfFiveFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask], [0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfSixFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask], [0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask], [0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfSevenFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask], [0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask], [0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfEightFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.Test_AllocationOfNineFontProfiles_WinFontsOnly;
begin
  inherited;
  ExpectWorkAtPluginSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask, CNineFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 0, 1]);
  ExpectWorkAtWorkerSide(['Txt_0=1&', 'Txt_1=1&', 'Txt_2=1&', 'Txt_3=1&', 'Txt_4=1&', 'Txt_5=1&', 'Txt_6=1&', 'Txt_7=1&', 'Txt_8=1&'], 3, [COneFontProfileTask, CTwoFontProfilesTask, CThreeFontProfilesTask, CFourFontProfilesTask, CFiveFontProfilesTask, CSixFontProfilesTask, CSevenFontProfilesTask, CEightFontProfilesTask, CNineFontProfilesTask], [0, 0, 0, 0, 0, 0, 0, 0, 1]);
end;


procedure TTestDistPluginWinLinCustomFonts_LinPlugin.AfterAll_AlwaysExecute;
begin
  inherited;
end;


//


constructor TTestDistPlugin_PmtvText.Create;
begin
  inherited Create;
end;


procedure TTestDistPlugin_PmtvText.BeforeAll_AlwaysExecute;
begin
  BeforeAll(FReportedOSes, FReportedFonts);
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_WinFontsOnly;
begin
  FCalledAction := 'Find_WinOnly';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_LinFontsOnly;
begin
  FCalledAction := 'Find_LinOnly';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_WinLinFonts;
begin
  FCalledAction := 'Find_WinLin';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly;
begin
  FCalledAction := 'Find_WinOnly_LinOnly';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_WinLinFonts;
begin
  FCalledAction := 'Find_WinOnly_WinLin';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_LinFontsOnly_WinLinFonts;
begin
  FCalledAction := 'Find_LinOnly_WinLin';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly_WinLinFonts;
begin
  FCalledAction := 'Find_WinOnly_LinOnly_WinLin';
  ExecutePluginTestTemplate_FullPath('..\..\..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\AllocatePmtv.clktmpl');
end;


procedure TTestDistPlugin_PmtvText.AfterAll_AlwaysExecute;
begin
  AfterAll;
end;


//


constructor TTestDistPlugin_PmtvText_WinLinWorkers.Create;
begin
  inherited Create;
  SetReportedOSes([CReportedOS_Win, CReportedOS_Win, CReportedOS_Lin, CReportedOS_Lin]);
  SetReportedFonts(['DejaVu Sans,DejaVu Serif', 'Courier New,Tahoma,Verdana', 'DejaVu Sans,DejaVu Sans Mono', 'DejaVu Serif,Monospace,Ubuntu Mono']);
  //SetPluginUsedOS(CReportedOS_WinLin);
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.BeforeAll_AlwaysExecute;
begin
  inherited;
  //ExpectWorkAtPluginSide([], 4, [], []);   //something similar
  //ExpectWorkAtWorkerSide([], 4, [], []);
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_WinFontsOnly;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_LinFontsOnly;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_WinLinFonts;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_WinLinFonts;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_LinFontsOnly_WinLinFonts;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.Test_AllocationOfPmtvFontProfiles_WinFontsOnly_LinFontsOnly_WinLinFonts;
begin
  inherited;
end;


procedure TTestDistPlugin_PmtvText_WinLinWorkers.AfterAll_AlwaysExecute;
begin
  inherited;
end;


initialization

  RegisterTest(TTestDistPluginWinDefaultFonts);
  RegisterTest(TTestDistPluginLinDefaultFonts);
  RegisterTest(TTestDistPluginWinLinDefaultFonts);
  RegisterTest(TTestDistPluginWinLinDefaultFonts_WinPlugin);
  RegisterTest(TTestDistPluginWinLinDefaultFonts_LinPlugin);
  RegisterTest(TTestDistPluginWinDefaultFonts_LinPlugin);
  RegisterTest(TTestDistPluginLinDefaultFonts_WinPlugin);
  RegisterTest(TTestDistPluginWinLinCustomFonts_WinPlugin);
  RegisterTest(TTestDistPluginWinLinCustomFonts_LinPlugin);
  RegisterTest(TTestDistPlugin_PmtvText_WinLinWorkers);
end.

