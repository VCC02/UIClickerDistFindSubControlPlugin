{
    Copyright (C) 2025 VCC
    creation date: 07 May 2024
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


unit DistFindSubControlCommonConsts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MemArchive, TplLzmaUnit, TplZlibUnit;


type
  TCompressionAlgorithm = (caZlib, caLzma);


const
  CMQTT_AppToWorkerCmd_GetCapabilities = 'AppToWorker_GetCapabilities';
  //CMQTT_AppToWorkerCmd_FindSubControl = 'AppToWorker_FindSubControl';  //the actual action content should be sent instead

  //CMQTT_WorkerToAppCmd_GetCapabilities = 'WorkerToApp_GetCapabilities';
  //CMQTT_WorkerToAppCmd_FindSubControl = 'WorkerToApp_FindSubControl';

  CMQTT_Worker_UnhandledRequest = 'UnhandledRequest';

  CTopicName_AppToWorker_GetCapabilities = 'AppToWorker_GetCapabilities';  //request to workers
  CTopicName_WorkerToApp_GetCapabilities = 'WorkerToApp_GetCapabilities';  //response from workers

  CTopicName_AppToWorker_SendBackground = 'AppToWorker_SendBackground';    //request to workers
  CTopicName_WorkerToApp_SendBackground = 'WorkerToApp_SendBackground';    //response from workers

  CTopicName_AppToWorker_FindSubControl = 'AppToWorker_FindSubControl';    //request to workers
  CTopicName_WorkerToApp_FindSubControl = 'WorkerToApp_FindSubControl';    //response from workers

  CTopicName_AppToWorker_GetListOfFonts = 'AppToWorker_GetListOfFonts';    //request to workers
  CTopicName_WorkerToApp_GetListOfFonts = 'WorkerToApp_GetListOfFonts';    //response from workers

  CTopicName_WorkerToWorker_Ping = 'Ping_'; //followed by worker ID

  CCallbackID_GetCapabilities = 0;
  CCallbackID_SendBackgroundToAll = 1;
  CCallbackID_SendBackgroundToSome = 2;
  CCallbackID_SendFindSubControl = 3;
  CCallbackID_GetListOfFonts = 4;

  CCompressionAlgorithms: array[TCompressionAlgorithm] of string = ('Zlib', 'Lzma');

  CProtocolParam_Name = 'Name';
  CProtocolParam_OS = 'OS';
  CProtocolParam_FileCache = 'FileCache';
  CProtocolParam_ExtraName = 'ExtraName';

  CProtocolParam_ImageSourceRawContent = 'ImageSourceRawContent';
  CProtocolParam_UsingCompression = 'UsingCompression';
  CProtocolParam_CompressionAlgorithm = 'CompressionAlgorithm';
  CProtocolParam_ResponseArchiveSize = 'ResponseArchiveSize';
  CProtocolParam_ResultImageArchive = 'ResponseArchiveDebugImage';
  CProtocolParam_Fonts = 'Fonts';
  CProtocolParam_RequestID = 'RequestID';

  CBackgroundFileNameInArchive = '_:\Background.bmp'; //a name which cannot be used as OS's file system
  CBackgroundFileNameForUIClicker = 'Background.bmp';
  CResultFileNameInArchive = 'Result.bmp';
  CBackgroundOKResponse = 'Done';

  CVarsForWorkersInArchive_Names = 'VarsForWorkers_Names.txt';
  CVarsForWorkersInArchive_Values = 'VarsForWorkers_Values.txt';
  CVarsForWorkersInArchive_EvalBefore = 'VarsForWorkers_EvalBefore.txt';
  //There is a verification in ProcessFindSubControlRequest procedure about avoiding these files. Please add the new filename if that's the case.

                               //original value: 1500
  //CFindSubControlTimeoutDiff = 2500; //Difference between plugin timeout and the actual FindSubControl action timeout. This is available for compressing, decompressing and transmission (Main_UIClicker.plugin <-> broker <-> worker <-> Dest.UIClicker)
  CMinFindSubControlActionTimeout = 100; //Minimum FindSubControl action timeout, set as default in case the computed one results in a a smaller value. This means that the destination UIClicker should still have at least this value as action timeout. Greater values may lead to successful processing of FindSubControl, but failed plugin action (because of total plugin timeout).
                                         //Please update the FindSubControlWorkerTimeout property hint (on CPluginHints constant) if modifying these values.

  CWorkerTaskAssignmentOperator = '^^~^^~~^^~^^';
  CWorkerTaskLineBreak = '***@@@***';

  CGetListOfFontsResultVarName = '$GetListOfFontsResult$';

  CReportedOS_Win = 'Win';
  CReportedOS_Lin = 'Lin';

  CWorkerTask_TxtPrefix = 'Txt_';
  CWorkerTask_BmpPrefix = 'Bmp_';
  CWorkerTask_PmtvPrefix = 'Pmtv_';

  CEmptyWorkTask = 'TxtCnt=0&BmpCnt=0&PmtvCnt=0&';


type
  TOnAddToLogNoObj = procedure(AMsg: string);

  TArchiveHandlers = class
  private
    FCompressionAlgorithm: TCompressionAlgorithm;
    FLzmaOptions: TplLzmaOptions;
    FOnAddToLogNoObj: TOnAddToLogNoObj;

    procedure DoOnAddToLogNoObj(AMsg: string);
  public
    constructor Create;

    procedure HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
    procedure HandleOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
    function HandleOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;

    property CompressionAlgorithm: TCompressionAlgorithm read FCompressionAlgorithm write FCompressionAlgorithm;
    property LzmaOptions: TplLzmaOptions write FLzmaOptions;

    property OnAddToLogNoObj: TOnAddToLogNoObj write FOnAddToLogNoObj;
  end;


  TResponseVars = record
    ControlLeft, ControlTop, ControlRight, ControlBottom: string;
    ControlWidth, ControlHeight, HalfControlWidth, HalfControlHeight: string;
    SubCnvXOffset, SubCnvYOffset: string;

    AllControl_Handles, AllControl_XOffsets, AllControl_YOffsets: string;
    AllControl_MatchSource, AllControl_DetailedMatchSource, AllControl_ResultedErrorCount: string;
    AllControl_Lefts, AllControl_Tops, AllControl_Rights, AllControl_Bottoms: string;
    AllControl_Widths, AllControl_Heights, AllHalfControl_Widths, AllHalfControl_Heights: string;
  end;

  TWorker = record
    Name: string; //Maybe a unique part of the ClientID (or full ID if required), as assigned by broker. This name will also be used when requesting the execution of FindSubControl by the plugin.
    OS: string; // Win/Lin
    WorkerSpecificTask: string; //This field is set by plugin when preparing FindSubControl requests. Every worker receives instructions about what part of a FindSubControl action should work on (not the entire action).
    FileCacheInfo: TStringList; //list of files and their hashes, currently present in worker's InMemFS (not its pair UIClicker).
    ExtraName: string; //user-defined name
    FilesToSend: string; // CRLF separated list of filenames (bmps, pmtvs etc)
    BmpFilesToSend, PmtvFilesToSend: string; // CRLF separated list of filenames (bmps, pmtvs) - two different lists, used for updating actions
    ArchiveStream: TMemoryStream;
    Archive: TMemArchive;
    CompressionTime: QWord;   //for statistics only
    WorkerActionContentStr: string; //What is being sent via MQTT to a worker. This field contains action params, ArchiveStream and various other compression params.
    TxtCntW, BmpCntW, PmtvCntW: Integer; //counts per worker
    ResponseReceived: Boolean;    //Most dest UIClicker will timeout, because they won't find the search bmp. At least one should find.
    FindSubControlFound: Boolean; //At least one worker should have this property set to True, to pass the action.
    Response: string;
    ResponseVars: TResponseVars;
    ResponseBitmapStream: TMemoryStream;
    Fonts: string;
    FindSubControlRequestID: string;
  end;

  TWorkerArr = array of TWorker;

  TDistPluginOptions = record
    FindSubControlAction: string;
    CredentialsFullFileName: string;
    Address: string;
    Port: string;
    WorkerQoS: Byte;
    GetWorkerCapabilitiesTimeout: Integer;
    GetListOfFontsTimeout: Integer;
    FindSubControlWorkerTimeout: Integer;
    FindSubControlTimeoutDiff: Integer;
    WorkerCapabilitiesSource: string; //enum
    LoadWorkerCapabilitiesCacheAction: string;
    SaveWorkerCapabilitiesCacheAction: string;

    TextRenderingOS: string;
    ListOfMultiValuePropertyNames: string;  //reserved - not used for now
    UseCompression: Boolean;
    CompressionAlgorithm: string; //enum

    //LzmaEndOfStream; //EOS
    //LzmaAlgorithm;
    //LzmaNumBenchMarkPasses;
    //LzmaDictionarySize;
    //LzmaMatchFinder;
    //LzmaLiteralContext;
    //LzmaLiteralPosBits;
    //LzmaPosBits;
    //LzmaFastBytes;
    LzmaOptions: TplLzmaOptions;

    VariablesForWorkers: string;
    ExtraDebuggingInfo: Boolean;
    EvaluateFileNameBeforeSending: Boolean;

    CustomFontProfiles: string; //Filename of a "font profiles" file
    UseFontProfiles: Boolean;

    MinExpectedWorkerCount: Integer;
    UpdateBackgroundInterval: Integer;
  end;

  TVarsForWorkers = record
    Names, Values, EvalBefore: string;
  end;

function CompressionAlgorithmsStrToType(AStr: string): TCompressionAlgorithm;
procedure FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList; var ADestLzmaOptions: TplLzmaOptions); overload;
function FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList): TplLzmaOptions; overload;
function FillInDefaultLzmaOptionsFromPluginProperties: TplLzmaOptions;

procedure GetPluginSettingsFromProperties(AProperties: TStringList; AListOfVars: string; out ADistPluginOptions: TDistPluginOptions; out AVarsForWorkers: TVarsForWorkers);

implementation


uses
  Math, ClickerExtraUtils, IdGlobal,
  DistFindSubControlPluginProperties, ClickerUtils;


function CompressionAlgorithmsStrToType(AStr: string): TCompressionAlgorithm;
var
  i: TCompressionAlgorithm;
begin
  Result := Low(TCompressionAlgorithm);
  for i := Low(TCompressionAlgorithm) to High(TCompressionAlgorithm) do
    if CCompressionAlgorithms[i] = AStr then
    begin
      Result := i;
      Exit;
    end;
end;


procedure FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList; var ADestLzmaOptions: TplLzmaOptions); overload;
begin
  ADestLzmaOptions.Algorithm := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaAlgorithmPropertyIndex], 2), 0), 2);
  ADestLzmaOptions.DictionarySize := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaDictionarySizePropertyIndex], 73000), 0), 100 * 1048576);       //73000 gives 2503 bytes from a 307KB BMP
  ADestLzmaOptions.EOS := APluginProperties.ValueFromIndex[CLzmaEndOfStreamPropertyIndex] = 'True'; //End Of Stream marker
  ADestLzmaOptions.MatchFinder := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaMatchFinderPropertyIndex], 1), 0), 1);
  ADestLzmaOptions.NumBenchMarkPasses := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaNumBenchMarkPassesPropertyIndex], 10), 0), 10);
  ADestLzmaOptions.Fb := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaFastBytesPropertyIndex], 273), 0), 273); //273 - ok
  ADestLzmaOptions.Lc := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaLiteralContextPropertyIndex], 3), 0), 8); //3 - ok
  ADestLzmaOptions.Lp := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaLiteralPosBitsPropertyIndex], 0), 0), 4);
  ADestLzmaOptions.Pb := Min(Max(StrToIntDef(APluginProperties.ValueFromIndex[CLzmaPosBitsPropertyIndex], 2), 0), 4);
end;


function FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList): TplLzmaOptions; overload;
begin
  Result.Algorithm := 2;
  FillInLzmaOptionsFromPluginProperties(APluginProperties, Result);
end;


function FillInDefaultLzmaOptionsFromPluginProperties: TplLzmaOptions;
begin
  Result.Algorithm := 2;
  Result.DictionarySize := 1048576;
  Result.EOS := False;
  Result.MatchFinder := 1;
  Result.NumBenchMarkPasses := 10;
  Result.Fb := 5;
  Result.Lc := 3;
  Result.Lp := 0;
  Result.Pb := 0;
end;


constructor TArchiveHandlers.Create;
begin
  inherited Create;
  FCompressionAlgorithm := caLzma;
  FOnAddToLogNoObj := nil;
end;


procedure TArchiveHandlers.DoOnAddToLogNoObj(AMsg: string);
begin
  if not Assigned(FOnAddToLogNoObj) then
    raise Exception.Create('OnAddToLogNoObj not assigned');

  FOnAddToLogNoObj(AMsg);
end;


procedure TArchiveHandlers.HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
var
  i: Integer;
  HashStr: string;
begin
  HashStr := ClickerExtraUtils.ComputeHash(AArchiveStream, AArchiveStreamSize); //An MD5 hash is 16 bytes long. This string is hex encoded to 32 bytes. For file integrity, MD5 should be ok.

  for i := 0 to 32 - 1 do
    AResultedHash[i] := Ord(HashStr[i + 1]);

  //DoOnAddToLogNoObj('========================== hash: ' + HashStr + '   ' + AAdditionalInfo);
end;


procedure TArchiveHandlers.HandleOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer); //compresses APlainStream, results AArchiveStream
begin
  if FCompressionAlgorithm = caLzma then
  begin
    //DoOnAddToLogNoObj('Compressing with Lzma ' + ComputeHash(APlainStream.Memory, APlainStream.Size));
    CompressStreamLzma(APlainStream, AArchiveStream, FLzmaOptions);
  end
  else
  begin
    //DoOnAddToLogNoObj('Compressing with ZLib ' + ComputeHash(APlainStream.Memory, APlainStream.Size));
    CompressStreamZLib(APlainStream, AArchiveStream);
  end;

  //DoOnAddToLogNoObj('Hash of compressed: ' + ComputeHash(AArchiveStream.Memory, AArchiveStream.Size));
end;


function TArchiveHandlers.HandleOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;  //decompresses AArchiveStream, results APlainStream
begin
  //FindControl results ////////////////////////////////////

  Result := True;
  try
    if CompressionAlgorithm = caLzma then
    begin
      //DoOnAddToLogNoObj('Decompressing with Lzma ' + ComputeHash(AArchiveStream.Memory, AArchiveStream.Size));
      ExtractStreamLzma(AArchiveStream, APlainStream)
    end
    else
    begin
      //DoOnAddToLogNoObj('Decompressing with ZLib ' + ComputeHash(AArchiveStream.Memory, AArchiveStream.Size));
      ExtractStreamZLib(AArchiveStream, APlainStream);
    end;

    //DoOnAddToLogNoObj('Hash of decompressed: ' + ComputeHash(APlainStream.Memory, APlainStream.Size));
  except
    on E: Exception do
    begin
      Result := False;
      DoOnAddToLogNoObj('Decompression error: ' + E.Message);
    end;
  end;
end;


procedure GetAllVarsAndValuesFromVarsForWorkers(AVarsForWorkers: string; AListOfAllVars: TStringList; out AAllNames, AAllValues, AAllEvalBefore: string);
var
  ListOfVarsForWorkers: TStringList;
  i: Integer;
  VarName, VarValue: string;
begin
  ListOfVarsForWorkers := TStringList.Create;
  try
    ListOfVarsForWorkers.LineBreak := #13#10;
    AVarsForWorkers := StringReplace(AVarsForWorkers, ', ', #13#10, [rfReplaceAll]);
    AVarsForWorkers := StringReplace(AVarsForWorkers, ',', #13#10, [rfReplaceAll]);

    ListOfVarsForWorkers.Text := AVarsForWorkers;

    AAllNames := '';
    AAllValues := '';
    AAllEvalBefore := '';
    for i := 0 to ListOfVarsForWorkers.Count - 1 do
    begin
      VarName := ListOfVarsForWorkers.Strings[i];
      VarValue := AListOfAllVars.Values[VarName];

      AAllNames := AAllNames + VarName + #4#5;
      AAllValues := AAllValues + VarValue + #4#5;
      AAllEvalBefore := AAllEvalBefore + '0' + #4#5;
    end;
  finally
    ListOfVarsForWorkers.Free;
  end;
end;


procedure GetPluginSettingsFromProperties(AProperties: TStringList; AListOfVars: string; out ADistPluginOptions: TDistPluginOptions; out AVarsForWorkers: TVarsForWorkers);
var
  ListOfVars: TStringList;
begin
  ADistPluginOptions.FindSubControlAction := AProperties.ValueFromIndex[CFindSubControlActionPropertyIndex];  //0
  ADistPluginOptions.CredentialsFullFileName := AProperties.ValueFromIndex[CCredentialsFullFileNamePropertyIndex]; //1 //for connection to broker
  ADistPluginOptions.Address := AProperties.ValueFromIndex[CAddressPropertyIndex]; //2
  ADistPluginOptions.Port := AProperties.ValueFromIndex[CPortPropertyIndex];  //3

  ListOfVars := TStringList.Create;
  try
    ListOfVars.LineBreak := #13#10;
    ListOfVars.Text := AListOfVars;
    ADistPluginOptions.CredentialsFullFileName := EvaluateAllReplacements(ListOfVars, ADistPluginOptions.CredentialsFullFileName);
    ADistPluginOptions.Address := EvaluateAllReplacements(ListOfVars, ADistPluginOptions.Address);
    ADistPluginOptions.Port := EvaluateAllReplacements(ListOfVars, ADistPluginOptions.Port);

    // AProperties.Values[CVariablesForWorkersPropertyName] is a comma-separated list of vars
    GetAllVarsAndValuesFromVarsForWorkers(AProperties.ValueFromIndex[CVariablesForWorkersPropertyIndex], ListOfVars, AVarsForWorkers.Names, AVarsForWorkers.Values, AVarsForWorkers.EvalBefore);
  finally
    ListOfVars.Free;
  end;

  ADistPluginOptions.WorkerQoS := StrToIntDef(AProperties.ValueFromIndex[CWorkerQoSPropertyIndex], 2);  //4
  ADistPluginOptions.WorkerQoS := Min(Max(ADistPluginOptions.WorkerQoS, 1), 2);  //limited to 1..2

  ADistPluginOptions.GetWorkerCapabilitiesTimeout := StrToIntDef(AProperties.ValueFromIndex[CGetWorkerCapabilitiesTimeoutPropertyIndex], 500);  //5
  ADistPluginOptions.GetWorkerCapabilitiesTimeout := Min(Max(ADistPluginOptions.GetWorkerCapabilitiesTimeout, 50), 60000);  //limited to 50..60000

  ADistPluginOptions.GetListOfFontsTimeout := StrToIntDef(AProperties.ValueFromIndex[CGetListOfFontsTimeoutPropertyIndex], 500);  //6
  ADistPluginOptions.GetListOfFontsTimeout := Min(Max(ADistPluginOptions.GetListOfFontsTimeout, 50), 60000);  //limited to 50..60000

  ADistPluginOptions.FindSubControlWorkerTimeout := StrToIntDef(AProperties.ValueFromIndex[CFindSubControlWorkerTimeoutPropertyIndex], 3500);  //7
  ADistPluginOptions.FindSubControlWorkerTimeout := Min(Max(ADistPluginOptions.FindSubControlWorkerTimeout, 50), 60000);  //limited to 50..60000

  ADistPluginOptions.FindSubControlTimeoutDiff := StrToIntDef(AProperties.ValueFromIndex[CFindSubControlTimeoutDiffPropertyIndex], 2500);  //8
  ADistPluginOptions.FindSubControlTimeoutDiff := Min(Max(ADistPluginOptions.FindSubControlTimeoutDiff, 1500), 60000);  //limited to 1500..60000

  ADistPluginOptions.WorkerCapabilitiesSource := AProperties.ValueFromIndex[CWorkerCapabilitiesSourcePropertyIndex];  //9
  ADistPluginOptions.LoadWorkerCapabilitiesCacheAction := AProperties.ValueFromIndex[CLoadWorkerCapabilitiesCacheActionPropertyIndex];  //10
  ADistPluginOptions.SaveWorkerCapabilitiesCacheAction := AProperties.ValueFromIndex[CSaveWorkerCapabilitiesCacheActionPropertyIndex];  //11
  ADistPluginOptions.TextRenderingOS := AProperties.ValueFromIndex[CTextRenderingOSPropertyIndex];  //12
  ADistPluginOptions.ListOfMultiValuePropertyNames := AProperties.ValueFromIndex[CListOfMultiValuePropertyNamesPropertyIndex];  //13
  ADistPluginOptions.UseCompression := AProperties.ValueFromIndex[CUseCompressionPropertyIndex] = 'True';  //14
  ADistPluginOptions.CompressionAlgorithm := AProperties.ValueFromIndex[CCompressionAlgorithmPropertyIndex];  //15

  //16..24

  //CVariablesForWorkersPropertyIndex  //25
  ADistPluginOptions.ExtraDebuggingInfo := AProperties.ValueFromIndex[CExtraDebuggingInfoPropertyIndex] = 'True';  //26
  ADistPluginOptions.EvaluateFileNameBeforeSending := AProperties.ValueFromIndex[CEvaluateFileNameBeforeSendingPropertyIndex] = 'True';  //27
  ADistPluginOptions.UseFontProfiles := AProperties.ValueFromIndex[CUseFontProfilesPropertyIndex] = CUseFontProfiles_FromCustom;  //29
  ADistPluginOptions.CustomFontProfiles := AProperties.ValueFromIndex[CCustomFontProfilesPropertyIndex];  //28

  ADistPluginOptions.MinExpectedWorkerCount := StrToIntDef(AProperties.ValueFromIndex[CMinExpectedWorkerCountPropertyIndex], 4);  //30
  ADistPluginOptions.MinExpectedWorkerCount := Min(Max(ADistPluginOptions.MinExpectedWorkerCount, 1), 50);  //limited to 1..50
  ADistPluginOptions.UpdateBackgroundInterval := StrToIntDef(AProperties.ValueFromIndex[CUpdateBackgroundIntervalPropertyIndex], 4);  //31
  ADistPluginOptions.UpdateBackgroundInterval := Min(Max(ADistPluginOptions.MinExpectedWorkerCount, -1), 10000);  //limited to 1..10000
end;

end.

