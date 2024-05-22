{
    Copyright (C) 2024 VCC
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

  CTopicName_AppToWorker_FindSubControl = 'AppToWorker_FindSubControl';    //request to workers
  CTopicName_WorkerToApp_FindSubControl = 'WorkerToApp_FindSubControl';    //response from workers

  CCompressionAlgorithms: array[TCompressionAlgorithm] of string = ('Zlib', 'Lzma');

  CProtocolParam_Name = 'Name';
  CProtocolParam_OS = 'OS';
  CProtocolParam_FileCache = 'FileCache';

  CProtocolParam_ImageSourceRawContent = 'ImageSourceRawContent';
  CProtocolParam_UsingCompression = 'UsingCompression';
  CProtocolParam_CompressionAlgorithm = 'CompressionAlgorithm';

  CBackgroundFileNameInArchive = '_:\Background.bmp'; //a name which cannot be used as OS's file system


type
  TOnAddToLogNoObj = procedure(AMsg: string);

  TArchiveHandlers = class
  private
    FCompressionAlgorithm: TCompressionAlgorithm;
    FPluginProperties: TStringList;  //set from outside
    FLzmaOptions: TplLzmaOptions;
    FOnAddToLogNoObj: TOnAddToLogNoObj;

    procedure DoOnAddToLogNoObj(AMsg: string);
  public
    constructor Create;

    procedure HandleOnComputeArchiveHash(AArchiveStream: Pointer; AArchiveStreamSize: Int64; var AResultedHash: TArr32OfByte; AAdditionalInfo: string = '');
    procedure HandleOnCompress(APlainStream, AArchiveStream: TMemoryStream; ACompressionLevel: Integer);
    function HandleOnDecompress(AArchiveStream, APlainStream: TMemoryStream): Boolean;

    property CompressionAlgorithm: TCompressionAlgorithm read FCompressionAlgorithm write FCompressionAlgorithm;
    property PluginProperties: TStringList write FPluginProperties;
    property LzmaOptions: TplLzmaOptions write FLzmaOptions;

    property OnAddToLogNoObj: TOnAddToLogNoObj write FOnAddToLogNoObj;
  end;


function CompressionAlgorithmsStrToType(AStr: string): TCompressionAlgorithm;
procedure FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList; var ADestLzmaOptions: TplLzmaOptions); overload;
function FillInLzmaOptionsFromPluginProperties(APluginProperties: TStringList): TplLzmaOptions; overload;


implementation


uses
  Math, ClickerExtraUtils, IdGlobal,
  DistFindSubControlPluginProperties;


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
    FillInLzmaOptionsFromPluginProperties(FPluginProperties, FLzmaOptions);
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


end.
