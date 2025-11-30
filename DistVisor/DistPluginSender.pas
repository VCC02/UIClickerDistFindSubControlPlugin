{
    Copyright (C) 2025 VCC
    creation date: 28 Nov 2025
    initial release date: 28 Nov 2025

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


unit DistPluginSender;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ClickerUtils;


procedure SendAllDistPlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness: string);  //expected '32' or '64' for Bitness
procedure SendAllServicePlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness: string);  //expected '32' or '64' for Bitness
procedure SetKeys;


implementation


uses
  Process, AsyncProcess;


function CreateSenderProcess(AExe, AParams, AExeInput: string; ATimeout: Integer = 8000): string;
var
  tk: QWord;
  TempStringList: TStringList;
  Proc: TAsyncProcess;
begin
  Result := '';
  if not FileExists(AExe) then
    raise Exception.Create('The executable file must exist at this path: "' + AExe + '"');

  Proc := TAsyncProcess.Create(nil);
  try
    Proc.Executable := AExe;
    Proc.Parameters.Text := FastReplace_45ToReturn(AParams);
    Proc.CurrentDirectory := ExtractFileDir(AExe);
    Proc.InheritHandles := True;
    Proc.Options := [poUsePipes, poStderrToOutPut{, poNewConsole, poNoConsole}];

    Proc.Execute;

    TempStringList := TStringList.Create;
    try
      TempStringList.LineBreak := #13#10;    //leave #13#10 here for conversion from #4#5.
      TempStringList.Text := FastReplace_45ToReturn(AExeInput);
      //replace #13#10 with #10 on Lin ???
      TempStringList.SaveToStream(Proc.Input);
    finally
      TempStringList.Free;
    end;

    tk := GetTickCount64;
    repeat
      Sleep(10);

      if GetTickCount64 - tk >= ATimeout then  //WaitForApp until timeout
      begin
        WriteLn('Timeout waiting for process to finish: ' + AExe);
        Break;
      end;
    until not Proc.Active;

    if Assigned(Proc.Output) and (Proc.Output <> nil) and (Proc.Output.NumBytesAvailable > 0) then     //read once more (in case the timeout stopped the reading)
    begin
      Proc.Output.Position := 0;
      SetLength(Result, Proc.Output.NumBytesAvailable);
      Proc.Output.Read(Result[1], Length(Result));
    end;
  finally
    Proc.Free;
  end;
end;


function SendPlugin(AUIClickerAddress, AUIClickerPort, ASenderApp, APluginToBeSent, APluginToBeSentDir, ATXKey, ATXIV, ANextKey, ANextIV: string; ADecryptionPluginName: string = ''): string;
var
  DistPath: string;
  PathToApp, AppParams, AppStdIn: string;
begin
  DistPath := ExtractFilePath(ParamStr(0)) + '..\';
  if (APluginToBeSentDir > '') and (APluginToBeSentDir[Length(APluginToBeSentDir)] <> '\') then
    APluginToBeSentDir := APluginToBeSentDir + '\';

  PathToApp := DistPath + ASenderApp + '\' + ASenderApp + '.exe';
  AppParams := '--ClickerClient' + #4#5 + DistPath + '..\UIClicker\ClickerClient\ClickerClient.dll' + #4#5 +
               '--PluginToBeSent' + #4#5 + DistPath + APluginToBeSentDir + 'lib\' + APluginToBeSent + '.dll' + #4#5 +
               '--PluginToBeSentDestName' + #4#5 + APluginToBeSent + '.dll' + #4#5 +
               '--UIClickerAddress' + #4#5 + AUIClickerAddress + #4#5 +
               '--UIClickerPort' + #4#5 + AUIClickerPort;

  if ADecryptionPluginName <> '' then
    AppParams := AppParams + #4#5 + '--DecryptionPluginName' + #4#5 + ADecryptionPluginName + '.dllarc|Mem:\' + ADecryptionPluginName + '.dll';

  AppStdIn := ATXKey + #4#5 +      //This is provided when DistInitialEnc.exe prints "Trasmission Key" on StdOut.
              ATXIV + #4#5 +       //This is provided when DistInitialEnc.exe prints "Trasmission IV" on StdOut.
              ANextKey + #4#5 +    //This is provided when DistInitialEnc.exe prints "Subsequent Key" on StdOut.
              ANextIV + #4#5;      //This is provided when DistInitialEnc.exe prints "Subsequent IV" on StdOut.

  try
    Result := CreateSenderProcess(PathToApp, AppParams, AppStdIn);
  except
    on E: Exception do
      Result := 'Ex on starting sender process: ' + E.Message;
  end;

  Result := FastReplace_ReturnTo45(Result);
end;


const
  CDecryptionPluginName = 'DistDec';
  CSenderApp_DistInitialEnc = 'DistInitialEnc';
  CSenderApp_DistEnc = 'DistEnc';

var
  InitialTransmissionKey: string = 'dummy_key';
  InitialTrasmissionIV: string = 'dummy_iv';
  InitialSubsequentKey: string = 'ABCDEF';
  InitialSubsequentIV: string = 'IV.IV';
  DistDecSubsequentKey: string = 'ABCDEFDistDec';
  DistDecSubsequentIV: string = 'IVDistDec.IV';
  FindSubControlKey: string = 'ABCDEFFindSubControl';
  FindSubControlIV: string = 'IVFindSubControl';


function Send_DistInitialDecDll_Via_DistInitialEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir: string): string;
begin
  Result := SendPlugin(AUIClickerAddress, AUIClickerPort, CSenderApp_DistInitialEnc, ABitnessDir + '\DistInitialDec', 'DistInitialDec', InitialTransmissionKey, InitialTrasmissionIV, InitialSubsequentKey, InitialSubsequentIV);
end;


function Send_DistDecDll_Via_DistInitialEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir: string): string;
begin
  Result := SendPlugin(AUIClickerAddress, AUIClickerPort, CSenderApp_DistInitialEnc, ABitnessDir + '\' + CDecryptionPluginName, CDecryptionPluginName, InitialSubsequentKey, InitialSubsequentIV, DistDecSubsequentKey, DistDecSubsequentIV, 'DistInitialDec');
end;


function Send_UIClickerDistFindSubControlDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir: string): string;
begin
  Result := SendPlugin(AUIClickerAddress, AUIClickerPort, CSenderApp_DistEnc, ABitnessDir + '\UIClickerDistFindSubControl', '', DistDecSubsequentKey, DistDecSubsequentIV, FindSubControlKey, FindSubControlIV, CDecryptionPluginName);
end;


function Send_PoolClientDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir: string): string;
begin
  Result := SendPlugin(AUIClickerAddress, AUIClickerPort, CSenderApp_DistEnc, ABitnessDir + '\PoolClient', 'PoolClient', DistDecSubsequentKey, DistDecSubsequentIV, FindSubControlKey, FindSubControlIV, CDecryptionPluginName);
end;


function Send_BrokerParamsDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir: string): string;
begin
  Result := SendPlugin(AUIClickerAddress, AUIClickerPort, CSenderApp_DistEnc, ABitnessDir + '\BrokerParams', 'BrokerParams', DistDecSubsequentKey, DistDecSubsequentIV, FindSubControlKey, FindSubControlIV, CDecryptionPluginName);
end;


function GetBitnessDir(AUIClickerDistBitness: string): string;
begin
  if AUIClickerDistBitness = '32' then
    Result := 'i386-win32'
  else
    if AUIClickerDistBitness = '64' then
      Result := 'x86_64-win64'
    else
      raise Exception.Create('Invalid bitness: "' + AUIClickerDistBitness + '".');
end;


procedure GetExpectedResponses(AUIClickerAddress, AUIClickerPort: string; out AExpectedResponse_Unencrypted, AExpectedResponse_Encrypted: string);
var
  Prefix, Operation: string;
begin
  Prefix := 'Trasmission Key=Trasmission IV=Subsequent Key=Subsequent IV=';
  Operation := 'Sending plugin to http://' + AUIClickerAddress + ':' + AUIClickerPort + '/';
  AExpectedResponse_Unencrypted := Prefix + 'Sending unencrypted...' + Operation + 'Response: OK';
  AExpectedResponse_Encrypted := Prefix + Operation + 'Using archive encryption..Response: OK';
end;


procedure SendDecPlugins(AUIClickerAddress, AUIClickerPort, AUIClickerDistBitness, ABitnessDir, AExpectedResponse_Unencrypted, AExpectedResponse_Encrypted: string);
var
  Res: string;
begin
  WriteLn('Sending DistInitialDecDll to ' + AUIClickerAddress + ':' + AUIClickerPort + '...');
  Res := Send_DistInitialDecDll_Via_DistInitialEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir);
  if Res <> AExpectedResponse_Unencrypted then
    raise Exception.Create('Error sending DistInitialDecDll: ' + Res);
  WriteLn('DistInitialDecDll sent.');
  WriteLn;

  WriteLn('Sending DistDecDll to ' + AUIClickerAddress + ':' + AUIClickerPort + '...');
  Res := Send_DistDecDll_Via_DistInitialEnc(AUIClickerAddress, AUIClickerPort, ABitnessDir);
  if Res <> AExpectedResponse_Encrypted then
    raise Exception.Create('Error sending DistDecDll: ' + Res);
  WriteLn('DistDecDll sent.');
  WriteLn;
end;


procedure SendAllDistPlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness: string);  //expected '32' or '64' for Bitness
var
  BitnessDir, Res: string;   //plugin dir bitness
  ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted: string;
begin
  BitnessDir := GetBitnessDir(AUIClickerBitness);
  GetExpectedResponses(AUIClickerAddress, AUIClickerPort, ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted);

  SendDecPlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness, BitnessDir, ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted);

  WriteLn('Sending UIClickerDistFindSubControlDll to ' + AUIClickerAddress + ':' + AUIClickerPort + '...');
  Res := Send_UIClickerDistFindSubControlDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, BitnessDir);
  if Res <> ExpectedResponse_Encrypted then
    raise Exception.Create('Error sending UIClickerDistFindSubControlDll: ' + Res);
  WriteLn('UIClickerDistFindSubControlDll sent.');
  WriteLn;

  WriteLn('Sending PoolClientDll to ' + AUIClickerAddress + ':' + AUIClickerPort + '...');
  Res := Send_PoolClientDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, BitnessDir);
  if Res <> ExpectedResponse_Encrypted then
    raise Exception.Create('Error sending PoolClientDll: ' + Res);
  WriteLn('PoolClientDll sent.');
  WriteLn;
end;


procedure SendAllServicePlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness: string);  //expected '32' or '64' for Bitness
var
  BitnessDir, Res: string;   //plugin dir bitness
  ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted: string;
begin
  BitnessDir := GetBitnessDir(AUIClickerBitness);
  GetExpectedResponses(AUIClickerAddress, AUIClickerPort, ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted);

  SendDecPlugins(AUIClickerAddress, AUIClickerPort, AUIClickerBitness, BitnessDir, ExpectedResponse_Unencrypted, ExpectedResponse_Encrypted);

  WriteLn('Sending BrokerParamsDll to ' + AUIClickerAddress + ':' + AUIClickerPort + '...');
  Res := Send_BrokerParamsDll_Via_DistEnc(AUIClickerAddress, AUIClickerPort, BitnessDir);
  if Res <> ExpectedResponse_Encrypted then
    raise Exception.Create('Error sending BrokerParamsDll: ' + Res);
  WriteLn('BrokerParamsDll sent.');
  WriteLn;
end;


procedure SetKeys;
begin
  Randomize;
  InitialTransmissionKey := 'A new key every time ' + IntToHex(GetTickCount64) + IntToStr(Random(MaxInt));
  Sleep(33);

  Randomize;
  InitialTrasmissionIV := 'A new IV every time ' + IntToHex(GetTickCount64) + IntToStr(Random(MaxInt));
  Sleep(33);

  Randomize;
  InitialSubsequentKey := 'A new subsequent key every time ' + IntToHex(GetTickCount64) + IntToStr(Random(MaxInt));
  Sleep(33);

  Randomize;
  InitialSubsequentIV := 'A new subsequent IV every time ' + IntToHex(GetTickCount64) + IntToStr(Random(MaxInt));
end;

end.

