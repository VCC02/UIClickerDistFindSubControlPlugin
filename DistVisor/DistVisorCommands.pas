{
    Copyright (C) 2025 VCC
    creation date: 30 Nov 2025
    initial release date: 30 Nov 2025

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


unit DistVisorCommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  IdHTTPServer, IdCustomHTTPServer, IdContext, IdGlobal {, IdSync, IdSchedulerOfThreadPool};


type
  TCmdHandlers = class(TObject)
  private
    procedure IdHTTPServer1Connect(AContext: TIdContext);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  end;


procedure CreateServerModule;
procedure DestroyServerModule;

const
  CDVCmd_AddMachine = 'AddMachine';
  CDVCmd_RemoveMachine = 'RemoveMachine';

  CDVCmdMachineKindParam = 'MachineKind';
  CDVCmdMachineAddressParam = 'MachineAddress';
  CDVCmdMonitoringUIClickerPortParam = 'MonitoringUIClickerPort';
  CDVCmdServiceUIClickerPortParam = 'ServiceUIClickerPort';
  CDVCmdToolPortParam = 'ToolPort';  //When MachineKind is 0, this is Dist port. When MachineKind is 1 or 2, this is igonred.


// Examples:
//  http://127.0.0.1:54000/AddMachine?MachineKind=0&MachineAddress=10.0.3.7&MonitoringUIClickerPort=54400&ServiceUIClickerPort=55444&ToolPort=5444
//  http://127.0.0.1:54000/RemoveMachine?MachineKind=0&MachineAddress=10.0.3.7


implementation


uses
  DistPluginSender, DistVisorFSM;


var
  HTTPServer: TIdHTTPServer;
  ServerHandlers: TCmdHandlers;


procedure CreateServerModule;
begin
  if HTTPServer <> nil then
  begin
    WriteLn('The HTTPServer already exists.');
    Exit;
  end;

  ServerHandlers := TCmdHandlers.Create;

  HTTPServer := TIdHTTPServer.Create;
  HTTPServer.OnConnect := @ServerHandlers.IdHTTPServer1Connect;
  HTTPServer.OnCommandGet := @ServerHandlers.IdHTTPServer1CommandGet;

  try
    HTTPServer.DefaultPort := 54000;
    HTTPServer.Active := True;
    WriteLn('Listening on port ' + IntToStr(HTTPServer.DefaultPort));
  except
    on E: Exception do
      WriteLn('Can''t listen on port ' + IntToStr(HTTPServer.DefaultPort) + '. ' + E.Message);
  end;
end;


procedure DestroyServerModule;
var
  tk: QWord;
begin
  HTTPServer.Active := False;
  tk := GetTickCount64;
  repeat
    Sleep(10);
  until GetTickCount64 - tk > 1000;

  FreeAndNil(HTTPServer);
  FreeAndNil(ServerHandlers);
end;


procedure TCmdHandlers.IdHTTPServer1Connect(AContext: TIdContext);
begin
  AContext.Connection.Socket.ReadTimeout := 60000;   //if no bytes are received in 1min, then close the connection
  AContext.Connection.Socket.MaxLineLength := 1048576;
end;


procedure TCmdHandlers.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Cmd: string;
  MachineKind: TMachineKind;
  MachineKindInt: Integer;
  MachineAddress: string;
  MonitoringUIClickerPort: string;
  ServiceUIClickerPort: string;
  ToolPort: string;
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + CDVCmd_AddMachine then
  begin
    MachineKindInt := StrToIntDef(ARequestInfo.Params.Values[CDVCmdMachineKindParam], -1);
    MachineAddress := ARequestInfo.Params.Values[CDVCmdMachineAddressParam];
    MonitoringUIClickerPort := ARequestInfo.Params.Values[CDVCmdMonitoringUIClickerPortParam];
    ServiceUIClickerPort := ARequestInfo.Params.Values[CDVCmdServiceUIClickerPortParam];
    ToolPort := ARequestInfo.Params.Values[CDVCmdToolPortParam];

    if not (MachineKindInt in [0, 1, 2]) then    //mkDist, mkWorker, mkWPM
    begin
      AResponseInfo.ContentText := 'Valid values for MachineKind are: 0, 1 and 2.';
      Exit;
    end;

    MachineKind := TMachineKind(MachineKindInt);

    if MachineAddress = '' then    //More validations are required here. E.g. should be a local IP address only.
    begin
      AResponseInfo.ContentText := 'MachineAddress must not be empty.';
      Exit;
    end;

    if StrToIntDef(MonitoringUIClickerPort, 0) = 0 then
    begin
      AResponseInfo.ContentText := 'MonitoringUIClickerPort must be a value between 0 and 65535.';
      Exit;
    end;

    if StrToIntDef(ServiceUIClickerPort, 0) = 0 then
    begin
      AResponseInfo.ContentText := 'ServiceUIClickerPort must be a value between 0 and 65535.';
      Exit;
    end;

    if StrToIntDef(ToolPort, 0) = 0 then
    begin
      AResponseInfo.ContentText := 'ToolPort must be a value between 0 and 65535.';
      Exit;
    end;

    try
      AddMachine(MachineKind, MachineAddress, MonitoringUIClickerPort, ServiceUIClickerPort, ToolPort);
      AResponseInfo.ContentText := 'Done';
      WriteLn('Machine added: ' + MachineAddress + '  MachineKind: ' + IntToStr(MachineKindInt));
    except
      on E: Exception do
        AResponseInfo.ContentText := 'Can''t add machine to list. ' + E.Message;
    end;

    Exit;
  end;

  if Cmd = '/' + CDVCmd_RemoveMachine then
  begin
    MachineAddress := ARequestInfo.Params.Values[CDVCmdMachineAddressParam];

    if MachineAddress = '' then    //More validations are required here. E.g. should be a local IP address only.
    begin
      AResponseInfo.ContentText := 'MachineAddress must not be empty.';
      Exit;
    end;

    try
      RemoveMachine(MachineAddress);
      AResponseInfo.ContentText := 'Done';
      WriteLn('Machine removed: ' + MachineAddress);
    except
      on E: Exception do
        AResponseInfo.ContentText := 'Can''t remove machine from list. ' + E.Message;
    end;

    Exit;
  end;

  {$IFDEF TestBuild}
    //other commands
  {$ENDIF}
end;

end.

