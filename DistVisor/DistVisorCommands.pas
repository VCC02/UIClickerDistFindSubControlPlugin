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

function StartWorkerPoolManager: string;
function StartMonitoringUIClicker: string;


const
  CDVCmd_SendDistPlugins = 'SendDistPlugins';
  CDVCmd_SendServicePlugins = 'SendServicePlugins';
  CDVCmd_StartWorkerPoolManager = 'StartWorkerPoolManager';  //Not sure if this has to be exposed as an HTTP command. Maybe for debugging only.
  CDVCmd_StartMonitoringUIClicker = 'StartMonitoringUIClicker'; //A safe instance of UIClicker, which is used to start the "Service" instance, in case it crashes. This instance doesn't have to be UIClicker, but it's already available.

  CMonitoringPort = '54400';  //monitoring UIClicker instance
  CMonitoringExtraCaption = 'Monitor';

  CServiceExtraCaption = 'Service';

var
  DistUIClickerAddress: string;
  DistUIClickerPort: string;
  DistUIClickerBitness: string;

  ServiceUIClickerAddress: string;
  ServiceUIClickerPort: string;
  ServiceUIClickerBitness: string;


implementation


uses
  DistPluginSender, ClickerUtils, ClickerActionsClient, ClickerActionProperties;


var
  HTTPServer: TIdHTTPServer;
  ServerHandlers: TCmdHandlers;


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


function StartWorkerPoolManager: string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\..\UIClickerDistFindSubControlPlugin\WorkerPoolManager\WorkerPoolManager.exe';

  Res := ExecuteExecAppAction('http://' + ServiceUIClickerAddress + ':' + ServiceUIClickerPort + '/', ExecApp, 'Start WorkerPoolManager', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


function StartMonitoringUIClicker: string;
var
  ExecApp: TClkExecAppOptions;
  Res: string;
begin
  GetDefaultPropertyValues_ExecApp(ExecApp);
  ExecApp.PathToApp := '$AppDir$\UIClicker.exe';
  ExecApp.ListOfParams := StringReplace('--SetExecMode Server --AutoSwitchToExecTab Yes --ServerPort ' + CMonitoringPort + ' ' + '--ExtraCaption ' + CMonitoringExtraCaption + ' --AddAppArgsToLog Yes', ' ', #4#5, [rfReplaceAll]);

  Res := ExecuteExecAppAction('http://' + ServiceUIClickerAddress + ':' + ServiceUIClickerPort + '/', ExecApp, 'Start Monitoring UIClicker', 1000, False); //CallAppProcMsg is set to False, because is called from a server module thread.
  Result := Get_ExecAction_Err_FromExecutionResult(Res);
end;


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
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;

  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type

  if Cmd = '/' + CDVCmd_SendDistPlugins then
  begin
    SendAllDistPlugins(DistUIClickerAddress, DistUIClickerPort, DistUIClickerBitness);
    AResponseInfo.ContentText := 'Done';
    Exit;
  end;

  if Cmd = '/' + CDVCmd_SendServicePlugins then
  begin
    SendAllServicePlugins(ServiceUIClickerAddress, ServiceUIClickerPort, ServiceUIClickerBitness);
    AResponseInfo.ContentText := 'Done';
    Exit;
  end;

  {$IFDEF TestBuild}
    if Cmd = '/' + CDVCmd_StartWorkerPoolManager then   //This command is more like an internal command, under TestBuild.
    begin
      StartWorkerPoolManager;
      AResponseInfo.ContentText := 'Done';
      Exit;
    end;

    if Cmd = '/' + CDVCmd_StartMonitoringUIClicker then   //This command is more like an internal command, under TestBuild.
    begin
      StartMonitoringUIClicker;
      AResponseInfo.ContentText := 'Done';
      Exit;
    end;
  {$ENDIF}
end;

end.

