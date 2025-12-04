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


type
  TDistVisorFSM = (SInit,
                   SCheckForMonitoringUIClicker, SStartMonitoringUIClicker, SWaitForMonitoringUIClicker,
                   SCheckForServiceUIClicker, SStartServiceUIClicker, SWaitForServiceUIClicker,
                   SCheckForWPM, SStartWPM, SWaitForWPM, //WPM = WorkePoolManager
                   SSendPlugins
                   );

var
  State: TDistVisorFSM = SInit;
  NextState: TDistVisorFSM = SCheckForMonitoringUIClicker;

  AutosendPluginsOnStartup: Boolean;


procedure ExecuteFSM;

implementation


uses
  ClickerActionsClient, DistVisorCommands;


var
  MonitoringUIClickerIsRunning: Boolean;
  ServiceUIClickerIsRunning: Boolean;
  WPMIsRunning: Boolean;

  MonitoringUIClicker_tk: QWord;
  ServiceUIClicker_tk: QWord;
  WPM_tk: QWord;

  StartMonitoringUIClickerResult: string;


procedure ExecuteFSM_Part1;
begin
  case State of
    SInit:
    begin
      MonitoringUIClickerIsRunning := False;
      ServiceUIClickerIsRunning := False;
      WPMIsRunning := False;
    end;

    SCheckForMonitoringUIClicker:      //DistVisor expects that ServiceUIClicker is started automatically by the OS or a startup app/script.
    begin
      MonitoringUIClickerIsRunning := TestConnection('http://' + ServiceUIClickerAddress + ':' + CMonitoringPort + '/', False) = CREResp_ConnectionOK;
      if not MonitoringUIClickerIsRunning then
        WriteLn('The MonitoringUIClicker is not running. Attempting to start it.');
    end;

    SStartMonitoringUIClicker:
    begin
      MonitoringUIClicker_tk := GetTickCount64;
      StartMonitoringUIClickerResult := StartMonitoringUIClicker; //sends a command to the Service UIClicker, to start the Monitoring one.
    end;

    SWaitForMonitoringUIClicker:
      MonitoringUIClickerIsRunning := TestConnection('http://' + ServiceUIClickerAddress + ':' + CMonitoringPort + '/', False) = CREResp_ConnectionOK;

    SCheckForServiceUIClicker:
      ;

    SStartServiceUIClicker:
    begin
      ServiceUIClicker_tk := GetTickCount64;
    end;

    SWaitForServiceUIClicker:
      ;

    SCheckForWPM:
      ;

    SStartWPM:
    begin
      WPM_tk := GetTickCount64;
    end;

    SWaitForWPM:
      ;

    SSendPlugins:
      ;
  end;
end;


procedure ExecuteFSM_Part2;
begin
  case State of
    SInit:
      NextState := SCheckForMonitoringUIClicker;

    SCheckForMonitoringUIClicker:
      if MonitoringUIClickerIsRunning then
        NextState := SCheckForServiceUIClicker  //next tool
      else
        NextState := SStartMonitoringUIClicker;      //maybe report an error if entering here too often

    SStartMonitoringUIClicker:
    begin
      if StartMonitoringUIClickerResult = '' then
        NextState := SWaitForMonitoringUIClicker
      else
      begin
        WriteLn('Error starting MonitoringUIClicker: "' + StartMonitoringUIClickerResult + '".');
        NextState := SCheckForServiceUIClicker;
      end;
    end;

    SWaitForMonitoringUIClicker:
      if MonitoringUIClickerIsRunning then
        NextState := SCheckForServiceUIClicker
      else
        if MonitoringUIClicker_tk < 10000 then
        begin
          NextState := SWaitForMonitoringUIClicker;
          WriteLn('Error: Timeout (' + IntToStr(MonitoringUIClicker_tk) + ' ms) waiting for MonitoringUIClicker to become available.');
        end
        else
          NextState := SCheckForServiceUIClicker;

    SCheckForServiceUIClicker:
      ;

    SStartServiceUIClicker:
      ;

    SWaitForServiceUIClicker:
      ;

    SCheckForWPM:
      ;

    SStartWPM:
      ;

    SWaitForWPM:
      ;               //somewhere, enter AutosendPluginsOnStartup

    SSendPlugins:
      ;
  end;
end;


procedure ExecuteFSM;
begin
  ExecuteFSM_Part1;
  ExecuteFSM_Part2;
  State := NextState;
end;

end.

