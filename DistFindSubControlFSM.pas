{
    Copyright (C) 2026 VCC
    creation date: 24 Aug 2026  - code moved here from UIClickerDistFindSubControl.ppr
    initial release date: 24 Aug 2026

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


unit DistFindSubControlFSM;

{$H+}
{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  ClickerUtils,
  DistFindSubControlCommonConsts;


type
  TFSM = (SInit, SConnectToBroker, SWaitForConAck,
          SSubscribeToResponses, SWaitForResponsesSubAck,   //Responses can be capabilities, lists of missing files, processing results
          SRequestCapabilities, SWaitForCapabilitiesResponse,
          SGetFindSubControlActionContent,
          SPublishBackgroundImageToAllWorkers, SWaitForBackgroundImageResults,
          SPublishFindSubControlToAllWorkers, SWaitForFindSubControlResults,  //in addition to FindSubControl action details, the publish contains a list of assigned work info for every worker, so they know what exactly to work on
          SGetListOfFonts, SWaitForListOfFonts,
          SUpdateBackgroundImageToAllWorkers, SWaitForUpdateBackgroundImageResults,
          SUpdateCache, SLoadCache,
          SUnsubscribeFromResponses, SWaitForResponsesUnsubAck,
          SDisconnectFromBroker,
          SDone
         );

const
  CFSMStr: array[TFSM] of string = (
          'SInit', 'SConnectToBroker', 'SWaitForConAck',
          'SSubscribeToResponses', 'SWaitForResponsesSubAck',
          'SRequestCapabilities', 'SWaitForCapabilitiesResponse',
          'SGetFindSubControlActionContent',
          'SPublishBackgroundImageToAllWorkers', 'SWaitForBackgroundImageResults',
          'SPublishFindSubControlToAllWorkers', 'SWaitForFindSubControlResults',
          'SGetListOfFonts', 'SWaitForListOfFonts',
          'SUpdateBackgroundImageToAllWorkers', 'SWaitForUpdateBackgroundImageResults',
          'SUpdateCache', 'SLoadCache',
          'SUnsubscribeFromResponses', 'SWaitForResponsesUnsubAck',
          'SDisconnectFromBroker',
          'SDone'
          );

 CNoWorkerFoundSubControlErr = 'None of the responding workers found the SubControl.';
 CNoWorkerMatchesOSErr = 'None of the responding workers matches the selected target OS.';

type
  TOnConnectToBroker = function: Boolean of object;
  TOnGetAllWorkersCount = function: Integer of object;
  TOnSaveWorkerCapabilitiesCache = function: Boolean of object;
  TOnLoadWorkerCapabilitiesCache = function: Boolean of object;
  TOnDisconnectFromBroker = function: Boolean of object;

  TDistFSM = class
  private
    FState: TFSM;
    NextState: TFSM;
    FFSMDone: Boolean;
    FSMError: string;

    FWillWaitForUpdateBackgroundResponses: Boolean;
    FUpdateBackgroundTk: Int64;

    FSendBackgroundToAll_tk: QWord; //used for timeouts
    ConnectToBroker_tk: QWord; //used for timeouts
    SubscribeToResponses_tk: QWord; //used for timeouts
    UnsubscribeFromResponses_tk: QWord; //used for timeouts
    GetCapabilities_tk: QWord; //used for timeouts
    //SendBackgroundToSome_tk: QWord; //used for timeouts
    SendFindSubControl_tk: QWord; //used for timeouts
    GetListOfFonts_tk: QWord; //used for timeouts

    FConnectedSucessfully: Boolean;
    FConAckReceived: Boolean;
    FSubAckReceived: Boolean;
    FUnsubAckReceived: Boolean;
    FSubscribedSucessfully: Boolean;
    FUnsubscribedSucessfully: Boolean;
    FDisconnectedSuccessfully: Boolean;

    FVerbLevel: Integer;
    FDistPluginOptions: TDistPluginOptions;
    FWaitingForWorkerCapabilities: Boolean;
    FWaitingForWorkerFonts: Boolean;

    FWorkerRespondedCountBG: Word; //Background
    FWorkerRespondedCountFS: Word; //FindSubControl
    FWorkerRespondedCountLoF: Word; //ListOfFonts
    FAtLeastOneWorkerFoundTheSubControl: Boolean;

    FOnAddToLog: TOnAddToLog;
    FOnConnectToBroker: TOnConnectToBroker;
    FOnGetAllWorkersCount: TOnGetAllWorkersCount;
    FOnSaveWorkerCapabilitiesCache: TOnSaveWorkerCapabilitiesCache;
    FOnLoadWorkerCapabilitiesCache: TOnLoadWorkerCapabilitiesCache;
    FOnDisconnectFromBroker: TOnDisconnectFromBroker;

    procedure ExecFSM_Part1;
    procedure ExecFSM_Part2;

    procedure DoOnAddToLog(s: string);
    function DoOnConnectToBroker: Boolean;
    function DoOnGetAllWorkersCount: Integer;
    function DoOnSaveWorkerCapabilitiesCache: Boolean;
    function DoOnLoadWorkerCapabilitiesCache: Boolean;
    function DoOnDisconnectFromBroker: Boolean;
  public
    constructor Create;
    function ExecFSM: string;
    procedure ResetFSM;

    property State: TFSM read FState;
    property FSMDone: Boolean read FFSMDone;

    property WillWaitForUpdateBackgroundResponses: Boolean read FWillWaitForUpdateBackgroundResponses write FWillWaitForUpdateBackgroundResponses;
    property UpdateBackgroundTk: Int64 read FUpdateBackgroundTk write FUpdateBackgroundTk;
    property SendBackgroundToAll_tk: QWord read FSendBackgroundToAll_tk write FSendBackgroundToAll_tk;

    //property ConnectedSucessfully: Boolean read FConnectedSucessfully write FConnectedSucessfully;
    property ConAckReceived: Boolean read FConAckReceived write FConAckReceived;
    property SubAckReceived: Boolean read FSubAckReceived write FSubAckReceived;
    property UnsubAckReceived: Boolean read FUnsubAckReceived write FUnsubAckReceived;
    //property SubscribedSucessfully: Boolean read FSubscribedSucessfully write FSubscribedSucessfully;
    //property UnsubscribedSucessfully: Boolean read FUnsubscribedSucessfully write FUnsubscribedSucessfully;
    property DisconnectedSuccessfully: Boolean read FDisconnectedSuccessfully;

    property VerbLevel: Integer write FVerbLevel;
    property DistPluginOptions: TDistPluginOptions write FDistPluginOptions;
    property WaitingForWorkerCapabilities: Boolean read FWaitingForWorkerCapabilities write FWaitingForWorkerCapabilities; //must be assigned to the address of some "FWaitingForWorkerCapabilities" boolean field.
    property WaitingForWorkerFonts: Boolean read FWaitingForWorkerFonts write FWaitingForWorkerFonts;

    property WorkerRespondedCountBG: Word read FWorkerRespondedCountBG write FWorkerRespondedCountBG; //Background
    property WorkerRespondedCountFS: Word read FWorkerRespondedCountFS write FWorkerRespondedCountFS; //FindSubControl
    property WorkerRespondedCountLoF: Word read FWorkerRespondedCountLoF write FWorkerRespondedCountLoF; //ListOfFonts
    property AtLeastOneWorkerFoundTheSubControl: Boolean write FAtLeastOneWorkerFoundTheSubControl;

    property OnAddToLog: TOnAddToLog write FOnAddToLog;
    property OnConnectToBroker: TOnConnectToBroker write FOnConnectToBroker;
    property OnGetAllWorkersCount: TOnGetAllWorkersCount write FOnGetAllWorkersCount;
    property OnSaveWorkerCapabilitiesCache: TOnSaveWorkerCapabilitiesCache write FOnSaveWorkerCapabilitiesCache;
    property OnLoadWorkerCapabilitiesCache: TOnLoadWorkerCapabilitiesCache write FOnLoadWorkerCapabilitiesCache;
    property OnDisconnectFromBroker: TOnDisconnectFromBroker write FOnDisconnectFromBroker;
  end;


implementation


uses
  ClickerActionPlugins, DistFindSubControlPluginProperties,
  MQTTClient, Math;


constructor TDistFSM.Create;
begin
  inherited Create;
  FFSMDone := False;
  FSMError := '';
  FWaitingForWorkerCapabilities := False;
  FWaitingForWorkerFonts := False;
  FWorkerRespondedCountBG := 0;
  FWorkerRespondedCountFS := 0;
  FWorkerRespondedCountLoF := 0;

  FOnAddToLog := nil;
  FOnConnectToBroker := nil;
  FOnGetAllWorkersCount := nil;
  FOnSaveWorkerCapabilitiesCache := nil;
  FOnLoadWorkerCapabilitiesCache := nil;
  FOnDisconnectFromBroker := nil;

  FState := SInit;
end;


procedure TDistFSM.ResetFSM;
begin
  FFSMDone := True;
end;


procedure TDistFSM.DoOnAddToLog(s: string);
begin
  if Assigned(FOnAddToLog) then
    FOnAddToLog(s)
  else
    raise Exception.Create('OnAddToLog not assigned.');
end;


function TDistFSM.DoOnConnectToBroker: Boolean;
begin
  if Assigned(FOnConnectToBroker) then
    Result := FOnConnectToBroker()
  else
    raise Exception.Create('OnConnectToBroker not assigned.');
end;


function TDistFSM.DoOnGetAllWorkersCount: Integer;
begin
  if Assigned(FOnGetAllWorkersCount) then
    Result := FOnGetAllWorkersCount()
  else
    raise Exception.Create('OnGetAllWorkersCount not assigned.');
end;


function TDistFSM.DoOnSaveWorkerCapabilitiesCache: Boolean;
begin
  if Assigned(FOnSaveWorkerCapabilitiesCache) then
    Result := FOnSaveWorkerCapabilitiesCache()
  else
    raise Exception.Create('OnSaveWorkerCapabilitiesCache not assigned.');
end;


function TDistFSM.DoOnLoadWorkerCapabilitiesCache: Boolean;
begin
  if Assigned(FOnLoadWorkerCapabilitiesCache) then
    Result := FOnLoadWorkerCapabilitiesCache()
  else
    raise Exception.Create('OnLoadWorkerCapabilitiesCache not assigned.');
end;


function TDistFSM.DoOnDisconnectFromBroker: Boolean;
begin
  if Assigned(FOnDisconnectFromBroker) then
    Result := FOnDisconnectFromBroker()
  else
    raise Exception.Create('OnDisconnectFromBroker not assigned.');
end;


procedure TDistFSM.ExecFSM_Part1;
var
  i: Integer;
begin
  case FState of
    SInit:
    begin
      FFSMDone := False;
      FConnectedSucessfully := False;
      FConAckReceived := False;
      FSubAckReceived := False;
      FUnsubAckReceived := False;
      FSubscribedSucessfully := False;
      FUnsubscribedSucessfully := False;
      FDisconnectedSuccessfully := False;
      FAtLeastOneWorkerFoundTheSubControl := False;

      //ConnectToBroker_tk := GetTickCount64;
      //SubscribeToResponses_tk := GetTickCount64;
      //UnsubscribeFromResponses_tk := GetTickCount64;
      GetCapabilities_tk := GetTickCount64;
      FSendBackgroundToAll_tk := GetTickCount64;
      //SendBackgroundToSome_tk := GetTickCount64;
      SendFindSubControl_tk := GetTickCount64;
      GetListOfFonts_tk := GetTickCount64;
    end;

    SConnectToBroker:
    begin
      if FVerbLevel < 2 then
        DoOnAddToLog('Connecting to broker.');

      FConnectedSucessfully := DoOnConnectToBroker;

      if FConnectedSucessfully then
      begin
        ConnectToBroker_tk := GetTickCount64;
        DbgPoint('Connected to broker.', '')
      end
      else
      begin
        FSMError := 'Can''t connect to broker. ' + FDistPluginOptions.Address + ':' + FDistPluginOptions.Port;
        DoOnAddToLog(FSMError);
      end;
    end;

    SWaitForConAck:
    begin
      // FConAckReceived is set by handler
    end;

    SSubscribeToResponses:
    begin
      FSubscribedSucessfully := MQTT_SUBSCRIBE(0, 0);     //subscribing with CTopicNameResult
      if not FSubscribedSucessfully then
      begin
        FSMError := 'Can''t subscribe for responses.';
        DoOnAddToLog(FSMError);
      end
      else
        SubscribeToResponses_tk := GetTickCount64;
    end;

    SWaitForResponsesSubAck:   //Responses can be capabilities, lists of missing files, processing results
    begin
      //FSubAckReceived is set by handler
    end;

    SRequestCapabilities:
    begin
      FWaitingForWorkerCapabilities := True;    //reset below
      if not MQTT_PUBLISH(0, CCallbackID_GetCapabilities, FDistPluginOptions.WorkerQoS) then  //notice CallbackID = 0      (i.e. 'GetCapabilities')
      begin
        FSMError := 'Can''t send GetCapabilities request to broker.';
        DoOnAddToLog(FSMError);
      end
      else
        GetCapabilities_tk := GetTickCount64;
    end;

    SWaitForCapabilitiesResponse:
    begin
      // Every worker has to respond with what capabilities is configured.
      // Some can process Text, others Bmp files, others Primitives.
      // If the FindSubControl action has only two text profiles, it makes sense to distribute the search to both workers.
      // That means, all workers should be able to process anything. Anyway, since all have UIClicker installed, all of them should be able to do all.
      // The only real difference between workers would be the OS (Win vs Lin), where each may have different lists of fonts and different GPU rendering settings.
      // So, the best option would be to evenly split (as best as possible) all types of FindSubControls (Txt, Bmp, Pmtv) across all workers.

      // For efficient use of the network, it would be better if all workers can respond here with the list of the bmp/pmtv files (including backgrounds),
      // along with their MD5 hashes, so that the plugin would know what files to send to each worker.
      // Since all backgrounds may have the same name ('Background.bmp'), this bmp should be renamed to 'Background_<MD5>.bmp'.
      // The backgrounds are likely to change, but some of them, like menus or various window parts may stay the same.
      // The worker should be configurable about the cache size.
    end;

    SGetFindSubControlActionContent:
    begin
      //This is where GetFindControlActionProperties is called (unfortunately in FSM's loop) instead of this case item.
    end;

    SPublishBackgroundImageToAllWorkers:
    begin
      //AtLeastOneWorkerFoundTheSubControl := False;   //not sure why this was here (maybe a copy-paste)
      FWorkerRespondedCountBG := 0;

      if FVerbLevel < 2 then
        DoOnAddToLog('Sending background image to broker...');

      if not MQTT_PUBLISH(0, CCallbackID_SendBackgroundToAll, FDistPluginOptions.WorkerQoS) then   //notice CallbackID = 1     (i.e. 'SendBackgroundToAll')
      begin
        FSMError := 'Can''t send background image to all workers.';
        DoOnAddToLog(FSMError);
      end
      else
        FSendBackgroundToAll_tk := GetTickCount64;

      if FVerbLevel < 2 then
        DoOnAddToLog('Done sending background image.');
    end;

    SWaitForBackgroundImageResults:
    begin

    end;

    SPublishFindSubControlToAllWorkers:
    begin
      FAtLeastOneWorkerFoundTheSubControl := False;
      FWorkerRespondedCountFS := 0;
      FWillWaitForUpdateBackgroundResponses := False;

      if FVerbLevel < 2 then
        DoOnAddToLog('Sending FindSubControl settings to broker...');

      for i := 0 to DoOnGetAllWorkersCount - 1 do
      begin
        if not MQTT_PUBLISH(0, CCallbackID_SendFindSubControl or (i shl 8), FDistPluginOptions.WorkerQoS) then   //notice CallbackID = 3     (i.e. 'FindSubControl')
        begin
          FSMError := 'Can''t send FindSubControl settings to broker for worker[' + IntToStr(i) + '].';
          DoOnAddToLog(FSMError);
        end
        else
          SendFindSubControl_tk := GetTickCount64;
      end;

      if FVerbLevel < 2 then
        DoOnAddToLog('Done sending FindSubControl settings.');

      FUpdateBackgroundTk := GetTickCount64;
    end;

    SWaitForFindSubControlResults:
    begin
      // It is possible that the plugin may receive "missing files" requests.
      // If that's the case, it should send those files, here in the same state.
    end;

    SGetListOfFonts:
    begin
      WaitingForWorkerFonts := True; //reset below
      WorkerRespondedCountLoF := 0;

      if not MQTT_PUBLISH(0, CCallbackID_GetListOfFonts, FDistPluginOptions.WorkerQoS) then   //notice CallbackID = 4     (i.e. 'GetListOfFonts')
      begin
        FSMError := 'Can''t send GetListOfFonts settings to broker.';
        DoOnAddToLog(FSMError);
      end
      else
        GetListOfFonts_tk := GetTickCount64;
    end;

    SWaitForListOfFonts:
    begin

    end;

    SUpdateBackgroundImageToAllWorkers:
    begin
      FWillWaitForUpdateBackgroundResponses := False; //Reset here.  Set in main loop.
      //SendBackgroundToAll_tk := GetTickCount64;  //Set in main loop.
      FWorkerRespondedCountBG := 0;
    end;

    SWaitForUpdateBackgroundImageResults:
    begin

    end;

    SUpdateCache:
    begin
      if FVerbLevel < 2 then
        DoOnAddToLog('Updating plugin cache..');

      if FDistPluginOptions.SaveWorkerCapabilitiesCacheAction = '' then
      begin
        FSMError := 'There is no configured action for updating plugin cache.';
        DoOnAddToLog(FSMError);
      end
      else
        if not DoOnSaveWorkerCapabilitiesCache then
          FSMError := 'Error updating plugin cache. The ' + FDistPluginOptions.SaveWorkerCapabilitiesCacheAction + ' action failed.';
          //ToDo: load all vars, then read the value of $ExecAction_Err$ and concatenate it to the error message above.
    end;

    SLoadCache:
    begin
      if FVerbLevel < 2 then
        DoOnAddToLog('Loading plugin cache..');

      if FDistPluginOptions.LoadWorkerCapabilitiesCacheAction = '' then
      begin
        FSMError := 'There is no configured action for loading plugin cache.';
        DoOnAddToLog(FSMError);
      end
      else
        if not DoOnLoadWorkerCapabilitiesCache then
          FSMError := 'Error loading plugin cache. The ' + FDistPluginOptions.LoadWorkerCapabilitiesCacheAction + ' action failed.';
          //ToDo: load all vars, then read the value of $ExecAction_Err$ and concatenate it to the error message above.
    end;

    SUnsubscribeFromResponses:
    begin
      FUnsubscribedSucessfully := MQTT_UNSUBSCRIBE(0, 0);     //unsubscribing with CTopicNameResult
      if not FUnsubscribedSucessfully then
      begin
        FSMError := 'Can''t unsubscribe from responses.';
        DoOnAddToLog(FSMError);
      end
      else
        UnsubscribeFromResponses_tk := GetTickCount64;
    end;

    SWaitForResponsesUnsubAck:
    begin
      //FUnsubAckReceived is set by handler
    end;

    SDisconnectFromBroker:
    begin
      FFSMDone := True;
      FDisconnectedSuccessfully := DoOnDisconnectFromBroker;
    end;

    SDone:
    begin
      FFSMDone := True;
      DoOnAddToLog('Plugin done');
    end;
  end;
end;


procedure TDistFSM.ExecFSM_Part2;
begin
  case FState of
    SInit:
      NextState := SConnectToBroker;

    SConnectToBroker:
      if FConnectedSucessfully then
        NextState := SWaitForConAck
      else
        NextState := SDisconnectFromBroker;

    SWaitForConAck:
    begin
      if FConAckReceived then  //there should be one FConAckReceived event only (this plugin to broker), unlike multiple workers below
        NextState := SSubscribeToResponses
      else
        if GetTickCount64 - ConnectToBroker_tk > 1000 then
          NextState := SDisconnectFromBroker  //Timeout
        else
          NextState := SWaitForConAck;
    end;

    SSubscribeToResponses:
      if FSubscribedSucessfully then
        NextState := SWaitForResponsesSubAck
      else
        NextState := SDisconnectFromBroker;

    SWaitForResponsesSubAck:   //Responses can be capabilities, lists of missing files, processing results
      if FSubAckReceived then
      begin
        if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsLoadCacheAndFindSubControl then
          NextState := SLoadCache
        else
          NextState := SRequestCapabilities;
      end
      else
      begin
        if GetTickCount64 - SubscribeToResponses_tk > 1000 then
          NextState := SDisconnectFromBroker  //Timeout
        else
          NextState := SWaitForResponsesSubAck;
      end;

    SRequestCapabilities:
      NextState := SWaitForCapabilitiesResponse;

    SWaitForCapabilitiesResponse:
      if (GetTickCount64 - GetCapabilities_tk > FDistPluginOptions.GetWorkerCapabilitiesTimeout) or  //Waiting for some time, for all workers to present their capabilities. This way, the plugin knows home many are they.
         (DoOnGetAllWorkersCount >= FDistPluginOptions.MinExpectedWorkerCount) then      //Or enough workers are available, no need to wait for more.
      begin
        if DoOnGetAllWorkersCount >= FDistPluginOptions.MinExpectedWorkerCount then
          DoOnAddToLog('Enough workers are available (' + IntToStr(DoOnGetAllWorkersCount) + '). Skipping timeout (Capabilities).');

        if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsReqCapAndFindSubControl then
          NextState := SGetFindSubControlActionContent
        else
          if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsReqCapAndGetFonts then
            NextState := SGetListOfFonts
          else
            if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl then
              NextState := SGetListOfFonts  //in SWaitForListOfFonts, there is still a verification of WorkerCapabilitiesSource
            else
              if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsReqCapAndUpdateCache then
                NextState := SUpdateCache
              else
                if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsLoadCacheAndFindSubControl then   //////////// this should not happen in this state
                begin
                  NextState := SUnsubscribeFromResponses; /////////////// set here to a different state when implemented
                  DoOnAddToLog('________' + CReqCapOperation_wcsLoadCacheAndFindSubControl + ' not implemented yet...'); //remove comment
                end
                else
                begin
                  NextState := SUnsubscribeFromResponses;
                  DoOnAddToLog('________ Unknown operation: ' + FDistPluginOptions.WorkerCapabilitiesSource + '. Exiting plugin.');
                end;

        FWaitingForWorkerCapabilities := False;
      end
      else
        NextState := SWaitForCapabilitiesResponse;

    SGetFindSubControlActionContent:
    begin
      if DoOnGetAllWorkersCount > 255 then
      begin
        FSMError := 'Too many workers (' + IntToStr(DoOnGetAllWorkersCount) + '). Please modify the index encoding to raise the limit above 255.';
        DoOnAddToLog(FSMError);
        NextState := SUnsubscribeFromResponses;
      end
      else
        NextState := SPublishBackgroundImageToAllWorkers;
    end;

    SPublishBackgroundImageToAllWorkers:
      NextState := SWaitForBackgroundImageResults;

    SWaitForBackgroundImageResults:
      if GetTickCount64 - FSendBackgroundToAll_tk > FDistPluginOptions.FindSubControlWorkerTimeout then   //not sure what timeout to use here
      begin
        NextState := SUnsubscribeFromResponses;  //Timeout

        if FWorkerRespondedCountBG = 0 then
          FSMError := 'Timeout waiting for workers to send the background image to their local UIClicker.'
        else
          FSMError := 'Some of the responding workers could not send the background image to their local UIClicker.';

        FSMError := FSMError + ' ResponseCount = ' + IntToStr(FWorkerRespondedCountBG) + ' / ' + IntToStr(DoOnGetAllWorkersCount) + '.';  //Do not change this format. It is used later.

        DoOnAddToLog(FSMError);
        DoOnAddToLog('Sending background image duration: ' + IntToStr(GetTickCount64 - FSendBackgroundToAll_tk) + 'ms.  FindSubControlWorkerTimeout is set to ' + IntToStr(FDistPluginOptions.FindSubControlWorkerTimeout) + 'ms.');
      end
      else
      begin
        NextState := SWaitForBackgroundImageResults;

        if FWorkerRespondedCountBG >= DoOnGetAllWorkersCount then
        begin
          NextState := SPublishFindSubControlToAllWorkers;

          if FVerbLevel < 2 then
            DoOnAddToLog('All workers responded after having the background image in ' + IntToStr(GetTickCount64 - FSendBackgroundToAll_tk) + 'ms.');
        end;
      end;

    SPublishFindSubControlToAllWorkers:
      NextState := SWaitForFindSubControlResults;

    SWaitForFindSubControlResults:
      if FAtLeastOneWorkerFoundTheSubControl then
        NextState := SUnsubscribeFromResponses
      else
        if GetTickCount64 - SendFindSubControl_tk > FDistPluginOptions.FindSubControlWorkerTimeout then
        begin
          NextState := SUnsubscribeFromResponses;  //Timeout

          if FWorkerRespondedCountFS = 0 then
            FSMError := 'Timeout waiting for workers to find the SubControl.'
          else
            FSMError := CNoWorkerFoundSubControlErr;

          FSMError := FSMError + ' ResponseCount = ' + IntToStr(FWorkerRespondedCountFS) + ' / ' + IntToStr(DoOnGetAllWorkersCount) + '.';  //Do not change this format. It is used later.

          DoOnAddToLog(FSMError);
        end
        else
        begin
          if not FWillWaitForUpdateBackgroundResponses then
            NextState := SWaitForFindSubControlResults
          else
            NextState := SUpdateBackgroundImageToAllWorkers;
        end;

    SGetListOfFonts:
      NextState := SWaitForListOfFonts;

    SWaitForListOfFonts:
      if (GetTickCount64 - GetListOfFonts_tk > FDistPluginOptions.GetListOfFontsTimeout) or
         (FWorkerRespondedCountLoF >= FDistPluginOptions.MinExpectedWorkerCount) then   //Waiting for some time, for all workers to present their fonts.
      begin
        if DoOnGetAllWorkersCount >= FDistPluginOptions.MinExpectedWorkerCount then
          DoOnAddToLog('Enough workers are available (' + IntToStr(DoOnGetAllWorkersCount) + '). Skipping timeout (ListOfFonts).');

        if FDistPluginOptions.WorkerCapabilitiesSource = CReqCapOperation_wcsReqCapAndGetFontsAndFindSubControl then
          NextState := SGetFindSubControlActionContent
        else
          NextState := SUnsubscribeFromResponses;

        if FWorkerRespondedCountLoF = 0 then
        begin
          FSMError := 'Timeout waiting for workers to respond with lists of fonts.';
          NextState := SUnsubscribeFromResponses;
        end
        else
          if FWorkerRespondedCountLoF < DoOnGetAllWorkersCount then
          begin
            FSMError := 'Not all workers reponded in time with their lists of fonts.';
            FSMError := FSMError + ' ListOfFontsResponseCount = ' + IntToStr(FWorkerRespondedCountLoF) + ' / ' + IntToStr(DoOnGetAllWorkersCount) + '.';  //Do not change this format. It is used later.
            NextState := SUnsubscribeFromResponses;
          end;

        DoOnAddToLog(FSMError);
        WaitingForWorkerFonts := False;
      end
      else
        NextState := SWaitForListOfFonts;

    SUpdateBackgroundImageToAllWorkers:
      NextState := SWaitForUpdateBackgroundImageResults;

    SWaitForUpdateBackgroundImageResults:
    begin
      if GetTickCount64 - FSendBackgroundToAll_tk > Max(500, FDistPluginOptions.UpdateBackgroundInterval - 100) then   //not sure what timeout to use here
      begin
        //Do not set "NextState := SUnsubscribeFromResponses;"  //Timeout     A timeout on updating the background, should not stop the plugin.
        NextState := SWaitForFindSubControlResults;   //go back to the main waiting state

        if FWorkerRespondedCountBG = 0 then
          DoOnAddToLog('Timeout waiting for workers to send the background image to their local UIClicker.')
        else
          DoOnAddToLog('Some of the responding workers could not send the background image to their local UIClicker.');

        DoOnAddToLog(' ResponseCount = ' + IntToStr(FWorkerRespondedCountBG) + ' / ' + IntToStr(DoOnGetAllWorkersCount) + '.');  //Do not change this format. It is used later.
        DoOnAddToLog('Updating background image duration: ' + IntToStr(GetTickCount64 - FSendBackgroundToAll_tk) + 'ms.  FindSubControlWorkerTimeout is set to ' + IntToStr(FDistPluginOptions.FindSubControlWorkerTimeout) + 'ms.');
      end
      else
      begin
        NextState := SWaitForUpdateBackgroundImageResults;

        if FWorkerRespondedCountBG >= DoOnGetAllWorkersCount then
        begin
          NextState := SWaitForFindSubControlResults;  //go back to the main waiting state

          if FVerbLevel < 2 then
            DoOnAddToLog('All workers responded after having the updated background image in ' + IntToStr(GetTickCount64 - FSendBackgroundToAll_tk) + 'ms.');
        end;
      end;
    end;

    SUpdateCache:
      NextState := SUnsubscribeFromResponses;

    SLoadCache:
    begin
      //NextState := SGetFindSubControlActionContent;  //from SLoadCache, the next state should be SGetFindSubControlActionContent
      FDistPluginOptions.WorkerCapabilitiesSource := CReqCapOperation_wcsReqCapAndFindSubControl; //but the list of workers is not updated yet by the cache.  ToDo
      NextState := SRequestCapabilities; //Go and get capabilities, which updates the list of workers and then execute FindSubControl. (Temp solution.)
    end;

    SUnsubscribeFromResponses:
      if FUnsubscribedSucessfully then
        NextState := SWaitForResponsesUnsubAck
      else
        NextState := SDisconnectFromBroker;

    SWaitForResponsesUnsubAck:
      if FUnsubAckReceived then
        NextState := SDisconnectFromBroker
      else
      begin
        if GetTickCount64 - UnsubscribeFromResponses_tk > 1000 then
          NextState := SDisconnectFromBroker  //Timeout
        else
          NextState := SWaitForResponsesUnsubAck;
      end;

    SDisconnectFromBroker:
      NextState := SDone;

    SDone:
      NextState := SDone;
  end;
end;


function TDistFSM.ExecFSM: string;
begin
  ExecFSM_Part1;
  ExecFSM_Part2;
  FState := NextState;

  Result := FSMError;
end;

end.

