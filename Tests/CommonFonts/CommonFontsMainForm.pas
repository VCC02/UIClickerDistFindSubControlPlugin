{
    Copyright (C) 2025 VCC
    creation date: 08 Apr 2025
    initial release date: 16 Apr 2025

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


unit CommonFontsMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IdHTTPServer, IdCustomHTTPServer, IdContext, IdSync;

type

  { TfrmCommonFontsMain }

  TfrmCommonFontsMain = class(TForm)
    IdHTTPServer1: TIdHTTPServer;
    Image1: TImage;
    imgWinLinResults: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    lblDraft: TLabel;
    lblAntialiased: TLabel;
    lblCleartype: TLabel;
    lblProof: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblNonAntialiased: TLabel;
    tmrDelay: TTimer;
    tmrStartup: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure Label35DblClick(Sender: TObject);
    procedure tmrDelayTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    FNewLabelColor: TColor;

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure UpdateLabelColorFromServer(ANewColor: TColor; ADelayMs: Integer = 0);
  public

  end;


  TSyncObj = class(TIdSync)
  private
    FNewColor: TColor;
    FDelayMs: Integer;
  protected
    procedure DoSynchronize; override;
  end;

var
  frmCommonFontsMain: TfrmCommonFontsMain;

implementation

{$R *.frm}

uses
  IniFiles, Math;


procedure TSyncObj.DoSynchronize;
begin
  if FDelayMs <> 0 then
  begin
    frmCommonFontsMain.FNewLabelColor := FNewColor;
    frmCommonFontsMain.tmrDelay.Interval := Min(Max(0, FDelayMs), 60000);
    frmCommonFontsMain.tmrDelay.Enabled := True;
  end
  else
    frmCommonFontsMain.Label35.Color := FNewColor;

  frmCommonFontsMain.Label35.Hint := 'Double-click to reset.';
end;


procedure TfrmCommonFontsMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CustomFonts.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);
  finally
    Ini.Free;
  end;
end;


procedure TfrmCommonFontsMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CustomFonts.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmCommonFontsMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  FNewLabelColor := clGray;
  LoadSettingsFromIni;

  try
    IdHTTPServer1.Active := True;
  except
    on E: Exception do
    begin
      Label35.ShowHint := True;
      Label35.Hint := 'Can''t start server module:' + #13#10 + E.Message;
      UpdateLabelColorFromServer(clFuchsia);
    end;
  end;
end;


procedure TfrmCommonFontsMain.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
end;


procedure TfrmCommonFontsMain.UpdateLabelColorFromServer(ANewColor: TColor; ADelayMs: Integer = 0);
var
  SyncObj: TSyncObj;
begin
  SyncObj := TSyncObj.Create;
  try
    SyncObj.FNewColor := ANewColor;
    SyncObj.FDelayMs := ADelayMs;
    SyncObj.Synchronize;
  finally
    SyncObj.Free;
  end;
end;


procedure TfrmCommonFontsMain.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Cmd: string;
begin
  Cmd := ARequestInfo.Document;
  ARequestInfo.Params.LineBreak := #13#10;
  AResponseInfo.ContentType := 'text/plain'; // 'text/html';  default type
  AResponseInfo.ContentText := 'Unknown command';

  if Cmd = '/SetSingleLabelColor' then   //this has priority over other commands  (it is used with KeepAlive)
  begin
    Randomize;
    AResponseInfo.ContentText := 'Done ' + IntToStr(Random(MaxInt));
    UpdateLabelColorFromServer(StrToIntDef(ARequestInfo.Params.Values['Color'], clWindowText),
                               StrToIntDef(ARequestInfo.Params.Values['Delay'], 0));
  end;
end;


procedure TfrmCommonFontsMain.Label35DblClick(Sender: TObject);
begin
  Label35.Color := clGreen;
end;


procedure TfrmCommonFontsMain.tmrDelayTimer(Sender: TObject);
begin
  tmrDelay.Enabled := False;
  Label35.Color := FNewLabelColor;
end;


procedure TfrmCommonFontsMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    SaveSettingsToIni;
  except
  end;
end;

end.

