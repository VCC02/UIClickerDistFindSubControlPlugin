{
    Copyright (C) 2025 VCC
    creation date: 10 Jul 2025
    initial release date: 10 Jul 2025

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


unit DistFindSubControlPropertyEditorForm;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TClkFindControlMatchBitmapTextDist = record   //Same fields as in TClkFindControlMatchBitmapText, but these are strings, to allow intervals and enumerations.
    ForegroundColor: string;
    BackgroundColor: string;
    FontName: string;
    FontSize: string;//Integer;
    Bold: string;//Boolean;
    Italic: string;//Boolean;
    Underline: string;//Boolean;
    StrikeOut: string;//Boolean;
    FontQuality: string;//TFontQuality;
    FontQualityUsesReplacement: string;//Boolean;
    FontQualityReplacement: string;
    ProfileName: string;
    CropLeft: string;
    CropTop: string;
    CropRight: string;
    CropBottom: string;
    IgnoreBackgroundColor: string;//Boolean;
  end;

  TClkFindControlMatchBitmapTextDistArr = array of TClkFindControlMatchBitmapTextDist;

  { TfrmDistFindSubControlPropertyEditor }

  TfrmDistFindSubControlPropertyEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnBrowseClkPrfFile: TButton;
    lbeClkPrfFile: TLabeledEdit;
    procedure btnBrowseClkPrfFileClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FEditedFile: TClkFindControlMatchBitmapTextDistArr;
  public

  end;


function EditCustomFontProfilesProperty(APropertyIndex: Integer; ACurrentValue: string; out ANewValue: string): Boolean;
procedure LoadClkPrf(AFnm: string; var AContent: TClkFindControlMatchBitmapTextDistArr);
procedure SaveClkPrf(AFnm: string; var AContent: TClkFindControlMatchBitmapTextDistArr);


implementation

{$R *.frm}


uses
  DistFindSubControlPluginProperties, ClickerIniFiles;


function EditCustomFontProfilesProperty(APropertyIndex: Integer; ACurrentValue: string; out ANewValue: string): Boolean;
var
  frmDistFindSubControlPropertyEditor: TfrmDistFindSubControlPropertyEditor;
begin
  Application.CreateForm(TfrmDistFindSubControlPropertyEditor, frmDistFindSubControlPropertyEditor);

  //decode ACurrentValue
  frmDistFindSubControlPropertyEditor.lbeClkPrfFile.Text := ACurrentValue;
  frmDistFindSubControlPropertyEditor.lbeClkPrfFile.EditLabel.Caption := frmDistFindSubControlPropertyEditor.lbeClkPrfFile.EditLabel.Caption + ' - ' + CRequiredSubControlPropertyNames[APropertyIndex];

  frmDistFindSubControlPropertyEditor.ShowModal;
  Result := frmDistFindSubControlPropertyEditor.Tag = 1;
  if Result then
  begin
    //encode
    ANewValue := frmDistFindSubControlPropertyEditor.lbeClkPrfFile.Text;
  end;
end;


procedure LoadClkPrf(AFnm: string; var AContent: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniReadonlyFile;
  i: Integer;
begin
  //if Pos('MEM:', Fnm) = 1 then
  //  Ini := TClkIniReadonlyFile.Create(AMemStream)
  //else
    Ini := TClkIniReadonlyFile.Create(AFnm);
  try
    SetLength(AContent, Ini.GetSectionCount);
    for i := 0 to Length(AContent) - 1 do
    begin
      AContent[i].ForegroundColor := Ini.ReadString(i, 'ForegroundColor', '000000');
      AContent[i].BackgroundColor := Ini.ReadString(i, 'BackgroundColor', 'FFFFFF');
      AContent[i].FontName := Ini.ReadString(i, 'FontName', 'Tahoma');
      AContent[i].FontSize := Ini.ReadString(i, 'FontSize', '8');
      AContent[i].Bold := Ini.ReadString(i, 'Bold', '0');
      AContent[i].Italic := Ini.ReadString(i, 'Italic', '0');
      AContent[i].Underline := Ini.ReadString(i, 'Underline', '0');
      AContent[i].StrikeOut := Ini.ReadString(i, 'StrikeOut', '0');
      AContent[i].FontQuality := Ini.ReadString(i, 'FontQuality', '0');
      AContent[i].FontQualityUsesReplacement := Ini.ReadString(i, 'FontQualityUsesReplacement', '0');
      AContent[i].FontQualityReplacement := Ini.ReadString(i, 'FontQualityReplacement', '0');
      AContent[i].ProfileName := Ini.ReadString(i, 'ProfileName', '0');
      AContent[i].CropLeft := Ini.ReadString(i, 'CropLeft', '0');
      AContent[i].CropTop := Ini.ReadString(i, 'CropTop', '0');
      AContent[i].CropRight := Ini.ReadString(i, 'CropRight', '0');
      AContent[i].CropBottom := Ini.ReadString(i, 'CropBottom', '0');
      AContent[i].IgnoreBackgroundColor := Ini.ReadString(i, 'IgnoreBackgroundColor', '0');
    end;
  finally
    Ini.Free;
  end;
end;


procedure SaveClkPrf(AFnm: string; var AContent: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniFile;
  i: Integer;
begin
  //if Pos('MEM:', Fnm) = 1 then
  //  Ini := TClkIniFile.Create(AMemStream)
  //else
    Ini := TClkIniFile.Create(AFnm);
  try
    SetLength(AContent, Ini.GetSectionCount);
    for i := 0 to Length(AContent) - 1 do
    begin
      Ini.WriteString(i, 'ForegroundColor', AContent[i].ForegroundColor);
      Ini.WriteString(i, 'BackgroundColor', AContent[i].BackgroundColor);
      Ini.WriteString(i, 'FontName', AContent[i].FontName);
      Ini.WriteString(i, 'FontSize', AContent[i].FontSize);
      Ini.WriteString(i, 'Bold', AContent[i].Bold);
      Ini.WriteString(i, 'Italic', AContent[i].Italic);
      Ini.WriteString(i, 'Underline', AContent[i].Underline);
      Ini.WriteString(i, 'StrikeOut', AContent[i].StrikeOut);
      Ini.WriteString(i, 'FontQuality', AContent[i].FontQuality);
      Ini.WriteString(i, 'FontQualityUsesReplacement', AContent[i].FontQualityUsesReplacement);
      Ini.WriteString(i, 'FontQualityReplacement', AContent[i].FontQualityReplacement);
      Ini.WriteString(i, 'ProfileName', AContent[i].ProfileName);
      Ini.WriteString(i, 'CropLeft', AContent[i].CropLeft);
      Ini.WriteString(i, 'CropTop', AContent[i].CropTop);
      Ini.WriteString(i, 'CropRight', AContent[i].CropRight);
      Ini.WriteString(i, 'CropBottom', AContent[i].CropBottom);
      Ini.WriteString(i, 'IgnoreBackgroundColor', AContent[i].IgnoreBackgroundColor);
    end;

    //if Pos('MEM:', Fnm) = 1 then
    //  Ini := UpdateStream
    //else
      Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


{ TfrmDistFindSubControlPropertyEditor }

procedure TfrmDistFindSubControlPropertyEditor.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TfrmDistFindSubControlPropertyEditor.btnCancelClick(Sender: TObject);
begin
  Tag := 0;
  Close;
end;


procedure TfrmDistFindSubControlPropertyEditor.btnBrowseClkPrfFileClick(
  Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  OpenDialog.Filter := 'Font profiles (*.clkprf)|*.clkprf|All files (*.*)|*.*';

  OpenDialog.FileName := lbeClkPrfFile.Text;
  if not OpenDialog.Execute then
    Exit;

  lbeClkPrfFile.Text := OpenDialog.FileName;
end;


procedure TfrmDistFindSubControlPropertyEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

