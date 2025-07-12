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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ClickerUtils;

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
procedure LoadClkPrf(AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
procedure SaveClkPrf(AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
procedure UpdateFindSubControlActionWithNewProfiles(var AAction: TClkFindSubControlOptions; AProfiles: TClkFindControlMatchBitmapTextDistArr);


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


procedure LoadClkPrf(AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniReadonlyFile;
  i: Integer;
begin
  //if Pos('MEM:', Fnm) = 1 then
  //  Ini := TClkIniReadonlyFile.Create(AMemStream)
  //else
    Ini := TClkIniReadonlyFile.Create(AFnm);
  try
    SetLength(AProfiles, Ini.GetSectionCount);
    for i := 0 to Length(AProfiles) - 1 do
    begin
      AProfiles[i].ForegroundColor := Ini.ReadString(i, 'ForegroundColor', '000000');
      AProfiles[i].BackgroundColor := Ini.ReadString(i, 'BackgroundColor', 'FFFFFF');
      AProfiles[i].FontName := Ini.ReadString(i, 'FontName', 'Tahoma');
      AProfiles[i].FontSize := Ini.ReadString(i, 'FontSize', '8');
      AProfiles[i].Bold := Ini.ReadString(i, 'Bold', '0');
      AProfiles[i].Italic := Ini.ReadString(i, 'Italic', '0');
      AProfiles[i].Underline := Ini.ReadString(i, 'Underline', '0');
      AProfiles[i].StrikeOut := Ini.ReadString(i, 'StrikeOut', '0');
      AProfiles[i].FontQuality := Ini.ReadString(i, 'FontQuality', '0');
      AProfiles[i].FontQualityUsesReplacement := Ini.ReadString(i, 'FontQualityUsesReplacement', '0');
      AProfiles[i].FontQualityReplacement := Ini.ReadString(i, 'FontQualityReplacement', '0');
      AProfiles[i].ProfileName := Ini.ReadString(i, 'ProfileName', '0');
      AProfiles[i].CropLeft := Ini.ReadString(i, 'CropLeft', '0');
      AProfiles[i].CropTop := Ini.ReadString(i, 'CropTop', '0');
      AProfiles[i].CropRight := Ini.ReadString(i, 'CropRight', '0');
      AProfiles[i].CropBottom := Ini.ReadString(i, 'CropBottom', '0');
      AProfiles[i].IgnoreBackgroundColor := Ini.ReadString(i, 'IgnoreBackgroundColor', '0');
    end;
  finally
    Ini.Free;
  end;
end;


procedure SaveClkPrf(AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniFile;
  i: Integer;
begin
  //if Pos('MEM:', Fnm) = 1 then
  //  Ini := TClkIniFile.Create(AMemStream)
  //else
    Ini := TClkIniFile.Create(AFnm);
  try
    SetLength(AProfiles, Ini.GetSectionCount);
    for i := 0 to Length(AProfiles) - 1 do
    begin
      Ini.WriteString(i, 'ForegroundColor', AProfiles[i].ForegroundColor);
      Ini.WriteString(i, 'BackgroundColor', AProfiles[i].BackgroundColor);
      Ini.WriteString(i, 'FontName', AProfiles[i].FontName);
      Ini.WriteString(i, 'FontSize', AProfiles[i].FontSize);
      Ini.WriteString(i, 'Bold', AProfiles[i].Bold);
      Ini.WriteString(i, 'Italic', AProfiles[i].Italic);
      Ini.WriteString(i, 'Underline', AProfiles[i].Underline);
      Ini.WriteString(i, 'StrikeOut', AProfiles[i].StrikeOut);
      Ini.WriteString(i, 'FontQuality', AProfiles[i].FontQuality);
      Ini.WriteString(i, 'FontQualityUsesReplacement', AProfiles[i].FontQualityUsesReplacement);
      Ini.WriteString(i, 'FontQualityReplacement', AProfiles[i].FontQualityReplacement);
      Ini.WriteString(i, 'ProfileName', AProfiles[i].ProfileName);
      Ini.WriteString(i, 'CropLeft', AProfiles[i].CropLeft);
      Ini.WriteString(i, 'CropTop', AProfiles[i].CropTop);
      Ini.WriteString(i, 'CropRight', AProfiles[i].CropRight);
      Ini.WriteString(i, 'CropBottom', AProfiles[i].CropBottom);
      Ini.WriteString(i, 'IgnoreBackgroundColor', AProfiles[i].IgnoreBackgroundColor);
    end;

    //if Pos('MEM:', Fnm) = 1 then
    //  Ini := UpdateStream
    //else
      Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


//As an example, if AField is '008000..008F00,00AE00..00BF20', then AFieldRanges will contain two items: '008000..008F00' and '00AE00..00BF20'.
procedure GetFieldRanges(AField: string; AFieldRanges: TStringList);
var
  i: Integer;
begin
  AFieldRanges.Text := StringReplace(AField, ',', #13#10, [rfReplaceAll]);
  for i := 0 to AFieldRanges.Count - 1 do
    if Pos(' ', AFieldRanges.Strings[i]) > 0 then
      AFieldRanges.Strings[i] := Trim(AFieldRanges.Strings[i]);
end;


type
  TRange = record
    MinValue, MaxValue: Integer;
    StartColor, EndColor: TColor; //used if is hex color
  end;

procedure GetFieldRangeFromString(ARangeStr: string; AIsHexColor: Boolean; out ARange: TRange);
var
  MinValueStr, MaxValueStr: string;
  ph: Integer;
  R1, R2, G1, G2, B1, B2: Byte;
  IntervalR, IntervalG, IntervalB: Byte;
begin
  if Pos('..', ARangeStr) > 0 then
  begin
    MinValueStr := Trim(Copy(ARangeStr, 1, Pos('..', ARangeStr) - 1));
    MaxValueStr := Trim(Copy(ARangeStr, Pos('..', ARangeStr) + 2, MaxInt));
  end
  else
  begin
    MinValueStr := Trim(ARangeStr);
    MaxValueStr := ARangeStr;
  end;

  if AIsHexColor then
  begin
    ARange.StartColor := HexToInt(MinValueStr);
    ARange.EndColor := HexToInt(MaxValueStr);
    RedGreenBlue(ARange.StartColor, R1, G1, B1);
    RedGreenBlue(ARange.EndColor, R2, G2, B2);
    IntervalR := Abs(SmallInt(R1) - SmallInt(R2));
    IntervalG := Abs(SmallInt(G1) - SmallInt(G2));
    IntervalB := Abs(SmallInt(B1) - SmallInt(B2));

    if IntervalR > IntervalG then
    begin
      ARange.MinValue := R1;
      ARange.MaxValue := R2;

      if IntervalB > IntervalR then
      begin
        ARange.MinValue := B1;
        ARange.MaxValue := B2;
      end;
    end
    else
    begin
      ARange.MinValue := G1;
      ARange.MaxValue := G2;

      if IntervalB > IntervalG then
      begin
        ARange.MinValue := B1;
        ARange.MaxValue := B2;
      end;
    end;
  end
  else
  begin
    ARange.MinValue := StrToIntDef(MinValueStr, 0);
    ARange.MaxValue := StrToIntDef(MaxValueStr, 0);
  end;

  if ARange.MaxValue < ARange.MinValue then
  begin            //Swapping for the sake of color intervals, which may represent a gradient.
    ph := ARange.MinValue;
    ARange.MinValue := ARange.MaxValue;
    ARange.MaxValue := ph;
  end;

  if ARange.MaxValue - ARange.MinValue > 128 then
    ARange.MaxValue := ARange.MinValue + 128;        //limit the interval to an acceptable size
end;


procedure GetAllFieldRanges(AField: string; AIsHexColor: Boolean; ADestFieldRanges: TStringList);
var
  TempRanges: TStringList;
  i, j: Integer;
  Range: TRange;
  Bmp: TBitmap;
  GradRect: TRect;
begin
  TempRanges := TStringList.Create;
  try
    TempRanges.LineBreak := #13#10;
    ADestFieldRanges.LineBreak := #13#10;

    GetFieldRanges(AField, TempRanges); // TempRanges may contain items, which are intervals, e.g. '00AE00..00BF20'
    for i := 0 to TempRanges.Count - 1 do
      if Pos('..', TempRanges.Strings[i]) = 0 then //single value
        ADestFieldRanges.Add(TempRanges.Strings[i])
      else
      begin
        GetFieldRangeFromString(TempRanges.Strings[i], AIsHexColor, Range);  //this will limit the interval to an acceptable size

        if AIsHexColor then
        begin
          Bmp := TBitmap.Create;
          try
            Bmp.PixelFormat := pf24bit;
            Bmp.SetSize(Range.MaxValue - Range.MinValue + 1, 1);
            Bmp.Canvas.Pen.Style := psClear;
            Bmp.Canvas.Brush.Style := bsSolid;

            GradRect.Left := 0;
            GradRect.Top := 0;
            GradRect.Width := Bmp.Width;
            GradRect.Height := 1;
            Bmp.Canvas.GradientFill(GradRect, Range.MinValue, Range.MaxValue, gdHorizontal);

            for j := 0 to Bmp.Width - 1 do
              ADestFieldRanges.Add(IntToHex(Bmp.Canvas.Pixels[j, 0], 6));
          finally
            Bmp.Free;
          end;
        end
        else
        begin
          for j := Range.MinValue to Range.MaxValue do
            ADestFieldRanges.Add(IntToStr(j));
        end;
      end;
  finally
    TempRanges.Free;
  end;

  if ADestFieldRanges.Count = 0 then
    ADestFieldRanges.Add('0'); //have at least one item
end;


function FontQuality_AsStringToValue(AFontQualityAsString: string): TFontQuality;
var
  i: TFontQuality;
begin
  Result := fqCleartype;
  for i := Low(TFontQuality) to High(TFontQuality) do
    if CFontQualityStr[i] = AFontQualityAsString then
    begin
      Result := i;
      Exit;
    end;
end;


procedure AddProfileRangesToActionProfiles(var AMatchBitmapText: TClkFindControlMatchBitmapTextArr; var AProfileRange: TClkFindControlMatchBitmapTextDist);
var
  AllFieldsRanges: array of TStringList;
  i, n: Integer;
  r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16: Integer;
begin
  SetLength(AllFieldsRanges, 17);//number of fields in a TClkFindControlMatchBitmapText structure
  for i := 0 to Length(AllFieldsRanges) - 1 do
    AllFieldsRanges[i] := TStringList.Create;
  try
    GetAllFieldRanges(AProfileRange.ForegroundColor, True, AllFieldsRanges[0]);
    GetAllFieldRanges(AProfileRange.BackgroundColor, True, AllFieldsRanges[1]);
    GetAllFieldRanges(AProfileRange.FontName, False, AllFieldsRanges[2]);
    GetAllFieldRanges(AProfileRange.FontSize, False, AllFieldsRanges[3]);
    GetAllFieldRanges(AProfileRange.Bold, False, AllFieldsRanges[4]);
    GetAllFieldRanges(AProfileRange.Italic, False, AllFieldsRanges[5]);
    GetAllFieldRanges(AProfileRange.Underline, False, AllFieldsRanges[6]);
    GetAllFieldRanges(AProfileRange.StrikeOut, False, AllFieldsRanges[7]);
    GetAllFieldRanges(AProfileRange.FontQuality, False, AllFieldsRanges[8]);
    GetAllFieldRanges(AProfileRange.FontQualityUsesReplacement, False, AllFieldsRanges[9]);
    GetAllFieldRanges(AProfileRange.FontQualityReplacement, False, AllFieldsRanges[10]);
    GetAllFieldRanges(AProfileRange.ProfileName, False, AllFieldsRanges[11]);
    GetAllFieldRanges(AProfileRange.CropLeft, False, AllFieldsRanges[12]);
    GetAllFieldRanges(AProfileRange.CropTop, False, AllFieldsRanges[13]);
    GetAllFieldRanges(AProfileRange.CropRight, False, AllFieldsRanges[14]);
    GetAllFieldRanges(AProfileRange.CropBottom, False, AllFieldsRanges[15]);
    GetAllFieldRanges(AProfileRange.IgnoreBackgroundColor, False, AllFieldsRanges[16]);

    for r0 := 0 to AllFieldsRanges[0].Count - 1 do
      for r1 := 0 to AllFieldsRanges[1].Count - 1 do
        for r2 := 0 to AllFieldsRanges[2].Count - 1 do
          for r3 := 0 to AllFieldsRanges[3].Count - 1 do
            for r4 := 0 to AllFieldsRanges[4].Count - 1 do
              for r5 := 0 to AllFieldsRanges[5].Count - 1 do
                for r6 := 0 to AllFieldsRanges[6].Count - 1 do
                  for r7 := 0 to AllFieldsRanges[7].Count - 1 do
                    for r8 := 0 to AllFieldsRanges[8].Count - 1 do
                      for r9 := 0 to AllFieldsRanges[9].Count - 1 do
                        for r10 := 0 to AllFieldsRanges[10].Count - 1 do
                          for r11 := 0 to AllFieldsRanges[11].Count - 1 do
                            for r12 := 0 to AllFieldsRanges[12].Count - 1 do
                              for r13 := 0 to AllFieldsRanges[13].Count - 1 do
                                for r14 := 0 to AllFieldsRanges[14].Count - 1 do
                                  for r15 := 0 to AllFieldsRanges[15].Count - 1 do
                                    for r16 := 0 to AllFieldsRanges[16].Count - 1 do
                                    begin
                                      n := Length(AMatchBitmapText);
                                      SetLength(AMatchBitmapText, n + 1);

                                      AMatchBitmapText[n].ForegroundColor := AllFieldsRanges[0].Strings[r0];      //is hex
                                      AMatchBitmapText[n].BackgroundColor := AllFieldsRanges[1].Strings[r1];      //is hex
                                      AMatchBitmapText[n].FontName := AllFieldsRanges[2].Strings[r2];
                                      AMatchBitmapText[n].FontSize := StrToIntDef(AllFieldsRanges[3].Strings[r3], 8);
                                      AMatchBitmapText[n].Bold := StrToBool(AllFieldsRanges[4].Strings[r4]) or (AllFieldsRanges[4].Strings[r4] = '1');
                                      AMatchBitmapText[n].Italic := StrToBool(AllFieldsRanges[5].Strings[r5]) or (AllFieldsRanges[5].Strings[r5] = '1');
                                      AMatchBitmapText[n].Underline := StrToBool(AllFieldsRanges[6].Strings[r6]) or (AllFieldsRanges[6].Strings[r6] = '1');
                                      AMatchBitmapText[n].StrikeOut := StrToBool(AllFieldsRanges[7].Strings[r7]) or (AllFieldsRanges[7].Strings[r7] = '1');
                                      AMatchBitmapText[n].FontQuality := FontQuality_AsStringToValue(AllFieldsRanges[8].Strings[r8]);
                                      AMatchBitmapText[n].FontQualityUsesReplacement := StrToBool(AllFieldsRanges[9].Strings[r9]) or (AllFieldsRanges[9].Strings[r9] = '1');
                                      AMatchBitmapText[n].FontQualityReplacement := AllFieldsRanges[10].Strings[r10];
                                      AMatchBitmapText[n].ProfileName := AllFieldsRanges[11].Strings[r11];
                                      AMatchBitmapText[n].CropLeft := AllFieldsRanges[12].Strings[r12];
                                      AMatchBitmapText[n].CropTop := AllFieldsRanges[13].Strings[r13];
                                      AMatchBitmapText[n].CropRight := AllFieldsRanges[14].Strings[r14];
                                      AMatchBitmapText[n].CropBottom := AllFieldsRanges[15].Strings[r15];
                                      AMatchBitmapText[n].IgnoreBackgroundColor := StrToBool(AllFieldsRanges[16].Strings[r16]) or (AllFieldsRanges[16].Strings[r16] = '1');
                                    end;
  finally
    for i := 0 to Length(AllFieldsRanges) - 1 do
      AllFieldsRanges[i].Free;
  end;
end;


procedure UpdateFindSubControlActionWithNewProfiles(var AAction: TClkFindSubControlOptions; AProfiles: TClkFindControlMatchBitmapTextDistArr);
var
  i: Integer;
begin
  SetLength(AAction.MatchBitmapText, 0);
  for i := 0 to Length(AProfiles) - 1 do
    AddProfileRangesToActionProfiles(AAction.MatchBitmapText, AProfiles[i]);
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

