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
  Buttons, Menus, ClickerUtils, ClickerActionPlugins;

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
    btnSaveEmptyFileToInMemFS: TButton;
    btnLoadFileFromDiskAndSaveItToInMemFS: TButton;
    imgGradient: TImage;
    lbeClkPrfFile: TLabeledEdit;
    MenuItem_BrowseInMemFS: TMenuItem;
    pmBrowseInMemFS: TPopupMenu;
    spdbtnBrowseInMemFS: TSpeedButton;
    procedure btnBrowseClkPrfFileClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadFileFromDiskAndSaveItToInMemFSClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSaveEmptyFileToInMemFSClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure spdbtnBrowseInMemFSClick(Sender: TObject);
  private
    FEditedFile: TClkFindControlMatchBitmapTextDistArr;

    FPluginReference: Pointer;
    FOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS;

    procedure InMemFSItemClick(Sender: TObject);
  public

  end;


function EditCustomFontProfilesProperty(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; APropertyIndex: Integer; ACurrentValue: string; out ANewValue: string): Boolean;
procedure LoadClkPrf(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
procedure SaveClkPrf(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
procedure UpdateFindSubControlActionWithNewProfiles(var AAction: TClkFindSubControlOptions; AProfiles: TClkFindControlMatchBitmapTextDistArr);


implementation

{$R *.frm}


uses
  DistFindSubControlPluginProperties, ClickerIniFiles, Math
  //, ClickerTemplates
  , ClickerPluginInMemFileSystem
  ;


function EditCustomFontProfilesProperty(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; APropertyIndex: Integer; ACurrentValue: string; out ANewValue: string): Boolean;
var
  frmDistFindSubControlPropertyEditor: TfrmDistFindSubControlPropertyEditor;
begin
  Application.CreateForm(TfrmDistFindSubControlPropertyEditor, frmDistFindSubControlPropertyEditor);

  frmDistFindSubControlPropertyEditor.FPluginReference := APluginReference;
  frmDistFindSubControlPropertyEditor.FOnActionPlugin_InMemFS := AOnActionPlugin_InMemFS;

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


procedure LoadClkPrf(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniReadonlyFile;
  i: Integer;
  PluginInMemFS: TPluginInMemFileSystem;
  Content: TMemoryStream;
begin
  if Pos('MEM:', UpperCase(AFnm)) = 1 then
  begin
    Content := TMemoryStream.Create;
    PluginInMemFS := TPluginInMemFileSystem.Create;
    try
      PluginInMemFS.PluginReference := APluginReference;

      if not PluginInMemFS.FileExistsInMem(AOnActionPlugin_InMemFS, AFnm) then
        raise Exception.Create('File not found in In-Mem FS: ' + #13#10 + AFnm);

      Content.SetSize(PluginInMemFS.GetFileSize(AOnActionPlugin_InMemFS, AFnm));
      PluginInMemFS.LoadFileFromMem(AOnActionPlugin_InMemFS, AFnm, Content.Memory);

      Content.Position := 0;
      Ini := TClkIniReadonlyFile.Create(Content);
    finally
      PluginInMemFS.Free;
      Content.Free;
    end;
  end
  else
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


procedure SaveClkPrf(APluginReference: Pointer; AOnActionPlugin_InMemFS: TOnActionPlugin_InMemFS; AFnm: string; var AProfiles: TClkFindControlMatchBitmapTextDistArr);
var
  Ini: TClkIniFile;
  i: Integer;
begin
  //if Pos('MEM:', UpperCase(AFnm)) = 1 then
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
    R1, R2, G1, G2, B1, B2: Byte;
    IntervalR, IntervalG, IntervalB: Byte;
    MaxIntervalIndex: Byte; //0 = R, 1 = G, 2 = B
  end;

procedure GetFieldRangeFromString(ARangeStr: string; AIsHexColor: Boolean; out ARange: TRange);
var
  MinValueStr, MaxValueStr: string;
  ph: Integer;
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
    RedGreenBlue(ARange.StartColor, ARange.R1, ARange.G1, ARange.B1);
    RedGreenBlue(ARange.EndColor, ARange.R2, ARange.G2, ARange.B2);
    ARange.IntervalR := Abs(SmallInt(ARange.R1) - SmallInt(ARange.R2));
    ARange.IntervalG := Abs(SmallInt(ARange.G1) - SmallInt(ARange.G2));
    ARange.IntervalB := Abs(SmallInt(ARange.B1) - SmallInt(ARange.B2));

    if ARange.IntervalR > ARange.IntervalG then
    begin
      ARange.MinValue := ARange.R1;
      ARange.MaxValue := ARange.R2;
      ARange.MaxIntervalIndex := 0;

      if ARange.IntervalB > ARange.IntervalR then
      begin
        ARange.MinValue := ARange.B1;
        ARange.MaxValue := ARange.B2;
        ARange.MaxIntervalIndex := 2;
      end;
    end
    else
    begin
      ARange.MinValue := ARange.G1;
      ARange.MaxValue := ARange.G2;
      ARange.MaxIntervalIndex := 1;

      if ARange.IntervalB > ARange.IntervalG then
      begin
        ARange.MinValue := ARange.B1;
        ARange.MaxValue := ARange.B2;
        ARange.MaxIntervalIndex := 2;
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


procedure DrawWipeRect(ACanvas: TCanvas; NewWidth, NewHeight: Integer);
begin
  ACanvas.Lock;
  try
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Brush.Color := clRed;
    ACanvas.Pen.Color := clRed;
    ACanvas.Rectangle(0, 0, NewWidth {- 1}, NewHeight {- 1});
  finally
    ACanvas.Unlock
  end;
end;


procedure WipeBitmap(ABitmap: TBitmap; NewWidth, NewHeight: Integer);
begin
  //ABitmap.Clear;
  ABitmap.SetSize(NewWidth, NewHeight);
  DrawWipeRect(ABitmap.Canvas, NewWidth, NewHeight);
end;


procedure GetAllFieldRanges(AField: string; AIsHexColor: Boolean; ADestFieldRanges: TStringList);
var
  TempRanges: TStringList;
  i, j: Integer;
  Range: TRange;
  //Bmp: TBitmap;
  //GradRect: TRect;
  R, G, B: Byte;
  MaxRGBInterval: Integer;
  RInc, GInc, BInc: Double;
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
          //Bmp := TBitmap.Create;
          //try
          //  Bmp.PixelFormat := pf24bit;
          //  WipeBitmap(Bmp, Range.MaxValue - Range.MinValue + 1, 3);
          //
          //  Bmp.Canvas.Pen.Style := psSolid; //gradients are painted with pen style
          //  Bmp.Canvas.Brush.Style := bsClear;
          //
          //  GradRect.Left := 0;
          //  GradRect.Top := 0;
          //  GradRect.Width := Bmp.Width;
          //  GradRect.Height := 3;
          //  Bmp.Canvas.GradientFill(GradRect, Range.StartColor, Range.EndColor, gdHorizontal);
          //
          //  for j := 0 to Bmp.Width - 1 do
          //    ADestFieldRanges.Add(IntToHex(Bmp.Canvas.Pixels[j, 1], 6));
          //finally
          //  Bmp.Free;
          //end;

          //Alternate implementation, without bitmap and gradient fill. This is a bit more accurate. Still not perfect, because of rounding.
          MaxRGBInterval := Range.MaxValue - Range.MinValue + 1;

          if Range.IntervalR = 0 then
            Range.IntervalR := 1;

          if Range.IntervalG = 0 then
            Range.IntervalG := 1;

          if Range.IntervalB = 0 then
            Range.IntervalB := 1;

          case Range.MaxIntervalIndex of
            0:  //R;
            begin
              RInc :=                                  1 * Sign(SmallInt(Range.R2) - SmallInt(Range.R1));
              GInc := (Range.IntervalG / MaxRGBInterval) * Sign(SmallInt(Range.G2) - SmallInt(Range.G1));
              BInc := (Range.IntervalB / MaxRGBInterval) * Sign(SmallInt(Range.B2) - SmallInt(Range.B1));
            end;  //R

            1:  //G;
            begin
              RInc := (Range.IntervalR / MaxRGBInterval) * Sign(SmallInt(Range.R2) - SmallInt(Range.R1));
              GInc :=                                  1 * Sign(SmallInt(Range.G2) - SmallInt(Range.G1));
              BInc := (Range.IntervalB / MaxRGBInterval) * Sign(SmallInt(Range.B2) - SmallInt(Range.B1));
            end;  //G

            2:  //G;
            begin
              RInc := (Range.IntervalR / MaxRGBInterval) * Sign(SmallInt(Range.R2) - SmallInt(Range.R1));
              GInc := (Range.IntervalG / MaxRGBInterval) * Sign(SmallInt(Range.G2) - SmallInt(Range.G1));
              BInc :=                                  1 * Sign(SmallInt(Range.B2) - SmallInt(Range.B1));
            end;  //G
          end; //case

          for j := 0 to MaxRGBInterval - 1 do
          begin
            R := Round(Range.R1 + j * RInc);
            G := Round(Range.G1 + j * GInc);
            B := Round(Range.B1 + j * BInc);

            ADestFieldRanges.Add(IntToHex(RGBToColor(R, G, B), 6));
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
                                      AMatchBitmapText[n].FontQuality := {FontQuality_AsStringToValue} TFontQuality(StrToIntDef(AllFieldsRanges[8].Strings[r8], 0));
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
  //Template: TClkActionsRecArr;
  //Content: TStringList;
  //TempAction: TClkActionRec;
begin
  SetLength(AAction.MatchBitmapText, 0);
  for i := 0 to Length(AProfiles) - 1 do
    AddProfileRangesToActionProfiles(AAction.MatchBitmapText, AProfiles[i]);

  //For Debugging only:
  //Content := TStringList.Create;
  //try
  //  SetLength(Template, 1);
  //  TempAction.ActionOptions.Action := acFindSubControl;
  //  TempAction.ActionOptions.ActionName := 'Debugging';
  //  TempAction.FindSubControlOptions := AAction;
  //  CopyActionContent(TempAction, Template[0]);
  //  SaveTemplateWithCustomActionsToStringList_V2(Content, Template, 'Debugging...', 'NoIcon.ico');
  //
  //  Content.SaveToFile(ExtractFilePath(ParamStr(0)) + '..\UIClickerDistFindSubControlPlugin\Tests\TestFiles\DebuggingGeneratedCustomProfiles.txt');
  //finally
  //  Content.Free;
  //end;
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


const
  CNoFiles = 'No files in In-Mem FS...';


procedure TfrmDistFindSubControlPropertyEditor.InMemFSItemClick(Sender: TObject);
var
  Fnm: string;
begin
  Fnm := StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]);
  if Fnm = CNoFiles then
  begin
    MessageBoxFunction(PChar(Fnm), 'UIClickerDistFindSubControl', 0);
    Exit;
  end;

  lbeClkPrfFile.Text := Fnm;
end;


procedure TfrmDistFindSubControlPropertyEditor.spdbtnBrowseInMemFSClick(
  Sender: TObject);
var
  PluginInMemFS: TPluginInMemFileSystem;
  ListOfFiles: TStringList;
  i: Integer;
  TempMenuItem: TMenuItem;
begin
  PluginInMemFS := TPluginInMemFileSystem.Create;
  try
    PluginInMemFS.PluginReference := FPluginReference;

    ListOfFiles := TStringList.Create;
    try
      ListOfFiles.LineBreak := #13#10;
      ListOfFiles.Text := PluginInMemFS.ListMemFilesAsString(FOnActionPlugin_InMemFS);

      if ListOfFiles.Count = 0 then
        ListOfFiles.Add(CNoFiles);

      MenuItem_BrowseInMemFS.Clear;
      for i := 0 to ListOfFiles.Count - 1 do
      begin
        TempMenuItem := TMenuItem.Create(Self);
        TempMenuItem.Caption := ListOfFiles.Strings[i];
        TempMenuItem.OnClick := InMemFSItemClick;

        MenuItem_BrowseInMemFS.Add(TempMenuItem);
      end;
    finally
      ListOfFiles.Free;
    end;
  finally
    PluginInMemFS.Free;
  end;

  pmBrowseInMemFS.PopUp;
end;


procedure TfrmDistFindSubControlPropertyEditor.btnSaveEmptyFileToInMemFSClick(Sender: TObject);
var
  PluginInMemFS: TPluginInMemFileSystem;
begin
  PluginInMemFS := TPluginInMemFileSystem.Create;
  try
    PluginInMemFS.PluginReference := FPluginReference;
    PluginInMemFS.SaveFileToMem(FOnActionPlugin_InMemFS, 'Random_' + DateTimeToStr(Now), nil, 0);
  finally
    PluginInMemFS.Free;
  end;
end;


procedure TfrmDistFindSubControlPropertyEditor.btnLoadFileFromDiskAndSaveItToInMemFSClick
  (Sender: TObject);
var
  PluginInMemFS: TPluginInMemFileSystem;
  TempOpenDialog: TOpenDialog;
  Content: TMemoryStream;
  Fnm: string;
begin
  TempOpenDialog := TOpenDialog.Create(nil);
  try
    TempOpenDialog.Filter := 'Font profiles (*.clkprf)|*.clkprf|All files (*.*)|*.*';
    if not TempOpenDialog.Execute then
      Exit;

    Content := TMemoryStream.Create;
    PluginInMemFS := TPluginInMemFileSystem.Create;
    try
      Content.LoadFromFile(TempOpenDialog.FileName);

      PluginInMemFS.PluginReference := FPluginReference;
      Fnm := 'Mem:' + Copy(TempOpenDialog.FileName, 3, MaxInt);
      PluginInMemFS.SaveFileToMem(FOnActionPlugin_InMemFS, Fnm, Content.Memory, Content.Size);

      MessageBoxFunction(PChar('ClkPrf filesize: ' + IntToStr(Content.Size) + #13#10 +
                               '  in mem size: ' + IntToStr(PluginInMemFS.GetFileSize(FOnActionPlugin_InMemFS, Fnm))), 'Dist', 0);
    finally
      PluginInMemFS.Free;
      Content.Free;
    end;
  finally
    TempOpenDialog.Free;
  end;
end;

end.

