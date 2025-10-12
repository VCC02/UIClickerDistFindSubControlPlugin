{
    Copyright (C) 2025 VCC
    creation date: 12 Oct 2025
    initial release date: 12 Oct 2025

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


unit FontSorting;

{$mode Delphi}

interface

uses
  Classes, SysUtils, ClickerUtils, DistFindSubControlCommonConsts, ClickerActionPlugins;


var
  AllWorkersForSorting: TWorkerArr = nil;
  AddToLogCallbackForSorting: TOnActionPlugin_AddToLog = nil;
  FindSubControlFontsForSorting: TClkFindControlMatchBitmapTextArr = nil;
  FontsUsedByPmtvsForSorting: TStringArray = nil;


procedure SortByDupFonts_Txt(var AArr: TIntArr);
procedure SortByDupFonts_Pmtv(var AArr: TIntArr);
procedure SortByTxtTaskCount(var AArr: TIntArr);
procedure SortByPmtvTaskCount(var AArr: TIntArr);

function FontExistsInList(AFontName, AListOfFontsStr: string): Boolean;
function MultipleFontsExistInList(AListOfSearchedFontNames, AListOfFontsStr: string): Boolean;


implementation


uses
  IntegerList;


procedure AddToLog(AMsg: string);  //thread safe
begin
  if @AddToLogCallbackForSorting <> nil then
    AddToLogCallbackForSorting(DefaultPluginReference, @AMsg[1]);
end;


function FontExistsInList(AFontName, AListOfFontsStr: string): Boolean;
var
  ListOfFonts: TStringList;
begin
  ListOfFonts := TStringList.Create;
  try
    ListOfFonts.LineBreak := #13#10;
    ListOfFonts.Text := FastReplace_45ToReturn(AListOfFontsStr);
    Result := ListOfFonts.IndexOf(AFontName) > -1;
  finally
    ListOfFonts.Free;
  end;
end;


//Searches for fonts from AListOfSearchedFontNames in AListOfFontsStr. All must exist.
function MultipleFontsExistInList(AListOfSearchedFontNames, AListOfFontsStr: string): Boolean;
var
  ListOfSearchedFonts: TStringList;
  ListOfFonts: TStringList;
  i: Integer;
begin
  if AListOfSearchedFontNames = '' then
  begin
    //Result := False;
    Result := True;
    Exit;
  end;

  ListOfFonts := TStringList.Create;
  ListOfSearchedFonts := TStringList.Create;
  try
    ListOfFonts.LineBreak := #13#10;
    ListOfSearchedFonts.LineBreak := #13#10;
    ListOfFonts.Text := FastReplace_45ToReturn(AListOfFontsStr);
    ListOfSearchedFonts.Text := FastReplace_45ToReturn(AListOfSearchedFontNames);  //probably the FastReplace_45ToReturn is not needed here, since this is used for fonts from pmtv files, which are stored as #13#10 separated items.

    Result := True;
    for i := 0 to ListOfSearchedFonts.Count - 1 do
      Result := Result and (ListOfFonts.IndexOf(ListOfSearchedFonts.Strings[i]) > -1);
  finally
    ListOfFonts.Free;
    ListOfSearchedFonts.Free;
  end;
end;


function CompFuncByTxtTaskCount(const a, b: Int64): Integer;
begin
  try
    if AllWorkersForSorting[a].TxtCntW < AllWorkersForSorting[b].TxtCntW then
      Result := -1
    else
      if AllWorkersForSorting[a].TxtCntW = AllWorkersForSorting[b].TxtCntW then
        Result := 0
      else
        Result := 1;
  except
    Result := -1;
    AddToLog('Ex on CompFuncByTxtTaskCount  a = ' + IntToStr(a) + '  b = ' + IntToStr(b) + '  Len(AllWorkers) = ' + IntToStr(Length(AllWorkersForSorting)));
  end;
end;


function CompFuncByPmtvTaskCount(const a, b: Int64): Integer;
begin
  try
    if AllWorkersForSorting[a].PmtvCntW < AllWorkersForSorting[b].PmtvCntW then
      Result := -1
    else
      if AllWorkersForSorting[a].PmtvCntW = AllWorkersForSorting[b].PmtvCntW then
        Result := 0
      else
        Result := 1;
  except
    Result := -1;
    AddToLog('Ex on CompFuncByPmtvTaskCount  a = ' + IntToStr(a) + '  b = ' + IntToStr(b) + '  Len(AllWorkers) = ' + IntToStr(Length(AllWorkersForSorting)));
  end;
end;


type
  TCompFunc = function(const a, b: Int64): Integer;

procedure CustomSortByTaskCount(var AArr: TIntArr; ACmpFunc: TCompFunc);
var
  SortingArray: TInt64List;
  i: Integer;
begin
  SortingArray := TInt64List.Create;
  try
    SortingArray.Capacity := Length(AArr);
    for i := 0 to Length(AArr) - 1 do
      SortingArray.Add(AArr[i]);

    SortingArray.Sort(@ACmpFunc);

    for i := 0 to Length(AArr) - 1 do
      AArr[i] := SortingArray.Items[i];
  finally
    SortingArray.Free;
  end;
end;


procedure SortByTxtTaskCount(var AArr: TIntArr);
begin
  CustomSortByTaskCount(AArr, @CompFuncByTxtTaskCount);
end;


procedure SortByPmtvTaskCount(var AArr: TIntArr);
begin
  CustomSortByTaskCount(AArr, @CompFuncByPmtvTaskCount);
end;


function FontExistsInAllWorkers(AFontName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(AllWorkersForSorting) - 1 do                          //with so many nested loops, these arrays should be cached
    if not FontExistsInList(AFontName, AllWorkersForSorting[i].Fonts) then
    begin
      Result := False;
      Break;
    end;
end;


function MultipleFontsExistInAllWorkers(AFontNames: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Length(AllWorkersForSorting) - 1 do                          //with so many nested loops, these arrays should be cached
    if not MultipleFontsExistInList(AFontNames, AllWorkersForSorting[i].Fonts) then
    begin
      Result := False;
      Break;
    end;
end;


function CompFuncByDupFonts_Txt(const a, b: Int64): Integer;
var
  a_bool: Boolean;
  b_bool: Boolean;
begin
  if FindSubControlFontsForSorting = nil then
  begin
    AddToLog('+++++++++++++++++++++++++++++ FindSubControlFonts = nil');
    Result := -1;
    Exit;
  end;

  if Length(FindSubControlFontsForSorting) = 0 then
    AddToLog('+++++++++++++++++++++++++++++ FindSubControlFonts is empty.');

  if (a > Length(FindSubControlFontsForSorting) - 1) or (b > Length(FindSubControlFontsForSorting) - 1) then
    raise Exception.Create('One of the arguments of the CompFuncByDupFonts_Txt function is out of range.  a = ' + IntToStr(a) + '  b = ' + IntToStr(b) + '  Len(FindSubControlFonts) = ' + IntToStr(Length(FindSubControlFontsForSorting)) + '. If this doesn''t happen on 32-bit, then it must be a problem on 64-bit only.');

  try
    a_bool := FontExistsInAllWorkers(FindSubControlFontsForSorting[a].FontName);
    b_bool := FontExistsInAllWorkers(FindSubControlFontsForSorting[b].FontName);

    Result := 0;
    if not a_bool and b_bool then
      Result := -1
    //else           //the following comparisons are not required:
      //if (a_bool and b_bool) or (not a_bool and not b_bool) then    //"not a_bool and not b_bool" should not be the case
      //  Result := 0
      //else
      //  if a_bool and not b_bool then
      //    Result := 1;
  except
    Result := -1;
    AddToLog('Ex on CompFuncByDupFonts  a = ' + FindSubControlFontsForSorting[a].FontName + '  b = ' + FindSubControlFontsForSorting[b].FontName + '  Len(Fonts) = ' + IntToStr(Length(FindSubControlFontsForSorting)));
  end;
end;


function CompFuncByDupFonts_Pmtv(const a, b: Int64): Integer;
var
  a_bool: Boolean;
  b_bool: Boolean;
begin
  if Length(FontsUsedByPmtvsForSorting) = 0 then
  begin
    AddToLog('+++++++++++++++++++++++++++++ FontsUsedByPmtvs is empty');
    Result := -1;
    Exit;
  end;

  if Length(FontsUsedByPmtvsForSorting) = 0 then
    AddToLog('+++++++++++++++++++++++++++++ FindSubControlFonts is empty.');

  if (a > Length(FontsUsedByPmtvsForSorting) - 1) or (b > Length(FontsUsedByPmtvsForSorting) - 1) then
    raise Exception.Create('One of the arguments of the CompFuncByDupFonts_Pmtv function is out of range.  a = ' + IntToStr(a) + '  b = ' + IntToStr(b) + '  Len(FontsUsedByPmtvs) = ' + IntToStr(Length(FontsUsedByPmtvsForSorting)) + '. If this doesn''t happen on 32-bit, then it must be a problem on 64-bit only.');

  try
    a_bool := MultipleFontsExistInAllWorkers(FontsUsedByPmtvsForSorting[a]);
    b_bool := MultipleFontsExistInAllWorkers(FontsUsedByPmtvsForSorting[b]);

    Result := 0;
    if not a_bool and b_bool then
      Result := -1
    //else           //the following comparisons are not required:
      //if (a_bool and b_bool) or (not a_bool and not b_bool) then    //"not a_bool and not b_bool" should not be the case
      //  Result := 0
      //else
      //  if a_bool and not b_bool then
      //    Result := 1;
  except
    Result := -1;
    AddToLog('Ex on CompFuncByDupFonts  a = ' + FontsUsedByPmtvsForSorting[a] + '  b = ' + FontsUsedByPmtvsForSorting[b] + '  Len(Fonts) = ' + IntToStr(Length(FontsUsedByPmtvsForSorting)));
  end;
end;


procedure SortByDupFonts_Txt(var AArr: TIntArr);
begin
  CustomSortByTaskCount(AArr, @CompFuncByDupFonts_Txt);
end;


procedure SortByDupFonts_Pmtv(var AArr: TIntArr);
begin
  CustomSortByTaskCount(AArr, @CompFuncByDupFonts_Pmtv);
end;

end.

