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

  { TfrmDistFindSubControlPropertyEditor }

  TfrmDistFindSubControlPropertyEditor = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lbeProperty: TLabeledEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;


function EditCustomFontProfilesProperty(ACurrentValue: string; out ANewValue: string): Boolean;


implementation

{$R *.frm}

function EditCustomFontProfilesProperty(ACurrentValue: string; out ANewValue: string): Boolean;
var
  frmDistFindSubControlPropertyEditor: TfrmDistFindSubControlPropertyEditor;
begin
  Application.CreateForm(TfrmDistFindSubControlPropertyEditor, frmDistFindSubControlPropertyEditor);

  //decode ACurrentValue
  frmDistFindSubControlPropertyEditor.lbeProperty.Text := ACurrentValue;

  frmDistFindSubControlPropertyEditor.ShowModal;
  Result := frmDistFindSubControlPropertyEditor.Tag = 1;
  if Result then
  begin
    //encode
    ANewValue := frmDistFindSubControlPropertyEditor.lbeProperty.Text;
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


procedure TfrmDistFindSubControlPropertyEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

end.

