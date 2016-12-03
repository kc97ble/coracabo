unit StringListEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ButtonPanel;

type

  { TStringListEditor }

  TStringListEditor = class(TForm)
    ButtonPanel1: TButtonPanel;
    SynEdit1: TSynEdit;
  private
    { private declarations }
  public
    class procedure DefaultExecute(List: TStringList);
  end;

implementation

{$R *.lfm}

{ TStringListEditor }

class procedure TStringListEditor.DefaultExecute(List: TStringList);
var
  Dialog: TStringListEditor;
begin
  Dialog := TStringListEditor.Create(nil);
  Dialog.SynEdit1.Lines.Assign(List);
  if Dialog.ShowModal=mrOK then
  List.Assign(Dialog.SynEdit1.Lines);
  Dialog.Free;
end;

end.

