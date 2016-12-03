program coracabo;

{$mode objfpc}{$H+}

uses
  sysutils, Classes,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainFormUnit, ExecFormUnit, ConfUnit, StringListEditorUnit
  { you can add units after this };

{$R *.res}

begin
  chdir(ExtractFileDir(ParamStr(0)));
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

