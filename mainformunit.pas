unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, process, ExecFormUnit, LazFileUtils, ConfUnit, BaseUnix,
  zipper, StringListEditorUnit;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DLoadZipBtn: TButton;
    DLoadDirBtn: TButton;
    DClearBtn: TButton;
    DLabel2: TLabel;
    SLabel2: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    SSubmitBtn: TButton;
    SClearBtn: TButton;
    SCompileBtn: TButton;
    DLabel: TLabel;
    SLabel: TLabel;
    DListBox: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    SMemo: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DLoadZipBtnClick(Sender: TObject);
    procedure DLoadDirBtnClick(Sender: TObject);
    procedure DClearBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SClearBtnClick(Sender: TObject);
    procedure SCompileBtnClick(Sender: TObject);
    procedure SSubmitBtnClick(Sender: TObject);
  private
    procedure FormIdle(Sender: TObject; var Done: Boolean);
  public
    {}
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DLoadDirBtnClick(Sender: TObject);
var
  Dir: String;
  OutputStr: String;
begin
  if SelectDirectory('', '', Dir) then
  begin
    RunCommand('/usr/bin/python', ['testlide.py', Dir], OutputStr);
    DListBox.Items.Text:=OutputStr;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  FileName: String;
begin
  FileName := Conf.ExecuteOpenDialog;
  if FileName='' then exit;
  SMemo.Lines.LoadFromFile(FileName);
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  FileName: String;
begin
  FileName := Conf.ExecuteSaveDialog;
  if FileName='' then exit;
  SMemo.Lines.SaveToFile(FileName);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  TStringListEditor.DefaultExecute(Conf.PreferenceDict);
end;

procedure TMainForm.DLoadZipBtnClick(Sender: TObject);
var
  FileNameValue: String;
  OutputStr: String;
begin
  FileNameValue := Conf.ExecuteOpenDialog;
  if FileNameValue='' then exit;

  with TUnZipper.Create do
  begin
    FileName:=FileNameValue;
    OutputPath:=Conf.CreateTempDir;
    Examine;
    UnZipAllFiles;
    RunCommand('/usr/bin/python', ['testlide.py', OutputPath], OutputStr);
    DListBox.Items.Text:=OutputStr;
    Conf.RemoveLater(OutputPath, True);
    Free;
  end;
end;

procedure TMainForm.DClearBtnClick(Sender: TObject);
begin
  DListBox.Clear;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.AddOnIdleHandler(@FormIdle);
end;

procedure TMainForm.FormDeactivate(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnIdleHandler(@FormIdle);
end;

procedure TMainForm.SClearBtnClick(Sender: TObject);
begin
  SMemo.Clear;
end;

procedure TMainForm.SCompileBtnClick(Sender: TObject);
var
  Dir: String;
  SolutionFile: String;
  ExecutableFile: String;
begin
  Dir := Conf.CreateTempDir;
  chdir(Dir);
  SolutionFile := ConcatPaths([Dir, 'solution.cpp']);
  ExecutableFile := ConcatPaths([Dir, 'solution']);
  SMemo.Lines.SaveToFile(SolutionFile);

  ExecForm.Clear;
  ExecForm.AddJob(Format(Conf.GetCompileFmt, [SolutionFile, ExecutableFile]));
  ExecForm.ShowModal;

  DeleteFile(ExecutableFile);
  DeleteFile(SolutionFile);
  chdir(ExtractFileDir(ParamStr(0)));
  RemoveDir(Dir);
end;

procedure TMainForm.SSubmitBtnClick(Sender: TObject);
var
  Dir, DIFile, DOFile, SolutionFile, ExecutableFile: String;
  i: Integer;
begin
  Dir := Conf.CreateTempDir;
  chdir(Dir);
  SolutionFile := ConcatPaths([Dir, 'solution.cpp']);
  ExecutableFile := ConcatPaths([Dir, 'solution']);
  SMemo.Lines.SaveToFile(SolutionFile);

  ExecForm.Clear;
  ExecForm.AddJob(Format(Conf.GetCompileFmt, [SolutionFile, ExecutableFile]));
  ExecForm.ShowModal;

  if ExecForm.Jobs[0].ExitCode = 0 then
  begin
    ExecForm.Clear;
    for i := 0 to DListBox.Count div 2 - 1 do
    begin
      DIFile := DListBox.Items[i*2];
      DOFile := DListBox.Items[i*2+1];
      ExecForm.AddJob(Format('/usr/bin/python "%s" "%s" "%s" "%s" -t %f', [
        ConcatPaths([ExtractFileDir(ParamStr(0)), 'evaluate.py']),
        ExecutableFile, DIFile, DOFile, Conf.GetEvaluateTimeLimit]));
    end;
    ExecForm.ShowModal;
  end;

  DeleteFile(ExecutableFile);
  DeleteFile(SolutionFile);
  chdir(ExtractFileDir(ParamStr(0)));
  RemoveDir(Dir);
end;

procedure TMainForm.FormIdle(Sender: TObject; var Done: Boolean);
begin
  DLabel2.Caption:=Format('%d test(s)', [DListBox.Count]);
  SLabel2.Caption:=Format('%d line(s)', [SMemo.Lines.Count]);
  Self.Caption := 'Coracabo';
end;

end.

