unit ExecFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Spin, process, math;

type

  TJob = record
    Command: String;
    ExitCode: Integer;
    ExitStatus: Integer;
    Finished: Boolean;
    Running: Boolean;
    StdOutText: String;
    StdErrText: String;
  end;

  { TExecForm }

  TExecForm = class(TForm)
    DLabel: TLabel;
    DLabel2: TLabel;
    OListBox: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    DEdit: TSpinEdit;
    DMemo: TSynEdit;
    SynAnySyn1: TSynAnySyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure DEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OListBoxDblClick(Sender: TObject);
  private
    procedure UpdateDetails(UpdateNow: Boolean);
    procedure UpdateOverview(UpdateNow: Boolean);
  public
    Jobs: array of TJob;
    procedure UpdateView(UpdateNow: Boolean);
    procedure StartExecute(Data: PtrInt);
    procedure Execute;
    procedure Clear;
    procedure AddJob(Command: String);
  end;

var
  ExecForm: TExecForm;

implementation

{$R *.lfm}

function _GetJobStatus(J: TJob): String;
begin
  if J.Running then
    exit('Running');
  if not J.Finished then
    exit('Pending');
  if J.ExitCode<>0 then
    exit('Failed');
  if J.ExitStatus<>0 then
    exit('Fatal');
  exit('OK');
end;

{ TExecForm }

procedure TExecForm.FormCreate(Sender: TObject);
begin

end;

procedure TExecForm.DEditChange(Sender: TObject);
begin
  UpdateDetails(True);
end;

procedure TExecForm.FormDestroy(Sender: TObject);
begin

end;

procedure TExecForm.FormShow(Sender: TObject);
begin
  Application.QueueAsyncCall(@StartExecute, 0);
end;

procedure TExecForm.OListBoxDblClick(Sender: TObject);
begin
  if OListBox.ItemIndex = -1 then exit;
  DEdit.Value := OListBox.ItemIndex;
  PageControl1.TabIndex:=TabSheet2.TabIndex;
end;

function _Decorate(const S: String; N: Integer): String;
begin
  if Length(S) <= N then
    Result := S
  else
    Result := LeftStr(S, N) + '...';
end;

procedure TExecForm.UpdateDetails(UpdateNow: Boolean);
var
  J: TJob;
begin
  if not InRange(DEdit.Value, 0, Length(Jobs)-1) then
  begin
    DMemo.Clear;
    DLabel2.Caption:='';
  end
  else begin
    J := Jobs[DEdit.Value];
    DMemo.Text := J.Command + LineEnding;
    if J.StdOutText <> '' then
      DMemo.Text := DMemo.Text + StringOfChar('-', DMemo.RightEdge) + LineEnding
      + J.StdOutText + LineEnding;
    if J.StdErrText <> '' then
      DMemo.Text := DMemo.Text + StringOfChar('=', DMemo.RightEdge) + LineEnding
      + J.StdErrText + LineEnding;
    DLabel2.Caption:=_GetJobStatus(Jobs[DEdit.Value]);
  end;
  if UpdateNow then
    Application.ProcessMessages;
end;

procedure TExecForm.UpdateOverview(UpdateNow: Boolean);
var
  J: TJob;
  i: Integer;
begin
  with OListBox.Items do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Length(Jobs)-1 do
      Add(Format('Job #%d: %s', [i, _GetJobStatus(Jobs[i])]));
    EndUpdate;
  end;
  if UpdateNow then
    Application.ProcessMessages;
end;

procedure TExecForm.UpdateView(UpdateNow: Boolean);
var
  J: TJob;
  CountFinished: Integer = 0;
begin
  for J in Jobs do
    if J.Finished then
      CountFinished += 1;
  with ProgressBar1 do
  begin
    Min := 0;
    Max := Length(Jobs);
    Position := CountFinished;
  end;

  UpdateOverview(False);
  UpdateDetails(False);
  if UpdateNow then
    Application.ProcessMessages;
end;

procedure TExecForm.StartExecute(Data: PtrInt);
begin
  Execute;
end;

function _CreateProcessInstance(Executable: String;
  Parameters: array of String): TProcess;
begin
  Result := TProcess.Create(nil);
  Result.Executable := Executable;
  Result.Parameters.AddStrings(Parameters);
  Result.Options:=Result.Options+[poUsePipes];
end;

procedure TExecForm.Execute;
var
  i: Integer;
  P: TProcess;
  T0: String[192+1];
  T1: String[192+1];
begin
  if Length(Jobs)=1 then
    PageControl1.TabIndex:=TabSheet2.TabIndex
  else
    PageControl1.TabIndex:=TabSheet1.TabIndex;

  DEdit.MaxValue:=Length(Jobs)-1;
  UpdateView(True);
  for i := 0 to Length(Jobs)-1 do
  begin
    Jobs[i].Running := True;
    P := _CreateProcessInstance('/bin/sh', ['-c', Jobs[i].Command]);
    P.Execute;

    repeat
      T0[0] := Char(P.Output.Read(T0[1], Min(192, P.Output.NumBytesAvailable)));
      Jobs[i].StdOutText += T0;
      T1[0] := Char(P.Stderr.Read(T1[1], Min(192, P.Stderr.NumBytesAvailable)));
      Jobs[i].StdErrText += T1;
      if (T0='') and (T1='') and not P.Running then
        Break;
      UpdateView(True);
      Sleep(50);
    until False;

    Jobs[i].ExitCode := P.ExitCode;
    Jobs[i].ExitStatus := P.ExitStatus;
    Jobs[i].Finished := True;
    Jobs[i].Running := False;
    UpdateView(True);
    P.Free;
  end;
end;

procedure TExecForm.Clear;
begin
  SetLength(Jobs, 0);
end;

procedure TExecForm.AddJob(Command: String);
begin
  SetLength(Jobs, Length(Jobs)+1);
  Jobs[Length(Jobs)-1].Command := Command;
end;

initialization
  ExecForm := TExecForm.Create(nil);

finalization
  ExecForm.Free;

end.

