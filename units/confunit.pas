unit ConfUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, FileUtil, IniFiles, LazFileUtils;

type

  { TConf }

  TConf = class
  private
    NodesToBeRemoved: TStringList; //
    TreesToBeRemoved: TStringList; //
  public
    PreferenceDict: TStringList; //
    function GetPreferenceValue(Key: String; Def: String): String;
    function GetCompileFmt: String;
    function GetEvaluateTimeLimit: Extended;
    function CreateTempDir: String;
    function ExecuteOpenDialog: String;
    function ExecuteSaveDialog: String;
    procedure RemoveLater(FileName: String; Recursively: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Conf: TConf;

implementation

{ TConf }

function TConf.GetPreferenceValue(Key: String; Def: String): String;
var
  Index: Integer;
begin
  Index := PreferenceDict.IndexOfName(Key);
  if Index=-1 then Index := PreferenceDict.Add(Key+'='+Def);
  Result := PreferenceDict.ValueFromIndex[Index];
end;

function TConf.GetCompileFmt: String;
begin
  Result := GetPreferenceValue('CompileFmt', 'g++ -O2 "%s" -o "%s"');
end;

function TConf.GetEvaluateTimeLimit: Extended;
begin
  Result := StrToFloatDef(GetPreferenceValue('EvaluateTimeLimit', '1.0'), 1.0);
end;

function TConf.CreateTempDir: String;
begin
  Result := GetTempFileNameUTF8('', '');
  if not CreateDirUTF8(Result) then
    raise Exception.CreateFmt('Cannot create directory "%s"', [Result]);
end;

function TConf.ExecuteOpenDialog: String;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  if Dialog.Execute then
    Result := Dialog.FileName
  else
    Result := '';
  Dialog.Free;
end;

function TConf.ExecuteSaveDialog: String;
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(nil);
  if Dialog.Execute then
    Result := Dialog.FileName
  else
    Result := '';
  Dialog.Free;
end;

procedure TConf.RemoveLater(FileName: String; Recursively: Boolean);
begin
  if Recursively then
    TreesToBeRemoved.Add(FileName)
  else
    NodesToBeRemoved.Add(FileName);
end;

constructor TConf.Create;
begin
  NodesToBeRemoved := TStringList.Create;
  TreesToBeRemoved := TStringList.Create;
  PreferenceDict := TStringList.Create;
  if FileExists(GetAppConfigFile(False)) then
  PreferenceDict.LoadFromFile(GetAppConfigFile(False));
end;

destructor TConf.Destroy;
var
  FileName: String;
begin
  for FileName in NodesToBeRemoved do
    if DirectoryExists(FileName) then
      RemoveDir(FileName)
    else
      DeleteFile(FileName);

  for FileName in TreesToBeRemoved do
    DeleteDirectory(FileName, False);

  ForceDirectories(ExtractFileDir(GetAppConfigFile(False)));
  PreferenceDict.SaveToFile(GetAppConfigFile(False));

  NodesToBeRemoved.Free;
  TreesToBeRemoved.Free;
  PreferenceDict.Free;
  inherited Destroy;
end;

initialization
  Conf := TConf.Create;

finalization
  Conf.Free;

end.

