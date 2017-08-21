unit SupervisorThread;

{$mode objfpc}{$H+}

interface

uses
  Process, Classes, SysUtils;

Type

    { TSupervisorThread }

    TSupervisorThread = class(TThread)
    private
      FArguments: array of string;
      FPath,FFileName,FCommand: string;
      FEndFileName: string;
      FParameters: string;
      FProcess: TProcess;
      procedure SetCommand(AValue: string);
      procedure SetParameters(AValue: string);
    protected
      FTerminated:Boolean;
      procedure Execute; override;
    public
      constructor Create;
      destructor Destroy; override;

      procedure Terminate;

      property Command:string read FCommand write SetCommand;
      property Parameters:string read FParameters write SetParameters;
      property EndFileName:string read FEndFileName write FEndFileName;

    end;


implementation

{ TSupervisorThread }

procedure TSupervisorThread.SetCommand(AValue: string);
begin

  AValue := Trim(AValue);

  if FCommand = AValue then
    Exit;

  FCommand := AValue;

  FPath := ExtractFilePath(FCommand);
  FFileName := ExtractFileName(FCommand);

end;

procedure TSupervisorThread.SetParameters(AValue: string);
var
  Params:TStringList;
  I:Integer;
begin

  if FParameters = AValue then
    Exit;

  FParameters := AValue;

  Params := TStringList.Create;
  try

    Params.Delimiter := ' ';
    Params.QuoteChar := '"';
    Params.DelimitedText := FParameters;

    SetLength(FArguments, Params.Count);

    for I := 0 to Params.Count -1 do
      FArguments[I] := Params.Strings[I];

  finally
    Params.Free;
  end;

end;

procedure TSupervisorThread.Execute;
var
   I: Integer;
begin

  FTerminated := False;

  while not FTerminated do
  begin

    try

        FProcess.Executable := FCommand;

        for I := 0 to Length(FArguments) - 1 do
            FProcess.Parameters.Add(FArguments[I]);

        FProcess.Options := FProcess.Options + [poWaitOnExit];

        FProcess.Execute;

    except

    end;

    if FileExists(FEndFileName) then
    begin
      Terminate;
      Break;
    end;

  end;

end;

constructor TSupervisorThread.Create;
begin
  inherited Create(True);
  FProcess := TProcess.Create(nil);
end;

destructor TSupervisorThread.Destroy;
begin
  FProcess.Free;
  inherited Destroy;
end;

procedure TSupervisorThread.Terminate;
begin
  FTerminated := True;
  FProcess.Terminate(0);
end;

end.

