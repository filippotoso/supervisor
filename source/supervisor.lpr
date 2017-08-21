program supervisor;

{$mode objfpc}{$H+}
uses
{$ifdef unix}
  cthreads, cmem,
{$endif}
  SysUtils,
  Classes,
  IniFiles,
  SupervisorThread;

var
  I: integer;
  Directory, LockFileName, EndFileName, ConfigFileName: string;
  Lock: NativeInt;

  Sections: TStringList;
  Threads: TThreadList;
  ThreadsList: TList;
  Thread: TSupervisorThread;
  Config: TINIFile;

begin

  if (ParamCount < 1) then
  begin
    WriteLn('Please provice a run folder');
    Exit;
  end;

  ConfigFileName := ParamStr(1);

  Directory := IncludeTrailingPathDelimiter(ExtractFileDir(ConfigFileName));

  LockFileName := Directory + 'supervisor.lock';
  EndFileName := Directory + 'supervisor.end';

  Lock := FileCreate(LockFileName, fmOpenReadWrite or fmShareExclusive);
  try

    // Already Running, terminating...
    if (Lock = feInvalidHandle) then
      Exit;

    if (not FileExists(ConfigFileName)) then
    begin
      WriteLn(Format('Please create a valid config file (%s)', [ConfigFileName]));
      Exit;
    end;

    // End file exists, terminating...
    if (FileExists(EndFileName)) then
      Exit;

    Sections:=TStringList.Create;
    Threads:=TThreadList.Create;
    Config:=TIniFile.Create(ConfigFileName);

    try

      Config.ReadSections(Sections);

      for I := 0 to Sections.Count - 1 do
      begin
        Thread := TSupervisorThread.Create;
        Thread.EndFileName := EndFileName;
        Thread.Command := Config.ReadString(Sections.Strings[I], 'Command', '');
        Thread.Parameters := Config.ReadString(Sections.Strings[I], 'Parameters', '');
        Thread.Start;
        Threads.Add(Thread);
      end;

      while not FileExists(EndFileName) do
        Sleep(30 * 1000);

      Sleep(30 * 1000);

      ThreadsList := Threads.LockList;
      try
        for I := 0 to ThreadsList.Count - 1 do
          TSupervisorThread(ThreadsList.Items[I]).Terminate();
      finally
        Threads.UnlockList;
      end;

      Sleep(30 * 1000);

      if FileExists(EndFileName) then
        DeleteFile(EndFileName);

    finally
      Threads.Free;
      Sections.Free;
      Config.Free;
    end;

  finally
    FileClose(Lock);
  end;

end.
