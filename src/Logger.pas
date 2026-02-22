unit Logger;

interface

//uses Hash;

type
  TLogger = class
  private
  type
    TLevel = (llOff, llCritical, llError, llWarning, llInfo, llDebug, llTrace);
//    TErrorListHash = THash<Cardinal, string>;
  class var
    FLogFileName: string;
    FFileH: THandle;
    FLevel: TLevel;
  public
    //class var ErrorStringIDs: TErrorListHash;
    class constructor Create;
    class destructor Destroy;
    class procedure Init(const LogFileName: string);
    class procedure Shutdown();
    class procedure Log(lv: TLevel; const Msg: string);
    class procedure LogFmt(lv: TLevel; const Msg: string; Values: array of const);
    class procedure Trace(const Msg: string);
    class procedure Debug(const Msg: string);
    class procedure Info(const Msg: string);
    class procedure Warn(const Msg: string);
    class procedure Error(const Msg: string);
    class procedure Crit(const Msg: string);
    class procedure TraceFmt(const Msg: string; Values: array of const);
    class procedure DebugFmt(const Msg: string; Values: array of const);
    class procedure InfoFmt(const Msg: string; Values: array of const);
    class procedure WarnFmt(const Msg: string; Values: array of const);
    class procedure ErrorFmt(const Msg: string; Values: array of const);
    class procedure CritFmt(const Msg: string; Values: array of const);
    class procedure SetLogLevel(lv: TLevel);
    class function GetLogLevel: TLevel;
  end;

implementation

uses  Classes, SysUtils, Winapi.Windows, IOUtils;

//const DEF_LOG_FILENAME = 'FinderX_denug.log';
const
  INVALID_SET_FILE_POINTER = Cardinal(-1);

class constructor TLogger.Create;
begin
  FFileH := INVALID_HANDLE_VALUE;
  FLevel := llInfo;

 { ErrorStringIDs := TErrorListHash.Create;
  ErrorStringIDs[ERROR_FILE_NOT_FOUND] := 'ERROR_FILE_NOT_FOUND'; //3
  ErrorStringIDs[ERROR_PATH_NOT_FOUND] := 'ERROR_PATH_NOT_FOUND'; //5
  ErrorStringIDs[ERROR_MORE_DATA]      := 'ERROR_MORE_DATA'; //234
  ErrorStringIDs[ERROR_NO_TOKEN]       := 'ERROR_NO_TOKEN'; //1008
  ErrorStringIDs[ERROR_NO_MORE_ITEMS]  := 'ERROR_NO_MORE_ITEMS'; //259 }
end;

class procedure TLogger.Crit(const Msg: string);
begin
  Log(llCritical, Msg);
end;

class procedure TLogger.CritFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llCritical, Msg, Values);
end;

class procedure TLogger.Debug(const Msg: string);
begin
  Log(llDebug, Msg);
end;

class procedure TLogger.DebugFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llDebug, Msg, Values);
end;

class destructor TLogger.Destroy;
begin
  Shutdown;
  //FreeAndNil(ErrorStringIDs);
end;

class procedure TLogger.Error(const Msg: string);
begin
  Log(llError, Msg);
end;

class procedure TLogger.ErrorFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llError, Msg, Values);
end;

class function TLogger.GetLogLevel: TLevel;
begin
  Result := FLevel;
end;

class procedure TLogger.Info(const Msg: string);
begin
  Log(llInfo, Msg);
end;

class procedure TLogger.InfoFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llInfo, Msg, Values);
end;

class procedure TLogger.Init(const LogFileName: string);
begin
  // do nothing if logfilename has not changed
  if (FFileH <> INVALID_HANDLE_VALUE) AND (CompareText(FLogFileName, LogFileName) = 0) then Exit;

  CloseHandle(FFileH); // close old log file

  // check whether LogFileName contains path or contains file name only
  if TPath.GetDirectoryName(LogFileName) = ''
    then FLogFileName := TPath.GetAppPath + '\' + LogFileName
    else FLogFileName := LogFileName;

  FFileH := CreateFile(PChar(FLogFileName), GENERIC_WRITE OR GENERIC_READ, FILE_SHARE_READ OR FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if FFileH = INVALID_HANDLE_VALUE then begin
    raise EFOpenError.CreateFmt('Cannot open log file (' + GetLastError.ToString + '): %s', [FLogFileName]);
  end;

  Info('------------ Logger Start ------------');
end;

class procedure TLogger.SetLogLevel(lv: TLevel);
begin
  FLevel := lv;
end;

class procedure TLogger.Shutdown();
begin
  Info('------------ Logger Shutdown ------------');
  CloseHandle(FFileH);
  FFileH := INVALID_HANDLE_VALUE;
  FLogFileName := '';
end;

class procedure TLogger.Trace(const Msg: string);
begin
  Log(llTrace, Msg);
end;

class procedure TLogger.TraceFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llTrace, Msg, Values);
end;

class procedure TLogger.Warn(const Msg: string);
begin
  Log(llWarning, Msg);
end;

class procedure TLogger.WarnFmt(const Msg: string; Values: array of const);
begin
  LogFmt(llWarning, Msg, Values);
end;

class procedure TLogger.Log(lv: TLevel; const Msg: string);
var
  MsgA: AnsiString;
  bytesWritten: DWORD;
  res1: Cardinal;
  res2: LongBool;
begin
  if lv > FLevel then Exit;
  if FFileH = INVALID_HANDLE_VALUE then Exit; // logger is in shutdown state or not initialized

  res1 := SetFilePointer(FFileH, 0, nil, FILE_END);
  if res1 = INVALID_SET_FILE_POINTER then
    raise  EInOutError.CreateFmt('Cannot position to the end of file (%s). Error code: %s', [FLogFileName, GetLastError.ToString]);

  MsgA := AnsiString(DateTimeToStr(Now) + ' ' + Msg) + sLineBreak; // use AnsiString here to be able to easily view log file in any file viewer.
  res2 := WriteFile(FFileH, PAnsiChar(MsgA)^, DWORD(Length(MsgA)), bytesWritten, nil);
  if NOT res2 then
    raise EInOutError.CreateFmt('Write to log file error (%s). Error code: %s', [FLogFileName, GetLastError.ToString]);
end;

class procedure TLogger.LogFmt(lv: TLevel; const Msg: string; Values: array of const);
begin
  if lv > FLevel then Exit;
  if FFileH = INVALID_HANDLE_VALUE then Exit; // logger is in shutdown state or error opening file

  Log(lv, Format(Msg, Values));
end;

end.
