unit Functions;

interface

uses
  SysUtils, Winapi.Windows, WinAPI.Messages, Classes, DynamicArray, FileCache;

type
  SplitRec = record
    str: string;
    flag: Boolean;
  end;

   // thread for background filling IconIndex, FileType string, and DisplayName fields in TCacheItem(s)
   // for each item we need make a call to ShGetFileInfo API function. It takes too much time and slows down search process.
   // it is possible to make searches during this bg process, but they will work a bit slowly because ShGetFileInfo will be called for search result item
const
  WM_FileShellInfo_MSG = WM_APP + 1;
  WM_SearchResultsShellInfo_MSG = WM_APP + 2;

type
  TFileShellInfoThread = class(TThread)
  private
    FBegin: Cardinal;
    FFinish: Cardinal;
    FCancelFlag: PBoolean;
    FWinHandle: THandle;
  protected
    procedure Execute; override;
  public
    procedure Start(WinHandle: THandle; CancelFlag: PBoolean; lvStart: Cardinal = 0; lvEnd: Cardinal = 0); overload;
    class procedure RunGetShellInfoBgThread(WinHandle: THandle; CancelFlag: PBoolean);
  end;

  function  MillisecToStr(ms: Cardinal): string;
  function  GetLocalTime(ftm: TFileTime): string;
  function  StringListToArray(Strings: TStrings): TArray<string>;
  procedure ArrayToStringList(Arr: TArray<string>; Strings: TStrings);
  // split string to array of strings using Delim as delimiter
  procedure StringToArray(const str: string; var arr:THArrayG<string>; const Delim:Char {= '\n'});
  // splits string to array of strings using Delim as delimiter
  procedure StringToArrayAccum(const str:string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
  function  GetErrorMessageText(lastError: Cardinal; const errorPlace: string): string;
  function  FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  function  DateTimeToFileTime(FileTime: TDateTime): TFileTime;
  function  ThousandSep(Num: UInt64): string;
  // the same as Pos() but does case INsensitive search
  function  XPos(const cSubStr, cString: string; Offset: Integer = 1): Integer;
  procedure SplitByString(InputString: string; DelimString: string; var arr: THArrayG<SplitRec>);
  function  AttrStr(Attr: Integer): string;
  function  AttrStr2(Attr: Integer): string;
  function  GetLogicalDrives: TArray<string>;
  function  IsDriveRemovable(drive: string): Boolean;
  function  GetFileShellInfo(FullFileName: TFileName; Item: TCacheItem): Boolean;

  {$IFDEF FFDEBUG} procedure LogMessage(Msg: string);{$ENDIF}


implementation

uses
  WinAPI.ShellAPI, StrUtils, SyncObjs, Math;

function MillisecToStr(ms: Cardinal): string;
var
  milliseconds: Cardinal;
  seconds: Cardinal;
  minutes: Cardinal;
  hours: Cardinal;
begin
  milliseconds := ms mod 1000;
  seconds := (ms div 1000) mod 60;
  minutes := (ms div 60000) mod 60;
  hours := (ms div 3600000) mod 24;

  //char buf[100];
  if hours > 0 then
    Result := Format('%u h %u min %u sec %u ms', [hours, minutes, seconds, milliseconds])
  else if minutes > 0 then
    Result := Format('%u min %u sec %u ms', [minutes, seconds, milliseconds])
  else
    Result := Format('%u sec %u ms', [seconds, milliseconds]);
end;

procedure ArrayToStringList(Arr: TArray<string>; Strings: TStrings);
var
  i, sz: Integer;
begin
  sz := Length(Arr);
  for i := 0 to sz - 1 do Strings.Add(Arr[i]);
end;

function StringListToArray(Strings: TStrings): TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, Strings.Count);
  for i := 0 to Strings.Count - 1 do Result[i] := Strings[i];
end;

// split string to array of strings using Delim as delimiter
procedure StringToArray(const str: string; var arr:THArrayG<string>; const Delim:Char {= '\n'});
var
  i, len: Cardinal;
  s: string;
begin
  i := 1;
  len := Cardinal(Length(str));
  //SetLength(s, len);

  while i <= len do begin
    s := '';
    while i <= len do begin
      if str[i] = Delim then begin
        Inc(i);
        break;
      end;
      s := s + str[i];
      Inc(i);
    end;

    if Length(s) > 0 then arr.AddValue(s);
  end;
end;

// splits string to array of strings using Delim as delimiter
procedure StringToArrayAccum(const str:string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
var
  i, len: Cardinal;
  s:string;
begin
  i := 1;
  len := Cardinal(Length(str));
    //SetLength(s, len);

  while i <= len do begin
    if str[i] = Delim then
      if Length(s) > 0 then arr.AddValue(s);
      s := s + str[i];
      Inc(i);
  end;

  if Length(s) > 0 then arr.AddValue(s);
end;


function GetErrorMessageText(lastError: Cardinal; const errorPlace: string): string;
var
  buf: string;
begin
  SetLength(buf, 1000);

  Winapi.Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_IGNORE_INSERTS,
          nil, lastError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), PChar(buf), 1000, nil);

  Result := Format('%s failed with error code %d as follows:\n%s', [errorPlace, lastError, buf]);
  //Windows.StringCchPrintf(PChar(buf2), Length(buf2), '%s failed with error code %d as follows:\n%s', PChar(errorPlace), lastError, pChar(buf));
end;

function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
 var
   ModifiedTime: TFileTime;
   SystemTime: TSystemTime;
 begin
   Result := 0;
   if (FileTime.dwLowDateTime = 0) and (FileTime.dwHighDateTime = 0) then Exit;
   try
     FileTimeToLocalFileTime(FileTime, ModifiedTime);
     FileTimeToSystemTime(ModifiedTime, SystemTime);
     Result := SystemTimeToDateTime(SystemTime);
   except
     Result := Now;  // Something to return in case of error
  end;
 end;

 function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
 var
   LocalFileTime, Ft: TFileTime;
   SystemTime: TSystemTime;
 begin
   Result.dwLowDateTime  := 0;
   Result.dwHighDateTime := 0;
   DateTimeToSystemTime(FileTime, SystemTime);
   SystemTimeToFileTime(SystemTime, LocalFileTime);
   LocalFileTimeToFileTime(LocalFileTime, Ft);
   Result := Ft;
 end;

function ThousandSep(Num: UInt64): string;
const
  MaxChar = 30; // Probably, 26 is enough: 19 digits + 7 separators
var
  Count: Integer;
  Rem: UInt64;
  Res: array[0..MaxChar] of Char;
  WritePtr: PChar;
begin
  WritePtr := @Res[MaxChar];
  WritePtr^ := #0;
  Count := 0;
  while Num > 0 do
  begin
    DivMod(Num, 10, Num, Rem);
    Dec(WritePtr);
    WritePtr^ := Char(Byte(Rem) + Ord('0'));
    Inc(Count);
    if Count = 3 then
    begin
      Dec(WritePtr);
      WritePtr^ :=  FormatSettings.ThousandSeparator; //'.';
      Count := 0;
    end;
  end;
  if WritePtr^ = FormatSettings.ThousandSeparator {'.'} then Inc(WritePtr);
  Count := MaxChar - Integer((NativeInt(WritePtr) - NativeInt(@Res)) shr 1);

  if Count = 0 then begin
    Result := '0';
  end
  else
  begin
    SetLength(Result, Count);
    Move(WritePtr^, PByte(Result)^, Count * SizeOf(Char));
  end;
end;

// This function retrieves the last time, the given file was written to disk
function GetLocalTime(ftm: TFileTime): string;
var
  mtm: TSystemTime;
  at: TFileTime;
  //ds, ts:ShortString;
  ds, ts: string;
const
  MAX_DATETIME_STR = 255;
begin
  SetLength(ds, MAX_DATETIME_STR);
  SetLength(ts, MAX_DATETIME_STR);
  // Time must get converted, else there is an error of one hour
  // Does anybody know what this function does ?
  // Maybe something like summertime/wintertime (or what you call it out of Germany) ?
  FileTimeToLocalFileTime(ftm, at);
  FileTimeToSystemTime(at, mtm);

  SetLength(ds, GetDateFormat(LOCALE_USER_DEFAULT, 0, @mtm, NIL, @ds[1], MAX_DATETIME_STR) - 1);
  SetLength(ts, GetTimeFormat(LOCALE_USER_DEFAULT, TIME_NOSECONDS, @mtm, NIL, @ts[1], MAX_DATETIME_STR) - 1);
  Result := ds + '  ' + ts;
end;

function XPos(const cSubStr, cString: string; Offset: Integer = 1): Integer;
var
  nLen0, nLen1, nCnt, nCnt2: Integer;
  cFirst: Char;
begin
  nLen0 := Length(cSubStr);
  nLen1 := Length(cString);

  if nLen0 > nLen1 then Result := 0 // the substr is longer than the cString
  else
  if nLen0 = 0 then Result := 0 // null substr not allowed
  else begin
    // the outer loop finds the first matching character....
    cFirst := UpCase( cSubStr[1] );
    Result := 0;

    for nCnt := Offset to nLen1 - nLen0 + 1 do begin
      if UpCase( cString[nCnt] ) = cFirst then begin
        // this might be the start of the substring...at least the first character matches....
        Result := nCnt;
        for nCnt2 := 2 to nLen0 do begin
          if UpCase( cString[nCnt + nCnt2 - 1] ) <> UpCase( cSubStr[nCnt2] ) then begin
            // failed
            Result := 0;
            break;
          end;
        end;
      end;

      if Result > 0 then break;

    end;
  end;
end;

procedure SplitByString(InputString: string; DelimString: string; var arr: THArrayG<SplitRec>);
var
  p1, p2: Integer;
  len: Integer;
  val: SplitRec;
begin
  p1 := 1;
  len := length(DelimString);
  arr.Clear;
  while True do begin
    p2 := XPos(DelimString, InputString, p1);

    if p2 = 0 then begin
      val.str := Copy(InputString, p1);
      val.flag := False;
      arr.AddValue(val);
      break;  // nothing more found
    end else begin
      val.str := Copy(InputString, p1, p2 - p1);
      val.flag := False;
      arr.AddValue(val); // adding part of string

      val.str := Copy(InputString, p2, len);
      val.flag := True;
      arr.AddValue(val);  // adding split string itself
    end;
    p1 := p2 + len;
  end;
end;

// returns a string with file attributes (DRSH)
function AttrStr(Attr: Integer): string;
begin
  Result := '';
  if (Attr AND FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result := Result + 'A';
  if (Attr AND FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result := Result + 'D';
  if (Attr AND FILE_ATTRIBUTE_READONLY)   > 0 then Result := Result + 'R';
  if (Attr AND FILE_ATTRIBUTE_SYSTEM)     > 0 then Result := Result + 'S';
  if (Attr AND FILE_ATTRIBUTE_HIDDEN)     > 0 then Result := Result + 'H';
  if (Attr AND FILE_ATTRIBUTE_COMPRESSED) > 0 then Result := Result + 'C';
  if (Attr AND FILE_ATTRIBUTE_TEMPORARY)  > 0 then Result := Result + 'T';
  if (Attr AND FILE_ATTRIBUTE_ENCRYPTED)  > 0 then Result := Result + 'E';
end;


// returns a string with file attributes in fixed format
function AttrStr2(Attr: Integer): string;
begin
  Result := '--------';
  if (Attr AND FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result[2] := 'A';
  if (Attr AND FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result[1] := 'D';
  if (Attr AND FILE_ATTRIBUTE_READONLY)   > 0 then Result[3] := 'R';
  if (Attr AND FILE_ATTRIBUTE_SYSTEM)     > 0 then Result[4] := 'S';
  if (Attr AND FILE_ATTRIBUTE_HIDDEN)     > 0 then Result[5] := 'H';
  if (Attr AND FILE_ATTRIBUTE_COMPRESSED) > 0 then Result[6] := 'C';
  if (Attr AND FILE_ATTRIBUTE_TEMPORARY)  > 0 then Result[7] := 'T';
  if (Attr AND FILE_ATTRIBUTE_ENCRYPTED)  > 0 then Result[8] := 'E';
end;

function ZStrArrayToDelphiArray(ZStrings: PChar): TArray<string>;
begin
  SetLength(Result, 0);
  while (True) do begin
    if ZStrings[0] = #0 then break;
    Insert(ZStrings, Result, Length(Result));
    ZStrings := ZStrings + Strlen(ZStrings) + 1;
  end;
end;

function GetLogicalDrives: TArray<string>;
var
  Names: string;
  Len: Cardinal;
begin
  Len := MAX_PATH * 3; // allocate enough storage for list of drives
  SetLength(Names, Len);
  Len := GetLogicalDriveStrings(Len, PChar(Names));

  Result := ZStrArrayToDelphiArray(PChar(Names));
end;

function  IsDriveRemovable(drive: string): Boolean;
var
  dt: Cardinal;
begin
  dt := GetDriveType(PChar(drive));
  Result := (dt = DRIVE_REMOVABLE) OR (dt = DRIVE_CDROM);
end;

// FileName - must be full path to existing file or folder
// or relative path to existing file/folder

{$WRITEABLECONST ON}   // needed for CallsCount static variable
function GetFileShellInfo(FullFileName: TFileName; Item: TCacheItem): Boolean;
const
  CallsCount: Cardinal = 0;
var
  ShFileInfo: TShFileInfo;
begin
  Inc(CallsCount);

  ZeroMemory(@ShFileInfo, SizeOf(ShFileInfo));

  // Get Windows file name, system file type string and icon index
  var Res := ShGetFileInfo(PChar(FullFileName), 0 {Item.FFileData.dwFileAttributes}, ShFileInfo, SizeOf(ShFileInfo),
  {SHGFI_USEFILEATTRIBUTES OR} SHGFI_TYPENAME OR SHGFI_DISPLAYNAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON { OR SHGFI_ICON } );

  if Res = 0 then begin // looks like file not found or some other error occurred
    Item.FDisplayName := ExtractFileName(FullFileName);
    Item.FIconIndex := 0; // ShFileInfo.IIcon;
    Item.FDenied := True; // mark such items with red icon too

    var err := GetLastError();
    if (err = ERROR_FILE_NOT_FOUND) OR (err = ERROR_PATH_NOT_FOUND)  then begin // file not found error
      Item.FFileType := 'File not found (probably deleted)';
    end else begin
      Item.FFileType := 'Unknown file type';
    end;
    LogMessage(Format('Error %d returned by ShGetFileInfo("%s")', [err, FullFileName]));
  end
  else
  begin
    //Item.FDenied := False;
    Item.FDisplayName := IfThen(ShFileInfo.szDisplayName[0] = #0, ExtractFileName(FullFileName), ShFileInfo.szDisplayName); // Set the item caption
    Item.FIconIndex := ShFileInfo.IIcon;  // Set file icon index from system image list
    Item.FFileType := ShFileInfo.szTypeName;
  end;

  Result := Res <> 0;
end;
{$WRITEABLECONST OFF}

{ TFileShellInfoThread }

procedure TFileShellInfoThread.Execute;
var
  i, j: Cardinal;
  levels, lvlCount: Cardinal;
  start: Cardinal;
  tmpI: TCacheItem;
begin
  start := GetTickCount;

  for i := FBegin to FFinish do begin
    lvlCount := TFSC.Instance.LevelCount(i);
    for j := 1 to lvlCount do begin
      if ((j mod 100) = 0) AND FCancelFlag^ then Exit;  // check for cancel every 100th item

      var item := TFSC.Instance.GetItem(i, j - 1);
      if item.FFileType = '' then begin
        TmpI := TCacheItem.Create;
        TmpI.Assign(item);
        GetFileShellInfo(TFSC.Instance.MakePathString(i, j - 1), TmpI);
        PostMessage(FwinHandle, WM_FileShellInfo_MSG, WPARAM(TmpI), LPARAM(j - 1));
      end;
    end;
  end;

  LogMessage('[TFileShellInfoThread]['+ IntToStr(ThreadID) +'] FINISHED. Time spent: '+ MillisecToStr(GetTickCount - start));
end;

procedure TFileShellInfoThread.Start(WinHandle: THandle; CancelFlag: PBoolean; lvStart: Cardinal = 0; lvEnd: Cardinal = 0);
begin
  FBegin := lvStart;
  FFinish := lvEnd;
  FCancelFlag := CancelFlag;
  FWinHandle := WinHandle;

  Start();
end;

class procedure TFileShellInfoThread.RunGetShellInfoBgThread(WinHandle: THandle; CancelFlag: PBoolean);
begin
  LogMessage('[TFileShellInfoThread] STARTING');
  var FileShellInfoThread := TFileShellInfoThread.Create(True);
  FileShellInfoThread.FreeOnTerminate := True;
  FileShellInfoThread.Start(WinHandle, CancelFlag, 0, TFSC.Instance.Levels - 1);
end;


{$IFDEF FFDEBUG}
const LogFileName = 'FileFind_debug.log';
var FileH: THandle = 0;

procedure LogMessage(Msg: string);
var
  MsgA: AnsiString;
  bytesWritten: DWORD;
begin
  if FileH = 0
    then FileH := CreateFile(PChar(LogFileName), GENERIC_WRITE OR GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  if FileH = INVALID_HANDLE_VALUE then raise Exception.CreateFmt('Cannot open/create file : %s', [LogFileName]);


   SetFilePointer(FileH, 0, nil, FILE_END);
   MsgA := AnsiString(DateTimeToStr(Now) + ' ' + Msg) + sLineBreak; // use AnsiString here to be able to easily view log file in any file viewer.
   WriteFile(FileH, PAnsiChar(MsgA)^, DWORD(Length(MsgA)), bytesWritten, nil);
end;

initialization
   FileH := 0;
finalization
   CloseHandle(FileH);
{$ENDIF}

end.
