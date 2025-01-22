unit Functions;

interface

uses
  SysUtils, Winapi.Windows, DynamicArray, FileCache;

type
  SplitRec = record
    str: string;
    flag: Boolean;
  end;

  function  MillisecToStr(ms: Cardinal): string;
  function  GetLocalTime(ftm: TFileTime): string;
  // split string to array of strings using Delim as delimiter
  procedure StringToArray(const str: string; var arr:THArrayG<string>; const Delim:Char {= '\n'});
  // splits string to array of strings using Delim as delimiter
  procedure StringToArrayAccum(const str:string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
  function  GetErrorMessageText(lastError: Cardinal; const errorPlace: string): string;
  function  FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  function  DateTimeToFileTime(FileTime: TDateTime): TFileTime;
  function  ThousandSep(Num: UInt64): string;
  // the same as Pos() but does case INsensitive search
  function XPos(const cSubStr, cString: string; Offset: Integer = 1): Integer;
  procedure SplitByString(InputString: string; DelimString: string; var arr: THArrayG<SplitRec>);
  function GetFileShellInfo(FileName: TFileName; Item: TCacheItem): Boolean;

  {$IFDEF FFDEBUG} procedure LogMessage(Msg: string);{$ENDIF}

implementation

uses
  WinAPI.ShellAPI, Math;

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

function GetFileShellInfo(FileName: TFileName; Item: TCacheItem): Boolean;
var
  ShFileInfo: TShFileInfo;
begin
  ZeroMemory(@ShFileInfo, SizeOf(ShFileInfo));
  var Res := ShGetFileInfo(PChar(FileName), Item.FFileData.dwFileAttributes, ShFileInfo, SizeOf(ShFileInfo), // Get Windows file name, system file type and icon
  SHGFI_USEFILEATTRIBUTES OR SHGFI_TYPENAME OR SHGFI_DISPLAYNAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON { OR SHGFI_ICON } );

  if Res = 0 then begin // looks like file not found or some other error occurred
    Item.FDisplayName := ExtractFileName(FileName);
    Item.FIconIndex := 0; //ShFileInfo.IIcon;
    Item.FFileType := 'Unknown file type';
  end
  else
  begin
    Item.FDisplayName := ShFileInfo.szDisplayName; // Set the item caption
    Item.FIconIndex := ShFileInfo.IIcon;      // Set file icon index
    Item.FFileType := ShFileInfo.szTypeName;
  end;

  //if Item.FDenied then Item.FIconIndex := 0;

  Result := Res <> 0;
end;

{$IFDEF FFDEBUG}
const LogFileName = 'FileFind_debug.log';
var FileH: THandle = 0;

procedure LogMessage(Msg: string);
//var
  //AlreadyExists: Boolean;

  {procedure WriteTo(const Msg: AnsiString);
  var tmpI: DWORD;
  begin
    if FileH <> INVALID_HANDLE_VALUE then
      //try
        WriteFile(FileH, PAnsiChar(Msg)^, DWORD(Length(Msg)), tmpI, nil);
      //except
      //  on E:Exception do
      //    MessageDlg(Format('Error writing to log file : %s', [E.Message]), mtError, [mbOk], 0);
      //end;
  end; }
var
  MsgA: AnsiString;
  bytesWritten: DWORD;
begin
  if FileH = 0
    then FileH := CreateFile(PChar(LogFileName), GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

 // AlreadyExists := GetLastError = ERROR_ALREADY_EXISTS;

  if FileH = INVALID_HANDLE_VALUE then raise Exception.CreateFmt('Cannot open/create file : %s', [LogFileName]);

   SetFilePointer(FileH, 0, nil, FILE_END);
   MsgA := AnsiString(Msg) + sLineBreak;
   WriteFile(FileH, PAnsiChar(MsgA)^, DWORD(Length(MsgA)), bytesWritten, nil);
  // WriteTo(Msg + sLineBreak {#13#10});
end;

initialization
   FileH := 0;
finalization
   CloseHandle(FileH);
{$ENDIF}

end.
