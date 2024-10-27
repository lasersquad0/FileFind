unit Functions;

interface

uses
  Winapi.Windows, DynamicArray;


  function MillisecToStr(ms: Cardinal): string;
  function GetLocalTime(ftm: TFileTime): string;
  // split string to array of strings using Delim as delimiter
  procedure StringToArray(const str: string; var arr:THArrayG<string>; const Delim:Char {= '\n'});
  // splits string to array of strings using Delim as delimiter
  procedure StringToArrayAccum(const str:string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
  function GetErrorMessageText(lastError: Cardinal; const errorPlace: string): string;
  function FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
  function ThousandSep(Num: UInt64): string;

implementation

uses
  SysUtils, Math;

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
  Count := MaxChar - ((NativeInt(WritePtr) - NativeInt(@Res)) shr 1);

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

end.
