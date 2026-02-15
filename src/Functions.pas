unit Functions;

interface

uses
  SysUtils, Winapi.Windows, WinAPI.Messages, Classes, DynamicArray, Hash, CacheItem;

type
  SplitRec = record
    str: string;
    flag: Boolean;
  end;

  TTernary = class
    class function IfThen<T>(Cond: Boolean; ValueTrue, ValueFalse: T): T;
  end;

  TLogger = class
  private
  type
    TErrorListHash = THash<Cardinal, string>;
  class var
    FLogFileName: string;
    FFileH: THandle;
  public
    //class var ErrorStringIDs: TErrorListHash;
    class constructor Create;
    class destructor Destroy;
    class procedure Init(const LogFileName: string);
    class procedure Shutdown();
    class procedure Log(const Msg: string);
    class procedure LogFmt(const Msg: string; Values: array of const);
  end;

  EBadFileFormat = class(Exception)

  end;


   // thread for background filling IconIndex, FileType string, and DisplayName fields in TCacheItem(s)
   // for each item we need make a call to ShGetFileInfo API function. It takes too much time and slows down search process.
   // it is possible to make searches during this bg process, but they will work a bit slowly because ShGetFileInfo will be called for search result item
const
  //WM_FILESHELLINFO_MSG = WM_APP + 1;
  WM_SEARCHRESULTSSHELLINFO_MSG = WM_APP + 2;
  WM_RESTORE_MAINFORM_MSG = WM_APP + 3;

  // split string to array of strings using Delim as delimiter
  procedure StringToArray(const str: string; var arr: THArrayG<string>; const Delim: Char {= '\n'});

  // splits string to array of strings using Delim as delimiter
  procedure StringToArrayAccum(const str: string; var arr: THArrayG<string>; const Delim: Char {= '\n'});

  procedure SplitByStrings(InputStr: string; DelimStrList: THArrayG<string>; var output: THArrayG<SplitRec>);

    // the same as Pos() but does case INsensitive search
  function  XPos(const SubStr, Str: string; Offset: Integer = 1): Integer;

  function  MillisecToStr(ms: Cardinal): string;

  function  FileTimeToString(const FileTime: TFileTime): string;
  function  StringListToArray(Strings: TStrings): TArray<string>;
  procedure ArrayToStringList(Arr: TArray<string>; Strings: TStrings);
  procedure WriteStringToStream(OStream: TStream; str: string);
  function  ReadStringFromStream(IStream: TStream): string;
  function  FileTimeToDateTime(FileTime: TFileTime): TDateTime;
  function  DateTimeToFileTime(FileTime: TDateTime): TFileTime;
  function  ThousandSep(Num: UInt64): string;

  function  AttrStr2(Attr: DWORD): string;
  function  GetLogicalDrives: TArray<string>;
  function  IsDriveRemovable(drive: string): Boolean;
  function  GetFileShellInfo(FullFileName: TFileName; Item: TCacheItem): Boolean;

  function IsAppRunningAsAdminMode(): Boolean;
  function CheckTokenMembership(TokenHandle: THandle; SidToCheck: PSID; IsMember: PLongBool): LongBool; stdcall;
  function GetFileOwnerName(FilePath: string): string;

  // function  GetErrorMessageText(lastError: Cardinal; const errorPlace: string): string;
  // function  GetLocalTime(ftm: TFileTime): string;
  // function  AttrStr(Attr: DWORD): string;

implementation

uses
  WinAPI.ShellAPI, WinAPI.AclAPI, WinAPI.AccCtrl, StrUtils, IOUtils, SyncObjs, Math;

// need for checking if app running with admin rights
function CheckTokenMembership; external 'advapi32.dll' name 'CheckTokenMembership';

class function TTernary.IfThen<T>(Cond: Boolean; ValueTrue, ValueFalse: T): T;
begin
  if Cond
    then Result := ValueTrue
    else Result := ValueFalse;
end;

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
// does not add empty strings to output array
var gStringb: TStringBuilder;
{procedure StringToArray(const str: string; var arr: THArrayG<string>; const Delim: Char {= '\n'}{);
var
  i, len: Cardinal;
begin
  i := 1;
  len := Cardinal(Length(str));

  while i <= len do begin
    gStringb.Clear;  // Clear sets Capacity to 16 (default capacity)
    while i <= len do begin
      if str[i] = Delim then begin
        Inc(i);
        break;
      end;
      gStringb.Append(str[i]);
      Inc(i);
    end;

    if gStringb.Length > 0 then arr.AddValue(gStringb.ToString);
  end;
end;
 }
procedure StringToArray(const str: string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
var
  i, len: Cardinal;
begin
  i := 1;
  len := Cardinal(Length(str));

    gStringb.Clear;  // Clear sets Capacity to 16 (default capacity)
    while i <= len do begin
      if str[i] = Delim then begin
        if gStringb.Length > 0 then arr.AddValue(gStringb.ToString);
        gStringb.Clear;
      end else
        gStringb.Append(str[i]);
      Inc(i);
    end;

    if gStringb.Length > 0 then arr.AddValue(gStringb.ToString);
end;

// splits string to array of strings using Delim as delimiter
// multiple consecutive Delims are treated as a single Delim.
// Delims (one or several) in the end of string are ignored (do not generate items in arr)
procedure StringToArrayAccum(const str: string; var arr: THArrayG<string>; const Delim: Char {= '\n'});
var
  i, len: Cardinal;
  DelimFlag: Boolean;
begin
  i := 1;
  len := Cardinal(Length(str));

  gStringb.Clear;
  DelimFlag := True;
  while i <= len do begin
    if str[i] = Delim then begin
      if {Length(s) > 0} NOT DelimFlag then begin
        arr.AddValue(gStringb.ToString);
        gStringb.Append(str[i]);
      end;
      DelimFlag := True;
    end else begin
      gStringb.Append(str[i]);
      DelimFlag := False;
    end;
    Inc(i);
  end;

  if {Length(s) > 0} NOT DelimFlag then arr.AddValue(gStringb.ToString);
end;


// equivalent of Pos() function but does case INSENSITIVE search of substring in a given string
// return 0 if substring is not found
// othewise returns index of beginning SubStr in Str
function XPos(const SubStr, Str: string; Offset: Integer = 1): Integer;
var
  nLen0, nLen1, nCnt, nCnt2: Integer;
  cFirst: Char;
begin
  nLen0 := Length(SubStr);
  nLen1 := Length(Str);

  if nLen0 > nLen1 then Result := 0 // the substr is longer than the cString
  else
  if nLen0 = 0 then Result := 0 // null substr not allowed
  else begin
    // the outer loop finds the first matching character....
    cFirst := UpCase(SubStr[1]);
    Result := 0;

    for nCnt := Offset to nLen1 - nLen0 + 1 do begin
      if UpCase(Str[nCnt]) = cFirst then begin
        // this might be the start of the substring...at least the first character matches....
        Result := nCnt;
        for nCnt2 := 2 to nLen0 do begin
          if UpCase(Str[nCnt + nCnt2 - 1]) <> UpCase(SubStr[nCnt2]) then begin
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

procedure SplitByStrings(InputStr: string; DelimStrList: THArrayG<string>; var output: THArrayG<SplitRec>);
var
  p1, p2: Integer;
  i, len: Integer;
  val: SplitRec;
begin
  output.Clear;

  if DelimStrList.Count = 0 then Exit;

  p1 := 1;
  while True do begin
    for i := 1 to DelimStrList.Count do begin
      len := Length(DelimStrList[i - 1]);

      p2 := XPos(DelimStrList[i - 1], InputStr, p1);

      if p2 = 0 then begin
        val.str := Copy(InputStr, p1);
        val.flag := False;
        output.AddValue(val);
        Exit; //break; // nothing more found
      end else begin
        val.str := Copy(InputStr, p1, p2 - p1);
        val.flag := False;
        output.AddValue(val); // adding part of string

        val.str := Copy(InputStr, p2, len);
        val.flag := True;
        output.AddValue(val); // adding split string itself
      end;
      p1 := p2 + len;
    end;
  end;
end;

procedure WriteStringToStream(OStream: TStream; str: string);
var
  lenBytes: Integer;
begin
  lenBytes := ByteLength(str);
  Assert(lenBytes < MAX_PATH * sizeof(str[1]));
  OStream.WriteData<Integer>(lenBytes);
  if lenBytes > 0 then OStream.Write(str[1], lenBytes);
end;

function ReadStringFromStream(IStream: TStream): string;
var
  lenBytes: Cardinal;
begin
  IStream.ReadData<Cardinal>(lenBytes);
  Assert(lenBytes < MAX_PATH * sizeof(Result[1]));
  if lenBytes > 0 then begin
    SetLength(Result, lenBytes div sizeof(Result[1]));
    IStream.Read(Result[1], lenBytes);
  end;
end;

// **** Use built-in SysUtils.SysErrorMessage function  *****
{function GetErrorMessageText(LastError: Cardinal; const ErrorPlace: string): string;
var
  buf: string;
begin
  SetLength(buf, 1000);

  Winapi.Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM OR FORMAT_MESSAGE_IGNORE_INSERTS,
          nil, lastError, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), PChar(buf), 1000, nil);

  Result := Format('%s failed with error code %d as follows:\n%s', [ErrorPlace, LastError, buf]);
  //Windows.StringCchPrintf(PChar(buf2), Length(buf2), '%s failed with error code %d as follows:\n%s', PChar(errorPlace), lastError, pChar(buf));
end;
 }

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

// This function retrieves the last time as string, the given file was written to disk
{function GetLocalTime(ftm: TFileTime): string;
const
  MAX_DATETIME_STR = 255;
var
  mtm: TSystemTime;
  at: TFileTime;
  ds, ts: string;
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
 }

function FileTimeToString(const FileTime: TFileTime): string;
var
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
  DateTime: TDateTime;
begin
  // Convert to local file time
  if FileTimeToLocalFileTime(FileTime, LocalFileTime) then begin
    if FileTimeToSystemTime(LocalFileTime, SystemTime) then begin
      DateTime := SystemTimeToDateTime(SystemTime);
      Result := DateTimeToStr(DateTime);
    end
      else Result := 'Invalid system time';
  end
    else Result := 'Invalid file time';
end;


// returns a string with file attributes (ADRSHCTE)
{function AttrStr(Attr: DWORD): string;
begin
  Result := '';
  if (Attr AND FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result := Result + 'D';
  if (Attr AND FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result := Result + 'A';
  if (Attr AND FILE_ATTRIBUTE_READONLY)   > 0 then Result := Result + 'R';
  if (Attr AND FILE_ATTRIBUTE_SYSTEM)     > 0 then Result := Result + 'S';
  if (Attr AND FILE_ATTRIBUTE_HIDDEN)     > 0 then Result := Result + 'H';
  if (Attr AND FILE_ATTRIBUTE_COMPRESSED) > 0 then Result := Result + 'C';
  if (Attr AND FILE_ATTRIBUTE_TEMPORARY)  > 0 then Result := Result + 'T';
  if (Attr AND FILE_ATTRIBUTE_ENCRYPTED)  > 0 then Result := Result + 'E';
end;
 }

// returns a string with file attributes in fixed format (e.g. D-R-H---)
function AttrStr2(Attr: DWORD): string;
begin
  Result := '--------';
  if (Attr AND FILE_ATTRIBUTE_DIRECTORY)  > 0 then Result[1] := 'D';
  if (Attr AND FILE_ATTRIBUTE_ARCHIVE)    > 0 then Result[2] := 'A';
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
  Len, Res: Cardinal;
begin
  Len := MAX_PATH * 3; // allocate enough storage for list of drives
  SetLength(Names, Len);
  Res := GetLogicalDriveStrings(Len, PChar(Names));
  if Res = 0 then TLogger.Log('GetLogicalDriveStrings returned error: ' + GetLastError.ToString);

  Result := ZStrArrayToDelphiArray(PChar(Names));
end;

function  IsDriveRemovable(drive: string): Boolean;
var
  dt: Cardinal;
begin
  dt := GetDriveType(PChar(drive));
  Result := (dt = DRIVE_REMOVABLE) OR (dt = DRIVE_CDROM);
end;

{$IFOPT J+}
  {$DEFINE WAS_WRITABLECONST_ON}
{$ENDIF}
{$WRITEABLECONST ON}   // needed for CallsCount static variable
// FileName - must be full path to existing file or folder (or relative path to existing file/folder)
function GetFileShellInfo(FullFileName: TFileName; Item: TCacheItem): Boolean;
const
  CallsCount: Cardinal = 0;
var
  ShFileInfo: TShFileInfo;
  //errStr: string;
begin
  Inc(CallsCount);

  ZeroMemory(@ShFileInfo, SizeOf(ShFileInfo));

  // get file display name, system file type string and icon index
  var Res := ShGetFileInfo(PChar(FullFileName), 0 {Item.FFileData.dwFileAttributes}, ShFileInfo, SizeOf(ShFileInfo),
  {SHGFI_USEFILEATTRIBUTES OR} SHGFI_TYPENAME OR SHGFI_DISPLAYNAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON { OR SHGFI_ICON } );

  if Res = 0 then begin // looks like file not found or some other error occurred
    Item.FDisplayName := ExtractFileName(FullFileName);
    Item.FIconIndex := 0;
    Item.FDenied := True; // mark such items with red icon too

    var err := GetLastError();
    if (err = ERROR_FILE_NOT_FOUND) OR (err = ERROR_PATH_NOT_FOUND) then begin // file not found error
      Item.FFileType := 'File not found (probably deleted)';
    end else begin
      Item.FFileType := 'Unknown file type';
    end;

    TLogger.Log(Format('Error (code=%d) "%s" in WINAPI call ShGetFileInfo(%s).', [err, SysErrorMessage(err), FullFileName]));
  end
  else
  begin
    if ShFileInfo.szDisplayName[0] = #0
      then Item.FDisplayName := ExtractFileName(FullFileName)
      else Item.FDisplayName := ShFileInfo.szDisplayName;
    Item.FIconIndex := ShFileInfo.IIcon;  // Set file icon index from system image list
    Item.FFileType := ShFileInfo.szTypeName;
  end;

  Result := Res <> 0;
end;
{$IFDEF WAS_WRITABLECONST_ON}
  {$WRITEABLECONST ON}
  {$UNDEF WAS_WRITABLECONST_ON}
{$ELSE}
  {$WRITEABLECONST OFF}
{$ENDIF}


function IsAppRunningAsAdminMode(): Boolean;
type
  TArr = array[0..5] of Byte;
const
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  //SECURITY_NT_AUTHORITY: array[0..5] of Byte = (0,0,0,0,0,5);
var
  fIsRunAsAdmin: LongBool;
  dwError: DWORD;
  pAdministratorsGroup: PSID;
  NtAuthority: SID_IDENTIFIER_AUTHORITY;

label Cleanup;
begin
  fIsRunAsAdmin := FALSE;
  dwError := ERROR_SUCCESS;
  pAdministratorsGroup := nil;

  // Allocate and initialize a SID of the administrators group.
  NtAuthority.Value[0] := 0; //SECURITY_NT_AUTHORITY;
  NtAuthority.Value[1] := 0;
  NtAuthority.Value[2] := 0;
  NtAuthority.Value[3] := 0;
  NtAuthority.Value[4] := 0;
  NtAuthority.Value[5] := 5;

  if NOT AllocateAndInitializeSid(
        &NtAuthority,
        2,
        SECURITY_BUILTIN_DOMAIN_RID,
        DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0,
        &pAdministratorsGroup)
  then begin
    dwError := GetLastError();
    goto Cleanup;
  end;

  // Determine whether the SID of administrators group is enabled in
  // the primary access token of the process.
  if NOT CheckTokenMembership(0, pAdministratorsGroup, @fIsRunAsAdmin) then begin
    dwError := GetLastError();
    goto Cleanup;
  end;

Cleanup:
    // Centralized cleanup for all allocated resources.
  if Assigned(pAdministratorsGroup) then begin
    FreeSid(pAdministratorsGroup);
    pAdministratorsGroup := nil;
  end;

    // Throw the error if something failed in the function.
  if ERROR_SUCCESS <> dwError then raise Exception.Create('dwError:' + dwError.ToString);

  Result := fIsRunAsAdmin;
end;

function SidToString(Sid: PSID): string;
var
  StringSid: PChar;
begin
  StringSid := nil;

  // 1. Basic validation: Check if the SID pointer is valid
 // if not IsValidSid(Sid) then Exit;

  // 2. Perform the conversion
  if ConvertSidToStringSid(Sid, StringSid) then begin
    Result := StringSid;
  end else begin
    TLogger.Log('[SidToString] WINAPI call ConvertSidToStringSid failed.');
  end;

  // 3. Important: Free the memory allocated by ConvertSidToStringSid
  LocalFree(HLOCAL(StringSid));

end;

function GetFileOwnerName(FilePath: string): string;
var
   pSidOwner: PSID;
   pSD: PSECURITY_DESCRIPTOR;
   dwResult: Cardinal;
   szOwnerName: array[0..MAX_PATH] of Char;
   szDomainName: array[0..MAX_PATH] of Char;
   dwOwnerNameSize:Cardinal;
   dwDomainNameSize: Cardinal;
   sidType: SID_NAME_USE;
   bOwnerDefaulted: BOOL;
 begin
   pSidOwner := nil;
   pSD := nil;
   szOwnerName[0] := #0;
   szDomainName[0] := #0;
   dwOwnerNameSize := MAX_PATH;
   dwDomainNameSize := MAX_PATH;

   // Get security descriptor with owner information
   dwResult := GetNamedSecurityInfo(PChar(FilePath), // File path
     SE_FILE_OBJECT, // Object type (file)
     OWNER_SECURITY_INFORMATION, // Request owner info only
     @pSidOwner, // Receives owner SID
     nil, nil, nil, @pSD);

   if dwResult <> ERROR_SUCCESS then begin
     TLogger.Log(Format('Error (code=%d) "%s" in WINAPI call GetNamedSecurityInfo(%s).', [dwResult, SysErrorMessage(dwResult), FilePath]));
     LocalFree(pSD);
     Exit;
   end;

   { if pSidOwner = nil then begin
     bOwnerDefaulted := FALSE;
     if NOT GetSecurityDescriptorOwner(pSD, pSidOwner, &bOwnerDefaulted)
     then TLogger.LogFmt('Failed to get owner from SD: %d', [GetLastError()])
     else if pSidOwner = nil then TLogger.Log('SD finally has no owner');
     end;
   }

   // Convert SID to account name
   if NOT LookupAccountSid(nil, // Local system
     pSidOwner, // SID to lookup
     szOwnerName, // Account name buffer
     dwOwnerNameSize, // Size of name buffer
     szDomainName, // Domain name buffer
     dwDomainNameSize, // Size of domain buffer
     sidType) // SID type
   then begin
     var err := GetLastError();
     TLogger.Log(Format('Error (code=%d) "%s" in WINAPI call LookAccountSid(%s).', [err, SysErrorMessage(err), FilePath]));
     Result := SidToString(pSidOwner);
     LocalFree(pSD);
     Exit;
   end;

   LocalFree(pSD);

   Result := szOwnerName;
 end;



//const DEF_LOG_FILENAME = 'FinderX_denug.log';
const
  INVALID_SET_FILE_POINTER = -1;

class constructor TLogger.Create;
begin
  FFileH := INVALID_HANDLE_VALUE;

 { ErrorStringIDs := TErrorListHash.Create;
  ErrorStringIDs[ERROR_FILE_NOT_FOUND] := 'ERROR_FILE_NOT_FOUND'; //3
  ErrorStringIDs[ERROR_PATH_NOT_FOUND] := 'ERROR_PATH_NOT_FOUND'; //5
  ErrorStringIDs[ERROR_MORE_DATA]      := 'ERROR_MORE_DATA'; //234
  ErrorStringIDs[ERROR_NO_TOKEN]       := 'ERROR_NO_TOKEN'; //1008
  ErrorStringIDs[ERROR_NO_MORE_ITEMS]  := 'ERROR_NO_MORE_ITEMS'; //259 }
end;

class destructor TLogger.Destroy;
begin
  Shutdown;
  //FreeAndNil(ErrorStringIDs);
end;

class procedure TLogger.Init(const LogFileName: string);
var tdir: TDirectory;
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

  Log('------------ Logger Start ------------');
end;

class procedure TLogger.Shutdown();
begin
  Log('------------ Logger Shutdown ------------');
  CloseHandle(FFileH); // close old log file
  FFileH := INVALID_HANDLE_VALUE;
  FLogFileName := '';
end;

class procedure TLogger.Log(const Msg: string);
var
  MsgA: AnsiString;
  bytesWritten: DWORD;
  res1: Cardinal;
  res2: LongBool;
begin
  if FFileH = INVALID_HANDLE_VALUE then Exit; // logger is in shutdown state or not initialized

  res1 := SetFilePointer(FFileH, 0, nil, FILE_END);
  if res1 = INVALID_SET_FILE_POINTER then
    raise  EInOutError.CreateFmt('Cannot position to the end of file (%s). Error code: %s', [FLogFileName, GetLastError.ToString]);

  MsgA := AnsiString(DateTimeToStr(Now) + ' ' + Msg) + sLineBreak; // use AnsiString here to be able to easily view log file in any file viewer.
  res2 := WriteFile(FFileH, PAnsiChar(MsgA)^, DWORD(Length(MsgA)), bytesWritten, nil);
  if NOT res2 then
    raise EInOutError.CreateFmt('Write to log file error (%s). Error code: %s', [FLogFileName, GetLastError.ToString]);
end;

class procedure TLogger.LogFmt(const Msg: string; Values: array of const);
begin
  if FFileH = INVALID_HANDLE_VALUE then Exit; // logger is in shutdown state or error opening file

  Log(Format(Msg, Values));
end;

initialization
  gStringb := TStringBuilder.Create;
finalization
  FreeAndNil(gStringb);

end.
