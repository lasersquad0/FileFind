Unit FileCache;

interface

uses System.Classes, System.SysUtils, Windows, DynamicArray, DynamicArrays, Hash, SortedArray, CacheItem;

type

 TFileTypes = (ftFile, ftDir, ftTemp, ftArchive, ftReadOnly, ftHidden, ftSystem, ftDevice, ftSymbolic, ftCompressed,
               ftEncrypted, ftOffline, ftSparse, ftPinned, ftNotIndexed, ftVirtual, ftAll);

 TFileTypeNames = array [TFileTypes] of string;
 TFileSystemStat = array [TFileTypes] of Cardinal;
 TFileSystemStatIndex = array [Ord(Low(TFileTypes)).. Ord(High(TFileTypes))] of TFileTypes;

 TFileSystemStatRecord = record
   Stat : TFileSystemStat;
   Index: TFileSystemStatIndex;
 end;


 TStatRecord = record
   Directories: Cardinal;
   Archive:   Cardinal;
   ReadOnly:  Cardinal;
   Files:     Cardinal;
   Hidden:    Cardinal;
   Temporary: Cardinal;
   Devices:   Cardinal;
   System:    Cardinal;
   Symbolic:  Cardinal;
   Compressed:Cardinal;
   Encrypted: Cardinal;
   Offline:   Cardinal;
   Sparse:    Cardinal;
 end;


 TError = record
   ErrCode: Integer; // ErrCode=0 means 'no error', all other values is signal about error
   ErrText: array [0..255] of char;
   function HasError: Boolean;
   procedure Create(code: Integer; msg: string);
   procedure SetMsg(msg: string);
   function GetMsg: string;
   property Msg: string read GetMsg write SetMsg;
 end;


 IIndexingProgress = class
   procedure Start(Notes: string; P100: Integer = -1); virtual; abstract; // defines Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; virtual; abstract;
   function Progress(Prgress: Integer): Boolean; virtual; abstract; // Result=False stops process if indexing takes too long time
   procedure ReportError(Error: TError); virtual; abstract;
 end;

  // if True is returned as a result of this function that means 'stop searching'
  TFNCSearchResult = function(FullPath: string; FileData: TCacheItem): Boolean of object;

  TSearchResult = (srOK, srWrongPath, srNoIndexData, srCancelled);
  TFileSizeCompare = (fscEquals, fscMore, fscLess);
  TSearchDateType = (sdNone, sdCreated, sdModified, sdLastAccess);


  TSearchFilter = record
    //StartFrom: string;
    SearchStr: string;
    SearchStrUpper: string; // optimization for case insensitive search
    ExactSearch: Boolean;
    CaseSensitive: Boolean;
    SearchByFileSize: Boolean;
    FileSize: UInt64;
    FileSizeCmpType: TFileSizeCompare;
    //SearchByModifiedDate: Boolean;
    SearchByDateType: TSearchDateType;
    DateFrom: TFileTime;
    DateTo: TFileTime;
    SearchByAttributes: Boolean;
    Attributes: Cardinal;
  end;


  // We need THarray here because we store TCacheItems as sequential pieces of memory
  // TCacheITem objects are created on these pieces of memory using InitInstance(pointer) calls
  // This is done to reduce memory fragmentation because we need to store many small TCacheItems
 TLevelType = THArray;

 PFileLevel = ^TFileLevel;
 PPFileLevel = ^PFileLevel;

 TVolumeCache = class
 private
   FName: string;
   FCacheData: THArrayG<TLevelType>;
   FProgressListeners: THArrayG<IIndexingProgress>;
   FIndexedDateTime: TDateTime; // datetime when Volume cache data has been created (indexed).
   FExecTime: Cardinal;
   FExclFolders: THArraySorted<string>;

   procedure Serialize(OStream: TStream);
   procedure Deserialize(IStream: TStream); overload;
   procedure Deserialize(IStream: PPFileLevel; Count: Cardinal); overload;
   procedure SaveTo(const fileName: string);
   function  CompareProcString(item1, item2: string): Integer;
   function  AddLevel(level: Cardinal): TLevelType;
   function  AddRootItem(var fileData: TWin32FindData):TCacheItemRef;
   function  AddItem(parent: Cardinal; var fileData:TWin32FindData; itemLevel: Cardinal; doSearch: Boolean = False): TCacheItemRef;
   function  AddFullPath(const Path: string): TCacheItemRef;
   function  GetItem(itemRef: TCacheItemRef): TCacheItem; overload;
   procedure FillFileData(const filePath: string; var fileData: TWin32FindData);
   function  ReadDirectory(const currDir: TFileName; parent: TCacheItemRef; ShowProgress: Boolean): UInt64;
   procedure NotifyStart(Notes: string);
   procedure NotifyFinish;
   function  NotifyProgress(Progrs: Integer): Boolean;
   procedure NotifyError(Error: TError);
   //procedure CloseFindHandles;
   //constructor CreatePrivate;
   //class procedure FreeInst;
   procedure StatSort(var Stat: TFileSystemStatRecord);

 public
   constructor Create;
   destructor Destroy; override;
   procedure SerializeTo(const FileName:string);
   //procedure DeserializeFrom(const FileName:string);
   procedure Clear;
   function  Size: UInt64;
   function  Count: Cardinal;
   function  GetItem(Level: Cardinal; Index: Cardinal): TCacheItem; overload;
   function  LevelCount(Level: Cardinal): Cardinal;
   function  Levels(): Cardinal;
//   function  ReadFileSystem(const Volumes: TArray<string>): UInt64;
   function  ReadVolume(Volume: string; ExclusionsList: TArray<string>): UInt64;
   function  ReadVolumeFast(Volume: string; ExclusionsList: TArray<string>): UInt64;
   function  MakePathString(ref: TCacheItemRef): string; overload;
   function  MakePathString(itemLevel, itemIndex: Cardinal): string; overload;
   function  Search(Filter: TSearchFilter; Callback: TFNCSearchResult): TSearchResult;

   procedure PrintLevelsStat(list: TStrings);
   procedure PrintAllItems(list: TStrings);
   function  GetStat: TFileSystemStatRecord;
   procedure PrintStat(stat: TFileSystemStat; list: TStrings);

   // integrity checks
   procedure CheckThatParentIsDirectory;
   function  CheckHangingDirectories: THArrayG<string>;
   procedure CheckLevelsDataTsCorrect;
   procedure CheckFileDatesAreCorrect;

   property  IndexedDateTime: TDateTime read FIndexedDateTime;
   property  VolName: string read FName;
   property  ExecTime: Cardinal read FExecTime;
 end;

  TVolumeExecData = record
    VolumeName: string;
    ExecTime: Cardinal;
    VolSize: UInt64;
    ItemsCount: Cardinal;
  end;

 TCache = class
 private
   class var GInstance: TCache; // single instance of cache
   class var GInstance2: TCache;
 private
   FVolumeData: THash<string, TVolumeCache>;
   FProgressListeners: THArrayG<IIndexingProgress>;
   FIndexFileSaveDate: TDateTime; // datetime when index file was saved, valid only after loading index file.

   procedure Serialize(OStream: TStream);
   procedure Deserialize(IStream: TStream);
//   procedure SaveTo(const fileName: string);
   constructor CreatePrivate;
   destructor Destroy; override;
   class destructor FreeInst; // this will be automatically called by Delphi to free resources

 public
   constructor Create; // raises an exception to avoid creating other instances of cache
   class procedure FreeInst2;
   class function Instance: TCache;
   class function NewInstance: TCache;
   class procedure Swap;
   class function HasNewInstance: Boolean;
   procedure SerializeTo(const FileName:string);
   procedure DeserializeFrom(const FileName:string);
   procedure Clear; overload;  // clears data of all volumes
   procedure Clear(Volume: string); overload;  // clears specified volume only
   function  VolumesCount: Cardinal;
   function  GetVolume(Volume: string): TVolumeCache;
   function  VolumePresent(Volume: string): TVolumeCache;
   function  GetVolumes: TArray<string>;
   function  GetOrCreateVolume(Volume: string): TVolumeCache;
   function  GetExecData: TArray<TVolumeExecData>;
   function  GetVolumeNamesAsString: string;
   procedure ReadVolume(Volume: string; ExclusionsList: TArray<string>);
   procedure ReadVolumeFast(Volume: string; ExclusionsList: TArray<string>);
   procedure ReadVolumesFast(Volumes: TArray<string>; ExclusionsList: TArray<string>);
   function  Search(Filter: TSearchFilter; Callback: TFNCSearchResult): TSearchResult;
   procedure AddProgressListener(listener: IIndexingProgress);
   procedure RemoveProgressListener(listener: IIndexingProgress);

   // integrity checks procedures
   function CheckHangingDirectories: THArrayG<string>;
   procedure CheckThatParentIsDirectory;
   procedure CheckLevelsDataTsCorrect;
   procedure CheckFileDatesAreCorrect;

   function  GetStat(Volume: string): TFileSystemStatRecord;

   property  IndexFileSaveDate: TDateTime read FIndexFileSaveDate;
 end;

MFT_INDEX = packed record
        low:  UInt32; // The low part of the file number.
        high: UInt16; // The high part of the file number.
        seq:  UInt16; // The sequence number of MFT record.
end; // 8 bytes

// MFT record number structure.
MFT_REF = packed record
    case Byte of
      0: (sId: MFT_INDEX);
      1: (Id: UInt64);
end; // 8 bytes

NTFS_DUP_INFO = packed record
    CreateTime:     TFileTime; //UInt64; // 0x00 File creation file.
    ModifyTime:     TFileTime; //UInt64; // 0x08 File modification time.
    ModifyAttrTime: TFileTime; //UInt64; // 0x10 Last time any attribute was modified.
    LastAccessTime: TFileTime; //UInt64; // 0x18 File last access time.
    AllocSize:      UInt64;      // 0x20 Data attribute allocated size (for unnamed $DATA attribute), multiple of cluster size.
    FileSize:       UInt64;     // 0x28 Actual data attribute size <= AllocSize.
    FileAttrib:     UInt32;     // 0x30 Standard DOS attributes & more.
    ea_size: UInt16; // 0x34 Packed EAs.
    reparse: UInt16; // 0x36 Used by Reparse.
end; // 0x38= 56 bytes

// Filename attribute structure (0x30). Resident only
ATTR_FILE_NAME = packed record
    ParentDir: MFT_REF;   // 0x00 reference to MFT record for parent directory.
    dup: NTFS_DUP_INFO;   // 0x08
    FileNameLen: UInt8; // 0x40 File name length in words for unicode.
    NameType:    UInt8; // 0x41 File name type: POSIX=0, UNICODE=1, DOS=2, BOTH=3
end; // 0x42=66 bytes

CACHE_ITEM = packed record
    FParent: UInt32;
    FLevel:  UInt32;
    FMFTRecID: MFT_REF; // MFT Id of this file
    FileAttr:ATTR_FILE_NAME; // 66 bytes. must be last field in FILELIST_ITEM because it has variable size

    function Size: UInt32;
    function Name: string;
end; // 82 bytes long

 PCACHE_ITEM = ^CACHE_ITEM;

 TFileLevel = packed record
  FCount: UInt32;
  FStart: PCACHE_ITEM;
end;


var
  FileTypeNames: TFileTypeNames = ('File', 'Directory', 'Temporary', 'Archive', 'ReadOnly', 'Hidden', 'System', 'Device',
                                 'Symbolic Link', 'Compressed', 'Encrypted', 'Offline', 'Sparse', 'Pinned', 'NotIndexed', 'Virtual', 'UNDEF');

implementation

uses
  System.Math, System.Generics.Defaults, Functions, MaskSearch, ObjectsCache, Hash2;

const MAX_LEVELS = 100;
const MAX_LEVEL_DIRS = 10_000;
//const MAX_FIND_HANDLES_CAPACITY = 30_000;

////////////////////////////////////
// Common Functions
///////////////////////////////////

function IS_DOT_DIR(dirName: PWideChar): Boolean;
begin
  //Result := (dirName = '.') OR (dirName = '..');
  Result := ( (dirName[0] = '.') AND (dirName[1] = #0) ) OR ( (dirName[0] = '.') AND (dirName[1] = '.') AND (dirName[2] = #0) );
end;

function IsDirectory(var FileData: TWin32FindData): Boolean;
begin
  IsDirectory := (FileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0;
end;

function IsReparse(var FileData: TWin32FindData): Boolean;
begin
  IsReparse := (FileData.dwFileAttributes AND FILE_ATTRIBUTE_REPARSE_POINT) > 0;
end;


////////////////////////////////////
// TVolumeCache methods
///////////////////////////////////

 function TVolumeCache.CompareProcString(item1, item2: string): Integer;
 begin
   Result := CompareText(item1, item2);
 end;

{$WRITEABLECONST ON}   // needed for ProgressCounter static variable
function TVolumeCache.ReadDirectory(const CurrDir: TFileName; Parent: TCacheItemRef; ShowProgress: Boolean): UInt64;
const
  FIND_FIRST_EX_LARGE_FETCH = $00000002;
  ProgressCounter: Integer = 0;  // this works as static variable inside a procedure
var
  DirSize: UInt64;
  SearchDir: string;
  fileData: TWin32FindData;
  hFind: THandle;
  tmp: LARGE_INTEGER;
  Err: TError;
begin
    Assert(sizeof(UInt64) = 8);
    Result := 0;

    // bypass directories from Exclude list
    if FExclFolders.QuickFind(CompareProcString, CurrDir) > 0 then Exit;

    DirSize := 0;
    tmp.QuadPart := 0;

    SearchDir := '\\?\' + CurrDir + '\*';  // add prefix to extend path string to 32767 symbols
    ZeroMemory(@fileData, sizeof(fileData));

    // this will be true only for the first ReadDirectory call in reccursion because for all other calls ShowProgress=false
    if ShowProgress then begin
      ProgressCounter := 0; // all reccursive calls are made with ShowProgress=False;
      NotifyStart(CurrDir);
    end;

    hFind := FindFirstFileEx(PChar(SearchDir), FindExInfoBasic, @fileData, FindExSearchNameMatch, nil, FIND_FIRST_EX_LARGE_FETCH);
    //hFind := Windows.FindFirstFile(PChar(searchDir), fileData);

    if (hFind = INVALID_HANDLE_VALUE) then begin
      Err.ErrCode := GetLastError();
      if Err.ErrCode = ERROR_ACCESS_DENIED then begin // special processing for ERROR_ACCESS_DENIED error
        Err.Msg := 'Access denied: ' + CurrDir;
        NotifyError(Err);
        var item := GetItem(Parent);
        item.FDenied := True; // set flag that we cannot enter into this folder because of permission denied.
      end
      else // other than ERROR_ACCESS_DENIED error encountered
      begin
        Err.Msg := 'ERROR in FindFirstFileEx: ' + CurrDir + ' GetLastError: ' + IntToStr(Err.ErrCode);
        NotifyError(Err); // this callback logs message into log file and shows messagebox with error message
      end;

      Result := DirSize;
      Exit;
    end;

    Assert(fileData.cFileName[0] <> #0);

    // bypass dirs '.' and '..'
    if NOT IS_DOT_DIR(fileData.cFileName) then begin
      var itemRef := AddItem(parent.ItemIndex, fileData, Parent.ItemLevel + 1);

      if IsDirectory(fileData) then begin
        if NOT IsReparse(fileData) then DirSize := ReadDirectory(CurrDir + '\' + fileData.cFileName, itemRef, False);
      end
        else DirSize := MakeFileSize(fileData.nFileSizeHigh, fileData.nFileSizeLow);
    end;

    while(True) do begin
      if ShowProgress then begin
        Inc(ProgressCounter);
        // if user pressed cancel then we raise an exception to be able to exit from all reccursive ReadDirectory calls.
        if NOT NotifyProgress(ProgressCounter) then raise EOperationCancelled.Create('User aborted.');
      end;

      if FindNextFile(hFind, fileData) then begin
        Assert(fileData.cFileName[0] <> #0);

        var tmp2:string := fileData.cFileName;

        if IS_DOT_DIR(fileData.cFileName) then continue;

        var itemRef := AddItem(parent.ItemIndex, fileData, Parent.ItemLevel + 1);

        if IsDirectory(fileData) then begin
          if NOT IsReparse(fileData) then DirSize := DirSize + ReadDirectory(currDir + '\' + fileData.cFileName, itemRef, False);
        end
          else DirSize := DirSize + MakeFileSize(fileData.nFileSizeHigh, fileData.nFileSizeLow)
      end
      else
      begin
        Err.ErrCode := GetLastError();
        if Err.ErrCode = ERROR_NO_MORE_FILES then break; // this is NOT an error
        Err.Msg := 'ERROR in FindNexFile: ' + CurrDir + ' GetLastError: ' + IntToStr(Err.ErrCode);
        NotifyError(Err);  // this callback logs message into log file and shows messagebox with error message
        break;
      end
    end;

    var item := GetItem(Parent);
    item.FFileSize := DirSize;

  //  FFindHandles.AddValue(hFind);
    Windows.FindClose(hFind); // weird, this call takes too much time for some reason. It is called for each scanned directory.

    if ShowProgress then NotifyFinish;

    Result := DirSize;
end;
{$WRITEABLECONST OFF}

//TODO shall we convert procedure to function to be able to return an error?
procedure TVolumeCache.FillFileData(const FilePath: string; var FileData: TWin32FindData);
var
  hf: THandle;
  fileSize: LARGE_INTEGER;
begin
  fileSize.QuadPart := 0;
  FileData.dwFileAttributes := Windows.GetFileAttributes(PChar(FilePath));

  // we need file handle first to get file time and file size
  hf := Windows.CreateFile(PChar(FilePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_BACKUP_SEMANTICS, 0);
  try
    if hf = INVALID_HANDLE_VALUE then begin
      raise EInOutError.Create(SysErrorMessage(GetLastError()), FilePath);
    end;

    Windows.GetFileTime(hf, @FileData.ftCreationTime, @FileData.ftLastAccessTime, @FileData.ftLastWriteTime);
    Windows.GetFileSizeEx(hf, fileSize.QuadPart);
    FileData.nFileSizeHigh := DWORD(fileSize.HighPart);
    FileData.nFileSizeLow := DWORD(fileSize.LowPart);

  finally
    Windows.CloseHandle(hf);
  end;
end;

// finds empty dirs - dirs that do not contain any files and subdirs
// calcs count of empty dirs and finds a directory with max number of files in it.
// on the way it does some integrity checks
function TVolumeCache.CheckHangingDirectories: THArrayG<string>;
var
  i, j : Cardinal;
  item, parent: TCacheItem;
  table: THash2<Cardinal, Cardinal, Cardinal>;
  pValue: THash2<Cardinal, Cardinal, Cardinal>.PointerV;
begin
  table := THash2<Cardinal, Cardinal, Cardinal>.Create;
  Result := THArrayG<string>.Create;
  try
    table.SetValue(0, 0, 0); // root is C: set counter for it
    for i := 1 to FCacheData.Count - 1 do begin // start from 1 here because we look for parent
      var lv := FCacheData[i];
      for j := 0 to lv.Count - 1 do begin
        item := TCacheItem(lv.GetAddr(j));
        if item.FDenied then Assert(item.IsDirectory); // FDenied can be set for directories only

        // bypass denied dirs
        if item.IsDirectory AND NOT item.FDenied AND NOT item.IsReparsePoint then begin // bypass symbolic links
          pValue := table.GetValuePointer(i, j);
          if pValue = nil then table.SetValue(i, j, 0); // if item is directory then set its links counter to zero
        end;

        parent := GetItem(i - 1, item.FParent);
        Assert(parent.IsDirectory);
        pValue := table.GetValuePointer(i - 1, item.FParent);
        Assert((pValue <> nil) OR parent.IsReparsePoint); // cannot be nil because we have already marked all previous level dirs with zero counter
        if Assigned(pValue) then Inc(pValue^); // increase links counter
      end;
    end;

    var mx: Cardinal := 0;
    var mxDir: string;

    for i := 1 to table.Count do begin
      for j := 1 to table.Count(i) do begin
        pValue := table.GetValuePointer(i - 1, j - 1);
        if Assigned(pValue) then begin
          if pValue^ = 0 then begin
            //item := GetItem(i - 1, j - 1);
            Result.AddValue(MakePathString(i - 1, j - 1));
          end;

          if mx < pValue^ then begin
            mx := pValue^; // find dir with maximum links count
            mxDir := MakePathString(i - 1, j - 1);
          end;
        end;
      end;
    end;

    Result.AddValue('Empty folders count: ' + Result.Count.ToString);
    Result.AddValue('Maximum items in Dir: ' + mx.ToString + ' - ' + mxDir);

  finally
    table.Free;
  end;
end;

procedure TVolumeCache.CheckThatParentIsDirectory;
var
  i, j: Cardinal;
  item, parent: TCacheItem;
begin
  for i := 1 to FCacheData.Count - 1 do begin
    var lv := FCacheData[i];
    for j := 0 to lv.Count - 1 do begin
      item := TCacheItem(lv.GetAddr(j));
      // Parent of any item must be a directory
      parent := GetItem(i - 1, item.FParent);
      Assert(parent.IsDirectory);
    end;
  end;
end;

procedure TVolumeCache.CheckLevelsDataTsCorrect;
var
  i, j: Cardinal;
  item: TCacheItem;
begin
  // check that all items contain correct Level data.
  for i := 1 to FCacheData.Count  do begin
    var lv := FCacheData[i - 1];
    for j := 1 to lv.Count do begin
      item := TCacheItem(lv.GetAddr(j - 1));
      Assert(item.FLevel = i - 1);
    end;
  end;
end;

procedure TVolumeCache.CheckFileDatesAreCorrect;
var
  i, j: Cardinal;
  item: TCacheItem;
begin
  // check that each item contain correct date fields.
  for i := 1 to FCacheData.Count  do begin
    var lv := FCacheData[i - 1];
    for j := 1 to lv.Count do begin
      item := TCacheItem(lv.GetAddr(j - 1));
      Assert(item.FFileAttrs <> 0);
      Assert(item.FFileName <> '');
      Assert(PInt64(@item.FCreationTime)^ <> 0);
      Assert(PInt64(@item.FLastAccessTime)^ <> 0);
      Assert(PInt64(@item.FModifiedTime)^ <> 0);
      // check size of files only, because directory size is size of all files inside directory
      if NOT item.IsDirectory then Assert(item.FFileSize < UInt64(100)*1024*1024*1024); // check that all sizes are less than 100G
    end;
  end;
end;
                             {
class procedure TVolumeCache.FreeInst;
begin
  if Assigned(GInstance) then FreeAndNil(GInstance);
end;                          }


procedure TVolumeCache.Clear;
var
  item: TCacheItem;
begin
  if FCacheData.Count = 0 then Exit;

  for var i: Cardinal := 0 to FCacheData.Count - 1 do begin
    var lv := FCacheData[i];
    for var j: Cardinal := 0 to lv.Count - 1 do begin
      item := TCacheItem(lv.GetAddr(j));
      item.CleanupInstance;
    end;
    lv.Free;
  end;

  FCacheData.Clear;
  FIndexedDateTime := 0; // reset indexed time
end;
{
procedure TVolumeCache.CloseFindHandles;
var i: Cardinal;
begin
  for i := 1 to FFindHandles.Count do Windows.FindClose(FFindHandles[i - 1]);
  FFindHandles.Clear;
end;
 }
function TVolumeCache.Count: Cardinal;
begin
  Result := 0;
  if FCacheData.Count = 0 then Exit;

  for var i: Cardinal := 0 to FCacheData.Count - 1 do
    Result := Result + FCacheData[i].Count;
end;

constructor TVolumeCache.Create;
begin
  FCacheData := THArrayG<TLevelType>.Create;
  FCacheData.SetCapacity(MAX_LEVELS);
  FProgressListeners := nil; //THArrayG<IIndexingProgress>.Create;
  FExclFolders :=  THArraySorted<string>.Create(TIStringComparer.Ordinal);
  FIndexedDateTime := 0; // default value, because index is not created yet
end;

destructor TVolumeCache.Destroy;
begin
  Clear;
  FreeAndNil(FCacheData);
  FreeAndNil(FExclFolders);
end;

/////////////////////////////////////
//  Auxiliary functions for Search
////////////////////////////////////

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForFileSize(var Filter: TSearchFilter; FileSize: UInt64{; IsDir: Boolean}): Boolean;
begin
  Result := False;
  case Filter.FileSizeCmpType of
    fscEquals: Result := FileSize = Filter.FileSize;
    fscMore:   Result := FileSize > Filter.FileSize;
    fscLess:   Result := FileSize < Filter.FileSize;
  end;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
// True if Filter.SearchStr is a substring of FileName
function CheckForFileName(var Filter: TSearchFilter; GrepList: TStringList; FileName, FileNameUpper: string): Boolean;
begin
  if GrepList = nil then begin
    if Filter.ExactSearch then // look for full name instead of substr
      if Filter.CaseSensitive
        then Result := Filter.SearchStr = FileName
        else Result := Filter.SearchStrUpper = FileNameUpper
    else
      if Filter.CaseSensitive   // look for substr
        then Result := Pos(Filter.SearchStr, FileName) > 0
        else Result := Pos(Filter.SearchStrUpper, FileNameUpper) > 0;
  end
  else
  begin   // use mask search
    if Filter.CaseSensitive
      then Result := cmpmask(FileName, GrepList)
      else Result := cmpmask(FileNameUpper, GrepList);
  end;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForDate(var Filter: TSearchFilter; item: TCacheItem): Boolean;
begin
  case Filter.SearchByDateType of
    sdModified:   Result := (CompareFileTime(item.FModifiedTime, Filter.DateFrom)  >= 0) AND (CompareFileTime(item.FModifiedTime, Filter.DateTo) <= 0);
    sdCreated:    Result := (CompareFileTime(item.FCreationTime, Filter.DateFrom)   >= 0) AND (CompareFileTime(item.FCreationTime, Filter.DateTo) <= 0);
    sdLastAccess: Result := (CompareFileTime(item.FLastAccessTime, Filter.DateFrom) >= 0) AND (CompareFileTime(item.FLastAccessTime, Filter.DateTo) <= 0);
    sdNone:       Result := False;
  end;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForAttributes(var Filter: TSearchFilter; FileAttributes: Cardinal): Boolean;
begin
  Result := (FileAttributes AND Filter.Attributes) > 0;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
// if GrepList=nil use substr search otherwise use mask search functions
function ApplyFilter(var Filter: TSearchFilter; GrepList: TStringList; Item: TCacheItem): Boolean;
begin
  Result := False; //Result=False by default means 'not found'
  if Filter.SearchStr <> '' then
    if NOT CheckForFileName(Filter, GrepList, Item.FFileName, Item.FUpperCaseName) then Exit;
  if Filter.SearchByFileSize then
    if NOT CheckForFileSize(Filter, Item.FFileSize{, IsDirectory(Item)}) then Exit;
  if Filter.SearchByDateType <> sdNone then
    if NOT CheckForDate(Filter, item) then Exit;
  if Filter.SearchByAttributes then
    if NOT CheckForAttributes(Filter, item.FFileAttrs) then Exit;

  Result := True;
end;

function TVolumeCache.Search(Filter: TSearchFilter; Callback: TFNCSearchResult): TSearchResult;
var
  //startArray: THArrayG<string>;
  GrepList: TStringList;
  i, j, len: Cardinal;
  //Found: Boolean;
  item: TCacheItem;
begin
  if FCacheData.Count = 0 then Exit(srNoIndexData);

  // determine whether we need to make "whole words" search
  Filter.ExactSearch := False;
  len := Length(Filter.SearchStr);
  if len > 2 then begin // check for surrounding double quotes and single quotes
    if (Filter.SearchStr[1] = '"')  AND (Filter.SearchStr[len] = '"') OR
       (Filter.SearchStr[1] = '''') AND (Filter.SearchStr[len] = '''') then begin
    Filter.ExactSearch := True;
    Filter.SearchStr := Copy(Filter.SearchStr, 2, len - 2);
    end;
  end;

  Filter.SearchStrUpper := AnsiUpperCase(Filter.SearchStr);
  //Found := False;

  //startArray := THArrayG<string>.Create;

  GrepList := nil;
  try
    // check if filter str has wildcards
    if (Pos('*', Filter.SearchStr) > 0) OR (Pos('?', Filter.SearchStr) > 0 ) then begin
      GrepList := TStringList.Create;
      if Filter.CaseSensitive // compile filters into GrepList
        then SetFilters(Filter.SearchStr, GrepList)
        else SetFilters(Filter.SearchStrUpper, GrepList);
    end;

    {StringToArray(Filter.StartFrom, startArray, '\');

    if StartArray.Count > 0 then begin      // verify that index DB contains all folders in Filter.StartFrom path
      for i := 0 to StartArray.Count - 1 do begin
        var lev := FCacheData[i];
        Found := False;
        for j := 0 to lev.Count - 1 do begin
          item := lev.GetAddr(j);
          if CompareText(item.FUpperCaseName, startArray[i]) = 0 then begin
            Found := True;
            break;
          end;
        end;
        if NOT Found then break; //looks like StartFrom is not found in index DB.
      end;

      if NOT Found then Exit(srWrongPath);
    end;
     }
    for i := 1{startArray.Count} to FCacheData.Count do begin // bypass Filter.StartFrom folders because each level contain only one folder from StartFrom path
      var lv := FCacheData[i - 1];
      Assert(lv.Count > 0);
      for j := 1 to lv.Count do begin
        item := TCacheItem(lv.GetAddr(j - 1));

        if ApplyFilter(Filter, GrepList, item) then begin
          if item.IsDirectory then begin // build PathString only for directories
            if item.FPath = '' then item.FPath := MakePathString(i - 1, j - 1);
            if NOT Callback(item.FPath, item) then Exit(srCancelled); //TODO: optimization: cache PathString in the item and use it during next searches
          end else begin
           if NOT Callback(MakePathString(i - 1, j - 1), item) then Exit(srCancelled);
          end;
        end;

      end;
    end;
    Result := srOK;
  finally
    //startArray.Free;
    FreeAndNil(GrepList); // works even when GrepList=nil
  end;
end;

procedure TVolumeCache.Serialize(OStream: TStream);
var
  i, j: Cardinal;
  item: TCacheItem;
begin
  OStream.WriteData<TDateTime>(FIndexedDateTime); // write datetime of latest index file update
  WriteStringToStream(OStream, FName);
  OStream.WriteData<Cardinal>(FCacheData.Count);
  for i := 1 to FCacheData.Count do begin
    var lv := FCacheData[i - 1];
    OStream.WriteData<Cardinal>(lv.Count);
    for j := 1 to lv.Count do begin
      item := TCacheItem(lv.GetAddr(j - 1));
      item.Serialize(OStream);
    end;
  end;

 // FModified := False;
end;

procedure TVolumeCache.Deserialize(IStream: TStream);
var
  i, j, start: Cardinal;
  CacheSize, levelSize: Cardinal;
  level: TLevelType;
  item: TCacheItem;
begin
  start := GetTickCount;

  Clear;

  IStream.ReadData<TDateTime>(FIndexedDateTime);
  FName := ReadStringFromStream(IStream);
  IStream.ReadData<Cardinal>(CacheSize);
  //Assert(cacheSize > 0);
  if cacheSize = 0 then begin // empty volume is OK. just write a warning into log file
    TLogger.LogFmt('[TVolumeCache.Deserialize] cache size read from index file is zero for volume "%s"!', [FName]);
    //Exit;
  end;

  FCacheData.SetCapacity(CacheSize);

  for i := 1 to CacheSize do begin
    level := TLevelType.Create;
    level.ItemSize := Cardinal(TCacheItem.InstanceSize);
    FCacheData.AddValue(level);

    IStream.ReadData<Cardinal>(levelSize);
    Assert(levelSize > 0);

    level.AddFillValues(levelSize);
    for j := 1 to levelSize do begin
      item := TCacheItem(TCacheItem.InitInstance(level.GetAddr(j-1))); // init object by specitied address
      item.Create;  // call constructor on object instantiated at specified address
      item.Deserialize(IStream);
     // level.Add(item);
    end;
  end;

  //FModified := False; // loading index file does not mean "Modified". Modified=True after updating data from file system
  FExecTime := GetTickCount - start;
end;



function CACHE_ITEM.Size: UInt32;
begin
  Result := sizeof(CACHE_ITEM) + FileAttr.FileNameLen * sizeof(WChar);
end;

function CACHE_ITEM.Name: string;
var p: PChar;
begin
  p := PChar(NativeInt(@Self) + sizeof(CACHE_ITEM));
  SetString(Result, p, fileAttr.FileNameLen);
end;

procedure TVolumeCache.Deserialize(IStream: PPFileLevel; Count: Cardinal);
const NTFS_DIRECTORY_MASK = $10000000; // this is NTFS attr directory flag, we neeed to move it into DOS_DIR flag place
var
  fileCache: PPFileLevel;
  origLevel: PFileLevel;
  origItem: PCACHE_ITEM;
  level: TLevelType;
  item: TCacheItem;
  j, i: Integer;
  start, err, len: Cardinal;
begin
  start := GetTickCount;

  Clear;

  FIndexedDateTime := Now;
  fileCache := IStream;

  for i := 1 to Count do begin
    origLevel := fileCache^;

    // avoid empty levels
    if origLevel.FCount > 0 then begin

      level := TLevelType.Create;
      level.ItemSize := Cardinal(TCacheItem.InstanceSize);
      FCacheData.AddValue(level);

      origItem := origLevel.FStart;

      level.AddFillValues(origLevel.FCount);
      for j := 1 to level.Count do begin
        item := TCacheItem(TCacheItem.InitInstance(level.GetAddr(j - 1))); // init object by specitied address
        item.Create; // call constructor on object instantiated at specified address

        //item.FVolume := Self;
        item.FParent := origItem.FParent;
        item.FLevel := origItem.FLevel;

        Assert((origItem.FileAttr.dup.FileAttrib AND FILE_ATTRIBUTE_DIRECTORY) = 0); // in NTFS this attr bit is always zero

        // some files have FileAttrib=0 for some reason. WINAPI never return zero attrs.
        // it sets NORMAL bit for such files. Below we do the same.
        if origItem.FileAttr.dup.FileAttrib = 0 then begin
          item.FFileAttrs := FILE_ATTRIBUTE_NORMAL;
        end else begin
          // move NTFS_DIR flag/bit ($10000000) into DOS_DOR place, because all WINAPI functions return this flag in DOS_DIR place ($00000010)
          item.FFileAttrs := origItem.FileAttr.dup.FileAttrib OR ((origItem.FileAttr.dup.FileAttrib AND NTFS_DIRECTORY_MASK) shr 24);
        end;

        Assert(item.FFileAttrs > 0);

        item.FCreationTime := origItem.FileAttr.dup.CreateTime;
        item.FLastAccessTime := origItem.FileAttr.dup.LastAccessTime;
        item.FModifiedTime := origItem.FileAttr.dup.ModifyTime;
        item.FFileSize := origItem.FileAttr.dup.FileSize;
        item.FFileName := origItem.Name;
        item.FUpperCaseName := AnsiUpperCase(item.FFileName);
        item.FDenied := False;

        origItem := PCACHE_ITEM(NativeInt(origItem) + origItem^.Size);
      end;

    end;

    fileCache := PPFileLevel(NativeInt(fileCache) + SizeOf(PPFileLevel));
  end;

  FExecTime := GetTickCount - start;

end;

function TVolumeCache.GetItem(Level, Index: Cardinal): TCacheItem;
begin
  Result := TCacheItem(FCacheData[Level].GetAddr(Index));
end;

function TVolumeCache.GetItem(itemRef: TCacheItemRef): TCacheItem;
begin
  Result:= TCacheItem(FCacheData.GetValue(itemRef.ItemLevel).GetAddr(itemRef.ItemIndex));
end;

function TVolumeCache.LevelCount(Level: Cardinal): Cardinal;
begin
  Result := FCacheData[Level].Count;
end;

function TVolumeCache.Levels(): Cardinal;
begin
  Result := FCacheData.Count;
end;
        {
class function TVolumeCache.Instance: TVolumeCache;
begin
  if NOT Assigned(GInstance) then GInstance := TVolumeCache.CreatePrivate;
  Result := GInstance;
end;

class function TVolumeCache.NewInstance: TVolumeCache;
begin
  Result := TVolumeCache.CreatePrivate;
end;

class function TVolumeCache.Swap(NewInstance: TVolumeCache): TVolumeCache;
begin
  Result := GInstance;
  GInstance := NewInstance;
end;      }

function TVolumeCache.AddLevel(level: Cardinal): TLevelType;
begin
  Assert(level <= FCacheData.Count);

  if level < FCacheData.Count then begin
    Result := FCacheData[level];
  end
  else
  begin
    Result := TLevelType.Create;
    Result.ItemSize := Cardinal(TCacheItem.InstanceSize);
    Result.SetCapacity(MAX_LEVEL_DIRS);
    FCacheData.AddValue(Result); // pointer is added to FCacheData because TCacheItem is class
  end;
end;

function TVolumeCache.AddRootItem(var fileData: TWin32FindData): TCacheItemRef;
var
  level: TLevelType;
  i: Cardinal;
  item: TCacheItem;
  //ref: TCacheItemRef;
begin
  level := AddLevel(0);

  i := 0;
  // for root item we always need to do search
  // it is posible to have several root items on level 0
  while i < level.Count do begin
    if TCacheItem(level.GetAddr(i)).FFileName = fileData.cFileName then break;
    Inc(i);
  end;

  if i < level.Count then begin // fount root item
    item := TCacheItem(level.GetAddr(i));
    Result := TCacheItemRef.Create(item.FLevel, i);
    Exit;
  end;

  // need to create new root items
  //level.AddFillValues(1);
  item := TCacheItem(TCacheItem.InitInstance(level.AddFillValues(1){level.GetAddr(level.Count - 1)}));
  item.Create(0, 0, fileData);
  //level.Add(item);
  Result.ItemLevel := 0;
  Result.ItemIndex := level.Count - 1;
  //Result := ref;
end;

function TVolumeCache.AddItem(parent: Cardinal; var fileData: TWin32FindData; itemLevel: Cardinal; doSearch: Boolean = False): TCacheItemRef;
var
  item: TCacheItem;
begin
  var level := AddLevel(itemLevel);

  //level.AddFillValues(1);
  item := TCacheItem(TCacheItem.InitInstance(level.AddFillValues(1){level.GetAddr(level.Count - 1)}));
  item.Create(parent, itemLevel, fileData);
  //level.Add(item);
  Result.ItemLevel := itemLevel;
  Result.ItemIndex := level.Count - 1;
end;

function TVolumeCache.AddFullPath(const Path: string): TCacheItemRef;
var
  pathArray: THArrayG<string>;
  pathArrayAccum: THArrayG<string>;
  fileData: TWin32FindData;
  lv: Cardinal;
  parent: TCacheItemRef;
begin
  pathArray := THArrayG<string>.Create;
  pathArrayAccum := THArrayG<string>.Create;

  try
    StringToArray(Path, pathArray, '\');
    StringToArrayAccum(Path, pathArrayAccum, '\');

    ZeroMemory(@fileData, sizeof(fileData));
    lstrcpy(fileData.cFileName, PWideChar(pathArray[0]));
    // _tcsncpy_s<MAX_PATH>(fileData.cFileName, pathArray[0].c_str(), pathArray[0].size());
    // fileData.cFileName[pathArray[0].size()] = '\0';
    FillFileData(pathArrayAccum[0], fileData);
    parent := AddRootItem(fileData);

    lv := 1;
    Assert(lv = parent.ItemLevel + 1);
    for lv := 1 to pathArray.Count - 1 do begin
      Assert(lv = parent.ItemLevel + 1);
      lstrcpy(fileData.cFileName, PWideChar(pathArray[lv]));
      // _tcsncpy_s<MAX_PATH>(fileData.cFileName, pathArray[lv].c_str(), pathArray[lv].size());
      // fileData.cFileName[pathArray[lv].size()] = '\0';
      FillFileData(pathArrayAccum[lv], fileData);
      parent := AddItem(parent.ItemIndex, fileData, lv, True);
    end;

  finally
    pathArray.Free;;
    pathArrayAccum.Free;
  end;

  Result := parent;
end;

procedure TVolumeCache.SerializeTo(const FileName: string);
var
  mout: TMemoryStream;
begin
  mout := TMemoryStream.Create;
  try
    Serialize(mout);
    mout.SaveToFile(FileName);
  finally
    mout.Free;
  end;
end;

function TVolumeCache.Size: UInt64;
begin
  if Count = 0 then Exit(0);
  Result := GetItem(0, 0).FFileSize;
end;

{procedure TVolumeCache.DeserializeFrom(const FileName: string);
var
  msin: TMemoryStream;
begin
  msin := TMemoryStream.Create;
  try
    if FileExists(FileName) then begin //TODO: shall we raise an exception or return false in case index file is not found?
      msin.LoadFromFile(FileName);
      Deserialize(msin);
    end else begin
      Logger.LogFmt('Index file is not found or not accessible ().', [FileName]);
    end;
  finally
    msin.Free;
  end;
end;}

procedure TVolumeCache.SaveTo(const FileName: string);
var
  fout:TFileStream;
  ii, k: Integer;
  i,j, index: Cardinal;
  sitem, item: TCacheItem;
  pathStr: string;
  path: THArrayG<TCacheItem>;
begin

  fout := TFileStream.Create(fileName, fmCreate);
  path := THArrayG<TCacheItem>.Create;
  path.SetCapacity(MAX_LEVELS);

  try
    for i := FCacheData.Count - 1 downto 0 do begin
      var level := FCacheData[i];

      for j := 0 to level.Count - 1 do begin
        sitem := TCacheItem(level.GetAddr(j));

        path.Clear();
        path.AddValue(sitem);
        index := sitem.FParent;
        ii := Integer(i) - 1; // ii variable need to be signed Integer

        while ii >= 0 do begin
          item := GetItem(Cardinal(ii), index);
          path.AddValue(item);
          index := item.FParent;
          Dec(ii);
        end;

        pathStr := path[path.Count - 1].FFileName;
        for k := Integer(path.Count) - 2 downto 0 do begin
          pathStr := pathStr + '\' +  path[Cardinal(k)].FFileName;
        end;

        fout.Write(PChar(pathStr)^, ByteLength(pathStr){*sizeof(Char)});
        fout.Write(PChar(sLineBreak)^, ByteLength(sLineBreak){*sizeof(Char)});
      end;
    end;

  finally
    fout.Free;
    path.Free;
  end;
end;

function TVolumeCache.ReadVolume(Volume: string; ExclusionsList: TArray<string>): UInt64;
var
  i: Cardinal;
  str: string;
begin
  var start := GetTickCount;
  Clear;
  FName := Volume;

  //TODO: may be list of exclusions should be Volume agnostic somehow???

  // fill ExclFolders with values related to the specified Volume only
  for i := 1 to Length(ExclusionsList) do begin
    str := ExclusionsList[i - 1];
    if str.StartsWith(Volume) then begin
      str := ExcludeTrailingPathDelimiter(str); // delete trailing backslash for proper comparing in ReadDirectory method
      FExclFolders.AddValue(str);
    end;
  end;

  Volume := ExcludeTrailingPathDelimiter(Volume);
  var startItemRef := AddFullPath(Volume);
  Result := ReadDirectory(Volume, startItemRef, True);

  FExecTime := GetTickCount - start;
  //FModified := True;
  FIndexedDateTime := Now;
end;

procedure TError.SetMsg(msg: string);
begin
  Assert(Length(msg) < 255); // one symbol is for zero terminator
  StrPCopy(ErrText, msg);
end;

function TError.GetMsg: string;
begin
  Result := ErrText;
end;

procedure TError.Create(code: Integer; msg: string);
begin
  Assert(Length(msg) < 255); // one symbol is for zero terminator
  ErrCode := code;
  StrPCopy(ErrText, msg);
end;

function TError.HasError;
begin
  Result := ErrCode <> 0;
end;

// function from MFT DLL
function ReadVolumeDirect(Volume: PChar; var VolSize: UInt64; var Cnt: Cardinal; var Data: Pointer): TError; stdcall; external 'MFTReaderDLL.dll' name 'ReadVolume';

function TVolumeCache.ReadVolumeFast(Volume: string; ExclusionsList: TArray<string>): UInt64;
var
  fileCache: PPFileLevel;
  i, cnt: Cardinal;
  volSize: UInt64;
  str: string;
  err: TError;
begin
  var start := GetTickCount;

  Result := 0;

  NotifyStart(Volume);

  Clear;
  FName := Volume;

  //TODO: may be list of exclusions should be Volume agnostic somehow???

  // fill ExclFolders with values related to the specified Volume only
  for i := 1 to Length(ExclusionsList) do begin
    str := ExclusionsList[i - 1];
    if str.StartsWith(Volume) then begin
      str := ExcludeTrailingPathDelimiter(str); // delete trailing backslash for proper comparing in ReadDirectory method
      FExclFolders.AddValue(str);
    end;
  end;

  Volume := ExcludeTrailingPathDelimiter(Volume);

  NotifyProgress(50);

  err := ReadVolumeDirect(PChar(Volume), volSize, cnt, Pointer(fileCache));

  if err.HasError then begin
    TLogger.LogFmt('Error loading volume %s. Error code: %d. Error msg: %s.', [Volume, err.ErrCode, err.Msg]);
    NotifyError(err);
    Exit;
  end;

  Deserialize(fileCache, cnt);  // this call initializes FIndexedDateTime to Now()

  NotifyFinish();

  FExecTime := GetTickCount - start;
  //FModified := True;
  //FIndexedDateTime := Now;
end;

{
function TVolumeCache.ReadFileSystem(const Volumes: TArray<string>): UInt64;
var
  i, cnt: Cardinal;
begin
  Clear; // remove previous cache data

  cnt := Length(Volumes);
  for i := 0 to cnt - 1 do begin
    var startItemRef := AddFullPath(Volumes[i]);
    Result := ReadDirectory(Volumes[i], startItemRef, True);
  end;

  FModified := True;
end;
 }

function TVolumeCache.MakePathString(ref: TCacheItemRef):string;
begin
  Result := MakePathString(ref.ItemLevel, ref.ItemIndex);
end;


//type
//  TGPath = THArrayG<string>;
var
  GPathCache: TObjectsCache<THArrayG<string>> = nil; // optimization, global array to hold path items before converting into full path string

function TVolumeCache.MakePathString(itemLevel, itemIndex: Cardinal): string;
var
  k, ii: Integer;
  GPath: THArrayG<string>;
begin
  GPath := GPathCache.GetItem;
  //GPath.SetCapacity(MAX_DIR_LEVELS); // because of cache memory for capacity will be allocated only once during first call
  //GPath.Clear;

  try
    var item := GetItem(ItemLevel, ItemIndex);

    if itemLevel = 0 then begin  // we asked for root level item, return it and exit
      Result := item.FFileName;
      Exit;
    end;

    GPath.AddValue(item.FFileName); //TODO: may be we need change it

    var index := item.FParent;
    ii := Integer(ItemLevel - 1); // ii variable needs to be signed Integer

    while ii >= 0 do begin
      item := GetItem(Cardinal(ii), index);
      GPath.AddValue(item.FFileName); //TODO: may be we need change it
      index := item.FParent;
      Dec(ii);
    end;

    Result := GPath[GPath.Count - 1];
    for k := Integer(GPath.Count) - 2 downto 0 do begin
      Result := Result + '\' + GPath[Cardinal(k)];
    end;

  finally
    GPath.Clear; // memory is not de-allocated, only reset Count to zero.
    GPathCache.PutItem(GPath); // return object back to cache
  end;
end;

procedure TVolumeCache.NotifyFinish;
begin
  if Assigned(FProgressListeners) then
    for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].Finish;
end;

function TVolumeCache.NotifyProgress(Progrs: Integer): Boolean;
begin
  Result := False; // cancel by default
  if Assigned(FProgressListeners) then
    for var i := 1 to FProgressListeners.Count do
      if NOT FProgressListeners[i - 1].Progress(Progrs) then Exit;
  Result := True;  // everything is ok return true
end;

procedure TVolumeCache.NotifyStart(Notes: string);
begin
  if Assigned(FProgressListeners) then
    for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].Start(Notes);
end;

procedure TVolumeCache.NotifyError(Error: TError);
begin
  if Assigned(FProgressListeners) then
    for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].ReportError(Error);
end;

procedure TVolumeCache.PrintLevelsStat(list: TStrings);
begin
  list.Add(Format('Levels : %u', [FCacheData.Count]));
  var sum: Cardinal := 0;

  for var i: Cardinal := 0 to FCacheData.Count - 1 do begin
    var item := FCacheData[i];
    list.Add(Format('Level %u : %u', [i, item.Count]));
    sum := sum + item.Count;
  end;

  list.Add(Format('SUMM of Levels : ', [sum]));
end;

procedure TVolumeCache.PrintAllItems(list: TStrings);
var
  i, j: Cardinal;
  sitem: TCacheItem;
  pathStr: string;
begin
  for i := 1 to FCacheData.Count do begin
    var level := FCacheData[Cardinal(i - 1)];

    for j := 1 to level.Count do begin
      sitem := TCacheItem(level.GetAddr(j - 1));

      if sitem.IsDirectory then begin
        pathStr := MakePathString(i - 1, j - 1);

        list.Add(Format('%s \t %u', [pathStr, sitem.FFileSize]));
        //"\t" << FileTimeToString(sitem.FFileData.ftLastWriteTime) << "\t" << FileTimeToString(sitem.FFileData.ftLastAccessTime) << std::endl;
      end;
    end;
  end;
 end;

function TVolumeCache.GetStat(): TFileSystemStatRecord;
var
   i,j: Cardinal;
   item: TCacheItem;
begin
  ZeroMemory(@Result, sizeof(Result));

  if FCacheData.Count = 0 then Exit;

  //var totalItems: Cardinal := 0;
  var countedItems: Cardinal := 0;

  for i := 0 to FCacheData.Count - 1 do begin
    var lv := FCacheData[i];
    // totalItems := totalItems + lv.Count;
    for j := 0 to lv.Count - 1 do begin
      item := TCacheItem(lv.GetAddr(j));
      var counted: Boolean := false;

      if (item.FFileAttrs AND FILE_ATTRIBUTE_DIRECTORY)    > 0 then begin Inc(Result.Stat[ftDir]);       counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_ARCHIVE)      > 0 then begin Inc(Result.Stat[ftArchive]);   counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_READONLY)     > 0 then begin Inc(Result.Stat[ftReadOnly]);  counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_NORMAL)       > 0 then begin Inc(Result.Stat[ftFile]);      counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_HIDDEN)       > 0 then begin Inc(Result.Stat[ftHidden]);    counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_TEMPORARY)    > 0 then begin Inc(Result.Stat[ftTemp]);      counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_SYSTEM)       > 0 then begin Inc(Result.Stat[ftSystem]);    counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_DEVICE)       > 0 then begin Inc(Result.Stat[ftDevice]);    counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_REPARSE_POINT)> 0 then begin Inc(Result.Stat[ftSymbolic]);  counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_COMPRESSED)   > 0 then begin Inc(Result.Stat[ftCompressed]);counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_ENCRYPTED)    > 0 then begin Inc(Result.Stat[ftEncrypted]); counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_OFFLINE)      > 0 then begin Inc(Result.Stat[ftOffline]);   counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_SPARSE_FILE)  > 0 then begin Inc(Result.Stat[ftSparse]);    counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_VIRTUAL)      > 0 then begin Inc(Result.Stat[ftVirtual]);   counted := True; end;
      if (item.FFileAttrs AND FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) > 0 then begin Inc(Result.Stat[ftNotIndexed]); counted := True; end;
      //if (item.FFileData.dwFileAttrs AND FILE_ATTRIBUTE_PINNED) > 0 then begin Inc(Result[ftPinned]); counted := true; end;
      if counted
        then Inc(countedItems)
        else raise Exception.Create('Uncounted file type encontered!'); //list.Add(Format('Missing file attribute : %s : %u', [item.FFileData.cFileName, item.FFileData.dwFileAttributes]));
    end;
  end;

  Result.Stat[ftAll] := countedItems;

  StatSort(Result);
end;

procedure TVolumeCache.StatSort(var Stat: TFileSystemStatRecord);
var
  i, j: Cardinal;
  k, val, L, R: Cardinal;
  valInd: TFileTypes;
begin
  for k := Low(Stat.Index) to High(Stat.Index) do Stat.Index[k] := TFileTypes(k);

  L := Low(Stat.Index);
  R := High(Stat.Index);

  for i := L + 1 to R do begin
    j := i;
    valInd := Stat.Index[i];
    val := Stat.Stat[valInd];
    while (j > L) AND (Stat.Stat[Stat.Index[j - 1]] < val) do begin  // '<' means sorting in reverse order
        Stat.Index[j] := Stat.Index[j - 1];
        Dec(j);
    end;
    Stat.Index[j] := valInd;
  end;
end;

procedure TVolumeCache.PrintStat(stat: TFileSystemStat; list: TStrings);
begin
  list.Add(Format('Total number of files and dirs : %u', [stat[ftAll]]));
  // list.Add(Format('Total without dirs : %u', [totalItems - stat[ftDir]]));
  list.Add(Format('Directories : %u', [stat[ftDir]]));
  list.Add(Format('Read Only   : %u', [stat[ftReadOnly]]));
  list.Add(Format('Archive     : %u', [stat[ftArchive]]));
  list.Add(Format('Hidden      : %u', [stat[ftHidden]]));
  list.Add(Format('Temporary   : %u', [stat[ftTemp]]));
  list.Add(Format('System      : %u', [stat[ftSystem]]));
  list.Add(Format('Devices     : %u', [stat[ftDevice]]));
  list.Add(Format('Symbolic    : %u', [stat[ftSymbolic]]));
  list.Add(Format('Compressed  : %u', [stat[ftCompressed]]));
  list.Add(Format('Encrypted   : %u', [stat[ftEncrypted]]));
  list.Add(Format('Offline     : %u', [stat[ftOffline]]));
  list.Add(Format('Sparse      : %u', [stat[ftSparse]]));
  list.Add(Format('Normal      : %u', [stat[ftFile]]));
  list.Add(Format('Pinned      : %u', [stat[ftPinned]]));
  list.Add(Format('NOT Indexed : %u', [stat[ftNotIndexed]]));

  //list.Add(Format('Remaining   : %u', [totalItems - countedItems]));

end;


{ TCache }

function TCache.CheckHangingDirectories: THArrayG<string>;
var
  i: Integer;
  emptyDirs: THArrayG<string>;
begin
  for i := 1 to FVolumeData.Count do begin
    emptyDirs := FVolumeData.GetPair(i - 1).Second.CheckHangingDirectories; //TODO: do something useful with emptyDirs info
  end;
end;

procedure TCache.CheckLevelsDataTsCorrect;
var
  i: Integer;
begin
  for i := 1 to FVolumeData.Count do FVolumeData.GetPair(i - 1).Second.CheckLevelsDataTsCorrect;
end;

procedure TCache.CheckThatParentIsDirectory;
var
  i: Integer;
begin
  for i := 1 to FVolumeData.Count do FVolumeData.GetPair(i - 1).Second.CheckThatParentIsDirectory;
end;

procedure TCache.CheckFileDatesAreCorrect;
var
  i: Integer;
begin
  for i := 1 to FVolumeData.Count do FVolumeData.GetPair(i - 1).Second.CheckFileDatesAreCorrect;
end;

procedure TCache.Clear(Volume: string);
begin
  FVolumeData.GetValue(Volume).Free;
  FVolumeData.Delete(Volume);
end;

procedure TCache.Clear;
var
  i: Cardinal;
begin
  for i := 1 to FVolumeData.Count do FVolumeData.GetPair(i - 1).Second.Free;
  FVolumeData.Clear;
  FIndexFileSaveDate := 0; // reset index file date
end;

constructor TCache.Create;
begin
  raise ENoConstructException.Create('TCache instance cannot be directly constructed.');
end;

constructor TCache.CreatePrivate;
begin
  FVolumeData := THash<string, TVolumeCache>.Create;
  FProgressListeners := THArrayG<IIndexingProgress>.Create;
  FIndexFileSaveDate := 0; // default value, because index file is not loaded yet
end;

procedure TCache.Deserialize(IStream: TStream);
var
  i, VolumesCnt: Cardinal;
  vol: TVolumeCache;
begin
  Clear;

  IStream.ReadData<TDateTime>(FIndexFileSaveDate);
  IStream.ReadData<Cardinal>(VolumesCnt);
  if VolumesCnt = 0 then Exit;

  //TODO: possibly we can do it more effective - do not delete TVolumeCache cashes but just clear them and preserve allocated memory
  for i := 1 to VolumesCnt do begin
    vol := TVolumeCache.Create;
    vol.Deserialize(IStream);
    FVolumeData.SetValue(vol.FName, vol);
  end;
end;

// loads nothing if file does not exist
//TODO: think of if this function need to raise an exception if file does not exist or not accessible
procedure TCache.DeserializeFrom(const FileName: string);
var
  msin: TMemoryStream;
begin
  msin := TMemoryStream.Create;
  try
    if FileExists(fileName) then begin
      msin.LoadFromFile(FileName);
      Deserialize(msin);
    end else begin
      TLogger.LogFmt('Index file is not found or not accessible ().', [FileName]);
    end;
  finally
    msin.Free;
  end;
end;

procedure TCache.Serialize(OStream: TStream);
var
  i: Cardinal;
begin
  FIndexFileSaveDate := Now;
  OStream.WriteData<TDateTime>(FIndexFileSaveDate);
  OStream.WriteData<Cardinal>(FVolumeData.Count);
  for i := 1 to FVolumeData.Count do FVolumeData.GetPair(i - 1).Second.Serialize(OStream);
end;

procedure TCache.SerializeTo(const FileName: string);
var
  mout: TMemoryStream;
begin
  mout := TMemoryStream.Create;
  try
    Serialize(mout);
    mout.SaveToFile(FileName);
  finally
    mout.Free;
  end;
end;

destructor TCache.Destroy;
begin
  Clear;
  FreeAndNil(FVolumeData);
  FreeAndNil(FProgressListeners);

  inherited;
end;

function TCache.VolumePresent(Volume: string): TVolumeCache;
var
  p: ^TVolumeCache;
begin
   Result := nil;
   p := FVolumeData.GetValuePointer(Volume);
   if Assigned(p) then Result := p^;
end;

function TCache.VolumesCount: Cardinal;
begin
  Result := FVolumeData.Count;
end;

function TCache.GetExecData: TArray<TVolumeExecData>;
var
  i: Cardinal;
  rec: TVolumeExecData;
begin
  for i := 1 to FVolumeData.Count do begin
    var vol := FVolumeData.GetPair(i - 1).Second;
    rec.VolumeName := vol.VolName;
    rec.ExecTime := vol.ExecTime;
    rec.VolSize := vol.Size;
    rec.ItemsCount := vol.Count;
    Insert(rec, Result, Length(Result));
  end;
end;

{
function TCache.GetModified: Boolean;
var
  i: Cardinal;
begin
  Result := False;
  for i := 1 to FVolumeData.Count do begin
    Result := Result OR FVolumeData.GetPair(i - 1).Second.Modified;
    if Result then break;
  end;
end;
 }

function TCache.GetStat(Volume: string): TFileSystemStatRecord;
begin
  Result := FVolumeData[Volume].GetStat;
end;

function TCache.GetVolume(Volume: string): TVolumeCache;
begin
   Result := FVolumeData.GetValue(Volume);
end;

function TCache.GetVolumeNamesAsString: string;
var
  i: Cardinal;
begin
  for i := 1 to FVolumeData.Count do
    Result := Result + ' ' + FVolumeData.GetPair(i - 1).Second.VolName;
end;

function TCache.GetVolumes: TArray<string>;
var
  i: Cardinal;
begin
  for i := 1 to FVolumeData.Count do
    Insert(FVolumeData.GetPair(i - 1).Second.VolName, Result, Length(Result));
end;

class destructor TCache.FreeInst;
begin
  if Assigned(GInstance) then FreeAndNil(GInstance);
  if Assigned(GInstance2) then FreeAndNil(GInstance2);
end;

class procedure TCache.FreeInst2;
begin
  if Assigned(GInstance2) then FreeAndNil(GInstance2);
end;

class function TCache.Instance: TCache;
begin
  if NOT Assigned(GInstance) then GInstance := TCache.CreatePrivate;
  Result := GInstance;
end;

class function TCache.NewInstance: TCache;
begin
  if NOT Assigned(GInstance2) then GInstance2 := TCache.CreatePrivate;
  Result := GInstance2;
end;

function TCache.Search(Filter: TSearchFilter; Callback: TFNCSearchResult): TSearchResult;
var
  i: Cardinal;
begin
  Result := srOK;
  for i := 1 to FVolumeData.Count do
    Result := FVolumeData.GetPair(i - 1).Second.Search(Filter, Callback);
end;

class function TCache.HasNewInstance: Boolean;
begin
  Result := Assigned(GInstance2);
end;

class procedure TCache.Swap;
begin
  if Assigned(GInstance) then GInstance.Free;
  GInstance := GInstance2;
  GInstance2 := nil;
end;

procedure TCache.AddProgressListener(listener: IIndexingProgress);
begin
  if NOT Assigned(listener) then Exit;

  // check if listener has already added to the list
  if FProgressListeners.IndexOf(listener) = -1 then FProgressListeners.AddValue(listener);
end;


function TCache.GetOrCreateVolume(Volume: string): TVolumeCache;
var
  pvol: ^TVolumeCache;
begin
  pvol := FVolumeData.GetValuePointer(Volume);

  if Assigned(pvol) then begin
    Result := pvol^;
  end else begin
    Result := TVolumeCache.Create;
    FVolumeData[Volume] := Result;
  end;
end;

procedure TCache.ReadVolume(Volume: string; ExclusionsList: TArray<string>);
var
  vol: TVolumeCache;
begin
  vol := GetOrCreateVolume(Volume);

  vol.FProgressListeners := FProgressListeners;
  vol.ReadVolume(Volume, ExclusionsList);
  vol.FProgressListeners := nil;
end;

// works only for NTFS drives
procedure TCache.ReadVolumeFast(Volume: string; ExclusionsList: TArray<string>);
var
  vol: TVolumeCache;
  MaxComponentLen, SystemFlags: DWORD;
  fsType: string;
begin
  vol := GetOrCreateVolume(Volume);

  // check that Volume is NTFS volume type.
  // if not - call ReadVolume function that works with all volume types (but slower)
  SetLength(fsType, MAX_PATH);
  GetVolumeInformation(PChar(Volume), nil, 0, nil, MaxComponentLen, SystemFlags, PChar(fsType), MAX_PATH);

  if fsType.StartsWith('NTFS', True) then begin
    vol.FProgressListeners := FProgressListeners;
    vol.ReadVolumeFast(Volume, ExclusionsList);
    vol.FProgressListeners := nil;
  end else begin
    ReadVolume(Volume, ExclusionsList);
  end;

end;

procedure TCache.ReadVolumesFast(Volumes: TArray<string>; ExclusionsList: TArray<string>);
var
  i: Cardinal;
  vol: TVolumeCache;
begin
  for i := Low(Volumes) to High(Volumes) do begin
    vol := GetOrCreateVolume(Volumes[i]);
    vol.ReadVolumeFast(Volumes[i], ExclusionsList);
  end;
end;


procedure TCache.RemoveProgressListener(listener: IIndexingProgress);
begin
  var index := FProgressListeners.IndexOf(listener);
  if index <> -1 then FProgressListeners.DeleteValue(Cardinal(index));
end;


initialization
  GPathCache := TObjectsCache<THArrayG<string>>.Create(3, True); //we have two threads that will work with this global objects, so 3 items should be enough
  //GPath.SetCapacity(MAX_DIR_LEVELS);

finalization
  FreeAndNil(GPathCache);
  //TCache.FreeInst; // free cache singlton
end.
