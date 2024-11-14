Unit FileNamesCache;

interface

uses System.Classes, System.SysUtils, Windows, DynamicArray;

type

 TCacheItemRef = record
    ItemLevel: Cardinal;
    ItemIndex: Cardinal;
    constructor Create(level, index: Cardinal);
 end;

 TCacheItem = class
 public
   FParent: Cardinal;
   FLevel: Cardinal;
   FFileData: TWin32FindData;
   FFullFileSize: uint64;
   FUpperCaseName: string; // name of file/dir in upper case. need for search routines
   FDisplayName: string;
   FFileType: string;
   FIconIndex: Integer;
   FDenied: Boolean;
   constructor Create; overload;
   constructor Create(Parent: Cardinal; var FileData: TWin32FindData; Level: Cardinal); overload;
   procedure Serialize(OStream: TStream);
   procedure Deserialize(IStream: TStream);
 end;

 TLevelType = THArrayG<TCacheItem>;

 TFileTypes = (ftFile, ftDir, ftTemp, ftArchive, ftReadOnly, ftHidden, ftSystem, ftDevice, ftSymbolic, ftCompressed,
               ftEncrypted, ftOffline, ftSparse, ftPinned, ftNotIndexed, ftVirtual, ftAll);

 TFileTypeNames = array [TFileTypes] of string;
 TFileSystemStat = array [TFileTypes] of Cardinal;
 TFileSystemStatIndex = array [Ord(Low(TFileTypes)).. Ord(High(TFileTypes))] of TFileTypes;

 TFileSystemStatRecord = record
   Stat: TFileSystemStat;
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

 TFNCSearchResult = function(FullPath: string; FileData: TCacheItem): Boolean of object;
// TFNCIndexingProgress = procedure(Progress: Integer) of object;

 IIndexingProgress = class
   procedure Start(P100: Integer); virtual; abstract; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; virtual; abstract;
   function Progress(Prgress: Integer): Boolean; virtual; abstract; // allows to stop process if indexing takes too long time
   procedure ReportError(ErrorStr: string); virtual; abstract;
 end;

  TFileSizeCompare = (fscEquals, fscMore, fscLess);

  TSearchFilter = record
    StartFrom: string;
    SearchStr: string;
    SearchStrUpper: string; // optimization for case insensitive search
    CaseSensitive: Boolean;
    SearchByFileSize: Boolean;
    FileSize: uint64;
    FileSizeCmpType: TFileSizeCompare;
    SearchByModifiedDate: Boolean;
    ModifiedDateFrom: TFileTime;
    ModifiedDateTo: TFileTime;
    SearchByAttributes: Boolean;
    Attributes: Cardinal;
  end;


 TFileNamesCache = class
 private
   class var GInstance: TFileNamesCache; // single instance of cache
 private // do not remove this 'private' keyword
   FCacheData: THArrayG<TLevelType>;
   FProgressListeners: THArrayG<IIndexingProgress>;
   FFindHandles: THarrayG<THandle>;
   FModified: Boolean;
   FDateTimeIndexFile: TDateTime; // datetime when loaded index file was saved, valid only after loading index file.

   procedure Serialize(OStream: TStream);
   procedure Deserialize(IStream: TStream);
   procedure SaveTo(const fileName: string);
   function AddLevel(level: Cardinal): TLevelType;
   function AddRootItem(var fileData: TWin32FindData):TCacheItemRef;
   function AddItem(parent: Cardinal; var fileData:TWin32FindData; itemLevel: Cardinal; doSearch: Boolean = False): TCacheItemRef;
   function AddFullPath(const path: string): TCacheItemRef;
   function GetItem(itemRef: TCacheItemRef): TCacheItem; overload;
   procedure FillFileData(const filePath: string; var fileData: TWin32FindData);
   function ReadDirectory(const currDir: TFileName; parent: TCacheItemRef; ShowProgress: Boolean): uint64;
   procedure NotifyStart;
   procedure NotifyFinish;
   function NotifyProgress(prog: Integer): Boolean;
   procedure NotifyError(ErrorStr: string);
   procedure CloseFindHandles;
   constructor CreatePrivate;
   destructor Destroy; override;
   class procedure FreeInst;
   procedure StatSort(var Stat: TFileSystemStatRecord);

 public
   constructor Create; // raises an exception to avoid creating other instances of cache
   class function Instance: TFileNamesCache;
   procedure SerializeTo(const fileName:string);
   procedure DeserializeFrom(const fileName:string);
   procedure Clear;
   function  Count: Cardinal;
   function  GetItem(Level: Cardinal; Index: Cardinal): TCacheItem; overload;
   function  GetLevelCount(Level: Cardinal): Cardinal;
   function  ReadFileSystem(const startDir: string): uint64;
   function  MakePathString(ref: TCacheItemRef): string; overload;
   function  MakePathString(itemLevel, itemIndex: Cardinal): string; overload;
   procedure AddProgressListener(listener: IIndexingProgress);
   procedure RemoveProgressListener(listener: IIndexingProgress);
   procedure Search(Filter: TSearchFilter; Callback: TFNCSearchResult);

   procedure PrintLevelsStat(list: TStrings);
   procedure PrintAllItems(list: TStrings);
   function  GetStat: TFileSystemStatRecord;
   procedure PrintStat(stat: TFileSystemStat; list: TStrings);

   property  Modified: Boolean read FModified write FModified;
   property  IndexFileDate: TDateTime read FDateTimeIndexFile;
   property  FindHandles: THArrayG<THandle> read FFindHandles;
 end;


 TTopFolders = class
 protected
   FItems: THArrayG<TCacheItem>;
   FMin: uint64;
   FMax: uint64;
   FSize: Cardinal;
   procedure UpdateMinMaxAndDelete();
   procedure AddValue(Item: TCacheItem);
   function CompareProc(Item1, Item2: TCacheItem): Integer;
 public
   constructor Create(Num: Cardinal);
   destructor Destroy; override;
   procedure BuildTopFolders;
   procedure BuildTopFiles;
   function GetItem(Index: Cardinal): TCacheItem;
 end;

  TFSC = TFileNamesCache;  // short alias for class name, just for convenience

  function IsDirectory(Item: TCacheItem): Boolean; overload;
  function IsDirectory(var FileData: TWin32FindData): Boolean; overload;

var
  FileTypeNames: TFileTypeNames = ('File', 'Directory', 'Temporary', 'Archive', 'ReadOnly', 'Hidden', 'System', 'Device',
                                 'Symbolic Link', 'Compressed', 'Encrypted', 'Offline', 'Sparse', 'Pinned', 'NotIndexed', 'Virtual', 'UNDEF');

implementation

uses
  System.UITypes, System.Math, Dialogs, Functions, MaskSearch;

const MAX_DIR_LEVELS = 100;
const MAX_DIRS = 10_000;
const MAX_FIND_HANDLES_CAPACITY = 30_000;

////////////////////////////////////
// Common Functions
///////////////////////////////////

function IS_DOT_DIR(dirName: PWideChar): Boolean;
begin
  //Result := (dirName = '.') OR (dirName = '..');
	Result := ( (dirName[0] = '.') AND (dirName[1] = #0) ) OR ( (dirName[0] = '.') AND (dirName[1] = '.') AND (dirName[2] = #0) );
end;

function MakeFileSize(hi, lo: Cardinal) : uint64; inline;
begin
  Result := (uint64(hi) shl 32) + uint64(lo);
end;

////////////////////////////////////
// TCacheItem class methods
///////////////////////////////////

constructor TCacheItemRef.Create(level: Cardinal; index: Cardinal);
begin
  ItemLevel := level;
  ItemIndex := index;
end;

constructor TCacheItem.Create();
begin
    FParent := 0;
    FLevel := 0;
    ZeroMemory(@FFileData, sizeof(TWin32FindData));
    FFullFileSize := 0;
    FIconIndex := -1;
    FDenied := False;
    FUpperCaseName := '';
end;

constructor TCacheItem.Create(Parent: Cardinal; var FileData: TWin32FindData; Level: Cardinal);
begin
    FParent := Parent;
    FLevel := Level;
    FFileData := FileData; // because TWin32FindData is a record, data is copied here to FFileData
    FFullFileSize := MakeFileSize(FileData.nFileSizeHigh, FileData.nFileSizeLow);
    FDenied := False;
    FUpperCaseName := AnsiUpperCase(FileData.cFileName);
end;

procedure TCacheItem.Serialize(OStream: TStream);
begin
  OStream.WriteData<Cardinal>(FParent);
  OStream.WriteData<Cardinal>(FFileData.dwFileAttributes);
  OStream.WriteData<Cardinal>(FFileData.ftCreationTime.dwHighDateTime);
  OStream.WriteData<Cardinal>(FFileData.ftCreationTime.dwLowDateTime);
  OStream.WriteData<Cardinal>(FFileData.ftLastAccessTime.dwHighDateTime);
  OStream.WriteData<Cardinal>(FFileData.ftLastAccessTime.dwLowDateTime);
  OStream.WriteData<Cardinal>(FFileData.ftLastWriteTime.dwHighDateTime);
  OStream.WriteData<Cardinal>(FFileData.ftLastWriteTime.dwLowDateTime);
  OStream.WriteData<Cardinal>(FFileData.nFileSizeHigh);
  OStream.WriteData<Cardinal>(FFileData.nFileSizeLow);  // TODO: do we need to save size twice?
 // OStream.WriteData<uint64>(FFullFileSize); // require to store FFullFileSize for directories
  OStream.WriteData<Cardinal>(FLevel);
  OStream.WriteData<Boolean>(FDenied);
 // var lenBytes := Length(FFileData.cFileName) * sizeof(FFileData.cFileName[0]); // this is unlikely that only file name will be longer than 259 symbols
  var lenBytes := StrLen(FFileData.cFileName) * sizeof(FFileData.cFileName[0]);
  Assert(lenBytes < MAX_PATH * sizeof(FFileData.cFileName[0]));
  OStream.WriteData<Cardinal>(lenBytes);
  OStream.Write(FFileData.cFileName, lenBytes);
end;

procedure TCacheItem.Deserialize(IStream: TStream);
var
  lenBytes: Cardinal;
begin
  IStream.ReadData<Cardinal>(FParent);
  IStream.ReadData<Cardinal>(FFileData.dwFileAttributes);
  IStream.ReadData<Cardinal>(FFileData.ftCreationTime.dwHighDateTime);
  IStream.ReadData<Cardinal>(FFileData.ftCreationTime.dwLowDateTime);
  IStream.ReadData<Cardinal>(FFileData.ftLastAccessTime.dwHighDateTime);
  IStream.ReadData<Cardinal>(FFileData.ftLastAccessTime.dwLowDateTime);
  IStream.ReadData<Cardinal>(FFileData.ftLastWriteTime.dwHighDateTime);
  IStream.ReadData<Cardinal>(FFileData.ftLastWriteTime.dwLowDateTime);
  IStream.ReadData<Cardinal>(FFileData.nFileSizeHigh);
  IStream.ReadData<Cardinal>(FFileData.nFileSizeLow);
 // IStream.ReadData<uint64>(FFullFileSize);    // require to store FFullFileSize for directories
  FFullFileSize := MakeFileSize(FFileData.nFileSizeHigh, FFileData.nFileSizeLow);
  IStream.ReadData<Cardinal>(FLevel);
  IStream.ReadData<Boolean>(FDenied);

  IStream.ReadData<Cardinal>(lenBytes);
  Assert(lenBytes < MAX_PATH * sizeof(FFileData.cFileName[0]));
  IStream.Read(FFileData.cFileName, lenBytes);
  FUpperCaseName := AnsiUpperCase(FFileData.cFileName);
end;

////////////////////////////////////
// TFileNamesCache class methods
///////////////////////////////////

function TFileNamesCache.AddLevel(level: Cardinal): TLevelType;
begin
  Assert(level <= FCacheData.Count);

  if level < FCacheData.Count then begin
    Result := FCacheData[level];
  end
  else
  begin
    Result := TLevelType.Create;
    Result.SetCapacity(MAX_DIRS);
    FCacheData.AddValue(Result);
  end;
end;

procedure TFileNamesCache.AddProgressListener(listener: IIndexingProgress);
begin
  if NOT Assigned(listener) then exit;

  // check if listener has already added to the list
  if FProgressListeners.IndexOf(listener) = -1 then FProgressListeners.AddValue(listener);
end;

var
  ProgressCounter: Integer;  // TODO: think of moving it into inside ReadDirectory method

function TFileNamesCache.ReadDirectory(const currDir: TFileName; parent: TCacheItemRef; ShowProgress: Boolean): uint64;
const FIND_FIRST_EX_LARGE_FETCH = $00000002;
var
  dirSize: uint64;
  searchDir: string;
  fileData: TWin32FindData;
  hFind: THandle;
  tmp: LARGE_INTEGER;
begin
    Assert(sizeof(uint64) = 8);

    dirSize := 0;
    tmp.QuadPart := 0;
    searchDir := '\\?\' + currDir + '\*';  // add prefix to extend path string to 32767 symbols
    ZeroMemory(@fileData, sizeof(fileData));

    // this will be true only for the first ReadDirectory call in reccursion because for all other calls ShowProgress=false
    if ShowProgress then begin
      ProgressCounter := 0; // all reccursive calls are made with ShowProgress=False;
      NotifyStart;
    end;

    hFind := FindFirstFileEx(PChar(searchDir), FindExInfoBasic, @fileData, FindExSearchNameMatch, nil, FIND_FIRST_EX_LARGE_FETCH);
    //hFind := Windows.FindFirstFile(PChar(searchDir), fileData);

    if (hFind = INVALID_HANDLE_VALUE) then begin
      var err := GetLastError();
      if err = ERROR_ACCESS_DENIED then begin // print error message only if other than ERROR_ACCESS_DENIED error occurred
        NotifyError('Access denied: ' + currDir);
        var item := GetItem(parent);
        item.FDenied := True; // set flag that we cannot enter into this folder because of permission denied or other reason.
      end
      else
        MessageDlg('ERROR: ' + searchDir + ' GetLastError: ' + IntToStr(err), mtError, [mbOK], 0); //TODO: shall we raise and exception here or call NotifyError()?

      Result := dirSize;
      exit;
    end;

    Assert(fileData.cFileName[0] <> #0);

    // bypass dirs with names '.' and '..'
    if NOT IS_DOT_DIR(fileData.cFileName) then begin
      var itemRef := AddItem(parent.ItemIndex, fileData, parent.ItemLevel + 1);

      if IsDirectory(fileData) //(fileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0
        then dirSize := ReadDirectory(currDir + '\' + fileData.cFileName, itemRef, False)
        else dirSize := MakeFileSize(fileData.nFileSizeHigh, fileData.nFileSizeLow);
    end;

    while (True) do begin
      if ShowProgress then begin
        Inc(ProgressCounter);
        // if user pressed cancel then we raise an exception to be able to exit from all reccursive ReadDirectory calls.
        if NOT NotifyProgress(ProgressCounter) then raise EOperationCancelled.Create('User aborted.');
      end;

      if Windows.FindNextFile(hFind, fileData) then begin
        Assert(fileData.cFileName[0] <> #0);

        if IS_DOT_DIR(fileData.cFileName) then continue;

        var itemRef := AddItem(parent.ItemIndex, fileData, parent.ItemLevel + 1);

        if IsDirectory(fileData) //(fileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0
          then dirSize := dirSize + ReadDirectory(currDir + '\' + fileData.cFileName, itemRef, false)
          else dirSize := dirSize + MakeFileSize(fileData.nFileSizeHigh, fileData.nFileSizeLow);
      end
      else
      begin
        if (GetLastError() = ERROR_NO_MORE_FILES) then break;
        MessageDlg('Error in FindNextFile().', TMsgDlgType.mtError, [mbOK], 0);  //TODO: shall we raise and exception here or call NotifyError()?
        break;
      end
    end;

    var item := GetItem(parent);
    tmp.QuadPart := Int64(dirSize);
    item.FFileData.nFileSizeHigh := DWORD(tmp.HighPart);
    item.FFileData.nFileSizeLow := DWORD(tmp.LowPart);
    item.FFullFileSize := dirSize;

  //  FFindHandles.AddValue(hFind);
    Windows.FindClose(hFind);

    if ShowProgress then NotifyFinish;

    Result := dirSize;
end;

procedure TFileNamesCache.FillFileData(const filePath: string; var fileData: TWin32FindData);
var
  hf: THandle;
  fileSize: LARGE_INTEGER;
begin
  fileSize.QuadPart := 0;
  fileData.dwFileAttributes := Windows.GetFileAttributes(PChar(filePath));

  hf := Windows.CreateFile(PChar(filePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_BACKUP_SEMANTICS, 0);
  try
    if hf = INVALID_HANDLE_VALUE then begin
      MessageDlg(GetErrorMessageText(GetLastError(), 'Open file') + filePath, mtError, [mbOK], 0);
      exit;
    end;

    Windows.GetFileTime(hf, @fileData.ftCreationTime, @fileData.ftLastAccessTime, @fileData.ftLastWriteTime);
    Windows.GetFileSizeEx(hf, fileSize.QuadPart);
    fileData.nFileSizeHigh := fileSize.HighPart;
    fileData.nFileSizeLow := fileSize.LowPart;

  finally
    Windows.CloseHandle(hf);
  end;
end;

class procedure TFileNamesCache.FreeInst;
begin
  if Assigned(GInstance) then FreeAndNil(GInstance);
end;

procedure TFileNamesCache.Clear;
begin
  if FCacheData.Count = 0 then exit;

  for var i: Cardinal := 0 to FCacheData.Count - 1 do begin
    var lv := FCacheData[i];
    for var j: Cardinal := 0 to lv.Count - 1 do lv[j].Free;
    lv.Free;
  end;

  FCacheData.Clear;
end;

procedure TFileNamesCache.CloseFindHandles;
var i: Cardinal;
begin
  for i := 1 to FFindHandles.Count do Windows.FindClose(FFindHandles[i - 1]);
  FFindHandles.Clear;
end;

function TFileNamesCache.Count: Cardinal;
begin
  Result := 0;
  if FCacheData.Count = 0 then exit;

  for var i: Cardinal := 0 to FCacheData.Count - 1 do
    Result := Result + FCacheData[i].Count;
end;

constructor TFileNamesCache.Create;
begin
  raise ENoConstructException.Create('TFileNamesCache instance cannot be directly constructed');
end;

constructor TFileNamesCache.CreatePrivate;
begin
  FCacheData := THArrayG<TLevelType>.Create;
  FCacheData.SetCapacity(MAX_DIR_LEVELS);
  FProgressListeners := THArrayG<IIndexingProgress>.Create;
  FFindHandles :=  THArrayG<THandle>.Create;
  FFindHandles.SetCapacity(MAX_FIND_HANDLES_CAPACITY);
  FModified := False;
  FDateTimeIndexFile := 0; // default value, because index file is not loaded yet
end;

destructor TFileNamesCache.Destroy;
begin
  Clear;
  FreeAndNil(FCacheData);
  FreeAndNil(FProgressListeners);
  FreeAndNil(FFindHandles);
end;

/////////////////////////////////////
//  Auxiliary functions for Search
////////////////////////////////////

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForFileSize(var Filter: TSearchFilter; FileSize: uint64{; IsDir: Boolean}): Boolean;
//var
//  FilterFileSize: uint64;
begin
  //if IsDir
  //  then FilterFileSize := 0 // for directories assume that FileSize=0
  //  else FilterFileSize := Filter.FileSize;

  Result := False;
  case Filter.FileSizeCmpType of
    fscEquals: Result := FileSize = Filter.FileSize;
    fscMore:   Result := FileSize > Filter.FileSize;
    fscLess:   Result := FileSize < Filter.FileSize;
  end;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
// True if SearchStr is a substring of FileName
function CheckForFileName(var Filter: TSearchFilter; GrepList: TStringList; FileName, FileNameUpper: string): Boolean;
begin
  if GrepList = nil then begin
    if Filter.CaseSensitive
      then Result := Pos(Filter.SearchStr, FileName) > 0
      else Result := Pos(Filter.SearchStrUpper, FileNameUpper) > 0;
  end
  else
  begin
    if Filter.CaseSensitive
      then Result := cmpmask(FileName, GrepList)
      else Result := cmpmask(FileNameUpper, GrepList);
  end;
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForModifiedDate(var Filter: TSearchFilter; ModifiedDate: TFileTime): Boolean;
begin
  Result := (CompareFileTime(ModifiedDate, Filter.ModifiedDateFrom) >= 0) AND (CompareFileTime(ModifiedDate, Filter.ModifiedDateTo) <= 0);
end;

// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
function CheckForAttributes(var Filter: TSearchFilter; FileAttributes: Cardinal): Boolean;
begin
  Result := FileAttributes AND Filter.Attributes > 0;
end;

function IsDirectory(Item: TCacheItem): Boolean;
begin
  IsDirectory := Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY > 0;
end;

function IsDirectory(var FileData: TWin32FindData): Boolean;
begin
  IsDirectory := FileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY > 0;
end;


// Filter passed by reference intentionally to avoid unnessesary copy its data during function call
// if GrepList=nil use substr search otherwise use mask search functions
function ApplyFilter(var Filter: TSearchFilter; GrepList: TStringList; Item: TCacheItem): Boolean;
begin
  Result := False; //Result=False by default means 'not found'
  if Filter.SearchStr <> '' then
    if NOT CheckForFileName(Filter, GrepList, Item.FFileData.cFileName, item.FUpperCaseName) then exit;
  if Filter.SearchByFileSize then
    if NOT CheckForFileSize(Filter, Item.FFullFileSize{, IsDirectory(Item)}) then exit;
  if Filter.SearchByModifiedDate then
    if NOT CheckForModifiedDate(Filter, item.FFileData.ftLastWriteTime) then exit;
  if Filter.SearchByAttributes then
    if NOT CheckForAttributes(Filter, item.FFileData.dwFileAttributes) then exit;

  Result := True;
end;

procedure TFileNamesCache.Search(Filter: TSearchFilter; Callback: TFNCSearchResult);
var
  startArray: THArrayG<string>;
  GrepList: TStringList;
  i, j: Cardinal;
  Found: Boolean;
begin
  if FCacheData.Count = 0 then exit;
  Filter.SearchStrUpper := AnsiUpperCase(Filter.SearchStr);
  Found := False;

  startArray := THArrayG<string>.Create;
  GrepList := nil; // substr search by default
  try

    //check if filter str has wildcards
    if (Pos('*', Filter.SearchStr) > 0) OR (Pos('?', Filter.SearchStr) > 0 ) then begin
      GrepList := TStringList.Create;
      if Filter.CaseSensitive // compile filters into GrepList
        then SetFilters(Filter.SearchStr, GrepList)
        else SetFilters(Filter.SearchStrUpper, GrepList);
    end;

    StringToArray(Filter.StartFrom, startArray, '\');

    if StartArray.Count > 0 then begin
      for i := 0 to StartArray.Count - 1 do begin
        var lev := FCacheData[i];
        Found := False;
        for j := 0 to lev.Count - 1 do begin
          var ite := lev[j];
          if CompareText(ite.FUpperCaseName, startArray[i]) = 0 then begin
            Found := True;
            break;
          end;
        end;
        if NOT Found then break;  // found nothing
      end;

      if NOT Found then exit;  // if startDir not found in index DB then just exit
    end;

    for i := startArray.Count to FCacheData.Count - 1 do begin
      var lv := FCacheData[i];
      for j := 0 to lv.Count - 1 do begin
        var item := lv[j];
        if ApplyFilter(Filter, GrepList, item) then
          if NOT Callback(MakePathString(i, j), item) then exit;   //TODO: optimization: cache PathString in the item and use it during next searches
      end;
    end;
  finally
    startArray.Free;
    FreeAndNil(GrepList); // works even when GrepList=nil
  end;

end;

procedure TFileNamesCache.Serialize(OStream: TStream);
var
  i, j: Cardinal;
  tmpDate: TDateTime;
begin
    tmpDate := Now();
    OStream.WriteData<TDateTime>(tmpDate); // write datetime of latest index file update
    OStream.WriteData<Cardinal>(FCacheData.Count);
    for i := 0 to FCacheData.Count - 1 do begin
      var lv := FCacheData[i];
      OStream.WriteData<Cardinal>(lv.Count);
      for j := 0 to lv.Count - 1 do begin
        var item := lv[j];
        item.Serialize(OStream);
      end;
    end;
    FDateTimeIndexFile := tmpDate; // update index file date field after successfull saving
end;

procedure TFileNamesCache.Deserialize(IStream: TStream);
var
  i, j: Cardinal;
  cacheSize, levelSize: Cardinal;
  level: TLevelType;
  item: TCacheItem;
begin
    Clear;

    IStream.ReadData<TDateTime>(FDateTimeIndexFile);
    IStream.ReadData<Cardinal>(cacheSize);
    if cacheSize = 0 then exit;

    FCacheData.SetCapacity(cacheSize);

    for i := 0 to cacheSize - 1 do begin
      level := TLevelType.Create;
      FCacheData.AddValue(level);

      IStream.ReadData<Cardinal>(levelSize);
      level.SetCapacity(levelSize);
      Assert(levelSize > 0);

      for j := 0 to levelSize - 1 do begin
        item := TCacheItem.Create;
        item.Deserialize(IStream);
        level.AddValue(item);
      end;
    end;

    FModified := False; // loading index file does not mean "Modified". Modified=True after updating data from file system
end;

function TFileNamesCache.GetItem(Level, Index: Cardinal): TCacheItem;
begin
	Result := FCacheData[Level].GetValue(Index);
end;

function TFileNamesCache.GetLevelCount(Level: Cardinal): Cardinal;
begin
  Result := FCacheData[Level].Count;
end;

class function TFileNamesCache.Instance: TFileNamesCache;
begin
  if NOT Assigned(GInstance) then GInstance := TFileNamesCache.CreatePrivate;
  Result := GInstance;
end;

function TFileNamesCache.GetItem(itemRef: TCacheItemRef):	TCacheItem;
begin
  Result:= FCacheData.GetValue(itemRef.ItemLevel).GetValue(itemRef.ItemIndex);
end;

function TFileNamesCache.AddRootItem(var fileData: TWin32FindData): TCacheItemRef;
var
  level: TLevelType;
  i: Cardinal;
  item: TCacheItem;
  ref: TCacheItemRef;
begin
  level := AddLevel(0);

  i := 0;
  // for root item we always need to do search
  while i < level.Count do begin
    if level[i].FFileData.cFileName = fileData.cFileName then break;
    Inc(i);
  end;

  //auto iter = std::find_if(biter, eiter, [&fn](const TCacheItem& elem) -> bool { return _tcsnicmp(elem.FFileData.cFileName, fn, MAX_PATH) == 0; });

  if i < level.Count then begin
    item := level[i];
    Result := TCacheItemRef.Create(item.FLevel, i);
    exit;
  end;

  item := TCacheItem.Create(0, fileData, 0);
  level.AddValue(item);
  ref.ItemLevel := 0;
  ref.ItemIndex := level.Count - 1;
  Result := ref;
end;

function TFileNamesCache.AddItem(parent: Cardinal; var fileData: TWin32FindData; itemLevel: Cardinal; doSearch: Boolean = False): TCacheItemRef;
var
  item: TCacheItem;
begin
  var level := AddLevel(itemLevel);

  //if (doSearch) then begin
    //	auto biter = level.begin();
    //	auto eiter = level.end();
    //	ci_string itemName = fileData.cFileName;
    //	auto iter = std::find_if(biter, eiter, [&itemName, parent](const TCacheItem& elem) -> bool { return elem.FParent == parent && elem.FFileData.cFileName == itemName; });

    //	if (iter != eiter) // we've found an item
    //		return TCacheItemRef{ iter->FLevel, (size_t)std::distance(biter, iter) };
  //  end;

    item := TCacheItem.Create(parent, fileData, itemLevel);
    level.AddValue(item);
    Result.ItemLevel := itemLevel;
    Result.ItemIndex := level.Count - 1;
end;

function TFileNamesCache.AddFullPath(const path: string): TCacheItemRef;
var
  pathArray: THArrayG<string>;
  pathArrayAccum: THArrayG<string>;
  fileData:TWin32FindData;
  lv: Cardinal;
  parent: TCacheItemRef;
begin
  pathArray := THArrayG<string>.Create;
  pathArrayAccum := THArrayG<string>.Create;

  try
    StringToArray(path, pathArray, '\');
    StringToArrayAccum(path, pathArrayAccum, '\');

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

procedure TFileNamesCache.SerializeTo(const FileName: string);
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

procedure TFileNamesCache.DeserializeFrom(const FileName: string);
var
//  fin:TFileStream;
  msin: TMemoryStream;
begin
  //fin := TFileStream.Create(FileName, fmOpenRead);
  msin := TMemoryStream.Create;
  try
    if FileExists(FileName) then begin
      msin.LoadFromFile(FileName);
      Deserialize(msin);
    end;
  finally
    msin.Free;
  end;
end;

procedure TFileNamesCache.SaveTo(const FileName: string);
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
  path.SetCapacity(MAX_DIR_LEVELS);

  try
    for i := FCacheData.Count - 1 downto 0 do begin
      var level := FCacheData[i];

      for j := 0 to level.Count - 1 do begin
        sitem := level[j];

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

        pathStr := path[path.Count - 1].FFileData.cFileName;
        for k := Integer(path.Count) - 2 downto 0 do begin
          pathStr := pathStr + '\' +  path[k].FFileData.cFileName;
        end;

        fout.Write(PChar(pathStr)^, Length(pathStr)*sizeof(Char));
        fout.Write(PChar(sLineBreak)^, Length(sLineBreak)*sizeof(Char));
      end;
    end;

  finally
    fout.Free;
    path.Free;
  end;
end;

function TFileNamesCache.ReadFileSystem(const startDir: string): uint64;
begin
  Clear; // remove previous data
  var startItemRef := AddFullPath(startDir);

  Result := ReadDirectory(startDir, startItemRef, True);

  FModified := True;
end;

procedure TFileNamesCache.RemoveProgressListener(listener: IIndexingProgress);
begin
  var index := FProgressListeners.IndexOf(listener);
  if index <> -1 then FProgressListeners.DeleteValue(Cardinal(index));
end;

function TFileNamesCache.MakePathString(ref: TCacheItemRef):string;
begin
  Result := MakePathString(ref.ItemLevel, ref.ItemIndex);
end;

var
  GPath: THArrayG<PChar> = nil; // optimization, global array to hold path items before converting into full path string
  //GStrBuilder: TStringBuilder = nil;


function TFileNamesCache.MakePathString(itemLevel, itemIndex: Cardinal): string;
var
  k, ii: Integer;
begin
  GPath.Clear;
  //GStrBuilder.Clear;
  //GStrBuilder.Capacity := MAX_PATH;

  var item := GetItem(ItemLevel, ItemIndex);

  if itemLevel = 0 then begin  // we asked for root level item, return it and exit
    Result := item.FFileData.cFileName;
    exit;
  end;

  GPath.AddValue(item.FFileData.cFileName);

  var index := item.FParent;
  ii := Integer(ItemLevel - 1); // ii variable needs to be signed Integer

  while ii >= 0 do begin
    item := GetItem(Cardinal(ii), index);
    GPath.AddValue(item.FFileData.cFileName);
    index := item.FParent;
    Dec(ii);
  end;

  Result := GPath[GPath.Count - 1];
  //GStrBuilder.Append(GPath[GPath.Count - 1]);
  for k := Integer(GPath.Count) - 2 downto 0 do begin
    Result := Result + '\' + GPath[k];
    //GStrBuilder.Append('\');
    //GStrBuilder.Append(GPath[k]);
  end;

  //Result := GStrBuilder.ToString;
end;

procedure TFileNamesCache.NotifyFinish;
begin
  for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].Finish;
end;

function TFileNamesCache.NotifyProgress(prog: Integer): Boolean;
begin
  Result := False; // cancel by default
  for var i := 1 to FProgressListeners.Count do if NOT FProgressListeners[i - 1].Progress(prog) then exit;
  Result := True;  // everything is ok return true
end;

procedure TFileNamesCache.NotifyStart;
begin
  for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].Start(-1);
end;

procedure TFileNamesCache.NotifyError(ErrorStr: string);
begin
  for var i := 1 to FProgressListeners.Count do FProgressListeners[i - 1].ReportError(ErrorStr);
end;

procedure TFileNamesCache.PrintLevelsStat(list: TStrings);
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

procedure TFileNamesCache.PrintAllItems(list: TStrings);
begin
  for var i: Integer := Integer(FCacheData.Count) - 1 downto 0 do begin
    var level := FCacheData[i];
    var pathStr: string;

    for var j: Cardinal := 0 to level.Count - 1 do begin
      var sitem := level[j];

      if IsDirectory(sitem) {(sitem.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0} then begin
        pathStr := MakePathString(i, j);

        list.Add(Format('%s \t %u', [pathStr, sitem.FFullFileSize]));
        //"\t" << FileTimeToString(sitem.FFileData.ftLastWriteTime) << "\t" << FileTimeToString(sitem.FFileData.ftLastAccessTime) << std::endl;
      end;
    end;
  end;
 end;

function TFileNamesCache.GetStat(): TFileSystemStatRecord;
var
   i,j: Cardinal;
begin
  ZeroMemory(@Result, sizeof(Result));

  if FCacheData.Count = 0 then exit;

  //var totalItems: Cardinal := 0;
  var countedItems: Cardinal := 0;

  for i := 0 to FCacheData.Count - 1 do begin
    var lv := FCacheData[i];
    // totalItems := totalItems + lv.Count;
    for j := 0 to lv.Count - 1 do begin
      var item := lv[j];
      var counted: Boolean := false;

      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY)    > 0 then begin Inc(Result.Stat[ftDir]);       counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_ARCHIVE)      > 0 then begin Inc(Result.Stat[ftArchive]);   counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_READONLY)     > 0 then begin Inc(Result.Stat[ftReadOnly]);  counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_NORMAL)       > 0 then begin Inc(Result.Stat[ftFile]);      counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_HIDDEN)       > 0 then begin Inc(Result.Stat[ftHidden]);    counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_TEMPORARY)    > 0 then begin Inc(Result.Stat[ftTemp]);      counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_SYSTEM)       > 0 then begin Inc(Result.Stat[ftSystem]);    counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DEVICE)       > 0 then begin Inc(Result.Stat[ftDevice]);    counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_REPARSE_POINT)> 0 then begin Inc(Result.Stat[ftSymbolic]);  counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_COMPRESSED)   > 0 then begin Inc(Result.Stat[ftCompressed]);counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_ENCRYPTED)    > 0 then begin Inc(Result.Stat[ftEncrypted]); counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_OFFLINE)      > 0 then begin Inc(Result.Stat[ftOffline]);   counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_SPARSE_FILE)  > 0 then begin Inc(Result.Stat[ftSparse]);    counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_VIRTUAL)      > 0 then begin Inc(Result.Stat[ftVirtual]); counted := true; end;
      if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_NOT_CONTENT_INDEXED) > 0 then begin Inc(Result.Stat[ftNotIndexed]); counted := true; end;
      //if (item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_PINNED) > 0 then begin Inc(Result[ftPinned]); counted := true; end;
      if counted
        then Inc(countedItems)
        else raise Exception.Create('Uncounted file type encontered!'); //list.Add(Format('Missing file attribute : %s : %u', [item.FFileData.cFileName, item.FFileData.dwFileAttributes]));
    end;
  end;

  Result.Stat[ftAll] := countedItems;

  StatSort(Result);
end;

procedure TFileNamesCache.StatSort(var Stat: TFileSystemStatRecord);
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

procedure TFileNamesCache.PrintStat(stat: TFileSystemStat; list: TStrings);
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





procedure TTopFolders.UpdateMinMaxAndDelete();
var
  i, MinIndex: Cardinal;
  Item: TCacheItem;
begin
  if FItems.Count = 0 then exit;

  FMax := 0;
  FMin := FItems[0].FFullFileSize;
  MinIndex := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := FItems[i];
    if FMax < item.FFullFileSize then FMax := item.FFullFileSize;
    if FMin > item.FFullFileSize then begin
      FMin := item.FFullFileSize;
      MinIndex := i;
    end;
  end;

  FItems.DeleteValue(minIndex);
end;

function TTopFolders.CompareProc(Item1, Item2: TCacheItem): Integer;
begin
  if Item1.FFullFileSize > Item2.FFullFileSize then Result := 1
  else if Item1.FFullFileSize < Item2.FFullFileSize then Result := -1
  else Result := 0;
end;

constructor TTopFolders.Create(Num: Cardinal);
begin
  FItems := THarrayG<TCacheItem>.Create;
  FItems.SetCapacity(Num);
  FSize := Num;
end;

destructor TTopFolders.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TTopFolders.GetItem(Index: Cardinal): TCacheItem;
begin
  Result := FItems[Index];
end;

procedure TTopFolders.AddValue(Item: TCacheItem);
begin
  if FItems.Count < FSize then begin
    FItems.AddValue(Item);
    if FMax < item.FFullFileSize then FMax := item.FFullFileSize;
    if FMin > item.FFullFileSize then FMin := item.FFullFileSize;
  end else begin
    if Item.FFullFileSize > FMin then begin
      FItems.AddValue(Item);
      UpdateMinMaxAndDelete();
    end;
  end;
end;

procedure TTopFolders.BuildTopFolders();
var
  level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
begin
  var Cache: TFileNamesCache := TFSC.Instance;
  if Cache.Count = 0 then exit;

		//for i = (int)FCacheData.size() - 1; i >= 0; --i)
  for i := 0 to Cache.FCacheData.Count - 1 do begin
    Level := Cache.FCacheData[i];
    for j := 0 to Level.Count - 1 do begin
      Item := Level[j];
      if IsDirectory(Item) {(Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0} then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;

procedure TTopFolders.BuildTopFiles();
var
  level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
begin
  var Cache: TFileNamesCache := TFSC.Instance;
  if Cache.Count = 0 then exit;

  for i := 0 to Cache.FCacheData.Count - 1 do begin
    Level := Cache.FCacheData[i];
    for j := 0 to Level.Count - 1 do begin
      Item := Level[j];
      if NOT (IsDirectory(Item) {(Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) > 0)}
         OR ((Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DEVICE) > 0)) then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;


initialization
  GPath := THArrayG<PChar>.Create;
  GPath.SetCapacity(MAX_DIR_LEVELS);

finalization
  if GPath <> nil then FreeAndNil(GPath);
  TFSC.FreeInst; // free cache singlton
end.





