unit CacheItem;

interface

uses System.Classes, System.SysUtils, Windows;

type

 TCacheItemRef = record
    ItemLevel: Cardinal;
    ItemIndex: Cardinal;
    constructor Create(level, index: Cardinal);
 end;

 TCacheItem = class
 public
  // FVolume: TVolumeCache; // refers to big object root cache object for single volume
   FParent: Cardinal;
   FLevel: Cardinal;
   FFileAttrs: DWORD;
   FCreationTime: TFileTime;
   FLastAccessTime: TFileTime;
   FModifiedTime: TFileTime;
   FCreationTimeStr: string;
   FLastAccessTimeStr: string;
   FModifiedTimeStr: string;
   FFileSize: UInt64;
   FFileName: TFileName;
   FUpperCaseName: TFileName; // name of file/dir in upper case. need for search routines
   FDisplayName: string;
   FFileType: string;
   FPath: string; // full path to directory, for optimization filled only for items that were searched
   FIconIndex: Integer;
   FDenied: Boolean;

   constructor Create; overload;
   constructor Create(Parent, Level: Cardinal; var FileData: TWin32FindData); overload;
   procedure Assign(Other: TCacheItem);
   procedure Serialize(OStream: TStream);
   procedure Deserialize(IStream: TStream);
   function IsDirectory: Boolean;
   function IsReparsePoint: Boolean;
 end;

  TCacheItemExt = class(TCacheItem)
  public
    CacheItem: TCacheItem;
  end;

 function MakeFileSize(hi, lo: Cardinal) : UInt64; inline;


implementation

function MakeFileSize(hi, lo: Cardinal) : UInt64; inline;
begin
  Result := (UInt64(hi) shl 32) + UInt64(lo);
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
  //FVolume := nil;
  FParent := 0;
  FLevel := 0;
  FFileAttrs := 0;
  FCreationTime.dwLowDateTime := 0;
  FCreationTime.dwHighDateTime := 0;
  FLastAccessTime.dwLowDateTime := 0;
  FLastAccessTime.dwHighDateTime := 0;
  FModifiedTime.dwLowDateTime := 0;
  FModifiedTime.dwHighDateTime := 0;
  FFileSize := 0;
  FIconIndex := 0;
  FDenied := False;
end;

procedure TCacheItem.Assign(Other: TCacheItem);
begin
  //FVolume         := Other.FVolume;
  FParent         := Other.FParent;
  FLevel          := Other.FLevel;
  FFileAttrs      := Other.FFileAttrs;
  FCreationTime   := Other.FCreationTime;
  FLastAccessTime := Other.FLastAccessTime;
  FModifiedTime   := Other.FModifiedTime;
  FFileSize       := Other.FFileSize;
  FUpperCaseName  := Other.FUpperCaseName;
  FDisplayName    := Other.FDisplayName;
  FFileType       := Other.FFileType;
  FIconIndex      := Other.FIconIndex;
  FDenied         := Other.FDenied;
end;

constructor TCacheItem.Create(Parent, Level: Cardinal; var FileData: TWin32FindData);
begin
  Create(); // call default constructor to fill cache item with default values
  //FVolume := Volume;
  FParent := Parent;
  FLevel := Level;
  FFileName := FileData.cFileName;
  FDisplayName := FFileName;
  FFileAttrs := FileData.dwFileAttributes;
  FCreationTime   := FileData.ftCreationTime;
  FLastAccessTime := FileData.ftLastAccessTime;
  FModifiedTime  := FileData.ftLastWriteTime;
  FFileSize := MakeFileSize(FileData.nFileSizeHigh, FileData.nFileSizeLow);
  FUpperCaseName := AnsiUpperCase(FFileName);
end;

procedure TCacheItem.Serialize(OStream: TStream);
begin
  OStream.WriteData<Cardinal>(FParent);
  OStream.WriteData<Cardinal>(FFileAttrs);
  OStream.WriteData<TFileTime>(FCreationTime);
  OStream.WriteData<TFileTime>(FLastAccessTime);
  OStream.WriteData<TFileTime>(FModifiedTime);
  OStream.WriteData<UInt64>(FFileSize);
  OStream.WriteData<Cardinal>(FLevel);
  OStream.WriteData<Boolean>(FDenied);

  var lenBytes := ByteLength(FFileName); //StrLen(FFileData.cFileName) * sizeof(FFileData.cFileName[0]);
  Assert(lenBytes < MAX_PATH * sizeof(FFileName[1]));
  OStream.WriteData<Integer>(lenBytes);
  OStream.Write(FFileName[1], lenBytes);
end;

procedure TCacheItem.Deserialize(IStream: TStream);
var
  lenBytes: Cardinal;
begin
  IStream.ReadData<Cardinal>(FParent);
  IStream.ReadData<Cardinal>(FFileAttrs);
  IStream.ReadData<TFileTime>(FCreationTime);
  IStream.ReadData<TFileTime>(FLastAccessTime);
  IStream.ReadData<TFileTime>(FModifiedTime);
  IStream.ReadData<UInt64>(FFileSize);
  IStream.ReadData<Cardinal>(FLevel);
  IStream.ReadData<Boolean>(FDenied);

  IStream.ReadData<Cardinal>(lenBytes);
  Assert(lenBytes < MAX_PATH * sizeof(FFileName[1]));
  SetLength(FFileName, lenBytes div sizeof(FFileName[1])); // SetLength needs size in characters
  IStream.Read(FFileName[1], lenBytes);
  FUpperCaseName := AnsiUpperCase(FFileName);
  FDisplayName := FFileName;
end;

function TCacheItem.IsDirectory: Boolean;
begin
  IsDirectory := (FFileAttrs AND FILE_ATTRIBUTE_DIRECTORY) > 0;
end;

function TCacheItem.IsReparsePoint: Boolean;
begin
  IsReparsePoint := (FFileAttrs and FILE_ATTRIBUTE_REPARSE_POINT) > 0;
end;


end.
