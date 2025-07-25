unit DynamicArrays;

{
 Dynamic arrays and hashes for storing and manupulating with a various types of data.

 Arrays:
  THArray - Common array. Parent of all dynamic arrays.
  THArrayObjects,  THArrayByte,     THArraySmallInt, THArrayWord,      THArrayInt64,
  THArrayLongWord, THArrayInteger,  THArrayPointer,  THArrayBoolean,   THArrayDouble,
  THArrayCurrency, THArrayExtended, THArrayString,   THArrayStringFix, THArrayAnsiStringFix,
  THArrayWideStrings

 Hashes:
  THash - Common hash. Parent of all hashes.
  THashExists,   THashBoolean, THashInteger, THashPointer,
  THashCurrency, THashDouble,  THashString.

 Double Hashes:
  Like a table. Each value has two keys. Keys are always Integer values.
  See DynamicArrays.html for detail.
  THash2 - Common double hash. Parent of all double hashes.
  THash2Exists, THash2Currency, THash2Integer, THash2String.
}

interface

uses Classes, {Windows,} SysUtils;

 resourcestring
   SItemNotFound = 'Element with index %d not found !';
   SKeyNotFound  = 'Element with index%d not found in Read-only hash !';
   SNoCompareProc = 'Cannot sort without CompareProc!';
   SNoFindProc = 'Cannot do QuickFind without FindProc!';
   SWrongCallSetItemSize = 'Impossible to set item size for array contining defined types.';
   SUseCreateSizeConsructor = 'Constructor T*.Create() is prohibited, use T*.CreateSize(Size: Cardinal) instead.';

 type

  THarray = class;
  {Compare callback function. Return values must be:
   0 - elements are equal
   1 - arr[i] > arr[j]
  -1 - arr[i] < arr[j] }
  TCompareProc = function(arr : THArray; i,j : Cardinal) : Integer of object;
  {Find callback function.
   FindData - pointer to the seaching data. Seaching data can be int, float, string and any other type.
   Return values must be.
   0 - arr[i] = FindData as <needed type>
   1 - arr[i] > FindData as <needed type>
  -1 - arr[i] < FindData as <needed type>
   See example application how to use TFindProc.
  }
  TFindProc = function(arr : THArray; i : Cardinal; FindData: Pointer): Integer of object;
  TSwapProc = procedure(arr : THArray; i, j : Cardinal) of object;

(***********************************************************)
(*  Arrays                                                 *)
(***********************************************************)


  THArray = class  //common class of all dynamic arrays, does not depend on a type of stored data
  private
   FCount:    Cardinal;         // number of elements
   FCapacity: Cardinal;         // number of elements on which memory is allocated
   FItemSize: Cardinal;         // size of one element in bytes
   procedure SetItemSize(Size: Cardinal); virtual;
  protected
   FValues: Pointer;
   procedure Error(Value, MaxValue: Cardinal);
   function CalcAddr(Index: Cardinal): Pointer; virtual;
   procedure InternalQuickSort(CompareProc: TCompareProc; SwapProc: TSwapProc; L,R: Cardinal);
   function InternalQuickFind(FindProc: TFindProc; FindData: Pointer; L, R: Cardinal): Integer;

  public
   //type InnerType = Byte;
   constructor Create; virtual;
   destructor Destroy; override;
   procedure Clear; virtual;
   procedure ClearMem; virtual;
   function  Add(pValue: Pointer): Cardinal; virtual;
   procedure AddMany(pValue: Pointer; Cnt: Cardinal);
   function  AddFillValues(Cnt: Cardinal): Pointer;
   procedure Delete(Index: Cardinal); virtual;
   procedure Hold;
   procedure Get(Index: Cardinal; pValue: Pointer); virtual;
   function  GetAddr(Index: Cardinal): Pointer;
   procedure Grow;
   procedure GrowTo(Cnt: Cardinal);
   function  Insert(Index: Cardinal; pValue: Pointer): Cardinal; virtual;
   procedure InsertMany(StartIndex: Cardinal; pValue: Pointer; Cnt: Cardinal);
   function  IndexOf(Value: Pointer): Integer;
   function  IndexOfFrom(Value: Pointer; Start: Cardinal): Integer;
   procedure MoveData(FromPos, Cnt: Cardinal; Offset: Integer); virtual;
   procedure SetCapacity(Value: Cardinal);
   procedure Update(Index: Cardinal; pValue: Pointer); virtual; // fills Index value with zero if pValue=nil
   procedure UpdateMany(StartIndex: Cardinal; pValue: Pointer; Cnt: Cardinal);
   procedure Zero;
   procedure LoadFromStream(s: TStream); virtual; // read values will be added to existing ones
   procedure SaveToStream(s: TStream); virtual;
   function  ToBytes: TBytes;
   procedure Swap(Index1, Index2: Cardinal);virtual;
   procedure BubbleSort(CompareProc: TCompareProc);
   procedure Sort(CompareProc : TCompareProc);
   procedure QuickSort(CompareProc: TCompareProc; SwapProc: TSwapProc = nil);
   function  QuickFind(FindProc: TFindProc; FindData: Pointer): Integer; // Finds value in SORTED array!!

   property Capacity: Cardinal read FCapacity;
   property Count: Cardinal read FCount;
   property ItemSize: Cardinal read FItemSize write SetItemSize;
   property Memory: Pointer read FValues;
  end;

  THArrayObjects = class(THArray)
  protected
   function GetValue(Index: Cardinal): TObject;
   procedure SetValue(Index: Cardinal; const Value: TObject);
  public
   //type InnerType = TObject;
   constructor Create; override;
   procedure ClearMem; override;              // (!) destroyes all saved objects! and deletes all references on them.
   procedure SafeClearMem;                    // deletes only references on all stored objects. Objects are leave safe
   procedure Delete(Index: Cardinal); override; // (!) destroyes object with index Index and deletes reference on it.
   procedure SafeDelete(Index: Cardinal);       // deletes only reference on object with index Index. Object is left as is.
   function AddValue(Value: TObject): Cardinal;
   function IndexOf(Value: TObject): Integer;
   function IndexOfFrom(Value: TObject; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: TObject read GetValue write SetValue; default;
  end;

  THArrayByte = class(THArray)
  protected
   function GetValue(Index: Cardinal): Byte;
   procedure SetValue(Index: Cardinal; Value: Byte);
  public
   constructor Create; override;
   function AddValue(Value: Byte): Cardinal;
   function IndexOf(Value: Byte): Integer;
   function IndexOfFrom(Value: Byte; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: Byte read GetValue write SetValue; default;
  end;

  THArraySmallInt = class(THArray)
  private
   procedure SetItemSize(Size: Cardinal); override;
  protected
   function GetValue(Index: Cardinal): SmallInt;
   procedure SetValue(Index: Cardinal; Value: SmallInt);
  public
   constructor Create; override;
   function AddValue(Value: SmallInt): Cardinal;
   function IndexOf(Value: SmallInt): Integer;
   function IndexOfFrom(Value: SmallInt; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: SmallInt read GetValue write SetValue; default;
  end;

  THArrayWord = class(THArray)
  protected
   function GetValue(Index: Cardinal): Word;
   procedure SetValue(Index: Cardinal; Value: Word);
  public
   constructor Create; override;
   function AddValue(Value: word): Cardinal;
   function IndexOf(Value: word): Integer;
   function IndexOfFrom(Value: word; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: word read GetValue write SetValue; default;
  end;

  THArrayInt64 = class(THArray)
  protected
   function GetValue(Index: Cardinal): int64;
   procedure SetValue(Index: Cardinal; Value: int64);
  public
   //type InnerType = Int64;
   constructor Create; override;
   function AddValue(Value: int64): Cardinal;
   function IndexOf(Value: int64): Integer;
   function IndexOfFrom(Value: int64; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: int64 read GetValue write SetValue; default;
  end;

  THArrayUInt64 = class(THArray)
  protected
   function GetValue(Index: Cardinal): UInt64;
   procedure SetValue(Index: Cardinal; Value: UInt64);
  public
   constructor Create; override;
   function AddValue(Value: UInt64): Cardinal;
   function IndexOf(Value: UInt64): Integer;
   function IndexOfFrom(Value: UInt64; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: UInt64 read GetValue write SetValue; default;
  end;

  THArrayLongWord = class(THArray)
  protected
   function GetValue(Index: Cardinal): LongWord;
   procedure SetValue(Index: Cardinal; Value: LongWord);
  public
   constructor Create; override;
   function AddValue(Value: LongWord): Cardinal;
   function IndexOf(Value: LongWord): Integer;
   function IndexOfFrom(Value: LongWord; Start: Cardinal): Integer;
   property Value[Index: Cardinal]:LongWord read GetValue write SetValue; default;
  end;

  THArrayInteger = class(THArray)
  protected
   function GetValue(Index: Cardinal): Integer;
   procedure SetValue(Index: Cardinal; Value: Integer);
  public
   //type InnerType = Integer;
   constructor Create; override;
   function IndexOf(Value: Integer): Integer;
   function IndexOfFrom(Value: Integer; Start: Cardinal): Integer;
   function AddValue(Value: Integer): Cardinal;
   function InsertValue(Index: Cardinal; Value: Integer): Cardinal;
   function Pop: Integer;
   procedure Push(Value: Integer);
   property Value[Index: Cardinal]: Integer read GetValue write SetValue; default;
   function GetAsString: string;
   procedure AddFromString(InputString, Delimiters: string);
   function CalcMax: Integer;
//   procedure QuickSort(l,r:Integer);overload;
  end;

  THArrayPointer = class(THArray)
  protected
   function GetValue(Index: Cardinal): Pointer;
   procedure SetValue(Index: Cardinal; Value: Pointer);
  public
   constructor Create; override;
   function IndexOf(Value: Pointer): Integer;
   function IndexOfFrom(Value: Pointer; Start: Cardinal): Integer;
   function AddValue(Value: Pointer): Cardinal;
   property Value[Index: Cardinal]: Pointer read GetValue write SetValue; default;
  end;

  THArrayBoolean = class(THArray)
  protected
   function GetValue(Index: Cardinal): Boolean;
   procedure SetValue(Index: Cardinal; Value: Boolean);
  public
   constructor Create; override;
   function AddValue(Value: Boolean): Cardinal;
   function IndexOf(Value: Boolean): Integer;
   function IndexOfFrom(Value: Boolean; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: Boolean read GetValue write SetValue; default;
  end;

  THArrayDouble = class(THArray)
  protected
   function GetValue(Index: Cardinal): Double;
   procedure SetValue(Index: Cardinal; Value: Double);
  public
   constructor Create; override;
   function AddValue(Value: Double): Cardinal;
   function IndexOf(Value: Double): Integer;
   function IndexOfFrom(Value: Double; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: Double read GetValue write SetValue; default;
  end;

  THArrayCurrency = class(THArray)
  protected
   function GetValue(Index: Cardinal): Currency;
   procedure SetValue(Index: Cardinal; Value: Currency);
  public
   constructor Create; override;
   function AddValue(Value: Currency): Cardinal;
   function IndexOf(Value: Currency):Integer;
   function IndexOfFrom(Value: Currency; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: Currency read GetValue write SetValue; default;
  end;

  THArrayExtended = class(THArray)
  protected
   function GetValue(Index: Cardinal): Extended;
   procedure SetValue(Index: Cardinal; Value: Extended);
  public
   constructor Create; override;
   function AddValue(Value: Extended): Cardinal;
   function IndexOf(Value: Extended): Integer;
   function IndexOfFrom(Value: Extended; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: Extended read GetValue write SetValue; default;
  end;

  TWideString = class
   Str: WideString;
  public
   constructor Create(Value: WideString);
  end;

  THArrayWideStrings = class(THArrayObjects)
  protected
   function GetValue(Index: Cardinal): WideString;
   procedure SetValue(Index: Cardinal; Value: WideString);
  public
   function AddValue(Value: WideString): Cardinal;
   function IndexOf(Value: WideString): Integer;
   function IndexOfFrom(Value: WideString; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: WideString read GetValue write SetValue; default;
  end;

{  THArrayString_ = class(THArray)
  private
   str_ptr: THArrayPointer;
  protected
   function GetValue(Index: Cardinal): string;
   procedure SetValue(Index: Cardinal; Value: string);
   function CalcAddr(num: Cardinal): Pointer; override;
  public
   constructor Create; override;
   destructor Destroy; override;
   function AddValue(Value: string): Cardinal;
   function Add(pValue: Pointer): Cardinal; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(num: Cardinal); override;
   procedure Get(num:Cardinal; pValue: Pointer); override;
   function Insert(num: Cardinal; pValue: Pointer): Cardinal; override;
   function IndexOf(Value:string): Integer;
   function IndexOfFrom(Value: string; Start: Cardinal): Integer;
   procedure MoveData(FromPos, Cnt: Cardinal; Offset: Integer); override;
   procedure Swap(Index1, Index2: Cardinal); override;
   procedure Update(num: Cardinal; pValue: Pointer); override;
   property Value[Index: Cardinal]: string read GetValue write SetValue; default;
  end;
                 }
  THArrayString = class(THArrayPointer)
  private
   procedure ClearStrings;
   function DublicateStr(pValue: Pointer): PChar;
  protected
   function GetValue(Index: Cardinal): string;
   procedure SetValue(Index: Cardinal; Value: string);
  public
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   function Add(pValue: Pointer): Cardinal; override;
   function AddValue(Value: string): Cardinal;
   procedure Delete(num: Cardinal); override;
   function Insert(num: Cardinal; pValue: Pointer): Cardinal; overload; override;
   function Insert(num: Cardinal; Value: string): Cardinal; reintroduce; overload;
   procedure Update(num: Cardinal; pValue: Pointer); override;
   function IndexOf(Value: string): Integer;
   function IndexOfFrom(Value: string; Start: Integer): Integer;
   procedure LoadFromStream(s: TStream); override; // readed values will be added to existing
   procedure SaveToStream(s: TStream); override;
   property Value[Index: Cardinal]: string read GetValue write SetValue; default;
  end;

  THArrayAnsiStringFix = class(THArray)
  protected
   function GetValue(Index: Cardinal): AnsiString;
   procedure SetValue(Index: Cardinal; Value: AnsiString);
  public
   constructor Create; override;
   constructor CreateSize(Size: Cardinal);
   function AddValue(Value: AnsiString): Cardinal;
   function IndexOf(Value: AnsiString): Integer;
   function IndexOfFrom(Value: AnsiString; Start: Cardinal): Integer;
   property Value[Index:Cardinal]: AnsiString read GetValue write SetValue; default;
  end;

  THArrayStringFix = class(THArray)
  protected
   function GetValue(Index: Cardinal): string;
   procedure SetValue(Index: Cardinal; Value: string);
  public
   constructor Create; override;
   constructor CreateSize(SizeOfItem: Cardinal);
   function AddValue(Value: string): Cardinal;
   function IndexOf(Value: string): Integer;
   function IndexOfFrom(Value: string; Start: Cardinal): Integer;
   property Value[Index: Cardinal]: string read GetValue write SetValue; default;
  end;


(***********************************************************)
(*  Hashes                                                 *)
(*                                                         *)
(* Keys are always Integer type, Values may be any type    *)
(***********************************************************)
  THash = class
  private
   FReadOnly: Boolean;
   FAIndex: THArrayInteger;
   function GetKey(Index: Cardinal): Integer;
   function GetCount: Cardinal;
  public
   constructor Create; virtual;
   destructor Destroy; override;
   procedure Clear; virtual;
   procedure ClearMem; virtual;
   function IfExist(Key: Integer): Boolean;  // check if values with key Key is exists in hash
   procedure Delete(Key: Integer); virtual; abstract;// deletes value with key=Key
   property Count: Cardinal read GetCount;
   property Keys[Index: Cardinal]: Integer read GetKey;
   property AIndexes: THArrayInteger read FAIndex;
  end;

  THashExists = class (THash)
  private
   procedure SetValue(Key: Integer; Value: Boolean);
   function GetValue(Key: Integer): Boolean;
  public
   constructor Create; override;
   destructor Destroy; override;
   procedure Delete(Key: Integer); override;
   property Value[Key: Integer]: Boolean read GetValue write SetValue; default;
  end;

  THashBoolean = class (THash)
  private
   FAValues: THArrayBoolean;
   procedure SetValue(Key: Integer; Value: Boolean);
   function GetValue(Key: Integer): Boolean;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayBoolean);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key: Integer); override;
   property Value[Key: Integer]: Boolean read GetValue write SetValue; default;
  end;

  THashInteger = class (THash)
  private
   FAValues: THArrayInteger;
   procedure SetValue(Key: Integer; Value: Integer);
   function GetValue(Key: Integer): Integer;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayInteger);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key: Integer); override;
   property AValues: THArrayInteger read FAValues;
   property Value[Key: Integer]: Integer read GetValue write SetValue; default;
  end;

  THashPointer = class (THash)
  private
   FAValues: THArrayPointer;
   procedure SetValue(Key: Integer; Value: Pointer);
   function GetValue(Key: Integer): Pointer;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayPointer);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key: Integer); override;
   property AValues: THArrayPointer read FAValues;
   property Value[Index: Integer]: Pointer read GetValue write SetValue; default;
  end;

  THashCurrency = class (THash)
  private
   FAValues:THArrayCurrency;
   procedure SetValue(Key:Integer;Value:currency);
   function GetValue(Key:Integer):currency;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayCurrency);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:Integer); override;
   procedure Inc(Key:Integer;Value:currency); // increases stored value with key=Key on a Value. If value with key=Key does not exists then it will be created with value=Value.
   property Value[Index:Integer]:currency read GetValue write SetValue; default;
  end;

  THashDouble = class (THash)
  private
   FAValues:THArrayDouble;
   procedure SetValue(Key:Integer;Value:Double);
   function GetValue(Key:Integer):Double;
  public
   constructor Create; override;
   constructor CreateFromHArrays(IndexHArray:THArrayInteger;ValueHArray:THArrayDouble);
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key:Integer); override;
   procedure Inc(Key:Integer;Value:Double); // increases stored value with key=Key on a Value. If value with key=Key does not exists then it will be created with value=Value.
   property Value[Index:Integer]:Double read GetValue write SetValue; default;
  end;

  THashString = class (THash)
  private
   FAllowEmptyStr: Boolean;
   FAValues: TStrings;
   procedure SetValue(Key: Integer; Value: string);
   function GetValue(Key: Integer): string;
  public
   constructor Create; override;
   destructor Destroy; override;
   procedure Clear; override;
   procedure ClearMem; override;
   procedure Delete(Key: Integer); override;
   property AllowEmptyStr: Boolean read FAllowEmptyStr write FAllowEmptyStr;
   property Value[Index: Integer]: string read GetValue write SetValue; default;
  end;

  THash2 = class
  private
   MainListIndex: THArrayInteger;
   MainListValue: THArrayPointer;
//   function GetKey(Index:Integer):Integer;
   function GetChildHash(Key: Integer): THash;
  public
   constructor Create; virtual;
   destructor Destroy; override;
//   function Count:Integer;
   procedure Clear; virtual; abstract;  // Creares hash. Allocated memory do not frees.
   procedure ClearMem;                  // Cleares hash. Allocated memory frees too.
   procedure Delete(MainIndex, Index: Integer);
//   function ExistMainHash(MainIndex:Integer):boolean;
//   function ExistIndex(Index:Integer):boolean;
//   property Keys[Index:Integer]:Integer read GetKey;
   property MainIndexes: THArrayInteger read MainListIndex;
  end;

  THash2Exists = class (THash2)
  public
   procedure SetValue(MainIndex, Index: Integer; Value: Boolean); // creates new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex, Index: Integer): Boolean;        // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex: Integer): THashExists;
   function CreateHash(Index: Integer): THashExists;
//   procedure ExportChildHash(Hash:THashBoolean);
//   procedure DeleteMainIndex(MainIndex:Integer);
//   procedure DeleteIndex(Index:Integer);
  end;

  THash2Currency = class(THash2)
  public
   procedure SetValue(MainIndex, Index: Integer; Value: Currency); // creates new record with keys MainIndex, Index
   procedure Inc(MainIndex, Index: Integer; Value: Currency);      // increases exists/create new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex, Index: Integer): Currency;         // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex: Integer): THashCurrency;
   function CreateHash(Index: Integer): THashCurrency;
//   procedure ExportChildHash(Hash:THashCurrency);
  end;

  THash2Integer = class(THash2)
  public
   procedure SetValue(MainIndex, Index: Integer; Value: Integer); // creates new record with keys MainIndex, Index
   procedure Clear; override;
   function GetValue(MainIndex, Index: Integer): Integer;        // Gets Value by keys MainIndex, Index
   function CreateMainHash(MainIndex: Integer): THashInteger;
   function CreateHash(Index: Integer): THashInteger;
//   procedure ExportChildHash(Hash:THashInteger);
  end;

  THash2String = class(THash2)
  protected
   procedure SetValue(MainIndex, Index: Integer; Value: string); // creates new record with keys MainIndex, Index
   function GetValue(MainIndex, Index: Integer): string;        // Gets Value by keys MainIndex, Index
  public
   procedure Clear; override;
   function CreateMainHash(MainIndex: Integer): THashString;
   function CreateHash(Index: Integer): THashString;
//   procedure ExportChildHash(Hash:THashCurrency);
   property Value[MainIndex, Index: Integer]: string read GetValue write SetValue; default;
  end;

procedure memcpy(pi, po: Pointer; Count: Cardinal); stdcall;
procedure memclr(po: Pointer; Count: Cardinal); stdcall;
procedure memset(po: Pointer; Value: Byte; Count: Cardinal); stdcall;
function memfinddword(pi: Pointer; Value: Integer; Count: Cardinal): Integer; stdcall;
function memfindbyte(pi: Pointer; Value: Byte; Count: Cardinal): Integer; stdcall;
function memfindword(pi: Pointer; Value: Word; Count: Cardinal): Integer; stdcall;
function memfindint64(pi: Pointer; Value: int64; Count: Cardinal): Integer; stdcall;
function memfindgeneral(pi, pValue: Pointer; ValueSize: Cardinal; Count: Cardinal): Integer; stdcall;

function HGetToken(InputString: string; Delimiters: string; OnlyOneDelimiter: Boolean; Index: Integer): string;
function HGetTokenCount(InputString:string; Delimiters:string; OnlyOneDelimiter:boolean):Integer;

implementation

uses AnsiStrings;

//const
// BLOCK=1024;

function HGetToken(InputString: string; Delimiters: string; OnlyOneDelimiter: Boolean; Index: Integer): string;
var
  i,p: Integer;
  len: Integer;
begin
  Result := '';
  p := 1;
  len := length(InputString);
  while (p <= len) and (Pos(InputString[p], Delimiters) <> 0) do Inc(p); // bypass leading delimiters
  for i := 1 to index do begin
    while (p <= len) and (Pos(InputString[p], Delimiters) = 0) do Inc(p); // iterating till next delimiter
      if OnlyOneDelimiter
        then Inc(p)
        else while (p <= len) and (Pos(InputString[p],Delimiters) <> 0) do Inc(p);
  end;

  while (p <= len) and (Pos(InputString[p], Delimiters) = 0) do begin
    Result := Result + InputString[p];
    Inc(p);
  end;
end;

function HGetTokenCount(InputString: string; Delimiters: string; OnlyOneDelimiter: Boolean): Integer;
var p: Integer;
begin
  Result := 0;
  if InputString = '' then exit;
  p := 1;
  while (p <= length(InputString)) and (Pos(InputString[p], Delimiters) <> 0) do Inc(p);
  while (p <= length(InputString)) do begin
    while (p <= length(InputString)) and (Pos(InputString[p], Delimiters) = 0) do Inc(p);
    if OnlyOneDelimiter
      then Inc(p)
      else while (p <= length(InputString)) and (Pos(InputString[p], Delimiters) <> 0) do Inc(p);
   Result := Result + 1;
  end;

  //Result := Result;
end;

 {$IFDEF CPUX86}
 procedure memcpyfromend(pi, po: Pointer; Count: Cardinal); stdcall;
 asm
  pushad
  pushfd
  mov ECX, Count
  mov EDI,po
  mov ESI,pi
  add ESI,ECX
  add EDI,ECX
  dec ESI
  dec EDI
  std
  repne MOVSB
  popfd
  popad
 end;

procedure memcpyfrombegin(pi, po: Pointer; Count: Cardinal); stdcall;  // copying from begin
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  mov ESI,pi
  cld
  repne MOVSB
  popfd
  popad
end;

procedure memclr(po: Pointer; Count: Cardinal); stdcall;
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  xor AL,AL
  cld
  repne STOSB
  popfd
  popad
 end;

procedure memset(po: Pointer; Value: Byte; Count: Cardinal); stdcall;
 asm
  pushad
  pushfd
  mov ECX,Count
  mov EDI,po
  mov AL,Value
  cld
  repne STOSB
  popfd
  popad
 end;

function memfinddword(pi: Pointer; Value: Integer; Count: Cardinal): Integer; stdcall;
 asm
  pushad
  pushfd
  mov Result, 0
  mov ECX, Count
  cmp ECX, 0
  jz @ex
  mov EAX, Value
  mov EDI, pi
  cld
  repne SCASD
  jne @ex
  mov EAX, Count
  sub EAX, ECX
  mov Result, EAX
@ex:
  dec Result
  popfd
  popad
 end;

function memfindbyte(pi: Pointer; Value: Byte; Count: Cardinal): Integer; stdcall;
 asm
  pushad
  pushfd
  mov @Result, 0
  mov ECX, Count
  cmp ECX, 0
  jz @ex
  mov AL, Value
  mov EDI, pi
  cld
  repne SCASB
  jne @ex
  mov EAX, Count
  sub EAX, ECX
  mov @Result, EAX
@ex:
  dec @Result
  popfd
  popad
 end;

 function memfindword(pi: Pointer; Value: Word; Count: Cardinal): Integer; stdcall;
//label ex;
//begin
 asm
  pushad
  pushfd
  mov @Result, 0
  mov ECX, Count
  cmp ECX, 0
  jz @ex
  mov AX, Value
  mov EDI, pi
  cld
  repne SCASW
  jne @ex
  mov EAX, Count
  sub EAX, ECX
  mov @Result, EAX
@ex:
  dec @Result
  popfd
  popad
 end;
//end;

function memfindint64(pi: Pointer; Value: int64; Count: Cardinal): Integer; stdcall;
asm
  pushad
  pushfd
  mov @Result, 0
  mov ECX, Count
  cmp ECX, 0
  jz @ex
  mov EAX, dword ptr Value
  mov EBX, dword ptr (Value+4)
  mov EDI, pi
@loop:
  cmp EAX, [EDI]
  je @found1
  dec ECX
  jz @ex
  add EDI, 8     // go to next int 64 value
  jmp @loop
@found1:
  add EDI, 4     // go to next half of current int64 value
  cmp EBX, [EDI]
  je @found2
  dec ECX
  jz @ex
  add EDI, 4
  jmp @loop
@found2:
  mov EAX, Count
  sub EAX, ECX
  mov @Result, EAX
@ex:
  dec @Result
  popfd
  popad
end;

function memfindgeneral(
  pi: Pointer;        // start address for finding
  pValue: Pointer;    // pointer to the finding value
  ValueSize: Cardinal; // the size of finding value in bytes
  Count: Cardinal      // number of values in array
  ): Integer; stdcall;
 asm
  pushad
  pushfd
  mov @Result, 0
  mov EBX, Count
  cmp EBX, 0
  jz @ex
  mov EDI, pi
@loop:
  mov ESI, pValue
  mov ECX, ValueSize;
  cld
  repe CMPSB
  jz @ex1
  add EDI, ECX
  dec EBX
  jnz @loop
  jmp @ex
@ex1:
  dec EBX
  mov EAX, Count
  sub EAX, EBX
  mov @Result, EAX
@ex:
  dec @Result
  popfd
  popad
 end;
{$ENDIF CPUX86}

{$IFDEF CPUX64}
 // in 64bit mode order of parameters in procedure: RCX, RDX, R8, R9, stack... (differs from x86)
procedure memcpyfromend(pi, po: Pointer; Count: Cardinal);
asm
  push RDI
  push RSI
  push RCX
  mov RSI, RCX   // pi is in RCX
  mov RDI, RDX   // po is in RDX
  mov RCX, R8    // count is in R8
  add RSI, RCX
  add RDI, RCX
  dec RSI
  dec RDI
  std
  repne MOVSB
  pop RCX
  pop RSI
  pop RDI
end;

// in 64bit mode order of parameters in procedure: RCX, RDX, R8, R9, stack... (differs from x86)
procedure memcpyfrombegin(pi, po: Pointer; Count: Cardinal);  // copying from begin
asm
  push RDI
  push RSI
  push RCX
  mov RSI, RCX   // pi is in RCX
  mov RDI, RDX   // po is in RDX
  mov RCX, R8    // count is in R8
  cld
  repne MOVSB
  pop RCX
  pop RSI
  pop RDI
end;

// in 64bit mode order of parameters in procedure: RCX, RDX, R8, R9, stack... (differs from x86)
procedure memclr(po: Pointer; Count: Cardinal);
asm
  push RDI
  push RCX
  mov RDI, RCX
  mov RCX, RDX
  xor AL, AL
  cld
  repnz STOSB // repne STOSB
  pop RCX
  pop RDI
end;

procedure memset(po: Pointer; Value: Byte; Count: Cardinal);
asm
  push RDI
//  push RSI
  push RCX
  mov RDI, RCX
  mov RCX, R8
  mov AL, DL
  cld
  repne STOSB
  pop RCX
//  pop RSI
  pop RDI
 end;

 // Value parameter is always 32bit here and does not depend on 32 or 64bit platform
 // in 64bit mode order of parameters in procedure: RCX, RDX, R8, R9, stack... (differs from x86)
 // 64bit calling conversion does not depend on stdcall, fastcall and other convention words, they are ignored and take effect only for 32bit.
function memfinddword(pi: Pointer; Value: Integer; Count: Cardinal): Integer;
 asm
  push RAX
  push RDI
  //push RSI
  push RCX
  mov Result, 0
  cmp R8, 0       // Count is in R8, handle case where Count=0
  jz @ex
  mov RDI, RCX    // pi is in RCX
  mov RCX, R8
  mov EAX, EDX
  cld
  repne SCASD
  jne @ex
  mov RAX, R8    // original Count
  sub EAX, ECX
  mov Result, EAX
@ex:
  dec Result
  pop RCX
  //pop RSI
  pop RDI
  pop RAX
 end;


// Value parameter is always 32bit here and does not depend on 32 or 64bit platform
// in 64bit mode order of parameters in procedure: RCX, RDX, R8, R9, stack... (differs from x86)
// 64bit calling conversion does not depend on stdcall, fastcall and other convention words, they are ignored and take effect only for 32bit.
function memfindbyte(pi: Pointer; Value: Byte; Count: Cardinal): Integer;
asm
  push RAX
  push RDI
  push RSI
  push RCX
  mov Result, 0
  cmp R8, 0
  jz @ex
  mov RDI, RCX
  mov RCX, R8
  mov AL, DL
  cld
  repne SCASB
  jne @ex
  mov RAX, R8
  sub EAX, ECX
  mov Result, EAX //mov qword ptr Result,RAX
@ex:
  dec Result
  pop RCX
  pop RSI
  pop RDI
  pop RAX
end;

function memfindword(pi: Pointer; Value: word; Count: Cardinal): Integer;
asm
  push RAX
  push RDI
  push RSI
  push RCX
  mov Result, 0
  cmp R8, 0
  jz @ex
  mov RDI, RCX
  mov RCX, R8
  mov AX, DX
  cld
  repne SCASW
  jne @ex
  mov RAX, R8
  sub RAX, RCX
  mov Result, EAX
@ex:
  dec Result
  pop RCX
  pop RSI
  pop RDI
  pop RAX
 end;

function memfindint64(pi: Pointer; Value: int64; Count: Cardinal): Integer;
asm
  push RAX
  push RBX
  push RDI
  push RSI
  push RCX
  mov Result, 0
  cmp R8, 0
  jz @ex
  mov RDI, RCX
  mov RCX, R8
  mov RAX, RDX
  cld
  repne SCASQ
  jne @ex
  mov RAX, R8
  sub RAX, RCX
  mov Result, EAX

  {
  mov RBX, qword ptr (Value+4)
@loop:
  cmp RAX,[RDI]
  je @found1
  dec RCX
  jz @ex
  add RDI,8     // go to next int 64 value
  jmp @loop
@found1:
  add RDI,4     // go to next half of current int64 value
  cmp RBX,[RDI]
  je @found2
  dec RCX
  jz @ex
  add RDI,4
  jmp @loop
@found2:
  mov RAX,[ LARGE Count]
  sub RAX,RCX
  mov qword ptr Result,RAX}
@ex:
  dec Result
  pop RCX
  pop RSI
  pop RDI
  pop RBX
  pop RAX
end;


function memfindgeneral(
  pi: Pointer;        // start address for finding
  pValue: Pointer;    // pointer to the finding value
  ValueSize: Cardinal; // the size of finding value in bytes
  Count: Cardinal      // number of values in array
  ): Integer;
 asm
  push RAX
  push RBX
  push RDI
  push RSI
  push RCX
  mov Result, 0
  cmp R9, 0
  jz @ex
  mov RDI, RCX    // pi
  mov RBX, R9     // Count
@loop:
  mov RSI, RDX    // pValue
  mov RCX, R8;    // ValueSize
  cld
  repe CMPSB
  jz @ex1
  add RDI, RCX
  dec RBX
  jnz @loop
  jmp @ex
@ex1:
  dec RBX
  mov RAX, R9
  sub RAX, RBX
  mov Result, EAX
@ex:
  dec Result
  pop RCX
  pop RSI
  pop RDI
  pop RBX
  pop RAX
 end;
{$ENDIF CPUX64}

procedure memcpy(pi, po: Pointer; Count: Cardinal); stdcall;
begin
 if ((NativeInt(pi) + NativeInt(Count)) > NativeInt(po)) and (NativeInt(pi) < NativeInt(po))
 then memcpyfromend(pi, po, Count) // copy from end
 else memcpyfrombegin(pi, po, Count); //Move(PPointer(pi)^, PPointer(po)^, Count);

end;

 { THArray }
constructor THArray.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  FItemSize := 1;
  FValues := nil;
end;

destructor THArray.Destroy;
begin
  ClearMem;
  FItemSize := 0;
  inherited Destroy;
end;

procedure THArray.Delete(Index: Cardinal);
begin
  Error(Index, FCount);
  if Index < (FCount - 1) then memcpy(GetAddr(Index+1), GetAddr(Index), (FCount-Index-1)*FItemSize);
  Dec(FCount);
end;

procedure THArray.Clear;
begin
  FCount := 0;
end;

procedure THArray.ClearMem;
begin
  FCount := 0;
  FCapacity := 0;
  FreeMem(FValues);
  FValues := nil;
end;

function THArray.Add(pValue: Pointer): Cardinal;
begin
  Result := Insert(FCount, pValue);
end;

procedure THArray.AddMany(pValue: Pointer; Cnt: Cardinal);
begin
  //if Cnt <= 0 then exit;
  InsertMany(FCount, pValue, Cnt);
end;

procedure THarray.Hold;
// frees unused memory
begin
  SetCapacity(FCount);
end;

procedure THArray.SetCapacity(Value: Cardinal);
begin
  ReAllocMem(FValues, Value*FItemSize);
  FCapacity := Value;
  if FCount > FCapacity then FCount := FCapacity;
end;

function THArray.AddFillValues(Cnt: Cardinal): Pointer;
begin
  if FCount + Cnt > Capacity then GrowTo(FCount + Cnt);
  Result := CalcAddr(FCount);
  //FillChar(PByte(CalcAddr(FCount))^, Cnt * ItemSize, 0);
  memclr(Result, Cnt*FItemSize);
  FCount := FCount + Cnt;
end;

procedure THArray.Zero;
begin
  if FCount = 0 then exit;
  memclr(FValues, FCount*FItemSize);
  //FillChar(PByte(FValues)^, FCount * ItemSize, 0);
end;

procedure THArray.Grow;
// allocates memory for more number of elements by the next rules
//     the size of allocated memory increases on 25% if array has more than 64 elements
//     the size of allocated memory increases on 16 elements if array has from 8 to 64 elements
//     the size of allocated memory increases on 4 elements if array has less than 8 elements
var Delta: Cardinal;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16 else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure THArray.GrowTo(Cnt: Cardinal);
// increases size of allocated memory till Cnt elements (if count enough large) or
// to a number as described in Grow procedure
var Delta: Cardinal;
begin
  if Cnt <= FCapacity then exit;
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16 else Delta := 4;

  if (FCapacity + Delta) < Cnt then Delta := Cnt - FCapacity;
  SetCapacity(FCapacity + Delta);
end;

function THArray.Insert(Index: Cardinal; pValue: Pointer): Cardinal;
begin
  Error(Index, FCount + 1);
  Inc(FCount);
  if FCount >= FCapacity then Grow;
  memcpy(CalcAddr(Index), CalcAddr(Index + 1), (FCount - Index- 1 ) * FItemSize); // make place to insert
  Update(Index, pValue);
  Result := Index;
end;

procedure THArray.InsertMany(StartIndex: Cardinal; pValue: Pointer; Cnt: Cardinal);
begin
  if Cnt = 0 then exit; // nothing to do
  Error(StartIndex, FCount + 1);
  if FCount + Cnt > FCapacity then GrowTo(FCount + Cnt);
  FCount := FCount + Cnt;
  memcpy(CalcAddr(StartIndex), CalcAddr(StartIndex + Cnt), (FCount - StartIndex - Cnt) * FItemSize);  // make place to insert
  UpdateMany(StartIndex, pValue, Cnt);
end;

procedure THArray.Update(Index: Cardinal; pValue: Pointer);
begin
  Error(Index, FCount);
  if pValue = nil
    then memclr(GetAddr(Index), FItemSize)
    else memcpy(pValue, GetAddr(Index), FItemSize);
end;

//TODO: what if pValue=nil?
procedure THArray.UpdateMany(StartIndex: Cardinal; pValue: Pointer; Cnt: Cardinal);
begin
  if Cnt = 0 then exit;  // nothing to update
  Error(StartIndex + Cnt - 1, FCount);
  memcpy(pValue, GetAddr(StartIndex), FItemSize*Cnt);
end;

procedure THArray.Get(Index: Cardinal; pValue: Pointer);
begin
  memcpy(GetAddr(Index), pValue, FItemSize);
end;

function THArray.GetAddr(Index: Cardinal): Pointer;
begin
  Error(Index, FCount);
  Result := CalcAddr(Index);
end;

function THArray.CalcAddr(Index: Cardinal): Pointer;
begin
  Result := Pointer(NativeInt(FValues) + NativeInt(Index * FItemSize));
end;

procedure THArray.Error(Value, MaxValue: Cardinal);
begin
  if Value >= MaxValue then raise ERangeError.Create(Format(SItemNotFound, [Value]));
end;

// intentionally done: SetItemSize does not generate an exception when Size=0.
// need for Oracle components when they define ItemSize after creating THarray instance.
procedure THArray.SetItemSize(Size: Cardinal);
begin
  ClearMem;
  //TODO: is that ok that when Size=0 SetItemSize DOES NOT really change FItemSize?
  if (FCount = 0) and (Size > 0) then FItemSize := Size;
end;

procedure THArray.MoveData(FromPos, Cnt: Cardinal; Offset: Integer);
var mem: Pointer;
begin
  Assert(Integer(FromPos) + Offset > 0);

  Error(FromPos + Cnt, FCount);
  Error(Integer(FromPos) + Offset, FCount); // Offset can be negative number
  Error(Integer(FromPos + Cnt) + Offset, FCount);

  mem := AllocMem(Cnt * FItemSize);
  try
    memcpy(CalcAddr(FromPos), mem, Cnt * FItemSize);
    if Offset < 0 then memcpy(CalcAddr(Integer(FromPos) + Offset), CalcAddr(Integer(FromPos + Cnt) + Offset), Cardinal(-Offset) * FItemSize);
    if Offset > 0 then memcpy(CalcAddr(FromPos + Cnt), CalcAddr(FromPos), Cardinal(Offset) * FItemSize);
    memcpy(mem, CalcAddr(Integer(FromPos) + Offset), Cnt * FItemSize);
  finally
    FreeMem(mem);
  end;
end;

// this is actually Selection Sort algorithm
procedure THArray.Sort(CompareProc : TCompareProc);
var
  maxEl: Cardinal;
  i, j : Cardinal;
begin
  if FCount < 2 then exit;
  if @CompareProc = nil then raise EArgumentException.Create(SNoCompareProc);

  for i := FCount - 1 downto 1 do begin
    maxEl := i;
    j := 0;
    while j < i do begin
      if CompareProc(self, maxEl, j) < 0 then maxEl := j;
      Inc(j);
    end;

    if maxEl <> i then begin
      Swap(i, maxEl);
//      MoveData(i,1,maxEl-i);
//      MoveData(maxEl-1,1,i-maxEl+1);
    end;
  end;
end;

// Bubble sort
// Everybody knows it
procedure THArray.BubbleSort(CompareProc: TCompareProc);
var
  i, j  : Cardinal;
  WasSwap: Boolean;
begin
  if FCount < 2 then exit; // one or zero elements in array, no need to sort
  if @CompareProc = nil then raise EArgumentException.Create(SNoCompareProc);

  for i := 0 to FCount - 2 do begin
    WasSwap := False;
    for j := 0 to FCount - 2 - i do
      if CompareProc(self, j, j + 1) > 0 then begin
        Swap(j, j + 1);
        WasSwap := True;
      end;
    if NOT WasSwap then break; // there was no Swap in internal for. it means that whole array is sorted already
  end;
end;


procedure THArray.Swap(Index1, Index2: Cardinal);
var p: Pointer;
begin
  p := AllocMem(FItemSize);
  try
    memcpy(GetAddr(Index1), p, FItemSize);
    memcpy(GetAddr(Index2), GetAddr(Index1), FItemSize);
    memcpy(p,GetAddr(Index2), FItemSize);
  finally
    FreeMem(p);
  end;
end;

function THArray.ToBytes: TBytes;
begin
  if FCount = 0 then Exit;

  SetLength(Result, FCount*FItemSize);
  memcpy(FValues, @Result[0], FCount*FItemSize);
end;

procedure THArray.QuickSort(CompareProc: TCompareProc; SwapProc: TSwapProc);
begin
  if FCount < 2 then exit;
  InternalQuickSort(CompareProc, SwapProc, 0, FCount - 1);
end;

procedure THArray.InternalQuickSort(CompareProc: TCompareProc; SwapProc: TSwapProc; L, R: Cardinal);
var
  I,J: Cardinal;
  P: Cardinal;
begin
  if @CompareProc = nil then raise EArgumentException.Create(SNoCompareProc);

  i := L;
  j := R;
  P := (i + j) shr 1;
 //	repeat
  while i < j do begin
    while ((CompareProc(self, i, P) < 0) { and(I<=J) } ) do Inc(i);
    while ((CompareProc(self, j, P) > 0) { and(I<=J) } ) do Dec(j);

    if i <= j then begin
      if i = P then P := j // count a case when element with index P will be swapped and receive another index
      else if j = P then P := i;
      //Swap(i, j);
      if @SwapProc = nil then Swap(i, j) else SwapProc(self, i, j);
      Inc(i);

      //TODO: to avoid out of range exception then Dec(j) when j=0. I think there should be better solution to bypasss this situation
      // for example do not call InternalQuickSort when R-L=1 because sorting is trivial in a such interval
      // another solution might be to call InsertSort instead of QuickSort for intervals less then some value (e.g. 40)
      if j = 0 then break;
      Dec(j);
    end;
  end;
  //until i > j;
  if L < j then
    InternalQuickSort(CompareProc, SwapProc, L, j);
  if i < R then
    InternalQuickSort(CompareProc, SwapProc, i, R);
    {
  if @CompareProc=nil then exit;

    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while ((CompareProc(self,I,P) < 0){and(I<=J)}{) do Inc(I);
      while ((CompareProc(self,J,P) > 0){and(I<=J)}{) do Dec(J);
      if I <= J then begin
        if I = P then P := J
         else if J = P then P := I;
        if @SwapProc = nil
         then Swap(I,J)
         else SwapProc(self, I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then InternalQuickSort(CompareProc,SwapProc, L, J);
    if I < R then InternalQuickSort(CompareProc,SwapProc, I, R);
    }
end;

function THArray.QuickFind(FindProc: TFindProc; FindData: Pointer): Integer;
var
  L, R: Cardinal;
begin
  Result := -1; // 'not found' by default
  if FCount = 0 then exit;

  if @FindProc = nil then raise EArgumentException.Create(SNoFindProc);

  L := 0;
  R := FCount - 1;
  if FindProc(self, R, FindData) > 0 then begin // FindData is larger than last element in the array
    Result := -Integer(R + 2);
    exit;
  end;
  if FindProc(self, L, FindData) < 0 then begin // FindData is smaller than first element in the array
    Result := -1;
    exit;
  end;

  Result := InternalQuickFind(FindProc, FindData, L, R);
end;

function THArray.InternalQuickFind(FindProc: TFindProc; FindData: Pointer; L, R: Cardinal): Integer;
var
  middle: Cardinal;
  res: Integer;
begin
begin
  Result := -1; // may be default value not needed here

  if L = R then begin
    res := FindProc(self, L, FindData);
    if res < 0 then Result := -Integer(L + 1)
    else if res > 0 then Result := -Integer(L + 2)
    else Result := Integer(L);
  end
  else
  if R - L = 1 then begin
    res := FindProc(self, L, FindData);
    if res = 0 then Result := Integer(L)
    else if res < 0 then Result := -Integer(L + 1)
    else if res > 0 then begin
      res := FindProc(self, R, FindData);
      if res = 0 then Result := Integer(R)
      else if res < 0 then Result := -Integer(R + 1)
      else if res > 0 then Result := -Integer(R + 2)
    end;
  end
  else
  begin
    while True do begin
      middle := (L + R) div 2;
      res := FindProc(self, middle, FindData);
      if res < 0 then begin Result := InternalQuickFind(FindProc, FindData,  L, middle - 1); break; end  // searched element is on the left
      else if res > 0 then begin Result := InternalQuickFind(FindProc, FindData,  middle + 1, R); break; end  // searched elemnt is on the right
      else begin  // we've found element being searched
        if middle = L then break;
        Dec(middle);
        while (True) do // look for lowest index in case several elements =Value exist in the array
          if FindProc(self, middle, FindData) = 0 then begin
            if middle > L
              then Dec(middle)
              else break; // stop if we arrived to Start index
          end else begin
            Inc(middle);
            break;
          end;
        Result := Integer(middle);
        exit;
      end;
    end;
  end;
end;
{
  Result := -1;
  if FCount = 0 then exit;
  if @FindProc = nil then exit;
  L := 0;
  R := FCount - 1;
  if FindProc(self, R, FindData) < 0 then begin
    Result := -1;//R;
    exit;
  end;

  while True do begin
    was1 := abs(R-L) = 1;
    Result := (L+R) shr 1;
    if Integer(L) = Result then goto fin;//exit;
    res := FindProc(self, Result, FindData);
    if res < 0 then L := Result
     else if res > 0 then R := Result
      else goto fin;//exit;
    if was1 then goto fin;//exit;
  end;
fin: }
end;

procedure THArray.LoadFromStream(s: TStream);
var i, oc: Cardinal;
begin
  s.Read(i, sizeof(i));
  oc := FCount;
  AddFillValues(i);
  s.Read(CalcAddr(oc)^, i * FItemSize);
end;

procedure THArray.SaveToStream(s: TStream);
begin
  s.Write(FCount, sizeof(Integer));
  s.Write(PChar(FValues)^, FCount * FItemSize);
end;

function THArray.IndexOf(Value: Pointer): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArray.IndexOfFrom(Value: Pointer; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then Exit;
  if Value = nil then Exit;
 //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindgeneral(GetAddr(Start), Value, FItemSize, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayObjects }
function THArrayObjects.AddValue(Value: TObject): Cardinal;
begin
  Result := inherited Add(@Value);
end;

procedure THArrayObjects.ClearMem;
var i: Cardinal;
begin
  for i := 1 to FCount do GetValue(i - 1).Free;
  inherited;
end;

procedure THArrayObjects.SafeClearMem;
begin
  inherited ClearMem;
end;

constructor THArrayObjects.Create;
begin
  inherited;
  FItemSize := sizeof(TObject);
end;

procedure THArrayObjects.Delete(Index: Cardinal);
var o: TObject;
begin
  o := GetValue(Index);
  inherited;
  FreeAndNil(o); // Assigned(o) then o.Free;
end;

// SafeDelete does not free TObject only deletes its pointer from the array
procedure THArrayObjects.SafeDelete(Index: Cardinal);
begin
  inherited Delete(Index);
end;

function THArrayObjects.GetValue(Index: Cardinal): TObject;
begin
  Result := TObject(GetAddr(Index)^);
end;

procedure THArrayObjects.SetValue(Index: Cardinal; const Value: TObject);
begin
  Update(Index, @Value);
end;

function THArrayObjects.IndexOf(Value: TObject): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

// memfindgeneral because TObject can be either 4 bytes or 8 bytes long depending on Windows 32bit or 64bit
function THArrayObjects.IndexOfFrom(Value: TObject; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindgeneral(GetAddr(Start), @Value, sizeof(Value), FCount - Start); //memfinddword(GetAddr(Start), NativeUInt(Value), FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayByte }
function THArrayByte.AddValue(Value: Byte): Cardinal;
begin
  Result := inherited Add(@Value);
end;

constructor THArrayByte.Create;
begin
  inherited Create;
  FItemSize := sizeof(Byte);
end;

function THArrayByte.GetValue(Index: Cardinal): Byte;
begin
  Result := PByte(GetAddr(Index))^;
end;

function THArrayByte.IndexOf(Value: byte): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayByte.IndexOfFrom(Value: Byte; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindbyte(GetAddr(Start), Value, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

procedure THArrayByte.SetValue(Index: Cardinal; Value: Byte);
begin
  Update(Index,@Value);
end;

{ THArraySmallInt }
constructor THArraySmallInt.Create;
begin
  inherited Create;
  FItemSize := sizeof(Smallint);
end;

function THArraySmallInt.AddValue(Value:SmallInt): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArraySmallInt.GetValue(Index: Cardinal):SmallInt;
begin
  Result := PSmallint(GetAddr(Index))^;
end;

procedure THArraySmallInt.SetItemSize(Size: Cardinal);
begin
  raise ENotSupportedException.Create(SWrongCallSetItemSize);
end;

procedure THArraySmallInt.SetValue(Index: Cardinal; Value: SmallInt);
begin
  Update(Index, @Value);
end;

function THArraySmallInt.IndexOf(Value: SmallInt): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArraySmallInt.IndexOfFrom(Value: SmallInt; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
 //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindword(GetAddr(Start), word(Value), FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayWord }
constructor THArrayWord.Create;
begin
  inherited Create;
  FItemSize := sizeof(Word);
end;

function THArrayWord.AddValue(Value: Word): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayWord.GetValue(Index: Cardinal): Word;
begin
  Result := PWord(GetAddr(Index))^;
end;

procedure THArrayWord.SetValue(Index:Cardinal; Value: Word);
begin
  Update(Index,@Value);
end;

function THArrayWord.IndexOf(Value: word): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayWord.IndexOfFrom(Value: word; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindword(GetAddr(Start), Value, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayLongWord }
constructor THArrayLongWord.Create;
begin
  inherited Create;
  FItemSize := sizeof(LongWord);
end;

function THArrayLongWord.AddValue(Value: LongWord): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayLongWord.GetValue(Index: Cardinal): LongWord;
begin
  Result := PLongWord(GetAddr(Index))^;
end;

procedure THArrayLongWord.SetValue(Index: Cardinal; Value: LongWord);
begin
  Update(Index, @Value);
end;

function THArrayLongWord.IndexOf(Value: LongWord): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

//LongWord is unsigned and always 4 bytes long (on both 32bit and 64bit Windows platforms)
function THArrayLongWord.IndexOfFrom(Value: LongWord; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfinddword(GetAddr(Start), Integer(Value), FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

 { THArrayInt64 }
constructor THArrayInt64.Create;
begin
  inherited Create;
  FItemSize := sizeof(Int64);
end;

function THArrayInt64.AddValue(Value: Int64): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayInt64.GetValue(Index: Cardinal): Int64;
begin
  Result := PInt64(GetAddr(Index))^;
end;

procedure THArrayInt64.SetValue(Index: Cardinal; Value: Int64);
begin
  Update(Index, @Value);
end;

function THArrayInt64.IndexOf(Value: int64): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayInt64.IndexOfFrom(Value: int64; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindint64(GetAddr(Start), Value, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayUInt64 }

constructor THArrayUInt64.Create;
begin
  inherited Create;
  FItemSize := sizeof(UInt64);
end;

function THArrayUInt64.AddValue(Value: UInt64): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayUInt64.GetValue(Index: Cardinal): UInt64;
begin
  Result := PUInt64(GetAddr(Index))^;
end;

procedure THArrayUInt64.SetValue(Index: Cardinal; Value: UInt64);
begin
  Update(Index, @Value);
end;

function THArrayUInt64.IndexOf(Value: UInt64): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayUInt64.IndexOfFrom(Value: UInt64; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindint64(GetAddr(Start), Int64(Value), FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;


{ THArrayInteger }
constructor THArrayInteger.Create;
begin
  inherited Create;
  FItemSize := sizeof(Integer);
end;

function THArrayInteger.AddValue(Value: Integer): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayInteger.InsertValue(Index: Cardinal; Value: Integer): Cardinal;
begin
  Result := inherited Insert(Index, @Value);
end;

function THArrayInteger.IndexOf(Value: Integer): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayInteger.IndexOfFrom(Value: Integer; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues = nil
    then Result := -1
    else begin
      Result := memfinddword(GetAddr(Start), Value, FCount - Start);
      if Result <> -1 then Result := Result + Integer(Start);
    end;
end;

function THArrayInteger.GetValue(Index: Cardinal): Integer;
begin
  Result := PInteger(GetAddr(Index))^;
end;

procedure THArrayInteger.SetValue(Index: Cardinal; Value: Integer);
begin
  Update(Index, @Value);
end;

procedure THArrayInteger.Push(Value: Integer);
begin
  AddValue(Value);
end;

function THArrayInteger.Pop: Integer;
begin
  Result := Value[FCount - 1];
  Delete(FCount - 1);
end;

procedure THArrayInteger.AddFromString(InputString, Delimiters: string);
var i, c: Integer;
begin
  c := HGetTokenCount(InputString, Delimiters, False);
  for i := 0 to c - 1 do
    AddValue(StrToInt(HGetToken(InputString, Delimiters, False, i)));
end;

function THArrayInteger.GetAsString: string;
var i: Cardinal;
begin
  Result := ' ';
  for i := 1 to FCount do
    Result := Result + IntToStr(Value[i - 1]) + ' ';
end;

function THArrayInteger.CalcMax: Integer;
var i: Cardinal;
begin
  if FCount = 0 then exit(-1);
  Result := Value[0];
  for i := 1 to FCount - 1 do
    if Value[i] > Result then Result := Value[i];
end;

{procedure THArrayInteger.QuickSort(L,R:Integer);
 var
  I,J,P,temp: Integer;
begin
  I:=L;
  J:=R;
  p:=(L+R) shr 1;
  repeat
    while Value[I]<Value[P] do Inc(I);
    while Value[J]>Value[P] do Dec(J);
    if I <= J then
    begin
      temp:=Value[I];
      Value[I]:=Value[J];
      Value[I]:=temp;
      Inc(I);
      Dec(J);
    end;
  until I > J;
  if L<J then QuickSort(L,J);
  if I<R then QuickSort(I,R);
end;}

{ THArrayPointer }
constructor THArrayPointer.Create;
begin
  inherited Create;
  FItemSize := sizeof(Pointer);
end;

function THArrayPointer.AddValue(Value: Pointer): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayPointer.IndexOf(Value: Pointer): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayPointer.IndexOfFrom(Value: Pointer; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if FValues <> nil then begin
    Result := memfindgeneral(GetAddr(Start), @Value, sizeof(Value), FCount - Start); //memfinddword(GetAddr(Start), v, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

function THArrayPointer.GetValue(Index: Cardinal): Pointer;
begin
  Result := PPointer(GetAddr(Index))^;
end;

procedure THArrayPointer.SetValue(Index: Cardinal; Value: Pointer);
begin
  Update(Index, @Value);
end;

 { THArrayBoolean }
constructor THArrayBoolean.Create;
begin
  inherited Create;
  FItemSize := sizeof(Boolean);
end;

function THArrayBoolean.AddValue(Value: Boolean): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayBoolean.GetValue(Index: Cardinal): Boolean;
begin
  Result := PBoolean(GetAddr(Index))^;
end;

procedure THArrayBoolean.SetValue(Index: Cardinal; Value: Boolean);
begin
  Update(Index, @Value);
end;

function THArrayBoolean.IndexOf(Value: Boolean): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayBoolean.IndexOfFrom(Value: Boolean; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then begin
    Result := memfindbyte(GetAddr(Start), Byte(Value), FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayDouble }
constructor THArrayDouble.Create;
begin
  inherited Create;
  FItemSize := sizeof(Double);
end;

function THArrayDouble.AddValue(Value: Double): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayDouble.GetValue(Index: Cardinal): Double;
begin
  Result := PDouble(GetAddr(Index))^;
end;

procedure THArrayDouble.SetValue(Index: Cardinal; Value: Double);
begin
  Update(Index, @Value);
end;

function THArrayDouble.IndexOf(Value: Double): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayDouble.IndexOfFrom(Value: Double; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
 //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then begin
    Result := memfindgeneral(FValues, @Value, ItemSize, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayCurrency }
constructor THArrayCurrency.Create;
begin
  inherited Create;
  FItemSize := sizeof(Currency);
end;

function THArrayCurrency.AddValue(Value: Currency): Cardinal;
begin
  Result := inherited Add(@Value);
end;

function THArrayCurrency.GetValue(Index: Cardinal): Currency;
begin
  Result := PCurrency(GetAddr(Index))^;
end;

procedure THArrayCurrency.SetValue(Index: Cardinal; Value: Currency);
begin
  Update(Index, @Value);
end;

function THArrayCurrency.IndexOf(Value: Currency): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayCurrency.IndexOfFrom(Value: Currency; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then begin
    Result := memfindgeneral(FValues, @Value, ItemSize, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ THArrayExtended }
constructor THArrayExtended.Create;
begin
  inherited Create;
  FItemSize := sizeof(Extended);
end;

function THArrayExtended.GetValue(Index: Cardinal): Extended;
begin
  Result := PExtended(GetAddr(Index))^;
end;

function THArrayExtended.AddValue(Value: Extended): Cardinal;
begin
  Result := inherited Add(@Value);
end;

procedure THArrayExtended.SetValue(Index: Cardinal; Value: Extended);
begin
  Update(Index, @Value);
end;

function THArrayExtended.IndexOf(Value: Extended): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayExtended.IndexOfFrom(Value: Extended; Start: Cardinal): Integer;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then begin
    Result := memfindgeneral(FValues, @Value, ItemSize, FCount - Start);
    if Result <> -1 then Result := Result + Integer(Start);
  end;
end;

{ TWideString }
constructor TWideString.Create(Value: WideString);
begin
  Str := Value;
end;

{ THArrayWideStrings }
function THArrayWideStrings.AddValue(Value: WideString): Cardinal;
begin
  Result := inherited AddValue(TWideString.Create(Value));
end;

function THArrayWideStrings.GetValue(Index: Cardinal): WideString;
begin
  Result := TWideString(inherited GetValue(Index)).Str;
end;

function THArrayWideStrings.IndexOf(Value: WideString): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayWideStrings.IndexOfFrom(Value: WideString; Start: Cardinal): Integer;
var
  index: Cardinal;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then
    for index := Start to FCount - 1 do
      if GetValue(index) = Value then begin Result := Integer(index); exit; end;
  //Result := -1;
end;

procedure THArrayWideStrings.SetValue(Index: Cardinal; Value: WideString);
begin
  TWideString(inherited GetValue(Index)).Str := Value;
end;

{ THArrayString }  {
constructor THArrayString.Create;
begin
  str_ptr := THArrayPointer.Create;
  FCount := 0;
  FCapacity := 0;
  FItemSize := sizeof(Pointer); // because we store strings here as pointers to memory pieces allocated separately for each string
  FValues := nil;
end;

destructor THArrayString.Destroy;
var
  i    : Cardinal;
  pStr : PChar;
begin
  for i := 1 to str_ptr.Count do
  begin
    pStr := PChar(str_ptr.Value[i - 1]);
    StrDispose(pStr);
  end;
  str_ptr.Free;
end;

function THArrayString.CalcAddr(num: Cardinal): Pointer;
begin
  Result := Pointer(NativeInt(str_ptr.FValues) + NativeInt(num * FItemSize));
end;

function THArrayString.AddValue(Value: String): Cardinal;
begin
  Result := self.Add(PChar(Value));
end;

function THArrayString.Add(pValue: Pointer): Cardinal;
begin
  Result := Insert(FCount, pValue);
end;

function THArrayString.Insert(num: Cardinal; pValue: Pointer): Cardinal;
var
  pStr: PChar;
  l   : Cardinal;
begin
  l := StrLen(PChar(pValue)) * sizeof(Char); // size in bytes
  pStr := StrAlloc(l + 1);
  memcpy(pValue, pStr, l + 1);
  Result := str_ptr.Insert(num, @pStr);
  FCount := str_ptr.Count;
  FCapacity := str_ptr.Capacity;
 // Result := FCount;
end;

procedure THArrayString.Update(num: Cardinal; pValue: Pointer);
var
  pStr : PChar;
  l    : Cardinal;
begin
  pStr := PChar(str_ptr.Value[num]);
  if pStr <> nil then StrDispose(pStr);
  if pValue <> nil then begin
    l := StrLen(PChar(pValue)) * sizeof(Char);
    pStr := StrAlloc(l + 1);
    memcpy(pValue, pStr, l + 1);
    str_ptr.Value[num] := pStr;
  end else
    str_ptr.Value[num] := nil;
end;

procedure THArrayString.MoveData(FromPos, Cnt: Cardinal; Offset: Integer);
begin
  str_ptr.MoveData(FromPos, Cnt, Offset);
end;

procedure THArrayString.Delete(num: Cardinal);
var pStr: PChar;
begin
  pStr := PChar(str_ptr.Value[num]);
  StrDispose(pStr);
  str_ptr.Delete(num);
  FCount := str_ptr.Count;
end;

procedure THArrayString.Get(num: Cardinal; pValue: Pointer);
var
  pStr: PChar;
  l   : Cardinal;
begin
  pStr := PChar(str_ptr[num]);
  l := StrLen(pStr) * sizeof(Char);
  memcpy(Pointer(pStr), pValue, l + 1);
end;

function THArrayString.GetValue(Index: Cardinal): string;
var
  pStr : PChar;
begin
  pStr := PChar(str_ptr[Index]);
  Result := pStr;
end;

procedure THArrayString.SetValue(Index: Cardinal; Value: string);
begin
  Self.Update(Index, PChar(Value));
end;

procedure THArrayString.Clear;
var
  i: Cardinal;
  pStr: PChar;
begin
  for i := 1 to str_ptr.Count do begin
    pStr := PChar(str_ptr[i - 1]);
    StrDispose(pStr);
  end;
  str_ptr.Clear;
  inherited Clear;
//  FCount := str_ptr.Count;
//  FCapacity := str_ptr.Capacity;
end;

procedure THArrayString.ClearMem;
var
  i   : Cardinal;
  pStr: PChar;
begin
  for i := 1 to str_ptr.Count do begin
    pStr := PChar(str_ptr[i - 1]);
    StrDispose(pStr);
  end;
  str_ptr.ClearMem;
  inherited ClearMem;
end;

function THArrayString.IndexOf(Value: string): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayString.IndexOfFrom(Value: string; Start: Cardinal): Integer;
var Index: Cardinal;
begin
  Result := -1;
  if Start >= FCount then exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then
    for Index := Start to FCount - 1 do
      if self.Value[Index] = Value then begin Result := Integer(Index); exit; end;
  //Result := -1;
end;

procedure THArrayString.Swap(Index1, Index2: Cardinal);
begin
  str_ptr.Swap(Index1, Index2);
end;                }

{ THArrayAnsiStringFix }

function THArrayAnsiStringFix.AddValue(Value: AnsiString): Cardinal;
var
  buf: Pointer;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(buf^, FItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    AnsiStrings.StrPLCopy(buf, Value, FItemSize);
    Result := inherited Add(buf);
  finally
    FreeMem(buf);
  end;
end;

constructor THArrayAnsiStringFix.Create;
begin
  raise ENotSupportedException.Create(SUseCreateSizeConsructor);
end;

// It is possible to create THArrayAnsiStringFix with size 0. needed for Oracle fields
constructor THArrayAnsiStringFix.CreateSize(Size: Cardinal);
begin
  inherited Create;
  // if Size = 0 then raise EInvalidOpException.Create('Value of parameter Size should be greater than zero.');
  FItemSize := Size;
end;

function THArrayAnsiStringFix.GetValue(Index: Cardinal): AnsiString;
var
  buf: PAnsiChar;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(buf^, FItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    memcpy(GetAddr(Index), buf, FItemSize);
    Result := AnsiString(buf);
    //SetString(Result, buf, FItemSize);
    //SetLength(Result, FItemSize);
  finally
    FreeMem(buf);
  end;
end;

function THArrayAnsiStringFix.IndexOf(Value: AnsiString): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayAnsiStringFix.IndexOfFrom(Value: AnsiString; Start: Cardinal): Integer;
var Index: Cardinal;
begin
  Result := -1;
  if Start >= FCount then Exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then
    for Index := Start to FCount - 1 do
      if self.Value[Index] = Value then begin Result := Integer(Index); Exit; end;
  //Result := -1;
end;

procedure THArrayAnsiStringFix.SetValue(Index: Cardinal; Value: AnsiString);
var
  buf: Pointer;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(buf^, ItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    AnsiStrings.StrPLCopy(buf, Value, FItemSize);
    inherited Update(Index, buf);
  finally
    FreeMem(buf);
  end;
end;

{ THArrayStringFix }

function THArrayStringFix.AddValue(Value: string): Cardinal;
var buf: Pointer;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(buf^, FItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    StrPLCopy(buf, Value, FItemSize);
    Result := inherited Add(buf);
  finally
    FreeMem(buf);
  end;
end;

constructor THArrayStringFix.Create;
begin
  raise ENotSupportedException.Create(SUseCreateSizeConsructor);
end;

// SizeOfItem - in characters, not bytes !!!!
constructor THArrayStringFix.CreateSize(SizeofItem: Cardinal);
begin
  inherited Create;
  FItemSize := SizeofItem * sizeof(Char);  // remember string is UNICODE and sizeof(Char) may be equal 2
end;

function THArrayStringFix.GetValue(Index: Cardinal): string;
var buf: Pointer;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(PByte(buf)^, FItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    memcpy(GetAddr(Index), buf, FItemSize);
    Result := PChar(buf);
    //SetLength(Result, FItemSize);
  finally
    FreeMem(buf);
  end;
end;

function THArrayStringFix.IndexOf(Value: string): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayStringFix.IndexOfFrom(Value: string; Start: Cardinal): Integer;
var Index: Cardinal;
begin
  Result := -1;
  if Start >= FCount then Exit;
  //Error(Start, Integer(FCount) - 1);
  if Assigned(FValues) then
    for Index := Start to FCount - 1 do
      if self.Value[Index] = Value then begin Result := Integer(Index); Exit; end;
  //Result := -1;
end;

procedure THArrayStringFix.SetValue(Index: Cardinal; Value: string);
var buf: Pointer;
begin
  buf := AllocMem(FItemSize + 1);
  //FillChar(buf^, FItemSize + 1, 0);
  memclr(buf, FItemSize + 1);
  try
    StrPLCopy(buf, Value, FItemSize);
    inherited Update(Index, buf);
  finally
    FreeMem(buf);
  end;
end;

{ THash }
constructor THash.Create;
begin
  FReadOnly := False;
  FAIndex := THArrayInteger.Create;
end;

destructor THash.Destroy;
begin
  if not FReadOnly then FAIndex.Free;
  inherited Destroy;
end;

procedure THash.Clear;
begin
  FAIndex.Clear;
end;

procedure THash.ClearMem;
begin
  FAIndex.ClearMem;
end;

function THash.GetCount: Cardinal;
begin
  Result := FAIndex.Count;
end;

function THash.GetKey(Index: Cardinal): Integer;
begin
  Result := FAIndex[Index];
end;

function THash.IfExist(Key: Integer): Boolean;
begin
  Result := FAIndex.IndexOf(Key) <> -1;
end;

 { THashExists }
constructor THashExists.Create;
begin
  inherited Create;
end;

destructor THashExists.Destroy;
begin
  inherited Destroy;
end;

procedure THashExists.SetValue(Key: Integer; Value: Boolean);
var r: Integer;
begin
  r := FAIndex.IndexOf(Key);
  if (r = -1) and Value then FAIndex.AddValue(Key);
  if (r <> -1) and (not Value) then FAIndex.Delete(Cardinal(r));
end;

procedure THashExists.Delete(Key: Integer);
var r: Integer;
begin
  r := FAIndex.IndexOf(Key);
  if r <> -1 then FAIndex.Delete(Cardinal(r));
end;

function THashExists.GetValue(Key: Integer): Boolean;
var r: Integer;
begin
  r := FAIndex.IndexOf(Key);
  Result := (r<>-1);
end;

 { THashBoolean }
constructor THashBoolean.Create;
begin
  inherited Create;
  FAValues := THArrayBoolean.Create;
end;

constructor THashBoolean.CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayBoolean);
begin
  FAIndex   := IndexHArray;
  FAValues  := ValueHArray;
  FReadOnly := True;
end;

destructor THashBoolean.Destroy;
begin
  if not FReadOnly then  FAValues.Free;
  inherited Destroy;
end;

procedure THashBoolean.SetValue(Key: Integer; Value: Boolean);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
    exit;
  end;

  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
  FAIndex.AddValue(Key);
  FAValues.AddValue(Value);
end;

function THashBoolean.GetValue(Key: Integer): Boolean;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[Cardinal(n)];
  end else begin
    Result := False;
  end;
end;

procedure THashBoolean.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashBoolean.ClearMem;
begin
  inherited ClearMem;
  FAValues.ClearMem;
end;

procedure THashBoolean.Delete(Key: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(Cardinal(n));
  end;
end;

 { THashInteger }
constructor THashInteger.Create;
begin
  inherited Create;
  FAValues := THArrayInteger.Create;
end;

constructor THashInteger.CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayInteger);
begin
  FAIndex  := IndexHArray;
  FAValues := ValueHArray;
  FReadOnly:= True;
end;

destructor THashInteger.Destroy;
begin
  if not FReadOnly then  FAValues.Free;
  inherited Destroy;
end;

procedure THashInteger.SetValue(Key: Integer; Value: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
    exit;
  end;

  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound,[Key]));
  FAIndex.AddValue(Key);
  FAValues.AddValue(Value);
end;

function THashInteger.GetValue(Key: Integer): Integer;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[Cardinal(n)];
  end else begin
    Result := 0;
  end;
end;

procedure THashInteger.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashInteger.ClearMem;
begin
  inherited ClearMem;
  FAValues.ClearMem;
end;

procedure THashInteger.Delete(Key: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(Cardinal(n));
  end;
end;

 { THashPointer }
constructor THashPointer.Create;
begin
  inherited Create;
  FAValues := THArrayPointer.Create;
end;

constructor THashPointer.CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayPointer);
begin
  FAIndex := IndexHArray;
  FAValues := ValueHArray;
  FReadOnly := True;
end;

destructor THashPointer.Destroy;
begin
  if not FReadOnly then  FAValues.Free;
  inherited Destroy;
end;

procedure THashPointer.SetValue(Key: Integer; Value: Pointer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
    exit;
  end;

  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
  FAIndex.AddValue(Key);
  FAValues.AddValue(Value);
end;

function THashPointer.GetValue(Key: Integer): Pointer;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[Cardinal(n)];
  end else begin
    Result := nil;
  end;
end;

procedure THashPointer.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashPointer.ClearMem;
begin
  inherited ClearMem;
  FAValues.ClearMem;
end;

procedure THashPointer.Delete(Key: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(Cardinal(n));
  end;
end;

 { THashCurrency }
constructor THashCurrency.Create;
begin
  inherited Create;
  FAValues := THArrayCurrency.Create;
end;

constructor THashCurrency.CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayCurrency);
begin
  FAIndex := IndexHArray;
  FAValues := ValueHArray;
  FReadOnly := True;
end;

destructor THashCurrency.Destroy;
begin
  if not FReadOnly then FAValues.Free;
  inherited Destroy;
end;

procedure THashCurrency.SetValue(Key: Integer; Value: Currency);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
    exit;
  end;

  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
  FAIndex.AddValue(Key);
  FAValues.AddValue(Value);
end;

procedure THashCurrency.Inc(Key: Integer; Value: Currency);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := FAValues[Cardinal(n)] + Value;
  end else begin
    if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
    SetValue(Key, Value);
  end;
end;

function THashCurrency.GetValue(Key: Integer): Currency;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[Cardinal(n)];
  end else begin
    Result := 0;
  end;
end;

procedure THashCurrency.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashCurrency.ClearMem;
begin
  inherited ClearMem;
  FAValues.ClearMem;
end;

procedure THashCurrency.Delete(Key: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(Cardinal(n));
  end;
end;

 { THashDouble }
constructor THashDouble.Create;
begin
  inherited Create;
  FAValues := THArrayDouble.Create;
end;

constructor THashDouble.CreateFromHArrays(IndexHArray: THArrayInteger; ValueHArray: THArrayDouble);
begin
  FAIndex := IndexHArray;
  FAValues := ValueHArray;
  FReadOnly := True;
end;

destructor THashDouble.Destroy;
begin
  if not FReadOnly then FAValues.Free;
  inherited Destroy;
end;

procedure THashDouble.SetValue(Key: Integer; Value: Double);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
    exit;
  end;
  if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
  FAIndex.AddValue(Key);
  FAValues.AddValue(Value);
end;

procedure THashDouble.Inc(Key: Integer; Value: Double);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := FAValues[Cardinal(n)] + Value;
  end else begin
    if FReadOnly then raise ERangeError.Create(Format(SKeyNotFound, [Key]));
    SetValue(Key, Value);
  end;
end;

function THashDouble.GetValue(Key: Integer): Double;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[Cardinal(n)];
  end else begin
    Result := 0;
  end;
end;

procedure THashDouble.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashDouble.ClearMem;
begin
  inherited ClearMem;
  FAValues.ClearMem;
end;

procedure THashDouble.Delete(Key: Integer);
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(Cardinal(n));
  end;
end;

 { THashString }
constructor THashString.Create;
begin
  inherited Create;
  FAValues := TStringList.Create;
  FAllowEmptyStr := True;
end;

destructor THashString.Destroy;
begin
  FAValues.Free;
  inherited Destroy;
end;

procedure THashString.SetValue(Key: Integer; Value: string);
var
  n: Integer;
  n2: Cardinal;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    n2 := Cardinal(n);
    if not FAllowEmptyStr and (Value = '')
      then begin FAValues.Delete(n); FAIndex.Delete(n2); end
      else FAValues[n] := Value;
  end else
    if FAllowEmptyStr or (Value<>'') then begin
      FAIndex.AddValue(Key);
      FAValues.Add(Value);
    end;
end;

function THashString.GetValue(Key: Integer): string;
var n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    Result := FAValues[n];
  end else begin
    Result := '';
  end;
end;

procedure THashString.Clear;
begin
  inherited Clear;
  FAValues.Clear;
end;

procedure THashString.ClearMem;
begin
  inherited ClearMem;
  FAValues.Clear;
end;

procedure THashString.Delete(Key: Integer);
var
  n: Integer;
begin
  n := FAIndex.IndexOf(Key);
  if n >= 0 then begin
    FAIndex.Delete(Cardinal(n));
    FAValues.Delete(n);
  end;
end;

 { THash2 }
constructor THash2.Create;
begin
  MainListIndex := THArrayInteger.Create;
  MainListValue := THArrayPointer.Create;
end;

destructor THash2.Destroy;
begin
  Clear;
  MainListValue.Free;
  MainListIndex.Free;
  inherited Destroy;
end;

{function THash2.GetKey(Index:Integer):Integer;
begin
 Result:=MainListIndex[Index];
end;}

procedure THash2.ClearMem;
begin
  Clear;
  MainListValue.ClearMem;
  MainListIndex.ClearMem;
end;

function THash2.GetChildHash(Key: Integer): THash;
var n: Integer;
begin
  n := MainListIndex.IndexOf(Key);
  if n = -1
    then Result := nil
    else Result := MainListValue[Cardinal(n)];
end;

procedure THash2.Delete(MainIndex, Index: Integer);
var
  n: Integer;
  n2: Cardinal;
  arr: THashBoolean;
begin
  n := MainListIndex.IndexOf(MainIndex);
  if n = -1 then Exit;

  n2 := Cardinal(n);
  arr := MainListValue[n2];
  (arr as THash).Delete(Index);
  if arr.Count = 0 then begin
    arr.Free;
    MainListValue.Delete(n2);
    MainListIndex.Delete(n2);
  end;
end;

{function THash2.ExistMainHash(MainIndex:Integer):boolean;
var n:Integer;
begin
 n:=MainListIndex.IndexOf(MainIndex);
 Result:=n<>-1;
end;}

 { THash2Exists }
 procedure THash2Exists.Clear;
var i:  Cardinal;
begin
  for i := 1 to MainListValue.Count do begin
    THashExists(MainListValue[i - 1]).Free;
  end;
  MainListValue.Clear;
  MainListIndex.Clear;
end;

procedure THash2Exists.SetValue(MainIndex, Index: Integer; Value: Boolean);
var arr: THashExists;
begin
  arr := THashExists(GetChildHash(MainIndex));
  if arr = nil then begin
    arr := THashExists.Create;
    MainListIndex.AddValue(MainIndex);
    MainListValue.AddValue(arr);
  end;
  arr[Index] := Value;
end;

function THash2Exists.GetValue(MainIndex, Index: Integer): Boolean;
var arr: THashExists;
begin
  Result := False;
  arr := THashExists(GetChildHash(MainIndex));
  if arr = nil then exit;
  Result := arr[Index];
end;

function THash2Exists.CreateMainHash(MainIndex: Integer): THashExists;
var Co: Cardinal;
    n: Integer;
    arr: THashExists;
begin
  Result := nil;
  n := MainListIndex.IndexOf(MainIndex);
  if n = -1 then Exit;

  Result := THashExists.Create;
  arr := MainListValue[Cardinal(n)];
  Co := arr.Count;
  if Co > 0 then begin
    Result.FAIndex.SetCapacity(Co);
    Result.FAIndex.FCount := Co;
    memcpy(arr.FAIndex.FValues, Result.FAIndex.FValues, Co*Result.FAIndex.FItemSize);
  end else begin
    Result.Free;
    Result := nil;
  end;
end;

function THash2Exists.CreateHash(Index: Integer): THashExists;
var i: Cardinal;
begin
  Result := THashExists.Create;
  for i := 1 to MainListIndex.Count do begin
    if THashExists(MainListValue[i - 1])[Index] then Result.FAIndex.AddValue(MainListIndex[i - 1]);
  end;

  if Result.Count = 0 then begin
    Result.Free;
    Result := nil;
  end;
end;

 { THash2Currency }
procedure THash2Currency.Clear;
var i: Cardinal;
begin
  for i := 1 to MainListValue.Count do begin
    THashCurrency(MainListValue[i - 1]).Free;
  end;
  MainListValue.Clear;
  MainListIndex.Clear;
end;

procedure THash2Currency.SetValue(MainIndex, Index: Integer; Value: Currency);
var arr: THashCurrency;
begin
  arr := THashCurrency(GetChildHash(MainIndex));
  if arr = nil then begin
    arr := THashCurrency.Create;
    MainListIndex.AddValue(MainIndex);
    MainListValue.AddValue(arr);
  end;
  arr[Index] := Value;
end;

procedure THash2Currency.Inc(MainIndex, Index: Integer; Value: Currency);
var c: Currency;
begin
  c := GetValue(MainIndex, Index);
  SetValue(MainIndex, Index, Value + c);
end;

function THash2Currency.GetValue(MainIndex, Index: Integer): Currency;
var arr: THashCurrency;
begin
  Result := 0;
  arr := THashCurrency(GetChildHash(MainIndex));
  if arr = nil then exit;
  Result := arr[Index];
end;

function THash2Currency.CreateMainHash(MainIndex: Integer): THashCurrency;
var arr: THashCurrency;
    Co: Cardinal;
    n: Integer;
begin
  Result := nil;
  n := MainListIndex.IndexOf(MainIndex);
  if n = -1 then Exit;

  Result := THashCurrency.Create;
  arr := MainListValue[Cardinal(n)];
  Co := arr.Count;
  if Co > 0 then begin
    Result.FAIndex.SetCapacity(Co);
    Result.FAIndex.FCount := Co;
    Result.FAValues.SetCapacity(Co);
    Result.FAValues.FCount := Co;
    memcpy(arr.FAIndex.FValues,Result.FAIndex.FValues, Co*Result.FAIndex.FItemSize);
    memcpy(arr.FAValues.FValues,Result.FAValues.FValues, Co*Result.FAValues.FItemSize);
  end else begin
    Result.Free;
    Result := nil;
  end;
end;

function THash2Currency.CreateHash(Index: Integer): THashCurrency;
var i: Cardinal;
begin
  Result := THashCurrency.Create;
  for i := 1 to MainListIndex.Count do begin
    if THashCurrency(MainListValue[i - 1]).FAIndex.IndexOf(Index) <> -1 then begin
      Result.FAIndex.AddValue(i - 1);
      Result.FAValues.AddValue(THashCurrency(MainListValue[i - 1])[Index]);
    end;
  end;

  if Result.Count = 0 then begin
    Result.Free;
    Result := nil;
  end;
end;

 { THash2Integer }
procedure THash2Integer.Clear;
var i: Cardinal;
begin
  for i := 1 to MainListValue.Count do begin
    THashInteger(MainListValue[i - 1]).Free;
  end;
  MainListValue.Clear;
  MainListIndex.Clear;
end;

procedure THash2Integer.SetValue(MainIndex, Index: Integer; Value: Integer);
var arr: THashInteger;
begin
  arr := THashInteger(GetChildHash(MainIndex));
  if arr = nil then begin
    arr := THashInteger.Create;
    MainListIndex.AddValue(MainIndex);
    MainListValue.AddValue(arr);
  end;
  arr[Index] := Value;
end;

function THash2Integer.GetValue(MainIndex, Index: Integer): Integer;
var arr: THashInteger;
begin
  Result := 0;
  arr := THashInteger(GetChildHash(MainIndex));
  if arr = nil then exit;
  Result := arr[Index];
end;

function THash2Integer.CreateMainHash(MainIndex: Integer): THashInteger;
var arr: THashInteger;
    Co: Cardinal;
    n: Integer;
begin
  Result := nil;
  n := MainListIndex.IndexOf(MainIndex);
  if n = -1 then exit;
  Result := THashInteger.Create;
  arr := MainListValue[Cardinal(n)];
  Co := arr.Count;
  if Co > 0 then begin
    Result.FAIndex.SetCapacity(Co);
    Result.FAIndex.FCount := Co;
    Result.FAValues.SetCapacity(Co);
    Result.FAValues.FCount := Co;
    memcpy(arr.FAIndex.FValues,  Result.FAIndex.FValues,  Co*Result.FAIndex.FItemSize);
    memcpy(arr.FAValues.FValues, Result.FAValues.FValues, Co*Result.FAValues.FItemSize);
  end else begin
    Result.Free;
    Result := nil;
 end;
end;

function THash2Integer.CreateHash(Index: Integer): THashInteger;
var i: Cardinal;
begin
  Result := THashInteger.Create;
  for i := 1 to MainListIndex.Count do begin
    if THashInteger(MainListValue[i - 1]).FAIndex.IndexOf(Index) <> -1 then begin
      Result.FAIndex.AddValue(i - 1);
      Result.FAValues.AddValue(THashInteger(MainListValue[i - 1])[Index]);
    end;
  end;

  if Result.Count = 0 then begin
    Result.Free;
    Result := nil;
  end;
end;

 { THash2String }
procedure THash2String.Clear;
var i: Cardinal;
begin
  for i := 1 to MainListValue.Count do begin
    THashString(MainListValue[i - 1]).Free;
  end;
  MainListValue.Clear;
  MainListIndex.Clear;
end;

procedure THash2String.SetValue(MainIndex, Index: Integer; Value: string);
var arr: THashString;
begin
  arr := THashString(GetChildHash(MainIndex));
  if arr = nil then begin
    arr := THashString.Create;
    MainListIndex.AddValue(MainIndex);
    MainListValue.AddValue(arr);
  end;
  arr[Index] := Value;
end;

function THash2String.GetValue(MainIndex, Index: Integer): string;
var arr: THashString;
begin
  Result := '';
  arr := THashString(GetChildHash(MainIndex));
  if arr = nil then exit;
  Result := arr[Index];
end;

function THash2String.CreateMainHash(MainIndex: Integer): THashString;
var arr: THashString;
    Co, i: Cardinal;
    n: Integer;
begin
  Result := nil;
  n := MainListIndex.IndexOf(MainIndex);
  if n = -1 then exit;
  Result := THashString.Create;
  arr := MainListValue[Cardinal(n)];
  Co := arr.Count;
  if Co > 0 then begin
    Result.FAIndex.SetCapacity(Co);
    for i := 1 to arr.Count do begin
      Result[arr.Keys[i - 1]] := arr[arr.Keys[i - 1]];
    end;
  end else begin
    Result.Free;
    Result := nil;
  end;
end;

function THash2String.CreateHash(Index: Integer): THashString;
var i: Cardinal;
begin
  Result := THashString.Create;
  for i := 1 to MainListIndex.Count do begin
    if THashString(MainListValue[i - 1]).FAIndex.IndexOf(Index) <> -1 then begin
      Result.FAIndex.AddValue(i - 1);
      Result.FAValues.Add(THashString(MainListValue[i - 1])[Index]);
    end;
  end;

  if Result.Count = 0 then begin
    Result.Free;
    Result := nil;
  end;
end;

{ THArrayString }

function THArrayString.Add(pValue: Pointer): Cardinal;
var pStr: PChar;
begin
  pStr := DublicateStr(pValue);
  Result := inherited Add(@pStr);
end;

function THArrayString.AddValue(Value: string): Cardinal;
var pStr: PChar;
begin
  pStr := DublicateStr(PChar(Value));
  Result := inherited Add(@pStr);
end;

procedure THArrayString.Clear;
begin
  ClearStrings;
  inherited Clear;
end;

procedure THArrayString.ClearMem;
begin
  ClearStrings;
  inherited ClearMem;
end;

procedure THArrayString.ClearStrings;
var
  i: Cardinal;
  pStr: PChar;
begin
  for i := 1 to Count do begin
   Get(i - 1, pStr);
   StrDispose(pStr);
  end;
end;

procedure THArrayString.Delete(num: Cardinal);
var pStr:PChar;
begin
  Get(num, pStr);
  StrDispose(pStr);
  inherited Delete(num);
end;

destructor THArrayString.Destroy;
begin
  ClearStrings;
  inherited Destroy;
end;

function THArrayString.DublicateStr(pValue: Pointer): PChar;
var len: Cardinal;
begin
  if pValue <> nil then begin
    len := StrLen(PChar(pValue)) + 1;
    Result := StrAlloc(len);
    memcpy(pValue, Result, len* sizeof(Char)); // terminating zero also copied here
  end else
    Result := nil;
end;

function THArrayString.GetValue(Index: Cardinal): string;
begin
  Result := PChar(PPointer(GetAddr(Index))^);
end;

function THArrayString.IndexOf(Value: string): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayString.IndexOfFrom(Value: string; Start: Integer): Integer;
begin
  Result := -1; //TODO: implement this method
end;

function THArrayString.Insert(num: Cardinal; Value: string): Cardinal;
begin
  Result := Insert(num, Pointer(PChar(Value)));
end;

function THArrayString.Insert(num: Cardinal; pValue: Pointer): Cardinal;
var pStr: PChar;
begin
  pStr := DublicateStr(pValue);
  Result := inherited Insert(num, pStr);
end;

procedure THArrayString.LoadFromStream(s: TStream);

  function LoadString(Stream: TStream): PChar;
  var len: Cardinal;
  begin
    Stream.Read(len, sizeof(len));
    Result := StrAlloc(len div sizeof(Char)); // size in characters here
    Stream.Read(Result^, len);
  end;

var
  c, i: Integer;
  PStr: PChar;
begin
  s.Read(c, sizeof(Integer));
  for i := 1 to c do begin
    PStr := LoadString(s);
    inherited Add(@PStr);
  end;
end;

procedure THArrayString.SaveToStream(s: TStream);

  procedure SaveString(Stream: TStream; pValue: PChar);
  var len: Cardinal;
  begin
    len := (StrLen(pValue) + 1) * sizeof(Char); // len in bytes + one extra symbol (2 bytes) for terminating zero
    Stream.Write(len, sizeof(len));
    Stream.Write(pValue^, len);
  end;

var
  i: Cardinal;
  pStr: PChar;
begin
  i := Count;
  s.Write(i, sizeof(i)); // number of elements
  for i := 1 to Count do begin
    Get(i - 1, pStr);
    SaveString(s, pStr);
  end;
end;

procedure THArrayString.SetValue(Index: Cardinal; Value: string);
begin
  Update(Index, PChar(Value));
end;

procedure THArrayString.Update(num: Cardinal; pValue: Pointer);
var
  pStr : PChar;
 // l    : Integer;
begin
  Get(num, pStr);
  StrDispose(pStr);
  pStr := DublicateStr(pValue);
  inherited Update(num, pStr);
end;


end.

