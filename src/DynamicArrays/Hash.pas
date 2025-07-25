unit Hash;

interface

uses DynamicArray, SortedArray;

resourcestring
  SKeyNotFound = 'Element is not found in Hash!';
  SCannotModifyReadOnly  = 'Cannot modify Read-only hash!';

{************************************************************************************************************
 *  THash
 * THash class stores pairs of values Key (K) (can be any type) and Value (V) (can be any type too)
 * Values can be accessed by Key or also by integer Index (like usual array).
 * Indexes of Values (not Keys) are not preserved and may change depending on which pairs Key:Value are stored in the hash
 * Comparator Cmp used to order Keys in THash for fater search (binary search used to find keys)
 ************************************************************************************************************}

 type
  THash<K; V> = class
  public type
   TKeysArray = THArraySorted<K>; //TODO: replace by SortedArray for speed? Delphi will generate Internal Compiler Error. Find how to avoid that
   TValuesArray = THArrayG<V>;
   PointerV = TValuesArray.PointerT; // just new name of existing type for better understanding
   Pair = record First: K; Second: V; end;
  protected
   FReadOnly: Boolean;
   FAIndexes: TKeysArray;
   FAValues : TValuesArray;
   function GetKey(Index: Cardinal): K;
   function GetCount: Cardinal;
  public
   constructor Create; virtual;
//   constructor CreateFromHArrays(IndexHArray:THArraySorted<K>; ValueHArray:THArrayG<V>);
   destructor Destroy; override;

   // resets Count to zero, do not release allocated memory
   // use this function if you planning to add elements to array after clearing
   procedure Clear; virtual;

   // sets Count to zero and frees all allocated memory
   procedure ClearMem; virtual;

   // raises an exception if has is read only
   procedure SetValue(Key: K; Value: V);

   // raises an exception if Key is not found in hash
   function GetValue(Key: K): V;

   // returns pointer to the Value associated with Key
   // returns nil if Key is not found
   // does not raise exception if Key is not found in hash
   function GetValuePointer(Key: K): PointerV;

   // Returns pair Key-Value by ordinal index
   // Ordinal indexes are not preserved during time
   // Usefull when you need to iterate through entire hash
   function GetPair(Index: Cardinal): Pair;

   // check if value with key Key exists in hash
   function IfExist(const Key: K): Boolean;

   // returns index of value with key Key or -1 if Key does not exist
   // returned index can then be used in GetPair call for example
   function IndexOf(const Key: K): Integer;

   // deletes value with key=Key, does nothing if Key is not found
   procedure Delete(const Key: K); virtual;

   property Count: Cardinal read GetCount;

   // access Key by ordial index.
   // this is one of the ways to iterate through hash
   property Keys[Index: Cardinal]: K read GetKey;
   property Values[Key: K]: V read GetValue write SetValue; default;

   // two properties: list of Keys and list of Values as an arrays
   property AIndexes: TKeysArray   read FAIndexes;
   property AValues:  TValuesArray read FAValues;
  end;

implementation

uses SysUtils;

constructor THash<K,V>.Create;
begin
  inherited Create;
  FReadOnly := False;
  FAIndexes := TKeysArray.Create;
  FAValues  := TValuesArray.Create;
end;

{
  constructor THash<K,V>.CreateFromHArrays(IndexHArray:THArraySorted<K>; ValueHArray:THArrayG<V>);
  begin
    inherited Create;
    FAIndexes := IndexHArray;
    FAValues  := ValueHArray;
    FReadOnly := True;
  end;
}

procedure THash<K,V>.Delete(const Key: K);
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0 then begin
    FAIndexes.DeleteValue(Cardinal(n));
    FAValues.DeleteValue(Cardinal(n));
  end;
end;

destructor THash<K,V>.Destroy;
begin
  if not FReadOnly then begin
    FAIndexes.Free;
    FAValues.Free;
  end;

  inherited Destroy;
end;

procedure THash<K;V>.Clear;
begin
  FAIndexes.Clear;
  FAValues.Clear;
end;

procedure THash<K,V>.ClearMem;
begin
  FAIndexes.ClearMem;
  FAValues.ClearMem;
end;

function THash<K,V>.GetCount:Cardinal;
begin
  Assert(FAIndexes.Count = FAValues.Count);
  Result := FAIndexes.Count;
end;

function THash<K,V>.GetKey(Index: Cardinal): K;
begin
  Result := FAIndexes[Index];
end;

function THash<K, V>.GetPair(Index: Cardinal): Pair;
begin
  Result.First := FAIndexes[Index];
  Result.Second := FAValues[Index];
end;

function THash<K,V>.GetValue(Key: K): V;
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0
    then Result := FAValues[Cardinal(n)]
    else raise ERangeError.Create(SKeyNotFound);
end;

function THash<K,V>.GetValuePointer(Key: K): PointerV;
var n: Integer;
begin
  n := FAIndexes.IndexOf(Key);
  if n >= 0
    then Result := FAValues.GetValuePointer(Cardinal(n))
    else Result := nil;
end;

function THash<K,V>.IfExist(const Key: K): Boolean;
begin
  Result := FAIndexes.IndexOf(Key) <> -1;
end;

function THash<K,V>.IndexOf(const Key: K): Integer;
begin
  Result := FAIndexes.IndexOf(Key);
end;

procedure THash<K,V>.SetValue(Key: K; Value: V);
var
  n: Integer;
  ind: Cardinal;
begin
  if FReadOnly then raise ERangeError.Create(SCannotModifyReadOnly);

  n := FAIndexes.IndexOf(Key);
  if n >= 0 then begin
    FAValues[Cardinal(n)] := Value;
  end else begin
   ind := FAIndexes.AddValue(Key);
   FAValues.InsertValue(ind, Value);
  end;
end;


end.