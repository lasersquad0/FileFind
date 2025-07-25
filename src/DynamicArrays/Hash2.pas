unit Hash2;

interface

uses DynamicArray, Hash;

type

(************************************************************************************************************)
(*  THash2
(* THash2 can be considered as kind of table where values can be accessed by two keys (two indexes).
(* Keys can be any type, indexes can be any type too.
*)

  THash2<K1; K2; V> = class
  private type
   THashChildType = THash<K2, V>;
   THashValuesType = THash<K1, THashChildType>;
  public type
   PointerV = THashChildType.PointerV;
  private
   FValues: THashValuesType;
   function GetKey(Index: Cardinal): K1;
   function GetChildHash(Key: K1): THashChildType;
  public
   constructor Create; virtual;
   destructor Destroy; override;

   // Creares hash. Allocated memory does not free (remains allocated).
   procedure Clear; virtual;

   // Cleares hash. Allocated memory frees too.
   procedure ClearMem; virtual;

   // creates new record with keys Key1, Key2 and value Value
   procedure SetValue(Key1: K1; Key2: K2; Value: V); virtual;

   // Gets Value by keys Key1, Key2
   // raises an exception if Key1 and/or Keys does not exist in hash2
   function  GetValue(Key1: K1; Key2: K2): V; virtual;

    // returns nil if value does not exist
    // does not raise an exception when Key1 and/or Key2 is not found
   function  GetValuePointer(Key1: K1; Key2: K2): PointerV; virtual;
   //procedure Inc(Index1: I1; Index2: I2; Value: V);     // increases exists/create new record with keys MainIndex, Index

   // total number of values in the hash2
   function  CountTotal: Cardinal; overload;

   //number of values with K1=Key in the hash2
   function  Count(Key: K1): Cardinal; overload;

   // returns number of K1 keys in hash2
   function  Count: Cardinal; overload;

   // delete value associated with Key1 and Key3 in hash2
   // does not raise an exception when Key1 and/or Key2 is not found
   procedure Delete(Key1: K1; Key2: K2); overload; virtual;

   // deletes all values associated with Key1 regardless of Key2 values
   // does not raise an exception when Key1 is not found
   procedure Delete(Key1: K1); overload; virtual;

   // returns true when hash2 contains at least one Value associated with Key1
   function  IfExist(Key1: K1): Boolean; overload;

   // returns true when hash2 contains Value associated with Key1 and Key2
   function  IfExist(Key1: K1; Key2: K2): Boolean; overload;

   // get a Key by ordinal index.
   // this is the way to iterate through entire hash2
   property Keys[Index: Cardinal]: K1 read GetKey;
   property Value[Key1: K1; Key2: K2]: V read GetValue write SetValue; default;
  end;

implementation

uses SysUtils;

constructor THash2<K1,K2,V>.Create;
begin
  FValues := THashValuesType.Create;
end;

procedure THash2<K1,K2,V>.Clear;
var i: Cardinal;
begin
  if FValues.Count > 0 then
    for i := 0 to FValues.Count - 1 do FValues.AValues[i].Clear;
 FValues.Clear;
end;

procedure THash2<K1,K2,V>.ClearMem;
var i: Cardinal;
begin
  if FValues.Count > 0 then
    for i := 0 to FValues.Count - 1 do FValues.AValues[i].Free;
  FValues.ClearMem;
end;

procedure THash2<K1,K2,V>.Delete(Key1: K1);
begin
  FValues.Delete(Key1);
end;

destructor THash2<K1,K2,V>.Destroy;
var i: Cardinal;
begin
  for i := 1 to FValues.AValues.Count do FValues.AValues[i - 1].Free;
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure THash2<K1,K2,V>.Delete(Key1: K1; Key2: K2);
var
  n: Integer;
  h: THashChildType;
begin
  n := FValues.IndexOf(Key1);
  if n = -1 then Exit;   // nothing to delete

  h := FValues.AValues[Cardinal(n)];
  h.Delete(Key2);
  if h.Count = 0  then begin
    FValues.Delete(Key1);  {TODO: double indexof called, might desrease performance}
    h.free;
  end;
end;

function THash2<K1;K2;V>.GetKey(Index: Cardinal): K1;
begin
  Result := FValues.Keys[Index];
end;

function THash2<K1, K2, V>.Count: Cardinal;
begin
  Result := FValues.Count;
end;

function THash2<K1,K2,V>.Count(Key: K1): Cardinal;
var
  h: THashChildType;
begin
  Result := 0;
  h := GetChildHash(Key);
  if h <> nil then Result := h.Count;
end;

function THash2<K1,K2,V>.CountTotal: Cardinal;
var i: Cardinal;
begin
  Result := 0;
  if FValues.Count = 0 then exit;

  for i := 0 to FValues.Count - 1 do
    Result := Result + Count(FValues.AIndexes[i]); //TODO: shall we replace it to FValues.AValues[i].Count ?

end;

function THash2<K1,K2,V>.GetChildHash(Key: K1): THashChildType;
var n: Integer;
begin
  n := FValues.IndexOf(Key);
  if n = -1
    then Result := nil
    else Result := FValues.AValues[Cardinal(n)];
end;

function THash2<K1,K2,V>.IfExist(Key1: K1): Boolean;
begin
  Result := FValues.IndexOf(Key1) <> -1;
end;

function THash2<K1,K2,V>.IfExist(Key1: K1; Key2: K2): Boolean;
var h: THashChildType;
begin
  Result := False;
  h := GetChildHash(Key1);
  if h <> nil then begin
    Result := h.IfExist(Key2);
  end;
end;

procedure THash2<K1,K2,V>.SetValue(Key1: K1; Key2: K2; Value: V);
var h: THashChildType;
begin
  h := GetChildHash(Key1);
  if h = nil then begin
    h := THashChildType.Create;
    FValues.SetValue(Key1, h);
  end;
  h[Key2] := Value;
end;

{
  procedure THash2<I1;I2;V>.Inc(Index1: I1; Index2: I2; Value: V);
  var c: V;
  begin
   c := GetValue(Index1, Index2);
   SetValue(Index1, Index2, c + Value);
  end;
}

function THash2<K1,K2,V>.GetValue(Key1: K1; Key2: K2): V;
var h: THashChildType;
begin
  h := GetChildHash(Key1);
  if h = nil
    then raise ERangeError.Create(SKeyNotFound)
    else Result := h[Key2];
end;


function THash2<K1, K2, V>.GetValuePointer(Key1: K1; Key2: K2): PointerV;
var h: THashChildType;
begin
  Result := nil;
  h := GetChildHash(Key1);
  if Assigned(h) then Result := h.GetValuePointer(Key2);
end;

end.
