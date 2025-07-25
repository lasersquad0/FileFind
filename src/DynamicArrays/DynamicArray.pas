//PROFILE-NO
unit DynamicArray;

interface

uses Classes;

resourcestring
 SItemNotFound = 'Element with index %d not found !';
 //SKeyNotFound  = 'Element with index%d not found in Read-only hash !';
 //SCannotInsertIntoSortedArray = 'It is impossible to insert value into sorted array. Use AddValue() instead.';
 //SOperationNotSupportedBySortedArray = 'Operation is not supprted by sorted array.';


type

  // generic class of dynamic arrays, does not depend on a type of stored data
 THArrayG<T> = class
 public type

   {
   Compare callback functions. Return values must be:
   0 - elements are equal
   1 - arr[i] > arr[j]
  -1 - arr[i] < arr[j]
  }

  // this function is useful when you need access to other elements than arr[i], arr[j] for comparing
  // or you have quicker method of access ith and jth elements in the array
  TCompareProc = function(arr : THArrayG<T>; i, j: Cardinal): Integer of object;

  // standard compare function
  TCompareProc2 = function(item1, item2: T): Integer of object;
 // TSwapProc = procedure(arr : THArrayG<T>; i, j : Cardinal) of object;

  private type
   TInternalArrayType = array of T;
  private
   // number of elements for which memory is allocated
   FCapacity: Cardinal;
   // size in bytes of type T
   function GetItemSize(): Cardinal;
  protected
   // number of elements in dynamic array
   FCount: Cardinal;
   // internal array of values
   FValues: TInternalArrayType;
   procedure Error(Value, max: Cardinal);
   procedure InternalQuickSort(CompareProc: TCompareProc; L, R: Cardinal);
   procedure InternalInsertSort(CompareProc: TCompareProc2; L, R: Cardinal);
  public type
   PointerT = ^T;

  public
   /// <summary>Creates an empty array with Count and Capacity set to zero.</summary>
   constructor Create; overload; virtual;

   /// <summary>Creates an empty array and allocates memory for InitialCapacity elements</summary>
   /// <remarks>Call this method when you are going to add at least InitialCapacity elements to avoid memory re-allocations.</remarks>
   ///  <param name="InitialCapacity">Sets initial capacity for the array</param>
   constructor Create(InitialCapacity: Cardinal); overload; virtual;

   destructor Destroy; override;

   /// <summary>Sets Count to zero only, memory remains allocated.</summary>
   /// <remarks>Usefull when you are planning to add many elements to the array after cleaning</remarks>
   procedure Clear; virtual;

   /// <summary>Sets Count to zero AND deallocates all memory. Use this method if you are not planning to add many elements to array any more.</summary>
   procedure ClearMem; virtual;

   /// <summary>Returns extra memory back to the system.</summary>
   /// <remarks>Extra memory is part of memory that is not occupied by array elements. After calling this method Capacity = Count</remarks>
   procedure Hold;

   // Increase capacity to the next level (usually adding 25% to existing capacity)
   procedure Grow;

   // sets size of allocated memory till NewCapacity elements or to a number as calculated in Grow procedure which is bigger
   procedure GrowTo(NewCapacity: Cardinal);

   /// <summary>Allocates memory for Value elements and sets Capacity=Value. Existing elements are moved into new memory.</summary>
   /// <remarks>Capacity allows to reduce number of memory re-allocations when the array grows (new items being added)
   /// if Value < Count, number of elements in the array will be reduced to Value, Count becomes = Value.</remarks>
   procedure SetCapacity(Value: Cardinal);

   /// <summary>Copies all items from Other.</summary>
   procedure Assign(Other: TArray<T>); virtual;

   /// <summary>Adds Value in the end of the array. Array's Capacity is increased when needed.</summary>
   function AddValue(Value: T): Cardinal; virtual;

   /// <summary>Adds Cnt elements to the end of array and fills them with Value.</summary>
   /// <remarks>Array's Capacity is increased when needed.</remarks>
   procedure AddMany(Value: T; Cnt: Cardinal); virtual;
   // procedure AddFillValues(ACount:Cardinal);

   /// <summary>Deletes element with index=Index from the array.</summary>
   /// <remarks>All elements on the right from Index are shifted 1 position to the left.
   /// Count is decreased by 1.
   /// Capacity remains unchanged.</remarks>
   procedure DeleteValue(Index: Cardinal); virtual;

   /// <summary>Returns value with index Index</summary>
   function GetValue(Index: Cardinal): T; virtual;

   /// <summary>Returns pointer to the element in array with index Index.</summary>
   /// <remarks> Allows effective modification of elements directly in the array.<remarks/>
   function GetValuePointer(Index: Cardinal): PointerT; virtual;

   /// <summary>Sets value Value in the array with index Index</summary>
   procedure SetValue(Index: Cardinal; Value: T); virtual;

   /// <summary>Inserts value into the array at position Index. All elements on the right are shifted 1 position right.</summary>
   /// <remarks>Count is increased by 1. Capacity is increased if needed.</remarks>
   function InsertValue(Index: Cardinal; Value: T): Cardinal; virtual;

   /// <summary>Inserts Cnt values with value Value into the array starting at position Index. All elements on the right are shifted at Cnt positions right.</summary>
   /// <remarks>Count is increased by Cnt. Capacity is increased if needed.</remarks>
   procedure InsertMany(Index: Cardinal; Value: T; Cnt: Cardinal); virtual;

   /// <summary>Returns index of element Value in the array.</summary>
   /// <remarks>If element is not found index -1 is returned.</remarks>
   function IndexOf(Value: T): Integer; virtual;

   /// <summary>Returns index of element Value in the array. Search is started from position Start till end of array.</summary>
   /// <remarks>If element is not found -1 is returned.</remarks>
   function IndexOfFrom(Value: T; Start: Cardinal): Integer; virtual;
   //procedure MoveData(FromPos, Cnt:Cardinal; Offset: Integer);virtual;

   /// <summary>Updates Cnt elements in the array, sets them into value Value.</summary>
   /// <remarks>Array should contain at least Index+Cnt elements before calling UpdateMany.</remarks>
   procedure UpdateMany(Index: Cardinal; Value: T; Cnt: Cardinal); virtual;
  // procedure Zero;

   /// <summary>Reads values from TStream and adds them into the array.</summary>
   /// <remarks>Existing elements remain unchanged in array, new elements are added at the end of array.
   /// First read value is treated as count of elements to be read from TStream.</remarks>
   procedure LoadFromStream(s: TStream); virtual; // read values will be added to existing ones

   // Returns last element in array and deletes it from array
   function Pop: T; virtual;

   // Adds element Value to the end of the array. Just calls AddValue(Value).
   procedure Push(Value: T);

   /// <summary>Saves all array elements to the TStream.</summary>
   /// <remarks>All elements remain in array after saving.
   /// Method saves to TStream count of elements then saves elements one by one.</remarks>
   procedure SaveToStream(s: TStream); virtual;

   /// <summary>Swaps to elements in the array.</summary>
   /// <remarks>Element with Index1 moved to Index2. Element with Index2 moved to Index1</remarks>
   procedure Swap(Index1, Index2: Cardinal); virtual;

   /// <summary>Five sorting algorithms</summary>
   /// <remarks>Use any of them that fits your purposes.
   /// BubbleSort is the slowest one, added mostly for learning purposes.
   /// The fastest algorithm is QuickSort.</remarks>
   procedure BubbleSort(CompareProc: TCompareProc); virtual;
   procedure SelectionSort(CompareProc: TCompareProc); virtual;
   procedure QuickSort(CompareProc: TCompareProc); virtual;
   procedure InsertSort(CompareProc: TCompareProc2); virtual;
   procedure ShakerSort(CompareProc: TCompareProc2); virtual;

   /// <summary>Quickly finds elements in the array using binary search method.</summary>
   /// <remarks>*** Please note that the array MUST BE SORTED for proper work of this procedure. ****
   /// Use any available sorting method to sort array.
   /// For unsorted arrays search result will be undefined.</remarks>
   /// <returns>A positive value specifying index of found element in the array.
   /// If element is not found then it returns negative index absolute value of which represents index when missing element
   /// going to be inserted according to sorting. </returns>
   function QuickFind(CompareProc: TCompareProc2; FindWhat: T; Start: Cardinal = 0): Integer; virtual;
   function InternalQuickFind(CompareProc: TCompareProc2; FindWhat: T; L, R: Cardinal): Integer;

   /// <summary>Finds elements in the array.</summary>
   /// <remarks>It compares elements one by one with FindWhat value until successfull compare or end if array reached.
   /// Does not require array to be sorted in contrast to QuickFind method.</remarks>
   /// <returns>A positive value specifying index of found element in the array or -1 if nothing is found.</returns>
   function Find(CompareProc: TCompareProc2; FindWhat: T; Start: Cardinal = 0): Integer; virtual;

   /// <summary>Returns the current capacity.</summary>
   /// <returns>A positive number that specifies the number of elements that the array can hold before it
   /// needs to grow again.</returns>
   /// <remarks>The value of this method is greater or equal to the amount of elements in the array. If this value
   /// is greater then the number of elements, it means that the array has some extra capacity to operate upon.</remarks>
   property Capacity: Cardinal read FCapacity write SetCapacity;

   /// <summary>Returns the number of elements in the array.</summary>
   /// <returns>A positive value specifying the number of elements in the array.</returns>
   property Count: Cardinal read FCount;

   property Value[Index: Cardinal]: T read GetValue write SetValue; default;

   /// <summary>Returns size in bytes of the element in the array.</summary>
   /// <remarks>It might be usefull to know size of element stored in the array.</remarks>
   property ItemSize: Cardinal read GetItemSize;
  end;

   /// <summary>Splits string into substrings separated by delimiter.</summary>
   /// <remarks>All substrings are added into <c>arr<c> array. Substring do not contain delimiter symbols.
   /// <c>arr<a> array is not cleared before adding substrings.</remarks>
  procedure HGetTokens(InputString: string; Delims: string; OnlyOneDelim: Boolean; var arr: THArrayG<string>);


implementation

uses SysUtils, System.Generics.Defaults;

procedure HGetTokens(InputString: string; Delims: string; OnlyOneDelim: Boolean; var arr: THArrayG<string>);
var p, start: Integer;
    len: Integer;
begin
  p := 1;
  len := length(InputString);
  while (p <= len) and (pos(InputString[p], Delims) <> 0) do inc(p); // bypass leading delimiters
  //for i := 1 to index do begin
  while p <= len do begin
    start := p;
    while (p <= len) and (pos(InputString[p], Delims) = 0) do inc(p); // iterating till next delimiter
    arr.AddValue(Copy(InputString, start, p - start));
    if OnlyOneDelim
      then inc(p)
      else while (p <= len) and (pos(InputString[p], Delims) <> 0) do inc(p);
  end;
end;


 { THArrayG<T> }

constructor THArrayG<T>.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 0;
  FValues := nil;
end;

procedure THArrayG<T>.Clear;
begin
  FCount := 0;
end;

procedure THArrayG<T>.ClearMem;
begin
  FCount := 0;
  FCapacity := 0;
  FValues := nil; // automatically frees array's memory
end;

constructor THArrayG<T>.Create(InitialCapacity: Cardinal);
begin
  Create;
  SetCapacity(InitialCapacity);
end;

destructor THArrayG<T>.Destroy;
begin
  ClearMem;
  inherited Destroy;
end;

// Capacity stays the same here
procedure THArrayG<T>.DeleteValue(Index: Cardinal);
var
  i: Cardinal;
begin
  Error(Index, FCount);

  for i := Index + 1 to FCount - 1 do
    FValues[i-1] := FValues[i];
//  System.Delete(FValues, Index, 1);
  Dec(FCount);
 // FCapacity := Cardinal(Length(FValues));
end;

procedure THArrayG<T>.Hold; // frees unused memory
begin
  SetCapacity(FCount);
end;

procedure THArrayG<T>.SetCapacity(Value: Cardinal);
begin
  SetLength(FValues, Value);
  FCapacity := Value;
  if FCount > FCapacity then FCount := FCapacity;
end;

function THArrayG<T>.AddValue(Value: T): Cardinal;
begin
  if FCount + 1 >= FCapacity then Grow;
  FValues[FCount] := Value;
  Result := FCount;
  Inc(FCount);
end;

procedure THArrayG<T>.Assign(Other: TArray<T>);
begin
  FValues := Other;
  FCount := Cardinal(Length(Other));
  FCapacity := FCount;
end;

procedure THArrayG<T>.AddMany(Value: T; Cnt: Cardinal);
var
  i: Cardinal;
begin
  if Cnt = 0 then exit;
  if FCount + Cnt > FCapacity then GrowTo(FCount + Cnt);
  for i := FCount to FCount + Cnt - 1 do
      FValues[i] := Value;
  FCount := FCount + Cnt;
end;
 {
procedure THArrayG<T>.Zero;
var zero:T;
    i: Integer;
begin
 if FCount = 0 then exit;
 zero := T.Create();
 for i := 0 to FCount - 1 do
      FValues[i] := zero;
end;
  }
procedure THArrayG<T>.Grow;
// allocates memory for more number of elements by the next rules
//     the size of allocated memory increases on 25% if array has more than 64 elements
//     the size of allocated memory increases on 16 elements if array has from 8 to 64 elements
//     the size of allocated memory increases on 4 elements if array has less than 8 elements
var Delta: Cardinal;
begin
  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure THArrayG<T>.GrowTo(NewCapacity: Cardinal);
// increases size of allocated memory till NewCapacity elements (if NewCapacity enough large) or
// to a number as described in Grow procedure
var Delta: Cardinal;
begin
  if NewCapacity <= FCapacity then exit;

  if FCapacity > 64 then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16
  else Delta := 4;

  if (FCapacity + Delta) < NewCapacity then Delta := NewCapacity - FCapacity;
  SetCapacity(FCapacity + Delta);
end;

function THArrayG<T>.InsertValue(Index: Cardinal; Value: T): Cardinal;
var
  i: Cardinal;
begin
  Error(Index, FCount + 1);
  if FCount + 1 >= FCapacity then Grow;
  for i := FCount downto Index + 1 do
    FValues[i] := FValues[i - 1];
  FValues[Index] := Value;
  Inc(FCount);
  FCapacity := Cardinal(Length(FValues)); // real capacity has changed by Insert call, update FCapacity field
  //memcpy(CalcAddr(num), CalcAddr(num + 1), (FCount-num-1)*FItemSize); // make place to insert
  //Update(num, pValue);
  Result := Index;
end;

procedure THArrayG<T>.InsertMany(Index: Cardinal; Value: T; Cnt: Cardinal);
var i: Cardinal;
begin
  if Cnt = 0 then exit;
  Error(Index, FCount + 1);

  FCount := FCount + Cnt;
  if FCount > FCapacity then GrowTo(FCount);
  for i := FCount - 1 downto Index + Cnt do
    FValues[i] := FValues[i - Cnt];
  for i := Index to Index + Cnt - 1 do
    FValues[i] := Value;
 //memcpy(CalcAddr(num), CalcAddr(num+Cnt), (FCount-num-Cnt)*FItemSize);
 //UpdateMany(num, Value, Cnt);
end;

procedure THArrayG<T>.SetValue(Index: Cardinal; Value: T);
begin
  Error(Index, FCount);
  FValues[Index] := Value;
// if pValue = nil
//  then memclr(GetAddr(num), FItemSize)
//  else memcpy(pValue, GetAddr(num), FItemSize);
end;

procedure THArrayG<T>.Swap(Index1, Index2: Cardinal);
var
  tmp: T;
begin
  Error(Index1, FCount);
  Error(Index2, FCount);

  tmp := FValues[Index1];
  FValues[Index1] := FValues[Index2];
  FValues[Index2] := tmp;
end;

procedure THArrayG<T>.UpdateMany(Index: Cardinal; Value: T; Cnt: Cardinal);
var i: Cardinal;
begin
  if Cnt = 0 then exit;
  Error(Index + Cnt - 1, FCount);
  for i := Index to Index + Cnt - 1 do FValues[i] := Value;

// memcpy(pValue, GetAddr(num), FItemSize*Cnt);
end;

function THArrayG<T>.GetItemSize: Cardinal;
begin
  Result := Cardinal(sizeof(T));
end;

function THArrayG<T>.GetValue(Index: Cardinal): T;
begin
  Error(Index, FCount);
  Result := FValues[Index];
// memcpy(GetAddr(num), pValue, FItemSize);
end;

function THArrayG<T>.GetValuePointer(Index: Cardinal): PointerT;
begin
  Error(Index, FCount);
  Result := @FValues[Index];
end;

function THArrayG<T>.Pop: T;
begin
  Error(0, FCount);
  Dec(FCount);
  Result := FValues[FCount];
end;

procedure THArrayG<T>.Push(Value: T);
begin
  AddValue(Value);
end;

procedure THArrayG<T>.Error(Value, max: Cardinal);
begin
  Assert(FCapacity = Length(FValues));
  Assert(FCapacity >= FCount);

  if Value >= max then raise ERangeError.Create(Format(SItemNotFound, [Value]));
end;

function THArrayG<T>.IndexOf(Value: T): Integer;
begin
  Result := IndexOfFrom(Value, 0);
end;

function THArrayG<T>.IndexOfFrom(Value: T; Start: Cardinal): Integer;
var i: Cardinal;
    cmp: IComparer<T>;
begin
  Result := -1;
  if Start >= FCount then exit;
 //Error(Start, Integer(FCount) - 1);
  cmp := TComparer<T>.Default;
  for i := Start to FCount - 1 do
    if cmp.Compare(FValues[i], Value) = 0 then begin
      Result := Integer(i);
      exit;
    end;
end;

(*
  procedure THArrayG<T>.MoveData(FromPos, Cnt:Cardinal; Offset:Integer);
  var mem:pointer;
  begin
   Error(FromPos, Integer(FCount) - 1);
   Error(FromPos + Cnt, FCount);
   Error(FromPos + Offset, Integer(FCount) - 1);
   Error(FromPos + Offset + Cnt, FCount);
   mem := AllocMem(Cnt*FItemSize);
   try
    memcpy(CalcAddr(FromPos), mem, Cnt*FItemSize);
    if Offset < 0 then memcpy(CalcAddr(FromPos+Offset),CalcAddr(FromPos+Offset+Cnt),(-Offset)*FItemSize);
    if Offset > 0 then memcpy(CalcAddr(FromPos+Cnt),CalcAddr(FromPos),Offset*FItemSize);
    memcpy(mem,CalcAddr(FromPos+Offset),Cnt*FItemSize);
   finally
    FreeMem(mem);
   end;
  end;

*)

// Bubble sort
// Everybody knows it
procedure THArrayG<T>.BubbleSort(CompareProc: TCompareProc);
var
  i, j  : Cardinal;
  cmp: IComparer<T>;
  WasSwap: Boolean;
begin
  if FCount < 2 then exit; // one or zero elements in array, no need to sort

  if @CompareProc = nil then begin
    cmp := TComparer<T>.Default;
    for i := 0 to FCount - 2 do begin
      WasSwap := False;
      for j := 0 to FCount - 2 - i do
        if cmp.Compare(FValues[j], FValues[j + 1]) > 0 then begin
          Swap(j, j + 1);
          WasSwap := True;
        end;
      if NOT WasSwap then break; // there was no Swap in internal for. it means that whole array is sorted already
    end;
  end else begin
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
end;

// Selection sort
// Each time we search for minimum element in the rest of array and put this element into appropriate place in array
// this algorithm is kind of slow
procedure THArrayG<T>.SelectionSort(CompareProc: TCompareProc);
var
  minEl : Cardinal;
  i, j  : Cardinal;
  cmp: IComparer<T>;
begin
  if FCount < 2 then exit; // one or zero elements in array, no need to sort

  if @CompareProc = nil then begin
    cmp := TComparer<T>.Default;
    for i := 0 to FCount - 2 do begin
      minEl := i;
      for j := i + 1 to FCount - 1 do
        if cmp.Compare(FValues[j], FValues[minEl]) < 0 then minEl := j;
      if minEl <> i then Swap(i, minEl);
    end;
  end else begin
    for i := 0 to FCount - 2 do begin
      minEl := i;
      for j := i + 1 to FCount - 1 do
        if CompareProc(self, j, minEl) < 0 then minEl := j;
      if minEl <> i then Swap(i, minEl);
    end;
  end;
end;

procedure THArrayG<T>.QuickSort(CompareProc: TCompareProc);
begin
  if FCount < 2 then exit;
  InternalQuickSort(CompareProc, 0, FCount - 1);
end;

procedure THArrayG<T>.InternalQuickSort(CompareProc: TCompareProc; L, R: Cardinal);
var
  i, j: Cardinal;
  P: Cardinal;
  cmp: IComparer<T>;
begin
  if @CompareProc = nil then cmp := TComparer<T>.Default;

  i := L;
  j := R;
  P := (i + j) shr 1;  // pivot element

  while i < j do begin
    if @CompareProc = nil then begin
      while (cmp.Compare(FValues[i], FValues[P]) < 0) do Inc(i);
      while (cmp.Compare(FValues[j], FValues[P]) > 0) do Dec(j);
    end else begin
      while (CompareProc(self, i, P) < 0) do Inc(i);
      while (CompareProc(self, j, P) > 0) do Dec(j);
    end;

    if i <= j then begin
      if i = P then P := j // count a case when element with index P will be swapped and receive another index
      else if j = P then P := i;
      Swap(i, j);
      //if @SwapProc = nil then Swap(i, j) else SwapProc(self, i, j);
      Inc(i);

      //TODO: to avoid out of range exception then Dec(j) when j=0. I think there should be better solution to bypasss this situation
      // for example do not call InternalQuickSort when R-L=1 because sorting is trivial in a such interval
      // another solution might be to call InsertSort instead of QuickSort for intervals less then some value (e.g. 40)
      if j = 0 then break;
      Dec(j);
    end;
  end;

  if L < j then InternalQuickSort(CompareProc, L, j);
  if i < R then InternalQuickSort(CompareProc, i, R);
end;

// Insertion sort
// L..i-1 this is already sorted part of array
// we take i'th element and find a proper place where to insert it in in sorted part
// then we take i+1'th element , find place for it and so on
procedure THArrayG<T>.InternalInsertSort(CompareProc: TCompareProc2; L, R: Cardinal);
var
  i, j: Cardinal;
  tmp: T;
  cmp: IComparer<T>;
begin
  if @CompareProc = nil then begin
    cmp := TComparer<T>.Default;
    for i := L + 1 to R do begin
      j := i;
      tmp := FValues[i];
      while (j > L) and (cmp.Compare(FValues[j - 1], tmp) > 0) {(FValues[j-1] > tmp)}  do begin
        FValues[j] := FValues[j - 1];
        Dec(j);
      end;
      FValues[j] := tmp;
    end;

  end else begin

    for i := L + 1 to R do begin
      j := i;
      tmp := FValues[i];
      while (j > L) and (CompareProc(FValues[j - 1], tmp) > 0) {(FValues[j-1] > tmp)}  do begin
        FValues[j] := FValues[j - 1];
        Dec(j);
      end;
      FValues[j] := tmp;
    end;
  end;
end;

// see InternalInsertSort for brief description of algorithm.
procedure THArrayG<T>.InsertSort(CompareProc: TCompareProc2);
begin
  if FCount < 2 then exit;
  InternalInsertSort(CompareProc, 0, FCount - 1);
end;

// Shaker Sort
procedure THArrayG<T>.ShakerSort(CompareProc: TCompareProc2);
var
  i, j: Cardinal;
  Min, Max: Cardinal;
  cmp: IComparer<T>;
begin
  if FCount < 2 then exit;

  if @CompareProc = nil then begin
    cmp := TComparer<T>.Default;

    for i := 0 to (FCount div 2) - 1 do begin
      if cmp.Compare(FValues[i], FValues[i + 1]) > 0 then begin
        Max := i;
        Min := i + 1;
      end else begin
        Max := i + 1;
        Min := i;
      end;

      for j := i + 2 to FCount - i - 1 do
        if cmp.Compare(FValues[j], FValues[Max]) > 0 then Max := j
        else if cmp.Compare(FValues[j], FValues[Min]) < 0 then Min := j;

      Swap(i, Min);
      if Max = i then Max := Min; // this is new place of Max element after Swap(i, Min);
      Swap(FCount - i - 1, Max);
    end;

  end else begin

    for i := 0 to (FCount div 2) - 1 do begin
      if CompareProc(FValues[i], FValues[i + 1]) > 0 then begin
        Max := i;
        Min := i + 1;
      end else begin
        Max := i + 1;
        Min := i;
      end;

      for j := i + 2 to FCount - i - 1 do
        if CompareProc(FValues[j], FValues[Max]) > 0 then Max := j
        else if CompareProc(FValues[j], FValues[Min]) < 0 then Min := j;

      Swap(i, Min);
      if Max = i then Max := Min; // this is new place of Max element after Swap(i, Min);
      Swap(FCount - i - 1, Max);
    end;
  end;
end;

// *** array MUST be SORTED in ASCENDING order for proper work of thos procedure!!!
function THArrayG<T>.QuickFind(CompareProc: TCompareProc2; FindWhat: T; Start: Cardinal = 0): Integer;
var
  L, R: Cardinal;
 // middle: Cardinal;
 // res: Integer;
begin
  Result := -1; // 'not found' by default
  if FCount = 0 then exit;
  if @CompareProc = nil then exit; //TODO: may be use default comparator here as it is done in Sort functions or raise an exception?

  L := Start;
  R := FCount - 1;
  if CompareProc(FindWhat, FValues[R]) > 0 then begin // FindWhat is larger than last element in the array
    Result := -Integer(R + 2);
    exit;
  end;
  if CompareProc(FindWhat, FValues[L]) < 0 then begin // FindWhat is smaller than first element in the array
    Result := -1;
    exit;
  end;

  Result := InternalQuickFind(CompareProc, FindWhat, L, R);

  {
  while True do begin
    //was1 := abs(R - L) = 1;
    middle := (L + R) div 2;
    //if middle = L then goto fin; // exit;
    res := CompareProc(FindWhat, FValues[middle]);
    if res < 0 then R := middle - 1  // searched elemnt is on the left
    else if res > 0 then L := middle + 1  // searched elemnt is on the right
    else begin  // we've found element being searched
      if middle = Start then break;
      Dec(middle);
      while (True) do  // look for lowest index in case several elements =Value exist in the array
        if CompareProc(FindWhat, FValues[middle]) = 0 then begin
          if middle > Start
            then Dec(middle)
            else break; // stop if we arrived to Start index
        end else begin
          Inc(middle);
          break;
        end;
      Result := middle;
      exit;
    end;

    if L >= R then break;
  end;

  Result := -Integer(L + 1);}
end;

function THArrayG<T>.InternalQuickFind(CompareProc: TCompareProc2; FindWhat: T; L, R: Cardinal): Integer;
var
  middle: Cardinal;
  res: Integer;
begin
  Result := -1; // may be default value not needed here

  if L = R then begin
    res := CompareProc(FindWhat, FValues[L]);
    if res < 0 then Result := -Integer(L + 1)
    else if res > 0 then Result := -Integer(L + 2)
    else Result := Integer(L);
  end
  else
  if R - L = 1 then begin
    res := CompareProc(FindWhat, FValues[L]);
    if res = 0 then Result := Integer(L)
    else if res < 0 then Result := -Integer(L + 1)
    else if res > 0 then begin
      res := CompareProc(FindWhat, FValues[R]);
      if res = 0 then Result := Integer(R)
      else if res < 0 then Result := -Integer(R + 1)
      else if res > 0 then Result := -Integer(R + 2)
    end;
  end
  else
  begin
    while True do begin
      middle := (L + R) div 2;
      res := CompareProc(FindWhat, FValues[middle]);
      if res < 0 then begin Result := InternalQuickFind(CompareProc, FindWhat,  L, middle - 1); break; end  // searched element is on the left
      else if res > 0 then begin Result := InternalQuickFind(CompareProc, FindWhat,  middle + 1, R); break; end  // searched elemnt is on the right
      else begin  // we've found element being searched
        if middle = L then break;
        Dec(middle);
        while (True) do // look for lowest index in case several elements =Value exist in the array
          if CompareProc(FindWhat, FValues[middle]) = 0 then begin
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

function THArrayG<T>.Find(CompareProc: TCompareProc2; FindWhat: T; Start: Cardinal): Integer;
var i: Cardinal;
begin
   if FCount = 0 then Exit(-1);

   for i := Start to FCount - 1 do begin
     if CompareProc(FindWhat, FValues[i]) = 0 then Exit(Integer(i));
   end;
   Result := -1; // nothing found
end;

procedure THArrayG<T>.LoadFromStream(s: TStream);
var cnt, i: Cardinal;
    tmp: T;
begin
   s.ReadData<Cardinal>(cnt);
   SetCapacity(FCount + cnt);

   for i := 0 to cnt - 1 do begin
     s.ReadData<T>(tmp);
     AddValue(tmp);
   end;
end;

procedure THArrayG<T>.SaveToStream(s: TStream);
var i: Cardinal;
begin
   s.WriteData<Cardinal>(FCount);
   for i := 0 to FCount - 1 do
     s.WriteData<T>(FValues[i]);
end;


end.
