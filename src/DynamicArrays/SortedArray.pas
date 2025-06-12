unit SortedArray;

interface

uses Classes, System.Generics.Defaults, DynamicArray;

type
  // Stores elements in sorted order
  // Sorting rules are defined Compare method that need to be ovveriden in descendant class (seee THArraySorted).
  THCustomArraySorted<T:constructor> = class(THArrayG<T>)
  private
    // Since array is sorted methods that can break sorting are moved into private section
    procedure AddMany(Value: T; Cnt: Cardinal); override;
    function  InsertValue(Index: Cardinal; Value: T): Cardinal; override;
    procedure InsertMany(Index: Cardinal; Value: T; Cnt: Cardinal); override;
    procedure SetValue(Index:Cardinal; Value: T); override;
    procedure UpdateMany(Index: Cardinal; Value: T; Cnt: Cardinal); override;
    procedure Swap(Index1, Index2: Cardinal); override;
    // sorting methods are not needed for sorted array
    procedure BubbleSort(CompareProc: THArrayG<T>.TCompareProc); override;
    procedure SelectionSort(CompareProc: THArrayG<T>.TCompareProc); override;
    procedure InsertSort(CompareProc: THArrayG<T>.TCompareProc2); override;
    procedure QuickSort(CompareProc: THArrayG<T>.TCompareProc); override;
    procedure ShakerSort(CompareProc: THArrayG<T>.TCompareProc2); override;
  protected
    // override this method to implement comparing of elements make proper (sorted) order of elemenets in the array
    // this method is called by InternalIndexOfFrom and AddValue to define sorting order
    function Compare(const Left, Right: T): Integer; virtual; abstract;
    function InternalIndexOfFrom(Value: T; Start: Cardinal): Integer;
  public
    // adds element Value into array into position according to sorting order
    function AddValue(Value: T): Cardinal; override;

    // returns index of found element or -1 if element is not found
    function IndexOfFrom(Value: T; Start: Cardinal): Integer; override;
    //constructor Create; overload; override;

    // read only property
    property Value[Index: Cardinal]: T read GetValue; default; // we need read only property here
 end;


  // This is generic class with comparator type parameter CmpT that has to be descendant of TComparer<T>
  // Overrided function Compare uses CmpT claa for doing citems comparison
  THArraySorted<T:constructor; CmpT: TComparer<T>, constructor> = class(THCustomArraySorted<T>)
  private
    FComparer: CmpT;
//    function InsertValue(Index: Cardinal; Value: T): Cardinal; override;
  protected
    function Compare(const Left, Right: T): Integer; override;
    //function InternalIndexOfFrom(Value: T; Start: Cardinal): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    // you can set you own comparer after instance is created
    // if new comparer differs from old one then all elements are deleted before new comparer is set.
    // this is done because sorting rules chnage with new comparer
    procedure SetComparer(Comparer: CmpT);
    function GetComparer: CmpT;
 end;

  // This is generic class uses default TComparer<T>.Default comparer for sorting element in the array
 THArraySorted<T:constructor> = class(THCustomArraySorted<T>)
  private
    FComparer: IComparer<T>;
  protected
    function Compare(const Left, Right: T): Integer; override;
    //function InternalIndexOfFrom(Value: T; Start: Cardinal): Integer;
  public
    // initialises FComparer to TComparer<T>.Default value
    constructor Create; overload; override;

    // this is a way to define your own comparer
    constructor Create(Comparer: IComparer<T>); overload;
    destructor Destroy; override;

    // you can set you own comparer after instance is created
    // if new comparer differs from old one then all elements are deleted before new comparer is set.
    // this is done because sorting rules chnage with new comparer
    procedure SetComparer(Comparer: IComparer<T>);
    function GetComparer: IComparer<T>;
 end;

implementation

 uses SysUtils;

{ THArraySorted<T> 2 type params }

constructor THArraySorted<T, CmpT>.Create;
begin
  inherited Create;
  FComparer := CmpT(CmpT.NewInstance);
end;

destructor THArraySorted<T, CmpT>.Destroy;
begin
  FComparer._Release;
  inherited;
end;

{
function THArraySorted<T, CmpT>.AddValue(Value: T): Cardinal;
var n: Integer;
begin
  n := InternalIndexOfFrom(Value, 0);
  if n < 0
    then Result := Cardinal(-(n + 1))
    else Result := Cardinal(n);

  inherited InsertValue(Result, Value);
end;

function THArraySorted<T, CmpT>.IndexOfFrom(Value: T; Start: Cardinal): Integer;
var n: Integer;
begin
  n := InternalIndexOfFrom(Value, Start);

  if n < 0
    then Result := -1
    else Result := n;
end;
 }

function THArraySorted<T, CmpT>.Compare(const Left, Right: T): Integer;
begin
  Result := FComparer.Compare(Left, Right);
end;

function THArraySorted<T, CmpT>.GetComparer: CmpT;
begin
  Result := FComparer;
end;

procedure THArraySorted<T, CmpT>.SetComparer(Comparer: CmpT);
begin
  if FComparer = Comparer then exit;

  Clear; // setting new comparer changes sorting order, that is why we delete all items from the array
  FComparer := Comparer;
end;

{ THArraySorted<T> 1 type param}

constructor THArraySorted<T>.Create;
begin
  inherited Create;
  FComparer := TComparer<T>.Default;
end;

constructor THArraySorted<T>.Create(Comparer: IComparer<T>);
begin
  inherited Create;
  FComparer := Comparer;
end;

destructor THArraySorted<T>.Destroy;
begin
  FComparer._Release;
  inherited;
end;

function THArraySorted<T>.Compare(const Left, Right: T): Integer;
begin
  Result := FComparer.Compare(Left, Right);
end;

function THArraySorted<T>.GetComparer: IComparer<T>;
begin
  Result := FComparer;
end;

procedure THArraySorted<T>.SetComparer(Comparer: IComparer<T>);
begin
  if FComparer = Comparer then exit;

  Clear; // setting new comparer changes sorting order, that is why we delete all items from the array
  FComparer := Comparer;
end;

{
function THArraySorted<T>.InsertValue(Index: Cardinal; Value: T): Cardinal;
begin
  raise EInvalidInsert.Create(SCannotInsertIntoSortedArray);
end;
 }

{ THCustomArraySorted<T> }

function THCustomArraySorted<T>.AddValue(Value: T): Cardinal;
var n: Integer;
begin
  n := InternalIndexOfFrom(Value, 0);
  if n < 0
    then Result := Cardinal(-(n + 1))
    else Result := Cardinal(n);

   inherited InsertValue(Result, Value);
end;

function THCustomArraySorted<T>.IndexOfFrom(Value: T; Start: Cardinal): Integer;
var n: Integer;
begin
  n := InternalIndexOfFrom(Value, Start);

  if n < 0
    then Result := -1
    else Result := n;
end;

function THCustomArraySorted<T>.InternalIndexOfFrom(Value: T; Start: Cardinal): Integer;
var
  left, cnt: Cardinal;
  step, middle: Cardinal;
begin
  if (Start >= FCount) AND (FCount <> 0) then raise ERangeError.Create(SItemNotFound);

  Result := -1;
  if FCount = 0 then exit;

  left := Start;
  cnt := FCount - Start;

  // binary search
  while cnt > 0 do begin
    step := cnt div 2;
    middle := left + step;
    if Compare(Value, FValues[middle]) > 0 then begin
      left := middle + 1;
      cnt := cnt - (step + 1);
    end
    else
    if Compare(Value, FValues[middle]) < 0 then
      cnt := step
    else begin
      // look for lowest index in case several elements =Value exist in the array
      while (True) do
       if Compare(Value, FValues[middle]) = 0 then begin
         if middle > Start
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

  if (left < FCount) AND (Value = FValues[left]) then begin
    Result := Integer(left);
    exit;
  end;

  Result := -Integer(left + 1);  // return position (with negative sign) where element is going to be according to sorting
end;

function THCustomArraySorted<T>.InsertValue(Index: Cardinal; Value: T): Cardinal;
begin
  raise EInvalidInsert.Create(SCannotInsertIntoSortedArray);
end;

procedure THCustomArraySorted<T>.AddMany(Value: T; Cnt: Cardinal);
begin
  raise EInvalidInsert.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.BubbleSort(CompareProc: THArrayG<T>.TCompareProc);
begin
  raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.SelectionSort(CompareProc : THArrayG<T>.TCompareProc);
begin
  raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.ShakerSort(CompareProc: THArrayG<T>.TCompareProc2);
begin
  raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.QuickSort(CompareProc: THArrayG<T>.TCompareProc);
begin
  raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.InsertSort(CompareProc: THArrayG<T>.TCompareProc2);
begin
  raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.InsertMany(Index: Cardinal; Value: T; Cnt: Cardinal);
begin
  raise EInvalidInsert.Create(SCannotInsertIntoSortedArray);
end;

procedure THCustomArraySorted<T>.SetValue(Index: Cardinal; Value: T);
begin
  raise EInvalidInsert.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.Swap(Index1, Index2: Cardinal);
begin
   raise EInvalidOpException.Create(SOperationNotSupportedBySortedArray);
end;

procedure THCustomArraySorted<T>.UpdateMany(Index: Cardinal; Value: T; Cnt: Cardinal);
begin
   raise EInvalidInsert.Create(SOperationNotSupportedBySortedArray);
end;

end.


