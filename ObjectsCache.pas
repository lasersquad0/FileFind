unit ObjectsCache;

interface

uses SyncObjs, DynamicArray;

type
 TObjectsCache<T:constructor, class> = class
   private
     FObjects: THArrayG<T>;
     FLock: TCriticalSection;
   public
     function GetItem(): T;
     procedure PutItem(Item: T);
     procedure SetCapacity(Capacity: Cardinal);
     constructor Create(Capacity: Cardinal; ThreadSafe: Boolean);
     destructor Destroy; override;
 end;

implementation

uses SysUtils;

{ TObjectsCache<T> }

constructor TObjectsCache<T>.Create(Capacity: Cardinal; ThreadSafe: Boolean);
var i: Cardinal;
begin
  FObjects := THArrayG<T>.Create(Capacity);
  if ThreadSafe
    then FLock :=  TCriticalSection.Create
    else FLock := nil;
    //TInterlocked
  // fill with cached values
  for i := 1 to Capacity do FObjects.AddValue(T.Create);
end;

destructor TObjectsCache<T>.Destroy;
var
  i: Cardinal;
begin
  if Assigned(FLock) then FLock.Acquire;
  try
    for i := 1 to FObjects.Count do FObjects[i - 1].Free;
  finally
    if Assigned(FLock) then FLock.Release;
    FreeAndNil(FObjects);
    FreeAndNil(FLock);
  end;

  inherited Destroy;
end;

function TObjectsCache<T>.GetItem: T;
begin
  if Assigned(FLock) then FLock.Acquire;
  try
    if FObjects.Count > 0
      then Result := FObjects.Pop()
      else Result := T.Create;
  finally
    if Assigned(FLock) then FLock.Release;
  end;
end;

procedure TObjectsCache<T>.PutItem(Item: T);
begin
  if Assigned(FLock) then FLock.Acquire;
  try
    FObjects.AddValue(Item);
  finally
    if Assigned(FLock) then FLock.Release;
  end;
end;

procedure TObjectsCache<T>.SetCapacity(Capacity: Cardinal);
begin
  FObjects.SetCapacity(Capacity); //TODO: may be create Capacity-Count instances of T here?
end;

end.
