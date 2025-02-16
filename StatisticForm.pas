unit StatisticForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, Vcl.Tabs,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TStatisticForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TopFoldersChart: TChart;
    Series1: TPieSeries;
    TopFilesChart: TChart;
    PieSeries1: TPieSeries;
    FileByCatChart: TChart;
    PieSeries2: TBarSeries;
    procedure TabSheet3Show(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StatisticForm1: TStatisticForm1;

implementation

{$R *.dfm}

uses
  TypInfo, DynamicArray, FileCache;

type
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

procedure TStatisticForm1.TabSheet1Show(Sender: TObject);
var
  top: TTopFolders;
  Item: TCacheItem;
  i: Cardinal;
begin
  top := TTopFolders.Create(10);
  try
    top.BuildTopFolders;

    TopFoldersChart.Series[0].Clear;
    for i := 0 to 10-1 do begin
      Item := top.GetItem(i);
      TopFoldersChart.Series[0].Add(Item.FFullFileSize, Item.FFileData.cFileName);
    end;

  finally
    top.Free;
  end;
end;

procedure TStatisticForm1.TabSheet2Show(Sender: TObject);
var
  top: TTopFolders;
  Item: TCacheItem;
  i: Cardinal;
begin
  top := TTopFolders.Create(10);
  try
    top.BuildTopFiles();
    TopFilesChart.Series[0].Clear;
    for i := 0 to 10-1 do begin
      Item := top.GetItem(i);
      TopFilesChart.Series[0].Add(Item.FFullFileSize, Item.FFileData.cFileName);
    end;

  finally
    top.Free;
  end;
end;

procedure TStatisticForm1.TabSheet3Show(Sender: TObject);
var
  Stat: TFileSystemStatRecord;
  i: Cardinal;
begin
  FileByCatChart.Series[0].Clear;
  Stat := TFSC.Instance.GetStat;
    for i := Low(Stat.Index) to High(Stat.Index) do begin
      if Stat.Index[i] <> ftAll then
        FileByCatChart.Series[0].Add(stat.Stat[stat.Index[i]], FileTypeNames[stat.Index[i]]); //GetEnumName(TypeInfo(TFileTypes), Ord(i)));
    end;

end;

{ TTopFolders }

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
  //level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
  vol: TVolumeCache;
begin
  var Cache: TCache := TFSC.Instance;
  if Cache.VolumesCount = 0 then Exit;

  vol := Cache.GetVolume('C:\'); //TODO: make it work with many volumes
  for i := 0 to vol.Levels - 1 do begin
    //Level := Cache.FCacheData[i];
    for j := 0 to vol.LevelCount(i) - 1 do begin
      Item := vol.GetItem(i, j); //Level.GetAddr(j);
      if IsDirectory(Item) then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;

procedure TTopFolders.BuildTopFiles();
var
  //level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
  vol: TVolumeCache;
begin
  var Cache: TCache := TFSC.Instance;
  if Cache.VolumesCount = 0 then Exit;

  vol := Cache.GetVolume('C:\'); //TODO: make it work with many volumes
  for i := 0 to vol.Levels - 1 do begin
    //Level := Cache.FCacheData[i];
    for j := 0 to vol.LevelCount(i) - 1 do begin
      Item := vol.GetItem(i, j); //levelGetAddr(j);
      if NOT (IsDirectory(Item)
         OR ((Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DEVICE) > 0)) then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;


end.
