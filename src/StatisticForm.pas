unit StatisticForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, Vcl.ExtCtrls, VCLTee.TeeProcs, VCLTee.Chart, Vcl.Tabs,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls, Hash;

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
    TopFoldersComboBox: TComboBox;
    TopFilesComboBox: TComboBox;
    CategoriesComboBox: TComboBox;
    procedure TabSheet3Show(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TopFoldersComboBoxChange(Sender: TObject);
    procedure TopFilesComboBoxChange(Sender: TObject);
    procedure CategoriesComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHash: THash<TTabSheet, TComboBox>;
    FFlag: Boolean;
  public
    { Public declarations }
  end;

var
  StatisticForm1: TStatisticForm1;

implementation

{$R *.dfm}

uses
  TypInfo, DynamicArray, CacheItem, FileCache;

type
 TTopFolders = class
 protected
   FItems: THArrayG<TCacheItem>;
   FMin: UInt64;
   FMax: UInt64;
   FSize: Cardinal;
   procedure UpdateMinMaxAndDelete();
   procedure AddValue(Item: TCacheItem);
   function CompareProc(Item1, Item2: TCacheItem): Integer;
 public
   constructor Create(Num: Cardinal);
   destructor Destroy; override;
   procedure BuildTopFolders(Volume: string);
   procedure BuildTopFiles(Volume: string);
   function GetItem(Index: Cardinal): TCacheItem;
 end;

procedure TStatisticForm1.FormCreate(Sender: TObject);
begin
  FHash := THash<TTabSheet, TComboBox>.Create;
  FHash.SetValue(TabSheet1, TopFoldersComboBox);
  FHash.SetValue(TabSheet2, TopFilesComboBox);
  FHash.SetValue(TabSheet3, CategoriesComboBox);
  FFlag := False;
end;

procedure TStatisticForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHash);
end;

procedure TStatisticForm1.FormShow(Sender: TObject);
var
  i: Cardinal;
  Volumes: TArray<string>;

begin
  Volumes := TCache.Instance.GetVolumes;

  TopFoldersComboBox.Items.BeginUpdate;
  TopFilesComboBox.Items.BeginUpdate;
  CategoriesComboBox.Items.BeginUpdate;
  try
    TopFoldersComboBox.Clear;
    TopFilesComboBox.Clear;
    CategoriesComboBox.Clear;
    for i := 0 to Length(Volumes) - 1 do begin
      TopFoldersComboBox.Items.Add(Volumes[i]);
      TopFilesComboBox.Items.Add(Volumes[i]);
      CategoriesComboBox.Items.Add(Volumes[i]);
    end;

    TopFoldersComboBox.ItemIndex := 0;
    TopFilesComboBox.ItemIndex := 0;
    CategoriesComboBox.ItemIndex := 0;
  finally
    TopFoldersComboBox.Items.EndUpdate;
    TopFilesComboBox.Items.EndUpdate;
    CategoriesComboBox.Items.EndUpdate;
  end;

  FHash[PageControl1.ActivePage].OnChange(self);
  FFlag := True;
end;

procedure TStatisticForm1.TabSheet1Show(Sender: TObject);
begin
  if FFlag then TopFoldersComboBoxChange(TopFoldersComboBox);
end;

procedure TStatisticForm1.TabSheet2Show(Sender: TObject);
begin
  if FFlag then TopFilesComboBoxChange(TopFilesComboBox);
end;

procedure TStatisticForm1.TabSheet3Show(Sender: TObject);
begin
  if FFlag then CategoriesComboBoxChange(CategoriesComboBox);
end;

procedure TStatisticForm1.TopFilesComboBoxChange(Sender: TObject);
var
  top: TTopFolders;
  Item: TCacheItem;
  i: Cardinal;
begin
  top := TTopFolders.Create(10);
  try
    top.BuildTopFiles(TopFilesComboBox.Text);
    TopFilesChart.Series[0].Clear;
    for i := 0 to 10-1 do begin
      Item := top.GetItem(i);
      TopFilesChart.Series[0].Add(Item.FFileSize, Item.FFileName);
    end;

  finally
    top.Free;
  end;
end;

procedure TStatisticForm1.TopFoldersComboBoxChange(Sender: TObject);
var
  top: TTopFolders;
  Item: TCacheItem;
  i: Cardinal;
begin
  top := TTopFolders.Create(10);
  try
    top.BuildTopFolders(TopFoldersComboBox.Text);

    TopFoldersChart.Series[0].Clear;
    for i := 0 to 10-1 do begin
      Item := top.GetItem(i);
      TopFoldersChart.Series[0].Add(Item.FFileSize, Item.FFileName);
    end;

  finally
    top.Free;
  end;
end;

procedure TStatisticForm1.CategoriesComboBoxChange(Sender: TObject);
var
  Stat: TFileSystemStatRecord;
  i: Cardinal;
begin
  Stat := TCache.Instance.GetStat(CategoriesComboBox.Text);
  FileByCatChart.Series[0].Clear;
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
  FMin := FItems[0].FFileSize;
  MinIndex := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := FItems[i];
    if FMax < item.FFileSize then FMax := item.FFileSize;
    if FMin > item.FFileSize then begin
      FMin := item.FFileSize;
      MinIndex := i;
    end;
  end;

  FItems.DeleteValue(minIndex);
end;

function TTopFolders.CompareProc(Item1, Item2: TCacheItem): Integer;
begin
  if Item1.FFileSize > Item2.FFileSize then Result := 1
  else if Item1.FFileSize < Item2.FFileSize then Result := -1
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
    if FMax < item.FFileSize then FMax := item.FFileSize;
    if FMin > item.FFileSize then FMin := item.FFileSize;
  end else begin
    if Item.FFileSize > FMin then begin
      FItems.AddValue(Item);
      UpdateMinMaxAndDelete();
    end;
  end;
end;

procedure TTopFolders.BuildTopFolders(Volume: string);
var
  //level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
  vol: TVolumeCache;
begin
  var Cache: TCache := TCache.Instance;
  if Cache.VolumesCount = 0 then Exit;

  vol := Cache.GetVolume(Volume); //TODO: make it work with many volumes
  for i := 1 to vol.Levels do begin
    //Level := Cache.FCacheData[i];
    for j := 1 to vol.LevelCount(i - 1) do begin
      Item := vol.GetItem(i - 1, j - 1); //Level.GetAddr(j);
      if Item.IsDirectory then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;

procedure TTopFolders.BuildTopFiles(Volume: string);
var
  //level: TLevelType;
  Item: TCacheItem;
  i, j: Cardinal;
  vol: TVolumeCache;
begin
  var Cache: TCache := TCache.Instance;
  if Cache.VolumesCount = 0 then Exit;

  vol := Cache.GetVolume(Volume);
  for i := 1 to vol.Levels do begin
    //Level := Cache.FCacheData[i];
    for j := 1 to vol.LevelCount(i - 1) do begin
      Item := vol.GetItem(i - 1, j - 1); //levelGetAddr(j);
      if NOT (Item.IsDirectory
         OR ((Item.FFileAttrs AND FILE_ATTRIBUTE_DEVICE) > 0)) then AddValue(Item);
    end;
  end;

  FItems.InsertSort(CompareProc);
end;


end.
