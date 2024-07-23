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
  TypInfo, FileNamesCache;

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

end.
