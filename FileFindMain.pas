unit FileFindMain;

interface

uses
  WinAPI.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  System.Generics.Collections,
  FileNamesCache, LoadFSThread, DynamicArray, Hash, Vcl.Menus, Vcl.NumberBox,
  System.ImageList, Vcl.ImgList, Vcl.WinXPickers, Vcl.Mask;

type
  TSearchResultsItem = class
  public
    Caption: string;
    Size: Uint64;
    SizeStr: string;
    FileType: string;
    FileAttr: DWORD;
    Modified: TFileTime;
    ModifiedStr: string;
    LastAccess: TFileTime;
    LastAccessStr: string;
    Path: string;
    IIcon: Integer;
    function IsDirectory: Boolean;
 end;

 TSearchResultsCache = class
   private
     FData: THArrayG<TSearchResultsItem>;
   public
     function GetItem(): TSearchResultsItem;
     procedure PutItem(Item: TSearchResultsItem);
     procedure SetCapacity(Capacity: Cardinal);
     constructor Create(Capacity: Cardinal);
     destructor Destroy; override;
 end;

 TMainFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
 public
   procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time

 end;


  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    FileOpenDialog1: TFileOpenDialog;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    File2: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Settings1: TMenuItem;
    SearchPanel: TPanel;
    SearchEdit: TLabeledEdit;
    SearchByFileSize: TCheckBox;
    FileSizeOp: TComboBox;
    SearchFileSize: TNumberBox;
    FileSizeFactor: TComboBox;
    ImageList1: TImageList;
    AdvancedSearchButton: TSpeedButton;
    SearchByModifiedDate: TCheckBox;
    DateTimePickerFrom: TDateTimePicker;
    DateTimePickerTo: TDateTimePicker;
    LabelAnd: TLabel;
    SearchByAttributes: TCheckBox;
    AttrArchive: TCheckBox;
    AttrHidden: TCheckBox;
    AttrDirectory: TCheckBox;
    AttrEncrypted: TCheckBox;
    AttrCompressed: TCheckBox;
    AttrReadonly: TCheckBox;
    AttrSystem: TCheckBox;
    StartSearchBtn: TBitBtn;
    PopupMenu1: TPopupMenu;
    Openfilefolder1: TMenuItem;
    Statistics1: TMenuItem;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    IndexingBitBtn: TBitBtn;
   // procedure BuildIndexBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    //procedure SpeedButton1Click(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1DblClick(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure SearchByFileSizeClick(Sender: TObject);
    procedure AdvancedSearchButtonClick(Sender: TObject);
    procedure SearchByModifiedDateClick(Sender: TObject);
    procedure SearchByAttributesClick(Sender: TObject);
    procedure StartSearchBtnClick(Sender: TObject);
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
    procedure Openfilefolder1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Statistics1Click(Sender: TObject);
    procedure IndexingBitBtnClick(Sender: TObject);
  private
    { Private declarations }
    //FIndexingThread: TLoadFSThread;
    FSortColumnID: Integer;
    FInvertSort: Boolean;
    FSearchResults: THArrayG<TSearchResultsItem>;
    FSearchResultsCache: TSearchResultsCache;
    FColumnMap: THash<Integer, Integer>;
    FProgressBar: TProgressBar;
    FProgressLabel: TLabel;
    FIndexingThread: TLoadFSThread;
    FProgressListener: TIndexingProgress;
    procedure OnThreadTerminate(Sender: TObject);
    //procedure ReCreateIndexingThread;
    class function GetFileShellInfo(FileName: TFileName; Item: TCacheItem): Boolean;
    procedure GetSystemImageList;
    procedure ClearSearchResults;
    procedure ClearSortingMarks;
    procedure DoSort;
    function CompareData(arr : THArrayG<TSearchResultsItem>; i, j: Cardinal): Integer;
    function CompareData2(item1, item2: TSearchResultsItem): Integer;
   // function GetValue(item: TSearchResultsItem; ColID: Integer): string;
    procedure UpdateStatusBar(ExecTime, ItemsCount: Cardinal; DirSize: uint64);
    function OnSearchFile(FullPath: string; Item: TCacheItem): Boolean;
    function GetFileSizeOp(): TFileSizeCompare;
    function GetFileSizeFactor(): Cardinal;
    function GetAttributes: Cardinal;
    procedure MakeSearch;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  WinAPI.ShellAPI, WinAPI.CommCtrl, Math,
  Settings, SettingsForm, IndexingLog, About, Functions, StatisticForm;

{$R *.dfm}

const INDEX_FILENAME = 'FileFindIndex.idx';

/// <summary>During search FileNamesCache call this callback function only for items that successfully passed filter.</summary>
/// <remarks>Define this method to add filtered items into ListView. TCahceItem is a class therefore passed by reference.
/// If you modify Item's fields, new data be stored and available for next searches. </remarks>
///  <param name="FullPath">Full path that includes file name of the file being added</param>
///  <param name="Item">Item that contain other file information: size, modified date etc.</param>
///  <return> Returns False to stop filtering (for example when maximum number of items in ListView is reached). In other cases function should return true.</return>
function TMainForm.OnSearchFile(FullPath: string; Item: TCacheItem): Boolean;
var
 // i: Cardinal;
  ResultsItem: TSearchResultsItem;
begin
   Result := False;
   if FSearchResults.Count >= AppSettings.MaxFoundItems then exit;  // limit number of found items by value from settings

   ResultsItem := FSearchResultsCache.GetItem;

   if item.FFileType = '' then  // for each Item GetFileShellInfo will be called only once. Results are cached.
     GetFileShellInfo(FullPath, Item);  // FullPath contains filename too

   // for directories FindNextFile returns various small values that does not reflect actual dir size
   // therefore we set Size to zero for directories
   //if (Item.FFileData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY then begin
   //  ResultsItem.Size := 0;
   //  ResultsItem.SizeStr := '';
   //end else begin
   // end
   ResultsItem.Size := (uint64(Item.FFileData.nFileSizeHigh) shl 32) + uint64(Item.FFileData.nFileSizeLow);
   if Item.FDenied then
     ResultsItem.SizeStr := 'N/A'
   else
     ResultsItem.SizeStr := ThousandSep(ResultsItem.Size);

   ResultsITem.FileAttr := Item.FFileData.dwFileAttributes;
   ResultsItem.Modified := Item.FFileData.ftLastWriteTime;
   ResultsItem.ModifiedStr := GetLocalTime(Item.FFileData.ftLastWriteTime);
   ResultsItem.LastAccessStr := GetLocalTime(Item.FFileData.ftLastAccessTime);
   ResultsItem.LastAccess := Item.FFileData.ftLastAccessTime;
   ResultsItem.Path := FullPath;
   ResultsItem.IIcon := Item.FIconIndex;
   ResultsItem.FileType := Item.FFileType;
   ResultsItem.Caption := Item.FDisplayName;

   FSearchResults.AddValue(ResultsItem);

   Result := True;
end;

procedure TMainForm.Openfilefolder1Click(Sender: TObject);
begin
  ListView1DblClick(nil);
end;

function TMainForm.GetFileSizeOp: TFileSizeCompare;
begin
  GetFileSizeOp := TFileSizeCompare(FileSizeOp.ItemIndex);
end;

function TMainForm.GetFileSizeFactor: Cardinal;
begin
  GetFileSizeFactor := 1;  // default factor just in case
  case FileSizeFactor.ItemIndex of
    0: GetFileSizeFactor := 1;
    1: GetFileSizeFactor := 1024;
    2: GetFileSizeFactor := 1024*1024;
    3: GetFileSizeFactor := 1024*1024*1024;
  end;
end;

function TMainForm.GetAttributes: Cardinal;
begin
  Result := 0;
  Result := Result OR Cardinal(IfThen(AttrArchive.Checked,   FILE_ATTRIBUTE_ARCHIVE));
  Result := Result OR Cardinal(IfThen(AttrHidden.Checked,    FILE_ATTRIBUTE_HIDDEN));
  Result := Result OR Cardinal(IfThen(AttrDirectory.Checked, FILE_ATTRIBUTE_DIRECTORY));
  Result := Result OR Cardinal(IfThen(AttrEncrypted.Checked, FILE_ATTRIBUTE_ENCRYPTED));
  Result := Result OR Cardinal(IfThen(AttrCompressed.Checked,FILE_ATTRIBUTE_COMPRESSED));
  Result := Result OR Cardinal(IfThen(AttrReadonly.Checked,  FILE_ATTRIBUTE_READONLY));
  Result := Result OR Cardinal(IfThen(AttrSystem.Checked,    FILE_ATTRIBUTE_SYSTEM));
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  MakeSearch;
end;

procedure TMainForm.MakeSearch();
var
  Filter: TSearchFilter;
begin
  Screen.Cursor := crHourGlass;

  try
    ListView1.Items.BeginUpdate;

    var start := GetTickCount;

    ListView1.Items.Count := 0;
    ClearSearchResults;
    //ClearSorting; //FSortColumnID := -1;  // reset sorting column
    //FInvertSort := False;

    Filter.SearchStr := Trim(SearchEdit.Text);
    Filter.CaseSensitive := AppSettings.CaseSensitiveSearch;
    Filter.SearchByFileSize := SearchByFileSize.Checked;
    Filter.FileSize := uint64(SearchFileSize.ValueInt) * uint64(GetFileSizeFactor());
    Filter.FileSizeCmpType := GetFileSizeOp();
    Filter.SearchByModifiedDate := SearchByModifiedDate.Checked;
    Filter.ModifiedDateFrom := DateTimeToFileTime(DateTimePickerFrom.DateTime);
    Filter.ModifiedDateTo := DateTimeToFileTime(DateTimePickerTo.DateTime);
    Filter.SearchByAttributes := SearchByAttributes.Checked;
    Filter.Attributes := GetAttributes();

    TFSC.Instance.Search(Filter, OnSearchFile); // cache is global singleton

    DoSort;

    ListView1.Items.Count := Integer(FSearchResults.Count);

    var stop := GetTickCount;

    StatusBar1.Panels[0].Text := Format('  Found items: %u', [ListView1.Items.Count]);
    StatusBar1.Panels[1].Text := Format('Search time: %s', [MillisecToStr(stop - start)]);
  finally
    Screen.Cursor := crDefault;
    ListView1.Items.EndUpdate;
    ListView1.Repaint;
  end;
end;

procedure TMainForm.ClearSortingMarks();
var
 hdr: HWND;
 Item: THDItem;
begin
  hdr := ListView_GetHeader(ListView1.Handle);

  // clear sorting mark of current column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  //ColIndex := Columns.FindItemID(FSortColumnID).Index;
  Header_GetItem(hdr, FSortColumnID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  Header_SetItem(hdr, FSortColumnID, Item);
end;

procedure TMainForm.DoSort;
begin
  if FSortColumnID = -1 then exit;

  //var start := GetTickCount;
  //FSearchResults.Sort(CompareData);
  //FSearchResults.InsertSort(CompareData2);
  FSearchResults.QuickSort(CompareData);
  //var stop := GetTickCount;
  //StatusBar1.Panels[1].Text := Format('Search time: %s', [MillisecToStr(stop - start)]);
  ListView1.Refresh;
end;

procedure TMainForm.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
var
 hdr: HWND;
 Item: THDItem;
begin
  // clearing previous sorting marks
  ClearSortingMarks;

  if FSortColumnID = Column.ID then
    FInvertSort := NOT FInvertSort
  else begin
    FSortColumnID := Column.ID;
    FInvertSort := False;
  end;

  hdr := ListView_GetHeader(ListView1.Handle);

  // set sorting mark to the new sorted column
  FillChar(Item, sizeof(Item), 0);
  Item.Mask := HDI_FORMAT;
  //ColIndex := Columns.FindItemID(FSortColumnID).Index;
  Header_GetItem(hdr, FSortColumnID, Item);
  Item.Mask := Item.Mask OR HDI_FORMAT;
  Item.fmt  := Item.fmt AND (NOT (HDF_SORTDOWN OR HDF_SORTUP));  // clear HDF_SORTDOWN and HDF_SORTUP bits in Item.fmt
  Item.fmt  := Item.fmt  OR IfThen(FInvertSort, HDF_SORTDOWN, HDF_SORTUP); // set required bit
  Header_SetItem(hdr, FSortColumnID, Item);

  DoSort;
end;

function TSearchResultsItem.IsDirectory: Boolean;
begin
   IsDirectory := FileAttr AND FILE_ATTRIBUTE_DIRECTORY > 0;
end;

function TMainForm.CompareData(arr: THArrayG<TSearchResultsItem>; i, j: Cardinal): Integer;
//var
 // String1, String2: string;
begin
 //	Result := 0; // Defaults to equal
  var item1 := arr[i];
  var item2 := arr[j];

  Result := CompareData2(item1, item2);

  {
  String1 := AnsiUpperCase(GetValue(item1, FSortColumnID));
  String2 := AnsiUpperCase(GetValue(item2, FSortColumnID));

  // compare the selected values
  if String1 > String2 then Result := 1
  else if String1 < String2 then Result := -1
  else begin
	// if String1 = String2 then  // stings are equal, try to sort on Caption
  // Converts NAME to uppercase to ignore case
      String1 := AnsiUpperCase(item1.Caption);
      String2 := AnsiUpperCase(item2.Caption);
      if String1 > String2 then Result := 1
      else if String1 < String2 then Result := -1
    end;

  // invert Sort if requested
  if FInvertSort then Result := -Result;}
end;

function TMainForm.CompareData2(item1, item2: TSearchResultsItem): Integer;
//var
//  String1, String2: string;
begin
	Result := 0; // Defaults to equal

  var ColIndex := FColumnMap[FSortColumnID];


 //	if AppSettings.FoldersOnTop then begin
    if AppSettings.FoldersOnTop  AND item1.IsDirectory() AND NOT item2.IsDirectory() then Result := -1 // directory is always 'greater' than file
    else if AppSettings.FoldersOnTop AND NOT item1.IsDirectory() AND item2.IsDirectory() then Result := 1
    else begin
      if AppSettings.CaseSensitiveSort then begin // case sensitive comparison
        case ColIndex of
          0: Result := AnsiCompareStr(item1.Caption, item2.Caption);
          1: if item1.Size > item2.Size then Result := 1
               else if item1.Size < item2.Size then Result := -1;
          2: Result := AnsiCompareStr(item1.FileType, item2.FileType);
          3: Result := CompareFileTime(item1.Modified, item2.Modified);
          4: Result := CompareFileTime(item1.LastAccess, item2.LastAccess);
          5: Result := AnsiCompareStr(item1.Path, item2.Path);
          // else Result := 'UNKNOWN';
        end;
      end else begin // case INsensitive comparison
        case ColIndex of
          0: Result := AnsiCompareText(item1.Caption, item2.Caption);
          1: if item1.Size > item2.Size then Result := 1
             else if item1.Size < item2.Size then Result := -1;
          2: Result := AnsiCompareText(item1.FileType, item2.FileType);
          3: Result := CompareFileTime(item1.Modified, item2.Modified);
          4: Result := CompareFileTime(item1.LastAccess, item2.LastAccess);
          5: Result := AnsiCompareText(item1.Path, item2.Path);
          // else Result := 'UNKNOWN';
        end;
      end;
    end;
 //end;

  // invert Sort if requested
  if FInvertSort then Result := -Result;
end;

procedure TMainForm.ListView1Data(Sender: TObject; Item: TListItem);
var
  origItem: TSearchResultsItem;
begin
  origItem := FSearchResults[Cardinal(Item.Index)];

  item.Caption := origItem.Caption;
  item.ImageIndex := origItem.IIcon;
  item.SubItems.Add(origItem.SizeStr);
  item.SubItems.Add(origItem.FileType);
  item.SubItems.Add(origItem.ModifiedStr);
  item.SubItems.Add(origItem.LastAccessStr);
  item.SubItems.Add(origItem.Path);
end;

procedure TMainForm.ListView1DblClick(Sender: TObject);
var
  path: string;
  res: Integer;
begin
  if ListView1.Selected = nil then exit;

  var item := FSearchResults[Cardinal(ListView1.Selected.Index)];
  if item.IsDirectory
    then path := item.Path // this is directory
    else path := ExtractFilePath(item.Path); // this is file, open its directory

  res := ShellExecute(Handle, 'explore', PChar(path), nil, nil, SW_SHOWNORMAL);
  if res < 33 then MessageDlg('ShellExecute error: ' + IntToStr(res), TMsgDlgType.mtError, [mbOK], 0);
end;

procedure TMainForm.OnThreadTerminate(Sender: TObject);
begin
  TFSC.Instance.RemoveProgressListener(FProgressListener);
  FreeAndNil(FProgressListener);

  ListView1.Items.Count := 0; // reset search results to zero
  ClearSearchResults;

  Settings1.Enabled := True; // main menu item

  UpdateStatusBar(FIndexingThread.ExecData.ExecTime, TFSC.Instance.Count, FIndexingThread.ExecData.DirSize {TFSC.Instance.GetItem(0, 0).FFullFileSize});
end;

procedure TMainForm.ClearSearchResults;
var
  i: Cardinal;
begin
  // return SearchResultItems back to cache without freing.
  for i := 1 to FSearchResults.Count do FSearchResultsCache.PutItem(FSearchResults[i - 1]);
  FSearchResults.Clear;
end;

procedure TMainForm.SearchByAttributesClick(Sender: TObject);
begin
  AttrArchive.Enabled    := SearchByAttributes.Checked;
  AttrHidden.Enabled     := SearchByAttributes.Checked;
  AttrDirectory.Enabled  := SearchByAttributes.Checked;
  AttrEncrypted.Enabled  := SearchByAttributes.Checked;
  AttrCompressed.Enabled := SearchByAttributes.Checked;
  AttrReadonly.Enabled   := SearchByAttributes.Checked;
  AttrSystem.Enabled     := SearchByAttributes.Checked;
end;

procedure TMainForm.SearchByFileSizeClick(Sender: TObject);
begin
  FileSizeOp.Enabled     := SearchByFileSize.Checked;
  FileSizeFactor.Enabled := SearchByFileSize.Checked;
  SearchFileSize.Enabled := SearchByFileSize.Checked;
end;

procedure TMainForm.SearchByModifiedDateClick(Sender: TObject);
begin
  DateTimePickerFrom.Enabled := SearchByModifiedDate.Checked;
  DateTimePickerTo.Enabled   := SearchByModifiedDate.Checked;
  LabelAnd.Enabled := SearchByModifiedDate.Checked;
end;

procedure TMainForm.SearchEditChange(Sender: TObject);
begin
  Timer1.Enabled := False; // stop timer to prevent triggering while typing

  if Length(SearchEdit.Text) < 3 then exit; // search only when 3 and more symbols entered

 	Timer1.Enabled := True;  // restart timer
end;

procedure TMainForm.SearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then MakeSearch();
end;

procedure TMainForm.Settings1Click(Sender: TObject);
begin
  //SettingsForm1.FCache := FCache;
  if SettingsForm1.ShowModal = mrOk then begin
    ListView1.Items.Count := 0; // reset search results to zero
    ClearSearchResults;
    IndexingBitBtn.Hint := 'Folders to  index: ' + AppSettings.FolderToIndex;
  end;

  UpdateStatusBar(SettingsForm1.ExecData.ExecTime, TFSC.Instance.Count, SettingsForm1.ExecData.DirSize);
end;

procedure TMainForm.UpdateStatusBar(ExecTime, ItemsCount: Cardinal; DirSize: uint64);
begin
  StatusBar1.Panels[2].Text := Format('Data load time: %s', [MillisecToStr(ExecTime)]);
  StatusBar1.Panels[3].Text := Format('Items loaded: %s', [ThousandSep(ItemsCount)]);
  StatusBar1.Panels[4].Text := Format('Folder size: %s', [ThousandSep(DirSize)]);
end;

procedure TMainForm.StartSearchBtnClick(Sender: TObject);
begin
  MakeSearch(); // execute the same code when timer has triggered
end;

procedure TMainForm.Statistics1Click(Sender: TObject);
begin
  StatisticForm1.ShowModal;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TMainForm.AdvancedSearchButtonClick(Sender: TObject);
begin
  if AdvancedSearchButton.Down then begin  // open adv search panel
    AdvancedSearchButton.ImageIndex := 1;
    SearchPanel.Height := SearchPanel.Height * 7 div 2
  end else begin                          // close adv search panel
    AdvancedSearchButton.ImageIndex := 0;
    SearchPanel.Height := SearchPanel.Height * 2 div 7;
    SearchByFileSize.Checked := False;  // uncheck (make inactive) all search critetias to avoid misunderstanding
    SearchByModifiedDate.Checked := False;
    SearchByAttributes.Checked := False;
  end;
end;

procedure TMainForm.IndexingBitBtnClick(Sender: TObject);
begin
  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FProgressListener := TIndexingProgress.Create(FIndexingThread, IndexingLogForm.LogMemo.Lines);
  TFSC.Instance.AddProgressListener(FProgressListener);

  FIndexingThread.OnTerminate := OnThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  FIndexingThread.ProgressBar := ProgressBar1;
  FIndexingThread.StartDir := AppSettings.FolderToIndex;

  Settings1.Enabled := False;  // main menu item
  FIndexingThread.Start([StartSearchBtn, SearchEdit, IndexingBitBtn], [ProgressBar1, ProgressLabel], []);

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ProgressBar1.Parent := StatusBar1;
  ProgressBar1.Width := 100;
  ProgressBar1.Height := 17;
  ProgressBar1.Left := StatusBar1.Width - ProgressBar1.Width - 20;
  ProgressBar1.Top := 4;

  ProgressLabel.Parent := StatusBar1;
  ProgressLabel.Width := 110;
  ProgressLabel.Height := 17;
  ProgressLabel.Top := 5;
  ProgressLabel.Left := ProgressBar1.Left - ProgressLabel.Width;
  ProgressLabel.Caption := 'Indexing progress...';

  var start := GetTickCount;
  TFSC.Instance.DeserializeFrom(INDEX_FILENAME);
  TFSC.Instance.Modified := False;
  if TFSC.Instance.Count > 0 then
    UpdateStatusBar(GetTickCount - start, TFSC.Instance.Count, TFSC.Instance.GetItem(0, 0).FFullFileSize);

  AppSettings.Load; // loading settings from registry
  IndexingBitBtn.Hint := 'Folders to  index: ' + AppSettings.FolderToIndex;

  FSearchResults := THArrayG<TSearchResultsItem>.Create(AppSettings.MaxFoundItems + 1);  // default capacity (+1 just in case)
  FSearchResultsCache := TSearchResultsCache.Create(AppSettings.MaxFoundItems + 1);
  FColumnMap := THash<Integer, Integer>.Create; // maps column IDs into default column indexes

  SearchByFileSizeClick(SearchByFileSize); // disable search controls by default
  SearchByModifiedDateClick(SearchByModifiedDate);
  SearchByAttributesClick(SearchByAttributes);

  FSortColumnID := -1; // no sorting by default
  GetSystemImageList;
  var Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 0); // default index for Name column is 0
  Col.Caption := 'Name';
  Col.Width := 300;
  Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 1); // default index for Size column is 1
  Col.Caption := 'Size';
  Col.Width := 100;
  Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 2);
  Col.Caption := 'Type';
	Col.Width := 100;
  Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 3);
  Col.Caption := 'Modified';
	Col.Width := 150;
  Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 4);
  Col.Caption := 'Last Access';
	Col.Width := 150;
  Col := ListView1.Columns.Add;
  FColumnMap.SetValue(Col.ID, 5);
  Col.Caption := 'Path';
	Col.Width := 450;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FIndexingThread);
  //FreeAndNil(FCache);

  // auto save index data only if data was modified (e.g. re-loaded from file system)
  if TFSC.Instance.Modified then
    TFSC.Instance.SerializeTo(INDEX_FILENAME);

  ClearSearchResults;
 // TFSC.Get.RemoveProgressListener(FProgressListener);
  FreeAndNil(FSearchResults);
  FreeAndNil(FSearchResultsCache);
  FreeAndNil(FColumnMap);

end;

class function TMainForm.GetFileShellInfo(FileName: TFileName; Item: TCacheItem): Boolean;
var
  ShFileInfo: TShFileInfo;
begin
  ZeroMemory(@ShFileInfo, SizeOf(ShFileInfo));
  var Res := ShGetFileInfo(PChar(FileName), Item.FFileData.dwFileAttributes, ShFileInfo, SizeOf(ShFileInfo), // Get Windows file name, system file type and icon
  SHGFI_USEFILEATTRIBUTES OR SHGFI_TYPENAME OR SHGFI_DISPLAYNAME OR SHGFI_SYSICONINDEX OR SHGFI_SMALLICON { OR SHGFI_ICON } );

  if Res = 0 then begin // looks like file not found or some other error occurred
    Item.FDisplayName := ExtractFileName(FileName);
    Item.FIconIndex := 0; //ShFileInfo.IIcon;
    Item.FFileType := 'Unknown file type';
  end
  else
  begin
    Item.FDisplayName := ShFileInfo.szDisplayName; // Set the item caption
    Item.FIconIndex := ShFileInfo.IIcon;      // Set file icon index
    Item.FFileType := ShFileInfo.szTypeName;
  end;

  //if Item.FDenied then Item.FIconIndex := 0;

  Result := Res <> 0;
end;

procedure TMainForm.GetSystemImageList;
var
  SysImageList: UInt64;      // temporary handle for System ImageLists
  ShFileInfo: TShFileInfo; // Shell File Info structure
begin
  ListView1.LargeImages := TImageList.Create(self);
  FillChar(ShFileInfo, SizeOf(ShFileInfo), 0);
  SysImageList := ShGetFileInfo(nil, 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysImageList <> 0 then
  begin
    ListView1.LargeImages.Handle := SysImageList;
    ListView1.LargeImages.ShareImages := True; // Avoid freeing of System Image List !
  end;
  ListView1.SmallImages := TImageList.Create(self);
  SysImageList := ShGetFileInfo('', 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysImageList <> 0 then
  begin
    ListView1.SmallImages.Handle := SysImageList;
    ListView1.SmallImages.ShareImages := True; // Avoid freeing of System Image List !
  end;
end;

 {
function TMainForm.GetValue(item: TSearchResultsItem; ColID: Integer): string;
begin
  var ColIndex := FColumnMap[ColID];

	case ColIndex of
    0: Result := item.Caption;
    1: Result := item.SizeStr;
    2: Result := item.FileType;
    3: Result := item.ModifiedStr;
	  4: Result := item.LastAccessStr;
    5: Result := item.Path;
    else Result := 'UNKNOWN';
  end;
end;
  }

{ TMainFormIndexingProgress }

procedure TMainFormIndexingProgress.Finish;
begin
//  MainForm.StatusBar1.Panels[2].Text := Format('FS data load time: %s', [MillisecToStr(SettingsForm1.ExecData.ExecTime)]);
//  MainForm.StatusBar1.Panels[3].Text := Format('Items loaded: %s', [ThousandSep(TFSC.Get.Count)]);
//  MainForm.StatusBar1.Panels[4].Text := Format('Folder size: %s', [ThousandSep(SettingsForm1.ExecData.DirSize)]);
end;

function TMainFormIndexingProgress.Progress(Prgress: Integer): Boolean;
begin
  // do nothing
  Result := True;
end;

procedure TMainFormIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
end;

{ TSearchResultsCache }

constructor TSearchResultsCache.Create(Capacity: Cardinal);
begin
  FData := THArrayG<TSearchResultsItem>.Create(Capacity);
end;

destructor TSearchResultsCache.Destroy;
var
  i: Cardinal;
begin
  for i := 1 to FData.Count do FData[i - 1].Free;
  FreeAndNil(FData);
  inherited Destroy;
end;

function TSearchResultsCache.GetItem: TSearchResultsItem;
begin
  if FData.Count > 0
    then Result := FData.Pop()
    else Result := TSearchResultsItem.Create;
end;

procedure TSearchResultsCache.PutItem(Item: TSearchResultsItem);
begin
  FData.AddValue(Item);
end;

procedure TSearchResultsCache.SetCapacity(Capacity: Cardinal);
begin
  FData.SetCapacity(Capacity);
end;

end.
