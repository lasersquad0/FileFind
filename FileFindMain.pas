unit FileFindMain;

interface

uses
  WinAPI.Windows, System.SysUtils, System.Classes, System.Messaging, WinAPI.Messages,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.Generics.Collections, Vcl.Menus, Vcl.NumberBox, System.ImageList, Vcl.ImgList, Vcl.Imaging.pngimage, Vcl.Mask,
  FileCache, LoadFSThread, DynamicArray, Hash, HistoryEdit, ObjectsCache, Functions, Vcl.AppEvnts;

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
    Denied: Boolean; // flag that entering to this directory is denied during searching
    function IsDirectory: Boolean;
 end;

 {TSearchResultsCache = class
   private
     FData: THArrayG<TSearchResultsItem>;
   public
     function GetItem(): TSearchResultsItem;
     procedure PutItem(Item: TSearchResultsItem);
     procedure SetCapacity(Capacity: Cardinal);
     constructor Create(Capacity: Cardinal);
     destructor Destroy; override;
 end; }

 TMainFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
   FThread: TLoadFSThread;
 public
   constructor Create(Thread: TLoadFSThread);
   procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
   procedure ReportError(ErrorStr: string); override;
 end;

 type
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
    AlertPanel1: TPanel;
    Timer2: TTimer;
    Image1: TImage;
    CancelBtn: TSpeedButton;
    StartSearchFolder: TLabeledEdit;
    SelectFolderButton: TSpeedButton;
    StateImageList: TImageList;
    Copy1: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1DblClick(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure SearchByFileSizeClick(Sender: TObject);
    procedure AdvancedSearchButtonClick(Sender: TObject);
    procedure SearchByModifiedDateClick(Sender: TObject);
    procedure SearchByAttributesClick(Sender: TObject);
    procedure StartSearchBtnClick(Sender: TObject);
    procedure Openfilefolder1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Statistics1Click(Sender: TObject);
    procedure IndexingBitBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SelectFolderButtonClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
  private
    SearchEdit: TSearchEdit;
    FSortColumnID: Integer;
    FInvertSort: Boolean;
    FSearchResults: THArrayG<TSearchResultsItem>;
    FSearchResultsCache: TObjectsCache<TSearchResultsItem>;
    FColumnMap: THash<Integer, Integer>;
    FIndexingThread: TLoadFSThread;
    FProgressListener: TMainFormIndexingProgress;
    FCancelIndexing: Boolean;
    FCancelGetFileInfo: Boolean;
    FFileInfoMessagesCount: Cardinal;

    procedure InitColumns;
    procedure InitSearchEdit;
    procedure OnIndexingThreadTerminate(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
    //procedure ReCreateIndexingThread;
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
    function BuildIndexingBtnHint: string;
    procedure OnFileShellInfo(var Msg: TMessage); message WM_FileShellInfo_MSG;
  public
   // property Cancel: Boolean read FCancel;
  end;



var
  MainForm: TMainForm;


implementation

uses
  System.TypInfo, WinAPI.ShellAPI, WinAPI.CommCtrl, System.UITypes, Vcl.Graphics, Math, DateUtils, ClipBrd,
  SyncObjs, Settings, SettingsForm, IndexingLog, About, StatisticForm;

{$R *.dfm}

/// <summary>During search FileNamesCache call this callback function only for items that successfully passed filter.</summary>
/// <remarks>Define this method to add filtered items into ListView. TCacheItem is a class therefore passed by reference.
/// If you modify Item's fields, new data be stored and available for next searches. </remarks>
///  <param name="FullPath">Full path that includes file name of the file being added</param>
///  <param name="Item">Item that contains other file information: size, modified date etc.</param>
///  <return> Returns False to stop filtering (for example when maximum number of items in ListView is reached). In other cases function should return True.</return>
function TMainForm.OnSearchFile(FullPath: string; Item: TCacheItem): Boolean;
var
 // i: Cardinal;
  ResultsItem: TSearchResultsItem;
begin
   //Result := False;
   if FSearchResults.Count >= AppSettings.MaxFoundItems then Exit(False);  // limit number of found items by value from settings

   ResultsItem := FSearchResultsCache.GetItem;

   if Item.FFileType = '' then  // for each Item GetFileShellInfo will be called only once. Results are cached.
     GetFileShellInfo(FullPath, Item);  // FullPath contains filename too

   if IsDirectory(Item) then begin
     if Item.FDenied then begin
       ResultsItem.Size := 0;
       ResultsItem.SizeStr := 'N/A';
     end else begin
       if AppSettings.HideFoldersSize then begin
         ResultsItem.Size := 0;
         ResultsItem.SizeStr := '-';
       end else begin
         ResultsItem.Size := Item.FFullFileSize;
         ResultsItem.SizeStr := ThousandSep(ResultsItem.Size);
         //ResultsItem.Size := (uint64(Item.FFileData.nFileSizeHigh) shl 32) + uint64(Item.FFileData.nFileSizeLow);
       end;
     end;
   end else begin // file, not a directory
     ResultsItem.Size := Item.FFullFileSize;
     ResultsItem.SizeStr := ThousandSep(ResultsItem.Size);
   end;

   ResultsItem.FileAttr := Item.FFileData.dwFileAttributes;
   ResultsItem.Modified := Item.FFileData.ftLastWriteTime;
   ResultsItem.ModifiedStr := GetLocalTime(Item.FFileData.ftLastWriteTime);
   ResultsItem.LastAccessStr := GetLocalTime(Item.FFileData.ftLastAccessTime);
   ResultsItem.LastAccess := Item.FFileData.ftLastAccessTime;
   ResultsItem.Path := FullPath;
   ResultsItem.IIcon := Item.FIconIndex;
   ResultsItem.FileType := Item.FFileType;
   ResultsItem.Caption := Item.FDisplayName;
   ResultsItem.Denied := Item.FDenied;

   FSearchResults.AddValue(ResultsItem);

   Result := True;
end;

procedure TMainForm.OnFileShellInfo(var Msg: TMessage);
var
  item, TmpItem: TCacheItem;
  cnt: Cardinal;
begin
  TmpItem := TCacheItem(Msg.WParam);
 // LogMessage('[OnFileShellInfo] GetItem('+ IntToStr(TmpItem.FLevel) + ', '+ IntToStr(Msg.LParam) + ')');

  item := TFSC.Instance.GetItem(TmpItem.FLevel, Msg.LParam);
  item.FDisplayName := TmpItem.FDisplayName;
  item.FFileType := TmpItem.FFileType;
  item.FIconIndex := TmpITem.FIconIndex;
  TmpItem.Free;

  cnt := TFSC.Instance.Count;
  Inc(FFileInfoMessagesCount);
  StatusBar1.Panels[3].Text := Format('Items loaded: %s (%d%%)', [ThousandSep(cnt), FFileInfoMessagesCount*100 div cnt]);
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

procedure TMainForm.Timer2Timer(Sender: TObject);
//var
//  iFileDate: TDateTime;
//  Days: Integer;
begin
  // after 14 days we ask to refresh index (if not done)
  //iFileDate := TFSC.Instance.IndexFileDate;
  //days := DaysBetween(Now(), iFileDate);
  AlertPanel1.Visible := DaysBetween(Now(), TFSC.Instance.IndexFileDate) >= 14;
end;

procedure TMainForm.MakeSearch();
var
  Filter: TSearchFilter;
  SearchResult: TSearchResult;
begin
  SearchEdit.ACStrings.Add(Trim(SearchEdit.Text)); // updating search history only when actual search triggered

  Screen.Cursor := crHourGlass;

  try
    ListView1.Items.BeginUpdate;

    var start := GetTickCount;

    ListView1.Items.Count := 0;
    ClearSearchResults;
    //ClearSorting; //FSortColumnID := -1;  // reset sorting column
    //FInvertSort := False;
    Filter.StartFrom := Trim(StartSearchFolder.Text);
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

    SearchResult := TFSC.Instance.Search(Filter, OnSearchFile); // cache is global singleton

    DoSort;

    ListView1.Items.Count := Integer(FSearchResults.Count);

    var stop := GetTickCount;

    StatusBar1.Panels[0].Text := Format('  Found items: %u', [ListView1.Items.Count]);
    StatusBar1.Panels[1].Text := Format('Search time: %s', [MillisecToStr(stop - start)]);
    if SearchResult = srWrongPath then MessageDlg('Search folder ''' + Filter.StartFrom + ''' is not in search index. Please rebuild index that includes required search folder.', TMsgDlgType.mtWarning, [mbOK], 0);

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
  FSearchResults.QuickSort(CompareData);
  //var stop := GetTickCount;
  //StatusBar1.Panels[1].Text := Format('Search time: %s', [MillisecToStr(stop - start)]);
  ListView1.Refresh;
end;

{
function DrawStateToString(State: TCustomDrawState): string;
var
  i: TCustomDrawStateItem;
begin
  Result := '[';
  for i := Low(TCustomDrawStateItem) to High(TCustomDrawStateItem) do begin
    if i in State then Result := Result + GetEnumName(TypeInfo(TCustomDrawStateItem), Ord(i)) + ',';
  end;
  Result := Result + ']';
end;

function RectToString(rect: TRect): string;
begin
  Result := Format('(l=%d, t=%d, r=%d, b=%d, w=%d, h=%d)', [rect.Left, rect.Top, rect.Right, rect.Bottom, rect.Width, rect.Height]);
end;
 }
var SplitArray: THArrayG<SplitRec>; // optimization for marking bold search term during drawing ListView

procedure TMainForm.ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  ItemRect: TRect;
  sz: TSize;
  oldFont: TFontStyles;
  oldBrush: TColor;
  retOldBrush: Boolean;
  srchStr, tmpStr: string;
  i: Cardinal;
const
  HoverClr = TColor($FFF3E5);
  SelectClr = TColor($FFE8CC);

begin
  if Stage = cdPostPaint then begin

   srchStr := SearchEdit.Text;
   SplitByString(Item.Caption, srchStr, SplitArray);
   if SplitArray.Count = 0 then Exit;  // SplitArray.Count can be =0 when we doing search with wildcards.

    retOldBrush := False;

    if cdsFocused in State then begin
      oldBrush := Sender.Canvas.Brush.Color;
      Sender.Canvas.Brush.Color := SelectClr; //TColors.Aliceblue;
      retOldBrush := True;
    end
    else
    if cdsHot in State then begin
      oldBrush := Sender.Canvas.Brush.Color;
      Sender.Canvas.Brush.Color := HoverClr;
      retOldBrush := True;
    end;

    ItemRect := Item.DisplayRect(drLabel);

    //ListView_GetItemRect(Sender.Handle, Item.Index, ItemRect, LVIR_LABEL);

    ItemRect.Inflate(-2, -2);

    for i := 1 to SplitArray.Count do begin
      tmpStr := SplitArray[i - 1].str;
      if SplitArray[i - 1].flag = False then begin
        Sender.Canvas.TextRect(ItemRect, ItemRect.Left, ItemRect.Top, tmpStr);
        sz := Sender.Canvas.TextExtent(tmpStr);
        ItemRect.Left := ItemRect.Left + sz.cx;
      end else begin
        oldFont := Sender.Canvas.Font.Style;
        Sender.Canvas.Font.Style := [fsBold];
        sz := Sender.Canvas.TextExtent(tmpStr);
        Sender.Canvas.TextRect(ItemRect, ItemRect.Left, ItemRect.Top, tmpStr);
        Sender.Canvas.Font.Style := oldFont;
        ItemRect.Left := ItemRect.Left + sz.cx;
      end;
      if ItemRect.Left >= ItemRect.Right then break;
    end;

    if retOldBrush then begin
      Sender.Canvas.Brush.Color := oldBrush;
    end;

  end;

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
begin
  var item1 := arr[i];
  var item2 := arr[j];

  Result := CompareData2(item1, item2);
end;

function TMainForm.CompareData2(item1, item2: TSearchResultsItem): Integer;
begin
  Result := 0; // Defaults to equal

  var ColIndex := FColumnMap[FSortColumnID];

   if AppSettings.FoldersOnTop AND item1.IsDirectory() AND NOT item2.IsDirectory()
     then Result := -1 // directory is always 'greater' than file
   else if AppSettings.FoldersOnTop AND NOT item1.IsDirectory() AND item2.IsDirectory()
     then Result := 1
   else begin
     if AppSettings.CaseSensitiveSort then begin // case sensitive comparison
       case ColIndex of
         0: Result := CompareStr(item1.Caption, item2.Caption);
         1: if item1.Size > item2.Size then Result := 1
              else if item1.Size < item2.Size then Result := -1;
         2: Result := CompareStr(item1.FileType, item2.FileType);
         3: Result := CompareFileTime(item1.Modified, item2.Modified);
         4: Result := CompareFileTime(item1.LastAccess, item2.LastAccess);
         5: Result := CompareStr(item1.Path, item2.Path);
          // else Result := 'UNKNOWN';
        end;
      end else begin // case INsensitive comparison
        case ColIndex of
          0: Result := CompareText(item1.Caption, item2.Caption);
          1: if item1.Size > item2.Size then Result := 1
             else if item1.Size < item2.Size then Result := -1;
          2: Result := CompareText(item1.FileType, item2.FileType);
          3: Result := CompareFileTime(item1.Modified, item2.Modified);
          4: Result := CompareFileTime(item1.LastAccess, item2.LastAccess);
          5: Result := CompareText(item1.Path, item2.Path);
          // else Result := 'UNKNOWN';
        end;
      end;
    end;

  // invert Sort if requested
  if FInvertSort then Result := -Result;
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  if ListView1.Selected = nil then exit;
  var item := FSearchResults[Cardinal(ListView1.Selected.Index)];

  Clipboard.AsText := item.Path;
end;

procedure TMainForm.ListView1Data(Sender: TObject; Item: TListItem);
var
  origItem: TSearchResultsItem;
begin
  origItem := FSearchResults[Cardinal(Item.Index)];

  Item.Caption := origItem.Caption;
  Item.ImageIndex := origItem.IIcon;

  if origItem.Denied
    then item.StateIndex := 0 // mark denied items in the list
    else item.StateIndex := -1;


  Assert(Item.SubItems.Count = 0);

  Item.SubItems.Add(origItem.SizeStr);
  Item.SubItems.Add(origItem.FileType);
  Item.SubItems.Add(origItem.ModifiedStr);
  Item.SubItems.Add(origItem.LastAccessStr);
  Item.SubItems.Add(origItem.Path);
end;

procedure TMainForm.ListView1DblClick(Sender: TObject);
var
  path: string;
  res: UInt64;
begin
  if ListView1.Selected = nil then exit;

  var item := FSearchResults[Cardinal(ListView1.Selected.Index)];
  if item.IsDirectory
    then path := item.Path // this is directory
    else path := ExtractFilePath(item.Path); // this is file, open its directory

  res := ShellExecute(Handle, 'explore', PChar(path), nil, nil, SW_SHOWNORMAL);
  if res < 33 then MessageDlg('ShellExecute error: ' + res.ToString, TMsgDlgType.mtError, [mbOK], 0);
end;

procedure TMainForm.OnIndexingThreadTerminate(Sender: TObject);
begin
  TFSC.Instance.RemoveProgressListener(FProgressListener);
  FreeAndNil(FProgressListener);

  ListView1.Items.Count := 0; // reset search results to zero
  ClearSearchResults;

  Settings1.Enabled := True; // main menu item
  AlertPanel1.Visible := False; // hide alert panel if it was visible
  IndexingBitBtn.Hint := BuildIndexingBtnHint;
  TFSC.Instance.SerializeTo(INDEX_FILENAME); // save loaded data into .idx file

  UpdateStatusBar(FIndexingThread.ExecData.ExecTime, TFSC.Instance.Count, FIndexingThread.ExecData.DirSize {TFSC.Instance.GetItem(0, 0).FFullFileSize});

  FFileInfoMessagesCount := 0;
  TInterlocked.Exchange(FCancelGetFileInfo, False); // reset stop flag
  TFileShellInfoThread.RunGetShellInfoBgThread(self.Handle, @FCancelGetFileInfo); // start getting File infos after IndexDB refresh
end;

procedure TMainForm.CancelBtnClick(Sender: TObject);
begin
  // because other threads read this variable as a signal to stop execution
  TInterlocked.Exchange(FCancelIndexing, mrYes = MessageDlg('Are you sure you want to cancel indexing?', TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0, mbYes));
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

procedure TMainForm.SelectFolderButtonClick(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := StartSearchFolder.Text;
  if FileOpenDialog1.Execute then StartSearchFolder.Text := FileOpenDialog1.FileName;
end;

procedure TMainForm.Settings1Click(Sender: TObject);
begin
  if SettingsForm1.ShowModal = mrOk then begin
    ListView1.Items.Count := 0; // reset search results to zero
    ClearSearchResults;
    IndexingBitBtn.Hint := BuildIndexingBtnHint;
    SearchEdit.ACEnabled := AppSettings.EnableSearchHistory;
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
    SearchPanel.Height := SearchPanel.Height * 5 div 2
  end else begin                          // close adv search panel
    AdvancedSearchButton.ImageIndex := 0;
    SearchPanel.Height := SearchPanel.Height * 2 div 5;
    SearchByFileSize.Checked := False;  // uncheck (make inactive) all search critetias to avoid misunderstanding
    SearchByModifiedDate.Checked := False;
    SearchByAttributes.Checked := False;
  end;
end;

var ItemRefSHInfo: TCacheItemRef;

function TMainForm.BuildIndexingBtnHint: string;
begin
  Result := 'Folders to index: ' + AppSettings.FolderToIndex + sLineBreak +
            'IndexDB updated on ' + DateTimeToStr(TFSC.Instance.IndexFileDate);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  AlertPanel1.Visible := NOT AlertPanel1.Visible;
end;

procedure TMainForm.IndexingBitBtnClick(Sender: TObject);
begin
{$IFDEF PROFILING}
  TFSC.Instance.ReadFileSystem(AppSettings.FolderToIndex);
{$ELSE}
  TInterlocked.Exchange(FCancelGetFileInfo, True); // stop GetFileInfo thread if it is running
  Sleep(200);

  TInterlocked.Exchange(FCancelIndexing, False); // because other threads read this variable as a signal to stop execution
  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FProgressListener := TMainFormIndexingProgress.Create(FIndexingThread);
  TFSC.Instance.AddProgressListener(FProgressListener);

  FIndexingThread.OnTerminate := OnIndexingThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  FIndexingThread.ProgressBar := ProgressBar1;
  FIndexingThread.ExecData.StartDir := AppSettings.FolderToIndex;

  Settings1.Enabled := False;  // main menu item
  FIndexingThread.Start([StartSearchBtn, SearchEdit, IndexingBitBtn], [CancelBtn, ProgressBar1, ProgressLabel], []);
{$ENDIF}
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FProgressListener)
    then MessageDlg('Cannot close form because indexing is in progress.', TMsgDlgType.mtInformation, [mbOK], 0);

  CanClose := NOT Assigned(FProgressListener);

  if CanClose then begin
    TInterlocked.Exchange(FCancelGetFileInfo, True); // because other threads read this variable as a signal to stop execution
    Sleep(200); // give other threads chance to stop executing
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  var start := GetTickCount;

  ProgressBar1.Parent := StatusBar1;
  ProgressBar1.Width := 100;
  ProgressBar1.Height := 17;
  ProgressBar1.Left := StatusBar1.Width - ProgressBar1.Width - 20 - 60; // 60 for cancel button
  ProgressBar1.Top := 4;

  CancelBtn.Parent := StatusBar1;
  CancelBtn.Left := StatusBar1.Width - CancelBtn.Width - 15;
  CancelBtn.Top := 2;

  ProgressLabel.Parent := StatusBar1;
  ProgressLabel.Width := 110;
  ProgressLabel.Height := 17;
  ProgressLabel.Top := 5;
  ProgressLabel.Left := ProgressBar1.Left - ProgressLabel.Width;
  ProgressLabel.Caption := 'Indexing progress...';

{$IFNDEF PROFILING}
  TFSC.Instance.DeserializeFrom(INDEX_FILENAME);
{$ENDIF}
  TFSC.Instance.Modified := False;

  AppSettings.Load; // loading settings from registry
  IndexingBitBtn.Hint := BuildIndexingBtnHint;
  StartSearchFolder.Text := AppSettings.FolderToIndex;

  FSearchResults := THArrayG<TSearchResultsItem>.Create(AppSettings.MaxFoundItems + 1);  // default capacity (+1 just in case)
  FSearchResultsCache := TObjectsCache<TSearchResultsItem>.Create(AppSettings.MaxFoundItems + 1, False);
  FColumnMap := THash<Integer, Integer>.Create; // maps column IDs into default column indexes

  SearchByFileSizeClick(SearchByFileSize); // disable search controls by default
  SearchByModifiedDateClick(SearchByModifiedDate);
  SearchByAttributesClick(SearchByAttributes);

  InitSearchEdit; // should be called after AppSettings.Load;
  InitColumns;

  if TFSC.Instance.Count > 0 then begin
    FFileInfoMessagesCount := 0;
    TInterlocked.Exchange(FCancelGetFileInfo, False); // reset flag begore starting bg thread
    TFileShellInfoThread.RunGetShellInfoBgThread(self.Handle, @FCancelGetFileInfo);

    // measure time of all app loading steps here
    UpdateStatusBar(GetTickCount - start, TFSC.Instance.Count, TFSC.Instance.GetItem(0, 0).FFullFileSize);
  end;

  //ListView_SetTextBkColor(ListView1.Handle, CLR_NONE); // I donot know how it works but it needed to properly repaint listview rows when active row is changes
  //temporarily - remove it.
  // UpdateStatusBar(GetTickCount - start, TFSC.Instance.Count, TFSC.Instance.GetItem(0, 0).FFullFileSize);

end;

procedure TMainForm.InitSearchEdit();
begin
  SearchEdit := TSearchEdit.Create(self);
  SearchEdit.Parent := SearchPanel;
  SearchEdit.LabelPosition := lpLeft;
  SearchEdit.Left := 114;
  SearchEdit.Top := 33;
  SearchEdit.Width := 389;
  SearchEdit.Height := 23;
  SearchEdit.OnChange := SearchEditChange;
  SearchEdit.OnKeyPress := SearchEditKeyPress;
  SearchEdit.Hint := 'Enter your search here';
  SearchEdit.ShowHint := True;

  SearchEdit.ACEnabled := AppSettings.EnableSearchHistory;
  SearchEdit.ACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
  SearchEdit.ACSource := acsList;
end;

procedure TMainForm.InitColumns();
begin
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
{$IFNDEF PROFILING}
  // auto save index data only if data was modified (e.g. re-loaded from file system)
  if TFSC.Instance.Modified then
    TFSC.Instance.SerializeTo(INDEX_FILENAME); // loaded data is saved into index file right after loading. perhaps we do not need these 3 lines any more.
  ClearSearchResults;
{$ENDIF}

  FreeAndNil(FSearchResults);
  FreeAndNil(FSearchResultsCache);
  FreeAndNil(FColumnMap);

  AppSettings.SearchHistory.Assign(SearchEdit.ACStrings);
  AppSettings.Save; // saving updated search history data
  SearchEdit.Free;
end;

procedure TMainForm.GetSystemImageList;
var
  SysImageList: UInt64;      // temporary handle for System ImageLists
  ShFileInfo: TShFileInfo; // Shell File Info structure
begin
  ListView1.LargeImages := TImageList.Create(self);
  FillChar(ShFileInfo, SizeOf(ShFileInfo), 0);
  SysImageList := ShGetFileInfo('', 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
  if SysImageList <> 0 then begin
    ListView1.LargeImages.Handle := SysImageList;
    ListView1.LargeImages.ShareImages := True; // Avoid freeing of System Image List !
  end;

  ListView1.SmallImages := TImageList.Create(self);
  SysImageList := ShGetFileInfo('', 0, ShFileInfo, SizeOf(ShFileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SysImageList <> 0 then begin
    ListView1.SmallImages.Handle := SysImageList;
    ListView1.SmallImages.ShareImages := True; // Avoid freeing of System Image List !
  end;
end;

{ TMainFormIndexingProgress }

constructor TMainFormIndexingProgress.Create(Thread: TLoadFSThread);
begin
  inherited Create;
  FThread := Thread;
end;

procedure TMainFormIndexingProgress.Finish;
begin
   // nothing to do here
end;

function TMainFormIndexingProgress.Progress(Prgress: Integer): Boolean;
var
  b: Boolean;
begin
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
     // logrithmic progress since we do not know total progress value
      MainForm.ProgressBar1.Position := MainForm.ProgressBar1.Position + (MainForm.ProgressBar1.Max - MainForm.ProgressBar1.Position) div 20;
      b := NOT MainForm.FCancelIndexing;
   end
   );
  Result := b;
end;

procedure TMainFormIndexingProgress.ReportError(ErrorStr: string);
begin

end;

procedure TMainFormIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
end;

{ TSearchResultsCache }

{constructor TSearchResultsCache.Create(Capacity: Cardinal);
var i: Cardinal;
begin
  FData := THArrayG<TSearchResultsItem>.Create(Capacity);

  // fill with cached values
  for i := 0 to Capacity - 1 do FData.AddValue(TSearchResultsItem.Create);
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
//var i: Cardinal;
begin
  FData.SetCapacity(Capacity);
end;
 }


initialization
  SplitArray := THArrayG<SplitRec>.Create();
finalization
  FreeAndNil(SplitArray);
end.
