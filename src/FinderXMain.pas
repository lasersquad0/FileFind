unit FinderXMain;

interface

uses
  WinAPI.Windows, System.SysUtils, System.Classes, System.Messaging, WinAPI.Messages,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.ExtCtrls,
  System.Generics.Collections, Vcl.Menus, Vcl.NumberBox, System.ImageList, Vcl.ImgList, Vcl.Imaging.pngimage, Vcl.Mask,
  FileCache, LoadFSThread, DynamicArray, Hash, HistoryEdit, ObjectsCache, Functions, CacheItem, Vcl.AppEvnts;

type
  TSearchResultsItem = class
  public
    Size: UInt64;
    SizeStr: string;
    ModifiedStr: string;
    LastAccessStr: string;
    CreatedStr: string;
    AttrStr: string;
    Path: string;
    Item: TCacheItem;
    function IsDirectory: Boolean;
 end;

 TMainFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
   FThread: TLoadFSThread;
 public
   constructor Create(Thread: TLoadFSThread);
   procedure Start(Notes: string; P100: Integer = -1); override; // defines Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function  Progress(Prgress: Integer): Boolean; override; // Result=False - stop process if indexing takes too long time
   procedure ReportError(Error: TError); override;
 end;

 type
  TMainForm = class(TForm)
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    ExitAppMenuItem: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    AboutMenuItem: TMenuItem;
    SettingsMenuItem: TMenuItem;
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
    SearchByAttributes: TCheckBox;
    AttrArchive: TCheckBox;
    AttrHidden: TCheckBox;
    AttrDirectory: TCheckBox;
    AttrEncrypted: TCheckBox;
    AttrCompressed: TCheckBox;
    AttrReadonly: TCheckBox;
    AttrSystem: TCheckBox;
    SearchBtn: TBitBtn;
    PopupMenu1: TPopupMenu;
    Openfilefolder1: TMenuItem;
    StatisticsMenuItem: TMenuItem;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    IndexingBitBtn: TBitBtn;
    AlertPanel1: TPanel;
    Timer2: TTimer;
    Image1: TImage;
    CancelBtn: TSpeedButton;
    StateImageList: TImageList;
    CopyFolder: TMenuItem;
    PopupMenu2: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    TrayIcon1: TTrayIcon;
    PopupMenuTray: TPopupMenu;
    OpenMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    ApplicationEvents1: TApplicationEvents;
    ProgressBarFileInfo: TProgressBar;
    DeleteIndex: TMenuItem;
    CheckIntegrity: TMenuItem;
    DateTypeComboBox: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1DblClick(Sender: TObject);
    procedure SettingsMenuItemClick(Sender: TObject);
    procedure SearchByFileSizeClick(Sender: TObject);
    procedure AdvancedSearchButtonClick(Sender: TObject);
    procedure SearchByModifiedDateClick(Sender: TObject);
    procedure SearchByAttributesClick(Sender: TObject);
    procedure SearchBtnClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure StatisticsMenuItemClick(Sender: TObject);
    procedure IndexingBitBtnClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CopyFolderClick(Sender: TObject);
    procedure ListView1AdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure Openfilefolder1Click(Sender: TObject);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure MenuItem8Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure ApplicationEvents1Minimize(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure ExitAppMenuItemClick(Sender: TObject);
    procedure DeleteIndexClick(Sender: TObject);
    procedure CheckIntegrityClick(Sender: TObject);
  private
    SearchEdit: TSearchEdit;
    FSortColumnID: Integer;
    FInvertSort: Boolean;
    FSearchResults: THArrayG<TSearchResultsItem>;
    FSearchResultsCache: TObjectsCache<TSearchResultsItem>;
    FIndexingThread: TLoadFSThread;
    FProgressListener: TMainFormIndexingProgress;
    FCancelIndexing: Boolean;
    FFileInfoMessagesCount: Cardinal;
    FSearchResultsFileInfoThread: TThread;

    procedure StoreColumns;
    procedure InitColumns;
    procedure InitSearchEdit;
    procedure OnIndexingThreadTerminate(Sender: TObject);
    procedure SearchEditEnter(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char);
    procedure GetSystemImageList;
    procedure ClearSearchResults;
    procedure ClearSortingMarks;
    procedure DoSort;
    function CompareData(arr : THArrayG<TSearchResultsItem>; i, j: Cardinal): Integer;
    function CompareData2(item1, item2: TSearchResultsItem): Integer;
   // function GetValue(item: TSearchResultsItem; ColID: Integer): string;
    procedure UpdateStatusBarPanelsWidth;
    procedure UpdateStatusBarX(FoundItems, Percent: Cardinal);
    procedure UpdateStatusBarXX(FoundItems, SearchTime: Cardinal);
    procedure UpdateStatusBarXXX(ExecData: TArray<TVolumeExecData>);
    function OnFileFound(FullPath: string; Item: TCacheItem): Boolean;
    function GetFileSizeOp(): TFileSizeCompare;
    function GetFileSizeFactor(): Cardinal;
    function GetAttributes: Cardinal;
    procedure MakeSearch;
    function BuildIndexingBtnHint: string;
    procedure RestoreMainForm();
    procedure StartIndexing(bg: Boolean);

    procedure OnSearchResultsShellInfo(var Msg: TMessage); message WM_SearchResultsShellInfo_MSG;
    procedure OnRestoreFormRemote(var Msg: TMessage); message WM_RESTORE_MAINFORM_MSG;
    procedure OnDEVICECHANGE(var Msg: TMessage); message WM_DEVICECHANGE;
  end;

type
  TSearchResultsShellInfoThread = class(TThread)
  protected
    procedure Execute; override;
  public
    //procedure Start(WinHandle: THandle; CancelFlag: PBoolean; lvStart: Cardinal = 0; lvEnd: Cardinal = 0); overload;
    class function CreateAndRun(): TThread;
  end;


var
  MainForm: TMainForm;


implementation

uses
  System.TypInfo, WinAPI.ShellAPI, WinAPI.CommCtrl, System.UITypes, Vcl.Graphics, Math, DateUtils, ClipBrd,
  SyncObjs, ActiveX, Settings, SettingsForm, {IndexingLog,} About, StatisticForm;

{$R *.dfm}

// builds string representation of file size adds suffixes KB, MB, GB depending of the value
function MakeSizeStr(Size: UInt64): string;
begin
  case AppSettings.SizeFormat of
    sfAuto: begin
              if Size <= 10000-1 then Result := ThousandSep(Size)
              else if Size <= 10000*1024-1 then Result := ThousandSep(Size div 1024) + ' KB'
              else if Size <= UInt64(10000)*1024*1024 - 1 then Result := ThousandSep(Size div 1024 div 1024) + ' MB'
              else Result := ThousandSep(Size div 1024 div 1024 div 1024) + ' GB'
            end;
    sfBytes: begin Result := ThousandSep(Size); end;
    sfKilobytes: begin Result := ThousandSep(Size div 1024) + ' KB'; end;
    sfMegabytes: begin Result := ThousandSep(Size div 1024 div 1024) + ' MB'; end;
    else Result := 'unknown';
  end;

end;

/// <summary>During search FileCache calls this callback function only for items that successfully passed filter.</summary>
/// <remarks>Define this method to add filtered items into ListView. TCacheItem is a class therefore passed by reference.
/// If you modify Item's fields, new data be stored and available for next searches. </remarks>
///  <param name="FullPath">Full path that includes file name of the file being added</param>
///  <param name="Item">Item that contains other file information: size, modified date etc.</param>
///  <return> Returns False to stop filtering (for example when maximum number of items in ListView is reached). In other cases function should return True.</return>
procedure TMainForm.OnDEVICECHANGE(var Msg: TMessage);
begin
  TLogger.LogFmt('DEVICECHANGE detected: wparam: %d', [Msg.WParam]);
end;

function TMainForm.OnFileFound(FullPath: string; Item: TCacheItem): Boolean;
var
  ResultsItem: TSearchResultsItem;
begin
   if FSearchResults.Count >= AppSettings.MaxFoundItems then Exit(False);  // limit number of found items by value from settings

   ResultsItem := FSearchResultsCache.GetItem;

   //if Item.FFileType = '' then  // for each Item GetFileShellInfo will be called only once. Results are cached.
     //GetFileShellInfo(FullPath, Item);  // FullPath contains filename too

   if Item.IsDirectory then begin
     if Item.FDenied then begin
       ResultsItem.Size := 0;
       ResultsItem.SizeStr := 'N/A';
     end else begin
       if AppSettings.HideFoldersSize then begin
         ResultsItem.Size := 0;
         ResultsItem.SizeStr := '-';
       end else begin
         ResultsItem.Size := Item.FFileSize;
         ResultsItem.SizeStr := MakeSizeStr(ResultsItem.Size); //ThousandSep(ResultsItem.Size);
         //ResultsItem.Size := (UInt64(Item.FFileData.nFileSizeHigh) shl 32) + UInt64(Item.FFileData.nFileSizeLow);
       end;
     end;
   end else begin // file, not a directory
     ResultsItem.Size := Item.FFileSize;
     ResultsItem.SizeStr := MakeSizeStr(ResultsItem.Size); //ThousandSep(ResultsItem.Size);
   end;

   Item.FDisplayName := Item.FFileName;
   //Item.FIconIndex := 1; // default icon until proper icon is loaded via GetFileShellInfo

   ResultsItem.Item := Item;

   ResultsItem.ModifiedStr := GetLocalTime(Item.FModifiedTime); //TODO: calc and store local time in TCacheItem for performance puporses
   ResultsItem.LastAccessStr := GetLocalTime(Item.FLastAccessTime);
   ResultsItem.CreatedStr := GetLocalTime(Item.FCreationTime);
   ResultsItem.AttrStr := AttrStr2(Item.FFileAttrs);
   ResultsItem.Path := FullPath;

   FSearchResults.AddValue(ResultsItem);

   Result := True;
end;

// processes WM_SearchResultsShellInfo_MSG messages received from another thread.
procedure TMainForm.OnSearchResultsShellInfo(var Msg: TMessage);
var
  TmpItem: TCacheItem;
  resItem: TSearchResultsItem;
  cnt, index: Cardinal;
begin
  index := Cardinal(Msg.LParam); // index of item in current FSearchResults
  TmpItem := TCacheItem(Msg.WParam); // can be nil for the last message only

  if Assigned(TmpItem) then begin
    if index < FSearchResults.Count then begin
      // LogMessage('[OnSearchResultsShellInfo] GetItem('+ IntToStr(TmpItem.FLevel) + ', '+ IntToStr(Msg.LParam) + ')');
      resItem := FSearchResults[index];

      resItem.Item.FDisplayName := TmpItem.FDisplayName;
      resItem.Item.FFileType := TmpItem.FFileType;
      resItem.Item.FIconIndex := TmpITem.FIconIndex;
      resItem.Item.FDenied := TmpITem.FDenied;
    end;

    TmpItem.Free;

    cnt := FSearchResults.Count;
    cnt := IfThen(cnt = 0, 1, cnt); // to avoid division by zero
    Inc(FFileInfoMessagesCount);
    ProgressBarFileInfo.Position := index*100 div cnt;
    UpdateStatusBarX(cnt, FFileInfoMessagesCount*100 div cnt);
  end
  else
  begin
    ProgressBarFileInfo.Position := ProgressBarFileInfo.Max;
    Application.ProcessMessages; // chance to show 100% progress
    ProgressBarFileInfo.Visible := False;
  end;
end;

procedure TMainForm.OnRestoreFormRemote(var Msg: TMessage);
begin
  RestoreMainForm;
end;

procedure TMainForm.Openfilefolder1Click(Sender: TObject);
begin
  ListView1DblClick(nil);
end;

procedure TMainForm.PopupMenu2Popup(Sender: TObject);
var
  i: Cardinal;
begin
  for i := 0 to High(AppSettings.ColumnInfos) do
    PopupMenu2.Items[Ord(AppSettings.ColumnInfos[i].ColType)].Checked := AppSettings.ColumnInfos[i].Visible;
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
var
  iFileDate: TDateTime;
  Days: Integer;
begin
  // after 14 days we ask to refresh index (if not done)
  iFileDate := TCache.Instance.IndexFileSaveDate;
  days := DaysBetween(Now(), iFileDate);
  AlertPanel1.Visible := days >= 14;
end;

procedure TMainForm.RestoreMainForm();
begin
   // show the window, setting its state property to wsNormal.
  Show();
  WindowState := wsNormal;
  Application.BringToFront();
end;

procedure TMainForm.TrayIcon1DblClick(Sender: TObject);
begin
   RestoreMainForm;
end;

procedure TMainForm.MakeSearch();
var
  Filter: TSearchFilter;
  SearchResult: TSearchResult;
begin
  if Assigned(FSearchResultsFileInfoThread) then FSearchResultsFileInfoThread.Terminate; // ask thread to terminate

  SearchEdit.ACStrings.Add(Trim(SearchEdit.Text)); // updating search history only when actual search triggered

  Screen.Cursor := crHourGlass;

  try
    ListView1.Items.BeginUpdate;

    var start := GetTickCount;

    ListView1.Items.Count := 0;
    //ClearSorting; //FSortColumnID := -1;  // reset sorting column
    //FInvertSort := False;
    //Filter.StartFrom := Trim(StartSearchFolder.Text);

    // fields ExactSearch and SearchStrUpper  are filled in TCache.Instance.Search() function
    Filter.SearchStr := Trim(SearchEdit.Text);
    Filter.CaseSensitive := AppSettings.CaseSensitiveSearch;
    Filter.SearchByFileSize := SearchByFileSize.Checked;
    Filter.FileSize := UInt64(SearchFileSize.ValueInt) * UInt64(GetFileSizeFactor());
    Filter.FileSizeCmpType := GetFileSizeOp();
    //Filter.SearchByModifiedDate := SearchByModifiedDate.Checked;
    Filter.SearchByDateType := TSearchDateType(DateTypeComboBox.ItemIndex);
    Filter.DateFrom := DateTimeToFileTime(DateTimePickerFrom.DateTime);
    Filter.DateTo := DateTimeToFileTime(DateTimePickerTo.DateTime);
    Filter.SearchByAttributes := SearchByAttributes.Checked;
    Filter.Attributes := GetAttributes();

    if Assigned(FSearchResultsFileInfoThread) then FSearchResultsFileInfoThread.WaitFor; // wait to thread to termnate
    ClearSearchResults; // must be after .WairFor call

    SearchResult := TCache.Instance.Search(Filter, OnFileFound); // actual search

    DoSort;

    ListView1.Items.Count := Integer(FSearchResults.Count);

    if (SearchResult = srOK) OR (SearchResult = srCancelled) then begin
      if Assigned(FSearchResultsFileInfoThread) then FSearchResultsFileInfoThread.Free; // free previous instance of bg thread
      FFileInfoMessagesCount := 0;
      ProgressBarFileInfo.Position := 0;
      ProgressBarFileInfo.Visible := True;
      FSearchResultsFileInfoThread := TSearchResultsShellInfoThread.CreateAndRun(); // start getting File infos after search
    end;

    var stop := GetTickCount;
    UpdateStatusBarXX(ListView1.Items.Count, stop - start);

    //if SearchResult = srWrongPath then MessageDlg('Search folder ''' + Filter.StartFrom + ''' is not in search index. Please rebuild index that includes required search folder.', TMsgDlgType.mtWarning, [mbOK], 0);

  finally
    Screen.Cursor := crDefault;
    ListView1.Items.EndUpdate;
    ListView1.Repaint;
  end;
end;

procedure TMainForm.MenuItem8Click(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
  col: TListColumn;
begin
  col := nil;
  item := Sender as TMenuItem;
  for i := 0 to ListView1.Columns.Count - 1 do
    if ListView1.Columns[i].Tag = item.Tag then begin col := ListView1.Columns[i]; break; end;

  if col <> nil then begin
    if item.Checked
      then col.Width := 0
      else col.Width := 50;
  end else MessageDlg('Column not found.', TMsgDlgType.mtWarning, [mbOK], 0);

  item.Checked := NOT item.Checked;

  StoreColumns;
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

procedure TMainForm.DeleteIndexClick(Sender: TObject);
begin
   if mrYes = MessageDlg('Are you sure you want to delete index file (' + AppSettings.IndexFileName + ')?', TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0, mbYes) then begin
     DeleteFile(AppSettings.IndexFileName);
     ListView1.Items.Count := 0; // reset search results to zero
     ClearSearchResults;
     TCache.Instance.Clear; // clear all volumes data
     IndexingBitBtn.Hint := BuildIndexingBtnHint;
     UpdateStatusBarXXX(TCache.Instance.GetExecData);
   end;
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

procedure TMainForm.ExitAppMenuItemClick(Sender: TObject);
begin
  Close;
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
  if NOT AppSettings.HighlightSearchTerms then Exit;

  if Stage = cdPostPaint then begin

   srchStr := Trim(SearchEdit.Text);
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

  if FSortColumnID = Column.Tag then
    FInvertSort := NOT FInvertSort
  else begin
    FSortColumnID := Integer(Column.Tag);
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

procedure TMainForm.ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  HeaderRect: TRect;
  Pos: TPoint;
begin
  GetWindowRect(ListView_GetHeader(ListView1.Handle), HeaderRect);
  Pos := ListView1.ClientToScreen(MousePos);
  if PtInRect(HeaderRect, Pos) then
    PopupMenu2.Popup(Pos.X, Pos.Y)
  else
    PopupMenu1.Popup(Pos.X, Pos.Y);
end;

function TSearchResultsItem.IsDirectory: Boolean;
begin
   IsDirectory := Item.FFileAttrs AND FILE_ATTRIBUTE_DIRECTORY > 0;
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

  //var ColIndex := FColumnMap[FSortColumnID];

  if AppSettings.FoldersOnTop AND item1.IsDirectory AND NOT item2.IsDirectory
    then Result := -1 // directory is always 'greater' than file
    else if AppSettings.FoldersOnTop AND NOT item1.IsDirectory AND item2.IsDirectory
      then Result := 1
      else begin
        if AppSettings.CaseSensitiveSort then begin // case sensitive comparison
          case TFileInfo(FSortColumnID) of
            fiName: Result := CompareStr(item1.Item.FDisplayName, item2.Item.FDisplayName);
            fiSize: if item1.Size > item2.Size then Result := 1
                     else if item1.Size < item2.Size then Result := -1;
            fiType: Result := CompareStr(item1.Item.FFileType, item2.Item.FFileType);
            fiModified: Result := CompareFileTime(item1.Item.FModifiedTime, item2.Item.FModifiedTime);
            fiLastAccess: Result := CompareFileTime(item1.Item.FLastAccessTime, item2.Item.FLastAccessTime);
            fiCreated: Result := CompareFileTime(item1.Item.FCreationTime, item2.Item.FCreationTime);
            fiAttributes: Result := CompareStr(item1.AttrStr, item2.AttrStr);
            fiPath: Result := CompareStr(item1.Path, item2.Path);
            // else Result := 'UNKNOWN';
          end;
        end else begin // case INsensitive comparison
          case TFileInfo(FSortColumnID) of
            fiName: Result := CompareText(item1.Item.FDisplayName, item2.Item.FDisplayName);
            fiSize: if item1.Size > item2.Size then Result := 1
                 else if item1.Size < item2.Size then Result := -1;
            fiType: Result := CompareText(item1.Item.FFileType, item2.Item.FFileType);
            fiModified: Result := CompareFileTime(item1.Item.FModifiedTime, item2.Item.FModifiedTime);
            fiLastAccess: Result := CompareFileTime(item1.Item.FLastAccessTime, item2.Item.FLastAccessTime);
            fiCreated: Result := CompareFileTime(item1.Item.FCreationTime, item2.Item.FCreationTime);
            fiAttributes: Result := CompareText(item1.AttrStr, item2.AttrStr);
            fiPath: Result := CompareText(item1.Path, item2.Path);
            // else Result := 'UNKNOWN';
          end;
        end;
      end;

  // invert Sort if requested
  if FInvertSort then Result := -Result;
end;

procedure TMainForm.CopyFolderClick(Sender: TObject);
begin
  if ListView1.Selected = nil then Exit;
  var item := FSearchResults[Cardinal(ListView1.Selected.Index)];

  Clipboard.AsText := item.Path;
end;

procedure TMainForm.ListView1Data(Sender: TObject; Item: TListItem);
var
  origItem: TSearchResultsItem;
begin
  origItem := FSearchResults[Cardinal(Item.Index)];

  Assert(Item.SubItems.Count = 0);

  Item.Caption := origItem.Item.FDisplayName;
  Item.ImageIndex := origItem.Item.FIconIndex;

  if origItem.Item.FDenied
    then Item.StateIndex := 0 // mark denied items in the list
    else Item.StateIndex := -1;

  Item.SubItems.Add(origItem.SizeStr);
  Item.SubItems.Add(origItem.Item.FFileType);
  Item.SubItems.Add(origItem.ModifiedStr);
  Item.SubItems.Add(origItem.LastAccessStr);
  Item.SubItems.Add(origItem.CreatedStr);
  Item.SubItems.Add(origItem.AttrStr);
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


  res := ShellExecute(Handle, 'open', PChar('explorer.exe'), PChar('/select,' + item.path), nil, SW_SHOWNORMAL);

  //res := ShellExecute(Handle, 'explore', PChar(path), nil, nil, SW_SHOWNORMAL);
  if res < 33 then MessageDlg('ShellExecute error: ' + res.ToString, TMsgDlgType.mtError, [mbOK], 0);
end;

procedure TMainForm.OnIndexingThreadTerminate(Sender: TObject);
begin
  FreeAndNil(FProgressListener);

  // need to check HasNewInstance because ThreadTerminate also called when user cancels indexing process
  // when indexing thread finished it work successfully - new instance will present
  // if indexing thread was cancelled then no new instance will be created
  if TCache.HasNewInstance then begin
    // need to stop this thread before Swap instances
    if Assigned(FSearchResultsFileInfoThread) then begin
      FSearchResultsFileInfoThread.Terminate;
      FSearchResultsFileInfoThread.WaitFor;
    end;

    //TODO: it might be dangerous to Swap here because of search results. Move Swap to the very end???
    TCache.Swap; // if there is no search results we can successfully do Swap here

    try
      // protection against incorrect IndexFileName stored earlier in registry
      // e.g. one of directories in file path might not exist any more
      TCache.Instance.SerializeTo(AppSettings.IndexFileName); // save loaded data into .idx file
    except
      on E:EFCreateError do begin
        try
          // get file name only from path from registry and add current dir to this name
          var fn := ExpandFileName(ExtractFileName(AppSettings.IndexFileName));
          TCache.Instance.SerializeTo(fn);
          AppSettings.IndexFileName := fn; // update registry setting
        except
          on E:EFCreateError do begin
            // use default file name without path (current FinderX dir will be used)
            TCache.Instance.SerializeTo(TSettings.INDEX_FILENAME);
            AppSettings.IndexFileName := ExpandFileName(TSettings.INDEX_FILENAME); // update registry setting
          end;
        end;
      end;
    end;

    IndexingBitBtn.Hint := BuildIndexingBtnHint;
    UpdateStatusBarXXX(FIndexingThread.ExecData);

    // repeat search on refreshed data
    if ListView1.Items.Count > 0 then MakeSearch;
  end;

  SettingsMenuItem.Enabled := True; // main menu item
  ExitAppMenuItem.Enabled  := True; // main menu item
  AlertPanel1.Visible := False; // hide alert panel if it was visible

end;

procedure TMainForm.CancelBtnClick(Sender: TObject);
var
  ifCancel: Integer;
begin
  ifCancel := MessageDlg('Are you sure you want to cancel indexing?', TMsgDlgType.mtWarning, [mbYes, mbNo], 0, mbYes);
  // because other threads read this variable as a signal to stop execution
  TInterlocked.Exchange(FCancelIndexing, ifCancel = mrYes);
end;

procedure TMainForm.CheckIntegrityClick(Sender: TObject);
begin
  TCache.Instance.CheckThatParentIsDirectory;
  TCache.Instance.CheckLevelsDataTsCorrect;
  TCache.Instance.CheckHangingDirectories; //TODO: it would be great to check returned value as well
  TCache.Instance.CheckFileDatesAreCorrect;
  ShowMessage('Integrity checks are OK');
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
  DateTimePickerFrom.Enabled := DateTypeComboBox.ItemIndex > 0; //SearchByModifiedDate.Checked;
  DateTimePickerTo.Enabled   := DateTypeComboBox.ItemIndex > 0; //SearchByModifiedDate.Checked;
end;

procedure TMainForm.SearchEditEnter(Sender: TObject);
begin
  PostMessage(SearchEdit.Handle, EM_SETSEL, 0, -1); // need PostMessage here instead of .SelectAll for proper work
end;

procedure TMainForm.SearchEditChange(Sender: TObject);
begin
  Timer1.Enabled := False; // stop timer to prevent triggering while typing

  if Cardinal(Length(SearchEdit.Text)) < AppSettings.SearchAfterSymbols then Exit; // search only when SearchAfterSymbols (default=3) and more symbols entered

  if AppSettings.SearchAsYouType then Timer1.Enabled := True;  // restart timer only if SearchAsYouType is ON in settings
end;

procedure TMainForm.SearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then MakeSearch();
end;

procedure TMainForm.SettingsMenuItemClick(Sender: TObject);
begin
  if SettingsForm1.ShowModal = mrOk then begin
    ListView1.Items.Count := 0; // since various settings may be changed on Settings form we reset search results to zero
    if AppSettings.WriteLogFile
      then TLogger.Init(AppSettings.LogFileName) // in case log file name has changed in Settings
      else TLogger.Shutdown;
    ClearSearchResults;
    IndexingBitBtn.Hint := BuildIndexingBtnHint;
    SearchEdit.ACEnabled := AppSettings.EnableSearchHistory;
    if AppSettings.SearchHistory.Count = 0 then SearchEdit.ACStrings.Clear;//update history in SearchEdit if history was cleared by Clear History button on settings form
    TrayIcon1.Visible := AppSettings.ShowTrayIcon;
    //UpdateStatusBarXXX(TCache.Instance.GetExecData);
  end;
end;

procedure TMainForm.UpdateStatusBarPanelsWidth;
var
  w2, w3: Integer;
begin
  w2 := StatusBar1.Canvas.TextWidth(StatusBar1.Panels[2].Text);
  w3 := StatusBar1.Canvas.TextWidth(StatusBar1.Panels[3].Text);
  if w2 > StatusBar1.Panels[2].Width then StatusBar1.Panels[2].Width := Round(w2*1.05); // +5% extra width
  if w3 > StatusBar1.Panels[3].Width then StatusBar1.Panels[3].Width := Round(w3*1.05);
end;

// updates panels 2,3,4 only
procedure TMainForm.UpdateStatusBarXXX(ExecData: TArray<TVolumeExecData>);
var
  i: Cardinal;
  VolSize, LoadTime, ItemsCnt: string;
begin
  for i := 1 to Length(ExecData) do begin
     LoadTime := LoadTime + Format('%s %s ', [ExcludeTrailingPathDelimiter(ExecData[i - 1].VolumeName), MillisecToStr(ExecData[i - 1].ExecTime)]);
     ItemsCnt := ItemsCnt + Format('%s %s  ', [ExcludeTrailingPathDelimiter(ExecData[i - 1].VolumeName), ThousandSep(ExecData[i - 1].ItemsCount)]);
     VolSize  := VolSize  + Format('%s %s  ', [ExcludeTrailingPathDelimiter(ExecData[i - 1].VolumeName), MakeSizeStr(ExecData[i - 1].VolSize)]);
  end;

  VolSize := VolSize + '| ' + TCache.Instance.IndexFileSaveDate.ToString;
  StatusBar1.Panels[2].Text := 'Load: ' + LoadTime;
  StatusBar1.Panels[3].Text := 'Items: ' + ItemsCnt;
  StatusBar1.Panels[4].Text := 'Size: ' + VolSize;

  UpdateStatusBarPanelsWidth;
end;

// updates panels 0, 1 only.
// SearchTime should be in milliseconds
procedure TMainForm.UpdateStatusBarXX(FoundItems, SearchTime: Cardinal);
begin
  StatusBar1.Panels[0].Text := Format('  Found: %s', [ThousandSep(FoundItems)]);
  StatusBar1.Panels[1].Text := Format('Search time: %s', [MillisecToStr(SearchTime)]);
end;

// updates panels 0 only with extra info.
procedure TMainForm.UpdateStatusBarX(FoundItems, Percent: Cardinal);
begin
  StatusBar1.Panels[0].Text := Format('  Found: %s (%u%%)', [ThousandSep(FoundItems), Percent]);
 // StatusBar1.Invalidate; //TODO: may be we do not need it
end;

procedure TMainForm.SearchBtnClick(Sender: TObject);
begin
  MakeSearch(); // execute the same code when timer has triggered
end;

procedure TMainForm.StatisticsMenuItemClick(Sender: TObject);
begin
  StatisticForm1.ShowModal;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
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
    //SearchByModifiedDate.Checked := False;
    DateTypeComboBox.ItemIndex := 0; // means "there is no search by date"
    SearchByAttributes.Checked := False;
  end;
end;

procedure TMainForm.ApplicationEvents1Minimize(Sender: TObject);
begin
  if AppSettings.ShowTrayIcon AND AppSettings.MinimizeToTray then begin
    // Hide the window and set its state variable to wsMinimized.
    Hide();
    WindowState := wsMinimized;
    TrayIcon1.ShowBalloonHint;
  end;
end;

function TMainForm.BuildIndexingBtnHint: string;
begin
  var cache := TCache.Instance;
  if cache.VolumesCount > 0
    then Result := Format('Index file has volumes %s. Indexing date %s.', [cache.GetVolumeNamesAsString, cache.IndexFileSaveDate.ToString {DateTimeToStr(cache.IndexFileSaveDate)}])
    else Result := 'Index file has not volumes. Press "Refresh Index" button to index volume(s).';
end;

procedure TMainForm.StartIndexing(bg: Boolean);
var Empty: TArray<string>;
begin
  TInterlocked.Exchange(FCancelIndexing, False); // flag to stop indexing e.g. user has cancelled indexing
  FIndexingThread := TLoadFSThread.Create(AppSettings.VolumesToIndex,
                                          TTernary.IfThen(AppSettings.ExcludeFolders, AppSettings.ExcludeFoldersList, Empty),
                                          AppSettings.FastReadingNTFS); // create suspended
  FIndexingThread.OnTerminate := OnIndexingThreadTerminate;
  FProgressListener := TMainFormIndexingProgress.Create(FIndexingThread);

  SettingsMenuItem.Enabled := False;  // main menu item. it is not TControl, we cannot pass it to .Start below
  ExitAppMenuItem.Enabled  := False;  // main menu item. it is not TControl, we cannot pass it to .Start below
  if bg
    then FIndexingThread.Start([IndexingBitBtn], [], [], FProgressListener)
    else FIndexingThread.Start([IndexingBitBtn], [CancelBtn, ProgressBar1, ProgressLabel], [], FProgressListener);
end;

procedure TMainForm.IndexingBitBtnClick(Sender: TObject);
begin
  StartIndexing(False);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FProgressListener)
    then MessageDlg('Cannot close form because indexing is in progress.', TMsgDlgType.mtWarning, [mbOK], 0);

  CanClose := NOT Assigned(FProgressListener);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var ifsDate: TDateTime;
begin
  MsgDlgIcons[mtInformation] := TMsgDlgIcon.mdiInformation;
  MsgDlgIcons[mtConfirmation] := TMsgDlgIcon.mdiShield;

  // This call just exits if index file does not exist
  TCache.Instance.DeserializeFrom(AppSettings.IndexFileName);

  Timer2Timer(nil); // show yellow warning immediately after app start if IndexDB is old or absent.

  // settings are already loaded in FinderX.dpr
  TrayIcon1.Visible := AppSettings.ShowTrayIcon;

  FSearchResults := THArrayG<TSearchResultsItem>.Create(AppSettings.MaxFoundItems + 1);  // default capacity (+1 just in case)
  FSearchResultsCache := TObjectsCache<TSearchResultsItem>.Create(AppSettings.MaxFoundItems + 1, False);
  FSearchResultsFileInfoThread := nil;
  ProgressBarFileInfo.Visible := False;

  SearchByFileSizeClick(SearchByFileSize); // disable search controls by default
  SearchByModifiedDateClick(SearchByModifiedDate);
  SearchByAttributesClick(SearchByAttributes);

  IndexingBitBtn.Hint := BuildIndexingBtnHint;

  InitSearchEdit; // must be called after AppSettings.Load;
  InitColumns;    // must be called after AppSettings.Load;

  ProgressBar1.Parent := StatusBar1;
  ProgressBar1.Width := 100;
  ProgressBar1.Height := 17;
  ProgressBar1.Left := StatusBar1.Width - ProgressBar1.Width - 20 - 60; // 60 for cancel button
  ProgressBar1.Top := 4;
  ProgressBar1.Anchors := [akTop, akRight];

  CancelBtn.Parent := StatusBar1;
  CancelBtn.Left := StatusBar1.Width - CancelBtn.Width - 15;
  CancelBtn.Top := 2;
  CancelBtn.Anchors := [akTop, akRight];

  ProgressLabel.Parent := StatusBar1;
  ProgressLabel.Width := 110;
  ProgressLabel.Height := 17;
  ProgressLabel.Top := 5;
  ProgressLabel.Left := ProgressBar1.Left - ProgressLabel.Width;
  ProgressLabel.Caption := 'Indexing progress...';
  ProgressLabel.Anchors := [akTop, akRight];

  UpdateStatusBarXXX(TCache.Instance.GetExecData);

  // refresh data on start only once per day
  ifsDate := TCache.Instance.IndexFileSaveDate;
  if DaysBetween(Now(), ifsDate) > 1
    then StartIndexing(True);

  //ListView_SetTextBkColor(ListView1.Handle, CLR_NONE); // I donot know how it works but it needed to properly repaint listview rows when active row is changes
end;

procedure TMainForm.InitSearchEdit();
begin
  SearchEdit := TSearchEdit.Create(self);
  SearchEdit.Parent := SearchPanel;
  SearchEdit.EditLabel.Caption := 'Search here';
  SearchEdit.LabelPosition := lpLeft;
  SearchEdit.Left := 70;
  SearchEdit.Top := 8;
  SearchEdit.Width := 389;
  SearchEdit.Height := 25;
  SearchEdit.OnChange := SearchEditChange;
  SearchEdit.OnKeyPress := SearchEditKeyPress;
  SearchEdit.OnEnter := SearchEditEnter;
  SearchEdit.Hint := 'Type your search here';
  SearchEdit.ShowHint := True;
  SearchEdit.AutoSelect := True;

  SearchEdit.ACEnabled := AppSettings.EnableSearchHistory;
  SearchEdit.ACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
  SearchEdit.ACSource := acsList;

  ActiveControl := SearchEdit;
end;

procedure TMainForm.InitColumns();
var
  i, j: Integer;
  Col: TListColumn;
  ColType: Cardinal;
begin
  GetSystemImageList;
  FSortColumnID := -1; // no sorting by default

  // load columns order, visibility and widths from settings
  for i := 0 to High(AppSettings.ColumnInfos) do begin
    ColType := Ord(AppSettings.ColumnInfos[i].ColType);

    for j := 0 to ListView1.Columns.Count - 1 do begin
      Col := ListView1.Columns[j];
      if Col.Tag = ColType then begin
        Col.Index := i;
        Col.Width := AppSettings.ColumnInfos[i].Width;
        //FColumnMap.SetValue(Col.ID, i); // default index for Name column is 0
        // Col.Visible := AppSettings.ColumnInfos[i].Visible;
        break;
      end;
    end;
  end;

  Top := AppSettings.MainWindow.Top;
  Left := AppSettings.MainWindow.Left;
  Width := AppSettings.MainWindow.Width;
  Height := AppSettings.MainWindow.Height;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FSearchResultsFileInfoThread) then FSearchResultsFileInfoThread.Terminate;

  // do some usefull work while thread is terminating

  if Assigned(FSearchResultsFileInfoThread) then FSearchResultsFileInfoThread.WaitFor;

  ClearSearchResults; // to move items into FSearchResultsCache where they will be properly destroyed.
  FreeAndNil(FSearchResults);
  FreeAndNil(FSearchResultsCache);
  FreeAndNil(FSearchResultsFileInfoThread);

  StoreColumns;
  AppSettings.SearchHistory.Assign(SearchEdit.ACStrings);
  AppSettings.Save; // save all settings including updated search history data

  SearchEdit.Free;
end;

procedure TMainForm.StoreColumns;
var
  i: Integer;
begin
  for i := 0 to ListView1.Columns.Count - 1 do begin
    AppSettings.ColumnInfos[i].Width   := ListView1.Columns[i].Width;
    AppSettings.ColumnInfos[i].ColType := TFileInfo(ListView1.Columns[i].Tag);
    AppSettings.ColumnInfos[i].Visible := ListView1.Columns[i].Width > 0;
  end;

  AppSettings.MainWindow.Top := Top;
  AppSettings.MainWindow.Left := Left;
  AppSettings.MainWindow.Width := Width;
  AppSettings.MainWindow.Height := Height;
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

procedure TMainFormIndexingProgress.ReportError(Error: TError);
begin
  TLogger.Log(Error.ErrText);

  // do NOT show error in case of Access Denied
  if (Error.ErrCode = NO_ERROR) OR (Error.ErrCode = ERROR_ACCESS_DENIED) then Exit;

  MessageDlg(Error.ErrText, mtError, [mbOK], 0);
end;

procedure TMainFormIndexingProgress.Start(Notes: string; P100: Integer);
begin
  FMaxValue := P100;
  TLoadFSThread.Synchronize(FThread,
   procedure
   begin
      MainForm.ProgressLabel.Caption := 'Indexing ' + Notes + '...';
      MainForm.ProgressBar1.Position := 0;
   end
   );

end;


 { TSearchResultsShellInfoThread }

 // load file icons in background
procedure TSearchResultsShellInfoThread.Execute;
var
  i: Cardinal;
  start: Cardinal;
  tmpI: TCacheItem;
  LogPrefix: string;
begin
  start := GetTickCount;

  CoInitialize(nil); // this need to be called for each thread in app for proper work of ShGetFileInfo, for main thread it is called automatically by Delphi

  LogPrefix := '[' + ClassName + ']['+ ThreadID.ToString + ']';
  try
    TLogger.Log(LogPrefix + ' STARTED');
    if Terminated then begin
      TLogger.Log(LogPrefix + ' Terminated = True detected');
      Exit;
    end;

    for i := 1 to MainForm.FSearchResults.Count do begin
      if ((i mod 100) = 0) then begin // repaint list of items after every 100 loaded icons
        MainForm.ListView1.Invalidate;
        if Terminated then begin
          TLogger.Log(LogPrefix + ' Terminated = True detected');
          Exit;
        end;
      end;

      var resItem := MainForm.FSearchResults[i - 1];

      if resItem.Item.FFileType = '' then begin
        TmpI := TCacheItem.Create;
        TmpI.Assign(resItem.Item);
        GetFileShellInfo(resItem.Path, TmpI); // we do not need to check function return value here
        PostMessage(MainForm.Handle, WM_SearchResultsShellInfo_MSG, WPARAM(TmpI), LPARAM(i - 1));
      end;

    end;
  finally
    // sending "notification" message that work has finished
    PostMessage(MainForm.Handle, WM_SearchResultsShellInfo_MSG, WPARAM(nil), LPARAM(MainForm.FSearchResults.Count));
    MainForm.ListView1.Invalidate;
    CoUninitialize;
    TLogger.Log(LogPrefix + ' FINISHED. Time spent: ' + MillisecToStr(GetTickCount - start));
  end;
end;

{
procedure TSearchResultsShellInfoThread.Start(WinHandle: THandle; CancelFlag: PBoolean; lvStart: Cardinal = 0; lvEnd: Cardinal = 0);
begin
  FBegin := lvStart;
  FFinish := lvEnd;
  FCancelFlag := CancelFlag;
  FWinHandle := WinHandle;
  Start();
end;
 }

class function TSearchResultsShellInfoThread.CreateAndRun(): TThread;
begin
  Result := TSearchResultsShellInfoThread.Create(True);
  Result.Start();
end;


initialization
  SplitArray := THArrayG<SplitRec>.Create();

finalization
  FreeAndNil(SplitArray);
end.
