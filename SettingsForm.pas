unit SettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.NumberBox,
  LoadFSThread, System.ImageList, Vcl.ImgList, {FileNamesCache,} FileCache, Vcl.WinXPanels;

type


  TSettingsForm1 = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    FileOpenDialog1: TFileOpenDialog;
    Sections: TListBox;
    SettingsPanels: TCardPanel;
    Card1: TCard;
    MinimizeToTrayCheckBox: TCheckBox;
    ShowTrayIconCheckBox: TCheckBox;
    Card2: TCard;
    CaseSearchCheckBox: TCheckBox;
    EnableSearchHistoryCheckBox: TCheckBox;
    SearchAsYouTypeLabel2: TLabel;
    SearchAfterNumberBox: TNumberBox;
    SearchAsYouTypeCheckBox: TCheckBox;
    SearchAsYouTypeLabel1: TLabel;
    Card3: TCard;
    FoldersOnTopCheckBox: TCheckBox;
    MaxNumFoundBox: TNumberBox;
    Label1: TLabel;
    CaseSortCheckBox: TCheckBox;
    HideFoldersSizeCheckbox: TCheckBox;
    Card4: TCard;
    IndexingProgressLabel: TLabel;
    ProgressBar1: TProgressBar;
    IndexInfoLabel: TLabel;
    Button1: TButton;
    BuildIndexButton: TButton;
    IncludeNewFixedDrivesCheckBox: TCheckBox;
    IncludeNewRemovableDrivesCheckBox: TCheckBox;
    RemoveOfflineDrivesCheckBox: TCheckBox;
    VolumesListBox: TListBox;
    RemoveDriveButton: TButton;
    RunAsAdminCheckBox: TCheckBox;
    StartWithWindowsCheckBox: TCheckBox;
    Card5: TCard;
    ExcludeFoldersCheckBox: TCheckBox;
    ExcludeFoldersListBox: TListBox;
    AddFolderButton: TButton;
    EditFolderButton: TButton;
    RemoveFolderButton: TButton;
    SizeFormatComboBox: TComboBox;
    SizeFormatLabel: TLabel;
    ShowRowOnMouseOverCheckBox: TCheckBox;
    HighlightSearchTermsCheckBox: TCheckBox;
    ImageList1: TImageList;
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BuildIndexButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SearchAsYouTypeCheckBoxClick(Sender: TObject);
    procedure ShowTrayIconCheckBoxClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure SectionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveDriveButtonClick(Sender: TObject);
    procedure AddFolderButtonClick(Sender: TObject);
    procedure RemoveFolderButtonClick(Sender: TObject);
    procedure EditFolderButtonClick(Sender: TObject);
  private
    FIndexingThread: TLoadFSThread;
    FProgressListener: IIndexingProgress;
    FCancel: Boolean;
    procedure OnThreadTerminate(Sender: TObject);
  public
    ExecData: TExecutionData;
    property Cancel: Boolean read FCancel;
  end;

 TSettingsFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
   FThread: TLoadFSThread;
   FErrors: TStrings;
 public
   constructor Create(Thread: TLoadFSThread; Output: TStrings);
   procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
   procedure ReportError(ErrorStr: string); override;
 end;


var
  SettingsForm1: TSettingsForm1;

const
  INDEX_FILENAME = 'FileFindIndexDB.idx';

implementation

{$R *.dfm}

uses
  System.UITypes, System.StrUtils, ShlObj, WinApi.KnownFolders, WinApi.ActiveX, Registry, Settings, IndexingLog, Functions;

function  StringListToArrayTab(Strings: TStrings): TArray<string>;
var
  i, p1, p2: Integer;
  str: string;
begin
  SetLength(Result, Strings.Count);
  for i := 0 to Strings.Count - 1 do begin
    str := Strings[i];
    p1 := Pos(#9, str);
    p2 := Pos(#9, str, p1 + 1);
    Result[i] := Copy(str, p1 + 1, p2 - p1 - 1);
  end;
end;

procedure TSettingsForm1.OKButtonClick(Sender: TObject);
begin
   AppSettings.CaseSensitiveSearch := CaseSearchCheckBox.Checked;
   AppSettings.CaseSensitiveSort := CaseSortCheckBox.Checked;
   AppSettings.HideFoldersSize := HideFoldersSizeCheckbox.Checked;
   AppSettings.EnableSearchHistory := EnableSearchHistoryCheckBox.Checked;
   AppSettings.FoldersOnTop := FoldersOnTopCheckBox.Checked;
   AppSettings.MaxFoundItems := Cardinal(MaxNumFoundBox.ValueInt);
   AppSettings.VolumesToIndex := StringListToArrayTab(VolumesListBox.Items); //FolderToIndexEditBox.Text;
   AppSettings.SearchAsYouType := SearchAsYouTypeCheckBox.Checked;
   AppSettings.SearchAfterSymbols := SearchAfterNumberBox.ValueInt;
   AppSettings.ShowTrayIcon := ShowTrayIconCheckBox.Checked;
   AppSettings.MinimizeToTray := MinimizeToTrayCheckBox.Checked;
   AppSettings.RunAsAdmin := RunAsAdminCheckBox.Checked;
   AppSettings.HighlightSearchTerms := HighlightSearchTermsCheckBox.Checked;
   AppSettings.ShowRowMouseover := ShowRowOnMouseOverCheckBox.Checked;
   AppSettings.StartAppWithSystem := StartWithWindowsCheckBox.Checked;
   AppSettings.IncludeNewFixedVolumes := IncludeNewFixedDrivesCheckBox.Checked;
   AppSettings.IncludeNewRemovableVolumes := IncludeNewRemovableDrivesCheckBox.Checked;
   AppSettings.RemoveOfflineVolumes := RemoveOfflineDrivesCheckBox.Checked;
   AppSettings.ExcludeFolders := ExcludeFoldersCheckBox.Checked;
   AppSettings.ExcludeFoldersList := StringListToArray(ExcludeFoldersListBox.Items);
   AppSettings.SizeFormat := SizeFormatComboBox.Text;

   AppSettings.Save;
end;

procedure TSettingsForm1.OnThreadTerminate(Sender: TObject);
begin
  IndexInfoLabel.Caption := 'Indexing done in ' + MillisecToStr(FIndexingThread.ExecData.ExecTime);

  TFSC.Instance.RemoveProgressListener(FProgressListener);
  FreeAndNil(FProgressListener);

  ExecData := FIndexingThread.ExecData;
  TFSC.Instance.SerializeTo(INDEX_FILENAME); // saving data into .idx file
  FCancel := False;
end;

procedure TSettingsForm1.RemoveDriveButtonClick(Sender: TObject);
begin
  if VolumesListBox.Count > 1 then VolumesListBox.DeleteSelected;
end;

procedure TSettingsForm1.SearchAsYouTypeCheckBoxClick(Sender: TObject);
begin
  SearchAfterNumberBox.Enabled  := SearchAsYouTypeCheckBox.Checked;
  SearchAsYouTypeLabel1.Enabled := SearchAsYouTypeCheckBox.Checked;
  SearchAsYouTypeLabel2.Enabled := SearchAsYouTypeCheckBox.Checked;
end;

procedure TSettingsForm1.SectionsClick(Sender: TObject);
begin
  SettingsPanels.ActiveCardIndex := Sections.ItemIndex;
end;

procedure TSettingsForm1.ShowTrayIconCheckBoxClick(Sender: TObject);
begin
  MinimizeToTrayCheckBox.Enabled := ShowTrayIconCheckBox.Checked;
end;

procedure TSettingsForm1.RemoveFolderButtonClick(Sender: TObject);
begin
  ExcludeFoldersListBox.DeleteSelected;
end;

procedure TSettingsForm1.AddFolderButtonClick(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := ''; // use recently opened folder
  if FileOpenDialog1.Execute then begin
    ExcludeFoldersListBox.Items.Add(FileOpenDialog1.FileName);
  end;
end;

procedure TSettingsForm1.EditFolderButtonClick(Sender: TObject);
begin
  if ExcludeFoldersListBox.ItemIndex = -1 then Exit;

  FileOpenDialog1.DefaultFolder := ExcludeFoldersListBox.Items[ExcludeFoldersListBox.ItemIndex];
  if FileOpenDialog1.Execute then begin
    ExcludeFoldersListBox.Items.Add(FileOpenDialog1.FileName);
  end;
end;

procedure TSettingsForm1.BuildIndexButtonClick(Sender: TObject);
begin
  FCancel := False;
  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FProgressListener := TSettingsFormIndexingProgress.Create(FIndexingThread, IndexingLogForm.LogMemo.Lines);
  TFSC.Instance.AddProgressListener(FProgressListener);

  FIndexingThread.OnTerminate := OnThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  FIndexingThread.ProgressBar := ProgressBar1;
  FIndexingThread.ExecData.VolumesToIndex := StringListToArray(VolumesListBox.Items); //FolderToIndexEditBox.Text;

  FIndexingThread.Start([{FolderToIndexEditBox,} BuildIndexButton{, SelectFolderButton}], [ProgressBar1, IndexingProgressLabel], [IndexInfoLabel]);
end;

procedure TSettingsForm1.Button1Click(Sender: TObject);
begin
  IndexingLogForm.ShowModal;
end;

procedure TSettingsForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FProgressListener) then begin
    FCancel := mrYes = MessageDlg('Indexing is in progress. Are you sure you want to cancel indexing and close Settings dialog?', TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0, mbYes);
  //MessageDlg('Cannot close form because indexing is in progress.', TMsgDlgType.mtInformation, [mbOK], 0);
    while FCancel do begin TThread.Sleep(200); Application.ProcessMessages; end; //waiting till thread finishes
  end;

  CanClose := NOT Assigned(FProgressListener);
end;

procedure TSettingsForm1.FormCreate(Sender: TObject);
begin
  Sections.ItemIndex := 0;
  SectionsClick(Sections);
end;

function GetDriveTypeString(drive: string): string;
var
  dt: Cardinal;
begin
  dt := GetDriveType(PChar(drive));
  case dt of
    DRIVE_REMOVABLE: Result := 'removable';
    DRIVE_CDROM: Result := 'removable';
    DRIVE_UNKNOWN:Result := 'unknown';
    DRIVE_FIXED: Result := 'fixed';
    DRIVE_RAMDISK: Result := 'fixed';
    else
      Result := 'unknown2';
  end;
end;


procedure TSettingsForm1.FormShow(Sender: TObject);
var
  i, j, error, VolCnt: Cardinal;
  VolName, VolName2, CurrVol, str: string;
  MaxComponentLen, SystemFlags: DWORD;
  res: LongBool;
  TmpFolder: PChar;
  Found: Boolean;
  Volumes: TArray<string>;
begin
  AppSettings.Load; // load settings from registry each time settings form is shown

  CaseSearchCheckBox.Checked            := AppSettings.CaseSensitiveSearch;
  CaseSortCheckBox.Checked              := AppSettings.CaseSensitiveSort;
  HideFoldersSizeCheckbox.Checked       := AppSettings.HideFoldersSize;
  EnableSearchHistoryCheckBox.Checked   := AppSettings.EnableSearchHistory;
  FoldersOnTopCheckBox.Checked          := AppSettings.FoldersOnTop;
  MaxNumFoundBox.ValueInt               := Integer(AppSettings.MaxFoundItems);
  SearchAsYouTypeCheckBox.Checked       := AppSettings.SearchAsYouType;
  ShowTrayIconCheckBox.Checked          := AppSettings.ShowTrayIcon;
  MinimizeToTrayCheckBox.Checked        := AppSettings.MinimizeToTray;
  SearchAfterNumberBox.ValueInt         := AppSettings.SearchAfterSymbols;
  RunAsAdminCheckBox.Checked            := AppSettings.RunAsAdmin;
  HighlightSearchTermsCheckBox.Checked  := AppSettings.HighlightSearchTerms;
  ShowRowOnMouseOverCheckBox.Checked    := AppSettings.ShowRowMouseover;
  StartWithWindowsCheckBox.Checked      := AppSettings.StartAppWithSystem;
  IncludeNewFixedDrivesCheckBox.Checked := AppSettings.IncludeNewFixedVolumes;
  IncludeNewRemovableDrivesCheckBox.Checked := AppSettings.IncludeNewRemovableVolumes;
  RemoveOfflineDrivesCheckBox.Checked   := AppSettings.RemoveOfflineVolumes;
  ExcludeFoldersCheckBox.Checked        := AppSettings.ExcludeFolders;
  SizeFormatComboBox.Text               := AppSettings.SizeFormat;

  //ArrayToStringList(AppSettings.VolumesToIndex, VolumesListBox.Items);
  ArrayToStringList(AppSettings.ExcludeFoldersList, ExcludeFoldersListBox.Items);


  if TFSC.Instance.Count = 0 then begin
    IndexInfoLabel.Visible := True;
    IndexInfoLabel.Caption := 'Index is not created, press Build Index button';
  end else begin
    IndexInfoLabel.Visible := False;
  end;

  MaxNumFoundBox.Hint := Format('Enter value between %u and %u', [Round(MaxNumFoundBox.MinValue), Round(MaxNumFoundBox.MaxValue)]);

  var drv := GetLogicalDrives;
  Volumes := AppSettings.VolumesToIndex;
  VolumesListBox.Clear;
  // merge list of volumes actually availalbe on the PC with list read from registry.
  for i := 1 to Length(drv) do begin
    Found := False;
    for j := 1 to Length(Volumes) do
      if drv[i - 1] = Volumes[j - 1] then begin
         Found := True;  // found volume in stored list of volumes
         break
      end;

    if NOT Found then Insert(drv[i-1], Volumes, Length(Volumes));
  end;


  VolCnt := Length(Volumes);
  for i := 1 to VolCnt do begin
    CurrVol := Volumes[i - 1];
    SetLength(VolName, MAX_PATH);
    res := GetVolumeInformation(PChar(CurrVol), PChar(VolName), MAX_PATH, nil, MaxComponentLen, SystemFlags, nil, 0);
    if res = False then begin
      error := GetLastError;
      if error = ERROR_NOT_READY // CD-ROM is present but no disk there - we get ERROR_NOT_READY while attempting to read volume name
        then VolName := '<not ready>'
        else MessageDlg('GetVolumeInformation failed with error: ' + error.ToString, mtError, [mbOK], 0);
    end;

    VolName2 := PChar(VolName);
    str := VolName2 + IfThen(Length(VolName2) > 9, ' ', #9) + CurrVol + #9 + GetDriveTypeString(CurrVol);
    VolumesListBox.Items.Add(str);
    VolName := '';
  end;


  ExcludeFoldersListBox.Clear;

  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Temp');
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppDataLow, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Temp');
  CoTaskMemFree(TmpFolder);

 { if S_OK <> SHGetKnownFolderPath(FOLDERID_Profile, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder);
  CoTaskMemFree(TmpFolder);
  }
  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Microsoft\Temp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Google\Temp');
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Microsoft\Search\Data\Temp');
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_Windows, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then MessageDlg('SHGetKnownFolderPath failed. ', mtError, [mbOK], 0);
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Temp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\WinSyS\Temp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\assembly\Temp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\assembly\tmp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\System32\DriverStore\Temp');
  ExcludeFoldersListBox.Items.Add(TmpFolder + '\Microsoft Antimalware\Tmp');
  CoTaskMemFree(TmpFolder);
end;


procedure TSettingsForm1.ListBox1Click(Sender: TObject);
begin

end;

{ TSettingsFormIndexingProgress }

constructor TSettingsFormIndexingProgress.Create(Thread: TLoadFSThread; Output: TStrings);
begin
  inherited Create;
  FThread := Thread;
  FErrors := Output;
end;

procedure TSettingsFormIndexingProgress.Finish;
begin
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
      if Assigned(FErrors) then FERrors.Add('Finished Indexing');
   end
   );
end;

function TSettingsFormIndexingProgress.Progress(Prgress: Integer): Boolean;
var
  b: Boolean;
begin
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
     // logrithmic progress since we do not know total progress value
      SettingsForm1.ProgressBar1.Position := SettingsForm1.ProgressBar1.Position + (SettingsForm1.ProgressBar1.Max - SettingsForm1.ProgressBar1.Position) div 20;
      b := NOT SettingsForm1.Cancel;
   end
   );
  Result := b;
end;

procedure TSettingsFormIndexingProgress.ReportError(ErrorStr: string);
begin
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
     if Assigned(FErrors) then FErrors.Add(ErrorStr);
   end
   );
end;

procedure TSettingsFormIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
    TLoadFSThread.Synchronize(FThread,
    procedure
    begin
      if Assigned(FErrors) then FErrors.Add('Start Indexing');
    end
    );
end;


end.
