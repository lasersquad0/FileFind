unit SettingsForm;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.NumberBox,
  LoadFSThread, System.ImageList, Vcl.ImgList, FileCache, Vcl.WinXPanels;

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
    IndexingLogButton: TButton;
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
    ResetToDefaultButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BuildIndexButtonClick(Sender: TObject);
    procedure IndexingLogButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SearchAsYouTypeCheckBoxClick(Sender: TObject);
    procedure ShowTrayIconCheckBoxClick(Sender: TObject);
    procedure SectionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveDriveButtonClick(Sender: TObject);
    procedure AddFolderButtonClick(Sender: TObject);
    procedure RemoveFolderButtonClick(Sender: TObject);
    procedure EditFolderButtonClick(Sender: TObject);
    procedure ExcludeFoldersCheckBoxClick(Sender: TObject);
    procedure ResetToDefaultButtonClick(Sender: TObject);
  private
    FIndexingThread: TLoadFSThread;
    FProgressListener: IIndexingProgress;
    FCancel: Boolean;
    FVolumes: TArray<string>;

    procedure OnThreadTerminate(Sender: TObject);
    function  GetDefaultTempFoldersList: TArray<string>;
  public
    ExecData: TVolumeExecData;
    property Cancel: Boolean read FCancel;
  end;

 TSettingsFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
   FThread: TLoadFSThread;
   FErrors: TStrings;
 public
   constructor Create(Thread: TLoadFSThread; Output: TStrings);
   procedure Start(P100: Integer; Notes: string); override; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
   procedure ReportError(ErrorStr: string); override;
 end;


var
  SettingsForm1: TSettingsForm1;

const
  INDEX_FILENAME = 'FinderXIndexDB.idx';

implementation

{$R *.dfm}

uses
  System.UITypes, System.StrUtils, ShlObj, WinApi.KnownFolders, WinApi.ActiveX, WinApi.Windows, Registry, Math,
  Settings, IndexingLog, Functions;

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
  if RunAsAdminCheckBox.Checked
    then MessageDlg('You selected to run FinderX as administrator. FinderX will run as adminisrator next time start it.', TMsgDlgType.mtConfirmation, [mbOK], 0);

  AppSettings.CaseSensitiveSearch := CaseSearchCheckBox.Checked;
  AppSettings.CaseSensitiveSort := CaseSortCheckBox.Checked;
  AppSettings.HideFoldersSize := HideFoldersSizeCheckbox.Checked;
  AppSettings.EnableSearchHistory := EnableSearchHistoryCheckBox.Checked;
  AppSettings.FoldersOnTop := FoldersOnTopCheckBox.Checked;
  AppSettings.MaxFoundItems := Cardinal(MaxNumFoundBox.ValueInt);
  AppSettings.VolumesToIndex := FVolumes; //StringListToArrayTab(VolumesListBox.Items); //FolderToIndexEditBox.Text;
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
  AppSettings.SizeFormat := TSizeFormat(SizeFormatComboBox.ItemIndex);

  AppSettings.Save;
end;

procedure TSettingsForm1.OnThreadTerminate(Sender: TObject);
begin
  //IndexInfoLabel.Caption := 'Indexing done in ' + MillisecToStr(FIndexingThread.ExecData.ExecTime);

  //TFSC.Instance.RemoveProgressListener(FProgressListener);
  FreeAndNil(FProgressListener);

  //ExecData := FIndexingThread.ExecData;
  TFSC.Instance.SerializeTo(INDEX_FILENAME); // saving data into .idx file
  FCancel := False;
end;

procedure TSettingsForm1.RemoveDriveButtonClick(Sender: TObject);
var
  idx: Integer;
begin
  if VolumesListBox.Count > 1 then begin
    idx := VolumesListBox.ItemIndex;
    Delete(FVolumes, VolumesListBox.ItemIndex, 1);
    VolumesListBox.DeleteSelected;
    VolumesListBox.ItemIndex := IfThen(idx < VolumesListBox.Count, idx, VolumesListBox.Count - 1);
  end;
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
var
  idx: Integer;
begin
  idx := ExcludeFoldersListBox.ItemIndex;
  ExcludeFoldersListBox.DeleteSelected;
  ExcludeFoldersListBox.ItemIndex := IfThen(idx < ExcludeFoldersListBox.Count, idx, ExcludeFoldersListBox.Count - 1);
end;

procedure TSettingsForm1.ResetToDefaultButtonClick(Sender: TObject);
begin
  ExcludeFoldersListBox.Clear;
  ArrayToStringList(GetDefaultTempFoldersList, ExcludeFoldersListBox.Items);
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
  FileOpenDialog1.FileName := '';
  if FileOpenDialog1.Execute
    then ExcludeFoldersListBox.Items[ExcludeFoldersListBox.ItemIndex] := FileOpenDialog1.FileName;
end;

procedure TSettingsForm1.ExcludeFoldersCheckBoxClick(Sender: TObject);
var
  Enbld: Boolean;
begin
  Enbld := ExcludeFoldersCheckBox.Checked;
  ExcludeFoldersListBox.Enabled := Enbld;
  AddFolderButton.Enabled := Enbld;
  EditFolderButton.Enabled := Enbld;
  RemoveFolderButton.Enabled := Enbld;
end;

procedure TSettingsForm1.BuildIndexButtonClick(Sender: TObject);
begin
  FCancel := False;
  //FProgressListener := TSettingsFormIndexingProgress.Create(FIndexingThread, IndexingLogForm.LogMemo.Lines);
  //FIndexingThread := TLoadFSThread.Create(True, FProgressListener); // create suspended
  //TFSC.Instance.AddProgressListener(FProgressListener);

  //FIndexingThread.OnTerminate := OnThreadTerminate;
  //FIndexingThread.FreeOnTerminate := True;
 // FIndexingThread.ProgressBar := ProgressBar1;
  //FIndexingThread.ExecData.VolumesToIndex := StringListToArray(VolumesListBox.Items); //FolderToIndexEditBox.Text;

 // FIndexingThread.Start([{FolderToIndexEditBox,} BuildIndexButton{, SelectFolderButton}], [ProgressBar1, IndexingProgressLabel], [IndexInfoLabel], FProgressListener);
end;

procedure TSettingsForm1.IndexingLogButtonClick(Sender: TObject);
begin
  IndexingLogForm.ShowModal;
end;

procedure TSettingsForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FProgressListener) then begin
    FCancel := mrYes = MessageDlg('Indexing is in progress. Are you sure you want to cancel indexing and close Settings dialog?', TMsgDlgType.mtWarning, [mbYes, mbNo], 0, mbYes);
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
  i, error, VolCnt: Cardinal;
  VolName, VolName2, CurrVol, str: string;
  MaxComponentLen, SystemFlags: DWORD;
  res: LongBool;
  //Found: Boolean;
  TmpFolders: TArray<string>;
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
  SizeFormatComboBox.ItemIndex          := Integer(AppSettings.SizeFormat);

  ExcludeFoldersListBox.Clear;
  ArrayToStringList(AppSettings.ExcludeFoldersList, ExcludeFoldersListBox.Items);

  ExcludeFoldersCheckBoxClick(self); // enable/disable controls

  // exclude folders: if list from registry is empty then populate it with default values.
  if(ExcludeFoldersListBox.Items.Count = 0) then begin
    ExcludeFoldersListBox.Clear;
    TmpFolders := GetDefaultTempFoldersList;
    for i := 1 to Length(TmpFolders) do begin
      ExcludeFoldersListBox.Items.Add(TmpFolders[i - 1]);
    end;
  end;


  if TFSC.Instance.VolumesCount = 0 then begin
    IndexInfoLabel.Visible := True;
    IndexInfoLabel.Caption := 'Index is not created, press Build Index button';
  end else begin
    IndexInfoLabel.Visible := False;
  end;

  MaxNumFoundBox.Hint := Format('Enter value between %u and %u', [Round(MaxNumFoundBox.MinValue), Round(MaxNumFoundBox.MaxValue)]);

  { AppSettings.VolumesToIndex already contains list of volumes read from registry and merged with volumes present on current PC
    Merging is done during loading application settings in AppSettings.Load method
    Below we load other Volumes info and add list of volumes into VolumesListBox.
    }
  VolumesListBox.Clear;
  FVolumes := AppSettings.VolumesToIndex;
  VolCnt := Length(FVolumes);
  for i := 1 to VolCnt do begin
    CurrVol := FVolumes[i - 1];
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
end;

function TSettingsForm1.GetDefaultTempFoldersList: TArray<string>;
var
  TmpFolder: PChar;
begin
  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then LogMessage('SHGetKnownFolderPath failed: cannot get FOLDERID_LocalAppData.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppDataLow, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then LogMessage('SHGetKnownFolderPath failed: cannot get FOLDERID_LocalAppDataLow.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then LogMessage('SHGetKnownFolderPath failed: cannot get FOLDERID_ProgramFilesX86.');
  Insert(TmpFolder + '\Microsoft\Temp', Result, Length(Result));
  Insert(TmpFolder + '\Google\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then LogMessage('SHGetKnownFolderPath failed: cannot get FOLDERID_ProgramData.');
  Insert(TmpFolder + '\Microsoft\Search\Data\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_Windows, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then LogMessage('SHGetKnownFolderPath failed: cannot get FOLDERID_Windows.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  Insert(TmpFolder + '\WinSyS\Temp', Result, Length(Result));
  Insert(TmpFolder + '\assembly\Temp', Result, Length(Result));
  Insert(TmpFolder + '\assembly\tmp', Result, Length(Result));
  Insert(TmpFolder + '\System32\DriversStore\Temp', Result, Length(Result));
  Insert(TmpFolder + '\Microsoft Antimalware\Tmp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);
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

procedure TSettingsFormIndexingProgress.Start(P100: Integer; Notes: string);
begin
  FMaxValue := P100;
  TLoadFSThread.Synchronize(FThread,
    procedure
    begin
      if Assigned(FErrors) then FErrors.Add('Start Indexing ' + Notes);
    end
    );
end;


end.
