unit Settings;

interface

uses System.Types, Classes;

type
  // display columns
  TFileInfo = (fiName, fiSize, fiType, fiModified, fiLastAccess, fiCreated, fiAttributes, fiPath, fiItemsCount);

  // Column settings structure
  TColumnInfo = record
    ColType: TFileInfo;
    Visible: Boolean;
    Width  : Integer;
  end;

  TColumnInfos = array [0..Ord(High(TFileInfo))] of TColumnInfo;

  TSizeFormat = (sfAuto, sfBytes, sfKilobytes, sfMegabytes);
  //TSizeFormatLabels = array[TSizeFormat] of string;


  TSettingInfo = record
    Name: string;
    DefValueB: Boolean;
    DefValueI: Integer;
    DefValueS: string;
  end;

  TSettingNames = (snCaseSensitiveSearch, snCaseSensitiveSort, snHideFoldersSize, snEnableSearchHistory,
                   snFoldersOnTop, snSearchAsYouType, snSearchAfterSymbols, snShowTrayIcon, snMinimizeToTray,
                   snRunAsAdmin, snHighlightSearchTerms, snShowRowMouseover, snStartAppWithSystem,
                   snIncludeNewFixedVolumes, snIncludeNewRemovableVolumes, snRemoveOfflineVolumes, snExcludeFolders,
                   snExcludeFoldersList, snSizeFormat,  snMaxFoundItems, snVolumesToIndex, snIndexFileName,
                   snWriteLogFile, snLogFileName, snNTFSFastReading);

  TSettingInfos = array [TSettingNames] of TSettingInfo;

  TSettings = class
  public
    CaseSensitiveSearch: Boolean;
    CaseSensitiveSort: Boolean;
    FoldersOnTop: Boolean;
    HideFoldersSize: Boolean;
    EnableSearchHistory: Boolean;
    SearchAsYouType: Boolean;
    ShowTrayIcon: Boolean;
    MinimizeToTray: Boolean;
    RunAsAdmin: Boolean;
    HighlightSearchTerms: Boolean;
    ShowRowMouseover: Boolean;
    StartAppWithSystem: Boolean;
    IncludeNewFixedVolumes: Boolean;
    IncludeNewRemovableVolumes: Boolean;
    RemoveOfflineVolumes: Boolean;
    ExcludeFolders: Boolean;
    SearchAfterSymbols: Cardinal;
    MaxFoundItems: Cardinal;
    SizeFormat: TSizeFormat;
    VolumesToIndex: TArray<string>;
    ExcludeFoldersList: TArray<string>;
    SearchHistory: TStringList;
    ColumnInfos: TColumnInfos;
    MainWindow: TRect;
    IndexFileName: string;
    WriteLogFile: Boolean;
    LogFileName: string;
    FastReadingNTFS: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    procedure AddRemoveToStartup(Add: Boolean); // make application to start with windows by modifying registry settings
    class function GetDefaultExcludeFoldersList: TArray<string>;

  const
    APPKEY = 'SOFTWARE\NotepadCo\FinderX';
    HISTORYKEY = APPKEY + '\SearchHistory';
    MAINWINDOWKEY = APPKEY + '\MainWindow';
    INDEX_FILENAME = 'FinderXIndexDB.idx'; // default file name for FinderX index database
  end;

var
  AppSettings: TSettings;
  //SizeLabels: TSizeFormatLabels = ('', 'Bytes', 'KB', 'MB');

implementation

uses
  System.SysUtils, WinAPI.Windows, Vcl.Dialogs, ShlObj, WinApi.KnownFolders, WinApi.ActiveX, Registry, Functions;



var
  DefColumnInfos: TColumnInfos = ((ColType:fiName;       Visible:True; Width:300),
                                  (ColType:fiSize;       Visible:True; Width:100),
                                  (ColType:fiType;       Visible:True; Width:130),
                                  (ColType:fiModified;   Visible:True; Width:140),
                                  (ColType:fiLastAccess; Visible:True; Width:140),
                                  (ColType:fiCreated;    Visible:True; Width:140),
                                  (ColType:fiAttributes; Visible:True; Width:70),
                                  (ColType:fiPath;       Visible:True; Width:400),
                                  (ColType:fiItemsCount; Visible:True; Width:80)
                                  );


  STVL: TSettingInfos = ( (Name:'CaseSensitiveSearch'; DefValueB: False), (Name:'CaseSensitiveSort';   DefValueB: False),
                          (Name:'HideFoldersSize';     DefValueB: False), (Name:'EnableSearchHistory'; DefValueB: True),
                          (Name:'FoldersOnTop';        DefValueB: False), (Name:'SearchAsYouType';     DefValueB: True),
                          (Name:'SearchAfterSymbols';  DefValueI: 3),     (Name:'ShowTrayIcon';        DefValueB: True),
                          (Name:'MinimizeToTray';      DefValueB: True),  (Name:'RunAsAdmin';          DefValueB: False),
                          (Name:'HighlightSearchTerms'; DefValueB: True), (Name:'ShowRowMouseover';    DefValueB: True),
                          (Name:'StartAppWithSystem';  DefValueB: False), (Name:'IcludeNewFixedVolumes'; DefValueB: True),
                          (Name:'IncludeNewRemovableVolumes'; DefValueB: False), (Name:'RemoveOfflineVolumes'; DefValueB: True),
                          (Name:'ExcludeFolders';      DefValueB: False), (Name:'ExcludeFoldersList';  DefValueS: ''),
                          (Name:'SizeFormat';          DefValueI: Ord(sfAuto)), (Name:'MaxFoundItems'; DefValueI: 20_000),
                          (Name:'VolumesToIndex';      DefValueS: 'C:\'), (Name:'IndexFileName';       DefValueS: TSettings.INDEX_FILENAME),
                          (Name:'WriteLogFile';        DefValueB: True),  (Name:'LogFileName';         DefValueS: 'FinderX_debug.log'),
                          (Name:'FastReadingNTFS';     DefValueB: True)
                        );

{ TSettings }

constructor TSettings.Create;
begin
  inherited;

  // default values for all settings
  CaseSensitiveSearch        := STVL[snCaseSensitiveSearch].DefValueB;
  CaseSensitiveSort          := STVL[snCaseSensitiveSort].DefValueB;
  HideFoldersSize            := STVL[snHideFoldersSize].DefValueB;
  EnableSearchHistory        := STVL[snEnableSearchHistory].DefValueB;
  FoldersOnTop               := STVL[snFoldersOnTop].DefValueB;
  SearchAsYouType            := STVL[snSearchAsYouType].DefValueB;
  SearchAfterSymbols         := STVL[snSearchAfterSymbols].DefValueI;
  ShowTrayIcon               := STVL[snShowTrayIcon].DefValueB;
  MinimizeToTray             := STVL[snMinimizeToTray].DefValueB;
  RunAsAdmin                 := STVL[snRunAsAdmin].DefValueB;
  HighlightSearchTerms       := STVL[snHighlightSearchTerms].DefValueB;
  ShowRowMouseover           := STVL[snShowRowMouseover].DefValueB;
  StartAppWithSystem         := STVL[snStartAppWithSystem].DefValueB;
  IncludeNewFixedVolumes     := STVL[snIncludeNewFixedVolumes].DefValueB;
  IncludeNewRemovableVolumes := STVL[snIncludeNewRemovableVolumes].DefValueB;
  RemoveOfflineVolumes       := STVL[snRemoveOfflineVolumes].DefValueB;
  ExcludeFolders             := STVL[snExcludeFolders].DefValueB;
  SizeFormat                 := TSizeFormat(STVL[snSizeFormat].DefValueI);
  MaxFoundItems              := STVL[snMaxFoundItems].DefValueI;
  WriteLogFile               := STVL[snWriteLogFile].DefValueB;
  LogFileName                := STVL[snLogFileName].DefValueS;
  IndexFileName              := ExpandFileName(STVL[snIndexFileName].DefValueS);
  FastReadingNTFS            := STVL[snNTFSFastReading].DefValueB;

  ExcludeFoldersList := GetDefaultExcludeFoldersList;
  Insert(STVL[snVolumesToIndex].DefValueS, VolumesToIndex, 0); // add one volume by default

  SearchHistory := TStringList.Create(dupIgnore, True, False); // important to have these values in Create for proper work of history fill/update;
  ColumnInfos := DefColumnInfos;
  MainWindow := TRect.Create(300, 300, 300 + 1300, 300 + 500);
  end;

destructor TSettings.Destroy;
begin
  SearchHistory.Free;
  inherited;
end;

procedure TSettings.Load;
var
  MaxComponentLen, SystemFlags: DWORD;
  i, j, DriveType, error: Cardinal;
  reg: TRegistry;
  list: TStrings;
  str: string;
  Found: Boolean;
  res: LongBool;
begin
   reg := TRegistry.Create;
   list := TStringList.Create();
   try
     reg.RootKey := HKEY_CURRENT_USER;
     if reg.OpenKeyReadOnly(APPKEY) then begin
       if reg.ValueExists(STVL[snCaseSensitiveSearch].Name)       then CaseSensitiveSearch       := reg.ReadBool(STVL[snCaseSensitiveSearch].Name);
       if reg.ValueExists(STVL[snCaseSensitiveSort].Name)         then CaseSensitiveSort         := reg.ReadBool(STVL[snCaseSensitiveSort].Name);
       if reg.ValueExists(STVL[snHideFoldersSize].Name)           then HideFoldersSize           := reg.ReadBool(STVL[snHideFoldersSize].Name);
       if reg.ValueExists(STVL[snEnableSearchHistory].Name)       then EnableSearchHistory       := reg.ReadBool(STVL[snEnableSearchHistory].Name);
       if reg.ValueExists(STVL[snFoldersOnTop].Name)              then FoldersOnTop              := reg.ReadBool(STVL[snFoldersOnTop].Name);
       if reg.ValueExists(STVL[snSearchAsYouType].Name)           then SearchAsYouType           := reg.ReadBool(STVL[snSearchAsYouType].Name);
       if reg.ValueExists(STVL[snShowTrayIcon].Name)              then ShowTrayIcon              := reg.ReadBool(STVL[snShowTrayIcon].Name);
       if reg.ValueExists(STVL[snMinimizeToTray].Name)            then MinimizeToTray            := reg.ReadBool(STVL[snMinimizeToTray].Name);
       if reg.ValueExists(STVL[snRunAsAdmin].Name)                then RunAsAdmin                := reg.ReadBool(STVL[snRunAsAdmin].Name);
       if reg.ValueExists(STVL[snHighlightSearchTerms].Name)      then HighlightSearchTerms      := reg.ReadBool(STVL[snHighlightSearchTerms].Name);
       if reg.ValueExists(STVL[snShowRowMouseover].Name)          then ShowRowMouseover          := reg.ReadBool(STVL[snShowRowMouseover].Name);
       if reg.ValueExists(STVL[snStartAppWithSystem].Name)        then StartAppWithSystem        := reg.ReadBool(STVL[snStartAppWithSystem].Name);
       if reg.ValueExists(STVL[snIncludeNewFixedVolumes].Name)    then IncludeNewFixedVolumes    := reg.ReadBool(STVL[snIncludeNewFixedVolumes].Name);
       if reg.ValueExists(STVL[snIncludeNewRemovableVolumes].Name)then IncludeNewRemovableVolumes:= reg.ReadBool(STVL[snIncludeNewRemovableVolumes].Name);
       if reg.ValueExists(STVL[snRemoveOfflineVolumes].Name)      then RemoveOfflineVolumes      := reg.ReadBool(STVL[snRemoveOfflineVolumes].Name);
       if reg.ValueExists(STVL[snExcludeFolders].Name)            then ExcludeFolders            := reg.ReadBool(STVL[snExcludeFolders].Name);
       if reg.ValueExists(STVL[snExcludeFoldersList].Name)        then ExcludeFoldersList        := reg.ReadMultiString(STVL[snExcludeFoldersList].Name);
       if reg.ValueExists(STVL[snSizeFormat].Name)                then SizeFormat                := TSizeFormat(reg.ReadInteger(STVL[snSizeFormat].Name));
       if reg.ValueExists(STVL[snSearchAfterSymbols].Name)        then SearchAfterSymbols        := reg.ReadInteger(STVL[snSearchAfterSymbols].Name);
       if reg.ValueExists(STVL[snMaxFoundItems].Name)             then MaxFoundItems             := Cardinal(reg.ReadInteger(STVL[snMaxFoundItems].Name));
       if reg.ValueExists(STVL[snVolumesToIndex].Name)            then VolumesToIndex            := reg.ReadMultiString(STVL[snVolumesToIndex].Name);
       if reg.ValueExists(STVL[snIndexFileName].Name)             then IndexFileName             := reg.ReadString(STVL[snIndexFileName].Name);
       if reg.ValueExists(STVL[snWriteLogFile].Name)              then WriteLogFile              := reg.ReadBool(STVL[snWriteLogFile].Name);
       if reg.ValueExists(STVL[snLogFileName].Name)               then LogFileName               := reg.ReadString(STVL[snLogFileName].Name);
       if reg.ValueExists(STVL[snNTFSFastReading].Name)           then FastReadingNTFS           := reg.ReadBool(STVL[snNTFSFastReading].Name);
     end;
     reg.CloseKey;

     if Length(ExcludeFoldersList) = 0 // fill with default values if empty
       then ExcludeFoldersList := GetDefaultExcludeFoldersList;


     // loading Search History items
     if reg.OpenKeyReadOnly(HISTORYKEY) then begin
       reg.GetValueNames(list);
       SearchHistory.Clear;
       for i := 1 to list.Count do SearchHistory.Add(reg.ReadString(list[i - 1]));
     end;
     reg.CloseKey;

     // loading Main Window position
     if reg.OpenKeyReadOnly(MAINWINDOWKEY) then begin
       for i := 0 to High(ColumnInfos) do begin
         str := 'Col' + i.ToString;
         if reg.ValueExists(str + '.Type')    then ColumnInfos[i].ColType := TFileInfo(reg.ReadInteger(str + '.Type'));
         if reg.ValueExists(str + '.Width')   then ColumnInfos[i].Width   := reg.ReadInteger(str + '.Width');
         if reg.ValueExists(str + '.Visible') then ColumnInfos[i].Visible := reg.ReadBool(str + '.Visible');
       end;

       if reg.ValueExists('Top')    then MainWindow.Top    := reg.ReadInteger('Top');
       if reg.ValueExists('Left')   then MainWindow.Left   := reg.ReadInteger('Left');
       if reg.ValueExists('Width')  then MainWindow.Width  := reg.ReadInteger('Width');
       if reg.ValueExists('Height') then MainWindow.Height := reg.ReadInteger('Height');
     end;

     // merge list of volumes actually availalbe on the PC with list read from registry.
     if IncludeNewFixedVolumes OR IncludeNewRemovableVolumes then begin
       var Drives := GetLogicalDrives;
       for i := 1 to Length(Drives) do begin
         Found := False;
         for j := 1 to Length(VolumesToIndex) do
           if Drives[i - 1] = VolumesToIndex[j - 1] then begin
             Found := True; // found volume in stored list of volumes
             break
           end;

         if NOT Found then begin // check whether user wants to automatically add fixed or removable volumes into index
           DriveType := GetDriveType(PChar(Drives[i - 1]));
           if (((DriveType = DRIVE_REMOVABLE) OR (DriveType = DRIVE_CDROM)) AND IncludeNewRemovableVolumes) OR
             ((DriveType = DRIVE_FIXED) AND IncludeNewFixedVolumes) then
               Insert(Drives[i - 1], VolumesToIndex, Length(VolumesToIndex)); // add drive to the list of VolumesToIndex
         end;
       end;
     end;

     // remove offline drives from VolumesToIndex
     if RemoveOfflineVolumes then begin
       i := Length(VolumesToIndex);
       while i > 0  do begin
         res := GetVolumeInformation(PChar(VolumesToIndex[i - 1]), nil, 0, nil, MaxComponentLen, SystemFlags, nil, 0);
         if res = False then begin
           error := GetLastError;
           if (error = ERROR_NOT_READY) OR (error = ERROR_PATH_NOT_FOUND) // CD-ROM is present but no disk there - we get ERROR_NOT_READY while attempting to read volume name
             then Delete(VolumesToIndex, i - 1, 1)
             else MessageDlg('Cannot read volume information for drive '+ VolumesToIndex[i - 1] +'. Error code: ' + error.ToString, mtError, [mbOK], 0);
         end;
         Dec(i);
       end;
     end;

   finally
     list.Free;
     reg.Free;
   end;
end;

procedure TSettings.Save;
var
  i: Integer;
  reg: TRegistry;
  str: string;
begin
   reg := TRegistry.Create;
   try
     reg.RootKey := HKEY_CURRENT_USER;
     if reg.OpenKey(APPKEY, True) then begin
       reg.WriteBool(STVL[snCaseSensitiveSearch].Name, CaseSensitiveSearch);
       reg.WriteBool(STVL[snCaseSensitiveSort].Name, CaseSensitiveSort);
       reg.WriteBool(STVL[snHideFoldersSize].Name, HideFoldersSize);
       reg.WriteBool(STVL[snEnableSearchHistory].Name, EnableSearchHistory);
       reg.WriteBool(STVL[snFoldersOnTop].Name, FoldersOnTop);
       reg.WriteBool(STVL[snSearchAsYouType].Name, SearchAsYouType);
       reg.WriteBool(STVL[snShowTrayIcon].Name, ShowTrayIcon);
       reg.WriteBool(STVL[snMinimizeToTray].Name, MinimizeToTray);
       reg.WriteBool(STVL[snRunAsAdmin].Name, RunAsAdmin);
       reg.WriteBool(STVL[snHighlightSearchTerms].Name, HighlightSearchTerms);
       reg.WriteBool(STVL[snShowRowMouseover].Name, ShowRowMouseover);
       reg.WriteBool(STVL[snStartAppWithSystem].Name, StartAppWithSystem);
       reg.WriteBool(STVL[snIncludeNewFixedVolumes].Name, IncludeNewFixedVolumes);
       reg.WriteBool(STVL[snIncludeNewRemovableVolumes].Name, IncludeNewRemovableVolumes);
       reg.WriteBool(STVL[snRemoveOfflineVolumes].Name, RemoveOfflineVolumes);
       reg.WriteBool(STVL[snExcludeFolders].Name, ExcludeFolders);
       reg.WriteMultiString(STVL[snExcludeFoldersList].Name, ExcludeFoldersList);
       reg.WriteInteger(STVL[snSizeFormat].Name, Integer(SizeFormat));
       reg.WriteInteger(STVL[snSearchAfterSymbols].Name, SearchAfterSymbols);
       reg.WriteInteger(STVL[snMaxFoundItems].Name, Integer(MaxFoundItems));
       reg.WriteMultiString(STVL[snVolumesToIndex].Name, VolumesToIndex);
       reg.WriteString(STVL[snIndexFileName].Name, IndexFileName);
       reg.WriteBool(STVL[snWriteLogFile].Name, WriteLogFile);
       reg.WriteString(STVL[snLogFileName].Name, LogFileName);
       reg.WriteBool(STVL[snNTFSFastReading].Name, FastReadingNTFS);
     end;
     reg.CloseKey;

     // store Search history
     reg.DeleteKey(HISTORYKEY); // remove old search history
     if reg.OpenKey(HISTORYKEY, True) then begin
       for i := 1 to SearchHistory.Count do reg.WriteString(i.ToString, SearchHistory[i - 1]);
     end;
     reg.CloseKey;

     // store Main Window position
     reg.DeleteKey(MAINWINDOWKEY); // remove old search history
     if reg.OpenKey(MAINWINDOWKEY, True) then begin
       for i := 0 to High(ColumnInfos) do begin
         str := 'Col' + i.ToString;
         reg.WriteInteger(str + '.Type', Ord(ColumnInfos[i].ColType));
         reg.WriteInteger(str + '.Width', ColumnInfos[i].Width);
         reg.WriteBool(str + '.Visible', ColumnInfos[i].Visible);
       end;

       reg.WriteInteger('Top', MainWindow.Top);
       reg.WriteInteger('Left', MainWindow.Left);
       reg.WriteInteger('Width', MainWindow.Width);
       reg.WriteInteger('Height', MainWindow.Height);
     end;

     // make application to start with windows by modifying registry settings
     AddRemoveToStartup(StartAppWithSystem);

   finally
     reg.Free;
   end;

end;

class function TSettings.GetDefaultExcludeFoldersList: TArray<string>;
var
  TmpFolder: PChar;
begin
  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then TLogger.Log('SHGetKnownFolderPath failed: cannot get FOLDERID_LocalAppData.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_LocalAppDataLow, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then TLogger.Log('SHGetKnownFolderPath failed: cannot get FOLDERID_LocalAppDataLow.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramFilesX86, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then TLogger.Log('SHGetKnownFolderPath failed: cannot get FOLDERID_ProgramFilesX86.');
  Insert(TmpFolder + '\Microsoft\Temp', Result, Length(Result));
  Insert(TmpFolder + '\Google\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then TLogger.Log('SHGetKnownFolderPath failed: cannot get FOLDERID_ProgramData.');
  Insert(TmpFolder + '\Microsoft\Search\Data\Temp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);

  if S_OK <> SHGetKnownFolderPath(FOLDERID_Windows, KF_FLAG_DONT_VERIFY, 0, TmpFolder)
    then TLogger.Log('SHGetKnownFolderPath failed: cannot get FOLDERID_Windows.');
  Insert(TmpFolder + '\Temp', Result, Length(Result));
  Insert(TmpFolder + '\WinSyS\Temp', Result, Length(Result));
  Insert(TmpFolder + '\assembly\Temp', Result, Length(Result));
  Insert(TmpFolder + '\assembly\tmp', Result, Length(Result));
  Insert(TmpFolder + '\System32\DriversStore\Temp', Result, Length(Result));
  Insert(TmpFolder + '\Microsoft Antimalware\Tmp', Result, Length(Result));
  CoTaskMemFree(TmpFolder);
end;

procedure TSettings.AddRemoveToStartup(Add: Boolean);
var
  Reg: TRegistry;
  AppPath: string;
begin
  AppPath := ParamStr(0);  // Path to your application

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', True) then begin
      if Add
        then Reg.WriteString('FinderX', AppPath)
        else Reg.DeleteValue('FinderX');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

initialization
  AppSettings := TSettings.Create;

finalization
  FreeAndNil(AppSettings);

end.
