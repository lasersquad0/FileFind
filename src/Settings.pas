unit Settings;

interface

uses System.Types, Classes;

type
  // display columns
  TFileInfo = (fiName, fiSize, fiType, fiModified, fiLastAccess, fiCreated, fiAttributes, fiPath);

  // Column settings structure
  TColumnInfo = record
    ColType: TFileInfo;
    Visible: Boolean;
    Width  : Integer;
  end;

  TColumnInfos = array [0..Ord(High(TFileInfo))] of TColumnInfo;

  TSizeFormat = (sfAuto, sfBytes, sfKilobytes, sfMegabytes);
  TSizeFormatLabels = array[TSizeFormat] of string;

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

    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    procedure AddRemoveToStartup(Add: Boolean); // make application to start with windows by modifying registry settings

  const
    APPKEY = 'SOFTWARE\NotepadCo\FinderX';
    HISTORYKEY = APPKEY + '\SearchHistory';
    MAINWINDOWKEY = APPKEY + '\MainWindow';
  end;

var
  AppSettings: TSettings;
  SizeLabels: TSizeFormatLabels = ('', 'Bytes', 'KB', 'MB');

implementation

uses
  System.SysUtils, WinAPI.Windows, Vcl.Dialogs, Registry, Functions;

const
  INDEX_FILENAME = 'FinderXIndexDB.idx'; // default file name for FinderX index database

var
  DefColumnInfos: TColumnInfos = ((ColType:fiName;       Visible:True; Width:300),
                                  (ColType:fiSize;       Visible:True; Width:100),
                                  (ColType:fiType;       Visible:True; Width:130),
                                  (ColType:fiModified;   Visible:True; Width:140),
                                  (ColType:fiLastAccess; Visible:True; Width:140),
                                  (ColType:fiCreated;    Visible:True; Width:140),
                                  (ColType:fiAttributes; Visible:True; Width:70),
                                  (ColType:fiPath;       Visible:True; Width:400)
                                  );


{ TSettings }

constructor TSettings.Create;
begin
  inherited;

  // default values for all settings
  CaseSensitiveSearch := False;
  CaseSensitiveSort := False;
  HideFoldersSize := False;
  EnableSearchHistory := True;
  FoldersOnTop := False;
  SearchAsYouType := True;
  ShowTrayIcon := True;
  MinimizeToTray := True;
  RunAsAdmin := False;
  HighlightSearchTerms := True;
  ShowRowMouseover := True;
  StartAppWithSystem := False;
  IncludeNewFixedVolumes := True;
  IncludeNewRemovableVolumes := False;
  RemoveOfflineVolumes := True;
  ExcludeFolders := False;
  //ExcludeFoldersList; - initialised by default as empty list
  SizeFormat := sfAuto;
  SearchAfterSymbols := 3;
  MaxFoundItems := 20000;
  Insert('C:\', VolumesToIndex, 0); // add one volume by default
  SearchHistory := TStringList.Create(dupIgnore, True, False); // important to have these values in Create for proper work of history fill/update;
  ColumnInfos := DefColumnInfos;
  MainWindow := TRect.Create(300, 300, 300 + 1300, 300 + 500);
  IndexFileName := ExpandFileName(INDEX_FILENAME);
  WriteLogFile := False;
  LogFileName := 'FinderX_debug.log';
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
       if reg.ValueExists('CaseSensitiveSearch')       then CaseSensitiveSearch    := reg.ReadBool('CaseSensitiveSearch');
       if reg.ValueExists('CaseSensitiveSort')         then CaseSensitiveSort      := reg.ReadBool('CaseSensitiveSort');
       if reg.ValueExists('HideFoldersSize')           then HideFoldersSize        := reg.ReadBool('HideFoldersSize');
       if reg.ValueExists('EnableSearchHistory')       then EnableSearchHistory    := reg.ReadBool('EnableSearchHistory');
       if reg.ValueExists('FoldersOnTop')              then FoldersOnTop           := reg.ReadBool('FoldersOnTop');
       if reg.ValueExists('SearchAsYouType')           then SearchAsYouType        := reg.ReadBool('SearchAsYouType');
       if reg.ValueExists('ShowTrayIcon')              then ShowTrayIcon           := reg.ReadBool('ShowTrayIcon');
       if reg.ValueExists('MinimizeToTray')            then MinimizeToTray         := reg.ReadBool('MinimizeToTray');
       if reg.ValueExists('RunAsAdmin')                then RunAsAdmin             := reg.ReadBool('RunAsAdmin');
       if reg.ValueExists('HighlightSearchTerms')      then HighlightSearchTerms   := reg.ReadBool('HighlightSearchTerms');
       if reg.ValueExists('ShowRowMouseover')          then ShowRowMouseover       := reg.ReadBool('ShowRowMouseover');
       if reg.ValueExists('StartAppWithSystem')        then StartAppWithSystem     := reg.ReadBool('StartAppWithSystem');
       if reg.ValueExists('IncludeNewFixedVolumes')    then IncludeNewFixedVolumes := reg.ReadBool('IncludeNewFixedVolumes');
       if reg.ValueExists('IncludeNewRemovableVolumes')then IncludeNewRemovableVolumes := reg.ReadBool('IncludeNewRemovableVolumes');
       if reg.ValueExists('RemoveOfflineVolumes')      then RemoveOfflineVolumes   := reg.ReadBool('RemoveOfflineVolumes');
       if reg.ValueExists('ExcludeFolders')            then ExcludeFolders         := reg.ReadBool('ExcludeFolders');
       if reg.ValueExists('ExcludeFoldersList')        then ExcludeFoldersList     := reg.ReadMultiString('ExcludeFoldersList');
       if reg.ValueExists('SizeFormat')                then SizeFormat             := TSizeFormat(reg.ReadInteger('SizeFormat'));
       if reg.ValueExists('SearchAfterSymbols')        then SearchAfterSymbols     := reg.ReadInteger('SearchAfterSymbols');
       if reg.ValueExists('MaxFoundItems')             then MaxFoundItems          := Cardinal(reg.ReadInteger('MaxFoundItems'));
       if reg.ValueExists('VolumesToIndex')            then VolumesToIndex         := reg.ReadMultiString('VolumesToIndex');
       if reg.ValueExists('IndexFileName')             then IndexFileName          := reg.ReadString('IndexFileName');
       if reg.ValueExists('WriteLogFile')              then WriteLogFile           := reg.ReadBool('WriteLogFile');
       if reg.ValueExists('LogFileName')               then LogFileName            := reg.ReadString('LogFileName');
     end;
     reg.CloseKey;

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
       reg.WriteBool('CaseSensitiveSearch', CaseSensitiveSearch);
       reg.WriteBool('CaseSensitiveSort', CaseSensitiveSort);
       reg.WriteBool('HideFoldersSize', HideFoldersSize);
       reg.WriteBool('EnableSearchHistory', EnableSearchHistory);
       reg.WriteBool('FoldersOnTop', FoldersOnTop);
       reg.WriteBool('SearchAsYouType', SearchAsYouType);
       reg.WriteBool('ShowTrayIcon', ShowTrayIcon);
       reg.WriteBool('MinimizeToTray', MinimizeToTray);
       reg.WriteBool('RunAsAdmin', RunAsAdmin);
       reg.WriteBool('HighlightSearchTerms', HighlightSearchTerms);
       reg.WriteBool('ShowRowMouseover', ShowRowMouseover);
       reg.WriteBool('StartAppWithSystem', StartAppWithSystem);
       reg.WriteBool('IncludeNewFixedVolumes', IncludeNewFixedVolumes);
       reg.WriteBool('IncludeNewRemovableVolumes', IncludeNewRemovableVolumes);
       reg.WriteBool('RemoveOfflineVolumes', RemoveOfflineVolumes);
       reg.WriteBool('ExcludeFolders', ExcludeFolders);
       reg.WriteMultiString('ExcludeFoldersList', ExcludeFoldersList);
       reg.WriteInteger('SizeFormat', Integer(SizeFormat));
       reg.WriteInteger('SearchAfterSymbols', SearchAfterSymbols);
       reg.WriteInteger('MaxFoundItems', Integer(MaxFoundItems));
       reg.WriteMultiString('VolumesToIndex', VolumesToIndex);
       reg.WriteString('IndexFileName', IndexFileName);
       reg.WriteBool('WriteLogFile', WriteLogFile);
       reg.WriteString('LogFileName', LogFileName);
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
