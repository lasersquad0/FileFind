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
    SizeFormat: string;
    VolumesToIndex: TArray<string>;
    ExcludeFoldersList: TArray<string>;
    SearchHistory: TStringList;
    ColumnInfos: TColumnInfos;
    MainWindow: TRect;

    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
  const
    APPKEY = 'SOFTWARE\NotepadCo\FileFind';
    HISTORYKEY = APPKEY + '\SearchHistory';
    MAINWINDOWKEY = APPKEY + '\MainWindow';
  end;

var
  AppSettings: TSettings;

implementation

uses
  System.SysUtils, WinAPI.Windows, Registry;

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
  SizeFormat := 'auto';
  SearchAfterSymbols := 3;
  MaxFoundItems := 20000;
  Insert('C:\', VolumesToIndex, 0); // add one volume by default
  //ExcludeFoldersList; - initialised by default as empty list
  SearchHistory := TStringList.Create(dupIgnore, True, False); // important to have these values in Create for proper work of history fill/update;
  ColumnInfos := DefColumnInfos;
  MainWindow := TRect.Create(300, 300, 300+ 1300, 300 + 500);
end;

destructor TSettings.Destroy;
begin
  SearchHistory.Free;
  inherited;
end;

procedure TSettings.Load;
var
  i: Cardinal;
  reg: TRegistry;
  list: TStrings;
  str: string;
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
       if reg.ValueExists('MaxFoundItems')             then MaxFoundItems          := Cardinal(reg.ReadInteger('MaxFoundItems'));
       if reg.ValueExists('VolumesToIndex')            then VolumesToIndex         := reg.ReadMultiString('VolumesToIndex');
       if reg.ValueExists('SearchAsYouType')           then SearchAsYouType        := reg.ReadBool('SearchAsYouType');
       if reg.ValueExists('ShowTrayIcon')              then ShowTrayIcon           := reg.ReadBool('ShowTrayIcon');
       if reg.ValueExists('MinimizeToTray')            then MinimizeToTray         := reg.ReadBool('MinimizeToTray');
       if reg.ValueExists('SearchAfterSymbols')        then SearchAfterSymbols     := reg.ReadInteger('SearchAfterSymbols');
       if reg.ValueExists('RunAsAdmin')                then RunAsAdmin             := reg.ReadBool('RunAsAdmin');
       if reg.ValueExists('HighlightSearchTerms')      then HighlightSearchTerms   := reg.ReadBool('HighlightSearchTerms');
       if reg.ValueExists('ShowRowMouseover')          then ShowRowMouseover       := reg.ReadBool('ShowRowMouseover');
       if reg.ValueExists('StartAppWithSystem')        then StartAppWithSystem     := reg.ReadBool('StartAppWithSystem');
       if reg.ValueExists('IncludeNewFixedVolumes')    then IncludeNewFixedVolumes := reg.ReadBool('IncludeNewFixedVolumes');
       if reg.ValueExists('IncludeNewRemovableVolumes')then IncludeNewRemovableVolumes := reg.ReadBool('IncludeNewRemovableVolumes');
       if reg.ValueExists('RemoveOfflineVolumes')      then RemoveOfflineVolumes    := reg.ReadBool('RemoveOfflineVolumes');
       if reg.ValueExists('ExcludeFolders')            then ExcludeFolders          := reg.ReadBool('ExcludeFolders');
       if reg.ValueExists('ExcludeFoldersList')        then ExcludeFoldersList      := reg.ReadMultiString('ExcludeFoldersList');
       if reg.ValueExists('SizeFormat')                then SizeFormat              := reg.ReadString('SizeFormat');
     end;
     reg.CloseKey;

     if reg.OpenKeyReadOnly(HISTORYKEY) then begin
       reg.GetValueNames(list);
       SearchHistory.Clear;
       for i := 1 to list.Count do SearchHistory.Add(reg.ReadString(list[i - 1]));
     end;
     reg.CloseKey;

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
       reg.WriteInteger('MaxFoundItems', Integer(MaxFoundItems));
       reg.WriteMultiString('VolumesToIndex', VolumesToIndex);
       reg.WriteBool('SearchAsYouType', SearchAsYouType);
       reg.WriteBool('ShowTrayIcon', ShowTrayIcon);
       reg.WriteBool('MinimizeToTray', MinimizeToTray);
       reg.WriteInteger('SearchAfterSymbols', SearchAfterSymbols);
       reg.WriteBool('RunAsAdmin', RunAsAdmin);
       reg.WriteBool('HighlightSearchTerms', HighlightSearchTerms);
       reg.WriteBool('ShowRowMouseover', ShowRowMouseover);
       reg.WriteBool('StartAppWithSystem', StartAppWithSystem);
       reg.WriteBool('IncludeNewFixedVolumes', IncludeNewFixedVolumes);
       reg.WriteBool('IncludeNewRemovableVolumes', IncludeNewRemovableVolumes);
       reg.WriteBool('RemoveOfflineVolumes', RemoveOfflineVolumes);
       reg.WriteBool('Exceludefolders', ExcludeFolders);
       reg.WriteMultiString('ExceludeFoldersList', ExcludeFoldersList);
       reg.WriteString('SizeFormat', SizeFormat);

     end;
     reg.CloseKey;

     reg.DeleteKey(HISTORYKEY); // remove old search history
     if reg.OpenKey(HISTORYKEY, True) then begin
       for i := 1 to SearchHistory.Count do reg.WriteString(i.ToString, SearchHistory[i - 1]);
     end;
     reg.CloseKey;

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

   finally
     reg.Free;
   end;

end;

initialization
  AppSettings := TSettings.Create;

finalization
  FreeAndNil(AppSettings);

end.
