unit Settings;

interface

uses Classes;

type

  TSettings = class
  public
    CaseSensitiveSearch: Boolean;
    CaseSensitiveSort: Boolean;
    FoldersOnTop: Boolean;
    HideFoldersSize: Boolean;
    EnableSearchHistory: Boolean;
    MaxFoundItems: Cardinal;
    FolderToIndex: string;
    SearchHistory: TStringList;

    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
  const
    APPKEY = 'SOFTWARE\NotepadCo\FileFind';
    HISTORYKEY = APPKEY + '\SearchHistory';
  end;

var
  AppSettings: TSettings;

implementation

uses
  System.SysUtils, WinAPI.Windows, Registry;

{ TSettings }

constructor TSettings.Create;
begin
  inherited;

  // default settings values
  CaseSensitiveSearch := False;
  CaseSensitiveSort := False;
  HideFoldersSize := False;
  EnableSearchHistory := True;
  FoldersOnTop := False;
  MaxFoundItems := 20000;
  FolderToIndex := 'c:\';
  SearchHistory := TStringList.Create(dupIgnore, True, False); // important to have these values in Create for proper work of history fill/update;
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
begin
   reg := TRegistry.Create;
   list := TStringList.Create();
   try
     reg.RootKey := HKEY_CURRENT_USER;
     if reg.OpenKeyReadOnly(APPKEY) then begin
       if reg.ValueExists('CaseSensitiveSearch')then CaseSensitiveSearch := reg.ReadBool('CaseSensitiveSearch');
       if reg.ValueExists('CaseSensitiveSort')  then CaseSensitiveSort := reg.ReadBool('CaseSensitiveSort');
       if reg.ValueExists('HideFoldersSize')    then HideFoldersSize := reg.ReadBool('HideFoldersSize');
       if reg.ValueExists('EnableSearchHistory')then EnableSearchHistory := reg.ReadBool('EnableSearchHistory');
       if reg.ValueExists('FoldersOnTop')       then FoldersOnTop := reg.ReadBool('FoldersOnTop');
       if reg.ValueExists('MaxFoundItems')      then MaxFoundItems := Cardinal(reg.ReadInteger('MaxFoundItems'));
       if reg.ValueExists('FolderToIndex')      then FolderToIndex := reg.ReadString('FolderToIndex');
     end;
     reg.CloseKey;
     if reg.OpenKeyReadOnly(HISTORYKEY) then begin
       reg.GetValueNames(list);
       SearchHistory.Clear;
       for i := 1 to list.Count do SearchHistory.Add(reg.ReadString(list[i - 1]));
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
       reg.WriteString('FolderToIndex', FolderToIndex);
     end;
     reg.CloseKey;
     reg.DeleteKey(HISTORYKEY); // remove old search history
     if reg.OpenKey(HISTORYKEY, True) then begin
       for i := 1 to SearchHistory.Count do reg.WriteString(IntToStr(i), SearchHistory[i - 1]);
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
