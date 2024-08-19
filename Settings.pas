unit Settings;

interface

type

  TSettings = class
  public
		CaseSensitiveSearch: Boolean;
    CaseSensitiveSort: Boolean;
    FoldersOnTop: Boolean;
    MaxFoundItems: Cardinal;
    FolderToIndex: string;

    constructor Create;
    procedure Save;
    procedure Load;
  const
    APPKEY = 'SOFTWARE\NotepadCo\FileFind';
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
  FoldersOnTop := False;
  MaxFoundItems := 20000;
  FolderToIndex := 'c:\';
end;

procedure TSettings.Load;
var
  reg: TRegistry;
begin
   reg := TRegistry.Create;
   try
     reg.RootKey := HKEY_CURRENT_USER;
     if reg.OpenKeyReadOnly(APPKEY) then begin
       if reg.ValueExists('CaseSensitiveSearch')then CaseSensitiveSearch := reg.ReadBool('CaseSensitiveSearch');
       if reg.ValueExists('CaseSensitiveSort')  then CaseSensitiveSort := reg.ReadBool('CaseSensitiveSort');
       if reg.ValueExists('FoldersOnTop')       then FoldersOnTop := reg.ReadBool('FoldersOnTop');
       if reg.ValueExists('MaxFoundItems')      then MaxFoundItems := Cardinal(reg.ReadInteger('MaxFoundItems'));
       if reg.ValueExists('FolderToIndex')      then FolderToIndex := reg.ReadString('FolderToIndex');
     end;
   finally
     reg.Free;
   end;
end;

procedure TSettings.Save;
var
  reg: TRegistry;
begin
   reg := TRegistry.Create;
   try
  	 reg.RootKey := HKEY_CURRENT_USER;
   	 reg.OpenKey(APPKEY, True);
   	 reg.WriteBool('CaseSensitiveSearch', CaseSensitiveSearch);
   	 reg.WriteBool('CaseSensitiveSort', CaseSensitiveSort);
   	 reg.WriteBool('FoldersOnTop', FoldersOnTop);
   	 reg.WriteInteger('MaxFoundItems', Integer(MaxFoundItems));
     reg.WriteString('FolderToIndex', FolderToIndex);
   finally
     reg.Free;
   end;

end;

initialization
  AppSettings := TSettings.Create;

finalization
  FreeAndNil(AppSettings);

end.
