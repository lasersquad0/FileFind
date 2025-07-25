unit HistoryEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.StdCtrls,  Vcl.ExtCtrls, Winapi.ActiveX,  Winapi.ShlObj;

type

  TEnumString = class(TInterfacedObject, IEnumString)
  private
    type
     TPointerList = array[0..0] of Pointer; //avoid bug of Classes.pas declaration TPointerList = array of Pointer;
    var
    FStrings: TStringList;
    FCurrIndex: Integer;
  public
    //IEnumString
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;

    constructor Create;
    destructor Destroy; override;
  end;

  TACOption = (acAutoAppend, acAutoSuggest, acUseArrowKey);
  TACOptions = set of TACOption;

  TACSource = (acsList, acsHistory, acsMRU, acsShell);


  TSearchEdit = class(TLabeledEdit)
  private
    FACList: TEnumString;
    FAutoComplete: IAutoComplete;
    FACEnabled: Boolean;
    FACOptions: TACOptions;
    FACSource: TACSource;
    function GetACStrings: TStringList;
    procedure SetACEnabled(const Value: Boolean);
    procedure SetACOptions(const Value: TACOptions);
    procedure SetACSource(const Value: TACSource);
    procedure SetACStrings(const Value: TStringList);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ACEnabled: Boolean read FACEnabled write SetACEnabled;
    property ACOptions: TACOptions read FACOptions write SetACOptions;
    property ACSource:  TACSource read FACSource write SetACSource;
    property ACStrings: TStringList read GetACStrings write SetACStrings;
  end;


implementation

uses
  System.Win.ComObj, SysUtils, Settings;


{procedure TForm1.FormCreate(Sender: TObject);
begin
  ButtonedEdit1.ACEnabled:=True;
  ButtonedEdit1.ACOptions:=[acAutoAppend, acAutoSuggest, acUseArrowKey];
  ButtonedEdit1.ACSource:=acsList;
  ButtonedEdit1.ACStrings.Add('string 1');
  ButtonedEdit1.ACStrings.Add('string 2');
  ButtonedEdit1.ACStrings.Add('string 3');
  ButtonedEdit1.ACStrings.Add('string 4');
end;
 }

{ TEnumString }

function TEnumString.Clone(out enm: IEnumString): HResult;
begin
  Result := E_NOTIMPL;
  Pointer(enm) := nil;
end;

constructor TEnumString.Create;
begin
  inherited Create;
  FStrings := TStringList.Create(dupIgnore, True, False);
  FCurrIndex := 0;
end;

destructor TEnumString.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TEnumString.Next(celt: Integer; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
  wStr: WideString;
begin
  I := 0;
  while (I < celt) and (FCurrIndex < FStrings.Count) do begin
    wStr := FStrings[FCurrIndex];
    TPointerList(elt)[I] := CoTaskMemAlloc(2 * (Length(wStr) + 1));
    StringToWideChar(wStr, TPointerList(elt)[I], 2 * (Length(wStr) + 1));
    Inc(I);
    Inc(FCurrIndex);
  end;

  if pceltFetched <> nil then
    pceltFetched^ := I;
  if I = celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TEnumString.Reset: HResult;
begin
  FCurrIndex := 0;
  Result := S_OK;
end;

function TEnumString.Skip(celt: Integer): HResult;
begin
  if (FCurrIndex + celt) <= FStrings.Count then
  begin
    Inc(FCurrIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FCurrIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

{ TSearchEdit }

constructor TSearchEdit.Create(AOwner: TComponent);
begin
  inherited;
  EditLabel.Caption := 'Search here';
  FACList := TEnumString.Create;
  FACList.FStrings.Assign(AppSettings.SearchHistory);
  FACEnabled := True;
  FACOptions := [acAutoAppend, acAutoSuggest, acUseArrowKey];
end;

destructor TSearchEdit.Destroy;
begin
  //FACList := nil;
  FACList._Release;
  inherited;
end;

procedure TSearchEdit.CreateWnd;
var
  Dummy: IUnknown;
  Strings: IEnumString;
begin
  inherited;
  if HandleAllocated then begin
    try
      Dummy := CreateComObject(CLSID_AutoComplete);
      if (Dummy <> nil) and (Dummy.QueryInterface(IID_IAutoComplete, FAutoComplete) = S_OK) then begin
        case FACSource of
          acsHistory:Strings := CreateComObject(CLSID_ACLHistory) as IEnumString;
          acsMRU:    Strings := CreateComObject(CLSID_ACLMRU) as IEnumString;
          acsShell:  Strings := CreateComObject(CLSID_ACListISF) as IEnumString;
        else
          Strings := FACList as IEnumString;
        end;
        if S_OK = FAutoComplete.Init(Handle, Strings, nil, nil) then begin
          SetACEnabled(FACEnabled);
          SetACOptions(FACOptions);
        end;
      end;
    except
      //CLSID_IAutoComplete is not available
    end;
  end;
end;

procedure TSearchEdit.DestroyWnd;
begin
  if (FAutoComplete <> nil) then begin
    FAutoComplete.Enable(False);
    FAutoComplete := nil;
  end;
  inherited;
end;

function TSearchEdit.GetACStrings: TStringList;
begin
  Result := FACList.FStrings;
end;

procedure TSearchEdit.SetACEnabled(const Value: Boolean);
begin
  if (FAutoComplete <> nil) then begin
    FAutoComplete.Enable(Value);
  end;
  FACEnabled := Value;
end;

procedure TSearchEdit.SetACOptions(const Value: TACOptions);
const
  Options : array[TACOption] of integer = (ACO_AUTOAPPEND,
                                           ACO_AUTOSUGGEST,
                                           ACO_UPDOWNKEYDROPSLIST);
var
  Option:TACOption;
  Opt: DWORD;
  AC2: IAutoComplete2;
begin
  if (FAutoComplete <> nil) then begin
    if S_OK = FAutoComplete.QueryInterface(IID_IAutoComplete2, AC2) then begin
      Opt := ACO_NONE;
      for Option := Low(Options) to High(Options) do begin
        if (Option in FACOptions) then
          Opt := Opt or DWORD(Options[Option]);
      end;
      AC2.SetOptions(Opt);
    end;
  end;
  FACOptions := Value;
end;

procedure TSearchEdit.SetACSource(const Value: TACSource);
begin
  if FACSource <> Value then begin
    FACSource := Value;
    RecreateWnd;
  end;
end;

procedure TSearchEdit.SetACStrings(const Value: TStringList);
begin
  if Value <> FACList.FStrings then FACList.FStrings.Assign(Value);
end;

end.
