unit AboutForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TFrmAbout = class(TForm)
    ImgLogo: TImage;
    LblAppName: TLabel;
    LblVersion: TLabel;
    LblCopyright: TLabel;
    BtnOk: TButton;
    MemoCredits: TMemo;
    Label1: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.dfm}

procedure TFrmAbout.BtnOkClick(Sender: TObject);
begin
 // Close;
end;

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  VerInfoSize, VerValueSize: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  AppName, AppVersion: string;
begin
  // path to .exe file
  AppName := ParamStr(0);

  // Size of version info section
  VerInfoSize := GetFileVersionInfoSize(PChar(AppName), Dummy);
  if VerInfoSize > 0 then begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(AppName), 0, VerInfoSize, VerInfo) then begin
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);

        // Make text version (Major.Minor.Release.Build)
        AppVersion := Format('%d.%d.%d.%d',
          [HiWord(VerValue.dwFileVersionMS),
           LoWord(VerValue.dwFileVersionMS),
           HiWord(VerValue.dwFileVersionLS),
           LoWord(VerValue.dwFileVersionLS)]);

        lblVersion.Caption := 'Version ' + AppVersion;
      end;
    finally
      FreeMem(VerInfo);
    end;
  end;
end;

end.
