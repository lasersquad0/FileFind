unit SettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.NumberBox,
  LoadFSThread, FileNamesCache;

type


  TSettingsForm1 = class(TForm)
    Label5: TLabel;
    Bevel2: TBevel;
    CaseSearchCheckBox: TCheckBox;
    Label6: TLabel;
    CaseSortCheckBox: TCheckBox;
    FoldersOnTopCheckBox: TCheckBox;
    Bevel1: TBevel;
    Label2: TLabel;
    Bevel3: TBevel;
    Label3: TLabel;
    FolderToIndexEditBox: TEdit;
    SelectFolderButton: TSpeedButton;
    BuildIndexButton: TButton;
    IndexingProgressLabel: TLabel;
    ProgressBar2: TProgressBar;
    Label1: TLabel;
    MaxNumFoundBox: TNumberBox;
    OKButton: TButton;
    CancelButton: TButton;
    FileOpenDialog1: TFileOpenDialog;
    IndexInfoLabel: TLabel;
    MaxNumberInfoLabel: TLabel;
    Button1: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SelectFolderButtonClick(Sender: TObject);
    procedure BuildIndexButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FIndexingThread: TLoadFSThread;
    FProgressListener: TIndexingProgress;
    procedure OnThreadTerminate(Sender: TObject);

  public
    { Public declarations }
    ExecData: TExecutionData;

  end;

var
  SettingsForm1: TSettingsForm1;

implementation

{$R *.dfm}

uses
  Registry, Settings, IndexingLog, Functions;


procedure TSettingsForm1.OKButtonClick(Sender: TObject);
begin
   AppSettings.CaseSensitiveSearch := CaseSearchCheckBox.Checked;
   AppSettings.CaseSensitiveSort := CaseSortCheckBox.Checked;
   AppSettings.FoldersOnTop := FoldersOnTopCheckBox.Checked;
   AppSettings.MaxFoundItems := Cardinal(MaxNumFoundBox.ValueInt);
   AppSettings.FolderToIndex := FolderToIndexEditBox.Text;
   AppSettings.Save;
end;

procedure TSettingsForm1.OnThreadTerminate(Sender: TObject);
begin
  IndexInfoLabel.Caption := 'Indexing done in ' + MillisecToStr(FIndexingThread.ExecData.ExecTime);

  TFSC.Instance.RemoveProgressListener(FProgressListener);
  FreeAndNil(FProgressListener);

  ExecData := FIndexingThread.ExecData;
end;

procedure TSettingsForm1.SelectFolderButtonClick(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := FolderToIndexEditBox.Text;
  if FileOpenDialog1.Execute then FolderToIndexEditBox.Text := FileOpenDialog1.FileName;
end;

procedure TSettingsForm1.BuildIndexButtonClick(Sender: TObject);
begin
  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FProgressListener := TIndexingProgress.Create(FIndexingThread, IndexingLogForm.LogMemo.Lines);
  TFSC.Instance.AddProgressListener(FProgressListener);

  FIndexingThread.OnTerminate := OnThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  FIndexingThread.ProgressBar := ProgressBar2;
  FIndexingThread.StartDir := FolderToIndexEditBox.Text;

  FIndexingThread.Start([FolderToIndexEditBox, BuildIndexButton, SelectFolderButton], [ProgressBar2, IndexingProgressLabel], [IndexInfoLabel]);
end;

procedure TSettingsForm1.Button1Click(Sender: TObject);
begin
  IndexingLogForm.ShowModal;
end;

procedure TSettingsForm1.FormShow(Sender: TObject);
begin
  AppSettings.Load; // load settings from registry each time settings form is shown

  CaseSearchCheckBox.Checked :=  AppSettings.CaseSensitiveSearch;
  CaseSortCheckBox.Checked   := AppSettings.CaseSensitiveSort;
  FoldersOnTopCheckBox.Checked := AppSettings.FoldersOnTop;
  MaxNumFoundBox.ValueInt    := Integer(AppSettings.MaxFoundItems);
  FolderToIndexEditBox.Text  := AppSettings.FolderToIndex;

  if TFSC.Instance.Count = 0 then begin
    IndexInfoLabel.Visible := True;
    IndexInfoLabel.Caption := 'Index is not created, press Build Index button';
  end else begin
	  IndexInfoLabel.Visible := False;
  end;
  MaxNumberInfoLabel.Caption := Format('Enter value between %u and %u', [Round(MaxNumFoundBox.MinValue), Round(MaxNumFoundBox.MaxValue)]);

end;


end.
