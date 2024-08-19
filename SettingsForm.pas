unit SettingsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.NumberBox,
  LoadFSThread, FileNamesCache;

type

 	TSettingsIndexingProgress = class(IIndexingProgress)
 	private
 		FMaxValue: Integer;
 	public
 		procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
 		procedure Finish; override;
 		function Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
    procedure ReportError(ErrorStr: string); override;
 	end;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FIndexingThread: TLoadFSThread;
    FProgressListener: TSettingsIndexingProgress;
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
  IndexInfoLabel.Visible := True;
  IndexingProgressLabel.Visible := False;
  BuildIndexButton.Enabled := True;
  SelectFolderButton.Enabled := True;
  FolderToIndexEditBox.Enabled := True;
  ProgressBar2.Visible := False;

  ExecData := FIndexingThread.ExecData;
end;

procedure TSettingsForm1.SelectFolderButtonClick(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := FolderToIndexEditBox.Text;
  if FileOpenDialog1.Execute then FolderToIndexEditBox.Text := FileOpenDialog1.FileName;
end;

procedure TSettingsForm1.BuildIndexButtonClick(Sender: TObject);
begin
	IndexInfoLabel.Visible := False;
  FolderToIndexEditBox.Enabled := False;
  BuildIndexButton.Enabled := False;
  SelectFolderButton.Enabled := False;
  ProgressBar2.Visible := True;
  ProgressBar2.Position := 0;
  IndexingProgressLabel.Visible := True;

  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FIndexingThread.OnTerminate := OnThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  //FIndexingThread.ProgressCallback := IndexingProgressCallback;
  FIndexingThread.StartDir := FolderToIndexEditBox.Text;
  FIndexingThread.Start;
end;

procedure TSettingsForm1.Button1Click(Sender: TObject);
begin
  IndexingLogForm.ShowModal;
end;

procedure TSettingsForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TFSC.Instance.RemoveProgressListener(FProgressListener);
end;

procedure TSettingsForm1.FormCreate(Sender: TObject);
begin
  FProgressListener := TSettingsIndexingProgress.Create;
end;

procedure TSettingsForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProgressListener);
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

  TFSC.Instance.AddProgressListener(FProgressListener);
end;

{ TSettingsIndexingProgress }

procedure TSettingsIndexingProgress.Finish;
begin
   TLoadFSThread.Synchronize(SettingsForm1.FIndexingThread,
   procedure
   begin
      IndexingLogForm.LogMemo.Lines.Add('Finished Indexing');
      IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
   end
   );
end;

function TSettingsIndexingProgress.Progress(Prgress: Integer): Boolean;
begin
  TLoadFSThread.Synchronize(SettingsForm1.FIndexingThread,
	  procedure
  	begin
    	with SettingsForm1 do
    		ProgressBar2.Position := ProgressBar2.Position + (ProgressBar2.Max - ProgressBar2.Position) div 20; // logrithmic progress since we do not know total progress value
   	end
    );
  Result := True;
end;

procedure TSettingsIndexingProgress.ReportError(ErrorStr: string);
begin
//   inherited;
   TLoadFSThread.Synchronize(SettingsForm1.FIndexingThread,
   procedure
   begin
     IndexingLogForm.LogMemo.Lines.Add(ErrorStr);
     IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
   end
   );
end;

procedure TSettingsIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
  TLoadFSThread.Synchronize(SettingsForm1.FIndexingThread,
    procedure
    begin
      IndexingLogForm.LogMemo.Lines.Add('Start Indexing');
      IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
    end
    );
end;

end.
