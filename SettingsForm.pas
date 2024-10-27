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
    ProgressBar1: TProgressBar;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FIndexingThread: TLoadFSThread;
    FProgressListener: IIndexingProgress;
    FCancel: Boolean;
    procedure OnThreadTerminate(Sender: TObject);
  public
    ExecData: TExecutionData;
    property Cancel: Boolean read FCancel;
  end;

 TSettingsFormIndexingProgress = class(IIndexingProgress)
 private
   FMaxValue: Integer;
   FThread: TLoadFSThread;
   FErrors: TStrings;
 public
   constructor Create(Thread: TLoadFSThread; Output: TStrings);
   procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
   procedure Finish; override;
   function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
   procedure ReportError(ErrorStr: string); override;
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
  FCancel := False;
end;

procedure TSettingsForm1.SelectFolderButtonClick(Sender: TObject);
begin
  FileOpenDialog1.DefaultFolder := FolderToIndexEditBox.Text;
  if FileOpenDialog1.Execute then FolderToIndexEditBox.Text := FileOpenDialog1.FileName;
end;

procedure TSettingsForm1.BuildIndexButtonClick(Sender: TObject);
begin
  FCancel := False;
  FIndexingThread := TLoadFSThread.Create(True); // create suspended
  FProgressListener := TSettingsFormIndexingProgress.Create(FIndexingThread, IndexingLogForm.LogMemo.Lines);
  TFSC.Instance.AddProgressListener(FProgressListener);

  FIndexingThread.OnTerminate := OnThreadTerminate;
  FIndexingThread.FreeOnTerminate := True;
  FIndexingThread.ProgressBar := ProgressBar1;
  FIndexingThread.StartDir := FolderToIndexEditBox.Text;

  FIndexingThread.Start([FolderToIndexEditBox, BuildIndexButton, SelectFolderButton], [ProgressBar1, IndexingProgressLabel], [IndexInfoLabel]);
end;

procedure TSettingsForm1.Button1Click(Sender: TObject);
begin
  IndexingLogForm.ShowModal;
end;

procedure TSettingsForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FProgressListener) then begin
    FCancel := mrYes = MessageDlg('Indexing is in progress. Are you sure you want to cancel indexing and close Settings dialog?', TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0, mbYes);
  //MessageDlg('Cannot close form because indexing is in progress.', TMsgDlgType.mtInformation, [mbOK], 0);
    while FCancel do begin TThread.Sleep(200); Application.ProcessMessages; end; //waiting till thread finishes
  end;

  CanClose := NOT Assigned(FProgressListener);
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

procedure TSettingsFormIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
    TLoadFSThread.Synchronize(FThread,
    procedure
    begin
      if Assigned(FErrors) then FErrors.Add('Start Indexing');
    end
    );
end;


end.
