unit LoadFSThread;

interface

uses
  System.Classes, Vcl.Controls, Vcl.ComCtrls, FileNamesCache;

type
  TExecutionData = record
    StartDir: string;
    ExecTime: Cardinal;
    DirSize: uint64;
  end;

  TLoadFSThread = class(TThread)    //TODO: put thread into inside cache class. So, cache itsef could do job in separate thread
  private
    FDisableCtrls: TArray<TControl>;
    FShowCtrls   : TArray<TControl>;
    FHideCtrls   : TArray<TControl>;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    ProgressBar: TProgressBar;
    StartDir: string;
    ExecData: TExecutionData;
    procedure Start(DisableCtrls, ShowCtrls, HideCtrls: TArray<TControl>); overload;
   // ProgressCallback: TFNCIndexingProgress;
    //constructor Create(cache: TFileNamesCache; startDir: string);
  end;

  TIndexingProgress = class(IIndexingProgress)
  private
    FMaxValue: Integer;
    FErrors: TStrings;
    FThread: TLoadFSThread;
  public
    constructor Create(Thread: TLoadFSThread; Output: TStrings = nil);
    procedure Start(P100: Integer); override; // define Max value for progress. -1 means that value for 100% progress is unknown
    procedure Finish; override;
    function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
    procedure ReportError(ErrorStr: string); override;
  end;


implementation


{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TLoadFSThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end;

    or

    Synchronize(
      procedure
      begin
        Form1.Caption := 'Updated in thread via an anonymous method'
      end
      )
    );

  where an anonymous method is passed.

  Similarly, the developer can call the Queue method with similar parameters as
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.

}

{ TLoadFSThread }

{constructor TLoadFSThread.Create(cache: TFileNamesCache; startDir: string);
begin
  inherited Create(True); // create suspended
  FCache := cache;
  FStartDir := startDir;
end;
 }

procedure TLoadFSThread.DoTerminate;
var
  i: Integer;
begin
  for i := 0 to High(FDisableCtrls) do FDisableCtrls[i].Enabled := True;
  for i := 0 to High(FShowCtrls)    do FShowCtrls[i].Visible    := False;
  for i := 0 to High(FHideCtrls)    do FHideCtrls[i].Visible    := True;

  inherited;
end;

procedure TLoadFSThread.Execute;
begin
   var start := GetTickCount;
   ExecData.DirSize := TFSC.Instance.ReadFileSystem(StartDir);
   var stop := GetTickCount;
   ExecData.ExecTime := stop - start;
   ExecData.StartDir := StartDir;
end;

procedure TLoadFSThread.Start(DisableCtrls, ShowCtrls, HideCtrls: TArray<TControl>);
var
  i: Integer;
begin
  FDisableCtrls := DisableCtrls;
  FShowCtrls := ShowCtrls;
  FHideCtrls := HideCtrls;

  for i := 0 to High(FDisableCtrls) do
    FDisableCtrls[i].Enabled := False;

  for i := 0 to High(FShowCtrls) do
    FShowCtrls[i].Visible := True;

  for i := 0 to High(FHideCtrls) do
    FHideCtrls[i].Visible := False;

  ProgressBar.Position := 0;

  Start;
end;

{ TIndexingProgress }

constructor TIndexingProgress.Create(Thread: TLoadFSThread; Output: TStrings);
begin
  inherited Create;
  FErrors := Output;
  FThread := Thread;
end;

procedure TIndexingProgress.Finish;
begin
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
      if Assigned(FErrors) then FERrors.Add('Finished Indexing');
      //IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
   end
   );
end;

function TIndexingProgress.Progress(Prgress: Integer): Boolean;
begin
  TLoadFSThread.Synchronize(FThread,
    procedure
    begin
        FThread.ProgressBar.Position := FThread.ProgressBar.Position + (FThread.ProgressBar.Max - FThread.ProgressBar.Position) div 20; // logrithmic progress since we do not know total progress value
    end
    );
  Result := True;
end;

procedure TIndexingProgress.ReportError(ErrorStr: string);
begin
//   inherited;
   TLoadFSThread.Synchronize(FThread,
   procedure
   begin
     if Assigned(FErrors) then FErrors.Add(ErrorStr);
     //IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
   end
   );
end;

procedure TIndexingProgress.Start(P100: Integer);
begin
  FMaxValue := P100;
  TLoadFSThread.Synchronize(FThread,
    procedure
    begin
      if Assigned(FErrors) then FErrors.Add('Start Indexing');
      //IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
    end
    );
end;


end.
