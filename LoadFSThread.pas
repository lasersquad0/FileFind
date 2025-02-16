unit LoadFSThread;

interface

uses
  System.Classes, Vcl.Controls, Vcl.ComCtrls, FileCache, DynamicArray;

type

  TLoadFSThread = class(TThread)
  private
    FDisableCtrls: TArray<TControl>;
    FShowCtrls   : TArray<TControl>;
    FHideCtrls   : TArray<TControl>;
    FListener: IIndexingProgress;
    FExclusionsList: TArray<string>;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    ExecData: TArray<TVolumeExecData>;

    constructor Create(Volumes: TArray<string>; ExclusionsList: TArray<string>);
    procedure Start(Listener: IIndexingProgress); overload;
    procedure Start(DisableCtrls, ShowCtrls, HideCtrls: TArray<TControl>; Listener: IIndexingProgress); overload;
    function GetReturnValue: Integer;
end;
                  {
  TIndexingProgress = class(IIndexingProgress)
  private
    FMaxValue: Integer;
    FErrors: TStrings;
    FThread: TLoadFSThread;
  public
    constructor Create(Thread: TLoadFSThread; Output: TStrings = nil);
    procedure Start(P100: Integer; Notes: string); override; // define Max value for progress. -1 means that value for 100% progress is unknown
    procedure Finish; override;
    function  Progress(Prgress: Integer): Boolean; override; // allows to stop process if indexing takes too long time
    procedure ReportError(ErrorStr: string); override;
  end;             }

  // separate thread for closing (freeing) windows find handles once indexing is finished.
  // since closing handles (FindClose WINAPI call) takes too much time according to profiling tests
 { TFindCloseThread = class(TThread)
  private
    FFindHandles: THArrayG<THandle>;
  protected
    procedure Execute; override;
  public
    procedure Start(handles: THArrayG<THandle>); overload;
  end;

  }

implementation

uses SysUtils, System.UITypes, Vcl.Dialogs, Windows, Functions;



{ TLoadFSThread }

constructor TLoadFSThread.Create(Volumes: TArray<string>; ExclusionsList: TArray<string>);
var
  i: Cardinal;
begin
  inherited Create(True); // create suspended
  FreeOnTerminate := True;
  FExclusionsList := ExclusionsList;

  SetLength(ExecData, Length(Volumes));
  for i := Low(Volumes) to High(Volumes) do begin
    ExecData[i].VolumeName := Volumes[i];
    ExecData[i].ExecTime := 0;
    ExecData[i].VolSize := 0;
    ExecData[i].ItemsCount := 0;
  end;
end;

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
var
  i, start: Cardinal;
  inst1, inst2: TCache;
begin
   SetReturnValue(0); // mark that thread didnt finish successfully

   try
     if TFSC.HasNewInstance then TFSC.FreeInst2;

     inst2 := TFSC.NewInstance;
     inst2.AddProgressListener(FListener);

     for i := Low(ExecData) to High(ExecData) do begin
       start := GetTickCount;
       inst2.ReadVolume(ExecData[i].VolumeName, FExclusionsList);
       ExecData[i].VolSize := inst2.GetVolume(ExecData[i].VolumeName).Size;
       ExecData[i].ExecTime := GetTickCount - start;
       LogMessage(Format('[IndexingThread][%d] Finished indexing volume %s. Time spent %s', [ThreadID, ExecData[i].VolumeName, MillisecToStr(ExecData[i].ExecTime)]));
     end;

     inst2.RemoveProgressListener(FListener);
     //inst1 := TFSC.Swap(inst2); // replace old data by new one
    // inst1.Free; // old File Cache Data is not needed any more
     SetReturnValue(1); // finiahed successfully
   except
     on E: EOperationCancelled do begin
       TCache.FreeInst2; // clear half filled Instance2
       Synchronize(procedure begin MessageDlg('User has cancelled file indexing operation.', mtInformation, [mbOK], 0) end);
     end;
     else begin
       TCache.FreeInst2; // clear half filled Instance2
     end;
   end;

  //var FindCloseThread := TFindCloseThread.Create(True); // thread to close all find handles
  //FindCloseThread.FreeOnTerminate := True;
  //FindCloseThread.Start(TFSC.Instance.FindHandles);
end;

function TLoadFSThread.GetReturnValue: Integer;
begin
  Result := ReturnValue;
end;

procedure TLoadFSThread.Start(Listener: IIndexingProgress);
begin
  FListener := Listener;

  //ProgressBar.Position := 0;

  LogMessage('[IndexingThread] STARTING');
  Start;
end;

procedure TLoadFSThread.Start(DisableCtrls, ShowCtrls, HideCtrls: TArray<TControl>; Listener: IIndexingProgress);
var
  i: Integer;
begin
  FDisableCtrls := DisableCtrls;
  FShowCtrls := ShowCtrls;
  FHideCtrls := HideCtrls;
  FListener := Listener;

  for i := 0 to High(FDisableCtrls) do FDisableCtrls[i].Enabled := False;
  for i := 0 to High(FShowCtrls)    do FShowCtrls[i].Visible := True;
  for i := 0 to High(FHideCtrls)    do FHideCtrls[i].Visible := False;

  //ProgressBar.Position := 0;

  LogMessage('[IndexingThread] STARTING');
  Start;
end;

{ TIndexingProgress }
                             {
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
      if Assigned(FErrors) then FErrors.Add('Finished Indexing');
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

procedure TIndexingProgress.Start(P100: Integer; Notes: string);
begin
  FMaxValue := P100;
  TLoadFSThread.Synchronize(FThread,
    procedure
    begin
      if Assigned(FErrors) then FErrors.Add('Start Indexing ' + Notes);
      FThread.ProgressBar.Position := 0;
      //IndexingLogForm.Caption := 'Indexing Error Log - ' + IntToStr(IndexingLogForm.LogMemo.Lines.Count);
    end
    );
end;
                              }
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

{ TFindCloseThread }

   {
procedure TFindCloseThread.Execute;
var i: Cardinal;
begin
  for i := 1 to FFindHandles.Count do Windows.FindClose(FFindHandles[i - 1]);
  FFindHandles.Clear;
end;

procedure TFindCloseThread.Start(handles: THArrayG<THandle>);
begin
  FFindHandles := handles;
  Start;
end; }

end.
