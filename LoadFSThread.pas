unit LoadFSThread;

interface

uses
  System.Classes, FileNamesCache;

type
  TExecutionData = record
    StartDir: string;
    ExecTime: Cardinal;
    DirSize: uint64;
  end;

  TLoadFSThread = class(TThread)    //TODO: put thread into inside cache class. So, cache itsef could do job in separate thread
  private
  protected
    procedure Execute; override;
  public
    //Cache: TFileNamesCache;
    StartDir: string;
    ExecData: TExecutionData;
   // ProgressCallback: TFNCIndexingProgress;
    //constructor Create(cache: TFileNamesCache; startDir: string);
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

procedure TLoadFSThread.Execute;
begin
   var start := GetTickCount;
   ExecData.DirSize := TFSC.Instance.ReadFileSystem(StartDir);
   var stop := GetTickCount;
   ExecData.ExecTime := stop - start;
   ExecData.StartDir := StartDir;
end;

end.
