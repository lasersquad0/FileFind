program FinderX;

uses
  Vcl.Forms,
  Windows,
  ShellApi,
  SysUtils,
  Classes,
  Vcl.Dialogs,
  System.UITypes,
  About in '..\..\src\About.pas' {AboutBox},
  FileCache in '..\..\src\FileCache.pas',
  FinderXMain in '..\..\src\FinderXMain.pas' {MainForm},
  Functions in '..\..\src\Functions.pas',
  HistoryEdit in '..\..\src\HistoryEdit.pas',
  IndexingLog in '..\..\src\IndexingLog.pas' {IndexingLogForm},
  LoadFSThread in '..\..\src\LoadFSThread.pas',
  MaskSearch in '..\..\src\MaskSearch.pas',
  ObjectsCache in '..\..\src\ObjectsCache.pas',
  Settings in '..\..\src\Settings.pas',
  SettingsForm in '..\..\src\SettingsForm.pas' {SettingsForm1},
  StatisticForm in '..\..\src\StatisticForm.pas' {StatisticForm1},
  CacheItem in '..\..\src\CacheItem.pas',
  DynamicArray in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArray.pas',
  DynamicArrays in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArrays.pas',
  Hash in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\Hash.pas',
  Hash2 in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\Hash2.pas',
  SortedArray in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\SortedArray.pas';

{$R *.res}

begin
  var start := GetTickCount;

  // first thing: load settings from registry and init logger
  AppSettings.Load;
  try
    //Logger.Init raises an exception when LoaFileName has incorrect path or file name
    if AppSettings.WriteLogFile then TLogger.Init(AppSettings.LogFileName);
  except
    on ex: EFOpenError do begin
      MessageDlg(ex.Message, TMsgDlgType.mtWarning, [mbOK], 0);
    end;
  end;

  var mutex := CreateMutex(nil, False, PChar('FinderX'));
  if GetLastError = ERROR_ALREADY_EXISTS then begin
    // copy of FinderX is already running, activating first app copy
    TLogger.Log('Another copy of FinderX app is running. Activating it.');
    var hWnd := FindWindow('TMainForm', 'FindexX - find files quick!');
    if hWnd = 0 then TLogger.Log('Cannot find FinderX window while mutex already exists.');
    SendMessage(hWnd, WM_RESTORE_MAINFORM_MSG, 0, 0); //send user message to restore main form from tray
    //ShowWindow(hWnd, SW_RESTORE {SHOWNORMAL});
    //SetForegroundWindow(hWnd);
    CloseHandle(mutex);
    TLogger.Log('Another copy is being activated. Exiting.');
    Exit;
  end;

  if AppSettings.RunAsAdmin then begin
    if IsAppRunningAsAdminMode then begin
      TLogger.Log('Application is running with ADMIN RIGHTS!');
    end else begin
      CloseHandle(mutex); // important to close mutex before running new instance by ShellExecute
      var fname := Application.ExeName;
      var res := ShellExecute(0, 'runas', PChar(fname), nil, nil, SW_SHOWNORMAL);
      if res < 33 then TLogger.Log('ShellExecute run as admin error: ' + IntToStr(res));
      Exit;
    end;
  end;

  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  TLogger.Log('Application initialization time cutoff 1:' + MillisecToStr(GetTickcount - start));
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TIndexingLogForm, IndexingLogForm);
  Application.CreateForm(TSettingsForm1, SettingsForm1);
  Application.CreateForm(TStatisticForm1, StatisticForm1);
  TLogger.Log('Application initialization time cutoff 2:' + MillisecToStr(GetTickcount - start));
  Application.Run;

  CloseHandle(mutex); // important to close mutex in the beginning of app shutdown
end.
