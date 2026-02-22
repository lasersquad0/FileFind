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
  CacheItem in '..\..\src\CacheItem.pas',
  DynamicArray in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArray.pas',
  DynamicArrays in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArrays.pas',
  Hash in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\Hash.pas',
  Hash2 in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\Hash2.pas',
  SortedArray in '..\..\..\DynamicArrays\dynamicarrays\src\Delphi\SortedArray.pas',
  Logger in '..\..\src\Logger.pas';

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
    TLogger.Info('Another copy of FinderX app is running. Activating it.');
    var hWnd := FindWindow('TMainForm', PChar(sMainWindowTitle));
    if hWnd = 0 then TLogger.Warn('Cannot find FinderX window while mutex already exists.');
    SendMessage(hWnd, WM_RESTORE_MAINFORM_MSG, 0, 0); //send user message to restore main form from tray
    CloseHandle(mutex);
    TLogger.Info('Another FinderX copy is being activated. Exiting.');
    Exit;
  end;

  if AppSettings.RunAsAdmin then begin
    if IsAppRunningAsAdminMode then begin
      TLogger.Warn('Application is running with ADMIN RIGHTS!');
    end else begin
      CloseHandle(mutex); // important to close mutex before running new instance by ShellExecute
      var fname := Application.ExeName;
      var res := ShellExecute(0, 'runas', PChar(fname), nil, nil, SW_SHOWNORMAL); // try to run app with admin rights
      if res < 33 then TLogger.Error('ShellExecute run as admin error: ' + IntToStr(res));
      Exit;
    end;
  end;

  TLogger.Info('Before Application.Initialize:' + MillisecToStr(GetTickcount - start) + ' (time from start)');
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  TLogger.Info('After Application.Initialize:' + MillisecToStr(GetTickcount - start) + ' (time from start)');
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TIndexingLogForm, IndexingLogForm);
  Application.CreateForm(TSettingsForm1, SettingsForm1);
  TLogger.Info('All application forms are created:' + MillisecToStr(GetTickcount - start) + ' (time from start)');
  Application.Run;

  CloseHandle(mutex); // leave mutex open while app is running, important to close mutex in the beginning of app shutdown

  TLogger.Info('FinderX - FINISH');
end.
