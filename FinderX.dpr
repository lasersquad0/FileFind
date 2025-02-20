program FinderX;

uses
  Vcl.Forms,
  Windows,
  ShellApi,
  SysUtils,
  FinderXMain in 'FinderXMain.pas' {MainForm},
  LoadFSThread in 'LoadFSThread.pas',
  SettingsForm in 'SettingsForm.pas' {SettingsForm1},
  Settings in 'Settings.pas',
  IndexingLog in 'IndexingLog.pas' {IndexingLogForm},
  Functions in 'Functions.pas',
  About in 'About.pas' {AboutBox},
  StatisticForm in 'StatisticForm.pas' {StatisticForm1},
  MaskSearch in 'MaskSearch.pas',
  HistoryEdit in 'HistoryEdit.pas',
  FileCache in 'FileCache.pas',
  ObjectsCache in 'ObjectsCache.pas',
  DynamicArray in '..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArray.pas',
  Hash in '..\DynamicArrays\dynamicarrays\src\Delphi\Hash.pas',
  Hash2 in '..\DynamicArrays\dynamicarrays\src\Delphi\Hash2.pas',
  SortedArray in '..\DynamicArrays\dynamicarrays\src\Delphi\SortedArray.pas',
  DynamicArrays in '..\DynamicArrays\dynamicarrays\src\Delphi\DynamicArrays.pas';

{$R *.res}

begin
  var start := GetTickCount;

  var mutex := CreateMutex(nil, False, PChar('FinderX'));
  if GetLastError = ERROR_ALREADY_EXISTS then begin
    // copy of FinderX is already running, activating first app copy
    LogMessage('Another copy of FinderX app is running. Activating it.');
    var hWnd := FindWindow('TMainForm', 'FindexX - find files quick!');
    if hWnd = 0 then LogMessage('Cannot find FinderX window.');
    ShowWindow(hWnd, SW_RESTORE {SHOWNORMAL});
    SetForegroundWindow(hWnd);
    CloseHandle(mutex);
    LogMessage('Another copy is activated. Exiting.');
    Exit;
  end;

  AppSettings.Load; // loading settings from registry
  if AppSettings.RunAsAdmin then
    if IsAppRunningAsAdminMode then begin
      LogMessage('Application is running with ADMIN RIGHTS!');
    end else begin
      CloseHandle(mutex); // important to close mutex before running new instance by ShellExecute
      var fname := Application.ExeName;
      var res := ShellExecute(0, 'runas', PChar(fname), nil, nil, SW_SHOWNORMAL);
      if res < 33 then LogMessage('ShellExecute error: ' + IntToStr(res));
      Exit;
    end;

  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm1, SettingsForm1);
  Application.CreateForm(TIndexingLogForm, IndexingLogForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TStatisticForm1, StatisticForm1);
  LogMessage('Application initialization time:' + MillisecToStr(GetTickcount - start));
  Application.Run;

  CloseHandle(mutex);
end.
