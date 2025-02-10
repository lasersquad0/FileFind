program FileFind;

uses
  Vcl.Forms, Windows,
  FileFindMain in 'FileFindMain.pas' {MainForm},
  DynamicArray in '..\DA\dynamicarrays\src\Delphi\DynamicArray.pas',
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
  DynamicArrays in '..\DA\dynamicarrays\src\Delphi\DynamicArrays.pas',
  ObjectsCache in 'ObjectsCache.pas',
  Hash2 in '..\DA\dynamicarrays\src\Delphi\Hash2.pas';

{$R *.res}

begin
  var start := GetTickCount;
  //LogMessage('BEGIN START');

  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm1, SettingsForm1);
  Application.CreateForm(TIndexingLogForm, IndexingLogForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TStatisticForm1, StatisticForm1);

  LogMessage('Application initialization time:' + MillisecToStr(GetTickcount - start));
  //LogMessage('STARTED');
  Application.Run;
end.
