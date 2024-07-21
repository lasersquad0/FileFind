program FileFind;

uses
  Vcl.Forms,
  FileFindMain in 'FileFindMain.pas' {MainForm},
  FileNamesCache in 'FileNamesCache.pas',
  DynamicArray in '..\DA\dynamicarrays\src\Delphi\DynamicArray.pas',
  LoadFSThread in 'LoadFSThread.pas',
  SettingsForm in 'SettingsForm.pas' {SettingsForm1},
  Settings in 'Settings.pas',
  IndexingLog in 'IndexingLog.pas' {IndexingLogForm},
  Functions in 'Functions.pas',
  ABOUT in 'About.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSettingsForm1, SettingsForm1);
  Application.CreateForm(TIndexingLogForm, IndexingLogForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
