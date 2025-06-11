unit IndexingLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TIndexingLogForm = class(TForm)
    LogMemo: TMemo;
    procedure LogMemoChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IndexingLogForm: TIndexingLogForm;

implementation

{$R *.dfm}

procedure TIndexingLogForm.LogMemoChange(Sender: TObject);
begin
  Caption := 'Indexing Error Log - ' + IntToStr(LogMemo.Lines.Count);
end;

end.
