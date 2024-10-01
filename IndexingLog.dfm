object IndexingLogForm: TIndexingLogForm
  Left = 0
  Top = 0
  Caption = 'Intexing error log'
  ClientHeight = 237
  ClientWidth = 675
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object LogMemo: TMemo
    Left = 0
    Top = 0
    Width = 675
    Height = 237
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    OnChange = LogMemoChange
  end
end
