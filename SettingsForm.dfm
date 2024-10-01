object SettingsForm1: TSettingsForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FindFile - Options'
  ClientHeight = 405
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnShow = FormShow
  TextHeight = 15
  object Label5: TLabel
    Left = 13
    Top = 16
    Width = 38
    Height = 15
    Caption = 'Search'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 66
    Top = 23
    Width = 517
    Height = 10
    Shape = bsTopLine
  end
  object Label6: TLabel
    Left = 13
    Top = 120
    Width = 41
    Height = 15
    Caption = 'Sorting'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 66
    Top = 127
    Width = 517
    Height = 10
    Shape = bsTopLine
  end
  object Label2: TLabel
    Left = 13
    Top = 237
    Width = 49
    Height = 15
    Caption = 'Indexing'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel3: TBevel
    Left = 72
    Top = 244
    Width = 511
    Height = 10
    Shape = bsTopLine
  end
  object Label3: TLabel
    Left = 13
    Top = 272
    Width = 79
    Height = 15
    Caption = 'Folder to index'
  end
  object SelectFolderButton: TSpeedButton
    Left = 443
    Top = 266
    Width = 24
    Height = 25
    Hint = 'Select folder to index and press  Index button'
    Caption = '...'
    ParentShowHint = False
    ShowHint = True
    OnClick = SelectFolderButtonClick
  end
  object IndexingProgressLabel: TLabel
    Left = 107
    Top = 328
    Width = 103
    Height = 15
    Caption = 'Indexing progress...'
    Visible = False
  end
  object Label1: TLabel
    Left = 32
    Top = 85
    Width = 149
    Height = 15
    Caption = 'Max number of found items'
  end
  object IndexInfoLabel: TLabel
    Left = 152
    Top = 307
    Width = 237
    Height = 15
    Caption = 'Index is not created, press Build Index button'
  end
  object MaxNumberInfoLabel: TLabel
    Left = 284
    Top = 85
    Width = 192
    Height = 15
    Caption = 'Enter value between  1000 and 20000'
  end
  object CaseSearchCheckBox: TCheckBox
    Left = 32
    Top = 42
    Width = 153
    Height = 25
    Caption = 'Case sensitive search'
    TabOrder = 0
  end
  object CaseSortCheckBox: TCheckBox
    Left = 32
    Top = 155
    Width = 129
    Height = 21
    Caption = 'Case sensitive sort'
    TabOrder = 1
  end
  object FoldersOnTopCheckBox: TCheckBox
    Left = 32
    Top = 188
    Width = 97
    Height = 29
    Caption = 'Folders on top'
    TabOrder = 2
  end
  object FolderToIndexEditBox: TEdit
    Left = 98
    Top = 268
    Width = 345
    Height = 23
    TabOrder = 3
    Text = 'C:\'
  end
  object BuildIndexButton: TButton
    Left = 486
    Top = 267
    Width = 97
    Height = 25
    Caption = 'Build Index...'
    TabOrder = 4
    OnClick = BuildIndexButtonClick
  end
  object ProgressBar2: TProgressBar
    Left = 219
    Top = 328
    Width = 198
    Height = 17
    TabOrder = 5
    Visible = False
  end
  object MaxNumFoundBox: TNumberBox
    Left = 195
    Top = 82
    Width = 78
    Height = 23
    Decimal = 0
    MinValue = 1000.000000000000000000
    MaxValue = 999999.000000000000000000
    MaxLength = 6
    TabOrder = 6
    Value = 20000.000000000000000000
    UseMouseWheel = True
  end
  object OKButton: TButton
    Left = 378
    Top = 368
    Width = 98
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 490
    Top = 368
    Width = 93
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object Button1: TButton
    Left = 486
    Top = 298
    Width = 97
    Height = 25
    Caption = 'Indexing log...'
    TabOrder = 9
    OnClick = Button1Click
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Title = 'Select indexing folder'
    Left = 336
    Top = 176
  end
end
