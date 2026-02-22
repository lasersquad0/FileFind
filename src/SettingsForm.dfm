object SettingsForm1: TSettingsForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FinderX - Options'
  ClientHeight = 363
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    476
    363)
  TextHeight = 15
  object OKButton: TButton
    Left = 262
    Top = 330
    Width = 98
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 374
    Top = 330
    Width = 93
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Sections: TListBox
    Left = 8
    Top = 10
    Width = 97
    Height = 297
    ExtendedSelect = False
    ItemHeight = 15
    Items.Strings = (
      'General'
      'Search'
      'Search Results'
      'Indexing'
      'Excludes')
    TabOrder = 2
    OnClick = SectionsClick
  end
  object SettingsPanels: TCardPanel
    Left = 110
    Top = 10
    Width = 363
    Height = 297
    ActiveCard = Card3
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'SettingsPanels'
    TabOrder = 3
    object Card1: TCard
      Left = 2
      Top = 2
      Width = 359
      Height = 293
      Caption = 'General'
      CardIndex = 0
      ShowCaption = True
      TabOrder = 0
      VerticalAlignment = taAlignTop
      object LogFileLabel: TLabel
        Left = 28
        Top = 188
        Width = 72
        Height = 15
        Caption = 'Log file name'
      end
      object MinimizeToTrayCheckBox: TCheckBox
        Left = 28
        Top = 55
        Width = 240
        Height = 21
        Hint = 
          'When checked application will be removed from Windows Task Bar w' +
          'hen minimized.'#13#10'FinderX will be available from system tray where' +
          ' icon will be shown.'
        Caption = 'Minimize to tray'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object ShowTrayIconCheckBox: TCheckBox
        Left = 9
        Top = 28
        Width = 240
        Height = 21
        Hint = 'Show FinderX icon in system tray while application is running.'
        Caption = 'Show tray icon'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = ShowTrayIconCheckBoxClick
      end
      object RunAsAdminCheckBox: TCheckBox
        Left = 8
        Top = 80
        Width = 240
        Height = 21
        Hint = 
          'If checked FinderX will Run with administrative rights next time' +
          ' you launch FinderX again.'
        Caption = 'Run as administrator'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object StartWithWindowsCheckBox: TCheckBox
        Left = 8
        Top = 106
        Width = 240
        Height = 21
        Hint = 'Automatically run FinderX when Windows starts.'
        Caption = 'Start FinderX on system startup'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = StartWithWindowsCheckBoxClick
      end
      object LogFileCheckBox: TCheckBox
        Left = 9
        Top = 158
        Width = 250
        Height = 21
        Hint = 
          'Diagnostic information will be written into specified file when ' +
          'checked.'
        Caption = 'Write diagnostic info into log file'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = LogFileCheckBoxClick
      end
      object LogFileEdit: TEdit
        Left = 129
        Top = 185
        Width = 159
        Height = 23
        TabOrder = 5
        Text = 'FinderX_debug.log'
      end
      object StartMinimizedCheckBox: TCheckBox
        Left = 28
        Top = 133
        Width = 240
        Height = 21
        Hint = 'Start with main window minimized into system tray.'
        Caption = 'Start minimized'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
    end
    object Card2: TCard
      Left = 2
      Top = 2
      Width = 359
      Height = 293
      Caption = 'Search'
      CardIndex = 1
      ShowCaption = True
      TabOrder = 1
      VerticalAlignment = taAlignTop
      object SearchAsYouTypeLabel2: TLabel
        Left = 193
        Top = 55
        Width = 77
        Height = 15
        Caption = 'symbols typed'
      end
      object SearchAsYouTypeLabel1: TLabel
        Left = 36
        Top = 55
        Width = 91
        Height = 15
        Hint = 
          'Search will start automatically only after you entered specified' +
          ' number of symbols.'
        Caption = 'Start search after '
        ParentShowHint = False
        ShowHint = True
      end
      object CaseSearchCheckBox: TCheckBox
        Left = 9
        Top = 105
        Width = 200
        Height = 21
        Hint = 'Perform case sensitive search.'
        Caption = 'Case sensitive search'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object EnableSearchHistoryCheckBox: TCheckBox
        Left = 9
        Top = 79
        Width = 170
        Height = 21
        Caption = 'Enable search history'
        TabOrder = 1
      end
      object SearchAfterNumberBox: TNumberBox
        Left = 147
        Top = 51
        Width = 35
        Height = 23
        Hint = 
          'Search will not start automatically until you enter this number ' +
          'of symbols.'
        Decimal = 0
        MinValue = 1.000000000000000000
        MaxValue = 100.000000000000000000
        MaxLength = 6
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Value = 3.000000000000000000
        UseMouseWheel = True
      end
      object SearchAsYouTypeCheckBox: TCheckBox
        Left = 9
        Top = 28
        Width = 200
        Height = 21
        Hint = 
          'Do not require pressing Search button to start search. Start sea' +
          'rch as you type symbols in search edit box.'#13#10'Search will be star' +
          'ted only of you entered certain number of symbols. '#13#10'See '#39'Start ' +
          'search after'#39' option.'
        Caption = 'Search as you type'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = SearchAsYouTypeCheckBoxClick
      end
      object ClearHistoryButton: TButton
        Left = 183
        Top = 80
        Width = 142
        Height = 25
        Caption = 'Clear history (90 items)'
        TabOrder = 4
        OnClick = ClearHistoryButtonClick
      end
    end
    object Card3: TCard
      Left = 2
      Top = 2
      Width = 359
      Height = 293
      Caption = 'Search Results'
      CardIndex = 2
      ShowCaption = True
      TabOrder = 2
      VerticalAlignment = taAlignTop
      object Label1: TLabel
        Left = 8
        Top = 162
        Width = 159
        Height = 15
        Caption = 'Max number of items to show'
      end
      object SizeFormatLabel: TLabel
        Left = 9
        Top = 188
        Width = 59
        Height = 15
        Hint = 'Format in which file/folders sizes are shown in search results.'
        Caption = 'Size format'
        ParentShowHint = False
        ShowHint = True
      end
      object FoldersOnTopCheckBox: TCheckBox
        Left = 8
        Top = 54
        Width = 220
        Height = 21
        Hint = 
          'Makes Windows Explorer like sorting of folders when all folders ' +
          'are put on top regardless of their names sorting order.'
        Caption = 'Put folders on top during sorting'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object MaxNumFoundBox: TNumberBox
        Left = 190
        Top = 159
        Width = 57
        Height = 23
        Decimal = 0
        MinValue = 1000.000000000000000000
        MaxValue = 999999.000000000000000000
        MaxLength = 6
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Value = 20000.000000000000000000
        UseMouseWheel = True
      end
      object CaseSortCheckBox: TCheckBox
        Left = 8
        Top = 80
        Width = 220
        Height = 21
        Caption = 'Case sensitive sort'
        TabOrder = 2
      end
      object HideFoldersSizeCheckbox: TCheckBox
        Left = 8
        Top = 28
        Width = 220
        Height = 21
        Hint = 
          'Show '#39'-'#39' instead of folders size. This option might be useful wh' +
          'en you are searching files by file size.'
        Caption = 'Do not show folders size'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
      object SizeFormatComboBox: TComboBox
        Left = 98
        Top = 183
        Width = 64
        Height = 23
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'auto'
        Items.Strings = (
          'auto'
          'Bytes'
          'KB'
          'MB')
      end
      object ShowRowOnMouseOverCheckBox: TCheckBox
        Left = 8
        Top = 132
        Width = 220
        Height = 21
        Caption = 'Show row mouseover'
        TabOrder = 5
      end
      object HighlightSearchTermsCheckBox: TCheckBox
        Left = 8
        Top = 106
        Width = 220
        Height = 21
        Hint = 
          'Turn on or off highlighting of searched symbols in search result' +
          's.'
        Caption = 'Highlight search terms'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
    end
    object Card4: TCard
      Left = 2
      Top = 2
      Width = 359
      Height = 293
      Caption = 'Indexing'
      CardIndex = 3
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      ShowCaption = True
      TabOrder = 3
      VerticalAlignment = taAlignTop
      object Label2: TLabel
        Left = 8
        Top = 216
        Width = 94
        Height = 15
        Caption = 'Index file location'
      end
      object IncludeNewFixedDrivesCheckBox: TCheckBox
        Left = 8
        Top = 28
        Width = 300
        Height = 17
        Caption = 'Automatically include new fixed volumes'
        TabOrder = 0
      end
      object IncludeNewRemovableDrivesCheckBox: TCheckBox
        Left = 8
        Top = 54
        Width = 300
        Height = 17
        Caption = 'Automatically include new removable volumes'
        TabOrder = 1
      end
      object RemoveOfflineDrivesCheckBox: TCheckBox
        Left = 8
        Top = 80
        Width = 300
        Height = 17
        Caption = 'Automatically remove offline volumnes'
        TabOrder = 2
      end
      object VolumesListBox: TListBox
        Left = 8
        Top = 106
        Width = 238
        Height = 95
        ItemHeight = 15
        TabOrder = 3
        TabWidth = 50
      end
      object RemoveDriveButton: TButton
        Left = 250
        Top = 106
        Width = 105
        Height = 23
        Caption = 'Remove'
        TabOrder = 4
        OnClick = RemoveDriveButtonClick
      end
      object IndexLocationEdit: TEdit
        Left = 8
        Top = 237
        Width = 321
        Height = 23
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 5
        Text = '\\location'
      end
    end
    object Card5: TCard
      Left = 2
      Top = 2
      Width = 359
      Height = 293
      Caption = 'Excludes'
      CardIndex = 4
      ShowCaption = True
      TabOrder = 4
      VerticalAlignment = taAlignTop
      object ExcludeFoldersCheckBox: TCheckBox
        Left = 8
        Top = 28
        Width = 260
        Height = 17
        Caption = 'Exclude folders from list below'
        TabOrder = 0
        OnClick = ExcludeFoldersCheckBoxClick
      end
      object ExcludeFoldersListBox: TListBox
        Left = 8
        Top = 54
        Width = 238
        Height = 230
        ExtendedSelect = False
        ItemHeight = 15
        TabOrder = 1
      end
      object AddFolderButton: TButton
        Left = 250
        Top = 54
        Width = 105
        Height = 23
        Caption = 'Add Folder...'
        TabOrder = 2
        OnClick = AddFolderButtonClick
      end
      object EditFolderButton: TButton
        Left = 250
        Top = 79
        Width = 105
        Height = 23
        Caption = 'Edit Folder...'
        TabOrder = 3
        OnClick = EditFolderButtonClick
      end
      object RemoveFolderButton: TButton
        Left = 250
        Top = 108
        Width = 105
        Height = 23
        Caption = 'Remove'
        TabOrder = 4
        OnClick = RemoveFolderButtonClick
      end
      object ResetToDefaultButton: TButton
        Left = 250
        Top = 260
        Width = 105
        Height = 23
        Caption = 'Reset to default'
        TabOrder = 5
        OnClick = ResetToDefaultButtonClick
      end
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Title = 'Select folder'
    Left = 40
    Top = 157
  end
  object ImageList1: TImageList
    Left = 40
    Top = 213
    Bitmap = {
      494C010101000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005F5F5F002F2F2F0000000000000000002F2F2F005F5F5F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F000F0F0F007D7D7D00BEBEBE000000000000000000BDBDBD007C7C7C000F0F
      0F006F6F6F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002F2F2F006060
      6000D6D6D600000000000000000000000000000000000000000000000000D6D6
      D600606060002F2F2F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006F6F6F0060606000E7E7
      E700000000000000000000000000000000000000000000000000000000000000
      0000000000005F5F5F006F6F6F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000020202000D5D5D5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000020202000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000606060008A8A8A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008B8B8B00606060000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000202020007F7F7F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BBBBBB00202020000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E7E7E700E7E7E70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009696960000000000000000000000
      00000000000000000000E6E6E600E7E7E700E7E7E70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BCBCBC00202020000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008A8A8A00606060000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010101000000000000000
      0000000000002020200000000000000000000000000000000000000000000000
      000000000000D5D5D50020202000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000606060000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E5E5E5005F5F5F006F6F6F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002F2F2F006060
      600000000000000000000000000000000000000000000000000000000000D7D7
      D7005F5F5F002F2F2F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006F6F
      6F00202020008B8B8B00BCBCBC000000000000000000BDBDBD008A8A8A002020
      20006F6F6F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000020202000000000000000
      0000000000005F5F5F001F1F1F000000000000000000202020005F5F5F00BCBC
      BC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F81F000000000000E187000000000000
      C7E30000000000008FF90000000000009FFD0000000000003FFC000000000000
      3C3C000000000000783E000000000000783E000000000000F83C000000000000
      FFFC00000000000083F90000000000009FF10000000000008FE3000000000000
      A187000000000000B80F00000000000000000000000000000000000000000000
      000000000000}
  end
end
