object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'FileFind - find files quick!'
  ClientHeight = 496
  ClientWidth = 1117
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000040000C20E0000C20E00000000000000000000927A
    02FF917A02FF917902FF917802FF917902FF917802FF907802FF917902FF9179
    02FF917902FF917902FF917901FF917902FF917902FF927A02FF927A02FF8B71
    00FF897104FF7B6A20FF575D62FF4A4856FF58380DFF644306FF6A4A06FF6B49
    05FF765606FF886B04FF886C03FF896E03FF8B7002FF8B7201FF8B7101FF8568
    03FF846805FF6A714EFF4B627AFF153CD0FF0A28D8FF221358FF482105FF461B
    07FF643B13FF886B0FFF866803FF866803FF866903FF866903FF866902FF7E5F
    02FF746119FF646157FF716847FF2F53A1FF0142FBFF0729E3FF250837FF7048
    13FFB0AA50FF82670DFF7D5F03FF7F6002FF7E6003FF7E6002FF7E6002FF7657
    02FF755602FF6E6025FF476583FF3B5579FF0C4FD8FF0242F8FF1B167EFF552E
    0AFF795C0FFF755602FF795A08FF70500BFF755605FF765803FF775703FF7051
    02FF6D520BFF61592EFF58644DFF5A665BFF135ED1FF0156FCFF171F99FF2F01
    03FF3D1107FF673E0DFF6C3E11FF693B10FF70490FFF705103FF705003FF6A49
    03FF694907FF6B5F35FF482616FF393754FF1F5BAEFF0562F1FF7E571BFF9474
    24FFA5913DFF947D37FFA69743FFCBC04BFF978639FF6B4A03FF6B4903FF6542
    03FF654103FF665023FF9C974DFF886520FF553534FF0768E7FF94A379FF5E48
    2FFF552608FF72470FFFAC9B3CFFBAC97AFF6C4C10FF674303FF664203FF623D
    03FF623D03FF545744FF3F7A8FFF6C6138FFBCB04BFF0F75D5FF0D5ED5FF310F
    14FF644219FF755018FF81764AFF4F3636FF623D03FF623D04FF613C03FF5E38
    01FF5E3801FF5E4A22FF556557FF687165FF507980FF019BFAFF058FF7FF6960
    4CFF9E893DFF4C576AFF1444B1FF1E3FB2FF5F3801FF5E3702FF5D3702FF5A33
    02FF5B3302FF5A4222FF436B73FF486679FF658280FF08B1FAFF089CF3FF766F
    49FFABA449FF4D82A7FF0B91F9FF2F82C9FF5B3202FF5B3303FF5B3202FF5930
    04FF5A3004FF613C0FFF525748FF4C6571FF486088FF19C7FBFF07ABF7FF412C
    5AFF8A8A6AFF41626EFF3D768AFF5D4328FF5A3005FF593004FF593004FF572E
    02FF572E03FF562F04FF5A4324FF62867FFF75B4B7FF28CCFAFF02BFFBFF177E
    ADFF205272FF263A53FF251F53FF1833AEFF582D02FF562E03FF572D03FF532A
    02FF532A02FF542A02FF532A02FF542902FF593715FF7FA89CFF6DE3F1FF3EDA
    FBFF19CAFAFF05BDFBFF0FB1FAFF1E8BEEFF552A02FF532A02FF532A02FF5127
    02FF522803FF522803FF512703FF522803FF522803FF512702FF552E0DFF6B65
    4EFF7A9789FF7AB8B6FF5CC0D1FF637167FF522702FF522803FF522703FF5126
    04FF512704FF512604FF522704FF512704FF512704FF512704FF512704FF5126
    04FF522603FF512603FF502303FF512604FF512704FF512704FF512604FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object ProgressLabel: TLabel
    Left = 832
    Top = 480
    Width = 73
    Height = 15
    Caption = 'ProgressLabel'
    Visible = False
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 472
    Width = 1117
    Height = 24
    Panels = <
      item
        Text = 'Found items : 0'
        Width = 120
      end
      item
        Text = 'Search time: 0'
        Width = 170
      end
      item
        Text = 'FS data load time: 0'
        Width = 200
      end
      item
        Text = 'Items loaded: 0'
        Width = 170
      end
      item
        Text = 'Folder size: 0'
        Width = 150
      end>
  end
  object ListView1: TListView
    Left = 0
    Top = 41
    Width = 1117
    Height = 431
    Align = alClient
    Columns = <>
    FullDrag = True
    OwnerData = True
    RowSelect = True
    PopupMenu = PopupMenu1
    SortType = stBoth
    TabOrder = 1
    ViewStyle = vsReport
    OnColumnClick = ListView1ColumnClick
    OnData = ListView1Data
    OnDblClick = ListView1DblClick
  end
  object SearchPanel: TPanel
    Left = 0
    Top = 0
    Width = 1117
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      1117
      41)
    object AdvancedSearchButton: TSpeedButton
      Left = 991
      Top = 8
      Width = 121
      Height = 27
      AllowAllUp = True
      Anchors = [akTop, akRight]
      GroupIndex = 1
      Caption = 'Advanced Search'
      ImageIndex = 0
      Images = ImageList1
      Flat = True
      Layout = blGlyphRight
      OnClick = AdvancedSearchButtonClick
    end
    object LabelAnd: TLabel
      Left = 270
      Top = 78
      Width = 20
      Height = 15
      Caption = 'and'
    end
    object SearchEdit: TLabeledEdit
      Left = 80
      Top = 10
      Width = 389
      Height = 23
      Hint = 'Enter your search here'
      EditLabel.Width = 64
      EditLabel.Height = 23
      EditLabel.Caption = 'Search here '
      LabelPosition = lpLeft
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = ''
      OnChange = SearchEditChange
      OnKeyPress = SearchEditKeyPress
    end
    object SearchByFileSize: TCheckBox
      Left = 11
      Top = 45
      Width = 66
      Height = 17
      Caption = 'File Size'
      TabOrder = 1
      OnClick = SearchByFileSizeClick
    end
    object FileSizeOp: TComboBox
      Left = 114
      Top = 42
      Width = 49
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = '='
      Items.Strings = (
        '='
        '>'
        '<')
    end
    object SearchFileSize: TNumberBox
      Left = 169
      Top = 42
      Width = 82
      Height = 23
      TabOrder = 3
    end
    object FileSizeFactor: TComboBox
      Left = 257
      Top = 42
      Width = 83
      Height = 23
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 4
      Text = 'Kilobytes'
      Items.Strings = (
        'Bytes'
        'Kilobytes'
        'Megabytes'
        'Gigabytes')
    end
    object SearchByModifiedDate: TCheckBox
      Left = 11
      Top = 78
      Width = 97
      Height = 17
      Caption = 'Date between'
      TabOrder = 5
      OnClick = SearchByModifiedDateClick
    end
    object DateTimePickerFrom: TDateTimePicker
      Left = 114
      Top = 74
      Width = 150
      Height = 23
      Date = 45491.000000000000000000
      Time = 45491.000000000000000000
      Checked = False
      Kind = dtkDateTime
      TabOrder = 6
    end
    object DateTimePickerTo: TDateTimePicker
      Left = 296
      Top = 74
      Width = 150
      Height = 23
      Date = 45491.000000000000000000
      Time = 45491.000000000000000000
      Kind = dtkDateTime
      TabOrder = 7
    end
    object SearchByAttributes: TCheckBox
      Left = 11
      Top = 110
      Width = 78
      Height = 17
      Caption = 'Attributes'
      TabOrder = 8
      OnClick = SearchByAttributesClick
    end
    object AttrArchive: TCheckBox
      Left = 624
      Top = 110
      Width = 65
      Height = 17
      Caption = 'Archive'
      TabOrder = 9
    end
    object AttrHidden: TCheckBox
      Left = 196
      Top = 110
      Width = 74
      Height = 17
      Caption = 'Hidden'
      TabOrder = 10
    end
    object AttrDirectory: TCheckBox
      Left = 114
      Top = 110
      Width = 81
      Height = 17
      Caption = 'Directory'
      TabOrder = 11
    end
    object AttrEncrypted: TCheckBox
      Left = 533
      Top = 110
      Width = 85
      Height = 17
      Caption = 'Encrypted'
      TabOrder = 12
    end
    object AttrCompressed: TCheckBox
      Left = 431
      Top = 110
      Width = 85
      Height = 17
      Caption = 'Compressed'
      TabOrder = 13
    end
    object AttrReadonly: TCheckBox
      Left = 347
      Top = 110
      Width = 80
      Height = 17
      Caption = 'Readonly'
      TabOrder = 14
    end
    object AttrSystem: TCheckBox
      Left = 273
      Top = 110
      Width = 59
      Height = 17
      Caption = 'System'
      TabOrder = 15
    end
    object StartSearchBtn: TBitBtn
      Left = 475
      Top = 8
      Width = 75
      Height = 28
      Caption = 'Search'
      ImageIndex = 2
      Images = ImageList1
      TabOrder = 16
      OnClick = StartSearchBtnClick
    end
    object IndexingBitBtn: TBitBtn
      Left = 568
      Top = 8
      Width = 97
      Height = 28
      Caption = 'Refresh Index...'
      TabOrder = 17
      OnClick = IndexingBitBtnClick
    end
  end
  object ProgressBar1: TProgressBar
    Left = 948
    Top = 477
    Width = 150
    Height = 17
    TabOrder = 3
    Visible = False
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 864
    Top = 336
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Title = 'Select folder'
    Left = 752
    Top = 392
  end
  object MainMenu1: TMainMenu
    Left = 160
    Top = 184
    object File1: TMenuItem
      Caption = 'File'
      object Statistics1: TMenuItem
        Caption = 'Statistics'
        OnClick = Statistics1Click
      end
      object File2: TMenuItem
        Caption = 'Exit'
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object Settings1: TMenuItem
        Caption = 'Settings...'
        OnClick = Settings1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
    end
  end
  object ImageList1: TImageList
    BlendColor = clWindow
    BkColor = clWhite
    Left = 360
    Top = 168
    Bitmap = {
      494C01010A001800040010001000FFFFFF00FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000096FF0000ACFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF303030FF969696FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BE7363FFBE73
      63FFBE7363FFBE7363FFBE7363FFBE7363FFBE7363FFBE7363FFBE7363FFBE73
      63FFBE7363FF000096FF0404ADFF0000CCFFE8E8E8FFE9E9E9FFE8E8E8FFE8E8
      E8FFBDBDBDFF101010FF8A8A8AFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE9E9E9FFE8E8E8FFE8E8E8FFE7E7E7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BE7363FFFFF3
      EDFFFFF3EDFFFFF3EDFFFFEEE7FFFFEEE7FFFFEAE3FFFFEAE3FFFFE6DFFFFFE6
      DFFF00009AFF0404AEFF0000CCFF00000000E8E8E8FFE7E7E7FFE8E8E8FFE8E8
      E8FFE8E8E8FFD5D5D5FF303030FF5F5F5FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BE7363FFFFF6
      F0FFFFF6F0FFFFF2ECFFFFF2ECFFFFEEE8FFFFEEE8FFFFEEE8FFFFE9E2FF0101
      A9FF0202B3FF0000CCFF0000000000000000E8E8E8FFE9E9E9FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFD9D9D9FF606060FF2F2F2FFFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE7E7E7FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C07767FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF1EAFFFFF1EAFFD8D3D5FF2244C8FF0505
      C0FF0000CCFFA9A9EDFF0000000000000000E8E8E8FFE7E7E7FFE8E8E8FFE8E8
      E8FFE9E9E9FFE8E8E8FFE8E8E8FFE6E6E6FF7E7E7EFF202020FFBEBEBEFFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C37C6CFFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF1EBFF5E99C5FF48A3E7FF222E
      CEFFA9A9EDFF3131D6FF0000000000000000E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE6E6E6FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF979797FF101010FFA4A4
      A4FFE8E8E8FFE7E7E7FFE9E9E9FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C68272FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFAEC2D3FF4AA0E1FF4AAEF9FF84C3
      F3FF1F1FD2FFBE7363FF0000000000000000E8E8E8FFE8E8E8FFE9E9E9FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFBDBDBDFF1010
      10FF979797FFE8E8E8FFE5E5E5FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C98778FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFEAE7E5FF000097FF77B9EBFFD3E1EFFFFCF1
      EBFFFFEFE8FFBE7363FF0000000000000000E8E8E8FFE8E8E8FFE9E9E9FFE8E8
      E8FFE8E8E8FFE7E7E7FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFC8C8
      C8FF303030FF5F5F5FFFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CC8D7DFFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FF000097FFCADFEFFFFFF6F0FFFFF6F0FFFFF6
      F0FFFFF1EBFFBE7363FF0000000000000000E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFD5D5D5FF303030FFA4A4A4FFE6E6E6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CF9283FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6
      F0FFFFF6F0FFBE7363FF0000000000000000E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFA4A4A4FF303030FFE5E5E5FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D29888FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFEEC2B6FFECBDB1FFECBD
      B1FFEDC1B5FFC07362FF0000000000000000E8E8E8FFE8E8E8FF101010FF0000
      00FF000000FF000000FF000000FF202020FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF010101FFE7E7E7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D59D8DFFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFF9E3DBFFDB8977FFE7A698FFE7A2
      93FFE9AFA1FFCC8D7EFF0000000000000000E8E8E8FFE8E8E8FF010101FF0F0F
      0FFF969696FFEAEAEAFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF010101FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D8A393FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFF6DCD3FFDE9483FFFFEFE9FFFFEF
      E9FFCC8D7EFF000000000000000000000000E8E8E8FFE8E8E8FF010101FFBEBE
      BEFF303030FF6F6F6FFFE6E6E6FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFA4A4A4FF303030FFE6E6E6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DBA899FFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFF7DDD4FFE09786FFFFF6F0FFCC8D
      7EFF00000000000000000000000000000000E8E8E8FFE8E8E8FF010101FFE7E7
      E7FFD7D7D7FF414141FF4F4F4FFFD5D5D5FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFD5D5D5FF303030FFA3A3A3FFE7E7E7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEAE9EFFFFF6
      F0FFFFF6F0FFFFF6F0FFFFF6F0FFFFF6F0FFF7DDD4FFDF9786FFFFF6F0FFCC8D
      7EFF00000000000000000000000000000000E8E8E8FFE8E8E8FF010101FFE6E6
      E6FFE6E6E6FFEAEAEAFF606060FF2F2F2FFFA4A4A4FFE8E8E8FFE8E8E8FFA4A4
      A4FF313131FF606060FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E1B3A4FFDEAE
      9FFFDCAA9AFFD9A596FFD7A091FFD49C8CFFD19685FFD29282FFCC8D7EFF0000
      000000000000000000000000000000000000E8E8E8FFE8E8E8FF212121FFE5E5
      E5FFE8E8E8FFE7E7E7FFE5E5E5FFA4A4A4FF414141FF000000FF000000FF2F2F
      2FFFA4A4A4FFE8E8E8FFE8E8E8FFE8E8E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FF000000FF000000FF000000FFE8E8E8FF000000FF000000FF0000
      00FF000000FF0F0F0FFF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF0000
      00FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF0F0F0FFF000000FFE8E8E8FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFE8E8E8FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FF000000FF000000FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FF0F0F0FFFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FFE8E8
      E8FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FF0000
      00FFE8E8E8FF000000FF000000FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FF000000FF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF202020FF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF000000FFE8E8
      E8FF000000FF000000FF000000FF202020FFE8E8E8FF000000FF000000FF0000
      00FF000000FFE8E8E8FF000000FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF202020FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FF000000FF000000FF0000
      00FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0F0F0FFF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FF000000FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF202020FF000000FF000000FF000000FFE8E8
      E8FF000000FF000000FF000000FF000000FFE8E8E8FF000000FF000000FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF000000FFE8E8
      E8FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0000
      00FFE8E8E8FF101010FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF101010FFE8E8
      E8FF000000FF202020FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0000
      00FFE8E8E8FF0F0F0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8
      E8FF000000FF202020FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF0000
      00FF000000FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0000
      00FF000000FF000000FF000000FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0F0F0FFF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FF000000FF000000FF000000FF000000FF000000FF000000FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8E8FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FF202020FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FF202020FFE8E8E8FF000000FF000000FF000000FF0000
      00FF000000FF000000FF0F0F0FFF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FF202020FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0000
      00FF000000FF000000FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FF202020FF000000FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FF2020
      20FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8
      E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF101010FFE8E8E8FFE8E8
      E8FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FF202020FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FF303030FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F
      0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0F0F0FFF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF2020
      20FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF0F0F0FFF000000FF000000FF000000FF000000FF000000FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FF000000FF000000FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF000000FF000000FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FF202020FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FF000000FF202020FF000000FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FF000000FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF101010FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF0000
      00FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF101010FF000000FF000000FFE8E8E8FFE8E8E8FF000000FF0000
      00FF000000FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FF202020FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FF202020FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FF000000FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FF0F0F0FFFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F
      0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F
      0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00E8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000FFFFFF00FFFFFF0000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FF0F0F0FFFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F
      0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F0FFFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF0F0F
      0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FF202020FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF202020FF0000
      00FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF202020FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FF2020
      20FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF2020
      20FF000000FFE8E8E8FFE8E8E8FFE8E8E8FF000000FF000000FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF0F0F0FFF000000FF0F0F0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FF0F0F0FFF000000FF0F0F0FFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8
      E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FF424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFF00000000FFFFFFFF00000000
      FE7FEFF700000000FC3FE7E700000000F81FE3C700000000F18FF18F00000000
      E3C7F81F00000000E667EC3700000000EC37E66700000000F81FE3C700000000
      F18FF18F00000000E3C7F81F00000000E7E7FC3F00000000EFF7FE7F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object PopupMenu1: TPopupMenu
    Left = 320
    Top = 104
    object Openfilefolder1: TMenuItem
      Caption = 'Open file folder...'
      OnClick = Openfilefolder1Click
    end
  end
end
