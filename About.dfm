object AboutBox: TAboutBox
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 213
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    object AboutIcon: TImage
      Left = 8
      Top = 8
      Width = 64
      Height = 64
      Picture.Data = {
        0954506E67496D61676589504E470D0A1A0A0000000D494844520000002C0000
        002C08060000001E845A01000000017352474200AECE1CE90000000467414D41
        0000B18F0BFC6105000000097048597300000EC200000EC20115284A80000001
        8769545874584D4C3A636F6D2E61646F62652E786D7000000000003C3F787061
        636B657420626567696E3D27EFBBBF272069643D2757354D304D704365686948
        7A7265537A4E54637A6B633964273F3E0D0A3C783A786D706D65746120786D6C
        6E733A783D2261646F62653A6E733A6D6574612F223E3C7264663A5244462078
        6D6C6E733A7264663D22687474703A2F2F7777772E77332E6F72672F31393939
        2F30322F32322D7264662D73796E7461782D6E7323223E3C7264663A44657363
        72697074696F6E207264663A61626F75743D22757569643A6661663562646435
        2D626133642D313164612D616433312D6433336437353138326631622220786D
        6C6E733A746966663D22687474703A2F2F6E732E61646F62652E636F6D2F7469
        66662F312E302F223E3C746966663A4F7269656E746174696F6E3E313C2F7469
        66663A4F7269656E746174696F6E3E3C2F7264663A4465736372697074696F6E
        3E3C2F7264663A5244463E3C2F783A786D706D6574613E0D0A3C3F787061636B
        657420656E643D2777273F3E2C94980B000000FF4944415478DAEDD9310E8230
        1406E076A0EE24DEC01B30E184CC6E1CC0137004278FC0093C001780854BE8E4
        496480B09080886D7DD047F2FF03014ACA474348E9934223D5AB6A74AEA34874
        88E45CFBD7C6F259369EF2D6720E52BF6BD1DD7B0AFF7162CDD1D4C9183D38E0
        869D42F73B5CB163B4B4C566B74CE4F7FC2F447249447A4D8DD0002F0EB67D77
        5D80BB9082C35328E273ACDD87BFF745700CCCC0C5A368D44E91806D46CC34A4
        230C30C000030C30C0466093D83E1CC0002F0936995EDA4C2DC9C19BFB4A000C
        30C0003300E3371FE011B8DB502D062E0D265FBD640D76911EBC05F4A064C01D
        3D5994E18A9E2D7B71436B15165DE37F956E5B26D267308B469B040000000049
        454E44AE426082}
      Stretch = True
      IsControl = True
    end
    object ProductName: TLabel
      Left = 88
      Top = 16
      Width = 36
      Height = 13
      Caption = 'FinderX'
      IsControl = True
    end
    object Version: TLabel
      Left = 88
      Top = 40
      Width = 24
      Height = 13
      Caption = '1.1.0'
      IsControl = True
    end
    object Copyright: TLabel
      Left = 8
      Top = 104
      Width = 230
      Height = 39
      Caption = 'Copyright '#13#10#13#10'Andrey Romanchenko     lasersquad@gmail.com'
      IsControl = True
    end
    object Comments: TLabel
      Left = 88
      Top = 59
      Width = 169
      Height = 13
      Caption = 'Possibly the best file find application'
      WordWrap = True
      IsControl = True
    end
  end
  object OKButton: TButton
    Left = 111
    Top = 180
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
