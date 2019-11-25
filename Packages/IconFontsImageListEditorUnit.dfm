object IconFontsImageListEditor: TIconFontsImageListEditor
  Left = 0
  Top = 0
  HelpContext = 26140
  Caption = 'Icon Fonts ImageList Editor - Copyright Ethea S.r.l.'
  ClientHeight = 580
  ClientWidth = 687
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    687
    580)
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 604
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 604
    Top = 37
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    OnClick = CancelButtonClick
  end
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 207
    Width = 687
    Height = 317
    Align = alClient
    Caption = ' Font &Icons'
    TabOrder = 1
    ExplicitHeight = 298
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 683
      Height = 300
      Align = alClient
      Columns = <>
      HideSelection = False
      IconOptions.AutoArrange = True
      ReadOnly = True
      TabOrder = 0
      OnSelectItem = ImageViewSelectItem
      ExplicitHeight = 281
    end
  end
  object ImageGroup: TGroupBox
    AlignWithMargins = True
    Left = 0
    Top = 103
    Width = 597
    Height = 104
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 90
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Properties of Selected &Icon n.%d'
    TabOrder = 0
    DesignSize = (
      597
      104)
    object IconNameLabel: TLabel
      Left = 427
      Top = 59
      Width = 87
      Height = 13
      AutoSize = False
      Caption = '&IconName'
      Transparent = False
    end
    object FontNameLabel: TLabel
      Left = 95
      Top = 18
      Width = 325
      Height = 13
      AutoSize = False
      Caption = 'Font&Name*'
      Transparent = False
    end
    object FontIconHexLabel: TLabel
      Left = 427
      Top = 18
      Width = 74
      Height = 13
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'FontIcon&Hex*'
      Transparent = False
    end
    object FontIconDecLabel: TLabel
      Left = 505
      Top = 18
      Width = 78
      Height = 13
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'FontIcon&Dec*'
      Transparent = False
    end
    object FontColorLabel: TLabel
      Left = 95
      Top = 59
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Font&Color*'
      Transparent = False
    end
    object MaskColorLabel: TLabel
      Left = 260
      Top = 59
      Width = 100
      Height = 13
      AutoSize = False
      Caption = '&MaskColor*'
      Transparent = False
    end
    object MainPanel: TPanel
      Left = 10
      Top = 18
      Width = 78
      Height = 78
      BevelOuter = bvNone
      BorderWidth = 5
      BorderStyle = bsSingle
      ParentColor = True
      TabOrder = 0
      object MainImage: TImage
        Left = 5
        Top = 5
        Width = 64
        Height = 64
        Align = alClient
        Stretch = True
        ExplicitWidth = 60
        ExplicitHeight = 60
      end
    end
    object IconName: TEdit
      Left = 426
      Top = 74
      Width = 160
      Height = 21
      Hint = 'Icon Name'
      TabOrder = 6
      OnExit = IconNameExit
    end
    object FontName: TComboBox
      Left = 95
      Top = 34
      Width = 326
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = FontNameChange
    end
    object FontIconDec: TSpinEdit
      Left = 505
      Top = 34
      Width = 80
      Height = 22
      Hint = 'Decimal value'
      Anchors = [akTop, akRight]
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = FontIconDecChange
      OnExit = FontIconDecExit
    end
    object FontIconHex: TEdit
      Left = 427
      Top = 34
      Width = 74
      Height = 21
      Hint = 'Hexadecimal value'
      Anchors = [akTop, akRight]
      TabOrder = 2
      OnExit = FontIconHexExit
    end
    object FontColor: TColorBox
      Left = 96
      Top = 74
      Width = 160
      Height = 22
      DefaultColorColor = clWindow
      NoneColorColor = clNone
      Selected = clNone
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 4
      OnChange = FontColorChange
    end
    object MaskColor: TColorBox
      Left = 260
      Top = 74
      Width = 160
      Height = 22
      DefaultColorColor = clWindow
      NoneColorColor = clNone
      Selected = clNone
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 5
      OnChange = MaskColorChange
    end
  end
  object AddButton: TButton
    Left = 604
    Top = 106
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add...'
    TabOrder = 5
    OnClick = AddButtonClick
  end
  object DeleteButton: TButton
    Left = 604
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    Enabled = False
    TabOrder = 6
    OnClick = DeleteButtonClick
  end
  object HelpButton: TButton
    Left = 604
    Top = 67
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 4
    OnClick = HelpButtonClick
  end
  object BuilderGroupBox: TGroupBox
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 597
    Height = 103
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 90
    Margins.Bottom = 0
    Align = alTop
    Caption = 'Properties of ImageList'
    TabOrder = 7
    DesignSize = (
      597
      103)
    object DefaultFontNameLabel: TLabel
      Left = 95
      Top = 15
      Width = 329
      Height = 13
      AutoSize = False
      Caption = 'Font&Name'
      Transparent = False
    end
    object DefaultFontColorLabel: TLabel
      Left = 96
      Top = 55
      Width = 100
      Height = 13
      AutoSize = False
      Caption = 'Font&Color*'
      Transparent = False
    end
    object DefaultMaskColorLabel: TLabel
      Left = 261
      Top = 55
      Width = 100
      Height = 13
      AutoSize = False
      Caption = '&MaskColor*'
      Transparent = False
    end
    object SizeLabel: TLabel
      Left = 8
      Top = 15
      Width = 58
      Height = 13
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '&Size'
      Transparent = False
    end
    object ShowCharMapButton: TButton
      Left = 489
      Top = 28
      Width = 99
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Show Char Map...'
      TabOrder = 2
      OnClick = ShowCharMapButtonClick
    end
    object DefaultFontName: TComboBox
      Left = 95
      Top = 30
      Width = 388
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = DefaultFontNameChange
    end
    object DefaultFontColorColorBox: TColorBox
      Left = 96
      Top = 70
      Width = 160
      Height = 22
      DefaultColorColor = clWindow
      NoneColorColor = clNone
      Selected = clNone
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 4
      OnChange = DefaultFontColorColorBoxChange
    end
    object DefaultMaskColorColorBox: TColorBox
      Left = 260
      Top = 70
      Width = 160
      Height = 22
      DefaultColorColor = clWindow
      NoneColorColor = clNone
      Selected = clNone
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
      TabOrder = 5
      OnChange = DefaultMaskColorColorBoxChange
    end
    object SizeSpinEdit: TSpinEdit
      Left = 8
      Top = 30
      Width = 81
      Height = 22
      Hint = 'Decimal value'
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SizeSpinEditChange
    end
    object StoreBitmapCheckBox: TCheckBox
      Left = 8
      Top = 68
      Width = 85
      Height = 17
      Caption = 'StoreBitmap'
      TabOrder = 3
      OnClick = StoreBitmapCheckBoxClick
    end
  end
  object ClearAllButton: TButton
    Left = 604
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear all'
    Enabled = False
    TabOrder = 8
    OnClick = ClearAllButtonClick
  end
  object IconBuilderGroupBox: TGroupBox
    Left = 0
    Top = 524
    Width = 687
    Height = 56
    Align = alBottom
    Caption = 'Icons Builder'
    TabOrder = 9
    DesignSize = (
      687
      56)
    object CharsEdit: TEdit
      Left = 8
      Top = 19
      Width = 574
      Height = 21
      Hint = 'Icon Name'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = CharsEditChange
    end
    object BuildButton: TButton
      Left = 586
      Top = 18
      Width = 98
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Build from chars'
      Enabled = False
      TabOrder = 1
      OnClick = BuildButtonClick
    end
  end
  object SaveDialog: TSavePictureDialog
    HelpContext = 27010
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Title = 'Export Images'
    Left = 80
    Top = 296
  end
end
