object IconFontsImageListEditor: TIconFontsImageListEditor
  Left = 352
  Top = 227
  HelpContext = 26140
  Caption = 'Icon Fonts ImageList Editor %s - Copyright Ethea S.r.l.'
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
  PixelsPerInch = 96
  TextHeight = 13
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 213
    Width = 687
    Height = 289
    Align = alClient
    Caption = ' Icons of Imagelist'
    TabOrder = 1
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 683
      Height = 272
      Align = alClient
      Columns = <>
      HideSelection = False
      IconOptions.AutoArrange = True
      ReadOnly = True
      TabOrder = 0
      OnKeyDown = ImageViewKeyDown
      OnSelectItem = ImageViewSelectItem
    end
  end
  object IconBuilderGroupBox: TGroupBox
    Left = 0
    Top = 502
    Width = 687
    Height = 78
    Align = alBottom
    Caption = 'Icons Builder'
    TabOrder = 2
    DesignSize = (
      687
      78)
    object FromHexNumLabel: TLabel
      Left = 299
      Top = 16
      Width = 68
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'From Hex N.'
      Transparent = True
    end
    object ToHexNumLabel: TLabel
      Left = 431
      Top = 16
      Width = 68
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'To Hex N.'
      Transparent = True
    end
    object CharsEditLabel: TLabel
      Left = 7
      Top = 26
      Width = 145
      Height = 13
      AutoSize = False
      Caption = 'Paste chars to build here'
      Transparent = True
    end
    object CharsEdit: TEdit
      Left = 7
      Top = 43
      Width = 568
      Height = 21
      Hint = 'Icon Name'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = EditChangeUpdateGUI
    end
    object BuildButton: TButton
      Left = 582
      Top = 43
      Width = 98
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Build from &Chars'
      Enabled = False
      TabOrder = 4
      OnClick = BuildButtonClick
    end
    object BuildFromHexButton: TButton
      Left = 582
      Top = 9
      Width = 98
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Build by &Hex N.'
      Enabled = False
      TabOrder = 2
      OnClick = BuildFromHexButtonClick
    end
    object FromHexNum: TEdit
      Left = 369
      Top = 13
      Width = 74
      Height = 21
      Anchors = [akTop, akRight]
      CharCase = ecUpperCase
      MaxLength = 5
      TabOrder = 0
      OnChange = EditChangeUpdateGUI
    end
    object ToHexNum: TEdit
      Left = 501
      Top = 13
      Width = 74
      Height = 21
      Anchors = [akTop, akRight]
      CharCase = ecUpperCase
      MaxLength = 5
      TabOrder = 1
      OnChange = EditChangeUpdateGUI
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 687
    Height = 213
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 604
      Height = 213
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object BuilderGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 604
        Height = 103
        Align = alTop
        Caption = 'Properties of ImageList'
        TabOrder = 0
        DesignSize = (
          604
          103)
        object DefaultFontNameLabel: TLabel
          Left = 95
          Top = 15
          Width = 161
          Height = 13
          AutoSize = False
          Caption = 'FontName (default)'
          Transparent = True
        end
        object DefaultFontColorLabel: TLabel
          Left = 96
          Top = 55
          Width = 100
          Height = 13
          AutoSize = False
          Caption = 'FontColor (default)'
          Transparent = True
        end
        object DefaultMaskColorLabel: TLabel
          Left = 261
          Top = 55
          Width = 100
          Height = 13
          AutoSize = False
          Caption = 'MaskColor (default)'
          Transparent = True
        end
        object SizeLabel: TLabel
          Left = 8
          Top = 15
          Width = 80
          Height = 13
          AutoSize = False
          Caption = 'Size (in pixel)'
          Transparent = True
        end
        object DefaultFontName: TComboBox
          Left = 95
          Top = 30
          Width = 395
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
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clScrollBar
          TabOrder = 3
          OnChange = DefaultFontColorColorBoxChange
        end
        object DefaultMaskColorColorBox: TColorBox
          Left = 260
          Top = 70
          Width = 160
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clScrollBar
          TabOrder = 4
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
          TabOrder = 2
          OnClick = StoreBitmapCheckBoxClick
        end
        object ShowCharMapButton: TButton
          Left = 499
          Top = 24
          Width = 99
          Height = 33
          Anchors = [akTop, akRight]
          Caption = 'Show Char Map...'
          TabOrder = 5
          OnClick = ShowCharMapButtonClick
        end
      end
      object ImageGroup: TGroupBox
        Left = 0
        Top = 103
        Width = 604
        Height = 104
        Align = alTop
        Caption = 'Properties of Selected Icon n.%d'
        TabOrder = 1
        DesignSize = (
          604
          104)
        object IconNameLabel: TLabel
          Left = 427
          Top = 59
          Width = 87
          Height = 13
          AutoSize = False
          Caption = 'IconName'
          Transparent = True
        end
        object FontNameLabel: TLabel
          Left = 95
          Top = 18
          Width = 325
          Height = 13
          AutoSize = False
          Caption = 'FontName'
          Transparent = True
        end
        object FontIconHexLabel: TLabel
          Left = 434
          Top = 18
          Width = 74
          Height = 13
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 'FontIconHex'
          Transparent = True
        end
        object FontIconDecLabel: TLabel
          Left = 512
          Top = 18
          Width = 78
          Height = 13
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 'FontIconDec'
          Transparent = True
        end
        object FontColorLabel: TLabel
          Left = 95
          Top = 59
          Width = 100
          Height = 13
          AutoSize = False
          Caption = 'FontColor'
          Transparent = True
        end
        object MaskColorLabel: TLabel
          Left = 260
          Top = 59
          Width = 100
          Height = 13
          AutoSize = False
          Caption = 'MaskColor'
          Transparent = True
        end
        object MainPanel: TPanel
          Left = 10
          Top = 18
          Width = 78
          Height = 78
          BevelOuter = bvNone
          BorderWidth = 2
          BorderStyle = bsSingle
          Color = clWindow
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
          object MainImage: TImage
            Left = 2
            Top = 2
            Width = 72
            Height = 72
            Align = alClient
            Stretch = True
            Transparent = True
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
          Width = 333
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = FontNameChange
        end
        object FontIconDec: TSpinEdit
          Left = 512
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
        end
        object FontIconHex: TEdit
          Left = 434
          Top = 34
          Width = 74
          Height = 21
          Hint = 'Hexadecimal value'
          Anchors = [akTop, akRight]
          CharCase = ecUpperCase
          MaxLength = 5
          TabOrder = 2
          OnChange = FontIconHexChange
        end
        object FontColor: TColorBox
          Left = 96
          Top = 74
          Width = 160
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clScrollBar
          TabOrder = 4
          OnChange = FontColorChange
        end
        object MaskColor: TColorBox
          Left = 260
          Top = 74
          Width = 160
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clScrollBar
          TabOrder = 5
          OnChange = MaskColorChange
        end
      end
    end
    object paButtons: TPanel
      Left = 604
      Top = 0
      Width = 83
      Height = 213
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object OKButton: TButton
        Left = 2
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object CancelButton: TButton
        Left = 2
        Top = 34
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object AddButton: TButton
        Left = 2
        Top = 90
        Width = 75
        Height = 25
        Caption = '&Add...'
        TabOrder = 3
        OnClick = AddButtonClick
      end
      object DeleteButton: TButton
        Left = 2
        Top = 116
        Width = 75
        Height = 25
        Caption = '&Delete'
        Enabled = False
        TabOrder = 4
        OnClick = DeleteButtonClick
      end
      object HelpButton: TButton
        Left = 2
        Top = 60
        Width = 75
        Height = 25
        Caption = '&Help'
        TabOrder = 2
        OnClick = HelpButtonClick
      end
      object ClearAllButton: TButton
        Left = 2
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Clear all'
        Enabled = False
        TabOrder = 5
        OnClick = ClearAllButtonClick
      end
      object ExportButton: TButton
        Left = 2
        Top = 178
        Width = 75
        Height = 25
        Caption = '&Export...'
        Enabled = False
        TabOrder = 6
        OnClick = ExportButtonClick
      end
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
