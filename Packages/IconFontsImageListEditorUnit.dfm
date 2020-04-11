object IconFontsImageListEditor: TIconFontsImageListEditor
  Left = 352
  Top = 227
  HelpContext = 26140
  Caption = 'Icon Fonts ImageList Editor %s - Copyright Ethea S.r.l.'
  ClientHeight = 580
  ClientWidth = 691
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 213
    Width = 691
    Height = 260
    Align = alClient
    Caption = ' Icons of Imagelist'
    TabOrder = 1
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 687
      Height = 243
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
    Width = 691
    Height = 78
    Align = alBottom
    Caption = 'Icons Builder'
    TabOrder = 3
    DesignSize = (
      691
      78)
    object FromHexNumLabel: TLabel
      Left = 303
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
      Left = 435
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
      Left = 8
      Top = 40
      Width = 572
      Height = 21
      Hint = 'Icon Name'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = EditChangeUpdateGUI
    end
    object BuildButton: TButton
      Left = 586
      Top = 40
      Width = 98
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Build from &Chars'
      Enabled = False
      TabOrder = 4
      OnClick = BuildButtonClick
    end
    object BuildFromHexButton: TButton
      Left = 586
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
      Left = 373
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
      Left = 505
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
    Width = 691
    Height = 213
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 608
      Height = 213
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object BuilderGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 608
        Height = 103
        Align = alTop
        Caption = 'Properties of ImageList'
        TabOrder = 0
        DesignSize = (
          608
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
          Width = 499
          Height = 22
          Style = csOwnerDrawFixed
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 30
          TabOrder = 1
          OnSelect = DefaultFontNameSelect
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
      end
      object ImageGroup: TGroupBox
        Left = 0
        Top = 103
        Width = 608
        Height = 104
        Align = alTop
        Caption = 'Properties of Selected Icon n.%d'
        TabOrder = 1
        DesignSize = (
          608
          104)
        object IconNameLabel: TLabel
          Left = 438
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
          Left = 438
          Top = 18
          Width = 74
          Height = 13
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 'FontIconHex'
          Transparent = True
        end
        object FontIconDecLabel: TLabel
          Left = 516
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
          Left = 438
          Top = 74
          Width = 158
          Height = 21
          Hint = 'Icon Name'
          TabOrder = 6
          OnExit = IconNameExit
        end
        object FontName: TComboBox
          Left = 95
          Top = 34
          Width = 337
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = FontNameChange
        end
        object FontIconDec: TSpinEdit
          Left = 516
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
          Left = 438
          Top = 34
          Width = 74
          Height = 21
          Hint = 'Hexadecimal value'
          Anchors = [akTop, akRight]
          CharCase = ecUpperCase
          MaxLength = 5
          TabOrder = 2
          OnExit = FontIconHexExit
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
      Left = 608
      Top = 0
      Width = 83
      Height = 213
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        83
        213)
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
      object HelpButton: TButton
        Left = 2
        Top = 60
        Width = 75
        Height = 25
        Caption = '&Help'
        TabOrder = 2
        OnClick = HelpButtonClick
      end
      object ShowCharMapButton: TButton
        Left = 2
        Top = 157
        Width = 75
        Height = 50
        Anchors = [akTop, akRight]
        Caption = 'Show Char Map...'
        TabOrder = 3
        WordWrap = True
        OnClick = ShowCharMapButtonClick
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 473
    Width = 691
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object AddButton: TButton
      Left = 6
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Add...'
      TabOrder = 0
      OnClick = AddButtonClick
    end
    object DeleteButton: TButton
      Left = 87
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 1
      OnClick = DeleteButtonClick
    end
    object ClearAllButton: TButton
      Left = 168
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Clear all'
      Enabled = False
      TabOrder = 2
      OnClick = ClearAllButtonClick
    end
    object ExportButton: TButton
      Left = 249
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Export...'
      Enabled = False
      TabOrder = 3
      OnClick = ExportButtonClick
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
