object IconFontsImageListEditor: TIconFontsImageListEditor
  Left = 352
  Top = 227
  Width = 800
  Height = 706
  HelpContext = 26140
  Caption = 'Icon Fonts ImageList Editor %s - Copyright Ethea S.r.l.'
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BottomSplitter: TSplitter
    Left = 0
    Top = 394
    Width = 784
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    MinSize = 220
  end
  object paImages: TPanel
    Left = 0
    Top = 57
    Width = 784
    Height = 337
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object CategorySplitter: TSplitter
      Left = 185
      Top = 0
      Width = 4
      Height = 337
      AutoSnap = False
      MinSize = 185
    end
    object Panel1: TPanel
      Left = 641
      Top = 0
      Width = 143
      Height = 337
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        143
        337)
      object SetCategoriesButton: TButton
        Left = 4
        Top = 121
        Width = 135
        Height = 25
        Caption = '&Set Categories...'
        TabOrder = 4
        OnClick = SetCategoriesButtonClick
      end
      object AddButton: TButton
        Left = 4
        Top = 5
        Width = 135
        Height = 25
        Caption = '&New'
        TabOrder = 0
        OnClick = AddButtonClick
      end
      object DeleteButton: TButton
        Left = 4
        Top = 34
        Width = 135
        Height = 25
        Caption = '&Delete'
        Enabled = False
        TabOrder = 1
        OnClick = DeleteButtonClick
      end
      object ClearAllButton: TButton
        Left = 4
        Top = 63
        Width = 135
        Height = 25
        Caption = '&Clear all'
        Enabled = False
        TabOrder = 2
        OnClick = ClearAllButtonClick
      end
      object ExportButton: TButton
        Left = 4
        Top = 92
        Width = 135
        Height = 25
        Caption = '&Export...'
        Enabled = False
        TabOrder = 3
        OnClick = ExportButtonClick
      end
      object WinCharMapButton: TButton
        Left = 4
        Top = 277
        Width = 134
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'System Char Map...'
        TabOrder = 5
        WordWrap = True
        OnClick = WinCharMapButtonClick
      end
      object ShowCharMapButton: TButton
        Left = 4
        Top = 306
        Width = 134
        Height = 26
        Anchors = [akRight, akBottom]
        Caption = 'Custom Char Map...'
        TabOrder = 6
        WordWrap = True
        OnClick = ShowCharMapButtonClick
      end
    end
    object ImagesPanel: TPanel
      Left = 0
      Top = 0
      Width = 185
      Height = 337
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object CategoryGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 185
        Height = 170
        Align = alClient
        Caption = 'Images/Categories'
        TabOrder = 0
        object CategoryListBox: TListBox
          Left = 2
          Top = 15
          Width = 181
          Height = 153
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          OnClick = CategoryListBoxClick
        end
      end
      object PropertiesGroupBox: TGroupBox
        Left = 0
        Top = 170
        Width = 185
        Height = 167
        Align = alBottom
        Caption = 'Global properties'
        TabOrder = 1
        DesignSize = (
          185
          167)
        object DefaultFontNameLabel: TLabel
          Left = 7
          Top = 15
          Width = 169
          Height = 13
          AutoSize = False
          Caption = 'FontName (default)'
          Transparent = True
        end
        object DefaultFontColorLabel: TLabel
          Left = 7
          Top = 55
          Width = 100
          Height = 13
          AutoSize = False
          Caption = 'FontColor (default)'
          Transparent = True
        end
        object DefaultMaskColorLabel: TLabel
          Left = 7
          Top = 93
          Width = 109
          Height = 13
          AutoSize = False
          Caption = 'MaskColor (default)'
          Transparent = True
        end
        object DefaultFontName: TComboBox
          Left = 7
          Top = 30
          Width = 169
          Height = 22
          Style = csOwnerDrawFixed
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 30
          ItemHeight = 16
          TabOrder = 0
          OnSelect = DefaultFontNameSelect
        end
        object DefaultFontColorColorBox: TColorBox
          Left = 7
          Top = 70
          Width = 169
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clDefault
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault]
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 16
          TabOrder = 1
          OnChange = DefaultFontColorColorBoxChange
        end
        object DefaultMaskColorColorBox: TColorBox
          Left = 7
          Top = 108
          Width = 169
          Height = 22
          DefaultColorColor = clNone
          NoneColorColor = clNone
          Selected = clDefault
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault]
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 16
          TabOrder = 2
          OnChange = DefaultMaskColorColorBoxChange
        end
        object StoreBitmapCheckBox: TCheckBox
          Left = 7
          Top = 139
          Width = 85
          Height = 17
          Caption = 'StoreBitmap'
          TabOrder = 3
          OnClick = StoreBitmapCheckBoxClick
        end
      end
    end
    object ImageListGroup: TGroupBox
      Left = 189
      Top = 0
      Width = 452
      Height = 337
      Align = alClient
      Caption = '%d Icons of Imagelist'
      TabOrder = 2
      object ImageView: TListView
        Left = 2
        Top = 15
        Width = 448
        Height = 320
        Align = alClient
        Columns = <>
        DragMode = dmAutomatic
        FullDrag = True
        HideSelection = False
        IconOptions.AutoArrange = True
        MultiSelect = True
        ReadOnly = True
        TabOrder = 0
        OnDragDrop = ImageViewDragDrop
        OnDragOver = ImageViewDragOver
        OnKeyDown = ImageViewKeyDown
        OnSelectItem = ImageViewSelectItem
      end
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 629
    Width = 784
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      784
      38)
    object OKButton: TButton
      Left = 427
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ApplyButton: TButton
      Left = 603
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = ApplyButtonClick
    end
    object CancelButton: TButton
      Left = 514
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 692
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 3
      OnClick = HelpButtonClick
    end
  end
  object ImageListGroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 784
    Height = 57
    Align = alTop
    Caption = 'Properties of ImageList'
    TabOrder = 0
    object SizeLabel: TLabel
      Left = 8
      Top = 15
      Width = 80
      Height = 13
      AutoSize = False
      Caption = 'Size (in pixel)'
      Transparent = True
    end
    object WidthLabel: TLabel
      Left = 94
      Top = 15
      Width = 80
      Height = 13
      AutoSize = False
      Caption = 'Width (in pixel)'
      Transparent = True
    end
    object HeightLabel: TLabel
      Left = 181
      Top = 15
      Width = 80
      Height = 13
      AutoSize = False
      Caption = 'Height (in pixel)'
      Transparent = True
    end
    object SizeSpinEdit: TSpinEdit
      Left = 8
      Top = 30
      Width = 81
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnChange = SizeSpinEditChange
    end
    object WidthSpinEdit: TSpinEdit
      Left = 95
      Top = 30
      Width = 81
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = WidthSpinEditChange
    end
    object HeightSpinEdit: TSpinEdit
      Left = 182
      Top = 30
      Width = 81
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = HeightSpinEditChange
    end
  end
  object ItemGroupBox: TGroupBox
    Left = 0
    Top = 398
    Width = 784
    Height = 163
    Align = alBottom
    Caption = 'Properties of Selected Icon n.%d'
    TabOrder = 3
    object LeftIconPanel: TPanel
      Left = 18
      Top = 15
      Width = 146
      Height = 146
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object IconPanel: TPanel
        Left = 0
        Top = 0
        Width = 146
        Height = 146
        Align = alClient
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWindow
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        object IconImage: TIconFontImage
          Left = 0
          Top = 0
          Width = 144
          Height = 144
          Align = alClient
        end
      end
    end
    object IconClientPanel: TPanel
      Left = 164
      Top = 15
      Width = 618
      Height = 146
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object FontNameLabel: TLabel
        Left = 25
        Top = 6
        Width = 87
        Height = 13
        AutoSize = False
        Caption = 'FontName'
        Transparent = True
      end
      object FontIconHexLabel: TLabel
        Left = 210
        Top = 6
        Width = 81
        Height = 13
        AutoSize = False
        Caption = 'FontIconHex'
        Transparent = True
      end
      object FontIconDecLabel: TLabel
        Left = 304
        Top = 6
        Width = 83
        Height = 13
        AutoSize = False
        Caption = 'FontIconDec'
        Transparent = True
      end
      object FontColorLabel: TLabel
        Left = 24
        Top = 46
        Width = 100
        Height = 13
        AutoSize = False
        Caption = 'FontColor'
        Transparent = True
      end
      object MaskColorLabel: TLabel
        Left = 210
        Top = 46
        Width = 100
        Height = 13
        AutoSize = False
        Caption = 'MaskColor'
        Transparent = True
      end
      object NameLabel: TLabel
        Left = 25
        Top = 88
        Width = 87
        Height = 13
        AutoSize = False
        Caption = 'Name'
        Transparent = True
      end
      object CategoryLabel: TLabel
        Left = 210
        Top = 88
        Width = 87
        Height = 13
        AutoSize = False
        Caption = 'Category'
        Transparent = True
      end
      object FontName: TComboBox
        Left = 25
        Top = 21
        Width = 180
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        OnChange = FontNameChange
      end
      object FontIconHex: TEdit
        Left = 210
        Top = 21
        Width = 86
        Height = 21
        CharCase = ecUpperCase
        MaxLength = 5
        TabOrder = 1
        OnExit = FontIconHexExit
      end
      object FontIconDec: TSpinEdit
        Left = 304
        Top = 21
        Width = 86
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = FontIconDecChange
      end
      object FontColor: TColorBox
        Left = 25
        Top = 61
        Width = 180
        Height = 22
        DefaultColorColor = clNone
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault]
        ItemHeight = 16
        TabOrder = 3
        OnChange = FontColorChange
      end
      object MaskColor: TColorBox
        Left = 210
        Top = 61
        Width = 180
        Height = 22
        DefaultColorColor = clNone
        NoneColorColor = clNone
        Selected = clDefault
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault]
        ItemHeight = 16
        TabOrder = 4
        OnChange = MaskColorChange
      end
      object NameEdit: TEdit
        Left = 25
        Top = 103
        Width = 180
        Height = 21
        TabOrder = 5
        OnExit = NameEditExit
      end
      object CategoryEdit: TEdit
        Left = 210
        Top = 103
        Width = 180
        Height = 21
        TabOrder = 6
        OnExit = CategoryEditExit
      end
    end
    object IconLeftMarginPanel: TPanel
      Left = 2
      Top = 15
      Width = 16
      Height = 146
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object IconBuilderGroupBox: TGroupBox
    Left = 0
    Top = 561
    Width = 784
    Height = 68
    Align = alBottom
    Caption = 'Icons Builder'
    TabOrder = 4
    DesignSize = (
      784
      68)
    object FromHexNumLabel: TLabel
      Left = 349
      Top = 17
      Width = 68
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'From Hex N.'
      Transparent = True
    end
    object ToHexNumLabel: TLabel
      Left = 503
      Top = 17
      Width = 55
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
      Top = 41
      Width = 628
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = EditChangeUpdateGUI
    end
    object BuildButton: TButton
      Left = 645
      Top = 39
      Width = 135
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Build from &Chars'
      Enabled = False
      TabOrder = 4
      OnClick = BuildButtonClick
    end
    object BuildFromHexButton: TButton
      Left = 645
      Top = 9
      Width = 135
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Build by &Hex N.'
      Enabled = False
      TabOrder = 2
      OnClick = BuildFromHexButtonClick
    end
    object FromHexNum: TEdit
      Left = 423
      Top = 12
      Width = 74
      Height = 21
      Anchors = [akTop, akRight]
      CharCase = ecUpperCase
      MaxLength = 5
      TabOrder = 0
      OnChange = EditChangeUpdateGUI
    end
    object ToHexNum: TEdit
      Left = 562
      Top = 12
      Width = 74
      Height = 21
      Anchors = [akTop, akRight]
      CharCase = ecUpperCase
      MaxLength = 5
      TabOrder = 1
      OnChange = EditChangeUpdateGUI
    end
  end
end
