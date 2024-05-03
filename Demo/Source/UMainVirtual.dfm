object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'Icon Fonts ImageList Demo - Copyright (c) Ethea S.r.l. - Apache ' +
    '2.0 Open Source License'
  ClientHeight = 547
  ClientWidth = 709
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter: TSplitter
    Left = 626
    Top = 38
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 80
    ExplicitLeft = 9
    ExplicitTop = 9
    ExplicitHeight = 427
  end
  object Panel1: TPanel
    Left = 0
    Top = 38
    Width = 201
    Height = 509
    Align = alLeft
    TabOrder = 0
    object SelectThemeRadioGroup: TRadioGroup
      Left = 1
      Top = 1
      Width = 199
      Height = 327
      Align = alClient
      Caption = 'Select Theme/Color'
      ItemIndex = 0
      Items.Strings = (
        'Black'
        'Green'
        'Blue'
        'Silver'
        'Olive'
        'Red')
      TabOrder = 0
      OnClick = SelectThemeRadioGroupClick
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 328
      Width = 199
      Height = 77
      Align = alBottom
      Caption = 'Icon builder'
      TabOrder = 1
      object Label2: TLabel
        Left = 12
        Top = 17
        Width = 95
        Height = 13
        Caption = 'N. of Icons to build:'
      end
      object NumSpinEdit: TSpinEdit
        Left = 12
        Top = 40
        Width = 46
        Height = 22
        MaxValue = 1000
        MinValue = 1
        TabOrder = 0
        Value = 100
      end
      object AssignIconsButton: TButton
        Left = 64
        Top = 36
        Width = 115
        Height = 30
        Caption = 'Build Random Icons'
        TabOrder = 1
        OnClick = AssignIconsButtonClick
      end
    end
    object SliderPanel: TPanel
      Left = 1
      Top = 446
      Width = 199
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object IconSizeLabel: TLabel
        Left = 8
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Icons size:'
      end
      object TrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 199
        Height = 39
        Align = alBottom
        Max = 128
        Min = 12
        Frequency = 8
        Position = 32
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = TrackBarChange
      end
    end
    object ButtonsPanel: TPanel
      Left = 1
      Top = 405
      Width = 199
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object ClearButton: TButton
        Left = 5
        Top = 5
        Width = 76
        Height = 30
        Caption = 'Clear Icons'
        TabOrder = 0
        OnClick = ClearButtonClick
      end
      object ShowImageEditorButton: TButton
        Left = 86
        Top = 5
        Width = 106
        Height = 30
        Caption = 'Show Image Editor'
        TabOrder = 1
        OnClick = ShowImageEditorButtonClick
      end
    end
  end
  object TopToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 709
    Height = 38
    AutoSize = True
    ButtonHeight = 38
    ButtonWidth = 39
    Images = VirtualImageList
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = DisabledAction
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      Action = DeleteIconAction
    end
    object ToolButton3: TToolButton
      Left = 78
      Top = 0
      ImageIndex = 2
      ImageName = 'amazon'
    end
    object ToolButton4: TToolButton
      Left = 117
      Top = 0
      ImageIndex = 3
      ImageName = 'google-chrome'
    end
    object ToolButton5: TToolButton
      Left = 156
      Top = 0
      ImageIndex = 4
      ImageName = 'android'
    end
    object ToolButton6: TToolButton
      Left = 195
      Top = 0
      Action = ChangeIconAction
    end
    object ToolButton7: TToolButton
      Left = 234
      Top = 0
      Action = ChangeColorAction
      Enabled = False
    end
  end
  object paButtons: TPanel
    Left = 629
    Top = 38
    Width = 80
    Height = 509
    Align = alRight
    TabOrder = 2
    OnClick = paButtonsClick
    OnResize = paButtonsResize
    object IconFontImage: TIconFontImage
      Left = 1
      Top = 430
      Width = 78
      Height = 78
      ImageIndex = 0
      Align = alBottom
      ExplicitLeft = 2
    end
    object DeleteButton: TButton
      Left = 2
      Top = 6
      Width = 73
      Height = 60
      Action = DeleteIconAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 0
    end
    object ChangeIconButton: TButton
      Left = 2
      Top = 71
      Width = 73
      Height = 60
      Action = ChangeIconAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 1
    end
    object ChangeColorButton: TButton
      Left = 3
      Top = 137
      Width = 73
      Height = 60
      Action = ChangeColorAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 2
    end
    object ShowCharMapButton: TButton
      Left = 3
      Top = 201
      Width = 73
      Height = 60
      Action = ShowCharMapAction
      Caption = 'Char Map...'
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 3
    end
  end
  object ClientPanel: TPanel
    Left = 201
    Top = 38
    Width = 425
    Height = 509
    Align = alClient
    TabOrder = 3
    object ImageListLabel: TLabel
      Left = 1
      Top = 223
      Width = 423
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Image List Icons Preview'
      ExplicitWidth = 119
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 423
      Height = 222
      Align = alTop
      Images = VirtualImageList
      Indent = 35
      TabOrder = 0
      Items.NodeData = {
        070300000009540054007200650065004E006F00640065002500000001000000
        01000000FFFFFFFFFFFFFFFF0000000000000000000100000001036F006E0065
        0000002D0000000400000004000000FFFFFFFFFFFFFFFF000000000000000000
        0000000001076F006E0065002D006F006E006500000025000000020000000200
        0000FFFFFFFFFFFFFFFF000000000000000000020000000103740077006F0000
        002D0000000500000005000000FFFFFFFFFFFFFFFF0000000000000000000000
        00000107740077006F0020006F006E00650000002D0000000600000007000000
        00000000FFFFFFFF000000000000000000000000000107740077006F00200074
        0077006F000000290000000300000003000000FFFFFFFFFFFFFFFF0000000000
        0000000000000000010574006800720065006500}
    end
    object ImageView: TListView
      Left = 1
      Top = 236
      Width = 423
      Height = 272
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      TabOrder = 1
      OnSelectItem = ImageViewSelectItem
    end
  end
  object ActionList: TActionList
    Images = VirtualImageList
    Left = 288
    Top = 296
    object ChangeIconAction: TAction
      Category = 'Edit'
      Caption = 'Change icon'
      ImageIndex = 5
      ImageName = 'file-replace'
      OnExecute = ChangeIconActionExecute
    end
    object DeleteIconAction: TAction
      Category = 'Edit'
      Caption = 'Delete Icon'
      ImageIndex = 1
      ImageName = 'delete'
      OnExecute = DeleteIconActionExecute
    end
    object ChangeColorAction: TAction
      Category = 'Edit'
      Caption = 'Change Color'
      ImageIndex = 7
      ImageName = 'palette'
      OnExecute = ChangeColorActionExecute
    end
    object DisabledAction: TAction
      Category = 'Edit'
      Caption = 'Disabled'
      Enabled = False
      ImageIndex = 0
      ImageName = 'account'
    end
    object ShowCharMapAction: TAction
      Category = 'Edit'
      Caption = 'CharMap...'
      Hint = 'Show Char Map...'
      ImageIndex = 8
      ImageName = 'format-font'
      OnExecute = ShowCharMapActionExecute
    end
  end
  object ColorDialog: TColorDialog
    Left = 496
    Top = 312
  end
  object VirtualImageList: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'account'
        Name = 'account'
      end
      item
        CollectionIndex = 1
        CollectionName = 'delete'
        Name = 'delete'
      end
      item
        CollectionIndex = 2
        CollectionName = 'apple'
        Name = 'amazon'
      end
      item
        CollectionIndex = 3
        CollectionName = 'google-chrome'
        Name = 'google-chrome'
      end
      item
        CollectionIndex = 4
        CollectionName = 'android'
        Name = 'android'
      end
      item
        CollectionIndex = 5
        CollectionName = 'file-replace'
        Name = 'file-replace'
      end
      item
        CollectionIndex = 6
        CollectionName = 'account-search'
        Name = 'account-search'
      end
      item
        CollectionIndex = 7
        CollectionName = 'palette'
        Name = 'palette'
      end
      item
        CollectionIndex = 8
        CollectionName = 'format-font'
        Name = 'format-font'
      end>
    ImageCollection = dmImages.IconFontsImageCollection
    Width = 32
    Height = 32
    Left = 400
    Top = 400
  end
end
