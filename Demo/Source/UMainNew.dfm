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
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 626
    Top = 38
    Height = 509
    Align = alRight
    AutoSnap = False
    MinSize = 80
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
      Height = 265
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
      Top = 266
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
      Top = 384
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
      Top = 343
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
    object ZoomPanel: TPanel
      Left = 1
      Top = 446
      Width = 199
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      object ZoomLabel: TLabel
        Left = 8
        Top = 3
        Width = 58
        Height = 13
        Caption = 'Icons zoom:'
      end
      object ZoomTrackBar: TTrackBar
        Left = 0
        Top = 23
        Width = 199
        Height = 39
        Align = alBottom
        Max = 100
        Min = 10
        Frequency = 8
        Position = 100
        PositionToolTip = ptBottom
        TabOrder = 0
        OnChange = ZoomTrackBarChange
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
    ButtonWidth = 40
    Images = VirtualImageList
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = DisabledAction
    end
    object ToolButton2: TToolButton
      Left = 40
      Top = 0
      Action = DeleteIconAction
    end
    object ToolButton3: TToolButton
      Left = 80
      Top = 0
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 120
      Top = 0
      ImageIndex = 3
    end
    object ToolButton5: TToolButton
      Left = 160
      Top = 0
      ImageIndex = 4
    end
    object ToolButton6: TToolButton
      Left = 200
      Top = 0
      Action = ChangeIconAction
    end
    object ToolButton7: TToolButton
      Left = 240
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
    OnResize = paButtonsResize
    object IconFontImage: TIconFontImage
      Left = 1
      Top = 430
      Width = 78
      Height = 78
      Hint = 
        'Click left - right mouse button to change icon into IconFontImag' +
        'e'
      ImageList = VirtualImageList
      ImageIndex = 0
      Align = alBottom
      OnMouseDown = IconFontImageMouseDown
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
    object NewFormButton: TButton
      Left = 3
      Top = 265
      Width = 73
      Height = 60
      Action = NewFormAction
      ImageAlignment = iaTop
      Images = VirtualImageList
      TabOrder = 5
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
        0303000000240000000100000001000000FFFFFFFFFFFFFFFF00000000000000
        000100000001036F006E0065002C0000000400000004000000FFFFFFFFFFFFFF
        FF00000000000000000000000001076F006E0065002D006F006E006500240000
        000200000002000000FFFFFFFFFFFFFFFF000000000000000002000000010374
        0077006F002C0000000500000005000000FFFFFFFFFFFFFFFF00000000000000
        00000000000107740077006F0020006F006E0065002C00000006000000070000
        0000000000FFFFFFFF0000000000000000000000000107740077006F00200074
        0077006F00280000000300000003000000FFFFFFFFFFFFFFFF00000000000000
        0000000000010574006800720065006500}
    end
    object ImageView: TListView
      Left = 1
      Top = 236
      Width = 423
      Height = 272
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      LargeImages = VirtualImageList
      SmallImages = VirtualImageList
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
      OnExecute = ChangeIconActionExecute
    end
    object DeleteIconAction: TAction
      Category = 'Edit'
      Caption = 'Delete Icon'
      ImageIndex = 1
      OnExecute = DeleteIconActionExecute
    end
    object ChangeColorAction: TAction
      Category = 'Edit'
      Caption = 'Change Color'
      ImageIndex = 7
      OnExecute = ChangeColorActionExecute
    end
    object DisabledAction: TAction
      Category = 'Edit'
      Caption = 'Disabled'
      Enabled = False
      ImageIndex = 0
    end
    object ShowCharMapAction: TAction
      Category = 'Edit'
      Caption = 'CharMap...'
      Hint = 'Show Char Map...'
      ImageIndex = 8
      OnExecute = ShowCharMapActionExecute
    end
    object NewFormAction: TAction
      Caption = 'New Form...'
      ImageIndex = 9
      OnExecute = NewFormActionExecute
    end
  end
  object ColorDialog: TColorDialog
    Left = 496
    Top = 312
  end
  object VirtualImageList: TVirtualImageList
    DisabledGrayscale = False
    DisabledSuffix = '_Disabled'
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'account'
        Disabled = False
        Name = 'account'
      end
      item
        CollectionIndex = 1
        CollectionName = 'delete'
        Disabled = False
        Name = 'delete'
      end
      item
        CollectionIndex = 2
        CollectionName = 'amazon'
        Disabled = False
        Name = 'amazon'
      end
      item
        CollectionIndex = 3
        CollectionName = 'google-chrome'
        Disabled = False
        Name = 'google-chrome'
      end
      item
        CollectionIndex = 4
        CollectionName = 'android'
        Disabled = False
        Name = 'android'
      end
      item
        CollectionIndex = 5
        CollectionName = 'file-replace'
        Disabled = False
        Name = 'file-replace'
      end
      item
        CollectionIndex = 6
        CollectionName = 'account-search'
        Disabled = False
        Name = 'account-search'
      end
      item
        CollectionIndex = 7
        CollectionName = 'palette'
        Disabled = False
        Name = 'palette'
      end
      item
        CollectionIndex = 8
        CollectionName = 'format-font'
        Disabled = False
        Name = 'format-font'
      end
      item
        CollectionIndex = 9
        CollectionName = 'open-in-new'
        Disabled = False
        Name = 'open-in-new'
      end>
    ImageCollection = dmImages.IconFontsImageCollection
    Width = 32
    Height = 32
    Left = 368
    Top = 384
  end
end
