object MainForm: TMainForm
  Left = 916
  Top = 169
  Caption = 
    'Icon Fonts ImageList Demo - Copyright (c) Ethea S.r.l. - Apache ' +
    '2.0 Open Source License'
  ClientHeight = 592
  ClientWidth = 946
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 201
    Height = 551
    Align = alLeft
    TabOrder = 0
    object SelectThemeRadioGroup: TRadioGroup
      Left = 1
      Top = 1
      Width = 199
      Height = 369
      Align = alClient
      Caption = 'Select Theme/Color'
      ItemIndex = 0
      Items.Strings = (
        'Black'
        'White'
        'Blue'
        'Silver'
        'Olive'
        'Red')
      TabOrder = 0
      OnClick = SelectThemeRadioGroupClick
    end
    object GroupBox1: TGroupBox
      Left = 1
      Top = 370
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
      object AssignIconsButton: TBitBtn
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
      Top = 488
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
      Top = 447
      Width = 199
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object ClearButton: TBitBtn
        Left = 5
        Top = 5
        Width = 76
        Height = 30
        Caption = 'Clear Icons'
        TabOrder = 0
        OnClick = ClearButtonClick
      end
      object ShowImageEditorButton: TBitBtn
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
    Width = 946
    Height = 41
    ButtonHeight = 38
    ButtonWidth = 39
    Images = IconFontsImageList
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      ImageIndex = 0
    end
    object ToolButton2: TToolButton
      Left = 39
      Top = 0
      ImageIndex = 1
    end
    object ToolButton3: TToolButton
      Left = 78
      Top = 0
      ImageIndex = 2
    end
    object ToolButton4: TToolButton
      Left = 117
      Top = 0
      ImageIndex = 3
    end
    object ToolButton5: TToolButton
      Left = 156
      Top = 0
      ImageIndex = 4
    end
    object ToolButton6: TToolButton
      Left = 195
      Top = 0
      ImageIndex = 5
    end
    object ToolButton7: TToolButton
      Left = 234
      Top = 0
      ImageIndex = 6
    end
  end
  object Panel2: TPanel
    Left = 864
    Top = 41
    Width = 82
    Height = 551
    Align = alRight
    TabOrder = 2
    object DeleteButton: TBitBtn
      Left = 5
      Top = 6
      Width = 73
      Height = 60
      Action = DeleteIconAction
      Caption = 'Delete Icon'
      Layout = blGlyphTop
      TabOrder = 0
    end
    object ChangeIconButton: TBitBtn
      Left = 5
      Top = 71
      Width = 73
      Height = 60
      Action = ChangeIconAction
      Caption = 'Change icon'
      Layout = blGlyphTop
      TabOrder = 1
    end
  end
  object ClientPanel: TPanel
    Left = 201
    Top = 41
    Width = 663
    Height = 551
    Align = alClient
    TabOrder = 3
    object ImageListLabel: TLabel
      Left = 1
      Top = 223
      Width = 661
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = 'Image List Icons Preview'
      ExplicitWidth = 119
    end
    object TreeView: TTreeView
      Left = 1
      Top = 1
      Width = 661
      Height = 222
      Align = alTop
      Images = IconFontsImageList
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
      Width = 661
      Height = 314
      Align = alClient
      Columns = <>
      IconOptions.AutoArrange = True
      LargeImages = IconFontsImageList
      SmallImages = IconFontsImageList
      TabOrder = 1
    end
  end
  object IconFontsImageList: TIconFontsImageList
    IconFontItems = <
      item
        FontIconDec = 61445
        IconName = 'Account'
      end
      item
        FontIconDec = 61888
        FontColor = clMaroon
        IconName = 'Delete'
      end
      item
        FontIconDec = 61485
        IconName = 'Amazon'
      end
      item
        FontIconDec = 62127
        IconName = 'Google chrome'
      end
      item
        FontIconDec = 63375
        FontColor = clTeal
        IconName = 'Android head'
      end
      item
        FontIconDec = 64279
        IconName = 'File-replace'
      end
      item
        FontIconDec = 61462
        IconName = 'Account-search'
      end>
    FontName = 'Material Design Icons'
    FontColor = clBlack
    MaskColor = clBtnFace
    Size = 32
    OnFontMissing = IconFontsImageListFontMissing
    Left = 368
    Top = 304
  end
  object ActionList: TActionList
    Images = IconFontsImageList
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
  end
end
