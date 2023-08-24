object ImageListEditor: TImageListEditor
  Left = 0
  Top = 0
  HelpContext = 26140
  Caption = 'ImageList Editor'
  ClientHeight = 445
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MinHeight = 333
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    584
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object OK: TButton
    Left = 501
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object Cancel: TButton
    Left = 501
    Top = 37
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Apply: TButton
    Left = 501
    Top = 67
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'A&pply'
    TabOrder = 2
    OnClick = ApplyClick
  end
  object Help: TButton
    Left = 501
    Top = 98
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpClick
  end
  object ImageListGroup: TGroupBox
    Left = 10
    Top = 159
    Width = 566
    Height = 271
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' &Images '
    TabOrder = 4
    ExplicitHeight = 123
    DesignSize = (
      566
      271)
    object ImageView: TListView
      Left = 3
      Top = 18
      Width = 545
      Height = 216
      Anchors = [akLeft, akTop, akRight, akBottom]
      Color = clBlack
      Columns = <
        item
          Caption = 'asdf'
          Width = 100
        end>
      DragCursor = crArrow
      DragMode = dmAutomatic
      HideSelection = False
      IconOptions.Arrangement = iaLeft
      IconOptions.AutoArrange = True
      IconOptions.WrapText = False
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnCompare = ImageViewCompare
      OnEndDrag = ImageViewEndDrag
      OnDragDrop = ImageViewDragDrop
      OnDragOver = ImageViewDragOver
      OnSelectItem = ImageViewSelectItem
      ExplicitHeight = 68
    end
    object Add: TButton
      Left = 31
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add...'
      TabOrder = 1
      OnClick = AddClick
      ExplicitTop = 92
    end
    object Delete: TButton
      Left = 243
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Delete'
      Enabled = False
      TabOrder = 2
      OnClick = DeleteClick
      ExplicitTop = 92
    end
    object Clear: TButton
      Left = 349
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Clear'
      Enabled = False
      TabOrder = 3
      OnClick = ClearClick
      ExplicitTop = 92
    end
    object ExportBtn: TButton
      Left = 455
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'E&xport...'
      Enabled = False
      TabOrder = 4
      OnClick = ExportBtnClick
      ExplicitTop = 92
    end
    object ReplaceBtn: TButton
      Left = 137
      Top = 240
      Width = 100
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Replace...'
      Enabled = False
      TabOrder = 5
      OnClick = AddClick
      ExplicitTop = 92
    end
  end
  object ImageGroup: TGroupBox
    Left = 10
    Top = 8
    Width = 485
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Caption = ' &Selected Image '
    TabOrder = 5
    DesignSize = (
      485
      145)
    object OptionsPanel: TPanel
      Left = 93
      Top = 15
      Width = 389
      Height = 131
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        389
        131)
      object FillLabel: TLabel
        Left = 10
        Top = 42
        Width = 372
        Height = 13
        AutoSize = False
        Caption = '&Fill Color:'
        Transparent = False
      end
      object TransparentLabel: TLabel
        Left = 10
        Top = 0
        Width = 372
        Height = 13
        AutoSize = False
        Caption = '&Transparent Color:'
        Transparent = False
      end
      object OptionsGroup: TRadioGroup
        Left = 10
        Top = 82
        Width = 372
        Height = 37
        Anchors = [akLeft, akTop, akRight]
        Caption = ' Options '
        Columns = 3
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'Cr&op'
          'St&retch'
          'C&enter')
        TabOrder = 0
        OnClick = OptionsGroupClick
      end
      object FillColor: TColorBox
        Left = 10
        Top = 56
        Width = 372
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = FillColorChange
      end
      object TransparentColor: TColorBox
        Left = 10
        Top = 14
        Width = 372
        Height = 22
        DefaultColorColor = clWindow
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeDefault, cbCustomColor, cbPrettyNames]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        OnChange = TransparentColorChange
      end
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
        OnMouseDown = MainImageMouseDown
        OnMouseMove = MainImageMouseMove
        OnMouseUp = MainImageMouseUp
        ExplicitWidth = 60
        ExplicitHeight = 60
      end
    end
  end
  object OpenDialog: TOpenPictureDialog
    HelpContext = 27000
    DefaultExt = 'bmp'
    Filter = 
      'All (*.bmp, *.ico)|*.bmp;*.ico|Bitmaps (*.bmp)|*.bmp|Icons (*.ic' +
      'o)|*.ico'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Add Images'
    Left = 32
    Top = 232
  end
  object DragTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DragTimerTimer
    Left = 32
    Top = 280
  end
  object SaveDialog: TSavePictureDialog
    HelpContext = 27010
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Options = [ofOverwritePrompt, ofEnableSizing]
    Title = 'Export Images'
    Left = 32
    Top = 184
  end
end
