object IconFontsCharMapForm: TIconFontsCharMapForm
  Left = 352
  Top = 227
  Width = 698
  Height = 619
  HelpContext = 26140
  Caption = 'Icon Fonts CharMap %s - Copyright Ethea S.r.l.'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ImageListGroup: TGroupBox
    Left = 0
    Top = 113
    Width = 682
    Height = 362
    Align = alClient
    Caption = '%d Icons available from selected Font'
    TabOrder = 1
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 678
      Height = 345
      Cursor = crHandPoint
      Hint = 'Use Ctrl+click to copy character'
      Align = alClient
      Columns = <>
      HideSelection = False
      IconOptions.AutoArrange = True
      ReadOnly = True
      TabOrder = 0
      OnDblClick = ImageViewDblClick
      OnMouseDown = ImageViewMouseDown
      OnSelectItem = ImageViewSelectItem
    end
  end
  object IconBuilderGroupBox: TGroupBox
    Left = 0
    Top = 481
    Width = 682
    Height = 61
    Align = alBottom
    Caption = 'Chars to copy'
    TabOrder = 2
    DesignSize = (
      682
      61)
    object CharsEdit: TEdit
      Left = 7
      Top = 25
      Width = 554
      Height = 21
      Hint = 'Icon Name'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = EditChangeUpdateGUI
    end
    object CopyToclipboardButton: TButton
      Left = 569
      Top = 22
      Width = 104
      Height = 30
      Action = CopyToCipboardAction
      Anchors = [akTop, akRight]
      TabOrder = 1
    end
  end
  object paTop: TPanel
    Left = 0
    Top = 0
    Width = 682
    Height = 113
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 682
      Height = 113
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageGroup: TGroupBox
        Left = 0
        Top = 0
        Width = 682
        Height = 113
        Align = alClient
        Caption = 'Properties of Selected Icon'
        TabOrder = 0
        DesignSize = (
          682
          113)
        object FontIconHexLabel: TLabel
          Left = 91
          Top = 63
          Width = 74
          Height = 13
          AutoSize = False
          Caption = 'Hex value'
          Transparent = True
        end
        object FontIconDecLabel: TLabel
          Left = 169
          Top = 63
          Width = 78
          Height = 13
          AutoSize = False
          Caption = 'Decimal value'
          Transparent = True
        end
        object DefaultFontNameLabel: TLabel
          Left = 91
          Top = 21
          Width = 156
          Height = 13
          AutoSize = False
          Caption = 'Name'
          Transparent = True
        end
        object IconNameLabel: TLabel
          Left = 255
          Top = 63
          Width = 244
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Icon Name (from metadata)'
          Transparent = True
          Visible = False
        end
        object MainPanel: TPanel
          Left = 5
          Top = 22
          Width = 78
          Height = 78
          BevelOuter = bvNone
          BorderWidth = 2
          BorderStyle = bsSingle
          Color = clWindow
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
          object MainImage: TIconFontImage
            Left = 2
            Top = 2
            Width = 72
            Height = 72
            DoubleBuffered = False
            Align = alClient
          end
        end
        object FontIconHex: TEdit
          Left = 91
          Top = 78
          Width = 70
          Height = 21
          Hint = 'Hexadecimal value'
          CharCase = ecUpperCase
          MaxLength = 5
          ReadOnly = True
          TabOrder = 2
        end
        object FontIconDec: TEdit
          Left = 169
          Top = 78
          Width = 78
          Height = 21
          Hint = 'Decimal value'
          ReadOnly = True
          TabOrder = 3
        end
        object DefaultFontName: TComboBox
          Left = 91
          Top = 36
          Width = 430
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 1
          OnSelect = DefaultFontNameSelect
        end
        object cbShowSurrogate: TCheckBox
          Left = 537
          Top = 38
          Width = 131
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Show "surrogate pairs"'
          TabOrder = 5
          OnClick = cbShowSurrogateClick
        end
        object ShowCaptionsCheckBox: TCheckBox
          Left = 537
          Top = 80
          Width = 131
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Show captions'
          Checked = True
          State = cbChecked
          TabOrder = 6
          OnClick = ShowCaptionsCheckBoxClick
        end
        object IconName: TEdit
          Left = 255
          Top = 78
          Width = 266
          Height = 21
          Hint = 'Icon Name (from metadata registered)'
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 4
          Visible = False
        end
      end
    end
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 475
    Width = 682
    Height = 6
    Align = alBottom
    TabOrder = 3
    Visible = False
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 542
    Width = 682
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      682
      38)
    object OKButton: TButton
      Left = 398
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = OKButtonClick
    end
    object HelpButton: TButton
      Left = 588
      Top = 6
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
    object CancelButton: TButton
      Left = 493
      Top = 6
      Width = 85
      Height = 25
      Hint = 'Close'
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelButtonClick
    end
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 336
    Top = 296
    object CopyToCipboardAction: TAction
      Caption = 'Copy to Cipboard'
      OnExecute = CopyToCipboardActionExecute
      OnUpdate = CopyToCipboardActionUpdate
    end
  end
end
