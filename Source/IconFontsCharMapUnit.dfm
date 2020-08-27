object IconFontsCharMapForm: TIconFontsCharMapForm
  Left = 352
  Top = 227
  HelpContext = 26140
  Caption = 'Icon Fonts CharMap %s - Copyright Ethea S.r.l.'
  ClientHeight = 580
  ClientWidth = 688
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
    Top = 97
    Width = 688
    Height = 422
    Align = alClient
    Caption = '%d Icons available from selected Font'
    TabOrder = 1
    object ImageView: TListView
      Left = 2
      Top = 15
      Width = 684
      Height = 405
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
    Top = 525
    Width = 688
    Height = 55
    Align = alBottom
    Caption = 'Chars to copy'
    TabOrder = 2
    DesignSize = (
      688
      55)
    object CharsEdit: TEdit
      Left = 7
      Top = 17
      Width = 569
      Height = 21
      Hint = 'Icon Name'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = EditChangeUpdateGUI
    end
    object CopyToclipboardButton: TButton
      Left = 579
      Top = 16
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
    Width = 688
    Height = 97
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object paClient: TPanel
      Left = 0
      Top = 0
      Width = 605
      Height = 97
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object ImageGroup: TGroupBox
        Left = 0
        Top = 0
        Width = 605
        Height = 97
        Align = alClient
        Caption = 'Properties of Selected Icon'
        TabOrder = 0
        DesignSize = (
          605
          97)
        object FontIconHexLabel: TLabel
          Left = 89
          Top = 55
          Width = 74
          Height = 13
          AutoSize = False
          Caption = 'Hex value'
          Transparent = True
        end
        object FontIconDecLabel: TLabel
          Left = 169
          Top = 55
          Width = 78
          Height = 13
          AutoSize = False
          Caption = 'Decimal value'
          Transparent = True
        end
        object DefaultFontNameLabel: TLabel
          Left = 89
          Top = 13
          Width = 156
          Height = 13
          AutoSize = False
          Caption = 'Name'
          Transparent = True
        end
        object IconNameLabel: TLabel
          Left = 255
          Top = 55
          Width = 201
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Icon Name (from metadata)'
          Transparent = True
          Visible = False
        end
        object MainPanel: TPanel
          Left = 5
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
          object MainImage: TIconFontImage
            Left = 2
            Top = 2
            Width = 72
            Height = 72
            Align = alClient
          end
        end
        object FontIconHex: TEdit
          Left = 89
          Top = 70
          Width = 74
          Height = 21
          Hint = 'Hexadecimal value'
          CharCase = ecUpperCase
          MaxLength = 5
          ReadOnly = True
          TabOrder = 2
        end
        object FontIconDec: TEdit
          Left = 169
          Top = 70
          Width = 80
          Height = 21
          Hint = 'Decimal value'
          ReadOnly = True
          TabOrder = 3
        end
        object DefaultFontName: TComboBox
          Left = 91
          Top = 28
          Width = 510
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnSelect = DefaultFontNameSelect
        end
        object cbShowSurrogate: TCheckBox
          Left = 465
          Top = 55
          Width = 131
          Height = 17
          Anchors = [akTop, akRight]
          Caption = 'Show "surrogate pairs"'
          TabOrder = 5
          OnClick = cbShowSurrogateClick
        end
        object ShowCaptionsCheckBox: TCheckBox
          Left = 465
          Top = 74
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
          Top = 70
          Width = 202
          Height = 21
          Hint = 'Icon Name (from metadata registered)'
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 4
          Visible = False
        end
      end
    end
    object paButtons: TPanel
      Left = 605
      Top = 0
      Width = 83
      Height = 97
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
        OnClick = OKButtonClick
      end
      object HelpButton: TButton
        Left = 2
        Top = 66
        Width = 75
        Height = 25
        Caption = '&Help'
        TabOrder = 2
        OnClick = HelpButtonClick
      end
      object CancelButton: TButton
        Left = 2
        Top = 35
        Width = 75
        Height = 25
        Hint = 'Close'
        Caption = 'Cancel'
        TabOrder = 1
        OnClick = CancelButtonClick
      end
    end
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 519
    Width = 688
    Height = 6
    Align = alBottom
    TabOrder = 3
    Visible = False
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
