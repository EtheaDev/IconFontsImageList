object dmImages: TdmImages
  OnDestroy = DataModuleDestroy
  Height = 278
  Width = 365
  object IconFontsImageCollection: TIconFontsImageCollection
    IconFontItems = <
      item
        FontIconDec = 983044
        IconName = 'account'
      end
      item
        FontIconDec = 983476
        FontColor = clMaroon
        IconName = 'delete'
      end
      item
        FontIconDec = 983093
        IconName = 'apple'
      end
      item
        FontIconDec = 983727
        IconName = 'google-chrome'
      end
      item
        FontIconDec = 983090
        FontColor = clTeal
        IconName = 'android'
      end
      item
        FontIconDec = 985906
        IconName = 'file-replace'
      end
      item
        FontIconDec = 983062
        IconName = 'account-search'
      end
      item
        FontIconDec = 984024
        IconName = 'palette'
      end
      item
        FontIconDec = 984790
        IconName = 'format-font'
      end
      item
        FontIconDec = 984012
        IconName = 'open-in-new'
      end>
    FontName = 'Material Design Icons'
    OnFontMissing = IconFontsImageCollectionFontMissing
    Left = 184
    Top = 152
  end
end
