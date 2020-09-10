# IconFontsImageList [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

## Components to simplify use of "Icon fonts": resize, color, opacity and more... with full support for High-DPI apps. Rendering optimized with GDI+

### Actual official version 2.3 (VCL) and 1.5.1 (FMX)

| Component | Description |
| - | - |
| ![https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageCollectionComponentIcon.png](https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageCollectionComponentIcon.png) | **TIconFontsImageCollection** is collection of "Icon Fonts" for Delphi to provide a centralized list of images for IconFontsVirtualImageList (only for VCL) |
| ![https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsVirtualImageListComponentIcon.png](https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsVirtualImageListComponentIcon.png) | **TIconFontsVirtualImageList** is a special "virtual" ImageList for Delphi linked to an IconFontsImageCollection (only for VCL) to simplify use of "Font Icons" (resize, color and more...) |
| ![https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageComponentIcon.png](https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageComponentIcon.png) | **TIconFontImage** is an extended Image component for Delphi (VCL+FMX) to show any Icon Font directly or included into a an IconFontsImageList with all functionality (stretch, color, opacity and more...) |
| ![https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageListComponentIcon.png](https://github.com/EtheaDev/IconFontsImageList/blob/master/Packages/IconFontsImageListComponentIcon.png) | **TIconFontsImageList** is an extended ImageList for Delphi (VCL+FMX) to simplify use of Icon Fonts (resize, color and more...). Use only for simple application with one form. |

![Delphi 10.4 Sydney Support](/Demo/Images/SupportingDelphi.jpg)

Sample image of VCL version
![https://github.com/EtheaDev/IconFontsImageList/blob/master/Demo/Images/Sample.jpg](/Demo/Images/Sample.jpg)

Sample image of FMX version
![https://github.com/EtheaDev/IconFontsImageList/blob/master/Demo/Images/SampleFMX.jpg](/Demo/Images/SampleFMX.jpg)

Follow the [guide in Wiki section](https://github.com/EtheaDev/IconFontsImageList/wiki) to known how to use those components to modernize your Delphi VCL or FMX applications scalable, colored and beautiful with few lines of code.

## Very important notice:

**TVirtualImageList** (available from D10.3) and **TIconFontsVirtualImageList** both use images from **TIconFontsImageCollection**. An important difference is that TVirtualImageList may use and create only a subset of the images in the collection, whereas TIconFontsVirtualImageList creates all images of the collection everytime it is needed (e,g. DPI change), which is slower and consumes more memory.

We advise that TIconFontsVirtualImageList should be used only for versions of Delphi before 10.3. For recent versions of Delphi the recommended combination should be **TIconFontsImageCollection + TVirtualImageList**. Don't forget also the importance of TVirtualImageList.PreserveItems when you have a large ImageCollection with many linked Actions. Without setting this property to "True", everytime you add or remove an icon in the collection, you have to check and change the ImageIndex of all the Actions.

**Warning: "Material Font" has changed**

Take care of changed Font file used in the Demos from v1.7(VCL)/v1.2(FMX) -> v1.8(VCL)/v1.3(FMX), as explained [here](https://github.com/EtheaDev/IconFontsImageList/wiki/Deploy-Applications). 

If you are using the old Font, you can automatically convert icons to the new ones as explained [here](https://github.com/EtheaDev/IconFontsImageList/wiki/Convert-Material-Icons).

**RELEASE NOTES:**
05 Sep 2020: version 2.3 (VCL) and 1.5.1 (FMX)
 - Added demo with VirtualImageList form D10.3 and D10.4
 - Refactoring to move methods from IconFontsImageList to IconFontsImageCollection
 - Update IconFontImage to connect also to a VirtualImageList
 - Fixed some packages and demos for older Delphi versions (Delphi7, DXE3, DXE6, DXE8)

04 Sep 2020: version 2.2.2 (VCL) and 1.5.1 (FMX)
 - Export Icons in png files into component editor
 - Updated "Material Design Font Desktop.ttf" font and metadata
 
27 Aug 2020: version 2.2.1 (VCL) and 1.5.0 (FMX)
 - Complete refactoring for full support of High-DPI
 - New IconFontsImageCollection component
 - New IconFontsVirtualImageList component
 - Redesigned component editor to support Categories for icons
 - New support for native VirtualImageList (from D10.3)
 - Fixed some issues

15 July 2020: official 2.1 (VCL) and 1.5 version (FMX)
- Fixed rendering on TButton!
- Fixed rendering for Delphi7

19 June 2020: official 2.0 (VCL) and 1.5 version (FMX)
- Added [GDI+ support](https://github.com/EtheaDev/IconFontsImageList/wiki/GDI) (from DXE6)
- Added new TIconFontsImage component
- Added Width, Height, DisabledFactor, Opacity properties for TIconFontsImageList

08 June 2020: official 1.10 (VCL) and 1.5 version (FMX)
- Added IconManager and an example of metadata info form Material Design Icons Desktop Font
- Updated CharMap with metadata support
- Support for Delphi 10.4 Sydney

17 May 2020: official 1.9 (VCL) and 1.4 version (FMX)
- Added component "menu option" to [convert "Material" Icons](https://github.com/EtheaDev/IconFontsImageList/wiki/Convert-Material-Icons) from old font "Material Design Icons" (materialdesignicons-webfont.ttf) to new font "Material Design Icons Desktop" (Material Design Icons Desktop.ttf)
- Restored old font "Material Design Icons" only for use with Delphi 7 and Delphi 2010.
- Fixed FMX components and demos to compile with mobile platforms

13 May 2020: official 1.8 (VCL) and 1.3 version (FMX)
- Changed font used in the Demos to preserve future changes of char/mapping.
- Added preview for new "Material Design Icons Desktop.ttf" in Demo/Fonts folder.

9 May 2020: official 1.2 version (FMX)
- Fixed component editor and packages for Delphi 10.1 and 10.2

7 May 2020: official 1.1 version (FMX)
- Added Component Editor for FMX version: [look here...](https://github.com/EtheaDev/IconFontsImageList/wiki/Component-Editor-(FMX)).
- Fixed some problems in FMX version
- Updated Demo for FMX version

2 May 2020: official 1.7 version (VCL)
- Added utilities to manage Disabled and Hot Imagelist
- Added a Video tutorial in Wiki section

12 April 2020: official 1.6 version (VCL)
- Added a custom CharMap form (VCL only) to show and collect icons more easily: [look here...](https://github.com/EtheaDev/IconFontsImageList/wiki/CharMap).

27 March 2020: official 1.5 version (VCL) and first 1.0 version (FMX)
- Added support for Icon Fonts that contains surrogate pair characters (VCL & FMX)!

Now you can use all the icons present in any "font collection" that containts those special characters: [look here...](https://github.com/EtheaDev/IconFontsImageList/wiki/Icon-Fonts-with-surrogate-pair).

24 March 2020: official 1.4 version (VCL) and first Beta version (FMX)
- Improved performances into IDE (VCL)
- Added two new FMX components: TIconFontsImageListFMX and TIconFontImageFMX (beta)
- Added two demos for TIconFontsImageListFMX and TIconFontImageFMX
- Minor fixes (VCL)

30 January 2020: official 1.3 version
- Added support for Delphi7 (generally for no Unicode version)
- Fixed when adding an icon without specifying a character
- Added SaveToFile method to save the icons into a single Bitmap
- Added Export button To ImageList Editor
- Added OnMissingFont event

21 January 2020: official 1.2 version
- Added support for DXE8
- Demos with various VCL Styles
- Utility to update automatically IconFonts color by active VCL Style
- Increased performance when switching VCL Style at runtime
- Minor fixes

13 December 2019: official 1.1 version
- Fixed some problems when adding or deleting icons into Extended Editor and Collection Editor
- Added support for building icons from/to Chars (into Extended Editor)
- Fixed some minor problems

27 November 2019: first stable version 1.0:
- The component TIconFontsImageList with advanced component editor.
- Demo to show how to change the size, fonts and colors of the icons.
- Very high performance for building hundreds of icons.
- Support from Delphi 2010 to 10.3 Rio

Inspired by the "Font Icon Editor" project by Luca Minuti:
[https://github.com/lminuti/FontIconEditor](https://github.com/lminuti/FontIconEditor)
