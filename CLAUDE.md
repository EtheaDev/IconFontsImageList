# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

IconFontsImageList is a Delphi component library (VCL + FMX) for using Icon Fonts (e.g. Material Design Icons) as scalable images and image lists, with High-DPI support and GDI+ rendering. It supports Delphi 7 through Delphi 13, Win32 and Win64. Not a git repo (SVN working copy); upstream is github.com/EtheaDev/IconFontsImageList. There is no automated test suite — verification is done by building packages and running the demos.

## Build

There is no command-line build script; packages and demos are built from the RAD Studio IDE (or `msbuild` on the `.dproj` from a RAD Studio command prompt).

- Packages: `Packages\<DelphiVer>\IconFontsImageGroupPackages.groupproj` (e.g. `Packages\D13\`). Folder names: `D7`, `D2010`, `DXE3`, `DXE6`, `DXE8`, `D10_1`…`D10_4`, `D11`, `D12`, `D13`.
  - Build order: runtime `IconFontsImageList` (VCL) / `IconFontsImageListFMX`, then design-time `dclIconFontsImageList` / `dclIconFontsImageListFMX` (install these in the IDE).
  - DCU output: `Lib\<DelphiVer>\$(Platform)\$(Config)`. Packages use `DllSuffix=$(Auto)` from D10.4 on.
- Demos: `Demo\<DelphiVer>\*.dproj` (VCL `IconFontsImageListDemo`, FMX `IconFontsImageListDemoFMX`, `IconFontImageDemoFMX`); shared demo sources live in `Demo\Source`. Demos require the "Material Design Icons" font from `Demo\Fonts`.
- Installer: `Setup\BuildSetup.bat` compiles `Setup\Setup.iss` with Inno Setup 6 and signs the output (`Setup\Output\IconFontsImageList_Setup.exe`). Installer logic is in `Setup\InnoSetupScripts\Source\*.inc` (shared Ethea/Skia4Delphi-derived scripts).

When adding or removing a unit, update the `.dpk`/`.dproj` for **every** Delphi version folder under `Packages\` (and the D7/D2010-era packages may need different `requires`).

## Architecture

All library sources are in `Source\`; design-time code (component editors, registration, splash/about) is in `Packages\*.pas` (not `Source\`).

VCL rendering core:
- `IconFontsItems.pas` — `TIconFont` (one glyph: font name, code point, color, opacity, zoom; does the actual painting, via GDI+ when `GDI+` is defined, else GDI) and `TIconFontItem`/`TIconFontItems` (the owned collection that both image lists and the image collection store). Handles surrogate-pair code points (> $FFFF).
- `IconFontsImageCollection.pas` — `TIconFontsImageCollection`: descends from `TCustomImageCollection` on D10.3+ (so it plugs into the native `TVirtualImageList`), else from `TComponent`.
- `IconFontsImageListBase.pas` — `TIconFontsImageListBase` (descends `TDragImageList`): shared image-list logic, DPI scaling, and the `IconFontsImageListVersion` constant.
  - `IconFontsImageList.pas` — `TIconFontsImageList`: owns its own `TIconFontItems`.
  - `IconFontsVirtualImageList.pas` — `TIconFontsVirtualImageList`: pulls items from a linked `TIconFontsImageCollection` (meant for Delphi < 10.3; on newer versions prefer collection + native `TVirtualImageList`).
- `IconFontsImage.pas` — `TIconFontImage` control; can draw a standalone icon or one taken from an image list / VirtualImageList.
- `IconFontsUtils.pas` — helpers (update icon colors from active VCL Style, disabled/hot image lists, etc.).
- `IconFontsCharMapUnit.pas` — CharMap form for browsing/picking glyphs (used by the editor, also usable at runtime).
- `Fonts\Icons.*.pas` — name→code point tables (Material Design, FontAwesome) used by the editor/CharMap search.

FMX (`FMX.IconFontsImageList.pas`, `FMX.IconFontImage.pas`) is a separate implementation built on FMX `TCustomImageList`/`TMultiResBitmap`/`TImage`; it does not share units with the VCL side and has its own `IconFontsImageListVersion` constant.

### Version-conditional code

Every unit includes `Source\IconFontsImageList.inc`, which maps `VERxxx` to cumulative defines (`D2010+`, `DXE+` … `D10_3+` … `D13+`) plus feature defines: `GDI+` (DXE4+), `HasStoreBitmapProperty` and `HiDPISupport` (D10.3+). Code must still compile on Delphi 7 (non-Unicode) — guard newer RTL/VCL usage with these defines. When adding a new Delphi version, extend this `.inc` and add `Packages\`, `Lib\` and `Demo\` folders for it.

### Releasing a new version

The version string appears in several places that must stay in sync: `IconFontsImageListVersion` in `Source\IconFontsImageListBase.pas` and `Source\FMX.IconFontsImageList.pas`, `LibraryVersion` in `Setup\Setup.iss`, and the "Actual official version" + release notes in `README.md` (and `README.htm`).

## DPM package (delphi.dev)

The library is also published as a DPM package (Delphi Package Manager, https://delphi.dev,
organization EtheaDev): spec `EtheaDev.IconFontsImageList.dspec` in the root (id `EtheaDev.IconFontsImageList`). It is packed from the GitHub
clone and pushed by `D:\ETHEA\GitHub_EtheaDev\_sync\sync_project.py IconFontsImageList --release`; `version`
is aligned to the README release notes automatically and `repositoryCommit: '#HASH#'` is replaced
at pack time: don't edit them by hand.

**Keep the dspec aligned with the packages and the Setup**: when a package folder is added or
removed (new Delphi version), a `.dpk` is added/renamed, a new kind of file is needed by the
packages (`.dfm`, `.res`, `.dcr`, ...), or a platform is enabled/disabled in a runtime `.dproj`,
update `targetPlatforms` / `templates` (`source`, `build`, `design`) in the same commit. Check
with `python D:\ETHEA\GitHub_EtheaDev\_sync\dpm_tools.py IconFontsImageList`: the publishing script refuses
to commit/release while the check fails.

- Delphi XE3, XE6, XE8: VCL only; 10.1 to 13: VCL + FMX (template `vclfmx`). D7 and D2010 are not
  supported by DPM.
