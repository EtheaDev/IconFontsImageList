{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/IconFontsImageList                         }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit IconFontsUtils;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Classes
  , ImgList
  , IconFontsImageListBase
  , IconFontsImageCollection
  , Windows
  , Graphics
  , ComCtrls;

{$IFDEF D2010+}
function SaveToPngFiles(ImageList: TIconFontsImageListBase;
  const AOutFolder: string): Integer;
{$ENDIF}
function UpdateIconFontListView(const AListView: TListView;
  const ACategory: string = ''): Integer;
function UpdateIconFontListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
procedure UpdateIconFontsColorByStyle(const IconFontsImageList: TIconFontsImageListBase;
  const AReplaceCustomColors: Boolean = False); overload;
procedure UpdateIconFontsColorByStyle(const IconFontsImageCollection: TIconFontsImageCollection;
  const AReplaceCustomColors: Boolean = False); overload;
procedure UpdateDisabledImageList(const ASourceImageList, ADestImageList: TIconFontsImageListBase;
  const APercent: Integer = 30; const AReplaceCustomColors: Boolean = False);
procedure UpdateHotImageList(const ASourceImageList, ADestImageList: TIconFontsImageListBase;
  const APercent: Integer = 30; const AResizePercent: Integer = 0;
  const AReplaceCustomColors: Boolean = False);
function DarkerColor(AColor: TColor; APercent: Integer): TColor;
function LighterColor(AColor: TColor; APercent: Integer): TColor;
function DisabledColor(AColor: TColor; APercent: Integer): TColor;
function IsLightColor(const AColor: TColor): Boolean;
function HotColor(AColor: TColor; const APercent: Integer): TColor;
function GrayscaleColor(AColor : TColor) : TColor;

implementation

uses
  SysUtils
  {$IFDEF D2010+}
  , PngImage
  {$ENDIF}
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
  , Themes
  , IconFontsItems
  ;

{$IFDEF D2010+}
// Source: http://www.entwickler-ecke.de/topic_Bitmap+pf32bit+mit+Alpha+afPremultipied+zu+PNG+speichern_103159,0.html
type
  TRGB = packed record B, G, R: byte end;
  TRGBA = packed record B, G, R, A: byte end;
  TRGBAArray = array[0..0] of TRGBA;

function PNG4TransparentBitMap(aBitmap: TBitmap): TPNGImage;
var
  X, Y: integer;
  BmpRGBA: ^TRGBAArray;
  PngRGB: ^TRGB;
begin
  //201011 Thomas Wassermann
  Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 8, aBitmap.Width , aBitmap.Height);

  Result.CreateAlpha;
  Result.Canvas.CopyMode:= cmSrcCopy;
  Result.Canvas.Draw(0, 0, aBitmap);

  for Y := 0 to Pred(aBitmap.Height) do
  begin
    BmpRGBA := aBitmap.ScanLine[Y];
    PngRGB:= Result.Scanline[Y];

    for X := 0 to Pred(aBitmap.width) do
    begin
      Result.AlphaScanline[Y][X] :=  BmpRGBA[X].A;
      if aBitmap.AlphaFormat in [afDefined, afPremultiplied] then
      begin
        if BmpRGBA[X].A <> 0 then
        begin
          PngRGB^.B := Round(BmpRGBA[X].B / BmpRGBA[X].A * 255);
          PngRGB^.R := Round(BmpRGBA[X].R / BmpRGBA[X].A * 255);
          PngRGB^.G := Round(BmpRGBA[X].G / BmpRGBA[X].A * 255);
        end else begin
          PngRGB^.B := Round(BmpRGBA[X].B * 255);
          PngRGB^.R := Round(BmpRGBA[X].R * 255);
          PngRGB^.G := Round(BmpRGBA[X].G * 255);
        end;
      end;
      Inc(PngRGB);
    end;
  end;
end;

function SaveToPngFiles(ImageList: TIconFontsImageListBase;
  const AOutFolder: string): Integer;
var
  LImagePng: TPngImage;
  LBitmap: TBitmap;
  LIconName, LFileName: string;
  I: Integer;
  LItem: TIconFontItem;
begin
  Result := 0;
  for I := 0 to ImageList.IconFontItems.Count -1 do
  begin
    LItem := ImageList.IconFontItems[I];
    LBitmap := nil;
    LImagePng := nil;
    try
      LBitmap := LItem.GetBitmap(ImageList.Width, ImageList.Height, True);
      LImagePng := PNG4TransparentBitMap(LBitmap);
      if LItem.IconName <> '' then
        LIconName := Format('%s - ($%s)', [LItem.IconName, LItem.FontIconHex])
      else
        LIconName := Format('%d - ($%s)', [LItem.Index, LItem.FontIconHex]);
      LFileName := IncludeTrailingPathDelimiter(AOutFolder)+
        StringReplace(LIconName, '\', '_',[rfReplaceAll])+'.png';
      LImagePng.SaveToFile(LFileName);
      Inc(Result);
    finally
      LBitmap.Free;
      LImagePng.Free;
    end;
  end;
end;
{$ENDIF}

function UpdateIconFontListView(const AListView: TListView;
  const ACategory: string = ''): Integer;
var
  I: Integer;
  LItem: TIconFontItem;
  LListItem: TListItem;
  LIconFontItems: TIconFontItems;
begin
  LIconFontItems := nil;
  if AListView.LargeImages is TIconFontsImageListBase then
    LIconFontItems := TIconFontsImageListBase(AListView.LargeImages).IconFontItems;
  {$IFDEF D10_3+}
  if (AListView.LargeImages is TVirtualImageList) and
    (TVirtualImageList(AListView.LargeImages).ImageCollection is TIconFontsImageCollection) then
    LIconFontItems := TIconFontsImageCollection(TVirtualImageList(AListView.LargeImages).ImageCollection).IconFontItems;
  {$ENDIF}
  AListView.Items.BeginUpdate;
  try
    AListView.Clear;
    if LIconFontItems <> nil then
    begin
      Result := LIconFontItems.Count;
      for I := 0 to Result -1 do
      begin
        LItem := LIconFontItems[I];
        if (ACategory = '') or
         (LowerCase(ACategory) = LowerCase(LItem.Category)) then
        begin
          LListItem := AListView.Items.Add;
          LListItem.Caption := Format('$%s%s%s',
            [LItem.FontIconHex,sLineBreak,
             Litem.Name]);
          LListItem.ImageIndex := I;
        end;
      end;
    end
    else
      Result := 0;
  finally
    AListView.Items.EndUpdate;
  end;
end;

function UpdateIconFontListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TIconFontItem;
  LListItem: TListItem;
  LIconFontsImageList: TIconFontsImageListBase;
begin
  LIconFontsImageList := AListView.LargeImages as TIconFontsImageListBase;
  AListView.Items.BeginUpdate;
  try
    Result := AListView.Items.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.IconFontItems[I];
      LListItem := AListView.Items[I];
      if AShowCaption then
      begin
        LListItem.Caption := Format('$%s%s%s',
          [LItem.FontIconHex,sLineBreak,
           Litem.Name]);
      end
      else
        LListItem.Caption := '';
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

procedure UpdateIconFontsColorByStyle(const IconFontsImageList: TIconFontsImageListBase;
  const AReplaceCustomColors: Boolean = False);
{$IFDEF DXE+}
var
  LStyleFontColor, LStyleMaskColor: TColor;
{$ENDIF}
begin
  {$IFDEF DXE+}
  LStyleFontColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextNormal);
  LStyleMaskColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextDisabled);
  IconFontsImageList.UpdateIconsAttributes(LStyleFontColor, LStyleMaskColor,
    AReplaceCustomColors);
  {$ENDIF}
end;

procedure UpdateIconFontsColorByStyle(const IconFontsImageCollection: TIconFontsImageCollection;
  const AReplaceCustomColors: Boolean = False); overload;
{$IFDEF DXE+}
var
  LStyleFontColor, LStyleMaskColor: TColor;
{$ENDIF}
begin
  {$IFDEF DXE+}
  LStyleFontColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextNormal);
  LStyleMaskColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextDisabled);
  IconFontsImageCollection.UpdateIconsAttributes(LStyleFontColor, LStyleMaskColor,
    AReplaceCustomColors);
  {$ENDIF}
end;

procedure UpdateDisabledImageList(const ASourceImageList, ADestImageList: TIconFontsImageListBase;
  const APercent: Integer = 30; const AReplaceCustomColors: Boolean = False);
begin
  ADestImageList.Assign(ASourceImageList);
  ADestImageList.FontColor := DisabledColor(ADestImageList.FontColor, APercent);
end;

procedure UpdateHotImageList(const ASourceImageList, ADestImageList: TIconFontsImageListBase;
  const APercent: Integer = 30; const AResizePercent: Integer = 0;
  const AReplaceCustomColors: Boolean = False);
begin
  if ADestImageList.Count = 0 then
    ADestImageList.Assign(ASourceImageList);
  ADestImageList.FontColor := HotColor(ASourceImageList.FontColor, APercent);
  if AResizePercent <> 0 then
    ADestImageList.Size := Round(ASourceImageList.Size * (100+AResizePercent) / 100);
end;

function DarkerColor(AColor: TColor; APercent: Integer): TColor;
var
  r,g,b: Byte;
begin
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  r := r-muldiv(r,APercent,100);  //APercent% closer to black
  g := g-muldiv(g,APercent,100);
  b := b-muldiv(b,APercent,100);
  result := RGB(r, g, b);
end;

function LighterColor(AColor: TColor; APercent: Integer): TColor;
var
  r,g,b: Byte;
begin
  AColor := ColorToRGB(AColor);
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  r := r+muldiv(255-r,APercent,100); //APercent% closer to white
  g := g+muldiv(255-g,APercent,100);
  b := b+muldiv(255-b,APercent,100);
  result := RGB(r, g, b);
end;

function IsLightColor(const AColor: TColor): Boolean;
var
  r, g, b, yiq: integer;
begin
  r := GetRValue(AColor);
  g := GetGValue(AColor);
  b := GetBValue(AColor);
  yiq := ((r*299)+(g*587)+(b*114)) div 1000;
  if (yiq >= 128) then
    result := True
  else
    result := False;
end;

function DisabledColor(AColor: TColor; APercent: Integer): TColor;
begin
  if IsLightColor(AColor) then
    Result := DarkerColor(AColor, APercent)
  else
    Result := LighterColor(AColor, APercent);
end;

function HotColor(AColor: TColor; const APercent: Integer): TColor;
begin
  if IsLightColor(AColor) then
    Result := LighterColor(AColor, APercent)
  else
    Result := DarkerColor(AColor, APercent);
end;

// Converts any color to grayscale
function GrayscaleColor(AColor : TColor) : TColor;
var
  LGray : byte;
begin
  // get the luminance according to https://www.w3.org/TR/AERT/#color-contrast
  LGray  := round((0.299 * GetRValue(AColor)) + (0.587 * GetGValue(AColor)) + (0.114 * GetBValue(AColor)));

  // set the result to the new grayscale color including the alpha info
  Result := (AColor and $FF000000) or rgb(LGray, LGray, LGray);
end;

end.
