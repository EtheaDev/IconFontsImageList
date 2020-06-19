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
  , IconFontsImageList
  , Graphics
  , ComCtrls;

function UpdateIconFontListView(const AListView: TListView): Integer;
function UpdateIconFontListViewCaptions(const AListView: TListView;
  const AShowCaption: Boolean = True): Integer;
procedure UpdateIconFontsColorByStyle(const IconFontsImageList: TIconFontsImageList;
  const AReplaceCustomColors: Boolean = False);
procedure UpdateDisabledImageList(const ASourceImageList, ADestImageList: TIconFontsImageList;
  const APercent: Integer = 30; const AReplaceCustomColors: Boolean = False);
procedure UpdateHotImageList(const ASourceImageList, ADestImageList: TIconFontsImageList;
  const APercent: Integer = 30; const AResizePercent: Integer = 0;
  const AReplaceCustomColors: Boolean = False);
function DarkerColor(AColor: TColor; APercent: Integer): TColor;
function LighterColor(AColor: TColor; APercent: Integer): TColor;
function DisabledColor(AColor: TColor; APercent: Integer): TColor;
function IsLightColor(const AColor: TColor): Boolean;
function HotColor(AColor: TColor; const APercent: Integer): TColor;
function GrayscaleColor(AColor : TColor) : TColor;
function IsFontIconValidValue(const AFontIconDec: Integer): Boolean;

implementation

uses
  SysUtils
  , Windows
  , Themes
  ;

function IsFontIconValidValue(const AFontIconDec: Integer): Boolean;
begin
  Result := ((AFontIconDec >= $0000) and (AFontIconDec <= $D7FF)) or
    ((AFontIconDec >= $E000) and (AFontIconDec < $FFFF)) or  //D800 to DFFF are reserved for code point values for Surrogate Pairs
    ((AFontIconDec >= $010000) and (AFontIconDec <= $10FFFF)); //Surrogate Pairs
end;

function UpdateIconFontListView(const AListView: TListView): Integer;
var
  I: Integer;
  LItem: TIconFontItem;
  LListItem: TListItem;
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := AListView.LargeImages as TIconFontsImageList;
  AListView.Items.BeginUpdate;
  try
    AListView.Clear;
    Result := AListView.Items.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.IconFontItems[I];
      LListItem := AListView.Items.Add;
      LListItem.Caption := Format('$%s%s%s',
        [LItem.FontIconHex,sLineBreak,
         Litem.IconName]);
      LListItem.ImageIndex := I;
    end;
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
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := AListView.LargeImages as TIconFontsImageList;
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
           Litem.IconName]);
      end
      else
        LListItem.Caption := '';
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

procedure UpdateIconFontsColorByStyle(const IconFontsImageList: TIconFontsImageList;
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

procedure UpdateDisabledImageList(const ASourceImageList, ADestImageList: TIconFontsImageList;
  const APercent: Integer = 30; const AReplaceCustomColors: Boolean = False);
begin
  ADestImageList.Assign(ASourceImageList);
  ADestImageList.FontColor := DisabledColor(ADestImageList.FontColor, APercent);
end;

procedure UpdateHotImageList(const ASourceImageList, ADestImageList: TIconFontsImageList;
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
