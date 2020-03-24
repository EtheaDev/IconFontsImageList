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
function UpdateIconFontListViewCaptions(const AListView: TListView): Integer;
procedure UpdateIconFontsColorByStyle(const IconFontsImageList: TIconFontsImageList;
    const AReplaceCustomColors: Boolean = False);
function DarkerColor(AColor: TColor; APercent: Integer): TColor;
function LighterColor(AColor: TColor; APercent: Integer): TColor;
function DisabledColor(AColor: TColor; APercent: Integer): TColor;
function IsLightColor(const AColor: TColor): Boolean;
function HotColor(AColor: TColor; APercent: Integer): TColor;

implementation

uses
  SysUtils
  , Windows
  , Themes
  ;

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
    Result := LIconFontsImageList.IconFontItems.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.IconFontItems[I];
      LListItem := AListView.Items.Add;
      LListItem.Caption := Format('%d%s$%s%s%s',
        [LItem.FontIconDec,sLineBreak,
         LItem.FontIconHex,sLineBreak,
         Litem.IconName]);
      LListItem.ImageIndex := I;
    end;
  finally
    AListView.Items.EndUpdate;
  end;
end;

function UpdateIconFontListViewCaptions(const AListView: TListView): Integer;
var
  I: Integer;
  LItem: TIconFontItem;
  LListItem: TListItem;
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := AListView.LargeImages as TIconFontsImageList;
  AListView.Items.BeginUpdate;
  try
    Result := LIconFontsImageList.IconFontItems.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.IconFontItems[I];
      LListItem := AListView.Items[I];
      LListItem.Caption := Format('%d%s$%s%s%s',
        [LItem.FontIconDec,sLineBreak,
         LItem.FontIconHex,sLineBreak,
         Litem.IconName]);
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

function HotColor(AColor: TColor; APercent: Integer): TColor;
begin
  if IsLightColor(AColor) then
    Result := LighterColor(AColor, APercent)
  else
    Result := DarkerColor(AColor, APercent);
end;

end.
