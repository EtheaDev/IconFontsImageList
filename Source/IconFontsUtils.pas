{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Contributors:                                                          }
{         Carlo Barazzetta                                                     }
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

implementation

uses
  SysUtils
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
var
  LStyleFontColor, LStyleMaskColor: TColor;
begin
  {$IFDEF DXE+}
  LStyleFontColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextNormal);
  LStyleMaskColor := TStyleManager.ActiveStyle.GetStyleFontColor(sfButtonTextDisabled);
  IconFontsImageList.UpdateIconsAttributes(LStyleFontColor, LStyleMaskColor,
    AReplaceCustomColors);
  {$ELSE}
  if LStyleName = 'Windows' then
    IconFontsImageList.UpdateIconsAttributes(clBlack, clBtnFace, '', False)
  else if LStyleName = 'Windows10' then
    IconFontsImageList.UpdateIconsAttributes(clBlack, clWhite)
  else if LStyleName = 'Windows10 SlateGray' then
    IconFontsImageList.UpdateIconsAttributes(clWhite, clBlack)
  else if LStyleName = 'Windows10 Blue' then
    IconFontsImageList.UpdateIconsAttributes(clBlue, clGray)
  else if LStyleName = 'Windows10 Dark' then
    IconFontsImageList.UpdateIconsAttributes(clSilver, clBlack)
  else if LStyleName = 'Windows10 Green' then
    IconFontsImageList.UpdateIconsAttributes(clOlive, clGreen)
  else if LStyleName = 'Windows10 Purple' then
    IconFontsImageList.UpdateIconsAttributes(clRed, clPurple);
  {$ENDIF}
end;

end.
