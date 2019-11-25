{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019 (Ethea S.r.l.)                                      }
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

uses
  Classes
  , ImgList
  , Graphics
  , ComCtrls;

function UpdateIconFontListView(const AListView: TListView): Integer;

implementation

uses
  SysUtils
  , IconFontsImageList;

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

end.
