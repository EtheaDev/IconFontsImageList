{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/VCL             }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia                                                     }
{         Luca Minuti                                                          }
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
unit IconFontsImageList;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Classes
  , ImgList
  , Windows
  , Graphics
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
  , IconFontsImageListBase
  , IconFontsItems;

type
  TIconFontsItem = IconFontsItems.TIconFontItem;
  TIconFontsItems = IconFontsItems.TIconFontItems;

  {TIconFontsImageList}
  TIconFontsImageList = class(TIconFontsImageListBase)
  private
    FIconFontItems: TIconFontItems;
  protected
    function GetIconFontItems: TIconFontItems; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property IconFontItems;
  end;


implementation

uses
  SysUtils
  , IconFontsUtils
  , Math
  , ComCtrls
  {$IFDEF DXE3+}
  , System.Character
  , Themes
  {$ENDIF}
  {$IFDEF GDI+}
  , Winapi.CommCtrl
  {$ENDIF}
  , StrUtils
  ;

{ TIconFontsImageList }

constructor TIconFontsImageList.Create(AOwner: TComponent);
begin
  inherited;
  FIconFontItems := TIconFontItems.Create(Self, TIconFontItem, OnItemChanged,
    CheckFontName, GetOwnerAttributes);
end;

destructor TIconFontsImageList.Destroy;
begin
  FreeAndNil(FIconFontItems);
  inherited;
end;

function TIconFontsImageList.GetIconFontItems: TIconFontItems;
begin
  Result := FIconFontItems;
end;

end.
