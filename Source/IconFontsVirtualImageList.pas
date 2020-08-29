{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/VCL             }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
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
unit IconFontsVirtualImageList;

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
  , IconFontsItems
  , IconFontsImageCollection;

type
  {TIconFontsVirtualImageList}
  TIconFontsVirtualImageList = class(TIconFontsImageListBase)
  private
    FImageCollection: TIconFontsImageCollection;
    function GetImageCollection: TIconFontsImageCollection;
    procedure SetImageCollection(const Value: TIconFontsImageCollection);
  protected
    function GetIconFontItems: TIconFontItems; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ImageCollection: TIconFontsImageCollection read GetImageCollection write SetImageCollection;
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

{ TIconFontsVirtualImageList }

constructor TIconFontsVirtualImageList.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TIconFontsVirtualImageList.Destroy;
begin
  inherited;
end;

function TIconFontsVirtualImageList.GetIconFontItems: TIconFontItems;
begin
  if Assigned(FImageCollection) then
    Result := FImageCollection.IconFontItems
  else
    Result := nil;
end;

function TIconFontsVirtualImageList.GetImageCollection: TIconFontsImageCollection;
begin
  Result := FImageCollection;
end;

procedure TIconFontsVirtualImageList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImageCollection) then
  begin
    FImageCollection := nil;
    RecreateBitmaps;
  end;
end;

procedure TIconFontsVirtualImageList.SetImageCollection(
  const Value: TIconFontsImageCollection);
begin
  if FImageCollection <> Value then
  begin
    FImageCollection := Value;
    if Assigned(FImageCollection) then
    begin
      FImageCollection.UpdateOwnerAttributes := GetOwnerAttributes;
      FImageCollection.NotifyItemChanged := OnItemChanged;
      FImageCollection.OnFontMissing := OnFontMissing;
    end;
    RecreateBitmaps;
  end;
end;

end.
