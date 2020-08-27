{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageCollection: An extended ImageCollection for Delphi/VCL }
{       to use VirtualImageList with Icon Fonts                                }
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
unit IconFontsImageCollection;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Classes
  , Windows
  , Graphics
  , IconFontsImageListBase
  {$IFDEF D10_3+}
  , Vcl.BaseImageCollection
  {$ENDIF}
  , IconFontsItems;

type
  {$IFDEF D10_3+}
  TIconFontsImageCollection = class(TCustomImageCollection)
  {$ELSE}
  TIconFontsImageCollection = class(TComponent)
  {$ENDIF}
  private
    FIconFontItems: TIconFontItems;
    FFontName: TFontName;
    FUpdateOwnerAttributes: TGetOwnerAttributesProc;
    FMaskColor: TColor;
    FFontColor: TColor;
    FNotifyItemChanged: TIconFontItemChangedProc;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure SetIconFontItems(Value: TIconFontItems);
  protected
    //Events for notification from item to imagelist
    procedure CheckFontName(const AFontName: TFontName);
    procedure OnItemChanged(Sender: TIconFontItem);
    procedure GetOwnerAttributes(out AFontName: TFontName;
      out AFontColor, AMaskColor: TColor);
    {$IFDEF D10_3+}
    function GetCount: Integer; override;
    {$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$IFDEF D10_3+}
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; override;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); overload; override;
    {$ENDIF}
    property UpdateOwnerAttributes: TGetOwnerAttributesProc read FUpdateOwnerAttributes write FUpdateOwnerAttributes;
    property NotifyItemChanged: TIconFontItemChangedProc read FNotifyItemChanged write FNotifyItemChanged;
  published
    /// <summary>
    /// Collection of items with source images.
    /// </summary>
    property IconFontItems: TIconFontItems read FIconFontItems write SetIconFontItems;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TColor read FFontColor write SetFontColor default clNone;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
  end;

implementation

uses
  SysUtils
{$IFDEF GDI+}
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
{$ENDIF}
  , Math
  ;
{ TIconFontsImageCollection }

procedure TIconFontsImageCollection.CheckFontName(const AFontName: TFontName);
begin
  ;
end;

constructor TIconFontsImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FIconFontItems := TIconFontItems.Create(Self, TIconFontItem,
    OnItemChanged, CheckFontName, GetOwnerAttributes);
  FFontColor := clDefault;
  FMaskColor := clDefault;
end;

destructor TIconFontsImageCollection.Destroy;
begin
  FreeAndNil(FIconFontItems);
  inherited;
end;

procedure TIconFontsImageCollection.SetFontColor(const AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    OnItemChanged(nil);
  end;
end;

procedure TIconFontsImageCollection.SetFontName(const AValue: TFontName);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    OnItemChanged(nil);
  end;
end;

procedure TIconFontsImageCollection.SetMaskColor(const AValue: TColor);
begin
  if FMaskColor <> AValue then
  begin
    FMaskColor := AValue;
    OnItemChanged(nil);
  end;
end;

procedure TIconFontsImageCollection.SetIconFontItems(Value: TIconFontItems);
begin
  if FIconFontItems <> Value then
  begin
    FIconFontItems := Value;
    OnItemChanged(nil);
  end;
end;

{$IFDEF D10_3+}
function TIconFontsImageCollection.GetCount: Integer;
begin
  if Assigned(FIconFontItems) then
    Result := FIconFontItems.Count
  else
    Result := 0;
end;

function TIconFontsImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := FIconFontItems[AIndex].IconName;
end;

function TIconFontsImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
  S: String;
begin
  Result := -1;
  S := LowerCase(AName);
  for I := 0 to FIconFontItems.Count - 1 do
    if LowerCase(FIconFontItems[I].IconName) = S then
      Exit(I);
end;

function TIconFontsImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count);
end;

function TIconFontsImageCollection.GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap;
var
  LIconFontItem: TIconFontItem;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex > Count-1) then
    Exit;
  LIconFontItem := FIconFontItems.Items[AIndex];
  Result := LIconFontItem.GetBitmap(AWidth, AHeight, True);
end;

procedure TIconFontsImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
  AProportional: Boolean = False);
var
  LIconFontItem: TIconFontItem;
  {$IFNDEF GDI+}
  LMaskColor: TColor;
  {$ENDIF}
begin
  if (AIndex < 0) or (AIndex > Count-1) then
    Exit;
  LIconFontItem := FIconFontItems.Items[AIndex];
  {$IFDEF GDI+}
  LIconFontItem.PaintTo(ACanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height);
  {$ELSE}
  LIconFontItem.PaintTo(ACanvas, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
    LMaskColor);
  {$ENDIF}
end;
{$ENDIF}

procedure TIconFontsImageCollection.Loaded;
begin
  inherited;
  if Assigned(FIconFontItems) then
    OnItemChanged(nil);
end;

procedure TIconFontsImageCollection.GetOwnerAttributes(out AFontName: TFontName;
  out AFontColor, AMaskColor: TColor);
var
  LFontName: TFontName;
  LFontColor, LMaskColor: TColor;
begin
  if Assigned(FUpdateOwnerAttributes) then
  begin
    FUpdateOwnerAttributes(LFontName, LFontColor, LMaskColor);

    if FFontName = '' then
      AFontName := LFontName
    else
      AFontName := FFontName;

    if FFontColor = clDefault then
      AFontColor := LFontColor
    else
      AFontColor := FFontColor;

    if FMaskColor = clDefault then
      AMaskColor := LMaskColor
    else
      AMaskColor := LMaskColor;
  end
  else
  begin
    LFontName := '';
    LFontColor := clDefault;
    LMaskColor := clDefault;
  end;
end;

procedure TIconFontsImageCollection.OnItemChanged(Sender: TIconFontItem);
begin
  if Assigned(NotifyItemChanged) then
    NotifyItemChanged(Sender);
end;

procedure TIconFontsImageCollection.Assign(Source: TPersistent);
begin
  if Source is TIconFontsImageCollection then
    TIconFontsImageCollection(Source).FIconFontItems.Assign(FIconFontItems)
  else
    inherited;
end;

end.
