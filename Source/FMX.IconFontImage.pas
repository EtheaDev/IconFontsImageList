{******************************************************************************}
{                                                                              }
{       Icon Font Image fmx: An extended Image for Delphi/FireMonkey           }
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
unit FMX.IconFontImage;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  System.Classes
  , System.UITypes
  , System.Rtti
  , System.Messaging
  , System.ImageList
  , System.Types
  , FMX.Controls
  , FMX.ImgList
  , FMX.MultiResBitmap
  , FMX.Types
  , FMX.Graphics
  , FMX.Objects
  ;

type
  TIconFontMissing = procedure (const AFontName: string) of object;

  TIconFontFixedMultiResBitmap = class;

  TIconFontFixedBitmapItem = class(TFixedBitmapItem)
  private
    FSize: Single;
    FFontName: string;
    FCharacter: WideChar;
    FFontColor: TAlphaColor;
    FOpacity: Single;
    FOwnerCollection: TIconFontFixedMultiResBitmap;
    FIconName: string;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetCharacter(const AValue: WideChar);
    procedure SetFontName(const AValue: string);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetSize(const AValue: Single);
    procedure UpdateBitmap;
    procedure SetOpacity(const AValue: Single);
    procedure SetIconName(const AValue: string);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    function GetCharacter: WideChar;
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property FontName: string read FFontName write SetFontName;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property Character: WideChar read GetCharacter write SetCharacter stored false default #0;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: Single read FOpacity write SetOpacity;
    property Size: Single read FSize write SetSize;
    property IconName: string read FIconName write SetIconName;
  end;

  TIconFontFixedBitmapItemClass = class of TIconFontFixedBitmapItem;
  TIconFontImage = class;

  TIconFontFixedMultiResBitmap = class(TFixedMultiResBitmap)
  private
    FOwnerImage: TIconFontImage;
    procedure UpdateImageSize(const ASize: Single);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontFixedBitmapItemClass); overload;
    constructor Create(AOwner: TPersistent); overload;
  end;

  TIconFontImage = class(TImage)
  private
    FIconFontMultiResBitmap: TIconFontFixedMultiResBitmap;
    function GetBitmapSize: Single;
    procedure SetBitmapSize(const AValue: Single);
  protected
    function CreateMultiResBitmap: TFixedMultiResBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  published
    property BitmapSize: Single read GetBitmapSize write SetBitmapSize;
  end;

implementation

uses
  System.Math
  , System.RTLConsts
  , System.SysUtils
  , FMX.Forms
  , FMX.Consts;

{ TIconFontFixedMultiResBitmap }

constructor TIconFontFixedMultiResBitmap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TIconFontFixedBitmapItem);
  if AOwner is TIconFontImage then
    FOwnerImage := TIconFontImage(AOwner);
end;

constructor TIconFontFixedMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TIconFontFixedBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if AOwner is TIconFontImage then
    FOwnerImage := TIconFontImage(AOwner);
end;

procedure TIconFontFixedMultiResBitmap.UpdateImageSize(const ASize: Single);
var
  I, J: Integer;
  LItem: TFixedBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J];
      if LItem is TIconFontFixedBitmapItem then
        TIconFontFixedBitmapItem(LItem).Size := ASize;
    end;
  end;
end;

{ TIconFontFixedBitmapItem }

function TIconFontFixedBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TIconFontFixedBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TIconFontFixedMultiResBitmap then
    FOwnerCollection := Collection as TIconFontFixedMultiResBitmap;
  FSize := 16;
  FOpacity := 1;
end;

procedure TIconFontFixedBitmapItem.UpdateBitmap;
var
  LFont: TFont;
  LBitmap: TBitmap;
  LBitmapSize: Single;
  LRect: TRectF;
begin
  LBitmap := inherited Bitmap;
  LBitmapSize := Size * Scale;
  LFont := TFont.Create;
  try
    LFont.Family := FFontName;
    LFont.Size := Size;
    LBitmap.Width  := Trunc(LBitmapSize);
    LBitmap.Height := Trunc(LBitmapSize);
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);
      LBitmap.Canvas.Fill.Color := FFontColor;
      LBitmap.Canvas.Font.Assign(LFont);
      LRect.Create(0,0,Size,Size);
      LBitmap.Canvas.FillText(LRect,
        FCharacter, False, FOpacity,
        [TFillTextFlag.RightToLeft],
        TTextAlign.Leading, TTextAlign.Center);
    finally
      LBitmap.Canvas.EndScene;
    end;
  finally
   LFont.Free;
  end;
end;

function TIconFontFixedBitmapItem.GetBitmap: TBitmapOfItem;
begin
  UpdateBitmap;
  Result := inherited Bitmap;
end;

function TIconFontFixedBitmapItem.GetCharacter: WideChar;
begin
  Result := FCharacter;
end;

function TIconFontFixedBitmapItem.GetDisplayName: string;
begin
  Result := FIconName;
end;

function TIconFontFixedBitmapItem.GetFontIconDec: Integer;
begin
  Result := ord(FCharacter);
end;

function TIconFontFixedBitmapItem.GetFontIconHex: string;
begin
  if FCharacter <> #0 then
    Result := IntToHex(Ord(FCharacter), 4)
  else
    Result := '';
end;

procedure TIconFontFixedBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TIconFontFixedBitmapItem.SetCharacter(const AValue: WideChar);
begin
  if AValue <> FCharacter then
  begin
    FCharacter := AValue;
    UpdateBitmap;
  end;
end;

procedure TIconFontFixedBitmapItem.SetFontColor(const AValue: TAlphaColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    UpdateBitmap;
  end;
end;

procedure TIconFontFixedBitmapItem.SetFontIconDec(const AValue: Integer);
begin
  Character := WideChar(AValue);
end;

procedure TIconFontFixedBitmapItem.SetFontIconHex(const AValue: string);
begin
  if (Length(AValue) = 4) then
    Character := WideChar(StrToInt('$' + AValue))
  else if (Length(AValue) = 0) then
    Character := #0
  else
    raise Exception.CreateFmt('Value %s not accepted!',[AValue]);
end;

procedure TIconFontFixedBitmapItem.SetFontName(const AValue: string);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    UpdateBitmap;
  end;
end;

procedure TIconFontFixedBitmapItem.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;

procedure TIconFontFixedBitmapItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  UpdateBitmap;
end;

procedure TIconFontFixedBitmapItem.SetSize(const AValue: Single);
begin
  if (Trunc(AValue) > 0) and (AValue <> FSize) then
  begin
    FSize := AValue;
    UpdateBitmap;
  end;
end;

{ TIconFontImage }

constructor TIconFontImage.Create(AOwner: TComponent);
begin
  inherited;
  DisableInterpolation := True;
  FIconFontMultiResBitmap := MultiResBitmap as TIconFontFixedMultiResBitmap;
end;

function TIconFontImage.CreateMultiResBitmap: TFixedMultiResBitmap;
begin
  Result := TIconFontFixedMultiResBitmap.Create(Self, TIconFontFixedBitmapItem);
end;

destructor TIconFontImage.Destroy;
begin
  inherited;
  FIconFontMultiResBitmap := nil;
end;

function TIconFontImage.GetBitmapSize: Single;
begin
  Result := Trunc(Inherited width);
end;

procedure TIconFontImage.SetBitmapSize(const AValue: Single);
begin
  if AValue <> 0 then
    FIconFontMultiResBitmap.UpdateImageSize(AValue);
end;

procedure TIconFontImage.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  BitmapSize := Trunc(Min(AWidth, AHeight));
end;

initialization
  RegisterFmxClasses([TIconFontImage]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontImage.TIconFontImage, TFmxObject);

end.
