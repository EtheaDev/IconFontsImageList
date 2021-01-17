{******************************************************************************}
{                                                                              }
{       Icon Font Image fmx: An extended Image for Delphi/FireMonkey           }
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

const
  DEFAULT_SIZE = 32;
  ZOOM_DEFAULT = 100;

type
  TIconFontMissing = procedure (const AFontName: TFontName) of object;

  TIconFontFixedMultiResBitmap = class;

  TIconFontFixedBitmapItem = class(TFixedBitmapItem)
  private
    FWidth, FHeight, FZoom: Single;
    FFontName: TFontName;
    FFontIconDec: Integer;
    FFontColor: TAlphaColor;
    FOpacity: Single;
    FOwnerCollection: TIconFontFixedMultiResBitmap;
    FIconName: string;
    function StoreOpacity: Boolean;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetFontName(const AValue: TFontName);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetIconSize(const AWidth, AHeight:Single;
      const AZoom: Integer);
    procedure DrawFontIcon;
    procedure SetOpacity(const AValue: Single);
    procedure SetIconName(const AValue: string);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    function GetCharacter: String;
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property FontName: TFontName read FFontName write SetFontName;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property Character: String read GetCharacter stored false;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: Single read FOpacity write SetOpacity stored StoreOpacity;
    property IconName: string read FIconName write SetIconName;
  end;

  TIconFontFixedBitmapItemClass = class of TIconFontFixedBitmapItem;
  TIconFontImage = class;

  TIconFontFixedMultiResBitmap = class(TFixedMultiResBitmap)
  private
    FOwnerImage: TIconFontImage;
    procedure OnDrawImage(Sender: TObject);
    procedure UpdateImageSize(const AWidth, AHeight: Single;
      const AZoom: Integer);
  public
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontFixedBitmapItemClass); overload;
    constructor Create(AOwner: TPersistent); overload;
  end;

  TIconFontImage = class(TImage)
  private
    FZoom: Integer;
    FIconFontMultiResBitmap: TIconFontFixedMultiResBitmap;
    procedure SetBitmapZoom(const AValue: Integer);
    procedure SetIconSize(const AWidth, AHeight: Single;
      const AZoom: Integer);
  protected
    function CreateMultiResBitmap: TFixedMultiResBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
  published
    property BitmapZoom: Integer read FZoom write SetBitmapZoom default ZOOM_DEFAULT;
  end;

implementation

uses
  System.Math
  , FMX.IconFontsImageList
  , System.RTLConsts
  , System.SysUtils
  , System.Character
  , FMX.Forms
  , FMX.Consts;

{ TIconFontFixedMultiResBitmap }

constructor TIconFontFixedMultiResBitmap.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TIconFontFixedBitmapItem);
  if AOwner is TIconFontImage then
    FOwnerImage := TIconFontImage(AOwner);
end;

procedure TIconFontFixedMultiResBitmap.OnDrawImage(Sender: TObject);
begin
  if Assigned(FOwnerImage) then
    FOwnerImage.Repaint;
end;

constructor TIconFontFixedMultiResBitmap.Create(AOwner: TPersistent; ItemClass: TIconFontFixedBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if AOwner is TIconFontImage then
    FOwnerImage := TIconFontImage(AOwner);
end;

procedure TIconFontFixedMultiResBitmap.UpdateImageSize(const AWidth, AHeight: Single;
  const AZoom: Integer);
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
        TIconFontFixedBitmapItem(LItem).SetIconSize(AWidth, AHeight, AZoom);
    end;
  end;
end;

{ TIconFontFixedBitmapItem }

procedure TIconFontFixedBitmapItem.Assign(Source: TPersistent);
begin
  if Source is TIconFontsSourceItem then
  begin
    FontName := TIconFontsSourceItem(Source).FontName;
    IconName := TIconFontsSourceItem(Source).IconName;
    if TIconFontsSourceItem(Source).FontColor <> TAlphaColors.Null then
      FontColor := TIconFontsSourceItem(Source).FontColor;
    FontIconDec := TIconFontsSourceItem(Source).FontIconDec;
  end
  else
    inherited;
end;

function TIconFontFixedBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TIconFontFixedBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TIconFontFixedMultiResBitmap then
    FOwnerCollection := Collection as TIconFontFixedMultiResBitmap;
  FZoom := ZOOM_DEFAULT;
  FOpacity := 1;
end;

procedure TIconFontFixedBitmapItem.DrawFontIcon;
var
  LFont: TFont;
  LBitmap: TBitmap;
  LBitmapWidth, LBitmapHeight: Integer;
  LRect: TRectF;
begin
  if (FWidth <= 0) or (FHeight <= 0) or (FZoom <= 0) or (FZoom >= 100) then
    Exit;
  LBitmap := inherited Bitmap;
  LBitmapWidth := Round(FWidth * Scale);
  LBitmapHeight := Round(FHeight * Scale);
  LFont := TFont.Create;
  try
    LFont.Family := FontName;
    LFont.Size := Min(FWidth, FHeight) * FZoom / 100;
    LFont.Size := LFont.Size * FZoom / 100;
    LBitmap.Width  := LBitmapWidth;
    LBitmap.Height := LBitmapHeight;
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);
      LBitmap.Canvas.Fill.Color := FontColor;
      LBitmap.Canvas.Font.Assign(LFont);
      LRect.Create(0,0,FWidth,FHeight);
      LBitmap.Canvas.FillText(LRect,
        Character, False, Opacity,
        [TFillTextFlag.RightToLeft],
        TTextAlign.Center, TTextAlign.Center);
    finally
      LBitmap.Canvas.EndScene;
    end;
    if Assigned(FOwnerCollection) then
      FOwnerCollection.OnDrawImage(Self);
  finally
    LFont.Free;
  end;
end;

function TIconFontFixedBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawFontIcon;
  Result := inherited Bitmap;
end;

function TIconFontFixedBitmapItem.GetCharacter: String;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  Result := ConvertFromUtf32(FFontIconDec);
end;

function TIconFontFixedBitmapItem.GetDisplayName: string;
begin
  Result := FIconName;
end;

function TIconFontFixedBitmapItem.GetFontIconDec: Integer;
begin
  Result := FFontIconDec;
end;

function TIconFontFixedBitmapItem.GetFontIconHex: string;
begin
  if FFontIconDec <> 0 then
    Result := IntToHex(FFontIconDec, 1)
  else
    Result := '';
end;

procedure TIconFontFixedBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TIconFontFixedBitmapItem.SetFontColor(const AValue: TAlphaColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontFixedBitmapItem.SetFontIconDec(const AValue: Integer);
begin
  if AValue <> FFontIconDec then
  begin
    FFontIconDec := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontFixedBitmapItem.SetFontIconHex(const AValue: string);
begin
  try
    if (Length(AValue) = 4) or (Length(AValue) = 5) then
      FontIconDec := StrToInt('$' + AValue)
    else if (Length(AValue) = 0) then
      FFontIconDec := 0
    else
      raise Exception.CreateFmt(ERR_ICONFONTSFMX_VALUE_NOT_ACCEPTED,[AValue]);
  except
    On E: EConvertError do
      raise Exception.CreateFmt(ERR_ICONFONTSFMX_VALUE_NOT_ACCEPTED,[AValue])
    else
      raise;
  end;
end;

procedure TIconFontFixedBitmapItem.SetFontName(const AValue: TFontName);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontFixedBitmapItem.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;

procedure TIconFontFixedBitmapItem.SetIconSize(const AWidth, AHeight:Single;
  const AZoom: Integer);
begin
  if (AWidth <> 0) and (AHeight <> 0) and
    ((AWidth <> FWidth) or (AHeight <> FHeight) or (AZoom <> FZoom)) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    FZoom := AZoom;
    DrawFontIcon;
  end;
end;

procedure TIconFontFixedBitmapItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  DrawFontIcon;
end;

function TIconFontFixedBitmapItem.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

{ TIconFontImage }

constructor TIconFontImage.Create(AOwner: TComponent);
begin
  inherited;
  DisableInterpolation := True;
  FIconFontMultiResBitmap := MultiResBitmap as TIconFontFixedMultiResBitmap;
  FZoom := ZOOM_DEFAULT;
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

procedure TIconFontImage.SetIconSize(const AWidth, AHeight: Single;
  const AZoom: Integer);
begin
  inherited Width := AWidth;
  inherited height := AHeight;
  FZoom := AZoom;
  FIconFontMultiResBitmap.UpdateImageSize(AWidth, AHeight, AZoom);
end;

procedure TIconFontImage.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  SetIconSize(AWidth, AHeight, FZoom);
end;

procedure TIconFontImage.SetBitmapZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
    SetIconSize(Width, Height, AValue);
end;

initialization
  RegisterFmxClasses([TIconFontImage]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontImage.TIconFontImage, TFmxObject);

end.
