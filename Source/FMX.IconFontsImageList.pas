{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList fmx: An extended ImageList for Delphi/FireMonkey  }
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
unit FMX.IconFontsImageList;

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

resourcestring
  ERR_ICONFONTSFMX_VALUE_NOT_ACCEPTED = 'Value %s not accepted!';
  ERR_ICONFONTSFMX_FONT_NOT_INSTALLED = 'Font "%s" is not installed!';

type
  TIconFontMissing = procedure (const AFontName: string) of object;

  TIconFontMultiResBitmap = class;
  TIconFontsImageList = class;
  TIconFontsSourceItem = class;

  TIconFontBitmapItem = class(TCustomBitmapItem)
  private
    FSize: Single;
    FOwnerMultiResBitmap: TIconFontMultiResBitmap;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetSize(const AValue: Single);
    procedure DrawFontIcon;
    function GetCharacter: WideString;
    function GetFontName: string;
    function GetFontColor: TAlphaColor;
    function GetOpacity: Single;
    function GetSize: Single;
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Scale;
    property Size: Single read GetSize write SetSize;
    //Readonly properties from Source Item
    property FontName: string read GetFontName;
    property Character: WideString read GetCharacter stored false;
    property FontColor: TAlphaColor read GetFontColor stored false;
    property Opacity: Single read GetOpacity  stored false;
  end;

  TIconFontBitmapItemClass = class of TIconFontBitmapItem;

  TIconFontMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TIconFontsSourceItem;
    procedure UpdateImageSize(const ASize: Single);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontBitmapItemClass); overload;
  public
  end;

  TIconFontsSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TIconFontsImageList;
    FFontIconDec: Integer;
    FOpacity: Single;
    FFontName: string;
    FFontColor: TAlphaColor;
    procedure UpdateAllItems;
    function GetCharacter: WideString;
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontName(const AValue: string);
    procedure SetOpacity(const AValue: Single);
    procedure AutoSizeBitmap(const ASize: Single);
    function GetIconName: string;
    procedure SetIconName(const Value: string);
    function GetFontName: string;
    function GetFontColor: TAlphaColor;
    function GetOpacity: Single;
  protected
    function GetDisplayName: string; override;
    function CreateMultiResBitmap: TMultiResBitmap; override;
    function StoreFontName: Boolean; virtual;
    function StoreFontColor: Boolean; virtual;
    function StoreOpacity: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
  published
    property MultiResBitmap;
    property IconName: string read GetIconName write SetIconName;
    property FontName: string read GetFontName write SetFontName stored StoreFontName;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property Character: WideString read GetCharacter;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor stored StoreFontColor;
    property Opacity: Single read GetOpacity write SetOpacity stored StoreOpacity;
  end;

  TIconFontsImageList = class(TCustomImageList)
  private
    FAutoSizeBitmaps: Boolean;
    FFontName: string;
    FFontColor: TAlphaColor;
    FOpacity: Single;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
    procedure SetFontName(const Value: string);
    procedure UpdateSourceItems;
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
  protected
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Source;
    property Destination;
    property OnChange;
    property OnChanged;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default True;
    property FontName: string read FFontName write SetFontName;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: Single read FOpacity write SetOpacity;
  end;

implementation

uses
  System.Math
  , System.RTLConsts
  , System.SysUtils
  , System.Character
  , FMX.Forms
  , FMX.Consts;

{ TIconFontBitmapItem }

function TIconFontBitmapItem.BitmapStored: Boolean;
begin
  Result := False;
end;

constructor TIconFontBitmapItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TIconFontMultiResBitmap then
    FOwnerMultiResBitmap := Collection as TIconFontMultiResBitmap;
  FSize := 16;
end;

procedure TIconFontBitmapItem.DrawFontIcon;
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
    LFont.Family := FontName;
    LFont.Size := Size;
    LBitmap.Width  := Trunc(LBitmapSize);
    LBitmap.Height := Trunc(LBitmapSize);
    LBitmap.Canvas.BeginScene;
    try
      LBitmap.Canvas.Clear(TAlphaColors.Null);
      LBitmap.Canvas.Fill.Color := FontColor;
      LBitmap.Canvas.Font.Assign(LFont);
      LRect.Create(0,0,Size,Size);
      LBitmap.Canvas.FillText(LRect,
        Character, False, Opacity,
        [TFillTextFlag.RightToLeft],
        TTextAlign.Leading, TTextAlign.Center);
    finally
      LBitmap.Canvas.EndScene;
    end;
  finally
   LFont.Free;
  end;
end;

function TIconFontBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawFontIcon;
  Result := inherited Bitmap;
end;

function TIconFontBitmapItem.GetCharacter: WideString;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Character;
end;

function TIconFontBitmapItem.GetDisplayName: string;
begin
  Result := Format('%s - %dx%d - Scale: %s', [FOwnerMultiResBitmap.FOwnerSourceItem.Name,
    Trunc(Size),Trunc(Size), FloatToStr(Scale)]);
end;

function TIconFontBitmapItem.GetFontColor: TAlphaColor;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontColor;
end;

function TIconFontBitmapItem.GetFontName: string;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontName;
end;

function TIconFontBitmapItem.GetOpacity: Single;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Opacity;
end;

function TIconFontBitmapItem.GetSize: Single;
begin
  Result := FSize;
end;

procedure TIconFontBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;


procedure TIconFontBitmapItem.SetSize(const AValue: Single);
begin
  if (Trunc(AValue) > 0) and (AValue <> FSize) then
  begin
    FSize := AValue;
    DrawFontIcon;
  end;
end;

{ TIconFontMultiResBitmap }

constructor TIconFontMultiResBitmap.Create(AOwner: TPersistent;
  ItemClass: TIconFontBitmapItemClass);
begin
  inherited Create(AOwner, ItemClass);
  if (AOwner is TIconFontsSourceItem) then
    FOwnerSourceItem := TIconFontsSourceItem(AOwner)
  else
    FOwnerSourceItem := nil;
end;

procedure TIconFontMultiResBitmap.UpdateImageSize(const ASize: Single);
var
  I, J: Integer;
  LItem: TCustomBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J];
      if LItem is TIconFontBitmapItem then
        TIconFontBitmapItem(LItem).Size := ASize;
    end;
  end;
end;

{ TIconFontsSourceItem }

procedure TIconFontsSourceItem.AutoSizeBitmap(const ASize: Single);
begin
  //If present, delete multiple items
  while MultiResBitmap.Count > 0 do
    MultiResBitmap.Delete(MultiResBitmap.Count-1);
  //Add only one item
  if MultiResBitmap.Count = 0 then
    MultiResBitmap.Add;
  (MultiResBitmap as TIconFontMultiResBitmap).UpdateImageSize(ASize);
end;

constructor TIconFontsSourceItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOpacity := 0;
  FFontName := '';
  FFontIconDec := 0;
  FFontColor := TAlphaColors.null;
end;

function TIconFontsSourceItem.CreateMultiResBitmap: TMultiResBitmap;
begin
  Result := TIconFontMultiResBitmap.Create(self, TIconFontBitmapItem);
  FOwnerImageList := Result.ImageList as TIconFontsImageList;
end;

function TIconFontsSourceItem.GetCharacter: WideString;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  Result := ConvertFromUtf32(FFontIconDec);
end;

function TIconFontsSourceItem.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Format('%s - Hex: %s - (%s)',
      [FontName, FontIconHex, Name])
  else
    Result := Format('%s - Hex: %s',
      [FontName, FontIconHex]);
end;

function TIconFontsSourceItem.GetFontColor: TAlphaColor;
begin
  if FFontColor = TAlphaColors.Null then
    Result := FOwnerImageList.FFontColor
  else
    Result := FFontColor;
end;

function TIconFontsSourceItem.GetFontIconDec: Integer;
begin
  Result := FFontIconDec;
end;

function TIconFontsSourceItem.GetFontIconHex: string;
begin
  if FFontIconDec <> 0 then
    Result := IntToHex(FFontIconDec, 1)
  else
    Result := '';
end;

function TIconFontsSourceItem.GetFontName: string;
begin
  if FFontName = '' then
    Result := FOwnerImageList.FFontName
  else
    Result := FFontName;
end;

function TIconFontsSourceItem.GetIconName: string;
begin
  Result := inherited Name;
end;

function TIconFontsSourceItem.GetOpacity: Single;
begin
  if FOpacity = 0 then
    Result := FOwnerImageList.FOpacity
  else
    Result := FOpacity;
end;

procedure TIconFontsSourceItem.SetFontColor(const AValue: TAlphaColor);
begin
  FFontColor := AValue;
  UpdateAllItems;
end;

procedure TIconFontsSourceItem.SetFontIconDec(const AValue: Integer);
begin
  if AValue <> FFontIconDec then
  begin
    FFontIconDec := AValue;
    UpdateAllItems;
  end;
end;

procedure TIconFontsSourceItem.SetFontIconHex(const AValue: string);
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

procedure TIconFontsSourceItem.SetFontName(const AValue: string);
begin
  if (FontName <> AValue) then
  begin
    if (AValue = FOwnerImageList.FontName) then
      FFontName := AValue
    else
    begin
      FFontName := AValue;
      UpdateAllItems;
    end;
  end;
end;

procedure TIconFontsSourceItem.SetIconName(const Value: string);
begin
  inherited Name := Value;
end;

procedure TIconFontsSourceItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  UpdateAllItems;
end;

function TIconFontsSourceItem.StoreFontColor: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FFontColor <> FOwnerImageList.FFontColor);
end;

function TIconFontsSourceItem.StoreFontName: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FFontName <> FOwnerImageList.FFontName);
end;

function TIconFontsSourceItem.StoreOpacity: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FOpacity <> FOwnerImageList.FOpacity);
end;

procedure TIconFontsSourceItem.UpdateAllItems;
var
  I: Integer;
  LItem: TIconFontBitmapItem;
begin
  for I := 0 to MultiResBitmap.Count -1 do
  begin
    LItem := MultiResBitmap.Items[I] as TIconFontBitmapItem;
    Litem.DrawFontIcon;
  end;
end;

{ TIconFontsImageList }

constructor TIconFontsImageList.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizeBitmaps := True;
  FFontColor := TAlphaColors.Black;
  FOpacity := 1;
end;

function TIconFontsImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TIconFontsSourceItem);
end;

function TIconFontsImageList.DoBitmap(Size: TSize;
  const Index: Integer): TBitmap;
var
  LDestItem: TDestinationItem;
  LSourceItem: TIconFontsSourceItem;
  LIndex: Integer;
  LSize: Single;
begin
  if FAutoSizeBitmaps then
  begin
    LDestItem := Destination.Items[Index] as TDestinationItem;
    if LDestItem.Layers.Count > 0 then
    begin
      LIndex := Source.indexOf(LDestItem.Layers[0].Name);
      if LIndex >= 0 then
      begin
        LSourceItem := Source.Items[LIndex] as TIconFontsSourceItem;
        if Assigned(LSourceItem) then
        begin
          LSize := Min(Size.cx, Size.cy);
          LDestItem.Layers[0].SourceRect.Top := 0;
          LDestItem.Layers[0].SourceRect.Left := 0;
          LDestItem.Layers[0].SourceRect.Right := LSize;
          LDestItem.Layers[0].SourceRect.Bottom := LSize;
          LSourceItem.AutoSizeBitmap(LSize);
        end;
      end;
    end;
  end;
  Result := inherited DoBitmap(Size, Index);
end;

procedure TIconFontsImageList.SetAutoSizeBitmaps(const Value: Boolean);
begin
  FAutoSizeBitmaps := Value;
end;

procedure TIconFontsImageList.UpdateSourceItems;
var
  I: Integer;
  LSourceItem: TIconFontsSourceItem;
begin
  for I := 0 to Source.Count -1 do
  begin
    LSourceItem := Source[I] as TIconFontsSourceItem;
    LSourceItem.FontName := FFontName;
    LSourceItem.FontColor := FFontColor;
    LSourceItem.Opacity := FOpacity;
  end;
end;

procedure TIconFontsImageList.SetFontColor(const Value: TAlphaColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetFontName(const Value: string);
begin
  if FFontName <> Value then
  begin
    FFontName := Value;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetOpacity(const Value: Single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateSourceItems;
  end;
end;

initialization
  RegisterFmxClasses([TIconFontsImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontsImageList.TIconFontsImageList, TFmxObject);

end.
