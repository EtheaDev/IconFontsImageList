{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList fmx: An extended ImageList for Delphi/FireMonkey  }
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

const
  IconFontsImageListVersion = '1.7.1';
  DEFAULT_SIZE = 32;
  ZOOM_DEFAULT = 100;

type
  //TIconFontMissing = procedure (const AFontName: TFontName) of object;

  TIconFontMultiResBitmap = class;
  TIconFontsImageList = class;
  TIconFontsSourceItem = class;

  TIconFontBitmapItem = class(TCustomBitmapItem)
  private
    FWidth, FHeight, FZoom: Integer;
    FOwnerMultiResBitmap: TIconFontMultiResBitmap;
    procedure SetBitmap(const AValue: TBitmapOfItem);
    function GetBitmap: TBitmapOfItem;
    procedure SetSize(const AValue: Integer);
    procedure DrawFontIcon;
    function GetCharacter: String;
    function GetFontName: TFontName;
    function GetFontColor: TAlphaColor;
    function GetOpacity: single;
    function GetSize: Integer;
    procedure SetIconSize(const AWidth, AHeight, AZoom: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;
    procedure SetZoom(const AValue: Integer);
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    property Character: String read GetCharacter;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Scale;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Zoom: Integer read FZoom write SetZoom default ZOOM_DEFAULT;
    //Readonly properties from Source Item
    property FontName: TFontName read GetFontName;
    property FontColor: TAlphaColor read GetFontColor stored false;
    property Opacity: single read GetOpacity stored false;
  end;

  TIconFontBitmapItemClass = class of TIconFontBitmapItem;

  TIconFontMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TIconFontsSourceItem;
    procedure UpdateImageSize(const AWidth, AHeight, AZoom: Integer);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontBitmapItemClass); overload;
  public
  end;

  {TIconFontsSourceItem}
  TIconFontsSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TIconFontsImageList;
    FFontIconDec: Integer;
    FOpacity: single;
    FFontName: TFontName;
    FFontColor: TAlphaColor;
    procedure UpdateAllItems;
    function GetCharacter: String;
    function GetFontName: TFontName;
    function GetFontColor: TAlphaColor;
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontName(const AValue: TFontName);
    procedure SetOpacity(const AValue: single);
    procedure AutoSizeBitmap(const AWidth, AHeight, AZoom: Integer);
    function GetIconName: string;
    procedure SetIconName(const Value: string);
    function GetOpacity: single;
    function GetDestinationItem: TCustomDestinationItem;
    procedure UpdateIconAttributes(const AFontColor: TAlphaColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
  protected
    function GetDisplayName: string; override;
    function CreateMultiResBitmap: TMultiResBitmap; override;
    function StoreFontName: Boolean; virtual;
    function StoreFontColor: Boolean; virtual;
    function StoreOpacity: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Character: String read GetCharacter;
  published
    property MultiResBitmap;
    property IconName: string read GetIconName write SetIconName;
    property FontName: TFontName read GetFontName write SetFontName stored StoreFontName;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property FontColor: TAlphaColor read GetFontColor write SetFontColor stored StoreFontColor;
    property Opacity: single read GetOpacity write SetOpacity stored StoreOpacity;
  end;

  {TIconFontsImageList}
  TIconFontsImageList = class(TCustomImageList)
  private
    FWidth, FHeight: Integer;
    FAutoSizeBitmaps: Boolean;
    FFontName: TFontName;
    FFontColor: TAlphaColor;
    FOpacity: single;
    FZoom: Integer;
    //FOnFontMissing: TIconFontMissing;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
    procedure SetFontName(const Value: TFontName);
    procedure UpdateSourceItems;
    procedure UpdateDestination(ASize: TSize; const Index: Integer);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: single);
    procedure SetIconSize(const AWidth, AHeight: Integer);
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure SetZoom(const AValue: Integer);
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;
  protected
    procedure Loaded; override;
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
    function StoreOpacity: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure DeleteIcon(const AIndex: Integer);
    function InsertIcon(const AIndex: Integer;
      const AFontIconDec: Integer;
      const AFontName: TFontName = '';
      const AFontColor: TAlphaColor = TAlphaColors.Null): Integer;
    //Multiple icons methods
    function AddIcons(const AFromIconDec, AToIconDec: Integer;
      const AFontName: TFontName = '';
      const AFontColor: TAlphaColor = TAlphaColors.Null): Integer;
    procedure ClearIcons; virtual;
    procedure UpdateIconAttributes(const ASize: Integer; const AFontColor: TAlphaColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = ''); overload;
    procedure UpdateIconAttributes(const AFontColor: TAlphaColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = ''); overload;
  published
    //Publishing properties of standard ImageList
    property Source;
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    property Zoom: Integer read FZoom write SetZoom default ZOOM_DEFAULT;
    property Destination;
    property OnChange;
    property OnChanged;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default True;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: single read FOpacity write SetOpacity stored StoreOpacity;
    //property OnFontMissing: TIconFontMissing read FOnFontMissing write FOnFontMissing;
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
  FWidth := DEFAULT_SIZE;
  FHeight := DEFAULT_SIZE;
  FZoom := ZOOM_DEFAULT;
  if Collection is TIconFontMultiResBitmap then
    FOwnerMultiResBitmap := Collection as TIconFontMultiResBitmap;
end;

procedure TIconFontBitmapItem.DrawFontIcon;
var
  LFont: TFont;
  LBitmap: TBitmap;
  LBitmapWidth, LBitmapHeight: Integer;
  LRect: TRectF;
begin
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
  finally
    LFont.DisposeOf;
  end;
end;

function TIconFontBitmapItem.GetBitmap: TBitmapOfItem;
begin
  DrawFontIcon;
  Result := inherited Bitmap;
end;

function TIconFontBitmapItem.GetCharacter: String;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Character;
end;

function TIconFontBitmapItem.GetDisplayName: string;
begin
  Result := Format('%s - %dx%d - Scale: %s',
    [FOwnerMultiResBitmap.FOwnerSourceItem.Name,
     Size, Size, FloatToStr(Scale)]);
end;

function TIconFontBitmapItem.GetFontColor: TAlphaColor;
begin
  if FOwnerMultiResBitmap.FOwnerSourceItem.FontColor <> TAlphaColors.Null then
    Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontColor
  else
    Result := FOwnerMultiResBitmap.FOwnerSourceItem.FOwnerImageList.FontColor;
end;

function TIconFontBitmapItem.GetFontName: TFontName;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontName;
end;

function TIconFontBitmapItem.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TIconFontBitmapItem.GetOpacity: single;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Opacity;
end;

function TIconFontBitmapItem.GetSize: Integer;
begin
  Result := Max(FWidth, FHeight);
  if Result = 0 then
    Result := DEFAULT_SIZE;
end;

function TIconFontBitmapItem.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TIconFontBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TIconFontBitmapItem.SetHeight(const AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontBitmapItem.SetWidth(const AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontBitmapItem.SetZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
  begin
    FZoom := AValue;
    DrawFontIcon;
  end;
end;

procedure TIconFontBitmapItem.SetIconSize(const AWidth, AHeight, AZoom: Integer);
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

procedure TIconFontBitmapItem.SetSize(const AValue: Integer);
begin
  if ((AValue <> FHeight) or (AValue <> FWidth)) then
    SetIconSize(AValue, AValue, FZoom);
end;

function TIconFontBitmapItem.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TIconFontBitmapItem.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TIconFontBitmapItem.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
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

procedure TIconFontMultiResBitmap.UpdateImageSize(const AWidth, AHeight, AZoom: Integer);
var
  I, J: Integer;
  LItem: TIconFontBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J] as TIconFontBitmapItem;
      if (LItem.FWidth <> AWidth) or (LItem.FHeight <> AHeight) then
      begin
        LItem.FWidth := AWidth;
        LItem.FHeight := AHeight;
        LItem.Zoom := AZoom;
        LItem.DrawFontIcon;
      end;
    end;
  end;
end;

{ TIconFontsSourceItem }

procedure TIconFontsSourceItem.Assign(Source: TPersistent);
begin
  if Source is TIconFontsSourceItem then
  begin
    FFontName    := TIconFontsSourceItem(Source).FFontName;
    FFontIconDec := TIconFontsSourceItem(Source).FFontIconDec;
    FFontColor   := TIconFontsSourceItem(Source).FFontColor;
    FOpacity     := TIconFontsSourceItem(Source).FOpacity;
  end;
  inherited;
end;

procedure TIconFontsSourceItem.AutoSizeBitmap(const AWidth, AHeight, AZoom: Integer);
begin
  //If present, delete multiple items
  while MultiResBitmap.Count > 1 do
    MultiResBitmap.Delete(MultiResBitmap.Count-1);
  //Add only one item
  if MultiResBitmap.Count = 0 then
    MultiResBitmap.Add;
  (MultiResBitmap as TIconFontMultiResBitmap).UpdateImageSize(AWidth, AHeight, AZoom);
end;

constructor TIconFontsSourceItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFontIconDec := 0;
  FOpacity := -1;
  FFontName := '';
  FFontColor := TAlphaColors.null;
  UpdateAllItems;
end;

function TIconFontsSourceItem.CreateMultiResBitmap: TMultiResBitmap;
begin
  Result := TIconFontMultiResBitmap.Create(self, TIconFontBitmapItem);
  FOwnerImageList := Result.ImageList as TIconFontsImageList;
end;

function TIconFontsSourceItem.GetCharacter: String;
begin
  {$WARN SYMBOL_DEPRECATED OFF}
  if FFontIconDec <> 0 then
    Result := ConvertFromUtf32(FFontIconDec)
  else
    Result := '';
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

function TIconFontsSourceItem.GetFontName: TFontName;
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

function TIconFontsSourceItem.GetOpacity: single;
begin
  if FOpacity = -1 then
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

procedure TIconFontsSourceItem.SetFontName(const AValue: TFontName);
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

function TIconFontsSourceItem.GetDestinationItem: TCustomDestinationItem;
var
  LDest: TCustomDestinationItem;
begin
  Result := nil;
  if FOwnerImageList.Destination.Count > Index then
  begin
    LDest := FOwnerImageList.Destination.Items[Index];
    if (LDest.LayersCount > 0) and
      SameText(LDest.Layers[0].Name, IconName) then
      Result := LDest;
  end;
end;

procedure TIconFontsSourceItem.SetIconName(const Value: string);
var
  LDest: TCustomDestinationItem;
begin
  if Value <> Name then
  begin
    LDest := GetDestinationItem;
    inherited Name := Value;
    if Assigned(LDest) then
      LDest.Layers[0].Name := Value;
  end;
end;

procedure TIconFontsSourceItem.SetOpacity(const AValue: single);
begin
  if Assigned(FOwnerImageList) and (AValue = FOwnerImageList.Opacity) then
  begin
    FOpacity := -1;
  end
  else
    FOpacity := AValue;
  UpdateAllItems;
end;

function TIconFontsSourceItem.StoreFontColor: Boolean;
begin
  Result := ((FOwnerImageList = nil) or (FFontColor <> FOwnerImageList.FFontColor))
    and (FFontColor <> TAlphaColors.Null);
end;

function TIconFontsSourceItem.StoreFontName: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FFontName <> FOwnerImageList.FFontName);
end;

function TIconFontsSourceItem.StoreOpacity: Boolean;
begin
  Result := (FOwnerImageList = nil) or (FOpacity <> FOwnerImageList.FOpacity);
end;

procedure TIconFontsSourceItem.UpdateIconAttributes(const AFontColor: TAlphaColor;
  const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
begin
  //If AReplaceFontColor is false then the color of single icon is preserved
  if AReplaceFontColor and (FFontColor <> TAlphaColors.Null) then
    FFontColor := AFontColor;
  //Replace FontName only if passed and different for specific Font
  if (AFontName <> '') and (FFontName <> '') and (AFontName <> FFontName) then
    FFontName := AFontName
  else
    FFontName := '';
end;

procedure TIconFontsSourceItem.UpdateAllItems;
var
  I: Integer;
  LItem: TIconFontBitmapItem;
  LSize: TSize;
begin
  for I := 0 to MultiResBitmap.Count -1 do
  begin
    LItem := MultiResBitmap.Items[I] as TIconFontBitmapItem;
    Litem.DrawFontIcon;
    if (I=0) and (FOwnerImageList <> nil) then
    begin
      LItem.SetIconSize(FOwnerImageList.Width, FOwnerImageList.Height, FOwnerImageList.Zoom);
      LSize.cx := LItem.Width;
      LSize.cy := LItem.Height;
      FOwnerImageList.UpdateDestination(LSize, Index);
    end;
  end;
end;

{ TIconFontsImageList }

function TIconFontsImageList.InsertIcon(
  const AIndex: Integer;
  const AFontIconDec: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TAlphaColor = TAlphaColors.Null): Integer;
var
  LItem: TIconFontsSourceItem;
  LDest: TCustomDestinationItem;
begin
  LItem := Self.Source.Insert(AIndex) as TIconFontsSourceItem;
  Result := LItem.Index;
  LItem.MultiResBitmap.Add;
  if AFontName <> '' then
    LItem.FontName := AFontName;
  LItem.FontIconDec := AFontIconDec;
  if AFontColor <> TAlphaColors.Null then
    LItem.FontColor := AFontColor;
  LDest := Self.Destination.Insert(AIndex);
  with LDest.Layers.Add do
    Name := LItem.Name;
end;

function TIconFontsImageList.AddIcons(const AFromIconDec, AToIconDec: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TAlphaColor = TAlphaColors.Null): Integer;
var
  LFontIconDec: Integer;
  LIndex: Integer;
begin
  LIndex := Count;
  for LFontIconDec := AFromIconDec to AToIconDec do
    LIndex := InsertIcon(LIndex, LFontIconDec, AFontName, AFontColor) + 1;
  Result := AFromIconDec - AToIconDec + 1;
end;

procedure TIconFontsImageList.Assign(Source: TPersistent);
begin
  if Source is TIconFontsImageList then
  begin
    FontName := TIconFontsImageList(Source).FontName;
    FontColor := TIconFontsImageList(Source).FontColor;
    Opacity := TIconFontsImageList(Source).Opacity;
    FAutoSizeBitmaps := TIconFontsImageList(Source).FAutoSizeBitmaps;
    Zoom := TIconFontsImageList(Source).FZoom;
    SetIconSize(TIconFontsImageList(Source).FWidth,
      TIconFontsImageList(Source).FHeight);
  end;
  inherited;
end;

procedure TIconFontsImageList.ClearIcons;
begin
  Source.Clear;
  Destination.Clear;
end;

constructor TIconFontsImageList.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizeBitmaps := True;
  FFontColor := TAlphaColors.Black;
  FOpacity := 1;
  FWidth := DEFAULT_SIZE;
  FHeight := DEFAULT_SIZE;
  FZoom := ZOOM_DEFAULT;
end;

function TIconFontsImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TIconFontsSourceItem);
end;

procedure TIconFontsImageList.UpdateDestination(ASize: TSize;
  const Index: Integer);
var
  LDestItem: TDestinationItem;
  LSourceItem: TIconFontsSourceItem;
  LIndex: Integer;
  LWidth, LHeight: Integer;
begin
  while Index > Destination.Count-1 do
    Destination.Add;
  LDestItem := Destination.Items[Index] as TDestinationItem;
  if LDestItem.Layers.Count > 0 then
  begin
    LIndex := Source.indexOf(LDestItem.Layers[0].Name);
    if LIndex >= 0 then
    begin
      LSourceItem := Source.Items[LIndex] as TIconFontsSourceItem;
      if Assigned(LSourceItem) then
      begin
        if FAutoSizeBitmaps then
        begin
          if FWidth = FHeight then
          begin
            LWidth := Min(ASize.cy, ASize.cx);
            LHeight := LWidth;
          end
          else if FWidth > FHeight then
          begin
            LWidth := Min(ASize.cy, ASize.cx);
            LHeight := Round((FHeight / FWidth) * ASize.cy);
          end
          else
          begin
            LHeight := ASize.cy;
            LWidth := Round((FWidth / FHeight) * ASize.cx);
          end;
          LSourceItem.AutoSizeBitmap(LWidth, LHeight, FZoom);
        end
        else
        begin
          LWidth := LSourceItem.FOwnerImageList.FWidth;
          LHeight := LSourceItem.FOwnerImageList.FHeight;
        end;
        LDestItem.Layers[0].SourceRect.Top := 0;
        LDestItem.Layers[0].SourceRect.Left := 0;
        LDestItem.Layers[0].SourceRect.Right := LWidth;
        LDestItem.Layers[0].SourceRect.Bottom := LHeight;
      end;
    end;
  end;
end;

procedure TIconFontsImageList.UpdateIconAttributes(
  const AFontColor: TAlphaColor; const AReplaceFontColor: Boolean;
  const AFontName: TFontName);
begin
  UpdateIconAttributes(Self.Size, AFontColor, AReplaceFontColor, AFontName);
end;

procedure TIconFontsImageList.UpdateIconAttributes(const ASize: Integer;
  const AFontColor: TAlphaColor; const AReplaceFontColor: Boolean;
  const AFontName: TFontName);
var
  I: Integer;
  LIconFontItem: TIconFontsSourceItem;
begin
  if (AFontColor <> TAlphaColors.null) then
  begin
    Self.Size := ASize;
    FFontColor := AFontColor;
    for I := 0 to Source.Count -1 do
    begin
      LIconFontItem := Source.Items[I] as TIconFontsSourceItem;
      LIconFontItem.UpdateIconAttributes(FFontColor, AReplaceFontColor, AFontName);
    end;
  end;
end;

procedure TIconFontsImageList.DeleteIcon(const AIndex: Integer);
var
  LDest: TCustomDestinationItem;
  LSourceItem: TIconFontsSourceItem;
begin
  LSourceItem := Source.Items[AIndex] as TIconFontsSourceItem;
  if Assigned(LSourceItem) then
  begin
    LDest := LSourceItem.GetDestinationItem;
    Source.Delete(AIndex);
    if Assigned(LDest) then
      Destination.Delete(AIndex);
  end;
end;

function TIconFontsImageList.DoBitmap(Size: TSize;
  const Index: Integer): TBitmap;
begin
  UpdateDestination(Size, Index);
  Result := inherited DoBitmap(Size, Index);
end;

function TIconFontsImageList.StoreSize: Boolean;
begin
  Result := (Width = Height) and (Width <> DEFAULT_SIZE);
end;

function TIconFontsImageList.StoreWidth: Boolean;
begin
  Result := (Width <> Height) and (Width <> DEFAULT_SIZE);
end;

function TIconFontsImageList.StoreHeight: Boolean;
begin
  Result := (Width <> Height) and (Height <> DEFAULT_SIZE);
end;

function TIconFontsImageList.GetSize: Integer;
begin
  Result := Max(FWidth, FHeight);
end;

function TIconFontsImageList.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TIconFontsImageList.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TIconFontsImageList.Loaded;
begin
  inherited;
  UpdateSourceItems;
end;

procedure TIconFontsImageList.SetAutoSizeBitmaps(const Value: Boolean);
begin
  FAutoSizeBitmaps := Value;
  if (Count > 0) then
    UpdateSourceItems;
end;

procedure TIconFontsImageList.UpdateSourceItems;
var
  I: Integer;
  LSourceItem: TIconFontsSourceItem;
begin
  for I := 0 to Source.Count -1 do
  begin
    LSourceItem := Source[I] as TIconFontsSourceItem;
    if LSourceItem.FFontName = '' then
      LSourceItem.FontName := FFontName;
    if LSourceItem.FOpacity = -1 then
      LSourceItem.Opacity := FOpacity;
    LSourceItem.UpdateAllItems;
  end;
end;

procedure TIconFontsImageList.SetFontColor(const Value: TAlphaColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    UpdateSourceItems;
    Change;
  end;
end;

procedure TIconFontsImageList.SetFontName(const Value: TFontName);
begin
  if FFontName <> Value then
  begin
    //TODO: check font exists (multi-platform)
    FFontName := Value;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetHeight(const AValue: Integer);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetIconSize(const AWidth, AHeight: Integer);
begin
  if (AWidth <> 0) and (AHeight <> 0) and
    ((AWidth <> FWidth) or (AHeight <> FHeight)) then
  begin
    FWidth := AWidth;
    FHeight := AHeight;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateSourceItems;
  end;
end;

function TIconFontsImageList.StoreOpacity: Boolean;
begin
  Result := FOpacity <> 1;
end;

procedure TIconFontsImageList.SetSize(const AValue: Integer);
begin
  if ((AValue <> FHeight) or (AValue <> FWidth)) then
    SetIconSize(AValue, AValue);
end;

procedure TIconFontsImageList.SetWidth(const AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetZoom(const AValue: Integer);
begin
  if (FZoom <> AValue) and (AValue <= 100) and (AValue >= 10) then
  begin
    FZoom := AValue;
    UpdateSourceItems;
  end;
end;

initialization
  RegisterFmxClasses([TIconFontsImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontsImageList.TIconFontsImageList, TFmxObject);

end.
