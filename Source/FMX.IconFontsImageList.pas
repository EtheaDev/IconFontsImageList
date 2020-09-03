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

const
  IconFontsImageListVersion = '1.5.1';

type
  //TIconFontMissing = procedure (const AFontName: TFontName) of object;

  TIconFontMultiResBitmap = class;
  TIconFontsImageList = class;
  TIconFontsSourceItem = class;

  TIconFontBitmapItem = class(TCustomBitmapItem)
  private
    FSize: Integer;
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
  protected
    function BitmapStored: Boolean; override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    property Character: String read GetCharacter;
  published
    property Bitmap: TBitmapOfItem read GetBitmap write SetBitmap stored False;
    property Scale;
    property Size: Integer read GetSize write SetSize default 32;
    //Readonly properties from Source Item
    property FontName: TFontName read GetFontName;
    property FontColor: TAlphaColor read GetFontColor stored false;
    property Opacity: single read GetOpacity stored false;
  end;

  TIconFontBitmapItemClass = class of TIconFontBitmapItem;

  TIconFontMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TIconFontsSourceItem;
    procedure UpdateImageSize(const ASize: Integer);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontBitmapItemClass); overload;
  public
  end;

  TIconFontsSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TIconFontsImageList;
    FFontIconDec: Integer;
    FOpacity: single;
    FFontName: TFontName;
    FFontColor: TAlphaColor;
    procedure UpdateAllItems;
    function GetCharacter: String;
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontName(const AValue: TFontName);
    procedure SetOpacity(const AValue: single);
    procedure AutoSizeBitmap(const ASize: Integer);
    function GetIconName: string;
    procedure SetIconName(const Value: string);
    function GetFontName: TFontName;
    function GetFontColor: TAlphaColor;
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

  TIconFontsImageList = class(TCustomImageList)
  private
    FSize: Integer;
    FAutoSizeBitmaps: Boolean;
    FFontName: TFontName;
    FFontColor: TAlphaColor;
    FOpacity: single;
    //FOnFontMissing: TIconFontMissing;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
    procedure SetFontName(const Value: TFontName);
    procedure UpdateSourceItems;
    procedure UpdateDestination(Size: TSize; const Index: Integer);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: single);
    function GetSize: Integer;
    procedure SetSize(const Value: Integer);
  protected
    procedure Loaded; override;
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
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
    property Source;
    property Destination;
    property OnChange;
    property OnChanged;
    property Size: Integer read GetSize write SetSize default 32;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default True;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: single read FOpacity write SetOpacity;
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
  if Collection is TIconFontMultiResBitmap then
    FOwnerMultiResBitmap := Collection as TIconFontMultiResBitmap;
  FSize := 32;
end;

procedure TIconFontBitmapItem.DrawFontIcon;
var
  LFont: TFont;
  LBitmap: TBitmap;
  LBitmapSize: Integer;
  LRect: TRectF;
begin
  LBitmap := inherited Bitmap;
  LBitmapSize := Round(Size * Scale);
  LFont := TFont.Create;
  try
    LFont.Family := FontName;
    LFont.Size := Size;
    LBitmap.Width  := LBitmapSize;
    LBitmap.Height := LBitmapSize;
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
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontColor;
end;

function TIconFontBitmapItem.GetFontName: TFontName;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.FontName;
end;

function TIconFontBitmapItem.GetOpacity: single;
begin
  Result := FOwnerMultiResBitmap.FOwnerSourceItem.Opacity;
end;

function TIconFontBitmapItem.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TIconFontBitmapItem.SetBitmap(const AValue: TBitmapOfItem);
begin
  inherited Bitmap.Assign(AValue);
  inherited Bitmap.BitmapScale := Scale;
end;

procedure TIconFontBitmapItem.SetSize(const AValue: Integer);
begin
  if (AValue > 0) and (AValue <> FSize) then
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

procedure TIconFontMultiResBitmap.UpdateImageSize(const ASize: Integer);
var
  I, J: Integer;
  LItem: TIconFontBitmapItem;
begin
  for I := 0 to ScaleList.Count - 1 do
  begin
    for J := 0 to Count - 1 do
    begin
      LItem := Items[J] as TIconFontBitmapItem;
      if LItem.Size <> ASize then
      begin
        LItem.Size := ASize;
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

procedure TIconFontsSourceItem.AutoSizeBitmap(const ASize: Integer);
begin
  //If present, delete multiple items
  while MultiResBitmap.Count > 1 do
    MultiResBitmap.Delete(MultiResBitmap.Count-1);
  //Add only one item
  if MultiResBitmap.Count = 0 then
    MultiResBitmap.Add;
  (MultiResBitmap as TIconFontMultiResBitmap).UpdateImageSize(ASize);
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

procedure TIconFontsSourceItem.UpdateIconAttributes(const AFontColor: TAlphaColor;
  const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
begin
  //If AReplaceFontColor is false then the color of single icon is preserved
  if AReplaceFontColor and (FFontColor <> TAlphaColors.Null) then
    FFontColor := AFontColor;
  //Replace FontName only if passed and different for specific Font
  if (AFontName <> '') and (FFontName <> '') and (AFontName <> FFontName) then
    FFontName := AFontName;
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
      LItem.Size := FOwnerImageList.Size;
      LSize.cx := LItem.Size;
      LSize.cy := LItem.Size;
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
    FSize := TIconFontsImageList(Source).FSize;
    FontName := TIconFontsImageList(Source).FontName;
    FontColor := TIconFontsImageList(Source).FontColor;
    Opacity := TIconFontsImageList(Source).Opacity;
    FAutoSizeBitmaps := TIconFontsImageList(Source).FAutoSizeBitmaps;
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
  FSize := 32;
end;

function TIconFontsImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TIconFontsSourceItem);
end;

procedure TIconFontsImageList.UpdateDestination(Size: TSize;
  const Index: Integer);
var
  LDestItem: TDestinationItem;
  LSourceItem: TIconFontsSourceItem;
  LIndex: Integer;
  LSize: Integer;
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
          LSize := Min(Size.cx, Size.cy);
          LSourceItem.AutoSizeBitmap(LSize);
        end
        else
          LSize := LSourceItem.FOwnerImageList.Size;
        LDestItem.Layers[0].SourceRect.Top := 0;
        LDestItem.Layers[0].SourceRect.Left := 0;
        LDestItem.Layers[0].SourceRect.Right := LSize;
        LDestItem.Layers[0].SourceRect.Bottom := LSize;
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

function TIconFontsImageList.GetSize: Integer;
begin
  Result := FSize;
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
    if LSourceItem.FFontColor = TAlphaColors.Null then
      LSourceItem.FontColor := FFontColor;
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

procedure TIconFontsImageList.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    UpdateSourceItems;
  end;
end;

procedure TIconFontsImageList.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    UpdateSourceItems;
  end;
end;

initialization
  RegisterFmxClasses([TIconFontsImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontsImageList.TIconFontsImageList, TFmxObject);

end.
