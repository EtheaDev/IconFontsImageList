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
    function GetCharacter: WideChar;
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
    property Character: WideChar read GetCharacter stored false;
    property FontColor: TAlphaColor read GetFontColor stored false;
    property Opacity: Single read GetOpacity  stored false;
  end;

  TIconFontBitmapItemClass = class of TIconFontBitmapItem;

  TIconFontMultiResBitmap = class(TMultiResBitmap)
  private
    FOwnerSourceItem: TIconFontsSourceItem;
    function OwnerImageList: TIconFontsImageList;
    procedure UpdateImageSize(const ASize: Single);
  protected
    constructor Create(AOwner: TPersistent; ItemClass: TIconFontBitmapItemClass); overload;
  public
  end;

  TIconFontsSourceItem = class(TCustomSourceItem)
  private
    FOwnerImageList: TIconFontsImageList;
    FCharacter: WideChar;
    FOpacity: Single;
    FFontName: string;
    FFontColor: TAlphaColor;
    procedure UpdateAllItems;
    function GetCharacter: WideChar;
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetCharacter(const AValue: WideChar);
    procedure SetFontColor(const AValue: TAlphaColor);
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontName(const AValue: string);
    procedure SetOpacity(const AValue: Single);
    procedure AutoSizeBitmap(const ASize: Single);
  protected
    function GetDisplayName: string; override;
    function CreateMultiResBitmap: TMultiResBitmap; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property MultiResBitmap;
    property Name;
    property FontName: string read FFontName write SetFontName;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property Character: WideChar read GetCharacter write SetCharacter stored false default #0;
    property FontColor: TAlphaColor read FFontColor write SetFontColor;
    property Opacity: Single read FOpacity write SetOpacity;
  end;

  TIconFontsImageList = class(TCustomImageList)
  private
    FAutoSizeBitmaps: Boolean;
    procedure SetAutoSizeBitmaps(const Value: Boolean);
  protected
    function CreateSource: TSourceCollection; override;
    function DoBitmap(Size: TSize; const Index: Integer): TBitmap; override;
  published
    property Source;
    property Destination;
    property OnChange;
    property OnChanged;
    property AutoSizeBitmaps: Boolean read FAutoSizeBitmaps write SetAutoSizeBitmaps default False;
  end;

implementation

uses
  System.Math
  , System.RTLConsts
  , System.SysUtils
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

function TIconFontBitmapItem.GetCharacter: WideChar;
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

function TIconFontMultiResBitmap.OwnerImageList: TIconFontsImageList;
begin
  if Assigned(FOwnerSourceItem) then
    Result := FOwnerSourceItem.FOwnerImageList
  else
    Result := nil;
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
var
  LItem: TIconFontBitmapItem;
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
  FOpacity := 1;
  FFontName := 'Material Design Icons';
  FCharacter := WideChar(StrToInt('$F008'));
  FFontColor := TAlphaColors.Black;
end;

function TIconFontsSourceItem.CreateMultiResBitmap: TMultiResBitmap;
begin
  Result := TIconFontMultiResBitmap.Create(self, TIconFontBitmapItem);
  FOwnerImageList := Result.ImageList as TIconFontsImageList;
end;

function TIconFontsSourceItem.GetCharacter: WideChar;
begin
  Result := FCharacter;
end;

function TIconFontsSourceItem.GetDisplayName: string;
begin
  if Name <> '' then
    Result := Format('%s - Hex: %s - (%s)',
      [FFontName, FontIconHex, Name])
  else
    Result := Format('%s - Hex: %s',
      [FFontName, FontIconHex]);
end;

function TIconFontsSourceItem.GetFontIconDec: Integer;
begin
  Result := ord(FCharacter);
end;

function TIconFontsSourceItem.GetFontIconHex: string;
begin
  if FCharacter <> #0 then
    Result := IntToHex(Ord(FCharacter), 4)
  else
    Result := '';
end;

procedure TIconFontsSourceItem.SetCharacter(const AValue: WideChar);
begin
  if AValue <> FCharacter then
  begin
    FCharacter := AValue;
    UpdateAllItems;
  end;
end;

procedure TIconFontsSourceItem.SetFontColor(const AValue: TAlphaColor);
begin
  FFontColor := AValue;
  UpdateAllItems;
end;

procedure TIconFontsSourceItem.SetFontIconDec(const AValue: Integer);
begin
  Character := WideChar(AValue);
end;

procedure TIconFontsSourceItem.SetFontIconHex(const AValue: string);
begin
  if (Length(AValue) = 4) then
    Character := WideChar(StrToInt('$' + AValue))
  else if (Length(AValue) = 0) then
    Character := #0
  else
    raise Exception.CreateFmt('Value %s not accepted!',[AValue]);
end;

procedure TIconFontsSourceItem.SetFontName(const AValue: string);
begin
  FFontName := AValue;
  UpdateAllItems;
end;

procedure TIconFontsSourceItem.SetOpacity(const AValue: Single);
begin
  FOpacity := AValue;
  UpdateAllItems;
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

function TIconFontsImageList.CreateSource: TSourceCollection;
begin
  Result := TSourceCollection.Create(self, TIconFontsSourceItem);
end;

function TIconFontsImageList.DoBitmap(Size: TSize;
  const Index: Integer): TBitmap;
var
  LSourceCollectionItem: TIconFontsSourceItem;
  LDestItem: TDestinationItem;
  LDinamicSize: Boolean;
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

initialization
  RegisterFmxClasses([TIconFontsImageList]);

  StartClassGroup(TFmxObject);
  ActivateClassGroup(TFmxObject);
  GroupDescendentsWith(FMX.IconFontsImageList.TIconFontsImageList, TFmxObject);

end.
