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
unit IconFontsImageList;

interface

uses
  Classes
  , ImgList
  , Graphics;

//StoreBitmap option introduced in Delphi 10.3 Rio
{$IF Defined(VER210) //Delphi 2010
  or Defined(VER220) //Delphi XE
  or Defined(VER230) //Delphi XE2
  or Defined(VER240) //Delphi XE3
  or Defined(VER250) //Delphi XE4
  or Defined(VER260) //Delphi XE5
  or Defined(VER270) //Delphi XE6
  or Defined(VER280) //Delphi XE7
  or Defined(VER290) //Delphi XE8
  or Defined(VER300) //Delphi 10 Seattle
  or Defined(VER310) //Delphi 10.1 Berlin
  or Defined(VER320) //Delphi 10.2 Tokyo
  }
  {$Define NeedStoreBitmapProperty}
{$IFEND}

type
  TIconFontsImageList = class;

  TIconFontItem = class(TCollectionItem)
  private
    FFontName: TFontName;
    FCharacter: Char;
    FFontColor: TColor;
    FMaskColor: TColor;
    FIconName: string;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure SetIconName(const AValue: string);
    procedure SetCharacter(const AValue: char);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontIconDec(const AValue: Integer);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure Changed;
    function GetCharacter: char;
    procedure UpdateIconAttributes(const AFontColor, AMaskColor: TColor;
      const AFontName: string = ''; const AReplace: Boolean = True);
    function GetIconFontsImageList: TIconFontsImageList;
    function FontColorDifferent: Boolean;
    function MaskColorDifferent: Boolean;
    function FontNameDifferent: Boolean;
  public
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored false;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property FontName: TFontName read FFontName write SetFontName stored FontNameDifferent;
    property FontColor: TColor read FFontColor write SetFontColor stored FontColorDifferent;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored MaskColorDifferent;
    property IconName: string read FIconName write SetIconName;
    property Character: char read GetCharacter write SetCharacter default #0;
  end;

  {TIconFontItems}
  TIconFontItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TIconFontItem;
    procedure SetItem(AIndex: Integer; const Value: TIconFontItem);
    procedure UpdateImage(const AIndex: Integer);
    function GetIconFontsImageList: TIconFontsImageList;
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    function Add: TIconFontItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(AIndex: Integer): TIconFontItem;
    FUNCTION GetIconByName(const AIconName: string): TIconFontItem;
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property Items[Index: Integer]: TIconFontItem read GetItem write SetItem; default;
  end;

  {TIconFontsImageList}
  TIconFontsImageList = class(TCustomImageList)
  private
    FStopDrawing: Integer;
    FIconFontItems: TIconFontItems;
    FFontName: TFontName;
    FMaskColor: TColor;
    FFontColor: TColor;
    {$IFDEF NeedStoreBitmapProperty}
    FStoreBitmap: Boolean;
    {$ENDIF}
    procedure SetIconSize(const ASize: Integer);
    procedure SetIconFontItems(const AValue: TIconFontItems);
    procedure UpdateImage(const AIndex: Integer);
    procedure ClearImages;
    procedure DrawFontIcon(const AIndex: Integer; const ABitmap: TBitmap;
      const AAdd: Boolean);
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure StopDrawing(const AStop: Boolean);
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
  protected
    {$IFDEF NeedStoreBitmapProperty}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    {$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete(AIndex: Integer);
    procedure AddIcon(const AChar: Char; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone);  virtual;
    //Multiple icons methods
    function AddIcons(const AFrom, ATo: Char; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;  virtual;
    procedure UpdateIconsAttributes(const AFontColor, AMaskColor: TColor;
      const AFontName: string = ''; const AReplace: Boolean = True); virtual;
    procedure ClearIcons; virtual;
    procedure RedrawImages; virtual;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  published
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property IconFontItems: TIconFontItems read FIconFontItems write SetIconFontItems;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TColor read FFontColor write SetFontColor default clNone;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property Size: Integer read GetSize write SetSize default 16;
    {$IFDEF NeedStoreBitmapProperty}
    property StoreBitmap: Boolean read FStoreBitmap write FStoreBitmap default False;
    {$ELSE}
    property StoreBitmap default False;
    {$ENDIF}
  end;

implementation

uses
  SysUtils
  , StrUtils;

{ TIconFontItem }

procedure TIconFontItem.Assign(Source: TPersistent);
begin
  if Source is TIconFontItem then
  begin
    FFontName := TIconFontItem(Source).FFontName;
    FCharacter := TIconFontItem(Source).FCharacter;
    FFontColor := TIconFontItem(Source).FFontColor;
    FMaskColor := TIconFontItem(Source).FMaskColor;
    FIconName := TIconFontItem(Source).FIconName;
  end
  else
    inherited Assign(Source);
end;

constructor TIconFontItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FCharacter := Chr(0);
  FFontColor := clNone;
  FMaskColor := clNone;

  //Default values inherited from owner ImageList
(*
  if Assigned(IconFontsImageList) then
  begin
    FFontName := IconFontsImageList.FFontName;
    FFontColor := IconFontsImageList.FFontColor;
    FMaskColor := IconFontsImageList.FMaskColor;
  end;
*)
end;

destructor TIconFontItem.Destroy;
begin
  inherited Destroy;
end;

function TIconFontItem.FontColorDifferent: Boolean;
begin
  Result := Assigned(IconFontsImageList) and
    (FFontColor <> IconFontsImageList.FFontColor);
end;

function TIconFontItem.FontNameDifferent: Boolean;
begin
  Result := Assigned(IconFontsImageList) and
    (FFontName <> IconFontsImageList.FFontName);
end;

function TIconFontItem.GetCharacter: char;
begin
  Result := FCharacter;
end;

function TIconFontItem.GetDisplayName: string;
begin
  Result := Format('%s - Hex: %s%s',
    [FFontName, FontIconHex, ifthen(FIconName<>'', ' - ('+FIconName+')', '')]);
end;

function TIconFontItem.GetFontIconDec: Integer;
begin
  Result := ord(FCharacter);
end;

function TIconFontItem.GetFontIconHex: string;
begin
  Result := IntToHex(Ord(FCharacter), 4);
end;

function TIconFontItem.GetIconFontsImageList: TIconFontsImageList;
begin
  if Assigned(Collection) then
    Result := TIconFontItems(Collection).IconFontsImageList
  else
    Result := nil;  
end;

function TIconFontItem.MaskColorDifferent: Boolean;
begin
  Result := Assigned(IconFontsImageList) and 
    (FMaskColor <> IconFontsImageList.FMaskColor);
end;

procedure TIconFontItem.SetCharacter(const AValue: char);
begin
  if AValue <> FCharacter then
  begin
    FCharacter := AValue;
    Changed;
  end;
end;

procedure TIconFontItem.SetFontColor(const AValue: TColor);
begin
  if AValue <> FFontColor then
  begin
    FFontColor := AValue;
    Changed;
  end;
end;

procedure TIconFontItem.SetFontIconDec(const AValue: Integer);
begin
  Character := Chr(AValue);
end;

procedure TIconFontItem.SetFontIconHex(const AValue: string);
begin
  if (Length(AValue) = 4) or (Length(AValue)=0) then
    Character := Chr(StrToInt('$' + AValue));
end;

procedure TIconFontItem.SetFontName(const AValue: TFontName);
begin
  if AValue <> FFontName then
  begin
    FFontName := AValue;
    if IconFontsImageList.FontName = '' then
      IconFontsImageList.FontName := FFontName;
    Changed;
  end;
end;

procedure TIconFontItem.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;

procedure TIconFontItem.SetMaskColor(const AValue: TColor);
begin
  if AValue <> FMaskColor then
  begin
    FMaskColor := AValue;
    Changed;
  end;
end;

procedure TIconFontItem.UpdateIconAttributes(const AFontColor, AMaskColor: TColor;
  const AFontName: string = ''; const AReplace: Boolean = True);
begin
  if (FFontColor <> AFontColor) or (FMaskColor <> AMaskColor) then
  begin
    if AReplace then
    begin
      FFontColor := AFontColor;
      FMaskColor := AMaskColor;
      if AFontName <> '' then
        FFontName := AFontName;
    end;
    Changed;
  end;
end;

procedure TIconFontItem.Changed;
begin
  if Assigned(Collection) then
    TIconFontItems(Collection).UpdateImage(Index);
end;

{ TIconFontsImageList }

procedure TIconFontsImageList.ClearIcons;
begin
  StopDrawing(True);
  try
    FIconFontItems.Clear;
    Clear;
  finally
    StopDrawing(False);
  end;
end;

procedure TIconFontsImageList.ClearImages;
begin
  StopDrawing(True);
  try
    while Count > 0 do
      inherited Delete(0);
  finally
    StopDrawing(False);
  end;
end;

constructor TIconFontsImageList.Create(AOwner: TComponent);
begin
  inherited;
  FStopDrawing := 0;
  FFontColor := clNone;
  FMaskColor := clNone;
  FIconFontItems := TIconFontItems.Create(Self, TIconFontItem);
  StoreBitmap := False;
end;

procedure TIconFontsImageList.Delete(AIndex: Integer);
begin
  //Don't call inherited method of ImageList, to avoid errors
  if Assigned(IconFontItems) then
    IconFontItems.Delete(AIndex);
end;

destructor TIconFontsImageList.Destroy;
begin
  FreeAndNil(FIconFontItems);
  inherited;
end;

procedure TIconFontsImageList.SetFontColor(const AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    UpdateIconsAttributes(FFontColor, FMaskColor, FFontName, False);
  end;
end;

procedure TIconFontsImageList.SetFontName(const AValue: TFontName);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    UpdateIconsAttributes(FFontColor, FMaskColor, FFontName, False);
  end;
end;

procedure TIconFontsImageList.SetHeight(const AValue: Integer);
begin
  SetIconSize(AValue);
end;

procedure TIconFontsImageList.SetIconFontItems(const AValue: TIconFontItems);
begin
  FIconFontItems := AValue;
end;

procedure TIconFontsImageList.SetIconSize(const ASize: Integer);
begin
  if Width <> ASize then
    inherited Width := ASize;
  if Height <> ASize then
    inherited Height := ASize;
end;

procedure TIconFontsImageList.SetMaskColor(const AValue: TColor);
begin
  if FMaskColor <> AValue then
  begin
    FMaskColor := AValue;
    UpdateIconsAttributes(FFontColor, FMaskColor, FFontName, False);
  end;
end;

procedure TIconFontsImageList.SetSize(const AValue: Integer);
begin
  if (AValue <> Height) or (AValue <> Width) then
  begin
    StopDrawing(True);
    try
      SetIconSize(AValue);
    finally
      StopDrawing(False);
    end;
    ClearImages;
    RedrawImages;
  end;
end;

procedure TIconFontsImageList.SetWidth(const AValue: Integer);
begin
  SetIconSize(AValue);
end;

procedure TIconFontsImageList.StopDrawing(const AStop: Boolean);
begin
  if AStop then
    Inc(FStopDrawing)
  else
    Dec(FStopDrawing);
end;

procedure TIconFontsImageList.AddIcon(const AChar: Char;
  const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone);
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := FIconFontItems.Add;
  LIconFontItem.FCharacter := AChar;
  if AFontName <> '' then
    LIconFontItem.FFontName := AFontName;
  if AFontColor <> clNone then
    LIconFontItem.FFontColor := AFontColor;
  if AMaskColor <> clNone then
    LIconFontItem.FMaskColor := AMaskColor;
end;

function TIconFontsImageList.AddIcons(const AFrom, ATo: Char;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;
var
  LChar: Char;
begin
  StopDrawing(True);
  try
    Result := 0;
    for LChar := AFrom to ATo do
    begin
      AddIcon(LChar, AFontName, AFontColor, AMaskColor);
      Inc(Result);
    end;
  finally
    StopDrawing(False);
  end;
  ClearImages;
  RedrawImages;
end;

procedure TIconFontsImageList.Assign(Source: TPersistent);
begin
  if Source is TIconFontsImageList then
  begin
    FFontName := TIconFontsImageList(Source).FontName;
    FFontColor := TIconFontsImageList(Source).FontColor;
    FMaskColor := TIconFontsImageList(Source).FMaskColor;
    StoreBitmap := TIconFontsImageList(Source).StoreBitmap;
    Height := TIconFontsImageList(Source).Height;
    FIconFontItems.Assign(TIconFontsImageList(Source).FIconFontItems);
  end
  else
  inherited;
end;

procedure TIconFontsImageList.DrawFontIcon(const AIndex: Integer;
  const ABitmap: TBitmap; const AAdd: Boolean);
var
  LIconFontItem: TIconFontItem;
  CharWidth: Integer;
  CharHeight: Integer;
  LCharacter: Char;
  LFontName: string;
  LMaskColor, LFontColor: TColor;
begin
  if FStopDrawing > 0 then
    Exit;
  if Assigned(ABitmap) then
  begin
    LIconFontItem := IconFontItems.GetItem(AIndex);
    //Default values from ImageList if not supplied from Item
    if LIconFontItem.FMaskColor <> clNone then
      LMaskColor := LIconFontItem.FMaskColor
    else
      LMaskColor := FMaskColor;
    if LIconFontItem.FFontColor <> clNone then
      LFontColor := LIconFontItem.FFontColor
    else
      LFontColor := FFontColor;
    if LIconFontItem.FFontName <> '' then
      LFontName := LIconFontItem.FFontName
    else
      LFontName := FFontName;
    LCharacter := LIconFontItem.Character;
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    with ABitmap.Canvas do
    begin
      Font.Name := LFontName;
      Font.Height := Height;
      Font.Color := LFontColor;
      Brush.Color := LMaskColor;
      FillRect(Rect(0, 0, Width, Height));
      CharWidth := TextWidth(LCharacter);
      CharHeight := TextHeight(LCharacter);
      TextOut((Width - CharWidth) div 2, (Height - CharHeight) div 2, LCharacter);
    end;
    if AAdd then
      AddMasked(ABitmap, LMaskColor)
    else
      ReplaceMasked(AIndex, ABitmap, LMaskColor);
  end;
end;

function TIconFontsImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TIconFontsImageList.GetSize: Integer;
begin
  Result := inherited Width;
end;

function TIconFontsImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TIconFontsImageList.Loaded;
begin
  inherited;
  if not StoreBitmap then
    RedrawImages;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const AFontColor,
  AMaskColor: TColor; const AFontName: string = '';
  const AReplace: Boolean = True);
var
  I: Integer;
  LIconFontItem: TIconFontItem;
begin
  if (AFontColor <> clNone) and (AMaskColor <> clNone) then
  begin
    FFontColor := AFontColor;
    FMaskColor := AMaskColor;
    for I := 0 to IconFontItems.Count -1 do
    begin
      LIconFontItem := IconFontItems.Items[I];
      LIconFontItem.UpdateIconAttributes(FFontColor, FMaskColor, AFontName, AReplace);
    end;
  end;
end;

procedure TIconFontsImageList.UpdateImage(const AIndex: Integer);
var
  LBitmap: TBitmap;
begin
  if (Height = 0) or (Width = 0) then
    Exit;
  LBitmap := TBitmap.Create;
  try
    DrawFontIcon(AIndex, LBitmap, Count <= AIndex);
    Self.Change;
  finally
    LBitmap.Free;
  end;
end;

procedure TIconFontsImageList.RedrawImages;
var
  I: Integer;
  LBitmap: TBitmap;
begin
  if not Assigned(FIconFontItems) or
    (csLoading in ComponentState) or
    (csDestroying in ComponentState) then
    Exit;
  inherited Clear;
  LBitmap := TBitmap.Create;
  try
    for I := 0 to FIconFontItems.Count -1 do
      DrawFontIcon(I, LBitmap, True);
    Self.Change;
  finally
    LBitmap.Free;
  end;
end;

{$IFDEF NeedStoreBitmapProperty}
procedure TIconFontsImageList.ReadData(Stream: TStream);
begin
  if FStoreBitmap then
    inherited;
end;

procedure TIconFontsImageList.WriteData(Stream: TStream);
begin
  if FStoreBitmap then
    inherited;
end;

procedure TIconFontsImageList.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
end;
{$ENDIF}

{ TIconFontItems }

function TIconFontItems.Add: TIconFontItem;
begin
  Result := TIconFontItem(inherited Add);
end;

procedure TIconFontItems.Assign(Source: TPersistent);
begin
  if (Source is TIconFontItems) and (IconFontsImageList <> nil) then
  begin
    IconFontsImageList.StopDrawing(True);
    try
      inherited;
    finally
      IconFontsImageList.StopDrawing(False);
    end;
    IconFontsImageList.RedrawImages;
  end
  else
    inherited;
end;

function TIconFontItems.GetIconByName(const AIconName: string): TIconFontItem;
var
  I: Integer;
  LIconFontItem: TIconFontItem;
begin
  Result := nil;
  for I := 0 to Count -1 do
  begin
    LIconFontItem := Items[I];
    if SameText(LIconFontItem.IconName, AIconName) then
    begin
      Result := LIconFontItem;
      Break;
    end;
  end;
end;

function TIconFontItems.GetIconFontsImageList: TIconFontsImageList;
begin
  if Owner <> nil then
    Result := TIconFontsImageList(Owner)
  else
    Result := nil;  
end;

function TIconFontItems.GetItem(AIndex: Integer): TIconFontItem;
begin
  Result := TIconFontItem(inherited GetItem(AIndex));
end;

function TIconFontItems.Insert(AIndex: Integer): TIconFontItem;
begin
  Result := TIconFontItem(inherited Insert(AIndex));
  IconFontsImageList.RedrawImages;
end;

procedure TIconFontItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if (Owner <> nil) and (IconFontsImageList.FStopDrawing = 0) and
    (Action in [cnExtracting, cnDeleting]) then
    TIconFontsImageList(Owner).RedrawImages;
end;

procedure TIconFontItems.SetItem(AIndex: Integer;
  const Value: TIconFontItem);
begin
  inherited SetItem(AIndex, Value);
end;

procedure TIconFontItems.UpdateImage(const AIndex: Integer);
begin
  if Owner <> nil then
    TIconFontsImageList(Owner).UpdateImage(AIndex);
end;

end.
