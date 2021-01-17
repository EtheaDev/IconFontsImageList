{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageCollection: An extended ImageCollection for Delphi/VCL }
{       to use VirtualImageList with Icon Fonts                                }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
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
    FOnFontMissing: TIconFontMissing;
    FFontNamesChecked: TStrings;
    FZoom: Integer;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure SetIconFontItems(Value: TIconFontItems);
    procedure SetZoom(const Value: Integer);
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
    //Single Icon Method
    procedure Delete(const AIndex: Integer);
    procedure Replace(const AIndex: Integer; const AChar: WideChar;
      const AFontName: TFontName = ''; const AFontColor: TColor = clDefault;
      AMaskColor: TColor = clNone); overload;
    procedure Replace(const AIndex: Integer; const AChar: Integer;
      const AFontName: TFontName = ''; const AFontColor: TColor = clDefault;
      AMaskColor: TColor = clNone); overload;
    function AddIcon(const AChar: Integer; const AIconName: string;
      const AFontName: TFontName = ''; const AFontColor: TColor = clDefault;
      const AMaskColor: TColor = clNone): TIconFontItem; overload;
    function AddIcon(const AChar: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clDefault; const AMaskColor: TColor = clNone): TIconFontItem; overload;
    function AddIcon(const AChar: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clDefault; const AMaskColor: TColor = clNone): TIconFontItem; overload;
    //Multiple icons methods
    function AddIcons(const AFrom, ATo: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clDefault; AMaskColor: TColor = clNone;
      const ACheckValid: Boolean = False): Integer;  overload;
    function AddIcons(const AFrom, ATo: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clDefault; AMaskColor: TColor = clNone;
      const ACheckValid: Boolean = False): Integer;  overload;
    function AddIcons(const ASourceString: WideString;
      const AFontName: TFontName = ''): Integer; overload;
    procedure UpdateIconsAttributes(const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = ''); overload;
    procedure ClearIcons;
    {$IFDEF D10_3+}
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; override;
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); overload; override;
    {$ENDIF}
    property UpdateOwnerAttributes: TGetOwnerAttributesProc read FUpdateOwnerAttributes write FUpdateOwnerAttributes;
    property NotifyItemChanged: TIconFontItemChangedProc read FNotifyItemChanged write FNotifyItemChanged;
    property NotifyFontUsed: TIconFontItemChangedProc read FNotifyItemChanged write FNotifyItemChanged;
  published
    /// <summary>
    /// Collection of items with source images.
    /// </summary>
    property IconFontItems: TIconFontItems read FIconFontItems write SetIconFontItems;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TColor read FFontColor write SetFontColor default clDefault;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property OnFontMissing: TIconFontMissing read FOnFontMissing write FOnFontMissing;
    property Zoom: Integer read FZoom write SetZoom default ZOOM_DEFAULT;
  end;

implementation

uses
  SysUtils
  {$IFDEF DXE3+}
  , System.Character
  {$ENDIF}
{$IFDEF GDI+}
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
{$ENDIF}
  , Forms
  , Math
  ;
{ TIconFontsImageCollection }

procedure TIconFontsImageCollection.CheckFontName(const AFontName: TFontName);
begin
  if AFontName <> '' then
  begin
    if FFontNamesChecked.IndexOf(AFontName) = -1 then //Speed-up check of a Font already checked
    begin
      FFontNamesChecked.Add(AFontName);
      if (Screen.Fonts.IndexOf(AFontName) = -1) then
      begin
        if Assigned(OnFontMissing) then
          OnFontMissing(AFontName)
        else if not (csDesigning in ComponentState) then
          raise Exception.CreateFmt(ERR_ICONFONTS_FONT_NOT_INSTALLED,[AFontName]);
      end
      else
        FFontNamesChecked.Add(AFontName);
    end;
  end;
end;

constructor TIconFontsImageCollection.Create(AOwner: TComponent);
begin
  inherited;
  FFontNamesChecked := TStringList.Create;
  FIconFontItems := TIconFontItems.Create(Self, TIconFontItem,
    OnItemChanged, CheckFontName, GetOwnerAttributes);
  FFontColor := clDefault;
  FMaskColor := clNone;
  FZoom := ZOOM_DEFAULT;
end;

procedure TIconFontsImageCollection.Delete(const AIndex: Integer);
begin
  IconFontItems.Delete(AIndex);
  OnItemChanged(nil);
end;

destructor TIconFontsImageCollection.Destroy;
begin
  FreeAndNil(FIconFontItems);
  FreeAndNil(FFontNamesChecked);
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

procedure TIconFontsImageCollection.SetZoom(const Value: Integer);
begin
  if FZoom <> Value then
  begin
    FZoom := Value;
    OnItemChanged(nil);
  end;
end;

procedure TIconFontsImageCollection.UpdateIconsAttributes(
  const AFontColor, AMaskColor: TColor; const AReplaceFontColor: Boolean;
  const AFontName: TFontName);
begin
  if AFontName <> '' then
    FFontName := AFontName;
  FFontColor := AFontColor;
  FMaskColor := AMaskColor;
  if AReplaceFontColor and Assigned(IconFontItems) then
    IconFontItems.UpdateIconsAttributes(AFontColor, AMaskColor, AFontName);
  OnItemChanged(nil);
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
  Result := LIconFontItem.GetBitmap(AWidth, AHeight, True,
    DEFAULT_OPACITY, DEFAULT_DISABLE_FACTOR, Zoom);
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
  LIconFontItem.PaintTo(ACanvas, ARect.Left, ARect.Top, ARect.Width,
    ARect.Height, True, DEFAULT_DISABLE_FACTOR, DEFAULT_OPACITY,
    Zoom);
  {$ELSE}
  LIconFontItem.PaintTo(ACanvas, ARect.Left, ARect.Top, ARect.Width,
    ARect.Height, LMaskColor, True, DEFAULT_DISABLE_FACTOR,
    Zoom);
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
    FUpdateOwnerAttributes(LFontName, LFontColor, LMaskColor)
  else
  begin
    LFontName := FFontName;
    LFontColor := FFontColor;
    LMaskColor := FMaskColor;
  end;
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
end;

procedure TIconFontsImageCollection.OnItemChanged(Sender: TIconFontItem);
begin
  if Assigned(NotifyItemChanged) then
    NotifyItemChanged(Sender)
{$IFDEF D10_3+}
  else
    Change;
{$ENDIF}
end;

function TIconFontsImageCollection.AddIcons(const AFrom, ATo: WideChar;
  const AFontName: TFontName; const AFontColor: TColor; AMaskColor: TColor;
  const ACheckValid: Boolean): Integer;
begin
  Result := AddIcons(Ord(AFrom), Ord(ATo), AFontName, AFontColor, AMaskColor, ACheckValid);
end;

function TIconFontsImageCollection.AddIcons(const ASourceString: WideString;
  const AFontName: TFontName): Integer;
{$IFDEF DXE3+}
var
  LChar: UCS4Char;
  I, L, ICharLen: Integer;
{$ENDIF}
begin
  Result := 0;
  {$IFDEF DXE3+}
  L := Length(ASourceString);
  I := 1;
  while I <= L do
  begin
    {$WARN SYMBOL_DEPRECATED OFF}
    if IsSurrogate(ASourceString[I]) then
    begin
      LChar := ConvertToUtf32(ASourceString, I, ICharLen);
    end
    else
    begin
      ICharLen := 1;
      LChar := UCS4Char(ASourceString[I]);
    end;
    {$WARN SYMBOL_DEPRECATED ON}
    AddIcon(Ord(LChar), AFontName);
    Inc(I, ICharLen);
    Inc(Result);
  end;
  {$ENDIF}
end;

procedure TIconFontsImageCollection.Replace(const AIndex, AChar: Integer;
  const AFontName: TFontName; const AFontColor: TColor; AMaskColor: TColor);
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := IconFontItems.Items[AIndex];
  if Assigned(LIconFontItem) then
    LIconFontItem.FontIconDec := AChar;
  OnItemChanged(LIconFontItem);
end;

procedure TIconFontsImageCollection.Replace(const AIndex: Integer;
  const AChar: WideChar; const AFontName: TFontName; const AFontColor: TColor;
  AMaskColor: TColor);
begin
  Replace(AIndex, Ord(AChar), AFontName, AFontColor, AMaskColor);
end;

procedure TIconFontsImageCollection.Assign(Source: TPersistent);
begin
  if Source is TIconFontsImageCollection then
  begin
    TIconFontsImageCollection(Source).FIconFontItems.Assign(FIconFontItems);
    OnItemChanged(nil);
  end
  else
    inherited;
end;

function TIconFontsImageCollection.AddIcons(const AFrom, ATo: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clDefault; AMaskColor: TColor = clNone;
  const ACheckValid: Boolean = False): Integer;
var
  LFontName: TFontName;
begin
  if AFontName <> '' then
    LFontName := AFontName
  else
    LFontName := FFontName;
  CheckFontName(LFontName);
  Result := FIconFontItems.AddIcons(AFrom, ATo, LFontName,
    AFontColor, AMaskColor);
  OnItemChanged(nil);
end;

function TIconFontsImageCollection.AddIcon(const AChar: Integer;
  const AFontName: TFontName; const AFontColor,
  AMaskColor: TColor): TIconFontItem;
begin
  Result := AddIcon(AChar, '', AFontName, AFontColor, AMaskColor);
end;

function TIconFontsImageCollection.AddIcon(const AChar: WideChar;
  const AFontName: TFontName; const AFontColor,
  AMaskColor: TColor): TIconFontItem;
begin
  Result := AddIcon(Ord(AChar), AFontName, AFontColor, AMaskColor);
end;

function TIconFontsImageCollection.AddIcon(const AChar: Integer;
  const AIconName: string; const AFontName: TFontName; const AFontColor,
  AMaskColor: TColor): TIconFontItem;
begin
  Result := IconFontItems.AddIcon(AChar, AIconName, AFontName, AFontColor, AMaskColor);
  OnItemChanged(Result);
end;

procedure TIconFontsImageCollection.ClearIcons;
begin
  IconFontItems.Clear;
  OnItemChanged(nil);
end;

end.
