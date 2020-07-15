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
unit IconFontsImageList;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Classes
  , ImgList
  , Windows
  , Graphics
{$IFDEF D10_4+}
  , System.UITypes
{$ENDIF}
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
{$IFDEF GDI+}
  , Winapi.GDIPOBJ
  , Winapi.GDIPAPI
{$ENDIF}
  , Forms;

resourcestring
  ERR_ICONFONTS_VALUE_NOT_ACCEPTED = 'Value %s not accepted!';
  ERR_ICONFONTS_FONT_NOT_INSTALLED = 'Font "%s" is not installed!';

const
  IconFontsImageListVersion = '2.1.0';
  DEFAULT_SIZE = 16;

type
  TIconFontsImageList = class;
  TIconFontMissing = procedure (const AFontName: TFontName) of object;

  TIconFont = class(TObject)
  private
    FFontName: TFontName;
    FFontIconDec: Integer;
    FFontColor: TColor;
    FMaskColor: TColor;
    FOpacity: Byte;
    FIconName: string;
    FDisabledFactor: Byte;
  public
    procedure Assign(Source: TIconFont);
    {$IFDEF GDI+}
    procedure PaintToGDI(const AGraphics: TGPGraphics;
      const X, Y, AWidth, AHeight: Single;
      const AFontName: TFontName; AFontIconDec: Integer;
      const AFontColor: TColor;
      const AEnabled: Boolean = True;
      const AOpacity: Byte = 255;
      const ADisabledFactor: Byte = 100);
    {$ENDIF}
    procedure PaintTo(const ACanvas: TCanvas;
      const X, Y, AWidth, AHeight: Integer;
      const AFontName: TFontName; AFontIconDec: Integer;
      const AFontColor, AMaskColor: TColor;
      const AEnabled: Boolean = True;
      const ADisabledFactor: Byte = 100);
  end;

  TIconFontItem = class(TCollectionItem)
  private
    FIconFont: TIconFont;
    FFontName: TFontName;
    FFontIconDec: Integer;
    FFontColor: TColor;
    FMaskColor: TColor;
    FIconName: string;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure SetIconName(const AValue: string);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontIconDec(const AValue: Integer);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure Changed;
    function GetCharacter: WideString;
    procedure UpdateIconAttributes(const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
    function GetIconFontsImageList: TIconFontsImageList;
    function StoreFontColor: Boolean;
    function StoreMaskColor: Boolean;
    function StoreFontName: Boolean;
    {$IFDEF GDI+}
    procedure PaintToGDI(const AGraphics: TGPGraphics;
      const X, Y, AWidth, AHeight: Integer;
      const AEnabled: Boolean = True;
      const AOpacity: Byte = 255;
      const ADisabledFactor: Byte = 100);
    {$ENDIF}
    procedure PaintTo(const ACanvas: TCanvas;
      const X, Y, AWidth, AHeight: Integer;
      out AMaskColor: TColor;
      const AEnabled: Boolean = True;
      const ADisabledFactor: Byte = 100);
    function GetIconFont: TIconFont;
  protected
    procedure SetIndex(Value: Integer); override;
  public
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Character: WideString read GetCharacter;
  published
    property IconFont: TIconFont read GetIconFont;
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property FontName: TFontName read FFontName write SetFontName stored StoreFontName;
    property FontColor: TColor read FFontColor write SetFontColor stored StoreFontColor;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored StoreMaskColor;
    property IconName: string read FIconName write SetIconName;
  end;

  {TIconFontItems}
  TIconFontItems = class(TOwnedCollection)
  private
    function GetItem(AIndex: Integer): TIconFontItem;
    procedure SetItem(AIndex: Integer; const Value: TIconFontItem);
    procedure UpdateImage(const AIndex: Integer);
    function GetIconFontsImageList: TIconFontsImageList;
  protected
  public
    function Add: TIconFontItem;
    procedure Assign(Source: TPersistent); override;
    function Insert(AIndex: Integer): TIconFontItem;
    procedure Delete(AIndex: Integer);
    function GetIconByName(const AIconName: string): TIconFontItem;
    function IndexOf(const S: string): Integer; virtual;
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property Items[Index: Integer]: TIconFontItem read GetItem write SetItem; default;
  end;

  {TIconFontsImageList}
  TDrawIconEvent = procedure (const ASender: TObject; const ACount: Integer;
    const AItem: TIconFontItem) of Object;

  TIconFontsImageList = class(TCustomImageList)
  private
    FStopDrawing: Integer;
    FIconFontItems: TIconFontItems;
    FFontName: TFontName;
    FMaskColor: TColor;
    FFontColor: TColor;
    FOnFontMissing: TIconFontMissing;
    FFontNamesChecked: TStrings;
    FOnDrawIcon: TDrawIconEvent;
    FIconsAdded: Integer;
    FDisabledFactor: Byte;
    {$IFDEF GDI+}
    FOpacity: Byte;
    {$ENDIF}
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    function GetNames(Index: Integer): string;
    procedure CheckFontName(const AFontName: TFontName);
    procedure SetIconSize(const ASize: Integer);
    procedure SetIconFontItems(const AValue: TIconFontItems);
    procedure UpdateImage(const AIndex: Integer);
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    procedure SetNames(Index: Integer; const Value: string);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    function IsCharAvailable(const ABitmap: TBitmap;
      const AFontIconDec: Integer): Boolean;
    function StoreWidth: Boolean;
    function StoreHeight: Boolean;
    function StoreSize: Boolean;
    {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: Messaging.TMessage);
    {$ENDIF}
    {$IFNDEF DXE8+}
    function GetCount: Integer;
    {$ENDIF}
    {$IFDEF GDI+}
    procedure SetOpacity(const Value: Byte);
    {$ENDIF}
    procedure SetDisabledFactor(const Value: Byte);
  protected
    {$IFDEF DXE8+}
    function GetCount: Integer; override;
    {$ENDIF}
    {$IFDEF GDI+}
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    {$ENDIF}
    procedure Loaded; override;
  public
    procedure RecreateBitmaps;
    procedure StopDrawing(const AStop: Boolean);
    procedure DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer); virtual;
    procedure Change; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Delete(const AIndex: Integer);
    procedure Replace(const AIndex: Integer; const AChar: WideChar;
      const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
      AMaskColor: TColor = clNone); overload;
    procedure Replace(const AIndex: Integer; const AChar: Integer;
      const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
      AMaskColor: TColor = clNone); overload;
    function AddIcon(const AChar: Integer; const AIconName: string;
      const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
      const AMaskColor: TColor = clNone): TIconFontItem; overload;
    function AddIcon(const AChar: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone): TIconFontItem; overload;
    function AddIcon(const AChar: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone): TIconFontItem; overload;
    //Multiple icons methods
    function AddIcons(const AFrom, ATo: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone;
      const ACheckValid: Boolean = False): Integer;  overload;
    function AddIcons(const AFrom, ATo: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone;
      const ACheckValid: Boolean = False): Integer;  overload;
    function AddIcons(const ASourceString: WideString;
      const AFontName: TFontName = ''): Integer; overload;
    procedure UpdateIconsAttributes(const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = ''); overload;
    procedure UpdateIconsAttributes(const ASize: Integer; const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = ''); overload;
    procedure ClearIcons; virtual;
    procedure RedrawImages; virtual;
    procedure SaveToFile(const AFileName: string);
    procedure PaintTo(const ACanvas: TCanvas; const AIndex: Integer;
      const X, Y, AWidth, AHeight: Integer;
      const AEnabled: Boolean = True;
      const ADisabledFactor: Byte = 100); overload;
    procedure PaintTo(const ACanvas: TCanvas; const AName: string;
      const X, Y, AWidth, AHeight: Integer;
      const AEnabled: Boolean = True;
      const ADisabledFactor: Byte = 100); overload;
    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function IsScaled: Boolean; override;
    function GetIndexByName(const AName: TImageName): TImageIndex; override;
    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}
    property Names[Index: Integer]: string read GetNames write SetNames;
    property Count: Integer read GetCount;
  published
    //Publishing properties of standard ImageList
    property Width: Integer read GetWidth write SetWidth stored StoreWidth default DEFAULT_SIZE;
    property Height: Integer read GetHeight write SetHeight stored StoreHeight default DEFAULT_SIZE;
    //Publishing properties of Custom Class
    property OnChange;
    //New properties
    property IconFontItems: TIconFontItems read FIconFontItems write SetIconFontItems;
    property DisabledFactor: Byte read FDisabledFactor write SetDisabledFactor default 100;
    property FontName: TFontName read FFontName write SetFontName;
    property FontColor: TColor read FFontColor write SetFontColor default clNone;
    property MaskColor: TColor read FMaskColor write SetMaskColor default clNone;
    property Size: Integer read GetSize write SetSize stored StoreSize default DEFAULT_SIZE;
    property OnFontMissing: TIconFontMissing read FOnFontMissing write FOnFontMissing;
    property OnDrawIcon: TDrawIconEvent read FOnDrawIcon write FOnDrawIcon;
    {$IFDEF HasStoreBitmapProperty}
    property StoreBitmap default False;
    {$ENDIF}
    {$IFDEF HiDPISupport}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
    {$IFDEF GDI+}
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    {$ENDIF}
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

{$IFDEF GDI+}
function GPColor(Col: TColor; Alpha: Byte): TGPColor;

type
  TInvRGBQUAD = packed record
  rgbRed: Byte;
  rgbGreen: Byte;
  rgbBlue: Byte;
  rgbReserved: Byte;
end;

var
  rec: TInvRGBQuad;
  rec2: TRGBQuad;
  ciCol: Cardinal;
begin
  { This will get the RGB color from a system colour, then we can
  convert this value to a GDI+ colour }
  rec := TInvRGBQuad(Col);
  if rec.rgbReserved = 128 then // $80 then {or =128}
  begin
  ciCol := $80000000 XOR DWORD(Col);
  ciCol := GetSysColor(ciCol);
  rec2 := TRGBQuad(ciCol);
  { Could also just use REC here, and pass Blue where red is
  requested, and Red where blue is requested, should probably
  still result in the same effect }
  Result := MakeColor(Alpha, rec2.rgbRed, rec2.rgbGreen, rec2.rgbBlue);
  end else
  Result := MakeColor(Alpha, rec.rgbRed, rec.rgbGreen, rec.rgbBlue);
end;
{$ENDIF}

{ TIconFontItem }

procedure TIconFontItem.Assign(Source: TPersistent);
begin
  if Source is TIconFontItem then
  begin
    FFontName := TIconFontItem(Source).FFontName;
    FFontIconDec := TIconFontItem(Source).FFontIconDec;
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
  FIconFont := TIconFont.Create;
  FFontIconDec := 0;
  FFontColor := clNone;
  FMaskColor := clNone;
end;

destructor TIconFontItem.Destroy;
var
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := IconFontsImageList;
  FIconFont.Free;
  inherited Destroy;
  if (LIconFontsImageList <> nil) and not (csDestroying in LIconFontsImageList.ComponentState) then
    LIconFontsImageList.RedrawImages;
end;

function TIconFontItem.StoreFontColor: Boolean;
begin
  Result := Assigned(IconFontsImageList) and
    (FFontColor <> IconFontsImageList.FFontColor) and
    (FFontColor <> clNone);
end;

function TIconFontItem.StoreFontName: Boolean;
begin
  Result := Assigned(IconFontsImageList) and
    (FFontName <> IconFontsImageList.FFontName) and
    (FFontName <> '');
end;

function TIconFontItem.StoreMaskColor: Boolean;
begin
  Result := Assigned(IconFontsImageList) and
    (FMaskColor <> IconFontsImageList.FMaskColor) and
    (FMaskColor <> clNone);
end;

function TIconFontItem.GetCharacter: WideString;
begin
{$IFDEF DXE3+}
  {$WARN SYMBOL_DEPRECATED OFF}
  Result := ConvertFromUtf32(FFontIconDec);
  {$WARN SYMBOL_DEPRECATED ON}
{$ELSE}
  Result := WideChar(FFontIconDec);
{$ENDIF}
end;

function TIconFontItem.GetDisplayName: string;
begin
  Result := Format('%s - Hex: %s%s',
    [FFontName, FontIconHex, ifthen(FIconName<>'', ' - ('+FIconName+')', '')]);
end;

function TIconFontItem.GetFontIconDec: Integer;
begin
  Result := FFontIconDec;
end;

function TIconFontItem.GetFontIconHex: string;
begin
  if FFontIconDec <> 0 then
    Result := RightStr('0000'+IntToHex(FFontIconDec, 1),5)
  else
    Result := '';
end;

function TIconFontItem.GetIconFont: TIconFont;
begin
  Result := FIconFont;
end;

function TIconFontItem.GetIconFontsImageList: TIconFontsImageList;
begin
  if Assigned(Collection) then
    Result := TIconFontItems(Collection).IconFontsImageList
  else
    Result := nil;
end;

procedure TIconFontsImageList.SetDisabledFactor(const Value: Byte);
begin
  if FDisabledFactor <> Value then
    FDisabledFactor := Value;
end;

{$IFDEF GDI+}
procedure TIconFontsImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean);
begin
  PaintTo(Canvas, Index, X, Y, Width, Height, Enabled);
end;

procedure TIconFontsImageList.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    RecreateBitmaps;
  end;
end;

procedure TIconFontItem.PaintToGDI(const AGraphics: TGPGraphics;
  const X, Y, AWidth, AHeight: Integer;
  const AEnabled: Boolean = True;
  const AOpacity: Byte = 255;
  const ADisabledFactor: Byte = 100);
var
  LFontColor: TColor;
  LFontName: TFontName;
begin
  if FFontColor <> clNone then
    LFontColor := FFontColor
  else
    LFontColor := IconFontsImageList.FFontColor;

  if FFontName <> '' then
    LFontName := FFontName
  else
    LFontName := IconFontsImageList.FFontName;
  IconFontsImageList.CheckFontName(LFontName);

  FIconFont.PaintToGDI(AGraphics, X, Y, AWidth, AHeight,
    LFontName, FFontIconDec, LFontColor,
    AEnabled, AOpacity, ADisabledFactor);

  if Assigned(IconFontsImageList.OnDrawIcon) then
    IconFontsImageList.OnDrawIcon(Self, IconFontsImageList.FIconsAdded, Self);
end;
{$ENDIF}

procedure TIconFontItem.PaintTo(const ACanvas: TCanvas;
  const X, Y, AWidth, AHeight: Integer;
  out AMaskColor: TColor;
  const AEnabled: Boolean = True;
  const ADisabledFactor: Byte = 100);
var
  LFontColor: TColor;
  LFontName: TFontName;
begin
  if IconFontsImageList = nil then
    Exit;

  //Default values from ImageList if not supplied from Item
  if FMaskColor <> clNone then
    AMaskColor := FMaskColor
  else
    AMaskColor := IconFontsImageList.FMaskColor;
  if FFontColor <> clNone then
    LFontColor := FFontColor
  else
    LFontColor := IconFontsImageList.FFontColor;

  if not AEnabled then
    LFontColor := GrayscaleColor(LFontColor);

  if FFontName <> '' then
    LFontName := FFontName
  else
    LFontName := IconFontsImageList.FFontName;
  IconFontsImageList.CheckFontName(LFontName);

  FIconFont.PaintTo(ACanvas, X, Y, AWidth, AHeight, LFontName,
    FFontIconDec, LFontColor, AMaskColor, AEnabled, ADisabledFactor);

  if Assigned(IconFontsImageList.OnDrawIcon) then
    IconFontsImageList.OnDrawIcon(Self, IconFontsImageList.FIconsAdded, Self);
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
  if AValue <> FFontIconDec then
  begin
    if not IsFontIconValidValue(AValue) then
      raise Exception.CreateFmt(ERR_ICONFONTS_VALUE_NOT_ACCEPTED,[IntToHex(AValue, 1)]);
    FFontIconDec := AValue;
    Changed;
  end;
end;

procedure TIconFontItem.SetFontIconHex(const AValue: string);
begin
  try
    if (Length(AValue) = 4) or (Length(AValue) = 5) then
      FontIconDec := StrToInt('$' + AValue)
    else if (Length(AValue) = 0) then
      FontIconDec := 0
    else
      raise Exception.CreateFmt(ERR_ICONFONTS_VALUE_NOT_ACCEPTED,[AValue]);
  except
    On E: EConvertError do
      raise Exception.CreateFmt(ERR_ICONFONTS_VALUE_NOT_ACCEPTED,[AValue])
    else
      raise;
  end;
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

procedure TIconFontItem.SetIndex(Value: Integer);
var
  LOldIndex: Integer;
begin
  LOldIndex := inherited Index;
  inherited;
  if (Index <> LOldIndex) and Assigned(IconFontsImageList) then
    IconFontsImageList.RedrawImages;
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
  const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
var
  LChanged: Boolean;
begin
  LChanged := False;
  //If AReplaceFontColor is false then the color of single icon is preserved
  if AReplaceFontColor and (FFontColor <> clNone) then
  begin
    FFontColor := AFontColor;
    LChanged := True;
  end;
  if AReplaceFontColor and (FMaskColor <> clNone) then
  begin
    FMaskColor := AMaskColor;
    LChanged := True;
  end;
  //Replace FontName only if passed and different for specific Font
  if (AFontName <> '') and (FFontName <> '') and (AFontName <> FFontName) then
  begin
    FFontName := AFontName;
    LChanged := True;
  end;
  if LChanged then
    Changed;
end;

procedure TIconFontItem.Changed;
begin
  if Assigned(Collection) and not (csLoading in IconFontsImageList.ComponentState) then
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

constructor TIconFontsImageList.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF D2010+}
  ColorDepth := cd32Bit;
  {$ENDIF}
  FStopDrawing := 0;
  FFontColor := clNone;
  FMaskColor := clNone;
  FDisabledFactor := 100;
  {$IFDEF GDI+}
  FOpacity := 255;
  {$ENDIF}
  FFontNamesChecked := TStringList.Create;
  FIconFontItems := TIconFontItems.Create(Self, TIconFontItem);
  {$IFDEF HasStoreBitmapProperty}
  StoreBitmap := False;
  {$ENDIF}
  {$IFDEF HiDPISupport}
  FScaled := True;
  FDPIChangedMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TChangeScaleMessage, DPIChangedMessageHandler);
  {$ENDIF}
end;

procedure TIconFontsImageList.Delete(const AIndex: Integer);
begin
  //Don't call inherited method of ImageList, to avoid errors
  if Assigned(IconFontItems) then
    IconFontItems.Delete(AIndex);
end;

destructor TIconFontsImageList.Destroy;
begin
  {$IFDEF HiDPISupport}
  TMessageManager.DefaultManager.Unsubscribe(TChangeScaleMessage, FDPIChangedMessageID);
  {$ENDIF}
  FreeAndNil(FFontNamesChecked);
  FreeAndNil(FIconFontItems);
  inherited;
end;

procedure TIconFontsImageList.Change;
begin
  //Optimization: Do not notify to components during redrawing of icons
  if FStopDrawing = 0 then
    inherited;
end;

procedure TIconFontsImageList.CheckFontName(const AFontName: TFontName);
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

procedure TIconFontsImageList.DPIChanged(Sender: TObject; const OldDPI, NewDPI: Integer);
var
  LSizeScaled: Integer;
begin
  LSizeScaled := MulDiv(Size, NewDPI, OldDPI);
  {$IFDEF D10_3+}
  FScaling := True;
  try
    SetSize(LSizeScaled);
  finally
    FScaling := False;
  end;
  {$ELSE}
    SetSize(LSizeScaled);
  {$ENDIF}
end;

{$IFDEF HiDPISupport}
procedure TIconFontsImageList.DPIChangedMessageHandler(const Sender: TObject;
  const Msg: Messaging.TMessage);
var
  LWidthScaled, LHeightScaled: Integer;
begin
  if FScaled and (TChangeScaleMessage(Msg).Sender = Owner) then
  begin
    LWidthScaled := MulDiv(Width, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    LHeightScaled := MulDiv(Height, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    FScaling := True;
    try
      if (Width <> LWidthScaled) or (Height <> LHeightScaled) then
      begin
        StopDrawing(True);
        try
          Width := LWidthScaled;
          Height := LHeightScaled;
        finally
          StopDrawing(False);
        end;
        RecreateBitmaps;
      end;
    finally
      FScaling := False;
    end;
  end;
end;
{$ENDIF}

procedure TIconFontsImageList.SetFontColor(const AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    UpdateIconsAttributes(FFontColor, FMaskColor, False);
  end;
end;

procedure TIconFontsImageList.SetFontName(const AValue: TFontName);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    UpdateIconsAttributes(FFontColor, FMaskColor, False);
  end;
end;

function TIconFontsImageList.GetCount: Integer;
begin
  Result := FIconFontItems.Count;
end;

procedure TIconFontsImageList.SetHeight(const AValue: Integer);
begin
  if Height <> AValue then
  begin
    inherited Height := AValue;
    RecreateBitmaps;
  end;
end;

function TIconFontsImageList.GetNames(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FIconFontItems.Count) then
    Result := FIconFontItems[Index].IconName
  else
    Result := '';
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
    UpdateIconsAttributes(FFontColor, FMaskColor, False);
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
  RecreateBitmaps;
  end;
end;

procedure TIconFontsImageList.SetWidth(const AValue: Integer);
begin
  if Width <> AValue then
  begin
    inherited Width := AValue;
    RecreateBitmaps;
  end;
end;

procedure TIconFontsImageList.StopDrawing(const AStop: Boolean);
begin
  if AStop then
    Inc(FStopDrawing)
  else
    Dec(FStopDrawing);
end;

function TIconFontsImageList.AddIcon(const AChar: WideChar;
  const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone): TIconFontItem;
begin
  Result := AddIcon(Ord(AChar), AFontName, AFontColor, AMaskColor);
end;

procedure TIconFontsImageList.PaintTo(const ACanvas: TCanvas;
  const AIndex: Integer;
  const X, Y, AWidth, AHeight: Integer;
  const AEnabled: Boolean = True;
  const ADisabledFactor: Byte = 100);
var
{$IFDEF GDI+}
  LGPGraphics: TGPGraphics;
{$ELSE}
  LMaskColor: TColor;
{$ENDIF}
  LItem: TIconFontItem;
begin
  if (AIndex >= 0) and (AIndex < FIconFontItems.Count) then
  begin
    LItem := FIconFontItems[AIndex];
    {$IFDEF GDI+}
    LGPGraphics := TGPGraphics.Create(ACanvas.Handle);
    try
      LItem.PaintToGDI(LGPGraphics, X, Y, AWidth, AHeight,
        AEnabled, FOpacity, ADisabledFactor);
    finally
      LGPGraphics.Free;
    end;
    {$ELSE}
    LItem.PaintTo(ACanvas, X, Y, AWidth, AHeight,
      LMaskColor, AEnabled, ADisabledFactor);
    {$ENDIF}
  end;
end;

procedure TIconFontsImageList.PaintTo(const ACanvas: TCanvas; const AName: string;
  const X, Y, AWidth, AHeight: Integer;
  const AEnabled: Boolean = True;
  const ADisabledFactor: Byte = 100);
var
  LIndex: Integer;
begin
  LIndex := FIconFontItems.IndexOf(Name);
  PaintTo(ACanvas, LIndex, X, Y, AWidth, AHeight,
    AEnabled, ADisabledFactor);
end;

function TIconFontsImageList.AddIcon(const AChar: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone): TIconFontItem;
begin
  Result := AddIcon(AChar, '', AFontName, AFontColor, AMaskColor);
end;

function TIconFontsImageList.AddIcon(const AChar: Integer;
  const AIconName: string; const AFontName: TFontName = '';
  const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone): TIconFontItem;
begin
  Result := FIconFontItems.Add;
  try
    Result.IconName := AIconName;
    Result.FontIconDec := AChar;
    if (AFontName <> '') and (AFontName <> FontName) then
      Result.FFontName := AFontName;
    if AFontColor <> clNone then
      Result.FFontColor := AFontColor;
    if AMaskColor <> clNone then
      Result.FMaskColor := AMaskColor;
  except
    FIconFontItems.Delete(Result.Index);
    raise;
  end;
end;

function TIconFontsImageList.AddIcons(const AFrom, ATo: WideChar;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone; AMaskColor: TColor = clNone;
  const ACheckValid: Boolean = False): Integer;
begin
  Result := AddIcons(Ord(AFrom), Ord(ATo), AFontName, AFontColor, AMaskColor, ACheckValid);
end;

function TIconFontsImageList.AddIcons(const AFrom, ATo: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone; AMaskColor: TColor = clNone;
  const ACheckValid: Boolean = False): Integer;
var
  LChar: Integer;
  LFontName: TFontName;
  LIsValid: Boolean;
  LBitmap: TBitmap;
begin
  CheckFontName(AFontName);
  StopDrawing(True);
  LBitmap := nil;
  try
    Result := 0;
    if ACheckValid then
    begin
      if AFontName <> '' then
        LFontName := AFontName
      else
        LFontName := FFontName;
      LBitmap := TBitmap.Create;
      LBitmap.Width := 10;
      LBitmap.Height := 10;
      with LBitmap.Canvas do
      begin
        Font.Name := LFontName;
        Font.Height := 10;
        Font.Color := clBlack;
        Brush.Color := clWhite;
      end;
    end;
    FIconsAdded := 0;
    for LChar := AFrom to ATo do
    begin
      if ACheckValid then
        LIsValid := IsFontIconValidValue(LChar) and IsCharAvailable(LBitmap, LChar)
      else
        LIsValid := IsFontIconValidValue(LChar);
      if LIsValid then
      begin
        AddIcon(LChar, AFontName, AFontColor, AMaskColor);
        Inc(Result);
      end;
    end;
    FIconsAdded := Result;
  finally
    LBitmap.Free;
    StopDrawing(False);
  end;
  RecreateBitmaps;
end;

procedure TIconFontsImageList.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TIconFontsImageList then
  begin
    StopDrawing(True);
    try
      FFontName := TIconFontsImageList(Source).FontName;
      FFontColor := TIconFontsImageList(Source).FontColor;
      FMaskColor := TIconFontsImageList(Source).FMaskColor;

      {$IFDEF GDI+}
      FDisabledFactor := TIconFontsImageList(Source).FDisabledFactor;
      FOpacity := TIconFontsImageList(Source).FOpacity;
      {$ENDIF}

      {$IFDEF HasStoreBitmapProperty}
      StoreBitmap := TIconFontsImageList(Source).StoreBitmap;
      {$ENDIF}
      Size := TIconFontsImageList(Source).Size;
      FIconFontItems.Assign(TIconFontsImageList(Source).FIconFontItems);
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;
end;

function TIconFontsImageList.IsCharAvailable(
  const ABitmap: TBitmap;
  const AFontIconDec: Integer): Boolean;
var
  Cnt: DWORD;
  len: Integer;
  buf : array of WORD;
  I: Integer;
  S: WideString;
  msBlank, msIcon: TMemoryStream;
  LIsSurrogate: Boolean;
  LRect: TRect;
begin
  Result := False;
  if Assigned(ABitmap) then
  begin
    LIsSurrogate := (AFontIconDec >= $010000) and (AFontIconDec <= $10FFFF);
    if LIsSurrogate then
    begin
      //To support surrogate Characters I cannot use GetGlyphIndicesW so I'm drawing the Character on a Canvas and
      //check if is already blank: this method is quite slow
      LRect := Rect(0,0, ABitmap.Width, ABitmap.Height);
      ABitmap.Canvas.FillRect(LRect);
      msBlank := TMemoryStream.Create;
      msIcon := TMemoryStream.Create;
      try
        ABitmap.SaveToStream(msBlank);
        {$IFDEF DXE3+}
        {$WARN SYMBOL_DEPRECATED OFF}
        S := ConvertFromUtf32(AFontIconDec);
        {$WARN SYMBOL_DEPRECATED ON}
        ABitmap.Canvas.TextOut(0, 0, S);
        {$ELSE}
        S := WideChar(AFontIconDec);
        TextOutW(ABitmap.Canvas.Handle, 0, 0, PWideChar(S), 1);
        {$ENDIF}
        ABitmap.SaveToStream(msIcon);
        Result := not ((msBlank.Size = msIcon.Size) and CompareMem(msBlank.Memory, msIcon.Memory, msBlank.Size));
      finally
        msBlank.Free;
        msIcon.Free;
      end;
    end
    else
    begin
      //Check for non surrogate pairs, using GetGlyphIndices
      S := WideChar(AFontIconDec);
      len := Length(S);
      SetLength( buf, len);
      {$IFDEF D2010+}
      Cnt := GetGlyphIndicesW( ABitmap.Canvas.Handle, PWideChar(S), len, @buf[0], GGI_MARK_NONEXISTING_GLYPHS);
      {$ELSE}
      {$WARN SUSPICIOUS_TYPECAST OFF}
      Cnt := GetGlyphIndicesW( ABitmap.Canvas.Handle, PAnsiChar(S), len, @buf[0], GGI_MARK_NONEXISTING_GLYPHS);
      {$ENDIF}
      if Cnt > 0 then
      begin
        for i := 0 to Cnt-1 do
          Result := buf[i] <> $FFFF;
      end;
    end;
  end;
end;

procedure TIconFontsImageList.SaveToFile(const AFileName: string);
var
  LImageStrip: TBitmap;
  LImageCount: Integer;
  LStripWidth, LStripHeight: Integer;

  procedure CreateLImageStrip(var AStrip: TBitmap);
  var
    I, J, K: Integer;
  begin
    with AStrip do
    begin
      Canvas.Brush.Color := MaskColor;
      Canvas.FillRect(Rect(0, 0, AStrip.Width, AStrip.Height));
      J := 0;
      K := 0;
      for I := 0 to Self.Count - 1 do
      begin
        Draw(Canvas, J * Width, K * Height, I, dsTransparent, itImage);
        Inc(J);
        if J >= LStripWidth then
        begin
          J := 0;
          Inc(K);
        end;
      end;
    end;
  end;

  procedure CalcDimensions(ACount: Integer; var AWidth, AHeight: Integer);
  var
    X: Double;
  begin
    X := Sqrt(ACount);
    AWidth := Trunc(X);
    if Frac(X) > 0 then
      Inc(AWidth);
    X := ACount / AWidth;
    AHeight := Trunc(X);
    if Frac(X) > 0 then
      Inc(AHeight);
  end;

begin
  LImageStrip := TBitmap.Create;
  try
    LImageCount := Count;
    CalcDimensions(LImageCount, LStripWidth, LStripHeight);
    LImageStrip.Width := LStripWidth * Size;
    LImageStrip.Height := LStripHeight * Size;
    CreateLImageStrip(LImageStrip);
    LImageStrip.SaveToFile(AFileName);
  finally
    LImageStrip.Free;
  end;
end;

{$IFDEF D10_4+}
function TIconFontsImageList.GetIndexByName(
  const AName: TImageName): TImageIndex;
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := IconFontItems.GetIconByName(AName);
  if Assigned(LIconFontItem) then
    Result := LIconFontItem.Index
  else
    Result := -1;
end;

function TIconFontsImageList.GetNameByIndex(AIndex: TImageIndex): TImageName;
begin
  Result := IconFontItems.Items[AIndex].IconName;
end;

function TIconFontsImageList.IsImageNameAvailable: Boolean;
begin
  Result := True;
end;

function TIconFontsImageList.IsScaled: Boolean;
begin
  Result := FScaled;
end;
{$ENDIF}

function TIconFontsImageList.GetSize: Integer;
begin
  Result := Max(Width, Height);
end;

function TIconFontsImageList.GetWidth: Integer;
begin
  Result := inherited Width;
end;

function TIconFontsImageList.GetHeight: Integer;
begin
  Result := inherited Height;
end;

procedure TIconFontsImageList.Loaded;
begin
  inherited;
  {$IFDEF HasStoreBitmapProperty}
  if (not StoreBitmap) then
    RecreateBitmaps;
  {$ENDIF}
  if (inherited Count = 0) or (csDesigning in ComponentState) then
    RecreateBitmaps;
end;

procedure TIconFontsImageList.SetNames(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index < FIconFontItems.Count) then
    FIconFontItems[Index].IconName := Value;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const ASize: Integer;
  const AFontColor, AMaskColor: TColor; const AReplaceFontColor: Boolean = False;
  const AFontName: TFontName = '');
var
  I: Integer;
  LIconFontItem: TIconFontItem;
begin
  if (AFontColor <> clNone) and (AMaskColor <> clNone) then
  begin
    StopDrawing(True);
    try
      SetIconSize(ASize);
      FFontColor := AFontColor;
      FMaskColor := AMaskColor;
      for I := 0 to IconFontItems.Count -1 do
      begin
        LIconFontItem := IconFontItems.Items[I];
        LIconFontItem.UpdateIconAttributes(FFontColor, FMaskColor, AReplaceFontColor, AFontName);
      end;
    finally
      StopDrawing(False);
    end;
    RecreateBitmaps;
  end;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const AFontColor,
  AMaskColor: TColor; const AReplaceFontColor: Boolean; const AFontName: TFontName);
begin
  UpdateIconsAttributes(Self.Size, AFontColor, AMaskColor, AReplaceFontColor, AFontName);
end;

procedure TIconFontsImageList.UpdateImage(const AIndex: Integer);
begin
  if (Height = 0) or (Width = 0) then
    Exit;
  if FStopDrawing > 0 then
    Exit;

  RecreateBitmaps;
end;

procedure TIconFontsImageList.RedrawImages;
begin
  if FStopDrawing > 0 then
    Exit;
  RecreateBitmaps;
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

procedure TIconFontsImageList.RecreateBitmaps;
var
  C: Integer;
  {$IFDEF GDI+}
  LIcon: HIcon;
  {$ENDIF}
  LMaskColor: TColor;
  LItem: TIconFontItem;

  procedure AddMaskedIcon(AIconFontItem: TIconFontItem;
    const AAdd: Boolean);
  var
    LBitmap: TBitmap;
  begin
    LBitmap := TBitmap.Create;
    try
      LBitmap.Width := Width;
      LBitmap.Height := Height;
      LBitmap.PixelFormat := pf32bit;
      {$IFDEF DXE+}
      LBitmap.alphaFormat := afIgnored;
      {$ENDIF}
      AIconFontItem.PaintTo(LBitmap.Canvas, 0, 0, Width, Height,
        LMaskColor, True, FDisabledFactor);
      if AAdd then
        AddMasked(LBitmap, LMaskColor)
      else
        ReplaceMasked(AIconFontItem.Index, LBitmap, LMaskColor);
    finally
      LBitmap.Free;
    end;
  end;

  {$IFDEF GDI+}
  function IconFontToIcon(AIconFontItem: TIconFontItem): HICON;

    function IconFontToIcon24: HIcon;
    var
      ColorBitmap, MaskBitmap: TBitmap;
      X: Integer;
      Y: Integer;
      Bits: PRGBQuad;
      IconInfo: TIconInfo;
      TransparentBitmap: TBitmap;
      BF: TBlendFunction;
      DC: THandle;
      Graphics: TGPGraphics;
    begin
      ColorBitmap := TBitmap.Create;
      MaskBitmap := TBitmap.Create;
      TransparentBitmap := TBitmap.Create;
      try
        TransparentBitmap.PixelFormat := pf32bit;
        TransparentBitmap.Width := Width;
        TransparentBitmap.Height := Height;
        FillChar(TransparentBitmap.Scanline[Height - 1]^, Width * Height * 4, 0);

        Graphics := TGPGraphics.Create(TransparentBitmap.Canvas.Handle);
        try
          AIconFontItem.PaintToGDI(Graphics, 0, 0, Width, Height, True, FOpacity);
        finally
          Graphics.Free;
        end;

        ColorBitmap.PixelFormat := pf32bit;
        ColorBitmap.Width := Width;
        ColorBitmap.Height := Height;
        MaskBitmap.PixelFormat := pf32bit;
        MaskBitmap.Width := Width;
        MaskBitmap.Height := Height;

        ColorBitmap.Canvas.Brush.Color := BkColor;
        ColorBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));

        BF.BlendOp := AC_SRC_OVER;
        BF.BlendFlags := 0;
        BF.SourceConstantAlpha := 255;
        BF.AlphaFormat := AC_SRC_ALPHA;
        AlphaBlend(ColorBitmap.Canvas.Handle, 0, 0, Width, Height,
          TransparentBitmap.Canvas.Handle, 0, 0, Width, Height, BF);

        DC := MaskBitmap.Canvas.Handle;
        for Y := 0 to Height - 1 do
        begin
          Bits := TransparentBitmap.ScanLine[Y];
          for X := 0 to Width - 1 do
          begin
            if Bits.rgbReserved = 0 then
              SetPixelV(DC, X, Y, clWhite)
            else
              SetPixelV(DC, X, Y, clBlack);
            Inc(Bits);
          end;
        end;

        IconInfo.fIcon := True;
        IconInfo.hbmColor := ColorBitmap.Handle;
        IconInfo.hbmMask := MaskBitmap.Handle;
        Result := CreateIconIndirect(IconInfo);
      finally
        TransparentBitmap.Free;
        ColorBitmap.Free;
        MaskBitmap.Free;
      end;
    end;

    function IconFontToIcon32: HICON;
    var
      Bitmap: TGPBitmap;
      Graphics: TGPGraphics;
    begin
      Bitmap := TGPBitmap.Create(Width, Height);
      Graphics := TGPGraphics.Create(Bitmap);
      AIconFontItem.PaintToGDI(Graphics, 0, 0, Width, Height, True, FOpacity);
      Graphics.Free;

      Bitmap.GetHICON(Result);
      Bitmap.Free;
    end;

  begin
    if GetFileVersion(comctl32) >= ComCtlVersionIE6 then
      Result := IconFontToIcon32
    else
      Result := IconFontToIcon24;
  end;
  {$ENDIF}

begin
  if not Assigned(FIconFontItems) or
    (FStopDrawing <> 0) or
    (csDestroying in ComponentState) or
    (csLoading in ComponentState) then
    Exit;
  StopDrawing(True);
  try
    begin
      {$IFDEF GDI+}
      ImageList_Remove(Handle, -1);
      if (Width > 0) and (Height > 0) then
      begin
        Handle := ImageList_Create(Width, Height,
          ILC_COLOR32 or (Integer(Masked) * ILC_MASK), 0, AllocBy);

        FIconsAdded := 0;
        for C := 0 to IconFontItems.Count - 1 do
        begin
          LItem := IconFontItems[C];
          if Assigned(LItem) then
          begin
            LIcon := IconFontToIcon(LItem);
            ImageList_AddIcon(Handle, LIcon);
            DestroyIcon(LIcon);
            Inc(FIconsAdded);
          end;
        end;
      end;
      {$ELSE}
      inherited Clear;
      FIconsAdded := 0;
      for C := 0 to FIconFontItems.Count -1 do
      begin
        LItem := IconFontItems[C];
        AddMaskedIcon(LItem, True);
        Inc(FIconsAdded);
      end;
      {$ENDIF}
    end;
  finally
    StopDrawing(False);
    Change;
  end;
end;

procedure TIconFontsImageList.Replace(const AIndex: Integer; const AChar: WideChar;
  const AFontName: TFontName; const AFontColor: TColor; AMaskColor: TColor);
begin
  Replace(AIndex, Ord(AChar), AFontName, AFontColor, AMaskColor);
end;

procedure TIconFontsImageList.Replace(const AIndex: Integer; const AChar: Integer;
  const AFontName: TFontName; const AFontColor: TColor; AMaskColor: TColor);
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := IconFontItems.GetItem(AIndex);
  if Assigned(LIconFontItem) then
  begin
    LIconFontItem.FontIconDec := AChar;
    LIconFontItem.UpdateIconAttributes(AFontColor, AMaskColor,
      True, AFontName);
  end;
end;

function TIconFontsImageList.AddIcons(const ASourceString: WideString;
  const AFontName: TFontName = ''): Integer;
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
  RedrawImages;
  {$ENDIF}
end;

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
    IconFontsImageList.RecreateBitmaps;
  end
  else
    inherited;
end;

procedure TIconFontItems.Delete(AIndex: Integer);
begin
  inherited Delete(AIndex);
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
  if Owner is TIconFontsImageList then
    Result := TIconFontsImageList(Owner)
  else
    Result := nil;  
end;

function TIconFontItems.GetItem(AIndex: Integer): TIconFontItem;
begin
  Result := TIconFontItem(inherited GetItem(AIndex));
end;

function TIconFontItems.IndexOf(const S: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(Items[Result].IconName, S) then
      Exit;
  Result := -1;
end;

function TIconFontItems.Insert(AIndex: Integer): TIconFontItem;
begin
  Result := TIconFontItem(inherited Insert(AIndex));
  UpdateImage(AIndex);
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

{ TIconFont }

procedure TIconFont.Assign(Source: TIconFont);
begin
  FFontName := Source.FFontName;
  FFontIconDec := Source.FFontIconDec;
  FFontColor := Source.FFontColor;
  FMaskColor := Source.FMaskColor;
  FOpacity := Source.FOpacity;
  FIconName := Source.FIconName;
  FDisabledFactor :=  Source.FDisabledFactor;
end;

procedure TIconFont.PaintTo(const ACanvas: TCanvas;
  const X, Y, AWidth, AHeight: Integer;
  const AFontName: TFontName; AFontIconDec: Integer;
  const AFontColor, AMaskColor: TColor;
  const AEnabled: Boolean; const ADisabledFactor: Byte);
var
  S: WideString;
  LRect: TRect;
  LFontColor: TColor;
begin
  if not AEnabled then
    LFontColor := DisabledColor(AFontColor, Round(100 * ADisabledFactor / 255))
  else
    LFontColor := AFontColor;

  with ACanvas do
  begin
    Font.Name := AFontName;
    Font.Height := AHeight;
    Font.Color := LFontColor;
    Brush.Color := AMaskColor;
    LRect.Left := X;
    LRect.Top := Y;
    {$IFDEF DXE8+}
    LRect.Width := AWidth;
    LRect.Height := AHeight;
    {$ELSE}
    LRect.Right := X + AWidth;
    LRect.Bottom := Y + AHeight;
    {$ENDIF}
    FillRect(LRect);
    {$IFDEF DXE3+}
    {$WARN SYMBOL_DEPRECATED OFF}
    S := ConvertFromUtf32(AFontIconDec);
    {$WARN SYMBOL_DEPRECATED ON}
    TextOut(X, Y, S);
    {$ELSE}
    S := WideChar(AFontIconDec);
    TextOutW(ACanvas.Handle, X, Y, PWideChar(S), 1);
    {$ENDIF}
  end;
end;

{$IFDEF GDI+}
procedure TIconFont.PaintToGDI(const AGraphics: TGPGraphics;
  const X, Y, AWidth, AHeight: Single;
  const AFontName: TFontName; AFontIconDec: Integer;
  const AFontColor: TColor; const AEnabled: Boolean;
  const AOpacity: Byte; const ADisabledFactor: Byte);
var
  LSolidBrush: TGPSolidBrush;
  LBounds: TGPRectF;
  LFont: TGPFont;
  LFontColor: TColor;
  LPoint: TGPPointF;
  S: WideString;
begin
  LSolidBrush := nil;
  LFont := nil;
  try
    LBounds.X := X;
    LBounds.Y := Y;
    LBounds.Width := AWidth;
    LBounds.Height := AHeight;

    if not AEnabled then
      LFontColor := DisabledColor(AFontColor, Round(100 * ADisabledFactor / 255))
    else
      LFontColor := AFontColor;

    LSolidBrush := TGPSolidBrush.Create(GPColor(LFontColor, AOpacity));

    LFont := TGPFont.Create(AFontName, LBounds.Height, FontStyleRegular, UnitPixel);

    AGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
    AGraphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    LPoint.X := LBounds.X - (LBounds.Width / 6);
    LPoint.Y := LBounds.Y;
    {$WARN SYMBOL_DEPRECATED OFF}
    S := ConvertFromUtf32(AFontIconDec);
    {$WARN SYMBOL_DEPRECATED ON}
    AGraphics.DrawString(S, Length(S), LFont, LPoint, LSolidBrush);
  finally
    LFont.Free;
    LSolidBrush.Free;
  end;
end;
{$ENDIF}

end.
