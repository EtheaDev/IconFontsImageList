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
(*
{$IFDEF D10_3+}
  , BaseImageCollection
{$ENDIF}
*)
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
  , Forms;

resourcestring
  ERR_ICONFONTS_VALUE_NOT_ACCEPTED = 'Value %s not accepted!';
  ERR_ICONFONTS_FONT_NOT_INSTALLED = 'Font "%s" is not installed!';

const
  IconFontsImageListVersion = '1.5.0';

type
  TIconFontsImageList = class;
  TIconFontMissing = procedure (const AFontName: string) of object;

  TIconFontItem = class(TCollectionItem)
  private
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
      const AReplaceFontColor: Boolean = True; const AFontName: string = '');
    function GetIconFontsImageList: TIconFontsImageList;
    function StoreFontColor: Boolean;
    function StoreMaskColor: Boolean;
    function StoreFontName: Boolean;
  protected
    procedure SetIndex(Value: Integer); override;
  public
    function GetDisplayName: string; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property FontName: TFontName read FFontName write SetFontName stored StoreFontName;
    property FontColor: TColor read FFontColor write SetFontColor stored StoreFontColor;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored StoreMaskColor;
    property IconName: string read FIconName write SetIconName;
    property Character: WideString read GetCharacter;
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
    FOnFontMissing: TIconFontMissing;
    FFontNamesChecked: TStrings;
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    {$IFDEF NeedStoreBitmapProperty}
    FStoreBitmap: Boolean;
    {$ENDIF}
    procedure CheckFontName(const AFontName: string);
    procedure SetIconSize(const ASize: Integer);
    procedure SetIconFontItems(const AValue: TIconFontItems);
    procedure UpdateImage(const AIndex: Integer);
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
    procedure InternalRedrawImages;
    {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: Messaging.TMessage);
    {$ENDIF}
  protected
    {$IFDEF NeedStoreBitmapProperty}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    {$ENDIF}
    procedure Loaded; override;
  public
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
    procedure AddIcon(const AChar: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone); overload;
    procedure AddIcon(const AChar: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone); overload;
    //Multiple icons methods
    function AddIcons(const AFrom, ATo: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;  overload;
    function AddIcons(const AFrom, ATo: Integer; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;  overload;
    procedure UpdateIconsAttributes(const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = True; const AFontName: string = ''); overload;
    procedure UpdateIconsAttributes(const ASize: Integer; const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = True; const AFontName: string = ''); overload;
    procedure ClearIcons; virtual;
    procedure RedrawImages; virtual;
    procedure SaveToFile(const AFileName: string);
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
    property OnFontMissing: TIconFontMissing read FOnFontMissing write FOnFontMissing;
    {$IFDEF HasStoreBitmapProperty}
    property StoreBitmap default False;
    {$ENDIF}
    /// <summary>
    /// Enable and disable scaling with form
    /// </summary>
    {$IFDEF HiDPISupport}
    property Scaled: Boolean read FScaled write FScaled default True;
    {$ENDIF}
  end;

(*
{$IFDEF D10_3+}
  /// <summary>
  /// Component to store, scale and draw images.
  /// </summary>
  TIconFontsImageCollection = class(TCustomImageCollection)
  private
    FIconFontsImageList: TIconFontsImageList;
    procedure SetIconFontsImages(Value: TIconFontsImageList);
    procedure DoDraw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer;
      AProportional: Boolean);
  protected
    function GetCount: Integer; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsIndexAvailable(AIndex: Integer): Boolean; override;
    function GetIndexByName(const AName: String): Integer; override;
    function GetNameByIndex(AIndex: Integer): String; override;
    /// <summary>
    /// Get scaled to specific size TBitmap from item with specific index.
    /// </summary>
    function GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap; overload; override;
    /// <summary>
    /// Get scaled to specific size TBitmap from item with specific name.
    /// </summary>
    function GetBitmap(const AName: String; AWidth, AHeight: Integer; AEnabled: Boolean = True): TBitmap; overload;
    /// <summary>
    /// Draw image from collection item with specific index to specific rect and proportional parameter.
    /// </summary>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False); overload; override;
    /// <summary>
    /// Draw image from collection item with specific name to specific rect and proportional parameter.
    /// </summary>
    procedure Draw(ACanvas: TCanvas; ARect: TRect; const AName: String; AProportional: Boolean = False); overload;
  published
    /// <summary>
    /// Collection of items with source images.
    /// </summary>
    property IconFontsImages: TIconFontsImageList read FIconFontsImageList write SetIconFontsImages;
  end;
{$ENDIF}
*)

implementation

uses
  SysUtils
  , Math
  {$IFDEF UNICODE}
  , System.Character
  {$ENDIF}
  , StrUtils;

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
  FFontIconDec := 0;
  FFontColor := clNone;
  FMaskColor := clNone;
end;

destructor TIconFontItem.Destroy;
var
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := IconFontsImageList;
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
{$IFDEF UNICODE}
  {$WARN SYMBOL_DEPRECATED OFF}
  Result := ConvertFromUtf32(FFontIconDec);
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
    Result := IntToHex(FFontIconDec, 1)
  else
    Result := '';
end;

function TIconFontItem.GetIconFontsImageList: TIconFontsImageList;
begin
  if Assigned(Collection) then
    Result := TIconFontItems(Collection).IconFontsImageList
  else
    Result := nil;  
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
      FFontIconDec := 0
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
  const AReplaceFontColor: Boolean = True; const AFontName: string = '');
begin
  if (FFontColor <> AFontColor) or (FMaskColor <> AMaskColor) or (FFontName <> AFontName) or AReplaceFontColor then
  begin
    //If AReplaceFontColor is false then the color of single icon is preserved
    if AReplaceFontColor then
      FFontColor := AFontColor;
    //Always replace MaskColor
    FMaskColor := AMaskColor;
    //Replace FontName only if passed
    if (AFontName <> '') then
      FFontName := AFontName;
    Changed;
  end;
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
  FStopDrawing := 0;
  FFontColor := clNone;
  FMaskColor := clNone;
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

procedure TIconFontsImageList.CheckFontName(const AFontName: string);
begin
  if FFontNamesChecked.IndexOf(AFontName) = -1 then //Speed-up check of a Font already checked
  begin
    FFontNamesChecked.Add(AFontName);
    if Screen.Fonts.IndexOf(AFontName) = -1 then
    begin
      if Assigned(OnFontMissing) then
        OnFontMissing(AFontName) else
        raise Exception.CreateFmt(ERR_ICONFONTS_FONT_NOT_INSTALLED,[AFontName]);
    end
    else
      FFontNamesChecked.Add(AFontName);
  end;
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
      //Use Minimum value of scaled size
      SetSize(Min(LWidthScaled, LHeightScaled));
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
    if not (csLoading in ComponentState) then
      InternalRedrawImages;
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

procedure TIconFontsImageList.AddIcon(const AChar: WideChar;
  const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone);
begin
  AddIcon(Ord(AChar), AFontName, AFontColor, AMaskColor);
end;

procedure TIconFontsImageList.AddIcon(const AChar: Integer;
  const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone);
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := FIconFontItems.Add;
  LIconFontItem.FFontIconDec := AChar;
  if (AFontName <> '') and (AFontName <> FontName) then
    LIconFontItem.FFontName := AFontName;
  if AFontColor <> clNone then
    LIconFontItem.FFontColor := AFontColor;
  if AMaskColor <> clNone then
    LIconFontItem.FMaskColor := AMaskColor;
end;

function TIconFontsImageList.AddIcons(const AFrom, ATo: WideChar;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;
begin
  Result := AddIcons(Ord(AFrom), Ord(ATo), AFontName, AFontColor, AMaskColor);
end;

function TIconFontsImageList.AddIcons(const AFrom, ATo: Integer;
  const AFontName: TFontName = '';
  const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;
var
  LChar: Integer;
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
  if not (csLoading in ComponentState) then
    InternalRedrawImages;
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
      {$IFDEF HasStoreBitmapProperty}
      StoreBitmap := TIconFontsImageList(Source).StoreBitmap;
      {$ENDIF}
      Size := TIconFontsImageList(Source).Size;
      FIconFontItems.Assign(TIconFontsImageList(Source).FIconFontItems);
    finally
      StopDrawing(False);
    end;
    if not (csLoading in ComponentState) then
      InternalRedrawImages;
  end;
end;

procedure TIconFontsImageList.DrawFontIcon(const AIndex: Integer;
  const ABitmap: TBitmap; const AAdd: Boolean);
var
  LIconFontItem: TIconFontItem;
  {$IFNDEF UNICODE}
  S: WideString;
  {$ENDIF}
  LFontName: string;
  LMaskColor, LFontColor: TColor;
  LRect: TRect;
begin
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
    CheckFontName(LFontName);
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    with ABitmap.Canvas do
    begin
      Font.Name := LFontName;
      Font.Height := Height;
      Font.Color := LFontColor;
      Brush.Color := LMaskColor;
      LRect.Left := 0;
      LRect.Top := 0;
      LRect.Right := Width;
      LRect.Bottom := Height;
      FillRect(LRect);
      {$IFDEF UNICODE}
      TextOut(0, 0, LIconFontItem.Character);
      {$ELSE}
      S := WideChar(LIconFontItem.FFontIconDec);
      TextOutW(ABitmap.Canvas.Handle, 0, 0, PWideChar(S), 1);
      {$ENDIF}
    end;
    if AAdd then
      AddMasked(ABitmap, LMaskColor)
    else
      ReplaceMasked(AIndex, ABitmap, LMaskColor);
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
  if not (csLoading in ComponentState) then
  begin
    {$IFDEF HasStoreBitmapProperty}
    if (not StoreBitmap) then
      InternalRedrawImages;
    {$ENDIF}
    if (inherited Count = 0) or (csDesigning in ComponentState) then
      InternalRedrawImages;
  end;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const ASize: Integer;
  const AFontColor, AMaskColor: TColor; const AReplaceFontColor: Boolean = True;
  const AFontName: string = '');
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
    if not (csLoading in ComponentState) then
      InternalRedrawImages;
  end;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const AFontColor,
  AMaskColor: TColor; const AReplaceFontColor: Boolean; const AFontName: string);
begin
  UpdateIconsAttributes(Self.Size, AFontColor, AMaskColor, AReplaceFontColor, AFontName);
end;

procedure TIconFontsImageList.UpdateImage(const AIndex: Integer);
var
  LBitmap: TBitmap;
begin
  if (Height = 0) or (Width = 0) then
    Exit;
  LBitmap := TBitmap.Create;
  try
    if FStopDrawing > 0 then
      Exit;
    DrawFontIcon(AIndex, LBitmap, Count <= AIndex);
    Self.Change;
  finally
    LBitmap.Free;
  end;
end;

procedure TIconFontsImageList.RedrawImages;
begin
  if FStopDrawing > 0 then
    Exit;
  if not (csLoading in ComponentState) then
    InternalRedrawImages;
end;

procedure TIconFontsImageList.InternalRedrawImages;
var
  I: Integer;
  LBitmap: TBitmap;
begin
  if not Assigned(FIconFontItems) or
    (csDestroying in ComponentState) then
    Exit;
  StopDrawing(True);
  try
    inherited Clear;
    LBitmap := TBitmap.Create;
    try
      for I := 0 to FIconFontItems.Count -1 do
        DrawFontIcon(I, LBitmap, True);
      Self.Change;
    finally
      LBitmap.Free;
    end;
  finally
    StopDrawing(False);
  end;
  Change;
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
    IconFontsImageList.InternalRedrawImages;
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

{ TIconFontsImageCollection }
(*
{$IFDEF D10_3+}
function UpdateRectForProportionalSize(ARect: TRect; AWidth, AHeight: Integer; AStretch: Boolean): TRect;
var
  w, h, cw, ch: Integer;
  xyaspect: Double;
begin
  Result := ARect;
  if AWidth * AHeight = 0 then
    Exit;

  w := AWidth;
  h := AHeight;
  cw := ARect.Width;
  ch := ARect.Height;

  if AStretch or ((w > cw) or (h > ch)) then
  begin
    xyaspect := w / h;
    if w > h then
    begin
      w := cw;
      h := Trunc(cw / xyaspect);
      if h > ch then
      begin
        h := ch;
        w := Trunc(ch * xyaspect);
      end;
     end
     else
     begin
       h := ch;
       w := Trunc(ch * xyaspect);
       if w > cw then
       begin
         w := cw;
         h := Trunc(cw / xyaspect);
       end;
     end;
  end;

  Result := Rect(0, 0, w, h);
  OffsetRect(Result, ARect.Left + (cw - w) div 2, ARect.Top + (ch - h) div 2);
end;

constructor TIconFontsImageCollection.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TIconFontsImageCollection.Destroy;
begin
  inherited;
end;

procedure TIconFontsImageCollection.SetIconFontsImages(Value: TIconFontsImageList);
begin
  FIconFontsImageList := Value;
  if Assigned(FIconFontsImageList) then
    FIconFontsImageList.InternalRedrawImages;
end;

function TIconFontsImageCollection.GetCount: Integer;
begin
  if Assigned(FIconFontsImageList) then
    Result := FIconFontsImageList.IconFontItems.Count
  else
    Result := 0;
end;

function TIconFontsImageCollection.GetNameByIndex(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := IconFontsImages.IconFontItems[AIndex].IconName;
end;

function TIconFontsImageCollection.GetIndexByName(const AName: String): Integer;
var
  I: Integer;
  S: String;
begin
  Result := -1;
  S := LowerCase(AName);
  for I := 0 to FIconFontsImageList.Count - 1 do
    if LowerCase(FIconFontsImageList.IconFontItems[I].IconName) = S then
      Exit(I);
end;

function TIconFontsImageCollection.IsIndexAvailable(AIndex: Integer): Boolean;
begin
  Result := (Count > 0) and (AIndex >= 0) and (AIndex < Count);
end;

procedure TIconFontsImageCollection.Loaded;
begin
  inherited;
  if Assigned(FIconFontsImageList) then
    Change;
end;

procedure TIconFontsImageCollection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FIconFontsImageList <> nil) and
    (AComponent = FIconFontsImageList) then
    FIconFontsImageList := nil;
end;

function TIconFontsImageCollection.GetBitmap(AIndex: Integer; AWidth, AHeight: Integer): TBitmap;
begin
  Result := nil;
  if (AIndex < 0) or (AIndex > Count-1) then
    Exit;
  //Resize collection to obtain best picture
  FIconFontsImageList.Size := Min(AWidth, AHeight);
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.AlphaFormat := afDefined;
  Result.TransparentMode := tmFixed;
  if FIconFontsImageList.GetBitmap(AIndex, Result) then
  begin
    Result.TransparentColor := FIconFontsImageList.MaskColor;
    Result.Canvas.Font.Color := FIconFontsImageList.FontColor;
  end;
end;

function TIconFontsImageCollection.GetBitmap(const AName: String; AWidth, AHeight: Integer;
  AEnabled: Boolean = True): TBitmap;
begin
  Result := GetBitmap(GetIndexByName(AName), AWidth, AHeight);
end;

procedure TIconFontsImageCollection.DoDraw(ACanvas: TCanvas; ARect: TRect;
  AIndex: Integer; AProportional: Boolean);
var
  SourceImage: TBitmap;
begin
  if ARect.IsEmpty then
    Exit;
  SourceImage := GetBitmap(AIndex, ARect.Width, ARect.Height);
  if SourceImage <> nil then
  begin
    if AProportional then
      ARect := UpdateRectForProportionalSize(ARect, SourceImage.Width, SourceImage.Height, True);
    ACanvas.Draw(ARect.Left, ARect.Top, SourceImage);
  end;
end;

procedure TIconFontsImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; AIndex: Integer; AProportional: Boolean = False);
begin
  DoDraw(ACanvas, ARect, AIndex, AProportional);
end;

procedure TIconFontsImageCollection.Draw(ACanvas: TCanvas; ARect: TRect; const AName: String; AProportional: Boolean = False);
begin
  DoDraw(ACanvas, ARect, GetIndexByName(AName), AProportional);
end;


procedure TIconFontsImageCollection.Assign(Source: TPersistent);
begin
  if Source is TIconFontsImageCollection then
    TIconFontsImageCollection(Source).FIconFontsImageList.Assign(FIconFontsImageList)
  else
    inherited;
end;
{$ENDIF}
*)

end.
