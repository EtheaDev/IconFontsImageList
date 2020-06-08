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
  , Forms;

resourcestring
  ERR_ICONFONTS_VALUE_NOT_ACCEPTED = 'Value %s not accepted!';
  ERR_ICONFONTS_FONT_NOT_INSTALLED = 'Font "%s" is not installed!';

const
  IconFontsImageListVersion = '1.10.0';

type
  TIconFontsImageList = class;
  TIconFontMissing = procedure (const AFontName: TFontName) of object;

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
      const AReplaceFontColor: Boolean = False; const AFontName: TFontName = '');
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
    property Character: WideString read GetCharacter;
  published
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
    property IconFontsImageList: TIconFontsImageList read GetIconFontsImageList;
    property Items[Index: Integer]: TIconFontItem read GetItem write SetItem; default;
  end;

  {TIconFontsImageList}
  TDrawIconEvent = procedure (const ASender: TObject; const ACount: Integer;
    const AItem: TIconFontItem; var AProceed: Boolean) of Object;

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
    {$IFDEF HiDPISupport}
    FScaled: Boolean;
    FDPIChangedMessageID: Integer;
    {$ENDIF}
    procedure CheckFontName(const AFontName: TFontName);
    procedure SetIconSize(const ASize: Integer);
    procedure SetIconFontItems(const AValue: TIconFontItems);
    procedure UpdateImage(const AIndex: Integer);
    function DrawFontIcon(const AIndex: Integer; const ABitmap: TBitmap;
      const AAdd: Boolean): Boolean;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    function GetSize: Integer;
    procedure SetSize(const AValue: Integer);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);
    procedure InternalRedrawImages;
    function IsCharAvailable(const ABitmap: TBitmap;
      const AFontIconDec: Integer): Boolean;
    {$IFDEF HiDPISupport}
    procedure DPIChangedMessageHandler(const Sender: TObject; const Msg: Messaging.TMessage);
    {$ENDIF}
  protected
    procedure Loaded; override;
  public
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
    {$IFDEF D10_4+}
    function IsImageNameAvailable: Boolean; override;
    function IsScaled: Boolean; override;
    function GetIndexByName(const AName: TImageName): TImageIndex; override;
    function GetNameByIndex(AIndex: TImageIndex): TImageName; override;
    {$ENDIF}
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
    property OnDrawIcon: TDrawIconEvent read FOnDrawIcon write FOnDrawIcon;
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


implementation

uses
  SysUtils
  , Math
  {$IFDEF DXE3+}
  , System.Character
  , GDIPOBJ
  , GDIPAPI
  {$ENDIF}
  , StrUtils
  ;

function IsValidValue(const AFontIconDec: Integer): Boolean;
begin
  Result := ((AFontIconDec >= $0000) and (AFontIconDec <= $D7FF)) or
    ((AFontIconDec >= $E000) and (AFontIconDec < $FFFF)) or  //D800 to DFFF are reserved for code point values for Surrogate Pairs
    ((AFontIconDec >= $010000) and (AFontIconDec <= $10FFFF)); //Surrogate Pairs
end;

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
    if not IsValidValue(AValue) then
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

function TIconFontsImageList.AddIcon(const AChar: WideChar;
  const AFontName: TFontName = ''; const AFontColor: TColor = clNone;
  const AMaskColor: TColor = clNone): TIconFontItem;
begin
  Result := AddIcon(Ord(AChar), AFontName, AFontColor, AMaskColor);
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
        LIsValid := IsValidValue(LChar) and IsCharAvailable(LBitmap, LChar)
      else
        LIsValid := IsValidValue(LChar);
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

function TIconFontsImageList.DrawFontIcon(const AIndex: Integer;
  const ABitmap: TBitmap; const AAdd: Boolean): Boolean;
var
  LIconFontItem: TIconFontItem;
  S: WideString;
  LFontName: TFontName;
  LMaskColor, LFontColor: TColor;
  LRect: TRect;
begin
  Result := False;
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
      {$IFDEF DXE3+}
      S := LIconFontItem.Character;
      TextOut(0, 0, S);
      {$ELSE}
      S := WideChar(LIconFontItem.FFontIconDec);
      TextOutW(ABitmap.Canvas.Handle, 0, 0, PWideChar(S), 1);
      {$ENDIF}
    end;
    if AAdd then
      AddMasked(ABitmap, LMaskColor)
    else
      ReplaceMasked(AIndex, ABitmap, LMaskColor);
    Result := True;
    if Assigned(FOnDrawIcon) then
      FOnDrawIcon(Self, FIconsAdded, LIconFontItem, Result);
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
    if not (csLoading in ComponentState) then
      InternalRedrawImages;
  end;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const AFontColor,
  AMaskColor: TColor; const AReplaceFontColor: Boolean; const AFontName: TFontName);
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
      begin
        if not DrawFontIcon(I, LBitmap, True) then
          Break;
      end;
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

end.
