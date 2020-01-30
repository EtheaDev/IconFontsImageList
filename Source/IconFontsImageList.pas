{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Contributors:                                                          }
{         Carlo Barazzetta                                                     }
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
{$IFDEF HiDPISupport}
  , Messaging
{$ENDIF}
  , Forms;

const
  IconFontsImageListVersion = '1.3.0';

type
  TIconFontsImageList = class;
  TIconFontMissing = procedure (const AFontName: string) of object;

  TIconFontItem = class(TCollectionItem)
  private
    FFontName: TFontName;
    FCharacter: WideChar;
    FFontColor: TColor;
    FMaskColor: TColor;
    FIconName: string;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontName(const AValue: TFontName);
    procedure SetMaskColor(const AValue: TColor);
    procedure SetIconName(const AValue: string);
    procedure SetCharacter(const AValue: WideChar);
    procedure SetFontIconHex(const AValue: string);
    procedure SetFontIconDec(const AValue: Integer);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure Changed;
    function GetCharacter: WideChar;
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
    property Character: WideChar read GetCharacter write SetCharacter stored false default #0;
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
      AMaskColor: TColor = clNone);
    procedure AddIcon(const AChar: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone);  virtual;
    //Multiple icons methods
    function AddIcons(const AFrom, ATo: WideChar; const AFontName: TFontName = '';
      const AFontColor: TColor = clNone; AMaskColor: TColor = clNone): Integer;  virtual;
    procedure UpdateIconsAttributes(const AFontColor, AMaskColor: TColor;
      const AReplaceFontColor: Boolean = True; const AFontName: string = ''); virtual;
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
  FCharacter := WideChar(0);
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

function TIconFontItem.GetCharacter: WideChar;
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
  if FCharacter <> #0 then
    Result := IntToHex(Ord(FCharacter), 4)
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

procedure TIconFontItem.SetCharacter(const AValue: WideChar);
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
  Character := WideChar(AValue);
end;

procedure TIconFontItem.SetFontIconHex(const AValue: string);
begin
  if (Length(AValue) = 4) then
    Character := WideChar(StrToInt('$' + AValue))
  else if (Length(AValue) = 0) then
    Character := #0
  else
    raise Exception.CreateFmt('Value %s not accepted!',[AValue]);
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
  if Screen.Fonts.IndexOf(AFontName) = -1 then
  begin
    if Assigned(OnFontMissing) then
      OnFontMissing(AFontName) else
      raise Exception.CreateFmt('Font "%s" is not installed!',[AFontName]);
  end;
end;

{$IFDEF HiDPISupport}
procedure TIconFontsImageList.DPIChangedMessageHandler(const Sender: TObject;
  const Msg: Messaging.TMessage);
var
  W: Integer;
  //H: Integer;
begin
  if FScaled and (TChangeScaleMessage(Msg).Sender = Owner) then
  begin
    W := MulDiv(Width, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    //H := MulDiv(Height, TChangeScaleMessage(Msg).M, TChangeScaleMessage(Msg).D);
    FScaling := True;
    try
      SetSize(W);
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
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := FIconFontItems.Add;
  LIconFontItem.FCharacter := AChar;
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
var
  LChar: WideChar;
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
  end;
end;

procedure TIconFontsImageList.DrawFontIcon(const AIndex: Integer;
  const ABitmap: TBitmap; const AAdd: Boolean);
var
  LIconFontItem: TIconFontItem;
  {$IFDEF UNICODE}
  LCharWidth: Integer;
  LCharHeight: Integer;
  {$ELSE}
  S: WideString;
  {$ENDIF}
  LCharacter: WideChar;
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
    LCharacter := LIconFontItem.Character;
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
      LCharWidth := TextWidth(LCharacter);
      LCharHeight := TextHeight(LCharacter);
      TextOut((Width - LCharWidth) div 2, (Height - LCharHeight) div 2, LCharacter);
      {$ELSE}
      S := LCharacter;
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
  {$IFDEF HasStoreBitmapProperty}
  if (not StoreBitmap) then
    InternalRedrawImages;
  {$ENDIF}
  if (inherited Count = 0) or (csDesigning in ComponentState) then
    InternalRedrawImages;
end;

procedure TIconFontsImageList.UpdateIconsAttributes(const AFontColor,
  AMaskColor: TColor; const AReplaceFontColor: Boolean = True;
  const AFontName: string = '');
var
  I: Integer;
  LIconFontItem: TIconFontItem;
begin
  if (AFontColor <> clNone) and (AMaskColor <> clNone) then
  begin
    StopDrawing(True);
    try
      FFontColor := AFontColor;
      FMaskColor := AMaskColor;
      for I := 0 to IconFontItems.Count -1 do
      begin
        LIconFontItem := IconFontItems.Items[I];
        LIconFontItem.UpdateIconAttributes(FFontColor, FMaskColor, AReplaceFontColor, AFontName);
      end;
    finally
      InternalRedrawImages;
      StopDrawing(False);
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
  InternalRedrawImages;
end;

procedure TIconFontsImageList.InternalRedrawImages;
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

procedure TIconFontsImageList.Replace(const AIndex: Integer; const AChar: WideChar;
  const AFontName: TFontName; const AFontColor: TColor; AMaskColor: TColor);
var
  LIconFontItem: TIconFontItem;
begin
  LIconFontItem := IconFontItems.GetItem(AIndex);
  if Assigned(LIconFontItem) then
  begin
    LIconFontItem.Character := AChar;
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
    IconFontsImageList.RedrawImages;
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
