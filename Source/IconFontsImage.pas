{******************************************************************************}
{                                                                              }
{       Icon Fonts Image: useful to show an Icon Font as an Image              }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
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
unit IconFontsImage;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Windows
  , SysUtils
{$IFDEF D10_4+}
  , System.UITypes
{$ENDIF}
{$IFDEF GDI+}
  , Winapi.GDIPOBJ
{$ENDIF}
  , Classes
  , Graphics
  , Controls
  , ImgList
  , IconFontsImageListBase
  , IconFontsImageCollection
  , ActnList
  , IconFontsItems;

type
  TIconFontImage = class(TCustomControl)
  private
    FImageList: TCustomImageList;
    FCenter: Boolean;
    FStretch: Boolean;
    FScale: Double;
    FOpacity: Byte;
    FImageIndex: Integer;
    FIconFont: TIconFont;
    FFontName: TFontName;
    FFontColor: TColor;
    FMaskColor: TColor;
    FDisabledFactor: Byte;
    FFontIconDec: Integer;
    FIconName: string;
    FOnFontMissing: TIconFontMissing;
    procedure SetCenter(Value: Boolean);
    procedure SetOpacity(Value: Byte);
    procedure SetImageIndex(const AValue: Integer);
    procedure SetStretch(const AValue: Boolean);
    procedure SetScale(const AValue: Double);
    procedure SetImageList(const AValue: TCustomImageList);
    procedure SetFontName(const AValue: TFontName);
    function GetFontIconDec: Integer;
    function GetFontIconHex: string;
    procedure SetFontColor(const AValue: TColor);
    procedure SetFontIconDec(const AValue: Integer);
    procedure SetFontIconHex(const AValue: string);
    procedure SetIconName(const AValue: string);
    procedure SetMaskColor(const AValue: TColor);
    function StoreFontColor: Boolean;
    function StoreMaskColor: Boolean;
    function StoreScale: Boolean;
    function UsingIconFont: Boolean;
    procedure CheckFontName(const AFontName: TFontName);
    procedure SetDisabledFactor(const AValue: Byte);
    function OwnerFontName: TFontName;
    function OwnerFontColor: TColor;
    function OwnerMaskColor: TColor;
    function OwnerDisabledFactor: Byte;
    {$IFDEF GDI+}
    function OwnerOpacity: Byte;
    {$ENDIF}
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Empty: Boolean;
    procedure Assign(Source: TPersistent); override;
    function IconFontItems: TIconFontItems;
  published
    property Center: Boolean read FCenter write SetCenter default True;
    property Stretch: Boolean read FStretch write SetStretch default True;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property Scale: Double read FScale write SetScale stored StoreScale;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property FontName: TFontName read FFontName write SetFontName stored UsingIconFont;
    property FontIconDec: Integer read GetFontIconDec write SetFontIconDec stored true default 0;
    property FontIconHex: string read GetFontIconHex write SetFontIconHex stored false;
    property FontColor: TColor read FFontColor write SetFontColor stored StoreFontColor;
    property MaskColor: TColor read FMaskColor write SetMaskColor stored StoreMaskColor;
    property IconName: string read FIconName write SetIconName;
    property OnFontMissing: TIconFontMissing read FOnFontMissing write FOnFontMissing;
    property DisabledFactor: Byte read FDisabledFactor write SetDisabledFactor default 100;

   {$IFDEF D2010+}
    property ParentDoubleBuffered;
   {$ENDIF}
    property DoubleBuffered;
    property ParentBackground default True;
    property Enabled;
    property Visible;
    property Constraints;
    property Anchors;
    property Align;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation

uses
  IconFontsUtils
  {$IFDEF D10_3+}
  , Vcl.VirtualImageList
  {$ENDIF}
{$IFDEF GDI+}
  , Winapi.GDIPAPI
{$ENDIF}
  , Forms
  , StrUtils;

constructor TIconFontImage.Create(AOwner: TComponent);
begin
  inherited;
  ParentBackground := True;
  FIconFont := TIconFont.Create;
  FCenter := True;
  FStretch := True;
  FOpacity := 255;
  FScale := 1;
  FImageIndex := -1;
  FFontIconDec := 0;
  FDisabledFactor := 100;
  FFontColor := clDefault;
  FMaskColor := clNone;
end;

destructor TIconFontImage.Destroy;
begin
  FIconFont.Free;
  inherited;
end;

procedure TIconFontImage.CheckFontName(const AFontName: TFontName);
begin
  if AFontName <> '' then
  begin
    if (Screen.Fonts.IndexOf(AFontName) = -1) then
    begin
      if Assigned(OnFontMissing) then
        OnFontMissing(AFontName)
      else if not (csDesigning in ComponentState) then
        raise Exception.CreateFmt(ERR_ICONFONTS_FONT_NOT_INSTALLED,[AFontName]);
    end;
  end;
end;

procedure TIconFontImage.Clear;
begin
  FImageIndex := -1;
  Repaint;
end;

function TIconFontImage.Empty: Boolean;
begin
  Empty := FImageIndex < 0;
end;

function TIconFontImage.GetFontIconDec: Integer;
begin
  Result := FFontIconDec;
end;

function TIconFontImage.GetFontIconHex: string;
begin
  if FFontIconDec <> 0 then
    Result := RightStr('0000'+IntToHex(FFontIconDec, 1),5)
  else
    Result := '';
end;

function TIconFontImage.IconFontItems: TIconFontItems;
begin
  Result := nil;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).IconFontItems;
  {$IFDEF D10_3+}
  if (FImageList is TVirtualImageList) and
    (TVirtualImageList(FImageList).ImageCollection is TIconFontsImageCollection) then
    Result := TIconFontsImageCollection(TVirtualImageList(FImageList).ImageCollection).IconFontItems;
  {$ENDIF}
end;

function TIconFontImage.UsingIconFont: Boolean;
begin
  Result := not (Assigned(FImageList) and
    (IconFontItems <> nil) and
    (FImageIndex >= 0) and
     (FImageIndex < FImageList.Count));
end;

procedure TIconFontImage.Paint;
var
  LItem: TIconFontItem;
  LIconFont: TIconFont;
  LFontName: TFontName;
  LFontColor: TColor;
  LDisabledFactor: Byte;
  LFontIconDec: Integer;

  {$IFDEF GDI+}
  LOpacity: Byte;
  LBounds: TGPRectF;
  LGraphics: TGPGraphics;
  {$ELSE}
  LMaskColor: TColor;
  LBounds: TRect;
  {$ENDIF}

  procedure CalcWidth(const ImageWidth, ImageHeight: Double);
  begin
    if FStretch then
    begin
      {$IFDEF GDI+}
      LBounds := MakeRect(0.0, 0.0, Width, Height);
      {$ELSE}
      LBounds := Rect(0, 0, Width, Height);
      {$ENDIF}
      Exit;
    end
    else
    begin
      {$IFDEF DXE4+}
      LBounds.Width := Round(ImageWidth * FScale);
      LBounds.Height := Round(ImageHeight * FScale);
      {$ELSE}
      LBounds.Right := Round(ImageWidth * FScale);
      LBounds.Bottom := Round(ImageHeight * FScale);
      {$ENDIF}
    end;
  end;

  procedure CalcOffset;
  begin
    {$IFDEF GDI+}
    LBounds.X := 0;
    LBounds.Y := 0;
    if FCenter then
    begin
      LBounds.X := (Width - LBounds.Width) / 2;
      LBounds.Y := (Height - LBounds.Height) / 2;
    end;
    {$ELSE}
    LBounds.Left := 0;
    LBounds.Top := 0;
    if FCenter then
    begin
      {$IFDEF DXE8+}
      LBounds.Left := Round((Width - LBounds.Width) / 2);
      LBounds.Top := Round((Height - LBounds.Height) / 2);
      {$ELSE}
      LBounds.Left := Round((Width - LBounds.Right - LBounds.Left) / 2);
      LBounds.Top := Round((Height - LBounds.Bottom - LBounds.Top) / 2);
      {$ENDIF}
    end;
    {$ENDIF}
  end;

begin
  if not UsingIconFont then
  begin
    LItem := IconFontItems.Items[FImageIndex];
    LIconFont := LItem.IconFont;
    if LItem.FontName = '' then
      LFontName := OwnerFontName
    else
      LFontName := LItem.FontName;
    if FFontColor <> clDefault then
      LFontColor := FFontColor
    else if LItem.FontColor = clDefault then
      LFontColor := OwnerFontColor
    else
      LFontColor := LItem.FontColor;
    LDisabledFactor := OwnerDisabledFactor;
    {$IFDEF GDI+}
    LOpacity := OwnerOpacity;
    {$ELSE}
    if FMaskColor <> clNone then
      LMaskColor := FMaskColor
    else if LItem.MaskColor = clNone then
      LMaskColor := OwnerMaskColor
    else
      LMaskColor := LItem.MaskColor;
    {$ENDIF}
    LFontIconDec := LItem.FontIconDec;
    CalcWidth(FImageList.Width, FImageList.Height);
  end
  else
  begin
    CheckFontName(FFontName);
    LIconFont := FIconFont;
    LFontName := FFontName;
    LFontColor := FFontColor;
    LDisabledFactor := FDisabledFactor;
    {$IFDEF GDI+}
    LOpacity := FOpacity;
    {$ELSE}
    LMaskColor := FMaskColor;
    {$ENDIF}
    LFontIconDec := FFontIconDec;
    CalcWidth(Self.Width, Self.Height);
  end;

  CalcOffset;

  if not UsingIconFont or (LFontColor <> clDefault) then
  begin
    {$IFDEF GDI+}
    LGraphics := TGPGraphics.Create(Canvas.Handle);
    try
      LIconFont.PaintToGDI(LGraphics,
        LBounds.X, LBounds.Y, LBounds.Width, LBounds.Height, LFontName,
          LFontIconDec, LFontColor, Enabled, LDisabledFactor, LOpacity);
    finally
      LGraphics.Free;
    end;
    {$ELSE}
      {$IFDEF DXE8+}
      LIconFont.PaintTo(Self.Canvas,
        LBounds.Left, LBounds.Top, LBounds.Width, LBounds.Height, LFontName,
          LFontIconDec, LFontColor, LMaskColor, Enabled, LDisabledFactor);
      {$ELSE}
      LIconFont.PaintTo(Self.Canvas,
        LBounds.Left, LBounds.Top, LBounds.Right - LBounds.Left, LBounds.Bottom - LBounds.Top, LFontName,
          LFontIconDec, LFontColor, LMaskColor, Enabled, LDisabledFactor);
      {$ENDIF}
    {$ENDIF}
  end;

  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDash;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TIconFontImage.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImageList) then
  begin
    FImageList := nil;
    Repaint;
  end;
end;

function TIconFontImage.OwnerDisabledFactor: Byte;
begin
  Result := FDisabledFactor;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).DisabledFactor
  {$IFDEF D10_3+}
  else if (FImageList is TVirtualImageList) then
    Result := TVirtualImageList(FImageList).DisabledOpacity;
  {$ENDIF}
end;

function TIconFontImage.OwnerFontColor: TColor;
begin
  Result := clDefault;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).FontColor
  {$IFDEF D10_3+}
  else if (FImageList is TVirtualImageList) and
    (TVirtualImageList(FImageList).ImageCollection is TIconFontsImageCollection) then
    Result := TIconFontsImageCollection(TVirtualImageList(FImageList).ImageCollection).FontColor;
  {$ENDIF}
end;

function TIconFontImage.OwnerFontName: TFontName;
begin
  Result := FFontName;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).FontName
  {$IFDEF D10_3+}
  else if (FImageList is TVirtualImageList) and
    (TVirtualImageList(FImageList).ImageCollection is TIconFontsImageCollection) then
    Result := TIconFontsImageCollection(TVirtualImageList(FImageList).ImageCollection).FontName;
  {$ENDIF}
end;

function TIconFontImage.OwnerMaskColor: TColor;
begin
  Result := FMaskColor;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).MaskColor
  {$IFDEF D10_3+}
  else if (FImageList is TVirtualImageList) and
    (TVirtualImageList(FImageList).ImageCollection is TIconFontsImageCollection) then
    Result := TIconFontsImageCollection(TVirtualImageList(FImageList).ImageCollection).MaskColor;
  {$ENDIF}
end;

{$IFDEF GDI+}
function TIconFontImage.OwnerOpacity: Byte;
begin
  Result := FOpacity;
  if FImageList is TIconFontsImageListBase then
    Result := TIconFontsImageListBase(FImageList).Opacity;
end;
{$ENDIF}

procedure TIconFontImage.Assign(Source: TPersistent);
begin
  if (Source is TIconFontImage) then
  begin
    FIconFont.Assign(TIconFontImage(Source).FIconFont);
    FImageIndex := -1;
    Repaint;
  end;

  if (Source.ClassType = TIconFont) then
  begin
    FIconFont.Assign(TIconFont(Source));
    FImageIndex := -1;
    Repaint;
  end;
end;

procedure TIconFontImage.SetCenter(Value: Boolean);
begin
  if Value <> FCenter then
  begin
    FCenter := Value;
    Repaint;
  end;
end;

procedure TIconFontImage.SetDisabledFactor(const AValue: Byte);
begin
  if FDisabledFactor <> AValue then
  begin
    FDisabledFactor := AValue;
    if not Enabled then
      Repaint;
  end;
end;

procedure TIconFontImage.SetEnabled(Value: Boolean);
begin
  if Enabled <> Value then
  begin
    inherited;
    Repaint;
  end;
end;

procedure TIconFontImage.SetScale(const AValue: Double);
begin
  if AValue <> FScale then
  begin
    FScale := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetStretch(const AValue: Boolean);
begin
  if AValue <> FStretch then
  begin
    FStretch := AValue;
    Repaint;
  end;
end;

function TIconFontImage.StoreFontColor: Boolean;
begin
  Result := (FFontColor <> clDefault) and
    (not Assigned(FImageList) or
    (FFontColor <> OwnerFontColor));
end;

function TIconFontImage.StoreMaskColor: Boolean;
begin
  Result := (FMaskColor <> clNone) and
    (not Assigned(FImageList) or
    (FMaskColor <> OwnerMaskColor));
end;

function TIconFontImage.StoreScale: Boolean;
begin
  Result := FScale <> 1;
end;

procedure TIconFontImage.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Repaint;
  end;
end;

procedure TIconFontImage.SetFontColor(const AValue: TColor);
begin
  if FFontColor <> AValue then
  begin
    FFontColor := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetFontIconDec(const AValue: Integer);
begin
  if AValue <> FFontIconDec then
  begin
    if not IsFontIconValidValue(AValue) then
      raise Exception.CreateFmt(ERR_ICONFONTS_VALUE_NOT_ACCEPTED,[IntToHex(AValue, 1)]);
    FFontIconDec := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetFontIconHex(const AValue: string);
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

procedure TIconFontImage.SetFontName(const AValue: TFontName);
begin
  if FFontName <> AValue then
  begin
    FFontName := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetIconName(const AValue: string);
begin
  FIconName := AValue;
end;

procedure TIconFontImage.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetImageList(const AValue: TCustomImageList);
begin
  if FImageList <> AValue then
  begin
    FImageList := AValue;
    Repaint;
  end;
end;

procedure TIconFontImage.SetMaskColor(const AValue: TColor);
begin
  if FMaskColor <> AValue then
  begin
    FMaskColor := AValue;
    Repaint;
  end;
end;

end.
