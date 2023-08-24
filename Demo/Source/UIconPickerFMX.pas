{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/FireMonkey      }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2023 (Ethea S.r.l.)                                 }
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
unit UIconPickerFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.ImageList, FMX.ImgList, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
  FMX.Colors, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Objects;

type
  TIconPicker = class(TForm)
    edtChar: TEdit;
    edtColor: TColorComboBox;
    Preview: TGroupBox;
    Image16x16: TImage;
    Image32x32: TImage;
    Image64x64: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtFont: TEdit;
    Image90x90: TImage;
    procedure FormCreate(Sender: TObject);
    procedure edtCharChange(Sender: TObject);
    procedure edtColorChange(Sender: TObject);
  private
    FChar: WideChar;
    procedure UpdatePreview;
  public

    { Public declarations }
  end;

var
  IconPicker: TIconPicker;

implementation

{$R *.fmx}

procedure CopyIconFont(const FontName: string; const Character: WideChar;
  const Size: Integer; const Color: TAlphaColor; ImageViewer: TImage);
var
  S: WideString;
  b: TBitmap;
  f: TFont;
begin
  b := TBitmap.Create;
  f := TFont.Create;
  try
    f.Family := FontName;
    f.Size := Size;
    b.Width := Size;
    b.Height:= Size;
    b.Canvas.BeginScene;
    try
      b.Canvas.Clear(TAlphaColors.Null);
      b.Canvas.Fill.Color := Color;
      b.Canvas.Font.Assign(f);
      b.Canvas.FillText(TRectF.Create(0,0,Size,Size),Character,False,1,
        [TFillTextFlag.RightToLeft], TTextAlign.Leading, TTextAlign.Center);
    finally
      b.Canvas.EndScene;
    end;
    ImageViewer.Bitmap := b;
  finally
   b.Free;
   f.Free;
  end;
end;

procedure TIconPicker.edtCharChange(Sender: TObject);
begin
  if Length(edtChar.Text) = 4 then
  begin
    FChar := WideChar(StrToInt('$' + edtChar.Text));
    UpdatePreview;
  end;
end;

procedure TIconPicker.edtColorChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TIconPicker.FormCreate(Sender: TObject);
begin
  edtCharChange(edtChar);
end;

procedure TIconPicker.UpdatePreview;
begin
  CopyIconFont(edtFont.Text, FChar, 16, edtColor.Color, Image16x16);
  CopyIconFont(edtFont.Text, FChar, 32, edtColor.Color, Image32x32);
  CopyIconFont(edtFont.Text, FChar, 64, edtColor.Color, Image64x64);
  CopyIconFont(edtFont.Text, FChar, 90, edtColor.Color, Image90x90);
end;



end.
