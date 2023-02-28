{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/VCL             }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{         Nicola Tambascia                                                     }
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
unit DImages;

{$INCLUDE IconFontsImageList.inc}

interface

uses
  WinApi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.ImgList,
  System.UITypes,
  System.ImageList, //if you are compiling with an older version of Delphi delete this line
  Vcl.BaseImageCollection, //if you are compiling with an older version of Delphi delete this line
  IconFontsImageCollection,
  Vcl.Controls;

type
  TdmImages = class(TDataModule)
    IconFontsImageCollection: TIconFontsImageCollection;
    procedure IconFontsImageCollectionFontMissing(const AFontName: TFontName);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FAutoAddFont: Boolean;
    FFontFileName: string;
  public
  end;

var
  dmImages: TdmImages;

implementation

{$R *.dfm}

uses
  Vcl.Graphics;

procedure TdmImages.DataModuleDestroy(Sender: TObject);
begin
  if FAutoAddFont then
  begin
    {$IFNDEF D2010+}
    RemoveFontResource(PChar(FFontFileName));
    {$ELSE}
    RemoveFontResource(PWideChar(FFontFileName));
    {$ENDIF}
    PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end;
end;

procedure TdmImages.IconFontsImageCollectionFontMissing(
  const AFontName: TFontName);
var
  LHWND: HWND;
begin
  inherited;
  //The "material desktop font is not installed into system: load and install now from disk
  FFontFileName := ExtractFilePath(ParamStr(0))+'..\Fonts\Material Design Icons Desktop.ttf';
  if FileExists(FFontFileName) then
  begin
    {$IFNDEF D2010+}
    AddFontResource(PChar(FFontFileName));
    {$ELSE}
    AddFontResource(PWideChar(FFontFileName));
    {$ENDIF}
    PostMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
    FAutoAddFont := True;
    {$IFDEF GDI+}
    //Wait for Font available on GDI+ collection for drawing...
    Sleep(500);
    {$ENDIF}
  end
  else
  begin
    //If the font file is not available
    raise Exception.CreateFmt('Warning: "%s" font is not present in your system!'+sLineBreak+
      'Please download at https://materialdesignicons.com and install it, because this demo is based on this font.',
        [AFontName]);
  end;
end;

end.
