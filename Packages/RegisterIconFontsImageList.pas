{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2025 (Ethea S.r.l.)                                 }
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
unit RegisterIconFontsImageList;

{$INCLUDE ..\Source\IconFontsImageList.inc}

interface

{$R ..\IconFontsImageListSplash.res}
uses
  Classes
  , DesignIntf
  , DesignEditors;

procedure Register;

implementation

uses
  SysUtils
  , ToolsAPI
  , Windows
  , Graphics
  , IconFontsImageListBase
  , IconFontsImageList
  , IconFontsVirtualImageList
  , IconFontsImageCollection
  , IconFontsImage
  , IconFontsImageListEditor
  , PngImage;

const
  {$IFDEF D11+}
  ABOUT_RES_NAME = 'ICONFONTSPLASH48PNG';
  SPLASH_RES_NAME = 'ICONFONTSPLASH48PNG';
  {$ELSE}
  ABOUT_RES_NAME = 'ICONFONTSPLASH24BMP';
  SPLASH_RES_NAME = 'ICONFONTSPLASH24BMP';
  {$ENDIF}
  RsAboutTitle = 'Ethea IconFontsImageList';
  RsAboutDescription = 'Ethea - IconFontsImageList Components - https://github.com/EtheaDev/IconFontsImageList/' + sLineBreak +
    'Four components to use Fonts as ImageList icons: (for VCL and FMX).';
  RsAboutLicense = 'Apache 2.0 (Free/Opensource)';
var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer;

{$IFDEF D11+}
function CreateBitmapFromPngRes(const AResName: string): TBitmap;
var
  LPngImage: TPngImage;
  LResStream: TResourceStream;
begin
  LPngImage := nil;
  try
    Result := TBitmap.Create;
    LPngImage := TPngImage.Create;
    LResStream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
    try
      LPngImage.LoadFromStream(LResStream);
      Result.Assign(LPngImage);
    finally
      LResStream.Free;
    end;
  finally
    LPngImage.Free;
  end;
end;

procedure RegisterAboutBox;
var
  LBitmap: TBitmap;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  LBitmap := CreateBitmapFromPngRes(ABOUT_RES_NAME);
  try
    AboutBoxIndex := AboutBoxServices.AddPluginInfo(
      RsAboutTitle+' '+IconFontsImageListVersion,
      RsAboutDescription, LBitmap.Handle, False, RsAboutLicense);
  finally
    LBitmap.Free;
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  LBitmap: TBitmap;
begin
  LBitmap := CreateBitmapFromPngRes(SPLASH_RES_NAME);
  try
    SplashScreenServices.AddPluginBitmap(
      RsAboutTitle+' '+IconFontsImageListVersion,
      LBitmap.Handle, False, RsAboutLicense, '');
  finally
    LBitmap.Free;
  end;
end;
{$ELSE}
procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ABOUT_RES_NAME);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(RsAboutTitle+' '+IconFontsImageListVersion, 
    RsAboutDescription, ProductImage, False, RsAboutLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterWithSplashScreen;
var
  ProductImage: HBITMAP;
begin
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), SPLASH_RES_NAME);
  SplashScreenServices.AddPluginBitmap(RsAboutTitle, ProductImage,
    False, RsAboutLicense);
end;
{$ENDIF}

procedure Register;
begin
  RegisterWithSplashScreen;

  RegisterComponents('Ethea', [
    TIconFontImage,
    TIconFontsImageCollection,
    TIconFontsVirtualImageList,
    TIconFontsImageList]);

  RegisterComponentEditor(TIconFontsImageList, TIconFontsImageListCompEditor);
  RegisterComponentEditor(TIconFontsVirtualImageList, TIconFontsImageListCompEditor);
  RegisterComponentEditor(TIconFontsImageCollection, TIconFontsImageCollectionCompEditor);
  RegisterPropertyEditor(TypeInfo(TIconFontsItems), TIconFontsImageList, 'IconFontItems', TIconFontsImageListProperty);
  RegisterPropertyEditor(TypeInfo(TIconFontsItems), TIconFontsImageCollection, 'IconFontItems', TIconFontsCollectionListProperty);
end;

initialization
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

end.
