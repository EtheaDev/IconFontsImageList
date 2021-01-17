{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
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

uses
  Classes
  , DesignIntf
  , DesignEditors;

procedure Register;

implementation

uses
  IconFontsImageList
  , IconFontsVirtualImageList
  , IconFontsImageCollection
  , IconFontsImage
  , IconFontsImageListEditor;

procedure Register;
begin
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

end.
