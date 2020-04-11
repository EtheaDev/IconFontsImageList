{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
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
unit IconFontsImageListEditor;

interface

uses
  SysUtils
  , Classes
  , Graphics
  , DesignIntf
  , DesignEditors;

type
  TIconFontsImageListCompEditor = class (TComponentEditor)
  private
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

implementation

uses
  ShellApi
  , Windows
  , IconFontsImageList
  , IconFontsImageListEditorUnit;

{ TIconFontsImageListCompEditor }

procedure TIconFontsImageListCompEditor.Edit;
begin
  inherited;
end;

procedure TIconFontsImageListCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EditIconFontsImageList(Component as TIconFontsImageList) then
      Designer.Modified;
  end
  else
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
end;

function TIconFontsImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'I&conFonts ImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[IconFontsImageListVersion]);
  end;
end;

function TIconFontsImageListCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
