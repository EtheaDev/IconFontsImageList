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
unit IconFontsImageListEditor;

{$INCLUDE ..\Source\IconFontsImageList.inc}

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
  end;

  TIconFontsVirtualImageListCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TIconFontsImageCollectionCompEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


  TIconFontsImageListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TIconFontsCollectionListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

implementation

uses
  ShellApi
  , Windows
  , IconFontsImageList
  , IconFontsImageListBase
  , IconFontsVirtualImageList
  , IconFontsImageCollection
  , IconFontsImageListEditorUnit
{$IFDEF D2010+}
  , MaterialFontConvert
{$ENDIF}
  , Dialogs;

{ TIconFontsImageListCompEditor }

procedure TIconFontsImageListCompEditor.ExecuteVerb(Index: Integer);
{$IFDEF D2010+}
var
  LConvertCount : integer;
  LMissingCount : integer;
{$ENDIF}
begin
  inherited;
  if Index = 0 then
  begin
    if (Component is TIconFontsImageListBase) and
      EditIconFontsImageList(TIconFontsImageListBase(Component)) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
{$IFDEF D2010+}
  end    
  else //Index = 2
  begin
    ConvertFont((Component as TIconFontsImageList), LConvertCount, LMissingCount);
    MessageDlg(Format(MSG_ICONFONTS_CONVERTED, [LConvertCount,LMissingCount]),
      mtInformation, [mbOK], 0);
    if LConvertCount > 0 then
      Designer.Modified;
{$ENDIF}
  end;
end;

function TIconFontsImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'I&conFonts ImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[IconFontsImageListVersion]);
{$IFDEF D2010+}
    2: Result := Format('Convert from "%s" to "%s"...',[OLD_FONT_NAME, NEW_FONT_NAME]);
{$ENDIF}
  end;
end;

function TIconFontsImageListCompEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TIconFontsImageListProperty }

procedure TIconFontsImageListProperty.Edit;
var
  SVGImageList: TIconFontsImageList;
begin
  SVGImageList := TIconFontsImageList(GetComponent(0));
  if EdiTIconFontsImageList(SVGImageList) then
    Modified;
end;

function TIconFontsImageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TIconFontsImageListProperty.GetValue: string;
begin
  Result := 'IconFonts';
end;


{ TIconFontsCollectionListProperty }

procedure TIconFontsCollectionListProperty.Edit;
var
  SVGImageCollection: TIconFontsImageCollection;
begin
  SVGImageCollection := TIconFontsImageCollection(GetComponent(0));
  if EditIconFontsImageCollection(SVGImageCollection) then
    Modified;
end;

function TIconFontsCollectionListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TIconFontsCollectionListProperty.GetValue: string;
begin
  Result := 'IconFontsImageCollection';
end;

{ TIconFontsImageCollectionCompEditor }

procedure TIconFontsImageCollectionCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EdiTIconFontsImageCollection(Component as TIconFontsImageCollection) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;

end;

function TIconFontsImageCollectionCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'I&conFonts ImageCollection Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[IconFontsImageListVersion]);
  end;
end;

function TIconFontsImageCollectionCompEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TIconFontsVirtualImageListCompEditor }

procedure TIconFontsVirtualImageListCompEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if Index = 0 then
  begin
    if EdiTIconFontsVirtualImageList(Component as TIconFontsVirtualImageList) then
      Designer.Modified;
  end
  else if Index = 1 then
  begin
    ShellExecute(0, 'open',
      PChar('https://github.com/EtheaDev/SVGIconImageList/wiki/Home'), nil, nil,
      SW_SHOWNORMAL)
  end;
end;

function TIconFontsVirtualImageListCompEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'I&conFonts VirtualImageList Editor...';
    1: Result := Format('Ver. %s - (c) Ethea S.r.l. - show help...',[IconFontsImageListVersion]);
  end;
end;

function TIconFontsVirtualImageListCompEditor.GetVerbCount: Integer;
begin
  result := 2;
end;

end.
