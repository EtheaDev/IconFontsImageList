{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/VCL             }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
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
unit UMainOld;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, IconFontsImageList, IconFontsItems, IconFontsImageListBase,
  IconFontsImage;

type
  TMainForm = class(TForm)
    IconFontsImageList: TIconFontsImageList;
    ActionList: TActionList;
    ChangeIconAction: TAction;
    Panel1: TPanel;
    SelectThemeRadioGroup: TRadioGroup;
    TopToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    Panel2: TPanel;
    DeleteButton: TBitBtn;
    ChangeIconButton: TBitBtn;
    ClientPanel: TPanel;
    TreeView: TTreeView;
    ImageView: TListView;
    ImageListLabel: TLabel;
    GroupBox1: TGroupBox;
    NumSpinEdit: TSpinEdit;
    Label2: TLabel;
    AssignIconsButton: TButton;
    DeleteIconAction: TAction;
    SliderPanel: TPanel;
    TrackBar: TTrackBar;
    IconSizeLabel: TLabel;
    ButtonsPanel: TPanel;
    ClearButton: TButton;
    ShowImageEditorButton: TButton;
    ChangeColorButton: TBitBtn;
    ChangeColorAction: TAction;
    ColorDialog: TColorDialog;
    DisabledAction: TAction;
    ShowCharMapButton: TBitBtn;
    ShowCharMapAction: TAction;
    IconFontImage: TIconFontImage;
    procedure AssignIconsButtonClick(Sender: TObject);
    procedure ChangeIconActionExecute(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowImageEditorButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure DeleteIconActionExecute(Sender: TObject);
    procedure IconFontsImageListFontMissing(const AFontName: TFontName);
    procedure ChangeColorActionExecute(Sender: TObject);
    procedure ShowCharMapActionExecute(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FIconFontsImageListHot: TIconFontsImageList;
    {$IFDEF HiDPISupport}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    {$ENDIF}
    procedure UpdateButtons;
    procedure UpdateGUI;
    procedure UpdateListView;
    procedure UpdateTreeView;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Themes
  , IconFontsUtils
  , IconFontsCharMapUnit
  , IconFontsImageListEditorUnit;

procedure TMainForm.UpdateButtons;
begin
  DeleteButton.Action := DeleteIconAction;
  ChangeIconButton.Action := ChangeIconAction;
  ChangeColorButton.Action :=ChangeColorAction;
  ShowCharMapButton.Action :=ShowCharMapAction;
end;

procedure TMainForm.UpdateListView;
var
  LItemsCount: Integer;
begin
  LItemsCount := UpdateIconFontListView(ImageView);
  ImageListLabel.Caption := Format('Image List Preview: %d icons',[LItemsCount]);
end;

procedure TMainForm.AssignIconsButtonClick(Sender: TObject);
var
  LRand1, LRand2: Integer;
  LStart, LStop: cardinal;  
begin
  Screen.Cursor := crHourGlass;
  try
    ImageView.Clear;
    LRand1 := 61441+Random(4000);
    LRand2 := LRand1+NumSpinEdit.Value-1;

    (*
    //Test for Icons with surrogate pairs
    LRand1 := $F0100;
    LRand2 := $F0207;
    *)

    LStart := GetTickCount;
    //Generate Icons
    IconFontsImageList.AddIcons(
      LRand1, //From Chr
      LRand2, //To Chr
      'Material Design Icons'
      );
    LStop := GetTickCount;
    MessageDlg(Format('Built %d Icons in %d milliseconds!', 
      [LRand2-LRand1+1, LStop - LStart]), mtInformation, [mbOK], 0);
  finally
    Screen.Cursor := crDefault;
  end;
  UpdateGUI;
end;

procedure TMainForm.ChangeColorActionExecute(Sender: TObject);
begin
  ColorDialog.Color := IconFontsImageList.FontColor;
  if ColorDialog.Execute then
    IconFontsImageList.FontColor := ColorDialog.Color;
  UpdateGUI;
end;

procedure TMainForm.ChangeIconActionExecute(Sender: TObject);
var
  LAction: TAction;
  LItem: TIconFontItem;
begin
  //Detach Action
  ChangeIconButton.Action := nil;
  LAction := Sender as TAction;
  //Change icon of the connected action
  LAction.ImageIndex := 0;
  LItem := IconFontsImageList.IconFontItems[0];
  LItem.FontIconDec := LItem.FontIconDec+1;
  //Attach Action
  ChangeIconButton.Action := ChangeIconAction;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  //Clear Collection
  IconFontsImageList.ClearIcons;
  UpdateGUI;
end;

procedure TMainForm.DeleteIconActionExecute(Sender: TObject);
begin
  if IconFontsImageList.IconFontItems.Count > 0 then
  begin
    IconFontsImageList.IconFontItems.Delete(0);
    UpdateGUI;
  end;
end;

{$IFDEF HiDPISupport}
procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  UpdateGUI;
end;
{$ENDIF}

procedure TMainForm.FormCreate(Sender: TObject);
{$IFDEF DXE+}
var
  I: integer;
{$ENDIF}
begin
  FIconFontsImageListHot := TIconFontsImageList.Create(Self);

  {$IFDEF HiDPISupport}
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;
  {$ENDIF}

  {$IFDEF DXE+}
  //Build available VCL Styles
  SelectThemeRadioGroup.Items.Clear;
  for I := 0 to High(TStyleManager.StyleNames) do
    SelectThemeRadioGroup.Items.Add(TStyleManager.StyleNames[I]);
  TStringList(SelectThemeRadioGroup.Items).Sort;
  SelectThemeRadioGroup.OnClick := nil;
  try
    SelectThemeRadioGroup.ItemIndex := SelectThemeRadioGroup.Items.IndexOf('Windows');
  finally
    SelectThemeRadioGroup.OnClick := SelectThemeRadioGroupClick;
  end;
  {$ENDIF}
  SelectThemeRadioGroupClick(SelectThemeRadioGroup);

  TrackBar.Position := IconFontsImageList.Height;
  TrackBarChange(TrackBar);
end;

procedure TMainForm.IconFontsImageListFontMissing(const AFontName: TFontName);
var
  LFontFileName: string;
begin
  inherited;
  //The "material design web-font is not installed into system: load and install now from disk
  LFontFileName := ExtractFilePath(Application.ExeName)+'..\Fonts\materialdesignicons-webfont.ttf';
  if FileExists(LFontFileName) then
  begin
    {$IFNDEF D2010+}
    AddFontResource(PChar(LFontFileName));
    {$ELSE}
    AddFontResource(PWideChar(LFontFileName));
    {$ENDIF}
    SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end
  else
  begin
    //If the font file is not available
    MessageDlg(Format('Warning: "%s" font is not present in your system!'+sLineBreak+
      'Please download at https://materialdesignicons.com and install it, because this demo is based on this font.',
        [AFontName]), mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.SelectThemeRadioGroupClick(Sender: TObject);
var
  LStyleName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    LStyleName := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex];
    {$IFDEF DXE+}
    TStyleManager.TrySetStyle(LStyleName);
    UpdateIconFontsColorByStyle(IconFontsImageList);
    {$ELSE}
    if LStyleName = 'Black' then
      IconFontsImageList.UpdateIconsAttributes(clBlack, clBtnFace)
    else if LStyleName = 'Green' then
      IconFontsImageList.UpdateIconsAttributes(clGreen, clBtnFace)
    else if LStyleName = 'Blue' then
      IconFontsImageList.UpdateIconsAttributes(clBlue, clBtnFace)
    else if LStyleName = 'Silver' then
      IconFontsImageList.UpdateIconsAttributes(clSilver, clBtnFace)
    else if LStyleName = 'Olive' then
      IconFontsImageList.UpdateIconsAttributes(clOlive, clBtnFace)
    else if LStyleName = 'Red' then
      IconFontsImageList.UpdateIconsAttributes(clRed, clBtnFace);
    ImageView.Invalidate;
    TopToolBar.Invalidate;
    {$ENDIF}

    //Override default: use Windows 10 blue color for Windows and Windows10 Style
    if SameText(LStyleName,'Windows') or SameText(LStyleName,'Windows10') then
    begin
      IconFontsImageList.FontColor := RGB(0, 120, 215); //Windows 10 Blue
      IconFontsImageList.MaskColor := clBtnFace;
    end;

    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ShowCharMapActionExecute(Sender: TObject);
begin
  ShowIconFontsCharMap(IconFontsImageList.FontName,
    24, IconFontsImageList.FontColor, IconFontsImageList.MaskColor);
end;

procedure TMainForm.ShowImageEditorButtonClick(Sender: TObject);
begin
  EditIconFontsImageList(IconFontsImageList);
  UpdateGUI;
end;

procedure TMainForm.updateGUI;
var
  LSize: Integer;
begin
  LSize := IconFontsImageList.Height;
  IconSizeLabel.Caption := Format('Icons size: %d',[LSize]);
  TopToolBar.ButtonHeight := LSize + 2;
  TopToolBar.ButtonWidth := LSize + 2;
  TopToolBar.Height := LSize + 6;
  TreeView.Indent := LSize;

  //Update attributes for Hot ImageList for the Toolbar
  UpdateHotImageList(IconFontsImageList, FIconFontsImageListHot, 30, 10);
  TopToolBar.HotImages := FIconFontsImageListHot;

  UpdateButtons;
  UpdateListView;
  UpdateTreeView;
end;

procedure TMainForm.UpdateTreeView;
var
  LItem: TTreeNode;
  I: Integer;
begin
  for I := 0 to TreeView.Items.Count - 1 do
  begin
    LItem := TreeView.Items[I];
    if IconFontsImageList.IconFontItems.Count > LItem.ImageIndex then
      LItem.Text := IconFontsImageList.IconFontItems.Items[LItem.ImageIndex].IconName
    else
      LItem.Text := '';
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  //Resize all icons into ImageList
  IconFontsImageList.Size := TrackBar.Position;
  UpdateGUI;
end;

procedure TMainForm.ImageViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  IconFontImage.ImageIndex := Item.Index;
end;

initialization
  //ReportMemoryLeaksOnShutdown := True;

end.
