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
unit UMain;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns, DImages,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, IconFontsImageList, IconFontsImageListBase,
  IconFontsItems, IconFontsVirtualImageList, IconFontsImageCollection,
  System.ImageList, //if you are compiling with an older version of Delphi delete this line
  System.Actions, //if you are compiling with an older version of Delphi delete this line
  IconFontsImage;

type
  TMainForm = class(TForm)
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
    paButtons: TPanel;
    DeleteButton: TButton;
    ChangeIconButton: TButton;
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
    ChangeColorButton: TButton;
    ChangeColorAction: TAction;
    ColorDialog: TColorDialog;
    DisabledAction: TAction;
    ShowCharMapButton: TButton;
    ShowCharMapAction: TAction;
    Splitter: TSplitter;
    IconFontImage: TIconFontImage;
    IconFontsVirtualImageList: TIconFontsVirtualImageList;
    NewFormAction: TAction;
    NewFormButton: TButton;
    procedure AssignIconsButtonClick(Sender: TObject);
    procedure ChangeIconActionExecute(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowImageEditorButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure DeleteIconActionExecute(Sender: TObject);
    procedure ChangeColorActionExecute(Sender: TObject);
    procedure ShowCharMapActionExecute(Sender: TObject);
    procedure paButtonsResize(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure IconFontImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewFormActionExecute(Sender: TObject);
  private
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
  {$IFDEF DXE3+}
  , UITypes
  {$ENDIF}
  , IconFontsUtils
  , IconFontsCharMapUnit
  , IconFontsImageListEditorUnit;

procedure TMainForm.UpdateButtons;
begin
  DeleteButton.Action := DeleteIconAction;
  ChangeIconButton.Action := ChangeIconAction;
  ChangeColorButton.Action :=ChangeColorAction;
  ShowCharMapButton.Action :=ShowCharMapAction;
  NewFormButton.Action := NewFormAction;
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
    LRand1 := $F0001+Random(5000);
    LRand2 := LRand1+NumSpinEdit.Value-1;

    LStart := GetTickCount;
    //Generate Icons
    IconFontsVirtualImageList.AddIcons(
      LRand1, //From Chr
      LRand2, //To Chr
      'Material Design Icons Desktop'
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
  ColorDialog.Color := IconFontsVirtualImageList.FontColor;
  if ColorDialog.Execute then
    IconFontsVirtualImageList.FontColor := ColorDialog.Color;
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
  LItem := IconFontsVirtualImageList.IconFontItems[0];
  LItem.FontIconDec := LItem.FontIconDec+1;
  //Attach Action
  ChangeIconButton.Action := ChangeIconAction;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  //Clear Collection
  IconFontsVirtualImageList.ClearIcons;
  UpdateGUI;
end;

procedure TMainForm.DeleteIconActionExecute(Sender: TObject);
begin
  if IconFontsVirtualImageList.IconFontItems.Count > 0 then
  begin
    IconFontsVirtualImageList.IconFontItems.Delete(0);
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

  TrackBar.Position := IconFontsVirtualImageList.Height;
  TrackBarChange(TrackBar);
end;

procedure TMainForm.IconFontImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    IconFontImage.ImageIndex := IconFontImage.ImageIndex + 1
  else
    IconFontImage.ImageIndex := IconFontImage.ImageIndex - 1;
end;

procedure TMainForm.ImageViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Item.Index <> -1 then
    IconFontImage.ImageIndex := Item.Index;
end;

procedure TMainForm.NewFormActionExecute(Sender: TObject);
begin
  with TMainForm.Create(Application) do
    Show;
end;

procedure TMainForm.paButtonsResize(Sender: TObject);
begin
  IconFontImage.Height := IconFontImage.width;
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
    //Override default: use Windows 10 blue color for Windows and Windows10 Style
    if SameText(LStyleName,'Windows') or SameText(LStyleName,'Windows10') then
      IconFontsVirtualImageList.UpdateIconsAttributes(RGB(0, 120, 215), clBtnFace)
    else
      UpdateIconFontsColorByStyle(IconFontsVirtualImageList);
    {$ELSE}
    if LStyleName = 'Black' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clBlack, clBtnFace)
    else if LStyleName = 'Green' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clGreen, clBtnFace)
    else if LStyleName = 'Blue' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clBlue, clBtnFace)
    else if LStyleName = 'Silver' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clSilver, clBtnFace)
    else if LStyleName = 'Olive' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clOlive, clBtnFace)
    else if LStyleName = 'Red' then
      IconFontsVirtualImageList.UpdateIconsAttributes(clRed, clBtnFace);
    ImageView.Invalidate;
    TopToolBar.Invalidate;
    {$ENDIF}
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ShowCharMapActionExecute(Sender: TObject);
begin
  ShowIconFontsCharMap(IconFontsVirtualImageList.FontName,
    IconFontsVirtualImageList.Size, IconFontsVirtualImageList.FontColor, IconFontsVirtualImageList.MaskColor);
end;

procedure TMainForm.ShowImageEditorButtonClick(Sender: TObject);
begin
  //Image Editor for Image Collection
  //EditIconFontsImageCollection(dmImages.IconFontsImageCollection);

  //Image Editor for Image List
  EditIconFontsImageList(IconFontsVirtualImageList);

  UpdateGUI;
end;

procedure TMainForm.updateGUI;
var
  LSize: Integer;
begin
  LSize := IconFontsVirtualImageList.Height;
  IconSizeLabel.Caption := Format('Icons size: %d',[LSize]);
  TopToolBar.ButtonHeight := LSize + 2;
  TopToolBar.ButtonWidth := LSize + 2;
  TopToolBar.Height := LSize + 6;
  TreeView.Indent := LSize;
  Splitter.MinSize := DeleteButton.Width + 8;

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
    if IconFontsVirtualImageList.IconFontItems.Count > LItem.ImageIndex then
      LItem.Text := IconFontsVirtualImageList.IconFontItems.Items[LItem.ImageIndex].IconName
    else
      LItem.Text := '';
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  //Resize all icons into ImageList
  IconFontsVirtualImageList.Size := TrackBar.Position;
  UpdateGUI;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
