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
unit UMainNew;

interface

{$INCLUDE IconFontsImageList.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns, DImages,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, IconFontsImageList, IconFontsImageListBase,
  IconFontsItems, IconFontsVirtualImageList, IconFontsImageCollection,
  System.ImageList, System.Actions, Vcl.VirtualImageList,
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
    NewFormAction: TAction;
    NewFormButton: TButton;
    VirtualImageList: TVirtualImageList;
    ZoomPanel: TPanel;
    ZoomLabel: TLabel;
    ZoomTrackBar: TTrackBar;
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
    procedure ZoomTrackBarChange(Sender: TObject);
  private
    FIconFontsVirtualImageListHot: TIconFontsVirtualImageList;
    {$IFDEF HiDPISupport}
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
    {$ENDIF}
    procedure UpdateButtons;
    procedure UpdateGUI(UpdateIcons: Boolean = True);
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
  , UITypes
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
  LRand1, LRand2, LCount: Integer;
  LStart, LStop: cardinal;
begin
  Screen.Cursor := crHourGlass;
  try
    ImageView.Clear;
    LRand1 := $F0001+Random(5000);
    LRand2 := LRand1+NumSpinEdit.Value-1;

    LStart := GetTickCount;
    //Generate Icons
    dmImages.IconFontsImageCollection.AddIcons(
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
  ColorDialog.Color := dmImages.IconFontsImageCollection.FontColor;
  if ColorDialog.Execute then
    dmImages.IconFontsImageCollection.FontColor := ColorDialog.Color;
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
  LItem := dmImages.IconFontsImageCollection.IconFontItems[0];
  LItem.FontIconDec := LItem.FontIconDec+1;
  //Attach Action
  ChangeIconButton.Action := ChangeIconAction;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  //Clear Collection
  VirtualImageList.Clear;
  dmImages.IconFontsImageCollection.ClearIcons;
  UpdateGUI;
end;

procedure TMainForm.DeleteIconActionExecute(Sender: TObject);
begin
  if VirtualImageList.Count > 0 then
  begin
    VirtualImageList.Delete(0);
    UpdateGUI;
  end;
end;

{$IFDEF HiDPISupport}
procedure TMainForm.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  UpdateGUI(False);
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
  SelectThemeRadioGroupClick(SelectThemeRadioGroup);

  TrackBar.Position := VirtualImageList.Height;
  TrackBarChange(TrackBar);
  ZoomTrackBar.Position := (VirtualImageList.ImageCollection as TIconFontsImageCollection).Zoom;
  IconFontImage.Zoom := ZoomTrackBar.Position;
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
    TStyleManager.TrySetStyle(LStyleName);
    //Override default: use Windows 10 blue color for Windows and Windows10 Style
    if SameText(LStyleName,'Windows') or SameText(LStyleName,'Windows10') then
      dmImages.IconFontsImageCollection.UpdateIconsAttributes(RGB(0, 120, 215), clBtnFace)
    else
      UpdateIconFontsColorByStyle(dmImages.IconFontsImageCollection);
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.ShowCharMapActionExecute(Sender: TObject);
begin
  ShowIconFontsCharMap(dmImages.IconFontsImageCollection.FontName,
    32, dmImages.IconFontsImageCollection.FontColor, dmImages.IconFontsImageCollection.MaskColor);
end;

procedure TMainForm.ShowImageEditorButtonClick(Sender: TObject);
begin
  //Image Editor for Image Collection
  EditIconFontsImageCollection(dmImages.IconFontsImageCollection);

  UpdateGUI;
end;

procedure TMainForm.updateGUI(UpdateIcons: Boolean = True);
var
  LSize: Integer;
begin
  if UpdateIcons then
  begin
    //Assign all icons to VirtualImageList
    VirtualImageList.Add('', 0, dmImages.IconFontsImageCollection.Count-1);
  end;

  LSize := VirtualImageList.Height;
  IconSizeLabel.Caption := Format('Icons size: %d',[LSize]);
  ZoomLabel.Caption := Format('Icons Zoom: %d%%',
    [(VirtualImageList.ImageCollection as TIconFontsImageCollection).Zoom]);
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
    if dmImages.IconFontsImageCollection.Count > LItem.ImageIndex then
      LItem.Text := dmImages.IconFontsImageCollection.IconFontItems[LItem.ImageIndex].IconName
    else
      LItem.Text := '';
  end;
end;

procedure TMainForm.ZoomTrackBarChange(Sender: TObject);
begin
  (VirtualImageList.ImageCollection as TIconFontsImageCollection).Zoom :=
    ZoomTrackBar.Position;
  IconFontImage.Zoom := ZoomTrackBar.Position;
  UpdateGUI(False);
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  //Resize all icons into ImageList
  VirtualImageList.SetSize(TrackBar.Position, TrackBar.Position);
  UpdateGUI(False);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
