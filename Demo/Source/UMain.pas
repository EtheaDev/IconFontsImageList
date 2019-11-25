{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019 (Ethea S.r.l.)                                      }
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
unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ImgList,
  StdCtrls, Buttons, StdActns,
  ActnList, ExtCtrls, ComCtrls, ToolWin,
  Spin, IconFontsImageList;

type
  TMainForm = class(TForm)
    IconFontsImageList: TIconFontsImageList;
    ActionList: TActionList;
    ChangeIconAction: TAction;
    Panel1: TPanel;
    SelectThemeRadioGroup: TRadioGroup;
    ShowImageEditorButton: TBitBtn;
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
    TrackBar: TTrackBar;
    IconSizeLabel: TLabel;
    ClientPanel: TPanel;
    TreeView: TTreeView;
    ImageView: TListView;
    ImageListLabel: TLabel;
    GroupBox1: TGroupBox;
    NumSpinEdit: TSpinEdit;
    Label2: TLabel;
    AssignIconsButton: TBitBtn;
    ClearButton: TBitBtn;
    DeleteIconAction: TAction;
    procedure AssignIconsButtonClick(Sender: TObject);
    procedure ChangeIconActionExecute(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowImageEditorButtonClick(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure IconFontsImageListChange(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ReplaceBitBtnClick(Sender: TObject);
    procedure DeleteIconActionExecute(Sender: TObject);
  private
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
  , IconFontsImageListEditorUnit;

procedure TMainForm.UpdateButtons;
begin
  DeleteButton.Action := DeleteIconAction;
  ChangeIconButton.Action := ChangeIconAction;
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

    LStart := GetTickCount;
    //Generate Icons
    IconFontsImageList.AddIcons(
      Chr(LRand1), //From Chr
      Chr(LRand2), //To Chr
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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Screen.Fonts.IndexOf('Material Design Icons') = -1 then
    MessageDlg('Warning: "Material Design Icons" font is not present in your system!'+sLineBreak+
      'Please download at https://materialdesignicons.com and install it, because this demo is based on this font.', mtError, [mbOK], 0);
  
  TrackBar.Position := IconFontsImageList.Height;
  TrackBarChange(TrackBar);
end;

procedure TMainForm.IconFontsImageListChange(Sender: TObject);
begin
  ;
end;

procedure TMainForm.ReplaceBitBtnClick(Sender: TObject);
begin
  ;
end;

procedure TMainForm.SelectThemeRadioGroupClick(Sender: TObject);
var
  LStyleName: string;
begin
  LStyleName := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex];
  //Comment this line if You are using an old version of Delphi!
  TStyleManager.TrySetStyle(LStyleName);

  if LStyleName = 'Windows' then
    IconFontsImageList.UpdateIconsAttributes(clBlack, clBtnFace)
  else if LStyleName = 'Windows10' then
    IconFontsImageList.UpdateIconsAttributes(clBlack, clWhite)
  else if LStyleName = 'Windows10 SlateGray' then
    IconFontsImageList.UpdateIconsAttributes(clWhite, clBlack)
  else if LStyleName = 'Windows10 Blue' then
    IconFontsImageList.UpdateIconsAttributes(clBlue, clGray)
  else if LStyleName = 'Windows10 Dark' then
    IconFontsImageList.UpdateIconsAttributes(clSilver, clBlack)
  else if LStyleName = 'Windows10 Green' then
    IconFontsImageList.UpdateIconsAttributes(clOlive, clGreen)
  else if LStyleName = 'Windows10 Purple' then
    IconFontsImageList.UpdateIconsAttributes(clRed, clPurple);

  UpdateButtons;
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
  TopToolBar.Height := LSize + 4;
  TreeView.Indent := LSize;
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
    begin
      LItem.Text := IconFontsImageList.IconFontItems.Items[LItem.ImageIndex].IconName;
    end
    else
    begin
      LItem.Text := '';
    end;
  end;
end;

procedure TMainForm.TrackBarChange(Sender: TObject);
begin
  //Resize all icons into ImageList
  IconFontsImageList.Size := TrackBar.Position;
  UpdateGUI;
end;

end.
