{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2020 (Ethea S.r.l.)                                 }
{       Contributors:                                                          }
{         Carlo Barazzetta                                                     }
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
unit IconFontsImageListEditorUnit;

interface

{$INCLUDE ..\Source\IconFontsImageList.inc}

uses
  Windows
  , Messages
  , SysUtils
  , Graphics
  , Forms
  , StdCtrls
  , ExtCtrls
  , Controls
  , Classes
  , Dialogs
  , ComCtrls
  , ImgList
  , ExtDlgs
  , Spin
  , IconFontsImageList;

type
  TIconFontsImageListEditor = class;


  TIconFontsImageListEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    SaveDialog: TSavePictureDialog;
    ImageListGroup: TGroupBox;
    ImageView: TListView;
    ImageGroup: TGroupBox;
    MainPanel: TPanel;
    MainImage: TImage;
    AddButton: TButton;
    DeleteButton: TButton;
    HelpButton: TButton;
    IconNameLabel: TLabel;
    IconName: TEdit;
    BuilderGroupBox: TGroupBox;
    FontNameLabel: TLabel;
    FontName: TComboBox;
    FontIconHexLabel: TLabel;
    FontIconDecLabel: TLabel;
    FontIconDec: TSpinEdit;
    FontIconHex: TEdit;
    FontColorLabel: TLabel;
    FontColor: TColorBox;
    MaskColorLabel: TLabel;
    MaskColor: TColorBox;
    ClearAllButton: TButton;
    DefaultFontNameLabel: TLabel;
    DefaultFontName: TComboBox;
    DefaultFontColorLabel: TLabel;
    DefaultFontColorColorBox: TColorBox;
    DefaultMaskColorLabel: TLabel;
    DefaultMaskColorColorBox: TColorBox;
    SizeSpinEdit: TSpinEdit;
    SizeLabel: TLabel;
    IconBuilderGroupBox: TGroupBox;
    CharsEdit: TEdit;
    BuildButton: TButton;
    StoreBitmapCheckBox: TCheckBox;
    BuildFromHexButton: TButton;
    FromHexNum: TEdit;
    FromHexNumLabel: TLabel;
    ToHexNumLabel: TLabel;
    ToHexNum: TEdit;
    CharsEditLabel: TLabel;
    ShowCharMapButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FontColorChange(Sender: TObject);
    procedure MaskColorChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure IconNameExit(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FontIconDecChange(Sender: TObject);
    procedure ShowCharMapButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildButtonClick(Sender: TObject);
    procedure SizeSpinEditChange(Sender: TObject);
    procedure DefaultFontNameChange(Sender: TObject);
    procedure StoreBitmapCheckBoxClick(Sender: TObject);
    procedure DefaultFontColorColorBoxChange(Sender: TObject);
    procedure DefaultMaskColorColorBoxChange(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
    procedure BuildFromHexButtonClick(Sender: TObject);
    procedure FontIconHexChange(Sender: TObject);
    procedure EditChangeUpdateGUI(Sender: TObject);
  private
    FIconIndexLabel: string;
    FUpdating: Boolean;
    FEditingList: TIconFontsImageList;
    FOldImageList: TIconFontsImageList;
    FOldIconFontItems: TIconFontItems;
    procedure UndoEditing;
    procedure AddColor(const S: string);
    procedure DeleteSelectedItem;
    procedure ClearAllImages;
    procedure UpdateGUI;
    procedure UpdateCharsToBuild;
    procedure SetImageMaskColor(Color: TColor);
    procedure SetImageFontColor(Color: TColor);
    procedure SetImageFontIconDec(IconDec: Integer);
    procedure SetImageFontIconHex(IconHex: String);
    procedure SetImageIconName(IconName: String);
    procedure SetImageFontName(FontName: String);
    function SelectedIconFont: TIconFontItem;
  end;

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;

implementation

{$R *.dfm}

uses
  CommCtrl
  , TypInfo
  , ShellApi
  , IconFontsUtils;

const
  crColorPick = -100;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;
var
  ListItems: TIconFontsImageListEditor;
begin
  if (AImageList.Width > 256) or (AImageList.Height > 256) then
    raise Exception.Create('ImageList Too Big');

  ListItems := TIconFontsImageListEditor.Create(Application);
  with ListItems do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FEditinglist := AImageList;
        SizeSpinEdit.Value := FEditinglist.Size;
        DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(FEditingList.FontName);
        DefaultFontColorColorBox.Selected := FEditingList.FontColor;
        DefaultMaskColorColorBox.Selected := FEditingList.MaskColor;
        {$IFDEF HasStoreBitmapProperty}
        StoreBitmapCheckBox.Checked := FEditingList.StoreBitmap;
        {$endif}
        FEditinglist.IconFontItems := AImageList.IconFontItems;
        FOldImageList.Assign(FEditingList);

        ImageView.LargeImages := FEditingList;
        ImageView.SmallImages := FEditingList;
        UpdateIconFontListView(ImageView);
        UpdateGUI;
        UpdateCharsToBuild;
        if ImageView.Items.Count > 0 then
          ImageView.ItemIndex := 0;

        if SavedBounds.Right - SavedBounds.Left > 0 then
          BoundsRect := SavedBounds;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      SavedBounds := BoundsRect;
    finally
      Free;
    end;
  end;
end;

{ TIconFontsImageListEditor }

procedure TIconFontsImageListEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/Home'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TIconFontsImageListEditor.SetImageMaskColor(Color: TColor);
begin
  SelectedIconFont.MaskColor := Color;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.ShowCharMapButtonClick(Sender: TObject);
begin
  ShowCharMapButton.SetFocus;
  ShellExecute(Handle, 'open', 'charmap', '', '', SW_SHOWNORMAL);
end;

procedure TIconFontsImageListEditor.SizeSpinEditChange(Sender: TObject);
begin
  FEditingList.Size := SizeSpinEdit.Value;
end;

procedure TIconFontsImageListEditor.StoreBitmapCheckBoxClick(Sender: TObject);
begin
  {$IFDEF HasStoreBitmapProperty}
  FEditingList.StoreBitmap := StoreBitmapCheckBox.Checked;
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.SetImageFontColor(Color: TColor);
begin
  SelectedIconFont.FontColor := Color;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageFontIconDec(IconDec: Integer);
begin
  SelectedIconFont.FontIconDec := IconDec;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageFontIconHex(IconHex: String);
begin
  SelectedIconFont.FontIconHex := IconHex;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageIconName(IconName: String);
begin
  SelectedIconFont.IconName := IconName;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageFontName(FontName: String);
begin
  SelectedIconFont.FontName := FontName;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.FontColorChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageFontColor(FontColor.Selected);
end;

procedure TIconFontsImageListEditor.FontIconDecChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageFontIconDec(FontIconDec.Value);
end;

procedure TIconFontsImageListEditor.FontIconHexChange(Sender: TObject);
var
  LText: string;
begin
  if FUpdating then Exit;
  LText := (Sender as TEdit).Text;
  if (Length(LText) = 4) or (Length(LText)=0) then
  begin
    if Sender = FontIconHex then
      SetImageFontIconHex(FontIconHex.Text);
  end;
end;

procedure TIconFontsImageListEditor.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageFontName(FontName.Text);
  UpdateCharsToBuild;
end;

procedure TIconFontsImageListEditor.UndoEditing;
begin
  FEditingList.ClearIcons;
  FEditingList.Assign(FOldImageList);
end;

procedure TIconFontsImageListEditor.UpdateCharsToBuild;
begin
  CharsEdit.Font.Size := 14;
  if FontName.Text <> '' then
  begin
    CharsEdit.Font.Name := FontName.Text;
    CharsEdit.Enabled := True;
  end
  else if DefaultFontName.Text <> '' then
  begin
    CharsEdit.Font.Name := DefaultFontName.Text;
    CharsEdit.Enabled := True;
  end
  else
  begin
    CharsEdit.Enabled := False;
    BuildButton.Enabled := False;
  end;
end;

procedure TIconFontsImageListEditor.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LItemFontName: string;
  LCharWidth: Integer;
  LCharHeight: Integer;
  LIconFontItem: TIconFontItem;
begin
  FUpdating := True;
  try
    LIconFontItem := SelectedIconFont;
    LIsItemSelected := LIconFontItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    BuildButton.Enabled := CharsEdit.Text <> '';
    BuildFromHexButton.Enabled := (Length(FromHexNum.Text) = 4) and (Length(ToHexNum.Text) = 4);
    DeleteButton.Enabled := LIsItemSelected;
    FontColor.Enabled := LIsItemSelected;
    MaskColor.Enabled := LIsItemSelected;
    FontName.Enabled := LIsItemSelected;
    FontIconDec.Enabled := LIsItemSelected;
    FontIconHex.Enabled := LIsItemSelected;
    IconName.Enabled := LIsItemSelected;
    ShowCharMapButton.Enabled := (FEditingList.FontName <> '');
    if LIsItemSelected then
    begin
      ImageGroup.Caption := Format(FIconIndexLabel,[LIconFontItem.Index]);
      MaskColor.Selected := LIconFontItem.MaskColor;
      FontColor.Selected := LIconFontItem.FontColor;
      LItemFontName := LIconFontItem.FontName;
      FontName.ItemIndex := FontName.Items.IndexOf(LItemFontName);
      IconName.Text := LIconFontItem.IconName;
      FontIconDec.Value := LIconFontItem.FontIconDec;
      FontIconHex.Text := LIconFontItem.FontIconHex;
      MainPanel.Invalidate;
    end
    else
    begin
      FontColor.Selected := clNone;
      MaskColor.Selected := clNone;
      FontName.ItemIndex := -1;
      IconName.Text := '';
      FontIconDec.Value := 0;
      FontIconHex.Text := '';
    end;
    MainImage.Canvas.Brush.Color :=  MainPanel.Color;
    MainImage.Canvas.FillRect(Rect(0, 0, MainImage.Height, MainImage.Height));
    if LIsItemSelected then
    begin
      if LIconFontItem.FontName <> '' then
        MainImage.Canvas.Font.Name := LIconFontItem.FontName
      else
        MainImage.Canvas.Font.Name := FEditingList.FontName;
      MainImage.Canvas.Font.Height := MainImage.Height;
      if LIconFontItem.FontColor <> clNone then
        MainImage.Canvas.Font.Color := LIconFontItem.FontColor
      else
        MainImage.Canvas.Font.Color := FEditingList.FontColor;
      if LIconFontItem.MaskColor <> clNone then
        MainImage.Canvas.Brush.Color := LIconFontItem.MaskColor
      else
        MainImage.Canvas.Brush.Color := FEditingList.MaskColor;
      MainImage.Canvas.FillRect(Rect(0, 0, MainImage.Height, MainImage.Height));
      MainImage.Canvas.TextOut(0, 0, LIconFontItem.Character);
    end;

    UpdateIconFontListViewCaptions(ImageView);
  finally
    FUpdating := False;
  end;
end;

procedure TIconFontsImageListEditor.DeleteSelectedItem;
var
  LIndex: Integer;
begin
  LIndex := ImageView.Selected.Index;
  FEditingList.Delete(LIndex);
  UpdateIconFontListView(ImageView);
  if LIndex < ImageView.Items.Count then
    ImageView.ItemIndex := LIndex
  else if ImageView.Items.Count > 0 then
    ImageView.ItemIndex := LIndex-1;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.CancelButtonClick(Sender: TObject);
begin
  UndoEditing;
end;

procedure TIconFontsImageListEditor.ClearAllImages;
begin
  Screen.Cursor := crHourglass;
  try
    FEditingList.ClearIcons;
  finally
    Screen.Cursor := crDefault;
  end;
end;

type
  THackPanel = class(TPanel);

procedure TIconFontsImageListEditor.ClearAllButtonClick(Sender: TObject);
begin
  ClearAllImages;
  UpdateIconFontListView(ImageView);
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.IconNameExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageIconName(IconName.Text);
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.ImageViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateGUI;
end;

procedure TIconFontsImageListEditor.ImportButtonClick(Sender: TObject);
var
  LFont: TFont;
begin
  LFont := TFont.Create;
  LFont.Name := FEditingList.FontName;
end;

procedure TIconFontsImageListEditor.DefaultFontColorColorBoxChange(
  Sender: TObject);
begin
  FEditingList.FontColor := DefaultFontColorColorBox.Selected;
end;

procedure TIconFontsImageListEditor.DefaultFontNameChange(Sender: TObject);
begin
  FEditingList.FontName := DefaultFontName.Text;
  UpdateCharsToBuild;
end;

procedure TIconFontsImageListEditor.DefaultMaskColorColorBoxChange(
  Sender: TObject);
begin
  FEditingList.MaskColor := DefaultMaskColorColorBox.Selected;
end;

procedure TIconFontsImageListEditor.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TIconFontsImageListEditor.MaskColorChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageMaskColor(MaskColor.Selected);
end;

procedure TIconFontsImageListEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
    OKButton.SetFocus;
  end
  else
  begin
    CancelButton.SetFocus;
    UndoEditing;
  end;
end;

procedure TIconFontsImageListEditor.FormCreate(Sender: TObject);
begin
  FUpdating := True;
  FOldImageList := TIconFontsImageList.Create(nil);
  FOldIconFontItems := TIconFontItems.Create(FOldImageList, TIconFontItem);
  GetColorValues(AddColor);
  FontColor.ItemIndex := -1;
  MaskColor.ItemIndex := -1;
  FontName.Items := Screen.Fonts;
  DefaultFontName.Items := Screen.Fonts;
  FIconIndexLabel := ImageGroup.Caption;
  {$IFNDEF HasStoreBitmapProperty}
  StoreBitmapCheckBox.Visible := False;
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FOldImageList);
  FreeAndNil( FOldIconFontItems);
  Screen.Cursors[crColorPick] := 0;
end;

procedure TIconFontsImageListEditor.FormResize(Sender: TObject);
var
  LEditSize: Integer;
begin
  LEditSize := (ImageGroup.Width - MainPanel.Width - 33) div 3;

  DefaultFontColorColorBox.Width := LEditSize;
  DefaultMaskColorColorBox.Left := DefaultFontColorColorBox.Left + DefaultFontColorColorBox.Width + 2;
  DefaultMaskColorLabel.Left := DefaultMaskColorColorBox.Left;
  DefaultMaskColorColorBox.Width := LEditSize;

  MaskColor.Width := LEditSize;
  FontColor.Width := LEditSize;
  MaskColor.Left := FontColor.Left + FontColor.Width + 2;
  MaskColorLabel.Left := MaskColor.Left;
  MaskColor.Width := LEditSize;

  IconName.Left := MaskColor.Left + MaskColor.Width + 2;
  IconNameLabel.Left := IconName.Left;
  IconName.Width := LEditSize;
end;

procedure TIconFontsImageListEditor.EditChangeUpdateGUI(Sender: TObject);
begin
  UpdateGUI;
end;

function TIconFontsImageListEditor.SelectedIconFont: TIconFontItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.IconFontItems.Count) then
    Result := FEditingList.IconFontItems[ImageView.Selected.Index]
  else
    Result := nil;
end;

procedure TIconFontsImageListEditor.AddButtonClick(Sender: TObject);
var
  LInsertIndex: Integer;
begin
  if (ImageView.Selected <> nil) then
    LInsertIndex := ImageView.Selected.Index +1
  else
    LInsertIndex := ImageView.Items.Count;
  ImageView.Selected := nil;
  FEditingList.IconFontItems.Insert(LInsertIndex);
  UpdateIconFontListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
end;

procedure TIconFontsImageListEditor.AddColor(const S: string);
begin
  FontColor.Items.Add(S);
  MaskColor.Items.Add(S);
end;

procedure TIconFontsImageListEditor.BuildButtonClick(Sender: TObject);
var
  C: Char;
begin
  for C in CharsEdit.Text do
    FEditingList.AddIcon(C);
  FEditingList.RedrawImages;
  UpdateIconFontListView(ImageView);
end;

procedure TIconFontsImageListEditor.BuildFromHexButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FEditingList.AddIcons(
      Chr(StrToInt('$' + FromHexNum.Text)), //From Chr
      Chr(StrToInt('$' + ToHexNum.Text)), //To Chr
      'Material Design Icons'
      );
    UpdateIconFontListView(ImageView);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
