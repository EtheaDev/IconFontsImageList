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
  , IconFontsCharMapUnit
  , IconFontsImageList
  , IconFontsImageListBase
  , IconFontsVirtualImageList
  , IconFontsImageCollection
  , IconFontsItems
  , IconFontsImage;

type
  TIconFontsImageListEditor = class(TForm)
    paImages: TPanel;
    CategorySplitter: TSplitter;
    Panel1: TPanel;
    SetCategoriesButton: TButton;
    ImagesPanel: TPanel;
    CategoryGroupBox: TGroupBox;
    CategoryListBox: TListBox;
    PropertiesGroupBox: TGroupBox;
    ImageListGroup: TGroupBox;
    ImageView: TListView;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearAllButton: TButton;
    ExportButton: TButton;
    WinCharMapButton: TButton;
    ShowCharMapButton: TButton;
    DefaultFontNameLabel: TLabel;
    DefaultFontName: TComboBox;
    DefaultFontColorLabel: TLabel;
    DefaultFontColorColorBox: TColorBox;
    DefaultMaskColorLabel: TLabel;
    DefaultMaskColorColorBox: TColorBox;
    StoreBitmapCheckBox: TCheckBox;
    BottomPanel: TPanel;
    OKButton: TButton;
    ApplyButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    ImageListGroupBox: TGroupBox;
    SizeLabel: TLabel;
    WidthLabel: TLabel;
    HeightLabel: TLabel;
    SizeSpinEdit: TSpinEdit;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    BottomSplitter: TSplitter;
    ItemGroupBox: TGroupBox;
    LeftIconPanel: TPanel;
    IconPanel: TPanel;
    IconImage: TIconFontImage;
    IconClientPanel: TPanel;
    IconBuilderGroupBox: TGroupBox;
    FromHexNumLabel: TLabel;
    ToHexNumLabel: TLabel;
    CharsEditLabel: TLabel;
    CharsEdit: TEdit;
    BuildButton: TButton;
    BuildFromHexButton: TButton;
    FromHexNum: TEdit;
    ToHexNum: TEdit;
    FontNameLabel: TLabel;
    FontName: TComboBox;
    FontIconHexLabel: TLabel;
    FontIconHex: TEdit;
    FontIconDecLabel: TLabel;
    FontIconDec: TSpinEdit;
    FontColorLabel: TLabel;
    FontColor: TColorBox;
    MaskColorLabel: TLabel;
    MaskColor: TColorBox;
    NameLabel: TLabel;
    NameEdit: TEdit;
    CategoryLabel: TLabel;
    CategoryEdit: TEdit;
    IconLeftMarginPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FontColorChange(Sender: TObject);
    procedure MaskColorChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure NameEditExit(Sender: TObject);
    procedure FontIconDecChange(Sender: TObject);
    procedure ShowCharMapButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildButtonClick(Sender: TObject);
    procedure SizeSpinEditChange(Sender: TObject);
    procedure StoreBitmapCheckBoxClick(Sender: TObject);
    procedure DefaultFontColorColorBoxChange(Sender: TObject);
    procedure DefaultMaskColorColorBoxChange(Sender: TObject);
    procedure BuildFromHexButtonClick(Sender: TObject);
    procedure EditChangeUpdateGUI(Sender: TObject);
    procedure ImageViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExportButtonClick(Sender: TObject);
    procedure DefaultFontNameSelect(Sender: TObject);
    procedure FontIconHexExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImageViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WidthSpinEditChange(Sender: TObject);
    procedure HeightSpinEditChange(Sender: TObject);
    procedure WinCharMapButtonClick(Sender: TObject);
    procedure CategoryListBoxClick(Sender: TObject);
    procedure SetCategoriesButtonClick(Sender: TObject);
    procedure CategoryEditExit(Sender: TObject);
  private
    FSelectedCategory: string;
    FSourceList, FEditingList: TIconFontsImageListBase;
    FCharMap: TIconFontsCharMapForm;
    FIconIndexLabel: string;
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FChanged: Boolean;
    FModified: Boolean;
    procedure IconFontsImageListFontMissing(const AFontName: TFontName);
    procedure CloseCharMap(Sender: TObject; var Action: TCloseAction);
    procedure BuildList(Selected: Integer);
    procedure AddColor(const S: string);
    procedure UpdateCategories;
    procedure UpdateSizeGUI;
    procedure AddNewItem;
    procedure DeleteSelectedItem;
    procedure Apply;
    procedure UpdateGUI;
    procedure UpdateCharsToBuild;
    {$IFNDEF GDI+}
    procedure SetImageMaskColor(Color: TColor);
    {$ENDIF}
    procedure SetImageFontColor(Color: TColor);
    procedure SetImageFontIconDec(IconDec: Integer);
    procedure SetImageFontIconHex(IconHex: String);
    procedure SetImageIconName(Name: String);
    procedure SetImageFontName(FontName: TFontName);
    function SelectedIcon: TIconFontItem;
    procedure InitGUI;
  public
    destructor Destroy; override;
    property Modified: Boolean read FModified;
    property IconFontsImageList: TIconFontsImageListBase read FEditingList;
  end;

function EditIconFontsImageList(const AImageList: TIconFontsImageListBase): Boolean;
function EditIconFontsVirtualImageList(const AImageList: TIconFontsVirtualImageList): Boolean;
function EditIconFontsImageCollection(const AImageCollection: TIconFontsImageCollection): Boolean;

implementation

{$R *.dfm}

uses
  CommCtrl
  {$WARN UNIT_PLATFORM OFF}
  , FileCtrl
  , TypInfo
  , ShellApi
  {$IFDEF DXE3+}
  , UITypes
  , System.Types
  , System.Character
  {$ENDIF}
  , IconFontsUtils;

const
  crColorPick = -100;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function EditIconFontsImageList(const AImageList: TIconFontsImageListBase): Boolean;
var
  LEditor: TIconFontsImageListEditor;
begin
  LEditor := TIconFontsImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := AImageList;
        FEditinglist.Assign(AImageList);
        InitGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageList.Assign(FEditingList);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
    finally
      Free;
    end;
  end;
end;

function EditIconFontsVirtualImageList(const AImageList: TIconFontsVirtualImageList): Boolean;
var
  LEditor: TIconFontsImageListEditor;
begin
  if AImageList.ImageCollection = nil then
  begin
    Result := false;
    Exit;
  end;
  LEditor := TIconFontsImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := TIconFontsImageList.Create(LEditor);
        FSourceList.Assign(AImageList);
        FEditingList.Assign(AImageList);
        InitGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageList.ImageCollection.IconFontItems.Assign(LEditor.FEditingList.IconFontItems);
          AImageList.Assign(LEditor.FEditingList);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
    finally
      Free;
    end;
  end;
end;

function EditIconFontsImageCollection(const AImageCollection: TIconFontsImageCollection): Boolean;
var
  LEditor: TIconFontsImageListEditor;
begin
  LEditor := TIconFontsImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FSourceList := TIconFontsImageList.Create(LEditor);
        FSourceList.IconFontItems.Assign(AImageCollection.IconFontItems);
        FSourceList.Size := 64; //Force 64 pixel size for image collection icons
        FSourceList.FontName := AImageCollection.FontName;
        FSourceList.FontColor := AImageCollection.FontColor;
        FSourceList.MaskColor := AImageCollection.MaskColor;
        ImageListGroupBox.Visible := False;
        FSourceList.IconFontItems.Assign(AImageCollection.IconFontItems);
        FEditingList.Assign(FSourceList);
        InitGUI;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
      begin
        Screen.Cursor := crHourglass;
        try
          AImageCollection.IconFontItems.Assign(LEditor.FEditingList.IconFontItems);
          AImageCollection.FontName := DefaultFontName.Text;
          AImageCollection.FontColor := DefaultFontColorColorBox.Selected;
          AImageCollection.MaskColor := DefaultMaskColorColorBox.Selected;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
      SavedBounds := BoundsRect;
    finally
      Free;
    end;
  end;
end;

{ TIconFontsImageListEditor }

procedure TIconFontsImageListEditor.UpdateSizeGUI;
begin
  WidthSpinEdit.Value := FEditingList.Width;
  HeightSpinEdit.Value := FEditingList.Height;
  SizeSpinEdit.Value := FEditingList.Size;
  if FEditingList.Width = FEditingList.Height then
  begin
    SizeSpinEdit.Enabled := True;
    IconPanel.Align := alClient;
  end
  else
  begin
    SizeSpinEdit.Enabled := False;
    if FEditingList.Width > FEditingList.Height then
    begin
      IconPanel.Align := alTop;
      IconPanel.Height := Round(IconPanel.Width * FEditingList.Height / FEditingList.Width);
    end
    else
    begin
      IconPanel.Align := alLeft;
      IconPanel.Height := Round(IconPanel.Height * FEditingList.Width / FEditingList.Height);
    end;
  end;
end;

procedure TIconFontsImageListEditor.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/Component-Editor-(VCL)'), nil, nil,
    SW_SHOWNORMAL)
end;

{$IFNDEF GDI+}
procedure TIconFontsImageListEditor.SetImageMaskColor(Color: TColor);
begin
  SelectedIcon.MaskColor := Color;
  UpdateGUI;
end;
{$ENDIF}

procedure TIconFontsImageListEditor.ShowCharMapButtonClick(Sender: TObject);
begin
  ShowCharMapButton.SetFocus;
  if not Assigned(FCharMap) then
  begin
    FCharMap := TIconFontsCharMapForm.CreateForImageList(Self, FEditingList, FontName.Text);
    FCharMap.OnClose := CloseCharMap;
  end;
  FCharMap.AssignImageList(FEditingList, FontName.Text);
  FCharMap.Show;
end;

procedure TIconFontsImageListEditor.StoreBitmapCheckBoxClick(Sender: TObject);
begin
  {$IFDEF HasStoreBitmapProperty}
  FEditingList.StoreBitmap := StoreBitmapCheckBox.Checked;
  FChanged := True;
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.SetImageFontColor(Color: TColor);
begin
  SelectedIcon.FontColor := Color;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageFontIconDec(IconDec: Integer);
begin
  SelectedIcon.FontIconDec := IconDec;
  BuildList(SelectedIcon.Index);
end;

procedure TIconFontsImageListEditor.SetImageFontIconHex(IconHex: String);
begin
  SelectedIcon.FontIconHex := IconHex;
  BuildList(SelectedIcon.Index);
end;

procedure TIconFontsImageListEditor.SetImageIconName(Name: String);
begin
  SelectedIcon.Name := Name;
  BuildList(SelectedIcon.Index);
end;

procedure TIconFontsImageListEditor.SetImageFontName(FontName: TFontName);
begin
  SelectedIcon.FontName := FontName;
  BuildList(SelectedIcon.Index);
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

procedure TIconFontsImageListEditor.FontIconHexExit(Sender: TObject);
var
  LText: string;
begin
  if FUpdating then Exit;
  LText := (Sender as TEdit).Text;
  if (Length(LText) = 4) or (Length(LText) = 5) or (Length(LText)=0) then
  begin
    if Sender = FontIconHex then
      SetImageFontIconHex(FontIconHex.Text);
  end;
end;

procedure TIconFontsImageListEditor.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  if (FontName.Text = '') or (Screen.Fonts.IndexOf(FontName.Text) >= 0) then
  begin
    SetImageFontName(FontName.Text);
    UpdateCharsToBuild;
  end;
end;

procedure TIconFontsImageListEditor.UpdateCharsToBuild;
begin
  CharsEdit.Font.Size := 12;
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

procedure TIconFontsImageListEditor.UpdateCategories;
var
  I: Integer;
  LCategory: string;
begin
  CategoryListBox.Items.Clear;
  CategoryListBox.AddItem('All', nil);
  for I := 0 to FEditingList.IconFontItems.Count -1 do
  begin
    LCategory := FEditingList.IconFontItems[I].Category;
    if (LCategory <> '') and (CategoryListBox.Items.IndexOf(LCategory)<0) then
      CategoryListBox.AddItem(LCategory,nil);
  end;
  if (FSelectedCategory <> '') then
  begin
    I := CategoryListBox.Items.IndexOf(FSelectedCategory);
    if I >= 0 then
      CategoryListBox.Selected[I] := True;
  end
  else
    CategoryListBox.Selected[0] := True;
end;

procedure TIconFontsImageListEditor.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LItemFontName: TFontName;
  LIconFontItem: TIconFontItem;
begin
  FUpdating := True;
  try
    UpdateCategories;
    LIconFontItem := SelectedIcon;
    LIsItemSelected := LIconFontItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    ExportButton.Enabled := FEditingList.Count > 0;
    BuildButton.Enabled := CharsEdit.Text <> '';
    BuildFromHexButton.Enabled := (Length(FromHexNum.Text) in [4,5]) and (Length(ToHexNum.Text) in [4,5]);
    DeleteButton.Enabled := LIsItemSelected;
    SetCategoriesButton.Enabled := LIsItemSelected;
    ApplyButton.Enabled := FChanged;
    FontColor.Enabled := LIsItemSelected;
    MaskColor.Enabled := LIsItemSelected;
    FontName.Enabled := LIsItemSelected;
    FontIconDec.Enabled := LIsItemSelected;
    FontIconHex.Enabled := LIsItemSelected;
    NameEdit.Enabled := LIsItemSelected;
    CategoryEdit.Enabled := LIsItemSelected;
    ShowCharMapButton.Enabled := (FEditingList.FontName <> '');
    ImageListGroup.Caption := Format(FTotIconsLabel, [FEditingList.Count]);
    if LIsItemSelected then
    begin
      IconImage.ImageIndex := SelectedIcon.Index;
      ItemGroupBox.Caption := Format(FIconIndexLabel,[LIconFontItem.Index]);
      {$IFNDEF GDI+}
      if LIconFontItem.MaskColor <> FEditingList.MaskColor then
        MaskColor.Selected := LIconFontItem.MaskColor
      else
        MaskColor.Selected := clNone;
      {$ELSE}
        MaskColor.Selected := clNone;
        MaskColor.Enabled := False;
      {$ENDIF}
      if LIconFontItem.FontColor <> FEditingList.FontColor then
        FontColor.Selected := LIconFontItem.FontColor
      else
        FontColor.Selected := clDefault;
      LItemFontName := LIconFontItem.FontName;
      FontName.ItemIndex := FontName.Items.IndexOf(LItemFontName);
      NameEdit.Text := LIconFontItem.Name;
      CategoryEdit.Text := LIconFontItem.Category;
      FontIconDec.Value := LIconFontItem.FontIconDec;
      FontIconHex.Text := LIconFontItem.FontIconHex;
      IconPanel.Invalidate;

      //Draw Icon
      IconImage.Invalidate;
    end
    else
    begin
      FontColor.Selected := clDefault;
      MaskColor.Selected := clNone;
      FontName.ItemIndex := -1;
      IconImage.ImageIndex := -1;
      ItemGroupBox.Caption := '';
      NameEdit.Text := '';
      CategoryEdit.Text := '';
      FontIconDec.Value := 0;
      FontIconHex.Text := '';
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TIconFontsImageListEditor.WidthSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Width := WidthSpinEdit.Value;
  UpdateSizeGUI;
end;

procedure TIconFontsImageListEditor.WinCharMapButtonClick(Sender: TObject);
begin
  WinCharMapButton.SetFocus;
  ShellExecute(Handle, 'open', 'charmap', '', '', SW_SHOWNORMAL);
end;

procedure TIconFontsImageListEditor.ImageViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LTargetItem: TListItem;
  LItem: TCollectionItem;
  SIndex, DIndex: Integer;
begin
  LTargetItem := ImageView.GetItemAt(X, Y);

  if not Assigned(LTargetItem) then
    LTargetItem := ImageView.GetNearestItem(Point(X, Y), sdRight);

  if Assigned(LTargetItem) then
    DIndex := LTargetItem.ImageIndex
  else
    DIndex := ImageView.Items.Count - 1;

  SIndex := ImageView.Items[ImageView.ItemIndex].ImageIndex;
  LItem := FEditingList.IconFontItems[SIndex];
  LItem.Index := DIndex;
  BuildList(LItem.Index);
  if SIndex <> DIndex then
    FChanged := True;
end;

procedure TIconFontsImageListEditor.ImageViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Sender;
end;

procedure TIconFontsImageListEditor.DeleteSelectedItem;
var
  LIndex: Integer;
begin
  LIndex := ImageView.Selected.Index;
  FEditingList.Delete(LIndex);
  FChanged := True;
  if ImageView.Items.Count > 1 then
    BuildList(ImageView.Items[LIndex-1].ImageIndex);
  FChanged := True;
end;

destructor TIconFontsImageListEditor.Destroy;
begin
  FCharMap.Free;
  inherited;
end;

procedure TIconFontsImageListEditor.CategoryListBoxClick(Sender: TObject);
var
  LIndex: Integer;
begin
  if SelectedIcon <> nil then
    LIndex := SelectedIcon.Index
  else
    LIndex := -1;  
  if CategoryListBox.ItemIndex <= 0 then
    FSelectedCategory := ''
  else
    FSelectedCategory := CategoryListBox.Items[CategoryListBox.ItemIndex];
  BuildList(LIndex);
end;

procedure TIconFontsImageListEditor.CloseCharMap(Sender: TObject;
  var Action: TCloseAction);
begin
  if FCharMap.ModalResult = mrOK then
  begin
    if FCharMap.CharsEdit.Text <> '' then
    begin
      FEditingList.AddIcons(FCharMap.CharsEdit.Text, FCharMap.DefaultFontName.Text);
      FChanged := True;
      BuildList(ImageView.Items[ImageView.Items.Count-1].ImageIndex);
    end;
  end;
end;

procedure TIconFontsImageListEditor.ClearAllButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    ImageView.Clear;
    FEditingList.ClearIcons;
    FChanged := True;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsImageListEditor.IconFontsImageListFontMissing(
  const AFontName: TFontName);
begin
  MessageDlg(Format(ERR_ICONFONTS_FONT_NOT_INSTALLED,[AFontName]),
    mtError, [mbOK], 0);
end;

procedure TIconFontsImageListEditor.NameEditExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageIconName(NameEdit.Text);
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.ImageViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_INSERT) and (Shift = []) then
    AddNewItem
  else if (Key = VK_DELETE) and (Shift = []) then
    DeleteSelectedItem;
end;

function TIconFontsImageListEditor.SelectedIcon: TIconFontItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.IconFontItems.Count) then
    Result := FEditingList.IconFontItems[ImageView.Selected.ImageIndex]
  else
    Result := nil;
end;

procedure TIconFontsImageListEditor.SetCategoriesButtonClick(Sender: TObject);
var
  LIndex: Integer;
  Selected: Integer;
  LIconFontItem: TIconFontItem;
  LCategoryName: string;
begin
  LCategoryName := InputBox('Set Category', 'Name', FSelectedCategory);
  if (LCategoryName = FSelectedCategory) then
    Exit;

  Screen.Cursor := crHourGlass;
  try
    FSelectedCategory := LCategoryName;
    Selected := ImageView.ItemIndex;
    FEditingList.StopDrawing(True);
    try
      for LIndex := ImageView.Items.Count - 1 downto 0 do
      begin
        if ImageView.Items[LIndex].Selected then
        begin
          LIconFontItem := FEditingList.IconFontItems[ImageView.Items[LIndex].ImageIndex];
          LIconFontItem.Category := FSelectedCategory;
        end;
      end;
    finally
      FEditingList.StopDrawing(False);
    end;
    FChanged := True;
    BuildList(Selected);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsImageListEditor.ImageViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateGUI;
end;

procedure TIconFontsImageListEditor.InitGUI;
begin
  SizeSpinEdit.Value := FEditinglist.Size;
  DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(FEditingList.FontName);
  DefaultFontColorColorBox.Selected := FEditingList.FontColor;
  {$IFNDEF GDI+}
  DefaultMaskColorColorBox.Selected := FEditingList.MaskColor;
  {$ELSE}
  DefaultMaskColorColorBox.Enabled := False;
  {$ENDIF}
  {$IFDEF HasStoreBitmapProperty}
  StoreBitmapCheckBox.Checked := FEditingList.StoreBitmap;
  {$endif}
  BuildList(0);
  UpdateCharsToBuild;
end;

procedure TIconFontsImageListEditor.SizeSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  if FEditingList.Width = FEditingList.Height then
    FEditingList.Size := SizeSpinEdit.Value;
  FChanged := True;
  UpdateSizeGUI;
end;

procedure TIconFontsImageListEditor.BuildList(Selected: Integer);
begin
  UpdateIconFontListView(ImageView, FSelectedCategory);

  if Selected < -1 then
    Selected := -1
  else if (Selected = -1) and (ImageView.Items.Count > 0) then
    Selected := 0
  else if Selected >= ImageView.Items.Count then
    Selected := ImageView.Items.Count - 1;

  ImageView.ItemIndex := Selected;
  if (FSelectedCategory <> '') and (SelectedIcon <> nil) and (SelectedIcon.Category <> FSelectedCategory) then
  begin
    FSelectedCategory := SelectedIcon.Category;
    BuildList(SelectedIcon.Index);
  end
  else
    UpdateGUI;
end;

procedure TIconFontsImageListEditor.DefaultFontColorColorBoxChange(
  Sender: TObject);
begin
  FEditingList.FontColor := DefaultFontColorColorBox.Selected;
  FChanged := True;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.DefaultFontNameSelect(Sender: TObject);
begin
  FEditingList.FontName := DefaultFontName.Text;
  FChanged := True;
  UpdateCharsToBuild;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.DefaultMaskColorColorBoxChange(
  Sender: TObject);
begin
  FEditingList.MaskColor := DefaultMaskColorColorBox.Selected;
  FChanged := True;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TIconFontsImageListEditor.MaskColorChange(Sender: TObject);
begin
  {$IFNDEF GDI+}
  if FUpdating then Exit;
  SetImageMaskColor(MaskColor.Selected);
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus
  else
    CancelButton.SetFocus;
end;

procedure TIconFontsImageListEditor.FormCreate(Sender: TObject);

  procedure InitColorBox(AColorBox: TColorBox;
    AColor: TColor);
  begin
    {$IFDEF UNICODE}
    AColorBox.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
      cbIncludeNone, cbIncludeDefault, cbCustomColor, cbCustomColors, cbPrettyNames];
    {$ENDIF}
    AColorBox.Selected := AColor;
  end;

begin
  {$IFNDEF UNICODE}
  CharsEditLabel.Visible := False;
  CharsEdit.Visible := False;
  BuildButton.Visible := False;
  IconBuilderGroupBox.Height := IconBuilderGroupBox.Height - BuildButton.Height -4;
  FontIconHex.MaxLength := 4;
  ExportButton.Visible := False;
  {$ENDIF}
  InitColorBox(DefaultFontColorColorBox, clDefault);
  InitColorBox(DefaultMaskColorColorBox, clNone);
  InitColorBox(FontColor, clDefault);
  InitColorBox(MaskColor, clNone);
  Caption := Format(Caption, [IconFontsImageListVersion]);
  FUpdating := True;
  FEditingList := TIconFontsImageList.Create(nil);
  ImageView.LargeImages := FEditingList;
  IconImage.ImageList := FEditingList;
  FEditingList.OnFontMissing := IconFontsImageListFontMissing;
  GetColorValues(AddColor);
  FontColor.ItemIndex := -1;
  MaskColor.ItemIndex := -1;
  FontName.Items := Screen.Fonts;
  DefaultFontName.Items := Screen.Fonts;
  FIconIndexLabel := ItemGroupBox.Caption;
  FTotIconsLabel := ImageListGroup.Caption;
  FChanged := False;
  FModified := False;
  {$IFNDEF HasStoreBitmapProperty}
  StoreBitmapCheckBox.Visible := False;
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.Apply;
begin
  if not FChanged then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FSourceList.StopDrawing(True);
    Try
      FSourceList.Assign(FEditingList);
      FChanged := False;
      FModified := True;
    Finally
      FSourceList.StopDrawing(False);
      FSourceList.RedrawImages;
    End;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsImageListEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEditingList);
  Screen.Cursors[crColorPick] := 0;
end;

procedure TIconFontsImageListEditor.FormShow(Sender: TObject);
begin
  UpdateSizeGUI;
  {$IFDEF DXE8+}
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Width, SavedBounds.Height);
  {$ELSE}
  if SavedBounds.Right - SavedBounds.Left > 0 then
    SetBounds(SavedBounds.Left, SavedBounds.Top, SavedBounds.Right-SavedBounds.Left,
      SavedBounds.Bottom-SavedBounds.Top);
  {$ENDIF}
  if ImageView.CanFocus then
    ImageView.SetFocus;
end;

procedure TIconFontsImageListEditor.EditChangeUpdateGUI(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.ExportButtonClick(Sender: TObject);
var
  LFolder: string;
  LCount: Integer;
begin
  LFolder := ExtractFileDrive(Application.ExeName);
  if SelectDirectory(LFolder, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    Screen.Cursor := crHourGlass;
    try
      LCount := FEditingList.SaveToPngFiles(LFolder);
    finally
      Screen.Cursor := crDefault;
    end;
    MessageDlg(Format(MSG_ICONS_EXPORTED, [LCount, LFolder]), mtInformation, [mbOK], 0);
  end;
end;


procedure TIconFontsImageListEditor.AddButtonClick(Sender: TObject);
begin
  AddNewItem;
end;

procedure TIconFontsImageListEditor.CategoryEditExit(Sender: TObject);
begin
  if FUpdating then Exit;
  if SelectedIcon.Category <> CategoryEdit.Text then
  begin
    SelectedIcon.Category := CategoryEdit.Text;
    if FSelectedCategory <> SelectedIcon.Category then
    begin
      FSelectedCategory := SelectedIcon.Category;
      UpdateCategories;
      BuildList(SelectedIcon.Index);
    end
    else
      UpdateIconFontListViewCaptions(ImageView);
  end;
end;

procedure TIconFontsImageListEditor.AddNewItem;
var
  LInsertIndex: Integer;
begin
  if (ImageView.Selected <> nil) then
    LInsertIndex := ImageView.Selected.Index +1
  else
    LInsertIndex := ImageView.Items.Count;
  ImageView.Selected := nil;
  FEditingList.IconFontItems.Insert(LInsertIndex);
  FChanged := True;
  BuildList(LInsertIndex);
end;

procedure TIconFontsImageListEditor.ApplyButtonClick(Sender: TObject);
begin
  Apply;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.AddColor(const S: string);
begin
  FontColor.Items.Add(S);
  MaskColor.Items.Add(S);
end;

procedure TIconFontsImageListEditor.HeightSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FEditingList.Height := HeightSpinEdit.Value;
  UpdateSizeGUI;
end;

procedure TIconFontsImageListEditor.BuildButtonClick(Sender: TObject);
begin
  {$IFDEF UNICODE}
  FEditingList.AddIcons(CharsEdit.Text);
  FChanged := True;
  BuildList(ImageView.Items[ImageView.Items.Count-1].ImageIndex);
  {$ENDIF}
end;

procedure TIconFontsImageListEditor.BuildFromHexButtonClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    FEditingList.AddIcons(
      StrToInt('$' + FromHexNum.Text), //From Chr
      StrToInt('$' + ToHexNum.Text), //To Chr
      FontName.Text
      );
    FChanged := True;
    BuildList(ImageView.Items[ImageView.Items.Count-1].ImageIndex);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
