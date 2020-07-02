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
  , IconFontsImageList;

type
  TIconFontsImageListEditor = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    SaveDialog: TSavePictureDialog;
    ImageListGroup: TGroupBox;
    ImageView: TListView;
    ItemGroupBox: TGroupBox;
    IconPanel: TPanel;
    IconImage: TImage;
    AddButton: TButton;
    DeleteButton: TButton;
    HelpButton: TButton;
    IconNameLabel: TLabel;
    IconName: TEdit;
    paTop: TPanel;
    paButtons: TPanel;
    paClient: TPanel;
    ImageListGroupBox: TGroupBox;
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
    ApplyButton: TButton;
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
    BottomPanel: TPanel;
    ExportButton: TButton;
    WidthLabel: TLabel;
    WidthSpinEdit: TSpinEdit;
    HeightLabel: TLabel;
    HeightSpinEdit: TSpinEdit;
    WinCharMapButton: TButton;
    ShowCharMapButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
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
  private
    FSourceList, FEditingList: TIconFontsImageList;
    FCharMap: TIconFontsCharMapForm;
    FIconIndexLabel: string;
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FChanged: Boolean;
    FModified: Boolean;
    procedure IconFontsImageListFontMissing(const AFontName: TFontName);
    procedure CloseCharMap(Sender: TObject; var Action: TCloseAction);
    procedure AddColor(const S: string);
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
    procedure SetImageIconName(IconName: String);
    procedure SetImageFontName(FontName: TFontName);
    function SelectedIconFont: TIconFontItem;
  public
    destructor Destroy; override;
    property Modified: Boolean read FModified;
    property IconFontsImageList: TIconFontsImageList read FEditingList;
  end;

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;

implementation

{$R *.dfm}

uses
  CommCtrl
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

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;
var
  LEditor: TIconFontsImageListEditor;
  LCount: Integer;
begin
  LEditor := TIconFontsImageListEditor.Create(nil);
  with LEditor do
  begin
    try
      Screen.Cursor := crHourglass;
      try
        FEditinglist.Assign(AImageList);
        FSourceList := AImageList;
        SizeSpinEdit.Value := FEditinglist.Size;
        DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(FEditingList.FontName);
        DefaultFontColorColorBox.Selected := FEditingList.FontColor;
        DefaultMaskColorColorBox.Selected := FEditingList.MaskColor;
        {$IFDEF HasStoreBitmapProperty}
        StoreBitmapCheckBox.Checked := FEditingList.StoreBitmap;
        {$endif}
        ImageView.LargeImages := FEditingList;
        ImageView.SmallImages := FEditingList;
        LCount := UpdateIconFontListView(ImageView);
        if LCount > 0 then
          ImageView.ItemIndex := LCount-1
        else
          ImageView.ItemIndex := -1;
        UpdateGUI;
        UpdateCharsToBuild;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
        AImageList.Assign(FEditingList);
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
    IconImage.Align := alClient;
  end
  else
  begin
    SizeSpinEdit.Enabled := False;
    if FEditingList.Width > FEditingList.Height then
    begin
      IconImage.Align := alTop;
      IconImage.Height := Round(IconImage.Width * FEditingList.Height / FEditingList.Width);
    end
    else
    begin
      IconImage.Align := alLeft;
      IconImage.Height := Round(IconImage.Height * FEditingList.Width / FEditingList.Height);
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
  SelectedIconFont.MaskColor := Color;
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
  SelectedIconFont.FontColor := Color;
  UpdateGUI;
end;

procedure TIconFontsImageListEditor.SetImageFontIconDec(IconDec: Integer);
begin
  SelectedIconFont.FontIconDec := IconDec;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditor.SetImageFontIconHex(IconHex: String);
begin
  SelectedIconFont.FontIconHex := IconHex;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditor.SetImageIconName(IconName: String);
begin
  SelectedIconFont.IconName := IconName;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditor.SetImageFontName(FontName: TFontName);
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
  if Screen.Fonts.IndexOf(FontName.Text) >= 0 then
  begin
    SetImageFontName(FontName.Text);
    UpdateCharsToBuild;
  end;
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
  LItemFontName: TFontName;
  LIconFontItem: TIconFontItem;
  {$IFNDEF UNICODE}
  S: WideString;
  {$ENDIF}
begin
  FUpdating := True;
  try
    LIconFontItem := SelectedIconFont;
    LIsItemSelected := LIconFontItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    ExportButton.Enabled := FEditingList.Count > 0;
    BuildButton.Enabled := CharsEdit.Text <> '';
    BuildFromHexButton.Enabled := (Length(FromHexNum.Text) in [4,5]) and (Length(ToHexNum.Text) in [4,5]);
    DeleteButton.Enabled := LIsItemSelected;
    ApplyButton.Enabled := FChanged;
    FontColor.Enabled := LIsItemSelected;
    MaskColor.Enabled := LIsItemSelected;
    FontName.Enabled := LIsItemSelected;
    FontIconDec.Enabled := LIsItemSelected;
    FontIconHex.Enabled := LIsItemSelected;
    IconName.Enabled := LIsItemSelected;
    ShowCharMapButton.Enabled := (FEditingList.FontName <> '');
    IconImage.Canvas.Brush.Color :=  IconPanel.Color;
    IconImage.Canvas.FillRect(Rect(0, 0, IconImage.Height, IconImage.Height));
    ImageListGroup.Caption := Format(FTotIconsLabel, [FEditingList.Count]);
    if LIsItemSelected then
    begin
      ItemGroupBox.Caption := Format(FIconIndexLabel,[LIconFontItem.Index]);
      {$IFNDEF GDI+}
      if LIconFontItem.MaskColor <> FEditingList.MaskColor then
        MaskColor.Selected := LIconFontItem.MaskColor
      else
        MaskColor.Selected := clNone;
      {$ENDIF}
      if LIconFontItem.FontColor <> FEditingList.FontColor then
        FontColor.Selected := LIconFontItem.FontColor
      else
        FontColor.Selected := clNone;
      LItemFontName := LIconFontItem.FontName;
      FontName.ItemIndex := FontName.Items.IndexOf(LItemFontName);
      IconName.Text := LIconFontItem.IconName;
      FontIconDec.Value := LIconFontItem.FontIconDec;
      FontIconHex.Text := LIconFontItem.FontIconHex;
      IconPanel.Invalidate;

      //Draw Icon
      if LIconFontItem.FontName <> '' then
        IconImage.Canvas.Font.Name := LIconFontItem.FontName
      else
        IconImage.Canvas.Font.Name := FEditingList.FontName;
      IconImage.Canvas.Font.Height := IconImage.Height;
      if LIconFontItem.FontColor <> clNone then
        IconImage.Canvas.Font.Color := LIconFontItem.FontColor
      else
        IconImage.Canvas.Font.Color := FEditingList.FontColor;
      {$IFNDEF GDI+}
      if LIconFontItem.MaskColor <> clNone then
        IconImage.Canvas.Brush.Color := LIconFontItem.MaskColor
      else
        IconImage.Canvas.Brush.Color := FEditingList.MaskColor;
      {$ENDIF}
      IconImage.Canvas.FillRect(Rect(0, 0, IconImage.Height, IconImage.Height));
      {$IFNDEF UNICODE}
      S := LIconFontItem.Character;
      TextOutW(IconImage.Canvas.Handle, 0, 0, PWideChar(S), 1);
      {$ELSE}
      IconImage.Canvas.TextOut(0, 0, LIconFontItem.Character);
      {$ENDIF}
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
  Target: TListItem;
  Item: TCollectionItem;
  SIndex, DIndex: Integer;
begin
  SIndex := ImageView.ItemIndex;
  Target := ImageView.GetItemAt(X, Y);
  if Target = nil then
    Target := ImageView.GetNearestItem(Point(X, Y), sdRight);

  if Assigned(Target) then
    DIndex := ImageView.Items.IndexOf(Target)
  else
    DIndex := ImageView.Items.Count - 1;

  Item := FEditingList.IconFontItems[SIndex];
  Item.Index := DIndex;
  UpdateIconFontListView(ImageView);
  if SIndex <> DIndex then
    FChanged := True;
  UpdateGUI;
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
  UpdateIconFontListView(ImageView);
  if LIndex < ImageView.Items.Count then
    ImageView.ItemIndex := LIndex
  else if ImageView.Items.Count > 0 then
    ImageView.ItemIndex := LIndex-1;
  FChanged := True;
  UpdateGUI;
end;

destructor TIconFontsImageListEditor.Destroy;
begin
  FCharMap.Free;
  inherited;
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
      UpdateIconFontListView(ImageView);
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

procedure TIconFontsImageListEditor.IconNameExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageIconName(IconName.Text);
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

procedure TIconFontsImageListEditor.ImageViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateGUI;
end;

procedure TIconFontsImageListEditor.SizeSpinEditChange(Sender: TObject);
begin
  if FUpdating then Exit;
  if FEditingList.Width = FEditingList.Height then
    FEditingList.Size := SizeSpinEdit.Value;
  FChanged := True;
  UpdateSizeGUI;
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

  procedure InitColorBox(AColorBox: TColorBox);
  begin
    {$IFDEF UNICODE}
    AColorBox.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
      cbIncludeNone, cbIncludeDefault, cbCustomColor, cbCustomColors, cbPrettyNames];
    {$ENDIF}
    AColorBox.Selected := clNone;
  end;

begin
  {$IFNDEF UNICODE}
  CharsEditLabel.Visible := False;
  CharsEdit.Visible := False;
  BuildButton.Visible := False;
  IconBuilderGroupBox.Height := IconBuilderGroupBox.Height - BuildButton.Height -4;
  FontIconHex.MaxLength := 4;
  {$ENDIF}
  InitColorBox(DefaultFontColorColorBox);
  InitColorBox(DefaultMaskColorColorBox);
  InitColorBox(FontColor);
  InitColorBox(MaskColor);
  Caption := Format(Caption, [IconFontsImageListVersion]);
  FUpdating := True;
  FEditingList := TIconFontsImageList.Create(nil);
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

procedure TIconFontsImageListEditor.FormResize(Sender: TObject);
var
  LEditSize: Integer;
begin
  LEditSize := (ItemGroupBox.Width - IconPanel.Width - (FontIconHex.Width div 2)) div 3;

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
begin
  if SaveDialog.Execute then
  begin
    Screen.Cursor := crHourGlass;
    try
      FEditingList.SaveToFile(SaveDialog.FileName);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TIconFontsImageListEditor.SelectedIconFont: TIconFontItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.IconFontItems.Count) then
    Result := FEditingList.IconFontItems[ImageView.Selected.Index]
  else
    Result := nil;
end;

procedure TIconFontsImageListEditor.AddButtonClick(Sender: TObject);
begin
  AddNewItem;
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
  UpdateIconFontListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
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
  UpdateIconFontListView(ImageView);
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
    UpdateIconFontListView(ImageView);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
