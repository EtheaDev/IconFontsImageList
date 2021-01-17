{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi/VLC+FMX         }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
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
unit FMX.IconFontsImageListEditorUnit;

interface

{$INCLUDE ..\Source\IconFontsImageList.inc}

uses
  System.SysUtils, System.Types, System.UITypes, FMX.Controls, System.Classes,
  System.Actions, FMX.Forms, FMX.Graphics, FMX.ActnList, FMX.StdCtrls, FMX.Colors, FMX.ListBox,
  FMX.Controls.Presentation, FMX.ImgList, FMX.Types, FMX.Layouts,
  System.ImageList, FMX.IconFontsImageList, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.Effects, FMX.Filter.Effects;

type
  TIconFontsImageListEditorFMX = class(TForm)
    BottomPanel: TPanel;
    ClientPanel: TPanel;
    ButtonsPanel: TPanel;
    paClient: TPanel;
    PanelIconBuilder: TPanel;
    ListBoxItemStyleBook: TStyleBook;
    IconBuilderGroupBox: TGroupBox;
    FromLabel: TLabel;
    FromHexNum: TEdit;
    ToHexNum: TEdit;
    ToLabel: TLabel;
    BuildFromHexButton: TButton;
    IconsGroupBox: TGroupBox;
    ImageView: TListBox;
    ItemPanel: TPanel;
    GlobalGroupBox: TGroupBox;
    DefaultFontName: TComboBox;
    DefaultFontNameLabel: TLabel;
    DefaultFontColorLabel: TLabel;
    DefaultFontColorColorBox: TColorComboBox;
    DefaultOpacityLabel: TLabel;
    DefaultOpacitySpinBox: TSpinBox;
    ItemGroupBox: TGroupBox;
    FontNameLabel: TLabel;
    FontName: TComboBox;
    FontIconHexLabel: TLabel;
    FontIconHex: TEdit;
    FontIconDecLabel: TLabel;
    FontIconDec: TSpinBox;
    IconNameLabel: TLabel;
    IconName: TEdit;
    OpacityLabel: TLabel;
    OpacitySpinBox: TSpinBox;
    FontColorLabel: TLabel;
    FontColor: TColorComboBox;
    IconPanel: TPanel;
    IconImage: TGlyph;
    SizeLabel: TLabel;
    SizeSpinBox: TSpinBox;
    WidthLabel: TLabel;
    WidthSpinBox: TSpinBox;
    HeightLabel: TLabel;
    HeightSpinBox: TSpinBox;
    ZoomLabel: TLabel;
    ZoomSpinBox: TSpinBox;
    AutoSizeCheckBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearAllButton: TButton;
    WinCharMap: TButton;
    procedure ClearAllButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FontColorChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure IconNameExit(Sender: TObject);
    procedure FontIconDecChange(Sender: TObject);
    procedure ShowCharMapButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildButtonClick(Sender: TObject);
    procedure AutoSizeCheckBoxClick(Sender: TObject);
    procedure DefaultFontColorColorBoxChange(Sender: TObject);
    procedure BuildFromHexButtonClick(Sender: TObject);
    procedure EditChangeUpdateGUI(Sender: TObject);
    procedure DefaultFontNameSelect(Sender: TObject);
    procedure FontIconHexExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DefaultOpacitySpinBoxChange(Sender: TObject);
    procedure OpacitySpinBoxChange(Sender: TObject);
    procedure SizeChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WinCharMapClick(Sender: TObject);
    procedure ZoomChange(Sender: TObject);
  private
    FIconIndexLabel: string;
    FTotIconsLabel: string;
    FUpdating: Boolean;
    FEditingList: TIconFontsImageList;
    //procedure IconFontsImageListFontMissing(const AFontName: TFontName);
    //procedure CloseCharMap(Sender: TObject; var Action: TCloseAction);
    function GetFontName(FontComboBox: TComboBox): string;
    procedure AddNewItem;
    procedure DeleteSelectedItem;
    procedure ClearAllImages;
    procedure UpdateGUI;
    procedure UpdateCharsToBuild;
    procedure SetImageFontColor(Color: TColor);
    procedure SetImageOpacity(Opacity: Single);
    procedure SetImageFontIconDec(IconDec: Integer);
    procedure SetImageFontIconHex(IconHex: String);
    procedure SetImageIconName(IconName: String);
    procedure SetImageFontName(FontName: TFontName);
    function SelectedIconFont: TIconFontsSourceItem;
  public
    destructor Destroy; override;
  end;

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;

implementation

{$R *.fmx}

uses
  Winapi.Messages
  , Winapi.Windows
  , Winapi.ShellApi
  , System.Math;

procedure CollectFonts(const FontList: TStrings);
var
  DC: HDC;
  LFont: TLogFont;
  LList: TStringList;

  function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
    FontType: Integer; Data: Pointer): Integer; stdcall;
  var
    S: TStrings;
    Temp: string;
  begin
    S := TStrings(Data);
    Temp := LogFont.lfFaceName;
    if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
      S.Add(Temp);
    Result := 1;
  end;

begin
  LList := TStringList.Create;
  try
    DC := GetDC(0);
    FillChar(LFont, sizeof(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(LList), 0);
    ReleaseDC(0, DC);
    LList.Sort;
    FontList.Assign(LList);
  finally
    LList.Free;
  end;
end;

function UpdateIconFontListView(const AListBox: TListBox): Integer;
var
  I: Integer;
  LItem: TIconFontsSourceItem;
  LListItem: TListBoxItem;
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := AListBox.Images as TIconFontsImageList;
  AListBox.Items.BeginUpdate;
  try
    AListBox.Clear;
    Result := LIconFontsImageList.Source.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.Source.Items[I] as TIconFontsSourceItem;
      LListItem := TListBoxItem.Create(AListBox);
      LListItem.StyleLookup := 'CustomListBoxItemStyle';
      AListBox.AddObject(LListItem);
(*
      LListItem.Text :=
        Format('%d (%s)%s%s',
        [LItem.FontIconDec,LItem.FontIconHex,sLineBreak,
         Litem.IconName]);
*)
      LListItem.Text := Format('%s',[Litem.IconName]);
       LListItem.ImageIndex := I;
    end;
  finally
    AListBox.Items.EndUpdate;
  end;
end;

function UpdateIconFontListViewCaptions(const AListBox: TListBox;
  const AShowCaption: Boolean = True): Integer;
var
  I: Integer;
  LItem: TIconFontsSourceItem;
  LIconFontsImageList: TIconFontsImageList;
begin
  LIconFontsImageList := AListBox.Images as TIconFontsImageList;
  //AListView.Items.BeginUpdate;
  try
    Result := LIconFontsImageList.Source.Count;
    for I := 0 to Result -1 do
    begin
      LItem := LIconFontsImageList.Source[I] as TIconFontsSourceItem;
      if AShowCaption then
      begin
        AListBox.Items[I] := Format('%d%s$%s%s%s',
          [LItem.FontIconDec,sLineBreak,
           LItem.FontIconHex,sLineBreak,
           Litem.IconName]);
      end
      else
        AListBox.Items[I] := '';
    end;
  finally
    //AListView.Items.EndUpdate;
  end;
end;

function EditIconFontsImageList(const AImageList: TIconFontsImageList): Boolean;
var
  LEditor: TIconFontsImageListEditorFMX;
begin
  LEditor := TIconFontsImageListEditorFMX.Create(nil);
  with LEditor do
  begin
    try
      //Screen.Cursor := crHourglass;
      try
        FEditinglist.Assign(AImageList);
        //DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(FEditingList.FontName);
        DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(FEditingList.FontName);
        SizeSpinBox.Value := Max(FEditingList.Width, FEditingList.Height);
        WidthSpinBox.Value := FEditingList.Width;
        HeightSpinBox.Value := FEditingList.Height;
        ZoomSpinBox.Value := FEditingList.Zoom;
        DefaultFontColorColorBox.Color := FEditingList.FontColor;
        AutoSizeCheckBox.IsChecked := FEditingList.AutoSizeBitmaps;
        DefaultOpacitySpinBox.Value := FEditingList.Opacity * 100;
        ImageView.Images := FEditinglist;
        UpdateIconFontListView(ImageView);
        //UpdateGUI;
        //UpdateCharsToBuild;
        if ImageView.Items.Count > 0 then
          ImageView.ItemIndex := 0;
        //if SavedBounds.Right - SavedBounds.Left > 0 then
        //  BoundsRect := SavedBounds;
      finally
        //Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      if Result then
        AImageList.Assign(FEditingList);
      //Savedprocedure TIconFontsImageListEditorFMX.AddButtonClick(Sender: TObject);
      //Bounds := BoundsRect;
    finally
      DisposeOf;
    end;
  end;
end;

{ TIconFontsImageListEditorFMX }

procedure TIconFontsImageListEditorFMX.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(0, 'open',
    PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/Component-Editor-(FMX)'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TIconFontsImageListEditorFMX.ShowCharMapButtonClick(Sender: TObject);
begin
(*
  ShowCharMapButton.SetFocus;
  if not Assigned(FCharMap) then
  begin
    FCharMap := TIconFontsCharMapForm.CreateForImageList(Self, FEditingList, FontName.Text);
    FCharMap.OnClose := CloseCharMap;
  end;
  FCharMap.AssignImageList(FEditingList, FontName.Text);
  FCharMap.Show;
*)
end;

procedure TIconFontsImageListEditorFMX.SizeChange(Sender: TObject);
begin
  if not FUpdating then
  begin
    if Sender = SizeSpinBox then
      FEditingList.Size := Round(SizeSpinBox.Value);
    if Sender = WidthSpinBox then
      FEditingList.Width := Round(WidthSpinBox.Value);
    if Sender = HeightSpinBox then
      FEditingList.Height := Round(HeightSpinBox.Value);
    UpdateGUI;
  end;
end;

procedure TIconFontsImageListEditorFMX.AutoSizeCheckBoxClick(Sender: TObject);
begin
  FEditingList.AutoSizeBitmaps := AutoSizeCheckBox.IsChecked;
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.SetImageFontColor(Color: TColor);
begin
  SelectedIconFont.FontColor := Color;
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.SetImageFontIconDec(IconDec: Integer);
begin
  SelectedIconFont.FontIconDec := IconDec;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditorFMX.SetImageFontIconHex(IconHex: String);
begin
  SelectedIconFont.FontIconHex := IconHex;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditorFMX.SetImageIconName(IconName: String);
begin
  SelectedIconFont.IconName := IconName;
  UpdateGUI;
  UpdateIconFontListViewCaptions(ImageView);
end;

procedure TIconFontsImageListEditorFMX.SetImageOpacity(Opacity: Single);
begin
  SelectedIconFont.Opacity := Opacity / 100;
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.SetImageFontName(FontName: TFontName);
begin
  SelectedIconFont.FontName := FontName;
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.FontColorChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageFontColor(FontColor.Color);
end;

procedure TIconFontsImageListEditorFMX.FontIconDecChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageFontIconDec(Trunc(FontIconDec.Value));
end;

procedure TIconFontsImageListEditorFMX.FontIconHexExit(Sender: TObject);
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

procedure TIconFontsImageListEditorFMX.FontNameChange(Sender: TObject);
begin
  if FUpdating then Exit;
  begin
    SetImageFontName(GetFontName(FontName));
    UpdateCharsToBuild;
  end;
end;

procedure TIconFontsImageListEditorFMX.UpdateCharsToBuild;
begin
(*
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
*)
end;

procedure TIconFontsImageListEditorFMX.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LItemFontName: TFontName;
  LIconFontItem: TIconFontsSourceItem;
  {$IFNDEF UNICODE}
  S: WideString;
  {$ENDIF}
begin
  FUpdating := True;
  try
    SizeSpinBox.Value := Max(FEditingList.Width, FEditingList.Height);
    WidthSpinBox.Value := FEditingList.Width;
    HeightSpinBox.Value := FEditingList.Height;
    ZoomSpinBox.Value := FEditingList.Zoom;

    LIconFontItem := SelectedIconFont;
    LIsItemSelected := LIconFontItem <> nil;
    ClearAllButton.Enabled := FEditingList.Count > 0;
    //ExportButton.Enabled := FEditingList.Count > 0;
    //BuildButton.Enabled := CharsEdit.Text <> '';
    BuildFromHexButton.Enabled := (Length(FromHexNum.Text) in [4,5]) and (Length(ToHexNum.Text) in [4,5]);
    DeleteButton.Enabled := LIsItemSelected;
    FontColor.Enabled := LIsItemSelected;
    FontName.Enabled := LIsItemSelected;
    FontIconDec.Enabled := LIsItemSelected;
    FontIconHex.Enabled := LIsItemSelected;
    OpacitySpinBox.Enabled := LIsItemSelected;
    IconName.Enabled := LIsItemSelected;
    //ShowCharMapButton.Enabled := (FEditingList.FontName <> '');
    IconsGroupBox.Text := Format(FTotIconsLabel, [FEditingList.Count]);
    if LIsItemSelected then
    begin
      ItemGroupBox.Text := Format(FIconIndexLabel,[LIconFontItem.Index]);
      if LIconFontItem.FontColor <> FEditingList.FontColor then
        FontColor.Color := LIconFontItem.FontColor
      else
        FontColor.Color := TAlphaColors.Null;
      LItemFontName := LIconFontItem.FontName;
      FontName.ItemIndex := FontName.Items.IndexOf(LItemFontName);
      IconName.Text := LIconFontItem.IconName;
      FontIconDec.Value := LIconFontItem.FontIconDec;
      FontIconHex.Text := LIconFontItem.FontIconHex;
      OpacitySpinBox.Value := LIconFontItem.Opacity * 100;
      IconImage.ImageIndex := LIconFontItem.Index;
      IconImage.Repaint;
    end
    else
    begin
      FontColor.Color := TAlphaColors.Null;
      FontName.ItemIndex := -1;
      IconName.Text := '';
      FontIconDec.Value := 0;
      FontIconHex.Text := '';
      IconImage.ImageIndex := -1;
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TIconFontsImageListEditorFMX.WinCharMapClick(Sender: TObject);
begin
  WinCharMap.SetFocus;
  ShellExecute(0, 'open', 'charmap', '', '', SW_SHOWNORMAL);
end;

procedure TIconFontsImageListEditorFMX.ZoomChange(Sender: TObject);
begin
  if not FUpdating then
    FEditingList.Zoom := Round(ZoomSpinBox.Value);
end;

procedure TIconFontsImageListEditorFMX.DeleteSelectedItem;
var
  LIndex: Integer;
begin
  LIndex := ImageView.Selected.Index;
  FEditingList.DeleteIcon(LIndex);
  UpdateIconFontListView(ImageView);
  if LIndex < ImageView.Items.Count then
    ImageView.ItemIndex := LIndex
  else if ImageView.Items.Count > 0 then
    ImageView.ItemIndex := LIndex-1;
  UpdateGUI;
end;

destructor TIconFontsImageListEditorFMX.Destroy;
begin
  inherited;
end;

procedure TIconFontsImageListEditorFMX.ClearAllImages;
begin
  //Screen.Cursor := crHourglass;
  try
    FEditingList.ClearIcons;
  finally
    //Screen.Cursor := crDefault;
  end;
end;

(*
procedure TIconFontsImageListEditorFMX.CloseCharMap(Sender: TObject;
  var Action: TCloseAction);
begin
  if FCharMap.ModalResult = mrOK then
  begin
    if FCharMap.CharsEdit.Text <> '' then
    begin
      FEditingList.AddIcons(FCharMap.CharsEdit.Text, FCharMap.DefaultFontName.Text);
      UpdateIconFontListView(ImageView);
    end;
  end;
end;
*)

procedure TIconFontsImageListEditorFMX.ClearAllButtonClick(Sender: TObject);
begin
  ClearAllImages;
  UpdateIconFontListView(ImageView);
  UpdateGUI;
end;

(*
procedure TIconFontsImageListEditorFMX.IconFontsImageListFontMissing(
  const AFontName: TFontName);
begin
  MessageDlg(Format(ERR_ICONFONTS_FONT_NOT_INSTALLED,[AFontName]),
    mtError, [mbOK], 0);
end;
*)

procedure TIconFontsImageListEditorFMX.IconNameExit(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageIconName(IconName.Text);
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.ImageViewSelectItem(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.OpacitySpinBoxChange(Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageOpacity(OpacitySpinBox.Value);
end;

procedure TIconFontsImageListEditorFMX.DefaultFontColorColorBoxChange(
  Sender: TObject);
begin
  FEditingList.FontColor := DefaultFontColorColorBox.Color;
end;

procedure TIconFontsImageListEditorFMX.DefaultFontNameSelect(Sender: TObject);
begin
  FEditingList.FontName := GetFontName(DefaultFontName);
  UpdateGUI;
end;

procedure TIconFontsImageListEditorFMX.DefaultOpacitySpinBoxChange(
  Sender: TObject);
begin
  if FUpdating then Exit;
  SetImageOpacity(DefaultOpacitySpinBox.Value);
end;

procedure TIconFontsImageListEditorFMX.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TIconFontsImageListEditorFMX.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus
  else
    CancelButton.SetFocus;
end;

procedure TIconFontsImageListEditorFMX.FormCreate(Sender: TObject);
begin
  Caption := Format(Caption, [IconFontsImageListVersion]);
  FUpdating := True;
  FEditingList := TIconFontsImageList.Create(nil);
  //FEditingList.OnFontMissing := IconFontsImageListFontMissing;
  FontColor.Color := talphacolors.Null;

  CollectFonts(DefaultFontName.Items);
  CollectFonts(FontName.Items);
  DefaultFontName.ItemIndex := -1;
  FIconIndexLabel := ItemGroupBox.Text;
  FTotIconsLabel := IconsGroupBox.Text;
  IconImage.Images := FEditingList;
  {$IFDEF D10_2+}
  FontIconHex.CharCase := TEditCharCase.ecUpperCase;
  FromHexNum.CharCase := TEditCharCase.ecUpperCase;
  ToHexNum.CharCase := TEditCharCase.ecUpperCase;
  {$ENDIF}
end;

procedure TIconFontsImageListEditorFMX.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEditingList);
  //Screen.Cursors[crColorPick] := 0;
end;

procedure TIconFontsImageListEditorFMX.FormResize(Sender: TObject);
begin
  if ClientWidth < 610 then
    ClientWidth := 610;
  if ClientHeight < 480 then
    ClientHeight := 480;
end;

function TIconFontsImageListEditorFMX.GetFontName(
  FontComboBox: TComboBox): string;
begin
  if FontComboBox.ItemIndex = -1 then
    Result := ''
  else
    Result := FontComboBox.Items.Strings[FontComboBox.ItemIndex];
end;

procedure TIconFontsImageListEditorFMX.EditChangeUpdateGUI(Sender: TObject);
begin
  UpdateGUI;
end;

function TIconFontsImageListEditorFMX.SelectedIconFont: TIconFontsSourceItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FEditingList.Source.Count) then
    Result := FEditingList.Source.Items[ImageView.Selected.Index] as TIconFontsSourceItem
  else
    Result := nil;
end;

procedure TIconFontsImageListEditorFMX.AddButtonClick(Sender: TObject);
begin
  AddNewItem;
end;

procedure TIconFontsImageListEditorFMX.AddNewItem;
var
  LInsertIndex: Integer;
begin
  if (ImageView.Selected <> nil) then
    LInsertIndex := ImageView.Selected.Index +1
  else
    LInsertIndex := ImageView.Items.Count;
  FEditingList.InsertIcon(LInsertIndex, 0);
  UpdateIconFontListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
end;

procedure TIconFontsImageListEditorFMX.BuildButtonClick(Sender: TObject);
begin
//  FEditingList.AddIcons(CharsEdit.Text);
//  UpdateIconFontListView(ImageView);
end;

procedure TIconFontsImageListEditorFMX.BuildFromHexButtonClick(Sender: TObject);
begin
  try
    FEditingList.AddIcons(
      StrToInt('$' + FromHexNum.Text), //From Chr
      StrToInt('$' + ToHexNum.Text), //To Chr
      GetFontName(FontName)
      );
    UpdateIconFontListView(ImageView);
  finally
    //Screen.Cursor := crDefault;
  end;
end;

end.
