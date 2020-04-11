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
unit IconFontsCharMapUnit;

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
  , IconFontsImageList, ActnList;

type
  TIconFontsCharMapForm = class(TForm)
    OKButton: TButton;
    ImageListGroup: TGroupBox;
    ImageView: TListView;
    HelpButton: TButton;
    paTop: TPanel;
    paButtons: TPanel;
    paClient: TPanel;
    IconBuilderGroupBox: TGroupBox;
    CharsEdit: TEdit;
    CopyToclipboardButton: TButton;
    ImageGroup: TGroupBox;
    FontIconHexLabel: TLabel;
    FontIconDecLabel: TLabel;
    MainPanel: TPanel;
    MainImage: TImage;
    FontIconHex: TEdit;
    FontIconDec: TEdit;
    DefaultFontName: TComboBox;
    DefaultFontNameLabel: TLabel;
    CancelButton: TButton;
    cbShowSurrogate: TCheckBox;
    ProgressBar: TProgressBar;
    ActionList: TActionList;
    CopyToCipboardAction: TAction;
    ShowCaptionsCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BuildAllIcons(const ASurrogate: Boolean = False);
    procedure EditChangeUpdateGUI(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbShowSurrogateClick(Sender: TObject);
    procedure ImageViewDblClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure CopyToCipboardActionExecute(Sender: TObject);
    procedure CopyToCipboardActionUpdate(Sender: TObject);
    procedure DefaultFontNameSelect(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CancelButtonClick(Sender: TObject);
    procedure ShowCaptionsCheckBoxClick(Sender: TObject);
  private
    FStopped: Boolean;
    FBuilding: Boolean;
    FFirstTime: Boolean;
    FMaxIcons: Integer;
    FFirstIcon: Integer;
    FIconsCount: Integer;
    FIconIndexLabel: string;
    FUpdating: Boolean;
    FCharMapList: TIconFontsImageList;
    FIconFontItems: TIconFontItems;
    FImageListCaption: string;
    procedure DrawIconProgress(const ASender: TObject; const ACount: Integer;
      const AItem: TIconFontItem; var AProceed: Boolean);
    function AssignSource(AIconFontsImageList: TIconFontsImageList;
      const AFontName: string = ''): Boolean;
    procedure AddNewItem;
    procedure DeleteSelectedItem;
    procedure ClearAllImages;
    procedure UpdateGUI;
    procedure UpdateCharsToBuild;
    function SelectedIconFont: TIconFontItem;
    function GetFontName: string;
    procedure SetFontName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateForFont(AOwner: TComponent;
      const AFontName: string; const ASize: Integer = 32;
      const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone); virtual;
    constructor CreateForImageList(AOwner: TComponent;
      AIconFontsImageList: TIconFontsImageList;
      const AFontName: string = ''); virtual;
    procedure AssignImageList(const AIconFontsImageList: TIconFontsImageList;
      const AFontName: string = '');
    property FontName: string read GetFontName write SetFontName;
  end;

function ShowIconFontsCharMap(const AFontName: string;
  const ASize: Integer = 32;
  const AFontColor: TColor = clBlack;
  const AMaskColor: TColor = clWhite): string;

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

function ShowIconFontsCharMap(const AFontName: string;
  const ASize: Integer = 32;
  const AFontColor: TColor = clBlack;
  const AMaskColor: TColor = clWhite): string;
var
  IconFontsCharMapForm: TIconFontsCharMapForm;
begin
  IconFontsCharMapForm := TIconFontsCharMapForm.CreateForFont(nil, AFontName,
    24, AFontColor, AMaskColor);
  try
    if IconFontsCharMapForm.ShowModal = mrOk then
      Result := IconFontsCharMapForm.CharsEdit.Text
    else
      Result := '';
  finally
    IconFontsCharMapForm.Free;
  end;
end;

{ TIconFontsCharMapForm }

procedure TIconFontsCharMapForm.HelpButtonClick(Sender: TObject);
begin
  ShellExecute(handle, 'open',
    PChar('https://github.com/EtheaDev/IconFontsImageList/wiki/CharMap'), nil, nil,
    SW_SHOWNORMAL)
end;

procedure TIconFontsCharMapForm.UpdateCharsToBuild;
begin
  CharsEdit.Font.Size := 14;
  if DefaultFontName.Text <> '' then
  begin
    CharsEdit.Font.Name := DefaultFontName.Text;
    CharsEdit.Enabled := True;
  end
  else
  begin
    CharsEdit.Enabled := False;
    CopyToClipboardButton.Enabled := False;
  end;
end;

procedure TIconFontsCharMapForm.UpdateGUI;
var
  LIsItemSelected: Boolean;
  LItemFontName: string;
  LIconFontItem: TIconFontItem;
  {$IFNDEF UNICODE}
  S: WideString;
  {$ENDIF}
begin
  FUpdating := True;
  try
    LIconFontItem := SelectedIconFont;
    LIsItemSelected := LIconFontItem <> nil;
    CopyToClipboardButton.Enabled := CharsEdit.Text <> '';
    FontIconDec.Enabled := False;
    FontIconHex.Enabled := False;
    if LIsItemSelected then
    begin
      ImageGroup.Caption := Format(FIconIndexLabel,[LIconFontItem.Index]);
      LItemFontName := LIconFontItem.FontName;
      FontIconDec.Text := IntToStr(LIconFontItem.FontIconDec);
      FontIconHex.Text := LIconFontItem.FontIconHex;
      MainPanel.Invalidate;
    end
    else
    begin
      FontIconDec.Text := '0';
      FontIconHex.Text := '';
    end;
    MainImage.Canvas.Brush.Color :=  MainPanel.Color;
    MainImage.Canvas.FillRect(Rect(0, 0, MainImage.Height, MainImage.Height));
    if LIsItemSelected then
    begin
      if LIconFontItem.FontName <> '' then
        MainImage.Canvas.Font.Name := LIconFontItem.FontName
      else
        MainImage.Canvas.Font.Name := FCharMapList.FontName;
      MainImage.Canvas.Font.Height := MainImage.Height;
      if LIconFontItem.FontColor <> clNone then
        MainImage.Canvas.Font.Color := LIconFontItem.FontColor
      else
        MainImage.Canvas.Font.Color := FCharMapList.FontColor;
      if LIconFontItem.MaskColor <> clNone then
        MainImage.Canvas.Brush.Color := LIconFontItem.MaskColor
      else
        MainImage.Canvas.Brush.Color := FCharMapList.MaskColor;
      MainImage.Canvas.FillRect(Rect(0, 0, MainImage.Height, MainImage.Height));
      {$IFNDEF UNICODE}
      S := LIconFontItem.Character;
      TextOutW(MainImage.Canvas.Handle, 0, 0, PWideChar(S), 1);
      {$ELSE}
      MainImage.Canvas.TextOut(0, 0, LIconFontItem.Character);
      {$ENDIF}
    end;

    //UpdateIconFontListViewCaptions(ImageView);
  finally
    FUpdating := False;
  end;
end;

procedure TIconFontsCharMapForm.CancelButtonClick(Sender: TObject);
begin
  if not FBuilding then
  begin
    ModalResult := mrCancel;
    Close;
  end;
end;

procedure TIconFontsCharMapForm.DeleteSelectedItem;
var
  LIndex: Integer;
begin
  LIndex := ImageView.Selected.Index;
  FCharMapList.Delete(LIndex);
  UpdateIconFontListView(ImageView);
  if LIndex < ImageView.Items.Count then
    ImageView.ItemIndex := LIndex
  else if ImageView.Items.Count > 0 then
    ImageView.ItemIndex := LIndex-1;
  UpdateGUI;
end;

procedure TIconFontsCharMapForm.ClearAllImages;
begin
  Screen.Cursor := crHourglass;
  try
    FCharMapList.ClearIcons;
    ImageView.Clear;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsCharMapForm.DrawIconProgress(const ASender: TObject;
  const ACount: Integer;
  const AItem: TIconFontItem;
  var AProceed: Boolean);
var
  LPosition: Integer;
  LListItem: TListItem;
begin
  LPosition := Round((FMaxIcons-FFirstIcon)/(ACount)*AItem.Index/(FMaxIcons-FFirstIcon)*100);
  if ProgressBar.Position <> LPosition then
  begin
    ProgressBar.Position := LPosition;
    Application.ProcessMessages;
    AProceed := not FStopped;
  end;
  LListItem := ImageView.Items.Add;
  if ShowCaptionsCheckBox.Checked then
  begin
    LListItem.Caption := Format('%d%s$%s%s%s',
      [AItem.FontIconDec,sLineBreak,
       AItem.FontIconHex,sLineBreak,
       AItem.IconName]);
  end;
  LListItem.ImageIndex := LListItem.Index;
end;

constructor TIconFontsCharMapForm.Create(AOwner: TComponent);
begin
  FCharMapList := TIconFontsImageList.Create(Self);
  FCharMapList.OnDrawIcon := DrawIconProgress;
  FFirstTime := True;
  inherited;
end;

constructor TIconFontsCharMapForm.CreateForFont(AOwner: TComponent;
  const AFontName: string; const ASize: Integer = 32;
  const AFontColor: TColor = clNone; const AMaskColor: TColor = clNone);
begin
  Create(AOwner);
  FCharMapList.FontName := AFontName;
  FCharMapList.Size := ASize;
  if AFontColor <> clNone then
    FCharMapList.FontColor := AFontColor
  else
    FCharMapList.FontColor := clWindowText;
  if AMaskColor <> clNone then
    FCharMapList.MaskColor := AMaskColor
  else
    FCharMapList.MaskColor := clBtnFace;
end;

function TIconFontsCharMapForm.AssignSource(AIconFontsImageList: TIconFontsImageList;
  const AFontName: string = ''): Boolean;
var
  LFontName: string;
begin
  Result := False;
  if AFontName <> '' then
    LFontName := AFontName
  else
    LFontName := AIconFontsImageList.FontName;
  if FCharMapList.FontName <> LFontName then
  begin
    ClearAllImages;
    FCharMapList.FontName := LFontName;
    Result := True;
  end;
  if FCharMapList.Size <> AIconFontsImageList.Size then
  begin
    ClearAllImages;
    FCharMapList.Size := AIconFontsImageList.Size;
    Result := True;
  end;
  if FCharMapList.FontColor <> AIconFontsImageList.FontColor then
  begin
    ClearAllImages;
    FCharMapList.FontColor := AIconFontsImageList.FontColor;
    Result := True;
  end;
  if FCharMapList.MaskColor <> AIconFontsImageList.MaskColor then
  begin
    ClearAllImages;
    FCharMapList.MaskColor := AIconFontsImageList.MaskColor;
    Result := True;
  end;
  if Result then
    UpdateCharsToBuild;
end;

constructor TIconFontsCharMapForm.CreateForImageList(
  AOwner: TComponent;
  AIconFontsImageList: TIconFontsImageList;
  const AFontName: string = '');
begin
  Create(AOwner);
  AssignSource(AIconFontsImageList, AFontName);
end;

procedure TIconFontsCharMapForm.ImageViewDblClick(Sender: TObject);
begin
  if (SelectedIconFont <> nil) then
    CharsEdit.Text := CharsEdit.Text + SelectedIconFont.Character;
end;

procedure TIconFontsCharMapForm.ImageViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LSelected: TListItem;
  LIconFontItem: TIconFontItem;
begin
  if (ssCtrl in Shift) then
  begin
    LSelected := ImageView.GetItemAt(X,Y);
    if Assigned(LSelected) then
    begin
      LIconFontItem := FCharMapList.IconFontItems[LSelected.Index];
      CharsEdit.Text := CharsEdit.Text + LIconFontItem.Character;
    end;
  end;
end;

procedure TIconFontsCharMapForm.ImageViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    UpdateGUI;
end;

procedure TIconFontsCharMapForm.OKButtonClick(Sender: TObject);
begin
  if not FBuilding then
  begin
    ModalResult := mrOK;
    Close;
  end;
end;

procedure TIconFontsCharMapForm.DefaultFontNameSelect(Sender: TObject);
begin
  if FCharMapList.FontName <> DefaultFontName.Text then
  begin
    FCharMapList.ClearIcons;
    FCharMapList.FontName := DefaultFontName.Text;
    BuildAllIcons;
  end;
end;

procedure TIconFontsCharMapForm.DeleteButtonClick(Sender: TObject);
begin
  DeleteSelectedItem;
end;

procedure TIconFontsCharMapForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus;
end;

procedure TIconFontsCharMapForm.FormCreate(Sender: TObject);

  procedure InitColorBox(AColorBox: TColorBox);
  begin
    {$IFDEF UNICODE}
    AColorBox.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
      cbIncludeNone, cbIncludeDefault, cbCustomColor, cbCustomColors, cbPrettyNames];
    {$ENDIF}
    AColorBox.Selected := clNone;
  end;

begin
  FImageListCaption := ImageListGroup.Caption;
  ImageView.LargeImages := FCharMapList;
  ImageView.SmallImages := FCharMapList;
  Caption := Format(Caption, [IconFontsImageListVersion]);
  FUpdating := True;
  FIconFontItems := TIconFontItems.Create(FCharMapList, TIconFontItem);
  DefaultFontName.Items := Screen.Fonts;
  FIconIndexLabel := ImageGroup.Caption;
end;

procedure TIconFontsCharMapForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FIconFontItems);
  Screen.Cursors[crColorPick] := 0;
end;

procedure TIconFontsCharMapForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and FBuilding then
  begin
    FStopped := True;
    Key := #0;
  end;
end;

procedure TIconFontsCharMapForm.FormShow(Sender: TObject);
begin
  CharsEdit.Text := '';
  if FFirstTime or FStopped then
  begin
    FStopped := False;
    FCharMapList.ClearIcons;
    DefaultFontName.ItemIndex := -1;
    DefaultFontName.Text := '';
    FFirstTime := True;
  end;
end;

function TIconFontsCharMapForm.GetFontName: string;
begin
  Result := DefaultFontName.Text;
end;

procedure TIconFontsCharMapForm.EditChangeUpdateGUI(Sender: TObject);
begin
  UpdateGUI;
end;

function TIconFontsCharMapForm.SelectedIconFont: TIconFontItem;
begin
  if (ImageView.Selected <> nil) and (ImageView.Selected.Index < FCharMapList.IconFontItems.Count) then
    Result := FCharMapList.IconFontItems[ImageView.Selected.Index]
  else
    Result := nil;
end;

procedure TIconFontsCharMapForm.SetFontName(const Value: string);
begin
  if (Value <> '') and (DefaultFontName.Text <> Value) then
  begin
    DefaultFontName.ItemIndex := DefaultFontName.Items.IndexOf(Value);
    BuildAllIcons;
  end;
end;

procedure TIconFontsCharMapForm.ShowCaptionsCheckBoxClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Try
    UpdateIconFontListViewCaptions(ImageView, ShowCaptionsCheckBox.Checked);
  Finally
    Screen.Cursor := crDefault;
  End;
end;

procedure TIconFontsCharMapForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if FFirstTime then
  begin
    FFirstTime := False;
    FontName := FCharMapList.FontName;
    UpdateGUI;
  end;
end;

procedure TIconFontsCharMapForm.AddButtonClick(Sender: TObject);
begin
  AddNewItem;
end;

procedure TIconFontsCharMapForm.AddNewItem;
var
  LInsertIndex: Integer;
begin
  if (ImageView.Selected <> nil) then
    LInsertIndex := ImageView.Selected.Index +1
  else
    LInsertIndex := ImageView.Items.Count;
  ImageView.Selected := nil;
  FCharMapList.IconFontItems.Insert(LInsertIndex);
  UpdateIconFontListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
end;

procedure TIconFontsCharMapForm.AssignImageList(
  const AIconFontsImageList: TIconFontsImageList;
  const AFontName: string = '');
begin
  if AssignSource(AIconFontsImageList, AFontName) then
  begin
    FontName := '';
    FFirstTime := True;
  end;
end;

procedure TIconFontsCharMapForm.CopyToCipboardActionExecute(Sender: TObject);
begin
  CharsEdit.SelectAll;
  CharsEdit.CopyToClipboard;
end;

procedure TIconFontsCharMapForm.CopyToCipboardActionUpdate(Sender: TObject);
begin
  CopyToCipboardAction.Enabled := CharsEdit.Text <> '';
end;

procedure TIconFontsCharMapForm.BuildAllIcons(const ASurrogate: Boolean = False);
var
  LStart, LEnd: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    paClient.Enabled := False;
    paButtons.Enabled := False;
    ImageView.Enabled := False;
    IconBuilderGroupBox.Enabled := False;
    FStopped := False;
    if not ASurrogate then
    begin
      //Clear
      FIconsCount := 0;
      CharsEdit.Text := '';
      ImageListGroup.Caption := '';
      ClearAllImages;
      //Normal Chars
      LStart := $0001;
      LEnd := $FFFF;
    end
    else
    begin
      //Surrogate Pairs Chars
      LStart := $F0000;
      LEnd := $FFFFF;
    end;
    ImageView.Clear;
    FFirstIcon := FCharMapList.Count;
    FMaxIcons := LEnd - LStart;
    ProgressBar.Position := 0;
    FBuilding := True;
    Try
      ImageView.Items.BeginUpdate;
      ProgressBar.Visible := True;
      FIconsCount := FIconsCount + FCharMapList.AddIcons(
        LStart, //From Chr
        LEnd, //To Chr
        DefaultFontName.Text,
        FCharMapList.FontColor,
        FCharMapList.MaskColor,
        True);
      UpdateCharsToBuild;
      ImageListGroup.Caption := Format(FImageListCaption, [FIconsCount]);
    Finally
      ImageView.Items.EndUpdate;
      FBuilding := False;
      ProgressBar.Visible := False;
    End;
    ImageView.ItemIndex := FFirstIcon;
  finally
    IconBuilderGroupBox.Enabled := True;
    ImageView.Enabled := True;
    paButtons.Enabled := True;
    paClient.Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsCharMapForm.cbShowSurrogateClick(Sender: TObject);
begin
  BuildAllIcons(cbShowSurrogate.Checked);
end;

end.
