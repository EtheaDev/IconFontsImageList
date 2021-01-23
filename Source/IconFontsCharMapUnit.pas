{******************************************************************************}
{                                                                              }
{       Icon Fonts ImageList: An extended ImageList for Delphi                 }
{       to simplify use of Icons (resize, colors and more...)                  }
{                                                                              }
{       Copyright (c) 2019-2021 (Ethea S.r.l.)                                 }
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
  , IconFontsImageListBase
  , IconFontsImageList
  , ActnList
  , IconFontsItems
  , IconFontsImage
  {$IFDEF DXE3+}, System.Actions{$ENDIF}
  ;

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
    MainImage: TIconFontImage;
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
    IconName: TEdit;
    IconNameLabel: TLabel;
    procedure FormCreate(Sender: TObject);
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
    FTotIcons: Integer;
    FIconsCount: Integer;
    FIconIndexLabel: string;
    FUpdating: Boolean;
    FCharMapList: TIconFontsImageList;
    FIconFontItems: TIconFontItems;
    FImageListCaption: string;
    procedure DrawIconProgress(const ASender: TObject; const ACount: Integer;
      const AItem: TIconFontItem);
    function AssignSource(AIconFontsImageList: TIconFontsImageListBase;
      const AFontName: TFontName = ''): Boolean;
    procedure AddNewItem;
    procedure ClearAllImages;
    procedure UpdateGUI;
    procedure UpdateCharsToBuild;
    function GetFontName: TFontName;
    procedure SetFontName(const Value: TFontName);
    //Events for notification from item to imagelist
    procedure CheckFontName(const AFontName: TFontName);
    procedure OnItemChanged(Sender: TIconFontItem);
    procedure GetOwnerAttributes(out AFontName: TFontName;
      out AFontColor, AMaskColor: TColor);
    procedure BuildList(Selected: Integer);
  public
    function SelectedIconFont: TIconFontItem;
    constructor Create(AOwner: TComponent); override;
    constructor CreateForFont(AOwner: TComponent;
      const AFontName: TFontName; const ASize: Integer = 32;
      const AFontColor: TColor = clDefault; const AMaskColor: TColor = clNone); virtual;
    constructor CreateForImageList(AOwner: TComponent;
      AIconFontsImageList: TIconFontsImageListBase;
      const AFontName: TFontName = ''); virtual;
    procedure AssignImageList(const AIconFontsImageList: TIconFontsImageListBase;
      const AFontName: TFontName = '');
    property FontName: TFontName read GetFontName write SetFontName;
  end;

function ShowIconFontsCharMap(const AFontName: TFontName;
  const ASize: Integer = 32;
  const AFontColor: TColor = clBlack;
  const AMaskColor: TColor = clWhite): string;

implementation

{$R *.dfm}

uses
  CommCtrl
  , TypInfo
  , ShellApi
  , IconFontsUtils
  {$IFDEF D2010+}
  , Icons.Utils
  {$ENDIF}
  ;

const
  crColorPick = -100;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function ShowIconFontsCharMap(const AFontName: TFontName;
  const ASize: Integer = 32;
  const AFontColor: TColor = clBlack;
  const AMaskColor: TColor = clWhite): string;
var
  IconFontsCharMapForm: TIconFontsCharMapForm;
begin
  IconFontsCharMapForm := TIconFontsCharMapForm.CreateForFont(nil, AFontName,
    ASize, AFontColor, AMaskColor);
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
    CharsEdit.Font.Name := FCharMapList.FontName;
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
  LItemFontName: TFontName;
  LIconFontItem: TIconFontItem;
begin
  FUpdating := True;
  try
    LIconFontItem := SelectedIconFont;
    LIsItemSelected := LIconFontItem <> nil;
    CopyToClipboardButton.Enabled := CharsEdit.Text <> '';
    if LIsItemSelected then
    begin
      ImageGroup.Caption := Format(FIconIndexLabel,[LIconFontItem.Index]);
      LItemFontName := LIconFontItem.FontName;
      FontIconDec.Text := IntToStr(LIconFontItem.FontIconDec);
      FontIconHex.Text := LIconFontItem.FontIconHex;
      IconName.Text := LIconFontItem.IconName;
      MainPanel.Invalidate;
    end
    else
    begin
      FontIconDec.Text := '0';
      FontIconHex.Text := '';
    end;
    if LIsItemSelected then
      MainImage.ImageIndex := LIconFontItem.Index
    else
      MainImage.ImageIndex := -1;
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

procedure TIconFontsCharMapForm.CheckFontName(const AFontName: TFontName);
begin
  //Don't check anything
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
  const AItem: TIconFontItem);
var
  LPosition: Integer;
  LTotCount: Integer;
begin
  LTotCount := (FMaxIcons-FFirstIcon);
  if ACount <>0 then
    FIconsCount := ACount
  else
    FIconsCount := FTotIcons+1;
  LPosition := Round(FIconsCount*100 / LTotCount);
  if ProgressBar.Position <> LPosition then
  begin
    ProgressBar.Position := LPosition;
    Application.ProcessMessages;
    if FStopped then
      Abort;
  end;
end;

constructor TIconFontsCharMapForm.Create(AOwner: TComponent);
begin
  FCharMapList := TIconFontsImageList.Create(Self);
  FCharMapList.OnDrawIcon := DrawIconProgress;
  FFirstTime := True;
  inherited;
  MainImage.ImageList := FCharMapList;
end;

constructor TIconFontsCharMapForm.CreateForFont(AOwner: TComponent;
  const AFontName: TFontName; const ASize: Integer = 32;
  const AFontColor: TColor = clDefault; const AMaskColor: TColor = clNone);
begin
  Create(AOwner);
  FCharMapList.FontName := AFontName;
  FCharMapList.Size := ASize;
  if AFontColor <> clDefault then
    FCharMapList.FontColor := AFontColor
  else
    FCharMapList.FontColor := clWindowText;
  if AMaskColor <> clNone then
    FCharMapList.MaskColor := AMaskColor
  else
    FCharMapList.MaskColor := clBtnFace;
end;

function TIconFontsCharMapForm.AssignSource(AIconFontsImageList: TIconFontsImageListBase;
  const AFontName: TFontName = ''): Boolean;
var
  LFontName: TFontName;
begin
  Result := False;
  if AFontName <> '' then
    LFontName := AFontName
  else
    LFontName := AIconFontsImageList.FontName;
  if LFontName = '' then
    Exit;
  if FCharMapList.FontName <> LFontName then
  begin
    FCharMapList.FontName := LFontName;
    Result := True;
  end;
  if FCharMapList.Size <> AIconFontsImageList.Size then
  begin
    FCharMapList.Size := AIconFontsImageList.Size;
    Result := True;
  end;
  if FCharMapList.FontColor <> AIconFontsImageList.FontColor then
  begin
    FCharMapList.FontColor := AIconFontsImageList.FontColor;
    Result := True;
  end;
  if FCharMapList.MaskColor <> AIconFontsImageList.MaskColor then
  begin
    FCharMapList.MaskColor := AIconFontsImageList.MaskColor;
    Result := True;
  end;
  ClearAllImages;
end;

constructor TIconFontsCharMapForm.CreateForImageList(
  AOwner: TComponent;
  AIconFontsImageList: TIconFontsImageListBase;
  const AFontName: TFontName = '');
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

procedure TIconFontsCharMapForm.OnItemChanged(Sender: TIconFontItem);
begin
  ;
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

procedure TIconFontsCharMapForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if ModalResult = mrOK then
    OKButton.SetFocus;
end;

procedure TIconFontsCharMapForm.FormCreate(Sender: TObject);
begin
  {$IFDEF D2010+}
  cbShowSurrogate.Visible := True;
  {$ELSE}
  cbShowSurrogate.Visible := False;
  {$ENDIF}
  FImageListCaption := ImageListGroup.Caption;
  ImageListGroup.Caption := '';
  ImageView.LargeImages := FCharMapList;
  ImageView.SmallImages := FCharMapList;
  Caption := Format(Caption, [IconFontsImageListVersion]);
  FUpdating := True;
  FIconFontItems := TIconFontItems.Create(FCharMapList, TIconFontItem,
    OnItemChanged, CheckFontName, GetOwnerAttributes);
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
  if FFirstTime or FStopped or (ImageView.Items.Count = 0) then
  begin
    FStopped := False;
    FCharMapList.ClearIcons;
    DefaultFontName.ItemIndex := -1;
    DefaultFontName.Text := '';
    FFirstTime := True;
  end;
  if ImageView.CanFocus then
    ImageView.SetFocus;
end;

function TIconFontsCharMapForm.GetFontName: TFontName;
begin
  Result := DefaultFontName.Text;
end;

procedure TIconFontsCharMapForm.GetOwnerAttributes(out AFontName: TFontName;
  out AFontColor, AMaskColor: TColor);
begin
  AFontName := FCharMapList.FontName;
  AFontColor := FCharMapList.FontColor;
  AMaskColor := FCharMapList.MaskColor;
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

procedure TIconFontsCharMapForm.SetFontName(const Value: TFontName);
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
  if FFirstTime and Assigned(FCharMapList) then
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
  //UpdateIconFontListView(ImageView);
  ImageView.ItemIndex := LInsertIndex;
end;

procedure TIconFontsCharMapForm.AssignImageList(
  const AIconFontsImageList: TIconFontsImageListBase;
  const AFontName: TFontName = '');
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
  {$IFDEF D2010+}
  LIconCollection: TIconCollection;
  {$ENDIF}
  LFontName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    paClient.Enabled := False;
    paButtons.Enabled := False;
    ImageView.Enabled := False;
    IconBuilderGroupBox.Enabled := False;
    FStopped := False;
    LFontName := DefaultFontName.Text;

    if not ASurrogate then
    begin
      {$IFDEF D2010+}
      cbShowSurrogate.Visible := True;
      {$ENDIF}
    end;
    if FCharMapList.Count > 0 then
      FFirstIcon := FCharMapList.Count-1
    else
      FFirstIcon := -1;
    ImageView.Clear;

    //Check for metadata font registered
    {$IFDEF D2010+}
    if TIconManager.Instance.FindCollection(LFontName, LIconCollection) then
    begin
      IconName.Visible := True;
      IconNameLabel.Visible := True;
      cbShowSurrogate.Visible := False;
      ProgressBar.Position := 0;
      FBuilding := True;
      Try
        //If metadata exists iterate to add icons into CharMap
        ProgressBar.Visible := True;
        ImageView.Items.BeginUpdate;
        FMaxIcons := 0;
        LIconCollection.ForEach(
          function (const Entry: TIconEntry): Boolean
          begin
            Result := True;
            Inc(FMaxIcons);
          end);
        FCharMapList.StopDrawing(True);
        try
          LIconCollection.ForEach(
            function (const Entry: TIconEntry): Boolean
            begin
              Result := True;
              FCharMapList.AddIcon(
                Entry.codepoint,
                Entry.name);
              Inc(FTotIcons);
            end);
        finally
          FCharMapList.StopDrawing(False);
          FCharMapList.RecreateBitmaps;
        end;
      Finally
        ImageView.Items.EndUpdate;
        FBuilding := False;
        ProgressBar.Visible := False;
        ImageListGroup.Caption := Format(FImageListCaption, [FIconsCount]);
        BuildList(FFirstIcon);
      End;
    end
    else
    {$ENDIF}
    begin
      IconName.Visible := False;
      IconNameLabel.Visible := False;
      if not ASurrogate then
      begin
        //Clear
        FTotIcons := 0;
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
      FMaxIcons := LEnd - LStart;
      ProgressBar.Position := 0;
      FBuilding := True;
      Try
        ImageView.Items.BeginUpdate;
        ProgressBar.Visible := True;
        FTotIcons := FTotIcons + FCharMapList.AddIcons(
          LStart, //From Chr
          LEnd, //To Chr
          LFontName,
          FCharMapList.FontColor,
          FCharMapList.MaskColor,
          True);
      Finally
        ImageView.Items.EndUpdate;
        FBuilding := False;
        ProgressBar.Visible := False;
        ImageListGroup.Caption := Format(FImageListCaption, [FIconsCount]);
        BuildList(FFirstIcon);
      End;
    end;
  finally
    IconBuilderGroupBox.Enabled := True;
    ImageView.Enabled := True;
    paButtons.Enabled := True;
    paClient.Enabled := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsCharMapForm.BuildList(Selected: Integer);
begin
  Screen.Cursor := crHourGlass;
  try
    UpdateIconFontListView(ImageView, '');

    if Selected < -1 then
      Selected := -1
    else if (Selected = -1) and (ImageView.Items.Count > 0) then
      Selected := 0
    else if Selected >= ImageView.Items.Count then
      Selected := ImageView.Items.Count - 1;

    ImageView.ItemIndex := Selected;
    if Self.Visible and (ImageView.ItemIndex >= 0) and ImageView.CanFocus then
      ImageView.SetFocus;
    UpdateCharsToBuild;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TIconFontsCharMapForm.cbShowSurrogateClick(Sender: TObject);
begin
  BuildAllIcons(cbShowSurrogate.Checked);
end;

end.
