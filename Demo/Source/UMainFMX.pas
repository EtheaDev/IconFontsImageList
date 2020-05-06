unit UMainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects, FMX.MultiresBitmap, System.Rtti, System.Messaging,
  FMX.IconFontsImageList, FMX.ListBox, FMX.Colors, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Edit, FMX.EditBox, FMX.SpinBox;

type
  TIconFontImageListForm = class(TForm)
    NextButton: TButton;
    Panel1: TPanel;
    edtColor: TColorComboBox;
    IconFontsImageList: TIconFontsImageList;
    RandomButton: TButton;
    IconsLabel: TLabel;
    CurrentLabel: TLabel;
    AutoSizeCheckBox: TCheckBox;
    PrevButton: TButton;
    ShowEditorButton: TButton;
    ImageView: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    SpinBox1: TSpinBox;
    ListBoxItem3: TListBoxItem;
    V: TPanel;
    Glyph2: TGlyph;
    Glyph1: TGlyph;
    Glyph: TGlyph;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure edtColorChange(Sender: TObject);
    procedure RandomButtonClick(Sender: TObject);
    procedure AutoSizeCheckBoxChange(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure ShowEditorButtonClick(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
  private
    procedure UpdateGUI;
  public
    { Public declarations }
  end;

var
  IconFontImageListForm: TIconFontImageListForm;

implementation

uses
  System.Math
  , FMX.Consts
  , FMX.IconFontsImageListEditorUnit;

{$R *.fmx}

procedure TIconFontImageListForm.NextButtonClick(Sender: TObject);
begin
  if IconFontsImageList.Count-1 = Glyph.ImageIndex  then
    Glyph.ImageIndex := 0
  else
    Glyph.ImageIndex := Glyph.ImageIndex +1;
  UpdateGUI;
end;

procedure TIconFontImageListForm.PrevButtonClick(Sender: TObject);
begin
  if Glyph.ImageIndex = 0 then
    Glyph.ImageIndex := IconFontsImageList.Count-1
  else
    Glyph.ImageIndex := Glyph.ImageIndex -1;
  UpdateGUI;
end;

procedure TIconFontImageListForm.RandomButtonClick(Sender: TObject);
var
  LRand1, LRand2: Integer;
  LRandomCount: Integer;
begin
  LRandomCount := 100;
  LRand1 := 61441+Random(4000);
  LRand2 := LRand1+LRandomCount-1;

  //Test for Icons with surrogate pairs
  (*
  LRand1 := $F0100;
  LRand2 := $F0207;
  *)

  //Generate Icons
  Glyph.ImageIndex := -1;
  IconFontsImageList.AddIcons(LRand1, LRand2);
  Glyph.ImageIndex := IconFontsImageList.Count - LRandomCount;
  UpdateGUI;
end;

procedure TIconFontImageListForm.ShowEditorButtonClick(Sender: TObject);
begin
  EditIconFontsImageList(IconFontsImageList);
end;

procedure TIconFontImageListForm.SpinBox1Change(Sender: TObject);
begin
  IconFontsImageList.Size := Round(SpinBox1.Value);
end;

procedure TIconFontImageListForm.UpdateGUI;
begin
  IconsLabel.Text := Format('Total icons: %d', [IconFontsImageList.Count]);
  CurrentLabel.Text := Format('Current: %d', [Glyph.ImageIndex]);
end;

procedure TIconFontImageListForm.AutoSizeCheckBoxChange(Sender: TObject);
begin
  IconFontsImageList.AutoSizeBitmaps := AutoSizeCheckBox.IsChecked;
end;

procedure TIconFontImageListForm.edtColorChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to IconFontsImageList.Source.Count-1 do
    (IconFontsImageList.Source.Items[I] as TIconFontsSourceItem).FontColor := edtColor.Color;
end;

procedure TIconFontImageListForm.FormCreate(Sender: TObject);
begin
  UpdateGUI;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
