unit UIconFontImageFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Objects, FMX.MultiresBitmap, System.Rtti, System.Messaging,
  FMX.IconFontsImageList, FMX.ListBox, FMX.Colors;

type
  TIconFontImageForm = class(TForm)
    IconFontImage: TIconFontImage;
    Button: TButton;
    Panel1: TPanel;
    edtColor: TColorComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure IconFontImageResize(Sender: TObject);
    procedure edtColorChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IconFontImageForm: TIconFontImageForm;

implementation

uses
  System.Math
  , FMX.Consts;

{$R *.fmx}

procedure TIconFontImageForm.ButtonClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to IconFontImage.MultiResBitmap.Count -1 do
    (IconFontImage.MultiResBitmap.Items[I] as TIconFontFixedBitmapItem).Character :=
      WideChar(Ord((IconFontImage.MultiResBitmap.Items[I]as TIconFontFixedBitmapItem).Character)+1);
  IconFontImage.Repaint;
end;

procedure TIconFontImageForm.edtColorChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to IconFontImage.MultiResBitmap.Count -1 do
    (IconFontImage.MultiResBitmap.Items[I] as TIconFontFixedBitmapItem).FontColor :=
      edtColor.Color;
  IconFontImage.Repaint;
end;

procedure TIconFontImageForm.FormCreate(Sender: TObject);
begin
  (IconFontImage.MultiResBitmap.Items[0] as TIconFontFixedBitmapItem).Character := #61448;
end;

procedure TIconFontImageForm.IconFontImageResize(Sender: TObject);
begin
  ;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

end.
