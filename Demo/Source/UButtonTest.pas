unit UButtonTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, IconFontsImageList, Vcl.StdCtrls, Vcl.ExtCtrls,
  SVGIconImageList, SVGIconImageListBase, IconFontsImageListBase;

type
  TForm22 = class(TForm)
    Button: TButton;
    IconFontsImageList: TIconFontsImageList;
    ActionList1: TActionList;
    Action: TAction;
    SelectThemeRadioGroup: TRadioGroup;
    Button1: TButton;
    ActionList2: TActionList;
    Action2: TAction;
    ImageList: TImageList;
    Button2: TButton;
    SVGIconImageList: TSVGIconImageList;
    ActionList3: TActionList;
    Action3: TAction;
    procedure ActionExecute(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SelectThemeRadioGroupClick(Sender: TObject);
  private
    procedure UpdateGUI;
  public
    { Public declarations }
  end;

var
  Form22: TForm22;

implementation

{$R *.dfm}

uses
  Themes
  , IconFontsUtils;

procedure TForm22.ActionExecute(Sender: TObject);
begin
  showmessage('click');
end;

procedure TForm22.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  //Build available VCL Styles
  SelectThemeRadioGroup.Items.Clear;
  for I := 0 to High(TStyleManager.StyleNames) do
    SelectThemeRadioGroup.Items.Add(TStyleManager.StyleNames[I]);

  UpdateGUI;
end;

procedure TForm22.RadioGroup1Click(Sender: TObject);
begin
  ;
end;

procedure TForm22.SelectThemeRadioGroupClick(Sender: TObject);
var
  LStyleName: string;
begin
  Screen.Cursor := crHourGlass;
  try
    LStyleName := SelectThemeRadioGroup.Items[SelectThemeRadioGroup.ItemIndex];
    TStyleManager.TrySetStyle(LStyleName);
    //Override default: use Windows 10 blue color for Windows and Windows10 Style
    if SameText(LStyleName,'Windows10') then
      IconFontsImageList.UpdateIconsAttributes(RGB(0, 120, 215), clBtnFace)
    else
      UpdateIconFontsColorByStyle(IconFontsImageList);
    SVGIconImageList.FixedColor := IconFontsImageList.FontColor;
    UpdateGUI;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm22.UpdateGUI;
begin
  Button.Action := Action;
end;

end.
