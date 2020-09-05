program IconFontsImageListDemo;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  IconFontsImageListEditorUnit in '..\..\Packages\IconFontsImageListEditorUnit.pas' {IconFontsImageListEditor},
  UMainNew in '..\Source\UMainNew.pas' {MainForm},
  Icons.MaterialDesign in '..\..\Source\Fonts\Icons.MaterialDesign.pas',
  Icons.Utils in '..\..\Source\Fonts\Icons.Utils.pas',
  DImages in '..\Source\DImages.pas' {dmImages: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmImages, dmImages);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
