program IconFontsImageListDemo;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  IconFontsImageListEditorUnit in '..\..\Packages\IconFontsImageListEditorUnit.pas' {IconFontsImageListEditor},
  UMain in '..\Source\UMain.pas' {MainForm},
  Icons.MaterialDesign in '..\..\Source\Fonts\Icons.MaterialDesign.pas',
  Icons.Utils in '..\..\Source\Fonts\Icons.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
