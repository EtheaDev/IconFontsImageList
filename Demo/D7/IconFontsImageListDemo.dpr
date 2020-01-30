program IconFontsImageListDemo;

uses
  Forms,
  Themes,
  IconFontsImageListEditorUnit in '..\..\Packages\IconFontsImageListEditorUnit.pas' {IconFontsImageListEditor},
  UMain in '..\Source\UMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
//  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
