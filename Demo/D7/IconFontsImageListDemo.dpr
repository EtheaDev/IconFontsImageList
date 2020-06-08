program IconFontsImageListDemo;

uses
  Forms,
  Themes,
  IconFontsImageListEditorUnit in '..\..\Packages\IconFontsImageListEditorUnit.pas' {IconFontsImageListEditor},
  UMainOld in '..\Source\UMainOld.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

//  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
end.
