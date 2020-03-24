program IconFontImageDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UIconFontImageFMX in '..\Source\UIconFontImageFMX.pas' {IconFontImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIconFontImageForm, IconFontImageForm);
  Application.Run;
end.
