//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

program IconFontsImageListDemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMainFMX in '..\Source\UMainFMX.pas' {IconFontImageListForm},
  FMX.IconFontsImageListEditorUnit in '..\..\Packages\FMX.IconFontsImageListEditorUnit.pas' {IconFontsImageListEditorFMX};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TIconFontImageListForm, IconFontImageListForm);
  Application.Run;
end.
