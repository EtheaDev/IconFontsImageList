// Delphi 2010
{$IFDEF VER210}
  {$DEFINE D2010+}
{$ENDIF}

// Delphi XE
{$IFDEF VER220}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
{$ENDIF}

// Delphi XE2
{$IFDEF VER230}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
{$ENDIF}

// Delphi XE3
{$IFDEF VER240}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
{$ENDIF}

// Delphi XE4
{$IFDEF VER250}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
{$ENDIF}

// Delphi XE5
{$IFDEF VER260}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
{$ENDIF}

// Delphi XE6
{$IFDEF VER270}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
{$ENDIF}

// Delphi XE7
{$IFDEF VER280}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
{$ENDIF}

// Delphi XE8
{$IFDEF VER290}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
{$ENDIF}

// Delphi 10 Seattle
{$IFDEF VER300}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
{$ENDIF}

// Delphi 10.1 Berlin
{$IFDEF VER310}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
{$ENDIF}

// Delphi 10.2 Tokyo
{$IFDEF VER320}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
{$ENDIF}

// Delphi 10.3 Rio
{$IFDEF VER330}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
{$ENDIF}

// Delphi 10.4 Sydney
{$IFDEF VER340}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
{$ENDIF}

// Delphi 11 Alexandria
{$IFDEF VER350}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11+}
{$ENDIF}

// Delphi 12
{$IFDEF VER360}
  {$DEFINE D2010+}
  {$DEFINE DXE+}
  {$DEFINE DXE2+}
  {$DEFINE DXE3+}
  {$DEFINE DXE4+}
  {$DEFINE DXE5+}
  {$DEFINE DXE6+}
  {$DEFINE DXE7+}
  {$DEFINE DXE8+}
  {$DEFINE D10_0+}
  {$DEFINE D10_1+}
  {$DEFINE D10_2+}
  {$DEFINE D10_3+}
  {$DEFINE D10_4+}
  {$DEFINE D11+}
  {$DEFINE D12+}
{$ENDIF}

//GDI+ support available from DXE4
{$IFDEF DXE4+}
  {$Define GDI+}
{$ENDIF}

//StoreBitmap option introduced in Delphi 10.3 Rio
{$IFDEF D10_3+}
  {$Define HasStoreBitmapProperty}
{$ENDIF}

{$IFDEF D10_3+}
  {$Define HiDPISupport}
{$ENDIF}
