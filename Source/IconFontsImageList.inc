//StoreBitmap option introduced in Delphi 10.3 Rio
{$IF Defined(VER210) //Delphi 2010
  or Defined(VER220) //Delphi XE
  or Defined(VER230) //Delphi XE2
  or Defined(VER240) //Delphi XE3
  or Defined(VER250) //Delphi XE4
  or Defined(VER260) //Delphi XE5
  or Defined(VER270) //Delphi XE6
  or Defined(VER280) //Delphi XE7
  or Defined(VER290) //Delphi XE8
  or Defined(VER300) //Delphi 10 Seattle
  or Defined(VER310) //Delphi 10.1 Berlin
  or Defined(VER320) //Delphi 10.2 Tokyo
  }
  {$Define NeedStoreBitmapProperty}
{$IFEND}

{$IF Defined(VER330)}
  {$Define HiDPISupport}
{$IFEND}
