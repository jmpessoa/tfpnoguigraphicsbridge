{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tfpnoguigraphicsbridge_pack;

interface

uses
  FPNoGUIGraphicsBridge, regnoguigraphicsbridge, GridData, ViewPort, 
  FPColorBridge, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('regnoguigraphicsbridge', @regnoguigraphicsbridge.Register);
end;

initialization
  RegisterPackage('tfpnoguigraphicsbridge_pack', @Register);
end.
