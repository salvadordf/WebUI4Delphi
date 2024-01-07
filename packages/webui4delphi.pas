{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit webui4delphi;

{$warn 5023 off : no warning about unused units}
interface

uses
  uWebUI, uWebUIBase64, uWebUIConstants, uWebUIEventHandler, 
  uWebUILibFunctions, uWebUIMiscFunctions, uWebUITypes, uWebUIWindow, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('webui4delphi', @Register);
end.
