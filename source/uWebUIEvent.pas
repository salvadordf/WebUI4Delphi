unit uWebUIEvent;

{$I uWebUI.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils, Winapi.ShlObj, System.Math,
  {$ELSE}
  Windows, Classes, SysUtils, ShlObj, Math,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  TWebUIEvent = class

  end;

implementation

end.
