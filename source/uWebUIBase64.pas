unit uWebUIBase64;

{$I uWebUI.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
  Windows, Classes, SysUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  TWebUIBase64 = class
    public
      class function Encode(const str : PWebUIChar) : PWebUIChar;
      class function Decode(const str : PWebUIChar) : PWebUIChar;
  end;

implementation

uses
  uWebUI;

class function TWebUIBase64.Encode(const str : PWebUIChar) : PWebUIChar;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_encode(str)
   else
    Result := nil;
end;

class function TWebUIBase64.Decode(const str : PWebUIChar) : PWebUIChar;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_decode(str)
   else
    Result := nil;
end;

end.
