program Minimal;

{$MODE Delphiunicode}

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uWebUI, uWebUIWindow, uWebUITypes;

var
  LWindow : IWebUIWindow;

begin
  try
    WebUI := TWebUI.Create;
    {$IFDEF DEBUG}
    //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
    {$ENDIF}
    if WebUI.Initialize then
      begin
        LWindow := TWebUIWindow.Create;
        LWindow.Show('<html><head><script src="webui.js"></script></head> Hello World ! </html>');
        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
