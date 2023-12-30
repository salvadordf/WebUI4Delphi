program text_editor;

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants, uWebUIMiscFunctions;

var
  LWindow : IWebUIWindow;

procedure Close(e: PWebUIEvent);
begin
	writeln('Exit.');

	// Close all opened windows
	WebUI.Exit;
end;

begin
  try
    WebUI := TWebUI.Create;
    {$IFDEF DEBUG}
    //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
    {$ENDIF}
    if WebUI.Initialize then
      begin
        LWindow := TWebUIWindow.Create;

        // Set the web-server root folder for the first window
        LWindow.SetRootFolder(CustomAbsolutePath('..\assets\text_editor\', True));

        // Bind HTML elements with the specified ID to C functions
        LWindow.Bind('__close-btn', Close);

        // Show the window, preferably in a chromium based browser
        if not(LWindow.ShowBrowser('index.html', ChromiumBased)) then
          LWindow.Show('index.html');

        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
