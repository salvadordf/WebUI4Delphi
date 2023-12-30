program text_editor;

{$I ..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants, uWebUIMiscFunctions;

var
  LWindow : TWebUIWindow;

procedure Close(e: PWebUIEvent);
begin
	writeln('Exit.');

	// Close all opened windows
	WebUI.Exit;
end;

begin
  LWindow := nil;
  try
    try
      WebUI := TWebUI.Create;
      {$IFDEF DEBUG}
      WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
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
    finally
      if assigned(LWindow) then
        FreeAndNil(LWindow);

      DestroyWebUI;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
