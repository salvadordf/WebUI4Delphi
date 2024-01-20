program serve_a_folder;

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants, uWebUIMiscFunctions;

var
  LWindow, LSecondWindow : IWebUIWindow;
  LCount : integer;
  LRoot : string;

procedure exit_app(e: PWebUIEvent);
begin
	WebUI.Exit;
end;

procedure events(e: PWebUIEvent);
var
  LEvent : TWebUIEventHandler;
  LUrl : string;
begin
  LEvent := TWebUIEventHandler.Create(e);

  case LEvent.EventType of
    WEBUI_EVENT_DISCONNECTED : writeln('disconnected.');
    WEBUI_EVENT_CONNECTED    : writeln('connected.');
    WEBUI_EVENT_MOUSE_CLICK  : writeln('click.');
    WEBUI_EVENT_NAVIGATION   :
      begin
        LUrl := LEvent.GetString;
        writeln('navigating to ' + LUrl);

        // Because we used `LWindow.Bind('', events);`
        // WebUI will block all `href` link clicks and sent here instead.
        // We can then control the behaviour of links as needed.
        LEvent.Window.Navigate(LUrl);
      end;
  end;

  LEvent.Free;
end;

procedure switch_to_second_page(e: PWebUIEvent);
var
  LEvent : TWebUIEventHandler;
begin
  LEvent := TWebUIEventHandler.Create(e);

	// This function gets called every
	// time the user clicks on "SwitchToSecondPage"

	// Switch to `/second.html` in the same opened window.
  LEvent.Window.Show('second.html');

  LEvent.Free;
end;

procedure show_second_window(e: PWebUIEvent);
begin
	// This function gets called every
	// time the user clicks on "OpenNewWindow"

	// Show a new window, and navigate to `/second.html`
	// if it's already open, then switch in the same window
  LSecondWindow.Show('second.html');
end;

function my_files_handler(const filename: PWebUIChar; len: PInteger): PWebUIChar;
var
  LFilename, LResult : string;
begin
  LFilename := UTF8ToString(PAnsiChar(filename));
  WriteLn('File: ' + LFilename);

  if (CompareText(LFilename, '/test.txt') = 0) then
    begin
      // Const static file example
      // Note: The connection will drop if the content
      // does not have `<script src="webui.js"></script>`
      LResult := 'This is a embedded file content example.';
      Result  := StringToPWebUIChar(LResult, len^);
    end
   else
    if (CompareText(LFilename, '/dynamic.html') = 0) then
      begin
        // Dynamic file example
        inc(LCount);

        LResult := '<html>' + CRLF +
                   '   This is a dynamic file content example. <br>' + CRLF +
                   '   Count: ' + inttostr(LCount) + ' <a href="dynamic.html">[Refresh]</a><br>' + CRLF +
                   '   <script src="webui.js"></script>' + CRLF + // To keep connection with WebUI
                   '</html>';
        Result  := StringToPWebUIChar(LResult, len^);
      end
     else
      begin
        // Other files:
        // A NULL return will make WebUI
        // looks for the file locally.
        len^   := 0;
        Result := nil;
      end;
end;

begin
  try
    WebUI := TWebUI.Create;
    {$IFDEF DEBUG}
    //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
    {$ENDIF}
    if WebUI.Initialize then
      begin
        LWindow       := TWebUIWindow.Create;
        LSecondWindow := TWebUIWindow.Create;

        // Bind HTML element IDs with a C functions
        LWindow.Bind('SwitchToSecondPage', switch_to_second_page);
        LWindow.Bind('OpenNewWindow', show_second_window);
        LWindow.Bind('Exit', exit_app);
        LSecondWindow.Bind('Exit', exit_app);

        // Bind events
        LWindow.Bind('', events);

        // Make Deno as the `.ts` and `.js` interpreter
        LWindow.SetRuntime(Deno);

        // Set a custom files handler
        LWindow.SetFileHandler(my_files_handler);

        // Set window size
        LWindow.SetSize(800, 800);

        // Set window position
        LWindow.SetPosition(200, 200);

        // Set the web-server root folder for the first window
        {$IFDEF MSWINDOWS}
        LRoot := CustomAbsolutePath('..\assets\serve_a_folder\', True);
        {$ELSE}
        LRoot := CustomAbsolutePath('../assets/serve_a_folder/', True);
        {$ENDIF}
        LWindow.SetRootFolder(LRoot);
        LSecondWindow.SetRootFolder(LRoot);

        // Show a new window
        LWindow.Show('index.html');

        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
