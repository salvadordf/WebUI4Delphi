program custom_web_server;

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
  uWebUIConstants;

var
  LWindow : TWebUIWindow;

procedure my_backend_func(e: PWebUIEvent);
var
  LEvent : TWebUIEventHandler;
  number_1, number_2, number_3 : int64;
begin
  LEvent := TWebUIEventHandler.Create(e);

	// JavaScript:
	// my_backend_func(123, 456, 789);
	// or webui.my_backend_func(...);

  number_1 := LEvent.GetIntAt(0);
  number_2 := LEvent.GetIntAt(1);
  number_3 := LEvent.GetIntAt(2);
  writeln('my_backend_func 1: ' + inttostr(number_1));
  writeln('my_backend_func 2: ' + inttostr(number_2));
  writeln('my_backend_func 3: ' + inttostr(number_3));

  LEvent.Free;
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
        LWindow.Navigate(LUrl);
      end;
  end;

  LEvent.Free;
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

          // Bind all events
          LWindow.Bind('', events);

          // Bind HTML elements with C functions
          LWindow.Bind('my_backend_func', my_backend_func);

          // Set web server network port WebUI should use
          // this mean `webui.js` will be available at:
          // http://localhost:8081/webui.js
          LWindow.SetPort(8081);

          // Show a new window and show our custom web server
          // Assuming the custom web server is running on port
          // 8080...
          // Run the \assets\custom_web_server\simple_web_server.py script to create a simple web server
          LWindow.Show('http://localhost:8080/');

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
