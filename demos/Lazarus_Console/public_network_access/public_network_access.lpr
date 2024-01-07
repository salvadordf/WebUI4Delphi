program public_network_access;

{$MODE Delphiunicode}

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

var
  LPrvWindow, LPubWindow : IWebUIWindow;
  LPrvHTML, LPubHTML, LPubUrl, LJavascript : string;

procedure app_exit(e: PWebUIEvent);
begin
	WebUI.Exit;
end;

procedure public_window_events(e: PWebUIEvent);
var
  LEvent : TWebUIEventHandler;
begin
  LEvent := TWebUIEventHandler.Create(e);

  case LEvent.EventType of
    WEBUI_EVENT_DISCONNECTED : LPrvWindow.Run('document.getElementById("Logs").value += "Disconnected.\n";');
    WEBUI_EVENT_CONNECTED    : LPrvWindow.Run('document.getElementById("Logs").value += "New connection.\n";');
  end;

  LEvent.Free;
end;

begin
  try
    WebUI := TWebUI.Create;
    {$IFDEF DEBUG}
    //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
    {$ENDIF}
    if WebUI.Initialize then
      begin
        LPrvHTML := '<!DOCTYPE html>' + CRLF +
                    '<html>' + CRLF +
                    '  <head>' + CRLF +
                    '    <meta charset="UTF-8">' + CRLF +
                    '    <script src="webui.js"></script>' + CRLF +
                    '    <title>Public Network Access Example</title>' + CRLF +
                    '    <style>' + CRLF +
                    '       body {' + CRLF +
                    '            font-family: ' + quotedstr('Arial') + ', sans-serif;' + CRLF +
                    '            color: white;' + CRLF +
                    '            background: linear-gradient(to right, #507d91, #1c596f, #022737);' + CRLF +
                    '            text-align: center;' + CRLF +
                    '            font-size: 18px;' + CRLF +
                    '        }' + CRLF +
                    '        button, input {' + CRLF +
                    '            padding: 10px;' + CRLF +
                    '            margin: 10px;' + CRLF +
                    '            border-radius: 3px;' + CRLF +
                    '            border: 1px solid #ccc;' + CRLF +
                    '            box-shadow: 0 3px 5px rgba(0,0,0,0.1);' + CRLF +
                    '            transition: 0.2s;' + CRLF +
                    '        }' + CRLF +
                    '        button {' + CRLF +
                    '            background: #3498db;' + CRLF +
                    '            color: #fff; ' + CRLF +
                    '            cursor: pointer;' + CRLF +
                    '            font-size: 16px;' + CRLF +
                    '        }' + CRLF +
                    '        h1 { text-shadow: -7px 10px 7px rgb(67 57 57 / 76%); }' + CRLF +
                    '        button:hover { background: #c9913d; }' + CRLF +
                    '        input:focus { outline: none; border-color: #3498db; }' + CRLF +
                    '    </style>' + CRLF +
                    '  </head>' + CRLF +
                    '  <body>' + CRLF +
                    '    <h1>WebUI - Public Network Access Example</h1>' + CRLF +
                    '    <br>' + CRLF +
                    '    The second public window is configured to be accessible from <br>' + CRLF +
                    '    any device in the public network. <br>' + CRLF +
                    '    <br>' + CRLF +
                    '    Second public window link: <br>' + CRLF +
                    '    <h1 id="urlSpan" style="color:#c9913d">...</h1>' + CRLF +
                    '    Second public window events: <br>' + CRLF +
                    '    <textarea id="Logs" rows="4" cols="50" style="width:80%"></textarea>' + CRLF +
                    '    <br>' + CRLF +
                    '    <button id="Exit">Exit</button>' + CRLF +
                    '  </body>' + CRLF +
                    '</html>';

        LPubHTML := '<!DOCTYPE html>' + CRLF +
                    '<html>' + CRLF +
                    '  <head>' + CRLF +
                    '    <meta charset="UTF-8">' + CRLF +
                    '    <script src="webui.js"></script>' + CRLF +
                    '    <title>Welcome to Public UI</title>' + CRLF +
                    '  </head>' + CRLF +
                    '  <body>' + CRLF +
                    '    <h1>Welcome to Public UI!</h1>' + CRLF +
                    '  </body>' + CRLF +
                    '</html>';

        // Create windows
        LPrvWindow := TWebUIWindow.Create;
        LPubWindow := TWebUIWindow.Create;

        // App
        WebUI.Timeout := 0; // Wait forever (never timeout)

        // Public Window
        LPubWindow.SetPublic(True);
        LPubWindow.Bind('', public_window_events);
        LPubWindow.ShowBrowser(LPubHTML, NoBrowser);
        LPubUrl := LPubWindow.Url;

        // Main Private Window
        LPrvWindow.Bind('Exit', app_exit);
        LPrvWindow.Show(LPrvHTML);

        // Set URL in the UI
        LJavascript := 'document.getElementById(' + quotedstr('urlSpan') + ').innerHTML = ' + quotedstr(LPubUrl) + ';';
        LPrvWindow.Run(LJavascript);

        // Wait until all windows get closed
        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
