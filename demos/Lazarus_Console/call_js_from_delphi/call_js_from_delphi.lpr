program call_js_from_delphi;

{$MODE Delphiunicode}

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

var
  LWindow : IWebUIWindow;
  LMyHTML : string;

procedure my_function_exit(e: PWebUIEvent); cdecl;
begin
  WebUI.Exit;
end;

procedure my_function_count(e: PWebUIEvent); cdecl;
var
  LResult : string;
  LCount : integer;
begin
  // This function gets called every time the user clicks on "my_function_count"
  if not(LWindow.Script('return GetCount();', 0, LResult, 64)) then
    begin
      if not(LWindow.IsShown) then
        writeln('The window is closed.')
       else
        writeln('Javascript error : ' + LResult);
    end
   else
    begin
      LCount := StrToIntDef(LResult, 0);
      inc(LCount);
      LWindow.Run('SetCount(' + IntToStr(LCount) + ');');
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
        LMyHTML := '<!DOCTYPE html>' + CRLF +
                   '<html>' + CRLF +
                   '  <head>' + CRLF +
                   '    <meta charset="UTF-8">' + CRLF +
                   '    <script src="webui.js"></script>' + CRLF +
                   '    <title>Call JavaScript from C Example</title>' + CRLF +
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
                   '    <h1>WebUI - Call JavaScript from C</h1>' + CRLF +
                   '    <br>' + CRLF +
                   '    <h1 id="count">0</h1>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button OnClick="my_function_count();">Manual Count</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button id="MyTest" OnClick="AutoTest();">Auto Count (Every 500ms)</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button OnClick="my_function_exit();">Exit</button>' + CRLF +
                   '    <script>' + CRLF +
                   '      let count = 0;' + CRLF +
                   '      function GetCount() {' + CRLF +
                   '        return count;' + CRLF +
                   '      }' + CRLF +
                   '      function SetCount(number) {' + CRLF +
                   '        document.getElementById(' + quotedstr('count') + ').innerHTML = number;' + CRLF +
                   '        count = number;' + CRLF +
                   '      }' + CRLF +
                   '      function AutoTest(number) {' + CRLF +
                   '        setInterval(function(){ my_function_count(); }, 500);' + CRLF +
                   '      }' + CRLF +
                   '    </script>' + CRLF +
                   '  </body>' + CRLF +
                   '</html>';

        LWindow := TWebUIWindow.Create;
        LWindow.Bind('my_function_count', my_function_count);
        LWindow.Bind('my_function_exit', my_function_exit);
        LWindow.Show(LMyHTML);
        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
