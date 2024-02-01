program call_delphi_from_js;

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

procedure my_function_string(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
  str_1, str_2 : string;
begin
  LEvent := TWebUIEventHandler.Create(e);

  // JavaScript:
  // webui.call('MyID_One', 'Hello', 'World`);

  str_1  := LEvent.GetString;
  str_2  := LEvent.GetStringAt(1);
  writeln('my_function_string 1: ' + str_1);
  writeln('my_function_string 2: ' + str_2);  // The emoji is badly writen to the console but str_2 has the correct string
  LEvent.Free;
end;

procedure my_function_integer(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
  number_1, number_2, number_3 : int64;
begin
  LEvent := TWebUIEventHandler.Create(e);

  // JavaScript:
  // webui.call('MyID_Two', 123, 456, 789);

  number_1 := LEvent.GetInt;
  number_2 := LEvent.GetIntAt(1);
  number_3 := LEvent.GetIntAt(2);
  writeln('my_function_integer 1: ' + inttostr(number_1));
  writeln('my_function_integer 2: ' + inttostr(number_2));
  writeln('my_function_integer 3: ' + inttostr(number_3));
  LEvent.Free;
end;

procedure my_function_boolean(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
  status_1, status_2 : boolean;
begin
  LEvent := TWebUIEventHandler.Create(e);

  // JavaScript:
  // webui.call('MyID_Three', true, false);

  status_1  := LEvent.GetBool;
  status_2  := LEvent.GetBoolAt(1);
  writeln('my_function_boolean 1: ' + BoolToStr(status_1, true));
  writeln('my_function_boolean 2: ' + BoolToStr(status_2, true));
  LEvent.Free;
end;

procedure my_function_raw_binary(e: PWebUIEvent); cdecl;
var
  LEvent  : TWebUIEventHandler;
  LStream : TMemoryStream;
  LData   : byte;
  LHexStr : string;
begin
  LEvent  := TWebUIEventHandler.Create(e);
  LStream := TMemoryStream.Create;

  // JavaScript:
  // webui.call('MyID_RawBinary', new Uint8Array([0x41,0x42,0x43]), big_arr);

  if LEvent.GetStream(LStream) then
    begin
      LHexStr := 'my_function_raw_binary 1: ';

      while (LStream.Position < LStream.Size) do
        begin
          LData   := LStream.ReadByte;
          LHexStr := LHexStr + uppercase(inttohex(LData, 2));
        end;

      writeln(LHexStr);
    end;

  LStream.Free;
  LEvent.Free;
end;

procedure my_function_with_response(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
  number, times, res : int64;
begin
  LEvent := TWebUIEventHandler.Create(e);

  // JavaScript:
  // webui.call('MyID_Four', number, 2).then(...)

  number := LEvent.GetInt;
  times  := LEvent.GetIntAt(1);
  res    := number * times;

  writeln('my_function_with_response: ' + inttostr(number) + ' * ' + inttostr(times) + ' = ' + inttostr(res));

  LEvent.ReturnInt(res);
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
        LMyHTML := '<!DOCTYPE html>' + CRLF +
                   '<html>' + CRLF +
                   '  <head>' + CRLF +
                   '    <meta charset="UTF-8">' + CRLF +
                   '    <script src="webui.js"></script>' + CRLF +
                   '    <title>Call C from JavaScript Example</title>' + CRLF +
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
                   '    <h1>WebUI - Call C from JavaScript</h1>' + CRLF +
                   '    <p>Call C functions with arguments (<em>See the logs in your terminal</em>)</p>' + CRLF +
                   '    <button onclick="webui.call(' + quotedstr('MyID_One') + ', ' + quotedstr('Hello World') + ', ' + quotedstr('\u{1F3DD}') + ');">Call my_function_string()</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button onclick="webui.call(' + quotedstr('MyID_Two') + ', 123, 456, 789);">Call my_function_integer()</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button onclick="webui.call(' + quotedstr('MyID_Three') + ', true, false);">Call my_function_boolean()</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <button onclick="webui.call(' + quotedstr('MyID_RawBinary') + ', new Uint8Array([0x41,0x42,0x43]), big_arr);"> ' + CRLF +
                   '     Call my_function_raw_binary()</button>' + CRLF +
                   '    <br>' + CRLF +
                   '    <p>Call a C function that returns a response</p>' + CRLF +
                   '    <button onclick="MyJS();">Call my_function_with_response()</button>' + CRLF +
                   '    <div>Double: <input type="text" id="MyInputID" value="2"></div>' + CRLF +
                   '    <script>' + CRLF +
                   '      const arr_size = 512 * 1000;' + CRLF +
                   '      const big_arr = new Uint8Array(arr_size);' + CRLF +
                   '      big_arr[0] = 0xA1;' + CRLF +
                   '      big_arr[arr_size - 1] = 0xA2;' + CRLF +
                   '      function MyJS() {' + CRLF +
                   '        const MyInput = document.getElementById(' + quotedstr('MyInputID') + ');' + CRLF +
                   '        const number = MyInput.value;' + CRLF +
                   '        webui.call(' + quotedstr('MyID_Four') + ', number, 2).then((response) => {' + CRLF +
                   '            MyInput.value = response;' + CRLF +
                   '        });' + CRLF +
                   '      }' + CRLF +
                   '    </script>' + CRLF +
                   '  </body>' + CRLF +
                   '</html>';

        LWindow := TWebUIWindow.Create;
        LWindow.Bind('MyID_One', my_function_string);
        LWindow.Bind('MyID_Two', my_function_integer);
        LWindow.Bind('MyID_Three', my_function_boolean);
        LWindow.Bind('MyID_Four', my_function_with_response);
        LWindow.Bind('MyID_RawBinary', my_function_raw_binary);
        LWindow.Show(LMyHTML);
        WebUI.Wait;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
