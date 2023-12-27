program call_c_from_js;

{$I ..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IFDEF DELPHI16_UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler;

var
  LWindow : TWebUIWindow;
  LMyHTML : string;

procedure my_function_string(e: PWebUIEvent);
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
  writeln('my_function_string 2: ' + str_2);
  LEvent.Free;
end;

procedure my_function_integer(e: PWebUIEvent);
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

procedure my_function_boolean(e: PWebUIEvent);
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

procedure my_function_raw_binary(e: PWebUIEvent);
var
  LEvent : TWebUIEventHandler;
  raw_1, raw_2 : string;
  len_1, len_2 : NativeUInt;
begin
  LEvent := TWebUIEventHandler.Create(e);

	// JavaScript:
	// webui.call('MyID_RawBinary', new Uint8Array([0x41]), new Uint8Array([0x42, 0x43]));

  raw_1 := LEvent.GetString;
  raw_2 := LEvent.GetStringAt(1);
  len_1 := LEvent.GetSize;
  len_2 := LEvent.GetSizeAt(1);

  // ******

  LEvent.Free;
end;

procedure my_function_with_response(e: PWebUIEvent);
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
  LWindow := nil;
  try
    try
      WebUI := TWebUI.Create;
      if WebUI.Initialize then
        begin
          LMyHTML := '<!DOCTYPE html>' +
                     '<html>' +
                     '  <head>' +
                     '    <meta charset="UTF-8">' +
                     '    <script src="webui.js"></script>' +
                     '    <title>Call C from JavaScript Example</title>' +
                     '    <style>' +
                     '       body {' +
                     '            font-family: ' + quotedstr('Arial') + ', sans-serif;' +
                     '            color: white;' +
                     '            background: linear-gradient(to right, #507d91, #1c596f, #022737);' +
                     '            text-align: center;' +
                     '            font-size: 18px;' +
                     '        }' +
                     '        button, input {' +
                     '            padding: 10px;' +
                     '            margin: 10px;' +
                     '            border-radius: 3px;' +
                     '            border: 1px solid #ccc;' +
                     '            box-shadow: 0 3px 5px rgba(0,0,0,0.1);' +
                     '            transition: 0.2s;' +
                     '        }' +
                     '        button {' +
                     '            background: #3498db;' +
                     '            color: #fff; ' +
                     '            cursor: pointer;' +
                     '            font-size: 16px;' +
                     '        }' +
                     '        h1 { text-shadow: -7px 10px 7px rgb(67 57 57 / 76%); }' +
                     '        button:hover { background: #c9913d; }' +
                     '        input:focus { outline: none; border-color: #3498db; }' +
                     '    </style>' +
                     '  </head>' +
                     '  <body>' +
                     '    <h1>WebUI - Call C from JavaScript</h1>' +
                     '    <p>Call C functions with arguments (<em>See the logs in your terminal</em>)</p>' +
                     '    <button onclick="webui.call(' + quotedstr('MyID_One') + ', ' + quotedstr('Hello') + ', ' + quotedstr('World') + ');">Call my_function_string()</button>' +
                     '    <br>' +
                     '    <button onclick="webui.call(' + quotedstr('MyID_Two') + ', 123, 456, 789);">Call my_function_integer()</button>' +
                     '    <br>' +
                     '    <button onclick="webui.call(' + quotedstr('MyID_Three') + ', true, false);">Call my_function_boolean()</button>' +
                     '    <br>' +
                     '    <button onclick="webui.call(' + quotedstr('MyID_RawBinary') + ', new Uint8Array([0x41,0x42,0x43]), big_arr);"> ' +
                     '     Call my_function_raw_binary()</button>' +
                     '    <br>' +
                     '    <p>Call a C function that returns a response</p>' +
                     '    <button onclick="MyJS();">Call my_function_with_response()</button>' +
                     '    <div>Double: <input type="text" id="MyInputID" value="2"></div>' +
                     '    <script>' +
                     '      const arr_size = 512 * 1000;' +
                     '      const big_arr = new Uint8Array(arr_size);' +
                     '      big_arr[0] = 0xA1;' +
                     '      big_arr[arr_size - 1] = 0xA2;' +
                     '      function MyJS() {' +
                     '        const MyInput = document.getElementById(' + quotedstr('MyInputID') + ');' +
                     '        const number = MyInput.value;' +
                     '        webui.call(' + quotedstr('MyID_Four') + ', number, 2).then((response) => {' +
                     '            MyInput.value = response;' +
                     '        });' +
                     '      }' +
                     '    </script>' +
                     '  </body>' +
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
