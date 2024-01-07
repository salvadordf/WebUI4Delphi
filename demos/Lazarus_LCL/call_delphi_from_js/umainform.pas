unit uMainForm;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, SyncObjs,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type

  { TMainForm }

  TMainForm = class(TForm)
    ShowBrowserBtn: TButton;
    MainPanel: TPanel;
    Memo1: TMemo;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
  private
    FWindow      : IWebUIWindow;
    FCritSection : TCriticalSection;
    FLog         : TStringList;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure my_function_string(const aEvent: IWebUIEventHandler);
    procedure my_function_integer(const aEvent: IWebUIEventHandler);
    procedure my_function_boolean(const aEvent: IWebUIEventHandler);
    procedure my_function_raw_binary(const aEvent: IWebUIEventHandler);
    procedure my_function_with_response(const aEvent: IWebUIEventHandler);
    procedure LogMessage(const aMessage: string);
    procedure FlushLog(Data: PtrInt);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow  := nil;
  CanClose := True;

  FreeAndNil(FCritSection);
  FreeAndNil(FLog);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCritSection := TCriticalSection.Create;
  FLog         := TStringList.Create;
  WebUI        := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;
  MainPanel.Enabled := WebUI.Initialize;
end;     

procedure TMainForm.LogMessage(const aMessage: string);
begin
  FCritSection.Acquire;
  FLog.Add(UTF8Encode(aMessage));
  FCritSection.Release;
end;

procedure TMainForm.FlushLog(Data: PtrInt);
begin
  FCritSection.Acquire;
  if (FLog.Count > 0) then
    begin
      Memo1.Lines.AddStrings(FLog);
      FLog.Clear;
    end;
  FCritSection.Release;
end;

procedure TMainForm.my_function_string(const aEvent: IWebUIEventHandler);
var
  str_1, str_2 : string;
begin
  // JavaScript:
  // webui.call('MyID_One', 'Hello', 'World`);

  str_1  := aEvent.GetString;
  str_2  := aEvent.GetStringAt(1);
  LogMessage('my_function_string 1: ' + str_1);
  LogMessage('my_function_string 2: ' + str_2);
  Application.QueueAsyncCall(FlushLog, 0);
end;

procedure TMainForm.my_function_integer(const aEvent: IWebUIEventHandler);
var
  number_1, number_2, number_3 : int64;
begin
  // JavaScript:
  // webui.call('MyID_Two', 123, 456, 789);

  number_1 := aEvent.GetInt;
  number_2 := aEvent.GetIntAt(1);
  number_3 := aEvent.GetIntAt(2);
  LogMessage('my_function_integer 1: ' + inttostr(number_1));
  LogMessage('my_function_integer 2: ' + inttostr(number_2));
  LogMessage('my_function_integer 3: ' + inttostr(number_3));   
  Application.QueueAsyncCall(FlushLog, 0);
end;

procedure TMainForm.my_function_boolean(const aEvent: IWebUIEventHandler);
var
  status_1, status_2 : boolean;
begin
  // JavaScript:
  // webui.call('MyID_Three', true, false);

  status_1  := aEvent.GetBool;
  status_2  := aEvent.GetBoolAt(1);
  LogMessage('my_function_boolean 1: ' + BoolToStr(status_1, true));
  LogMessage('my_function_boolean 2: ' + BoolToStr(status_2, true));
  Application.QueueAsyncCall(FlushLog, 0);
end;

procedure TMainForm.my_function_raw_binary(const aEvent: IWebUIEventHandler);
var
  LStream : TMemoryStream;
  LData   : byte;
  LHexStr : string;
begin
  LStream := TMemoryStream.Create;

  // JavaScript:
  // webui.call('MyID_RawBinary', new Uint8Array([0x41,0x42,0x43]), big_arr);

  if aEvent.GetStream(LStream) then
    begin
      LHexStr := 'my_function_raw_binary 1: ';

      while (LStream.Position < LStream.Size) do
        begin
          LData   := LStream.ReadByte;
          LHexStr := LHexStr + uppercase(inttohex(LData, 2));
        end;

      LogMessage(LHexStr);
      Application.QueueAsyncCall(FlushLog, 0);
    end;

  LStream.Free;
end;

procedure TMainForm.my_function_with_response(const aEvent: IWebUIEventHandler);
var
  number, times, res : int64;
begin
  // JavaScript:
  // webui.call('MyID_Four', number, 2).then(...)

  number := aEvent.GetInt;
  times  := aEvent.GetIntAt(1);
  res    := number * times;

  LogMessage('my_function_with_response: ' + inttostr(number) + ' * ' + inttostr(times) + ' = ' + inttostr(res));
  Application.QueueAsyncCall(FlushLog, 0);

  aEvent.ReturnInt(res);
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'MyID_One') then
    my_function_string(aEvent)
  else if (aEvent.Element = 'MyID_Two') then
    my_function_integer(aEvent)
  else if (aEvent.Element = 'MyID_Three') then
    my_function_boolean(aEvent)
  else if (aEvent.Element = 'MyID_Four') then
    my_function_with_response(aEvent)
  else if (aEvent.Element = 'MyID_RawBinary') then
    my_function_raw_binary(aEvent);
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
var
  LMyHTML : string;
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

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

  FWindow := TWebUIWindow.Create;
  FWindow.Bind('MyID_One');
  FWindow.Bind('MyID_Two');
  FWindow.Bind('MyID_Three');
  FWindow.Bind('MyID_Four');
  FWindow.Bind('MyID_RawBinary');
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;
  FWindow.Show(LMyHTML);
end;

end.

