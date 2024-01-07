unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
  private
    FWindow : IWebUIWindow;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure my_function_exit(const aEvent: IWebUIEventHandler);
    procedure my_function_count(const aEvent: IWebUIEventHandler);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow  := nil;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;
  MainPanel.Enabled := WebUI.Initialize;
end;

procedure TMainForm.my_function_exit(const aEvent: IWebUIEventHandler);
begin
  WebUI.Exit;
  // You can't create more browsers after the exit call
  TThread.ForceQueue(nil,
    procedure
    begin
      Close;
    end);
end;

procedure TMainForm.my_function_count(const aEvent: IWebUIEventHandler);
var
  LResult : string;
  LCount : integer;
begin
  // This function gets called every time the user clicks on "MyButton1"
  if not(FWindow.Script('return GetCount();', 0, LResult, 64)) then
    begin
      if not(FWindow.IsShown) then
        writeln('The window is closed.')
       else
        writeln('Javascript error : ' + LResult);
    end
   else
    begin
      LCount := StrToIntDef(LResult, 0);
      inc(LCount);
      FWindow.Run('SetCount(' + IntToStr(LCount) + ');');
    end;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'MyButton1') then
    my_function_count(aEvent)
  else if (aEvent.Element = 'MyButton2') then
    my_function_exit(aEvent);
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
             '    <button id="MyButton1">Manual Count</button>' + CRLF +
             '    <br>' + CRLF +
             '    <button id="MyTest" OnClick="AutoTest();">Auto Count (Every 500ms)</button>' + CRLF +
             '    <br>' + CRLF +
             '    <button id="MyButton2">Exit</button>' + CRLF +
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
             '        setInterval(function(){ webui.call(' + quotedstr('MyButton1') + '); }, 500);' + CRLF +
             '      }' + CRLF +
             '    </script>' + CRLF +
             '  </body>' + CRLF +
             '</html>';

  FWindow := TWebUIWindow.Create;
  FWindow.Bind('MyButton1');
  FWindow.Bind('MyButton2');
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;
  FWindow.Show(LMyHTML);
end;

end.
