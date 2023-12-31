unit uMainForm;

interface

uses
  WinApi.Windows, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    Memo1: TMemo;
    OpenDefBrowserBtn: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
    procedure OpenDefBrowserBtnClick(Sender: TObject);
  private
    FPrvWindow, FPubWindow : IWebUIWindow;
    FPubUrl : string;
    procedure FPubWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure FPrvWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  uWebUIMiscFunctions;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FPrvWindow := nil;
  FPubWindow := nil;
  CanClose   := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FPrvWindow := nil;
  FPubWindow := nil;
  MainPanel.Enabled := WebUI.Initialize;
end;

procedure TMainForm.FPrvWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
	WebUI.Exit;
  // You can't create more browsers after the exit call
  TThread.ForceQueue(nil,
    procedure
    begin
      Close;
    end);
end;

procedure TMainForm.FPubWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  case aEvent.EventType of
    WEBUI_EVENT_DISCONNECTED : FPrvWindow.Run('document.getElementById("Logs").value += "Disconnected.\n";');
    WEBUI_EVENT_CONNECTED    : FPrvWindow.Run('document.getElementById("Logs").value += "New connection.\n";');
  end;
end;

procedure TMainForm.OpenDefBrowserBtnClick(Sender: TObject);
begin
  ExecuteFile(FPubUrl, '', '', SW_SHOWNORMAL);
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
var
  LPrvHTML, LPubHTML, LJavascript : string;
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

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
  FPrvWindow := TWebUIWindow.Create;
  FPubWindow := TWebUIWindow.Create;

  // App
  WebUI.Timeout := 0; // Wait forever (never timeout)

  // Public Window
  FPubWindow.SetPublic(True);
  FPubWindow.BindAllEvents;
  FPubWindow.OnWebUIEvent := FPubWindow_OnWebUIEvent;
  FPubWindow.ShowBrowser(LPubHTML, NoBrowser);
  FPubUrl := FPubWindow.Url;

  // Main Private Window
  FPrvWindow.Bind('Exit');
  FPrvWindow.OnWebUIEvent := FPrvWindow_OnWebUIEvent;
  FPrvWindow.Show(LPrvHTML);

  // Set URL in the UI
  LJavascript := 'document.getElementById(' + quotedstr('urlSpan') + ').innerHTML = ' + quotedstr(FPubUrl) + ';';
  FPrvWindow.Run(LJavascript);

  if (length(FPubUrl) > 0) then
    begin
      OpenDefBrowserBtn.Enabled := True;
      ShowBrowserBtn.Enabled    := False;
    end;
end;

end.
