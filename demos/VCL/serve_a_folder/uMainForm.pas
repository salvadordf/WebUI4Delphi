unit uMainForm;

{$I ..\..\..\source\uWebUI.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FWindow, FSecondWindow : IWebUIWindow;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure FSecondWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure exit_app(const aEvent: IWebUIEventHandler);
    procedure events(const aEvent: IWebUIEventHandler);
    procedure switch_to_second_page(const aEvent: IWebUIEventHandler);
    procedure show_second_window(const aEvent: IWebUIEventHandler);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  LCount : integer;

implementation

{$R *.dfm}

uses
  uWebUIMiscFunctions;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow := nil;
  FSecondWindow := nil;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;
  FSecondWindow := nil;
  MainPanel.Enabled := WebUI.Initialize;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'SwitchToSecondPage') then
    switch_to_second_page(aEvent)
  else if (aEvent.Element = 'OpenNewWindow') then
    show_second_window(aEvent)
  else if (aEvent.Element = 'Exit') then
    exit_app(aEvent)
  else
    events(aEvent);
end;

procedure TMainForm.FSecondWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  exit_app(aEvent);
end;

procedure TMainForm.exit_app(const aEvent: IWebUIEventHandler);
begin
  WebUI.Exit;
  // You can't create more browsers after the exit call
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TMainForm.events(const aEvent: IWebUIEventHandler);
var
  LUrl : string;
begin
  case aEvent.EventType of
    WEBUI_EVENT_DISCONNECTED : Memo1.Lines.Add('disconnected.');
    WEBUI_EVENT_CONNECTED    : Memo1.Lines.Add('connected.');
    WEBUI_EVENT_MOUSE_CLICK  : Memo1.Lines.Add('click.');
    WEBUI_EVENT_NAVIGATION   :
      begin
        LUrl := aEvent.GetString;
        Memo1.Lines.Add('navigating to ' + LUrl);

        // Because we used `LWindow.Bind('', events);`
        // WebUI will block all `href` link clicks and sent here instead.
        // We can then control the behaviour of links as needed.
        aEvent.Window.Navigate(LUrl);
      end;
  end;
end;

procedure TMainForm.switch_to_second_page(const aEvent: IWebUIEventHandler);
begin
	// This function gets called every
	// time the user clicks on "SwitchToSecondPage"

	// Switch to `/second.html` in the same opened window.
  aEvent.Window.Show('second.html');
end;

procedure TMainForm.show_second_window(const aEvent: IWebUIEventHandler);
begin
	// This function gets called every
	// time the user clicks on "OpenNewWindow"

	// Show a new window, and navigate to `/second.html`
	// if it's already open, then switch in the same window
  FSecondWindow.Show('second.html');
end;

function my_files_handler(const filename: PWebUIChar; len: PInteger): PWebUIChar;
var
  LFilename, LResult : string;
  LBuffer : AnsiString;
begin
  LFilename := UTF8ToString(PAnsiChar(filename));

  if (CompareText(LFilename, '/test.txt') = 0) then
    begin
      // Const static file example
      // Note: The connection will drop if the content
      // does not have `<script src="webui.js"></script>`
      LResult := 'This is a embedded file content example.' + #0;
      LBuffer := UTF8Encode(LResult);
      len^    := length(LBuffer);
      Result  := PWebUIChar(@LBuffer[1]);
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
                   '</html>' + #0;

        LBuffer := UTF8Encode(LResult);
        len^    := length(LBuffer);
        Result  := PWebUIChar(@LBuffer[1]);
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

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
var
  LRoot : string;
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  FWindow       := TWebUIWindow.Create;
  FSecondWindow := TWebUIWindow.Create;

  // Bind HTML element IDs with a C functions
  FWindow.Bind('SwitchToSecondPage');
  FWindow.Bind('OpenNewWindow');
  FWindow.Bind('Exit');

  FSecondWindow.Bind('Exit');
  FSecondWindow.OnWebUIEvent := FSecondWindow_OnWebUIEvent;

  // Bind events
  FWindow.BindAllEvents;
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;

  // Make Deno as the `.ts` and `.js` interpreter
  FWindow.SetRuntime(Deno);

  // Set a custom files handler
  FWindow.SetFileHandler(my_files_handler);

  // Set window size
  FWindow.SetSize(800, 800);

  // Set window position
  FWindow.SetPosition(200, 200);

  // Set the web-server root folder for the first window
  LRoot := CustomAbsolutePath('..\assets\serve_a_folder\', True);
  FWindow.SetRootFolder(LRoot);
  FSecondWindow.SetRootFolder(LRoot);

  // Show a new window
  FWindow.Show('index.html');
end;

end.
