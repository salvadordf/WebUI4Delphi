unit uMainForm;

{$I ..\..\..\source\uWebUI.inc}

interface

uses
  {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
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
    PythonBtn: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
    procedure PythonBtnClick(Sender: TObject);
  private
    FWindow : IWebUIWindow;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure my_backend_func(const aEvent: IWebUIEventHandler);
    procedure events(const aEvent: IWebUIEventHandler);
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

procedure TMainForm.PythonBtnClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
  LOldDir, LNewDir : string;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  LNewDir := CustomAbsolutePath('..\assets\custom_web_server\');
  LOldDir := GetCurrentDir;
  chdir(GetModulePath);

  // Python must be installed
  ExecuteFile('python.exe', LNewDir + 'simple_web_server.py', LNewDir, SW_SHOWNORMAL);

  chdir(LOldDir);
  {$ENDIF}

  {$IFDEF LINUX}
  // TO-DO: Find a way to run simple_web_server.py in Linux
  {$ENDIF}

  {$IFDEF MACOSX}
  // TO-DO: Find a way to run simple_web_server.py in MacOS
  {$ENDIF}

  PythonBtn.Enabled      := False;
  ShowBrowserBtn.Enabled := True;
end;

procedure TMainForm.my_backend_func(const aEvent: IWebUIEventHandler);
var
  number_1, number_2, number_3 : int64;
begin
	// JavaScript:
	// my_backend_func(123, 456, 789);
	// or webui.my_backend_func(...);

  number_1 := aEvent.GetIntAt(0);
  number_2 := aEvent.GetIntAt(1);
  number_3 := aEvent.GetIntAt(2);
  Memo1.Lines.Add('my_backend_func 1: ' + inttostr(number_1));
  Memo1.Lines.Add('my_backend_func 2: ' + inttostr(number_2));
  Memo1.Lines.Add('my_backend_func 3: ' + inttostr(number_3));
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

        // Because we used `FWindow.BindAllEvents;`
        // WebUI will block all `href` link clicks and sent here instead.
        // We can then control the behaviour of links as needed.
        FWindow.Navigate(LUrl);
      end;
  end;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'my_backend_func') then
    my_backend_func(aEvent)
   else
    events(aEvent);
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  FWindow := TWebUIWindow.Create;

  // Bind all events
  FWindow.BindAllEvents;

  // Bind HTML elements with C functions
  FWindow.Bind('my_backend_func');

  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;

  // Set the web-server/WebSocket port that WebUI should
  // use. This means `webui.js` will be available at:
  // http://localhost:MY_PORT_NUMBER/webui.js
  FWindow.SetPort(8081);

  // Show a new window and show our custom web server
  // Assuming the custom web server is running on port
  // 8080...
  // Run the \assets\custom_web_server\simple_web_server.py script to create a simple web server
  FWindow.Show('http://localhost:8080/');
end;

end.
