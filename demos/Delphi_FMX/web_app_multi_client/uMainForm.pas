unit uMainForm;

{$I ..\..\..\source\uWebUI.inc}

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
    FPublicInput : string;
    FPrivateInput_arr : array [0..255] of string;
    FUsers_count, FTab_count : integer;

    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure exit_app(const aEvent: IWebUIEventHandler);
    procedure save(const aEvent: IWebUIEventHandler);
    procedure saveAll(const aEvent: IWebUIEventHandler);
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

  FWindow := nil;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;

  if WebUI.Initialize then
    begin
      MainPanel.Enabled := True;

      // Allow multi-user connection
      WebUI.SetConfig(multi_client, True);

      // Allow cookies
      WebUI.SetConfig(use_cookies, True);
    end;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'exit_app') then
    exit_app(aEvent)
  else if (aEvent.Element = 'save') then
    save(aEvent)
  else  if (aEvent.Element = 'saveAll') then
    saveAll(aEvent)
  else
    events(aEvent);
end;

procedure TMainForm.exit_app(const aEvent: IWebUIEventHandler);
begin
  WebUI.Exit;
  // You can't create more browsers after the exit call
  TThread.ForceQueue(nil,
    procedure
    begin
      Close;
    end);
end;

procedure TMainForm.save(const aEvent: IWebUIEventHandler);
begin
  // Get input value and save it in the array
  FPrivateInput_arr[aEvent.ClientID] := aEvent.GetString;
end;

procedure TMainForm.saveAll(const aEvent: IWebUIEventHandler);
begin
  // Get input value and save it
  FPublicInput := aEvent.GetString;
  // Update all users
  aEvent.Window.Run('document.getElementById("publicInput").value = "' + FPublicInput + '";');
end;

procedure TMainForm.events(const aEvent: IWebUIEventHandler);
var
  LCookies : string;
  LClientID : TWebUIClientID;
  LConnectiondID : TWebUIConnectionID;
begin
	// This function gets called every time
	// there is an event

  // Full web browser cookies
  LCookies := aEvent.Cookies;

  // Static client (Based on web browser cookies)
  LClientID := aEvent.ClientID;

  // Dynamic client connection ID (Changes on connect/disconnect events)
  LConnectiondID := aEvent.ConnectionID;

  case aEvent.EventType of
    WEBUI_EVENT_DISCONNECTED :
      begin
        // Disconnection
        if (FTab_count > 0) then
          dec(FTab_count);
      end;

    WEBUI_EVENT_CONNECTED    :
      begin
        // New connection
        if (FUsers_count < succ(LClientID)) then   // +1 because it start from 0
          FUsers_count := succ(LClientID);

        inc(FTab_count);
      end;
  end;


  // Update this current user only

  // status
  aEvent.RunClient('document.getElementById("status").innerText = "Connected!";');

  // userNumber
  aEvent.RunClient('document.getElementById("userNumber").innerText = "' + inttostr(LClientID) + '";');

  // connectionNumber
  aEvent.RunClient('document.getElementById("connectionNumber").innerText = "' + inttostr(LConnectiondID) + '";');

  // privateInput
  aEvent.RunClient('document.getElementById("privateInput").value = "' + FPrivateInput_arr[LClientID] +'";');

  // publicInput
  aEvent.RunClient('document.getElementById("publicInput").value = "' + FPublicInput + '";');


  // Update all connected users

  // userCount
  aEvent.RunClient('document.getElementById("userCount").innerText = "' + inttostr(FUsers_count) + '";');

  // tabCount
  aEvent.RunClient('document.getElementById("tabCount").innerText = "' + inttostr(FTab_count) + '";');

  if FTab_count = 0 then
    exit_app(aEvent);
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
var
  LPath, LUrl : string;
  LContents : TStringList;
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  // Create new window
  FWindow := TWebUIWindow.Create;

  // Bind HTML with a C functions
  FWindow.Bind('save');
  FWindow.Bind('saveAll');
  FWindow.Bind('exit_app');

  // Bind all events
  FWindow.BindAllEvents;
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;

  {$IFDEF MSWINDOWS}
  LPath := CustomAbsolutePath('..\assets\web_app_multi_client\', True);
  {$ELSE}
  LPath := CustomAbsolutePath('../assets/web_app_multi_client/', True);
  {$ENDIF}

  LPath := LPath + 'index.html';

  if FileExists(LPath) then
    try
      LContents := nil;
      try
        LContents := TStringList.Create;
        LContents.LoadFromFile(LPath);
        // Start server only
        LUrl := FWindow.StartServer(LContents.Text);
      finally
        if assigned(LContents) then
          LContents.Free;
      end;

      // Open a new page in the default native web browser
      WebUI.OpenURL(LUrl);
    except
      on e : exception do
        if CustomExceptionHandler('TMainForm.ShowBrowserBtnClick', e) then raise;
    end;
end;

end.
