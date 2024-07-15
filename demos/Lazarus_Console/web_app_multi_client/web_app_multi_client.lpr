program web_app_multi_client;

{$MODE Delphiunicode}

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants, uWebUIMiscFunctions;

var
  LWindow : IWebUIWindow;
  LPath, LUrl, LPublicInput : string;
  LPrivateInput_arr : array [0..255] of string;
  LUsers_count, LTab_count : integer;
  LContents : TStringList;

procedure exit_app(e: PWebUIEvent); cdecl;
begin
	WebUI.Exit;
end;

procedure save(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
begin
  LEvent := TWebUIEventHandler.Create(e);
  // Get input value and save it in the array
  LPrivateInput_arr[LEvent.ClientID] := LEvent.GetString;
  LEvent.Free;
end;

procedure saveAll(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
begin
  LEvent := TWebUIEventHandler.Create(e);
  // Get input value and save it
  LPublicInput := LEvent.GetString;
  // Update all users
  LEvent.Window.Run('document.getElementById("publicInput").value = "' + LPublicInput + '";');
  LEvent.Free;
end;

procedure events(e: PWebUIEvent); cdecl;
var
  LEvent : TWebUIEventHandler;
  LCookies : string;
  LClientID : TWebUIClientID;
  LConnectiondID : TWebUIConnectionID;
begin
	// This function gets called every time
	// there is an event

  LEvent := TWebUIEventHandler.Create(e);

  // Full web browser cookies
  LCookies := LEvent.Cookies;

  // Static client (Based on web browser cookies)
  LClientID := LEvent.ClientID;

  // Dynamic client connection ID (Changes on connect/disconnect events)
  LConnectiondID := LEvent.ConnectionID;

  case LEvent.EventType of
    WEBUI_EVENT_DISCONNECTED :
      begin
        // Disconnection
        if (LTab_count > 0) then
          dec(LTab_count);
      end;

    WEBUI_EVENT_CONNECTED    :
      begin
        // New connection
        if (LUsers_count < succ(LClientID)) then   // +1 because it start from 0
          LUsers_count := succ(LClientID);

        inc(LTab_count);
      end;
  end;


  // Update this current user only

  // status
  LEvent.RunClient('document.getElementById("status").innerText = "Connected!";');

  // userNumber
  LEvent.RunClient('document.getElementById("userNumber").innerText = "' + inttostr(LClientID) + '";');

  // connectionNumber
  LEvent.RunClient('document.getElementById("connectionNumber").innerText = "' + inttostr(LConnectiondID) + '";');

  // privateInput
  LEvent.RunClient('document.getElementById("privateInput").value = "' + LPrivateInput_arr[LClientID] +'";');

  // publicInput
  LEvent.RunClient('document.getElementById("publicInput").value = "' + LPublicInput + '";');


  // Update all connected users

  // userCount
  LEvent.RunClient('document.getElementById("userCount").innerText = "' + inttostr(LUsers_count) + '";');

  // tabCount
  LEvent.RunClient('document.getElementById("tabCount").innerText = "' + inttostr(LTab_count) + '";');

  LEvent.Free;

  if LTab_count = 0 then
    WebUI.Exit;
end;

begin
  try
    LUsers_count := 0;
    LTab_count   := 0;

    WebUI := TWebUI.Create;
    {$IFDEF DEBUG}
    //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
    {$ENDIF}
    if WebUI.Initialize then
      begin
        // Allow multi-user connection
        WebUI.SetConfig(multi_client, True);

        // Allow cookies
        WebUI.SetConfig(use_cookies, True);

        // Create new window
        LWindow := TWebUIWindow.Create;

        // Bind HTML with a C functions
        LWindow.Bind('save', save);
        LWindow.Bind('saveAll', saveAll);
        LWindow.Bind('exit_app', exit_app);

        // Bind all events
        LWindow.BindAllEvents(events);

        {$IFDEF MSWINDOWS}
        LPath := CustomAbsolutePath('..\assets\web_app_multi_client\', True);
        {$ELSE}
        LPath := CustomAbsolutePath('../assets/web_app_multi_client/', True);
        {$ENDIF}

        LPath := LPath + 'index.html';

        if FileExists(LPath) then
          begin
            LContents := TStringList.Create;
            LContents.LoadFromFile(LPath);
            // Start server only
            LUrl := LWindow.StartServer(LContents.Text);
            LContents.Free;
            // Open a new page in the default native web browser
            WebUI.OpenURL(LUrl);

            WebUI.Wait;
          end;
      end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
