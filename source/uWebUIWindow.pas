unit uWebUIWindow;

{$I uWebUI.inc}

{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
  Windows, Classes, SysUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  /// <summary>
  /// Window wrapper for Window objects in WebUI.
  /// </summary>
  TWebUIWindow = class
    protected
      FID  : TWebUIWindowID;

      function GetInitialized : boolean;
      function GetIsShown : boolean;
      function GetUrl : string;
      function GetParentProcessID : NativeUInt;
      function GetChildProcessID : NativeUInt;

    public
      /// <summary>
      /// Create a new WebUI window object.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_new_window)</see></para>
      /// </remarks>
      constructor Create; overload;
      /// <summary>
      /// Create a new webui window object using a specified window number.
      /// </summary>
      /// <param name="windowId">The window number (should be > 0, and < WEBUI_MAX_IDS).</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_new_window_id)</see></para>
      /// </remarks>
      constructor Create(windowId : TWebUIWindowID; createWebUIWindow: boolean = True); overload;
      /// <summary>
      /// Close the window and free all memory resources.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_destroy)</see></para>
      /// </remarks>
      procedure   DestroyWindow;
      /// <summary>
      /// Bind a specific html element click event with a function. Empty element means all events.
      /// </summary>
      /// <param name="element_">The HTML ID.</param>
      /// <param name="func_">The callback function.</param>
      /// <returns>Returns a unique bind ID.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
      /// </remarks>
      function    Bind(const element_: string; func_: TWebUIBindCallback): TWebUIBindID; overload;
      /// <summary>
      /// Bind a specific HTML element click event with a function. Empty element means all events.
      /// </summary>
      /// <param name="element_">The element ID.</param>
      /// <param name="func_">The callback as myFunc(Window, EventType, Element, EventNumber, BindID).</param>
      /// <returns>Returns unique bind ID.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_bind)</see></para>
      /// </remarks>
      function    Bind(const element_: string; func_: TWebUIInterfaceEventCallback): TWebUIBindID; overload;
      /// <summary>
      /// Show a window using embedded HTML, or a file. If the window is already open, it will be refreshed.
      /// </summary>
      /// <param name="content">The HTML, URL, Or a local file.</param>
      /// <returns>Returns True if showing the window is successed.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show)</see></para>
      /// </remarks>
      function    Show(const content : string) : boolean;
      /// <summary>
      /// Same as `webui_show()`. But using a specific web browser.
      /// </summary>
      /// <param name="content">The HTML, URL, Or a local file.</param>
      /// <param name="browser">The web browser to be used.</param>
      /// <returns>Returns True if showing the window is successed.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_browser)</see></para>
      /// </remarks>
      function    ShowBrowser(const content : string; browser : TWebUIBrowsers) : boolean;
      /// <summary>
      /// Set the window in Kiosk mode (Full screen).
      /// </summary>
      /// <param name="status">True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_kiosk)</see></para>
      /// </remarks>
      procedure   SetKiosk(status: boolean);
      /// <summary>
      /// Close a specific window only. The window object will still exist.
      /// </summary>
      /// <param name="window">The window number.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_close)</see></para>
      /// </remarks>
      procedure   Close;
      /// <summary>
      /// Set the web-server root folder path for a specific window.
      /// </summary>
      /// <param name="path">The local folder full path.</param>
      /// <returns>Returns True if the function was successful.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_root_folder)</see></para>
      /// </remarks>
      function    SetRootFolder(const path : string) : boolean;
      /// <summary>
      /// Set the default embedded HTML favicon.
      /// </summary>
      /// <param name="icon">The icon as string: `<svg>...</svg>`.</param>
      /// <param name="icon_type">The icon type: `image/svg+xml`.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_icon)</see></para>
      /// </remarks>
      procedure   SetIcon(const icon, icon_type : string);
      /// <summary>
      /// Safely send raw data to the UI.
      /// </summary>
      /// <param name="window">The window number.</param>
      /// <param name="function_">The JavaScript function to receive raw data: `function * myFunc(myData){}`.</param>
      /// <param name="raw">The raw data buffer.</param>
      /// <param name="size">The raw data size in bytes.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_send_raw)</see></para>
      /// </remarks>
      procedure   SendRaw(const function_: string; const raw: Pointer; size: NativeUInt);
      /// <summary>
      /// Set a window in hidden mode. Should be called before `webui_show()`.
      /// </summary>
      /// <param name="status">The status: True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_hide)</see></para>
      /// </remarks>
      procedure   SetHide(status: boolean);
      /// <summary>
      /// Set the window size.
      /// </summary>
      /// <param name="width">The window width.</param>
      /// <param name="height">The window height.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_size)</see></para>
      /// </remarks>
      procedure   SetSize(width, height: cardinal);
      /// <summary>
      /// Set the window position.
      /// </summary>
      /// <param name="x">The window X.</param>
      /// <param name="y">The window Y.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_position)</see></para>
      /// </remarks>
      procedure   SetPosition(x, y: cardinal);
      /// <summary>
      /// Set the web browser profile to use. An empty `name` and `path` means the default user profile. Need to be called before `webui_show()`.
      /// </summary>
      /// <param name="name">The web browser profile name.</param>
      /// <param name="path">The web browser profile full path.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_profile)</see></para>
      /// </remarks>
      procedure   SetProfile(const name, path: string);
      /// <summary>
      /// Allow a specific window address to be accessible from a public network.
      /// </summary>
      /// <param name="status">True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_public)</see></para>
      /// </remarks>
      procedure   SetPublic(status: boolean);
      /// <summary>
      /// Navigate to a specific URL.
      /// </summary>
      /// <param name="url">Full HTTP URL.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_navigate)</see></para>
      /// </remarks>
      procedure   Navigate(const Url: string);
      /// <summary>
      /// Delete a specific window web-browser local folder profile.
      /// </summary>
      /// <remarks>
      /// <para>This can break functionality of other windows if using the same web-browser.</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_delete_profile)</see></para>
      /// </remarks>
      procedure   DeleteProfile;
      /// <summary>
      /// Set a custom web-server network port to be used by WebUI.
      /// This can be useful to determine the HTTP link of `webui.js` in case
      /// you are trying to use WebUI with an external web-server like NGNIX
      /// </summary>
      /// <param name="port">The web-server network port WebUI should use.</param>
      /// <returns>Returns True if the port is free and usable by WebUI.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_port)</see></para>
      /// </remarks>
      function    SetPort(port : NativeUInt): boolean;
      /// <summary>
      /// Run JavaScript without waiting for the response.
      /// </summary>
      /// <param name="script_">The JavaScript to be run.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_run)</see></para>
      /// </remarks>
      procedure   Run(const script_: string);
      /// <summary>
      /// Run JavaScript and get the response back.
      /// Make sure your local buffer can hold the response.
      /// </summary>
      /// <param name="script_">The JavaScript to be run.</param>
      /// <param name="timeout">The execution timeout.</param>
      /// <param name="buffer">The local buffer to hold the response.</param>
      /// <param name="buffer_length">The local buffer size.</param>
      /// <returns>Returns True if there is no execution error.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_script)</see></para>
      /// </remarks>
      function    Script(const script_: string; timeout: NativeUInt; var buffer: string; buffer_length: NativeUInt): boolean;
      /// <summary>
      /// Chose between Deno and Nodejs as runtime for .js and .ts files.
      /// </summary>
      /// <param name="runtime">Deno or Nodejs.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_runtime)</see></para>
      /// </remarks>
      procedure   SetRuntime(runtime: TWebUIRuntime);
      /// <summary>
      /// Get a free window number that can be used with `webui_new_window_id()`.
      /// </summary>
      /// <returns>Returns the first available free window number. Starting from 1.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_new_window_id)</see></para>
      /// </remarks>
      class function GetNewWindowID : TWebUIWindowID;
      /// <summary>
      /// Window number or Window ID.
      /// </summary>
      property ID                : TWebUIWindowID   read FID;
      /// <summary>
      /// Returns true if the Window was created successfully.
      /// </summary>
      property Initialized       : boolean          read GetInitialized;
      /// <summary>
      /// Check if the specified window is still running.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_is_shown)</see></para>
      /// </remarks>
      property IsShown          : boolean           read GetIsShown;
      /// <summary>
      /// Get the full current URL.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_url)</see></para>
      /// </remarks>
      property Url              : string            read GetUrl;
      /// <summary>
      /// Get the ID of the parent process (The web browser may re-create another new process).
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_parent_process_id)</see></para>
      /// </remarks>
      property ParentProcessID  : NativeUInt        read GetParentProcessID;
      /// <summary>
      /// Get the ID of the last child process.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_child_process_id)</see></para>
      /// </remarks>
      property ChildProcessID   : NativeUInt        read GetChildProcessID;
  end;

implementation

uses
  uWebUI;

constructor TWebUIWindow.Create;
begin
  inherited Create;

  if (WebUI <> nil) and WebUI.Initialized then
    FID := webui_new_window()
   else
    FID := 0;
end;

constructor TWebUIWindow.Create(windowId : TWebUIWindowID; createWebUIWindow: boolean);
begin
  inherited Create;

  if (WebUI <> nil) and WebUI.Initialized then
    begin
      if createWebUIWindow then
        begin
          if (windowId > 0) and (windowId < WEBUI_MAX_IDS) then
            FID := webui_new_window_id(windowId)
           else
            FID := webui_new_window();
        end
       else
        FID := windowId;
    end
   else
    FID := 0;
end;

procedure TWebUIWindow.DestroyWindow;
begin
  if Initialized then
    begin
      webui_destroy(FID);
      FID := 0;
    end;
end;

function TWebUIWindow.GetInitialized : boolean;
begin
  Result := (WebUI <> nil) and
            WebUI.Initialized and
            (FID <> 0);
end;

function TWebUIWindow.GetIsShown : boolean;
begin
  Result := Initialized and
            webui_is_shown(FID);
end;

function TWebUIWindow.GetUrl : string;
begin
  if Initialized then
    Result := UTF8ToString(PAnsiChar(webui_get_url(FID)))
   else
    Result := '';
end;

function TWebUIWindow.GetParentProcessID : NativeUInt;
begin
  if Initialized then
    Result := webui_get_parent_process_id(FID)
   else
    Result := 0;
end;

function TWebUIWindow.GetChildProcessID : NativeUInt;
begin
  if Initialized then
    Result := webui_get_child_process_id(FID)
   else
    Result := 0;
end;

class function TWebUIWindow.GetNewWindowID : TWebUIWindowID;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_get_new_window_id()
   else
    Result := 0;
end;

function TWebUIWindow.Bind(const element_: string; func_: TWebUIBindCallback): TWebUIBindID;
var
  LElement: AnsiString;
begin
  Result := 0;

  if Initialized then
    begin
      LElement := UTF8Encode(element_ + #0);
      Result   := webui_bind(FID, @LElement[1], func_);
    end;
end;

function TWebUIWindow.Bind(const element_: string; func_: TWebUIInterfaceEventCallback): TWebUIBindID;
var
  LElement: AnsiString;
begin
  Result := 0;

  if Initialized then
    begin
      LElement := UTF8Encode(element_ + #0);
      //Result   := webui_bind(FID, @LElement[1], func_);
      Result   := webui_interface_bind(FID, @LElement[1], func_);
    end;
end;

function TWebUIWindow.Show(const content : string) : boolean;
var
  LContent: AnsiString;
begin
  Result := False;

  if Initialized then
    begin
      if (length(content) > 0) then
        begin
          LContent := UTF8Encode(content + #0);
          Result   := webui_show(FID, @LContent[1]);
        end
       else
        Result := webui_show(FID, nil);
    end;
end;

function TWebUIWindow.ShowBrowser(const content : string; browser : TWebUIBrowsers) : boolean;
var
  LContent: AnsiString;
begin
  Result := False;

  if Initialized then
    begin
      if (length(content) > 0) then
        begin
          LContent := UTF8Encode(content + #0);
          Result   := webui_show_browser(FID, @LContent[1], browser);
        end
       else
        Result := webui_show_browser(FID, nil, browser);
    end;
end;

procedure TWebUIWindow.SetKiosk(status: boolean);
begin
  if Initialized then
    webui_set_kiosk(FID, status);
end;

procedure TWebUIWindow.Close;
begin
  if Initialized then
    webui_close(FID);
end;

function TWebUIWindow.SetRootFolder(const path : string) : boolean;
var
  LPath: AnsiString;
begin
  Result := False;

  if Initialized then
    begin
      LPath  := UTF8Encode(path + #0);
      Result := webui_set_root_folder(FID, @LPath[1]);
    end;
end;

procedure TWebUIWindow.SetIcon(const icon, icon_type : string);
var
  LIcon, LIconType: AnsiString;
begin
  if Initialized then
    begin
      LIcon     := UTF8Encode(icon + #0);
      LIconType := UTF8Encode(icon_type + #0);
      webui_set_icon(FID, @LIcon[1], @LIconType[1]);
    end;
end;

procedure TWebUIWindow.SendRaw(const function_: string; const raw: Pointer; size: NativeUInt);
var
  LFunction: AnsiString;
begin
  if Initialized then
    begin
      LFunction := UTF8Encode(function_ + #0);
      webui_send_raw(FID, @LFunction[1], raw, size);
    end;
end;

procedure TWebUIWindow.SetHide(status: boolean);
begin
  if Initialized then
    webui_set_hide(FID, status);
end;

procedure TWebUIWindow.SetSize(width, height: cardinal);
begin
  if Initialized then
    webui_set_size(FID, width, height);
end;

procedure TWebUIWindow.SetPosition(x, y: cardinal);
begin
  if Initialized then
    webui_set_position(FID, x, y);
end;

procedure TWebUIWindow.SetProfile(const name, path: string);
var
  LName, LPath: AnsiString;
begin
  if Initialized then
    begin
      LName := UTF8Encode(name + #0);
      LPath := UTF8Encode(path + #0);
      webui_set_profile(FID, @LName[1], @LPath[1]);
    end;
end;

procedure TWebUIWindow.SetPublic(status: boolean);
begin
  if Initialized then
    webui_set_public(FID, status);
end;

procedure TWebUIWindow.Navigate(const Url: string);
var
  LUrl: AnsiString;
begin
  if Initialized then
    begin
      LUrl := UTF8Encode(Url + #0);
      webui_navigate(FID, @LUrl[1]);
    end;
end;

procedure TWebUIWindow.DeleteProfile;
begin
  if Initialized then
    webui_delete_profile(FID);
end;

function TWebUIWindow.SetPort(port : NativeUInt): boolean;
begin
  Result := Initialized and
            webui_set_port(FID, port);
end;

procedure TWebUIWindow.Run(const script_: string);
var
  LScript : AnsiString;
begin
  if Initialized then
    begin
      LScript := UTF8Encode(script_ + #0);
      webui_run(FID, @LScript[1]);
    end;
end;

function TWebUIWindow.Script(const script_: string; timeout: NativeUInt; var buffer: string; buffer_length: NativeUInt): boolean;
var
  LScript : AnsiString;
  LBuffer : PWebUIChar;
begin
  Result  := False;
  LBuffer := nil;

  if Initialized then
    try
      LBuffer := webui_malloc(buffer_length);
      LScript := UTF8Encode(script_ + #0);

      if webui_script(FID, @LScript[1], timeout, LBuffer, buffer_length) then
        begin
          buffer := UTF8ToString(PAnsiChar(LBuffer));
          Result := True;
        end;
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

procedure TWebUIWindow.SetRuntime(runtime: TWebUIRuntime);
begin
  if Initialized then
    webui_set_runtime(FID, runtime);
end;


end.
