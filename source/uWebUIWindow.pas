unit uWebUIWindow;

{$I uWebUI.inc}    

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils, System.SyncObjs,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, SyncObjs,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  /// <summary>
  /// Window wrapper for Window objects in WebUI.
  /// </summary>
  TWebUIWindow = class(TInterfacedObject, IWebUIWindow)
    protected
      FID            : TWebUIWindowID;
      FOnWebUIEvent  : TOnWebUIEvent;
      FBindIDList    : TList;
      FCritSect      : TCriticalSection;
      FAllowWebView  : boolean;

      function  GetID : TWebUIWindowID;
      function  GetInitialized : boolean;
      function  GetIsShown : boolean;
      function  GetUrl : string;
      function  GetParentProcessID : NativeUInt;
      function  GetChildProcessID : NativeUInt;
      function  GetOnWebUIEvent : TOnWebUIEvent;
      function  GetBestBrowser : TWebUIBrowser;
      function  GetAllowWebView : boolean;

      procedure SetOnWebUIEvent(const aEvent : TOnWebUIEvent);
      procedure SetAllowWebView(aAllow : boolean);

      function  Lock : boolean;
      procedure UnLock;
      procedure AddBindID(aID : TWebUIBindID);
      procedure AddWindowID;
      procedure RemoveWindowID;
      procedure doOnWebUIEvent(const aEvent: IWebUIEventHandler);

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
      constructor Create(windowId : TWebUIWindowID); overload;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      /// <summary>
      /// Destroy the window wrapper.
      /// </summary>
      destructor  Destroy; override;
      /// <summary>
      /// Close the window and free all memory resources.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_destroy)</see></para>
      /// </remarks>
      procedure   DestroyWindow;
      /// <summary>
      /// <para>Bind an HTML element and a JavaScript object with a backend function. Empty element name means all events.</para>
      /// </summary>
      /// <param name="element_">The HTML element / JavaScript object.</param>
      /// <returns>Returns a unique bind ID.</returns>
      /// <remarks>
      /// <para>The OnWebUIEvent event will be executed in the main application thread by default. Set WebUI.SyncedEvents to false in order to execute it in a background thread.</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
      /// </remarks>
      function    Bind(const element_: string): TWebUIBindID; overload;
      /// <summary>
      /// Bind a specific html element click event with a callback function. Empty element means all events.
      /// </summary>
      /// <param name="element_">The HTML element ID.</param>
      /// <param name="func_">The callback function.</param>
      /// <returns>Returns a unique bind ID.</returns>
      /// <remarks>
      /// <para>The callback function will always be executed in a background thread!</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
      /// </remarks>
      function    Bind(const element_: string; func_: TWebUIBindCallback): TWebUIBindID; overload;
      /// <summary>
      /// Bind a specific HTML element click event with a callback function. Empty element means all events.
      /// </summary>
      /// <param name="element_">The HTML element ID.</param>
      /// <param name="func_">The callback as myFunc(Window, EventType, Element, EventNumber, BindID).</param>
      /// <returns>Returns unique bind ID.</returns>
      /// <remarks>
      /// <para>The callback function will always be executed in a background thread!</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_bind)</see></para>
      /// </remarks>
      function    Bind(const element_: string; func_: TWebUIInterfaceEventCallback): TWebUIBindID; overload;
      /// <summary>
      /// <para>Bind all browser events with the OnWebUIEvent event.</para>
      /// </summary>
      /// <returns>Returns a unique bind ID.</returns>
      /// <remarks>
      /// <para>The OnWebUIEvent event will be executed in the main application thread by default. Set WebUI.SyncedEvents to false in order to execute it in a background thread.</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
      /// </remarks>
      function    BindAllEvents: TWebUIBindID; overload;
      /// <summary>
      /// <para>Bind all browser events with a callback function.</para>
      /// </summary>
      /// <param name="func_">The callback function.</param>
      /// <returns>Returns a unique bind ID.</returns>
      /// <remarks>
      /// <para>The callback function will always be executed in a background thread!</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
      /// </remarks>
      function    BindAllEvents(func_: TWebUIBindCallback): TWebUIBindID; overload;
      /// <summary>
      /// Show a window using embedded HTML, or a file. If the window is already open, it will be refreshed. This will refresh all windows in multi-client mode.
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
      function    ShowBrowser(const content : string; browser : TWebUIBrowser) : boolean;
      /// <summary>
      /// Show a WebView window using embedded HTML, or a file. If the window is already
      /// open, it will be refreshed. Note: Win32 need `WebView2Loader.dll`.
      /// </summary>
      /// <param name="content">The HTML, URL, Or a local file.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_wv)</see></para>
      /// </remarks>
      function    ShowWV(const content : string) : boolean;
      /// <summary>
      /// Set the window in Kiosk mode (Full screen).
      /// </summary>
      /// <param name="status">True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_kiosk)</see></para>
      /// </remarks>
      procedure   SetKiosk(status: boolean);
      /// <summary>
      /// Close a specific window only. The window object will still exist. All clients.
      /// </summary>
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
      /// Set a custom handler to serve files. This custom handler should return full HTTP header and body.
      /// </summary>
      /// <param name="handler">The handler function: `void myHandler(const char* filename, * int* length)`.</param>
      /// <remarks>
      /// <para>The callback function will always be executed in a background thread!</para>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_file_handler)</see></para>
      /// </remarks>>
      procedure   SetFileHandler(handler: TWebUIFileHandlerCallback);
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
      /// Safely send raw data to the UI. All clients.
      /// </summary>
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
      /// Set the web browser proxy_server to use. Need to be called before 'webui_show()'.
      /// </summary>
      /// <param name="proxy_server">The web browser proxy_server. For example 'http://127.0.0.1:8888'</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_proxy)</see></para>
      /// </remarks>
      procedure   SetProxy(const proxy_server: string);
      /// <summary>
      /// Allow a specific window address to be accessible from a public network.
      /// </summary>
      /// <param name="status">True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_public)</see></para>
      /// </remarks>
      procedure   SetPublic(status: boolean);
      /// <summary>
      /// Navigate to a specific URL. All clients.
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
      /// Get the network port of a running window. This can be useful to determine the HTTP link of `webui.js`
      /// </summary>
      /// <returns>Returns the network port of the window.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_port)</see></para>
      /// </remarks>
      function    GetPort : NativeUInt;
      /// <summary>
      /// Set a custom web-server/websocket network port to be used by WebUI.
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
      /// Set a custom web-server/websocket network port to be used by WebUI.
      /// This can be useful to determine the HTTP link of `webui.js` in case
      /// you are trying to use WebUI with an external web-server like NGNIX
      /// </summary>
      /// <param name="port">The web-server network port WebUI should use.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_port)</see></para>
      /// </remarks>
      procedure   SetPort2(port : NativeUInt);
      /// <summary>
      /// Run JavaScript without waiting for the response. All clients.
      /// </summary>
      /// <param name="script_">The JavaScript to be run.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_run)</see></para>
      /// </remarks>
      procedure   Run(const script_: string);
      /// <summary>
      /// Run JavaScript and get the response back. Work only in single client mode.
      /// Make sure your local buffer can hold the response.
      /// </summary>
      /// <param name="script_">The JavaScript to be run.</param>
      /// <param name="timeout">The execution timeout in seconds.</param>
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
      /// <param name="runtime">Deno, Bun, Nodejs or None.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_runtime)</see></para>
      /// </remarks>
      procedure   SetRuntime(runtime: TWebUIRuntime);
      /// <summary>
      /// Returns true if the bind id belongs to this window.
      /// </summary>
      /// <param name="aID">Bind ID that supposedly belongs to this window.</param>
      function    HasBindID(aID : TWebUIBindID): boolean;
      /// <summary>
      /// Control if UI events comming from this window should be processed
      /// one a time in a single blocking thread `True`, or process every event in
      /// a new non-blocking thread `False`. This update single window. You can use
      /// `webui_set_config(ui_event_blocking, ...)` to update all windows.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_event_blocking)</see></para>
      /// </remarks>
      procedure   SetEventBlocking(status: boolean);
      /// <summary>
      /// Start only the web server and return the URL. This is useful for web app.
      /// </summary>
      /// <param name="path">The local root folder full path.</param>
      /// <returns>Returns the url of this window server.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_start_server)</see></para>
      /// </remarks>
      function    StartServer(const path: string): string;
      /// <summary>
      /// Set the window with high-contrast support. Useful when you want to build a better high-contrast theme with CSS.
      /// </summary>
      /// <param name="status">True or False.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_high_contrast)</see></para>
      /// </remarks>
      procedure   SetHighContrast(status: boolean);
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
      property ID                : TWebUIWindowID   read GetID;
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
      /// <summary>
      /// Get the recommended web browser ID to use. If you are already using one,
      /// this function will return the same ID.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_best_browser)</see></para>
      /// </remarks>
      property BestBrowser      : TWebUIBrowser     read GetBestBrowser;
      /// <summary>
      /// Allow using WebView to show a browser with TWebUIWindow.Show.
      /// </summary>
      property AllowWebView     : boolean           read GetAllowWebView     write SetAllowWebView;
      /// <summary>
      /// Get the network port of a running window. This can be useful to determine the HTTP link of `webui.js`
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_port)</see></para>
      /// </remarks>
      property Port             : NativeUInt        read GetPort             write SetPort2;
      /// <summary>
      /// Event triggered on a browser event. It's necessay to bind the event using the TWebUIWindow.Bind* functions without a "func_" parameter.
      /// </summary>
      property OnWebUIEvent     : TOnWebUIEvent     read GetOnWebUIEvent     write SetOnWebUIEvent;
  end;

implementation

uses
  uWebUI;

constructor TWebUIWindow.Create;
begin
  inherited Create;

  FOnWebUIEvent := nil;
  FCritSect     := nil;
  FBindIDList   := nil;
  FAllowWebView := {$IFDEF MSWINDOWS}True{$ELSE}False{$ENDIF};

  if (WebUI <> nil) and WebUI.Initialized then
    FID := webui_new_window()
   else
    FID := 0;
end;

constructor TWebUIWindow.Create(windowId : TWebUIWindowID);
begin
  inherited Create;

  FOnWebUIEvent := nil;
  FCritSect     := nil;
  FBindIDList   := nil;

  if (WebUI <> nil) and WebUI.Initialized then
    begin
      if (windowId > 0) and (windowId < WEBUI_MAX_IDS) then
        FID := webui_new_window_id(windowId)
       else
        FID := webui_new_window();
    end
   else
    FID := 0;
end;

destructor TWebUIWindow.Destroy;
begin
  try
    Close;
    DestroyWindow;

    if assigned(FBindIDList) then
      FreeAndNil(FBindIDList);

    if assigned(FCritSect) then
      FreeAndNil(FCritSect);
  finally
    inherited Destroy;
  end;
end;

procedure TWebUIWindow.AfterConstruction;
begin
  inherited AfterConstruction;

  FCritSect   := TCriticalSection.Create;
  FBindIDList := TList.Create;

  AddWindowID;
end;

procedure TWebUIWindow.BeforeDestruction;
begin
  RemoveWindowID;

  inherited BeforeDestruction;
end;

procedure TWebUIWindow.DestroyWindow;
begin
  if Initialized then
    begin
      webui_destroy(FID);
      RemoveWindowID;
      FID := 0;
    end;
end;

procedure TWebUIWindow.AddWindowID;
begin
  if Initialized then
    WebUI.AddWindow(Self);
end;

function TWebUIWindow.Lock : boolean;
begin
  Result := False;

  if assigned(FCritSect) then
    begin
      FCritSect.Acquire;
      Result := True;
    end;
end;

procedure TWebUIWindow.UnLock;
begin
  if assigned(FCritSect) then
    FCritSect.Release;
end;

procedure TWebUIWindow.AddBindID(aID : TWebUIBindID);
begin
  if Lock then
    try
      if assigned(FBindIDList) then
        FBindIDList.Add(Pointer(aID));
    finally
      UnLock;
    end;
end;

function TWebUIWindow.HasBindID(aID : TWebUIBindID): boolean;
begin
  Result := False;

  if Lock then
    try
      Result := assigned(FBindIDList) and
                (FBindIDList.IndexOf(Pointer(aID)) >= 0);
    finally
      UnLock;
    end;
end;

procedure TWebUIWindow.RemoveWindowID;
begin
  if Initialized then
    WebUI.RemoveWindow(FID);
end;

function TWebUIWindow.GetID : TWebUIWindowID;
begin
  Result := FID;
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
    Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(webui_get_url(FID)))
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

function TWebUIWindow.GetOnWebUIEvent : TOnWebUIEvent;
begin
  Result := FOnWebUIEvent;
end;

function TWebUIWindow.GetBestBrowser : TWebUIBrowser;
begin
  if Initialized then
    Result := webui_get_best_browser(FID)
   else
    Result := NoBrowser;
end;

function TWebUIWindow.GetAllowWebView : boolean;
begin
  Result := FAllowWebView;
end;

procedure TWebUIWindow.SetOnWebUIEvent(const aEvent : TOnWebUIEvent);
begin
  FOnWebUIEvent := aEvent;
end;

procedure TWebUIWindow.SetAllowWebView(aAllow : boolean);
begin
  FAllowWebView := aAllow;
end;

procedure TWebUIWindow.SetEventBlocking(status: boolean);
begin
  if Initialized then
    webui_set_event_blocking(FID, status);
end;

function TWebUIWindow.StartServer(const path: string): string;
var
  LPath: AnsiString;
begin
  Result := '';

  if Initialized and (length(path) > 0) then
    begin
      LPath  := UTF8Encode(path + #0);
      Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(webui_start_server(FID, @LPath[1])))
    end;
end;

procedure TWebUIWindow.SetHighContrast(status: boolean);
begin
  if Initialized then
    webui_set_high_contrast(FID, status);
end;

procedure TWebUIWindow.doOnWebUIEvent(const aEvent: IWebUIEventHandler);
begin
  if assigned(FOnWebUIEvent) then
    begin
      {$IFDEF DELPHI14_UP}
      if WebUI.SyncedEvents then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              FOnWebUIEvent(self, aEvent);
            end);
        end
       else
      {$ENDIF}
        FOnWebUIEvent(self, aEvent);
    end;
end;

class function TWebUIWindow.GetNewWindowID : TWebUIWindowID;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_get_new_window_id()
   else
    Result := 0;
end;

function TWebUIWindow.Bind(const element_: string): TWebUIBindID;
begin
  Result := Bind(element_, global_webui_event_callback);
end;

function TWebUIWindow.Bind(const element_: string; func_: TWebUIBindCallback): TWebUIBindID;
var
  LElement    : AnsiString;
  LElementPtr : PWebUIChar;
begin
  Result := 0;

  if Initialized then
    begin
      if (length(element_) > 0) then
        begin
          LElement    := UTF8Encode(element_ + #0);
          LElementPtr := @LElement[1];
        end
       else
        LElementPtr := nil;

      Result := webui_bind(FID, LElementPtr, func_);
      AddBindID(Result);
    end;
end;

function TWebUIWindow.Bind(const element_: string; func_: TWebUIInterfaceEventCallback): TWebUIBindID;
var
  LElement    : AnsiString;
  LElementPtr : PWebUIChar;
begin
  Result := 0;

  if Initialized then
    begin
      if (length(element_) > 0) then
        begin
          LElement    := UTF8Encode(element_ + #0);
          LElementPtr := @LElement[1];
        end
       else
        LElementPtr := nil;

      Result := webui_interface_bind(FID, LElementPtr, func_);
      AddBindID(Result);
    end;
end;

function TWebUIWindow.BindAllEvents: TWebUIBindID;
begin
  Result := BindAllEvents(global_webui_event_callback);
end;

function TWebUIWindow.BindAllEvents(func_: TWebUIBindCallback): TWebUIBindID;
begin
  Result := 0;

  if Initialized then
    begin
      Result := webui_bind(FID, nil, func_);
      AddBindID(Result);
    end;
end;

function TWebUIWindow.Show(const content : string) : boolean;
var
  LContent    : AnsiString;
  LContentPtr : PWebUIChar;
begin
  Result := False;

  if Initialized then
    begin
      if (length(content) > 0) then
        begin
          LContent    := UTF8Encode(content + #0);
          LContentPtr := @LContent[1];
        end
       else
        LContentPtr := nil;

      if FAllowWebView then
        Result := webui_show(FID, LContentPtr)
       else
        Result := webui_show_browser(FID, LContentPtr, AnyBrowser);
    end;
end;

function TWebUIWindow.ShowBrowser(const content : string; browser : TWebUIBrowser) : boolean;
var
  LContent    : AnsiString;
  LContentPtr : PWebUIChar;
begin
  Result := False;

  if Initialized then
    begin
      if (length(content) > 0) then
        begin
          LContent    := UTF8Encode(content + #0);
          LContentPtr := @LContent[1];
        end
       else
        LContentPtr := nil;

      Result := webui_show_browser(FID, LContentPtr, browser);
    end;
end;

function TWebUIWindow.ShowWV(const content : string) : boolean;
var
  LContent: AnsiString;
begin
  Result := False;

  if Initialized and (length(content) > 0) then
    begin
      LContent := UTF8Encode(content + #0);
      Result   := webui_show_wv(FID, @LContent[1]);
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

  if Initialized and (length(path) > 0) then
    begin
      LPath  := UTF8Encode(path + #0);
      Result := webui_set_root_folder(FID, @LPath[1]);
    end;
end;

procedure TWebUIWindow.SetFileHandler(handler: TWebUIFileHandlerCallback);
begin
  if Initialized then
    webui_set_file_handler(FID, handler);
end;

procedure TWebUIWindow.SetIcon(const icon, icon_type : string);
var
  LIcon, LIconType: AnsiString;
  LIconPtr, LIconTypePtr : PWebUIChar;
begin
  if Initialized then
    begin
      if (length(icon) > 0) then
        begin
          LIcon    := UTF8Encode(icon + #0);
          LIconPtr := @LIcon[1];
        end
       else
        LIconPtr := nil;

      if (length(icon_type) > 0) then
        begin
          LIconType    := UTF8Encode(icon_type + #0);
          LIconTypePtr := @LIconType[1];
        end
       else
        LIconTypePtr := nil;

      webui_set_icon(FID, LIconPtr, LIconTypePtr);
    end;
end;

procedure TWebUIWindow.SendRaw(const function_: string; const raw: Pointer; size: NativeUInt);
var
  LFunction: AnsiString;
begin
  if Initialized and (length(function_) > 0) then
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
  LNamePtr, LPathPtr : PWebUIChar;
begin
  if Initialized then
    begin
      if (length(name) > 0) then
        begin
          LName    := UTF8Encode(name + #0);
          LNamePtr := @LName[1];
        end
       else
        LNamePtr := nil;

      if (length(path) > 0) then
        begin
          LPath    := UTF8Encode(path + #0);
          LPathPtr := @LPath[1];
        end
       else
        LPathPtr := nil;

      webui_set_profile(FID, LNamePtr, LPathPtr);
    end;
end;

procedure TWebUIWindow.SetProxy(const proxy_server: string);
var
  LProxyServer    : AnsiString;
  LProxyServerPtr : PWebUIChar;
begin
  if Initialized then
    begin
      if (length(proxy_server) > 0) then
        begin
          LProxyServer    := UTF8Encode(proxy_server + #0);
          LProxyServerPtr := @LProxyServer[1];
        end
       else
        LProxyServerPtr := nil;

      webui_set_proxy(FID, LProxyServerPtr);
    end;
end;

procedure TWebUIWindow.SetPublic(status: boolean);
begin
  if Initialized then
    webui_set_public(FID, status);
end;

procedure TWebUIWindow.Navigate(const Url: string);
var
  LUrl    : AnsiString;
  LUrlPtr : PWebUIChar;
begin
  if Initialized then
    begin
      if (length(Url) > 0) then
        begin
          LUrl    := UTF8Encode(Url + #0);
          LUrlPtr := @LUrl[1];
        end
       else
        LUrlPtr := nil;

      webui_navigate(FID, LUrlPtr);
    end;
end;

procedure TWebUIWindow.DeleteProfile;
begin
  if Initialized then
    webui_delete_profile(FID);
end;

function TWebUIWindow.GetPort : NativeUInt;
begin
  if Initialized then
    Result := webui_get_port(FID)
   else
    Result := 0;
end;

function TWebUIWindow.SetPort(port : NativeUInt): boolean;
begin
  Result := Initialized and
            webui_set_port(FID, port);
end;

procedure TWebUIWindow.SetPort2(port : NativeUInt);
begin
  SetPort(port);
end;

procedure TWebUIWindow.Run(const script_: string);
var
  LScript : AnsiString;
begin
  if Initialized and (length(script_) > 0) then
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

  if Initialized and (length(script_) > 0) then
    try
      LBuffer := webui_malloc(buffer_length);
      LScript := UTF8Encode(script_ + #0);

      if webui_script(FID, @LScript[1], timeout, LBuffer, buffer_length) then
        begin
          buffer := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(LBuffer));
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
