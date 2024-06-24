unit uWebUITypes;

{$I uWebUI.inc}    

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    System.Classes;
  {$ELSE}
    Classes;
  {$ENDIF}

type
  IWebUIEventHandler = interface;
  IWebUIWindow = interface;

  /// <summary>
  /// TWebUILoader status values
  /// </summary>
  TLoaderStatus = (lsCreated,
                   lsLoading,
                   lsLoaded,
                   lsInitialized,
                   lsError,
                   lsUnloaded);

  /// <summary>
  /// Supported web browsers.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_browser)</see></para>
  /// </remarks>
  TWebUIBrowser = (
    /// <summary>
    /// No web browser.
    /// </summary>
    NoBrowser = 0,
    /// <summary>
    /// Default web browser.
    /// </summary>
    AnyBrowser,
    /// <summary>
    /// Google Chrome.
    /// </summary>
    Chrome,
    /// <summary>
    /// Mozilla Firefox.
    /// </summary>
    Firefox,
    /// <summary>
    /// Microsoft Edge.
    /// </summary>
    Edge,
    /// <summary>
    /// Apple Safari.
    /// </summary>
    Safari,
    /// <summary>
    /// Chromium.
    /// </summary>
    Chromium,
    /// <summary>
    /// Opera.
    /// </summary>
    Opera,
    /// <summary>
    /// Brave.
    /// </summary>
    Brave,
    /// <summary>
    /// Vivaldi.
    /// </summary>
    Vivaldi,
    /// <summary>
    /// Epic.
    /// </summary>
    Epic,
    /// <summary>
    /// Yandex.
    /// </summary>
    Yandex,
    /// <summary>
    /// Any Chromium based browser.
    /// </summary>
    ChromiumBased
  );

  /// <summary>
  /// Supported runtimes.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_runtime)</see></para>
  /// </remarks>
  TWebUIRuntime = (
    /// <summary>
    /// Prevent WebUI from using any runtime for .js and .ts files.
    /// </summary>
    None = 0,
    /// <summary>
    /// Use Deno runtime for .js and .ts files.
    /// </summary>
    Deno,
    /// <summary>
    /// Use Nodejs runtime for .js files.
    /// </summary>
    NodeJS
  );

  /// <summary>
  /// Event types.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_event)</see></para>
  /// </remarks>
  TWebUIEventType = (
    /// <summary>
    /// Window disconnection event.
    /// </summary>
    WEBUI_EVENT_DISCONNECTED = 0,
    /// <summary>
    /// Window connection event.
    /// </summary>
    WEBUI_EVENT_CONNECTED,
    /// <summary>
    /// Mouse click event.
    /// </summary>
    WEBUI_EVENT_MOUSE_CLICK,
    /// <summary>
    /// Window navigation event.
    /// </summary>
    WEBUI_EVENT_NAVIGATION,
    /// <summary>
    /// Function call event.
    /// </summary>
    WEBUI_EVENT_CALLBACK
  );

  /// <summary>
  /// Window number or Window ID.
  /// </summary>
  TWebUIWindowID = type NativeUInt;

  /// <summary>
  /// Bind ID.
  /// </summary>
  TWebUIBindID = type NativeUInt;

  /// <summary>
  /// The event number or event ID.
  /// </summary>
  TWebUIEventID = type NativeUInt;

  /// <summary>
  /// WebUI char type.
  /// </summary>
  TWebUIChar = type AnsiChar;
  PWebUIChar = ^TWebUIChar;

  /// <summary>
  /// WebUI configuration.
  /// </summary>
  TWebUIConfig = (
    /// <summary>
    /// Control if `webui_show()`, `webui_show_browser()` and
    /// `webui_show_wv()` should wait for the window to connect
    /// before returns or not. Default: True.
    /// </summary>
    show_wait_connection = 0,
    /// <summary>
    /// Control if WebUI should block and process the UI events
    /// one a time in a single thread `True`, or process every
    /// event in a new non-blocking thread `False`. This updates
    /// all windows. You can use `webui_set_event_blocking()` for
    /// a specific single window update. Default: False.
    /// </summary>
    ui_event_blocking
  );

  /// <summary>
  /// WebUI event.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_event_t)</see></para>
  /// </remarks>
  TWebUIEvent = record
    /// <summary>
    /// The window object number.
    /// </summary>
    window       : TWebUIWindowID;
    /// <summary>
    /// Event type.
    /// </summary>
    event_type   : TWebUIEventType;
    /// <summary>
    /// HTML element ID.
    /// </summary>
    element      : PWebUIChar;
    /// <summary>
    /// Internal WebUI. Event number or Event ID.
    /// </summary>
    event_number : TWebUIEventID;
    /// <summary>
    /// Bind ID.
    /// </summary>
    bind_id      : TWebUIBindID;
  end;
  PWebUIEvent = ^TWebUIEvent;

  TWebUIBindCallback           = procedure(e: PWebUIEvent); cdecl;
  TWebUIFileHandlerCallback    = function(const filename: PWebUIChar; len: PInteger): Pointer; cdecl;
  TWebUIInterfaceEventCallback = procedure(window : TWebUIWindowID; event_type: TWebUIEventType; const element: PWebUIChar; event_number: TWebUIEventID; bind_id: TWebUIBindID); cdecl;

  TOnWebUIEvent = procedure(Sender: TObject; const aEvent: IWebUIEventHandler) of object;



  IWebUIEventHandler = interface
    ['{CD157359-C95D-4DBA-835F-B43E99DCE1D0}']
      function GetInitialized: boolean;
      function GetEvent: PWebUIEvent;
      function GetWindowID: TWebUIWindowID;
      function GetEventType: TWebUIEventType;
      function GetElement: string;
      function GetEventID: TWebUIEventID;
      function GetBindID: TWebUIBindID;
      function GetWindow: IWebUIWindow;
      function GetCount: NativeUInt;

      /// <summary>
      /// Get an argument as integer at a specific index.
      /// </summary>
      /// <param name="index">The argument position starting from 0.</param>
      /// <returns>Returns argument as integer.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_int_at)</see></para>
      /// </remarks>
      function GetIntAt(index: NativeUInt): int64;
      /// <summary>
      /// Get the first argument as integer.
      /// </summary>
      /// <returns>Returns argument as integer.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_int)</see></para>
      /// </remarks>
      function GetInt: int64;
      /// <summary>
      /// Get an argument as float at a specific index.
      /// </summary>
      /// <param name="index">The argument position starting from 0.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_float_at)</see></para>
      /// </remarks>
      function GetFloatAt(index: NativeUInt): double;
      /// <summary>
      /// Get the first argument as float.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_float)</see></para>
      /// </remarks>
      function GetFloat: double;
      /// <summary>
      /// Get an argument as string at a specific index.
      /// </summary>
      /// <param name="index">The argument position starting from 0.</param>
      /// <returns>Returns argument as string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string_at)</see></para>
      /// </remarks>
      function GetStringAt(index: NativeUInt): string;
      /// <summary>
      /// Get the first argument as string.
      /// </summary>
      /// <returns>Returns argument as string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string)</see></para>
      /// </remarks>
      function GetString: string;
      /// <summary>
      /// Get an argument as a stream at a specific index.
      /// </summary>
      /// <param name="aResultStream">The stream with the returned data.</param>
      /// <param name="index">The argument position starting from 0.</param>
      /// <returns>Returns true if the stream has data.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string_at)</see></para>
      /// </remarks>
      function GetStreamAt(var aResultStream: TMemoryStream; index: NativeUInt): boolean;
      /// <summary>
      /// Get the first argument as a stream.
      /// </summary>
      /// <param name="aResultStream">The stream with the returned data.</param>
      /// <returns>Returns true if the stream has data.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string)</see></para>
      /// </remarks>
      function GetStream(var aResultStream: TMemoryStream): boolean;
      /// <summary>
      /// Get an argument as boolean at a specific index.
      /// </summary>
      /// <param name="index">The argument position starting from 0.</param>
      /// <returns>Returns argument as boolean.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_bool_at)</see></para>
      /// </remarks>
      function GetBoolAt(index: NativeUInt): boolean;
      /// <summary>
      /// Get the first argument as boolean.
      /// </summary>
      /// <returns>Returns argument as boolean.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_bool)</see></para>
      /// </remarks>
      function GetBool: boolean;
      /// <summary>
      /// Get the size in bytes of an argument at a specific index.
      /// </summary>
      /// <param name="index">The argument position starting from 0.</param>
      /// <returns>Returns size in bytes.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_size_at)</see></para>
      /// </remarks>
      function GetSizeAt(index: NativeUInt): NativeUInt;
      /// <summary>
      /// Get size in bytes of the first argument.
      /// </summary>
      /// <returns>Returns size in bytes.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_size)</see></para>
      /// </remarks>
      function GetSize: NativeUInt;
      /// <summary>
      /// Return the response to JavaScript as integer.
      /// </summary>
      /// <param name="aReturnValue">The integer to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_int)</see></para>
      /// </remarks>
      procedure ReturnInt(aReturnValue: int64);
      /// <summary>
      /// Return the response to JavaScript as float.
      /// </summary>
      /// <param name="aReturnValue">The float to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_float)</see></para>
      /// </remarks>
      procedure ReturnFloat(aReturnValue: double);
      /// <summary>
      /// Return the response to JavaScript as string.
      /// </summary>
      /// <param name="aReturnValue">The string to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_string)</see></para>
      /// </remarks>
      procedure ReturnString(const aReturnValue: string);
      /// <summary>
      /// Return the response to JavaScript as a stream.
      /// </summary>
      /// <param name="aReturnValue">The stream to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_string)</see></para>
      /// </remarks>
      procedure ReturnStream(const aReturnValue: TMemoryStream); overload;
      /// <summary>
      /// Return the response to JavaScript as a stream.
      /// </summary>
      /// <param name="aReturnValue">The stream to be send to JavaScript.</param>
      /// <param name="aOffset">Moves the current stream position by aOffset bytes, relative to the beginning.</param>
      /// <param name="aCount">Copies aCount bytes from the stream.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_string)</see></para>
      /// </remarks>
      procedure ReturnStream(const aReturnValue: TMemoryStream; aOffset, aCount: int64); overload;
      /// <summary>
      /// Return the response to JavaScript as boolean.
      /// </summary>
      /// <param name="aReturnValue">The boolean to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_bool)</see></para>
      /// </remarks>
      procedure ReturnBool(aReturnValue: boolean);
      /// <summary>
      /// When using `webui_interface_bind()`, you may need this function to easily set a response.
      /// </summary>
      /// <param name="response">The response as string to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_set_response)</see></para>
      /// </remarks>
      procedure SetResponse(const response: string);

      /// <summary>
      /// Returns true if the Window was created successfully.
      /// </summary>
      property Initialized       : boolean          read GetInitialized;
      /// <summary>
      /// Pointer to WebUI event record.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_event_t)</see></para>
      /// </remarks>
      property Event             : PWebUIEvent      read GetEvent;
      /// <summary>
      /// Window wrapper for the Window object of this event.
      /// </summary>
      property Window            : IWebUIWindow     read GetWindow;
      /// <summary>
      /// The window object number or ID.
      /// </summary>
      property WindowID          : TWebUIWindowID   read GetWindowID;
      /// <summary>
      /// Event type.
      /// </summary>
      property EventType         : TWebUIEventType  read GetEventType;
      /// <summary>
      /// HTML element ID.
      /// </summary>
      property Element           : string           read GetElement;
      /// <summary>
      /// Event number or Event ID.
      /// </summary>
      property EventID           : TWebUIEventID    read GetEventID;
      /// <summary>
      /// Bind ID.
      /// </summary>
      property BindID            : TWebUIBindID     read GetBindID;
      /// <summary>
      /// Get how many arguments there are in an event.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_count)</see></para>
      /// </remarks>
      property Count             : NativeUInt       read GetCount;
  end;

  IWebUIWindow = interface
    ['{0F575CA7-D7B7-4A77-BDD2-2636F02BA804}']
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

    procedure doOnWebUIEvent(const aEvent: IWebUIEventHandler);

    /// <summary>
    /// Close the window and free all memory resources.
    /// </summary>
    /// <remarks>
    /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_destroy)</see></para>
    /// </remarks>
    procedure   DestroyWindow;
    /// <summary>
    /// <para>Bind a specific html element click event with the OnWebUIEvent event. Empty element means all events.</para>
    /// </summary>
    /// <param name="element_">The HTML element ID.</param>
    /// <returns>Returns a unique bind ID.</returns>
    /// <remarks>
    /// <para>The OnWebUIEvent event will be executed in the main application thread by default. Set WebUI.SyncedEvents to false in order to execute it in a background thread.</para>
    /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
    /// </remarks>
    function    Bind(const element_: string): TWebUIBindID; overload;
    /// <summary>
    /// Bind a specific html element click event with a function. Empty element means all events.
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
    /// Bind a specific HTML element click event with a function. Empty element means all events.
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
    /// Bind all events with a function.
    /// </summary>
    /// <param name="func_">The callback function.</param>
    /// <returns>Returns a unique bind ID.</returns>
    /// <remarks>
    /// <para>The callback function will always be executed in a background thread!</para>
    /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
    /// </remarks>
    function    BindAllEvents(func_: TWebUIBindCallback): TWebUIBindID; overload;
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
    function    ShowBrowser(const content : string; browser : TWebUIBrowser) : boolean;
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
    /// Set a custom handler to serve files.
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
    /// Safely send raw data to the UI.
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
    /// <param name="runtime">Deno, Nodejs or None.</param>
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
    /// Event triggered on a browser event. It's necessay to bind the event using the TWebUIWindow.Bind* functions without a "func_" parameter.
    /// </summary>
    property OnWebUIEvent     : TOnWebUIEvent     read GetOnWebUIEvent     write SetOnWebUIEvent;
  end;

implementation

end.
