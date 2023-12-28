unit uWebUITypes;

{$MINENUMSIZE 4}

interface

type
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
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_browsers)</see></para>
  /// </remarks>
  TWebUIBrowsers = (
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
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_runtimes)</see></para>
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
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_events)</see></para>
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

  TWebUIBindCallback           = procedure(e: PWebUIEvent);
  TWebUIFileHandlerCallback    = function(const filename: PWebUIChar; len: PInteger): PWebUIChar;
  TWebUIInterfaceEventCallback = procedure(window : TWebUIWindowID; event_type: TWebUIEventType; const element: PWebUIChar; event_number: TWebUIEventID; bind_id: TWebUIBindID);

implementation

end.
