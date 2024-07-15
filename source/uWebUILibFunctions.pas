unit uWebUILibFunctions;

{$I uWebUI.inc}   

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  uWebUIConstants, uWebUITypes;

var
  /// <summary>
  /// Create a new WebUI window object.
  /// </summary>
  /// <returns>Returns the window number.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_new_window)</see></para>
  /// </remarks>
  webui_new_window : function(): TWebUIWindowID; cdecl;

  /// <summary>
  /// Create a new webui window object using a specified window number.
  /// </summary>
  /// <param name="window_number">The window number (should be > 0, and < WEBUI_MAX_IDS).</param>
  /// <returns>Returns the same window number if success.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_new_window_id)</see></para>
  /// </remarks>
  webui_new_window_id : function(window_number : TWebUIWindowID): TWebUIWindowID; cdecl;

  /// <summary>
  /// Get a free window number that can be used with `webui_new_window_id()`.
  /// </summary>
  /// <returns>Returns the first available free window number. Starting from 1.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_new_window_id)</see></para>
  /// </remarks>
  webui_get_new_window_id : function(): TWebUIWindowID; cdecl;

  /// <summary>
  /// Bind an HTML element and a JavaScript object with a backend function. Empty element name means all events.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="element">The HTML element / JavaScript object.</param>
  /// <param name="func">The callback function.</param>
  /// <returns>Returns a unique bind ID.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_bind)</see></para>
  /// </remarks>
  webui_bind : function(window: TWebUIWindowID; const element: PWebUIChar; func: TWebUIBindCallback): TWebUIBindID; cdecl;

  /// <summary>
  /// Get the recommended web browser ID to use. If you are already using one,
  /// this function will return the same ID.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns a web browser ID.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_best_browser)</see></para>
  /// </remarks>
  webui_get_best_browser : function(window: TWebUIWindowID): TWebUIBrowser; cdecl;

  /// <summary>
  /// Show a window using embedded HTML, or a file. If the window is already open, it will be refreshed. This will refresh all windows in multi-client mode.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="content">The HTML, URL, Or a local file.</param>
  /// <returns>Returns True if showing the window is successed.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show)</see></para>
  /// </remarks>
  webui_show : function(window: TWebUIWindowID; const content: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Show a window using embedded HTML, or a file. If the window is already open, it will be refreshed. Single client.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="content">The HTML, URL, Or a local file.</param>
  /// <returns>Returns True if showing the window is successed.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_client)</see></para>
  /// </remarks>
  webui_show_client : function(e: PWebUIEvent; const content: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Same as `webui_show()`. But using a specific web browser.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="content">The HTML, URL, Or a local file.</param>
  /// <param name="browser">The web browser to be used.</param>
  /// <returns>Returns True if showing the window is successed.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_browser)</see></para>
  /// </remarks>
  webui_show_browser : function(window: TWebUIWindowID; const content: PWebUIChar; browser: TWebUIBrowser): boolean; cdecl;

  /// <summary>
  /// Start only the web server and return the URL. This is useful for web app.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="path">The local root folder full path.</param>
  /// <returns>Returns the url of this window server.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_start_server)</see></para>
  /// </remarks>
  webui_start_server : function(window: TWebUIWindowID; const path: PWebUIChar): PWebUIChar; cdecl;

  /// <summary>
  /// Show a WebView window using embedded HTML, or a file. If the window is already
  /// open, it will be refreshed. Note: Win32 need `WebView2Loader.dll`.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="content">The HTML, URL, Or a local file.</param>
  /// <returns>Returns True if showing the window is successed.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_wv)</see></para>
  /// </remarks>
  webui_show_wv : function(window: TWebUIWindowID; const content: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Set the window in Kiosk mode (Full screen).
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="status">True or False.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_kiosk)</see></para>
  /// </remarks>
  webui_set_kiosk : procedure(window: TWebUIWindowID; status: boolean); cdecl;

  /// <summary>
  /// Set the window with high-contrast support. Useful when you want to build a better high-contrast theme with CSS.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="status">True or False.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_high_contrast)</see></para>
  /// </remarks>
  webui_set_high_contrast : procedure(window: TWebUIWindowID; status: boolean); cdecl;

  /// <summary>
  /// Get OS high contrast preference.
  /// </summary>
  /// <returns>Returns True if OS is using high contrast theme.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_is_high_contrast)</see></para>
  /// </remarks>
  webui_is_high_contrast : function(): boolean; cdecl;

  /// <summary>
  /// Check if a web browser is installed.
  /// </summary>
  /// <param name="browser">The web browser to be found.</param>
  /// <returns>Returns True if the specified browser is available.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_is_high_contrast)</see></para>
  /// </remarks>
  webui_browser_exist : function(browser: TWebUIBrowser): boolean; cdecl;

  /// <summary>
  /// Wait until all opened windows get closed.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_wait)</see></para>
  /// </remarks>
  webui_wait : procedure(); cdecl;

  /// <summary>
  /// Close a specific window only. The window object will still exist. All clients.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_close)</see></para>
  /// </remarks>
  webui_close : procedure(window: TWebUIWindowID); cdecl;

  /// <summary>
  /// Close a specific client.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_close_client)</see></para>
  /// </remarks>
  webui_close_client : procedure(e: PWebUIEvent); cdecl;

  /// <summary>
  /// Close a specific window and free all memory resources.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_destroy)</see></para>
  /// </remarks>
  webui_destroy : procedure(window: TWebUIWindowID); cdecl;

  /// <summary>
  /// Close all open windows. `webui_wait()` will return (Break).
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_exit)</see></para>
  /// </remarks>
  webui_exit : procedure(); cdecl;

  /// <summary>
  /// Set the web-server root folder path for a specific window.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="path">The local folder full path.</param>
  /// <returns>Returns True if the function was successful.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_root_folder)</see></para>
  /// </remarks>
  webui_set_root_folder : function(window: TWebUIWindowID; const path: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Set the web-server root folder path for all windows. Should be used before `webui_show()`.
  /// </summary>
  /// <param name="path">The local folder full path.</param>
  /// <returns>Returns True if the function was successful.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_default_root_folder)</see></para>
  /// </remarks>
  webui_set_default_root_folder : function(const path: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Set a custom handler to serve files. This custom handler should return full HTTP header and body.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="handler">The handler function: `void myHandler(const char* filename, * int* length)`.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_file_handler)</see></para>
  /// </remarks>
  webui_set_file_handler : procedure(window: TWebUIWindowID; handler: TWebUIFileHandlerCallback); cdecl;

  /// <summary>
  /// Check if the specified window is still running.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns True if the window is still running.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_is_shown)</see></para>
  /// </remarks>
  webui_is_shown : function(window: TWebUIWindowID): boolean; cdecl;

  /// <summary>
  /// Set the maximum time in seconds to wait for the window to connect. This effect `show()` and `wait()`. Value of `0` means wait forever.
  /// </summary>
  /// <param name="second">The timeout in seconds.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_timeout)</see></para>
  /// </remarks>
  webui_set_timeout : procedure(second: NativeUInt); cdecl;

  /// <summary>
  /// Set the default embedded HTML favicon.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="icon">The icon as string: `<svg>...</svg>`.</param>
  /// <param name="icon_type">The icon type: `image/svg+xml`.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_icon)</see></para>
  /// </remarks>
  webui_set_icon : procedure(window: TWebUIWindowID; const icon, icon_type: PWebUIChar); cdecl;

  /// <summary>
  /// Encode text to Base64. The returned buffer need to be freed.
  /// </summary>
  /// <param name="str">The string to encode (Should be null terminated).</param>
  /// <returns>Returns the base64 encoded string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_encode)</see></para>
  /// </remarks>
  webui_encode : function(const str: PWebUIChar): PWebUIChar; cdecl;

  /// <summary>
  /// Decode a Base64 encoded text. The returned buffer need to be freed.
  /// </summary>
  /// <param name="str">The string to decode (Should be null terminated).</param>
  /// <returns>Returns the base64 decoded string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_decode)</see></para>
  /// </remarks>
  webui_decode : function(const str: PWebUIChar): PWebUIChar; cdecl;

  /// <summary>
  /// Safely free a buffer allocated by WebUI using `webui_malloc()`.
  /// </summary>
  /// <param name="ptr">The buffer to be freed.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_free)</see></para>
  /// </remarks>
  webui_free : procedure(ptr: Pointer); cdecl;

  /// <summary>
  /// Safely allocate memory using the WebUI memory management system. It can be safely freed using `webui_free()` at any time.
  /// </summary>
  /// <param name="size">The size of memory in bytes.</param>
  /// <returns>Returns a pointer to the allocated memory.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_malloc)</see></para>
  /// </remarks>
  webui_malloc : function(size: NativeUInt): Pointer; cdecl;

  /// <summary>
  /// Safely send raw data to the UI. All clients.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="function_">The JavaScript function to receive raw data: `function * myFunc(myData){}`.</param>
  /// <param name="raw">The raw data buffer.</param>
  /// <param name="size">The raw data size in bytes.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_send_raw)</see></para>
  /// </remarks>
  webui_send_raw : procedure(window: TWebUIWindowID; const function_: PWebUIChar; const raw: Pointer; size: NativeUInt); cdecl;

  /// <summary>
  /// Safely send raw data to the UI. Single client.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="function_">The JavaScript function to receive raw data: `function * myFunc(myData){}`.</param>
  /// <param name="raw">The raw data buffer.</param>
  /// <param name="size">The raw data size in bytes.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_send_raw_client)</see></para>
  /// </remarks>
  webui_send_raw_client : procedure(e: PWebUIEvent; const function_: PWebUIChar; const raw: Pointer; size: NativeUInt); cdecl;

  /// <summary>
  /// Set a window in hidden mode. Should be called before `webui_show()`.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="status">The status: True or False.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_hide)</see></para>
  /// </remarks>
  webui_set_hide : procedure(window: TWebUIWindowID; status: boolean); cdecl;

  /// <summary>
  /// Set the window size.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="width">The window width.</param>
  /// <param name="height">The window height.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_size)</see></para>
  /// </remarks>
  webui_set_size : procedure(window: TWebUIWindowID; width, height: cardinal); cdecl;

  /// <summary>
  /// Set the window position.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="x">The window X.</param>
  /// <param name="y">The window Y.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_position)</see></para>
  /// </remarks>
  webui_set_position : procedure(window: TWebUIWindowID; x, y: cardinal); cdecl;

  /// <summary>
  /// Set the web browser profile to use. An empty `name` and `path` means the default user profile. Need to be called before `webui_show()`.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="name">The web browser profile name.</param>
  /// <param name="path">The web browser profile full path.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_profile)</see></para>
  /// </remarks>
  webui_set_profile : procedure(window: TWebUIWindowID; const name, path: PWebUIChar); cdecl;

  /// <summary>
  /// Set the web browser proxy server to use. Need to be called before `webui_show()`.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="proxy_server">The web browser proxy_server.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_proxy)</see></para>
  /// </remarks>
  webui_set_proxy : procedure(window: TWebUIWindowID; const proxy_server: PWebUIChar); cdecl;

  /// <summary>
  /// Get the full current URL.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns the full URL string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_url)</see></para>
  /// </remarks>
  webui_get_url : function(window: TWebUIWindowID): PWebUIChar; cdecl;

  /// <summary>
  /// Open an URL in the native default web browser.
  /// </summary>
  /// <param name="url">The URL to open.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_open_url)</see></para>
  /// </remarks>
  webui_open_url : procedure(const url: PWebUIChar); cdecl;

  /// <summary>
  /// Allow a specific window address to be accessible from a public network.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="status">True or False.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_public)</see></para>
  /// </remarks>
  webui_set_public : procedure(window: TWebUIWindowID; status: boolean); cdecl;

  /// <summary>
  /// Navigate to a specific URL. All clients.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="url">Full HTTP URL.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_navigate)</see></para>
  /// </remarks>
  webui_navigate : procedure(window: TWebUIWindowID; const url: PWebUIChar); cdecl;

  /// <summary>
  /// Navigate to a specific URL. Single client.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="url">Full HTTP URL.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_navigate_client)</see></para>
  /// </remarks>
  webui_navigate_client : procedure(e: PWebUIEvent; const url: PWebUIChar); cdecl;

  /// <summary>
  /// Free all memory resources. Should be called only at the end.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_clean)</see></para>
  /// </remarks>
  webui_clean : procedure(); cdecl;

  /// <summary>
  /// Delete all local web-browser profiles folder. It should called at the end.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_delete_all_profiles)</see></para>
  /// </remarks>
  webui_delete_all_profiles : procedure(); cdecl;

  /// <summary>
  /// Delete a specific window web-browser local folder profile.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <remarks>
  /// <para>This can break functionality of other windows if using the same web-browser.</para>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_delete_profile)</see></para>
  /// </remarks>
  webui_delete_profile : procedure(window: TWebUIWindowID); cdecl;

  /// <summary>
  /// Get the ID of the parent process (The web browser may re-create another new process).
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns the the parent process id as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_parent_process_id)</see></para>
  /// </remarks>
  webui_get_parent_process_id : function(window: TWebUIWindowID): NativeUInt; cdecl;

  /// <summary>
  /// Get the ID of the last child process.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns the the child process id as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_child_process_id)</see></para>
  /// </remarks>
  webui_get_child_process_id : function(window: TWebUIWindowID): NativeUInt; cdecl;

  /// <summary>
  /// Get the network port of a running window. This can be useful to determine the HTTP link of `webui.js`
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns the network port of the window.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_port)</see></para>
  /// </remarks>
  webui_get_port : function(window: TWebUIWindowID): NativeUInt; cdecl;

  /// <summary>
  /// Set a custom web-server/websocket network port to be used by WebUI.
  /// This can be useful to determine the HTTP link of `webui.js` in case
  /// you are trying to use WebUI with an external web-server like NGNIX
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="port">The web-server network port WebUI should use.</param>
  /// <returns>Returns True if the port is free and usable by WebUI.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_port)</see></para>
  /// </remarks>
  webui_set_port : function(window: TWebUIWindowID; port: NativeUInt): boolean; cdecl;

  /// <summary>
  /// Get an available usable free network port.
  /// </summary>
  /// <returns>Returns a free port.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_free_port)</see></para>
  /// </remarks>
  webui_get_free_port : function(): NativeUInt; cdecl;

  /// <summary>
  /// Control the WebUI behaviour. It's recommended to be called at the beginning.
  /// </summary>
  /// <param name="option">The desired option from `webui_config` enum.</param>
  /// <param name="status">The status of the option, `true` or `false`.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_config)</see></para>
  /// </remarks>
  webui_set_config : procedure(option: TWebUIConfig; status: boolean); cdecl;

  /// <summary>
  /// Control if UI events comming from this window should be processed
  /// one a time in a single blocking thread `True`, or process every event in
  /// a new non-blocking thread `False`. This update single window. You can use
  /// `webui_set_config(ui_event_blocking, ...)` to update all windows.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="status">The blocking status `true` or `false`.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_event_blocking)</see></para>
  /// </remarks>
  webui_set_event_blocking : procedure(window: TWebUIWindowID; status: boolean); cdecl;

  /// <summary>
  /// Get the HTTP mime type of a file.
  /// </summary>
  /// <param name="file_">The file name.</param>
  /// <returns>Returns the HTTP mime string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_mime_type)</see></para>
  /// </remarks>
  webui_get_mime_type : function(const file_: PWebUIChar): PWebUIChar; cdecl;

  /// <summary>
  /// Set the SSL/TLS certificate and the private key content, both in PEM
  /// format. This works only with `webui-2-secure` library. If set empty WebUI
  /// will generate a self-signed certificate.
  /// </summary>
  /// <param name="certificate_pem">The SSL/TLS certificate content in PEM format.</param>
  /// <param name="private_key_pem">The private key content in PEM format.</param>
  /// <returns>Returns True if the certificate and the key are valid.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_tls_certificate)</see></para>
  /// </remarks>
  webui_set_tls_certificate : function(const certificate_pem, private_key_pem: PWebUIChar): boolean; cdecl;

  /// <summary>
  /// Run JavaScript without waiting for the response. All clients.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="script">The JavaScript to be run.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_run)</see></para>
  /// </remarks>
  webui_run : procedure(window: TWebUIWindowID; const script: PWebUIChar); cdecl;

  /// <summary>
  /// Run JavaScript without waiting for the response. Single client.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="script">The JavaScript to be run.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_run_client)</see></para>
  /// </remarks>
  webui_run_client : procedure(e: PWebUIEvent; const script: PWebUIChar); cdecl;

  /// <summary>
  /// Run JavaScript and get the response back. Work only in single client mode.
  /// Make sure your local buffer can hold the response.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="script">The JavaScript to be run.</param>
  /// <param name="timeout">The execution timeout in seconds.</param>
  /// <param name="buffer">The local buffer to hold the response.</param>
  /// <param name="buffer_length">The local buffer size.</param>
  /// <returns>Returns True if there is no execution error.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_script)</see></para>
  /// </remarks>
  webui_script : function(window: TWebUIWindowID; const script: PWebUIChar; timeout: NativeUInt; buffer: PWebUIChar; buffer_length: NativeUInt): boolean; cdecl;

  /// <summary>
  /// Run JavaScript and get the response back. Single client.
  /// Make sure your local buffer can hold the response.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="script">The JavaScript to be run.</param>
  /// <param name="timeout">The execution timeout in seconds.</param>
  /// <param name="buffer">The local buffer to hold the response.</param>
  /// <param name="buffer_length">The local buffer size.</param>
  /// <returns>Returns True if there is no execution error.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_script_client)</see></para>
  /// </remarks>
  webui_script_client : function(e: PWebUIEvent; const script: PWebUIChar; timeout: NativeUInt; buffer: PWebUIChar; buffer_length: NativeUInt): boolean; cdecl;

  /// <summary>
  /// Chose between Deno and Nodejs as runtime for .js and .ts files.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="runtime">Deno, Bun, Nodejs or None.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_runtime)</see></para>
  /// </remarks>
  webui_set_runtime : procedure(window: TWebUIWindowID; runtime: TWebUIRuntime); cdecl;

  /// <summary>
  /// Get how many arguments there are in an event.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns the arguments count.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_count)</see></para>
  /// </remarks>
  webui_get_count : function(e: PWebUIEvent): NativeUInt; cdecl;

  /// <summary>
  /// Get an argument as integer at a specific index.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="index">The argument position starting from 0.</param>
  /// <returns>Returns argument as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_int_at)</see></para>
  /// </remarks>
  webui_get_int_at : function(e: PWebUIEvent; index: NativeUInt): int64; cdecl;

  /// <summary>
  /// Get the first argument as integer.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns argument as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_int)</see></para>
  /// </remarks>
  webui_get_int : function(e: PWebUIEvent): int64; cdecl;

  /// <summary>
  /// Get an argument as float at a specific index.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="index">The argument position starting from 0.</param>
  /// <returns>Returns argument as float.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_float_at)</see></para>
  /// </remarks>
  webui_get_float_at : function(e: PWebUIEvent; index: NativeUInt): double; cdecl;

  /// <summary>
  /// Get the first argument as float.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns argument as float.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_float)</see></para>
  /// </remarks>
  webui_get_float : function(e: PWebUIEvent): double; cdecl;

  /// <summary>
  /// Get an argument as string at a specific index.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="index">The argument position starting from 0.</param>
  /// <returns>Returns argument as string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string_at)</see></para>
  /// </remarks>
  webui_get_string_at : function(e: PWebUIEvent; index: NativeUInt): PWebUIChar; cdecl;

  /// <summary>
  /// Get the first argument as string.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns argument as string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_string)</see></para>
  /// </remarks>
  webui_get_string : function(e: PWebUIEvent): PWebUIChar; cdecl;

  /// <summary>
  /// Get an argument as boolean at a specific index.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="index">The argument position starting from 0.</param>
  /// <returns>Returns argument as boolean.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_bool_at)</see></para>
  /// </remarks>
  webui_get_bool_at : function(e: PWebUIEvent; index: NativeUInt): boolean; cdecl;

  /// <summary>
  /// Get the first argument as boolean.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns argument as boolean.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_bool)</see></para>
  /// </remarks>
  webui_get_bool : function(e: PWebUIEvent): boolean; cdecl;

  /// <summary>
  /// Get the size in bytes of an argument at a specific index.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="index">The argument position starting from 0.</param>
  /// <returns>Returns size in bytes.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_size_at)</see></para>
  /// </remarks>
  webui_get_size_at : function(e: PWebUIEvent; index: NativeUInt): NativeUInt; cdecl;

  /// <summary>
  /// Get size in bytes of the first argument.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <returns>Returns size in bytes.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_size)</see></para>
  /// </remarks>
  webui_get_size : function(e: PWebUIEvent): NativeUInt; cdecl;

  /// <summary>
  /// Return the response to JavaScript as integer.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="n">The integer to be send to JavaScript.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_int)</see></para>
  /// </remarks>
  webui_return_int : procedure(e: PWebUIEvent; n: int64); cdecl;

  /// <summary>
  /// Return the response to JavaScript as float.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="f">The float number to be send to JavaScript.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_float)</see></para>
  /// </remarks>
  webui_return_float : procedure(e: PWebUIEvent; f: double); cdecl;

  /// <summary>
  /// Return the response to JavaScript as string.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="s">The string to be send to JavaScript.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_string)</see></para>
  /// </remarks>
  webui_return_string : procedure(e: PWebUIEvent; const s: PWebUIChar); cdecl;

  /// <summary>
  /// Return the response to JavaScript as boolean.
  /// </summary>
  /// <param name="e">The event struct.</param>
  /// <param name="b">The boolean to be send to JavaScript.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_bool)</see></para>
  /// </remarks>
  webui_return_bool : procedure(e: PWebUIEvent; b: boolean); cdecl;

  /// <summary>
  /// Bind a specific HTML element click event with a function. Empty element means all events.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="element">The element ID.</param>
  /// <param name="func">The callback as myFunc(Window, EventType, Element, EventNumber, BindID).</param>
  /// <returns>Returns unique bind ID.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_bind)</see></para>
  /// </remarks>
  webui_interface_bind : function(window: TWebUIWindowID; const element: PWebUIChar; func: TWebUIInterfaceEventCallback): TWebUIBindID; cdecl;

  /// <summary>
  /// When using `webui_interface_bind()`, you may need this function to easily set a response.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="response">The response as string to be send to JavaScript.</param>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_set_response)</see></para>
  /// </remarks>
  webui_interface_set_response : procedure(window: TWebUIWindowID; event_number: TWebUIEventID; const response: PWebUIChar); cdecl;

  /// <summary>
  /// Check if the app still running.
  /// </summary>
  /// <returns>Returns True if app is running.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_is_app_running)</see></para>
  /// </remarks>
  webui_interface_is_app_running : function(): boolean; cdecl;

  /// <summary>
  /// Get a unique window ID.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <returns>Returns the unique window ID as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_window_id)</see></para>
  /// </remarks>
  webui_interface_get_window_id : function(window: TWebUIWindowID): TWebUIWindowID; cdecl;

  /// <summary>
  /// Get an argument as string at a specific index.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="index">The argument position.</param>
  /// <returns>Returns argument as string.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_string_at)</see></para>
  /// </remarks>
  webui_interface_get_string_at : function(window: TWebUIWindowID; event_number: TWebUIEventID; index: NativeUInt): PWebUIChar; cdecl;

  /// <summary>
  /// Get an argument as integer at a specific index.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="index">The argument position.</param>
  /// <returns>Returns argument as integer.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_int_at)</see></para>
  /// </remarks>
  webui_interface_get_int_at : function(window: TWebUIWindowID; event_number: TWebUIEventID; index: NativeUInt): int64; cdecl;

  /// <summary>
  /// Get an argument as float at a specific index.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="index">The argument position.</param>
  /// <returns>Returns argument as float.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_float_at)</see></para>
  /// </remarks>
  webui_interface_get_float_at : function(window: TWebUIWindowID; event_number: TWebUIEventID; index: NativeUInt): double; cdecl;

  /// <summary>
  /// Get an argument as boolean at a specific index.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="index">The argument position.</param>
  /// <returns>Returns argument as boolean.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_bool_at)</see></para>
  /// </remarks>
  webui_interface_get_bool_at : function(window: TWebUIWindowID; event_number: TWebUIEventID; index: NativeUInt): boolean; cdecl;

  /// <summary>
  /// Get the size in bytes of an argument at a specific index.
  /// </summary>
  /// <param name="window">The window number.</param>
  /// <param name="event_number">The event number.</param>
  /// <param name="index">The argument position.</param>
  /// <returns>Returns size in bytes.</returns>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_get_size_at)</see></para>
  /// </remarks>
  webui_interface_get_size_at : function(window: TWebUIWindowID; event_number: TWebUIEventID; index: NativeUInt): NativeUInt; cdecl;

implementation

end.
