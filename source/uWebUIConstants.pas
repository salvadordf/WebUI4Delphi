unit uWebUIConstants;

{$I uWebUI.inc}

interface

const
  /// <summary>
  /// WebUI library version.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (WEBUI_VERSION)</see></para>
  /// </remarks>
  //WEBUI_VERSION = inttostr(WEBUI_VERSION_MAJOR) + '.' + inttostr(WEBUI_VERSION_MINOR) + '.' + inttostr(WEBUI_VERSION_RELEASE);

  /// <summary>
  /// Max windows, servers and threads.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (WEBUI_MAX_IDS)</see></para>
  /// </remarks>
  WEBUI_MAX_IDS = 256;

  /// <summary>
  /// Max allowed argument's index.
  /// </summary>
  /// <remarks>
  /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (WEBUI_MAX_ARG)</see></para>
  /// </remarks>
  WEBUI_MAX_ARG = 16;

  {$IFDEF MSWINDOWS}
  WEBUI_LIB = 'webui-2.dll';
  WEBUI_DEBUG_LIB = 'webui-2_debug.dll';
  {$ENDIF}
  {$IFDEF LINUX}
  WEBUI_LIB = 'webui-2.so';
  WEBUI_DEBUG_LIB = 'webui-2_debug.so';
  {$ENDIF}
  {$IFDEF MACOSX}
  WEBUI_LIB = 'webui-2.dylib';
  WEBUI_DEBUG_LIB = 'webui-2_debug.dylib';
  {$ENDIF}

  /// <summary>
  /// Default timeout in seconds before the browser starts.
  /// </summary>
  WEBUI_DEFAULT_TIMEOUT = 30;

  CRLF = #13 + #10;

implementation

end.
