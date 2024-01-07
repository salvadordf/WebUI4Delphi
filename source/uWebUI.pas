unit uWebUI;

{$I uWebUI.inc}

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$MINENUMSIZE 4}

{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
    System.Math, System.SyncObjs,
    {$IFDEF MACOS}
    FMX.Helpers.Mac, System.Messaging, Macapi.CoreFoundation, Macapi.Foundation,
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, Math, SyncObjs,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions, uWebUIWindow;

type
  /// <summary>
  /// Class used to simplify the WebUI initialization and destruction.
  /// </summary>
  TWebUI = class
    protected
      FLibHandle                              : THandle;
      FSetCurrentDir                          : boolean;
      FReRaiseExceptions                      : boolean;
      FLibraryPath                            : string;
      FStatus                                 : TLoaderStatus;
      FErrorLog                               : TStringList;
      FError                                  : int64;
      FShowMessageDlg                         : boolean;
      FTimeout                                : NativeUInt;
      FWindowList                             : TList;
      FCritSection                            : TCriticalSection;
      {$IFDEF DELPHI14_UP}
      FSyncedEvents                           : boolean;
      {$ENDIF}
      function  GetErrorMessage : string;
      function  GetInitialized : boolean;
      function  GetInitializationError : boolean;
      function  GetIsAppRunning : boolean;
      function  GetStatus : TLoaderStatus;
      function  GetLibraryVersion : string;
      function  DefaultLibraryPath : string;

      procedure SetTimeout(aValue: NativeUInt);
      procedure SetStatus(aValue: TLoaderStatus);

      procedure DestroyWindowList;
      function  LoadWebUILibrary : boolean;
      function  LoadLibProcedures : boolean;
      procedure UnLoadWebUILibrary;
      procedure ShowErrorMessageDlg(const aError : string);
      function  SearchWindowIndex(windowId: TWebUIWindowID) : integer;
      function  Lock: boolean;
      procedure Unlock;

    public
      constructor Create;
      procedure   AfterConstruction; override;
      procedure   BeforeDestruction; override;
      /// <summary>
      /// Initialize the WebUI library.
      /// </summary>
      function    Initialize : boolean;
      /// <summary>
      /// Append aText to the ErrorMessage property.
      /// </summary>
      procedure   AppendErrorLog(const aText : string);
      /// <summary>
      /// Wait until all opened windows get closed.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_wait)</see></para>
      /// </remarks>
      procedure   Wait;
      /// <summary>
      /// Free all memory resources. Should be called only at the end.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_clean)</see></para>
      /// </remarks>
      procedure   Clean;
      /// <summary>
      /// Close all open windows. `webui_wait()` will return (Break).
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_exit)</see></para>
      /// </remarks>
      procedure   Exit;
      /// <summary>
      /// Delete all local web-browser profiles folder. It should called at the end.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_delete_all_profiles)</see></para>
      /// </remarks>
      procedure   DeleteAllProfiles;
      /// <summary>
      /// Set the web-server root folder path for all windows. Should be used before `webui_show()`.
      /// </summary>
      /// <param name="path">The local folder full path.</param>
      /// <returns>Returns True if the function was successful.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_default_root_folder)</see></para>
      /// </remarks>
      function    SetDefaultRootFolder(const path : string) : boolean;
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
      function    SetTLSCertificate(const certificate_pem, private_key_pem : string): boolean;
      /// <summary>
      /// Search an IWebUIWindow instance.
      /// </summary>
      function    SearchWindow(windowId: TWebUIWindowID) : IWebUIWindow;
      /// <summary>
      /// Add an IWebUIWindow instance.
      /// </summary>
      function    AddWindow(const window: IWebUIWindow): int64;
      /// <summary>
      /// Remove an IWebUIWindow instance.
      /// </summary>
      procedure   RemoveWindow(windowId: TWebUIWindowID);

      /// <summary>
      /// Returns the TWVLoader initialization status.
      /// </summary>
      property Status                                 : TLoaderStatus                      read GetStatus                                write SetStatus;
      /// <summary>
      /// Returns all the text appended to the error log with AppendErrorLog.
      /// </summary>
      property ErrorMessage                           : string                             read GetErrorMessage;
      /// <summary>
      ///  Used to set the current directory when the WebView2 library is loaded. This is required if the application is launched from a different application.
      /// </summary>
      property SetCurrentDir                          : boolean                            read FSetCurrentDir                           write FSetCurrentDir;
      /// <summary>
      /// Set to true to raise all exceptions.
      /// </summary>
      property ReRaiseExceptions                      : boolean                            read FReRaiseExceptions                       write FReRaiseExceptions;
      /// <summary>
      /// Full path to WebUI library. Leave empty to load the library from the current directory.
      /// </summary>
      property LibraryPath                            : string                             read FLibraryPath                             write FLibraryPath;
      /// <summary>
      /// Supported WebUI library version.
      /// </summary>
      property LibraryVersion                         : string                             read GetLibraryVersion;
      /// <summary>
      /// Set to true when you need to use a showmessage dialog to show the error messages.
      /// </summary>
      property ShowMessageDlg                         : boolean                            read FShowMessageDlg                          write FShowMessageDlg;
      /// <summary>
      /// Returns true if the Status is lsInitialized.
      /// </summary>
      property Initialized                            : boolean                            read GetInitialized;
      /// <summary>
      /// Returns true if the Status is lsError.
      /// </summary>
      property InitializationError                    : boolean                            read GetInitializationError;
      /// <summary>
      /// Check if the app still running.
      /// </summary>
      /// <returns>Returns True if app is running.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_is_app_running)</see></para>
      /// </remarks>
      property IsAppRunning                           : boolean                            read GetIsAppRunning;
      /// <summary>
      /// Timeout in seconds before the browser starts. 0 means wait forever.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_set_timeout)</see></para>
      /// </remarks>
      property Timeout                                : NativeUInt                         read FTimeout                                 write SetTimeout;
      {$IFDEF DELPHI14_UP}
      /// <summary>
      /// Execute the events in the main application thread whenever it's possible.
      /// </summary>
      property SyncedEvents                           : boolean                            read FSyncedEvents                            write FSyncedEvents;
      {$ENDIF}
  end;

var
  WebUI : TWebUI = nil;

procedure DestroyWebUI;
procedure global_webui_event_callback(e: PWebUIEvent);

implementation

uses
  {$IFDEF LINUXFPC}Forms, InterfaceBase,{$ENDIF}
  uWebUIMiscFunctions, uWebUIEventHandler;

procedure DestroyWebUI;
begin
  if assigned(WebUI) then
    FreeAndNil(WebUI);
end;

procedure global_webui_event_callback(e: PWebUIEvent);
var
  LWindow : IWebUIWindow;
  LEvent  : IWebUIEventHandler;
begin
  if assigned(WebUI) and WebUI.Initialized then
    try
      LEvent  := TWebUIEventHandler.Create(e);
      LWindow := WebUI.SearchWindow(LEvent.WindowID);

      if assigned(LWindow) then
        LWindow.doOnWebUIEvent(LEvent);
    finally
      LEvent  := nil;
      LWindow := nil;
    end;
end;

constructor TWebUI.Create;
begin
  inherited Create;

  FLibHandle                              := 0;
  FSetCurrentDir                          := True;
  FReRaiseExceptions                      := False;
  FLibraryPath                          := '';
  FStatus                                 := lsCreated;
  FErrorLog                               := nil;
  FShowMessageDlg                         := True;
  FTimeout                                := WEBUI_DEFAULT_TIMEOUT;
  FWindowList                             := nil;
  FCritSection                            := nil;
  {$IFDEF DELPHI14_UP}
  FSyncedEvents                           := True;
  {$ENDIF}
end;

procedure TWebUI.AfterConstruction;
begin
  inherited AfterConstruction;

  FCritSection := TCriticalSection.Create;
  FWindowList  := TList.Create;
  FErrorLog    := TStringList.Create;
end;

procedure TWebUI.BeforeDestruction;
begin
  try
    DestroyWindowList;
    Clean;
    UnLoadWebUILibrary;

    if assigned(FCritSection) then
      FreeAndNil(FCritSection);

    if assigned(FErrorLog) then
      FreeAndNil(FErrorLog);
  finally
    inherited BeforeDestruction;
  end;
end;

function TWebUI.Lock: boolean;
begin
  Result := False;

  if assigned(FCritSection) then
    begin
      FCritSection.Acquire;
      Result := True;
    end;
end;

procedure TWebUI.Unlock;
begin
  if assigned(FCritSection) then
    FCritSection.Release;
end;

procedure TWebUI.DestroyWindowList;
var
  i: integer;
begin
  if assigned(FWindowList) then
    begin
      for i := 0 to pred(FWindowList.Count) do
        FWindowList[i] := nil;

      FreeAndNil(FWindowList);
    end;
end;

function TWebUI.Initialize : boolean;
begin
  Result := LoadWebUILibrary and
            LoadLibProcedures;
end;

procedure TWebUI.UnLoadWebUILibrary;
begin
  try
    if (FLibHandle <> 0) then
      begin
        FreeLibrary(FLibHandle);
        FLibHandle := 0;
        Status     := lsUnloaded;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TWebUI.UnLoadWebUILibrary', e) then raise;
  end;
end;

function TWebUI.LoadWebUILibrary : boolean;
var
  TempOldDir      : string;
  TempLibraryPath : string;
begin
  Result := False;

  try
    if (FLibHandle <> 0) then
      Result := True
     else
      try
        if FSetCurrentDir then
          begin
            TempOldDir := {$IFDEF FPC}string({$ENDIF}GetCurrentDir{$IFDEF FPC}){$ENDIF};
            chdir(GetModulePath);
          end;

        Status := lsLoading;

        if (FLibraryPath <> '') then
          TempLibraryPath := FLibraryPath
         else
          TempLibraryPath := DefaultLibraryPath;

        if FileExists(TempLibraryPath) then
          begin
            {$IFDEF FPC}
              {$IFDEF MSWINDOWS}
              FLibHandle := LoadLibraryW(PWideChar(TempLibraryPath));
              {$ELSE}
              FLibHandle := LoadLibrary(TempLibraryPath);
              {$ENDIF}
            {$ELSE}
            FLibHandle := LoadLibrary({$IFDEF DELPHI12_UP}PWideChar{$ELSE}PAnsiChar{$ENDIF}(TempLibraryPath));
            {$ENDIF}

            if (FLibHandle = 0) then
              begin
                Status := lsError;
                {$IFDEF MSWINDOWS}
                FError := GetLastError;
                {$ENDIF}
                AppendErrorLog('Error loading ' + TempLibraryPath);
                {$IFDEF MSWINDOWS}
                AppendErrorLog('Error code : 0x' + {$IFDEF FPC}string({$ENDIF}inttohex(cardinal(FError), 8)){$IFDEF FPC}){$ENDIF};
                AppendErrorLog({$IFDEF FPC}string({$ENDIF}SysErrorMessage(cardinal(FError)){$IFDEF FPC}){$ENDIF});
                {$ENDIF}
                ShowErrorMessageDlg(ErrorMessage);
              end
             else
              begin
                Status := lsLoaded;
                Result := True;
              end;
          end
         else
          begin
            Status := lsError;

            AppendErrorLog('Error loading ' + TempLibraryPath);
            AppendErrorLog('The WebUI library is missing.');

            ShowErrorMessageDlg(ErrorMessage);
          end;
      finally
        if FSetCurrentDir then
          chdir(TempOldDir);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TWebUI.LoadWebUILibrary', e) then raise;
  end;
end;

function TWebUI.LoadLibProcedures : boolean;
begin
  Result := False;

  if (FLibHandle <> 0) then
    try
      begin
        webui_new_window                    := GetProcAddress(FLibHandle, 'webui_new_window');
        webui_new_window_id                 := GetProcAddress(FLibHandle, 'webui_new_window_id');
        webui_get_new_window_id             := GetProcAddress(FLibHandle, 'webui_get_new_window_id');
        webui_bind                          := GetProcAddress(FLibHandle, 'webui_bind');
        webui_show                          := GetProcAddress(FLibHandle, 'webui_show');
        webui_show_browser                  := GetProcAddress(FLibHandle, 'webui_show_browser');
        webui_set_kiosk                     := GetProcAddress(FLibHandle, 'webui_set_kiosk');
        webui_wait                          := GetProcAddress(FLibHandle, 'webui_wait');
        webui_close                         := GetProcAddress(FLibHandle, 'webui_close');
        webui_destroy                       := GetProcAddress(FLibHandle, 'webui_destroy');
        webui_exit                          := GetProcAddress(FLibHandle, 'webui_exit');
        webui_set_root_folder               := GetProcAddress(FLibHandle, 'webui_set_root_folder');
        webui_set_default_root_folder       := GetProcAddress(FLibHandle, 'webui_set_default_root_folder');
        webui_set_file_handler              := GetProcAddress(FLibHandle, 'webui_set_file_handler');
        webui_is_shown                      := GetProcAddress(FLibHandle, 'webui_is_shown');
        webui_set_timeout                   := GetProcAddress(FLibHandle, 'webui_set_timeout');
        webui_set_icon                      := GetProcAddress(FLibHandle, 'webui_set_icon');
        webui_encode                        := GetProcAddress(FLibHandle, 'webui_encode');
        webui_decode                        := GetProcAddress(FLibHandle, 'webui_decode');
        webui_free                          := GetProcAddress(FLibHandle, 'webui_free');
        webui_malloc                        := GetProcAddress(FLibHandle, 'webui_malloc');
        webui_send_raw                      := GetProcAddress(FLibHandle, 'webui_send_raw');
        webui_set_hide                      := GetProcAddress(FLibHandle, 'webui_set_hide');
        webui_set_size                      := GetProcAddress(FLibHandle, 'webui_set_size');
        webui_set_position                  := GetProcAddress(FLibHandle, 'webui_set_position');
        webui_set_profile                   := GetProcAddress(FLibHandle, 'webui_set_profile');
        webui_set_proxy                     := GetProcAddress(FLibHandle, 'webui_set_proxy');
        webui_get_url                       := GetProcAddress(FLibHandle, 'webui_get_url');
        webui_set_public                    := GetProcAddress(FLibHandle, 'webui_set_public');
        webui_navigate                      := GetProcAddress(FLibHandle, 'webui_navigate');
        webui_clean                         := GetProcAddress(FLibHandle, 'webui_clean');
        webui_delete_all_profiles           := GetProcAddress(FLibHandle, 'webui_delete_all_profiles');
        webui_delete_profile                := GetProcAddress(FLibHandle, 'webui_delete_profile');
        webui_get_parent_process_id         := GetProcAddress(FLibHandle, 'webui_get_parent_process_id');
        webui_get_child_process_id          := GetProcAddress(FLibHandle, 'webui_get_child_process_id');
        webui_set_port                      := GetProcAddress(FLibHandle, 'webui_set_port');
        webui_set_tls_certificate           := GetProcAddress(FLibHandle, 'webui_set_tls_certificate');
        webui_run                           := GetProcAddress(FLibHandle, 'webui_run');
        webui_script                        := GetProcAddress(FLibHandle, 'webui_script');
        webui_set_runtime                   := GetProcAddress(FLibHandle, 'webui_set_runtime');
        webui_get_int_at                    := GetProcAddress(FLibHandle, 'webui_get_int_at');
        webui_get_int                       := GetProcAddress(FLibHandle, 'webui_get_int');
        webui_get_string_at                 := GetProcAddress(FLibHandle, 'webui_get_string_at');
        webui_get_string                    := GetProcAddress(FLibHandle, 'webui_get_string');
        webui_get_bool_at                   := GetProcAddress(FLibHandle, 'webui_get_bool_at');
        webui_get_bool                      := GetProcAddress(FLibHandle, 'webui_get_bool');
        webui_get_size_at                   := GetProcAddress(FLibHandle, 'webui_get_size_at');
        webui_get_size                      := GetProcAddress(FLibHandle, 'webui_get_size');
        webui_return_int                    := GetProcAddress(FLibHandle, 'webui_return_int');
        webui_return_string                 := GetProcAddress(FLibHandle, 'webui_return_string');
        webui_return_bool                   := GetProcAddress(FLibHandle, 'webui_return_bool');
        webui_interface_bind                := GetProcAddress(FLibHandle, 'webui_interface_bind');
        webui_interface_set_response        := GetProcAddress(FLibHandle, 'webui_interface_set_response');
        webui_interface_is_app_running      := GetProcAddress(FLibHandle, 'webui_interface_is_app_running');
        webui_interface_get_window_id       := GetProcAddress(FLibHandle, 'webui_interface_get_window_id');
        webui_interface_get_string_at       := GetProcAddress(FLibHandle, 'webui_interface_get_string_at');
        webui_interface_get_int_at          := GetProcAddress(FLibHandle, 'webui_interface_get_int_at');
        webui_interface_get_bool_at         := GetProcAddress(FLibHandle, 'webui_interface_get_bool_at');
        webui_interface_get_size_at         := GetProcAddress(FLibHandle, 'webui_interface_get_size_at');

        if assigned(webui_new_window) and
           assigned(webui_new_window_id) and
           assigned(webui_get_new_window_id) and
           assigned(webui_bind) and
           assigned(webui_show) and
           assigned(webui_show_browser) and
           assigned(webui_set_kiosk) and
           assigned(webui_wait) and
           assigned(webui_close) and
           assigned(webui_destroy) and
           assigned(webui_exit) and
           assigned(webui_set_root_folder) and
           assigned(webui_set_default_root_folder) and
           assigned(webui_set_file_handler) and
           assigned(webui_is_shown) and
           assigned(webui_set_timeout) and
           assigned(webui_set_icon) and
           assigned(webui_encode) and
           assigned(webui_decode) and
           assigned(webui_free) and
           assigned(webui_malloc) and
           assigned(webui_send_raw) and
           assigned(webui_set_hide) and
           assigned(webui_set_size) and
           assigned(webui_set_position) and
           assigned(webui_set_profile) and
           assigned(webui_set_proxy) and
           assigned(webui_get_url) and
           assigned(webui_set_public) and
           assigned(webui_navigate) and
           assigned(webui_clean) and
           assigned(webui_delete_all_profiles) and
           assigned(webui_delete_profile) and
           assigned(webui_get_parent_process_id) and
           assigned(webui_get_child_process_id) and
           assigned(webui_set_port) and
           assigned(webui_set_tls_certificate) and
           assigned(webui_run) and
           assigned(webui_script) and
           assigned(webui_set_runtime) and
           assigned(webui_get_int_at) and
           assigned(webui_get_int) and
           assigned(webui_get_string_at) and
           assigned(webui_get_string) and
           assigned(webui_get_bool_at) and
           assigned(webui_get_bool) and
           assigned(webui_get_size_at) and
           assigned(webui_get_size) and
           assigned(webui_return_int) and
           assigned(webui_return_string) and
           assigned(webui_return_bool) and
           assigned(webui_interface_bind) and
           assigned(webui_interface_set_response) and
           assigned(webui_interface_is_app_running) and
           assigned(webui_interface_get_window_id) and
           assigned(webui_interface_get_string_at) and
           assigned(webui_interface_get_int_at) and
           assigned(webui_interface_get_bool_at) and
           assigned(webui_interface_get_size_at) then
          begin
            Result := True;
            Status := lsInitialized;
          end
         else
          begin
            Status := lsError;
            AppendErrorLog('There was a problem loading the library procedures.');

            ShowErrorMessageDlg(ErrorMessage);
          end;
      end;
    except
      on e : exception do
        if CustomExceptionHandler('TWebUI.LoadLibProcedures', e) then raise;
    end;
end;

procedure TWebUI.AppendErrorLog(const aText : string);
begin
  OutputDebugMessage(aText);
  if Lock then
    try
      if assigned(FErrorLog) then
        FErrorLog.Add({$IFDEF FPC}UTF8Encode({$ENDIF}aText{$IFDEF FPC}){$ENDIF});
    finally
      UnLock;
    end;
end;

{$IFNDEF FPC}
{$IFDEF MACOSX}
procedure ShowMessageCF(const aHeading, aMessage : string; const aTimeoutInSecs : double = 0);
var
  TempHeading, TempMessage : CFStringRef;
  TempResponse : CFOptionFlags;
begin
  TempHeading := CFStringCreateWithCharactersNoCopy(nil, PChar(aHeading), Length(AHeading), kCFAllocatorNull);
  TempMessage := CFStringCreateWithCharactersNoCopy(nil, PChar(aMessage), Length(AMessage), kCFAllocatorNull);

  try
    CFUserNotificationDisplayAlert(aTimeoutInSecs, kCFUserNotificationNoteAlertLevel, nil, nil, nil, TempHeading, TempMessage, nil, nil, nil, TempResponse);
  finally
    CFRelease(TempHeading);
    CFRelease(TempMessage);
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TWebUI.ShowErrorMessageDlg(const aError : string);
{$IFDEF LINUXFPC}
const
  MB_OK        = $00000000;
  MB_ICONERROR = $00000010;
{$ENDIF}
begin
  OutputDebugMessage(aError);

  if FShowMessageDlg then
    begin
      {$IFDEF MSWINDOWS}
        {$IFDEF FPC}
        MessageBoxW(0, PWideChar(aError + #0), PWideChar('Error' + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
        {$ELSE}
        MessageBox(0, PChar(aError + #0), PChar('Error' + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
        {$ENDIF}
      {$ENDIF}

      {$IFDEF LINUX}
        {$IFDEF FPC}
        if (WidgetSet <> nil) then
          Application.MessageBox(PAnsiChar(UTF8Encode(aError + #0)), PAnsiChar(AnsiString('Error' + #0)), MB_ICONERROR or MB_OK);
        {$ELSE}
        // TO-DO: Find a way to show message boxes in FMXLinux
        {$ENDIF}
      {$ENDIF}

      {$IFDEF MACOSX}
        {$IFDEF FPC}
        // TO-DO: Find a way to show message boxes in Lazarus/FPC for MacOS
        {$ELSE}
        ShowMessageCF('Error', aError, 10);
        {$ENDIF}
      {$ENDIF}
    end;
end;

function TWebUI.GetErrorMessage : string;
begin
  Result := '';

  if Lock then
    try
      if assigned(FErrorLog) then
        Result := {$IFDEF FPC}UTF8Decode({$ENDIF}FErrorLog.Text{$IFDEF FPC}){$ENDIF};
    finally
      UnLock;
    end;
end;

function TWebUI.GetInitialized : boolean;
begin
  Result := False;

  if Lock then
    try
      Result := (FStatus = lsInitialized);
    finally
      UnLock;
    end;
end;

function TWebUI.GetInitializationError : boolean;
begin
  Result := False;

  if Lock then
    try
      Result := (FStatus = lsError);
    finally
      UnLock;
    end;
end;

function TWebUI.GetIsAppRunning : boolean;
begin
  Result := Initialized and
            webui_interface_is_app_running();
end;

function TWebUI.GetStatus : TLoaderStatus;
begin
  Result := lsCreated;
  if Lock then
    try
      Result := FStatus;
    finally
      UnLock;
    end;
end;

function TWebUI.GetLibraryVersion : string;
begin
  Result := {$IFDEF FPC}string({$ENDIF}inttostr(WEBUI_VERSION_MAJOR){$IFDEF FPC}){$ENDIF} + '.' +
            {$IFDEF FPC}string({$ENDIF}inttostr(WEBUI_VERSION_MINOR){$IFDEF FPC}){$ENDIF} + '.' +
            {$IFDEF FPC}string({$ENDIF}inttostr(WEBUI_VERSION_RELEASE){$IFDEF FPC}){$ENDIF};
end;

function TWebUI.DefaultLibraryPath : string;
begin
  {$IFDEF MACOSX}
  Result := IncludeTrailingPathDelimiter(GetModulePath) + 'Contents/Frameworks/' + WEBUI_LIB;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(GetModulePath) + WEBUI_LIB;
  {$ENDIF}
end;

procedure TWebUI.SetTimeout(aValue: NativeUInt);
begin
  FTimeout := aValue;

  if Initialized then
    webui_set_timeout(FTimeout);
end;

procedure TWebUI.SetStatus(aValue: TLoaderStatus);
begin
  if Lock then
    try
      FStatus := aValue;
    finally
      UnLock;
    end;
end;

procedure TWebUI.Wait;
begin
  if Initialized then
    webui_wait();
end;

procedure TWebUI.Clean;
begin
  if Initialized then
    webui_clean();
end;

procedure TWebUI.Exit;
begin
  if Initialized then
    webui_exit();
end;

procedure TWebUI.DeleteAllProfiles;
begin
  if Initialized then
    webui_delete_all_profiles();
end;

function TWebUI.SetDefaultRootFolder(const path : string) : boolean;
var
  LPath: AnsiString;
begin
  Result := False;

  if Initialized then
    begin
      LPath  := UTF8Encode(path + #0);
      Result := webui_set_default_root_folder(@LPath[1]);
    end;
end;

function TWebUI.SetTLSCertificate(const certificate_pem, private_key_pem : string): boolean;
var
  LCertificate, LPrivateKey : AnsiString;
begin
  Result := False;

  if Initialized then
    begin
      LCertificate  := UTF8Encode(certificate_pem + #0);
      LPrivateKey   := UTF8Encode(private_key_pem + #0);
      Result        := webui_set_tls_certificate(@LCertificate[1], @LPrivateKey[1]);
    end;
end;

function TWebUI.SearchWindowIndex(windowId: TWebUIWindowID) : integer;
var
  i, j: integer;
begin
  Result := -1;

  if assigned(FWindowList) then
    begin
      i := 0;
      j := FWindowList.Count;

      while (i < j) do
        begin
          if assigned(FWindowList[i]) and
             (IWebUIWindow(FWindowList[i]).ID = windowId) then
            begin
              Result := i;
              break;
            end;

          inc(i);
        end;
    end;
end;

function TWebUI.SearchWindow(windowId: TWebUIWindowID) : IWebUIWindow;
var
  i: integer;
begin
  Result := nil;

  if Lock then
    try
      i := SearchWindowIndex(windowId);
      if (i >= 0) then
        Result := IWebUIWindow(FWindowList[i]);
    finally
      Unlock;
    end;
end;

function TWebUI.AddWindow(const window: IWebUIWindow): int64;
begin
  Result := -1;

  if Lock then
    try
      if assigned(FWindowList) and (SearchWindowIndex(window.ID) < 0) then
        Result := FWindowList.Add(Pointer(window));
    finally
      Unlock;
    end;
end;

procedure TWebUI.RemoveWindow(windowId: TWebUIWindowID);
var
  i : int64;
begin
  if Lock then
    try
      i := SearchWindowIndex(windowId);
      if (i >= 0) then
        begin
          FWindowList[i] := nil;
          FWindowList.Delete(i);
        end;
    finally
      Unlock;
    end;
end;

initialization

finalization
  DestroyWebUI;

end.
