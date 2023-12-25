unit uWebUILoader;

{$I uWebUI.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils, Winapi.ShlObj, System.Math,
  {$ELSE}
  Windows, Classes, SysUtils, ShlObj, Math,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  TWebUILoader = class
    protected
      FLibHandle                              : THandle;
      FSetCurrentDir                          : boolean;
      FReRaiseExceptions                      : boolean;
      FLoaderDllPath                          : string;
      FStatus                                 : TLoaderStatus;
      FErrorLog                               : TStringList;
      FError                                  : int64;
      FShowMessageDlg                         : boolean;

      function  GetErrorMessage : string;

      function  LoadLibProcedures : boolean;
      function  LoadWebUILibrary : boolean;
      procedure UnLoadWebUILibrary;
      procedure ShowErrorMessageDlg(const aError : string);

    public
      constructor Create;
      destructor  Destroy; override;
      function    StartWebUI : boolean;
      /// <summary>
      /// Append aText to the ErrorMessage property.
      /// </summary>
      procedure   AppendErrorLog(const aText : string);

      /// <summary>
      /// Returns the TWVLoader initialization status.
      /// </summary>
      property Status                                 : TLoaderStatus                      read FStatus;
      /// <summary>
      /// Returns all the text appended to the error log with AppendErrorLog.
      /// </summary>
      property ErrorMessage                           : string                             read GetErrorMessage;
      /// <summary>
      ///	Used to set the current directory when the WebView2 library is loaded. This is required if the application is launched from a different application.
      /// </summary>
      property SetCurrentDir                          : boolean                            read FSetCurrentDir                           write FSetCurrentDir;
      /// <summary>
      /// Set to true to raise all exceptions.
      /// </summary>
      property ReRaiseExceptions                      : boolean                            read FReRaiseExceptions                       write FReRaiseExceptions;
      /// <summary>
      /// Full path to WebView2Loader.dll. Leave empty to load WebView2Loader.dll from the current directory.
      /// </summary>
      property LoaderDllPath                          : string                             read FLoaderDllPath                           write FLoaderDllPath;
      /// <summary>
      /// Set to true when you need to use a showmessage dialog to show the error messages.
      /// </summary>
      property ShowMessageDlg                         : boolean                            read FShowMessageDlg                          write FShowMessageDlg;
  end;

var
  GlobalWebUILoader : TWebUILoader = nil;

procedure DestroyGlobalWebUILoader;

implementation

uses
  uWebUIMiscFunctions;

procedure DestroyGlobalWebUILoader;
begin
  if assigned(GlobalWebUILoader) then
    FreeAndNil(GlobalWebUILoader);
end;

constructor TWebUILoader.Create;
begin
  inherited Create;

  FLibHandle                              := 0;
  FSetCurrentDir                          := True;
  FReRaiseExceptions                      := False;
  FLoaderDllPath                          := '';
  FStatus                                 := lsCreated;
  FErrorLog                               := nil;
  FShowMessageDlg                         := True;
end;

destructor TWebUILoader.Destroy;
begin
  try
    UnLoadWebUILibrary;

    if assigned(FErrorLog) then
      FreeAndNil(FErrorLog);
  finally
    inherited Destroy;
  end;
end;

function TWebUILoader.StartWebUI : boolean;
begin
  Result := LoadWebUILibrary and
            LoadLibProcedures;
end;

procedure TWebUILoader.UnLoadWebUILibrary;
begin
  try
    if (FLibHandle <> 0) then
      begin
        FreeLibrary(FLibHandle);
        FLibHandle := 0;
        FStatus    := lsUnloaded;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TWebUILoader.UnLoadWebUILibrary', e) then raise;
  end;
end;

function TWebUILoader.LoadWebUILibrary : boolean;
var
  TempOldDir : string;
  TempLoaderLibPath : string;
begin
  Result := False;

  try
    if (FLibHandle <> 0) then
      Result := True
     else
      try
        if FSetCurrentDir then
          begin
            TempOldDir := GetCurrentDir;
            chdir(GetModulePath);
          end;

        FStatus := lsLoading;

        if (FLoaderDllPath <> '') then
          TempLoaderLibPath := FLoaderDllPath
         else
          TempLoaderLibPath := WEBUI_LIB;

        FLibHandle := LoadLibraryW(PWideChar(TempLoaderLibPath));

        if (FLibHandle = 0) then
          begin
            FStatus   := lsError;
            FError    := GetLastError;

            AppendErrorLog('Error loading ' + TempLoaderLibPath);
            AppendErrorLog('Error code : 0x' + {$IFDEF FPC}UTF8Decode({$ENDIF}inttohex(cardinal(FError), 8){$IFDEF FPC}){$ENDIF});
            AppendErrorLog({$IFDEF FPC}UTF8Decode({$ENDIF}SysErrorMessage(cardinal(FError)){$IFDEF FPC}){$ENDIF});

            ShowErrorMessageDlg(ErrorMessage);
          end
         else
          begin
            FStatus := lsLoaded;
            Result  := True;
          end;
      finally
        if FSetCurrentDir then
          chdir(TempOldDir);
      end;
  except
    on e : exception do
      if CustomExceptionHandler('TWebUILoader.LoadWebUILibrary', e) then raise;
  end;
end;

function TWebUILoader.LoadLibProcedures : boolean;
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
            Result  := True;
            FStatus := lsImported;
          end
         else
          begin
            FStatus := lsError;
            AppendErrorLog('There was a problem loading the library procedures');

            ShowErrorMessageDlg(ErrorMessage);
          end;
      end;
    except
      on e : exception do
        if CustomExceptionHandler('TWebUILoader.LoadLibProcedures', e) then raise;
    end;
end;

procedure TWebUILoader.AppendErrorLog(const aText : string);
begin
  OutputDebugMessage(aText);
  if assigned(FErrorLog) then
    FErrorLog.Add(aText);
end;

procedure TWebUILoader.ShowErrorMessageDlg(const aError : string);
begin
  if FShowMessageDlg then
    MessageBoxW(0, PWideChar(aError + #0), PWideChar(WideString('Error') + #0), MB_ICONERROR or MB_OK or MB_TOPMOST);
end;

function TWebUILoader.GetErrorMessage : string;
begin
  if assigned(FErrorLog) then
    Result := FErrorLog.Text
   else
    Result := '';
end;

end.
