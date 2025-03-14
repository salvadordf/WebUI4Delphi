unit uWebUIEventHandler;

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
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions, uWebUIWindow;

type
  /// <summary>
  /// Event wrapper for Event objects in WebUI.
  /// </summary>
  TWebUIEventHandler = class(TInterfacedObject, IWebUIEventHandler)
    protected
      FEvent : TWebUIEvent;

      function GetInitialized: boolean;
      function GetEvent: PWebUIEvent;
      function GetWindowID: TWebUIWindowID;
      function GetEventType: TWebUIEventType;
      function GetElement: string;
      function GetEventID: TWebUIEventID;
      function GetBindID: TWebUIBindID;
      function GetWindow: IWebUIWindow;
      function GetCount: NativeUInt;
      function GetClientID: TWebUIClientID;
      function GetConnectionID : TWebUIConnectionID;
      function GetCookies : string;
      function GetContext : Pointer;

    public
      constructor Create(const aEvent: PWebUIEvent); overload;
      constructor Create(window: TWebUIWindowID; event_type: TWebUIEventType; const element: PWebUIChar; event_number: TWebUIEventID; bind_id: TWebUIBindID; client_id: TWebUIClientID; connection_id: TWebUIConnectionID; const cookies: PWebUIChar); overload;

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
      procedure SetResponse(const response: string); overload;
      /// <summary>
      /// When using `webui_interface_bind()`, you may need this function to easily set a response.
      /// </summary>
      /// <param name="response">The response as a stream to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_set_response)</see></para>
      /// </remarks>
      procedure SetResponse(const response: TMemoryStream); overload;
      /// <summary>
      /// When using `webui_interface_bind()`, you may need this function to easily set a response.
      /// </summary>
      /// <param name="response">The response as a stream to be send to JavaScript.</param>
      /// <param name="aOffset">Moves the current stream position by aOffset bytes, relative to the beginning.</param>
      /// <param name="aCount">Copies aCount bytes from the stream.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_interface_set_response)</see></para>
      /// </remarks>
      procedure SetResponse(const response: TMemoryStream; aOffset, aCount: int64); overload;
      /// <summary>
      /// Show a window using embedded HTML, or a file. If the window is already open, it will be refreshed. Single client.
      /// </summary>
      /// <param name="content">The HTML, URL, Or a local file.</param>
      /// <returns>Returns True if showing the window is successed.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_show_client)</see></para>
      /// </remarks>
      function  ShowClient(const content : string) : boolean;
      /// <summary>
      /// Close a specific client.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_close_client)</see></para>
      /// </remarks>
      procedure CloseClient;
      /// <summary>
      /// Safely send raw data to the UI. Single client.
      /// </summary>
      /// <param name="function_">The JavaScript function to receive raw data: `function * myFunc(myData){}`.</param>
      /// <param name="raw">The raw data buffer.</param>
      /// <param name="size">The raw data size in bytes.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_send_raw_client)</see></para>
      /// </remarks>
      procedure   SendRawClient(const function_: string; const raw: Pointer; size: NativeUInt);
      /// <summary>
      /// Navigate to a specific URL. Single client.
      /// </summary>
      /// <param name="url">Full HTTP URL.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_navigate_client)</see></para>
      /// </remarks>
      procedure   NavigateClient(const Url: string);
      /// <summary>
      /// Run JavaScript without waiting for the response. Single client.
      /// </summary>
      /// <param name="script_">The JavaScript to be run.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_run)</see></para>
      /// </remarks>
      procedure   RunClient(const script_: string);
      /// <summary>
      /// Run JavaScript and get the response back. Single client.
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
      function    ScriptClient(const script_: string; timeout: NativeUInt; var buffer: string; buffer_length: NativeUInt): boolean;

      /// <summary>
      /// Returns true if the Window was created successfully.
      /// </summary>
      property Initialized       : boolean            read GetInitialized;
      /// <summary>
      /// Pointer to WebUI event record.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_event_t)</see></para>
      /// </remarks>
      property Event             : PWebUIEvent        read GetEvent;
      /// <summary>
      /// Window wrapper for the Window object of this event.
      /// </summary>
      property Window            : IWebUIWindow       read GetWindow;
      /// <summary>
      /// The window object number or ID.
      /// </summary>
      property WindowID          : TWebUIWindowID     read GetWindowID;
      /// <summary>
      /// Event type.
      /// </summary>
      property EventType         : TWebUIEventType    read GetEventType;
      /// <summary>
      /// HTML element ID.
      /// </summary>
      property Element           : string             read GetElement;
      /// <summary>
      /// Event number or Event ID.
      /// </summary>
      property EventID           : TWebUIEventID      read GetEventID;
      /// <summary>
      /// Bind ID.
      /// </summary>
      property BindID            : TWebUIBindID       read GetBindID;
      /// <summary>
      /// Get how many arguments there are in an event.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_count)</see></para>
      /// </remarks>
      property Count             : NativeUInt         read GetCount;
      /// <summary>
      /// Client's unique ID.
      /// </summary>
      property ClientID          : TWebUIClientID     read GetClientID;
      /// <summary>
      /// Client's connection ID.
      /// </summary>
      property ConnectionID      : TWebUIConnectionID read GetConnectionID;
      /// <summary>
      /// Client's full cookies.
      /// </summary>
      property Cookies           : string             read GetCookies;
      /// <summary>
      /// Get user data that is set using `IWebUIWindow.SetContext`.
      /// </summary>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_get_context)</see></para>
      /// </remarks>
      property Context           : Pointer            read GetContext;
  end;

implementation

uses
  uWebUI;

constructor TWebUIEventHandler.Create(const aEvent: PWebUIEvent);
begin
  inherited Create;

  if assigned(aEvent) then
    begin
      FEvent.window        := aEvent^.window;
      FEvent.event_type    := aEvent^.event_type;
      FEvent.element       := aEvent^.element;
      FEvent.event_number  := aEvent^.event_number;
      FEvent.bind_id       := aEvent^.bind_id;
      FEvent.client_id     := aEvent^.client_id;
      FEvent.connection_id := aEvent^.connection_id;
      FEvent.cookies       := aEvent^.cookies;
    end
   else
    FillChar(FEvent, SizeOf(TWebUIEvent), #0);
end;

constructor TWebUIEventHandler.Create(      window        : TWebUIWindowID;
                                            event_type    : TWebUIEventType;
                                      const element       : PWebUIChar;
                                            event_number  : TWebUIEventID;
                                            bind_id       : TWebUIBindID;
                                            client_id     : TWebUIClientID;
                                            connection_id : TWebUIConnectionID;
                                      const cookies       : PWebUIChar);
begin
  inherited Create;

  FEvent.window        := window;
  FEvent.event_type    := event_type;
  FEvent.element       := element;
  FEvent.event_number  := event_number;
  FEvent.bind_id       := bind_id;
  FEvent.client_id     := client_id;
  FEvent.connection_id := connection_id;
  FEvent.cookies       := cookies;
end;

function TWebUIEventHandler.GetInitialized: boolean;
begin
  Result := (WebUI <> nil) and
            WebUI.Initialized and
            (FEvent.window <> 0);
end;

function TWebUIEventHandler.GetEvent: PWebUIEvent;
begin
  Result := @FEvent;
end;

function TWebUIEventHandler.GetWindowID: TWebUIWindowID;
begin
  Result := FEvent.window;
end;

function TWebUIEventHandler.GetEventType: TWebUIEventType;
begin
  Result := FEvent.event_type;
end;

function TWebUIEventHandler.GetElement: string;
begin
  if assigned(FEvent.Element) then
    Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(FEvent.element))
   else
    Result := '';
end;

function TWebUIEventHandler.GetEventID: TWebUIEventID;
begin
  Result := FEvent.event_number;
end;

function TWebUIEventHandler.GetBindID: TWebUIBindID;
begin
  Result := FEvent.bind_id;
end;

function TWebUIEventHandler.GetWindow: IWebUIWindow;
begin
  Result := WebUI.SearchWindow(FEvent.window);
end;

function TWebUIEventHandler.GetCount: NativeUInt;
begin
  if Initialized then
    Result := webui_get_count(@FEvent)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetClientID: TWebUIClientID;
begin
  Result := FEvent.client_id;
end;

function TWebUIEventHandler.GetConnectionID : TWebUIConnectionID;
begin
  Result := FEvent.connection_id;
end;

function TWebUIEventHandler.GetCookies : string;
begin
  if assigned(FEvent.cookies) then
    Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(FEvent.cookies))
   else
    Result := '';
end;

function TWebUIEventHandler.GetContext : Pointer;
begin
  if Initialized then
    Result := webui_get_context(@FEvent)
   else
    Result := nil;
end;

function TWebUIEventHandler.GetIntAt(index: NativeUInt): int64;
begin
  if Initialized then
    Result := webui_get_int_at(@FEvent, index)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetInt: int64;
begin
  if Initialized then
    Result := webui_get_int(@FEvent)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetFloatAt(index: NativeUInt): double;
begin
  if Initialized then
    Result := webui_get_float_at(@FEvent, index)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetFloat: double;
begin
  if Initialized then
    Result := webui_get_float(@FEvent)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetStringAt(index: NativeUInt): string;
begin
  if Initialized then
    Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(webui_get_string_at(@FEvent, index)))
   else
    Result := '';
end;

function TWebUIEventHandler.GetString: string;
begin
  if Initialized then
    Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(webui_get_string(@FEvent)))
   else
    Result := '';
end;

function TWebUIEventHandler.GetStreamAt(var aResultStream: TMemoryStream; index: NativeUInt): boolean;
var
  LSize   : NativeUInt;
  LBuffer : PWebUIChar;
begin
  Result := False;

  if assigned(aResultStream) then
    begin
      LSize := GetSizeAt(index);

      if (LSize > 0) then
        begin
          LBuffer := webui_get_string_at(@FEvent, index);
          aResultStream.Clear;
          aResultStream.Write(LBuffer^, LSize);
          aResultStream.Seek(0, soBeginning);
          Result := True;
        end;
    end;
end;

function TWebUIEventHandler.GetStream(var aResultStream: TMemoryStream): boolean;
var
  LSize   : NativeUInt;
  LBuffer : PWebUIChar;
begin
  Result := False;

  if assigned(aResultStream) then
    begin
      aResultStream.Clear;
      LSize := GetSize;

      if (LSize > 0) then
        begin
          LBuffer := webui_get_string(@FEvent);
          aResultStream.Clear;
          aResultStream.Write(LBuffer^, LSize);
          aResultStream.Seek(0, soBeginning);
          Result := True;
        end;
    end;
end;

function TWebUIEventHandler.GetBoolAt(index: NativeUInt): boolean;
begin
  Result := Initialized and
            webui_get_bool_at(@FEvent, index);
end;

function TWebUIEventHandler.GetBool: boolean;
begin
  Result := Initialized and
            webui_get_bool(@FEvent);
end;

function TWebUIEventHandler.GetSizeAt(index: NativeUInt): NativeUInt;
begin
  if Initialized then
    Result := webui_get_size_at(@FEvent, index)
   else
    Result := 0;
end;

function TWebUIEventHandler.GetSize: NativeUInt;
begin
  if Initialized then
    Result := webui_get_size(@FEvent)
   else
    Result := 0;
end;

procedure TWebUIEventHandler.ReturnInt(aReturnValue: int64);
begin
  if Initialized then
    webui_return_int(@FEvent, aReturnValue);
end;

procedure TWebUIEventHandler.ReturnFloat(aReturnValue: double);
begin
  if Initialized then
    webui_return_float(@FEvent, aReturnValue);
end;

procedure TWebUIEventHandler.ReturnString(const aReturnValue: string);
var
  LString: AnsiString;
begin
  if Initialized and (length(aReturnValue) > 0) then
    begin
      LString := UTF8Encode(aReturnValue + #0);
      webui_return_string(@FEvent, @LString[1]);
    end;
end;

procedure TWebUIEventHandler.ReturnStream(const aReturnValue: TMemoryStream);
var
  LBuffer : PWebUIChar;
begin
  LBuffer := nil;

  if Initialized and assigned(aReturnValue) and (aReturnValue.Size > 0) then
    try
      LBuffer := webui_malloc(aReturnValue.Size);
      aReturnValue.Seek(0, soBeginning);
      aReturnValue.Read(LBuffer^, aReturnValue.Size);
      aReturnValue.Seek(0, soBeginning);
      webui_return_string(@FEvent, LBuffer);
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

procedure TWebUIEventHandler.ReturnStream(const aReturnValue: TMemoryStream; aOffset, aCount: int64);
var
  LBuffer : PWebUIChar;
begin
  LBuffer := nil;

  if Initialized and assigned(aReturnValue) and (aOffset >= 0) and (aCount > 0) then
    try
      LBuffer := webui_malloc(aCount);
      aReturnValue.Seek(aOffset, soBeginning);
      aReturnValue.Read(LBuffer^, aCount);
      webui_return_string(@FEvent, LBuffer);
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

procedure TWebUIEventHandler.ReturnBool(aReturnValue: boolean);
begin
  if Initialized then
    webui_return_bool(@FEvent, aReturnValue);
end;

procedure TWebUIEventHandler.SetResponse(const response: string);
var
  LResponse    : AnsiString;
  LResponsePtr : PWebUIChar;
begin
  if Initialized then
    begin
      if (length(response) > 0) then
        begin
          LResponse    := UTF8Encode(response + #0);
          LResponsePtr := @LResponse[1];
        end
       else
        LResponsePtr := nil;

      webui_interface_set_response(FEvent.window, FEvent.event_number, LResponsePtr);
    end;
end;

procedure TWebUIEventHandler.SetResponse(const response: TMemoryStream);
var
  LBuffer : PWebUIChar;
begin
  LBuffer := nil;

  if Initialized and assigned(response) and (response.Size > 0) then
    try
      LBuffer := webui_malloc(response.Size);
      response.Seek(0, soBeginning);
      response.Read(LBuffer^, response.Size);
      response.Seek(0, soBeginning);
      webui_interface_set_response(FEvent.window, FEvent.event_number, LBuffer);
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

procedure TWebUIEventHandler.SetResponse(const response: TMemoryStream; aOffset, aCount: int64);
var
  LBuffer : PWebUIChar;
begin
  LBuffer := nil;

  if Initialized and assigned(response) and (aOffset >= 0) and (aCount > 0) then
    try
      LBuffer := webui_malloc(aCount);
      response.Seek(aOffset, soBeginning);
      response.Read(LBuffer^, aCount);
      webui_interface_set_response(FEvent.window, FEvent.event_number, LBuffer);
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

function TWebUIEventHandler.ShowClient(const content : string) : boolean;
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

      Result := webui_show_client(@FEvent, LContentPtr);
    end;
end;

procedure TWebUIEventHandler.CloseClient;
begin
  if Initialized then
    webui_close_client(@FEvent);
end;

procedure TWebUIEventHandler.SendRawClient(const function_: string; const raw: Pointer; size: NativeUInt);
var
  LFunction: AnsiString;
begin
  if Initialized and (length(function_) > 0) then
    begin
      LFunction := UTF8Encode(function_ + #0);
      webui_send_raw_client(@FEvent, @LFunction[1], raw, size);
    end;
end;

procedure TWebUIEventHandler.NavigateClient(const Url: string);
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

      webui_navigate_client(@FEvent, LUrlPtr);
    end;
end;

procedure TWebUIEventHandler.RunClient(const script_: string);
var
  LScript : AnsiString;
begin
  if Initialized and (length(script_) > 0) then
    begin
      LScript := UTF8Encode(script_ + #0);
      webui_run_client(@FEvent, @LScript[1]);
    end;
end;

function TWebUIEventHandler.ScriptClient(const script_: string; timeout: NativeUInt; var buffer: string; buffer_length: NativeUInt): boolean;
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

      if webui_script_client(@FEvent, @LScript[1], timeout, LBuffer, buffer_length) then
        begin
          buffer := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(LBuffer));
          Result := True;
        end;
    finally
      if (LBuffer <> nil) then
        webui_free(LBuffer);
    end;
end;

end.
