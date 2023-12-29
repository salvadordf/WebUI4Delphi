unit uWebUIEventHandler;

{$I uWebUI.inc}

{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
  WinApi.Windows, System.Classes, System.SysUtils,
  {$ELSE}
  Windows, Classes, SysUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions, uWebUIWindow;

type
  /// <summary>
  /// Event wrapper for Event objects in WebUI.
  /// </summary>
  TWebUIEventHandler = class
    protected
      FEvent  : TWebUIEvent;
      FWindow : TWebUIWindow;

      function GetInitialized: boolean;
      function GetEvent: PWebUIEvent;
      function GetWindowID: TWebUIWindowID;
      function GetEventType: TWebUIEventType;
      function GetElement: string;
      function GetEventID: TWebUIEventID;
      function GetBindID: TWebUIBindID;

    public
      constructor Create(const aEvent: PWebUIEvent); overload;
      constructor Create(window: TWebUIWindowID; event_type: TWebUIEventType; const element: PWebUIChar; event_number: TWebUIEventID; bind_id: TWebUIBindID); overload;
      destructor  Destroy; override;

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
      /// Return the response to JavaScript as string.
      /// </summary>
      /// <param name="aReturnValue">The string to be send to JavaScript.</param>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_return_string)</see></para>
      /// </remarks>
      procedure ReturnString(const aReturnValue: string);
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
      property Window            : TWebUIWindow     read FWindow;
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
  end;

implementation

uses
  uWebUI;

constructor TWebUIEventHandler.Create(const aEvent: PWebUIEvent);
begin
  inherited Create;

  if assigned(aEvent) then
    begin
      FEvent.window       := aEvent^.window;
      FEvent.event_type   := aEvent^.event_type;
      FEvent.element      := aEvent^.element;
      FEvent.event_number := aEvent^.event_number;
      FEvent.bind_id      := aEvent^.bind_id;
    end
   else
    begin
      FEvent.window       := 0;
      FEvent.event_type   := WEBUI_EVENT_DISCONNECTED;
      FEvent.element      := nil;
      FEvent.event_number := 0;
      FEvent.bind_id      := 0;
    end;

  if (FEvent.window <> 0) then
    FWindow := TWebUIWindow.Create(FEvent.window, False)
   else
    FWindow := nil;
end;

constructor TWebUIEventHandler.Create(window: TWebUIWindowID; event_type: TWebUIEventType; const element: PWebUIChar; event_number: TWebUIEventID; bind_id: TWebUIBindID);
begin
  inherited Create;

  FEvent.window       := window;
  FEvent.event_type   := event_type;
  FEvent.element      := element;
  FEvent.event_number := event_number;
  FEvent.bind_id      := bind_id;

  if (FEvent.window <> 0) then
    FWindow := TWebUIWindow.Create(FEvent.window, False)
   else
    FWindow := nil;
end;

destructor TWebUIEventHandler.Destroy;
begin
  if assigned(FWindow) then
    FreeAndNil(FWindow);

  inherited Destroy;
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
    Result := UTF8ToString(PAnsiChar(FEvent.element))
   else
    REsult := '';
end;

function TWebUIEventHandler.GetEventID: TWebUIEventID;
begin
  Result := FEvent.event_number;
end;

function TWebUIEventHandler.GetBindID: TWebUIBindID;
begin
  Result := FEvent.bind_id;
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

function TWebUIEventHandler.GetStringAt(index: NativeUInt): string;
begin
  if Initialized then
    Result := UTF8ToString(PAnsiChar(webui_get_string_at(@FEvent, index)))
   else
    Result := '';
end;

function TWebUIEventHandler.GetString: string;
begin
  if Initialized then
    Result := UTF8ToString(PAnsiChar(webui_get_string(@FEvent)))
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

procedure TWebUIEventHandler.ReturnString(const aReturnValue: string);
var
  LString: AnsiString;
begin
  if Initialized then
    begin
      if (length(aReturnValue) > 0) then
        begin
          LString := UTF8Encode(aReturnValue + #0);
          webui_return_string(@FEvent, @LString[1]);
        end
       else
        webui_return_string(@FEvent, nil);
    end;
end;

procedure TWebUIEventHandler.ReturnBool(aReturnValue: boolean);
begin
  if Initialized then
    webui_return_bool(@FEvent, aReturnValue);
end;

procedure TWebUIEventHandler.SetResponse(const response: string);
var
  LResponse: AnsiString;
begin
  if Initialized then
    begin
      if (length(response) > 0) then
        begin
          LResponse := UTF8Encode(response + #0);
          webui_interface_set_response(FEvent.window, FEvent.event_number, @LResponse[1]);
        end
       else
        webui_interface_set_response(FEvent.window, FEvent.event_number, nil);
    end;
end;

end.
