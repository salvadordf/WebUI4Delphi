unit uWebUIBase64;

{$I uWebUI.inc}     

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$IFNDEF TARGET_64BITS}{$ALIGN ON}{$ENDIF}
{$MINENUMSIZE 4}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.SysUtils,
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  /// <summary>
  /// Wrapper class for the Base64 conversion functions in WebUI.
  /// </summary>
  TWebUIBase64 = class
    public
      /// <summary>
      /// Encode text to Base64. The returned buffer need to be freed.
      /// </summary>
      /// <param name="str">The string to encode (Should be null terminated).</param>
      /// <returns>Returns the base64 encoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_encode)</see></para>
      /// </remarks>
      class function Encode(const str : PWebUIChar) : PWebUIChar; overload;
      /// <summary>
      /// Encode text to Base64. If it fails it will return an empty string.
      /// </summary>
      /// <param name="str">The string to encode.</param>
      /// <returns>Returns the base64 encoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_encode)</see></para>
      /// </remarks>
      class function Encode(const str : string) : string; overload;
      /// <summary>
      /// Decode a Base64 encoded text. The returned buffer need to be freed.
      /// </summary>
      /// <param name="str">The string to decode (Should be null terminated).</param>
      /// <returns>Returns the base64 decoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_decode)</see></para>
      /// </remarks>
      class function Decode(const str : PWebUIChar) : PWebUIChar; overload;
      /// <summary>
      /// Decode a Base64 encoded text. If it fails it will return an empty string.
      /// </summary>
      /// <param name="str">The string to decode.</param>
      /// <returns>Returns the base64 decoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_decode)</see></para>
      /// </remarks>
      class function Decode(const str : string) : string; overload;
  end;

implementation

uses
  uWebUI;

class function TWebUIBase64.Encode(const str : PWebUIChar) : PWebUIChar;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_encode(str)
   else
    Result := nil;
end;

class function TWebUIBase64.Encode(const str : string) : string;
var
  LString: AnsiString;
  LBuffer: PWebUIChar;
begin
  if (length(str) = 0) then
    Result := ''
   else
    begin
      LString := UTF8Encode(str + #0);
      LBuffer := Encode(@LString);

      if assigned(LBuffer) then
        try
          Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(LBuffer));
        finally
          webui_free(LBuffer);
        end;
    end;
end;

class function TWebUIBase64.Decode(const str : PWebUIChar) : PWebUIChar;
begin
  if (WebUI <> nil) and WebUI.Initialized then
    Result := webui_decode(str)
   else
    Result := nil;
end;

class function TWebUIBase64.Decode(const str : string) : string;
var
  LString: AnsiString;
  LBuffer: PWebUIChar;
begin
  if (length(str) = 0) then
    Result := ''
   else
    begin
      LString := UTF8Encode(str + #0);
      LBuffer := Decode(@LString);

      if assigned(LBuffer) then
        try
          Result := {$IFDEF DELPHI12_UP}UTF8ToString{$ELSE}UTF8Decode{$ENDIF}(PAnsiChar(LBuffer));
        finally
          webui_free(LBuffer);
        end;
    end;
end;

end.
