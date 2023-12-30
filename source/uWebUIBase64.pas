unit uWebUIBase64;

{$I uWebUI.inc}

{$MINENUMSIZE 4}

interface

uses
  WinApi.Windows, System.Classes, System.SysUtils,
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

type
  /// <summary>
  /// Wrapper class for the Base64 conversion functions in WebUI.
  /// </summary>
  TWebUIBase64 = class
    public
      /// <summary>
      /// Base64 encoding. Use this to safely send text based data to the UI. If it fails it will return NULL.
      /// </summary>
      /// <param name="str">The string to encode (Should be null terminated).</param>
      /// <returns>Returns a encoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_encode)</see></para>
      /// </remarks>
      class function Encode(const str : PWebUIChar) : PWebUIChar; overload;
      /// <summary>
      /// Base64 encoding. Use this to safely send text based data to the UI. If it fails it will return an empty string.
      /// </summary>
      /// <param name="str">The string to encode.</param>
      /// <returns>Returns a encoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_encode)</see></para>
      /// </remarks>
      class function Encode(const str : string) : string; overload;
      /// <summary>
      /// Base64 decoding. Use this to safely decode received Base64 text from the UI. If it fails it will return NULL.
      /// </summary>
      /// <param name="str">The string to decode (Should be null terminated).</param>
      /// <returns>Returns a decoded string.</returns>
      /// <remarks>
      /// <para><see href="https://github.com/webui-dev/webui/blob/main/include/webui.h">WebUI source file: /include/webui.h (webui_decode)</see></para>
      /// </remarks>
      class function Decode(const str : PWebUIChar) : PWebUIChar; overload;
      /// <summary>
      /// Base64 decoding. Use this to safely decode received Base64 text from the UI. If it fails it will return an empty string.
      /// </summary>
      /// <param name="str">The string to decode.</param>
      /// <returns>Returns a decoded string.</returns>
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
begin
  if (length(str) = 0) then
    Result := ''
   else
    begin
      LString := UTF8Encode(str + #0);
      Result  := UTF8ToString(PAnsiChar(Encode(@LString)));
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
begin
  if (length(str) = 0) then
    Result := ''
   else
    begin
      LString := UTF8Encode(str + #0);
      Result  := UTF8ToString(PAnsiChar(Decode(@LString)));
    end;
end;

end.
