unit uWebUIMiscFunctions;

{$I uWebUI.inc}    

{$IFDEF FPC}
  {$MODE delphiunicode}
{$ENDIF}

{$IFNDEF DELPHI12_UP}
  // Workaround for "Internal error" in old Delphi versions caused by uint64 handling
  {$R-}
{$ENDIF}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.UITypes,
    System.SysUtils, System.Math, System.StrUtils, System.Types, System.IOUtils,
    {$IFDEF FMX}FMX.Types, FMX.Platform,{$ENDIF}
    {$IFDEF MACOS}
    FMX.Helpers.Mac, System.Messaging, Macapi.CoreFoundation, Macapi.Foundation,
    {$ENDIF}
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, Math, StrUtils,
    {$IFDEF FPC}LCLType, LazFileUtils, fileutil,{$ENDIF}
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

const
  SHLWAPIDLL  = 'shlwapi.dll';

/// <summary>
/// Sends a string to the debugger for display.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-outputdebugstringw">See the OutputDebugStringW article.</see></para>
/// </remarks>
procedure OutputDebugMessage(const aMessage : string);
/// <summary>
/// Sends the function and exception names to the debugger for display and raises the exception.
/// </summary>
/// <param name="aFunctionName">Function that registered the exception.</param>
/// <param name="aException">Exception information.</param>
/// <returns>Returns true if the exception was raised.</returns>
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
/// <summary>
/// Returns true if aPath is a relative path.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisrelativew">See the PathIsRelativeW article.</see></para>
/// </remarks>
function CustomPathIsRelative(const aPath : string) : boolean;
/// <summary>
/// Simplifies a path by removing navigation elements such as "." and ".." to produce a direct, well-formed path.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathcanonicalizew">See the PathCanonicalizeW article.</see></para>
/// </remarks>
function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
/// <summary>
/// Returns the absolute path version of aPath.
/// </summary>
function CustomAbsolutePath(const aPath : string; aMustExist : boolean = False) : string;
/// <summary>
/// Tests aPath to determine if it conforms to a valid URL format.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisurlw">See the PathIsURLW article.</see></para>
/// </remarks>
function CustomPathIsURL(const aPath : string) : boolean;
/// <summary>
/// Determines if aPath is a valid Universal Naming Convention (UNC) path, as opposed to a path based on a drive letter.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shlwapi/nf-shlwapi-pathisuncw">See the PathIsUNCW article.</see></para>
/// </remarks>
function CustomPathIsUNC(const aPath : string) : boolean;
/// <summary>
/// Retrieves the fully qualified path for the current module.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/libloaderapi/nf-libloaderapi-getmodulefilenamew">See the GetModuleFileNameW article.</see></para>
/// </remarks>
function GetModulePath : string;
/// <summary>
/// Performs an operation on a specified file.
/// </summary>
/// <remarks>
/// <para><see href="https://learn.microsoft.com/en-us/windows/win32/api/shellapi/nf-shellapi-shellexecutew">See the ShellExecuteW article.</see></para>
/// </remarks>
function ExecuteFile(const filename, Params, DefaultDir: string; ShowCmd: integer): THandle;
{$IFDEF MACOSX}
/// <summary>
/// Copies the WebUI framework from the source path to the destination path.
/// </summary>
/// <param name="aSrcPath">Function that registered the exception.</param>
/// <param name="aDstPath">Exception information.</param>
/// <returns>Returns true if the framwork was copied successfully.</returns>
function CopyWebUIFramework(const aSrcPath, aDstPath: string): boolean;
{$ENDIF}
/// <summary>
/// Checks that the WebUI library exists.
/// </summary>
/// <param name="aPath">Path to the WebUI library.</param>
/// <returns>Returns true if it exists.</returns>
function LibraryExists(const aPath : string) : boolean;
/// <summary>
/// Converts a unicode string to a WebUI string.
/// This function should only be used by the file handler callback.
/// By allocating resources using webui_malloc() WebUI will automaticaly free the resources.
/// </summary>
/// <param name="aSrcString">The original unicode string.</param>
/// <param name="aRsltLength">The length of the result string.</param>
function StringToPWebUIChar(const aSrcString: string; var aRsltLength: integer): PWebUIChar;

{$IFDEF MSWINDOWS}
function PathIsRelativeAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeA';
function PathIsRelativeUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeW';
function PathCanonicalizeAnsi(pszBuf: LPSTR; pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeA';
function PathCanonicalizeUnicode(pszBuf: LPWSTR; pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeW';
function PathIsUNCAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCA';
function PathIsUNCUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCW';
function PathIsURLAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLA';
function PathIsURLUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLW';
{$ENDIF}


implementation

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI16_UP}
      WinApi.shellapi,
    {$ELSE}
      shellapi,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  uWebUI;

procedure OutputDebugMessage(const aMessage : string);
begin
  {$IFDEF DEBUG}
    {$IFDEF MSWINDOWS}
      {$IFDEF FMX}
        FMX.Types.Log.d(aMessage);
      {$ELSE}
        OutputDebugString({$IFDEF DELPHI12_UP}PWideChar{$ELSE}PAnsiChar{$ENDIF}(aMessage + chr(0)));
      {$ENDIF}
    {$ENDIF}

    {$IFDEF LINUX}
      {$IFDEF FPC}
        // TO-DO: Find a way to write in the error console using Lazarus in Linux
      {$ELSE}
        FMX.Types.Log.d(aMessage);
      {$ENDIF}
    {$ENDIF}
    {$IFDEF MACOSX}
      {$IFDEF FPC}
        // TO-DO: Find a way to write in the error console using Lazarus in MacOS
      {$ELSE}
        FMX.Types.Log.d(aMessage);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
begin
  OutputDebugMessage(aFunctionName + ' error : ' + aException.message);

  Result := (WebUI <> nil) and WebUI.ReRaiseExceptions;
end;

function CustomPathIsRelative(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IF DEFINED(DELPHI12_UP) OR DEFINED(FPC)}
    Result := PathIsRelativeUnicode(PChar(aPath));
    {$ELSE}
    Result := PathIsRelativeAnsi(PChar(aPath));
    {$IFEND}
  {$ELSE}
  Result := (length(aPath) > 0) and (aPath[1] <> '/');
  {$ENDIF}
end;

function CustomPathIsURL(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IF DEFINED(DELPHI12_UP) OR DEFINED(FPC)}
    Result := PathIsURLUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsURLAnsi(PChar(aPath + #0));
    {$IFEND}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function CustomPathIsUNC(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IF DEFINED(DELPHI12_UP) OR DEFINED(FPC)}
    Result := PathIsUNCUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsUNCAnsi(PChar(aPath + #0));
    {$IFEND}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
var
  TempBuffer: array [0..pred(MAX_PATH)] of Char;
begin
  Result         := False;
  aCanonicalPath := '';

  if (length(aOriginalPath) > MAX_PATH) or
     (Copy(aOriginalPath, 1, 4) = '\\?\') or
     CustomPathIsUNC(aOriginalPath) then
    exit;

  FillChar(TempBuffer, MAX_PATH * SizeOf(Char), 0);

  {$IFDEF MSWINDOWS}
    {$IF DEFINED(DELPHI12_UP) OR DEFINED(FPC)}
    if PathCanonicalizeUnicode(@TempBuffer[0], PChar(aOriginalPath + #0)) then
      begin
        aCanonicalPath := TempBuffer;
        Result         := True;
      end;
    {$ELSE}
    if PathCanonicalizeAnsi(@TempBuffer[0], PChar(aOriginalPath + #0)) then
      begin
        aCanonicalPath := TempBuffer;
        Result         := True;
      end;
    {$IFEND}
  {$ENDIF}
end;

function CustomAbsolutePath(const aPath : string; aMustExist : boolean) : string;
var
  TempNewPath, TempOldPath : string;
begin
  if (length(aPath) > 0) then
    begin
      if CustomPathIsRelative(aPath) then
        TempOldPath := GetModulePath + aPath
       else
        TempOldPath := aPath;

      if not(CustomPathCanonicalize(TempOldPath, TempNewPath)) then
        TempNewPath := TempOldPath;

      if aMustExist and not(DirectoryExists(TempNewPath)) then
        Result := ''
       else
        Result := TempNewPath;
    end
   else
    Result := '';
end;

function GetModulePath : string;
{$IFDEF MACOSX}
const
  MAC_APP_POSTFIX = '.app/';
  MAC_APP_SUBPATH = 'Contents/MacOS/';
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HINSTANCE{$IFDEF FPC}(){$ENDIF})));
  {$ENDIF}

  {$IFDEF LINUX}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  {$ENDIF}

  {$IFDEF MACOSX}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));

  {$IFDEF FPC}
  if copy(Result, Length(Result) + 1 - Length(MAC_APP_POSTFIX) - Length(MAC_APP_SUBPATH)) = MAC_APP_POSTFIX + MAC_APP_SUBPATH then
    SetLength(Result, Length(Result) - Length(MAC_APP_SUBPATH));

  Result := CreateAbsolutePath(Result, GetCurrentDirUTF8);
  {$ELSE}
  if Result.Contains(MAC_APP_POSTFIX + MAC_APP_SUBPATH) then
    Result := Result.Remove(Result.IndexOf(MAC_APP_SUBPATH));
  {$ENDIF}
  {$ENDIF}
end;

// https://stackoverflow.com/questions/43673143/firemonkey-application-launch-external-app-when-running-under-os-x
function ExecuteFile(const filename, Params, DefaultDir: string; ShowCmd: integer): THandle;
begin
  {$IFDEF MSWINDOWS}
    Result := ShellExecute(0, 'Open', PChar(filename), PChar(Params), PChar(DefaultDir), ShowCmd);
  {$ENDIF}

  {$IFDEF MACOSX}
    {$IFDEF FPC}
    // TO-DO: Find a way to execute commands using Lazarus in macOS
    {$ELSE}
    Result := _system(PAnsiChar('open ' + AnsiString(filename)));
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    // TO-DO: Find a way to execute a program in Linux
    Result := 0;
  {$ENDIF}
end;

{$IFDEF MACOSX}
function CopyWebUIFramework(const aSrcPath, aDstPath: string): boolean;  
var
  LDir: string;
begin
  Result := False;

  try
    if FileExists(aSrcPath) then
      begin          
        LDir := ExtractFileDir(aDstPath);
        {$IFDEF FPC}
        if not(DirectoryExists(LDir)) then
          CreateDir(LDir);

        CopyFile(aSrcPath, aDstPath, False);
        {$ELSE}
        if not(TDirectory.Exists(LDir)) then
          TDirectory.CreateDirectory(LDir);

        TFile.Copy(aSrcPath, aDstPath);
        TFile.SetAttributes(aDstPath, TFile.GetAttributes(aSrcPath));
        {$ENDIF}
        Result := True;
      end;
  except
    on e : exception do
      if CustomExceptionHandler('CopyWebUIFramework', e) then raise;
  end;
end;
{$ENDIF}

function LibraryExists(const aPath : string) : boolean;
begin
  Result := False;
  try
    if FileExists(aPath) then
      Result := True
     else
      begin
        {$IFDEF MACOSX}{$IFDEF DEBUG}
        Result := CopyWebUIFramework(IncludeTrailingPathDelimiter(GetModulePath) + WEBUI_LIB, aPath);
        {$ENDIF}{$ENDIF}
      end;
  except
    on e : exception do
      if CustomExceptionHandler('LibraryExists', e) then raise;
  end;
end;

function StringToPWebUIChar(const aSrcString: string; var aRsltLength: integer): PWebUIChar;
var
  LRsltString : AnsiString;
  LRsltBuffer : PWebUIChar;
begin
  LRsltString := UTF8Encode(aSrcString + #0);
  aRsltLength := length(LRsltString);
  LRsltBuffer := webui_malloc(aRsltLength);
  {$IFDEF MSWINDOWS}
  CopyMemory(LRsltBuffer, @LRsltString[1], aRsltLength);
  {$ELSE}
  Move(LRsltString[1], LRsltBuffer^, aRsltLength);
  {$ENDIF}
  Result      := LRsltBuffer;
end;

end.
