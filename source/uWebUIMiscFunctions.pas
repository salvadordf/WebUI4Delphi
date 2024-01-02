unit uWebUIMiscFunctions;

{$I uWebUI.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows,{$ENDIF} System.Classes, System.UITypes,
    System.SysUtils, System.Math, System.StrUtils,
    {$IFDEF FMX}FMX.Types, FMX.Platform,{$ENDIF}
  {$ELSE}
    Windows, Classes, SysUtils, Math, StrUtils,
  {$ENDIF}
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

const
  SHLWAPIDLL  = 'shlwapi.dll';

procedure OutputDebugMessage(const aMessage : string);
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
function ExecuteFile(const filename, Params, DefaultDir: string; ShowCmd: integer): THandle;

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
    {$IFDEF DELPHI12_UP}
    Result := PathIsRelativeUnicode(PChar(aPath));
    {$ELSE}
    Result := PathIsRelativeAnsi(PChar(aPath));
    {$ENDIF}
  {$ELSE}
  Result := (length(aPath) > 0) and (aPath[1] <> '/');
  {$ENDIF}
end;

function CustomPathIsURL(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    Result := PathIsURLUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsURLAnsi(PChar(aPath + #0));
    {$ENDIF}
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function CustomPathIsUNC(const aPath : string) : boolean;
begin
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI12_UP}
    Result := PathIsUNCUnicode(PChar(aPath + #0));
    {$ELSE}
    Result := PathIsUNCAnsi(PChar(aPath + #0));
    {$ENDIF}
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
    {$IFDEF DELPHI12_UP}
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
    {$ENDIF}
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
    result := ShellExecute(0, 'Open', PChar(filename), PChar(Params), PChar(DefaultDir), ShowCmd);
  {$ENDIF}

  {$IFDEF MACOSX}
    _system(PAnsiChar('open ' + AnsiString(filename)));
  {$ENDIF}

  {$IFDEF LINUX}
     // TO-DO: Find a way to execute a program in Linux
  {$ENDIF}
end;

end.
