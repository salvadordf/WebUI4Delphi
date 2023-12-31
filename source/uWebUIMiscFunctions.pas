unit uWebUIMiscFunctions;

{$I uWebUI.inc}

interface

uses
  Winapi.Windows, System.Classes, System.UITypes, Winapi.ActiveX, System.SysUtils, System.Math, System.StrUtils,
  uWebUIConstants, uWebUITypes, uWebUILibFunctions;

const
  SHLWAPIDLL  = 'shlwapi.dll';

procedure OutputDebugMessage(const aMessage : string);
function  CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;

function CustomPathIsRelative(const aPath : string) : boolean;
function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
function CustomAbsolutePath(const aPath : string; aMustExist : boolean = False) : string;
function CustomPathIsURL(const aPath : string) : boolean;
function CustomPathIsUNC(const aPath : string) : boolean;
function GetModulePath : string;

function PathIsRelativeAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeA';
function PathIsRelativeUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsRelativeW';
function PathCanonicalizeAnsi(pszBuf: LPSTR; pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeA';
function PathCanonicalizeUnicode(pszBuf: LPWSTR; pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathCanonicalizeW';
function PathIsUNCAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCA';
function PathIsUNCUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsUNCW';
function PathIsURLAnsi(pszPath: LPCSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLA';
function PathIsURLUnicode(pszPath: LPCWSTR): BOOL; stdcall; external SHLWAPIDLL name 'PathIsURLW';

function ExecuteFile(const filename, Params, DefaultDir: string; ShowCmd: integer): THandle;

implementation

uses
  {$IFDEF MSWINDOWS}
  winapi.shellapi,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  uWebUI;

procedure OutputDebugMessage(const aMessage : string);
begin
  {$IFDEF DEBUG}
  OutputDebugString({$IFNDEF DELPHI16_UP}PAnsiChar{$ELSE}PWideChar{$ENDIF}(aMessage + #0));
  {$ENDIF}
end;

function CustomExceptionHandler(const aFunctionName : string; const aException : exception) : boolean;
begin
  OutputDebugMessage(aFunctionName + ' error : ' + aException.message);

  Result := (WebUI <> nil) and WebUI.ReRaiseExceptions;
end;

function CustomPathIsRelative(const aPath : string) : boolean;
begin
  Result := PathIsRelativeUnicode(PWideChar(aPath));
end;

function CustomPathIsURL(const aPath : string) : boolean;
begin
  Result := PathIsURLUnicode(PWideChar(aPath + #0));
end;

function CustomPathIsUNC(const aPath : string) : boolean;
begin
  Result := PathIsUNCUnicode(PWideChar(aPath + #0));
end;

function CustomPathCanonicalize(const aOriginalPath : string; var aCanonicalPath : string) : boolean;
var
  TempBuffer: array [0..pred(MAX_PATH)] of WideChar;
begin
  Result         := False;
  aCanonicalPath := '';

  if (length(aOriginalPath) > MAX_PATH) or
     (Copy(aOriginalPath, 1, 4) = '\\?\') or
     CustomPathIsUNC(aOriginalPath) then
    exit;

  FillChar(TempBuffer, MAX_PATH * SizeOf(WideChar), 0);

  if PathCanonicalizeUnicode(@TempBuffer[0], PWideChar(aOriginalPath + #0)) then
    begin
      aCanonicalPath := TempBuffer;
      Result         := True;
    end;
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
begin
  {$IFDEF FPC}
  Result := UTF8Decode(IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HINSTANCE))));
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(GetModuleName(HINSTANCE)));
  {$ENDIF}
end;

// https://stackoverflow.com/questions/43673143/firemonkey-application-launch-external-app-when-running-under-os-x
function ExecuteFile(const filename, Params, DefaultDir: string; ShowCmd: integer): THandle;
begin
  {$IFDEF MSWINDOWS}
    result := ShellExecute(0, 'Open', PChar(filename), PChar(Params), PChar(DefaultDir), ShowCmd);
  {$ENDIF}
  {$IFDEF MACOS}
    _system(PAnsiChar('open ' + AnsiString(filename)));
  {$ENDIF}
end;

end.
