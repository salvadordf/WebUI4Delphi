unit uMainForm;

{$I ..\..\..\source\uWebUI.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FWindow : IWebUIWindow;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
    procedure exit_app(const aEvent: IWebUIEventHandler);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uWebUIMiscFunctions;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow := nil;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LibraryPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;
  MainPanel.Enabled := WebUI.Initialize;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  if (aEvent.Element = 'Exit') then
    exit_app(aEvent);
end;

procedure TMainForm.exit_app(const aEvent: IWebUIEventHandler);
begin
  WebUI.Exit;
  // You can't create more browsers after the exit call
  PostMessage(Handle, WM_CLOSE, 0, 0);
end;

function vfs(const filename: PWebUIChar; len: PInteger): Pointer; cdecl;
var
  LFilename, LPath : string;
  LResultStream : TMemoryStream;
  LFileStream : TMemoryStream;
  LFileLength : integer;
  LTotalLength : integer;
  LHTTPHeaderTemplate : AnsiString;
  LHTTPHeaderLength : integer;
  LHTTPHeader, LContent : string;
  LNullChar: AnsiString;
  LNullCharLength : integer;
begin
  len^          := 0;
  Result        := nil;
  LFileStream   := nil;
  LResultStream := nil;
  LFilename     := UTF8ToString(PAnsiChar(filename));

  try
    // Set the web-server root folder for the first window
    {$IFDEF MSWINDOWS}
    LPath     := CustomAbsolutePath('..\assets\virtual_file_system\', True);
    LFilename := StringReplace(LFilename, '/', '\', [rfReplaceAll]);
    if (pos('\', LFilename) = 1) then
      LFilename := copy(LFilename, 2, length(LFilename));
    {$ELSE}
    LPath := CustomAbsolutePath('../assets/virtual_file_system/', True);
    if (pos('/', LFilename) = 1) then
      LFilename := copy(LFilename, 2, length(LFilename));
    {$ENDIF}
    LPath := LPath + LFilename;

    // This function reads files in the drive but you can get the contents from
    // any other source like resources.
    try
      LResultStream := TMemoryStream.Create;

      if FileExists(LPath) then
        begin
          LFileStream := TMemoryStream.Create;
          LFileStream.LoadFromFile(LPath);
          LFileLength := LFileStream.Size;
          LFileStream.Position := 0;

          LHTTPHeader := 'HTTP/1.1 200 OK' + CRLF +
                         'Content-Type: ' + WebUI.GetMimeType(LFilename) + CRLF +
                         'Content-Length: ' + inttostr(LFileLength) + CRLF +
                         'Cache-Control: no-cache' + CRLF + CRLF;
          LHTTPHeaderTemplate := UTF8Encode(LHTTPHeader);
          LHTTPHeaderLength   := length(LHTTPHeaderTemplate);

          LNullChar       := AnsiChar(#0);
          LNullCharLength := length(LNullChar);

          LResultStream.Write(LHTTPHeaderTemplate[1], LHTTPHeaderLength);
          LResultStream.Write(LFileStream.Memory^, LFileLength);
          LResultStream.Write(LNullChar[1], LNullCharLength);
          LResultStream.Position := 0;

          LTotalLength := LHTTPHeaderLength + LFileLength + LNullCharLength;
          Result       := webui_malloc(LTotalLength);
          len^         := LResultStream.Read(Result^, LTotalLength);
        end
       else
        if DirectoryExists(LPath) then
          begin
            // Redirect requests to directories to the index.html file.

            LHTTPHeader := 'HTTP/1.1 302 Found' + CRLF +
                           'Location: ' + LFilename + '/index.html' + CRLF +
                           'Cache-Control: no-cache' + CRLF + CRLF + #0;


            LHTTPHeaderTemplate := UTF8Encode(LHTTPHeader);
            Result              := StringToPWebUIChar(LHTTPHeaderTemplate, len^);
          end
         else
          begin
            LContent := '<html><head><title>Resource Not Found</title></head>' +
                        '<body><p>The resource you requested has not been found ' +
                        'at the specified address. Please check the spelling of ' +
                        'the address.</p></body></html>';

            LHTTPHeader := 'HTTP/1.1 404 Not Found' + CRLF +
                           'Content-Type: text/html' + CRLF +
                           'Content-Length: ' + inttostr(length(LContent)) + CRLF +
                           'Cache-Control: no-cache' + CRLF + CRLF +
                           LContent + #0;

            LHTTPHeaderTemplate := UTF8Encode(LHTTPHeader);
            Result              := StringToPWebUIChar(LHTTPHeaderTemplate, len^);
          end;
    finally
      if assigned(LFileStream) then
        FreeAndNil(LFileStream);

      if assigned(LResultStream) then
        FreeAndNil(LResultStream);
    end;
  except
    on e : exception do
      if CustomExceptionHandler('vfs', e) then raise;
  end;
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  FWindow := TWebUIWindow.Create;

  // Bind HTML element IDs with a C functions
  FWindow.Bind('Exit');
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;

  // Set a custom files handler
  FWindow.SetFileHandler(vfs);

  // Show a new window
  FWindow.Show('index.html');
end;

end.
