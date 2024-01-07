unit uMainForm;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
  private
    FWindow : IWebUIWindow;
    procedure FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler); 
    procedure CloseProc(Data: PtrInt);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

uses
  uWebUIMiscFunctions;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow  := nil;
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

procedure TMainForm.CloseProc(Data: PtrInt);
begin
  Close;
end;

procedure TMainForm.FWindow_OnWebUIEvent(Sender: TObject; const aEvent: IWebUIEventHandler);
begin
  WebUI.Exit;
  // You can't create more browsers after the exit call
  Application.QueueAsyncCall(CloseProc, 0);
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  FWindow := TWebUIWindow.Create;

  // Set the web-server root folder for the first window
  {$IFDEF MSWINDOWS}
  FWindow.SetRootFolder(CustomAbsolutePath('..\assets\text_editor\', True));
  {$ELSE}            
  FWindow.SetRootFolder(CustomAbsolutePath('../assets/text_editor/', True));
  {$ENDIF}

  // Bind HTML elements with the specified ID to C functions
  FWindow.Bind('__close-btn');
  FWindow.OnWebUIEvent := FWindow_OnWebUIEvent;

  // Show the window, preferably in a chromium based browser
  if not(FWindow.ShowBrowser('index.html', ChromiumBased)) then
    FWindow.Show('index.html');
end;

end.

