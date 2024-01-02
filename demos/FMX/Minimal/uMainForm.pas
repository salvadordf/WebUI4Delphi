unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    ShowBrowserBtn: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ShowBrowserBtnClick(Sender: TObject);
  private
    FWindow : IWebUIWindow;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if assigned(WebUI) and WebUI.IsAppRunning then
    WebUI.Exit;

  FWindow  := nil;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
{$IFDEF MACOS}
const
  MAC_APP_POSTFIX = '.app/';
  MAC_APP_SUBPATH = 'Contents/MacOS/';
var
  LPath : string;
{$ENDIF}
begin
  WebUI := TWebUI.Create;
  {$IFDEF DEBUG}
  //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
  {$ENDIF}
  FWindow := nil;
  {$IFDEF MACOS}
  // Copy "webui-2.dylib" to the "Minimal.app/Contents/Frameworks/" directory before running the demo.
  LPath := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  if LPath.Contains(MAC_APP_POSTFIX + MAC_APP_SUBPATH) then
    LPath := LPath.Remove(LPath.IndexOf(MAC_APP_SUBPATH));
  WebUI.LoaderDllPath := LPath + 'Contents/Frameworks/' + WEBUI_LIB;
  {$ENDIF}
  MainPanel.Enabled := WebUI.Initialize;
end;

procedure TMainForm.ShowBrowserBtnClick(Sender: TObject);
var
  LMyHTML : string;
begin
  if assigned(WebUI) and WebUI.IsAppRunning then exit;

  LMyHTML := '<html><head><script src="webui.js"></script></head> Hello World ! </html>';
  FWindow := TWebUIWindow.Create;
  FWindow.Show(LMyHTML);
end;

end.
