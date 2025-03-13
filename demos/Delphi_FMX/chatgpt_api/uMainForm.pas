unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIEventHandler, uWebUILibFunctions,
  uWebUIConstants;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    AskChatGPTBtn: TButton;
    AnswerMem: TMemo;
    QuestionMem: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AskChatGPTBtnClick(Sender: TObject);
  private
    FWindow : IWebUIWindow;
    function run_ai_query(user_query : string; var ai_response : string): boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  USER_KEY       = 'sk-proj-xxx-xxxxxxxxxxxxxxxxxxxxxxx_xxx';
  USER_MODEL     = 'gpt-4o';
  USER_ASSISTANT = 'You are an assistant, answer with very short messages.';
  BUF_SIZE       = 1024;

function TMainForm.run_ai_query(user_query : string; var ai_response : string): boolean;
var
  LScript : string;
begin
  // Generate JavaScript
  LScript := 'return run_gpt_api(' +
                quotedstr(USER_KEY) + ', ' +
                quotedstr(USER_MODEL) + ', ' +
                quotedstr(USER_ASSISTANT) + ', ' +
                quotedstr(user_query) + ');';

  // Run HTTPS API
  Result := FWindow.Script(LScript, 30, ai_response, BUF_SIZE);
end;

procedure TMainForm.AskChatGPTBtnClick(Sender: TObject);
var
  LMyHTML, LAIResponse : string;
begin
  LMyHTML := '<!DOCTYPE html>' + CRLF +
             '<html>' + CRLF +
             '  <head><script src="webui.js"></script></head>' + CRLF +
             '  <body>' + CRLF +
             '    <script>' + CRLF +
             '      function run_gpt_api(userKey, userModel, userAssistant, userContent) {' + CRLF +
             '        const xhr = new XMLHttpRequest();' + CRLF +
             '        xhr.open("POST", "https://api.openai.com/v1/chat/completions", false);' + CRLF +
             '        xhr.setRequestHeader("Content-Type", "application/json");' + CRLF +
             '        xhr.setRequestHeader("Authorization", "Bearer " + userKey);' + CRLF +
             '        xhr.send(JSON.stringify({' + CRLF +
             '          model: userModel,' + CRLF +
             '          messages: [' + CRLF +
             '            { role: "developer", content: userAssistant },' + CRLF +
             '            { role: "user", content: userContent }' + CRLF +
             '          ]' + CRLF +
             '        }));' + CRLF +
             '        const responseJson = JSON.parse(xhr.responseText);' + CRLF +
             '        if (responseJson.error) {' + CRLF +
             '          return ' + quotedstr('Error: ') + ' + responseJson.error.message;' + CRLF +
             '        }' + CRLF +
             '        return (responseJson.choices[0].message.content).trim();' + CRLF +
             '      }' + CRLF +
             '    </script>' + CRLF +
             '  </body>' + CRLF +
             '</html>';

  // Start WebUI server
  FWindow := TWebUIWindow.Create;
  FWindow.SetHide(True);

  if FWindow.ShowBrowser(LMyHTML, ChromiumBased) then
    begin
      // Run HTTPS API
      if (run_ai_query(trim(QuestionMem.Lines.Text), LAIResponse)) then
        AnswerMem.Lines.Add('AI Response: ' + trim(LAIResponse))
       else
        AnswerMem.Lines.Add('Error: ' + trim(LAIResponse));
    end
   else
    AnswerMem.Lines.Add('There was an error showing the web browser.');
end;

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
  if WebUI.Initialize then
    MainPanel.Enabled := True
   else
    begin
      MainPanel.Enabled := False;
      ShowMessage(WebUI.ErrorMessage);
    end;
end;

end.
