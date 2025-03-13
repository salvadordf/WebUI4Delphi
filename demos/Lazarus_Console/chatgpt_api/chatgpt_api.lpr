program chatgpt_api;

{$MODE Delphiunicode}

{$I ..\..\..\source\uWebUI.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uWebUI, uWebUIWindow, uWebUITypes, uWebUIConstants;

const
  USER_KEY       = 'sk-proj-xxx-xxxxxxxxxxxxxxxxxxxxxxx_xxx';
  USER_MODEL     = 'gpt-4o';
  USER_ASSISTANT = 'You are an assistant, answer with very short messages.';
  BUF_SIZE       = 1024;

var
  LWindow : IWebUIWindow;
  LMyHTML : string;
  LQuery : string;
  LAIResponse : string;

  function run_ai_query(user_query : string; var ai_response : string): boolean;
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
    Result := LWindow.Script(LScript, 30, ai_response, BUF_SIZE);
  end;

  begin
    try
      WebUI := TWebUI.Create;
      {$IFDEF DEBUG}
      //WebUI.LoaderDllPath := WEBUI_DEBUG_LIB;
      {$ENDIF}
      if WebUI.Initialize then
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
          LWindow := TWebUIWindow.Create;
          LWindow.SetHide(True);

          if LWindow.ShowBrowser(LMyHTML, ChromiumBased) then
            begin
              LQuery := 'What is the capital of Canada?';
              // Run HTTPS API
              if (run_ai_query(LQuery, LAIResponse)) then
                WriteLn('AI Response: ' + trim(LAIResponse))
               else
                WriteLn('Error: ' + trim(LAIResponse));
            end
           else
            WriteLn('There was an error showing the web browser.');

          WebUI.Exit;

          WriteLn('Press ENTER to exit.');
          ReadLn;
        end;
    except
      on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
    end;
  end.

