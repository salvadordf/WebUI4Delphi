object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'chatgpt_api'
  ClientHeight = 281
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Padding.Left = 10
  Padding.Top = 10
  Padding.Right = 10
  Padding.Bottom = 10
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object MainPanel: TPanel
    Left = 10
    Top = 10
    Width = 864
    Height = 261
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 240
    ExplicitTop = 40
    ExplicitWidth = 185
    ExplicitHeight = 41
    object AskChatGPTBtn: TButton
      Left = 0
      Top = 0
      Width = 864
      Height = 22
      Align = alTop
      Caption = 'Ask ChatGPT'
      TabOrder = 0
      OnClick = AskChatGPTBtnClick
      ExplicitTop = -6
    end
    object QuestionMem: TMemo
      Left = 0
      Top = 22
      Width = 864
      Height = 131
      Align = alClient
      Lines.Strings = (
        'What is the capital of Canada?')
      ScrollBars = ssBoth
      TabOrder = 1
      ExplicitTop = 16
    end
    object AnswerMem: TMemo
      Left = 0
      Top = 153
      Width = 864
      Height = 108
      Align = alBottom
      ScrollBars = ssBoth
      TabOrder = 2
      ExplicitTop = 159
    end
  end
end
