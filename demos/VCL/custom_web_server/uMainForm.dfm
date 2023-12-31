object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'custom_web_server'
  ClientHeight = 275
  ClientWidth = 450
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
    Width = 430
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    DesignSize = (
      430
      255)
    object ShowBrowserBtn: TButton
      Left = 5
      Top = 36
      Width = 420
      Height = 25
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = '2. Show browser'
      Enabled = False
      TabOrder = 0
      OnClick = ShowBrowserBtnClick
    end
    object Memo1: TMemo
      Left = 5
      Top = 68
      Width = 420
      Height = 182
      Align = alBottom
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      ExplicitTop = 67
    end
    object PythonBtn: TButton
      Left = 5
      Top = 5
      Width = 420
      Height = 25
      Align = alTop
      Caption = '1. Run python script'
      TabOrder = 2
      OnClick = PythonBtnClick
      ExplicitLeft = 0
      ExplicitTop = 14
    end
  end
end
