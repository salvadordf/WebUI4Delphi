object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'text_editor'
  ClientHeight = 56
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
    Height = 36
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    ExplicitHeight = 255
    object ShowBrowserBtn: TButton
      Left = 5
      Top = 5
      Width = 420
      Height = 26
      Align = alClient
      Caption = 'Show browser'
      TabOrder = 0
      OnClick = ShowBrowserBtnClick
      ExplicitLeft = 0
      ExplicitTop = 3
      ExplicitHeight = 25
    end
  end
end
