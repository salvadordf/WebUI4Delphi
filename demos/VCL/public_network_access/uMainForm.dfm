object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'public_network_access'
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
      Top = 5
      Width = 420
      Height = 25
      Align = alTop
      Caption = '1. Show browser'
      TabOrder = 0
      OnClick = ShowBrowserBtnClick
      ExplicitTop = 6
    end
    object Memo1: TMemo
      Left = 5
      Top = 70
      Width = 420
      Height = 180
      Align = alBottom
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      ExplicitTop = 71
    end
    object OpenDefBrowserBtn: TButton
      Left = 5
      Top = 37
      Width = 420
      Height = 25
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = '2. Open default web browser'
      Enabled = False
      TabOrder = 2
      OnClick = OpenDefBrowserBtnClick
    end
  end
end
