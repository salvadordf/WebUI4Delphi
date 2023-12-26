object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Button1: TButton
    Left = 216
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Destroy'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 368
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Show'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 368
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Url'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 449
    Top = 257
    Width = 152
    Height = 23
    TabOrder = 4
    Text = 'Edit1'
  end
end
