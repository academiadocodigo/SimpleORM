object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 591
  ClientWidth = 969
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    969
    591)
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Insert Bind'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 89
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Update Object'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 89
    Top = 70
    Width = 97
    Height = 25
    Caption = 'Delete Object'
    TabOrder = 2
    OnClick = Button4Click
  end
  object btnFind: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Find'
    TabOrder = 3
    OnClick = btnFindClick
  end
  object Button6: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Caption = 'FindID'
    TabOrder = 4
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 8
    Top = 194
    Width = 75
    Height = 25
    Caption = 'FindWhere'
    TabOrder = 5
    OnClick = Button7Click
  end
  object Edit2: TEdit
    Left = 192
    Top = 14
    Width = 59
    Height = 21
    TabOrder = 6
  end
  object Edit1: TEdit
    Left = 192
    Top = 41
    Width = 241
    Height = 21
    TabOrder = 7
  end
  object DBGrid1: TDBGrid
    Left = 192
    Top = 68
    Width = 549
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = UniDataSource1
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 192
    Top = 356
    Width = 549
    Height = 66
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 9
  end
  object Edit3: TEdit
    Left = 439
    Top = 41
    Width = 121
    Height = 21
    TabOrder = 10
  end
  object Button2: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'FindObject'
    TabOrder = 11
    OnClick = Button2Click
  end
  object DateTimePicker1: TDateTimePicker
    Left = 566
    Top = 41
    Width = 175
    Height = 21
    Date = 43549.000000000000000000
    Time = 0.636917974537937000
    TabOrder = 12
  end
  object Button5: TButton
    Left = 89
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Insert Object'
    TabOrder = 13
    OnClick = Button5Click
  end
  object Button8: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Update Bind'
    TabOrder = 14
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Delete Bind'
    TabOrder = 15
    OnClick = Button9Click
  end
  object UniDataSource1: TUniDataSource
    Left = 688
    Top = 88
  end
  object UniConnection1: TUniConnection
    ProviderName = 'InterBase'
    Port = 3051
    Database = '/firebird/data/PDVUPDATES.FDB'
    Username = 'SYSDBA'
    Server = 'localhost'
    Connected = True
    LoginPrompt = False
    Left = 616
    Top = 88
    EncryptedPassword = 
      'CEFFC7FFCBFF9CFFC6FFC9FF9BFFC8FF9DFFCDFFC8FF9CFFC9FF99FFC8FF9BFF' +
      'C9FF9DFF9EFF9EFF'
  end
  object InterBaseUniProvider1: TInterBaseUniProvider
    Left = 784
    Top = 88
  end
end
