object Form9: TForm9
  Left = 0
  Top = 0
  Caption = 'Form9'
  ClientHeight = 342
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button3: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 0
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button4: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Find'
    TabOrder = 3
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'FindID'
    TabOrder = 4
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Caption = 'FindWhere'
    TabOrder = 5
    OnClick = Button7Click
  end
  object Edit2: TEdit
    Left = 112
    Top = 14
    Width = 59
    Height = 21
    TabOrder = 6
    Text = '1'
  end
  object Edit1: TEdit
    Left = 112
    Top = 41
    Width = 241
    Height = 21
    TabOrder = 7
    Text = 'Edit1'
  end
  object DBGrid1: TDBGrid
    Left = 112
    Top = 68
    Width = 489
    Height = 120
    DataSource = DataSource1
    TabOrder = 8
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Memo1: TMemo
    Left = 112
    Top = 194
    Width = 489
    Height = 128
    Lines.Strings = (
      'Memo1')
    TabOrder = 9
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 568
    Top = 8
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\Bancos\Firebird\PDVUPDATES.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    LoginPrompt = False
    Left = 520
    Top = 8
  end
end
