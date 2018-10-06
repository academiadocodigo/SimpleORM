object Form8: TForm8
  Left = 0
  Top = 0
  Caption = 'Form8'
  ClientHeight = 489
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 105
    Top = 95
    Width = 457
    Height = 201
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Find'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 105
    Top = 302
    Width = 457
    Height = 169
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 105
    Top = 56
    Width = 280
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 105
    Top = 29
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '1'
  end
  object Button3: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'FindID'
    TabOrder = 8
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Caption = 'FindWhere'
    TabOrder = 9
    OnClick = Button6Click
  end
  object RESTDWClientSQL1: TRESTDWClientSQL
    FieldDefs = <>
    IndexDefs = <>
    MasterFields = ''
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    ResourceOptions.AssignedValues = [rvSilentMode]
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvCheckRequired, uvAutoCommitUpdates]
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    MasterCascadeDelete = True
    Datapacks = -1
    DataCache = False
    Params = <>
    DataBase = RESTDWDataBase1
    CacheUpdateRecords = False
    AutoCommitData = False
    AutoRefreshAfterCommit = False
    Left = 312
    Top = 144
  end
  object RESTDWDataBase1: TRESTDWDataBase
    Active = False
    Compression = True
    MyIP = '0.0.0.0'
    Login = 'testserver'
    Password = 'testserver'
    Proxy = False
    ProxyOptions.Port = 8888
    PoolerService = '127.0.0.1'
    PoolerPort = 8082
    PoolerName = 'TServerMethodDM.RESTDWPoolerDB1'
    StateConnection.AutoCheck = False
    StateConnection.InTime = 1000
    RequestTimeOut = 10000
    EncodeStrings = True
    Encoding = esUtf8
    StrsTrim = False
    StrsEmpty2Null = False
    StrsTrim2Len = True
    ParamCreate = True
    ClientConnectionDefs.Active = False
    Left = 392
    Top = 152
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 504
    Top = 104
  end
end
