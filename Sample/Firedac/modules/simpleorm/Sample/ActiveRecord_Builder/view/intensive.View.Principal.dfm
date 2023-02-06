object frmListarClientes: TfrmListarClientes
  Left = 0
  Top = 0
  Caption = 'Listar Clientes'
  ClientHeight = 287
  ClientWidth = 578
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 247
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    object grdPessoa: TDBGrid
      Left = 5
      Top = 5
      Width = 568
      Height = 108
      Align = alTop
      DataSource = dsCliente
      Options = [dgTitles, dgColLines, dgRowLines, dgRowSelect]
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object grdEndereco: TDBGrid
      Left = 5
      Top = 122
      Width = 568
      Height = 120
      Align = alBottom
      DataSource = dsEndereco
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
    end
    object Panel3: TPanel
      Left = 5
      Top = 113
      Width = 568
      Height = 9
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 247
    Width = 578
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnNovo: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 34
      Align = alLeft
      Caption = 'Novo'
      TabOrder = 0
      OnClick = btnNovoClick
    end
    object btnEditar: TButton
      AlignWithMargins = True
      Left = 84
      Top = 3
      Width = 75
      Height = 34
      Align = alLeft
      Caption = 'Editar'
      TabOrder = 1
      OnClick = btnEditarClick
    end
    object btnExcluir: TButton
      AlignWithMargins = True
      Left = 419
      Top = 3
      Width = 75
      Height = 34
      Align = alRight
      Caption = 'Excluir'
      TabOrder = 2
    end
    object btnListar: TButton
      AlignWithMargins = True
      Left = 500
      Top = 3
      Width = 75
      Height = 34
      Align = alRight
      Caption = 'Listar'
      TabOrder = 3
      OnClick = btnListarClick
    end
  end
  object dsCliente: TDataSource
    Left = 440
    Top = 40
  end
  object dsEndereco: TDataSource
    Left = 432
    Top = 152
  end
end
