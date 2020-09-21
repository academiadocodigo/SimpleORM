object fBase: TfBase
  Left = 0
  Top = 0
  Caption = 'fBase'
  ClientHeight = 511
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    446
    511)
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 440
    Height = 19
    Align = alTop
    Caption = 'Busca: Tabela Base'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 135
  end
  object btnCancelar: TButton
    Left = 358
    Top = 330
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = ':: Cancelar ::'
    TabOrder = 0
    OnClick = btnCancelarClick
  end
  object btnNovo: TButton
    Left = 8
    Top = 330
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = ':: Novo ::'
    TabOrder = 1
    OnClick = btnNovoClick
  end
  object pnlErros: TPanel
    Left = 0
    Top = 361
    Width = 446
    Height = 150
    Align = alBottom
    TabOrder = 2
    object lstSaidas: TListBox
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 428
      Height = 132
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 221
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
    end
  end
  object btnSalvar: TButton
    Left = 270
    Top = 330
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = ':: Salvar ::'
    TabOrder = 3
    OnClick = btnSalvarClick
  end
  object btnExcluir: TButton
    Left = 183
    Top = 330
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = ':: Excluir ::'
    TabOrder = 4
    OnClick = btnExcluirClick
  end
  object btnEditar: TButton
    Left = 95
    Top = 330
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = ':: Editar ::'
    TabOrder = 5
    OnClick = btnEditarClick
  end
  object grdDados: TDBGrid
    Left = 0
    Top = 55
    Width = 446
    Height = 120
    Align = alTop
    DataSource = dtsDados
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
    TabOrder = 6
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object edtPesquisar: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 440
    Height = 24
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
  end
  object conFireDac: TFDConnection
    Params.Strings = (
      'Database=C:\LIXO\RTTI.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'DriverID=FB')
    LoginPrompt = False
    Left = 178
    Top = 72
  end
  object dtsDados: TDataSource
    Left = 258
    Top = 72
  end
end
