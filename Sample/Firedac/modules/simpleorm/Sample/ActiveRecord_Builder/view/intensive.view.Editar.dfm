object frmEditar: TfrmEditar
  Left = 0
  Top = 0
  Caption = 'Editar Cliente'
  ClientHeight = 385
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 487
    Height = 342
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object edtNome: TLabeledEdit
      Left = 88
      Top = 23
      Width = 249
      Height = 21
      EditLabel.Width = 27
      EditLabel.Height = 13
      EditLabel.Caption = 'Nome'
      TabOrder = 0
    end
    object edtTelefone: TLabeledEdit
      Left = 343
      Top = 23
      Width = 130
      Height = 21
      EditLabel.Width = 42
      EditLabel.Height = 13
      EditLabel.Caption = 'Telefone'
      TabOrder = 1
    end
    object edtLogradouro: TLabeledEdit
      Left = 16
      Top = 71
      Width = 362
      Height = 21
      EditLabel.Width = 55
      EditLabel.Height = 13
      EditLabel.Caption = 'Logradouro'
      TabOrder = 2
    end
    object edtCep: TLabeledEdit
      Left = 384
      Top = 71
      Width = 89
      Height = 21
      EditLabel.Width = 19
      EditLabel.Height = 13
      EditLabel.Caption = 'Cep'
      TabOrder = 3
    end
    object edtBairro: TLabeledEdit
      Left = 16
      Top = 118
      Width = 105
      Height = 21
      EditLabel.Width = 28
      EditLabel.Height = 13
      EditLabel.Caption = 'Bairro'
      TabOrder = 4
    end
    object edtCidade: TLabeledEdit
      Left = 127
      Top = 118
      Width = 276
      Height = 21
      EditLabel.Width = 33
      EditLabel.Height = 13
      EditLabel.Caption = 'Cidade'
      TabOrder = 5
    end
    object edtEstado: TLabeledEdit
      Left = 409
      Top = 118
      Width = 64
      Height = 21
      EditLabel.Width = 33
      EditLabel.Height = 13
      EditLabel.Caption = 'Estado'
      TabOrder = 6
    end
    object grdEnderecos: TDBGrid
      Left = 0
      Top = 160
      Width = 487
      Height = 182
      Align = alBottom
      BorderStyle = bsNone
      DataSource = dsEndereco
      Options = [dgTitles, dgColLines, dgRowLines, dgRowSelect]
      TabOrder = 7
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Tahoma'
      TitleFont.Style = []
      OnCellClick = grdEnderecosCellClick
    end
    object edtId: TLabeledEdit
      Left = 16
      Top = 23
      Width = 66
      Height = 21
      EditLabel.Width = 11
      EditLabel.Height = 13
      EditLabel.Caption = 'ID'
      TabOrder = 8
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 342
    Width = 487
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnSalvar: TButton
      AlignWithMargins = True
      Left = 328
      Top = 3
      Width = 75
      Height = 37
      Align = alRight
      Caption = 'Salvar'
      TabOrder = 0
      OnClick = btnSalvarClick
    end
    object btnCancelar: TButton
      AlignWithMargins = True
      Left = 409
      Top = 3
      Width = 75
      Height = 37
      Align = alRight
      Caption = 'Cancelar'
      TabOrder = 1
    end
  end
  object dsEndereco: TDataSource
    Left = 408
    Top = 216
  end
end
