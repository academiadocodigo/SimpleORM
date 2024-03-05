inherited fCliente: TfCliente
  Caption = 'Cadastro de Clientes'
  ClientHeight = 526
  OnShow = FormShow
  ExplicitHeight = 565
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 8
    Top = 260
    Width = 24
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Sexo'
    FocusControl = cbxSexo
  end
  object Label2: TLabel [1]
    Left = 138
    Top = 260
    Width = 55
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Nascimento'
    FocusControl = dtpNascimento
  end
  object Label3: TLabel [2]
    Left = 250
    Top = 260
    Width = 44
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Cadastro'
    FocusControl = dtpCadastro
  end
  object Label4: TLabel [3]
    Left = 360
    Top = 260
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Cr'#233'dito'
    FocusControl = edtCredito
  end
  object Label5: TLabel [4]
    Left = 8
    Top = 212
    Width = 45
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Endere'#231'o'
    FocusControl = edtEndereco
  end
  object Label6: TLabel [5]
    Left = 8
    Top = 165
    Width = 33
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'C'#243'digo'
    FocusControl = edtCodigo
  end
  object Label7: TLabel [6]
    Left = 87
    Top = 165
    Width = 27
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Nome'
    FocusControl = edtNome
  end
  object Label8: TLabel [7]
    Left = 323
    Top = 165
    Width = 54
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Documento'
    FocusControl = edtDocumento
  end
  inherited Label9: TLabel
    Caption = 'Busca: Tabela Clientes'
    ExplicitWidth = 158
  end
  inherited btnCancelar: TButton
    Top = 307
    TabOrder = 12
    ExplicitTop = 307
  end
  inherited btnNovo: TButton
    Top = 307
    TabOrder = 8
    ExplicitTop = 307
  end
  inherited pnlErros: TPanel
    Top = 376
    TabOrder = 14
    ExplicitTop = 376
  end
  inherited btnSalvar: TButton
    Top = 307
    TabOrder = 11
    ExplicitTop = 307
  end
  inherited btnExcluir: TButton
    Top = 307
    TabOrder = 10
    ExplicitTop = 307
  end
  inherited btnEditar: TButton
    Top = 307
    TabOrder = 9
    ExplicitTop = 307
  end
  inherited grdDados: TDBGrid
    Height = 104
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 15
  end
  object edtCodigo: TEdit [16]
    Left = 8
    Top = 184
    Width = 73
    Height = 21
    Anchors = [akLeft, akBottom]
    NumbersOnly = True
    TabOrder = 0
  end
  object edtNome: TEdit [17]
    Left = 87
    Top = 184
    Width = 230
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
  end
  object edtDocumento: TEdit [18]
    Left = 323
    Top = 184
    Width = 115
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object edtEndereco: TEdit [19]
    Left = 8
    Top = 231
    Width = 430
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
  end
  object edtCredito: TEdit [20]
    Left = 360
    Top = 279
    Width = 78
    Height = 21
    Anchors = [akLeft, akBottom]
    NumbersOnly = True
    TabOrder = 7
  end
  object cbxSexo: TComboBox [21]
    Left = 8
    Top = 279
    Width = 124
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Items.Strings = (
      'Masculino'
      'Feminino')
  end
  object btnGerarRelatorio: TButton [22]
    Left = 8
    Top = 338
    Width = 210
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Gerar Relat'#243'rio'
    TabOrder = 13
    OnClick = btnGerarRelatorioClick
  end
  object dtpNascimento: TDateTimePicker [23]
    Left = 138
    Top = 280
    Width = 100
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 44082.000000000000000000
    Time = 0.466106226849660700
    TabOrder = 5
  end
  object dtpCadastro: TDateTimePicker [24]
    Left = 250
    Top = 280
    Width = 100
    Height = 21
    Anchors = [akLeft, akBottom]
    Date = 44082.000000000000000000
    Time = 0.466106226849660700
    TabOrder = 6
  end
  inherited edtPesquisar: TEdit
    TabOrder = 16
    OnKeyUp = edtPesquisarKeyUp
  end
  object btnGerarJson: TButton [26]
    Left = 228
    Top = 338
    Width = 210
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Gerar JSON'
    TabOrder = 17
    OnClick = btnGerarJsonClick
  end
end
