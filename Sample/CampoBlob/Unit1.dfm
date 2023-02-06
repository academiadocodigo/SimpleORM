object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 381
  ClientWidth = 590
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
    Width = 590
    Height = 381
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 410
      Height = 381
      Align = alLeft
      BevelOuter = bvNone
      Padding.Left = 10
      Padding.Top = 10
      Padding.Right = 10
      Padding.Bottom = 8
      TabOrder = 0
      object Panel4: TPanel
        Left = 10
        Top = 290
        Width = 390
        Height = 83
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object btnListar: TButton
          Left = 4
          Top = 6
          Width = 88
          Height = 25
          Caption = 'Listar'
          TabOrder = 0
          OnClick = btnListarClick
        end
        object btnListarPorID: TButton
          Left = 4
          Top = 32
          Width = 88
          Height = 25
          Caption = 'Listar Por ID'
          TabOrder = 1
          OnClick = btnListarPorIDClick
        end
        object ListarPorNome: TButton
          Left = 4
          Top = 58
          Width = 88
          Height = 25
          Caption = 'Listar Por Nome'
          TabOrder = 2
          OnClick = ListarPorNomeClick
        end
        object btnInserir: TButton
          Left = 101
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Inserir'
          TabOrder = 3
          OnClick = btnInserirClick
        end
        object btnAtualizar: TButton
          Left = 101
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Atualizar'
          TabOrder = 4
          OnClick = btnAtualizarClick
        end
        object btnExcluir: TButton
          Left = 101
          Top = 58
          Width = 75
          Height = 25
          Caption = 'Excluir'
          TabOrder = 5
          OnClick = btnExcluirClick
        end
      end
      object Panel5: TPanel
        Left = 10
        Top = 10
        Width = 390
        Height = 55
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object edtCodigo: TLabeledEdit
          Left = 0
          Top = 24
          Width = 97
          Height = 24
          EditLabel.Width = 11
          EditLabel.Height = 13
          EditLabel.Caption = 'ID'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
        object edtNome: TLabeledEdit
          Left = 103
          Top = 24
          Width = 287
          Height = 24
          EditLabel.Width = 27
          EditLabel.Height = 13
          EditLabel.Caption = 'Nome'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
      end
      object Panel6: TPanel
        Left = 10
        Top = 65
        Width = 390
        Height = 225
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        object ListView1: TListView
          Left = 0
          Top = 0
          Width = 390
          Height = 225
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          Columns = <
            item
              Caption = 'Id'
            end
            item
              Caption = 'Nome'
              Width = 333
            end>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          RowSelect = True
          ParentFont = False
          TabOrder = 0
          ViewStyle = vsReport
          OnClick = ListView1Click
          OnDblClick = ListView1DblClick
        end
      end
    end
    object Panel3: TPanel
      Left = 401
      Top = 0
      Width = 189
      Height = 180
      BevelOuter = bvNone
      Padding.Left = 5
      Padding.Top = 5
      Padding.Right = 5
      Padding.Bottom = 5
      TabOrder = 1
      object Shape1: TShape
        Left = 5
        Top = 5
        Width = 179
        Height = 170
        Align = alClient
        ExplicitLeft = 124
        ExplicitTop = 115
        ExplicitWidth = 65
        ExplicitHeight = 65
      end
      object imgFoto: TImage
        AlignWithMargins = True
        Left = 15
        Top = 15
        Width = 159
        Height = 150
        Margins.Left = 10
        Margins.Top = 10
        Margins.Right = 10
        Margins.Bottom = 10
        Align = alClient
        Stretch = True
        ExplicitLeft = 64
        ExplicitTop = 88
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
      object SpeedButton1: TSpeedButton
        Left = 5
        Top = 5
        Width = 179
        Height = 170
        Align = alClient
        Flat = True
        OnClick = SpeedButton1Click
        ExplicitTop = 0
        ExplicitWidth = 189
        ExplicitHeight = 180
      end
    end
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=simpleorm.sdb'
      'LockingMode=Normal'
      'DriverID=SQLite')
    Left = 432
    Top = 200
  end
  object OpenDialog1: TOpenDialog
    Left = 512
    Top = 200
  end
end
