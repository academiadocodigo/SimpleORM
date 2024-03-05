unit Entidade.Cliente;

interface

uses
  SimpleAttributes, Data.DB, SimpleEntity;

type
  [Tabela('CLIENTE')]
  TCliente = class(TSimpleEntity)
  private
    FCodigo: Integer;
    FNome: string;
    FDocumento: string;
    FEndereco: string;
    FSexo: string;
    FDataNascimento: TDate;
    FDataCadastro: TDate;
    FCredito: Real;
  public
    [Campo('ID'),
    Display('C�digo'),
    NotNull,
    PK,
    Format(3)]
    property Codigo: Integer read FCodigo write FCodigo;
    [Campo('NOME'),
    NotNull,
    Format(40)]
    property Nome: string read FNome write FNome;
    [Campo('DOC'),
    NotNull,
    Format(14)]
    property Documento: string read FDocumento write FDocumento;
    [Campo('LOGRADOURO'),
    Display('Endere�o'),
    NotNull,
    Format(60)]
    property Endereco: string read FEndereco write FEndereco;
    [Format(10)] // se n�o por o atributo Campo('SEXO') ele usa o nome da propriedade
    property Sexo: string read FSexo write FSexo;
    [Campo('DTNASC'),
    Display('Nascimento'),
    NotNull,
    Format('dd-mm-yyyy')]
    property DataNascimento: TDate read FDataNascimento write FDataNascimento;
    [Campo('DTCAD'),
    Display('Cadastro'),
    NotNull,
    Format('yyyymmdd')]
    property DataCadastro: TDate read FDataCadastro write FDataCadastro;
    [Campo('CREDITO'),
    Display('Cr�dito'),
    Format(10, 2)]
    property Credito: Real read FCredito write FCredito;
  end;

  TClientes = TSimpleEntityList<TCliente>;

implementation

end.
