unit TestEntities;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  SimpleAttributes;

type
  { Entidade basica com todos os atributos principais }
  [Tabela('PEDIDO')]
  TPedidoTest = class
  private
    FID: Integer;
    FCLIENTE: String;
    FDATAPEDIDO: TDateTime;
    FVALORTOTAL: Currency;
    FOBSERVACAO: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('CLIENTE'), NotNull]
    property CLIENTE: String read FCLIENTE write FCLIENTE;
    [Campo('DATA_PEDIDO')]
    property DATAPEDIDO: TDateTime read FDATAPEDIDO write FDATAPEDIDO;
    [Campo('VALOR_TOTAL'), NotZero]
    property VALORTOTAL: Currency read FVALORTOTAL write FVALORTOTAL;
    [Ignore]
    property OBSERVACAO: String read FOBSERVACAO write FOBSERVACAO;
  end;

  { Entidade com SoftDelete }
  [Tabela('CLIENTE')]
  [SoftDelete('EXCLUIDO')]
  TClienteTest = class
  private
    FID: Integer;
    FNOME: String;
    FEMAIL: String;
    FEXCLUIDO: Integer;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('NOME'), NotNull]
    property NOME: String read FNOME write FNOME;
    [Campo('EMAIL'), Email]
    property EMAIL: String read FEMAIL write FEMAIL;
    [Campo('EXCLUIDO')]
    property EXCLUIDO: Integer read FEXCLUIDO write FEXCLUIDO;
  end;

  { Entidade com validacoes completas }
  [Tabela('PRODUTO')]
  TProdutoTest = class
  private
    FID: Integer;
    FNOME: String;
    FPRECO: Double;
    FQUANTIDADE: Integer;
    FCODIGO: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('NOME'), NotNull, Format([3, 100])]
    property NOME: String read FNOME write FNOME;
    [Campo('PRECO'), NotZero, MinValue(0.01), MaxValue(99999.99)]
    property PRECO: Double read FPRECO write FPRECO;
    [Campo('QUANTIDADE'), MinValue(0)]
    property QUANTIDADE: Integer read FQUANTIDADE write FQUANTIDADE;
    [Campo('CODIGO'), Regex('^[A-Z]{2}\d{4}$', 'Codigo deve ser 2 letras + 4 digitos')]
    property CODIGO: String read FCODIGO write FCODIGO;
  end;

  { Entidade com FK e relacionamentos }
  [Tabela('ITEM_PEDIDO')]
  TItemPedidoTest = class
  private
    FID: Integer;
    FPEDIDO_ID: Integer;
    FPRODUTO_ID: Integer;
    FQUANTIDADE: Integer;
    FVALOR: Currency;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('PEDIDO_ID'), FK, NotZero]
    property PEDIDO_ID: Integer read FPEDIDO_ID write FPEDIDO_ID;
    [Campo('PRODUTO_ID'), FK, NotZero]
    property PRODUTO_ID: Integer read FPRODUTO_ID write FPRODUTO_ID;
    [Campo('QUANTIDADE'), NotZero]
    property QUANTIDADE: Integer read FQUANTIDADE write FQUANTIDADE;
    [Campo('VALOR'), NotZero]
    property VALOR: Currency read FVALOR write FVALOR;
  end;

  { Entidade simples sem Campo (usa nome da property) }
  [Tabela('CATEGORIA')]
  TCategoriaTest = class
  private
    FID: Integer;
    FNOME: String;
    FATIVO: Integer;
  published
    [PK, AutoInc]
    property ID: Integer read FID write FID;
    [NotNull]
    property NOME: String read FNOME write FNOME;
    property ATIVO: Integer read FATIVO write FATIVO;
  end;

  { Entidade com Format (tamanho e precisao) }
  [Tabela('CONTA')]
  TContaTest = class
  private
    FID: Integer;
    FDESCRICAO: String;
    FVALOR: Currency;
    FNUMERO: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('DESCRICAO'), NotNull, Format(200)]
    property DESCRICAO: String read FDESCRICAO write FDESCRICAO;
    [Campo('VALOR'), Format(10, 2)]
    property VALOR: Currency read FVALOR write FVALOR;
    [Campo('NUMERO'), NumberOnly]
    property NUMERO: String read FNUMERO write FNUMERO;
  end;

  { Entidade com Display }
  [Tabela('USUARIO')]
  TUsuarioTest = class
  private
    FID: Integer;
    FNOME: String;
    FEMAIL: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('NOME'), NotNull, Display('Nome Completo')]
    property NOME: String read FNOME write FNOME;
    [Campo('EMAIL'), Email, Display('E-mail')]
    property EMAIL: String read FEMAIL write FEMAIL;
  end;

  { Entidade com Timestamps }
  [Tabela('ENTITY_TIMESTAMPS')]
  TTestTimestampEntity = class
  private
    FId: Integer;
    FNome: String;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
  published
    [Campo('ID')]
    [PK]
    [AutoInc]
    property Id: Integer read FId write FId;

    [Campo('NOME')]
    property Nome: String read FNome write FNome;

    [Campo('DT_CRIACAO')]
    [CreatedAt]
    property DataCriacao: TDateTime read FCreatedAt write FCreatedAt;

    [Campo('DT_ATUALIZACAO')]
    [UpdatedAt]
    property DataAtualizacao: TDateTime read FUpdatedAt write FUpdatedAt;
  end;

  { Entidade com CPF e CNPJ }
  [Tabela('PESSOA')]
  TPessoaTest = class
  private
    FID: Integer;
    FNOME: String;
    FCPF: String;
    FCNPJ: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('NOME'), NotNull]
    property NOME: String read FNOME write FNOME;
    [Campo('CPF'), CPF]
    property CPF: String read FCPF write FCPF;
    [Campo('CNPJ'), CNPJ]
    property CNPJ: String read FCNPJ write FCNPJ;
  end;

  { Entidade para teste de AI Skills }
  [Tabela('ARTIGO')]
  TArtigoTest = class
  private
    FID: Integer;
    FTITULO: String;
    FDESCRICAO: String;
    FTEXTO: String;
    FRESUMO: String;
    FTAGS: String;
    FSENTIMENTO: String;
    FTITULO_EN: String;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('TITULO'), NotNull]
    property TITULO: String read FTITULO write FTITULO;
    [Campo('DESCRICAO')]
    property DESCRICAO: String read FDESCRICAO write FDESCRICAO;
    [Campo('TEXTO')]
    property TEXTO: String read FTEXTO write FTEXTO;
    [Campo('RESUMO')]
    property RESUMO: String read FRESUMO write FRESUMO;
    [Campo('TAGS')]
    property TAGS: String read FTAGS write FTAGS;
    [Campo('SENTIMENTO')]
    property SENTIMENTO: String read FSENTIMENTO write FSENTIMENTO;
    [Campo('TITULO_EN')]
    property TITULO_EN: String read FTITULO_EN write FTITULO_EN;
  end;

  { Entidade para teste de CalcTotal }
  [Tabela('ITEM_CALC')]
  TItemCalcTest = class
  private
    FID: Integer;
    FQUANTIDADE: Double;
    FPRECO_UNITARIO: Double;
    FDESCONTO: Double;
    FVALOR_TOTAL: Double;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('QUANTIDADE')]
    property QUANTIDADE: Double read FQUANTIDADE write FQUANTIDADE;
    [Campo('PRECO_UNITARIO')]
    property PRECO_UNITARIO: Double read FPRECO_UNITARIO write FPRECO_UNITARIO;
    [Campo('DESCONTO')]
    property DESCONTO: Double read FDESCONTO write FDESCONTO;
    [Campo('VALOR_TOTAL')]
    property VALOR_TOTAL: Double read FVALOR_TOTAL write FVALOR_TOTAL;
  end;

  { Entidade que reproduz a issue #83 (Field Map Error) — mistura de tipos
    TDate, Boolean, Double e Int64 lidos de um DataSet, incluindo colunas NULL }
  [Tabela('BYQCONFIG')]
  TBYQConfigTest = class
  private
    FID: Integer;
    FFACTORYID: String;
    FTESTDATE: TDate;
    FPRESSMAX: Double;
    FPRESSMIN: Double;
    FISTESTFINISHED: Boolean;
    FBIGCOUNTER: Int64;
  published
    [Campo('ID'), PK, AutoInc]
    property ID: Integer read FID write FID;
    [Campo('FACTORYID'), NotNull, Format(60)]
    property FACTORYID: String read FFACTORYID write FFACTORYID;
    [Campo('TESTDATE')]
    property TESTDATE: TDate read FTESTDATE write FTESTDATE;
    [Campo('PRESSMAX')]
    property PRESSMAX: Double read FPRESSMAX write FPRESSMAX;
    [Campo('PRESSMIN')]
    property PRESSMIN: Double read FPRESSMIN write FPRESSMIN;
    [Campo('ISTESTFINISHED')]
    property ISTESTFINISHED: Boolean read FISTESTFINISHED write FISTESTFINISHED;
    [Campo('BIGCOUNTER')]
    property BIGCOUNTER: Int64 read FBIGCOUNTER write FBIGCOUNTER;
  end;

implementation

end.
