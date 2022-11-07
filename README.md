<p align="center">
  <a href="https://github.com/bittencourtthulio/SimpleORM/blob/master/assets/logo.fw.png">
    <img alt="router4d" src="https://github.com/bittencourtthulio/SimpleORM/blob/master/assets/logo.fw.png">
  </a>  
</p>
<br>
<p align="center">
  <img src="https://img.shields.io/github/v/release/bittencourtthulio/SimpleORM?style=flat-square">
  <img src="https://img.shields.io/github/stars/bittencourtthulio/SimpleORM?style=flat-square">
  <img src="https://img.shields.io/github/contributors/bittencourtthulio/SimpleORM?color=orange&style=flat-square">
  <img src="https://img.shields.io/github/forks/bittencourtthulio/SimpleORM?style=flat-square">
   <img src="https://tokei.rs/b1/github/bittencourtthulio/SimpleORM?color=red&category=lines">
  <img src="https://tokei.rs/b1/github/bittencourtthulio/SimpleORM?color=green&category=code">
  <img src="https://tokei.rs/b1/github/bittencourtthulio/SimpleORM?color=yellow&category=files">
</p>


# SimpleORM
ORM Simples para Aplicações Delphi

O SimpleORM tem o Objetivo de facilitar suas implementações de CRUD, agilizando mais de 80% do seu processo de desenvolvimento de software.

Homologado para os drivers de Conexão Firedac e RestDataware.

#### Link do Grupo no Telegram
[Grupo SimpleORM](https://t.me/joinchat/Tv8fVFe_hdz0rJwq)

#### Gerador de Classes para o SimpleORM criados pela Comunidade

[andersonlugarinhoramos](https://github.com/andersonlugarinhoramos/geradorsimpleorm)</br>
[alan-petry](https://github.com/alan-petry/GeraClassesSimpleORM)</br>
[Douglas09](https://github.com/Douglas09/GeradorDeClasses_SimpleORM_MySql)</br>

Entidade do Banco de Dados Mapeada

```delphi
uses
  SimpleAttributes;

Type
  [Tabela('PEDIDO')]
  TPEDIDO = class
  private
    FID: Integer;
    FCLIENTE: String;
    FDATAPEDIDO: TDatetime;
    FVALORTOTAL: Currency;
    procedure SetID(const Value: Integer);
    procedure SetCLIENTE(const Value: String);
    procedure SetDATAPEDIDO(const Value: TDatetime);
    procedure SetVALORTOTAL(const Value: Currency);
  public
    constructor Create;
    destructor Destroy; override;
  published
    [Campo('ID'), Pk, AutoInc]
    property ID: Integer read FID write SetID;
    [Campo('NOME')]
    property CLIENTE: String read FCLIENTE write SetCLIENTE;
    [Campo('DATA')]
    property DATAPEDIDO: TDatetime read FDATAPEDIDO write SetDATAPEDIDO;
    [Campo('VALOR')]
    property VALORTOTAL: Currency read FVALORTOTAL write SetVALORTOTAL;
  end;
```

Obs.: Em casos como o "Campo('ID')" a anotação é opcional, pois a `property` tem o mesmo nome do campo no banco de dados.

## Atributos
`Tabela`  - Informa o Nome da Tabela no Banco em que a Classe faz o mapeamento.

`Campo`   - Informa o Nome do Campo no Banco de Dados em que a property está fazendo Referência.

`PK`      - Informa se o Campo é PrimaryKey.

`FK`      - Informa se o Campo é ForeignKey.

`NotNull` - Informa se o Campo é NotNull.

`Ignore`  - Ignorar o Campo nas Operações de CRUD.

`AutoInc` - Informa se o Campo é AutoIncremento.

`NumberOnly` - Informa se o Campo deve aceitar somente números.

`Bind` - Informa o Nome do Campo no Banco de Dados ou a `property` que o componente visual está fazendo Referência.

`Display` - Informa a descrição que deve aparecer no título de grids.

`Format`  - Informa o formato que o campo deve ter, coisas como (tamanho, precisão, máscara e range).

# Principais Operações


## Instalação
Basta adicionar ao LibraryPatch o Caminho do SimpleORM ou via [Boss](https://github.com/HashLoad/boss) com o comando `boss install bittencourtthulio/SimpleORM`, não precisa realizar a instalação de nenhum componente.

## Dependências
  - DataSetConverter4Delphi
  - Rest Dataware CORE

## Uses Necessárias

SimpleInterface,

SimpleDAO,

SimpleAttributes,

`Dependendo do seu driver de Conexão utilizar as Uses` SimpleQueryFiredac ou SimpleQueryRestDW

## Inicialização do SimpleORM

```delphi
var
  Conn : iSimpleQuery;
  DAOPedido : iSimpleDAO<TPEDIDO>;
begin
  Conn := TSimpleQueryFiredac.New(FDConnection1);
  DAOPedido := TSimpleDAO<TPEDIDO>
                  .New(Conn)
                  .DataSource(DataSource1)
                  .BindForm(Self);
end;
```

`Conn`      - Instancia a Interface iSimpleQuery passando o componente de conexão que o SimpleORM irá trabalhar.

`DAOPedido` - Instância o DAO para uma Entidade Mapeada, passando a Classe de Mapeamento como Atributo Genérico.

`New`       - Recebe a Interface de Conexão iSimpleQuery.

`DataSource`- Você pode informar um DataSource para que os dados sejam armazenados nele para facilitar seu processo de listagem de dados, podem linkar ao DBGrid para exibição dos mesmos.

`Bind`      - Você pode informar o formulário que deseja que o SimpleORM faça o Bind automatico entre a Classe e os Componentes da tela (Edit, Combo, CheckBox, RadioButton e etc...)


## MAPEAMENTO DO BIND DO FORMULÁRIO
Quando você fizer o mapeamento Bind do Formulário, não precisará ligar manualmente os campos da classe ao Edits, o SimpleORM faz isso automáticamente, basta você realizar o mapeamento correto conforme abaixo.

```delphi
type
  TForm1 = class(TForm)
    [Bind('CLIENTE')]
    Edit1: TEdit;
    [Bind('ID')]
    Edit2: TEdit;
    [Bind('VALORTOTAL')]
    Edit3: TEdit;
    Button2: TButton;
    [Bind('DATAPEDIDO')]
    DateTimePicker1: TDateTimePicker;
```

No atributo Bind de cada campo, você deve informar o nome da Property correspondente na Classe Mapeada do banco de dados ou o nome do campo na tabela do banco de dados, ATENÇÃO de qualquer forma a Classe deve estar mapeada corretamente.

## INSERT COM BIND

```delphi
begin
  DAOPedido.Insert;
end;
```

## INSERT COM OBJETO
```delphi
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.CLIENTE := Edit1.Text;
    Pedido.DATAPEDIDO := now;
    Pedido.VALORTOTAL := StrToCurr(Edit3.Text);
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
  end;
end;
```

## INSERT COM OBJETO HERDADO DE `TSimpleEntity` E COM VALIDAÇÃO
```delphi
var
  Cliente: TCliente;
begin
  Cliente := TCliente.Create;
  try
    // PEGA OS VALORES DA TELA E PREENCHE O OBJETO VIA BIND
    Cliente.Parse(Self); 

    // VALIDA SEGUNDO ANOTAÇÕES DO OBJETO
    TSimpleValidator.Validate(Cliente); 
    
    // INSERE "CLIENTE" NO BANCO DE DADOS
    DAOCliente.Insert(Cliente);
  finally
    Cliente.Free;
  end;
end;
```

## UPDATE COM BIND
```delphi
begin
  DAOPedido.Update;
end;
```

## UPDATE COM OBJETO
```delphi
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.CLIENTE := Edit1.Text;
    Pedido.DATAPEDIDO := now;
    Pedido.VALORTOTAL := StrToCurr(Edit3.Text);
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
  end;
end;
```

## UPDATE COM OBJETO HERDADO DE `TSimpleEntity` E COM VALIDAÇÃO
```delphi
var
  Cliente: TCliente;
begin
  Cliente := TCliente.Create;
  try
    Cliente.Parse(Self); 
    TSimpleValidator.Validate(Cliente); 
    DAOCliente.Update(Cliente);
  finally
    Cliente.Free;
  end;
end;
```

## DELETE COM BIND
```delphi
begin
  DAOPedido.Delete;
end;
```

## DELETE COM OBJETO
```delphi
 var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    DAOPedido.Delete(Pedido);
  finally
    Pedido.Free;
  end;
end;
```

## SELECT 
Você pode executar desde operações simples como trazer todos os dados da tabela, como filtrar campos e outras instruções SQL, 

executando a instrução abaixo você retornará todos os dados da tabela
```delphi
  DAOPedido.Find;
```

Abaixo um exemplo de todas as operações possíveis no SimpleORM
```delphi
begin
  DAOPedido
    .SQL
      .Fields('Informe os Campos que deseja trazer separados por virgula')
      .Join('Informe a Instrução Join que desejar ex INNER JOIN CLIENTE ON CLIENTE.ID = PRODUTO.CLIENTE')
      .Where('Coloque a Clausula Where que desejar ex: CODIGO = 1')
      .OrderBy('Informe o nome do Campo que deseja ordenar ex: ID')
      .GroupBy('Informe os campos que deseja agrupar separados por virgula')
    .&End
  .Find;
end;
```

Você também pode retornar uma Lista de Objetos para trabalhar com ela manualmente, abaixo um exemplo de como trazer a lista e trabalhar com ela exibindo todos os valores em um Memo
```delphi
var
  Pedidos : TObjectList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := TObjectList<TPEDIDO>.Create;
  DAOPedido.Find(Pedidos);
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.CLIENTE + DateToStr(Pedido.DATAPEDIDO));
    end;
  finally
    Pedidos.Free;
  end;
end;
```

## Grupo do telegram
Esse grupo tem como objetivo ajudar seus integrantes em assuntos diversos ao uso do componente, e colaboração para a evolução desse maravilhoso ORM ([Object-relational mapping](https://pt.wikipedia.org/wiki/Mapeamento_objeto-relacional)).
Acesse com esse link para participar do [Grupo SimpleORM](https://t.me/joinchat/Tv8fVFe_hdz0rJwq)
