unit Model.Entity.Produto;

interface

uses
  SimpleAttributes;

type
  [Tabela('PRODUTO')]
  TPRODUTO = class
  private
    FGUUID :String;
    FCODIGO :String;
    FDESCRICAO :String;
    FPRECO :Currency;
    FNCM :Integer;
    FALIQUOTA :Currency;
    FST :Integer;
    FSTATUS :Integer;
    FDATAALTERACAO :TDateTime;

    procedure SetGUUID (const Value :String);
    function GetGUUID :String;

    procedure SetCODIGO (const Value :String);
    function GetCODIGO :String;

    procedure SetDESCRICAO (const Value :String);
    function GetDESCRICAO :String;

    procedure SetPRECO (const Value :Currency);
    function GetPRECO :Currency;

    procedure SetNCM (const Value :Integer);
    function GetNCM :Integer;

    procedure SetALIQUOTA (const Value :Currency);
    function GetALIQUOTA :Currency;

    procedure SetST (const Value :Integer);
    function GetST :Integer;

    procedure SetSTATUS (const Value :Integer);
    function GetSTATUS :Integer;

    procedure SetDATAALTERACAO (const Value :TDateTime);
    function GetDATAALTERACAO :TDateTime;


  public
    constructor Create;
    destructor Destroy; override;
    procedure Limpar;
    [Campo('GUUID'), PK]
    property GUUID :String read GetGUUID write SetGUUID;
    [Campo('CODIGO')]
    property CODIGO :String read GetCODIGO write SetCODIGO;
    [Campo('DESCRICAO')]
    property DESCRICAO :String read GetDESCRICAO write SetDESCRICAO;
    [Campo('PRECO')]
    property PRECO :Currency read GetPRECO write SetPRECO;
    [Campo('NCM')]
    property NCM :Integer read GetNCM write SetNCM;
    [Campo('ALIQUOTA')]
    property ALIQUOTA :Currency read GetALIQUOTA write SetALIQUOTA;
    [Campo('ST')]
    property ST :Integer read GetST write SetST;
    [Campo('STATUS')]
    property STATUS :Integer read GetSTATUS write SetSTATUS;
    [Campo('DATAALTERACAO')]
    property DATAALTERACAO :TDateTime read GetDATAALTERACAO write SetDATAALTERACAO;

end;

implementation

constructor TPRODUTO.Create;
begin
  Limpar;
end;

destructor TPRODUTO.Destroy;
begin

  inherited;
end;

procedure TPRODUTO.SetGUUID (const Value :String);
begin
  FGUUID := Value;
end;

function TPRODUTO.GetGUUID :String;
begin
  Result := FGUUID;
end;

procedure TPRODUTO.SetCODIGO (const Value :String);
begin
  FCODIGO := Value;
end;

function TPRODUTO.GetCODIGO :String;
begin
  Result := FCODIGO;
end;

procedure TPRODUTO.SetDESCRICAO (const Value :String);
begin
  FDESCRICAO := Value;
end;

function TPRODUTO.GetDESCRICAO :String;
begin
  Result := FDESCRICAO;
end;

procedure TPRODUTO.SetPRECO (const Value :Currency);
begin
  FPRECO := Value;
end;

function TPRODUTO.GetPRECO :Currency;
begin
  Result := FPRECO;
end;

procedure TPRODUTO.SetNCM (const Value :Integer);
begin
  FNCM := Value;
end;

function TPRODUTO.GetNCM :Integer;
begin
  Result := FNCM;
end;

procedure TPRODUTO.SetALIQUOTA (const Value :Currency);
begin
  FALIQUOTA := Value;
end;

function TPRODUTO.GetALIQUOTA :Currency;
begin
  Result := FALIQUOTA;
end;

procedure TPRODUTO.SetST (const Value :Integer);
begin
  FST := Value;
end;

function TPRODUTO.GetST :Integer;
begin
  Result := FST;
end;

procedure TPRODUTO.SetSTATUS (const Value :Integer);
begin
  FSTATUS := Value;
end;

function TPRODUTO.GetSTATUS :Integer;
begin
  Result := FSTATUS;
end;

procedure TPRODUTO.SetDATAALTERACAO (const Value :TDateTime);
begin
  FDATAALTERACAO := Value;
end;

function TPRODUTO.GetDATAALTERACAO :TDateTime;
begin
  Result := FDATAALTERACAO;
end;


procedure TPRODUTO.Limpar;
begin

  Self.GUUID := '';
  Self.CODIGO := '';
  Self.DESCRICAO := '';
  Self.PRECO := 0;
  Self.NCM := 0;
  Self.ALIQUOTA := 0;
  Self.ST := 0;
  Self.STATUS := 0;
  Self.DATAALTERACAO := 0;

end;

end.

