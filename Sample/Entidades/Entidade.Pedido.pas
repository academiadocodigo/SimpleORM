unit Entidade.Pedido;

interface

uses
  SimpleAttributes;

Type
  [Tabela('PEDIDO')]
//  [ProceName('PEDIDO_IU')]
  [ProceName('PEDIDO_IU_RESULT')]
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
    [FieldResult('RESULTID')]
    property ID: Integer read FID write SetID;
    [Campo('NOME')]
    property CLIENTE: String read FCLIENTE write SetCLIENTE;
    [Campo('DATA')]
    property DATAPEDIDO: TDatetime read FDATAPEDIDO write SetDATAPEDIDO;
    [Campo('VALOR')]
    property VALORTOTAL: Currency read FVALORTOTAL write SetVALORTOTAL;
  end;

implementation

{ TPEDIDO }

constructor TPEDIDO.Create;
begin

end;

destructor TPEDIDO.Destroy;
begin

  inherited;
end;

procedure TPEDIDO.SetDATAPEDIDO(const Value: TDatetime);
begin
  FDATAPEDIDO := Value;
end;

procedure TPEDIDO.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPEDIDO.SetCLIENTE(const Value: String);
begin
  FCLIENTE := Value;
end;

procedure TPEDIDO.SetVALORTOTAL(const Value: Currency);
begin
  FVALORTOTAL := Value;
end;

end.

