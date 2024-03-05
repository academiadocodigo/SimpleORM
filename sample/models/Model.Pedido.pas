unit Model.Pedido;

interface

uses
  SimpleORM;

Type
  [Tabela('PEDIDO')]
  TPedido = class
  private
    FID: Integer;
    FCliente: String;
    FDataPedido: TDatetime;
    FValorTotal: Currency;
    procedure SetID(const Value: Integer);
    procedure SetCliente(const Value: String);
    procedure SetDataPedido(const Value: TDatetime);
    procedure SetValorTotal(const Value: Currency);
  public
    [Campo('ID'), Pk, AutoInc]
    property ID: Integer read FID write SetID;
    [Campo('NOME')]
    property Cliente: String read FCliente write SetCliente;
    [Campo('DATA')]
    property DataPedido: TDatetime read FDataPedido write SetDataPedido;
    [Campo('VALOR')]
    property ValorTotal: Currency read FValorTotal write SetValorTotal;
  end;

implementation

{ TPedido }

procedure TPedido.SetDataPedido(const Value: TDatetime);
begin
  FDATAPEDIDO := Value;
end;

procedure TPedido.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPedido.SetCliente(const Value: String);
begin
  FCLIENTE := Value;
end;

procedure TPedido.SetValorTotal(const Value: Currency);
begin
  FValorTotal := Value;
end;

end.

