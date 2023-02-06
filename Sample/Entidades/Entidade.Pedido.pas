unit Entidade.Pedido;

interface

uses
  System.Classes,
  Data.DB,
  SimpleAttributes, System.SysUtils;

Type
  [Tabela('PEDIDO')]
  TPEDIDO = class
  private
    FID: Integer;
    FCLIENTE: String;
    FDATAPEDIDO: TDatetime;
    FVALORTOTAL: Currency;
    FFoto: TStream;
    procedure SetID(const Value: Integer);
    procedure SetCLIENTE(const Value: String);
    procedure SetDATAPEDIDO(const Value: TDatetime);
    procedure SetVALORTOTAL(const Value: Currency);
  public
    constructor Create;
    destructor Destroy; override;
  published
    [Campo('ID', ftInteger), Pk, AutoInc]
    property ID: Integer read FID write SetID;
    [Campo('NOME', ftString)]
    property CLIENTE: String read FCLIENTE write SetCLIENTE;
    [Campo('DATA', ftDateTime)]
    property DATAPEDIDO: TDatetime read FDATAPEDIDO write SetDATAPEDIDO;
    [Campo('VALOR', ftFloat)]
    property VALORTOTAL: Currency read FVALORTOTAL write SetVALORTOTAL;
    [Campo('FOTO', ftBlob)]
    property Foto: TStream read FFoto write FFoto;
  end;

implementation

{ TPEDIDO }

constructor TPEDIDO.Create;
begin
  FFoto := TMemoryStream.Create;
end;

destructor TPEDIDO.Destroy;
begin
//    FreeAndNil(FFoto);
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

