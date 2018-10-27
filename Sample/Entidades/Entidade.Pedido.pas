unit Entidade.Pedido;

interface

uses
  SimpleAttributes;

Type
  TPEDIDO = class
    private
    FID: Integer;
    FNOME: String;
    FDATA: TDatetime;
    FVALOR: Currency;
    procedure SetID(const Value: Integer);
    procedure SetNOME(const Value: String);
    procedure SetDATA(const Value: TDatetime);
    procedure SetVALOR(const Value: Currency);
    public
      constructor Create;
      destructor Destroy; override;
    published
      [PK, AutoInc]
      property ID : Integer read FID write SetID;
      property NOME : String read FNOME write SetNOME;
      property DATA : TDatetime read FDATA write SetDATA;
      property VALOR : Currency read FVALOR write SetVALOR;
    end;

implementation

{ TMinhaClasse }

constructor TPEDIDO.Create;
begin

end;

destructor TPEDIDO.Destroy;
begin

  inherited;
end;

procedure TPEDIDO.SetDATA(const Value: TDatetime);
begin
  FDATA := Value;
end;

procedure TPEDIDO.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TPEDIDO.SetNOME(const Value: String);
begin
  FNOME := Value;
end;

procedure TPEDIDO.SetVALOR(const Value: Currency);
begin
  FVALOR := Value;
end;

end.
