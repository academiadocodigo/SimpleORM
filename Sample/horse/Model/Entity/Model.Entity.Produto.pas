unit Model.Entity.Produto;

interface

uses
  SimpleAttributes;

Type
  [Tabela('PRODUTO')]
  TProduto = class
  private
    FDESCRICAO: String;
    FCODIGO: Integer;
    FID: Integer;
    procedure SetCODIGO(const Value: Integer);
    procedure SetDESCRICAO(const Value: String);
    procedure SetID(const Value: Integer);

  public
    constructor Create;
    destructor Destroy; override;
  published
    [Campo('ID')]
    property ID: Integer read FID write SetID;

    [Campo('CODIGO')]
    property CODIGO: Integer read FCODIGO write SetCODIGO;

    [Campo('DESCRICAO')]
    property DESCRICAO: String read FDESCRICAO write SetDESCRICAO;
  end;

implementation


{ TProduto }

constructor TProduto.Create;
begin

end;

destructor TProduto.Destroy;
begin

  inherited;
end;

procedure TProduto.SetCODIGO(const Value: Integer);
begin
  FCODIGO := Value;
end;

procedure TProduto.SetDESCRICAO(const Value: String);
begin
  FDESCRICAO := Value;
end;

procedure TProduto.SetID(const Value: Integer);
begin
  FID := Value;
end;

end.
