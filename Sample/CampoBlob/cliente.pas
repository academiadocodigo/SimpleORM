unit cliente;

interface

uses
  System.SysUtils,
  System.Classes,
  SimpleAttributes;

type
  [Tabela('CLIENTE')]
  TCliente = class
  private
    FId: Integer;
    FNome: String;
    FFoto: TStream;
    procedure setFoto(const Value: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    [Campo('ID'), Pk, AutoInc]
    property Id: Integer read FId write FId;
    [Campo('NOME')]
    property Nome: String read FNome write FNome;
    [Campo('FOTO', 'Blob')]
    property Foto: TStream read FFoto write setFoto;
  end;

implementation

{ TCliente }

constructor TCliente.Create;
begin
  FFoto := TMemoryStream.Create;
end;

destructor TCliente.Destroy;
begin
  FreeAndNil(FFoto);
  inherited;
end;

procedure TCliente.setFoto(const Value: TStream);
begin
  if not Assigned(Value) then
  begin
    TMemoryStream(FFoto).Clear;
    Exit;
  end;
  Value.Position := 0;
  TMemoryStream(FFoto).LoadFromStream(Value);
end;

end.
