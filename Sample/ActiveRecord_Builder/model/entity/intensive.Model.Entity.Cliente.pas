unit intensive.Model.Entity.Cliente;

interface

uses
  SimpleAttributes;

type
  [Tabela('CLIENTE')]
  TCliente = class
  private
    FId: Integer;
    FNome: String;
    FTelefone: String;
    FCpfCnpj: String;
    procedure SetId(const Value: Integer);
    procedure SetNome(const Value: String);
    procedure SetTelefone(const Value: String);
    procedure SetCpfCnpj(const Value: String);
  public
    [Campo('ID'), PK, AutoInc]
    property Id : Integer read FId write SetId;
    [Campo('NOME')]
    property Nome : String read FNome write SetNome;
    [Campo('TELEFONE')]
    property Telefone : String read FTelefone write SetTelefone;
    [Campo('CPF_CNPJ')]
    property CpfCnpj : String read FCpfCnpj write SetCpfCnpj;
  end;

implementation


{ TCliente }

procedure TCliente.SetCpfCnpj(const Value: String);
begin
  FCpfCnpj := Value;
end;

procedure TCliente.SetId(const Value: Integer);
begin
  FId := Value;
end;

procedure TCliente.SetNome(const Value: String);
begin
  FNome := Value;
end;

procedure TCliente.SetTelefone(const Value: String);
begin
  FTelefone := Value;
end;

end.

