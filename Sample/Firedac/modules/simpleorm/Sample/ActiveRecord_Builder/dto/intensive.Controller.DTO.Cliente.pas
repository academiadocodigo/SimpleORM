unit intensive.Controller.DTO.Cliente;

interface

uses
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI,
  Data.DB,
  FireDAC.Comp.Client,
  intensive.Model.Entity.Cliente,
  intensive.Controller.DTO.Interfaces,
  intensive.Services.Generic;

type
  TClienteDTO = class(TInterfacedObject, iClienteDTO)
    private
      FEntity : TCliente;
      FService : iService<TCliente>;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iClienteDTO;
      function Id(Value : Integer)  : iClienteDTO; overload;
      function Id : Integer; overload;
      function Nome(Value : String) : iClienteDTO; overload;
      function Nome : String; overload;
      function Telefone(Value : String) : iClienteDTO; overload;
      function Telefone : String; overload;
      function Build : iService<TCliente>;
  end;

implementation

function TClienteDTO.Build: iService<TCliente>;
begin
  Result := FSErvice;
end;

constructor TClienteDTO.Create;
begin
  FEntity := TCliente.Create;
  FService := TService<TCliente>.New(FEntity);
end;

destructor TClienteDTO.Destroy;
begin
  FEntity.DisposeOf;
  inherited;
end;

function TClienteDTO.Id: Integer;
begin
    Result := FEntity.Id;
end;

function TClienteDTO.Id(Value: Integer): iClienteDTO;
begin
  Result := Self;
  FEntity.Id := Value;
end;

class function TClienteDTO.New : iClienteDTO;
begin
  Result := Self.Create;
end;

function TClienteDTO.Nome: String;
begin
    Result := FEntity.Nome;
end;

function TClienteDTO.Nome(Value: String): iClienteDTO;
begin
  Result := Self;
  FEntity.Nome := Value;
end;

function TClienteDTO.Telefone(Value: String): iClienteDTO;
begin
  Result := Self;
  FEntity.Telefone := Value;
end;

function TClienteDTO.Telefone: String;
begin
    Result := FEntity.Telefone;
end;

end.
