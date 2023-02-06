unit intensive.Controller;

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
  intensive.Controller.Interfaces,
  intensive.Model.Entity.Cliente, intensive.Services.Generic,
  intensive.Controller.DTO.Interfaces, intensive.Controller.DTO.Cliente;

type
  TController = class(TInterfacedObject, iController)
    private
      FCliente : iClienteDTO;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iController;
      function Cliente : iClienteDTO;
  end;

implementation

constructor TController.Create;
begin

end;

destructor TController.Destroy;
begin

  inherited;
end;

class function TController.New : iController;
begin
  Result := Self.Create;
end;

function TController.Cliente: iClienteDTO;
begin
  if not Assigned(FCliente) then
    FCliente := TClienteDTO.New;
  Result := FCliente;
end;

end.
