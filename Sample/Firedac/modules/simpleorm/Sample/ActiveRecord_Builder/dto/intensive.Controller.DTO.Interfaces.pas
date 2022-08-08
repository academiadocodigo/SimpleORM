unit intensive.Controller.DTO.Interfaces;

interface

uses
  intensive.Services.Generic,
  intensive.Model.Entity.Cliente;

type
  iClienteDTO = interface
    function Id(Value : Integer)  : iClienteDTO; overload;
    function Id : Integer; overload;
    function Nome(Value : String) : iClienteDTO; overload;
    function Nome : String; overload;
    function Telefone(Value : String) : iClienteDTO; overload;
    function Telefone : String; overload;
    function Build : iService<TCliente>;
  end;

implementation

end.
