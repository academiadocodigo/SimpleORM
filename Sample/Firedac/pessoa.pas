unit pessoa;

interface

uses
  pessoa.interfaces;

type
  TPessoa = class(TInterfacedObject, iPessoa)
    private
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iPessoa;
      function Nome(Value:String):iPessoa; overload;
    function Nome:String; overload;
    function SobreNome(Value:String):iPessoa; overload;
    function SobreNome:String; overload;
  end;

implementation

constructor TPessoa.Create;
begin

end;

destructor TPessoa.Destroy;
begin

  inherited;
end;

class function TPessoa.New : iPessoa;
begin
  Result := Self.Create;
end;

end.
