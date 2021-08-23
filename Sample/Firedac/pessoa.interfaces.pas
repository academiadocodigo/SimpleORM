unit pessoa.interfaces;

interface

type
  iPessoa = interface
    function Nome(Value:String):iPessoa; overload;
    function Nome:String; overload;
    function SobreNome(Value:String):iPessoa; overload;
    function SobreNome:String; overload;
  end;

implementation

end.
