program SimpleORMFiredac;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {Form9},
  Entidade.Pedido in '..\Entidades\Entidade.Pedido.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
