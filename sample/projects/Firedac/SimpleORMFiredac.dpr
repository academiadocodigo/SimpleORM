program SimpleORMFiredac;

uses
  Vcl.Forms,
  Principal in 'Principal.pas' {FrmPrincipal},
  Model.Pedido in '..\..\models\Model.Pedido.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmPrincipal, FrmPrincipal);
  Application.Run;
end.
