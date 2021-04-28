program SimpleORMHorse;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Model.Connection in 'Model\Connection\Model.Connection.pas',
  Controller.Produto in 'Controller\Controller.Produto.pas',
  Model.Entity.Produto in 'Model\Entity\Model.Entity.Produto.pas',
  Model.DaoGeneric in 'Model\Connection\Model.DaoGeneric.pas',
  SimpleTypes in 'modules\simpleorm\SimpleTypes.pas';

var
  App : THorse;

begin
  App := THorse.Create(9000);
  App.Use(JHonson);
  App.Use(CORS);

  Controller.Produto.Registry(App);

  App.Start;
end.
