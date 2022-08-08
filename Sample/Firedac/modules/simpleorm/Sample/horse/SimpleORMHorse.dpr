program SimpleORMHorse;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Horse,
  Horse.Jhonson,
  Horse.CORS,
  Model.Connection in 'Model\Connection\Model.Connection.pas',
  DataSetConverter4D.Helper in 'DataSetConverter4D.Helper.pas',
  DataSetConverter4D.Impl in 'DataSetConverter4D.Impl.pas',
  DataSetConverter4D in 'DataSetConverter4D.pas',
  DataSetConverter4D.Util in 'DataSetConverter4D.Util.pas',
  Controller.Produto in 'Controller\Controller.Produto.pas',
  Model.Entity.Produto in 'Model\Entity\Model.Entity.Produto.pas',
  Model.DaoGeneric in 'Model\Connection\Model.DaoGeneric.pas';

var
  App : THorse;

begin
  App := THorse.Create(9000);
  App.Use(JHonson);
  App.Use(CORS);

  Controller.Produto.Registry(App);

  App.Start;
end.
