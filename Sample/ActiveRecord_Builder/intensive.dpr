program intensive;

uses
  Vcl.Forms,
  intensive.Model.Entity.Cliente in 'model\entity\intensive.Model.Entity.Cliente.pas',
  intensive.Resources.Interfaces in 'resources\intensive.Resources.Interfaces.pas',
  intensive.Resources.Conexao in 'resources\intensive.Resources.Conexao.pas',
  intensive.Services.Generic in 'services\intensive.Services.Generic.pas',
  intensive.view.Editar in 'view\intensive.view.Editar.pas' {frmEditar},
  intensive.View.Principal in 'view\intensive.View.Principal.pas' {frmListarClientes},
  intensive.Controller.DTO.Cliente in 'dto\intensive.Controller.DTO.Cliente.pas',
  intensive.Controller in 'controller\intensive.Controller.pas',
  intensive.Controller.Interfaces in 'controller\intensive.Controller.Interfaces.pas',
  intensive.Controller.DTO.Interfaces in 'dto\intensive.Controller.DTO.Interfaces.pas';
//  intensive.Controller.DTO.Endereco in 'dto\intensive.Controller.DTO.Endereco.pas';
//  intensive.Model.Entity.Endereco in 'model\entity\intensive.Model.Entity.Endereco.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmListarClientes, frmListarClientes);
  Application.CreateForm(TfrmListarClientes, frmListarClientes);
  Application.Run;
end.
