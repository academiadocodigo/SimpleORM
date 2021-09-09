program SimpleORM_RestDW;

uses
  Vcl.Forms,
  Unit8 in 'Unit8.pas' {Form8},
  Entidade.Pedido in '..\Entidades\Entidade.Pedido.pas',
  SimpleAttributes in '..\..\SimpleAttributes.pas',
  SimpleDAO in '..\..\SimpleDAO.pas',
  SimpleDAOSQLAttribute in '..\..\SimpleDAOSQLAttribute.pas',
  SimpleEntity in '..\..\SimpleEntity.pas',
  SimpleInterface in '..\..\SimpleInterface.pas',
  SimpleQueryFiredac in '..\..\SimpleQueryFiredac.pas',
  SimpleQueryRestDW in '..\..\SimpleQueryRestDW.pas',
  SimpleQueryZeos in '..\..\SimpleQueryZeos.pas',
  SimpleRTTI in '..\..\SimpleRTTI.pas',
  SimpleRTTIHelper in '..\..\SimpleRTTIHelper.pas',
  SimpleSQL in '..\..\SimpleSQL.pas',
  SimpleUtil in '..\..\SimpleUtil.pas',
  SimpleValidator in '..\..\SimpleValidator.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
