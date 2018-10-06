program SimpleORM;

uses
  Vcl.Forms,
  SimpleDAO in 'SimpleDAO.pas',
  SimpleInterface in 'SimpleInterface.pas',
  SimpleAttributes in 'SimpleAttributes.pas',
  SimpleRTTI in 'SimpleRTTI.pas',
  SimpleSQL in 'SimpleSQL.pas',
  SimpleQueryFiredac in 'SimpleQueryFiredac.pas',
  SimpleQueryRestDW in 'SimpleQueryRestDW.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.
