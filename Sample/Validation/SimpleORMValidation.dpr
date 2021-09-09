program SimpleORMValidation;

uses
  Vcl.Forms,
  Winapi.Windows,
  ufCliente in 'View\ufCliente.pas' {fCliente},
  Entidade.Cliente in 'Entidade\Entidade.Cliente.pas',
  ufBase in 'Base\View\ufBase.pas' {fBase},
  uExport in 'Export\uExport.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := IsDebuggerPresent;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfCliente, fCliente);
  Application.Run;
end.
