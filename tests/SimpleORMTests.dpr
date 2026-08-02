program SimpleORMTests;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  TestFramework,
  TextTestRunner,
  SimpleAttributes in '..\src\SimpleAttributes.pas',
  SimpleTypes in '..\src\SimpleTypes.pas',
  SimpleInterface in '..\src\SimpleInterface.pas',
  SimpleRTTIHelper in '..\src\SimpleRTTIHelper.pas',
  SimpleRTTI in '..\src\SimpleRTTI.pas',
  SimpleSQL in '..\src\SimpleSQL.pas',
  SimpleValidator in '..\src\SimpleValidator.pas',
  SimpleSerializer in '..\src\SimpleSerializer.pas',
  TestEntities in 'Entities\TestEntities.pas',
  TestSimpleAttributes in 'TestSimpleAttributes.pas',
  TestSimpleRTTIHelper in 'TestSimpleRTTIHelper.pas',
  TestSimpleSQL in 'TestSimpleSQL.pas',
  TestSimpleValidator in 'TestSimpleValidator.pas',
  TestSimpleSerializer in 'TestSimpleSerializer.pas',
  SimpleMigration in '..\src\SimpleMigration.pas',
  TestSimpleMigration in 'TestSimpleMigration.pas',
  SimpleMCPServer in '..\src\SimpleMCPServer.pas',
  SimpleMCPTypes in '..\src\SimpleMCPTypes.pas',
  TestSimpleMCPServer in 'TestSimpleMCPServer.pas',
  SimpleAIAttributes in '..\src\SimpleAIAttributes.pas',
  SimpleAIProcessor in '..\src\SimpleAIProcessor.pas',
  SimpleAIClient in '..\src\SimpleAIClient.pas',
  MockAIClient in 'Mocks\MockAIClient.pas',
  TestSimpleAIProcessor in 'TestSimpleAIProcessor.pas',
  SimpleAIQuery in '..\src\SimpleAIQuery.pas',
  TestSimpleAIQuery in 'TestSimpleAIQuery.pas',
  SimpleRules in '..\src\SimpleRules.pas',
  SimpleSkill in '..\src\SimpleSkill.pas',
  SimpleAgent in '..\src\SimpleAgent.pas',
  TestSimpleRules in 'TestSimpleRules.pas',
  TestSimpleSkill in 'TestSimpleSkill.pas',
  TestSimpleAgent in 'TestSimpleAgent.pas',
  SimpleAISkill in '..\src\SimpleAISkill.pas',
  TestSimpleAISkill in 'TestSimpleAISkill.pas',
  SimpleQuerySupabase in '..\src\SimpleQuerySupabase.pas',
  TestSimpleQuerySupabase in 'TestSimpleQuerySupabase.pas',
  SimpleSupabaseAuth in '..\src\SimpleSupabaseAuth.pas',
  TestSimpleSupabaseAuth in 'TestSimpleSupabaseAuth.pas',
  SimpleSupabaseRealtime in '..\src\SimpleSupabaseRealtime.pas',
  TestSimpleSupabaseRealtime in 'TestSimpleSupabaseRealtime.pas',
  SimpleDataMigration in '..\src\SimpleDataMigration.pas',
  TestSimpleDataMigration in 'TestSimpleDataMigration.pas',
  TestSimpleRTTIMapping in 'TestSimpleRTTIMapping.pas';

var
  ExitBehavior: TRunnerExitBehavior;
begin
  ReportMemoryLeaksOnShutdown := True;
  ExitBehavior := rxbHaltOnFailures;
  try
    if FindCmdLineSwitch('pause') then
      ExitBehavior := rxbPause;
    TextTestRunner.RunRegisteredTests(ExitBehavior);
  except
    on E: Exception do
    begin
      Writeln('Erro: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
