program SimpleORMFieldMapping;

// -----------------------------------------------------------------------------
// Sample da correcao da issue #83 (Field Map Error).
//
// Demonstra o mapeamento DataSet -> entidade com tipos mistos:
//   TDate, Boolean (vindo de tinyint/integer), Double e Int64,
// incluindo o caso de colunas NULL — que antes gerava erro de conversao.
//
// Usa TFDMemTable (FireDAC em memoria) para simular o retorno de um SELECT
// no MySQL, sem precisar de banco real. O caminho exercitado e o mesmo do
// TSimpleDAO<T>.Find(Lista): TSimpleRTTI<T>.DataSetToEntityList.
//
// Ajuste: nenhum ajuste de banco necessario — roda direto.
// -----------------------------------------------------------------------------

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.DateUtils,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  SimpleAttributes in '..\..\src\SimpleAttributes.pas',
  SimpleTypes in '..\..\src\SimpleTypes.pas',
  SimpleInterface in '..\..\src\SimpleInterface.pas',
  SimpleRTTIHelper in '..\..\src\SimpleRTTIHelper.pas',
  SimpleRTTI in '..\..\src\SimpleRTTI.pas';

type
  // Entidade equivalente ao TBYQConfig reportado na issue #83
  [Tabela('BYQCONFIG')]
  TBYQConfig = class
  private
    FID: Integer;
    FFactoryID: String;
    FTestDate: TDate;
    FPressMax: Double;
    FPressMin: Double;
    FIsTestFinished: Boolean;
  published
    [Campo('ID'), PK]
    property ID: Integer read FID write FID;
    [Campo('FACTORYID'), NotNull, Format(60)]
    property FactoryID: String read FFactoryID write FFactoryID;
    [Campo('TESTDATE')]
    property TestDate: TDate read FTestDate write FTestDate;
    [Campo('PRESSMAX')]
    property PressMax: Double read FPressMax write FPressMax;
    [Campo('PRESSMIN')]
    property PressMin: Double read FPressMin write FPressMin;
    [Campo('ISTESTFINISHED')]
    property IsTestFinished: Boolean read FIsTestFinished write FIsTestFinished;
  end;

function CriarDataSetSimulado: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Result.FieldDefs.Add('ID', ftInteger);
  Result.FieldDefs.Add('FACTORYID', ftString, 60);
  Result.FieldDefs.Add('TESTDATE', ftDate);
  Result.FieldDefs.Add('PRESSMAX', ftFloat);
  Result.FieldDefs.Add('PRESSMIN', ftFloat);
  Result.FieldDefs.Add('ISTESTFINISHED', ftInteger); // MySQL tinyint(1)
  Result.CreateDataSet;

  // Registro completo
  Result.Append;
  Result.FieldByName('ID').AsInteger := 1;
  Result.FieldByName('FACTORYID').AsString := 'FABRICA-A';
  Result.FieldByName('TESTDATE').AsDateTime := EncodeDate(2026, 5, 23);
  Result.FieldByName('PRESSMAX').AsFloat := 12.75;
  Result.FieldByName('PRESSMIN').AsFloat := 3.10;
  Result.FieldByName('ISTESTFINISHED').AsInteger := 1;
  Result.Post;

  // Registro com colunas NULL (antes do fix isso quebrava o Find)
  Result.Append;
  Result.FieldByName('ID').AsInteger := 2;
  Result.FieldByName('FACTORYID').AsString := 'FABRICA-B';
  Result.FieldByName('TESTDATE').Clear;
  Result.FieldByName('PRESSMAX').Clear;
  Result.FieldByName('PRESSMIN').Clear;
  Result.FieldByName('ISTESTFINISHED').Clear;
  Result.Post;

  Result.First;
end;

var
  LDataSet: TFDMemTable;
  LLista: TObjectList<TBYQConfig>;
  LItem: TBYQConfig;
begin
  ReportMemoryLeaksOnShutdown := True;
  try
    Writeln('=== SimpleORM - Field Mapping (issue #83) ===');
    Writeln;

    LDataSet := CriarDataSetSimulado;
    try
      LLista := TObjectList<TBYQConfig>.Create;
      try
        // Mesmo caminho interno de TSimpleDAO<T>.Find(Lista)
        TSimpleRTTI<TBYQConfig>.New(nil).DataSetToEntityList(LDataSet, LLista);

        Writeln('Registros mapeados: ', LLista.Count);
        Writeln;
        for LItem in LLista do
        begin
          Writeln('ID............: ', LItem.ID);
          Writeln('FactoryID.....: ', LItem.FactoryID);
          Writeln('TestDate......: ', DateToStr(LItem.TestDate));
          Writeln('PressMax......: ', FormatFloat('0.00', LItem.PressMax));
          Writeln('PressMin......: ', FormatFloat('0.00', LItem.PressMin));
          Writeln('IsTestFinished: ', BoolToStr(LItem.IsTestFinished, True));
          Writeln('----------------------------------------');
        end;

        Writeln;
        Writeln('OK: mapeamento concluido sem "Field Map Error".');
      finally
        LLista.Free;
      end;
    finally
      LDataSet.Free;
    end;
  except
    on E: Exception do
      Writeln('ERRO: ', E.ClassName, ': ', E.Message);
  end;

  Writeln;
  Writeln('Pressione ENTER para sair...');
  Readln;
end.
