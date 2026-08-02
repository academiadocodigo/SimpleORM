unit TestSimpleRTTIMapping;

// Testes para TSimpleRTTI<T>.DataSetToEntityList / DataSetToEntity.
// Reproduz a issue #83 (Field Map Error): mapeamento de DataSet -> entidade
// com tipos TDate, Boolean, Double e Int64, incluindo colunas NULL.
// Usa TFDMemTable (FireDAC em memoria) — NAO depende de banco real.

interface

uses
  TestFramework,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  SimpleInterface,
  SimpleRTTI,
  TestEntities;

type
  TTestRTTIMapping = class(TTestCase)
  private
    FMem: TFDMemTable;
    procedure BuildTable;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDataSetToEntityList_MixedTypes_ShouldMapAllFields;
    procedure TestDataSetToEntityList_BooleanFromInteger_ShouldMap;
    procedure TestDataSetToEntityList_NullFields_ShouldNotRaise;
    procedure TestDataSetToEntityList_Int64_ShouldMapLargeValue;
    procedure TestDataSetToEntityList_TDate_ShouldMapDate;
  end;

implementation

{ TTestRTTIMapping }

procedure TTestRTTIMapping.BuildTable;
begin
  FMem.Close;
  FMem.FieldDefs.Clear;
  FMem.FieldDefs.Add('ID', ftInteger);
  FMem.FieldDefs.Add('FACTORYID', ftString, 60);
  FMem.FieldDefs.Add('TESTDATE', ftDate);
  FMem.FieldDefs.Add('PRESSMAX', ftFloat);
  FMem.FieldDefs.Add('PRESSMIN', ftFloat);
  FMem.FieldDefs.Add('ISTESTFINISHED', ftInteger); // MySQL tinyint(1)
  FMem.FieldDefs.Add('BIGCOUNTER', ftLargeint);
  FMem.CreateDataSet;
end;

procedure TTestRTTIMapping.SetUp;
begin
  inherited;
  FMem := TFDMemTable.Create(nil);
  BuildTable;
end;

procedure TTestRTTIMapping.TearDown;
begin
  FreeAndNil(FMem);
  inherited;
end;

procedure TTestRTTIMapping.TestDataSetToEntityList_MixedTypes_ShouldMapAllFields;
var
  LList: TObjectList<TBYQConfigTest>;
begin
  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 10;
  FMem.FieldByName('FACTORYID').AsString := 'F-100';
  FMem.FieldByName('TESTDATE').AsDateTime := EncodeDate(2026, 5, 23);
  FMem.FieldByName('PRESSMAX').AsFloat := 12.5;
  FMem.FieldByName('PRESSMIN').AsFloat := 3.2;
  FMem.FieldByName('ISTESTFINISHED').AsInteger := 1;
  FMem.FieldByName('BIGCOUNTER').AsLargeInt := 5000000000;
  FMem.Post;

  LList := TObjectList<TBYQConfigTest>.Create;
  try
    TSimpleRTTI<TBYQConfigTest>.New(nil).DataSetToEntityList(FMem, LList);
    CheckEquals(1, LList.Count, 'Deveria mapear 1 registro');
    CheckEquals(10, LList[0].ID, 'ID');
    CheckEquals('F-100', LList[0].FACTORYID, 'FACTORYID');
    CheckEquals(12.5, LList[0].PRESSMAX, 0.001, 'PRESSMAX');
    CheckEquals(3.2, LList[0].PRESSMIN, 0.001, 'PRESSMIN');
    CheckTrue(LList[0].ISTESTFINISHED, 'ISTESTFINISHED deveria ser True');
    CheckEquals(Int64(5000000000), LList[0].BIGCOUNTER, 'BIGCOUNTER');
  finally
    LList.Free;
  end;
end;

procedure TTestRTTIMapping.TestDataSetToEntityList_BooleanFromInteger_ShouldMap;
var
  LList: TObjectList<TBYQConfigTest>;
begin
  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 1;
  FMem.FieldByName('FACTORYID').AsString := 'A';
  FMem.FieldByName('ISTESTFINISHED').AsInteger := 0;
  FMem.Post;

  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 2;
  FMem.FieldByName('FACTORYID').AsString := 'B';
  FMem.FieldByName('ISTESTFINISHED').AsInteger := 1;
  FMem.Post;

  LList := TObjectList<TBYQConfigTest>.Create;
  try
    TSimpleRTTI<TBYQConfigTest>.New(nil).DataSetToEntityList(FMem, LList);
    CheckEquals(2, LList.Count, 'Deveria mapear 2 registros');
    CheckFalse(LList[0].ISTESTFINISHED, 'Registro 1: False');
    CheckTrue(LList[1].ISTESTFINISHED, 'Registro 2: True');
  finally
    LList.Free;
  end;
end;

procedure TTestRTTIMapping.TestDataSetToEntityList_NullFields_ShouldNotRaise;
var
  LList: TObjectList<TBYQConfigTest>;
begin
  // Todas as colunas nao-chave ficam NULL: antes do fix, TDate/Double/Boolean
  // NULL provocavam erro de conversao ("Field Map Error").
  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 99;
  FMem.FieldByName('FACTORYID').AsString := 'ONLY-KEY';
  FMem.FieldByName('TESTDATE').Clear;
  FMem.FieldByName('PRESSMAX').Clear;
  FMem.FieldByName('PRESSMIN').Clear;
  FMem.FieldByName('ISTESTFINISHED').Clear;
  FMem.FieldByName('BIGCOUNTER').Clear;
  FMem.Post;

  LList := TObjectList<TBYQConfigTest>.Create;
  try
    TSimpleRTTI<TBYQConfigTest>.New(nil).DataSetToEntityList(FMem, LList);
    CheckEquals(1, LList.Count, 'Deveria mapear 1 registro mesmo com NULLs');
    CheckEquals(99, LList[0].ID, 'ID deveria ser preservado');
    CheckEquals('ONLY-KEY', LList[0].FACTORYID, 'FACTORYID deveria ser preservado');
    // Campos NULL permanecem com valor default do objeto (0 / False)
    CheckEquals(0.0, LList[0].PRESSMAX, 0.001, 'PRESSMAX default');
    CheckFalse(LList[0].ISTESTFINISHED, 'ISTESTFINISHED default False');
  finally
    LList.Free;
  end;
end;

procedure TTestRTTIMapping.TestDataSetToEntityList_Int64_ShouldMapLargeValue;
var
  LList: TObjectList<TBYQConfigTest>;
  LBig: Int64;
begin
  LBig := 9000000000; // > MaxInt, so AsLargeInt e obrigatorio
  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 1;
  FMem.FieldByName('FACTORYID').AsString := 'BIG';
  FMem.FieldByName('BIGCOUNTER').AsLargeInt := LBig;
  FMem.Post;

  LList := TObjectList<TBYQConfigTest>.Create;
  try
    TSimpleRTTI<TBYQConfigTest>.New(nil).DataSetToEntityList(FMem, LList);
    CheckEquals(LBig, LList[0].BIGCOUNTER, 'Int64 grande deveria ser mapeado sem overflow');
  finally
    LList.Free;
  end;
end;

procedure TTestRTTIMapping.TestDataSetToEntityList_TDate_ShouldMapDate;
var
  LList: TObjectList<TBYQConfigTest>;
  LExpected: TDate;
begin
  LExpected := EncodeDate(2026, 12, 31);
  FMem.Append;
  FMem.FieldByName('ID').AsInteger := 1;
  FMem.FieldByName('FACTORYID').AsString := 'DT';
  FMem.FieldByName('TESTDATE').AsDateTime := LExpected;
  FMem.Post;

  LList := TObjectList<TBYQConfigTest>.Create;
  try
    TSimpleRTTI<TBYQConfigTest>.New(nil).DataSetToEntityList(FMem, LList);
    CheckEquals(YearOf(LExpected), YearOf(LList[0].TESTDATE), 'Ano');
    CheckEquals(MonthOf(LExpected), MonthOf(LList[0].TESTDATE), 'Mes');
    CheckEquals(DayOf(LExpected), DayOf(LList[0].TESTDATE), 'Dia');
  finally
    LList.Free;
  end;
end;

initialization
  RegisterTest('RTTI.DataSetMapping', TTestRTTIMapping.Suite);

end.
