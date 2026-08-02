unit SimpleQueryRestDW;

// -----------------------------------------------------------------------------
// Este driver depende da biblioteca externa RestDataware (units uDWConstsData e
// uRESTDWPoolerDB). Para evitar que o package SimpleORM.dpk falhe ao compilar
// em ambientes sem o RestDataware instalado, todo o corpo do unit fica protegido
// pelo simbolo condicional USE_RESTDW.
//
// Para habilitar o driver, defina USE_RESTDW nas opcoes do projeto/package
// (Project > Options > Delphi Compiler > Conditional defines) ou adicione
// {$DEFINE USE_RESTDW} antes de usar este unit.
// -----------------------------------------------------------------------------

{$IFDEF USE_RESTDW}

interface

uses
  SimpleInterface, SimpleTypes, System.Classes, Data.DB, uDWConstsData, uRESTDWPoolerDB;

Type
  TSimpleQueryRestDW<T : class, constructor> = class(TInterfacedObject, iSimpleQuery)
    private
      FConnection : TRESTDWDataBase;
      FQuery : TRESTDWClientSQL;
      FSQLType : TSQLType;
    public
      constructor Create(aConnection : TRESTDWDataBase; aSQLType : TSQLType = TSQLType.Firebird);
      destructor Destroy; override;
      class function New(aConnection : TRESTDWDataBase; aSQLType : TSQLType = TSQLType.Firebird) : iSimpleQuery;
      function SQL : TStrings;
      function Params : TParams;
      function ExecSQL : iSimpleQuery;
      function DataSet : TDataSet;
      function Open(aSQL : String) : iSimpleQuery; overload;
      function Open : iSimpleQuery; overload;
      function &EndTransaction : iSimpleQuery;
      function StartTransaction : iSimpleQuery;
      function Commit : iSimpleQuery;
      function Rollback : iSimpleQuery;
      function InTransaction : Boolean;
      function SQLType : TSQLType;
      function RowsAffected : Integer;
  end;

implementation

uses
  System.SysUtils, SimpleRTTI;

{ TSimpleQuery<T> }

constructor TSimpleQueryRestDW<T>.Create(aConnection : TRESTDWDataBase; aSQLType : TSQLType = TSQLType.Firebird);
var
  aTable : String;
begin
  FConnection := aConnection;
  FQuery := TRESTDWClientSQL.Create(nil);
  FQuery.DataBase := FConnection;
  TSimpleRTTI<T>.New(nil).ClassName(aTable);
  FQuery.AutoCommitData := False;
  FQuery.AutoRefreshAfterCommit := True;
  //FQuery.SetInBlockEvents(false);
  FQuery.UpdateTableName := aTable;
  FSQLType := aSQLType;
end;

function TSimpleQueryRestDW<T>.DataSet: TDataSet;
begin
  Result := FQuery;
end;

destructor TSimpleQueryRestDW<T>.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

function TSimpleQueryRestDW<T>.ExecSQL: iSimpleQuery;
var
  aErro : String;
begin
  Result := Self;
  FQuery.ExecSQL(aErro);
  if aErro <> '' then
    raise Exception.Create(aErro);
  FQuery.ApplyUpdates(aErro);
  if aErro <> '' then
    raise Exception.Create(aErro);
end;

class function TSimpleQueryRestDW<T>.New(aConnection : TRESTDWDataBase; aSQLType : TSQLType): iSimpleQuery;
begin
  Result := Self.Create(aConnection, aSQLType);
end;

function TSimpleQueryRestDW<T>.Open: iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.Open;
end;

function TSimpleQueryRestDW<T>.Open(aSQL: String): iSimpleQuery;
begin
  FQuery.Close;
  Result := Self;
  FQuery.Open(aSQL);
end;

function TSimpleQueryRestDW<T>.Params: TParams;
begin
  Result := FQuery.Params;
end;

function TSimpleQueryRestDW<T>.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TSimpleQueryRestDW<T>.EndTransaction: iSimpleQuery;
begin
  Result := Commit;
end;

function TSimpleQueryRestDW<T>.StartTransaction: iSimpleQuery;
begin
  Result := Self;
  // RestDW does not support explicit transaction control
end;

function TSimpleQueryRestDW<T>.Commit: iSimpleQuery;
begin
  Result := Self;
  // RestDW does not support explicit transaction control
end;

function TSimpleQueryRestDW<T>.Rollback: iSimpleQuery;
begin
  Result := Self;
  // RestDW does not support explicit transaction control
end;

function TSimpleQueryRestDW<T>.InTransaction: Boolean;
begin
  Result := False;
  // RestDW does not support explicit transaction control
end;

function TSimpleQueryRestDW<T>.SQLType: TSQLType;
begin
  Result := FSQLType;
end;

function TSimpleQueryRestDW<T>.RowsAffected: Integer;
begin
  Result := -1;
end;

{$ELSE}

interface

implementation

{$ENDIF USE_RESTDW}

end.
