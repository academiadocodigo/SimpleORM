Unit SimpleQueryZeos;

// -----------------------------------------------------------------------------
// Este driver depende da biblioteca externa Zeos (ZAbstractConnection,
// ZConnection, ZDataset, etc.). Para evitar que o package SimpleORM.dpk falhe ao
// compilar em ambientes sem o Zeos instalado, todo o corpo do unit fica protegido
// pelo simbolo USE_ZEOS.
//
// Para habilitar o driver, defina USE_ZEOS nas opcoes do projeto/package
// (Project > Options > Delphi Compiler > Conditional defines).
// -----------------------------------------------------------------------------

{$IFDEF USE_ZEOS}

interface

uses
  SimpleInterface, SimpleTypes, ZAbstractConnection, ZConnection,
  ZAbstractRODataset, ZAbstractDataset, ZAbstractTable, ZDataset, System.Classes, Data.DB;

Type
  TSimpleQueryZeos = class(TInterfacedObject, iSimpleQuery)
    private
      FConnection : TZConnection;
      FQuery : TZQuery;
      FParams : TParams;
      FSQLType : TSQLType;
    public
      constructor Create(aConnection : TZConnection; aSQLType : TSQLType = TSQLType.Firebird);
      destructor Destroy; override;
      class function New(aConnection : TZConnection; aSQLType : TSQLType = TSQLType.Firebird) : iSimpleQuery;
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
  System.SysUtils;

{ TSimpleQuery<T> }

constructor TSimpleQueryZeos.Create(aConnection : TZConnection; aSQLType : TSQLType = TSQLType.Firebird);
begin
  FQuery := TZQuery.Create(nil);
  FConnection := aConnection;
  FQuery.Connection := FConnection;
  FSQLType := aSQLType;
end;

function TSimpleQueryZeos.DataSet: TDataSet;
begin
  Result := TDataSet(FQuery);
end;

destructor TSimpleQueryZeos.Destroy;
begin
  FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited;
end;

function TSimpleQueryZeos.ExecSQL: iSimpleQuery;
begin
  Result := Self;
  if Assigned(FParams) then
    FQuery.Params.Assign(FParams);

  FQuery.Prepare;

  try
    FQuery.ExecSQL;
  except
    on E: Exception do
    begin
      if FConnection.InTransaction then
        FConnection.Rollback;
      raise;
    end;
  end;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

class function TSimpleQueryZeos.New(aConnection : TZConnection; aSQLType : TSQLType): iSimpleQuery;
begin
  Result := Self.Create(aConnection, aSQLType);
end;

function TSimpleQueryZeos.Open: iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;

  if Assigned(FParams) then
    FQuery.Params.Assign(FParams);

  FQuery.Prepare;
  FQuery.Open;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

function TSimpleQueryZeos.Open(aSQL: String): iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Open;
end;

function TSimpleQueryZeos.Params: TParams;
begin
  if not Assigned(FParams) then
  begin
    FParams := TParams.Create(nil);
    FParams.Assign(FQuery.Params);
  end;
  Result := FParams;
end;

function TSimpleQueryZeos.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TSimpleQueryZeos.EndTransaction: iSimpleQuery;
begin
  Result := Commit;
end;

function TSimpleQueryZeos.StartTransaction: iSimpleQuery;
begin
  Result := Self;
  if not FConnection.InTransaction then
    FConnection.StartTransaction;
end;

function TSimpleQueryZeos.Commit: iSimpleQuery;
begin
  Result := Self;
  if FConnection.InTransaction then
    FConnection.Commit;
end;

function TSimpleQueryZeos.Rollback: iSimpleQuery;
begin
  Result := Self;
  if FConnection.InTransaction then
    FConnection.Rollback;
end;

function TSimpleQueryZeos.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TSimpleQueryZeos.SQLType: TSQLType;
begin
  Result := FSQLType;
end;

function TSimpleQueryZeos.RowsAffected: Integer;
begin
  Result := FQuery.RowsAffected;
end;

{$ELSE}

interface

implementation

{$ENDIF USE_ZEOS}

end.
