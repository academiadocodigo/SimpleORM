unit SimpleQueryUnidac;

// -----------------------------------------------------------------------------
// Este driver depende da biblioteca externa UniDAC (Devart) — unit Uni. Para
// evitar que o package SimpleORM.dpk falhe ao compilar em ambientes sem o UniDAC
// instalado, todo o corpo do unit fica protegido pelo simbolo USE_UNIDAC.
//
// Para habilitar o driver, defina USE_UNIDAC nas opcoes do projeto/package
// (Project > Options > Delphi Compiler > Conditional defines).
// -----------------------------------------------------------------------------

{$IFDEF USE_UNIDAC}

interface

uses
  System.Classes,
  Data.DB,
  Uni,
  SimpleInterface,
  SimpleTypes;

type
  TSimpleQueryUniDac = class(TInterfacedObject, iSimpleQuery)
  private
    FConnection : TUniConnection;
    FQuery : TUniQuery;
    FParams : TParams;
    FSQLType : TSQLType;
  public
    constructor Create(aConnection : TUniConnection; aSQLType : TSQLType = TSQLType.Firebird);
    destructor Destroy; override;
    class function New(aConnection : TUniConnection; aSQLType : TSQLType = TSQLType.Firebird) : iSimpleQuery;

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

{ TSimpleQueryUniDac }

constructor TSimpleQueryUniDac.Create(aConnection: TUniConnection; aSQLType: TSQLType = TSQLType.Firebird);
begin
  FQuery := TUniQuery.Create(nil);
  FConnection := aConnection;
  FQuery.Connection := FConnection;
  FSQLType := aSQLType;
end;

function TSimpleQueryUniDac.DataSet: TDataSet;
begin
   Result := TDataSet(FQuery);
end;

destructor TSimpleQueryUniDac.Destroy;
begin
  FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited;
end;

function TSimpleQueryUniDac.ExecSQL: iSimpleQuery;
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

class function TSimpleQueryUniDac.New(aConnection: TUniConnection; aSQLType: TSQLType): iSimpleQuery;
begin
  Result := Self.Create(aConnection, aSQLType);
end;

function TSimpleQueryUniDac.Open(aSQL: String): iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.SQL.Text := aSQL;
  FQuery.Open;
end;

function TSimpleQueryUniDac.Open: iSimpleQuery;
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

function TSimpleQueryUniDac.Params: TParams;
begin
  if not Assigned(FParams) then
  begin
    FParams := TParams.Create(nil);
    FParams.Assign(FQuery.Params);
  end;
  Result := FParams;
end;

function TSimpleQueryUniDac.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TSimpleQueryUniDac.EndTransaction: iSimpleQuery;
begin
  Result := Commit;
end;

function TSimpleQueryUniDac.StartTransaction: iSimpleQuery;
begin
  Result := Self;
  if not FConnection.InTransaction then
    FConnection.StartTransaction;
end;

function TSimpleQueryUniDac.Commit: iSimpleQuery;
begin
  Result := Self;
  if FConnection.InTransaction then
    FConnection.Commit;
end;

function TSimpleQueryUniDac.Rollback: iSimpleQuery;
begin
  Result := Self;
  if FConnection.InTransaction then
    FConnection.Rollback;
end;

function TSimpleQueryUniDac.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

function TSimpleQueryUniDac.SQLType: TSQLType;
begin
  Result := FSQLType;
end;

function TSimpleQueryUniDac.RowsAffected: Integer;
begin
  Result := FQuery.RowsAffected;
end;

{$ELSE}

interface

implementation

{$ENDIF USE_UNIDAC}

end.
