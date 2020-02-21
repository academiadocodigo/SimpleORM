unit SimpleQueryDbExpress;

interface

uses
  SimpleInterface,
  System.Classes,
  Data.DB,
  SysUtils,
  Data.FMTBcd,
  Data.SqlExpr,
  Datasnap.Provider,
  Datasnap.DBClient,
  Data.DBXFirebird;

Type
  TSimpleQueryDbExpress = class(TInterfacedObject, iSimpleQuery)
  private
    FConnection: TSQLConnection;
    FQuery: TSqlQuery;
    FParams: TParams;
  public
    constructor Create(aConnection: TSQLConnection);
    destructor Destroy; override;
    class function New(aConnection: TSQLConnection): iSimpleQuery;
    function SQL: TStrings;
    function Params: TParams;
    function ExecSQL: iSimpleQuery;
    function DataSet: TDataSet;
    function Open(aSQL: String): iSimpleQuery; overload;
    function Open: iSimpleQuery; overload;
  end;

implementation

{ TSimpleQuery<T> }

constructor TSimpleQueryDbExpress.Create(aConnection: TSQLConnection);
begin
  FQuery := TSqlQuery.Create(nil);
  FConnection := aConnection;
  FQuery.SQLConnection := FConnection;
end;

function TSimpleQueryDbExpress.DataSet: TDataSet;
begin
  Result := TDataSet(FQuery);
end;

destructor TSimpleQueryDbExpress.Destroy;
begin
  FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited;
end;

function TSimpleQueryDbExpress.ExecSQL: iSimpleQuery;
begin
  Result := Self;
  FQuery.Params.Assign(FParams);
  FQuery.ExecSQL;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

class function TSimpleQueryDbExpress.New(aConnection: TSQLConnection): iSimpleQuery;
begin
  Result := Self.Create(aConnection);
end;

function TSimpleQueryDbExpress.Open: iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;

  if Assigned(FParams) then
    FQuery.Params.Assign(FParams);

  FQuery.Open;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

function TSimpleQueryDbExpress.Open(aSQL: String): iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Open;
end;

function TSimpleQueryDbExpress.Params: TParams;
begin
  if not Assigned(FParams) then
  begin
    FParams := TParams.Create(nil);
    FParams.Assign(FQuery.Params);
  end;
  Result := FParams;
end;

function TSimpleQueryDbExpress.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

end.
