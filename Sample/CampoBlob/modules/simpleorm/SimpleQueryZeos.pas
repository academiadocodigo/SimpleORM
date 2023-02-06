Unit SimpleQueryZeos;

interface

uses
  SimpleInterface,  ZAbstractConnection, ZConnection,
  ZAbstractRODataset, ZAbstractDataset, ZAbstractTable, ZDataset, System.Classes, Data.DB;

Type
  TSimpleQueryZeos = class(TInterfacedObject, iSimpleQuery)
    private
      FConnection : TZConnection;
      FQuery : TZQuery;
      FParams : TParams;
    public
      constructor Create(aConnection : TZConnection);
      destructor Destroy; override;
      class function New(aConnection : TZConnection) : iSimpleQuery;
      function SQL : TStrings;
      function Params : TParams;
      function ExecSQL : iSimpleQuery;
      function DataSet : TDataSet;
      function Open(aSQL : String) : iSimpleQuery; overload;
      function Open : iSimpleQuery; overload;
  end;

implementation

uses
  System.SysUtils;

{ TSimpleQuery<T> }

constructor TSimpleQueryZeos.Create(aConnection : TZConnection);
begin
  FQuery := TZQuery.Create(nil);
  FConnection := aConnection;
  FQuery.Connection := FConnection;
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
var
  a: string;
begin
  Result := Self;
  FQuery.Params.Assign(FParams);
  FQuery.Prepare;
  FQuery.ExecSQL;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

class function TSimpleQueryZeos.New(aConnection : TZConnection): iSimpleQuery;
begin
  Result := Self.Create(aConnection);
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

end.
