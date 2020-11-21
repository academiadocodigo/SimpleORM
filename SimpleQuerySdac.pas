Unit SimpleQuerySdac;

interface

uses
  SimpleInterface, System.Classes, Data.DB,MSAccess;

Type
  TSimpleQuerySdac = class(TInterfacedObject, iSimpleQuery)
    private
      FConnection : TMSConnection;
      FQuery : TMSQuery;
      FParams : TParams;
    public
      constructor Create(aConnection : TMSConnection);
      destructor Destroy; override;
      class function New(aConnection : TMSConnection) : iSimpleQuery;
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

constructor TSimpleQuerySdac.Create(aConnection : TMSConnection);
begin
  FQuery := TMSQuery.Create(nil);
  FConnection := aConnection;
  FQuery.Connection := FConnection;
end;

function TSimpleQuerySdac.DataSet: TDataSet;
begin
  Result := TDataSet(FQuery);
end;

destructor TSimpleQuerySdac.Destroy;
begin
  FreeAndNil(FQuery);
  if Assigned(FParams) then
    FreeAndNil(FParams);
  inherited;
end;

function TSimpleQuerySdac.ExecSQL: iSimpleQuery;
var
  a: string;
begin
  Result := Self;
  if Assigned(FParams) then
      FQuery.Params.Assign(FParams);
  FQuery.Prepare;
  FQuery.ExecSQL;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

class function TSimpleQuerySdac.New(aConnection : TMSConnection): iSimpleQuery;
begin
  Result := Self.Create(aConnection);
end;

function TSimpleQuerySdac.Open: iSimpleQuery;
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

function TSimpleQuerySdac.Open(aSQL: String): iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Open;
end;

function TSimpleQuerySdac.Params: TParams;
begin
  if not Assigned(FParams) then
  begin
    FParams := TParams.Create(nil);
    FParams.Assign(FQuery.Params);
  end;
  Result := FParams;
end;

function TSimpleQuerySdac.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

end.
