unit SimpleQueryUnidac;

interface

uses
  System.Classes,
  Data.DB,
  Uni,
  SimpleInterface;

type
  TSimpleQueryUniDac = class(TInterfacedObject, iSimpleQuery)
  private
    FConnection : TUniConnection;
    FQuery : TUniQuery;
    FParams : TParams;
  public
    constructor Create(aConnection : TUniConnection);
    destructor Destroy; override;
    class function New(aConnection : TUniConnection) : iSimpleQuery;

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

{ TSimpleQueryUniDac }

constructor TSimpleQueryUniDac.Create(aConnection: TUniConnection);
begin
  FQuery := TUniQuery.Create(nil);
  FConnection := aConnection;
  FQuery.Connection := FConnection;
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
  FQuery.ExecSQL;

  if Assigned(FParams) then
    FreeAndNil(FParams);
end;

class function TSimpleQueryUniDac.New(aConnection: TUniConnection): iSimpleQuery;
begin
  Result := Self.Create(aConnection);
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

end.
