unit SimpleDAOSQLAttribute;

interface

uses
  SimpleInterface;

Type
  TSimpleDAOSQLAttribute<T : class> = class(TInterfacedObject,
    iSimpleDAOSQLAttribute<T>)
  private
    [weak]
    FParent: iSimpleDAO<T>;
    FFields: String;
    FWhere: String;
    FOrderBy: String;
    FGroupBy: String;
    FJoin : String;
  public
    constructor Create(Parent: iSimpleDAO<T>);
    destructor Destroy; override;
    class function New(Parent: iSimpleDAO<T>): iSimpleDAOSQLAttribute<T>;
    function Fields(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function Where(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function OrderBy(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function GroupBy (aSQL : String) : iSimpleDAOSQLAttribute<T>; overload;
    function Join (aSQL : String) : iSimpleDAOSQLAttribute<T>; overload;
    function Join : String; overload;
    function Clear : iSimpleDAOSQLAttribute<T>;
    function Fields: String; overload;
    function Where: String; overload;
    function OrderBy: String; overload;
    function GroupBy : String; overload;
    function &End: iSimpleDAO<T>;
  end;

implementation

{ TSimpleDAOSQLAttribute<T> }

function TSimpleDAOSQLAttribute<T>.&End: iSimpleDAO<T>;
begin
  Result := FParent;
end;

function TSimpleDAOSQLAttribute<T>.Fields: String;
begin
  Result := FFields;
end;

function TSimpleDAOSQLAttribute<T>.GroupBy: String;
begin
  Result := FGroupBy;
end;

function TSimpleDAOSQLAttribute<T>.Join: String;
begin
  Result := FJoin;
end;

function TSimpleDAOSQLAttribute<T>.Join(
  aSQL: String): iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FJoin := FJoin + ' ' + aSQL;
end;

function TSimpleDAOSQLAttribute<T>.GroupBy(
  aSQL: String): iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FGroupBy := FGroupBy + ' ' + aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Clear: iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FFields := '';
  FWhere := '';
  FOrderBy := '';
  FGroupBy := '';
  FJoin := '';
end;

constructor TSimpleDAOSQLAttribute<T>.Create(Parent: iSimpleDAO<T>);
begin
  FParent := Parent;
end;

destructor TSimpleDAOSQLAttribute<T>.Destroy;
begin

  inherited;
end;

function TSimpleDAOSQLAttribute<T>.Fields(aSQL: String)
  : iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FFields := FFields + ' ' + aSQL;
end;

class function TSimpleDAOSQLAttribute<T>.New(Parent: iSimpleDAO<T>)
  : iSimpleDAOSQLAttribute<T>;
begin
  Result := Self.Create(Parent);
end;

function TSimpleDAOSQLAttribute<T>.OrderBy: String;
begin
  Result := FOrderBy;
end;

function TSimpleDAOSQLAttribute<T>.OrderBy(aSQL: String)
  : iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FOrderBy := FOrderBy + ' ' + aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Where(aSQL: String)
  : iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FWhere := FWhere + ' ' + aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Where: String;
begin
  Result := FWhere;
end;

end.
