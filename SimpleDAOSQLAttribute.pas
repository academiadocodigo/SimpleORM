unit SimpleDAOSQLAttribute;

interface

uses
  SimpleInterface;

Type
  TSimpleDAOSQLAttribute<T> = class(TInterfacedObject,
    iSimpleDAOSQLAttribute<T>)
  private
    [weak]
    FParent: iSimpleDAO<T>;
    FFields: String;
    FWhere: String;
    FOrderBy: String;
    FGroupBy: String;
  public
    constructor Create(Parent: iSimpleDAO<T>);
    destructor Destroy; override;
    class function New(Parent: iSimpleDAO<T>): iSimpleDAOSQLAttribute<T>;
    function Fields(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function Where(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function OrderBy(aSQL: String): iSimpleDAOSQLAttribute<T>; overload;
    function GroupBy (aSQL : String) : iSimpleDAOSQLAttribute<T>; overload;
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

function TSimpleDAOSQLAttribute<T>.GroupBy(
  aSQL: String): iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FGroupBy := aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Clear: iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FFields := '';
  FWhere := '';
  FOrderBy := '';
  FGroupBy := '';
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
  FFields := aSQL;
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
  FOrderBy := aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Where(aSQL: String)
  : iSimpleDAOSQLAttribute<T>;
begin
  Result := Self;
  FWhere := aSQL;
end;

function TSimpleDAOSQLAttribute<T>.Where: String;
begin
  Result := FWhere;
end;

end.
