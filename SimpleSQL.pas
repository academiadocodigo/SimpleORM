unit SimpleSQL;
interface
uses
  SimpleInterface, SimpleTypes;
Type
  TSimpleSQL<T : class, constructor> = class(TInterfacedObject, iSimpleSQL<T>)
    private
      FSQLType : TSQLType;
      FInstance : T;
      FFields : String;
      FWhere : String;
      FOrderBy : String;
      FGroupBy : String;
      FJoin : String;
      FLimit : Integer;
      FPage : Integer;
    public
      constructor Create(aInstance : T);
      destructor Destroy; override;
      class function New(aInstance : T) : iSimpleSQL<T>;
      function SQLType(SQLType : TSQLType) : iSimpleSQL<T>;
      function Insert (var aSQL : String) : iSimpleSQL<T>;
      function Update (var aSQL : String) : iSimpleSQL<T>;
      function Delete (var aSQL : String) : iSimpleSQL<T>;
      function Select (var aSQL : String) : iSimpleSQL<T>;
      function SelectId(var aSQL: String): iSimpleSQL<T>;
      function Fields (aSQL : String) : iSimpleSQL<T>;
      function Where (aSQL : String) : iSimpleSQL<T>;
      function OrderBy (aSQL : String) : iSimpleSQL<T>;
      function GroupBy (aSQL : String) : iSimpleSQL<T>;
      function Join (aSQL : String) : iSimpleSQL<T>;
      function LastID (var aSQL : String) : iSimpleSQL<T>;
      function LastRecord (var aSQL : String) : iSimpleSQL<T>;
      function Paginate(aLimit, aPage : Integer) : iSimpleSQL<T>;
  end;
implementation

uses
  SimpleRTTI, System.Generics.Collections, System.SysUtils;
{ TSimpleSQL<T> }

constructor TSimpleSQL<T>.Create(aInstance : T);
begin
  FInstance := aInstance;
end;

function TSimpleSQL<T>.Delete(var aSQL: String): iSimpleSQL<T>;
var
  aClassName, aWhere : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance)
    .TableName(aClassName)
    .Where(aWhere);
  aSQL := aSQL + 'DELETE FROM ' + aClassName;
  aSQL := aSQL + ' WHERE ' + aWhere;
end;

destructor TSimpleSQL<T>.Destroy;
begin
  inherited;
end;

function TSimpleSQL<T>.Fields(aSQL: String): iSimpleSQL<T>;
begin
  Result := Self;
  if Trim(aSQL) <> '' then
    FFields := aSQL;
end;

function TSimpleSQL<T>.GroupBy(aSQL: String): iSimpleSQL<T>;
begin
  Result := Self;
  if Trim(aSQL) <> '' then
    FGroupBy := aSQL;
end;

function TSimpleSQL<T>.Insert(var aSQL: String): iSimpleSQL<T>;
var
  aClassName, aFields, aParam : String;
begin
  Result := Self;
    TSimpleRTTI<T>.New(FInstance)
      .TableName(aClassName)
      .FieldsInsert(aFields)
      .Param(aParam);
    aSQL := aSQL + 'INSERT INTO ' + aClassName;
    aSQL := aSQL + ' (' + aFields + ') ';
    aSQL := aSQL + ' VALUES (' + aParam + ');';
end;

function TSimpleSQL<T>.Join(aSQL: String): iSimpleSQL<T>;
begin
  Result := Self;
  FJoin := aSQL;
end;

function TSimpleSQL<T>.LastID(var aSQL: String): iSimpleSQL<T>;
var
  aClassName, aPK, aFields : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance)
    .TableName(aClassName)
    .PrimaryKey(aPK);

  case FSQLType of
    Firebird:
    begin
      aSQL := aSQL + 'select first(1) ' + aPK;
      aSQL := aSQL + ' from '+ aClassName;
      aSQL := aSQL + ' order by ' + aPK + ' desc';
    end;
    MySQL, SQLite:
    begin
      aSQL := aSQL + 'select ' + aPK;
      aSQL := aSQL + ' from '+ aClassName;
      aSQL := aSQL + ' order by ' + aPK + ' desc limit(1)';
    end;
  end;

end;

function TSimpleSQL<T>.LastRecord(var aSQL: String): iSimpleSQL<T>;
var
  aClassName, aPK, aFields : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance)
    .TableName(aClassName)
    .Fields(aFields)
    .PrimaryKey(aPK);

  case FSQLType of
    Firebird:
    begin
      aSQL := aSQL + 'select first(1) '+aFields;
      aSQL := aSQL + ' from '+ aClassName;
      aSQL := aSQL + ' order by ' + aPK + ' desc';
    end;
    MySQL, SQLite:
    begin
      aSQL := aSQL + 'select '+aFields;
      aSQL := aSQL + ' from '+ aClassName;
      aSQL := aSQL + ' order by ' + aPK + ' desc limit 1';
    end;
  end;
end;

class function TSimpleSQL<T>.New(aInstance : T): iSimpleSQL<T>;
begin
  Result := Self.Create(aInstance);
end;

function TSimpleSQL<T>.OrderBy(aSQL: String): iSimpleSQL<T>;
begin
  Result := Self;
  FOrderBy := aSQL;
end;

function TSimpleSQL<T>.Paginate(aLimit, aPage: Integer): iSimpleSQL<T>;
begin
  Result := Self;

  FLimit := aLimit;
  FPage := aPage;
end;

function TSimpleSQL<T>.Select (var aSQL : String) : iSimpleSQL<T>;
var
  aFields, aClassName : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(nil)
    .Fields(aFields)
    .TableName(aClassName);

  aSQL := aSQL + 'SELECT ';

  if FLimit <> 0 then
    case FSQLType of
      Firebird: aSQL := aSQL + ' FIRST '+FLimit.ToString + ' SKIP ' + IntToStr((FPAGE - 1)*FLimit);
    end;

  if Trim(FFields) <> '' then
    aSQL := aSQL + ' ' + FFields
  else
    aSQL := aSQL + ' ' + aFields;

  aSQL := aSQL + ' FROM ' + aClassName;

  if Trim(FJoin) <> '' then
    aSQL := aSQL + ' ' + FJoin + ' ';
  if Trim(FWhere) <> '' then
    aSQL := aSQL + ' WHERE ' + FWhere;
  if Trim(FGroupBy) <> '' then
    aSQL := aSQL + ' GROUP BY ' + FGroupBy;
  if Trim(FOrderBy) <> '' then
    aSQL := aSQL + ' ORDER BY ' + FOrderBy;

  if FLimit <> 0 then
    case FSQLType of
      SQLite, MySQL: aSQL := aSQL + ' LIMIT '+FLimit.ToString + ' OFFSET ' + IntToStr((FPAGE - 1)*FLimit);
    end;
end;

function TSimpleSQL<T>.SelectId(var aSQL: String): iSimpleSQL<T>;
var
  aFields, aClassName, aWhere : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance)
    .Fields(aFields)
    .TableName(aClassName)
    .Where(aWhere);
  if Trim(FWhere) <> '' then
    aSQL := aSQL + ' WHERE ' + FWhere;

  aSQL := aSQL + ' SELECT ' + aFields;
  aSQL := aSQL + ' FROM ' + aClassName;
  aSQL := aSQL + ' WHERE ' + aWhere;
end;

function TSimpleSQL<T>.SQLType(SQLType: TSQLType): iSimpleSQL<T>;
begin
  Result := Self;

  FSQLType := SQLType;
end;

function TSimpleSQL<T>.Update(var aSQL: String): iSimpleSQL<T>;
var
  ClassName, aUpdate, aWhere : String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance)
    .TableName(ClassName)
    .Update(aUpdate)
    .Where(aWhere);

  aSQL := aSQL + 'UPDATE ' + ClassName;
  aSQL := aSQL + ' SET ' + aUpdate;
  aSQL := aSQL + ' WHERE ' + aWhere;
end;

function TSimpleSQL<T>.Where(aSQL: String): iSimpleSQL<T>;
begin
  Result := Self;
  FWhere := aSQL;
end;
end.
