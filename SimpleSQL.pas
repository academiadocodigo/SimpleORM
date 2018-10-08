unit SimpleSQL;

interface

uses
  SimpleInterface;

Type
  TSimpleSQL<T : class, constructor> = class(TInterfacedObject, iSimpleSQL<T>)
    private
      FInstance : T;
    public
      constructor Create(aInstance : T);
      destructor Destroy; override;
      class function New(aInstance : T) : iSimpleSQL<T>;
      function Insert (var aSQL : String) : iSimpleSQL<T>;
      function Update (var aSQL : String) : iSimpleSQL<T>;
      function Delete (var aSQL : String) : iSimpleSQL<T>;
      function Select (OrderBy : String; var aSQL : String) : iSimpleSQL<T>;
      function SelectId (var aSQL : String) : iSimpleSQL<T>;
      function SelectWhere (aWhere : String; OrderBy : String ; var aSQL: String) : iSimpleSQL<T>;
  end;

implementation

uses
  SimpleRTTI, System.Generics.Collections;

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
    .ClassName(aClassName)
    .Where(aWhere);

  aSQL := aSQL + 'DELETE FROM ' + aClassName;
  aSQL := aSQL + ' WHERE ' + aWhere;

end;

destructor TSimpleSQL<T>.Destroy;
begin

  inherited;
end;

function TSimpleSQL<T>.Insert(var aSQL: String): iSimpleSQL<T>;
var
  aClassName, aFields, aParam : String;
begin
  Result := Self;

    TSimpleRTTI<T>.New(FInstance)
      .ClassName(aClassName)
      .FieldsInsert(aFields)
      .Param(aParam);

    aSQL := aSQL + 'INSERT INTO ' + aClassName;
    aSQL := aSQL + ' (' + aFields + ') ';
    aSQL := aSQL + ' VALUES (' + aParam + ');';
end;

class function TSimpleSQL<T>.New(aInstance : T): iSimpleSQL<T>;
begin
  Result := Self.Create(aInstance);
end;

function TSimpleSQL<T>.Select (OrderBy : String; var aSQL : String) : iSimpleSQL<T>;
var
  aFields, aClassName : String;
begin
  Result := Self;

  TSimpleRTTI<T>.New(nil)
    .Fields(aFields)
    .ClassName(aClassName);

  aSQL := aSQL + ' SELECT ' + aFields;
  aSQL := aSQL + ' FROM ' + aClassName;
  if OrderBy <> '' then
    aSQL := aSQL + ' ORDER BY ' + OrderBy;
end;

function TSimpleSQL<T>.SelectId(var aSQL: String): iSimpleSQL<T>;
var
  aFields, aClassName, aWhere : String;
begin
  Result := Self;

  TSimpleRTTI<T>.New(FInstance)
    .Fields(aFields)
    .ClassName(aClassName)
    .Where(aWhere);

  aSQL := aSQL + ' SELECT ' + aFields;
  aSQL := aSQL + ' FROM ' + aClassName;
  aSQL := aSQL + ' WHERE ' + aWhere;
end;

function TSimpleSQL<T>.SelectWhere (aWhere : String; OrderBy : String; var aSQL: String) : iSimpleSQL<T>;
var
  aFields : String;
  aClassName : String;
begin
  TSimpleRTTI<T>.New(nil)
    .Fields(aFields)
    .ClassName(aClassName);

  aSQL := aSQL + ' SELECT ' + aFields;
  aSQL := aSQL + ' FROM ' + aClassName;
  aSQL := aSQL + ' WHERE ' + aWhere;
  if OrderBy <> '' then
    aSQL := aSQL + ' ORDER BY' + OrderBy;
end;

function TSimpleSQL<T>.Update(var aSQL: String): iSimpleSQL<T>;
var
  ClassName, aUpdate, aWhere : String;
begin
  Result := Self;

  TSimpleRTTI<T>.New(FInstance)
    .ClassName(ClassName)
    .Update(aUpdate)
    .Where(aWhere);

  aSQL := aSQL + 'UPDATE ' + ClassName;
  aSQL := aSQL + ' SET ' + aUpdate;
  aSQL := aSQL + ' WHERE ' + aWhere;

end;

end.
