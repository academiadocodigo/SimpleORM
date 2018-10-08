unit SimpleDAO;

interface

uses
  SimpleInterface, System.RTTI, System.Generics.Collections, System.Classes,
  Data.DB, SimpleDAOSQLAttribute;

Type
  TSimpleDAO<T: class, constructor> = class(TInterfacedObject, iSimpleDAO<T>)
    private
      FQuery : iSimpleQuery;
      FDictionaryFields : TDictionary<string, variant>;
      FFields, FWhere, FUpdate, FParam : String;
      FClassName : String;
      FDataSource : TDataSource;
      FSQLAttribute : iSimpleDAOSQLAttribute<T>;
      function FillParameter(aInstance : T) : iSimpleDAO<T>; overload;
      function FillParameter(aInstance : T; aId : Variant) : iSimpleDAO<T>; overload;
    public
      constructor Create(aQuery : iSimpleQuery);
      destructor Destroy; override;
      class function New(aQuery : iSimpleQuery) : iSimpleDAO<T>;
      function DataSource( aDataSource : TDataSource) : iSimpleDAO<T>;
      function Insert(aValue : T) : iSimpleDAO<T>;
      function Update(aValue : T) : iSimpleDAO<T>;
      function Delete(aValue : T) : iSimpleDAO<T>;
      function Find : TList<T>; overload;
      function Find( aId : Integer) : T; overload;
      //function Find (aWhere : String) : TList<T>; overload;
      function SQL : iSimpleDAOSQLAttribute<T>;
  end;

implementation

uses
  System.SysUtils, SimpleAttributes, System.TypInfo, SimpleRTTI, SimpleSQL;

{ TGenericDAO }

constructor TSimpleDAO<T>.Create(aQuery : iSimpleQuery);
begin
  FQuery := aQuery;
  FSQLAttribute := TSimpleDAOSQLAttribute<T>.New(Self);
end;

function TSimpleDAO<T>.DataSource(aDataSource: TDataSource): iSimpleDAO<T>;
begin
  Result := Self;
  FDataSource := aDataSource;
  FDataSource.DataSet := FQuery.DataSet;
end;

function TSimpleDAO<T>.Delete(aValue: T): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>.New(aValue).Delete(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  Self.FillParameter(aValue);
  FQuery.ExecSQL;
end;

destructor TSimpleDAO<T>.Destroy;
begin
  inherited;
end;

function TSimpleDAO<T>.Find : TList<T>;
var
  aSQL : String;
begin
  Result := TList<T>.Create;

  TSimpleSQL<T>
    .New(nil)
    .Fields(FSQLAttribute.Fields)
    .Where(FSQLAttribute.Where)
    .OrderBy(FSQLAttribute.OrderBy)
    .Select(aSQL);

  FQuery.Open(aSQL);
  TSimpleRTTI<T>.New(nil).DataSetToEntityList(FQuery.DataSet, Result);
  FSQLAttribute.Clear;
end;

function TSimpleDAO<T>.Find(aId: Integer): T;
var
  aSQL : String;
begin
  Result := T.Create;
  TSimpleSQL<T>.New(nil).SelectId(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  Self.FillParameter(Result, aId);
  FQuery.Open;
  TSimpleRTTI<T>.New(nil).DataSetToEntity(FQuery.DataSet, Result);
end;

function TSimpleDAO<T>.Insert(aValue: T): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>.New(aValue).Insert(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  Self.FillParameter(aValue);
  FQuery.ExecSQL;
end;


class function TSimpleDAO<T>.New(aQuery : iSimpleQuery): iSimpleDAO<T>;
begin
  Result := Self.Create(aQuery);
end;

function TSimpleDAO<T>.SQL: iSimpleDAOSQLAttribute<T>;
begin
  Result := FSQLAttribute;
end;

function TSimpleDAO<T>.Update(aValue: T): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>.New(aValue).Update(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  Self.FillParameter(aValue);
  FQuery.ExecSQL;
end;

function TSimpleDAO<T>.FillParameter(aInstance: T): iSimpleDAO<T>;
var
  Key : String;
  DictionaryFields : TDictionary<String, Variant>;
  P : TParams;
begin
  DictionaryFields := TDictionary<String, Variant>.Create;
  TSimpleRTTI<T>.New(aInstance).DictionaryFields(DictionaryFields);
  try
    for Key in DictionaryFields.Keys do
    begin
      if FQuery.Params.FindParam(Key) <> nil then
        FQuery.Params.ParamByName(Key).Value := DictionaryFields.Items[Key];
    end;
  finally
    FreeAndNil(DictionaryFields);
  end;
end;

function TSimpleDAO<T>.FillParameter(aInstance: T;
  aId: Variant): iSimpleDAO<T>;
var
  I : Integer;
  ListFields : TList<String>;
begin
  ListFields := TList<String>.Create;
  TSimpleRTTI<T>.New(aInstance).ListFields(ListFields);
  try
    for I := 0 to Pred(ListFields.Count) do
    begin
      if FQuery.Params.FindParam(ListFields[I]) <> nil then
        FQuery.Params.ParamByName(ListFields[I]).Value := aId;
    end;
  finally
    FreeAndNil(ListFields);
  end;
end;

end.
