unit SimpleDAO;
interface
uses
  SimpleInterface,
  System.RTTI,
  System.Generics.Collections,
  System.Classes,
  Data.DB,
  {$IFNDEF CONSOLE}
    {$IFDEF FMX}
      FMX.Forms,
    {$ELSE}
      Vcl.Forms,
    {$ENDIF}
  {$ENDIF}
  SimpleDAOSQLAttribute,
  System.Threading;
Type
  TSimpleDAO<T: class, constructor> = class(TInterfacedObject, iSimpleDAO<T>)
    private
      FQuery : iSimpleQuery;
      FDataSource : TDataSource;
      FSQLAttribute : iSimpleDAOSQLAttribute<T>;
      {$IFNDEF CONSOLE}
      FForm : TForm;
      {$ENDIF}
      FList : TObjectList<T>;
      FPage : Integer;
      FLimit : Integer;
      function FillParameter(aInstance : T) : iSimpleDAO<T>; overload;
      function FillParameter(aInstance : T; aId : Variant) : iSimpleDAO<T>; overload;
      procedure OnDataChange(Sender : TObject; Field : TField);
    public
      constructor Create(aQuery : iSimpleQuery);
      destructor Destroy; override;
      class function New(aQuery : iSimpleQuery) : iSimpleDAO<T>; overload;
      function DataSource( aDataSource : TDataSource) : iSimpleDAO<T>;
      function Insert(aValue : T) : iSimpleDAO<T>; overload;
      function Update(aValue : T) : iSimpleDAO<T>; overload;
      function Delete(aValue : T) : iSimpleDAO<T>; overload;
      function Delete(aField : String; aValue : String) : iSimpleDAO<T>; overload;
      function Filter(aField : String; aValue : String) : iSimpleDAO<T>;
      function Paginate(aLimit, aPage: Integer) : iSimpleDAO<T>;
      function RecordCount(var aRecCount : Integer) : iSimpleDAO<T>;
      function LastID : iSimpleDAO<T>;
      function LastRecord : iSimpleDAO<T>;
      {$IFNDEF CONSOLE}
      function Insert: iSimpleDAO<T>; overload;
      function Update : iSimpleDAO<T>; overload;
      function Delete : iSimpleDAO<T>; overload;
      {$ENDIF}
      function Find(aBindList : Boolean = True) : iSimpleDAO<T>; overload;
      function Find(var aList : TObjectList<T>) : iSimpleDAO<T> ; overload;
      function Find( aId : Integer) : T; overload;
      function Find(aKey : String; aValue : Variant) : iSimpleDAO<T>; overload;
      function SQL : iSimpleDAOSQLAttribute<T>;
      {$IFNDEF CONSOLE}
      function BindForm(aForm : TForm)  : iSimpleDAO<T>;
      {$ENDIF}
  end;
implementation
uses
  System.SysUtils, SimpleAttributes, System.TypInfo, SimpleRTTI, SimpleSQL,
  SimpleTypes;
{ TGenericDAO }
{$IFNDEF CONSOLE}
function TSimpleDAO<T>.BindForm(aForm : TForm)  : iSimpleDAO<T>;
begin
  Result := Self;
  FForm := aForm;
end;
{$ENDIF}
constructor TSimpleDAO<T>.Create(aQuery : iSimpleQuery);
begin
  FQuery := aQuery;
  FSQLAttribute := TSimpleDAOSQLAttribute<T>.New(Self);
  FList := TObjectList<T>.Create;
  FPage := 0;
  FLimit := 0;
end;

function TSimpleDAO<T>.DataSource(aDataSource: TDataSource): iSimpleDAO<T>;
begin
  Result := Self;
  FDataSource := aDataSource;
  FDataSource.DataSet := FQuery.DataSet;
  FDataSource.OnDataChange := OnDataChange;
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
{$IFNDEF CONSOLE}
function TSimpleDAO<T>.Delete: iSimpleDAO<T>;
var
  aSQL : String;
  Entity : T;
begin
  Result := Self;
  Entity := T.Create;
  try
    TSimpleSQL<T>.New(Entity).Delete(aSQL);
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    TSimpleRTTI<T>.New(nil).BindFormToClass(FForm, Entity);
    Self.FillParameter(Entity);
    FQuery.ExecSQL;
  finally
    FreeAndNil(Entity);
  end;
end;
{$ENDIF}
function TSimpleDAO<T>.Delete(aField,
  aValue: String): iSimpleDAO<T>;
var
  aSQL : String;
  Entity : T;
  aTableName: string;
begin
  Result := Self;
  Entity := T.Create;
  try
    TSimpleSQL<T>.New(Entity).Delete(aSQL);
    TSimpleRTTI<T>
      .New(Entity)
      .TableName(aTableName);
    aSQL := 'DELETE FROM ' + aTableName + ' WHERE ' + aField + ' = ' + aValue;
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    FQuery.ExecSQL;
  finally
    FreeAndNil(Entity);
  end;
end;

destructor TSimpleDAO<T>.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;
function TSimpleDAO<T>.Find(aBindList : Boolean = True) : iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(FQuery.SQLType)
    .Paginate(FLimit, FPage)
    .Fields(FSQLAttribute.Fields)
    .Join(FSQLAttribute.Join)
    .Where(FSQLAttribute.Where)
	  .GroupBy(FSQLAttribute.GroupBy)
    .OrderBy(FSQLAttribute.OrderBy)
    .Select(aSQL);
  FQuery.DataSet.DisableControls;
  FQuery.Open(aSQL);
  if aBindList then
    TSimpleRTTI<T>.New(nil).DataSetToEntityList(FQuery.DataSet, FList);
  FSQLAttribute.Clear;
  FQuery.DataSet.EnableControls;
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
{$IFNDEF CONSOLE}
function TSimpleDAO<T>.Insert: iSimpleDAO<T>;
var
  aSQL : String;
  Entity : T;
begin
  Result := Self;
  Entity := T.Create;
  try
    TSimpleSQL<T>.New(Entity).Insert(aSQL);
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    TSimpleRTTI<T>.New(nil).BindFormToClass(FForm, Entity);
    Self.FillParameter(Entity);
    FQuery.ExecSQL;
  finally
    FreeAndNil(Entity);
  end;
end;

{$ENDIF}
function TSimpleDAO<T>.LastID: iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(Self.FQuery.SQLType)
    .LastID(aSQL);

  FQuery.Open(aSQL);
end;
function TSimpleDAO<T>.LastRecord: iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(Self.FQuery.SQLType)
    .LastRecord(aSQL);
  FQuery.Open(aSQL);
end;

function TSimpleDAO<T>.Find(var aList: TObjectList<T>): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(FQuery.SQLType)
    .Paginate(FLimit, FPage)
    .Fields(FSQLAttribute.Fields)
    .Join(FSQLAttribute.Join)
    .Where(FSQLAttribute.Where)
	  .GroupBy(FSQLAttribute.GroupBy)
    .OrderBy(FSQLAttribute.OrderBy)
    .Select(aSQL);
  FQuery.Open(aSQL);
  TSimpleRTTI<T>.New(nil).DataSetToEntityList(FQuery.DataSet, aList);
  FSQLAttribute.Clear;
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
  Self.LastRecord;
end;
class function TSimpleDAO<T>.New(aQuery : iSimpleQuery): iSimpleDAO<T>;
begin
  Result := Self.Create(aQuery);
end;
procedure TSimpleDAO<T>.OnDataChange(Sender: TObject; Field: TField);
begin
  if ( FList.Count > 0) and (FDataSource.DataSet.RecNo-1 <= FList.Count) then
  begin
    {$IFNDEF CONSOLE}
    if Assigned(FForm) then
     TSimpleRTTI<T>
      .New(nil)
      .BindClassToForm(FForm, FList[FDataSource.DataSet.RecNo-1]);
    {$ENDIF}
  end;
end;

function TSimpleDAO<T>.Paginate(aLimit, aPage: Integer): iSimpleDAO<T>;
begin
  Result := Self;

  FLimit := aLimit;
  FPage := aPage;
end;

function TSimpleDAO<T>.RecordCount(var aRecCount: Integer): iSimpleDAO<T>;
var
  aSQL : String;
begin
  aSQL := FQuery.SQL.Text;

  case FQuery.SQLType of
    SQLite, MySQL:
    begin
      aSQL := 'select count(*) as n ' + Copy(aSQL, Pos(' FROM ', aSQL), Length(aSQL));
      aSQL := copy(aSQL, 1, pos(' LIMIT ', aSQL));
    end;
    Firebird: aSQL := 'select count(*) as n ' + Copy(aSQL, Pos(' FROM ', aSQL), Length(aSQL));
  end;

  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Open;

  aRecCount := FQuery.DataSet.FieldByName('N').AsInteger;
end;

function TSimpleDAO<T>.SQL: iSimpleDAOSQLAttribute<T>;
begin
  Result := FSQLAttribute;
end;
{$IFNDEF CONSOLE}
function TSimpleDAO<T>.Update: iSimpleDAO<T>;
var
  aSQL : String;
  Entity : T;
begin
  Result := Self;
  Entity := T.Create;
  try
    TSimpleSQL<T>.New(Entity).Update(aSQL);
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    TSimpleRTTI<T>.New(nil).BindFormToClass(FForm, Entity);
    Self.FillParameter(Entity);
    FQuery.ExecSQL;
  finally
    FreeAndNil(Entity)
  end;
end;
{$ENDIF}
function TSimpleDAO<T>.Update(aValue: T): iSimpleDAO<T>;
var
  aSQL : String;
  DictionaryFields : TDictionary<String, Variant>;
  aPK : String;
  aPkValue : Integer;
begin
  Result := Self;
  TSimpleSQL<T>.New(aValue).Update(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  Self.FillParameter(aValue);
  FQuery.ExecSQL;
  DictionaryFields := TDictionary<String, Variant>.Create;
  TSimpleRTTI<T>.New(aValue).DictionaryFields(DictionaryFields).PrimaryKey(aPK);
  aPkValue := DictionaryFields.Items[aPK];
  Self.Find(aPKValue);
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

  TSimpleRTTI<T>.New(aInstance).ListBindFields(ListFields);

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

function TSimpleDAO<T>.Filter(aField, aValue: String): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(FQuery.SQLType)
    .Paginate(FLimit, FPage)
    .Where('upper('+aField+') like ' + QuotedStr('%') + '||' + 'upper(:'+aField+')||'+QuotedStr('%'))
    .Select(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Params.ParamByName(aField).Value := aValue;
  FQuery.Open;
end;

function TSimpleDAO<T>.Find(aKey: String; aValue: Variant): iSimpleDAO<T>;
var
  aSQL : String;
begin
  Result := Self;
  TSimpleSQL<T>
    .New(nil)
    .SQLType(FQuery.SQLType)
    .Paginate(FLimit, FPage)
    .Where(aKey + ' = :' + aKey)
    .Select(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Params.ParamByName(aKey).Value := aValue;
  FQuery.Open;
end;

end.
