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
        FQuery: iSimpleQuery;
        FDataSource: TDataSource;
        FSQLAttribute: iSimpleDAOSQLAttribute<T>;
{$IFNDEF CONSOLE}
        FForm: TForm;
{$ENDIF}
        FList: TObjectList<T>;
        function FillParameter(aInstance: T): iSimpleDAO<T>; overload;
        function FillParameter(aInstance: T; aId: Variant)
          : iSimpleDAO<T>; overload;
        function FillParameterWhere(aInstance: T; var aWhere : String): iSimpleDAO<T>;
        procedure OnDataChange(Sender: TObject; Field: TField);
        procedure FilledObject(var aValue: T; aSQL: string);
    public
        constructor Create(aQuery: iSimpleQuery);
        destructor Destroy; override;
        class function New(aQuery: iSimpleQuery): iSimpleDAO<T>; overload;
        function DataSource(aDataSource: TDataSource): iSimpleDAO<T>;
        function Insert(var aValue: T): iSimpleDAO<T>; overload;
        function Update(var aValue: T): iSimpleDAO<T>; overload;
        function Delete(aValue: T): iSimpleDAO<T>; overload;
        function Delete(aField: String; aValue: String): iSimpleDAO<T>;
          overload;
        function LastID: iSimpleDAO<T>;
        function LastRecord: iSimpleDAO<T>;
{$IFNDEF CONSOLE}
        function Insert: iSimpleDAO<T>; overload;
        function Update: iSimpleDAO<T>; overload;
        function Delete: iSimpleDAO<T>; overload;
{$ENDIF}
        function Find(aBindList: Boolean = True): iSimpleDAO<T>; overload;
        function Find(var aList: TObjectList<T>): iSimpleDAO<T>; overload;
        function Find(aId: Integer): T; overload;
        function Find(aKey: String; aValue: Variant): iSimpleDAO<T>; overload;
        function FindT : T; overload;
        function SQL: iSimpleDAOSQLAttribute<T>;
{$IFNDEF CONSOLE}
        function BindForm(aForm: TForm): iSimpleDAO<T>;
{$ENDIF}
    end;

implementation

uses
    System.SysUtils,
    SimpleAttributes,
    System.TypInfo,
    SimpleRTTI,
    SimpleSQL,
    Variants;

{ TGenericDAO }
{$IFNDEF CONSOLE}

function TSimpleDAO<T>.BindForm(aForm: TForm): iSimpleDAO<T>;
begin
    Result := Self;
    FForm := aForm;
end;
{$ENDIF}

procedure TSimpleDAO<T>.FilledObject(var aValue: T; aSQL: string);
var
  aWhere: string;
begin
  Self.FillParameterWhere(aValue, aWhere);
  TSimpleSQL<T>.New(nil).Where(aWhere).Select(aSQL);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(aSQL);
  FQuery.Open;
  TSimpleRTTI<T>.New(nil).DataSetToEntity(FQuery.DataSet, aValue);
  FSQLAttribute.Clear;
end;

constructor TSimpleDAO<T>.Create(aQuery: iSimpleQuery);
begin
    FQuery := aQuery;
    FSQLAttribute := TSimpleDAOSQLAttribute<T>.New(Self);
    FList := TObjectList<T>.Create;
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
    aSQL: String;
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
    aSQL: String;
    Entity: T;
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

function TSimpleDAO<T>.Delete(aField, aValue: String): iSimpleDAO<T>;
var
    aSQL: String;
    Entity: T;
    aTableName: string;
begin
    Result := Self;
    Entity := T.Create;
    try
        TSimpleSQL<T>.New(Entity).Delete(aSQL);
        TSimpleRTTI<T>.New(Entity).TableName(aTableName);
        aSQL := 'DELETE FROM ' + aTableName + ' WHERE ' + aField +
          ' = ' + aValue;
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

function TSimpleDAO<T>.Find(aBindList: Boolean = True): iSimpleDAO<T>;
var
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(nil).Fields(FSQLAttribute.Fields).Join(FSQLAttribute.Join)
      .Where(FSQLAttribute.Where).GroupBy(FSQLAttribute.GroupBy)
      .OrderBy(FSQLAttribute.OrderBy).Select(aSQL);
    FQuery.DataSet.DisableControls;
    FQuery.Open(aSQL);
    if aBindList then
        TSimpleRTTI<T>.New(nil).DataSetToEntityList(FQuery.DataSet, FList);
    FSQLAttribute.Clear;
    FQuery.DataSet.EnableControls;
end;

function TSimpleDAO<T>.Find(aId: Integer): T;
var
    aSQL: String;
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
    aSQL: String;
    Entity: T;
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
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(nil).LastID(aSQL);
    FQuery.Open(aSQL);
end;

function TSimpleDAO<T>.LastRecord: iSimpleDAO<T>;
var
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(nil).LastRecord(aSQL);
    FQuery.Open(aSQL);
end;

function TSimpleDAO<T>.Find(var aList: TObjectList<T>): iSimpleDAO<T>;
var
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(nil).Fields(FSQLAttribute.Fields).Join(FSQLAttribute.Join)
      .Where(FSQLAttribute.Where).GroupBy(FSQLAttribute.GroupBy)
      .OrderBy(FSQLAttribute.OrderBy).Select(aSQL);
    FQuery.Open(aSQL);
    TSimpleRTTI<T>.New(nil).DataSetToEntityList(FQuery.DataSet, aList);
    FSQLAttribute.Clear;
end;

function TSimpleDAO<T>.Insert(var aValue: T): iSimpleDAO<T>;
var
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(aValue).Insert(aSQL);
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    Self.FillParameter(aValue);
    FQuery.ExecSQL;

    aSQL := '';
    FilledObject(aValue,aSQL);
end;

class function TSimpleDAO<T>.New(aQuery: iSimpleQuery): iSimpleDAO<T>;
begin
    Result := Self.Create(aQuery);
end;

procedure TSimpleDAO<T>.OnDataChange(Sender: TObject; Field: TField);
begin
    if (FList.Count > 0) and (FDataSource.DataSet.RecNo - 1 <= FList.Count) then
    begin
{$IFNDEF CONSOLE}
        if Assigned(FForm) then
            TSimpleRTTI<T>.New(nil).BindClassToForm(FForm,
              FList[FDataSource.DataSet.RecNo - 1]);
{$ENDIF}
    end;
end;

function TSimpleDAO<T>.SQL: iSimpleDAOSQLAttribute<T>;
begin
    Result := FSQLAttribute;
end;
{$IFNDEF CONSOLE}

function TSimpleDAO<T>.Update: iSimpleDAO<T>;
var
    aSQL: String;
    Entity: T;
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

function TSimpleDAO<T>.Update(var aValue: T): iSimpleDAO<T>;
var
    aSQL: String;
    aPK: String;
    aPkValue: Integer;
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
    Key: String;
    DictionaryFields: TDictionary<String, Variant>;
    DictionaryTypeFields: TDictionary<String, TFieldType>;
    P: TParams;
    FieldType: TFieldType;
begin
    DictionaryFields := TDictionary<String, Variant>.Create;
    DictionaryTypeFields := TDictionary<String, TFieldType>.Create;

    TSimpleRTTI<T>.New(aInstance).DictionaryFields(DictionaryFields);
    TSimpleRTTI<T>.New(aInstance).DictionaryTypeFields(DictionaryTypeFields);
    try
        for Key in DictionaryFields.Keys do
        begin
            if FQuery.Params.FindParam(Key) <> nil then
            begin
                if DictionaryTypeFields.TryGetValue(Key, FieldType ) then
                  FQuery.Params.ParamByName(Key).DataType := FieldType;
                FQuery.Params.ParamByName(Key).Value := DictionaryFields.Items[Key];
            end;
        end;
    finally
        FreeAndNil(DictionaryFields);
        FreeAndNil(DictionaryTypeFields);
    end;
end;

function TSimpleDAO<T>.FillParameter(aInstance: T; aId: Variant): iSimpleDAO<T>;
var
    Key: String;
    DictionaryFields: TDictionary<String, Variant>;
    P: TParams;
begin
    DictionaryFields := TDictionary<String, Variant>.Create;
    TSimpleRTTI<T>.New(aInstance).DictionaryFields(DictionaryFields);
    try
        for Key in DictionaryFields.Keys do
        begin
            if FQuery.Params.FindParam(Key) <> nil then
                FQuery.Params.ParamByName(Key).Value := aId;
        end;
    finally
        FreeAndNil(DictionaryFields);
    end;
end;

function TSimpleDAO<T>.FillParameterWhere(aInstance: T; var aWhere : String): iSimpleDAO<T>;
var
    Key, PK: String;
    DictionaryFields: TDictionary<String, Variant>;
    P: TParams;
begin
    DictionaryFields := TDictionary<String, Variant>.Create;
    TSimpleRTTI<T>.New(aInstance).DictionaryFields(DictionaryFields).PrimaryKey(PK);
    try
        for Key in DictionaryFields.Keys do begin
            if key<>PK then begin
                if not aWhere.IsEmpty then
                    aWhere := aWhere + ' and '+key +'=:'+key
                else
                    aWhere := key +'=:'+key;
                if FQuery.Params.FindParam(Key) <> nil then
                FQuery.Params.ParamByName(Key).Value :=
                  DictionaryFields.Items[Key];
            end;
        end;

    finally
        FreeAndNil(DictionaryFields);
    end;
end;

function TSimpleDAO<T>.Find(aKey: String; aValue: Variant): iSimpleDAO<T>;
var
    aSQL: String;
begin
    Result := Self;
    TSimpleSQL<T>.New(nil).Where(aKey + ' = :' + aKey).Select(aSQL);
    FQuery.SQL.Clear;
    FQuery.SQL.Add(aSQL);
    FQuery.Params.ParamByName(aKey).Value := aValue;
    FQuery.Open;
end;

function TSimpleDAO<T>.FindT: T;
var
    aSQL: String;
begin
  // Nelson 19/04/2022
  Result := T.Create;
  TSimpleSQL<T>.New(nil).Fields(FSQLAttribute.Fields).Join(FSQLAttribute.Join)
    .Where(FSQLAttribute.Where).GroupBy(FSQLAttribute.GroupBy)
    .OrderBy(FSQLAttribute.OrderBy).Select(aSQL);
  FQuery.Open(aSQL);
  TSimpleRTTI<T>.New(nil).DataSetToEntity(FQuery.DataSet, Result);

  FSQLAttribute.Clear;

end;

end.
