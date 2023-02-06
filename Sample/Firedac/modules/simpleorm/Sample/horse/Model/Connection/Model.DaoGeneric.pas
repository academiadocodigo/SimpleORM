unit Model.DaoGeneric;

interface

uses
  System.JSON,
  REST.Json,
  SimpleInterface,
  SimpleDAO,
  SimpleAttributes,
  SimpleQueryFiredac,
  Data.DB,
  DataSetConverter4D,
  DataSetConverter4D.Impl,
  DataSetConverter4D.Helper,
  DataSetConverter4D.Util;

type

  iDAOGeneric<T : Class> = interface
    ['{2A6C6ED9-40BC-4AF5-A635-26615D8DD321}']
    function Find : TJsonArray; overload;
    function Find (const aID : String; var aObject : T ) : iDAOGeneric<T>; overload;
    function Find (const aID : String ) : TJsonObject; overload;
    function Insert (const aJsonObject : TJsonObject) : TJsonObject;
    function Update (const aJsonObject : TJsonObject) : TJsonObject; overload;
    function Update (const aObject : T) : iDAOGeneric<T>; overload;
    function Delete (aField : String; aValue : String) : TJsonObject;
    function DAO : ISimpleDAO<T>;
    function DataSetAsJsonArray : TJsonArray;
    function DataSetAsJsonObject : TJsonObject;
    function DataSet : TDataSet;
  end;

  TDAOGeneric<T : class, constructor> = class(TInterfacedObject, iDAOGeneric<T>)
  private
    FIndexConn : Integer;
    FConn : iSimpleQuery;
    FDAO : iSimpleDAO<T>;
    FDataSource : TDataSource;
  public
    constructor Create;
    destructor Destroy; override;
    class function New : iDAOGeneric<T>;
    function Find : TJsonArray; overload;
    function Find (const aID : String; var aObject : T ) : iDAOGeneric<T>; overload;
    function Find (const aID : String ) : TJsonObject; overload;
    function Insert (const aJsonObject : TJsonObject) : TJsonObject;
    function Update (const aJsonObject : TJsonObject) : TJsonObject; overload;
    function Update (const aObject : T) : iDAOGeneric<T>; overload;
    function Delete (aField : String; aValue : String) : TJsonObject;
    function DAO : ISimpleDAO<T>;
    function DataSetAsJsonArray : TJsonArray;
    function DataSetAsJsonObject : TJsonObject;
    function DataSet : TDataSet;
  end;

implementation

{ TDAOGeneric<T> }

uses Model.Connection, System.SysUtils;

constructor TDAOGeneric<T>.Create;
begin
  FDataSource := TDataSource.Create(nil);
  FIndexConn := Model.Connection.Connected;
  FConn := TSimpleQueryFiredac.New(Model.Connection.FConnList.Items[FIndexConn]);
  FDAO := TSimpleDAO<T>.New(FConn).DataSource(FDataSource);
end;

function TDAOGeneric<T>.DAO: ISimpleDAO<T>;
begin
  Result := FDAO;
end;

function TDAOGeneric<T>.DataSet: TDataSet;
begin
  Result := FDataSource.DataSet;
end;

function TDAOGeneric<T>.DataSetAsJsonArray: TJsonArray;
begin
  Result := FDataSource.DataSet.AsJSONArray;
end;

function TDAOGeneric<T>.DataSetAsJsonObject: TJsonObject;
begin
  Result := FDataSource.DataSet.AsJSONObject;
end;

function TDAOGeneric<T>.Delete(aField, aValue: String): TJsonObject;
begin
  FDAO.Delete(aField, aValue);
  Result := FDataSource.DataSet.AsJSONObject;
end;

destructor TDAOGeneric<T>.Destroy;
begin
  FDataSource.Free;
  Model.Connection.Disconnected(FIndexConn);
  inherited;
end;

function TDAOGeneric<T>.Find(const aID: String; var aObject: T): iDAOGeneric<T>;
begin
  Result := Self;
  aObject := FDAO.Find(StrToInt(aID));
end;

function TDAOGeneric<T>.Find(const aID: String): TJsonObject;
begin
  FDAO.Find(StrToInt(aID));
  Result := FDataSource.DataSet.AsJSONObject;
end;

function TDAOGeneric<T>.Find: TJsonArray;
begin
  FDAO.Find;
  Result := FDataSource.DataSet.AsJSONArray;
end;

function TDAOGeneric<T>.Insert(const aJsonObject: TJsonObject): TJsonObject;
begin
  FDAO.Insert(TJson.JsonToObject<T>(aJsonObject));
  Result := FDataSource.DataSet.AsJSONObject;
end;

class function TDAOGeneric<T>.New: iDAOGeneric<T>;
begin
  Result := Self.Create;
end;

function TDAOGeneric<T>.Update(const aJsonObject: TJsonObject): TJsonObject;
begin
  FDAO.Update(TJson.JsonToObject<T>(aJsonObject));
  Result := FDataSource.DataSet.AsJSONObject;
end;

function TDAOGeneric<T>.Update(const aObject: T): iDAOGeneric<T>;
begin
  FDAO.Update(aObject);
  Result := Self;
end;

end.
