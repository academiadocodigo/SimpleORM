unit SimpleQueryRestDW;

interface

uses
  SimpleInterface, System.Classes, Data.DB, uDWConstsData, uRESTDWPoolerDB;

Type
  TSimpleQueryRestDW<T : class, constructor> = class(TInterfacedObject, iSimpleQuery)
    private
      FConnection : TRESTDWDataBase;
      FQuery : TRESTDWClientSQL;
    public
      constructor Create(aConnection : TRESTDWDataBase);
      destructor Destroy; override;
      class function New(aConnection : TRESTDWDataBase) : iSimpleQuery;
      function SQL : TStrings;
      function Params : TParams;
      function ExecSQL : iSimpleQuery;
      function DataSet : TDataSet;
      function Open(aSQL : String) : iSimpleQuery; overload;
      function Open : iSimpleQuery; overload;
  end;

implementation

uses
  System.SysUtils, SimpleRTTI;

{ TSimpleQuery<T> }

constructor TSimpleQueryRestDW<T>.Create(aConnection : TRESTDWDataBase);
var
  aTable : String;
begin
  FConnection := aConnection;
  FQuery := TRESTDWClientSQL.Create(nil);
  FQuery.DataBase := FConnection;
  TSimpleRTTI<T>.New(nil).ClassName(aTable);
  FQuery.AutoCommitData := False;
  FQuery.AutoRefreshAfterCommit := True;
  //FQuery.SetInBlockEvents(false);
  FQuery.UpdateTableName := aTable;
end;

function TSimpleQueryRestDW<T>.DataSet: TDataSet;
begin
  Result := FQuery;
end;

destructor TSimpleQueryRestDW<T>.Destroy;
begin
  FreeAndNil(FQuery);
  inherited;
end;

function TSimpleQueryRestDW<T>.ExecSQL: iSimpleQuery;
var
  aErro : String;
begin
  Result := Self;
  FQuery.ExecSQL(aErro);
  FQuery.ApplyUpdates(aErro);
end;

class function TSimpleQueryRestDW<T>.New(aConnection : TRESTDWDataBase): iSimpleQuery;
begin
  Result := Self.Create(aConnection);
end;

function TSimpleQueryRestDW<T>.Open: iSimpleQuery;
begin
  Result := Self;
  FQuery.Close;
  FQuery.Open;
end;

function TSimpleQueryRestDW<T>.Open(aSQL: String): iSimpleQuery;
begin
  FQuery.Close;
  Result := Self;
  FQuery.Open(aSQL);
end;

function TSimpleQueryRestDW<T>.Params: TParams;
begin
  Result := FQuery.Params;
end;

function TSimpleQueryRestDW<T>.SQL: TStrings;
begin
  Result := FQuery.SQL;
end;

end.
