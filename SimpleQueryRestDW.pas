unit SimpleQueryRestDW;

interface

uses
  SimpleInterface, System.Classes, Data.DB, uDWConstsData, uRESTDWPoolerDB;

Type
  TSimpleQueryRestDW<T : class, constructor> = class(TInterfacedObject, iSimpleQuery)
    private
      FQuery : TRESTDWClientSQL;
    public
      constructor Create(aQuery : TRESTDWClientSQL);
      destructor Destroy; override;
      class function New(aQuery : TRESTDWClientSQL) : iSimpleQuery;
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

constructor TSimpleQueryRestDW<T>.Create(aQuery : TRESTDWClientSQL);
var
  aTable : String;
begin
  TSimpleRTTI<T>.New(nil).ClassName(aTable);
  FQuery := aQuery;   
  FQuery.AutoCommitData := False;
  FQuery.AutoRefreshAfterCommit := True;
  FQuery.SetInBlockEvents(false);  
  FQuery.UpdateTableName := aTable;
end;

function TSimpleQueryRestDW<T>.DataSet: TDataSet;
begin
  Result := FQuery;
end;

destructor TSimpleQueryRestDW<T>.Destroy;
begin
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

class function TSimpleQueryRestDW<T>.New(aQuery : TRESTDWClientSQL): iSimpleQuery;
begin
  Result := Self.Create(aQuery);
end;

function TSimpleQueryRestDW<T>.Open: iSimpleQuery;
begin
  FQuery.Close;
  Result := Self;
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
