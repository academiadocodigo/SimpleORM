unit intensive.Services.Generic;

interface

uses
  SimpleInterface,
  SimpleDAO,
  SimpleAttributes,
  SimpleQueryFiredac,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.UI,
  Data.DB,
  FireDAC.Comp.Client,
  intensive.Resources.Interfaces,
  intensive.Resources.Conexao,
  System.Generics.Collections;

type
  iService<T: Class> = interface
    function Listar: TDataSet;
    function ListarPorId(Id: Integer): TDataSet;
    function ListarPorFiltro(Key: String; Value: Variant): TDataSet;
    function Inserir: iService<T>;
    function Atualizar: iService<T>;
    function Excluir: iService<T>; overload;
    function Excluir(Field: String; Value: String): iService<T>; overload;
    function DataSource(var aDataSource: TDataSource): iService<T>;
    function Entity: T;
  end;

  TService<T: class, constructor> = class(TInterfacedObject, iService<T>)
  private
    FParent: T;
    FConexao: iConexao;
    FConn: iSimpleQuery;
    FDAO: iSimpleDAO<T>;
    FDataSource: TDataSource;
  public
    constructor Create(Parent: T);
    destructor Destroy; override;
    class function New(Parent: T): iService<T>;
    function Listar: TDataSet;
    function ListarPorId(Id: Integer): TDataSet;
    function ListarPorFiltro(Key: String; Value: Variant): TDataSet;
    function Inserir: iService<T>;
    function Atualizar: iService<T>;
    function Excluir: iService<T>; overload;
    function Excluir(Field: String; Value: String): iService<T>; overload;
    function DataSource(var aDataSource: TDataSource): iService<T>;
    function Entity: T;
  end;

implementation

function TService<T>.Atualizar: iService<T>;
begin
  Result := Self;
  FDAO.Update(FParent);
end;

constructor TService<T>.Create(Parent: T);
begin
  FParent := Parent;
  FDataSource := TDataSource.Create(nil);
  FConexao := TConexao.New;
  FConn := TSimpleQueryFiredac.New(TFDConnection(FConexao.Connect));
  FDAO := TSimpleDAO<T>.New(FConn).DataSource(FDataSource);
end;

function TService<T>.DataSource(var aDataSource: TDataSource): iService<T>;
begin
  Result := Self;
  aDataSource := FDataSource;
end;

destructor TService<T>.Destroy;
begin
  FDataSource.DisposeOf;
  inherited;
end;

function TService<T>.Excluir: iService<T>;
begin
  Result := Self;
  FDAO.Delete(FParent);
end;

function TService<T>.Excluir(Field, Value: String): iService<T>;
begin
  Result := Self;
  FDAO.Delete(Field, Value);
end;

function TService<T>.Inserir: iService<T>;
begin
  Result := Self;
  FDAO.Insert(FParent);
end;

function TService<T>.Listar: TDataSet;
begin
  FDAO.Find;
  Result := FDataSource.DataSet;
end;

function TService<T>.ListarPorFiltro(Key: String; Value: Variant): TDataSet;
begin
  FDAO.Find(Key, Value);
  Result := FDataSource.DataSet;
end;

function TService<T>.ListarPorId(Id: Integer): TDataSet;
begin
  FDAO.Find(Id);
  Result := FDataSource.DataSet;
end;

class function TService<T>.New(Parent: T): iService<T>;
begin
  Result := Self.Create(Parent);
end;

function TService<T>.Entity: T;
begin
  Result := FParent;
end;

end.
