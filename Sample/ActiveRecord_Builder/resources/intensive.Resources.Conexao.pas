unit intensive.Resources.Conexao;

interface

uses
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
  intensive.Resources.Interfaces;

type
  TConexao = class(TInterfacedObject, iConexao)
    private
      FConn : TFDConnection;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : iConexao;
      function Connect : TCustomConnection;
  end;

implementation

function TConexao.Connect: TCustomConnection;
begin
  Result := FConn;
end;

constructor TConexao.Create;
begin
  FConn := TFDConnection.Create(nil);
  FConn.Params.Clear;
  FConn.Params.Add('DriverID=SQLite');
  FConn.Params.Add('DataBase=..\..\database\Dados.sdb');
  FConn.Params.Add('LockingMode=Normal');
  FConn.Connected := True;
end;

destructor TConexao.Destroy;
begin
  FConn.DisposeOf;
  inherited;
end;

class function TConexao.New : iConexao;
begin
  Result := Self.Create;
end;

end.
