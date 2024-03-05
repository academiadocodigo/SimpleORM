unit Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  System.Generics.Collections, SimpleORM, Vcl.ExtCtrls, Vcl.ComCtrls,
  Model.Pedido;

type
  TFrmPrincipal = class(TForm)
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    btnFind: TButton;
    Button6: TButton;
    Button7: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Memo1: TMemo;
    FDConnection1: TFDConnection;

    [Bind('CLIENTE')]
    Edit1: TEdit;
    [Bind('ID')]
    Edit2: TEdit;
    [Bind('VALORTOTAL')]
    Edit3: TEdit;
    Button2: TButton;
    [Bind('DATAPEDIDO')]
    DateTimePicker1: TDateTimePicker;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;

    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
    DAOPedido : iSimpleDAO<TPedido>;
  public
    { Public declarations }
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

procedure TFrmPrincipal.Button1Click(Sender: TObject);
var
  Pedido : TPedido;
begin
  Pedido := TPedido.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.Cliente := Edit1.Text;
    Pedido.DataPedido := Now;
    Pedido.ValorTotal := StrToCurr(Edit3.Text);
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TFrmPrincipal.Button2Click(Sender: TObject);
var
  Pedidos : TObjectList<TPedido>;
  Pedido : TPedido;
begin
  Pedidos := TObjectList<TPedido>.Create;

  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find(Pedidos);

  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.Cliente + DateToStr(Pedido.DataPedido));
    end;
  finally
    Pedidos.Free;
  end;
end;

procedure TFrmPrincipal.Button3Click(Sender: TObject);
begin
  DAOPedido.Insert;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TFrmPrincipal.Button4Click(Sender: TObject);
var
  Pedido : TPedido;
begin
  Pedido := TPedido.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    DAOPedido.Delete(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TFrmPrincipal.Button5Click(Sender: TObject);
var
  Pedido : TPedido;
begin
  Pedido := TPedido.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.Cliente := Edit1.Text;
    Pedido.DataPedido := Now;
    Pedido.ValorTotal := StrToCurr(Edit3.Text);
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TFrmPrincipal.btnFindClick(Sender: TObject);
begin
  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find;
end;


procedure TFrmPrincipal.Button6Click(Sender: TObject);
var
  Pedido : TPedido;
begin
  Pedido := DAOPedido.Find(StrToInt(Edit2.Text));
  try
    Memo1.Lines.Add(Pedido.Cliente + DateToStr(Pedido.DataPedido));
  finally
    Pedido.Free;
  end;
end;

procedure TFrmPrincipal.Button7Click(Sender: TObject);
begin
  DAOPedido
    .SQL
      .Where(' Nome = ' + QuotedStr(Edit1.Text))
    .&End
  .Find;
end;

procedure TFrmPrincipal.Button8Click(Sender: TObject);
begin
  DAOPedido.Update;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TFrmPrincipal.Button9Click(Sender: TObject);
begin
  DAOPedido.Delete;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
var
  Conn: iSimpleQuery;
begin
  ReportMemoryLeaksOnShutdown := true;

  Conn := TSimpleQueryFiredac.New(FDConnection1);


  DAOPedido := iSimpleDAO<TPedido>(TSimpleDAO<TPedido>
    .New(Conn)
    .DataSource(DataSource1)
    .BindForm(Self));
end;

end.
