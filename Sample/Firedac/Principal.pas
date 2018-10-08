unit Principal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, SimpleInterface, SimpleDAO, Entidade.Pedido, System.Generics.Collections, SimpleQueryFiredac;

type
  TForm9 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    btnFind: TButton;
    Button6: TButton;
    Button7: TButton;
    Edit2: TEdit;
    Edit1: TEdit;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Memo1: TMemo;
    FDConnection1: TFDConnection;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    DAOPedido : iSimpleDAO<TPEDIDO>;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.NOME := Edit1.Text;
    Pedido.DATA := now;
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;

end;

procedure TForm9.Button3Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.NOME := Edit1.Text;
    Pedido.DATA := now;
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;

end;

procedure TForm9.Button4Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    DAOPedido.Delete(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TForm9.btnFindClick(Sender: TObject);
var
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := DAOPedido.SQL.OrderBy('ID').&End.Find;
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
      Pedido.Free;
    end;
  finally
    Pedidos.Free;
  end;
end;

procedure TForm9.Button6Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := DAOPedido.Find(StrToInt(Edit2.Text));
  try
    Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
  finally
    Pedido.Free;
  end;
end;

procedure TForm9.Button7Click(Sender: TObject);
var
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := DAOPedido.SQL.Where(' Nome = ' + QuotedStr(Edit1.Text)).&End.Find;
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
      Pedido.Free;
    end;
  finally
    Pedidos.Free;
  end;
end;

procedure TForm9.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  Edit1.Text := DataSource1.DataSet.FieldByName('NOME').AsString;
  Edit2.Text := DataSource1.DataSet.FieldByName('ID').AsString;
end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  DAOPedido := TSimpleDAO<TPEDIDO>
                  .New(TSimpleQueryFiredac.New(FDConnection1))
                  .DataSource(DataSource1);

end;

end.
