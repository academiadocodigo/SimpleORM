unit Unit8;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  uDWConstsData, uRESTDWPoolerDB, uDWAbout, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  System.Generics.Collections, Entidade.Pedido, SimpleInterface, SimpleDAO, SimpleQueryRestDW;

type
  TForm8 = class(TForm)
    RESTDWDataBase1: TRESTDWDataBase;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    btnFind: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure btnFindClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    DAOPedido : iSimpleDAO<TPEDIDO>;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.dfm}

procedure TForm8.btnFindClick(Sender: TObject);
var
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := DAOPedido.Find;
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
      Pedido.Free;
    end;
  finally
    FreeAndNil(Pedidos);
  end;

end;

procedure TForm8.Button2Click(Sender: TObject);
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

procedure TForm8.Button3Click(Sender: TObject);
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

procedure TForm8.Button4Click(Sender: TObject);
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

procedure TForm8.Button5Click(Sender: TObject);
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

procedure TForm8.Button6Click(Sender: TObject);
var
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := DAOPedido.Find(' Nome = ' + QuotedStr(Edit1.Text));
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

procedure TForm8.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  Edit1.Text := DataSource1.DataSet.FieldByName('NOME').AsString;
  Edit2.Text := DataSource1.DataSet.FieldByName('ID').AsString;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  DAOPedido := TSimpleDAO<TPEDIDO>
                .New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWDataBase1))
                .DataSource(DataSource1);
end;

end.
