unit Unit8;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  uDWConstsData, uRESTDWPoolerDB, uDWAbout, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids;

type
  TForm8 = class(TForm)
    RESTDWClientSQL1: TRESTDWClientSQL;
    RESTDWDataBase1: TRESTDWDataBase;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses
  System.Generics.Collections, Entidade.Pedido, SimpleInterface, SimpleDAO, SimpleQueryRestDW;

{$R *.dfm}

procedure TForm8.Button1Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  DAOPedido := TSimpleDAO<TPEDIDO>.New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1)).DataSource(DataSource1);
  Pedidos := DAOPedido.Find;
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

procedure TForm8.Button2Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  DAOPedido := TSimpleDAO<TPEDIDO>.New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1));
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.NOME := Edit1.Text;
    Pedido.DATA := now;
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
  end;

end;

procedure TForm8.Button3Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  DAOPedido := TSimpleDAO<TPEDIDO>.New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1));
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.NOME := Edit1.Text;
    Pedido.DATA := now;
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
  end;

end;

procedure TForm8.Button4Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  DAOPedido := TSimpleDAO<TPEDIDO>.New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1));
  try
    Pedido.ID := StrToInt(Edit2.Text);
    DAOPedido.Delete(Pedido);
  finally
    Pedido.Free;
  end;
end;

procedure TForm8.Button5Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  DAOPedido := TSimpleDAO<TPEDIDO>
                .New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1))
                .DataSource(DataSource1);

  Pedido := DAOPedido.Find(StrToInt(Edit2.Text));
  try
    Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
  finally
    Pedido.Free;
  end;
end;

procedure TForm8.Button6Click(Sender: TObject);
var
  DAOPedido : iSimpleDAO<TPEDIDO>;
  Pedidos : TList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  DAOPedido := TSimpleDAO<TPEDIDO>
                .New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWClientSQL1))
                .DataSource(DataSource1);

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

end.
