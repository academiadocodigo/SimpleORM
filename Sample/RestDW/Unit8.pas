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
    
    [Campo('ID')]
    Edit1: TEdit;
    [Campo('NOME')]
    Edit2: TEdit;

    btnFind: TButton;
    Memo1: TMemo;
    Button2: TButton;
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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
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
  Pedidos : TObjectList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := TObjectList<TPEDIDO>.Create();
   DAOPedido
              .SQL
                .OrderBy('ID')
              .&End
              .Find(Pedidos);
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
    end;
  finally
    Pedidos.free;
  end;

end;

procedure TForm8.Button1Click(Sender: TObject);
begin
  DAOPedido
    .SQL
      .Fields('MAX(ID)')
    .&End
    .Find;
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
  Pedidos := TObjectList<TPEDIDO>.Create();
  DAOPedido
              .SQL
                .Where(' Nome = ' + QuotedStr(Edit1.Text))
              .&End
              .Find;
  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
    end;
  finally
    Pedidos.Free;
  end;
end;

procedure TForm8.DBGrid1DblClick(Sender: TObject);
begin
  Edit1.Text := DataSource1.DataSet.FieldByName('NOME').AsString;
  Edit2.Text := DataSource1.DataSet.FieldByName('ID').AsString;
end;

Procedure TForm8.FormCreate(Sender: TObject);
begin
  DAOPedido := TSimpleDAO<TPEDIDO>
                .New(TSimpleQueryRestDW<TPEDIDO>.New(RESTDWDataBase1))
                .DataSource(DataSource1)
                .BindForm(Self);
end;

end.
