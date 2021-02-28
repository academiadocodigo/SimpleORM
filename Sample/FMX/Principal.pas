unit Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
   Entidade.Pedido, SimpleInterface, FMX.Edit, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.FMXUI.Wait, Data.DB,
  FireDAC.Comp.Client, FMX.DateTimeCtrls, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, Datasnap.DBClient,
  FMX.ListView, Datasnap.Provider, FMX.Grid.Style, Fmx.Bind.Grid,
  Data.Bind.Controls, FMX.Layouts, Fmx.Bind.Navigator, Data.Bind.Grid,
  SimpleAttributes,
  FMX.ScrollBox, FMX.Grid, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;

    [Bind('ID')]
    Edit2: TEdit;
    [Bind('CLIENTE')]
    Edit1: TEdit;
    [Bind('VALORTOTAL')]
    Edit3: TEdit;
    [Bind('DATAPEDIDO')]
    DateEdit1: TDateEdit;

    btnfind: TButton;
    FDConnection1: TFDConnection;
    DataSource1: TDataSource;
    Button3: TButton;
    Button4: TButton;
    BindSourceDB1: TBindSourceDB;
    StringGridBindSourceDB1: TStringGrid;
    BindingsList1: TBindingsList;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Memo1: TMemo;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnfindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    DAOPedido : iSimpleDAO<TPEDIDO>;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


uses System.Generics.Collections,
     SimpleQueryFiredac, SimpleDAO;

{$R *.fmx}

procedure TForm1.btnfindClick(Sender: TObject);
begin
  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DAOPedido.Insert;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.CLIENTE := Edit1.Text;
    Pedido.DATAPEDIDO := now;
    Pedido.VALORTOTAL := StrToCurr(Edit3.Text);
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
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

procedure TForm1.Button4Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.CLIENTE := Edit1.Text;
    Pedido.DATAPEDIDO := now;
    Pedido.VALORTOTAL := StrToCurr(Edit3.Text);
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  DAOPedido.Delete;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  DAOPedido.Update;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  Pedidos : TObjectList<TPEDIDO>;
  Pedido : TPEDIDO;
begin
  Pedidos := TObjectList<TPEDIDO>.Create;

  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find(Pedidos);

  try
    for Pedido in Pedidos do
    begin
      Memo1.Lines.Add(Pedido.CLIENTE + DateToStr(Pedido.DATAPEDIDO));
    end;
  finally
    Pedidos.Free;
  end;

end;

procedure TForm1.Button8Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := DAOPedido.Find(StrToInt(Edit2.Text));
  try
    Memo1.Lines.Add(Pedido.CLIENTE + DateToStr(Pedido.DATAPEDIDO));
  finally
    Pedido.Free;
  end;

end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  DAOPedido
    .SQL
      .Where(' Nome = ' + QuotedStr(Edit1.Text))
    .&End
  .Find;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Conn : iSimpleQuery;
begin
  ReportMemoryLeaksOnShutdown := true;

  Conn := TSimpleQueryFiredac.New(FDConnection1);


  DAOPedido := TSimpleDAO<TPEDIDO>
                  .New(Conn)
                  .DataSource(DataSource1)
                  .BindForm(Self);

end;

end.
