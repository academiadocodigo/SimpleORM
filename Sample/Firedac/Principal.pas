unit Principal;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  FireDAC.UI.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Phys,
  FireDAC.Phys.FB,
  FireDAC.Phys.FBDef,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  SimpleInterface,
  SimpleDAO,
  Entidade.Pedido,
  System.Generics.Collections,
  SimpleQueryFiredac,
  SimpleAttributes,
  Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TForm9 = class(TForm)
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
    Button2: TButton;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    edtPwd: TEdit;
    Label5: TLabel;
    edtUser: TEdit;
    Label4: TLabel;
    edtPort: TEdit;
    Label3: TLabel;
    edtHost: TEdit;
    Label2: TLabel;
    edtDatabase: TEdit;
    Label1: TLabel;

    [Bind('NOME')]
    Edit1: TEdit;

    [Bind('ID')]
    Edit2: TEdit;

    [Bind('VALOR')]
    Edit3: TEdit;

    [Bind('DATA')]
    DateTimePicker1: TDateTimePicker;

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
    DAOPedido : iSimpleDAO<TPEDIDO>;
    procedure SetDataBase;
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
    Pedido.VALOR := StrToCurr(Edit3.Text);
    DAOPedido.Update(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TForm9.Button2Click(Sender: TObject);
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
      Memo1.Lines.Add(Pedido.NOME + DateToStr(Pedido.DATA));
    end;
  finally
    Pedidos.Free;
  end;
end;

procedure TForm9.Button3Click(Sender: TObject);
begin
  DAOPedido.Insert;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
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

procedure TForm9.Button5Click(Sender: TObject);
var
  Pedido : TPEDIDO;
begin
  Pedido := TPEDIDO.Create;
  try
    Pedido.ID := StrToInt(Edit2.Text);
    Pedido.NOME := Edit1.Text;
    Pedido.DATA := now;
    Pedido.VALOR := StrToCurr(Edit3.Text);
    DAOPedido.Insert(Pedido);
  finally
    Pedido.Free;
    btnFindClick(nil);
  end;
end;

procedure TForm9.btnFindClick(Sender: TObject);
begin
  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find;
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
begin
  DAOPedido
    .SQL
      .Where(' Nome = ' + QuotedStr(Edit1.Text))
    .&End
  .Find;
end;

procedure TForm9.Button8Click(Sender: TObject);
begin
  DAOPedido.Update;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm9.Button9Click(Sender: TObject);
begin
  DAOPedido.Delete;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm9.FormCreate(Sender: TObject);
var
  Conn : iSimpleQuery;
begin
  SetDataBase;

  Conn := TSimpleQueryFiredac.New(FDConnection1);


  DAOPedido := TSimpleDAO<TPEDIDO>
                  .New(Conn)
                  .DataSource(DataSource1)
                  .BindForm(Self);
end;

procedure TForm9.SetDataBase;
var
 aux : string;
begin
  aux := ExtractFilePath(ParamStr(0));
  edtDatabase.Text :=  Copy(aux, 1, ( Pos('\Sample',aux) )) + 'Sample\Database\PDVUPDATES.FDB';

  with (FDConnection1.Params as TFDPhysFBConnectionDefParams) do
    begin
       Server   := edtHost.Text;
       Port     := StrToIntDef(edtPort.Text, 3050);
       UserName := edtUser.Text;
       Password := edtPwd.Text;
       Database := edtDatabase.Text;
    end;
end;

end.
