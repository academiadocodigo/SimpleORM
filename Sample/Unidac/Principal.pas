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
  UniProvider,
  InterBaseUniProvider,
  DBAccess,
  Uni,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.Grids,
  Vcl.DBGrids,
  SimpleInterface,
  SimpleDAO,
  SimpleAttributes,
  SimpleQueryUniDac,
  Entidade.Pedido;

type
  TForm1 = class(TForm)
    Button3: TButton;
    Button1: TButton;
    Button4: TButton;
    btnFind: TButton;
    Button6: TButton;
    Button7: TButton;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Button2: TButton;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    UniDataSource1: TUniDataSource;
    UniConnection1: TUniConnection;
    InterBaseUniProvider1: TInterBaseUniProvider;
    edtDatabase: TEdit;
    Label1: TLabel;
    edtHost: TEdit;
    edtPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edtUser: TEdit;
    edtPwd: TEdit;
    Label4: TLabel;
    Label5: TLabel;

    [Bind('NOME')]
    Edit1: TEdit;

    [Bind('ID')]
    Edit2: TEdit;

    [Bind('VALOR')]
    Edit3: TEdit;

    [Bind('DATA')]
    DateTimePicker1: TDateTimePicker;

    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    DAOPedido : iSimpleDAO<TPEDIDO>;
    procedure SetDataBase;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  System.Generics.Collections;

{$R *.dfm}

procedure TForm1.btnFindClick(Sender: TObject);
begin
  DAOPedido
    .SQL
      .OrderBy('ID')
    .&End
  .Find;
end;

procedure TForm1.Button1Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
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

procedure TForm1.Button3Click(Sender: TObject);
begin
  DAOPedido.Insert;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.Button4Click(Sender: TObject);
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

procedure TForm1.Button5Click(Sender: TObject);
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

procedure TForm1.Button6Click(Sender: TObject);
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

procedure TForm1.Button7Click(Sender: TObject);
begin
  DAOPedido
    .SQL
      .Where(' Nome = ' + QuotedStr(Edit1.Text))
    .&End
  .Find;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  DAOPedido.Update;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  DAOPedido.Delete;
  DAOPedido.SQL.OrderBy('ID').&End.Find;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Conn : iSimpleQuery;
begin
  SetDataBase;

  Conn := TSimpleQueryUniDac.New(UniConnection1);


  DAOPedido := TSimpleDAO<TPEDIDO>
                  .New(Conn)
                  .DataSource(UniDataSource1)
                  .BindForm(Self);
end;

procedure TForm1.SetDataBase;
var
 aux : string;
begin
  UniConnection1.Port     := StrToIntDef(edtPort.Text, 3050);
  UniConnection1.Server   := edtHost.Text;
  UniConnection1.Username := edtUser.Text;
  UniConnection1.Password := edtPwd.Text;
  aux := ExtractFilePath(ParamStr(0));
  edtDatabase.Text :=  Copy(aux, 1, ( Pos('\Sample',aux) )) + 'Sample\Database\PDVUPDATES.FDB';
  UniConnection1.Database := edtDatabase.Text;
end;

end.
