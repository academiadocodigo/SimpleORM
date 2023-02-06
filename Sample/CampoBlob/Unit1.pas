unit Unit1;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.VCLUI.Wait,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  SimpleQueryFiredac,
  SimpleInterface,
  SimpleDAO,
  cliente,
  Vcl.ComCtrls,
  System.Generics.Collections,
  Vcl.Imaging.jpeg,
  Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    imgFoto: TImage;
    SpeedButton1: TSpeedButton;
    Panel4: TPanel;
    Panel5: TPanel;
    edtCodigo: TLabeledEdit;
    edtNome: TLabeledEdit;
    Panel6: TPanel;
    btnListar: TButton;
    btnListarPorID: TButton;
    ListarPorNome: TButton;
    btnInserir: TButton;
    btnAtualizar: TButton;
    btnExcluir: TButton;
    FDConnection1: TFDConnection;
    OpenDialog1: TOpenDialog;
    ListView1: TListView;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure btnListarClick(Sender: TObject);
    procedure btnListarPorIDClick(Sender: TObject);
    procedure ListarPorNomeClick(Sender: TObject);
    procedure btnInserirClick(Sender: TObject);
    procedure btnAtualizarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
  private
    DAO: iSimpleDAO<TCliente>;
    FCliente : TCliente;
    procedure CriaTabelaSeNaoExistir;
    procedure PreparaORM;
    procedure PreencherListView(Clientes: TObjectList<TCliente>); overload;
    procedure PreencherListView(cliente: TCliente); overload;
    procedure ImageDefault;
    procedure LimparCampos;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnListarClick(Sender: TObject);
var
  lClientes: TObjectList<TCliente>;
begin
  lClientes := TObjectList<TCliente>.Create;
  try
    DAO.Find(lClientes);
    PreencherListView(lClientes);
  finally
    lClientes.DisposeOf;
  end;
end;

procedure TForm1.btnListarPorIDClick(Sender: TObject);
var
  lId: Integer;
  lCliente: TCliente;
begin
  if not TryStrToInt(edtCodigo.Text, lId) then
  begin
    ShowMessage('É necessário informar o id');
    Exit;
  end;
  lCliente := DAO.Find(lId);

  if not (lCliente.id <> 0) then
  begin
    ShowMessage('Não foi encontrado nenhum cliente com o id informado');
    LimparCampos;
    exit;
  end;
  PreencherListView(lCliente);
  LimparCampos;
end;

procedure TForm1.ListarPorNomeClick(Sender: TObject);
var
  lCliente: TCliente;
begin
  if not(edtNome.Text <> '') then
  begin
    ShowMessage('É necessário colocar o nome para pesquisar');
    Exit;
  end;
  DAO.Find(lCliente, 'NOME', edtNome.Text);
  PreencherListView(lCliente);
  LimparCampos;
end;

procedure TForm1.btnInserirClick(Sender: TObject);
var
  lCliente: TCliente;
  lFoto: TMemoryStream;
begin
  lCliente := TCliente.Create;
  lFoto := TMemoryStream.Create;
  try
    try
       if not (imgFoto.Picture = nil) then
       begin
         imgFoto.Picture.SaveToStream(lFoto);
         lCliente.Foto := lFoto;
       end;
       lCliente.Nome := edtNome.Text;

       DAO.Insert(lCliente);

       LimparCampos;
    except
      raise Exception.Create('Não foi possivel iniserir o cliente');
    end;
  finally
    lCliente.DisposeOf;
    lFoto.DisposeOf;
    btnListarClick(nil);
  end;
end;

procedure TForm1.btnAtualizarClick(Sender: TObject);
var
  lId: Integer;
  lCliente: TCliente;
  lFoto: TMemoryStream;
begin
  lCliente := TCliente.Create;
  lFoto := TMemoryStream.Create;
  try
    if not TryStrToInt(edtCodigo.Text, lId) then
    begin
      ShowMessage('É necessario informar o ID');
      Exit;
    end;

     if not (imgFoto.Picture = nil) then
     begin
      imgFoto.Picture.SaveToStream(lFoto);
      lCliente.Foto := lFoto;
     end;

     lCliente.Id := StrToInt(edtCodigo.Text);
    lCliente.Nome := edtNome.Text;

    DAO.Update(lCliente);

    LimparCampos;
  finally
    lCliente.DisposeOf;
    lFoto.DisposeOf;
    btnListarClick(nil)
  end;
end;

procedure TForm1.btnExcluirClick(Sender: TObject);
var
  lId: Integer;
  lCliente: TCliente;
begin
  lCliente := TCliente.Create;
  try
    if not TryStrToInt(edtCodigo.Text, lId) then
    begin
      ShowMessage('É necessrio informar o id');
      Exit;
    end;
    lCliente.Id := lId;

    DAO.Delete(lCliente);

    LimparCampos;
  finally
    lCliente.DisposeOf;
    btnListarClick(nil)
  end;
end;

procedure TForm1.CriaTabelaSeNaoExistir;
var
  lTabela: string;
begin
  lTabela := 'CREATE TABLE if not exists cliente (' +
    'id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,' + 'nome varchar(60),' +
    'foto blob' + ')';
  FDConnection1.ExecSQL(lTabela);
end;

procedure TForm1.PreencherListView(Clientes: TObjectList<TCliente>);
var
  lList: TListItem;
  lCliente: TCliente;
begin
  if not Assigned(Clientes) then
  begin
    ShowMessage('Nenhum cliente informado');
    Exit;
  end;
  ListView1.Clear;
  for lCliente in Clientes do
  begin
    lList := ListView1.Items.Add;
    lList.Caption := IntToStr(lCliente.Id);
    lList.SubItems.Add(lCliente.Nome);
    lList.Data := lCliente;
  end;
end;

procedure TForm1.PreencherListView(cliente: TCliente);
var
  lList: TListItem;
begin
  if not Assigned(cliente) then
  begin
    ShowMessage('Não existe cliente informado');
    Exit;
  end;
  ListView1.Clear;
  lList := ListView1.Items.Add;
  lList.Caption := IntToStr(cliente.Id);
  lList.SubItems.Add(cliente.Nome);
  lList.Data := cliente;
  imgFoto.Picture.LoadFromStream(Cliente.Foto);
end;

procedure TForm1.PreparaORM;
var
  lConn: iSimpleQuery;
begin
  lConn := TSimpleQueryFiredac.New(FDConnection1);
  DAO := TSimpleDAO<TCliente>.New(lConn);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDConnection1.Connected := True;
  CriaTabelaSeNaoExistir;
  PreparaORM;
  LimparCampos;
end;

procedure TForm1.ImageDefault;
var
  lResource: TResourceStream;
begin
  lResource:= TResourceStream.Create(HInstance, 'userdefault', RT_RCDATA);
  try
    imgFoto.Picture.LoadFromStream(lResource);
  finally
    lResource.Free;
  end;
end;

procedure TForm1.LimparCampos;
begin
  edtCodigo.Clear;
  edtNome.Clear;
//  ImageDefault;
end;

procedure TForm1.ListView1Click(Sender: TObject);
var
cliente:TCliente;
id:integer;
  lpng: TPNGImage;
  ljpg: TJPEGImage;
begin
  if not ListView1.ItemIndex >= 0 then
    exit;
  TryStrToInt(ListView1.Items[ListView1.Selected.Index].Caption,id);
  cliente:=dao.Find(id);
  edtCodigo.Text := ListView1.Items[ListView1.Selected.Index].Caption;
  edtNome.Text := ListView1.Items[ListView1.Selected.Index].SubItems[0];
  if not (imgFoto.Picture = nil) then
   imgFoto.Picture.LoadFromStream(cliente.Foto);
end;

procedure TForm1.ListView1DblClick(Sender: TObject);
begin
  edtCodigo.Text := ListView1.Items[ListView1.Selected.Index].Caption;
  edtNome.Text := ListView1.Items[ListView1.Selected.Index].SubItems[0];
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    imgFoto.Picture.LoadFromFile(OpenDialog1.FileName);
end;

end.
