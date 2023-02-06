unit intensive.view.Editar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Data.DB,
  Vcl.Grids, Vcl.DBGrids, intensive.Controller.Interfaces,
  intensive.Controller;

type
  TfrmEditar = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    btnSalvar: TButton;
    btnCancelar: TButton;
    edtNome: TLabeledEdit;
    edtTelefone: TLabeledEdit;
    edtLogradouro: TLabeledEdit;
    edtCep: TLabeledEdit;
    edtBairro: TLabeledEdit;
    edtCidade: TLabeledEdit;
    edtEstado: TLabeledEdit;
    grdEnderecos: TDBGrid;
    dsEndereco: TDataSource;
    edtId: TLabeledEdit;
    procedure FormDestroy(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure grdEnderecosCellClick(Column: TColumn);
  private
    FEditar : Boolean;
    FDts : TDataSource;
    FController : iController;

    procedure FillFields;
  public
    constructor Create(AOwner: TComponent; aDataSource: TDataSource;
       aEditar: Boolean = true);
  end;

var
  frmEditar: TfrmEditar;

implementation

{$R *.dfm}

{ TfrmEditar }

procedure TfrmEditar.btnSalvarClick(Sender: TObject);
begin
    FController.Cliente
        .Nome(edtNome.Text)
        .Telefone(edtTelefone.Text)
    .Build.Inserir;
//    FController.Endereco
//        .IdCliente(FController.Cliente.Id)
//        .Logradouro(edtLogradouro.Text)
//        .Cep(edtCep.Text)
//        .Bairro(edtBairro.Text)
//        .Cidade(edtCidade.Text)
//        .Estado(edtEstado.Text)
//    .Build.Inserir;
    FillFields;
end;

constructor TfrmEditar.Create(AOwner: TComponent; aDataSource: TDataSource;
       aEditar: Boolean = true);
begin
  inherited Create(AOwner);
  FDts := TDataSource.Create(nil);
  Fdts := aDataSource;
  FEditar := aEditar;
  if aEditar then
  begin
    edtNome.Text := FDts.DataSet.FieldByName('NOME').AsString;
    edtTelefone.Text := FDts.DataSet.FieldByName('TELEFONE').AsString;
  end;
  FController := TController.New;
end;

procedure TfrmEditar.grdEnderecosCellClick(Column: TColumn);
begin
  edtLogradouro.Text := dsEndereco.DataSet.FieldByName('LOGRADOURO').AsString;
  edtCep.Text := dsEndereco.DataSet.FieldByName('CEP').AsString;
  edtBairro.Text := dsEndereco.DataSet.FieldByName('BAIRRO').AsString;
  edtCidade.Text := dsEndereco.DataSet.FieldByName('CIDADE').AsString;
  edtEstado.Text := dsEndereco.DataSet.FieldByName('ESTADO').AsString;
end;

procedure TfrmEditar.FillFields;
begin
    edtid.Text := FController.Cliente.Id.ToString;
    edtNome.Text := FController.Cliente.Nome;
    edtTelefone.Text := FController.Cliente.Telefone;
//    edtLogradouro.Text := FController.Endereco.Logradouro;
//    edtCep.Text := FController.Endereco.Cep;
//    edtBairro.Text := FController.Endereco.Bairro;
//    edtCidade.Text := FController.Endereco.Cidade;
//    edtEstado.Text := FController.Endereco.Estado;
//
//    dsEndereco.DataSet := FController.Endereco.Build.Listar;
end;

procedure TfrmEditar.FormDestroy(Sender: TObject);
begin
  FDts.DisposeOf;
end;

end.
