unit intensive.View.Principal;

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
  Vcl.Grids,
  Vcl.DBGrids,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Datasnap.DBClient,
  Vcl.ExtCtrls,
  intensive.View.Editar, intensive.Controller.Interfaces, intensive.Controller;

type
  TfrmListarClientes = class(TForm)
    dsCliente: TDataSource;
    Panel1: TPanel;
    grdPessoa: TDBGrid;
    Panel2: TPanel;
    btnNovo: TButton;
    btnEditar: TButton;
    btnExcluir: TButton;
    btnListar: TButton;
    grdEndereco: TDBGrid;
    Panel3: TPanel;
    dsEndereco: TDataSource;
    procedure btnNovoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnListarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
  private
    FController : iController;
  public
    { Public declarations }
  end;

var
  frmListarClientes: TfrmListarClientes;

implementation

{$R *.dfm}


procedure TfrmListarClientes.btnEditarClick(Sender: TObject);
begin
//  try
//    frmEditar := TfrmEditar.Create(self, dsEndereco);
//    frmEditar.ShowModal;
//  finally
//    frmEditar.Destroy;
//  end;
end;

procedure TfrmListarClientes.btnListarClick(Sender: TObject);
begin
  dsCliente.DataSet := FController.Cliente.Build.Listar;
//  dsEndereco.DataSet := FController.Endereco
//    .Build.ListarPorFiltro('ID_CLIENTE',dsCliente.DataSet.FieldByName('ID').AsInteger);
end;

procedure TfrmListarClientes.btnNovoClick(Sender: TObject);
begin
  try
    frmEditar := TfrmEditar.Create(self, dsCliente, false);
    frmEditar.ShowModal;
  finally
    frmEditar.Destroy;
  end;
end;

procedure TfrmListarClientes.FormCreate(Sender: TObject);
begin
  FController := TController.New;
end;

end.
