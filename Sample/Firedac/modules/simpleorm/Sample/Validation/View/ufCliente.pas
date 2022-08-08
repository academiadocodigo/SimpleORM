unit ufCliente;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  ufBase, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Entidade.Cliente,
  SimpleInterface, SimpleDAO, SimpleAttributes, SimpleQueryFiredac, Data.DB,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, Vcl.DBGrids;

type
  TfCliente = class(TfBase)
    btnGerarRelatorio: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;

    [Bind('CODIGO')]
    edtCodigo: TEdit;
    [Bind('nome')]
    edtNome: TEdit;
    [Bind('documento')]
    edtDocumento: TEdit;
    [Bind('Endereco')]
    edtEndereco: TEdit;
    [Bind('CREDITO')]
    edtCredito: TEdit;
    [Bind('SEXO')]
    cbxSexo: TComboBox;
    [Bind('DTNASC')] // Pode usar o nome da propriedade quanto pelo nome do campo no banco de dados
    dtpNascimento: TDateTimePicker;
    [Bind('DataCadastro')]
    dtpCadastro: TDateTimePicker;
    btnGerarJson: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnGerarRelatorioClick(Sender: TObject);
    procedure edtPesquisarKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnGerarJsonClick(Sender: TObject);
  private
    DAOCliente: iSimpleDAO<TCliente>;
    procedure BuscarTodosClientes;
  end;

var
  fCliente: TfCliente;

implementation

uses
  SimpleValidator, uExport, System.Generics.Collections;

{$R *.dfm}

procedure TfCliente.btnCancelarClick(Sender: TObject);
begin
  inherited;
  BuscarTodosClientes;
end;

procedure TfCliente.btnEditarClick(Sender: TObject);
begin
  inherited;
  edtNome.SetFocus;
  edtNome.SelectAll;
end;

procedure TfCliente.btnExcluirClick(Sender: TObject);
begin
  inherited;
  DAOCliente.Delete;
  BuscarTodosClientes;
end;

procedure TfCliente.btnNovoClick(Sender: TObject);
begin
  inherited;
  edtCodigo.Text := '0';
  edtCredito.Text := '0';
  edtCodigo.SetFocus;
  edtCodigo.SelectAll;
end;

procedure TfCliente.btnSalvarClick(Sender: TObject);
var
  oCliente: TCliente;
begin
  lstSaidas.Clear;
  oCliente := TCliente.Create;
  try
    oCliente.Parse(Self);
    TSimpleValidator.Validate(oCliente, lstSaidas.Items);

    if lstSaidas.Items.Count > 0 then
      raise Exception.Create('Encontrado Erros de preenchimento!');

    case FStatus of
      stInsercao:
        DAOCliente.Insert(oCliente);
      stEdicao:
        DAOCliente.Update(oCliente);
    end;
    BuscarTodosClientes;

    inherited;
  finally
    FreeAndNil(oCliente);
  end;
end;

procedure TfCliente.btnGerarJsonClick(Sender: TObject);
var
  oClientes: TClientes;
begin
  inherited;
  lstSaidas.Items.Clear;

  oClientes := TClientes.Create;
  try
    oClientes.Parse(dtsDados.DataSet);
    lstSaidas.Items.Add(oClientes.ToJSON);
    lstSaidas.Items.Add(oClientes.ToJSONRefletion);
  finally
    FreeAndNil(oClientes);
  end;
end;

procedure TfCliente.btnGerarRelatorioClick(Sender: TObject);
var
  oClientes: TClientes;
begin
  inherited;
  oClientes := TClientes.Create;
  try
    oClientes.Parse(dtsDados.DataSet);
    TExport.TextReport<TCliente>(oClientes);
  finally
    FreeAndNil(oClientes);
  end;
end;

procedure TfCliente.edtPesquisarKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key = VK_RETURN then
  begin
    DAOCliente
      .SQL
        .Where(' NOME LIKE '+ QuotedStr('%' + edtPesquisar.Text + '%'))
        .OrderBy('ID')
      .&End
    .Find;
  end;
end;

procedure TfCliente.FormCreate(Sender: TObject);
var
  Conn : iSimpleQuery;
begin
  inherited;

  Conn := TSimpleQueryFiredac.New(conFireDac);


  DAOCliente := TSimpleDAO<TCliente>
                  .New(Conn)
                  .DataSource(dtsDados)
                  .BindForm(Self);
end;

procedure TfCliente.BuscarTodosClientes;
begin
  DAOCliente
    .SQL
      .OrderBy('ID')
    .&End
  .Find;
end;

procedure TfCliente.FormShow(Sender: TObject);
begin
  inherited;

  BuscarTodosClientes;
end;

end.
