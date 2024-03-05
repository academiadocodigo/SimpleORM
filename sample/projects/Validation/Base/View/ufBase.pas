unit ufBase;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, Data.DB, Vcl.DBGrids,
  FireDAC.Comp.Client;

type
  TStatus = (stInsercao, stEdicao, stNenhum);

  TfBase = class(TForm)
    btnCancelar: TButton;
    btnNovo: TButton;
    pnlErros: TPanel;
    lstSaidas: TListBox;
    btnSalvar: TButton;
    btnExcluir: TButton;
    btnEditar: TButton;
    conFireDac: TFDConnection;
    dtsDados: TDataSource;
    grdDados: TDBGrid;
    Label9: TLabel;
    edtPesquisar: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnNovoClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
  private
    procedure Limpar;
    procedure SetStatus(const poStatus: TStatus);
    function EstaEmModoEdicao: boolean;
  protected
    FStatus: TStatus;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfBase.btnCancelarClick(Sender: TObject);
begin
  SetStatus(stNenhum);
end;

procedure TfBase.btnEditarClick(Sender: TObject);
begin
  SetStatus(stEdicao);
end;

procedure TfBase.btnExcluirClick(Sender: TObject);
begin
  SetStatus(stNenhum);
end;

procedure TfBase.btnNovoClick(Sender: TObject);
begin
  SetStatus(stInsercao);
  Limpar;
end;

procedure TfBase.btnSalvarClick(Sender: TObject);
begin
  SetStatus(stNenhum);
end;

procedure TfBase.Limpar;
var
  nIndex: Integer;
begin
  for nIndex := 0 to Self.ComponentCount-1 do
  begin
    if Components[nIndex] is TCustomEdit then
      (Components[nIndex] as TCustomEdit).Clear;

    if Components[nIndex] is TComboBox then
      (Components[nIndex] as TComboBox).ItemIndex := -1;

    if Components[nIndex] is TDateTimePicker then
      (Components[nIndex] as TDateTimePicker).Date := 0;
  end;
end;

procedure TfBase.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;

  SetStatus(stNenhum);
end;

function TfBase.EstaEmModoEdicao: boolean;
begin
  Result := FStatus in [stInsercao, stEdicao];
end;

procedure TfBase.SetStatus(const poStatus: TStatus);
begin
  FStatus := poStatus;

  btnNovo.Enabled     := not EstaEmModoEdicao;
  btnEditar.Enabled   := not EstaEmModoEdicao;
  btnExcluir.Enabled  := not EstaEmModoEdicao;
  btnSalvar.Enabled   := EstaEmModoEdicao;
  btnCancelar.Enabled := EstaEmModoEdicao;
end;

end.
