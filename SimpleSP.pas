{ ****************************************************************************** }
{ Projeto: Componente SimpleOrm }
{ Colaboradores nesse arquivo: }
{ }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }

{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }

{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ ****************************************************************************** }
{ Arquivo SimpleSP.pas }
{ Desenvolvido e escrito por : Henrique Leonardo - hleonardo@yahoo.com.br }
{ Skype / Telegram : hleorj }
{ Sendo a mesma doada ao Projeto: Componente SimpleOrm }
{ **************************************************************************** }

unit SimpleSP;

interface

uses
  SimpleInterface;

Type
  TSimpleSP<T: class, constructor> = class(TInterfacedObject, iSimpleStoreProcedure<T>)
  private
    FInstance: T;
  public
    constructor Create(aInstance: T);
    destructor Destroy; override;
    class function New(aInstance: T): iSimpleStoreProcedure<T>;
    function ProcName(Out aStoreProcName: String): iSimpleStoreProcedure<T>;
    function ProcResult(var aResult: String): iSimpleStoreProcedure<T>;
  end;

implementation

uses
  SimpleRTTI, System.Generics.Collections;

{ TSimpleStoreProcedure<T> }

constructor TSimpleSP<T>.Create(aInstance: T);
begin
  FInstance := aInstance;
end;

destructor TSimpleSP<T>.Destroy;
begin

  inherited;
end;

class function TSimpleSP<T>.New(aInstance: T)
  : iSimpleStoreProcedure<T>;
begin
  Result := Self.Create(aInstance);
end;

function TSimpleSP<T>.ProcName(out aStoreProcName: String)
  : iSimpleStoreProcedure<T>;
{ Retorna o nome da Store Procedure }
var
  lProcedureName: String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance).StoreProcName(lProcedureName);
  aStoreProcName := lProcedureName;
end;

function TSimpleSP<T>.ProcResult(var aResult: String)
  : iSimpleStoreProcedure<T>;
{ Busca o atribuito de Retorno da Store Procedure }
var
  lResult: String;
begin
  Result := Self;
  TSimpleRTTI<T>.New(FInstance).StoreProcResult(lResult);
  aResult := lResult;
end;

end.
