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
{ Arquivo SimpleStoreProcInterface.pas }
{ Desenvolvido e escrito por : Henrique Leonardo - hleonardo@yahoo.com.br }
{ Skype / Telegram : hleorj }
{ Sendo a mesma doada ao Projeto: Componente SimpleOrm }
{ **************************************************************************** }

unit SimpleStoreProcInterface;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Data.DB,
  System.TypInfo;

type

  iSimpleStoreProc = interface
    ['{707123CC-8A18-4BEA-8014-60769773C252}']
    function NameStoreProc(Const aNome: String): iSimpleStoreProc;
    function Params(Const aNome: String; aValue: variant): iSimpleStoreProc;
    function ExecProc: iSimpleStoreProc;
    function Return(Const aNome: String; Out aValue: Variant ) : iSimpleStoreProc;
  end;

  iSimpleDAOStoreProc<T: class> = interface
    ['{E9014B4A-EA81-4F14-AA9B-4B7FE03C4E81}']
    function Update(aValue: T): iSimpleDAOStoreProc<T>;
    function Result: iSimpleDAOStoreProc<T>;
    function &End: iSimpleDAOStoreProc<T>;
  end;

implementation

end.
