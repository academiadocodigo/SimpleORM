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
{ Arquivo SimpleDAOStoreProc.pas }
{ Desenvolvido e escrito por : Henrique Leonardo - hleonardo@yahoo.com.br }
{ Skype / Telegram : hleorj }
{ Sendo a mesma doada ao Projeto: Componente SimpleOrm }
{ **************************************************************************** }

unit SimpleDAOStoreProc;

interface

uses
  System.RTTI,
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Variants,
  SimpleInterface,
  SimpleRTTI,
  SimpleAttributes,
  SimpleSP,
  SimpleStoreProcInterface;

Type
  TSimpleDAOStoreProc<T: class, constructor> = class(TInterfacedObject,
    iSimpleDAOStoreProc<T>)
  private
    FStoreProc: iSimpleStoreProc;
    FiSimple: iSimpleStoreProcedure<T>;
    fValue: T;
    fStoreProcName: String;
    function FillParameterStoreProc(aInstance: T): iSimpleDAOStoreProc<T>;
    procedure ATribuirResult(aValue: T; aVariant: Variant);
    function VarToInt(const aVariant: Variant): integer;
  public
    constructor Create(aStoreProc: iSimpleStoreProc);
    class function New(aStoreProc: iSimpleStoreProc): iSimpleDAOStoreProc<T>;
    destructor Destroy; override;
    function Execute(aValue: T): iSimpleDAOStoreProc<T>;
    function Result: iSimpleDAOStoreProc<T>;
    function &End: iSimpleDAOStoreProc<T>;
  end;

implementation

destructor TSimpleDAOStoreProc<T>.Destroy;
begin
  inherited;
end;

function TSimpleDAOStoreProc<T>.Execute(aValue: T): iSimpleDAOStoreProc<T>;
begin
  Result := Self;
  fValue := aValue;
  FiSimple := TSimpleSP<T>.New(aValue).ProcName(fStoreProcName);
  FStoreProc.NameStoreProc(fStoreProcName);
  Self.FillParameterStoreProc(fValue);
  FStoreProc.ExecProc;
end;

function TSimpleDAOStoreProc<T>.&End: iSimpleDAOStoreProc<T>;
begin
  Result := Self;
end;

constructor TSimpleDAOStoreProc<T>.Create(aStoreProc: iSimpleStoreProc);
begin
  FStoreProc := aStoreProc;
end;

class function TSimpleDAOStoreProc<T>.New(aStoreProc: iSimpleStoreProc)
  : iSimpleDAOStoreProc<T>;
begin
  Result := Self.Create(aStoreProc);
end;

function TSimpleDAOStoreProc<T>.FillParameterStoreProc(aInstance: T)
  : iSimpleDAOStoreProc<T>;
var
  Key: String;
  DictionaryFields: TDictionary<String, Variant>;
begin
  Result := Self;
  DictionaryFields := TDictionary<String, Variant>.Create;
  TSimpleRTTI<T>.New(aInstance).DictionaryFields(DictionaryFields);
  try
    for Key in DictionaryFields.Keys do
    begin
      FStoreProc.Params(Key, DictionaryFields.Items[Key]);
    end;
  finally
    FreeAndNil(DictionaryFields);
  end;
end;

function TSimpleDAOStoreProc<T>.Result: iSimpleDAOStoreProc<T>;
var
  aVariant: Variant;
begin
  Result := Self;
  FiSimple.ProcResult(fStoreProcName);
  FStoreProc.Return(fStoreProcName, aVariant);
  ATribuirResult(fValue, aVariant);
end;

function TSimpleDAOStoreProc<T>.VarToInt(const aVariant: Variant): integer;
begin
  // Se pode fazer outras conversoes para variant
  // nesta versao retorna um inteiro
  Result := StrToIntDef(Trim(VarToStr(aVariant)), 0);
end;

procedure TSimpleDAOStoreProc<T>.ATribuirResult(aValue: T; aVariant: Variant);
var
  Info: PTypeInfo;
  Prop: TRttiProperty;
  ctxRtti: TRttiContext;
  typRtti: TRttiType;
  Atributo: TCustomAttribute;
begin
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for Prop in typRtti.GetProperties do
    begin
      for Atributo in Prop.GetAttributes do
      begin
        if Atributo is FieldResult then
        Begin
          Prop.SetValue(TObject(aValue), VarToInt(aVariant));
          break;
        End;
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;

end.
