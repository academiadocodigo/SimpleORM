unit SimpleRTTIHelper;

interface

uses
  RTTI, SimpleAttributes;

type
  TCustomAttributeClass = class of TCustomAttribute;

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
    function IsNotNull: Boolean;
    function IsIgnore: Boolean;
    function IsAutoInc: Boolean;
    function EhCampo: Boolean;
    function EhChavePrimaria: Boolean;
    function EhChaveEstrangeira: Boolean;
    function EhSomenteNumeros: Boolean;
    function EhPermitidoNulo: Boolean;
    function DisplayName: string;
    function FieldName: string;
  end;

  TRttiTypeHelper = class helper for TRttiType
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
    function GetPropertyFromAttribute<T: TCustomAttribute>
      : TRttiProperty; overload;
    function GetPropertyFromAttribute<T: Campo>(const aFieldName: string)
      : TRttiProperty; overload;
    function GetPKField: TRttiProperty;
    function IsTabela: Boolean;
  end;

  TRttiFieldHelper = class helper for TRttiField
  public
    function Tem<T: TCustomAttribute>: Boolean;
    function GetAttribute<T: TCustomAttribute>: T;
  end;

implementation

{ TRttiPropertyMelhorado }

function TRttiPropertyHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiPropertyHelper.DisplayName: string;
begin
  Result := Name;

  if Tem<Display> then
    Result := GetAttribute<Display>.Name
end;

function TRttiPropertyHelper.EhCampo: Boolean;
begin
  Result := Tem<Campo>
end;

function TRttiPropertyHelper.EhChaveEstrangeira: Boolean;
begin
  Result := Tem<FK>
end;

function TRttiPropertyHelper.EhChavePrimaria: Boolean;
begin
  Result := Tem<PK>
end;

function TRttiPropertyHelper.IsNotNull: Boolean;
begin
  Result := Tem<NotNull>
end;

function TRttiPropertyHelper.IsIgnore: Boolean;
begin
  Result := Tem<Ignore>
end;

function TRttiPropertyHelper.IsAutoInc: Boolean;
begin
  Result := Tem<AutoInc>
end;

function TRttiPropertyHelper.EhPermitidoNulo: Boolean;
begin
  Result := not IsNotNull;
end;

function TRttiPropertyHelper.EhSomenteNumeros: Boolean;
begin
  Result := Tem<NumberOnly>
end;

function TRttiPropertyHelper.FieldName: string;
begin
  Result := Name;
  if EhCampo then
    Result := GetAttribute<Campo>.Name;
end;

function TRttiPropertyHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;

{ TRttiTypeMelhorado }

function TRttiTypeHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiTypeHelper.GetPKField: TRttiProperty;
begin
  Result := GetPropertyFromAttribute<PK>;
end;

function TRttiTypeHelper.GetPropertyFromAttribute<T>(
  const aFieldName: string): TRttiProperty;
var
  RttiProp: TRttiProperty;
begin
  Result := nil;
  for RttiProp in GetProperties do
  begin
    if RttiProp.GetAttribute<T> = nil then
      Continue;

    if RttiProp.GetAttribute<Campo>.Name = aFieldName then
      Exit(RttiProp);
  end;
end;

function TRttiTypeHelper.GetPropertyFromAttribute<T>: TRttiProperty;
var
  RttiProp: TRttiProperty;
begin
  Result := nil;
  for RttiProp in GetProperties do
    if RttiProp.GetAttribute<T> <> nil then
      Exit(RttiProp);
end;

function TRttiTypeHelper.isTabela: Boolean;
begin
  Result := Tem<Tabela>
end;

function TRttiTypeHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;

{ TRttiFieldHelper }

function TRttiFieldHelper.GetAttribute<T>: T;
var
  oAtributo: TCustomAttribute;
begin
  Result := nil;
  for oAtributo in GetAttributes do
    if oAtributo is T then
      Exit((oAtributo as T));
end;

function TRttiFieldHelper.Tem<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil
end;

end.

