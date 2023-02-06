unit SimpleValidator;


interface

uses
  System.Classes, RTTI, SimpleAttributes, System.SysUtils, SimpleRTTIHelper;

type
  ESimpleValidator = class(Exception);

  TSimpleValidator = class
  private
    class procedure ValidateNotNull(const aErrros: TStrings; const aObject:
      TObject; const aProperty: TRttiProperty); static;
  public
    class procedure Validate(const aObject: TObject); overload; static;
    class procedure Validate(const aObject: TObject; const aErrors:
      TStrings); overload; static;
    class function IsClassRtti(const aObject: TObject): Boolean; static;
  end;

implementation

const
  sMSG_NOT_NULL = 'O Campo %s não foi informado!';
  sMSG_NUMBER_NOT_NULL = 'O Campo %s não pode ser Zero!';
  sMSG_TIME_NOT_NULL = 'É obrigatório informar uma hora válida para %s';
  sMSG_DATE_NOT_NULL = 'É obrigatório informar uma data válida para %s';

class procedure TSimpleValidator.Validate(const aObject: TObject; const
  aErrors: TStrings);
var
  ctxRttiEntity: TRttiContext;
  typRttiEntity: TRttiType;
  prpRtti: TRttiProperty;
  Value: TValue;
begin
  ctxRttiEntity := TRttiContext.Create;
  typRttiEntity := ctxRttiEntity.GetType(aObject.ClassType);

  for prpRtti in typRttiEntity.GetProperties do
  begin
    Value := prpRtti.GetValue(aObject);
    if Value.IsObject then
      Validate(Value.AsObject, aErrors);

    if prpRtti.IsNotNull then
      ValidateNotNull(aErrors, aObject, prpRtti);
  end;
end;

class function TSimpleValidator.IsClassRtti(const aObject: TObject): Boolean;
var
  ctxRttiEntity: TRttiContext;
  typRttiEntity: TRttiType;
begin
  ctxRttiEntity := TRttiContext.Create;
  typRttiEntity := ctxRttiEntity.GetType(aObject.ClassType);
  Exit(typRttiEntity.GetAttributes <> nil);
end;

class procedure TSimpleValidator.Validate(const aObject: TObject);
var
  sErrors: TStringList;
begin
  sErrors := TStringList.Create;
  try
    TSimpleValidator.Validate(aObject, sErrors);
    if sErrors.Count > 0 then
      raise ESimpleValidator.Create('Encontrado erros de preenchimento!'#13 +
        'Detalhes:'#13 + sErrors.Text);
  finally
    FreeAndNil(sErrors);
  end;
end;

class procedure TSimpleValidator.ValidateNotNull(const aErrros: TStrings; const
  aObject: TObject; const aProperty: TRttiProperty);
var
  Value: TValue;
begin
  Value := aProperty.GetValue(aObject);
  case Value.Kind of
    tkUString:
      if string.IsNullOrWhiteSpace(Value.AsString) then
        aErrros.Add(Format(sMSG_NOT_NULL, [aProperty.DisplayName]));
    tkInteger:
      if (Value.AsInteger = 0) then
        aErrros.Add(Format(sMSG_NUMBER_NOT_NULL, [aProperty.DisplayName]));
    tkFloat:
      begin
        if (Value.AsExtended <> 0) then
          Exit;

        if (Value.TypeInfo = TypeInfo(Real)) or (Value.TypeInfo = TypeInfo(Double)) then
          aErrros.Add(Format(sMSG_NUMBER_NOT_NULL, [aProperty.DisplayName]));

        if Value.TypeInfo = TypeInfo(TTime) then
          aErrros.Add(Format(sMSG_TIME_NOT_NULL, [aProperty.DisplayName]));

        if (Value.TypeInfo = TypeInfo(TDate)) or (Value.TypeInfo = TypeInfo(TDateTime))
          then
          aErrros.Add(Format(sMSG_DATE_NOT_NULL, [aProperty.DisplayName]));
      end;
  else
    if Value.IsEmpty then
      aErrros.Add(Format(sMSG_NOT_NULL, [aProperty.DisplayName]));
  end;
end;

end.

