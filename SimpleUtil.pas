unit SimpleUtil;

interface

uses
  System.Classes, Data.DB, System.Generics.Collections,
  {$IFNDEF CONSOLE}
  Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls,
  {$ENDIF}
  SimpleEntity;

type
  TSimpleUtil = class
  private
    {$IFNDEF CONSOLE}
    class function GetTextFromComponent(aComponent: TComponent): string;
    {$ENDIF}
  public
    class procedure DataSetToObjectList<T: class, constructor>(const poDataSet:
      TDataSet; const poLista: TObjectList<T>); overload;
    class procedure DataSetToObjectList<T: TSimpleEntity, constructor>(const poDataSet:
      TDataSet; const poLista: TSimpleEntityList<T>); overload;
    class procedure GetValuesFromDataset(const poDataset: TDataSet; const poClasse: TObject);
    {$IFNDEF CONSOLE}
    class procedure GetObjectFromForm(const aForm: TForm; const aObject: TObject);
    class procedure SetFormFromObject(const aForm: TForm; const aObject: TObject);
    {$ENDIF}
  end;

implementation

{ TcdRTTIUtils }

uses
  System.Rtti, SysUtils, FireDAC.Comp.Client, SimpleRTTIHelper, SimpleAttributes;

class procedure TSimpleUtil.DataSetToObjectList<T>(const poDataSet: TDataSet; const
  poLista: TObjectList<T>);
var
  oObjeto: T;
begin
  poDataSet.DisableControls;
  poDataSet.First;
  while not poDataSet.Eof do
  begin
    oObjeto := T.Create;
    GetValuesFromDataset(poDataSet, oObjeto);
    poLista.Add(oObjeto);
    poDataSet.Next;
  end;
  poDataSet.EnableControls;
end;

class procedure TSimpleUtil.DataSetToObjectList<T>(const poDataSet: TDataSet;
  const poLista: TSimpleEntityList<T>);
var
  oObjeto: T;
begin
  poDataSet.DisableControls;
  poDataSet.First;
  while not poDataSet.Eof do
  begin
    oObjeto := T.Create;
    oObjeto.Parse(poDataSet);
    poLista.Add(oObjeto);
    poDataSet.Next;
  end;
  poDataSet.EnableControls;
end;

class procedure TSimpleUtil.GetValuesFromDataset(const poDataset: TDataSet;
  const poClasse: TObject);
var
  oContexto: TRttiContext;
  oTipo: TRttiType;
  oPropriedade: TRttiProperty;
  Value: TValue;

  function Campo: TField;
  begin
    Result := poDataset.FindField(oPropriedade.FieldName);
  end;

begin
  oTipo := oContexto.GetType(poClasse.classType);
  for oPropriedade in oTipo.GetProperties do
  begin
    Value := oPropriedade.GetValue(poClasse);
    if (Campo = nil) then
      Continue;

    case Value.Kind of
      tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
        Value := Campo.AsString;
      tkInteger:
        Value := StrToIntDef(Campo.AsString, 0);
      tkInt64:
        Value := StrToInt64Def(Campo.AsString, 0);
      tkFloat:
        begin
          if Value.TypeInfo = TypeInfo(TDate) then
            Value := Campo.AsDateTime
          else
            Value := Campo.AsExtended;
        end;
      tkClass:
        if oPropriedade.Tem<HasOne> then
        begin
          if Value.AsObject is TSimpleEntity then
            TSimpleEntity(Value.AsObject).Parse(poDataSet)
          else
            GetValuesFromDataset(poDataSet, Value.AsObject);
        end;
    end;

    if (Campo <> nil) then
      oPropriedade.SetValue(poClasse, Value);
  end;
end;

{$IFNDEF CONSOLE}

class procedure TSimpleUtil.GetObjectFromForm(const aForm: TForm;
  const aObject: TObject);
var
  ctxRttiEntity: TRttiContext;
  typRttiEntity: TRttiType;
  typRttiForm: TRttiType;
  fldRtti: TRttiField;
  prpRtti: TRttiProperty;
  Value: TValue;
  Component: TComponent;
  vFieldName: string;
begin
  ctxRttiEntity := TRttiContext.Create;
  typRttiEntity := ctxRttiEntity.GetType(aObject.ClassType);
  typRttiForm := ctxRttiEntity.GetType(aForm.ClassInfo);

  for prpRtti in typRttiEntity.GetProperties do
  begin
    for fldRtti in typRttiForm.GetFields do
    begin
      if not fldRtti.Tem<Bind> then
        Continue;

      vFieldName := LowerCase(fldRtti.GetAttribute<Bind>.Field);
      if not (vFieldName.Equals(LowerCase(prpRtti.FieldName))
        or vFieldName.Equals(LowerCase(prpRtti.Name))) then
        Continue;

      Component := (fldRtti.GetValue(aForm).AsObject as TComponent);
      case prpRtti.GetValue(aObject).Kind of
        tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
          Value := GetTextFromComponent(Component);
        tkInteger:
          Value := StrToInt(GetTextFromComponent(Component));
        tkFloat:
          Value := StrToFloat(GetTextFromComponent(Component));
      end;

      prpRtti.SetValue(aObject, Value);
    end;
  end;
end;

class function TSimpleUtil.GetTextFromComponent(aComponent: TComponent): string;
begin
  if aComponent is TEdit then
  begin
    if (aComponent as TEdit).NumbersOnly and ((aComponent as TEdit).Text = '') then
      Exit('0')
    else
      Exit((aComponent as TEdit).Text);
  end;

  if aComponent is TComboBox then
    Exit((aComponent as TComboBox).Text);

  if aComponent is TDateTimePicker then
    Exit(FloatToStr((aComponent as TDateTimePicker).Date));
end;

class procedure TSimpleUtil.SetFormFromObject(const aForm: TForm;
  const aObject: TObject);
var
  ctxRttiEntity: TRttiContext;
  typRttiEntity: TRttiType;
  typRttiForm: TRttiType;
  fldRtti: TRttiField;
  prpRtti: TRttiProperty;
  Component: TComponent;
  vFieldName, vFieldNameForm: string;
begin
  ctxRttiEntity := TRttiContext.Create;
  typRttiEntity := ctxRttiEntity.GetType(aObject.ClassType);
  typRttiForm := ctxRttiEntity.GetType(aForm.ClassInfo);

  for prpRtti in typRttiEntity.GetProperties do
  begin
    vFieldName := prpRtti.Name;
    if prpRtti.EhCampo then
      vFieldName := prpRtti.GetAttribute<Campo>.Name;
    for fldRtti in typRttiForm.GetFields do
    begin
      if not fldRtti.Tem<Bind> then
        Continue;

      vFieldNameForm := fldRtti.GetAttribute<Bind>.Field;
      if not vFieldNameForm.Equals(vFieldName) then
        Continue;

      Component := (fldRtti.GetValue(aForm).AsObject as TComponent);

      if Component is TEdit then
        (Component as TEdit).Text := prpRtti.GetValue(aObject).ToString;

      if Component is TComboBox then
        (Component as TComboBox).Text := prpRtti.GetValue(aObject).ToString;

      if Component is TDateTimePicker then
        (Component as TDateTimePicker).Date := prpRtti.GetValue(aObject)
          .AsExtended;
    end;
  end;
end;

{$ENDIF}

end.

