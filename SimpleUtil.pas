unit SimpleUtil;

interface

uses
  System.Classes, Data.DB, System.Generics.Collections,
  {$IFNDEF CONSOLE}
    {$IFDEF FMX}
      FMX.Forms, FMX.StdCtrls, FMX.DateTimeCtrls, FMX.Edit, FMX.ListBox,
    {$ELSE}
      Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls,
    {$ENDIF}
  {$ENDIF}
  Variants,
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

    class function StreamToVariant(const AStream: TStream) : Variant;   overload ;
    class procedure StreamToVariant(const AStream: TStream; var AVariant: Variant); overload;

    class procedure VariantToStream(const AVariant: Variant; AStream: TStream);
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

  s : String;
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
    {$IFNDEF FMX}
    if (aComponent as TEdit).NumbersOnly and ((aComponent as TEdit).Text = '') then
      Exit('0')
    else  {$ENDIF}
      Exit((aComponent as TEdit).Text);
  end;

  {$IFDEF FMX}
  if aComponent is TComboBox then
  begin
    if assigned((aComponent as TComboBox).Selected) then
      Exit((aComponent as TComboBox).Selected.Text);
  end;

  if aComponent is  TDateEdit then
  begin
     Exit(FloatToStr((aComponent as  TDateEdit).DateTime ));
  end;

  {$ELSE}
  if aComponent is TComboBox then
    Exit((aComponent as TComboBox).Text);

  if aComponent is TDateTimePicker then
    Exit(FloatToStr((aComponent as TDateTimePicker).Date));
  {$ENDIF}
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
  ComboBoxIndex : integer;
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

      {$IFDEF FMX}
      if Component is TComboBox then
      begin
        ComboBoxIndex := (Component as TComboBox).Items.IndexOf(prpRtti.GetValue(aObject).ToString);
        if ComboBoxIndex  >= 0 then
          (Component as TComboBox).itemindex := ComboBoxIndex;

      end;

      if Component is  TDateEdit then
      begin
        (Component as  TDateEdit).DateTime  := prpRtti.GetValue(aObject).AsType<TDateTime>;
      end;


     {$ELSE}

      if Component is TComboBox then
        (Component as TComboBox).Text := prpRtti.GetValue(aObject).ToString;

      if Component is TDateTimePicker then
        (Component as TDateTimePicker).Date := prpRtti.GetValue(aObject)
          .AsExtended;
      {$ENDIF}
    end;
  end;
end;


{$ENDIF}

class function TSimpleUtil.StreamToVariant(const AStream: TStream): Variant;
var
  AVariant: Variant;
begin
  StreamToVariant(AStream,AVariant );
  result := AVariant;
end;

class procedure TSimpleUtil.StreamToVariant(const AStream: TStream;
  var AVariant: Variant);
var
  StreamSize: Integer;
  Buffer: Pointer;
begin
  if not Assigned(AStream) then
    raise Exception.Create('Stream is not assigned.');

  StreamSize := AStream.Size;

  if StreamSize = 0 then
    raise Exception.Create('Stream is empty.');

  GetMem(Buffer, StreamSize);

  try
    AStream.Position := 0;
    AStream.Read(Buffer^, StreamSize);

    AVariant := VarArrayCreate([0, StreamSize - 1], varByte);
    Move(Buffer^, VarArrayLock(AVariant)^, StreamSize);
    VarArrayUnlock(AVariant);
  finally
    FreeMem(Buffer);
  end;

end;


class procedure TSimpleUtil.VariantToStream(const AVariant: Variant;
  AStream: TStream);
var
  VariantData: PByte;
  VariantSize: Integer;
begin
  if VarIsNull(AVariant) or VarIsEmpty(AVariant) then
    raise Exception.Create('Variant is empty.');


  if not Assigned(AStream) then
    raise Exception.Create('Stream is not assigned.');


  VariantData := VarArrayLock(AVariant);
  try

    VariantSize := VarArrayHighBound(AVariant, 1) - VarArrayLowBound(AVariant, 1) + 1;
    AStream.position := 0;
    AStream.WriteBuffer(VariantData^, VariantSize);
  finally
    VarArrayUnlock(AVariant);
  end;
end;

end.

