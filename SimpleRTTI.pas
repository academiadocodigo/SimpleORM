{$A8,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N-,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
{$WARN SYMBOL_DEPRECATED ON}
{$WARN SYMBOL_LIBRARY ON}
{$WARN SYMBOL_PLATFORM ON}
{$WARN SYMBOL_EXPERIMENTAL ON}
{$WARN UNIT_LIBRARY ON}
{$WARN UNIT_PLATFORM ON}
{$WARN UNIT_DEPRECATED ON}
{$WARN UNIT_EXPERIMENTAL ON}
{$WARN HRESULT_COMPAT ON}
{$WARN HIDING_MEMBER ON}
{$WARN HIDDEN_VIRTUAL ON}
{$WARN GARBAGE ON}
{$WARN BOUNDS_ERROR ON}
{$WARN ZERO_NIL_COMPAT ON}
{$WARN STRING_CONST_TRUNCED ON}
{$WARN FOR_LOOP_VAR_VARPAR ON}
{$WARN TYPED_CONST_VARPAR ON}
{$WARN ASG_TO_TYPED_CONST ON}
{$WARN CASE_LABEL_RANGE ON}
{$WARN FOR_VARIABLE ON}
{$WARN CONSTRUCTING_ABSTRACT ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMBINING_SIGNED_UNSIGNED ON}
{$WARN UNSUPPORTED_CONSTRUCT ON}
{$WARN FILE_OPEN ON}
{$WARN FILE_OPEN_UNITSRC ON}
{$WARN BAD_GLOBAL_SYMBOL ON}
{$WARN DUPLICATE_CTOR_DTOR ON}
{$WARN INVALID_DIRECTIVE ON}
{$WARN PACKAGE_NO_LINK ON}
{$WARN PACKAGED_THREADVAR ON}
{$WARN IMPLICIT_IMPORT ON}
{$WARN HPPEMIT_IGNORED ON}
{$WARN NO_RETVAL ON}
{$WARN USE_BEFORE_DEF ON}
{$WARN FOR_LOOP_VAR_UNDEF ON}
{$WARN UNIT_NAME_MISMATCH ON}
{$WARN NO_CFG_FILE_FOUND ON}
{$WARN IMPLICIT_VARIANTS ON}
{$WARN UNICODE_TO_LOCALE ON}
{$WARN LOCALE_TO_UNICODE ON}
{$WARN IMAGEBASE_MULTIPLE ON}
{$WARN SUSPICIOUS_TYPECAST ON}
{$WARN PRIVATE_PROPACCESSOR ON}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN OPTION_TRUNCATED ON}
{$WARN WIDECHAR_REDUCED ON}
{$WARN DUPLICATES_IGNORED ON}
{$WARN UNIT_INIT_SEQ ON}
{$WARN LOCAL_PINVOKE ON}
{$WARN MESSAGE_DIRECTIVE ON}
{$WARN TYPEINFO_IMPLICITLY_ADDED ON}
{$WARN RLINK_WARNING ON}
{$WARN IMPLICIT_STRING_CAST ON}
{$WARN IMPLICIT_STRING_CAST_LOSS ON}
{$WARN EXPLICIT_STRING_CAST OFF}
{$WARN EXPLICIT_STRING_CAST_LOSS OFF}
{$WARN CVT_WCHAR_TO_ACHAR ON}
{$WARN CVT_NARROWING_STRING_LOST ON}
{$WARN CVT_ACHAR_TO_WCHAR ON}
{$WARN CVT_WIDENING_STRING_LOST ON}
{$WARN NON_PORTABLE_TYPECAST ON}
{$WARN XML_WHITESPACE_NOT_ALLOWED ON}
{$WARN XML_UNKNOWN_ENTITY ON}
{$WARN XML_INVALID_NAME_START ON}
{$WARN XML_INVALID_NAME ON}
{$WARN XML_EXPECTED_CHARACTER ON}
{$WARN XML_CREF_NO_RESOLVE ON}
{$WARN XML_NO_PARM ON}
{$WARN XML_NO_MATCHING_PARM ON}
{$WARN IMMUTABLE_STRINGS OFF}
unit SimpleRTTI;

interface

uses
  SimpleInterface,
  System.Generics.Collections,
  System.RTTI,
  Data.DB,
  TypInfo,
  {$IFNDEF CONSOLE}
    {$IFDEF FMX}
      FMX.Types, FMX.Forms, FMX.Edit, FMX.ListBox, FMX.StdCtrls, FMX.DateTimeCtrls,
    {$ELSE}
      Vcl.Forms, VCL.StdCtrls, Vcl.ExtCtrls,
    {$ENDIF}
  {$ENDIF}
  System.Classes,
  System.SysUtils;

Type
  ESimpleRTTI = Exception;

  TSimpleRTTI<T : class, constructor> = class(TInterfacedObject, iSimpleRTTI<T>)
    private
      FInstance : T;
      function __FloatFormat( aValue : String ) : Currency;
      {$IFNDEF CONSOLE}
      function __BindValueToComponent( aComponent : TComponent; aValue : Variant) : iSimpleRTTI<T>;
      function __GetComponentToValue( aComponent : TComponent) : TValue;
      {$ENDIF}
      function __BindValueToProperty( aEntity : T; aProperty : TRttiProperty; aValue : TValue) : iSimpleRTTI<T>;

      function __GetRTTIPropertyValue(aEntity : T; aPropertyName : String) : Variant;
      function __GetRTTIProperty(aEntity : T; aPropertyName : String) : TRttiProperty;
    public
      constructor Create( aInstance : T );
      destructor Destroy; override;
      class function New( aInstance : T ) : iSimpleRTTI<T>;
      function TableName(var aTableName: String): ISimpleRTTI<T>;

      
      function Fields (var aFields : String) : iSimpleRTTI<T>;
      function FieldsInsert (var aFields : String) : iSimpleRTTI<T>;
      function Param (var aParam : String) : iSimpleRTTI<T>;
      function Where (var aWhere : String) : iSimpleRTTI<T>;
      function Update(var aUpdate : String) : iSimpleRTTI<T>;
      function DictionaryFields(var aDictionary : TDictionary<string, variant>) : iSimpleRTTI<T>;
      function ListFields (var List : TList<String>) : iSimpleRTTI<T>;
      function ClassName (var aClassName : String) : iSimpleRTTI<T>;
      function DataSetToEntityList (aDataSet : TDataSet; var aList : TObjectList<T>) : iSimpleRTTI<T>;
      function DataSetToEntity (aDataSet : TDataSet; var aEntity : T) : iSimpleRTTI<T>;
      function PrimaryKey(var aPK : String) : iSimpleRTTI<T>;
      {$IFNDEF CONSOLE}
      function BindClassToForm (aForm : TForm; const aEntity : T): iSimpleRTTI<T>;
      function BindFormToClass (aForm : TForm; var aEntity : T) : iSimpleRTTI<T>;
      {$ENDIF}
  end;

implementation

uses
  SimpleAttributes,


  {$IFNDEF CONSOLE}
    {$IFNDEF FMX}
      Vcl.ComCtrls,
      Vcl.Graphics,
    {$ENDIF}
  {$ENDIF}
  Variants,
  SimpleRTTIHelper,
  System.UITypes;

{ TSimpleRTTI }

{$IFNDEF CONSOLE}
function TSimpleRTTI<T>.__BindValueToComponent(aComponent: TComponent;
  aValue: Variant): iSimpleRTTI<T>;
begin
  if VarIsNull(aValue) then exit;

  if aComponent is TEdit then
    (aComponent as TEdit).Text := aValue;

  if aComponent is TComboBox then
    (aComponent as TComboBox).ItemIndex := (aComponent as TComboBox).Items.IndexOf(aValue);

  {$IFDEF VCL}
  if aComponent is TRadioGroup then
    (aComponent as TRadioGroup).ItemIndex := (aComponent as TRadioGroup).Items.IndexOf(aValue);

  if aComponent is TShape then
    (aComponent as TShape).Brush.Color := aValue;
  {$ENDIF}

  //DateControls
  {$IFDEF VCL}
    if aComponent is TDateTimePicker then
    (aComponent as TDateTimePicker).Date := aValue;
  {$ENDIF}
  {$IFDEF FMX}
  if aComponent is TDateEdit then
    (aComponent as TDateEdit).Date := aValue;
  {$ENDIF}

  if aComponent is TCheckBox then
  {$IFDEF VCL}
    (aComponent as TCheckBox).Checked := aValue;
  {$ELSEIF IFDEF FMX}
    (aComponent as TCheckBox).IsChecked := aValue;
  {$ENDIF}

  if aComponent is TTrackBar then
    {$IFDEF VCL}
      (aComponent as TTrackBar).Position := aValue;
    {$ELSEIF IFDEF FMX}
      (aComponent as TTrackBar).Position.X := aValue;
    {$ENDIF}



end;
{$ENDIF}

function TSimpleRTTI<T>.__BindValueToProperty( aEntity : T; aProperty : TRttiProperty; aValue : TValue) : iSimpleRTTI<T>;
begin
  case aProperty.PropertyType.TypeKind of
    tkUnknown: ;
    tkInteger: aProperty.SetValue(Pointer(aEntity), StrToInt(aValue.ToString));
    tkChar: ;
    tkEnumeration: ;
    tkFloat:
    begin
      if (aValue.TypeInfo = TypeInfo(TDate))
        or (aValue.TypeInfo = TypeInfo(TTime))
        or (aValue.TypeInfo = TypeInfo(TDateTime)) then
        aProperty.SetValue(Pointer(aEntity), StrToDateTime(aValue.ToString))
      else
        aProperty.SetValue(Pointer(aEntity), StrToFloat(aValue.ToString));
    end;
    tkSet: ;
    tkClass: ;
    tkMethod: ;
    tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
      aProperty.SetValue(Pointer(aEntity), aValue);
    tkArray: ;
    tkInterface: ;
    tkInt64: aProperty.SetValue(Pointer(aEntity), aValue.Cast<Int64>);
    tkDynArray: ;
    tkClassRef: ;
    tkPointer: ;
    tkProcedure: ;
    else
      aProperty.SetValue(Pointer(aEntity), aValue);
  end;

end;

function TSimpleRTTI<T>.__FloatFormat( aValue : String ) : Currency;
begin
  while Pos('.', aValue) > 0 do
    delete(aValue,Pos('.', aValue),1);

  Result := StrToCurr(aValue);
end;

{$IFNDEF CONSOLE}
function TSimpleRTTI<T>.__GetComponentToValue(aComponent: TComponent): TValue;
var
  a: string;
begin
  if aComponent is TEdit then
    Result := TValue.FromVariant((aComponent as TEdit).Text);

  if aComponent is TComboBox then
    Result := TValue.FromVariant((aComponent as TComboBox).Items[(aComponent as TComboBox).ItemIndex]);

  {$IFDEF VCL}
  if aComponent is TRadioGroup then
    Result := TValue.FromVariant((aComponent as TRadioGroup).Items[(aComponent as TRadioGroup).ItemIndex]);

  if aComponent is TShape then
    Result := TValue.FromVariant((aComponent as TShape).Brush.Color);
  {$ENDIF}

  if aComponent is TCheckBox then
  {$IFDEF VCL}
    Result := TValue.FromVariant((aComponent as TCheckBox).Checked);
  {$ELSEIF IFDEF FMX}
      Result := TValue.FromVariant((aComponent as TCheckBox).IsChecked);
  {$ENDIF}


  if aComponent is TTrackBar then
    {$IFDEF VCL}
      Result := TValue.FromVariant((aComponent as TTrackBar).Position);
    {$ELSEIF IFDEF FMX}
      Result := TValue.FromVariant((aComponent as TTrackBar).Position.X);
    {$ENDIF}

  {$IFDEF VCL}
  if aComponent is TDateTimePicker then
    Result := TValue.FromVariant((aComponent as TDateTimePicker).DateTime);
  {$ENDIF}
  {$IFDEF FMX}
  if aComponent is TDateEdit then
    Result := TValue.FromVariant((aComponent as TDateEdit).DateTime);
  {$ENDIF}


  a := Result.TOString;
end;
{$ENDIF}

function TSimpleRTTI<T>.__GetRTTIProperty(aEntity: T;
  aPropertyName: String): TRttiProperty;
var
  ctxRttiEntity : TRttiContext;
  typRttiEntity : TRttiType;
begin
  ctxRttiEntity := TRttiContext.Create;
  try
    typRttiEntity := ctxRttiEntity.GetType(aEntity.ClassInfo);
    Result := typRttiEntity.GetProperty(aPropertyName);
    if not Assigned(Result) then
      Result := typRttiEntity.GetPropertyFromAttribute<Campo>(aPropertyName);

    if not Assigned(Result) then
      raise ESimpleRTTI.Create('Property ' + aPropertyName + ' not found!');
  finally
    ctxRttiEntity.Free;
  end;

end;

function TSimpleRTTI<T>.__GetRTTIPropertyValue(aEntity: T;
  aPropertyName: String): Variant;
begin
  Result := __GetRTTIProperty(aEntity, aPropertyName).GetValue(Pointer(aEntity)).AsVariant;
end;

{$IFNDEF CONSOLE}
function TSimpleRTTI<T>.BindClassToForm(aForm: TForm;
  const aEntity: T): iSimpleRTTI<T>;
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
begin
  Result := Self;
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(aForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<Bind> then
      begin
        __BindValueToComponent(
                          aForm.FindComponent(prpRtti.Name),
                          __GetRTTIPropertyValue(
                                                   aEntity,
                                                   prpRtti.GetAttribute<Bind>.Field
                          )
        );
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.BindFormToClass(aForm: TForm;
  var aEntity: T): iSimpleRTTI<T>;
var
  ctxRtti : TRttiContext;
  typRtti : TRttiType;
  prpRtti : TRttiField;
begin
  Result := Self;
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(aForm.ClassInfo);
    for prpRtti in typRtti.GetFields do
    begin
      if prpRtti.Tem<Bind> then
      begin
        __BindValueToProperty(
          aEntity,
          __GetRTTIProperty(aEntity, prpRtti.GetAttribute<Bind>.Field),
          __GetComponentToValue(aForm.FindComponent(prpRtti.Name))
        );
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;
{$ENDIF}

function TSimpleRTTI<T>.ClassName (var aClassName : String) : iSimpleRTTI<T>;
var
  Info      : PTypeInfo;
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    aClassName := Copy(typRtti.Name, 2, Length(typRtti.Name));
  finally
    ctxRtti.Free;
  end;
end;

constructor TSimpleRTTI<T>.Create( aInstance : T );
begin
  FInstance := aInstance;
end;

function TSimpleRTTI<T>.DataSetToEntity(aDataSet: TDataSet;
  var aEntity: T): iSimpleRTTI<T>;
var
  Field : TField;
  teste: string;
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
  Value : TValue;
begin
  Result := Self;
  aDataSet.First;
  while not aDataSet.Eof do
  begin
    Info := System.TypeInfo(T);
    ctxRtti := TRttiContext.Create;
    try
      for Field in aDataSet.Fields do
      begin
          typRtti := ctxRtti.GetType(Info);
          for prpRtti in typRtti.GetProperties do
          begin
            if LowerCase(prpRtti.FieldName) = LowerCase(Field.DisplayName) then
            begin
              case prpRtti.PropertyType.TypeKind of
                tkUnknown, tkString, tkWChar, tkLString, tkWString, tkUString:
                  Value := Field.AsString;
                tkInteger, tkInt64:
                  Value := Field.AsInteger;
                tkChar: ;
                tkEnumeration:
                  Value := Field.AsBoolean;
                tkFloat: Value := Field.AsFloat;
                tkSet: ;
                tkClass: ;
                tkMethod: ;
                tkVariant: ;
                tkArray: ;
                tkInterface: ;
                tkDynArray: ;
                tkClassRef: ;
                tkPointer: ;
                tkProcedure: ;
              end;
              prpRtti.SetValue(Pointer(aEntity), Value);
            end;
          end;
      end;
    finally
      ctxRtti.Free;
    end;
    aDataSet.Next;
  end;
  aDataSet.First;
end;

function TSimpleRTTI<T>.DataSetToEntityList(aDataSet: TDataSet;
  var aList: TObjectList<T>): iSimpleRTTI<T>;
var
  Field : TField;
  teste: string;
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
  Value : TValue;
begin
  Result := Self;
  aList.Clear;
  while not aDataSet.Eof do
  begin
    aList.Add(T.Create);
    Info := System.TypeInfo(T);
    ctxRtti := TRttiContext.Create;
    try
      for Field in aDataSet.Fields do
      begin
        typRtti := ctxRtti.GetType(Info);
        for prpRtti in typRtti.GetProperties do
        begin
          if LowerCase(prpRtti.FieldName) = LowerCase(Field.FieldName) then
          begin
            Field.DisplayLabel := prpRtti.DisplayName;
            case prpRtti.PropertyType.TypeKind of
              tkUnknown, tkString, tkWChar, tkLString, tkWString, tkUString:
                Value := Field.AsString;
              tkInteger, tkInt64:
                Value := Field.AsInteger;
              tkChar: ;
              tkEnumeration:
                Value := Field.AsBoolean;
              tkFloat:
                Value := Field.AsFloat;
              tkSet: ;
              tkClass: ;
              tkMethod: ;
              tkVariant: ;
              tkArray: ;
              tkInterface: ;
              tkDynArray: ;
              tkClassRef: ;
              tkPointer: ;
              tkProcedure: ;
            end;
            prpRtti.SetValue(Pointer(aList[Pred(aList.Count)]), Value);
          end;
        end;
      end;
    finally
      ctxRtti.Free;
    end;
    aDataSet.Next;
  end;
  aDataSet.First;
end;

destructor TSimpleRTTI<T>.Destroy;
begin

  inherited;
end;

function TSimpleRTTI<T>.DictionaryFields(var aDictionary : TDictionary<string, variant>) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
  Aux : String;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.IsIgnore then
        Continue;

      case prpRtti.PropertyType.TypeKind of
        tkEnumeration:
          aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsBoolean);
        tkInteger, tkInt64:
          begin
            if prpRtti.EhChaveEstrangeira then
            begin
              if prpRtti.GetValue(Pointer(FInstance)).AsInteger = 0 then
                aDictionary.Add(prpRtti.FieldName, Null)
              else
                aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsInteger);
            end
            else
              aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsInteger);
          end;
        tkFloat       :
        begin
          if prpRtti.GetValue(Pointer(FInstance)).TypeInfo = TypeInfo(TDateTime) then
            aDictionary.Add(prpRtti.FieldName, StrToDateTime(prpRtti.GetValue(Pointer(FInstance)).ToString))
          else
          if prpRtti.GetValue(Pointer(FInstance)).TypeInfo = TypeInfo(TDate) then
              aDictionary.Add(prpRtti.FieldName, StrToDate(prpRtti.GetValue(Pointer(FInstance)).ToString))
          else
          if prpRtti.GetValue(Pointer(FInstance)).TypeInfo = TypeInfo(TTime) then
            aDictionary.Add(prpRtti.FieldName, StrToTime(prpRtti.GetValue(Pointer(FInstance)).ToString))
          else
            aDictionary.Add(prpRtti.FieldName, __FloatFormat(prpRtti.GetValue(Pointer(FInstance)).ToString));
        end;
        tkWChar,
        tkLString,
        tkWString,
        tkUString,
        tkString      : aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsString);
        tkVariant     : aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsVariant);
      else
          aDictionary.Add(prpRtti.FieldName, prpRtti.GetValue(Pointer(FInstance)).AsString);
      end;
    end;
  finally
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.Fields (var aFields : String) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if not prpRtti.IsIgnore then
        aFields := aFields + prpRtti.FieldName + ', ';
    end;
  finally
    aFields := Copy(aFields, 0, Length(aFields) - 2) + ' ';
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.FieldsInsert(var aFields: String): iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.IsAutoInc then
        Continue;

      if prpRtti.IsIgnore then
        Continue;

      aFields := aFields + prpRtti.FieldName + ', ';
    end;
  finally
    aFields := Copy(aFields, 0, Length(aFields) - 2) + ' ';
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.ListFields(var List: TList<String>): iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  if not Assigned(List) then
    List := TList<string>.Create;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
        List.Add(prpRtti.Name);
    end;
  finally
    ctxRtti.Free;
  end;

end;

class function TSimpleRTTI<T>.New( aInstance : T ): iSimpleRTTI<T>;
begin
  Result := Self.Create(aInstance);
end;

function TSimpleRTTI<T>.Param (var aParam : String) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.IsIgnore then
        Continue;

      if prpRtti.IsAutoInc then
        Continue;

      aParam  := aParam + ':' + prpRtti.FieldName + ', ';
    end;
  finally
    aParam := Copy(aParam, 0, Length(aParam) - 2) + ' ';
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.PrimaryKey(var aPK: String): iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);

    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.EhChavePrimaria then
        aPK := prpRtti.FieldName;
    end;
  finally
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.TableName(var aTableName: String): ISimpleRTTI<T>;
var
  vInfo   : PTypeInfo;
  vCtxRtti: TRttiContext;
  vTypRtti: TRttiType;
begin
  Result := Self;
  vInfo := System.TypeInfo(T);
  vCtxRtti := TRttiContext.Create;
  try
    vTypRtti := vCtxRtti.GetType(vInfo);
    if vTypRtti.Tem<Tabela> then
      aTableName := vTypRtti.GetAttribute<Tabela>.Name;
  finally
    vCtxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.Update(var aUpdate : String) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.IsIgnore then
        Continue;

      if prpRtti.IsAutoInc then
        Continue;

      aUpdate := aUpdate + prpRtti.FieldName + ' = :' + prpRtti.FieldName + ', ';
    end;
  finally
    aUpdate := Copy(aUpdate, 0, Length(aUpdate) - 2) + ' ';
    ctxRtti.Free;
  end;
end;

function TSimpleRTTI<T>.Where (var aWhere : String) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      if prpRtti.EhChavePrimaria then
        aWhere := aWhere + prpRtti.FieldName + ' = :' + prpRtti.FieldName + ' AND ';
    end;
  finally
    aWhere := Copy(aWhere, 0, Length(aWhere) - 4) + ' ';
    ctxRtti.Free;
  end;
end;

end.
