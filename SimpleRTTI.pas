unit SimpleRTTI;

interface

uses
  SimpleInterface, System.Generics.Collections, System.RTTI, Data.DB;

Type
  TSimpleRTTI<T : class, constructor> = class(TInterfacedObject, iSimpleRTTI<T>)
    private
      FInstance : T;
    public
      constructor Create( aInstance : T );
      destructor Destroy; override;
      class function New( aInstance : T ) : iSimpleRTTI<T>;
      function Fields (var aFields : String) : iSimpleRTTI<T>;
      function FieldsInsert (var aFields : String) : iSimpleRTTI<T>;
      function Param (var aParam : String) : iSimpleRTTI<T>;
      function Where (var aWhere : String) : iSimpleRTTI<T>;
      function Update(var aUpdate : String) : iSimpleRTTI<T>;
      function DictionaryFields(var aDictionary : TDictionary<string, variant>) : iSimpleRTTI<T>;
      function ListFields (var List : TList<String>) : iSimpleRTTI<T>;
      function ClassName (var aClassName : String) : iSimpleRTTI<T>;
      function DataSetToEntityList (aDataSet : TDataSet; var aList : TList<T>) : iSimpleRTTI<T>;
      function DataSetToEntity (aDataSet : TDataSet; var aEntity : T) : iSimpleRTTI<T>;
  end;

implementation

uses
  System.TypInfo, System.SysUtils, SimpleAttributes;

{ TSimpleRTTI }

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
            if prpRtti.Name = Field.DisplayName then
            begin
              case prpRtti.PropertyType.TypeKind of
                tkUnknown: Value := Field.AsString;
                tkInteger: Value := Field.AsInteger;
                tkChar: ;
                tkEnumeration: ;
                tkFloat: Value := Field.AsFloat;
                tkString: Value := Field.AsString;
                tkSet: ;
                tkClass: ;
                tkMethod: ;
                tkWChar:  Value := Field.AsString;
                tkLString: Value := Field.AsString;
                tkWString: Value := Field.AsString;
                tkVariant: ;
                tkArray: ;
                tkRecord: ;
                tkInterface: ;
                tkInt64: Value := Field.AsInteger;
                tkDynArray: ;
                tkUString: Value := Field.AsString;
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
  var aList: TList<T>): iSimpleRTTI<T>;
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
            if prpRtti.Name = Field.DisplayName then
            begin
              case prpRtti.PropertyType.TypeKind of
                tkUnknown: Value := Field.AsString;
                tkInteger: Value := Field.AsInteger;
                tkChar: ;
                tkEnumeration: ;
                tkFloat: Value := Field.AsFloat;
                tkString: Value := Field.AsString;
                tkSet: ;
                tkClass: ;
                tkMethod: ;
                tkWChar:  Value := Field.AsString;
                tkLString: Value := Field.AsString;
                tkWString: Value := Field.AsString;
                tkVariant: ;
                tkArray: ;
                tkRecord: ;
                tkInterface: ;
                tkInt64: Value := Field.AsInteger;
                tkDynArray: ;
                tkUString: Value := Field.AsString;
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
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
        aDictionary.Add(prpRtti.Name, prpRtti.GetValue(Pointer(FInstance)).ToString);
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
  vIgnore : Boolean;
  Attribute: TCustomAttribute;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      vIgnore := false;
      for Attribute in prpRtti.GetAttributes do
      begin
        if Attribute is Ignore then
          vIgnore := True;
      end;
      if not vIgnore then
        aFields := aFields + prpRtti.Name + ', ';
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
  vIgnore : Boolean;
  Attribute: TCustomAttribute;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      vIgnore := false;
      for Attribute in prpRtti.GetAttributes do
      begin
        if Attribute is AutoInc then
          vIgnore := True;
      end;
      if not vIgnore then
        aFields := aFields + prpRtti.Name + ', ';
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
  vIgnore : Boolean;
  Attribute: TCustomAttribute;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      vIgnore := false;
      for Attribute in prpRtti.GetAttributes do
      begin
        if Attribute is Ignore then
          vIgnore := True;

        if Attribute is AutoInc then
          vIgnore := True;
      end;
      if not vIgnore then
        aParam  := aParam + ':' + prpRtti.Name + ', ';
    end;
  finally
    aParam := Copy(aParam, 0, Length(aParam) - 2) + ' ';
    ctxRtti.Free;
  end;

end;

function TSimpleRTTI<T>.Update(var aUpdate : String) : iSimpleRTTI<T>;
var
  ctxRtti   : TRttiContext;
  typRtti   : TRttiType;
  prpRtti   : TRttiProperty;
  Info     : PTypeInfo;
  vIgnore : Boolean;
  Attribute: TCustomAttribute;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      vIgnore := false;
      for Attribute in prpRtti.GetAttributes do
      begin
        if Attribute is Ignore then
          vIgnore := True;
      end;
      if not vIgnore then
        aUpdate := aUpdate + prpRtti.Name + ' = :' + prpRtti.Name + ', ';
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
  Attribute: TCustomAttribute;
begin
  Result := Self;
  Info := System.TypeInfo(T);
  ctxRtti := TRttiContext.Create;
  try
    typRtti := ctxRtti.GetType(Info);
    for prpRtti in typRtti.GetProperties do
    begin
      for Attribute in prpRtti.GetAttributes do
      begin
        if Attribute is PK then
          aWhere := aWhere + prpRtti.Name + ' = :' + prpRtti.Name + ', ';
      end;
    end;
  finally
    aWhere := Copy(aWhere, 0, Length(aWhere) - 2) + ' ';
    ctxRtti.Free;
  end;
end;

end.
