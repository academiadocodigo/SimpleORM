unit SimpleJSONUtil;

interface

uses
  Data.DBXJSON, Data.DBXJSONReflect, System.Generics.Collections, System.JSON,
  REST.JSON, REST.Response.Adapter, Data.DB, System.SysUtils, System.Classes,
  System.TypInfo, System.DateUtils, System.StrUtils, System.NetEncoding;

type
  TJsonFlags = set of (jfFormat);

  TSimpleJsonUtil = class
  private
    class var
      JSONMarshal: TJSONMarshal;
    class var
      JSONUnMarshal: TJSONUnMarshal;
  public
    class function ObjectToJSON(poObject: TObject): TJSONValue;
    class function ObjectToJSONString(poObject: TObject): string;

    class function JSONToObject<T: class>(const psJSON: string): T; overload;
    class function JSONToObject<T: class>(const poJSON: TJSONValue): T; overload;

    class procedure JSONStringToObject(const psJSON: string; poObject: TObject);
    class function ListToJSONArray<T: class>(poList: TObjectList<T>): TJSONArray;

    class function ListToJSONArrayString<T: class>(poList: TObjectList<T>;
      const paJsonFlags: TJsonFlags = []): string; overload;
    class function ListToJSONArrayString(poList: TStringList;
      const paJsonFlags: TJsonFlags = []): string; overload;
    class function ListToJSONArrayString(poList: TObject;
      const paJsonFlags: TJsonFlags = []): string; overload;

    class function JSONArrayToList<T: class>(const psJSONArray: string):
      TObjectList<T>; overload;
    class function JSONArrayToList<T: class>(poJSONArray: TJSONArray):
      TObjectList<T>; overload;


    class procedure JSONArrayStringToList<T: Class>(psJSONArray: String;
      poList: TObjectList<T>); overload;

    class procedure JSONArrayStringToList<T: class>(poJSONArray: TJSONArray ;
      poList: TObjectList<T>); overload;

    class procedure JSONToDataset(const poDataset: TDataSet; const poJSON: string);
  end;

  var
    fmtSimpleJSONUtil : TFormatSettings;

implementation

uses
  System.Rtti, SimpleJSON, SimpleRTTIHelper, SimpleRTTI;

{ TJsonUtil }

class function TSimpleJsonUtil.JSONArrayToList<T>(poJSONArray: TJSONArray): TObjectList<T>;
var
  i: Integer;
begin
  Result := TObjectList<T>.Create;
  for i := 0 to poJSONArray.Count - 1 do
    Result.Add(Self.JSONToObject<T>(poJSONArray.Items[i]));
end;

class procedure TSimpleJsonUtil.JSONToDataset(const poDataset: TDataSet; const
  poJSON: string);
var
  JObj: TJSONArray;
  vConv: TCustomJSONDataSetAdapter;
begin
  if (poJSON = EmptyStr) then
    Exit;

  JObj := TJSONObject.ParseJSONValue(poJSON) as TJSONArray;
  vConv := TCustomJSONDataSetAdapter.Create(Nil);

  try
    vConv.Dataset := poDataset;
    vConv.UpdateDataSet(JObj);
  finally
    FreeAndNil(vConv);
    FreeAndNil(JObj);
  end;
end;

class function TSimpleJsonUtil.JSONArrayToList<T>(const psJSONArray: string): TObjectList<T>;
var
  OJSONArray: TJSONArray;
begin
  OJSONArray := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(psJSONArray),
    0) as TJSONArray;
  try
    Result := JSONArrayToList<T>(OJSONArray);
  finally
    FreeAndNil(OJSONArray);
  end;
end;

class function TSimpleJsonUtil.JSONToObject<T>(const poJSON: TJSONValue): T;
begin
  if (not Assigned(poJSON)) or (poJSON is TJSONNull) then
    Exit(nil);

  JSONUnMarshal := TJSONUnMarshal.Create;
  try
    Result := T(JSONUnMarshal.UnMarshal(poJSON));
  finally
    FreeAndNil(JSONUnMarshal);
  end;
end;

class function TSimpleJsonUtil.JSONToObject<T>(const psJSON: string): T;
var
  OJSONValue: TJSONValue;
begin
  OJSONValue := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(psJSON), 0);
  try
    Result := JSONToObject<T>(OJSONValue);
  finally
    FreeAndNil(OJSONValue);
  end;
end;

class function TSimpleJsonUtil.ListToJSONArray<T>(poList: TObjectList<T>): TJSONArray;
var
  i: Integer;
begin
  Result := TJSONArray.Create;
  for i := 0 to poList.Count - 1 do
    Result.AddElement(Self.ObjectToJSON(poList[i]));
end;

class function TSimpleJsonUtil.ListToJSONArrayString(poList: TObject;
  const paJsonFlags: TJsonFlags): string;
begin
  Result := '';
  if poList.ClassNameIs('TStringList') then
    Result := ListToJSONArrayString(TStringList(poList))
  else
  if poList.ClassName.Contains('TSimpleEntityList<') or
     poList.ClassName.Contains('TObjectList<') then
    Result := ListToJSONArrayString<TObject>(TObjectList<TObject>(poList));
end;

class function TSimpleJsonUtil.ListToJSONArrayString(poList: TStringList;
  const paJsonFlags: TJsonFlags): string;
var
  sMyElem: string;
begin
  Result := '';
  for sMyElem in poList do
    Result := Result + ',' + sMyElem;

  Result := Copy(Result, 2, Length(Result));
  Result := '[' + Result + ']';
end;

class function TSimpleJsonUtil.ListToJSONArrayString<T>(poList: TObjectList<T>;
  const paJsonFlags: TJsonFlags): string;
var
  i: Integer;
  oArrayJson: TStringList;
begin
  oArrayJson := TStringList.Create;
  try
    for i := 0 to poList.Count - 1 do
      oArrayJson.Add(Self.ObjectToJSONString(poList[i]) + ',');

    Result := oArrayJson.Text;
    Result := '[' + Copy(Result, 1, Length(Result)-2) + ']';

    if not (jfFormat in paJsonFlags) then
      Result := StringReplace(Result, sLineBreak, EmptyStr, [rfReplaceAll]);
  finally
    FreeAndNil(oArrayJson);
  end;
end;

class function TSimpleJsonUtil.ObjectToJSON(poObject: TObject): TJSONValue;
begin
  try
    if not Assigned(poObject) then
      Exit(TJSONNull.Create);

    JSONMarshal := TJSONMarshal.Create(TJSONConverter.Create);
    Result := JSONMarshal.Marshal(poObject);
  finally
    FreeAndNil(JSONMarshal);
  end;
end;

class function TSimpleJsonUtil.ObjectToJSONString(poObject: TObject): string;
var
  oRttiContexto: TRttiContext;
  oRttiProp: TRttiProperty;
  oRttiTipo: TRttiType;
  oValue: TValue;
  oJson: TSimpleJson;
  sArrayTemp: string;
  ssString : String;
  ms : TMemoryStream;
  ss : TStringStream;
  function IsBoolean: Boolean;
  begin
    Result :=   (oRttiProp.PropertyType.Handle = TypeInfo(Boolean));
  end;
begin
  sArrayTemp := '';
  oJson := TSimpleJson.Create;
  try
    oRttiTipo := oRttiContexto.GetType(poObject.ClassType);
    for oRttiProp in oRttiTipo.GetProperties do
    begin
      oValue := oRttiProp.GetValue(poObject);
      if oRttiProp.Name = 'RefCount' then
        Continue;

      if oRttiProp.IsIgnoreJson then
        Continue;

      case oValue.Kind of
        tkUString:
          begin
            if oRttiProp.EhSomenteNumeros then
              oJson.Put(oRttiProp.Name, oValue.AsStringNumberOnly)
            else
              oJson.Put(oRttiProp.Name, oValue.AsString);
          end;
        tkInteger, tkInt64:
          oJson.Put(oRttiProp.Name, oValue.AsInteger);

        tkEnumeration:
          if IsBoolean() then
          begin
            oJson.Put(oRttiProp.Name, oValue.AsBoolean);
          end
          else
          begin
//            I := GetEnumValue(oRttiProp.PropertyType.Handle , oJson[oRttiProp.Name].AsString);
//            bBool  := I >= 0;
//            if bBool then
//               oJson.Put(oRttiProp.Name, GetEnumName(oRttiProp.PropertyType.Handle , I));
          end;

        tkFloat:
          begin
            if (oValue.TypeInfo = TypeInfo(Real))
              or (oValue.TypeInfo = TypeInfo(Double))
              or (oValue.TypeInfo = TypeInfo(Currency)) then
              oJson.Put(oRttiProp.Name, oValue.AsExtended);

            if oValue.TypeInfo = TypeInfo(TDate) then
              oJson.Put(oRttiProp.Name, DateToStr( oValue.AsExtended, fmtSimpleJSONUtil));

            if oValue.TypeInfo = TypeInfo(TTime) then
              oJson.Put(oRttiProp.Name,TimeToStr(oValue.AsExtended, fmtSimpleJSONUtil));

            if oValue.TypeInfo = TypeInfo(TDateTime) then
              oJson.Put(oRttiProp.Name,  DateTimeToStr(oValue.AsExtended,fmtSimpleJSONUtil ));
          end;
        tkClass:
          begin
            if oValue.AsObject.ClassName.Contains('TSimpleEntityList<')
              or oValue.AsObject.ClassName.Contains('TObjectList<')
              or oValue.AsObject.ClassNameIs('TStringList') then
            begin
              sArrayTemp := ListToJSONArrayString(oValue.AsObject);
              oJson[oRttiProp.Name].AsArray.Parse(sArrayTemp);
            end
            else if oValue.AsObject is TStringStream then
            begin
              oJson.put(oRttiProp.Name,Trim((oValue.AsObject as TStringStream).DataString));
            end
            else if (oValue.AsObject is TMemoryStream) then
            begin
              if oRttiProp.IsJSONBase64 = false then
                continue;

              (oValue.AsObject as TMemoryStream).Position := 0;


              ss := TStringStream.Create;
              try
                TNetEncoding.Base64.Encode((oValue.AsObject as TMemoryStream), ss);
                oJson.put(oRttiProp.Name,Trim(ss.DataString));
              finally
                FreeAndNil(ss);
              end;
            end
            else
              oJson[oRttiProp.Name].AsObject.Parse(ObjectToJSONString(oValue.AsObject));
          end;
      end;
    end;
    Result := oJson.Stringify;
  finally
    FreeAndNil(oJson);
  end;
end;

class procedure TSimpleJsonUtil.JSONArrayStringToList<T>(poJSONArray: TJSONArray;
  poList: TObjectList<T>);
var
  i: Integer;
  rttiContext : TRttiContext;
  rttiType : TRttiInstanceType;
  rttiCreate : TRttiMethod;
  Instance : TValue;
  obj : T;
begin
  rttiContext := TRttiContext.Create;
  try
    rttiType := rttiContext.GetType(T).AsInstance;
    rttiCreate := rttiType.GetMethod('Create');

    poList.Clear;
    for i := 0 to poJSONArray.Count - 1 do
    begin
      Instance :=  rttiCreate.Invoke(rttiType.MetaclassType,[]);
      JSONStringToObject(poJSONArray.Items[i].ToString, Instance.AsObject);
      obj :=  Instance.AsObject as T;
      poList.Add(obj);
    end
  finally
    rttiContext.Free;
  end;
end;

class procedure TSimpleJsonUtil.JSONArrayStringToList<T>(psJSONArray: String;
  poList: TObjectList<T>);
var
  OJSONArray: TJSONArray;
begin
  OJSONArray := TJSONObject.ParseJSONValue(
                            TEncoding.UTF8.GetBytes(psJSONArray)
                            ,0) as TJSONArray;
  try
    JSONArrayStringToList<T>(OJSONArray,poList);
  finally
    FreeAndNil(OJSONArray);
  end;
end;

class procedure TSimpleJsonUtil.JSONStringToObject(const psJSON: string; poObject: TObject);
var
  oRttiContexto: TRttiContext;
  oRttiProp: TRttiProperty;
  oRttiTipo: TRttiType;
  oValue: TValue;
  oJson: TSimpleJson;
  I: Integer;
  sArrayTemp: string;
  ss : TStringStream;

  bBool :  Boolean;
  function IsBoolean: Boolean;
  begin
    Result :=   (oRttiProp.PropertyType.Handle = TypeInfo(Boolean));
  end;
begin
  sArrayTemp := '';
  oJson := TSimpleJson.Create;
  try
    oJson.Parse(psJSON);
    oRttiTipo := oRttiContexto.GetType(poObject.ClassType);
    for oRttiProp in oRttiTipo.GetProperties do
    begin
      if oRttiProp.Name = 'RefCount' then
        Continue;

      if oRttiProp.IsIgnoreJson then
        Continue;

      oValue := oRttiProp.GetValue(poObject);
      case oValue.Kind of
        tkUString:
          oValue := oJson[oRttiProp.Name].AsString;


        tkEnumeration:
          if IsBoolean() then
          begin
            oValue :=  StrTobool(oJson[oRttiProp.Name].AsString);
          end
          else
          begin
            I := GetEnumValue(oRttiProp.PropertyType.Handle , oJson[oRttiProp.Name].AsString);
            bBool  := I >= 0;
            if bBool then
              oValue := GetEnumName(oRttiProp.PropertyType.Handle , I);
          end;

        tkInteger:
          oValue := oJson[oRttiProp.Name].AsInteger;
        tkFloat:
          begin
            if (oValue.TypeInfo = TypeInfo(Real)) or (oValue.TypeInfo = TypeInfo(Double)) then
              oValue := oJson[oRttiProp.Name].AsNumber;


            if oValue.TypeInfo = TypeInfo(TDate) then
                oValue := StrToDateDef(oJson[oRttiProp.Name].AsString, 0, fmtSimpleJSONUtil);

            if oValue.TypeInfo = TypeInfo(TTime) then
              oValue := StrToTimeDef(oJson[oRttiProp.Name].AsString, 0, fmtSimpleJSONUtil);


            if oValue.TypeInfo = TypeInfo(TDateTime) then
              oValue := StrToDateTimeDef(oJson[oRttiProp.Name].AsString, 0, fmtSimpleJSONUtil);


          end;
        tkClass:
          begin
            case ansiIndexSTR(oValue.AsObject.ClassName,['TStringList','TMemoryStream','TStringStream']) OF
              0 :
                begin
                  for I := 0 to oJson[oRttiProp.Name].AsArray.Count - 1 do
                  begin
                    (oValue.AsObject as TStringList).Add(oJson[oRttiProp.Name].AsArray
                      [I].Stringify);
                  end;
                end;
              1 :
                begin
                  ss := TStringStream.Create(oJson[oRttiProp.Name].AsString);
                  try
                    if oRttiProp.IsJSONBase64 = false then
                      continue;

                    ss.Position := 0;
                    TNetEncoding.Base64.decode(SS, (oValue.AsObject as TMemoryStream));
                  finally
                    ss.Free;
                  end;
                end;
              2 :
                begin
                  (oValue.AsObject as TStringStream).WriteString(oJson[oRttiProp.Name].AsString);
                end;
            end;

            Continue;
          end;
      end;
      oRttiProp.SetValue(poObject, oValue);
    end;
  finally
    FreeAndNil(oJson);
  end;
end;

end.


