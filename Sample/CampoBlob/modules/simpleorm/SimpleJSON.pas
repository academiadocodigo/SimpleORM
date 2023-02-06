unit SimpleJSON;

interface

uses Classes, SysUtils;

type
  TcdJsonValueType = (jvNone, jvNull, jvString, jvNumber, jvBoolean, jvObject, jvArray);
  TcdJsonStructType = (jsNone, jsArray, jsObject);
  TcdJsonNull = (null);
  TcdJsonEmpty = (empty);

type
  TSimpleJsonBase = class(TObject)
  private
    FOwner: TSimpleJsonBase;
    function GetOwner: TSimpleJsonBase;

  protected
    function GetOwnerName: String;
    procedure RaiseError(const Msg: String);
    procedure RaiseParseError(const JsonString: String);
    procedure RaiseAssignError(Source: TSimpleJsonBase);

  public
    constructor Create(AOwner: TSimpleJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: String); virtual; abstract;
    function Stringify: String; virtual; abstract;

    procedure Assign(Source: TSimpleJsonBase); virtual; abstract;

    function Encode(const S: String): String;
    function Decode(const S: String): String;

    procedure Split(const S: String; const Delimiter: Char; Strings: TStrings);

    function IsJsonObject(const S: String): Boolean;
    class function IsJsonArray(const S: String): Boolean;
    function IsJsonString(const S: String): Boolean;
    function IsJsonNumber(const S: String): Boolean;
    function IsJsonBoolean(const S: String): Boolean;
    function IsJsonNull(const S: String): Boolean;

    function AnalyzeJsonValueType(const S: String): TcdJsonValueType;

  public
    property Owner: TSimpleJsonBase read GetOwner;

  end;

  TSimpleJsonObject = class;
  TSimpleJsonArray = class;
  TSimpleJsonValue = class(TSimpleJsonBase)
  private
    FValueType: TcdJsonValueType;
    FStringValue: String;
    FNumberValue: Extended;
    FBooleanValue: Boolean;
    FObjectValue: TSimpleJsonObject;
    FArrayValue: TSimpleJsonArray;

    function GetAsArray: TSimpleJsonArray;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsNumber: Extended;
    function GetAsObject: TSimpleJsonObject;
    function GetAsString: String;
    function GetIsNull: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsNumber(const Value: Extended);
    procedure SetAsString(const Value: String);
    procedure SetIsNull(const Value: Boolean);
    procedure SetAsArray(const Value: TSimpleJsonArray);
    procedure SetAsObject(const Value: TSimpleJsonObject);
    function GetIsEmpty: Boolean;
    procedure SetIsEmpty(const Value: Boolean);

  protected
    procedure RaiseValueTypeError(const AsValueType: TcdJsonValueType);

  public
    constructor Create(AOwner: TSimpleJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TSimpleJsonBase); override;

    procedure Clear;

  public
    property ValueType: TcdJsonValueType read FValueType;
    property AsString: String read GetAsString write SetAsString;
    property AsNumber: Extended read GetAsNumber write SetAsNumber;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TSimpleJsonObject read GetAsObject write SetAsObject;
    property AsArray: TSimpleJsonArray read GetAsArray write SetAsArray;
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsEmpty: Boolean read GetIsEmpty write SetIsEmpty;

  end;

  TSimpleJsonArray = class(TSimpleJsonBase)
  private
    FList: TList;
    function GetItems(Index: Integer): TSimpleJsonValue;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TSimpleJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TSimpleJsonBase); override;
    procedure Merge(Addition: TSimpleJsonArray);

    function Add: TSimpleJsonValue;
    function Insert(const Index: Integer): TSimpleJsonValue;

    function Put(const Value: TcdJsonEmpty): TSimpleJsonValue; overload;
    function Put(const Value: TcdJsonNull): TSimpleJsonValue; overload;
    function Put(const Value: Boolean): TSimpleJsonValue; overload;
    function Put(const Value: Integer): TSimpleJsonValue; overload;
    function Put(const Value: Extended): TSimpleJsonValue; overload;
    function Put(const Value: String): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonArray): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonObject): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonValue): TSimpleJsonValue; overload;

    procedure Delete(const Index: Integer);
    procedure Clear;

  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimpleJsonValue read GetItems; default;

  end;

  TSimpleJsonPair = class(TSimpleJsonBase)
  private
    FName: String;
    FValue: TSimpleJsonValue;

    procedure SetName(const Value: String);

  public
    constructor Create(AOwner: TSimpleJsonBase; const AName: String = '');
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TSimpleJsonBase); override;

  public
    property Name: String read FName write SetName;
    property Value: TSimpleJsonValue read FValue;

  end;

  TSimpleJsonObject = class(TSimpleJsonBase)
  private
    FList: TList;
    FAutoAdd: Boolean;
    function GetCount: Integer;
    function GetItems(Index: Integer): TSimpleJsonPair;
    function GetValues(Name: String): TSimpleJsonValue;
  public
    constructor Create(AOwner: TSimpleJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TSimpleJsonBase); override;
    procedure Merge(Addition: TSimpleJsonObject);

    function Add(const Name: String = ''): TSimpleJsonPair;
    function Insert(const Index: Integer; const Name: String = ''): TSimpleJsonPair;

    function Put(const Name: String; const Value: TcdJsonEmpty): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TcdJsonNull): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: String): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonArray): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonObject): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonValue): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonPair): TSimpleJsonValue; overload;

    function Find(const Name: String): Integer;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;

  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSimpleJsonPair read GetItems;
    property Values[Name: String]: TSimpleJsonValue read GetValues; default;
    property AutoAdd: Boolean read FAutoAdd write FAutoAdd;

  end;

  TSimpleJson = class(TSimpleJsonBase)
  private
    FStructType: TcdJsonStructType;
    FJsonArray: TSimpleJsonArray;
    FJsonObject: TSimpleJsonObject;

    function GetCount: Integer;
    function GeTcdJsonArray: TSimpleJsonArray;
    function GeTcdJsonObject: TSimpleJsonObject;
    function GetValues(Name: String): TSimpleJsonValue;
  protected
    procedure CreateArrayIfNone;
    procedure CreateObjectIfNone;

    procedure RaiseIfNone;
    procedure RaiseIfNotArray;
    procedure RaiseIfNotObject;

    procedure CheckJsonArray;
    procedure CheckJsonObject;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(JsonString: String); override;
    function Stringify: String; override;

    procedure Assign(Source: TSimpleJsonBase); override;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: String); overload;

    procedure Clear;

    function Get(const Index: Integer): TSimpleJsonValue; overload; //for both
    function Get(const Name: String): TSimpleJsonValue; overload; //for JsonObject

    //for JsonArray
    function Put(const Value: TcdJsonEmpty): TSimpleJsonValue; overload;
    function Put(const Value: TcdJsonNull): TSimpleJsonValue; overload;
    function Put(const Value: Boolean): TSimpleJsonValue; overload;
    function Put(const Value: Integer): TSimpleJsonValue; overload;
    function Put(const Value: Extended): TSimpleJsonValue; overload;
    function Put(const Value: String): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonArray): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonObject): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonValue): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJson): TSimpleJsonValue; overload;

    //for JsonObject
    function Put(const Name: String; const Value: TcdJsonEmpty): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TcdJsonNull): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Boolean): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: String): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonArray): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonObject): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJsonValue): TSimpleJsonValue; overload;
    function Put(const Name: String; const Value: TSimpleJson): TSimpleJsonValue; overload;
    function Put(const Value: TSimpleJsonPair): TSimpleJsonValue; overload;

  public
    property StructType: TcdJsonStructType read FStructType;
    property JsonObject: TSimpleJsonObject read GeTcdJsonObject;
    property JsonArray: TSimpleJsonArray read GeTcdJsonArray;

    property Count: Integer read GetCount;
    property Values[Name: String]: TSimpleJsonValue read GetValues; default; //for JsonObject

  end;

implementation

{ TcdJsonBase }

function TSimpleJsonBase.AnalyzeJsonValueType(const S: String): TcdJsonValueType;
var
  Len: Integer;
  Number: Extended;
begin
  Result := jvNone;
  Len := Length(S);
  if Len >= 2 then
  begin
    if (S[1] = '{') and (S[Len] = '}') then Result := jvObject
    else if (S[1] = '[') and (S[Len] = ']') then Result := jvArray
    else if (S[1] = '"') and (S[Len] = '"') then Result := jvString
    else if SameText(S, 'null') then Result := jvNull
    else if SameText(S, 'true') or SameText(S, 'false') then Result := jvBoolean
    else if TryStrToFloat(StringReplace(S, '.', ',', [rfReplaceAll]), Number) then Result := jvNumber;
  end
  else if TryStrToFloat(S, Number) then Result := jvNumber;
end;

constructor TSimpleJsonBase.Create(AOwner: TSimpleJsonBase);
begin
  FOwner := AOwner;
end;

function TSimpleJsonBase.Decode(const S: String): String;

  function HexValue(C: Char): Byte;
  begin
    case C of
      '0'..'9':  Result := Byte(C) - Byte('0');
      'a'..'f':  Result := (Byte(C) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(C) - Byte('A')) + 10;
      else raise Exception.Create('Illegal hexadecimal characters "' + C + '"');
    end;
  end;

  function HexToUnicode(Hex: String): String;
  begin
    try
      Result := Char((HexValue(Hex[3]) shl 4) + HexValue(Hex[4]))
              + Char((HexValue(Hex[1]) shl 4) + HexValue(Hex[2]));
    except
      raise Exception.Create('Illegal four-hex-digits "' + Hex + '"');
    end;
  end;

var
  I: Integer;
  C: Char;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    Inc(I);
    if C = '\' then
    begin
      C := S[I];
      Inc(I);
      case C of
        'b': Result := Result + #8;
        't': Result := Result + #9;
        'n': Result := Result + #10;
        'f': Result := Result + #12;
        'r': Result := Result + #13;
        'u':
          begin
            Result := Result + HexToUnicode(Copy(S, I, 4));
            Inc(I, 4);
          end;
        else Result := Result + C;
      end;
    end
    else Result := Result + C;
  end;
end;

destructor TSimpleJsonBase.Destroy;
begin
  inherited Destroy;
end;

function TSimpleJsonBase.Encode(const S: String): String;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"', '\': Result := Result + '\' + C;
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      Result := Result + C;
    end;
  end;
end;

function TSimpleJsonBase.GetOwner: TSimpleJsonBase;
begin
  Result := FOwner;
end;

function TSimpleJsonBase.GetOwnerName: String;
var
  TheOwner: TSimpleJsonBase;
begin
  Result := '';
  TheOwner := Owner;
  while True do
  begin
    if not Assigned(TheOwner) then Break
    else if TheOwner is TSimpleJsonPair then
    begin
      Result := (TheOwner as TSimpleJsonPair).Name;
      Break;
    end
    else TheOwner := TheOwner.Owner;
  end;
end;

class function TSimpleJsonBase.IsJsonArray(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '[') and (S[Len] = ']');
end;

function TSimpleJsonBase.IsJsonBoolean(const S: String): Boolean;
begin
  Result := SameText(S, 'true') or SameText(S, 'false');
end;

function TSimpleJsonBase.IsJsonNull(const S: String): Boolean;
begin
  Result := SameText(S, 'null');
end;

function TSimpleJsonBase.IsJsonNumber(const S: String): Boolean;
var
  Number: Extended;
begin
  Result := TryStrToFloat(S, Number);
end;

function TSimpleJsonBase.IsJsonObject(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '{') and (S[Len] = '}');
end;

function TSimpleJsonBase.IsJsonString(const S: String): Boolean;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '"') and (S[Len] = '"');
end;

procedure TSimpleJsonBase.RaiseAssignError(Source: TSimpleJsonBase);
var
  SourceClassName: String;
begin
  if Source is TObject then SourceClassName := Source.ClassName
  else SourceClassName := 'nil';
  RaiseError(Format('assign error: %s to %s', [SourceClassName, ClassName]));
end;

procedure TSimpleJsonBase.RaiseError(const Msg: String);
var
  S: String;
begin
  S := Format('<%s>%s', [ClassName, Msg]);
  raise Exception.Create(S);
end;

procedure TSimpleJsonBase.RaiseParseError(const JsonString: String);
begin
  RaiseError(Format('"%s" parse error: %s', [GetOwnerName, JsonString]));
end;

procedure TSimpleJsonBase.Split(const S: String; const Delimiter: Char;
  Strings: TStrings);

  function IsPairBegin(C: Char): Boolean;
  begin
    Result := (C = '{') or (C = '[') or (C = '"');
  end;

  function GetPairEnd(C: Char): Char;
  begin
    case C of
      '{': Result := '}';
      '[': Result := ']';
      '"': Result := '"';
      else Result := #0;
    end;
  end;

  function MoveToPair(P: PChar): PChar;
  var
    PairBegin, PairEnd: Char;
    C: Char;
  begin
    PairBegin := P^;
    PairEnd := GetPairEnd(PairBegin);
    Result := P;
    while Result^ <> #0 do
    begin
      Inc(Result);
      C := Result^;
      if C = PairEnd then Break
      else if (PairBegin = '"') and (C = '\') then Inc(Result)
      else if (PairBegin <> '"') and IsPairBegin(C) then Result := MoveToPair(Result);
    end;
  end;

var
  PtrBegin, PtrEnd: PChar;
  C: Char;
  StrItem: String;
begin
  PtrBegin := PChar(S);
  PtrEnd := PtrBegin;
  while PtrEnd^ <> #0 do
  begin
    C := PtrEnd^;
    if C = Delimiter then
    begin
      StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
      Strings.Add(StrItem);
      PtrBegin := PtrEnd + 1;
      PtrEnd := PtrBegin;
      Continue;
    end
    else if IsPairBegin(C) then PtrEnd := MoveToPair(PtrEnd);
    Inc(PtrEnd);
  end;
  StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
  if StrItem <> '' then Strings.Add(StrItem);
end;

{ TcdJsonValue }

procedure TSimpleJsonValue.Assign(Source: TSimpleJsonBase);
var
  Src: TSimpleJsonValue;
begin
  Clear;
  if not(Source is TSimpleJsonValue) and not(Source is TSimpleJsonObject) and not(Source is TSimpleJsonArray) then
    RaiseAssignError(Source);
  if Source is TSimpleJsonObject then
  begin
    FValueType := jvObject;
    FObjectValue := TSimpleJsonObject.Create(Self);
    FObjectValue.Assign(Source);
  end
  else if Source is TSimpleJsonArray then
  begin
    FValueType := jvArray;
    FArrayValue := TSimpleJsonArray.Create(Self);
    FArrayValue.Assign(Source);
  end
  else if Source is TSimpleJsonValue then
  begin
    Src := Source as TSimpleJsonValue;
    FValueType := Src.FValueType;
    case FValueType of
      jvNone, jvNull: ;
      jvString: FStringValue := Src.FStringValue;
      jvNumber: FNumberValue := Src.FNumberValue;
      jvBoolean: FBooleanValue := Src.FBooleanValue;
      jvObject:
        begin
          FObjectValue := TSimpleJsonObject.Create(Self);
          FObjectValue.Assign(Src.FObjectValue);
        end;
      jvArray:
        begin
          FArrayValue := TSimpleJsonArray.Create(Self);
          FArrayValue.Assign(Src.FArrayValue);
        end;
    end;
  end;
end;

procedure TSimpleJsonValue.Clear;
begin
  case FValueType of
    jvNone, jvNull: ;
    jvString: FStringValue := '';
    jvNumber: FNumberValue := 0;
    jvBoolean: FBooleanValue := False;
    jvObject:
      begin
        FObjectValue.Free;
        FObjectValue := nil;
      end;
    jvArray:
      begin
        FArrayValue.Free;
        FArrayValue := nil;
      end;
  end;
  FValueType := jvNone;
end;

constructor TSimpleJsonValue.Create(AOwner: TSimpleJsonBase);
begin
  inherited Create(AOwner);
  FStringValue := '';
  FNumberValue := 0;
  FBooleanValue := False;
  FObjectValue := nil;
  FArrayValue := nil;
  FValueType := jvNone;
end;

destructor TSimpleJsonValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSimpleJsonValue.GetAsArray: TSimpleJsonArray;
begin
  if IsEmpty then
  begin
    FValueType := jvArray;
    FArrayValue := TSimpleJsonArray.Create(Self);
  end;
  if FValueType <> jvArray then RaiseValueTypeError(jvArray);
  Result := FArrayValue;
end;

function TSimpleJsonValue.GetAsBoolean: Boolean;
begin
  Result := False;
  case FValueType of
    jvNone, jvNull: Result := False;
    jvString: Result := SameText(FStringValue, 'true');
    jvNumber: Result := (FNumberValue <> 0);
    jvBoolean: Result := FBooleanValue;
    jvObject, jvArray: RaiseValueTypeError(jvBoolean);
  end;
end;

function TSimpleJsonValue.GetAsInteger: Integer;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := Trunc(StrToFloat(FStringValue));
    jvNumber: Result := Trunc(FNumberValue);
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TSimpleJsonValue.GetAsNumber: Extended;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := StrToFloat(FStringValue);
    jvNumber: Result := FNumberValue;
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TSimpleJsonValue.GetAsObject: TSimpleJsonObject;
begin
  if IsEmpty then
  begin
    FValueType := jvObject;
    FObjectValue := TSimpleJsonObject.Create(Self);
  end;
  if FValueType <> jvObject then RaiseValueTypeError(jvObject);
  Result := FObjectValue;
end;

function TSimpleJsonValue.GetAsString: String;
const
  BooleanStr: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := '';
    jvString: Result := FStringValue;
    jvNumber: Result := FloatToStr(FNumberValue);
    jvBoolean: Result := BooleanStr[FBooleanValue];
    jvObject, jvArray: RaiseValueTypeError(jvString);
  end;
end;

function TSimpleJsonValue.GetIsEmpty: Boolean;
begin
  Result := (FValueType = jvNone);
end;

function TSimpleJsonValue.GetIsNull: Boolean;
begin
  Result := (FValueType = jvNull);
end;

procedure TSimpleJsonValue.Parse(JsonString: String);
begin
  Clear;
  FValueType := AnalyzeJsonValueType(JsonString);
  case FValueType of
    jvNone: RaiseParseError(JsonString);
    jvNull: ;
    jvString: FStringValue := Decode(Copy(JsonString, 2, Length(JsonString) - 2));
    jvNumber: FNumberValue := StrToFloat(JsonString);
    jvBoolean: FBooleanValue := SameText(JsonString, 'true');
    jvObject:
      begin
        FObjectValue := TSimpleJsonObject.Create(Self);
        FObjectValue.Parse(JsonString);
      end;
    jvArray:
      begin
        FArrayValue := TSimpleJsonArray.Create(Self);
        FArrayValue.Parse(JsonString);
      end;
  end;
end;

procedure TSimpleJsonValue.RaiseValueTypeError(const AsValueType: TcdJsonValueType);
const
  StrJsonValueType: array[TcdJsonValueType] of String = ('jvNone', 'jvNull', 'jvString', 'jvNumber', 'jvBoolean', 'jvObject', 'jvArray');
var
  S: String;
begin
  S := Format('"%s" value type error: %s to %s', [GetOwnerName, StrJsonValueType[FValueType], StrJsonValueType[AsValueType]]);
  RaiseError(S);
end;

procedure TSimpleJsonValue.SetAsArray(const Value: TSimpleJsonArray);
begin
  if FValueType <> jvArray then
  begin
    Clear;
    FValueType := jvArray;
    FArrayValue := TSimpleJsonArray.Create(Self);
  end;
  FArrayValue.Assign(Value);
end;

procedure TSimpleJsonValue.SetAsBoolean(const Value: Boolean);
begin
  if FValueType <> jvBoolean then
  begin
    Clear;
    FValueType := jvBoolean;
  end;
  FBooleanValue := Value;
end;

procedure TSimpleJsonValue.SetAsInteger(const Value: Integer);
begin
  SetAsNumber(Value);
end;

procedure TSimpleJsonValue.SetAsNumber(const Value: Extended);
begin
  if FValueType <> jvNumber then
  begin
    Clear;
    FValueType := jvNumber;
  end;
  FNumberValue := Value;
end;

procedure TSimpleJsonValue.SetAsObject(const Value: TSimpleJsonObject);
begin
  if FValueType <> jvObject then
  begin
    Clear;
    FValueType := jvObject;
    FObjectValue := TSimpleJsonObject.Create(Self);
  end;
  FObjectValue.Assign(Value);
end;

procedure TSimpleJsonValue.SetAsString(const Value: String);
begin
  if FValueType <> jvString then
  begin
    Clear;
    FValueType := jvString;
  end;
  FStringValue := Value;
end;

procedure TSimpleJsonValue.SetIsEmpty(const Value: Boolean);
const
  EmptyValueType: array[Boolean] of TcdJsonValueType = (jvNull, jvNone);
begin
  if FValueType <> EmptyValueType[Value] then
  begin
    Clear;
    FValueType := EmptyValueType[Value];
  end;
end;

procedure TSimpleJsonValue.SetIsNull(const Value: Boolean);
const
  NullValueType: array[Boolean] of TcdJsonValueType = (jvNone, jvNull);
begin
  if FValueType <> NullValueType[Value] then
  begin
    Clear;
    FValueType := NullValueType[Value];
  end;
end;

function TSimpleJsonValue.Stringify: String;
const
  StrBoolean: array[Boolean] of String = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := 'null';
    jvString: Result := '"' + Encode(FStringValue) + '"';
    jvNumber: Result := StringReplace(FloatToStr(FNumberValue), ',', '.', [rfReplaceAll]);
    jvBoolean: Result := StrBoolean[FBooleanValue];
    jvObject: Result := FObjectValue.Stringify;
    jvArray: Result := FArrayValue.Stringify;
  end;
end;

{ TcdJsonArray }

function TSimpleJsonArray.Add: TSimpleJsonValue;
begin
  Result := TSimpleJsonValue.Create(Self);
  FList.Add(Result);
end;

procedure TSimpleJsonArray.Assign(Source: TSimpleJsonBase);
var
  Src: TSimpleJsonArray;
  I: Integer;
begin
  Clear;
  if not(Source is TSimpleJsonArray) then RaiseAssignError(Source);
  Src := Source as TSimpleJsonArray;
  for I := 0 to Src.Count - 1 do Add.Assign(Src[I]);
end;

procedure TSimpleJsonArray.Clear;
var
  I: Integer;
  Item: TSimpleJsonValue;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    Item.Free;
  end;
  FList.Clear;
end;

constructor TSimpleJsonArray.Create(AOwner: TSimpleJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

procedure TSimpleJsonArray.Delete(const Index: Integer);
var
  Item: TSimpleJsonValue;
begin
  Item := FList[Index];
  Item.Free;
  FList.Delete(Index);
end;

destructor TSimpleJsonArray.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TSimpleJsonArray.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSimpleJsonArray.GetItems(Index: Integer): TSimpleJsonValue;
begin
  Result := FList[Index];
end;

function TSimpleJsonArray.Insert(const Index: Integer): TSimpleJsonValue;
begin
  Result := TSimpleJsonValue.Create(Self);
  FList.Insert(Index, Result);
end;

procedure TSimpleJsonArray.Merge(Addition: TSimpleJsonArray);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition[I]);
end;

procedure TSimpleJsonArray.Parse(JsonString: String);
var
  I: Integer;
  S: String;
  List: TStringList;
  Item: TSimpleJsonValue;
begin
  Clear;
  JsonString := Trim(JsonString);
  if not IsJsonArray(JsonString) then RaiseParseError(JsonString);
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TSimpleJsonArray.Put(const Value: Boolean): TSimpleJsonValue;
begin
  Result := Add;
  Result.AsBoolean := Value;
end;

function TSimpleJsonArray.Put(const Value: Integer): TSimpleJsonValue;
begin
  Result := Add;
  Result.AsInteger := Value;
end;

function TSimpleJsonArray.Put(const Value: TcdJsonEmpty): TSimpleJsonValue;
begin
  Result := Add;
  Result.IsEmpty := True;
end;

function TSimpleJsonArray.Put(const Value: TcdJsonNull): TSimpleJsonValue;
begin
  Result := Add;
  Result.IsNull := True;
end;

function TSimpleJsonArray.Put(const Value: Extended): TSimpleJsonValue;
begin
  Result := Add;
  Result.AsNumber := Value;
end;

function TSimpleJsonArray.Put(const Value: TSimpleJsonObject): TSimpleJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TSimpleJsonArray.Put(const Value: TSimpleJsonValue): TSimpleJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TSimpleJsonArray.Put(const Value: String): TSimpleJsonValue;
begin
  Result := Add;
  Result.AsString := Value;
end;

function TSimpleJsonArray.Put(const Value: TSimpleJsonArray): TSimpleJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TSimpleJsonArray.Stringify: String;
var
  I: Integer;
  Item: TSimpleJsonValue;
begin
  Result := '[';
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    if I > 0 then Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + ']';
end;

{ TcdJsonPair }

procedure TSimpleJsonPair.Assign(Source: TSimpleJsonBase);
var
  Src: TSimpleJsonPair;
begin
  if not(Source is TSimpleJsonPair) then RaiseAssignError(Source);
  Src := Source as TSimpleJsonPair;
  FName := Src.FName;
  FValue.Assign(Src.FValue);
end;

constructor TSimpleJsonPair.Create(AOwner: TSimpleJsonBase; const AName: String);
begin
  inherited Create(AOwner);
  FName := AName;
  FValue := TSimpleJsonValue.Create(Self);
end;

destructor TSimpleJsonPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

procedure TSimpleJsonPair.Parse(JsonString: String);
var
  List: TStringList;
  StrName: String;
begin
  List := TStringList.Create;
  try
    Split(JsonString, ':', List);
    if List.Count <> 2 then RaiseParseError(JsonString);
    StrName := List[0];
    if not IsJsonString(StrName) then RaiseParseError(StrName);
    FName := Decode(Copy(StrName, 2, Length(StrName) - 2));
    case AnalyzeJsonValueType(List[1]) of
      jvNumber: FValue.Parse(StringReplace(List[1], '.', ',', [rfReplaceAll]));
    else
      FValue.Parse(List[1]);
    end;
  finally
    FreeAndNil(List);
  end;
end;

procedure TSimpleJsonPair.SetName(const Value: String);
begin
  FName := Value;
end;

function TSimpleJsonPair.Stringify: String;
begin
  Result := Format('"%s":%s', [Encode(FName), FValue.Stringify]);
end;

{ TcdJsonObject }

function TSimpleJsonObject.Add(const Name: String): TSimpleJsonPair;
begin
  Result := TSimpleJsonPair.Create(Self, Name);
  FList.Add(Result);
end;

procedure TSimpleJsonObject.Assign(Source: TSimpleJsonBase);
var
  Src: TSimpleJsonObject;
  I: Integer;
begin
  Clear;
  if not(Source is TSimpleJsonObject) then RaiseAssignError(Source);
  Src := Source as TSimpleJsonObject;
  for I := 0 to Src.Count - 1 do Add.Assign(Src.Items[I]);
end;

procedure TSimpleJsonObject.Clear;
var
  I: Integer;
  Item: TSimpleJsonPair;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    Item.Free;
  end;
  FList.Clear;
end;

constructor TSimpleJsonObject.Create(AOwner: TSimpleJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FAutoAdd := True;
end;

procedure TSimpleJsonObject.Delete(const Index: Integer);
var
  Item: TSimpleJsonPair;
begin
  Item := FList[Index];
  Item.Free;
  FList.Delete(Index);
end;

procedure TSimpleJsonObject.Delete(const Name: String);
var
  Index: Integer;
begin
  Index := Find(Name);
  if Index < 0 then RaiseError(Format('"%s" not found', [Name]));
  Delete(Index);
end;

destructor TSimpleJsonObject.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TSimpleJsonObject.Find(const Name: String): Integer;
var
  I: Integer;
  Pair: TSimpleJsonPair;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    Pair := FList[I];
    if SameText(Name, Pair.Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TSimpleJsonObject.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TSimpleJsonObject.GetItems(Index: Integer): TSimpleJsonPair;
begin
  Result := FList[Index];
end;

function TSimpleJsonObject.GetValues(Name: String): TSimpleJsonValue;
var
  Index: Integer;
  Pair: TSimpleJsonPair;
begin
  Index := Find(Name);
  if Index < 0 then
  begin
    if not FAutoAdd then RaiseError(Format('%s not found', [Name]));
    Pair := Add(Name);
  end
  else Pair := FList[Index];
  Result := Pair.Value;
end;

function TSimpleJsonObject.Insert(const Index: Integer;
  const Name: String): TSimpleJsonPair;
begin
  Result := TSimpleJsonPair.Create(Self, Name);
  FList.Insert(Index, Result);
end;

procedure TSimpleJsonObject.Merge(Addition: TSimpleJsonObject);
var
  I: Integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition.Items[I]);
end;

procedure TSimpleJsonObject.Parse(JsonString: String);
var
  I: Integer;
  S: String;
  List: TStringList;
  Item: TSimpleJsonPair;
begin
  Clear;
  JsonString := Trim(JsonString);
  if not IsJsonObject(JsonString) then RaiseParseError(JsonString);
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: Integer): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsInteger := Value;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: Extended): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsNumber := Value;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: Boolean): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsBoolean := Value;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: TcdJsonEmpty): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsEmpty := True;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: TcdJsonNull): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsNull := True;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: TSimpleJsonValue): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TSimpleJsonObject.Put(const Value: TSimpleJsonPair): TSimpleJsonValue;
var
  Pair: TSimpleJsonPair;
begin
  Pair := Add;
  Pair.Assign(Value);
  Result := Pair.Value;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: TSimpleJsonObject): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TSimpleJsonObject.Put(const Name, Value: String): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsString := Value;
end;

function TSimpleJsonObject.Put(const Name: String;
  const Value: TSimpleJsonArray): TSimpleJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TSimpleJsonObject.Stringify: String;
var
  I: Integer;
  Item: TSimpleJsonPair;
begin
  Result := '{';
  for I := 0 to FList.Count - 1 do
  begin
    Item := FList[I];
    if I > 0 then Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + '}';
end;

{ TcdJson }

procedure TSimpleJson.Assign(Source: TSimpleJsonBase);
begin
  Clear;
  if Source is TSimpleJson then
  begin
    case (Source as TSimpleJson).FStructType of
      jsNone: ;
      jsArray:
        begin
          CreateArrayIfNone;
          FJsonArray.Assign((Source as TSimpleJson).FJsonArray);
        end;
      jsObject:
        begin
          CreateObjectIfNone;
          FJsonObject.Assign((Source as TSimpleJson).FJsonObject);
        end;
    end;
  end
  else if Source is TSimpleJsonArray then
  begin
    CreateArrayIfNone;
    FJsonArray.Assign(Source);
  end
  else if Source is TSimpleJsonObject then
  begin
    CreateObjectIfNone;
    FJsonObject.Assign(Source);
  end
  else if Source is TSimpleJsonValue then
  begin
    if (Source as TSimpleJsonValue).ValueType = jvArray then
    begin
      CreateArrayIfNone;
      FJsonArray.Assign((Source as TSimpleJsonValue).AsArray);
    end
    else if (Source as TSimpleJsonValue).ValueType = jvObject then
    begin
      CreateObjectIfNone;
      FJsonObject.Assign((Source as TSimpleJsonValue).AsObject);
    end
    else RaiseAssignError(Source);
  end
  else RaiseAssignError(Source);
end;

procedure TSimpleJson.CheckJsonArray;
begin
  CreateArrayIfNone;
  RaiseIfNotArray;
end;

procedure TSimpleJson.CheckJsonObject;
begin
  CreateObjectIfNone;
  RaiseIfNotObject;
end;

procedure TSimpleJson.Clear;
begin
  case FStructType of
    jsNone: ;
    jsArray:
      begin
        FJsonArray.Free;
        FJsonArray := nil;
      end;
    jsObject:
      begin
        FJsonObject.Free;
        FJsonObject := nil;
      end;
  end;
  FStructType := jsNone;
end;

constructor TSimpleJson.Create;
begin
  inherited Create(nil);
  FStructType := jsNone;
  FJsonArray := nil;
  FJsonObject := nil;
end;

procedure TSimpleJson.CreateArrayIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsArray;
    FJsonArray := TSimpleJsonArray.Create(Self);
  end;
end;

procedure TSimpleJson.CreateObjectIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsObject;
    FJsonObject := TSimpleJsonObject.Create(Self);
  end;
end;

procedure TSimpleJson.Delete(const Index: Integer);
begin
  RaiseIfNone;
  case FStructType of
    jsArray: FJsonArray.Delete(Index);
    jsObject: FJsonObject.Delete(Index);
  end;
end;

procedure TSimpleJson.Delete(const Name: String);
begin
  RaiseIfNotObject;
  FJsonObject.Delete(Name);
end;

destructor TSimpleJson.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TSimpleJson.Get(const Index: Integer): TSimpleJsonValue;
begin
  Result := nil;
  RaiseIfNone;
  case FStructType of
    jsArray: Result := FJsonArray.Items[Index];
    jsObject: Result := FJsonObject.Items[Index].Value;
  end;
end;

function TSimpleJson.Get(const Name: String): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Values[Name];
end;

function TSimpleJson.GetCount: Integer;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Count;
    jsObject: Result := FJsonObject.Count;
    else Result := 0;
  end;
end;

function TSimpleJson.GeTcdJsonArray: TSimpleJsonArray;
begin
  CheckJsonArray;
  Result := FJsonArray;
end;

function TSimpleJson.GeTcdJsonObject: TSimpleJsonObject;
begin
  CheckJsonObject;
  Result := FJsonObject;
end;

function TSimpleJson.GetValues(Name: String): TSimpleJsonValue;
begin
  Result := Get(Name);
end;

procedure TSimpleJson.Parse(JsonString: String);
begin
  Clear;
  JsonString := Trim(JsonString);
  if IsJsonArray(JsonString) then
  begin
    CreateArrayIfNone;
    FJsonArray.Parse(JsonString);
  end
  else if IsJsonObject(JsonString) then
  begin
    CreateObjectIfNone;
    FJsonObject.Parse(JsonString);
  end
  else RaiseParseError(JsonString);
end;

function TSimpleJson.Put(const Value: Integer): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: Extended): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: Boolean): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: TcdJsonEmpty): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: TcdJsonNull): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: String): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: TSimpleJsonValue): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: TSimpleJsonObject): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Value: TSimpleJsonArray): TSimpleJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TSimpleJson.Put(const Name: String; const Value: Integer): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String; const Value: Extended): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String; const Value: Boolean): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String;
  const Value: TcdJsonEmpty): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String; const Value: TcdJsonNull): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String;
  const Value: TSimpleJsonValue): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Value: TSimpleJsonPair): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Value);
end;

function TSimpleJson.Put(const Name: String;
  const Value: TSimpleJsonObject): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name, Value: String): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Name: String;
  const Value: TSimpleJsonArray): TSimpleJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TSimpleJson.Put(const Value: TSimpleJson): TSimpleJsonValue;
begin
  CheckJsonArray;
  case Value.FStructType of
    jsArray: Result := Put(Value.FJsonArray);
    jsObject: Result := Put(Value.FJsonObject);
    else Result := nil;
  end;
end;

function TSimpleJson.Put(const Name: String; const Value: TSimpleJson): TSimpleJsonValue;
begin
  CheckJsonObject;
  case Value.FStructType of
    jsArray: Result := Put(Name, Value.FJsonArray);
    jsObject: Result := Put(Name, Value.FJsonObject);
    else Result := nil;
  end;
end;

procedure TSimpleJson.RaiseIfNone;
begin
  if FStructType = jsNone then RaiseError('json struct type is jsNone');
end;

procedure TSimpleJson.RaiseIfNotArray;
begin
  if FStructType <> jsArray then RaiseError('json struct type is not jsArray');
end;

procedure TSimpleJson.RaiseIfNotObject;
begin
  if FStructType <> jsObject then RaiseError('json struct type is not jsObject');
end;

function TSimpleJson.Stringify: String;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Stringify;
    jsObject: Result := FJsonObject.Stringify;
  else
    Result := '';
  end;
end;

end.

