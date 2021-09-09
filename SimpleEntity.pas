unit SimpleEntity;

interface

uses
  {$IFNDEF CONSOLE}
    {$IFDEF FMX}
      FMX.Forms,
    {$ELSE}
      Vcl.Forms,
    {$ENDIF}
  {$ENDIF}
  Data.DB, System.Generics.Collections, System.SysUtils;

type
  TSimpleEntity = class
  public
    {$IFNDEF CONSOLE}
    function Parse(const aForm: TForm): TSimpleEntity; overload; virtual;
    {$ENDIF}
    function Parse(const aDataSet: TDataSet): TSimpleEntity; overload; virtual;
    procedure Parse(const psJSON: string); overload;
    procedure SaveToFileJSON(const poFileName: TFileName); overload; virtual;
    procedure SaveToFileJSON(const poFileName: TFileName; poEncoding: TEncoding); overload; virtual;
    function ToJSON: string;
    function ToJSONRefletion: string;
  end;

  TSimpleEntityList<T: TSimpleEntity, constructor> = class(TObjectList<T>)
  public
    function Parse(const aDataSet: TDataSet): TSimpleEntityList<T>; virtual;
    function ToJSON: string;
    function ToJSONRefletion: string;
  end;

implementation

uses
  SimpleUtil, SimpleJSONUtil, System.Classes, System.JSON, System.StrUtils;

{ TSimpleEntity }

function TSimpleEntity.Parse(const aDataSet: TDataSet): TSimpleEntity;
begin
  Result := Self;
  TSimpleUtil.GetValuesFromDataset(aDataSet, Self);
end;

procedure TSimpleEntity.SaveToFileJSON(const poFileName: TFileName);
begin
  SaveToFileJSON(poFileName, TEncoding.Default);
end;

procedure TSimpleEntity.Parse(const psJSON: string);
begin
  TSimpleJsonUtil.JSONStringToObject(psJSON, Self);
end;

procedure TSimpleEntity.SaveToFileJSON(const poFileName: TFileName;
  poEncoding: TEncoding);
var
  oConteudo: TStringList;
begin
  oConteudo := TStringList.Create;
  try
    oConteudo.Text := ToJSON;
    if ForceDirectories(ExtractFilePath(poFileName)) then
      oConteudo.SaveToFile(poFileName, poEncoding);
  finally
    FreeAndNil(oConteudo);
  end;
end;

function DecodeUnicodeEscapes(psEscaped: string): string;
var
  FoundPos: LongInt;
  HexCode: String;
  DecodedChars: String;
begin
  Result := psEscaped;
  FoundPos := Pos('\u', Result);
  while (FoundPos <> 0) and (FoundPos < Length(Result) - 4) do
  begin
    HexCode :=  Copy(Result, FoundPos + 2, 4);
    DecodedChars := WideChar(StrToInt('$' + HexCode));
    Result := AnsiReplaceStr(Result, '\u' + HexCode,
                             Utf8ToAnsi(UTF8Encode(DecodedChars)));
    FoundPos := Pos('\u', Result);
  end;
  Result := StringReplace(Result, '\/', '/', [rfReplaceAll])
end;

function TSimpleEntity.ToJSON: string;
begin
  Result := TSimpleJsonUtil.ObjectToJSONString(Self);
end;

function TSimpleEntity.ToJSONRefletion: string;
var
  oJSONValue: TJSONValue;
begin
  oJSONValue := TSimpleJsonUtil.ObjectToJSON(Self);
  try
    Result := DecodeUnicodeEscapes(oJSONValue.ToJSON);
  finally
    FreeAndNil(oJSONValue);
  end;
end;

{ TSimpleEntityList<T> }

function TSimpleEntityList<T>.Parse(const aDataSet: TDataSet): TSimpleEntityList<T>;
begin
  Result := Self;
  Self.Clear;
  TSimpleUtil.DataSetToObjectList<T>(aDataSet, Self);
end;

{$IFNDEF CONSOLE}

function TSimpleEntity.Parse(const aForm: TForm): TSimpleEntity;
begin
  Result := Self;
  TSimpleUtil.GetObjectFromForm(aForm, Self);
end;

{$ENDIF}

function TSimpleEntityList<T>.ToJSON: string;
begin
  Result := TSimpleJsonUtil.ListToJSONArrayString<T>(Self, [jfFormat]);
end;

function TSimpleEntityList<T>.ToJSONRefletion: string;
var
  oJSONArray: TJSONArray;
begin
  oJSONArray := TSimpleJsonUtil.ListToJSONArray<T>(Self);
  try
    Result := oJSONArray.ToJSON;
  finally
    FreeAndNil(oJSONArray);
  end;
end;

end.
