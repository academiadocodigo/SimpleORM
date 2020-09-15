unit SimpleEntity;

interface

uses
  {$IFNDEF CONSOLE}
  Vcl.Forms,
  {$ENDIF}
  Data.DB, System.Generics.Collections;

type
  TSimpleEntity = class
    function Parse(const aDataSet: TDataSet): TSimpleEntity; overload; virtual;
    {$IFNDEF CONSOLE}
    function Parse(const aForm: TForm): TSimpleEntity; overload; virtual;
    {$ENDIF}
  end;

  TSimpleEntityList<T: TSimpleEntity, constructor> = class(TObjectList<T>)
    function Parse(const aDataSet: TDataSet): TSimpleEntityList<T>; virtual;
  end;

implementation

uses
  SimpleUtil;

{ TSimpleEntity }

function TSimpleEntity.Parse(const aDataSet: TDataSet): TSimpleEntity;
begin
  Result := Self;
  TSimpleUtil.GetValuesFromDataset(aDataSet, Self);
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

end.
