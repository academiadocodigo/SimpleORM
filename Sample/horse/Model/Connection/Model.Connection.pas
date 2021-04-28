unit Model.Connection;

interface

uses
  System.JSON,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  Data.DB,
  FireDAC.Comp.Client,
  Firedac.DApt,
  FireDAC.Phys.FB,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.FBDef,
  System.Generics.Collections;

var
  FDriver : TFDPhysSQLiteDriverLink;
  FConnList : TObjectList<TFDConnection>;

function Connected : Integer;
procedure Disconnected(Index : Integer);

implementation

function Connected : Integer;
begin
  if not Assigned(FConnList) then
    FConnList := TObjectList<TFDConnection>.Create;

  FConnList.Add(TFDConnection.Create(nil));
  Result := Pred(FConnList.Count);
  FConnList.Items[Result].Params.DriverID := 'SQLite';
  FConnList.Items[Result].Params.Database := 'C:/Users/Willian Hubner/Desktop/SimpleORM-master/Sample/Database/DB.db';
  FConnList.Items[Result].Connected;
end;

procedure Disconnected(Index : Integer);
begin
  FConnList.Items[Index].Connected := False;
  FConnList.Items[Index].Free;
  FConnList.TrimExcess;
end;

end.
