unit SimpleCommon;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes;

type
  TSimpleCommon = class
  private
  public
    class function MemoryStreamToOleVariant(Stream: TMemoryStream): OleVariant;
    class function OleVariantToMemoryStream(Ov: OleVariant): TMemoryStream;
  end;

implementation

{ TSimpleCommon }

class function TSimpleCommon.MemoryStreamToOleVariant(Stream: TMemoryStream)
  : OleVariant;
var
  lData: PByteArray;
begin
  Result := VarArrayCreate([0, Stream.Size - 1], varByte);
  lData := VarArrayLock(Result);
  try
    Stream.Position := 0;
    Stream.ReadBuffer(lData^, Stream.Size);
  finally
    VarArrayUnlock(Result);
  end;
end;

class function TSimpleCommon.OleVariantToMemoryStream(Ov: OleVariant): TMemoryStream;
var
  lData: PByteArray;
  lSize: Integer;
begin
  Result := TMemoryStream.Create;
  try
    lSize := (VarArrayHighBound(Ov, 1) - (VarArrayLowBound(Ov, 1) + 1));
    lData := VarArrayLock(Ov);
    try
      Result.Position := 0;
      Result.WriteBuffer(lData^, lSize);
    finally
      VarArrayUnlock(Ov);
    end;
  except
    FreeAndNil(Result);
  end;
end;

end.
