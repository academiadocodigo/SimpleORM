unit uExport;

interface

uses
  RTTI, SysUtils, generics.collections, Classes, System.IOUtils, SimpleRTTIHelper,
  SimpleAttributes;

type
  TExport = class
  public
    class procedure TextReport<T: class, constructor>(Lista: TObjectList<T>);
  end;

implementation

uses
  System.TypInfo;

{ TExport }

class procedure TExport.TextReport<T>(Lista: TObjectList<T>);
var
  oObjeto: TObject;
  oTipo: TRttiType;
  oPropriedade: TRttiProperty;
  oFormat: Format;
  oValue: TValue;
  oRelatorio: TStringList;
  oLines: TStringBuilder;
  sPastaRelatorio: string;
begin
  oRelatorio := TStringList.Create;
  oLines := TStringBuilder.Create;
  oObjeto := T.create;
  try
    oTipo := TRttiContext.Create.GetType(oObjeto.classType);
  finally
    FreeAndNil(oObjeto);
  end;

  try
    for oObjeto in Lista do
    begin
      oLines.Clear;
      for oPropriedade in oTipo.GetProperties do
      begin
        oFormat := oPropriedade.GetAttribute<Format>;
        oValue := oPropriedade.GetValue(oObjeto);

        case oValue.Kind of
          tkString, tkWChar, tkLString, tkWString, tkVariant, tkUString:
            oLines.Append(oValue.ToString.PadRight(oFormat.MaxSize));
          tkInteger, tkInt64:
            oLines.Append(oValue.ToString.PadLeft(oFormat.MaxSize, '0'));
          tkFloat:
            begin
              if oValue.TypeInfo = TypeInfo(Real) then
                oLines.Append(FormatFloat(oFormat.GetNumericMask, oValue.AsExtended));

              if (oValue.TypeInfo = TypeInfo(TDate))
                or (oValue.TypeInfo = TypeInfo(TTime))
                or (oValue.TypeInfo = TypeInfo(TDateTime)) then
                oLines.Append(FormatDateTime(oFormat.Mask, oValue.AsExtended));
            end;
        end;
      end;
      oRelatorio.Add(oLines.ToString);
    end;
    sPastaRelatorio := ExtractFilePath(ParamStr(0)) + '\report';
    ForceDirectories(sPastaRelatorio);
    oRelatorio.SaveToFile(sPastaRelatorio + '\rel.txt');
  finally
    FreeAndNil(oLines);
    FreeAndNil(oRelatorio);
  end;
end;

end.
