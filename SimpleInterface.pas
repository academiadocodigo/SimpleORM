unit SimpleInterface;

interface

uses
  System.Classes, System.Generics.Collections, Data.DB, System.TypInfo;

type

  iSimpleDAO<T> = interface
    ['{19261B52-6122-4C41-9DDE-D3A1247CC461}']
    function Insert(aValue : T) : iSimpleDAO<T>;
    function Update(aValue : T) : iSimpleDAO<T>;
    function Delete(aValue : T) : iSimpleDAO<T>;
    function DataSource( aDataSource : TDataSource) : iSimpleDAO<T>;
    function Find : TList<T>; overload;
    function Find(aId : Integer) : T; overload;
    function Find (aWhere : String) : TList<T>; overload;
  end;

  iSimpleRTTI<T> = interface
    ['{EEC49F47-24AC-4D82-9BEE-C259330A8993}']
    function ClassName (var aClassName : String) : iSimpleRTTI<T>;
    function DictionaryFields(var aDictionary : TDictionary<string, variant>) : iSimpleRTTI<T>;
    function ListFields (var List : TList<String>) : iSimpleRTTI<T>;
    function Update (var aUpdate : String) : iSimpleRTTI<T>;
    function Where (var aWhere : String) : iSimpleRTTI<T>;
    function Fields (var aFields : String) : iSimpleRTTI<T>;
    function FieldsInsert (var aFields : String) : iSimpleRTTI<T>;
    function Param (var aParam : String) : iSimpleRTTI<T>;
    function DataSetToEntityList (aDataSet : TDataSet; var aList : TList<T>) : iSimpleRTTI<T>;
    function DataSetToEntity (aDataSet : TDataSet; var aEntity : T) : iSimpleRTTI<T>;
  end;

  iSimpleSQL<T> = interface
    ['{1590A7C6-6E32-4579-9E60-38C966C1EB49}']
    function Insert (var aSQL : String) : iSimpleSQL<T>;
    function Update (var aSQL : String) : iSimpleSQL<T>;
    function Delete (var aSQL : String) : iSimpleSQL<T>;
    function Select (OrderBy : String; var aSQL : String) : iSimpleSQL<T>;
    function SelectId (var aSQL : String) : iSimpleSQL<T>;
    function SelectWhere (aWhere : String; OrderBy : String; var aSQL: String) : iSimpleSQL<T>;
  end;

  iSimpleQuery = interface
    ['{6DCCA942-736D-4C66-AC9B-94151F14853A}']
    function SQL : TStrings;
    function Params : TParams;
    function ExecSQL : iSimpleQuery;
    function DataSet : TDataSet;
    function Open(aSQL : String) : iSimpleQuery; overload;
    function Open : iSimpleQuery; overload;
  end;



implementation

end.
