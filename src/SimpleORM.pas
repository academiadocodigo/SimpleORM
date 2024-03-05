unit SimpleORM;

interface

uses
  SimpleORM.Attributes,
  SimpleORM.DAO.DataSetToJSON,
  SimpleORM.DAO,
  SimpleORM.DAO.SQLAttribute,
  SimpleORM.Entity,
  SimpleORM.Intf,
  SimpleORM.JSON,
  SimpleORM.JSONUtil,
  SimpleORM.Query.Firedac,
  SimpleORM.RTTI,
  SimpleORM.RTTI.Helper,
  SimpleORM.SQL,
  SimpleORM.Types,
  SimpleORM.Util,
  SimpleORM.Validator;

type
  { SimpleORM.Attributes }
  Tabela = SimpleORM.Attributes.Tabela;
  Campo = SimpleORM.Attributes.Campo;
  PK = SimpleORM.Attributes.PK;
  FK = SimpleORM.Attributes.FK;
  NotNull = SimpleORM.Attributes.NotNull;
  Ignore = SimpleORM.Attributes.Ignore;
  AutoInc = SimpleORM.Attributes.AutoInc;
  NumberOnly = SimpleORM.Attributes.NumberOnly;
  Bind = SimpleORM.Attributes.Bind;
  Display = SimpleORM.Attributes.Display;
  Format = SimpleORM.Attributes.Format;
  Relationship = SimpleORM.Attributes.Relationship;
  HasOne = SimpleORM.Attributes.HasOne;
  BelongsTo = SimpleORM.Attributes.BelongsTo;
  HasMany = SimpleORM.Attributes.HasMany;
  BelongsToMany = SimpleORM.Attributes.BelongsToMany;
  Enumerator = SimpleORM.Attributes.Enumerator;

  { SimpleORM.DAO.DataSetToJSON }
  EDataSetToJSONException = SimpleORM.DAO.DataSetToJSON.EDataSetToJSONException;

  TBooleanFieldType = SimpleORM.DAO.DataSetToJSON.TBooleanFieldType;
  TDataSetFieldType = SimpleORM.DAO.DataSetToJSON.TDataSetFieldType;

  iDataSetToJSON<T: class> = interface(SimpleORM.DAO.DataSetToJSON.iDataSetToJSON<T>) end;
  TDataSetToJSON<T: class, constructor> = class(SimpleORM.DAO.DataSetToJSON.TDataSetToJSON<T>) end;

  { SimpleORM.DAO }
  TSimpleDAO<T: class, constructor> = class(SimpleORM.DAO.TSimpleDAO<T>) end;

  { SimpleORM.DAO.SQLAttribute }
  TSimpleDAOSQLAttribute<T: class, constructor> = class(SimpleORM.DAO.SQLAttribute.TSimpleDAOSQLAttribute<T>) end;

  { SimpleORM.Entity }
  TSimpleEntity = SimpleORM.Entity.TSimpleEntity;
  TSimpleEntityList<T: TSimpleEntity, constructor> = class(SimpleORM.Entity.TSimpleEntityList<T>) end;

  { SimpleORM.Intf }
  iSimpleDAO<T: class> = interface(SimpleORM.Intf.iSimpleDAO<T>) end;
  iSimpleDAOSQLAttribute<T: class> = interface(SimpleORM.Intf.iSimpleDAOSQLAttribute<T>) end;
  iSimpleRTTI<T: class> = interface(SimpleORM.Intf.iSimpleRTTI<T>) end;
  iSimpleSQL<T> = interface(SimpleORM.Intf.iSimpleSQL<T>) end;
  iSimpleQuery = SimpleORM.Intf.iSimpleQuery;

  { SimpleORM.JSON }
  TcdJsonValueType = SimpleORM.JSON.TcdJsonValueType;
  TcdJsonStructType = SimpleORM.JSON.TcdJsonStructType;
  TcdJsonNull = SimpleORM.JSON.TcdJsonNull;
  TcdJsonEmpty = SimpleORM.JSON.TcdJsonEmpty;
  TSimpleJsonBase = SimpleORM.JSON.TSimpleJsonBase;
  TSimpleJsonValue = SimpleORM.JSON.TSimpleJsonValue;
  TSimpleJsonArray = SimpleORM.JSON.TSimpleJsonArray;
  TSimpleJsonPair = SimpleORM.JSON.TSimpleJsonPair;
  TSimpleJsonObject = SimpleORM.JSON.TSimpleJsonObject;
  TSimpleJson = SimpleORM.JSON.TSimpleJson;

  { SimpleORM.JSONUtil }
  TJsonFlags = SimpleORM.JSONUtil.TJsonFlags;
  TSimpleJsonUtil = SimpleORM.JSONUtil.TSimpleJsonUtil;

  { SimpleORM.Query.Firedac }
  TSimpleQueryFiredac = SimpleORM.Query.Firedac.TSimpleQueryFiredac;

  { SimpleORM.RTTI }
  ESimpleRTTI = SimpleORM.RTTI.ESimpleRTTI;
  TSimpleRTTI<T: class, constructor> = class(SimpleORM.RTTI.TSimpleRTTI<T>) end;

  { SimpleORM.RTTI.Helper }
  TCustomAttributeClass = SimpleORM.RTTI.Helper.TCustomAttributeClass;
  TRttiPropertyHelper = SimpleORM.RTTI.Helper.TRttiPropertyHelper;
  TRttiTypeHelper = SimpleORM.RTTI.Helper.TRttiTypeHelper;
  TRttiFieldHelper = SimpleORM.RTTI.Helper.TRttiFieldHelper;

  { SimpleORM.SQL }
  TSimpleSQL<T: class, constructor> = class(SimpleORM.SQL.TSimpleSQL<T>) end;

  { SimpleORM.Types }
  TSQLType = SimpleORM.Types.TSQLType;

  { SimpleORM.Util }
  TSimpleUtil = SimpleORM.Util.TSimpleUtil;

  { SimpleORM.Validator }
  ESimpleValidator = SimpleORM.Validator.ESimpleValidator;
  TSimpleValidator = SimpleORM.Validator.TSimpleValidator;

implementation

end.
