unit SimpleAttributes;

interface

uses
  System.RTTI, System.Variants, System.Classes;

type
  PK = class(TCustomAttribute)
  end;

  Ignore = class(TCustomAttribute)
  end;

  AutoInc = class(TCustomAttribute)
  end;

implementation


end.
