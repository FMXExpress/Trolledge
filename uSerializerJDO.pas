unit uSerializerJDO;

interface
uses
    System.SysUtils, System.Classes, System.Rtti, Data.DBXJSON, Data.DBXJSONReflect,
    FMX.TreeView, SvCollections.Tries, System.JSON, System.IOUtils,
    uUnit, JsonDataObjects, uUnit.Helper, uSerializer;
type
    TSerializerJDO = class(TSerializer)
    public
    const
        SJDO : string = 'JDO';
    public
        constructor Create;
        destructor Destroy; override;

        class function Marshal(const Value : TUnit) : string; override;
        class function MarshalToFile(const Value : TUnit; const AFileName : string) : string; override;
        class function UnMarshalFromFile(const AFileName : string) : TUnit; override;
        class function UnMarshal(const Value : string) : TUnit; override;
        class function SEARCH_PATTERN : string; override;
    end;

implementation
{------------------------------------------------------------------------------}
{ TSerializerJDO }
{------------------------------------------------------------------------------}
constructor TSerializerJDO.Create;
begin

end;
{------------------------------------------------------------------------------}
destructor TSerializerJDO.Destroy;
begin

  inherited;
end;
{------------------------------------------------------------------------------}
class function TSerializerJDO.SEARCH_PATTERN: string;
begin
    Result := '*.' + SJDO;
end;
{------------------------------------------------------------------------------}
class function TSerializerJDO.Marshal(const Value: TUnit): string;
var
  Obj: TJsonObject;
begin
    Obj := Value.SaveToJson;
    Result := Obj.ToJSON();
    FreeAndNil(Obj);
end;
{------------------------------------------------------------------------------}
class function TSerializerJDO.MarshalToFile(const Value: TUnit;
  const AFileName: string): string;
var
    LValue : string;
begin
    LValue := TSerializerJDO.Marshal(Value);
    TSerializerUtils.SaveString(LValue, AFileName + '.' + SJDO);
    Result := LValue;
end;
{------------------------------------------------------------------------------}
class function TSerializerJDO.UnMarshal(const Value: string): TUnit;
begin
    Result := TUnit.LoadFromJson(Value);
end;
{------------------------------------------------------------------------------}
class function TSerializerJDO.UnMarshalFromFile(const AFileName: string): TUnit;
var
    LValue : string;
    LUnit : TUnit;
begin
    LUnit := nil;
    if TSerializerUtils.LoadString(AFileName, LValue) then
    begin
        LUnit := TSerializerJDO.UnMarshal(LValue);
    end;
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
end.
