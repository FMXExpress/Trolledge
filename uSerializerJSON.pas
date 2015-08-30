unit uSerializerJSON;

interface
uses
    System.SysUtils, System.Classes, System.Rtti, Data.DBXJSON, Data.DBXJSONReflect,
    FMX.TreeView, SvCollections.Tries, System.JSON, System.IOUtils,
    uUnit, uSerializer;
type
    TSerializerJSON = class(TSerializer)
    private
        class var
        FMarchal: TJSONMarshal;
        FUnMarshal: TJSONUnMarshal;
    public
    const
        SJSON : string = 'json';
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

{ TSerializer }
{------------------------------------------------------------------------------}
constructor TSerializerJSON.Create;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
destructor TSerializerJSON.Destroy;
begin

  inherited;
end;
{------------------------------------------------------------------------------}
class function TSerializerJSON.SEARCH_PATTERN: string;
begin
    Result := '*.' + SJSON;
end;
{------------------------------------------------------------------------------}
class function TSerializerJSON.Marshal(const Value: TUnit): string;
begin
    if not Assigned(Value) then
        Exit('');
    FMarchal:= TJSONMarshal.Create(TJSONConverter.Create);
    Result := FMarchal.Marshal(Value).ToString;
    FreeAndNil(FMarchal);
end;
{------------------------------------------------------------------------------}
class function TSerializerJSON.MarshalToFile(const Value : TUnit;
    const AFileName : string): string;
var
    LValue : string;
begin
    LValue := TSerializerJSON.Marshal(Value);
    TSerializerUtils.SaveString(LValue, AFileName + '.' + SJSON);
    Result := LValue;
end;
{------------------------------------------------------------------------------}
class function TSerializerJSON.UnMarshal(const Value: string): TUnit;
begin
    FUnMarshal := TJSONUnMarshal.Create;
    FUnMarshal.RegisterReverter(TUnit, 'FSections',
        procedure(Data: TObject; Field: String; Args: TListOfObjects)
        var
            obj: TObject;
            LUnit : TUnit;
            LSection : TUnit.TUnitSection;
      begin
        LUnit := TUnit(Data);
        for obj in Args do
        begin
            LSection := TUnit.TUnitSection(obj);
            LSection.Parent := LUnit;
            LUnit.Sections.Add(LSection);
        end
      end);
    FUnMarshal.RegisterReverter(TUnit.TUnitSection, 'FClasses',
        procedure(Data: TObject; Field: String; Args: TListOfObjects)
        var
            obj: TObject;
            LUnit : TUnit;
            LSection : TUnit.TUnitSection;
      begin
        LSection := TUnit.TUnitSection(Data);
        for obj in Args do
        begin
            LUnit := TUnit(obj);
            LUnit.ParentSection := LSection;
            LSection.AddUnit(LUnit);
        end
      end);
    Result := FUnMarshal.Unmarshal(TJSONObject.ParseJSONValue(Value)) as TUnit;
    FreeAndNil(FUnMarshal);
end;
{------------------------------------------------------------------------------}
class function TSerializerJSON.UnMarshalFromFile(const AFileName: string): TUnit;
var
    LValue : string;
    LUnit : TUnit;
begin
    LUnit := nil;
    if TSerializerUtils.LoadString(AFileName, LValue) then
    begin
        LUnit := TSerializerJSON.UnMarshal(LValue);
    end;
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
end.
