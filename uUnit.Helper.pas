unit uUnit.Helper;

interface
uses
    uUnit, JsonDataObjects, System.SysUtils,
    System.Generics.Collections, System.Generics.Defaults;
type
    TUnitItemHelper = record helper for TUnitItem
        procedure LoadFromJson(const AJson : string); overload;
        procedure LoadFromJson(const AJson : TJsonObject); overload;

        function SaveToJson : TJsonObject;
    end;
    TParameterHelper = record helper for TParameter
        procedure LoadFromJson(const AJson : string); overload;
        procedure LoadFromJson(const AJson : TJsonObject); overload;

        function SaveToJson : TJsonObject;
    end;
    TStatementsHelper = record helper for TStatements
        procedure LoadFromJson(const AJson : string); overload;
        procedure LoadFromJson(const AJson : TJsonObject); overload;

        function SaveToJson : TJsonObject;
    end;
    TMethodHelper = class helper for TMethod
        function SaveToJson : TJsonObject;

        class function LoadFromJson(const AJson : string) : TMethod; overload;
        class function LoadFromJson(const AJson : TJsonObject) : TMethod; overload;
    end;
    TUnitHelper = class helper for TUnit
        function SaveToJson : TJsonObject;

        class function LoadFromJson(const AJson : string) : TUnit; overload;
        class function LoadFromJson(const AJson : TJsonObject) : TUnit; overload;
    end;
    TUnitSectionHelper = class helper for TUnit.TUnitSection
        function SaveToJson : TJsonObject;

        class function LoadFromJson(const AJson : string) : TUnit.TUnitSection; overload;
        class function LoadFromJson(const AJson : TJsonObject) : TUnit.TUnitSection; overload;
    end;
    TArrayHelper = class helper for TArray
    public
        class procedure Add<T>(const AValue : T; var AValues: TArray<T>);
        class procedure Free<T>(AValues: TArray<T>);
    end;
implementation

{ TUnitItemHelper }



{ TUnitItemHelper }
{------------------------------------------------------------------------------}
procedure TUnitItemHelper.LoadFromJson(const AJson: string);
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
procedure TUnitItemHelper.LoadFromJson(const AJson: TJsonObject);
begin
    FName := AJson.S['FName'];
    FType := AJson.S['FType'];
    FValue := AJson.S['FValue'];
end;
{------------------------------------------------------------------------------}
function TUnitItemHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Create;
    JsonObj.S['FName'] := FName;
    JsonObj.S['FType'] := FType;
    JsonObj.S['FValue'] := FValue;
    Result := JsonObj;
end;
{------------------------------------------------------------------------------}
{ TParameterHelper }

procedure TParameterHelper.LoadFromJson(const AJson: string);
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
procedure TParameterHelper.LoadFromJson(const AJson: TJsonObject);
begin
    FName := AJson.S['FName'];
    FType := AJson.S['FType'];
    FKind := AJson.S['FKind'];
end;
{------------------------------------------------------------------------------}
function TParameterHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Create;
    JsonObj.S['FName'] := FName;
    JsonObj.S['FType'] := FType;
    JsonObj.S['FKind'] := FKind;
    Result := JsonObj;
end;
{------------------------------------------------------------------------------}
{ TStatementsHelper }

procedure TStatementsHelper.LoadFromJson(const AJson: string);
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
procedure TStatementsHelper.LoadFromJson(const AJson: TJsonObject);
begin
    FBegin.X := AJson.I['FBeginX'];
    FBegin.Y := AJson.I['FBeginY'];
    FEnd.X := AJson.I['FEndX'];
    FEnd.Y := AJson.I['FEndY'];
end;
{------------------------------------------------------------------------------}
function TStatementsHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Create;
    JsonObj.I['FBeginX'] := FBegin.X;
    JsonObj.I['FBeginY'] := FBegin.Y;
    JsonObj.I['FEndX'] := FEnd.X;
    JsonObj.I['FEndY'] := FEnd.Y;
    Result := JsonObj
end;
{------------------------------------------------------------------------------}
{ TMethodHelper }
{------------------------------------------------------------------------------}
class function TMethodHelper.LoadFromJson(const AJson: string): TMethod;
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    Result := LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
class function TMethodHelper.LoadFromJson(const AJson: TJsonObject): TMethod;
var
    LMethod, LChildMethod : TMethod;
    LJsonObject : TJsonObject;
    LParam : TParameter;
    Litem : TUnitItem;
    LUnit : TUnit;
begin
    LMethod := TMethod.Create;
    LMethod.FName := AJson.S['FName'];
    LMethod.FReturnType := AJson.S['FReturnTypeType'];
    LMethod.FKind := AJson.S['FKind'];
    LMethod.FClass := AJson.B['FClass'];
    LMethod.FStatements.LoadFromJson(AJson.O['FStatements']);
    for LJsonObject in AJson.A['FPARAMETERS'] do
    begin
        LParam.LoadFromJson(LJsonObject);
        TArray.Add<TParameter>(LParam, LMethod.FPARAMETERS);
    end;
    for LJsonObject in AJson.A['FConsts'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LMethod.FConsts);
    end;
    for LJsonObject in AJson.A['FVariables'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LMethod.FVariables);
    end;
    for LJsonObject in AJson.A['FMethods'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        LChildMethod := TMethod.LoadFromJson(LJsonObject);
        TArray.Add<TMethod>(LChildMethod, LMethod.FMethods);
    end;
    for LJsonObject in AJson.A['FClasses'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        LUnit := TUnit.LoadFromJson(LJsonObject);
        TArray.Add<TUnit>(LUnit, LMethod.FClasses);
    end;
    Result := LMethod;
end;
{------------------------------------------------------------------------------}
function TMethodHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
    LParam : TParameter;
    Litem : TUnitItem;
    LMethod : TMethod;
    LUnit : TUnit;
begin
    JsonObj := TJsonObject.Create;
    JsonObj.S['FName'] := FName;
    JsonObj.S['FReturnTypeType'] := FReturnType;
    JsonObj.S['FKind'] := FKind;
    JsonObj.B['FClass'] := FClass;
    JsonObj.O['FStatements'] := FStatements.SaveToJson;
    for LParam in FPARAMETERS do
        JsonObj.A['FPARAMETERS'].AddObject(LParam.SaveToJson);  ///
    for Litem in FConsts do
        JsonObj.A['FConsts'].AddObject(Litem.SaveToJson);
    for Litem in FVariables do
        JsonObj.A['FVariables'].AddObject(Litem.SaveToJson);
    for LMethod in FMethods do
        JsonObj.A['FMethods'].AddObject(LMethod.SaveToJson);
    for LUnit in FClasses do
        JsonObj.A['FClasses'].AddObject(LUnit.SaveToJson);
    Result := JsonObj;
end;
{------------------------------------------------------------------------------}
{ TUnitHelper }
{------------------------------------------------------------------------------}
class function TUnitHelper.LoadFromJson(const AJson: string): TUnit;
var
    JsonObj: TJsonObject;
    temp : string;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    temp := JsonObj.ToJSON(false);
    Result := LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
class function TUnitHelper.LoadFromJson(const AJson: TJsonObject): TUnit;
var
    LUnit : TUnit;
    JsonObj: TJsonObject;
    temp : string;
    LUsesList, LIdentifiers : TArray<string>;
    LSections : TArray<TUnitSection>;
    LSection : TUnitSection;
begin
    LUnit := TUnit.Create;
    LUnit.Name := AJson.S['FName'];
    LUnit.TypeA := AJson.S['FType'];
    LUnit.Parent := AJson.S['FParent'];
    LUnit.UnitType := TUnitType(AJson.I['FUnitType']);
    for temp in AJson.A['FUsesList'] do
        TArray.Add<string>(temp, LUsesList);
    LUnit.UsesList := LUsesList;
    for temp in AJson.A['FIdentifiers'] do
        TArray.Add<string>(temp, LIdentifiers);
    LUnit.Identifiers := LIdentifiers;
    for JsonObj in AJson.A['FSections'] do
    begin
        LSection := TUnitSection.LoadFromJson(JsonObj);
        LSection.Parent := LUnit;
        LUnit.Sections.Add(LSection);
        //TArray.Add<TUnitSection>(LSection, LSections);
    end;
    //LUnit.Sections := LSections;
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
function TUnitHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
    LUses, LIdentifier : string;
    LSection : TUnitSection;
begin
    JsonObj := TJsonObject.Create;
    JsonObj.S['FName'] := Name;
    JsonObj.S['FType'] := TypeA;
    JsonObj.S['FParent'] := Parent;
    case UnitType of
        TUnitType.utUnit : JsonObj.I['FUnitType'] := 0;
        TUnitType.utClass : JsonObj.I['FUnitType'] := 1;
        TUnitType.utClassHelper : JsonObj.I['FUnitType'] := 2;
        TUnitType.utRecord : JsonObj.I['FUnitType'] := 3;
        TUnitType.utEnum : JsonObj.I['FUnitType'] := 4;
        TUnitType.utPointer : JsonObj.I['FUnitType'] := 5;
    end;
    for LUses in UsesList do
        JsonObj.A['FUsesList'].Add(LUses);
    for LIdentifier in Identifiers do
        JsonObj.A['FIdentifiers'].Add(LIdentifier);
    for LSection in Sections do
        JsonObj.A['FSections'].AddObject(LSection.SaveToJson);
    Result := JsonObj;
end;
{------------------------------------------------------------------------------}
{ TUnitSectionHelper }
{------------------------------------------------------------------------------}
class function TUnitSectionHelper.LoadFromJson(
  const AJson: string): TUnit.TUnitSection;
var
    JsonObj: TJsonObject;
begin
    JsonObj := TJsonObject.Parse(AJson) as TJsonObject;
    Result := LoadFromJson(JsonObj);
    FreeAndNil(JsonObj);
end;
{------------------------------------------------------------------------------}
class function TUnitSectionHelper.LoadFromJson(
  const AJson: TJsonObject): TUnit.TUnitSection;
var
    LSection : TUnit.TUnitSection;
    LMethod : TMethod;
    LJsonObject : TJsonObject;
    LParam : TParameter;
    Litem : TUnitItem;
    LUnit : TUnit;
    LConsts : TArray<TUnitItem>;
    LVariables : TArray<TUnitItem>;
    LMethods : TArray<uUnit.TMethod>;
    LClasses : TArray<TUnit>;
    LFields : TArray<TUnitItem>;
    LProperties : TArray<TUnitItem>;
    LMethodsDescription : TArray<uUnit.TMethod>;
begin
    LSection := TUnit.TUnitSection.Create;
    LSection.Section := TSection(AJson.I['FSection']);
    for LJsonObject in AJson.A['FConsts'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LConsts);
    end;
    for LJsonObject in AJson.A['FVariables'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LVariables);
    end;
    for LJsonObject in AJson.A['FFields'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LFields);
    end;
    for LJsonObject in AJson.A['FProperties'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        TArray.Add<TUnitItem>(Litem, LProperties);
    end;
    for LJsonObject in AJson.A['FMethods'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        LMethod := TMethod.LoadFromJson(LJsonObject);
        TArray.Add<TMethod>(LMethod, LMethods);
    end;
    for LJsonObject in AJson.A['FMethodsDescription'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        LMethod := TMethod.LoadFromJson(LJsonObject);
        TArray.Add<TMethod>(LMethod, LMethodsDescription);
    end;
    for LJsonObject in AJson.A['FClasses'] do
    begin
        Litem.LoadFromJson(LJsonObject);
        LUnit := TUnit.LoadFromJson(LJsonObject);
        LUnit.ParentSection := LSection;
        TArray.Add<TUnit>(LUnit, LClasses);
    end;
    LSection.Consts := LConsts;
    LSection.Variables := LVariables;
    LSection.Methods := LMethods;
    LSection.Classes := LClasses;
    LSection.Fields := LFields;
    LSection.Properties := LProperties;
    LSection.MethodsDescription := LMethodsDescription;
    Result := LSection;
end;
{------------------------------------------------------------------------------}
function TUnitSectionHelper.SaveToJson: TJsonObject;
var
    JsonObj: TJsonObject;
    Litem : TUnitItem;
    LMethod : TMethod;
    LUnit : TUnit;
begin
    JsonObj := TJsonObject.Create;
    case Section of
        TSection.scPrivate : JsonObj.I['FSection'] := 0;
        TSection.scProtected : JsonObj.I['FSection'] := 1;
        TSection.scPublic : JsonObj.I['FSection'] := 2;
        TSection.scPublished : JsonObj.I['FSection'] := 3;
        TSection.scNone : JsonObj.I['FSection'] := 4;
    end;
    for Litem in Consts do
        JsonObj.A['FConsts'].AddObject(Litem.SaveToJson);
    for Litem in Variables do
        JsonObj.A['FVariables'].AddObject(Litem.SaveToJson);
    for Litem in Fields do
        JsonObj.A['FFields'].AddObject(Litem.SaveToJson);
    for Litem in Properties do
        JsonObj.A['FProperties'].AddObject(Litem.SaveToJson);
    for LMethod in Methods do
        JsonObj.A['FMethods'].AddObject(LMethod.SaveToJson);
    for LMethod in MethodsDescription do
        JsonObj.A['FMethodsDescription'].AddObject(LMethod.SaveToJson);
    for LUnit in Classes do
        JsonObj.A['FClasses'].AddObject(LUnit.SaveToJson);
    Result := JsonObj;
end;
{------------------------------------------------------------------------------}
{ TArrayHelper }
{------------------------------------------------------------------------------}
class procedure TArrayHelper.Add<T>(const AValue : T; var AValues: TArray<T>);
var
    LastIndex : integer;
begin
    LastIndex := Length(AValues) + 1;
    SetLength(AValues, LastIndex);
    AValues[LastIndex - 1] := AValue;
end;
{------------------------------------------------------------------------------}
class procedure TArrayHelper.Free<T>(AValues: TArray<T>);
var
    lItem : T;
    i : integer;
    temp : string;
begin
    for i := Low(AValues) to High(AValues) do
    begin
        try
            lItem := AValues[i];
            FreeAndNil(lItem);
        except on e : Exception do
            temp := e.Message;
        end;
    end;
end;
{------------------------------------------------------------------------------}
end.
