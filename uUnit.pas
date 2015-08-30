unit uUnit;

interface
uses
    System.StrUtils, System.Classes, System.SysUtils, System.Generics.Collections,
    System.Generics.Defaults, System.Types,
    OXmlPDOM, OXmlSAX, FMX.TreeView, SvCollections.Tries;
type
{$M+}
    TSection = (scPrivate, scProtected, scPublic, scPublished, scNone);
    TUnitType = (utUnit, utClass, utClassHelper, utRecord, utEnum, utPointer);
    TPairStringObject = TPair<string, TObject>;
    TDictnStringXMLList = TDictionary<string, IXMLNodeList>;
    TUnit = class;
    TUnitItem = record
    //private
    //public
        FName : string;
        FType : string;
        FValue : string;
        function LoadFromXML(AXML : PXMLNode) : Boolean;
        function GetText: string;

        property Text : string read GetText;
    end;
    TParameter = record
    private
        function GetText: string;
    public
        FName : string;
        FType : string;
        FKind : string;
        function LoadFromXML(AXML : PXMLNode) : Boolean;
        function ToUnitItem : TUnitItem;
        property Text : string read GetText;
    end;
    TStatements = record
    public
        FBegin : TPoint;
        FEnd : TPoint;
        function LoadFromXML(AXML : PXMLNode) : Boolean;
    end;
    TMethod = class
    private
        //FUnitSection : TObject;
        function GetText: string;
        function GetParent: string;
    function GetIsStatic: Boolean;
    public
        FGUID : string;
        FConsts : TArray<TUnitItem>;
        FVariables : TArray<TUnitItem>;
        FMethods : TArray<uUnit.TMethod>;
        FClasses : TArray<TUnit>;
        FKind : string;
        FName : string;
        FClass : Boolean;
        FPARAMETERS : TArray<TParameter>;
        FStatements : TStatements;
        FReturnType : string;
        constructor Create;
        destructor Destroy; override;
        procedure AddParametr(par : TParameter);

        function LoadFromXML(AXML : PXMLNode) : Boolean;
        function IsMePos(const APos : TPoint) : Boolean;
        function GetVariables : TList<TUnitItem>;
        function ToUnitItem : TUnitItem;

        property Text : string read GetText;
        property Parent : string read GetParent;
        property IsStatic : Boolean read GetIsStatic;
        //property UnitSection : TObject read FUnitSection write FUnitSection;
    end;
    TUnit = class
    public
    type
    {$REGION 'TUnitSection'}
        TUnitSection = class
        private
            [weak]FParent : TUnit;
            FSection : TSection;
            FConsts : TArray<TUnitItem>;
            FVariables : TArray<TUnitItem>;
            FMethods : TArray<uUnit.TMethod>;
            FClasses : TArray<TUnit>;
            FFields : TArray<TUnitItem>;
            FProperties : TArray<TUnitItem>;
            FMethodsDescription : TArray<uUnit.TMethod>;
            function GetTvItem: TTreeViewItem;
            function GetNameSection : string;
            function GetParent : TUnit;

            procedure SetParent(const AValue : TUnit);
        public
            constructor Create(const Root : IXMLNodeList; Section : TSection; Parent : TUnit); overload;
            constructor Create; overload;
            destructor Destroy; override;

            function LoadFromXML(const Root : IXMLNodeList) : Boolean;
            function ToList : TList<TPairStringObject>; overload;
            function ToList(const IsStatic : Boolean{ = False}) : TList<string>; overload;
            function ParentUnit : TUnit;
            function GetVariables(const OnlyStatic : Boolean) : TList<TUnitItem>;
            function IsEmpty : Boolean;

            class function LoadFromDict(const ADict : TDictnStringXMLList;
                Section : TSection; Parent : TUnit) : TUnitSection;

            procedure AddUnit(const Value : TUnit);
            property TvItem : TTreeViewItem read GetTvItem;
        published
            property Consts : TArray<TUnitItem> read FConsts write FConsts;
            property Variables : TArray<TUnitItem> read FVariables write FVariables;
            property Methods : TArray<uUnit.TMethod> read FMethods write FMethods;
            property Classes : TArray<TUnit> read FClasses write FClasses;
            property Fields : TArray<TUnitItem> read FFields write FFields;
            property Properties : TArray<TUnitItem> read FProperties write FProperties;
            property MethodsDescription : TArray<uUnit.TMethod> read FMethodsDescription write FMethodsDescription;
            property Section : TSection read FSection write FSection;
            property Parent : TUnit read GetParent write SetParent;
        end;
    {$ENDREGION}
    private
        [weak]FParentSection : TUnitSection;
        FName : string;
        FUnitType : TUnitType;
        FType : string;
        FParent : string;
        FSections : TObjectList<TUnitSection>;//TArray<TUnitSection>;
        //FTvItem : TTreeViewItem;
        FUsesList : TArray<string>;
        FIdentifiers : TArray<string>;
        function GetTvItem: TTreeViewItem;
        function GetText: string;
        function GetFullName: string;
    public
        class function LoadFromASTXml(const AXML : string) : TUnit;
        class function LoadFromDict(const ADict : TDictnStringXMLList;
            UnitType : TUnitType = TUnitType.utClass) : TUnit;
        class function LoadFromXMLNode(const ANode : PXMLNode) : TUnit;
        class function CreateDict(const ANode : PXMLNode; NeedChild : Boolean = True) : TDictnStringXMLList;
        class function S_UNIT_VALUE : string;
    public
        constructor Create(const Root : IXMLNodeList;
            UnitType : TUnitType = TUnitType.utClass); overload;
        constructor Create; overload;
        destructor Destroy; override;

        function ToList(const OnlyPublic : Boolean = true) :
            TList<TPairStringObject>; overload;
        function ToList(const IsStatic : Boolean; const OnlyPublic : Boolean{ = true}) :
            TList<string>; overload;
        function GetSections(const OnlyPublic : Boolean = true) :
            TList<TUnitSection>;
        function GetFileName: string;
        function MethodsDescription(const APos : TPoint) : TMethod;
        function ToUnitItem : TUnitItem;
        function GetVariables(const OnlyPublic: Boolean = True; const
            OnlyStatic : Boolean = False) : TList<TUnitItem>;
        function IsEMpty : Boolean;
        function ParseHeader(const ANode : PXMLNode) : Boolean;
        function GetClassName : string;

        procedure LoadUses(const UsesList : IXMLNodeList);

        property Text : string read GetText;
        property TvItem : TTreeViewItem read GetTvItem;
        property Name : string read FName write FName;
        property UnitType : TUnitType read FUnitType write FUnitType;
        property TypeA : string read FType write FType;
        property Parent : string read FParent write FParent;
        property Sections : TObjectList<TUnitSection> read FSections write FSections;
        property UsesList : TArray<string> read FUsesList write FUsesList;
        property Identifiers : TArray<string> read FIdentifiers write FIdentifiers;
        property ParentSection : TUnitSection read FParentSection write FParentSection;
        property FullName : string read GetFullName;
    end;

    TMethodHelper = class helper for TMethod
        function GetUnitSection : TUnit.TUnitSection;
    end;
implementation
uses
    uParser, uConst, uUnit.Helper;
const
    NameNodes : TArray<string> = ['CONSTANTS', 'FIELD', 'METHOD', 'PRIVATE',
        'PROTECTED', 'PUBLIC', 'PUBLISHED', 'TYPESECTION', 'VARIABLES'];
    ElementNames : TArray<string> = ['CONSTANT', 'FIELD', 'METHOD', 'PRIVATE',
        'PROTECTED', 'PUBLIC', 'PUBLISHED', 'TYPEDECL', 'VARIABLE'];
    DictNames : TArray<string> = ['TYPESECTION/TYPEDECL', 'METHOD', 'METHODDESCR',
        'VARIABLES', 'CONSTANTS', 'NAME', 'PROPERTY', 'PRIVATE', 'PROTECTED', 'PUBLIC',
        'PUBLISHED', 'FIELD'];
{ TMethod }
{------------------------------------------------------------------------------}
procedure TMethod.AddParametr(par: TParameter);
var
    LastIndex : integer;
begin
    LastIndex := Length(FPARAMETERS) + 1;
    SetLength(FPARAMETERS, LastIndex);
    FPARAMETERS[LastIndex - 1] := par;
end;
{------------------------------------------------------------------------------}
function TMethod.GetText: string;
var
    temp, paramString : string;
    param : TParameter;
begin
    for param in FPARAMETERS do
    begin
        if not paramString.IsEmpty then
            paramString := paramString + ';';
        paramString := paramString + param.Text;
    end;
    temp := FName + uConst.TConst.DELIMITER +
        '(' + paramString + ')';
    if not FReturnType.IsEmpty then
        temp := temp + ': ' + FReturnType;
    Result := temp + ';';
end;
{------------------------------------------------------------------------------}
function TMethod.GetVariables: TList<TUnitItem>;
var
    list : TList<TUnitItem>;
    LUnitItem : TUnitItem;
    LMethod : uUnit.TMethod;
    LUnit : TUnit;
    LParam : TParameter;
begin
    list := TList<TUnitItem>.Create;
    for LUnitItem in FConsts do
        list.Add(LUnitItem);
    for LUnitItem in FVariables do
        list.Add(LUnitItem);
    for LMethod in FMethods do
        list.Add(LMethod.ToUnitItem);
    for LUnit in FClasses do
        list.Add(LUnit.ToUnitItem);
    for LParam in FPARAMETERS do
        list.Add(LParam.ToUnitItem);
    Result := list;
end;
{------------------------------------------------------------------------------}
constructor TMethod.Create;
var
    LGUID : TGUID;
begin
    inherited;
    FGUID := TGUID.NewGuid.ToString;
end;
{------------------------------------------------------------------------------}
destructor TMethod.Destroy;
begin
    SetLength(FConsts, 0);
    SetLength(FVariables, 0);
    SetLength(FMethods, 0);
    SetLength(FClasses, 0);
    SetLength(FPARAMETERS, 0);
  inherited;
end;
{------------------------------------------------------------------------------}
function TMethod.GetIsStatic: Boolean;
begin
    Result := FClass or (FKind = 'constructor') or
        (FKind = 'destructor');
end;
{------------------------------------------------------------------------------}
function TMethod.GetParent: string;
begin
    if FName.Contains('.') then
        Result := FName
    else
        Result := EmptyStr;
    Delete(Result,LastDelimiter('.', Result), Result.Length - 1);
end;
{------------------------------------------------------------------------------}
function TMethod.IsMePos(const APos: TPoint): Boolean;
begin
    Result := ((FStatements.FEnd.Y > APos.Y) and (FStatements.FBegin.Y < APos.Y))
        or
        ((FStatements.FEnd.Y = APos.Y) and (FStatements.FEnd.X > APos.X - 1))
        or
        ((FStatements.FBegin.Y = APos.Y) and (FStatements.FBegin.X < APos.X + 1));
end;
{------------------------------------------------------------------------------}
function TMethod.LoadFromXML(AXML: PXMLNode): Boolean;
var
    APar : TParameter;
    AOutNode, AParameter : PXMLNode;
    LNodeList : TXMLResNodeList;
    LUnitSection : TUnit.TUnitSection;
    LUnit : TUnit;
    temp : string;
begin
    if not Assigned(AXML) then
        Exit(False);
    if AXML.FindChild('STATEMENTS', AOutNode) then
        FStatements.LoadFromXML(AOutNode);
    TParser.SetAttribute(AXML, 'name', FName);
    TParser.SetAttribute(AXML, 'kind', FKind);
    FClass := TParser.SetAttribute(AXML, 'class', temp);
    if AXML.FindChild('PARAMETERS', AOutNode) then
    begin
        AParameter := nil;
        while AOutNode.GetNextChild(AParameter) do
        begin
            APar.LoadFromXML(AParameter);
            AddParametr(APar);
        end;
    end;
    TParser.FindAndSetAttribute(AXML, ['RETURNTYPE', 'TYPE'], 'name', FReturnType);
    LNodeList := nil;
    TParser.GetChildXMLNodeList(AXML, LNodeList);
    LUnit := TUnit.Create(LNodeList);
    if LUnit.Sections.Count > 0 then
    begin
        LUnitSection := LUnit.Sections[0];
        //FUnitSection := LUnitSection;
        Self.FConsts := LUnitSection.Consts;
        Self.FVariables := LUnitSection.Variables;
        Self.FMethods := LUnitSection.Methods;
        Self.FClasses := LUnitSection.Classes;
    end;
    FreeAndNil(LUnit);
    Result := True;
end;
{------------------------------------------------------------------------------}
function TMethod.ToUnitItem: TUnitItem;
begin
    Result.FName := self.FName;
    Result.FType := self.FReturnType
end;
{------------------------------------------------------------------------------}
{ TParameter }
{------------------------------------------------------------------------------}
function TParameter.GetText: string;
begin
    Result := FKind + ' ' + FName + ': ' + FType;
end;
{------------------------------------------------------------------------------}
function TParameter.LoadFromXML(AXML: PXMLNode): Boolean;
var
    item : TUnitItem;
begin
    if not item.LoadFromXML(AXML) then
        Exit(False);
    FName := item.FName;
    FType := item.FType;
    TParser.SetAttribute(AXML, 'kind', FKind);
    Result := True;
end;
{------------------------------------------------------------------------------}
function TParameter.ToUnitItem: TUnitItem;
begin
    Result.FName := self.FName;
    Result.FType := Self.FType;
end;
{------------------------------------------------------------------------------}
{ TUnitItem }
{------------------------------------------------------------------------------}
function TUnitItem.GetText: string;
begin
    Result := Trim(FName + uConst.TConst.DELIMITER + ': ' + FValue + ' ' + FType);
end;
{------------------------------------------------------------------------------}
function TUnitItem.LoadFromXML(AXML: PXMLNode): Boolean;
var
    APar : TParameter;
    AOutNode, AParameter : PXMLNode;
begin
    if not Assigned(AXML) then
        Exit(False);
    FName := EmptyStr;
    TParser.FindAndSetAttribute(AXML, ['NAME'], 'value', FName);
    if FName.IsEmpty then
        TParser.SetAttribute(AXML, 'name', FName);
    TParser.FindAndSetAttribute(AXML, ['TYPE'], 'name', FType);
    TParser.FindAndSetAttribute(AXML, ['VALUE', 'EXPRESSION', 'LITERAL'], 'value', FValue);
    Result := True;
end;
{------------------------------------------------------------------------------}
{ TUnit }
{------------------------------------------------------------------------------}
constructor TUnit.Create(const Root : IXMLNodeList;
    UnitType : TUnitType);
var
    AXML : PXMLNode;
    unitSection : TUnitSection;
begin
    Self.Create;
    ParentSection := nil;
    FUnitType := UnitType;
    if Assigned(Root) then
        AXML := Root.GetFirst
    else
        AXML := nil;
    self.ParseHeader(AXML);
    unitSection := TUnitSection.Create(Root, TSection.scNone, self);
    if unitSection.IsEmpty then
        FreeAndNil(unitSection);
end;
{------------------------------------------------------------------------------}
constructor TUnit.Create;
begin
    inherited;
    FSections := TObjectList<TUnitSection>.Create;
end;
{------------------------------------------------------------------------------}
destructor TUnit.Destroy;
begin
    SetLength(FUsesList, 0);
    FSections.Free;
    //FreeAndNil(FSections);
    SetLength(FIdentifiers, 0);
  inherited;
end;
{------------------------------------------------------------------------------}
function TUnit.GetClassName: string;
begin
    case FUnitType of
        utUnit: Result := FName;
        utClass, utRecord, utEnum, utPointer: Result := FullName;
        utClassHelper: Result := self.Parent + TConst.GUID_CLASS_HELPER;
        else
            Result := EmptyStr;
    end;
end;
{------------------------------------------------------------------------------}
function TUnit.GetFileName: string;
begin
    Result := Fname + '.json';
end;
{------------------------------------------------------------------------------}
function TUnit.GetFullName: string;
var
    LName : string;
begin
    Result := EmptyStr;
    if Assigned(ParentSection) then
    begin
        Result := FName;
        LName := ParentSection.Parent.FullName;
        if not LName.IsEmpty then
            Result := LName + '.' + FName;
    end;
end;
{------------------------------------------------------------------------------}
function TUnit.GetSections(const OnlyPublic: Boolean): TList<TUnitSection>;
var
    tempSection : TUnitSection;
begin
    Result := TList<TUnitSection>.Create;
    for tempSection in FSections do
        if not OnlyPublic or
            (OnlyPublic and (tempSection.FSection in [scPublic, scPublished, scNone])) then
        begin
            Result.Add(tempSection);
        end;
end;
{------------------------------------------------------------------------------}
function TUnit.GetText: string;
begin
    Result := Trim(FName + uConst.TCOnst.DELIMITER + ': ' + FType + ' ' + FParent);
end;
{------------------------------------------------------------------------------}
function TUnit.GetTvItem: TTreeViewItem;
var
    tvItem, childItem, FTvItem : TTreeViewItem;
    unitSection : TUnitSection;
begin
    FTvItem := TTreeViewItem.Create(nil);
    FTvItem.Text := FName;
    for unitSection in FSections do
        unitSection.TvItem.Parent := FTvItem;
    Result := FTvItem;
end;
{------------------------------------------------------------------------------}
function TUnit.GetVariables(const OnlyPublic: Boolean; const OnlyStatic :
    Boolean): TList<TUnitItem>;
var
    LUnitSection : TUnit.TUnitSection;
    List : TList<TUnitItem>;
    LSections : TList<TUnitSection>;
begin
    LSections := GetSections(OnlyPublic);
    list := TList<TUnitItem>.Create;
    for LUnitSection in LSections do
        list.AddRange(LUnitSection.GetVariables(OnlyStatic).ToArray);
    FreeAndNil(LSections);
    Result := list;
end;
{------------------------------------------------------------------------------}
function TUnit.IsEMpty: Boolean;
begin
    Result := FName.IsEmpty;
end;
{------------------------------------------------------------------------------}
function TUnit.ParseHeader(const ANode : PXMLNode) : Boolean;
var
    LIdentifiers : IXMLNodeList;
    LNode : PXMLNode;
    temp : string;
    isHelper : Boolean;
begin
    TParser.SetAttribute(ANode, 'name', FName);
    FType := EmptyStr;
    TParser.FindAndSetAttribute(ANode, ['TYPE'], 'type', FType);
    if FType.IsEmpty then
        TParser.FindAndSetAttribute(ANode, ['TYPE'], 'name', FType);
    FIdentifiers := TParser.GetIdentifiers(ANode);
    FParent := EmptyStr;
    TParser.FindAndSetAttribute(ANode, ['TYPE', 'TYPE'], 'name', FParent);
    isHelper := FParent.IsEmpty and TParser.FindAndSetAttribute(ANode,
        ['TYPE', 'HELPER', 'TYPE'], 'name', FParent);
    //change unitType
    if isHelper then
        FUnitType := TUnitType.utClassHelper
    else
        if FType = 'enum' then
            FUnitType := TUnitType.utEnum
        else if FType = 'record' then
            FUnitType := TUnitType.utRecord
            else if FType = 'pointer' then
                FUnitType := TUnitType.utPointer;
end;
{------------------------------------------------------------------------------}
class function TUnit.LoadFromASTXml(const AXML: string): TUnit;
var
    LUnit : TUnit;
    XML: IXMLDocument;
    Root : PXMLNode;
    XMLUnit, XMLUsesList,
    XMLTypeDecl, XMLMethods, XMLMethodsDescription,
    XMLVars, XMLConsts : IXMLNodeList; temp : string;
    LDict : TDictnStringXMLList;
begin
    if AXML.IsEmpty then
        Exit(nil);
    //CREATE XML DOC
    XML := CreateXMLDoc;//create XML doc with root node named "root"
    XML.LoadFromXML(AXml);
    Root := XML.DocumentElement;
    XMLTypeDecl := Root.SelectNodes('/UNIT/INTERFACE/TYPESECTION/TYPEDECL');
    XMLMethods := Root.SelectNodes('/UNIT/INTERFACE/METHOD');
    XMLMethodsDescription := Root.SelectNodes('/UNIT/IMPLEMENTATION/METHOD');
    XMLVars := Root.SelectNodes('/UNIT/INTERFACE/VARIABLES/VARIABLE');
    XMLConsts := Root.SelectNodes('/UNIT/INTERFACE/CONSTANTS/CONSTANT');
    XMLUnit := Root.SelectNodes('/UNIT/');
    XMLUsesList := Root.SelectNodes('/UNIT/INTERFACE/USES | /UNIT/IMPLEMENTATION/USES');
    LDict := TParser.DictionaryFromArray(
        [XMLTypeDecl, XMLMethods, XMLMethodsDescription, XMLVars, XMLConsts, XMLUnit,
            nil, nil, nil, nil],
        DictNames);
    LUnit := TUnit.LoadFromDict(LDict, TUnitType.utUnit);
    LUnit.LoadUses(XMLUsesList);
    if LUnit.IsEMpty then
        FreeAndNil(LUnit);
    FreeAndNil(LDict);
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
class function TUnit.LoadFromDict(const ADict : TDictnStringXMLList; UnitType : TUnitType) : TUnit;
var
    LUnit : TUnit;
    AXML : PXMLNode;
    LList : IXMLNodeList;
begin
    LUnit := TUnit.Create;
    LUnit.ParentSection := nil;
    LUnit.FUnitType := UnitType;
    if ADict.TryGetValue(DictNames[5], LList) then
        AXML := LList.GetFirst
    else
        AXML := nil;
    LUnit.ParseHeader(AXML);
    TUnitSection.LoadFromDict(ADict, TSection.scNone, LUnit);
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
class function TUnit.LoadFromXMLNode(const ANode : PXMLNode) : TUnit;
var
    LUnit : TUnit;
    LDict : TDictnStringXMLList;
    count : integer;
begin
    count := ANode.ChildCount;
    if count < 1 then
        Exit(nil);
    LDict := TUnit.CreateDict(ANode);
    LUnit := TUnit.LoadFromDict(LDict, TUnitType.utClass);
    if LUnit.IsEMpty then
        FreeAndNil(LUnit);
    FreeAndNil(LDict);
    Result := LUnit;
end;
{------------------------------------------------------------------------------}
class function TUnit.CreateDict(const ANode : PXMLNode; NeedChild : Boolean) : TDictnStringXMLList;
var
    LNode : PXMLNode;
    XMLUnit, XMLTypeDecl,
    XMLPublic, XMLMethods, XMLPrivate,
    XMLPublished, XMLProtected,
    XMLFields, XMLProperties,
    XMLVars, XMLConsts : IXMLNodeList;
    temp : string;
    LDict : TDictnStringXMLList;
    count : integer;
begin
    temp := ANode.XML;
    if NeedChild then
        LNode := ANode.FirstChild
    else
        LNode := ANode;
    temp := LNode.XML;
    XMLUnit := TXMLResNodeList.Create;
    XMLUnit.Add(ANode);
    LNode.SelectNodes('TYPESECTION/TYPEDECL', XMLTypeDecl);
    LNode.SelectNodes('METHOD', XMLMethods);
    LNode.SelectNodes(DictNames[3], XMLVars);
    LNode.SelectNodes(DictNames[4], XMLConsts);
    LNode.SelectNodes(DictNames[6], XMLProperties);
    LNode.SelectNodes(DictNames[7], XMLPrivate);
    XMLPrivate := TParser.GetXMLListOneNode(XMLPrivate);
    LNode.SelectNodes(DictNames[8], XMLProtected);
    XMLProtected := TParser.GetXMLListOneNode(XMLProtected);
    LNode.SelectNodes(DictNames[9], XMLPublic);
    XMLPublic := TParser.GetXMLListOneNode(XMLPublic);
    LNode.SelectNodes(DictNames[10], XMLPublished);
    XMLPublished := TParser.GetXMLListOneNode(XMLPublished);
    LNode.SelectNodes(DictNames[11], XMLFields);
    LDict := TParser.DictionaryFromArray(
        [XMLTypeDecl, XMLMethods, nil, XMLVars, XMLConsts, XMLUnit,
            XMLProperties, XMLPrivate, XMLProtected, XMLPublic, XMLPublished, XMLFields],
        DictNames);
    Result := LDict;
end;
{------------------------------------------------------------------------------}
class function TUnit.S_UNIT_VALUE : string;
begin   //for assign utClass
    Result := '{BCC3D7FB-E0D1-4A8F-A37E-AD4776C0A6E3}';
end;
{------------------------------------------------------------------------------}
procedure TUnit.LoadUses(const UsesList: IXMLNodeList);
begin
    self.UsesList := TParser.GetUses(UsesList);
end;
{------------------------------------------------------------------------------}
function TUnit.MethodsDescription(const APos: TPoint): TMethod;
var
    LMethod : TMethod;
    LSection : TUnitSection;
begin
    Result := nil;
    for LSection in Sections do
        for LMethod in LSection.FMethodsDescription do
            if LMethod.IsMePos(APos) then
                Exit(LMethod);
end;
{------------------------------------------------------------------------------}
function TUnit.ToList(const OnlyPublic: Boolean): TList<TPairStringObject>;
var
    list : TList<TPairStringObject>;
    sections : TList<TUnitSection>;
    section : TUnitSection;
    LName : string;
begin
    list := TList<TPairStringObject>.Create;
    sections := GetSections(OnlyPublic);
    LName := GetClassName;
    list.Add(TPairStringObject.Create(LName, Pointer(self)));
    for section in sections do
        list.AddRange(section.ToList.ToArray);
    FreeAndNil(sections);
    Result := list;
end;
{------------------------------------------------------------------------------}
function TUnit.ToList(const IsStatic : Boolean; const OnlyPublic : Boolean) :
    TList<string>;
var
    list : TList<string>;
    sections : TList<TUnitSection>;
    section : TUnitSection;
begin
    list := TList<string>.Create;
    sections := GetSections(OnlyPublic);
    for section in sections do
        list.AddRange(section.ToList(IsStatic).ToArray);
    FreeAndNil(sections);
    Result := list;
end;
{------------------------------------------------------------------------------}
function TUnit.ToUnitItem: TUnitItem;
begin
    case FUnitType of
        utClassHelper: Result.FName := self.Parent + TConst.GUID_CLASS_HELPER;
        else
            Result.FName := self.FName;
    end;
    Result.FType := self.FParent;
    Result.FValue := S_UNIT_VALUE;
end;
{------------------------------------------------------------------------------}
{$REGION 'TUnitSection'}
{ TUnit.TUnitSection }
{------------------------------------------------------------------------------}
procedure TUnit.TUnitSection.AddUnit(const Value: TUnit);
var
    LastIndex : integer;
begin
    if not Assigned(Value) then
        exit;
    LastIndex := Length(FClasses) + 1;
    SetLength(FClasses, LastIndex);
    FClasses[LastIndex - 1] := Value;
end;
{------------------------------------------------------------------------------}
constructor TUnit.TUnitSection.Create(const Root: IXMLNodeList; Section : TSection; Parent : TUnit);
begin
    inherited Create;
    FParent := Parent;
    self.Section := Section;
    if LoadFromXML(Root) then
        Parent.Sections.Add(Self);
end;
{------------------------------------------------------------------------------}
constructor TUnit.TUnitSection.Create;
begin
    inherited;
    Section := TSection.scNone;
end;
{------------------------------------------------------------------------------}
destructor TUnit.TUnitSection.Destroy;
begin
    SetLength(FConsts, 0);
    SetLength(FVariables, 0);
    SetLength(FMethods, 0);
    SetLength(FClasses, 0);
    SetLength(FFields, 0);
    SetLength(FProperties, 0);
    SetLength(FMethodsDescription, 0);
  inherited;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.GetNameSection: string;
begin
    case Section of
        scPrivate: Result := 'PRIVATE';
        scProtected: Result := 'PROTECTED';
        scPublic: Result := 'PUBLIC';
        scPublished: Result := 'PUBLISHED';
        scNone: Result := 'PUBLISHED';
    end;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.GetParent : TUnit;
begin
    Result := FParent;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.ParentUnit : TUnit;
begin
    Result := TUnit(FParent);
end;
{------------------------------------------------------------------------------}
procedure TUnit.TUnitSection.SetParent(const AValue : TUnit);
begin
    FParent := AValue;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.GetTvItem: TTreeViewItem;
var
    mainItem, tvItem, childItem : TTreeViewItem;
    unitItem : TUnitItem;
    method : TMethod;
    AClass : TUnit;
begin
    mainItem := TTreeViewItem.Create(nil);
    mainItem.Text := GetNameSection;
    if Length(Consts) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'CONSTS';
        for unitItem in Consts do
        begin
            childItem := TTreeViewItem.Create(tvItem);
            childItem.Parent := tvItem;
            childItem.Text := unitItem.Text;
        end;
    end;
    if Length(Properties) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'PROPERTIES';
        for unitItem in Properties do
        begin
            childItem := TTreeViewItem.Create(tvItem);
            childItem.Parent := tvItem;
            childItem.Text := unitItem.Text;
        end;
    end;
    if Length(Variables) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'VAR';
        for unitItem in Variables do
        begin
            childItem := TTreeViewItem.Create(tvItem);
            childItem.Parent := tvItem;
            childItem.Text := unitItem.Text;
        end;
    end;
    if Length(Fields) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'FIELDS';
        for unitItem in Fields do
        begin
            childItem := TTreeViewItem.Create(tvItem);
            childItem.Parent := tvItem;
            childItem.Text := unitItem.Text;
        end;
    end;
    if Length(Methods) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'METHODS';
        for method in Methods do
        begin
            childItem := TTreeViewItem.Create(tvItem);
            childItem.Parent := tvItem;
            childItem.Text := method.Text;
        end;
    end;
    if Length(FClasses) > 0 then
    begin
        tvItem := TTreeViewItem.Create(mainItem);
        tvItem.Parent := mainItem;
        tvItem.Text := 'CLASSES';
        for AClass in FClasses do
        begin
           childItem := AClass.TvItem;
           childItem.Parent := tvItem;
        end;
    end;
    Result := mainItem;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.GetVariables(const OnlyStatic : Boolean): TList<TUnitItem>;
var
    list : TList<TUnitItem>;
    LUnitItem : TUnitItem;
    LMethod : uUnit.TMethod;
    LUnit : TUnit;
begin
    list := TList<TUnitItem>.Create;
    for LUnitItem in FConsts do
        list.Add(LUnitItem);
    if not OnlyStatic then
    begin
        for LUnitItem in FVariables do
            list.Add(LUnitItem);
        for LUnitItem in FFields do
            list.Add(LUnitItem);
        for LUnitItem in FProperties do
            list.Add(LUnitItem);
    end;
    for LMethod in FMethods do
        if not OnlyStatic or (OnlyStatic and LMethod.IsStatic) then
            list.Add(LMethod.ToUnitItem);
    for LUnit in FClasses do
        list.Add(LUnit.ToUnitItem);
    Result := list;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.IsEmpty: Boolean;
begin
    Result := (Length(self.Consts) + Length(self.Variables) + Length(self.Fields) +
        Length(self.Methods) + Length(self.FClasses) + Length(self.Properties)) = 0;
end;
{------------------------------------------------------------------------------}
class function TUnit.TUnitSection.LoadFromDict(const ADict: TDictnStringXMLList;
  Section: TSection; Parent: TUnit): TUnitSection;
var
    LSection :  TUnitSection;
    LUnit : TUnit;
    LList : IXMLNodeList;
begin
    LSection :=  TUnitSection.Create;
    LSection.FParent := Parent;
    LSection.Section := Section;
    if ADict.TryGetValue(DictNames[0], LList) then
        LSection.Classes := TParser.GetClasses(LList);
    if ADict.TryGetValue(DictNames[1], LList) then
        LSection.Methods := TParser.GetMethods(LList);
    if ADict.TryGetValue(DictNames[2], LList) then
        LSection.FMethodsDescription := TParser.GetMethods(LList);
    if ADict.TryGetValue(DictNames[3], LList) then
        LSection.Variables := TParser.GetConstOrVariables(LList);
    if ADict.TryGetValue(DictNames[4], LList) then
        LSection.Consts := TParser.GetConstOrVariables(LList);
    if ADict.TryGetValue(DictNames[6], LList) then
        LSection.Properties := TParser.GetConstOrVariables(LList);
    if ADict.TryGetValue(DictNames[7], LList) then
        TUnitSection.LoadFromDict(TUnit.CreateDict(LList.GetFirst, False),
            TSection.scPrivate, Parent);
    if ADict.TryGetValue(DictNames[8], LList) then
        TUnitSection.LoadFromDict(TUnit.CreateDict(LList.GetFirst, False),
            TSection.scProtected, Parent);
    if ADict.TryGetValue(DictNames[9], LList) then
        TUnitSection.LoadFromDict(TUnit.CreateDict(LList.GetFirst, False),
            TSection.scPublic, Parent);
    if ADict.TryGetValue(DictNames[10], LList) then
        TUnitSection.LoadFromDict(TUnit.CreateDict(LList.GetFirst, False),
            TSection.scPublished, Parent);
    if ADict.TryGetValue(DictNames[11], LList) then
        LSection.Fields := TParser.GetConstOrVariables(LList);
    for LUnit in LSection.CLasses do
        LUnit.ParentSection := LSection;
    if not LSection.IsEmpty then
        Parent.Sections.Add(LSection)
    else
        FreeAndNil(LSection);
    Result := LSection;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.LoadFromXML(const Root: IXMLNodeList): Boolean;
    procedure SetNodeList(const isNeedParent : Boolean; Index : Integer;
        const NodeName : string; const Node : PXMLNode;
        var NodeList: IXMLNodeList; const Section : TSection = TSection.scNone);
    var
        unitSection : TUnitSection;
        aNode : PXMLNode;
        aRoot : IXMLNodeList;
        temp : string;
        count : integer;
    begin
        if Assigned(NodeList) then
            exit;
        if isNeedParent then
            aNode := Node.ParentNode
        else
            aNode := Node;
        if Assigned(aNode) then
            aNode.SelectNodes(NodeName, NodeList);
        temp := aNode.XML;
        if Section <> TSection.scNone then
        begin
            count := NodeList.Count;
            aRoot := TParser.GetChildXMLNodeList(NodeList);
            unitSection := TUnitSection.Create(aRoot, Section, self.FParent);
            if unitSection.IsEmpty then
                FreeAndNil(unitSection);
        end;
    end;
var
    Constants, Variables, Methods, Classes,
    Fields, Privates, Protecteds, Publics, Publisheds,
    Properties, MethodsDescription : IXMLNodeList;
    aIXMLNodeLists : TArray<IXMLNodeList>;
    aNode, aParent : PXMLNode;
    aNodeName : string;
    FoundIndex : integer;
    isNeedParent : Boolean;
    LUnit : TUnit;
begin
    if not Assigned(Root) then
        Exit(False);
    aIXMLNodeLists := [Constants, Fields,  Methods, Privates,
        Protecteds, Publics, Publisheds, Classes, Variables];
    //aIXMLNodeLists := ;
    for aNode in Root do
    begin
        aNodeName := aNode.NodeName;
        if TArray.BinarySearch<String>(NameNodes, aNodeName, FoundIndex,
            TStringComparer.Ordinal) then
        begin
            isNeedParent := FoundIndex in [1..6];
            case FoundIndex of
                0..2, 7..8:
                    SetNodeList(isNeedParent, FoundIndex, ElementNames[FoundIndex],
                        aNode, aIXMLNodeLists[FoundIndex]);
                3..6:
                begin
                    SetNodeList(isNeedParent, FoundIndex, ElementNames[FoundIndex],
                        aNode, aIXMLNodeLists[FoundIndex], TSection(FoundIndex - 3));
                end;
            end;

        end
        else
        begin
            if aNodeName = 'PROPERTY' then
            begin
                SetNodeList(True, 0, 'PROPERTY',
                        aNode, Properties);
            end;
            if aNodeName = 'IMPLEMENTATION' then
            begin
                SetNodeList(False, 0, 'METHOD',
                        aNode, MethodsDescription);
            end;
        end;
    end;
    self.Consts := TParser.GetConstOrVariables(aIXMLNodeLists[0]);
    self.Variables := TParser.GetConstOrVariables(aIXMLNodeLists[8]);
    self.Fields := TParser.GetConstOrVariables(aIXMLNodeLists[1]);
    self.Methods := TParser.GetMethods(aIXMLNodeLists[2]);
    Self.FMethodsDescription := TParser.GetMethods(MethodsDescription);
    self.Classes := TParser.GetClasses(aIXMLNodeLists[7]);
    for LUnit in self.CLasses do
        LUnit.ParentSection := self;
    self.Properties := TParser.GetConstOrVariables(Properties);
    Result := not self.IsEmpty;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.ToList: TList<TPairStringObject>;
var
    list, LClassList : TList<TPairStringObject>;
    AClass : TUnit;
    temp : string;
begin
    list := TList<TPairStringObject>.Create;
    if Length(FClasses) > 0 then
        for AClass in FClasses do
        begin
            //list.Add(TPairStringObject.Create(AClass.FName, Pointer(AClass)));
            LClassList := AClass.ToList;
            list.AddRange(LClassList.ToArray);
        end;
    Result := list;
end;
{------------------------------------------------------------------------------}
function TUnit.TUnitSection.ToList(const IsStatic : Boolean) : TList<string>;
var
    list : TList<string>;
    unitItem : TUnitItem;
    method : TMethod;
    AClass : TUnit;
    LAddList : TProc<string, string>;
begin
    list := TList<string>.Create;
    LAddList := procedure (AName, AType : string)
        begin
            list.Add(AName + uConst.TConst.DELIMITER + AType);
        end;
    for unitItem in Consts do
    begin
        LAddList(unitItem.Text, 'const');
    end;
    if not IsStatic then
    begin
        for unitItem in Fields do
        begin
            LAddList(unitItem.Text, 'field');
        end;

        for unitItem in Properties do
        begin
            LAddList(unitItem.Text, 'property');
        end;

        for unitItem in Variables do
        begin
            LAddList(unitItem.Text, 'var');
        end;
    end;
    for method in Methods do
    begin
        if not IsStatic or (IsStatic and method.IsStatic) then
            LAddList(method.Text, method.FKind);
    end;
    for AClass in FClasses do
    begin
        LAddList(AClass.Text, 'type');
    end;
    Result := list;
end;
{------------------------------------------------------------------------------}
{$ENDREGION}
{ TMethodHelper }
{------------------------------------------------------------------------------}
function TMethodHelper.GetUnitSection: TUnit.TUnitSection;
begin
    //Result := UnitSection as TUnit.TUnitSection;
end;
{------------------------------------------------------------------------------}
{ TStatements }
{------------------------------------------------------------------------------}
function TStatements.LoadFromXML(AXML: PXMLNode): Boolean;
begin
    if not Assigned(AXML) then
        Exit(False);
    TParser.SetAttribute(AXML, 'begin_line', FBegin.Y);
    TParser.SetAttribute(AXML, 'begin_col', FBegin.X);
    TParser.SetAttribute(AXML, 'end_line', FEnd.Y);
    TParser.SetAttribute(AXML, 'end_col', FEnd.X);
    Result := True;
end;
{------------------------------------------------------------------------------}
end.
