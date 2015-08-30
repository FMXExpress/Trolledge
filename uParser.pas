unit uParser;

interface
uses
    System.StrUtils, System.Classes, System.SysUtils, System.Generics.Collections,
    OXmlPDOM, OXmlSAX, uUnit, uUnit.Helper;
type
    TParser = class
    public
        class function FindAndSetAttribute(const ANode : PXMLNode;
            const ChildNames : TArray<string>; const AttributeName : string;
            var AOut : string) : Boolean;
        class function SetAttribute(const ANode : PXMLNode;
            const AttributeName : string;
            var AOut : string) : Boolean; overload;
        class function SetAttribute(const ANode : PXMLNode;
            const AttributeName : string;
            var AOut : Integer) : Boolean; overload;
        class function FindChild(const ANode : PXMLNode;
            const ChildNames : TArray<string>;
            var outNode: PXMLNode) : Boolean;
        class function GetConstOrVariables(ANodeList : IXMLNodeList) : TArray<TUnitItem>;
        class function GetMethods(ANodeList : IXMLNodeList) : TArray<TMethod>;
        class function GetClasses(ANodeList : IXMLNodeList) : TArray<TUnit>;
        class function GetIdentifiers(const ANode : PXMLNode) : TArray<string>;
        class function GetXMLNodeList(ALists : TArray<IXMLNodeList>) : TXMLResNodeList;
        class function GetChildXMLNodeList(const ParentList :IXMLNodeList) : TXMLResNodeList; overload;
        class function GetChildXMLNodeList(const ParentNode :PXMLNode; var
            AList : TXMLResNodeList) : TXMLResNodeList; overload;
        class function GetUses(const ANodeList : IXMLNodeList) : TArray<string>;
        class function DictionaryFromArray(const ARoot : TArray<IXMLNodeList>;
            const ANames : TArray<string>) : TDictnStringXMLList;
        class function GetXMLListOneNode(const AList :IXMLNodeList) : TXMLResNodeList;
    end;
implementation
{ TParser }
{------------------------------------------------------------------------------}
class function TParser.DictionaryFromArray(const ARoot: TArray<IXMLNodeList>;
  const ANames: TArray<string>): TDictnStringXMLList;
var
    i, count : integer;
begin
    Result := TDictnStringXMLList.Create;
    count := Length(ARoot) - 1;
    for i := 0 to count do
        if Assigned(ARoot[i]) and (ARoot[i].Count > 0) then
            Result.Add(ANames[i], ARoot[i]);
end;
{------------------------------------------------------------------------------}
class function TParser.FindAndSetAttribute(const ANode: PXMLNode;
  const ChildNames : TArray<string>; const AttributeName: string; var AOut: string): Boolean;
var
    AOutNode : PXMLNode;
    Success : Boolean;
    childName : string;
begin
    Success := FindChild(ANode, ChildNames, AOutNode);
    if Success then
        SetAttribute(AOutNode, AttributeName, AOut);
    Result := Success;
end;
{------------------------------------------------------------------------------}
class function TParser.FindChild(const ANode: PXMLNode;
  const ChildNames: TArray<string>;
  var outNode: PXMLNode): Boolean;
var
    Success : Boolean;
    childName : string;
begin
    if not Assigned(ANode) then
        Exit(False);
    Success := True;
    outNode := ANode;
    for childName in ChildNames do
        if Success then
            Success := outNode.FindChild(childName, outNode);
    Result := Success;
end;
{------------------------------------------------------------------------------}
class function TParser.SetAttribute(const ANode: PXMLNode;
  const AttributeName: string; var AOut: string): Boolean;
begin
    Result := False;
    if not Assigned(ANode) then
        Exit(False);
    AOut := EmptyStr;
    if ANode.HasAttribute(AttributeName) then
    begin
        AOut := ANode.GetAttribute(AttributeName);
        Result := True;
    end;
end;
{------------------------------------------------------------------------------}
class function TParser.SetAttribute(const ANode : PXMLNode;
    const AttributeName : string;
    var AOut : Integer) : Boolean;
begin
    if not Assigned(ANode) then
        Exit(False);
    AOut := -1;
    if ANode.HasAttribute(AttributeName) then
        AOut := Integer.Parse(ANode.GetAttribute(AttributeName));
end;
{------------------------------------------------------------------------------}
class function TParser.GetConstOrVariables(ANodeList: IXMLNodeList): TArray<TUnitItem>;
var
    UnitNode : PXMLNode;
    i, count : Integer;
    AUnit : TUnitItem;
begin
  UnitNode := nil;
  if not Assigned(ANodeList) then
    Exit;
  count := ANodeList.Count;
  SetLength(Result, count);
  for i := 0 to count - 1 do
  begin
    UnitNode := ANodeList[i];
    if AUnit.LoadFromXML(UnitNode) then
        Result[i] := AUnit;
  end;
end;
{------------------------------------------------------------------------------}
class function TParser.GetIdentifiers(const ANode: PXMLNode): TArray<string>;
var
    LIdentifiers : IXMLNodeList;
    LNode : PXMLNode;
begin
    if not Assigned(ANode) then
        Exit;
    if ANode.SelectNodes('TYPE/IDENTIFIER/@name', LIdentifiers) then
        for LNode in LIdentifiers do
        begin
            TArray.Add<string>(Lnode.NodeValue, Result);
        end;
end;
{------------------------------------------------------------------------------}
class function TParser.GetMethods(ANodeList: IXMLNodeList): TArray<TMethod>;
var
    Method : PXMLNode;
    i, count : Integer;
    AMethod : TMethod;
begin
  if not Assigned(ANodeList) then
    Exit;
  Method := nil;
  count := ANodeList.Count;
  SetLength(Result, count);
  for i := 0 to count - 1 do
  begin
    Method := ANodeList[i];
    AMethod := TMethod.Create;
    if AMethod.LoadFromXML(Method) then
        Result[i] := AMethod
    else
        FreeAndNil(AMethod);
  end;
end;
{------------------------------------------------------------------------------}
class function TParser.GetUses(const ANodeList: IXMLNodeList): TArray<string>;
var
    LNodeList : TXMLResNodeList;
    LChildNode : PXMLNode;
    LCount, i : Integer;
begin
    if not Assigned(ANodeList) then
        Exit;
    LNodeList := TParser.GetChildXMLNodeList(ANodeList);
    LCount := LNodeList.Count;
    SetLength(Result, LCount);
    i := 0;
    for LChildNode in LNodeList do
    begin
        TParser.SetAttribute(LChildNode, 'name', Result[i]);
        Inc(i);
    end;
    LNodeList := nil;
    TArray.Sort<string>(Result);
end;
{------------------------------------------------------------------------------}
class function TParser.GetChildXMLNodeList(
  const ParentList: IXMLNodeList): TXMLResNodeList;
var
    nodeList : TXMLResNodeList;
    AChildNode, ATwoCHildNode : PXMLNode;
    temp : string;
begin
    nodeList := TXMLResNodeList.Create;
    for AChildNode in ParentList do
    begin
        temp := AChildNode.NodeName;
        TParser.GetChildXMLNodeList(AChildNode, nodeList);
    end;
    Result := nodeList;
end;
{------------------------------------------------------------------------------}
class function TParser.GetChildXMLNodeList(const ParentNode :PXMLNode; var
    AList : TXMLResNodeList) : TXMLResNodeList;
var
    AChildNode : PXMLNode;
begin
    if not Assigned(AList) then
        AList := TXMLResNodeList.Create;
    AChildNode := nil;
    while ParentNode.GetNextChild(AChildNode) do
        AList.Add(AChildNode);
end;
{------------------------------------------------------------------------------}
class function TParser.GetClasses(ANodeList: IXMLNodeList): TArray<TUnit>;
var
    AClass, AOutNode, AChildNode : PXMLNode;
    nodeList : IXMLNodeList;
    i, count : Integer;
    FClass : TUnit;
    temp : string;
begin
  if not Assigned(ANodeList) then
    Exit;
  AClass := nil;
  count := ANodeList.Count;
  for i := 0 to count - 1 do
  begin
    AClass := ANodeList[i];
    temp := AClass.XML;
    FClass := TUnit.LoadFromXMLNode(AClass);
    {nodeList := TXMLResNodeList.Create;
    nodeList.Add(AClass);
    if FindChild(AClass, ['TYPE'], AOutNode) then    //этим и отличается класс
    begin
        AChildNode := nil;
        while AOutNode.GetNextChild(AChildNode) do
        begin
            nodeList.Add(AChildNode);
            temp := AChildNode.NodeName;
        end;
    end;
    FClass := TUnit.Create(nodeList);}
    if Assigned(FClass) then
        TArray.Add<TUnit>(FClass, Result);
  end;
end;
{------------------------------------------------------------------------------}
class function TParser.GetXMLListOneNode(
  const AList: IXMLNodeList): TXMLResNodeList;
var
    LNode, LChildNode : PXMLNode;
    i, j, count : Integer;
begin
    if not Assigned(AList) then
        Exit(nil);
    count := AList.Count;
    for i := count - 1 downto 1 do
    begin
        LNode := AList.Nodes[i];
        LChildNode := LNode.FirstChild;
        while Assigned(LChildNode) do
        begin
            AList[0].ChildNodes.Add(LChildNode.CloneNode(True));
            LChildNode := LChildNode.NextSibling;
        end;
        AList.Delete(i);
    end;
    Exit(AList as TXMLResNodeList);
end;
{------------------------------------------------------------------------------}
class function TParser.GetXMLNodeList(
  ALists: TArray<IXMLNodeList>): TXMLResNodeList;
var
    aList : IXMLNodeList;
    aNode : PXMLNode;
    AXMLList : TXMLResNodeList;
begin
    AXMLList := TXMLResNodeList.Create;
    for aList in ALists do
        for aNode in aList do
            if Assigned(aNode) then
                AXMLList.Add(aNode);
    Result := AXMLList;
end;
{------------------------------------------------------------------------------}
end.
