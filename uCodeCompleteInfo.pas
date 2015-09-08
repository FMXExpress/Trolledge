unit uCodeCompleteInfo;

interface
uses
    System.StrUtils, System.Classes, System.SysUtils, System.Generics.Collections,
    System.Generics.Defaults, System.IOUtils, System.Types, System.Rtti, System.Threading,
    OXmlPDOM, OXmlSAX, FMX.TreeView, SvCollections.Tries, System.DateUtils,
    uUnit, uSerializer, uHelper.SvStringTrie, uConsts,
    uSerializerJDO, uSerializerJSON, uThread;
type
    TVarType = (vtLocal, vtParam, vtClass, vtUnit, vtGlobal);
    TUsesType = (utPas, utJson);
    TSvStringTrieList<T> = class(TSvObjectStringTrie<TList<T>>)
    private
        procedure AddUnitItem(const AList : TList<TUnitItem>; AType : TVarType);
        procedure RemoveUnitItem(const AList : TList<TUnitItem>; AType : TVarType);
    public
        procedure AddUnits(const ASourses : TArray<TUnit>; const AUses : TArray<string>);
        procedure Add(const S: String; const AValue: T);
        procedure Remove(const S: string; const AValue: T);
        procedure LoadFromUnit(IsStatic : Boolean; const AUnit : TUnit;
            const OnlyPublic : Boolean = True);
        procedure RemoveFromUnit(IsStatic : Boolean; const AUnit : TUnit;
            const OnlyPublic : Boolean = True);
        procedure LoadFromMethod(const AMethod : TMethod; const AParentUnit : TUnit = nil;
            const OnlyPublic : Boolean = False);
        procedure RemoveFromMethod(const AMethod : TMethod; const AParentUnit : TUnit = nil;
            const OnlyPublic : Boolean = False);
        function Find(const AName : string; var AValue : T) : Boolean;
    end;
    TVar = record
        FType : TVarType;
        FValue : TUnitItem;
    end;
    TPrevList = record
        private
            FIsParams : Boolean;
            FList : TArray<string>;
            procedure SetIsParams(const Value: Boolean);
        public
            property IsParam : Boolean read FIsParams write SetIsParams;
            function GetList : TList<string>;
    end;

    TCodeCompleteInfo = class
    private
        FPrevList : TPrevList;
        FPlatform: String;
        FThread : TLifeThread<string>;
        FSerializer : TSerializer;
        FSerializerType : TSerializerType;
        FPathList : TStringList;
        FLog : TStringList;
        FUnitList : TArray<TUnit>;
        FCurrentUnit : TUnit;
        FCurrentMethod : TMethod;
        FSvStringTrie : TSvObjectStringTrie<TUnit>;
        FVariablesTrie : TSvStringTrieList<TVar>;
        procedure SetCurrentUnit(const Value: TUnit);
        function GetUnitList: TArray<TUnit>;
        function Add(const AUnit : TUnit) : Integer;
        function CreateUnitCashList(const AUnit : TUnit) : TSvStringTrieUnit;
        //function CreateUnitVariables(const AUnit : TUnit) : TSvStringTrieList<TVar>;
        function GetCurrentUnit: TUnit;
        function GetCurrentPlatform: string;
        function GetCurrentMethod: TMethod;
        function CreateMissUsesList(const AUsesList : TArray<string>) : TList<string>;
        function LoadMissUses(const AUsesList : TList<string>) : Boolean; overload;
        function LoadMissUses(const AUnit : TUnit) : Boolean; overload;
        function LoadUsesItem(const AName : string) : Boolean;
        procedure SetCurrentPlatform(const Value: string);
        procedure SetCurrentMethod(const Value: TMethod);
        procedure RefreshVariables; overload;
        procedure RefreshVariables(const APrevMethod, ANewMethod : TMethod); overload;
        procedure SetSerializerType(const Value: TSerializerType);
        procedure LoadUnit(const AFileName : string);
        function GetPath(dirName : string) : string;
    public
        constructor Create;
        destructor Destroy; override;

        procedure SaveUnits;
        procedure LoadUnits; overload;
        procedure LoadUnits(const APath : string; AFindName : string = '*.json';
            AUsesType : TUsesType = TUsesType.utJson); overload;

        function Parse(AXml : string; AIsCurrent : Boolean = False) : Boolean;
        function ParseFromText(AText : string; AIsCurrent : Boolean = False; APlatform: string = '') : Boolean;
        function ParseFromFile(AFileName : string; AIsCurrent : Boolean = False; APlatform: string = 'MSWINDOWS') : Boolean;
        function TryGetValue(const S: string; out AValue: TList<string>;
            AOnlyPublic : Boolean = True; AParams: Boolean = False): Boolean;
        [SvSerialize('UnitList')]
        property UnitList : TArray<TUnit> read GetUnitList;
        function FindUnit(const AUnit : TUnit; var AIndex : Integer) : Boolean; overload;
        function FindUnit(const AName : string; var AIndex : Integer) : Boolean; overload;
        function FindMethod(const APos : TPoint) : TMethod;
        property CurrentUnit : TUnit read GetCurrentUnit write SetCurrentUnit;
        property CurrentMethod : TMethod read GetCurrentMethod write SetCurrentMethod;
        property PathList : TStringList read FPathList write FPathList;
        property SerializerType : TSerializerType read FSerializerType write SetSerializerType;
        property Log : TStringList read FLog;
        property ThreadLoad : TLifeThread<string> read FThread;
        property CurrentPlatform : string read GetCurrentPlatform write SetCurrentPlatform;
        property PrevList : TPrevList read FPrevList;
    public
        class function GetFoundUnits(const AValues : TArray<TUnit>;
            AUses : TArray<string>) : TList<TUnit>;
    end;


implementation
uses
    uParser, uHelper.SyntaxNode, DelphiAST.Classes, uUnit.Helper, uConst;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.GetPath(dirName : string) : string;
var
    Path : string;
begin
    {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
      Path := ExtractFilePath(ParamStr(0));
    {$ELSE}
      Path := System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim;
    {$ENDIF}
    Path := Path + System.SysUtils.PathDelim;
    if not dirName.IsEmpty then
        Path := Path + dirName + System.SysUtils.PathDelim + FPlatform + System.SysUtils.PathDelim;
    Result := Path;
end;
{------------------------------------------------------------------------------}
{ TCodeCompleteInfo }
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.Add(const AUnit: TUnit): Integer;
var
    tempPair : TPairStringObject;
    tempUnit : TUnit;
    LUnitList : TList<TPairStringObject>;
    index : Integer;
    isFInded : Boolean;
begin             
    if not Assigned(AUnit) then
        exit;
    isFInded := FindUnit(AUnit, index);
    try
        if isFInded then
            Exit(index);
        TArray.Add<TUnit>(AUnit, FUnitList);
        LUnitList := AUnit.ToList;
        for tempPair in LUnitList do
        begin
            tempUnit := TUnit(tempPair.Value);
            if Assigned(tempUnit) and not tempPair.Key.IsEmpty then
                FSvStringTrie.Add(tempPair.Key, tempUnit);
        end;
        Result := Length(FUnitList) - 1;
    finally
        LoadMissUses(AUnit);
        if Assigned(CurrentUnit) and not CurrentUnit.Equals(AUnit) then
            FVariablesTrie.AddUnits(UnitList, CurrentUnit.UsesList);
    end;
end;
{------------------------------------------------------------------------------}
constructor TCodeCompleteInfo.Create;
begin
    inherited;
    FPrevList.IsParam := False;
    FPlatform := 'MSWINDOWS';
    SerializerType := TSerializerType.stJSON;
    SerializerType := TSerializerType.stJDO;
    FSvStringTrie := TSvObjectStringTrie<TUnit>.Create(False, True);
    FVariablesTrie := TSvStringTrieList<TVar>.Create(False, True);
    FThread := TLifeThread<string>.Create
    (
        function (AValue : string) : Boolean
        begin
            Result := LoadUsesItem(AValue);
        end
    );
    FPathList := TStringList.Create;
    FLog := TStringList.Create;
    FPathList.Delimiter := PathSep;
    FCurrentUnit := nil;
    FCurrentMethod := nil;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.CreateUnitCashList(
  const AUnit: TUnit): TSvStringTrieUnit;
var
    LName : string;
    LUnit : TUnit;
    LUsesList : TList<string>;
begin
    Result := TSvStringTrie<TUnit>.Create;
    if not Assigned(AUnit) then
        Exit;
    LUsesList := TList<string>.Create;
    LUsesList.AddRange(AUnit.UsesList);    
    for LUnit in UnitList do
        if LUsesList.Contains(LUnit.Name) then
            Result.AddUnit(LUnit);
    Result.AddUnit(AUnit, False);
    FreeAndNil(LUsesList);
end;
{------------------------------------------------------------------------------}
destructor TCodeCompleteInfo.Destroy;
var
    i : Integer;
    LUnit : TUnit;
begin
    TThread.Synchronize(
        nil,
        procedure
        begin
            FThread.Terminate;
        end
        );
    //FThread.WaitFor;
    FreeAndNil(FThread);
    FreeAndNil(FCurrentUnit);
    FreeAndNil(FCurrentMethod);
    FreeAndNil(FPathList);
    FreeAndNil(FLog);
    FreeAndNil(FSvStringTrie);
    FreeAndNil(FVariablesTrie);
    FreeAndNil(FSerializer);
    SetLength(FUnitList, 0);
  inherited;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.FindMethod(const APos: TPoint): TMethod;
begin
    Result := nil;
    if Assigned(FCurrentUnit) then
    begin
        Result := FCurrentUnit.MethodsDescription(APos);
        CurrentMethod := Result;
    end;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.CreateMissUsesList(const AUsesList: TArray<string>):
    TList<string>;
var
    LUses : string;
    LUnit : TUnit;
    LUnitList : TList<string>;
begin
    Result := TList<string>.Create;
    LUnitList := TList<string>.Create;
    //load Names in temp list
    for LUnit in UnitList do
        LUnitList.Add(LUnit.Name);
    //find in loaded cash
    for LUses in AUsesList do
        if not LUnitList.Contains(LUses) then
            Result.Add(LUses);
    FreeAndNil(LUnitList);
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.FindUnit(const AUnit: TUnit;
  var AIndex: Integer): Boolean;
var
    LUnit : TUnit;
    i : integer;
begin
    if FindUnit(AUnit.Name, AIndex) then
        FUnitList[AIndex] := AUnit;
    Result := AIndex <> -1;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.FindUnit(const AName : string; var AIndex : Integer) : Boolean;
var
    i : integer;
begin
    AIndex := -1;
    for I := Low(FUnitList) to High(FUnitList) do
        if FUnitList[i].Name = AName then
        begin
            AIndex := I;
            Exit(True);
        end;
    Result := AIndex <> -1;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.GetCurrentMethod: TMethod;
begin
    Result := FCurrentMethod;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.GetCurrentUnit: TUnit;
begin
    Result := FCurrentUnit;
end;

function TCodeCompleteInfo.GetCurrentPlatform: string;
begin
    Result := FPlatform;
end;
{------------------------------------------------------------------------------}
class function TCodeCompleteInfo.GetFoundUnits(const AValues: TArray<TUnit>;
  AUses: TArray<string>): TList<TUnit>;
var
    LName : string;
    LUnit : TUnit;
    LUsesList : TList<string>;
begin
    LUsesList := TList<string>.Create;
    Result := TList<TUnit>.Create;
    LUsesList.AddRange(AUses);
    for LUnit in AValues do
        if LUsesList.Contains(LUnit.Name) then
            Result.Add(LUnit);
    FreeAndNil(LUsesList);
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.GetUnitList: TArray<TUnit>;
begin
    Result := FUnitList;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.LoadMissUses(
  const AUsesList: TList<string>): Boolean;
var
    LUses : string;
begin
    for LUses in AUsesList do
    begin
        LoadUsesItem(LUses);
    end;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.LoadUsesItem(const AName : string) : Boolean;
var
    LPasFile, LJsonFile : string;
    IsPasFound, IsJsonFound, IsNeedParse : Boolean;
    dtPasWrite, dtJsonWrite : TDateTime;
    temp : integer;
begin
    Result := False;
    try
        IsPasFound := TSerializerUtils.FindFIle(FPathList.ToStringArray, AName,
            SEARCH_PAS, LPasFile, dtPasWrite);
        IsJsonFound := TSerializerUtils.FindFIle([GetPath(CASHE_PATH)], AName,
            FSerializer.SEARCH_PATTERN, LJsonFile, dtJsonWrite);
        IsNeedParse := (dtPasWrite >  dtJsonWrite) and (IsPasFound or IsJsonFound);
        if IsNeedParse then
            Result := ParseFromFile(LPasFile, False, FPlatform)
        else
            if IsJsonFound and not FindUnit(AName, temp) then
            begin
                LoadUnit(LJsonFile);
                Result := True;
            end
            else
                Result := IsJsonFound;
    except
        on e : Exception do
            FLog.Add(TimeToStr(Now) + 'FName : ' + LPasFile + ' ' + e.Message);
    end;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.LoadMissUses(const AUnit : TUnit) : Boolean;
var
    LMissList : TList<string>;
    LTask : ITask;
begin
    LMissList := CreateMissUsesList(AUnit.UsesList);
    if Assigned(FThread) then
        FThread.AddTask(LMissList.ToArray);
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.LoadUnit(const AFileName: string);
var
    LUnit : TUnit;
begin
    LUnit := FSerializer.UnMarshalFromFile(AFileName);
    self.Add(LUnit);
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.LoadUnits(const APath: string; AFindName : string;
    AUsesType : TUsesType);
var
    FileName: string;
    LFiles : TStringDynArray;
begin
  if not ForceDirectories(APath) then
    Exit;
  LFiles := TDirectory.GetFiles(APath, AFindName, TSearchOption.soAllDirectories);
  for FileName in LFiles do
  begin
    try
        case AUsesType Of
            utJson:
            begin
                LoadUnit(FileName);
            end;
        end;
    except
      on E: Exception do
      begin
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.LoadUnits;
var
    LPath : string;
    LNow : TDateTime;
    LmSecCount : integer;
begin
    LNow := Now;
    LPath := GetPath(CASHE_PATH);
    LoadUnits(LPath, FSerializer.SEARCH_PATTERN);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('LoadUnits Time(mc) : ' + string.Parse(LmSecCount));
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.Parse(AXml: string; AIsCurrent : Boolean): Boolean;
var
    LUnit : TUnit;
    LNow : TDateTime;
    LmSecCount : integer;
    //LTask : ITask;
begin
    Result := False;
    LNow := Now;
    LUnit := nil;
    LUnit := TUnit.LoadFromASTXml(AXml);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('TUnit.LoadFromASTXml Time(mc) : ' + string.Parse(LmSecCount));
    Result := Assigned(LUnit);
    if Result then
    begin
        FSerializer.MarshalToFile(LUnit, GetPath(CASHE_PATH) + LUnit.Name);
        if AIsCurrent then
        begin
            CurrentUnit := LUnit;
            LoadMissUses(LUnit);
        end;
        self.Add(LUnit);
    end;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.ParseFromText(AText: string;
  AIsCurrent: Boolean; APlatform: string): Boolean;
var
    LXML : string;
    LNow : TDateTime;
    LmSecCount : integer;
begin
    if APlatform.IsEmpty then
        APlatform := self.CurrentPlatform;
    LNow := Now;
    LXML := TSyntaxNode.ParseText(AText, APlatform);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('TSyntaxNode.Parse Time(mc) : ' + string.Parse(LmSecCount));
    LNow := Now;
    Result := Parse(LXML, AIsCurrent);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('Info.Parse Time(mc) : ' + string.Parse(LmSecCount));
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.ParseFromFile(AFileName: string; 
  AIsCurrent: Boolean = False; APlatform: string = 'MSWINDOWS'): Boolean;
var
    LXML : string;
    LNow : TDateTime;
    LmSecCount : integer;
begin
    if APlatform.IsEmpty then
        APlatform := self.CurrentPlatform;
    LNow := Now;
    LXML := TSyntaxNode.ParseFile(AFileName, APlatform);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('TSyntaxNode.Parse Time(mc) : ' + string.Parse(LmSecCount));
    LNow := Now;
    Result := Parse(LXML, AIsCurrent);
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('Info.Parse Time(mc) : ' + string.Parse(LmSecCount));
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.RefreshVariables;
begin
    FreeAndNil(FVariablesTrie);
    FVariablesTrie := TSvStringTrieList<TVar>.Create(False, True);
    FVariablesTrie.LoadFromUnit(False, CurrentUnit, False);
    //nedd added pyblic methods in uses units
    FVariablesTrie.AddUnits(UnitList, CurrentUnit.UsesList);
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.RefreshVariables(const APrevMethod, ANewMethod : TMethod);
var
    LUnit : TUnit;
begin
    if Assigned(APrevMethod) and
        FSvStringTrie.TryGetValue(APrevMethod.Parent, LUnit) then
        FVariablesTrie.RemoveFromMethod(APrevMethod, LUnit);
    if Assigned(ANewMethod) and
        FSvStringTrie.TryGetValue(ANewMethod.Parent, LUnit) then
        FVariablesTrie.LoadFromMethod(ANewMethod, LUnit);
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.SaveUnits;
var
    LUnit : TUnit;
    LNow : TDateTime;
    LmSecCount : integer;
begin
    LNow := Now;
    for LUnit in UnitList do
    begin
        try
        FSerializer.MarshalToFile(LUnit, GetPath(CASHE_PATH) +
            LUnit.Name);
        except
            on e : Exception do
                FLog.Add('SaveUnits Error : ' + e.Message);
        end;
    end;
    LmSecCount := MilliSecondsBetween(Now, LNow);
    FLog.Add('SaveUnits Time(mc) : ' + string.Parse(LmSecCount));
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.SetCurrentMethod(const Value: TMethod);
var
    LGUID : string;
begin
    if Assigned(Value) then
        LGUID := Value.FGUID
    else
        LGUID := '{DF8483BB-144E-467C-BF3A-5D69A9769C35}';
    if not Assigned(FCurrentMethod) or not FCurrentMethod.FGUID.Contains(LGUID) then
    begin
        //здесь нужно добавлять в поисковой запрос переменные

        RefreshVariables(FCurrentMethod, Value);
        FCurrentMethod := Value;
    end;
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.SetCurrentUnit(const Value: TUnit);
begin
    FCurrentUnit := Value;
    RefreshVariables;
    //FSvStringTrie := CreateUnitCashList(Value);
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.SetCurrentPlatform(const Value: string);
begin
    FPlatform := Value;
end;
{------------------------------------------------------------------------------}
procedure TCodeCompleteInfo.SetSerializerType(const Value: TSerializerType);
begin
    if FSerializerType = Value then
        Exit;
    FSerializerType := Value;
    FreeAndNil(FSerializer);
    case Value of
        stJDO : FSerializer := TSerializerJDO.Create;
        stJSON : FSerializer := TSerializerJSON.Create
    end;
end;
{------------------------------------------------------------------------------}
function TCodeCompleteInfo.TryGetValue(const S: string;
  out AValue: TList<string>; AOnlyPublic: Boolean; AParams: Boolean): Boolean;

    procedure FilterList(const ANeedFilter : Boolean; const APrevList : TList<string>;
        AFilter : string; var ANewList : TList<string>);
    var
        LItem : string;
        LList : TList<string>;
    begin
        if not ANeedFilter then
            Exit;
        AFilter := string.LowerCase(AFilter);
        LList := TList<string>.Create;
        for LItem in APrevList do
            if SameText(AFilter, copy(string.LowerCase(LItem), 1, AFilter.Length)) then
                LList.Add(LItem);
        ANewList.Clear;
        ANewList.AddRange(LList);
        FreeAndNil(LList);
    end;
    procedure ParamCheck(const AParams : Boolean; var ANewList : TList<string>);
    var
        LItem, LParam : string;
        LList : TList<string>;
        posOpen : integer;
    begin
        FPrevList.IsParam := AParams;
        if not AParams then
            Exit;
        LList := TList<string>.Create;
        for LItem in ANewList do
        begin
            posOpen := Pos('(', LItem);
            if posOpen = 0 then
                Continue;
            LParam := Copy(LItem, posOpen + 1, LItem.Length - 1);
            Delete(LParam, Pos(')', LParam), LParam.Length - 1);
            //if SameText(AFilter, copy(string.LowerCase(LItem), 1, AFilter.Length)) then
            LList.Add(LParam);
        end;
        ANewList.Clear;
        ANewList.AddRange(LList);
        FreeAndNil(LList);
    end;
const
    END_POINT : Char = '.';
var
    LUnit, LUnitHelper : TUnit;
    LVar, LVarHelper : TVar;
    LFindList : TArray<string>;
    LFindType, LS, LToken : string;
    LCurrentTries : TSvStringTrieList<TVar>;
    LPosPoint, LLength : Integer;
    LFilter : string;
    LNeedFilter, IsFindedVar, IsFindedType, IsStatic,
    IsFIndedVarHelper, IsFindedTypeHelper : Boolean;
begin
    Result := False;
    if AParams and FPrevList.IsParam then
    begin
        AValue := FPrevList.GetList;
        Exit(AValue.Count > 0);
    end;
    if S.IsEmpty then
        Exit(False);
    IsFindedType := False;
    IsStatic := False;
    LS := S;
    LPosPoint := LS.LastDelimiter(END_POINT);
    LNeedFilter := LPosPoint <> LS.Length - 1;
    //Find last substring
    LFindList := LS.Split(END_POINT);
    if LNeedFilter then
    begin
        LLength := Length(LFindList);
        LFilter := LFindList[LLength - 1];
        SetLength(LFindList, LLength - 1);
    end;
    LCurrentTries := FVariablesTrie;
    try
        //find class
        for LToken in LFindList do
        begin
            IsFindedVar := LCurrentTries.Find(LToken, LVar);
            //find in pyblic
            IsFindedType := IsFindedVar and (FSvStringTrie.TryGetValue(LVar.FValue.FType, LUnit));
            //check pointer
            if IsFindedType and (LUnit.UnitType = TUnitType.utPointer) then
                IsFindedType := IsFindedVar and (FSvStringTrie.TryGetValue(LUnit.Parent, LUnit));
            //find static methods
            IsStatic := IsFindedVar and (LVar.FValue.FValue = TUnit.S_UNIT_VALUE) and
                not IsFindedType and FSvStringTrie.TryGetValue(LVar.FValue.FName, LUnit);
            if IsFindedType or IsStatic then
                LFindType := LUnit.GetClassName
            else
                LFindType := EmptyStr;
            IsFIndedVarHelper := not LFindType.IsEmpty and
                FVariablesTrie.Find(LFindType + TConst.GUID_CLASS_HELPER, LVarHelper);
            IsFindedTypeHelper := IsFIndedVarHelper and
                (FSvStringTrie.TryGetValue(LFindType + TConst.GUID_CLASS_HELPER, LUnitHelper));
            //end
            if IsFindedType or IsStatic then
            begin
                LCurrentTries := TSvStringTrieList<TVar>.Create(False, True);
                LCurrentTries.LoadFromUnit(IsStatic, LUnit);
                //add helper methods
                if IsFindedTypeHelper then
                    LCurrentTries.LoadFromUnit(IsStatic, LUnitHelper);
            end
            else
                Break;
        end;
        if IsFindedType or IsStatic then
        begin
            AValue := LUnit.ToList(IsStatic, AOnlyPublic);
                //add helper methods
            if IsFindedTypeHelper then
                AValue.AddRange(LUnitHelper.ToList(IsStatic, AOnlyPublic).ToArray);
            FilterList(LNeedFilter, AValue, LFilter, AValue);
            ParamCheck(AParams, AValue);
            FPrevList.FList := AValue.ToArray;
            Exit(AValue.Count > 0);
        end;
        Exit(false);
    finally
        if not FVariablesTrie.Equals(LCurrentTries) then
            FreeAndNil(LCurrentTries);
    end;
end;
{------------------------------------------------------------------------------}
{ TSvStringTrieList<T> }
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.Add(const S: String; const AValue: T);
var
    LValue : TList<T>;
begin
    if not TryGetValue(S, LValue, True) then
        LValue := TList<T>.Create;
    if not LValue.Contains(AValue) then
    begin
        LValue.Add(AValue);
        inherited Add(S, LValue);
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.Remove(const S: string; const AValue: T);
var
    LValue : TList<T>;
begin
    if not TryGetValue(S, LValue, True) then
        LValue := TList<T>.Create;
    if LValue.Contains(AValue) then
    begin
        LValue.Remove(AValue);
        if LValue.Count > 0 then
            inherited Add(S, LValue)
        else
        begin
            inherited Remove(S);
            FreeAndNil(LValue);
        end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.AddUnitItem(const AList: TList<TUnitItem>;
  AType: TVarType);
var
    item : TVar;
    LUnitItem : TUnitItem;
    LSelf : TSvStringTrieList<TVar>;
begin
    for LUnitItem in AList do
    begin
        item.FType := AType;
        item.FValue := LUnitItem;
        LSelf := TSvStringTrieList<TVar>(Self);
        if Assigned(LSelf) then
            LSelf.Add(LUnitItem.FName, item);
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.AddUnits(const ASourses: TArray<TUnit>;
  const AUses: TArray<string>);
var
    LUnit : TUnit;
    LUnits : TList<TUnit>;
begin
    LUnit := nil;
    LUnits := TCodeCompleteInfo.GetFoundUnits(ASourses, AUses);
    for LUnit in LUnits do
        self.LoadFromUnit(False, LUnit, True);
    FreeAndNil(LUnits);
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.RemoveUnitItem(const AList: TList<TUnitItem>;
  AType: TVarType);
var
    item : TVar;
    LUnitItem : TUnitItem;
    LSelf : TSvStringTrieList<TVar>;
begin
    for LUnitItem in AList do
    begin
        item.FType := AType;
        item.FValue := LUnitItem;
        LSelf := TSvStringTrieList<TVar>(Self);
        if Assigned(LSelf) then
            LSelf.Remove(LUnitItem.FName, item);
    end;
end;
{------------------------------------------------------------------------------}
function TSvStringTrieList<T>.Find(const AName: string; var AValue: T): Boolean;
var
    list : TList<T>;
begin
    Result := False;
    if TryGetValue(AName, list, True) then
    begin
        AValue := list.First;
        Result := True;
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.LoadFromMethod(const AMethod: TMethod;
    const AParentUnit : TUnit; const OnlyPublic : Boolean);
var
    unitItemList : TList<TUnitItem>;
begin
    if Assigned(AMethod) then
    begin
        unitItemList := AMethod.GetVariables;
        AddUnitItem(unitItemList, TVarType.vtLocal);
        LoadFromUnit(False, AParentUnit, False);
        FreeAndNil(unitItemList);
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.RemoveFromMethod(const AMethod: TMethod;
  const AParentUnit: TUnit; const OnlyPublic: Boolean);
var
    unitItemList : TList<TUnitItem>;
begin
    if Assigned(AMethod) then
    begin
        unitItemList := AMethod.GetVariables;
        RemoveUnitItem(unitItemList, TVarType.vtLocal);
        RemoveFromUnit(False, AParentUnit, False);
        FreeAndNil(unitItemList);
    end;
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.LoadFromUnit(IsStatic : Boolean; const AUnit: TUnit;
    const OnlyPublic : Boolean);
var
    LVarType : TVarType;
    unitItemList : TList<TUnitItem>;
begin
    if not Assigned(AUnit) then
        Exit;
    case AUnit.UnitType of
        utUnit : LVarType := TVarType.vtUnit;
        utClass : LVarType := TVarType.vtClass;
    end;
    unitItemList := AUnit.GetVariables(OnlyPublic);
    AddUnitItem(unitItemList, LVarType);
    FreeAndNil(unitItemList);
end;
{------------------------------------------------------------------------------}
procedure TSvStringTrieList<T>.RemoveFromUnit(IsStatic: Boolean;
  const AUnit: TUnit; const OnlyPublic: Boolean);
var
    LVarType : TVarType;
    unitItemList : TList<TUnitItem>;
begin
    if not Assigned(AUnit) then
        Exit;
    case AUnit.UnitType of
        utUnit : LVarType := TVarType.vtUnit;
        utClass : LVarType := TVarType.vtClass;
    end;
    unitItemList := AUnit.GetVariables(OnlyPublic);
    RemoveUnitItem(unitItemList, LVarType);
    FreeAndNil(unitItemList);
end;
{------------------------------------------------------------------------------}
{ TPrevList }
{------------------------------------------------------------------------------}
function TPrevList.GetList: TList<string>;
begin
    Result := TList<string>.Create;
    Result.AddRange(self.FList);
end;
{------------------------------------------------------------------------------}
procedure TPrevList.SetIsParams(const Value: Boolean);
begin
    if FIsParams and not Value then
        SetLength(FList, 0);
    FIsParams := Value;
end;
{------------------------------------------------------------------------------}
end.
