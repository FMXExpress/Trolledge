unit uHelper.SvStringTrie;

interface
uses
    System.Generics.Collections, System.SysUtils,
    uUnit, SvCollections.Tries;
type
    TSvStringTrieUnit = TSvStringTrie<TUnit>;
    TSvStringTrieHelper = class helper for TSvStringTrieUnit
        procedure AddUnit(const AUnit : TUnit; AOnlyPublic : Boolean = true);
    end;
implementation

{ TSvStringTrieHelper }
{------------------------------------------------------------------------------}
procedure TSvStringTrieHelper.AddUnit(const AUnit: TUnit; AOnlyPublic : Boolean);
var
    tempPair : TPairStringObject;
    tempUnit : TUnit;
    LUnitList : TList<TPairStringObject>;
begin
    LUnitList := AUnit.ToList(AOnlyPublic);
    for tempPair in LUnitList do
    begin
        tempUnit := TUnit(tempPair.Value);
        if Assigned(tempUnit) and not tempPair.Key.IsEmpty then
            self.Add(tempPair.Key, tempUnit);
    end;
end;
{------------------------------------------------------------------------------}
end.
