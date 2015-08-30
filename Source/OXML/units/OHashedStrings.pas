unit OHashedStrings;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OHashedStrings.pas

  TOHashedStrings
    - hashed unsorted string list
    - always keeps original order of strings (in contrary to TDictionary<,>)
    - every string is unique in the list
    - fast IndexOf() function
    - an object can be associated with every string

  TOHashedStringDictionary
    - a TDictionary<String,String> replacement for FPC and D6-2007
    - always keeps original order of keys
    - every key is unique in the list
}

{$I OXml.inc}

{$IFDEF O_DELPHI_XE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$BOOLEVAL OFF}

interface

uses
  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes,
  {$ELSE}
  SysUtils, Classes,
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    System.Generics.Collections,
    {$ELSE}
    Generics.Collections,
    {$ENDIF}
  {$ENDIF}

  OWideSupp;

type

  OHashedStringsIndex = Integer;
  OStringIndex = Integer;

  POHashItem = ^TOHashItem;
  TOHashItem = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fNext: POHashItem;
    fTextFast: OFastString;
    fIndex: OHashedStringsIndex;

    {$IFNDEF O_UNICODE}
    function GetText: OWideString;
    {$ENDIF}
  public
    function SameText(const aTextWithCase: OWideString; const aCaseSensitive: Boolean): Boolean;//aTextWithCase must be in correct case already (lowercase if not aCaseSensitive!
  public
    property TextFast: OFastString read fTextFast;
    property Text: OWideString read {$IFDEF O_UNICODE}fTextFast{$ELSE}GetText{$ENDIF};
  end;

  TOHashedStrings = class(TPersistent)
  private
    fCaseSensitive: Boolean;
    fItems: array of POHashItem;//array indexed by index
    fNextItemId: OHashedStringsIndex;//next list index to use
    fItemLength: OHashedStringsIndex;//count of allocated items in fList
    fMaxItemsBeforeGrowBuckets: OHashedStringsIndex;
    fBuckets: array of POHashItem;//array indexed by hash
    fLastHashI: OHashedStringsIndex;
    fBlockDelete: Integer;//speeds up multiple delete() calls

    procedure SetCaseSensitive(const aCaseSensitive: Boolean);
  protected
    function Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POHashItem;//aKey must be already processed with LowerCaseIfNotCaseSensitive
    procedure AddItem(const aItem: POHashItem; const aHash: OHashedStringsIndex);

    procedure GrowBuckets;
    procedure ClearBuckets;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function IndexOf(const aText: OWideString): OHashedStringsIndex;
    function Add(const aText: OWideString): OHashedStringsIndex; overload;
    function Add(const aText: OWideString; var outNewEntry: Boolean): OHashedStringsIndex; overload; virtual;
    function Get(const aIndex: OHashedStringsIndex): OWideString;
    function GetItem(const aIndex: OHashedStringsIndex): POHashItem;

    function Delete(const aText: OWideString): Boolean; overload;
    procedure Delete(const aIndex: OHashedStringsIndex); overload; virtual;
    procedure Clear(const aFullClear: Boolean = True); virtual;
    procedure BeginDelete;//speeds up multiple delete calls in a row
    procedure EndDelete;
    procedure EndDeleteForce;

    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property Strings[const aIndex: OHashedStringsIndex]: OWideString read Get; default;
    property Count: OHashedStringsIndex read fNextItemId;
  end;

  TOHashedStringObjDictionary = class(TOHashedStrings)
  private
    {$IFNDEF O_ARC}
    fOwnsObjects: Boolean;
    {$ENDIF}
    {$IFDEF O_GENERICS}
    fObjects: TList<Pointer>;
    {$ELSE}
    fObjects: TList;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    function Add(const aText: OWideString; var outNewEntry: Boolean): OHashedStringsIndex; overload; override;
    function AddObject(const aText: OWideString; const aObject: TObject;
      var outNewEntry: Boolean): OHashedStringsIndex; overload;
    function AddObject(const aText: OWideString; const aObject: TObject): OHashedStringsIndex; overload;
    procedure Delete(const aIndex: OHashedStringsIndex); overload; override;
    procedure Clear(const aFullClear: Boolean = True); override;

    procedure SetObject(const aIndex: OHashedStringsIndex; const aObject: TObject);
    function GetObject(const aIndex: OHashedStringsIndex): TObject;
    procedure SetPObject(const aIndex: OHashedStringsIndex; const aObject: Pointer);
    function GetPObject(const aIndex: OHashedStringsIndex): Pointer;

    {$IFNDEF O_ARC}
    property OwnsObjects: Boolean read fOwnsObjects write fOwnsObjects;
    {$ENDIF}
    property Objects[const aIndex: OHashedStringsIndex]: TObject read GetObject write SetObject;
    property PObjects[const aIndex: OHashedStringsIndex]: Pointer read GetPObject write SetPObject;
  end;

  TOHashedStringDictionaryEnum = class;
  {$IFDEF O_GENERICS}
  TOHashedStringDictionaryPair = TPair<OWideString,OWideString>;
  {$ELSE}
  TOHashedStringDictionaryPair = record
    Key: OWideString;
    Value: OWideString;
  end;
  {$ENDIF}

  TOHashedStringDictionary = class(TPersistent)
  private
    fKeys: TOHashedStrings;
    fValues: TOWideStringList;
    function GetCaseSensitive: Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetKey(const aIndex: OHashedStringsIndex): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetValue(const aIndex: OHashedStringsIndex): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure SetCaseSensitive(aCaseSensitive: Boolean); {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure SetValue(const aIndex: OHashedStringsIndex; const aValue: OWideString); {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetValueOfKey(const aKey: OWideString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure SetValueOfKey(const aKey, aValue: OWideString); {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetPair(const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair; {$IFDEF O_INLINE}inline;{$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function IndexOf(const aKey: OWideString): OHashedStringsIndex; {$IFDEF O_INLINE}inline;{$ENDIF}
    function Add(const aKey, aValue: OWideString): OHashedStringsIndex;
    function TryGetValue(const aKey: OWideString; var outValue: OWideString): Boolean;
    function Count: OHashedStringsIndex; {$IFDEF O_INLINE}inline;{$ENDIF}
    procedure Clear;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;

    property Keys[const aIndex: OHashedStringsIndex]: OWideString read GetKey;
    property Values[const aIndex: OHashedStringsIndex]: OWideString read GetValue write SetValue;
    property Items[const aKey: OWideString]: OWideString read GetValueOfKey write SetValueOfKey; default;
    property Pairs[const aIndex: OHashedStringsIndex]: TOHashedStringDictionaryPair read GetPair;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TOHashedStringDictionaryEnum;
    {$ENDIF}
  end;

  TOHashedStringDictionaryEnum = class(TObject)
  private
    fIndex: OHashedStringsIndex;
    fDictionary: TOHashedStringDictionary;
  public
    constructor Create(aDictionary: TOHashedStringDictionary);
    function GetCurrent: TOHashedStringDictionaryPair;
    function MoveNext: Boolean;
  public
    property Current: TOHashedStringDictionaryPair read GetCurrent;
  end;

  POVirtualHashItem = ^TOVirtualHashItem;
  TOVirtualHashItem = packed record
    fNext: POVirtualHashItem;
    fStringIndex: OStringIndex;
    fIndex: OHashedStringsIndex;
  end;

  {$IFDEF O_ANONYMOUS_METHODS}
  TOVirtualHashIndexGetStringProc = reference to function(const aStringIndex: OStringIndex): OWideString;
  TOVirtualHashIndexSameStringProc = reference to function(const aString1Index: OStringIndex;
    const aString2: OWideString; const aCaseSensitive: Boolean): Boolean;
  {$ELSE}
  TOVirtualHashIndexGetStringProc = function(const aStringIndex: OStringIndex): OWideString of Object;
  TOVirtualHashIndexSameStringProc = function(const aString1Index: OStringIndex;
    const aString2: OWideString; const aCaseSensitive: Boolean): Boolean of Object;
  {$ENDIF}

  TOVirtualHashedStrings = class(TPersistent)
  private
    fCaseSensitive: Boolean;
    fItems: array of POVirtualHashItem;//array indexed by index
    fNextItemId: OHashedStringsIndex;//next list index to use
    fItemLength: OHashedStringsIndex;//count of allocated items in fList
    fMaxItemsBeforeGrowBuckets: OHashedStringsIndex;
    fBuckets: array of POVirtualHashItem;//array indexed by hash
    fLastHashI: OHashedStringsIndex;
    fBlockDelete: Integer;//speeds up multiple delete() calls

    fOnGetString: TOVirtualHashIndexGetStringProc;
    fOnSameString: TOVirtualHashIndexSameStringProc;

    function DefOnSameString(const aString1Index: OStringIndex;
      const aString2: OWideString; const aCaseSensitive: Boolean): Boolean;

    procedure SetCaseSensitive(const aCaseSensitive: Boolean);
  protected
    function Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POVirtualHashItem;//aKey must be already processed with LowerCaseIfNotCaseSensitive
    procedure AddItem(const aItem: POVirtualHashItem; const aHash: OHashedStringsIndex);

    procedure GrowBuckets;
    procedure ClearBuckets;
  public
    constructor Create(
      const aOnGetString: TOVirtualHashIndexGetStringProc;
      const aOnSameString: TOVirtualHashIndexSameStringProc = nil);
    destructor Destroy; override;
  public
    function HashedIndexOf(const aText: OWideString): OHashedStringsIndex;
    function StringIndexOf(const aText: OWideString): OStringIndex;
    function Get(const aIndex: OHashedStringsIndex): OStringIndex;
    function GetItem(const aIndex: OHashedStringsIndex): POVirtualHashItem;

    function Add(const aStringIndex: OStringIndex): OHashedStringsIndex; overload;
    function Add(const aStringIndex: OStringIndex; var outNewEntry: Boolean): OHashedStringsIndex; overload;
    function DeleteByString(const aText: OWideString; const aDecStringIndex: Boolean = True): Boolean;
    function DeleteByStringIndex(const aStringIndex: OStringIndex; const aDecStringIndex: Boolean = True): Boolean;
    procedure DeleteByHashIndex(const aIndex: OHashedStringsIndex; const aDecStringIndex: Boolean = True);
    procedure Clear(const aFullClear: Boolean = True);
    procedure BeginDelete;//speeds up multiple delete calls in a row
    procedure EndDelete;
    procedure EndDeleteForce;

    property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive;
    property StringIndex[const aIndex: OHashedStringsIndex]: OStringIndex read Get; default;
    property Count: OHashedStringsIndex read fNextItemId;
  end;

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}
function HashOf(const aKey: OWideString): Cardinal; {$IFDEF O_INLINE}inline;{$ENDIF}
function HashOfFast(const aKey: OFastString): Cardinal; {$IFDEF O_INLINE}inline;{$ENDIF}
function LowerCaseIfNotCaseSensitive(const aText: OWideString; const aCaseSensitive: Boolean): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function FastLowerCaseIfNotCaseSensitive(const aText: OFastString; const aCaseSensitive: Boolean): OFastString; {$IFDEF O_INLINE}inline;{$ENDIF}

const
  OHASHEDSTRINGSINDEX_UNASSIGNED = -1;

implementation

uses
  OXmlLng
  {$IFDEF FPC}, LazUTF8{$ENDIF};

function LowerCaseIfNotCaseSensitive(const aText: OWideString; const aCaseSensitive: Boolean): OWideString;
begin
  if aCaseSensitive then
    Result := aText
  else
    Result := OLowerCase(aText);
end;

function FastLowerCaseIfNotCaseSensitive(const aText: OFastString; const aCaseSensitive: Boolean): OFastString;
begin
  if aCaseSensitive then
    Result := aText
  else
    Result := OFastLowerCase(aText);
end;

function OHashedStringsIndexAssigned(const aId: OHashedStringsIndex): Boolean;{$IFDEF O_INLINE}inline;{$ENDIF}
begin
  Result := aId <> OHASHEDSTRINGSINDEX_UNASSIGNED;
end;

function HashOf(const aKey: OWideString): Cardinal;
{$IFDEF O_UNICODE}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(aKey) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(aKey[I]);
{$ELSE}
var
  I, xLen: Integer;
  xK: PAnsiChar;
begin
  Result := 0;
  xLen := Length(aKey);
  if xLen > 0 then
  begin
    xK := @aKey[1];
    for I := 1 to xLen*2 do
    begin
      Result := ((Result shl 2) or (Result shr (SizeOf(Result)*8 - 2))) xor
        Ord(xK^);
      Inc(xK);
    end;
  end;
{$ENDIF}
end;

function HashOfFast(const aKey: OFastString): Cardinal;
{$IFDEF O_UNICODE}
begin
  Result := HashOf(aKey);
{$ELSE}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(aKey) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(aKey[I]);
{$ENDIF}
end;


{ TOHashedStringObjDictionary }

function TOHashedStringObjDictionary.Add(const aText: OWideString;
  var outNewEntry: Boolean): OHashedStringsIndex;
begin
  Result := inherited Add(aText, outNewEntry);

  while fObjects.Count < Count do
    fObjects.Add(nil);
end;

function TOHashedStringObjDictionary.AddObject(const aText: OWideString;
  const aObject: TObject): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := AddObject(aText, aObject, x{%H-});
end;

function TOHashedStringObjDictionary.AddObject(const aText: OWideString;
  const aObject: TObject; var outNewEntry: Boolean): OHashedStringsIndex;
begin
  Result := Add(aText, outNewEntry);
  SetObject(Result, aObject);
end;

procedure TOHashedStringObjDictionary.Clear(const aFullClear: Boolean);
var
  I: Integer;
begin
  {$IFNDEF O_ARC}
  if fOwnsObjects then
  {$ENDIF}
  for I := 0 to Count-1 do
  begin
    SetObject(I, nil);
  end;

  inherited Clear(aFullClear);

  if aFullClear then
    fObjects.Clear;
end;

constructor TOHashedStringObjDictionary.Create;
begin
  inherited Create;

  {$IFDEF O_GENERICS}
  fObjects := TList<Pointer>.Create;
  {$ELSE}
  fObjects := TList.Create;
  {$ENDIF}
end;

procedure TOHashedStringObjDictionary.Delete(const aIndex: OHashedStringsIndex);
begin
  {$IFNDEF O_ARC}
  if fOwnsObjects then
  {$ENDIF}
  begin
    SetObject(aIndex, nil);//free or nil when ARC or OwnsObjects!
  end;

  inherited Delete(aIndex);
end;

destructor TOHashedStringObjDictionary.Destroy;
begin
  inherited Destroy;

  fObjects.Free;//must be after destroy -> fObjects is used in Clear!
end;

function TOHashedStringObjDictionary.GetObject(const aIndex: OHashedStringsIndex
  ): TObject;
begin
  Result := TObject(fObjects[aIndex]);
end;

function TOHashedStringObjDictionary.GetPObject(
  const aIndex: OHashedStringsIndex): Pointer;
begin
  Result := fObjects[aIndex];
end;

procedure TOHashedStringObjDictionary.SetObject(
  const aIndex: OHashedStringsIndex; const aObject: TObject);
begin
  {$IFDEF O_ARC}
  if Assigned(fObjects[aIndex]) then
    TObject(fObjects[aIndex]).__ObjRelease;//release last object
  if Assigned(aObject) then
    aObject.__ObjAddRef;//increment reference count of new object
  {$ELSE}
  if fOwnsObjects and Assigned(fObjects[aIndex]) then
    TObject(fObjects[aIndex]).Free;
  {$ENDIF}
  fObjects[aIndex] := aObject;
end;

procedure TOHashedStringObjDictionary.SetPObject(
  const aIndex: OHashedStringsIndex; const aObject: Pointer);
begin
  fObjects[aIndex] := aObject;
end;

{ TOHashedStringDictionary }

{$IFDEF O_ENUMERATORS}
function TOHashedStringDictionary.GetEnumerator: TOHashedStringDictionaryEnum;
begin
  Result := TOHashedStringDictionaryEnum.Create(Self);
end;
{$ENDIF}

function TOHashedStringDictionary.GetKey(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fKeys.Get(aIndex);
end;

function TOHashedStringDictionary.GetPair(
  const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair;
begin
  Result.Key := Keys[aIndex];
  Result.Value := Values[aIndex];
end;

function TOHashedStringDictionary.GetValue(const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fValues[aIndex];
end;

function TOHashedStringDictionary.GetValueOfKey(
  const aKey: OWideString): OWideString;
begin
  if not TryGetValue(aKey, Result{%H-}) then
    Result := '';
end;

function TOHashedStringDictionary.IndexOf(
  const aKey: OWideString): OHashedStringsIndex;
begin
  Result := fKeys.IndexOf(aKey);
end;

procedure TOHashedStringDictionary.SetCaseSensitive(aCaseSensitive: Boolean);
begin
  fKeys.CaseSensitive := aCaseSensitive;
end;

procedure TOHashedStringDictionary.SetValue(const aIndex: OHashedStringsIndex;
  const aValue: OWideString);
begin
  fValues[aIndex] := aValue;
end;

procedure TOHashedStringDictionary.SetValueOfKey(const aKey,
  aValue: OWideString);
begin
  Add(aKey, aValue);
end;

constructor TOHashedStringDictionary.Create;
begin
  inherited Create;

  fKeys := TOHashedStrings.Create;
  fValues := TOWideStringList.Create;
end;

destructor TOHashedStringDictionary.Destroy;
begin
  fKeys.Free;
  fValues.Free;

  inherited Destroy;
end;

function TOHashedStringDictionary.GetCaseSensitive: Boolean;
begin
  Result := fKeys.CaseSensitive;
end;

function TOHashedStringDictionary.Add(const aKey, aValue: OWideString
  ): OHashedStringsIndex;
var
  xNew: Boolean;
begin
  Result := fKeys.Add(aKey, xNew{%H-});
  if xNew then
    fValues.Add(aValue)
  else
    fValues[Result] := aValue;
end;

function TOHashedStringDictionary.TryGetValue(const aKey: OWideString;
  var outValue: OWideString): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := fKeys.IndexOf(aKey);
  Result := (xIndex >= 0);
  if Result then
    outValue := fValues[xIndex]
  else
    outValue := '';
end;

function TOHashedStringDictionary.Count: OHashedStringsIndex;
begin
  Result := fKeys.Count;
end;

procedure TOHashedStringDictionary.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStringDictionary;
begin
  if Dest is TOHashedStringDictionary then
  begin
    xDest := TOHashedStringDictionary(Dest);

    xDest.fKeys.Assign(Self.fKeys);
    xDest.fValues.Assign(Self.fValues);
  end else
    inherited;
end;

procedure TOHashedStringDictionary.Clear;
begin
  if fKeys.Count > 0 then
  begin
    fValues.Clear;
    fKeys.Clear(True);
  end;
end;

{ TOHashedStringDictionaryEnum }

constructor TOHashedStringDictionaryEnum.Create(
  aDictionary: TOHashedStringDictionary);
begin
  inherited Create;

  fIndex := -1;
  fDictionary := aDictionary;
end;

function TOHashedStringDictionaryEnum.GetCurrent: TOHashedStringDictionaryPair;
begin
  Result := fDictionary.Pairs[fIndex];
end;

function TOHashedStringDictionaryEnum.MoveNext: Boolean;
begin
  Result := (fIndex < fDictionary.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TOHashedStrings }

function TOHashedStrings.Add(const aText: OWideString): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := Add(aText, x{%H-});
end;

function TOHashedStrings.Add(const aText: OWideString;
  var outNewEntry: Boolean): OHashedStringsIndex;
var
  xBucket: POHashItem;
  xHash: OHashedStringsIndex;
  xTextCase: OWideString;
begin
  if fBlockDelete > 0 then
    EndDeleteForce;

  xTextCase := LowerCaseIfNotCaseSensitive(aText, fCaseSensitive);
  xBucket := Find(xTextCase, xHash{%H-});
  if Assigned(xBucket) then
  begin
    Result := xBucket.fIndex;
    outNewEntry := False;
    Exit;
  end;

  if fNextItemId = fMaxItemsBeforeGrowBuckets then
  begin
    GrowBuckets;
    xHash := HashOf(xTextCase) mod Cardinal(Length(fBuckets));//must be here!!! -> the hash is changed!!!
  end;

  if fNextItemId = fItemLength then
  begin
    New(fItems[fNextItemId]);
    Inc(fItemLength);
  end;

  xBucket := fItems[fNextItemId];
  {$IFDEF O_UNICODE}
  xBucket.fTextFast := aText;
  {$ELSE}
  OWideToFast(aText, xBucket.fTextFast);
  {$ENDIF}
  xBucket.fIndex := fNextItemId;
  Result := fNextItemId;

  AddItem(xBucket, xHash);

  Inc(fNextItemId);

  outNewEntry := True;
end;

procedure TOHashedStrings.AddItem(const aItem: POHashItem;
  const aHash: OHashedStringsIndex);
begin
  aItem.fNext := fBuckets[aHash];
  fBuckets[aHash] := aItem;
end;

procedure TOHashedStrings.AssignTo(Dest: TPersistent);
var
  xDest: TOHashedStrings;
  I: Integer;
  x: Boolean;
begin
  if Dest is TOHashedStrings then
  begin
    xDest := TOHashedStrings(Dest);

    xDest.Clear(False);
    while xDest.fMaxItemsBeforeGrowBuckets < Self.Count do
      xDest.GrowBuckets;

    for I := 0 to Self.Count-1 do
      xDest.Add(Self.fItems[I].Text, x{%H-});
  end else
    inherited;
end;

procedure TOHashedStrings.BeginDelete;
begin
  Inc(fBlockDelete);
end;

procedure TOHashedStrings.Clear(const aFullClear: Boolean);
var
  I: OHashedStringsIndex;
begin
  fNextItemId := 0;

  if aFullClear then
  begin
    for I := 0 to fItemLength-1 do
      Dispose(POHashItem(fItems[I]));
    fItemLength := 0;

    fLastHashI := 0;
    GrowBuckets;
  end else
  begin
    ClearBuckets;
  end;
end;

procedure TOHashedStrings.ClearBuckets;
var
  I: OHashedStringsIndex;
begin
  for I := 0 to Length(fBuckets)-1 do
    fBuckets[I] := nil;
end;

constructor TOHashedStrings.Create;
begin
  inherited Create;

  fCaseSensitive := True;

  GrowBuckets;
end;

procedure TOHashedStrings.Delete(const aIndex: OHashedStringsIndex);
var
  xBucket, xRunner, xPrevious: POHashItem;
  xHash: OHashedStringsIndex;
  I: OHashedStringsIndex;
begin
  xBucket := GetItem(aIndex);

  //move current bucket to last position
  if aIndex < High(fItems) then
  begin
    System.Move(fItems[aIndex+1], fItems[aIndex], (Count-aIndex)*SizeOf(POHashItem));
    fItems[Count-1] := xBucket;
  end;
  Dec(fNextItemId);
  if fBlockDelete = 0 then
    for I := aIndex to Count-1 do
      Dec(fItems[I].fIndex);

  //remove from hash table
  xHash := HashOfFast(FastLowerCaseIfNotCaseSensitive(xBucket.TextFast, fCaseSensitive)) mod Cardinal(Length(fBuckets));
  xRunner := fBuckets[xHash];
  xPrevious := nil;
  while (xRunner <> nil) and (xRunner <> xBucket) do
  begin
    xPrevious := xRunner;
    xRunner := xRunner.fNext;
  end;
  Assert(xRunner = xBucket);
  if Assigned(xPrevious) then
    xPrevious.fNext := xBucket.fNext
  else
    fBuckets[xHash] := xBucket.fNext;
end;

function TOHashedStrings.Delete(const aText: OWideString): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := IndexOf(aText);
  Result := xIndex >= 0;
  if Result then
    Delete(xIndex);
end;

destructor TOHashedStrings.Destroy;
begin
  Clear(True);

  inherited;
end;

procedure TOHashedStrings.EndDelete;
var
  I: Integer;
begin
  if fBlockDelete > 0 then
    Dec(fBlockDelete);

  if fBlockDelete = 0 then
    for I := 0 to Count-1 do
      fItems[I].fIndex := I;
end;

procedure TOHashedStrings.EndDeleteForce;
begin
  if fBlockDelete > 0 then
  begin
    fBlockDelete := 1;
    EndDelete;
  end;
end;

function TOHashedStrings.Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POHashItem;
begin
  outHash := HashOf(aKey) mod Cardinal(Length(fBuckets));
  Result := fBuckets[outHash];
  while Result <> nil do
  begin
    if Result.SameText(aKey, fCaseSensitive) then
      Exit
    else
      Result := Result.fNext;
  end;
end;

function TOHashedStrings.Get(const aIndex: OHashedStringsIndex): OWideString;
begin
  if (aIndex < 0) or (aIndex >= fNextItemId) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := {$IFNDEF O_UNICODE}OFastToWide{$ENDIF}(fItems[aIndex].fTextFast);
end;

function TOHashedStrings.GetItem(const aIndex: OHashedStringsIndex): POHashItem;
begin
  if (aIndex < 0) or (aIndex >= fNextItemId) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := fItems[aIndex];
end;

const
  cHashTable: array[0..27] of LongWord =
  ( 53,         97,         193,       389,       769,
    1543,       3079,       6151,      12289,     24593,
    49157,      98317,      196613,    393241,    786433,
    1572869,    3145739,    6291469,   12582917,  25165843,
    50331653,   100663319,  201326611, 402653189, 805306457,
    1610612741, 3221225473, 4294967291 );

procedure TOHashedStrings.GrowBuckets;
var
  I: OHashedStringsIndex;
  xTableSize: LongWord;
begin
  ClearBuckets;

  xTableSize := cHashTable[fLastHashI];

  SetLength(fBuckets, xTableSize);
  fMaxItemsBeforeGrowBuckets := (xTableSize * 2) div 3;

  SetLength(fItems, fMaxItemsBeforeGrowBuckets);

  for I := 0 to fNextItemId-1 do
    AddItem(fItems[I], HashOfFast(FastLowerCaseIfNotCaseSensitive(fItems[I].fTextFast, fCaseSensitive)) mod Cardinal(Length(fBuckets)));

  Inc(fLastHashI);
end;

function TOHashedStrings.IndexOf(
  const aText: OWideString): OHashedStringsIndex;
var
  xP: POHashItem;
  xH: OHashedStringsIndex;
begin
  if fBlockDelete > 0 then
    EndDeleteForce;

  xP := Find(LowerCaseIfNotCaseSensitive(aText, fCaseSensitive), xH{%H-});
  if xP <> nil then
    Result := xP.fIndex
  else
    Result := -1;
end;

procedure TOHashedStrings.SetCaseSensitive(const aCaseSensitive: Boolean);
begin
  if fCaseSensitive = aCaseSensitive then
    Exit;

  if fNextItemId > 0 then
    raise Exception.Create('TOHashedStrings: cannot set CaseSensitive to a non-empty list.');

  fCaseSensitive := aCaseSensitive;
end;

{ TOHashItem }

{$IFNDEF O_UNICODE}
function TOHashItem.GetText: OWideString;
begin
  Result := OFastToWide(fTextFast);
end;
{$ENDIF}

function TOHashItem.SameText(const aTextWithCase: OWideString;
  const aCaseSensitive: Boolean): Boolean;
{$IFNDEF O_UNICODE}
var
  xTextFastCase: OFastString;
{$ENDIF}
begin
  {$IFDEF O_UNICODE}
  Result := (FastLowerCaseIfNotCaseSensitive(fTextFast, aCaseSensitive) = aTextWithCase);
  {$ELSE}
  Result := (Length(fTextFast) = Length(aTextWithCase)*SizeOf(OWideChar));
  if Result and (fTextFast <> '') then
  begin
    xTextFastCase := FastLowerCaseIfNotCaseSensitive(fTextFast, aCaseSensitive);
    Result := CompareMem(@xTextFastCase[1], @aTextWithCase[1], Length(xTextFastCase));
  end;
  {$ENDIF}
end;

{ TOVirtualHashedStrings }

function TOVirtualHashedStrings.Add(const aStringIndex: OStringIndex
  ): OHashedStringsIndex;
var
  x: Boolean;
begin
  Result := Add(aStringIndex, x{%H-});
end;

function TOVirtualHashedStrings.Add(const aStringIndex: OStringIndex;
  var outNewEntry: Boolean): OHashedStringsIndex;
var
  xBucket: POVirtualHashItem;
  xHash: OHashedStringsIndex;
  xTextCase: OWideString;
begin
  if fBlockDelete > 0 then
    EndDeleteForce;

  xTextCase := LowerCaseIfNotCaseSensitive(fOnGetString(aStringIndex), fCaseSensitive);
  xBucket := Find(xTextCase, xHash{%H-});
  if Assigned(xBucket) then
  begin
    Result := xBucket.fIndex;
    outNewEntry := False;
    Exit;
  end;

  if fNextItemId = fMaxItemsBeforeGrowBuckets then
  begin
    GrowBuckets;
    xHash := HashOf(xTextCase) mod Cardinal(Length(fBuckets));//must be here!!! -> the hash is changed!!!
  end;

  if fNextItemId = fItemLength then
  begin
    New(fItems[fNextItemId]);
    Inc(fItemLength);
  end;

  xBucket := fItems[fNextItemId];
  xBucket.fStringIndex := aStringIndex;
  xBucket.fIndex := fNextItemId;
  Result := fNextItemId;

  AddItem(xBucket, xHash);

  Inc(fNextItemId);

  outNewEntry := True;
end;

procedure TOVirtualHashedStrings.AddItem(const aItem: POVirtualHashItem;
  const aHash: OHashedStringsIndex);
begin
  aItem.fNext := fBuckets[aHash];
  fBuckets[aHash] := aItem;
end;

procedure TOVirtualHashedStrings.BeginDelete;
begin
  Inc(fBlockDelete);
end;

procedure TOVirtualHashedStrings.Clear(const aFullClear: Boolean);
var
  I: OHashedStringsIndex;
begin
  fNextItemId := 0;

  if aFullClear then
  begin
    for I := 0 to fItemLength-1 do
      Dispose(POVirtualHashItem(fItems[I]));
    fItemLength := 0;

    fLastHashI := 0;
    GrowBuckets;
  end else
  begin
    ClearBuckets;
  end;
end;

procedure TOVirtualHashedStrings.ClearBuckets;
var
  I: OHashedStringsIndex;
begin
  for I := 0 to Length(fBuckets)-1 do
    fBuckets[I] := nil;
end;

constructor TOVirtualHashedStrings.Create(
  const aOnGetString: TOVirtualHashIndexGetStringProc;
  const aOnSameString: TOVirtualHashIndexSameStringProc);
begin
  inherited Create;

  Assert(Assigned(aOnGetString));
  fOnGetString := aOnGetString;
  if Assigned(aOnSameString) then
    fOnSameString := aOnSameString
  else
    fOnSameString := DefOnSameString;

  fCaseSensitive := True;

  GrowBuckets;
end;

function TOVirtualHashedStrings.DefOnSameString(
  const aString1Index: OStringIndex; const aString2: OWideString;
  const aCaseSensitive: Boolean): Boolean;
begin
  Result := (LowerCaseIfNotCaseSensitive(fOnGetString(aString1Index), aCaseSensitive) = aString2);//aString2 must already be in lowercase!
end;

function TOVirtualHashedStrings.DeleteByStringIndex(
  const aStringIndex: OStringIndex; const aDecStringIndex: Boolean): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := HashedIndexOf(fOnGetString(aStringIndex));
  Result := xIndex >= 0;
  if Result then
    DeleteByHashIndex(xIndex, aDecStringIndex);
end;

procedure TOVirtualHashedStrings.DeleteByHashIndex(
  const aIndex: OHashedStringsIndex; const aDecStringIndex: Boolean);
var
  xBucket, xRunner, xPrevious: POVirtualHashItem;
  xHash: OHashedStringsIndex;
  I: OHashedStringsIndex;
begin
  xBucket := GetItem(aIndex);

  //move current bucket to last position
  if aIndex < High(fItems) then
  begin
    System.Move(fItems[aIndex+1], fItems[aIndex], (Count-aIndex)*SizeOf(POVirtualHashItem));
    fItems[Count-1] := xBucket;
  end;
  Dec(fNextItemId);
  if fBlockDelete = 0 then
    for I := aIndex to Count-1 do
      Dec(fItems[I].fIndex);

  //if string was deleted from original string container, decrement string indizes
  if aDecStringIndex then
    for I := 0 to Count-1 do
      if fItems[I].fStringIndex > xBucket.fStringIndex then
        Dec(fItems[I].fStringIndex);

  //remove from hash table
  xHash := HashOf(LowerCaseIfNotCaseSensitive(fOnGetString(xBucket.fStringIndex), fCaseSensitive)) mod Cardinal(Length(fBuckets));
  xRunner := fBuckets[xHash];
  xPrevious := nil;
  while (xRunner <> nil) and (xRunner <> xBucket) do
  begin
    xPrevious := xRunner;
    xRunner := xRunner.fNext;
  end;
  Assert(xRunner = xBucket);
  if Assigned(xPrevious) then
    xPrevious.fNext := xBucket.fNext
  else
    fBuckets[xHash] := xBucket.fNext;
end;

function TOVirtualHashedStrings.DeleteByString(const aText: OWideString;
  const aDecStringIndex: Boolean): Boolean;
var
  xIndex: OHashedStringsIndex;
begin
  xIndex := HashedIndexOf(aText);
  Result := xIndex >= 0;
  if Result then
    DeleteByHashIndex(xIndex, aDecStringIndex);
end;

destructor TOVirtualHashedStrings.Destroy;
begin
  Clear(True);

  inherited;
end;

procedure TOVirtualHashedStrings.EndDelete;
var
  I: Integer;
begin
  if fBlockDelete > 0 then
    Dec(fBlockDelete);

  if fBlockDelete = 0 then
    for I := 0 to Count-1 do
      fItems[I].fIndex := I;
end;

procedure TOVirtualHashedStrings.EndDeleteForce;
begin
  if fBlockDelete > 0 then
  begin
    fBlockDelete := 1;
    EndDelete;
  end;
end;

function TOVirtualHashedStrings.Find(const aKey: OWideString; var outHash: OHashedStringsIndex): POVirtualHashItem;
begin
  outHash := HashOf(aKey) mod Cardinal(Length(fBuckets));
  Result := fBuckets[outHash];
  while Result <> nil do
  begin
    if fOnSameString(Result.fStringIndex, aKey, fCaseSensitive) then
      Exit
    else
      Result := Result.fNext;
  end;
end;

function TOVirtualHashedStrings.Get(const aIndex: OHashedStringsIndex): OStringIndex;
begin
  if (aIndex < 0) or (aIndex >= fNextItemId) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := fItems[aIndex].fStringIndex;
end;

function TOVirtualHashedStrings.GetItem(const aIndex: OHashedStringsIndex): POVirtualHashItem;
begin
  if (aIndex < 0) or (aIndex >= fNextItemId) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := fItems[aIndex];
end;

procedure TOVirtualHashedStrings.GrowBuckets;
var
  I: OHashedStringsIndex;
  xTableSize: LongWord;
begin
  ClearBuckets;

  xTableSize := cHashTable[fLastHashI];

  SetLength(fBuckets, xTableSize);
  fMaxItemsBeforeGrowBuckets := (xTableSize * 2) div 3;

  SetLength(fItems, fMaxItemsBeforeGrowBuckets);

  for I := 0 to fNextItemId-1 do
    AddItem(fItems[I], HashOf(LowerCaseIfNotCaseSensitive(fOnGetString(fItems[I].fStringIndex), fCaseSensitive)) mod Cardinal(Length(fBuckets)));

  Inc(fLastHashI);
end;

function TOVirtualHashedStrings.HashedIndexOf(const aText: OWideString
  ): OHashedStringsIndex;
var
  xP: POVirtualHashItem;
  xH: OHashedStringsIndex;
begin
  if fBlockDelete > 0 then
    EndDeleteForce;

  xP := Find(LowerCaseIfNotCaseSensitive(aText, fCaseSensitive), xH{%H-});
  if xP <> nil then
    Result := xP.fIndex
  else
    Result := -1;
end;

function TOVirtualHashedStrings.StringIndexOf(
  const aText: OWideString): OStringIndex;
var
  xP: POVirtualHashItem;
  xH: OHashedStringsIndex;
begin
  if fBlockDelete > 0 then
    EndDeleteForce;

  xP := Find(LowerCaseIfNotCaseSensitive(aText, fCaseSensitive), xH{%H-});
  if xP <> nil then
    Result := xP.fStringIndex
  else
    Result := -1;
end;

procedure TOVirtualHashedStrings.SetCaseSensitive(const aCaseSensitive: Boolean);
begin
  if fCaseSensitive = aCaseSensitive then
    Exit;

  if fNextItemId > 0 then
    raise Exception.Create('TOVirtualHashedStrings: cannot set CaseSensitive to a non-empty list.');

  fCaseSensitive := aCaseSensitive;
end;

end.

