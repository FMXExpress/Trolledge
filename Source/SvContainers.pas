(*
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit SvContainers;

interface

uses
  Classes, Generics.Collections, SysUtils;

const
  // BucketSize determines max length of the list. Very big|small values decrease performance, while
  // the optimum value in range 4..16.
  BucketSize = 8;

type

  THashLinkedItem<T> = class(TObject)
  private
    FValue: Cardinal;
    FData: T;
    FNext: THashLinkedItem<T>;
  public
    constructor Create(Value: Cardinal; Data: T; Next: THashLinkedItem<T>);
    destructor Destroy; override;
  end;

 // THashTrie = class; //forward declaration
  TSvStringTrieIterateRef<T> = reference to procedure(const AKey: string; const AData: T;
    var Abort: Boolean);
  TSvFreeItemEvent<T> = procedure(Sender: TObject; const S: string; const Data: T) of object;

  THashTreeItem<T> = class(TObject)
  private
    FOwner: TObject;
    FLevel: Integer;
    FFilled: Integer;
    FItems: array of TObject; // This will be at most LeafSize entries.
  protected
    procedure AddDown(Value, Hash: Cardinal; const Data: T);
    procedure Delete(Value, Hash: Cardinal);
    function Find(Value, Hash: Cardinal; out Data: T): Boolean; overload;
    function Find(Value, Hash: Cardinal; out Data: THashLinkedItem<T>): Boolean; overload;
    function FindStartMatch(Value, Hash: Cardinal; out Data: TList<T>): Boolean;
    function GetFilled: Integer;
    function Modify(Value, Hash: Cardinal; const Data: T): Boolean;
    function ROR(Value: Cardinal): Cardinal;
    function RORN(Value: Cardinal; Level: Integer): Cardinal;
    function Traverse(AProc: TSvStringTrieIterateRef<T>): Boolean;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;

    procedure Clear;
  end;

  TLengthStatistics = array[1..BucketSize] of Integer;

  THashTrie<T> = class(TEnumerable<T>)
  private
    FRoot: THashTreeItem<T>;
    FOwnsObjects: Boolean;
    FOnFreeItem: TSvFreeItemEvent<T>;
    function GetCount: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    procedure AddDown(Value, Hash: Cardinal; const Data: T);
    function CompareValue(Value1, Value2: Cardinal): Boolean; virtual; abstract;
    procedure Delete(Value, Hash: Cardinal);
    procedure DestroyItem(var Value: Cardinal; var Data: T); virtual; abstract;
    function HashValue(Value: Cardinal): Cardinal; virtual; abstract;
    procedure TreeStat(Item: THashTreeItem<T>; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
      var LengthStatistics: TLengthStatistics);
    function Find(Value, Hash: Cardinal; out Data: T): Boolean; virtual;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    procedure Clear;

    procedure TrieStatistics(var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
      var LengthStatistics: TLengthStatistics);

    property Count: Integer read GetCount;
    property OnFreeItem: TSvFreeItemEvent<T> read FOnFreeItem write FOnFreeItem;
  end;


  ESvStringTrieException = class(Exception);

  TSvStringTrie<T> = class(THashTrie<T>)
  private
    FCaseSensitive: Boolean;
    function GetKeys(const AData: T): string;
    procedure SetKeys(const AData: T; const Value: string);
    function GetValues(const AKey: string): T;
    procedure SetValues(const AKey: string; const Value: T);
  protected
    function HashValue(Value: Cardinal): Cardinal; override;
    procedure DestroyItem(var Value: Cardinal; var Data: T); override;
    function CompareValue(Value1, Value2: Cardinal): Boolean; override;
    function HashStr(const S: string): Cardinal;
  public
    type
      TPairEnumerator = class(TEnumerator<TPair<string, T>>)
      private
        FTrie: TSvStringTrie<T>;
        FIndex: Integer;
        FItems: TArray<TPair<string, T>>;
        function GetCurrent: TPair<string, T>;
      protected
        function DoGetCurrent: TPair<string, T>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ATrie: TSvStringTrie<T>);
        destructor Destroy; override;

        property Current: TPair<string, T> read GetCurrent;
        function MoveNext: Boolean;
      end;

    function GetEnumerator: TPairEnumerator; reintroduce;
  public
    procedure Add(const S: string; const Data: T);
    procedure AddOrSetValue(const S: string; const Data: T);

    function Contains(const Value: T): Boolean;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: T): Boolean;

    function Extract(const AKey: string): T;

    function Remove(const AData: T): string;
    procedure Delete(const S: string);
    function TryGetValue(const S: string; out Data: T): Boolean;
    function TryGetValues(const S: string; out Data: TList<T>): Boolean;

    function IterateOver(AProc: TSvStringTrieIterateRef<T>): Boolean;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Keys[const AData: T]: string read GetKeys write SetKeys;
    property Values[const AKey: string]: T read GetValues write SetValues;
  end;

  TSvStringObjectTrie<T: class> = class(TSvStringTrie<T>)
  protected
    procedure DestroyItem(var Value: Cardinal; var Data: T); override;
  public
    constructor Create(AOwnsObject: Boolean = True); reintroduce;

    property OwnsObjects;
  end;

  function CalcStrCRC32(const S: string): Cardinal;
  function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;
  //procedure TrieStatistics(Trie: THashTrie<T>; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  //  var LengthStatistics: TLengthStatistics);


implementation

uses
  Generics.Defaults;

const
  BufferIncrement = 1024; // Size by which the buffer in the buffered string is incremented
                          // when the current space is exhausted.

  // LeafSize must be 256. No changes allowed.
  LeafSize = 256;

  const
  CRC32_POLYNOMIAL = $EDB88320;

var
  // Dynamic crc32 table.
  CCITT32Table: array of Cardinal;

procedure BuildCRCTable;

var
  i, j: longint;
  value: Cardinal;
begin
  SetLength(CCITT32Table, 256);
  for i := 0 to 255 do begin
    value := i;
    for j := 8 downto 1 do
      if ((value and 1) <> 0) then
        value := (value shr 1) xor CRC32_POLYNOMIAL
      else
        value := value shr 1;
    Ccitt32Table[i] := value;
  end
end;

//----------------------------------------------------------------------------------------------------------------------

function CalcStrCRC32(const S: string): Cardinal;

var
  I: Integer;

begin
  // Create CRC table if not yet done.
  if CCITT32Table = nil then
    BuildCRCTable;

  Result := $FFFFFFFF;
  for I:=1 to Length(S) do
    Result:= (((Result shr 8) and $00FFFFFF) xor (CCITT32Table[(Result xor Byte(S[I])) and $FF]));
end;

//----------------------------------------------------------------------------------------------------------------------

// By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net
//
// If you are hashing n strings (ub1 **)k, do it like this:
//   for (i=0, h=0; i<n; ++i) h = jhash( k[i], len[i], h);

procedure Mix(var A, B, C: Cardinal);

begin
  Dec(A, B); Dec(A, C); A := A xor (C shr 13);
  Dec(B, C); Dec(B, A); B := A xor (A shl 8);
  Dec(C, A); Dec(C, B); C := C xor (B shr 13);
  Dec(A, B); Dec(A, C); A := A xor (C shr 12);
  Dec(B, C); Dec(B, A); B := B xor (A shl 16);
  Dec(C, A); Dec(C, B); C := C xor (B shr 5);
  Dec(A, B); Dec(A, C); A := A xor (C shr 3);
  Dec(B, C); Dec(B, A); B := B xor (A shl 10);
  Dec(C, A); Dec(C, B); C := C xor (B shr 15);
end;

//----------------------------------------------------------------------------------------------------------------------

function JHash(Key: Pointer; Length, InitVal: Cardinal): Cardinal;

// Length: the length of the key.
// InitVal: the previous hash, or an arbitrary value.

var
  A, B, C, Len: Cardinal;
  K: PByteArray;

begin
  // Set up the internal state.
  Len := Length;
  K := Key;
  A := $9E3779B9;  // The golden ratio; an arbitrary value.
  B := $9E3779B9;
  C := InitVal;    // The previous hash value.

  // Handle most of the key.
  while Len >= 12 do
  begin
    Inc(A, K[0] + (Cardinal(K[1]) shl 8) + (Cardinal(K[2]) shl 16) + (Cardinal(K[3]) shl 24));
    Inc(B, K[4] +(Cardinal(K[5]) shl 8) + (Cardinal(K[6]) shl 16) + (Cardinal(K[7]) shl 24));
    Inc(C, K[8] + (Cardinal(K[9]) shl 8) + (Cardinal(K[10]) shl 16) + (Cardinal(K[11]) shl 24));
    Mix(A, B, C);
    Inc(PByte(K), 12);
    Dec(Len, 12);
  end;

   // Handle the last 11 bytes.
  Inc(C, Length);
  if Len >= 11 then
    Inc(C, Cardinal(K[10]) shl 24);
  if Len >= 10 then
    Inc(C, Cardinal(K[9]) shl 16);
  if Len >= 9 then
    Inc(C, Cardinal(K[8]) shl 8);
  if Len >= 8 then
    Inc(B, Cardinal(K[7]) shl 24);
  if Len >= 7 then
    Inc(B, Cardinal(K[6]) shl 16);
  if Len >= 6 then
    Inc(B, Cardinal(K[5]) shl 8);
  if Len >= 5 then
    Inc(B, Cardinal(K[4]));
  if Len >= 4 then
    Inc(A, Cardinal(K[3]) shl 24);
  if Len >= 3 then
    Inc(A, Cardinal(K[2]) shl 16);
  if Len >= 2 then
    Inc(A, Cardinal(K[1]) shl 8);
  if Len >= 1 then
    Inc(A, Cardinal(K[0]));
  // Case 0: nothing left to add.

  Mix(A, B, C);
  Result := C;
end;

{ THashLinkedItem }

constructor THashLinkedItem<T>.Create(Value: Cardinal; Data: T; Next: THashLinkedItem<T>);
begin
  inherited Create;
  FValue := Value;
  FData := Data;
  FNext := Next;
end;

destructor THashLinkedItem<T>.Destroy;
begin
  FNext.Free;
  inherited Destroy;
end;

{ THashTreeItem<T> }

procedure THashTreeItem<T>.AddDown(Value, Hash: Cardinal; const Data: T);
var
  I, J: Integer;
  TreeItem: THashTreeItem<T>;
  LinkedItem: THashLinkedItem<T>;
begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if FItems[I] = nil then
  begin
    FItems[I] := THashLinkedItem<T>.Create(Value, Data, nil);
    Inc(FFilled);
  end
  else
    if FItems[I] is THashTreeItem<T> then
      THashTreeItem<T>(FItems[I]).AddDown(Value, ROR(Hash), Data)
    else
    begin
      J := 0;
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          LinkedItem.FData := Data;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
        Inc(J)
      end;

      if J >= BucketSize then
      begin
        // full
        TreeItem := THashTreeItem<T>.Create(FOwner);
        TreeItem.FLevel := FLevel + 1;
        LinkedItem := THashLinkedItem<T>(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          TreeItem.AddDown(LinkedItem.FValue, RORN(THashTrie<T>(FOwner).HashValue(LinkedItem.FValue), FLevel + 1), LinkedItem.FData);
          LinkedItem := LinkedItem.FNext;
        end;
        TreeItem.AddDown(Value, ROR(Hash), Data);
        THashLinkedItem<T>(FItems[I]).Free;
        FItems[I] := TreeItem;
      end
      else
        FItems[I] := THashLinkedItem<T>.Create(Value, Data, THashLinkedItem<T>(FItems[I]));
    end;
end;

procedure THashTreeItem<T>.Clear;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem<T> then
      THashTreeItem<T>(FItems[I]).Free
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        THashTrie<T>(FOwner).DestroyItem(LinkedItem.FValue, LinkedItem.FData);
        LinkedItem := LinkedItem.FNext;
      end;
      THashLinkedItem<T>(FItems[I]).Free;
    end;
  FItems := nil;
end;

constructor THashTreeItem<T>.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure THashTreeItem<T>.Delete(Value, Hash: Cardinal);
var
  I: Integer;
  PrevLinkedItem,
  LinkedItem: THashLinkedItem<T>;

begin
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[i] is THashTreeItem<T> then
    begin
      THashTreeItem<T>(FItems[I]).Delete(Value, ROR(Hash));
      if THashTreeItem<T>(FItems[I]).FFilled = 0 then
      begin
        THashTreeItem<T>(FItems[I]).Free;
        FItems[I] := nil;
      end;
    end
    else
    begin
      PrevLinkedItem := nil;
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          if PrevLinkedItem = nil then
          begin
            FItems[I] := LinkedItem.FNext;
            if FItems[I] = nil then
              Dec(FFilled);
          end
          else
            PrevLinkedItem.FNext := LinkedItem.FNext;
          LinkedItem.FNext := nil;
          THashTrie<T>(FOwner).DestroyItem(LinkedItem.FValue, LinkedItem.FData);
          LinkedItem.Free;
          Exit;
        end;
        PrevLinkedItem := LinkedItem;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

destructor THashTreeItem<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function THashTreeItem<T>.Find(Value, Hash: Cardinal; out Data: THashLinkedItem<T>): Boolean;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := False;
  I := Hash and $FF;
  if High(FItems) < I then
    SetLength(FItems, I + 1);
  if Assigned(FItems[I]) then
  begin
    if FItems[I] is THashTreeItem<T> then
      Result := THashTreeItem<T>(FItems[I]).Find(Value, ROR(Hash), Data)
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        if THashTrie<T>(FOwner).CompareValue(LinkedItem.FValue, Value) then
        begin
          // found
          Data := LinkedItem;
          Result := True;
          Exit;
        end;
        LinkedItem := LinkedItem.FNext;
      end;
    end;
  end;
end;

function THashTreeItem<T>.FindStartMatch(Value, Hash: Cardinal; out Data: TList<T>): Boolean;
var
  LinkedItem: THashLinkedItem<T>;
begin
  Result := Find(Value, Hash, LinkedItem);
  if Result then
  begin
    Data := TList<T>.Create;

    while Assigned(LinkedItem) do
    begin
      Data.Add(LinkedItem.FData);

      LinkedItem := LinkedItem.FNext;
    end;
  end;
end;

function THashTreeItem<T>.Find(Value, Hash: Cardinal; out Data: T): Boolean;
var
  LinkedItem: THashLinkedItem<T>;
begin
  Result := Find(Value, Hash, LinkedItem);
  if Result then
  begin
    Data := LinkedItem.FData;
  end;
end;

function THashTreeItem<T>.GetFilled: Integer;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := 0;
  for I := 0 to High(FItems) do
    if FItems[I] is THashTreeItem<T> then
      Inc(Result, THashTreeItem<T>(FItems[I]).GetFilled)
    else
    begin
      LinkedItem := THashLinkedItem<T>(FItems[I]);
      while Assigned(LinkedItem) do
      begin
        Inc(Result);
        LinkedItem := LinkedItem.FNext;
      end;
    end;
end;

function THashTreeItem<T>.Modify(Value, Hash: Cardinal; const Data: T): Boolean;
var
  LinkedItem: THashLinkedItem<T>;
begin
  Result := Find(Value, Hash, LinkedItem);
  if Result then
  begin
    LinkedItem.FData := Data;
  end;
end;

function THashTreeItem<T>.ROR(Value: Cardinal): Cardinal;
begin
  Result := ((Value and $FF) shl 24) or ((Value shr 8) and $FFFFFF);
end;

function THashTreeItem<T>.RORN(Value: Cardinal; Level: Integer): Cardinal;
begin
  Result := Value;
  while Level > 0 do
  begin
    Result := ROR(Result);
    Dec(Level);
  end;
end;


function THashTreeItem<T>.Traverse(AProc: TSvStringTrieIterateRef<T>): Boolean;
var
  I: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Result := False;
  for I := Low(FItems) to High(FItems) do
  begin
    if Assigned(FItems[I]) then
    begin
      if FItems[I] is THashTreeItem<T> then
        Result := THashTreeItem<T>(FItems[I]).Traverse(AProc)
      else
      begin
        LinkedItem := THashLinkedItem<T>(FItems[I]);
        while Assigned(LinkedItem) do
        begin
          AProc(PChar(LinkedItem.FValue), LinkedItem.FData, Result);

          LinkedItem := LinkedItem.FNext;
        end;
      end;
      if Result then
        Break;
    end;
  end;
end;

{ THashTrie<T> }

procedure THashTrie<T>.AddDown(Value, Hash: Cardinal; const Data: T);
begin
  FRoot.AddDown(Value, Hash, Data);
end;

procedure THashTrie<T>.Clear;
begin
  FRoot.Clear;
end;

constructor THashTrie<T>.Create();
begin
  inherited Create;
  FOnFreeItem := nil;
  FOwnsObjects := False;
  FRoot := THashTreeItem<T>.Create(Self);
end;

procedure THashTrie<T>.Delete(Value, Hash: Cardinal);
begin
  FRoot.Delete(Value, Hash);
end;

destructor THashTrie<T>.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

function THashTrie<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function THashTrie<T>.Find(Value, Hash: Cardinal; out Data: T): Boolean;
begin
  Result := FRoot.Find(Value, Hash, Data);
end;

function THashTrie<T>.GetCount: Integer;
begin
  Result := FRoot.GetFilled;
end;

procedure THashTrie<T>.TreeStat(Item: THashTreeItem<T>; var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
      var LengthStatistics: TLengthStatistics);
var
  I, J: Integer;
  LinkedItem: THashLinkedItem<T>;
begin
  Inc(PeakCount);
  if Item.FLevel + 1 > MaxLevel then
    MaxLevel := Item.FLevel + 1;

  for J := 0 to High(Item.FItems) do
    if Assigned(Item.FItems[J]) then
    begin
      Inc(FillCount);
      if Item.FItems[J] is THashTreeItem<T> then
        TreeStat(THashTreeItem<T>(Item.FItems[J]), MaxLevel, PeakCount, FillCount, EmptyCount, LengthStatistics)
      else
      begin
        I := 0;
        LinkedItem := THashLinkedItem<T>(Item.FItems[J]);
        while Assigned(LinkedItem) do
        begin
          Inc(I);
          LinkedItem := LinkedItem.FNext;
        end;
        Inc(LengthStatistics[I]);
      end;
    end
    else
      Inc(EmptyCount);
end;

procedure THashTrie<T>.TrieStatistics(var MaxLevel, PeakCount, FillCount, EmptyCount: Integer;
  var LengthStatistics: TLengthStatistics);
begin
  MaxLevel := 0;
  PeakCount := 0;
  FillCount := 0;
  EmptyCount := 0;

  if Assigned(FRoot) then
    TreeStat(FRoot, MaxLevel, PeakCount, FillCount, EmptyCount, LengthStatistics);
end;


{ TStringHashTrie<T> }

procedure TSvStringTrie<T>.Add(const S: string; const Data: T);
var
  Value: PChar;
begin
  Value := StrNew(PChar(S));
  AddDown(Cardinal(Value), HashStr(S), Data);
end;

procedure TSvStringTrie<T>.AddOrSetValue(const S: string; const Data: T);
begin
  if ContainsKey(S) then
  begin
    Delete(S);
  end;

  Add(S, Data);
end;

function TSvStringTrie<T>.CompareValue(Value1, Value2: Cardinal): Boolean;
begin
  if FCaseSensitive then
    Result := StrComp(PChar(Value1), PChar(Value2)) = 0
  else
    Result := StrIComp(PChar(Value1), PChar(Value2)) = 0;
end;

function TSvStringTrie<T>.Contains(const Value: T): Boolean;
var
  sRes: string;
begin
  sRes := Keys[Value];
  Result := (sRes <> '');
end;

function TSvStringTrie<T>.ContainsKey(const Key: string): Boolean;
var
  Data: T;
begin
  Result := TryGetValue(Key, Data);
end;

function TSvStringTrie<T>.ContainsValue(const Value: T): Boolean;
begin
  Result := Contains(Value);
end;

procedure TSvStringTrie<T>.Delete(const S: string);
begin
  inherited Delete(Cardinal(PWideChar(S)), HashStr(S));
end;

procedure TSvStringTrie<T>.DestroyItem(var Value: Cardinal; var Data: T);
begin
  if Assigned(FOnFreeItem) then
    FOnFreeItem(Self, PChar(Value), Data);

  StrDispose(PChar(Value));
  Value := 0;
  {DONE -oLinas -cGeneral : add event on free item}

end;

function TSvStringTrie<T>.Extract(const AKey: string): T;
begin
  if TryGetValue(AKey, Result) then
  begin
    Delete(AKey);
  end;
end;

function TSvStringTrie<T>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TSvStringTrie<T>.GetKeys(const AData: T): string;
var
  sRes: string;
begin
  sRes := '';
  IterateOver(procedure(const AKey: string; const ADataVal: T; var Abort: Boolean)
    begin
      Abort := TEqualityComparer<T>.Default.Equals(AData, ADataVal);
      if Abort then
      begin
        sRes := AKey;
      end;
    end);

  Result := sRes;
end;

function TSvStringTrie<T>.GetValues(const AKey: string): T;
begin
  if not TryGetValue(AKey, Result) then
  begin
    raise ESvStringTrieException.Create('Value of a key ' + AKey + ' does not exist');
  end;
end;

function TSvStringTrie<T>.TryGetValue(const S: string; out Data: T): Boolean;
begin
  Result := Find(Cardinal(PChar(S)), HashStr(S), Data);
end;

function TSvStringTrie<T>.TryGetValues(const S: string; out Data: TList<T>): Boolean;
begin
  Result := FRoot.FindStartMatch(Cardinal(PChar(S)), HashStr(S), Data);
end;

function TSvStringTrie<T>.HashStr(const S: string): Cardinal;
begin
  if FCaseSensitive then
    Result := CalcStrCRC32(S)
  else
    Result := CalcStrCRC32(AnsiUpperCase(S));
end;

function TSvStringTrie<T>.HashValue(Value: Cardinal): Cardinal;
begin
  Result := HashStr(PChar(Value));
end;

function TSvStringTrie<T>.IterateOver(AProc: TSvStringTrieIterateRef<T>): Boolean;
begin
  Result := FRoot.Traverse(AProc);
end;

function TSvStringTrie<T>.Remove(const AData: T): string;
begin
  Result := Keys[AData];

  if Result <> '' then
  begin
    Delete(Result);
  end;
end;

procedure TSvStringTrie<T>.SetKeys(const AData: T; const Value: string);
begin
  AddOrSetValue(Value, AData);
end;

procedure TSvStringTrie<T>.SetValues(const AKey: string; const Value: T);
begin
  AddOrSetValue(AKey, Value);
end;

{ TSvStringTrie<T>.TPairEnumerator }

constructor TSvStringTrie<T>.TPairEnumerator.Create(ATrie: TSvStringTrie<T>);
var
  ix: Integer;
begin
  inherited Create;
  FTrie := ATrie;
  FIndex := -1;

  SetLength(FItems, FTrie.Count);
  ix := 0;
  //enumerate
  FTrie.IterateOver(
    procedure(const AKey: string; const AData: T; var Abort: Boolean)
    begin
      FItems[ix].Key := AKey;
      FItems[ix].Value := AData;
      Inc(ix);
    end);
end;

destructor TSvStringTrie<T>.TPairEnumerator.Destroy;
begin
  inherited Destroy;
end;

function TSvStringTrie<T>.TPairEnumerator.DoGetCurrent: TPair<string, T>;
begin
  Result := GetCurrent;
end;

function TSvStringTrie<T>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TSvStringTrie<T>.TPairEnumerator.GetCurrent: TPair<string, T>;
begin
  Result := FItems[FIndex];
end;

function TSvStringTrie<T>.TPairEnumerator.MoveNext: Boolean;
begin
  if FIndex >= Length(FItems) then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < Length(FItems);
end;

{ TSvStringObjectTrie<T> }

constructor TSvStringObjectTrie<T>.Create(AOwnsObject: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObject;
end;

procedure TSvStringObjectTrie<T>.DestroyItem(var Value: Cardinal; var Data: T);
begin
  inherited DestroyItem(Value, Data);
  if FOwnsObjects then
  begin
    Data.Free;
  end;
end;

end.
