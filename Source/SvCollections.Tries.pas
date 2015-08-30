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
unit SvCollections.Tries;

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TAlpha = Byte;

type
  TNodeNotification = (nAdded, nDeleted, nExtracted);

  ESvTrieException = class(Exception);

  TSuffixTrieNode<T> = class;

  TSvStringTrie<T> = class;

  TSuffixTrieNodeArray = array [TAlpha] of TObject;

  TTraverseProc<T> = reference to procedure(const AKey: string; ANode: TSuffixTrieNode<T>);

  { TSuffixTrieNode<T> }

  TSuffixTrieNode<T> = class
  strict private
    FChildren: TSuffixTrieNodeArray;
    FValue: T;
    FOwner: TSvStringTrie<T>;
    FParent: TSuffixTrieNode<T>;
  private
    FLastAdded: Boolean;
  protected
    function GetChildren(const AChar: TAlpha): TSuffixTrieNode<T>; overload; inline;
    function GetChildren(const AChar: Char): TSuffixTrieNode<T>; overload; inline;
  public
    constructor Create(AOwner: TSvStringTrie<T>; AParent: TSuffixTrieNode<T>; const AValue: T;  ALastChar: Boolean = False);
    destructor Destroy; override;

    procedure Add(const P: PChar; const AValue: T; ALastChar: Boolean = False);
    function Find(const P: PChar; out AValue: TSuffixTrieNode<T>): Boolean;
    function FindExact(const P: PChar; out AValue: TSuffixTrieNode<T>): Boolean;
    function Exists(const P: PChar; out AValue: T; AExactMatch: Boolean = False): Boolean; overload;
    function Exists(const P: PChar; out AValue: TSuffixTrieNode<T>; AExactMatch: Boolean = False): Boolean; overload;
    function ToString(const ps: String): String; reintroduce; overload;
    function HasChildren(): Boolean; inline;

    procedure Traverse(const ps: string; AProc: TTraverseProc<T>);

    property Children: TSuffixTrieNodeArray read FChildren;
    property Value: T read FValue;
    property LastCharAdded: Boolean read FLastAdded;
    property Parent: TSuffixTrieNode<T> read FParent;
  end;

  { TSvStringTrie<T> }

  TSvStringTrie<T> = class
  strict private
    FRoot: TSuffixTrieNode<T>;
  private
    FCaseSensitive: Boolean;
    FCount: Integer;
    FSearchFromStart: Boolean;
  protected
    procedure Notification(ANode: TSuffixTrieNode<T>; AAction: TNodeNotification); virtual; abstract;
    function Exists(const S: String; out AValue: T; AExactMatch: Boolean = False): Boolean; overload;
    function Exists(const S: String; out AValue: TSuffixTrieNode<T>; AExactMatch: Boolean = False): Boolean; overload;
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

    function GetEnumerator: TPairEnumerator;
  public
    constructor Create(ASearchFromStartOnly: Boolean = False); virtual;
    destructor Destroy; override;

    procedure Add(const S: String; const AValue: T);

    procedure Remove(const S: string);
    procedure Clear;
    function ContainsKey(const AKey: string; const AExactMatch: Boolean = True): Boolean;

    procedure Traverse(AProc: TTraverseProc<T>);

    function TryGetValue(const S: string; out AValue: T; AExactMatch: Boolean = False): Boolean;
    function TryGetValues(const S: string): TList<T>;

    function ToString: String; override;

    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property SearchFromStart: Boolean read FSearchFromStart;
    property Count: Integer read FCount;
  end;

   { TSvObjectStringTrie<T> }

  TSvObjectStringTrie<T: class> = class(TSvStringTrie<T>)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notification(ANode: TSuffixTrieNode<T>; AAction: TNodeNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True; ASearchFromStartOnly: Boolean = False); reintroduce;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

uses
  StrUtils;

{ TSuffixTrieNode<T> }

constructor TSuffixTrieNode<T>.Create(AOwner: TSvStringTrie<T>; AParent: TSuffixTrieNode<T>; const AValue: T; ALastChar: Boolean);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AParent;
  FValue := AValue;
  FLastAdded := ALastChar;
end;

destructor TSuffixTrieNode<T>.Destroy;
var
  c: TAlpha;
begin
  for c := Low(FChildren) to High(FChildren) do
  begin
    if Assigned(FChildren[c]) then
    begin
      {DONE -oLinas -cGeneral : free only when it is the last node with current value}
      FOwner.Notification(GetChildren(c), nDeleted);
      //FChildren[c].Free;
      FreeAndNil(FChildren[c]);

    end;
  end;
  inherited Destroy;
end;

procedure TSuffixTrieNode<T>.Add(const P: PChar; const AValue: T; ALastChar: Boolean);
begin
  if not Assigned(FChildren[Ord(P^)]) then
  begin
    FChildren[Ord(P^)] := TSuffixTrieNode<T>.Create(FOwner, Self, AValue, (P[1] = #0) and ALastChar);
  end;

  if P[1] <> #0 then
  begin
    GetChildren(P^).Add(@P[1], AValue, ALastChar);
  end
  else
  begin
    if ALastChar then
    begin
      GetChildren(P^).FLastAdded := True;
      GetChildren(P^).FValue := AValue;
    end;
  end;
end;

function TSuffixTrieNode<T>.Exists(const P: PChar; out AValue: T;
  AExactMatch: Boolean): Boolean;
var
  node: TSuffixTrieNode<T>;
begin
  Result := Exists(P, node, AExactMatch);

  if Result then
  begin
    AValue := node.Value;
  end;
end;

function TSuffixTrieNode<T>.Exists(const P: PChar; out AValue: TSuffixTrieNode<T>;
  AExactMatch: Boolean): Boolean;
begin
  if AExactMatch then
    Result := FindExact(P, AValue)
  else
    Result := Find(P, AValue);
end;

function TSuffixTrieNode<T>.Find(const P: PChar; out AValue: TSuffixTrieNode<T>): Boolean;
var
  c: TAlpha;
begin
  Result := Assigned(FChildren[Ord(P^)]);
  if Result then
  begin
    AValue := GetChildren(Ord(P^));
    if (P[1] <> #0) then
    begin
      Result := AValue.Find(@P[1], AValue);
    end;
  end;
end;

function TSuffixTrieNode<T>.FindExact(const P: PChar; out AValue: TSuffixTrieNode<T>): Boolean;
var
  c: TAlpha;
begin
  Result := Assigned(FChildren[Ord(P^)]);
  if Result then
  begin
    AValue := GetChildren(P^);
    if (P[1] <> #0) then
    begin
     // Result := AValue.LastCharAdded;
     // if not Result then
      Result := AValue.FindExact(@P[1], AValue);
    end
    else
    begin
      Result := AValue.LastCharAdded;
    end;
  end;
end;

function TSuffixTrieNode<T>.GetChildren(const AChar: Char): TSuffixTrieNode<T>;
begin
  Result := TSuffixTrieNode<T>(FChildren[Ord(AChar)]);
end;

function TSuffixTrieNode<T>.HasChildren: Boolean;
var
  c: TAlpha;
begin
  for c:= Low(FChildren) to High(FChildren) do
  begin
    if Assigned(FChildren[c]) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TSuffixTrieNode<T>.GetChildren(const AChar: TAlpha): TSuffixTrieNode<T>;
begin
  Result := TSuffixTrieNode<T>(FChildren[AChar]);
end;

function TSuffixTrieNode<T>.ToString(const ps: String): String;
var
  c: TAlpha;
begin
  Result := '';
  for c:= Low(FChildren) to High(FChildren) do
  begin
    if Assigned(FChildren[c]) then // dfs
      Result := Result + GetChildren(c).ToString(ps + Chr(c));
  end;

  if Result = '' then // leaf node
    Result := ps + #13#10;
end;

procedure TSuffixTrieNode<T>.Traverse(const ps: string; AProc: TTraverseProc<T>);
var
  c: TAlpha;
begin
  if FLastAdded then
  begin
    AProc(ps, Self);
  end;

  for c:= Low(FChildren) to High(FChildren) do
  begin
    if Assigned(FChildren[c]) then // dfs
    begin
      GetChildren(c).Traverse(ps + Chr(c), AProc);
    end;
  end;
end;

{ TSvStringTrie<T> }

procedure TSvStringTrie<T>.Clear;
begin
  if Assigned(FRoot) then
  begin
    FRoot.Free;
    FRoot := nil;
  end;

  FCount := 0;
end;

function TSvStringTrie<T>.ContainsKey(const AKey: string; const AExactMatch: Boolean): Boolean;
var
  val: T;
begin
  Result := TryGetValue(AKey, val, AExactMatch);
end;

constructor TSvStringTrie<T>.Create(ASearchFromStartOnly: Boolean);
begin
  inherited Create;
  FRoot := nil;
  FCount := 0;
  FSearchFromStart := ASearchFromStartOnly;
end;

destructor TSvStringTrie<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSvStringTrie<T>.Add(const S: String; const AValue: T);
var
  i: LongInt;
  sVal: string;
begin
  if (S = '') then
  begin
    raise ESvTrieException.Create('Cannot add empty string to the trie');
  end;

  if FRoot = nil then
  begin
    FRoot := TSuffixTrieNode<T>.Create(Self, nil, AValue);
  end;

  if FCaseSensitive then
    sVal := S
  else
    sVal := UpperCase(S, loInvariantLocale);

  if FSearchFromStart then
  begin
    FRoot.Add(@(sVal[1]), AValue, True);
  end
  else
  begin
    for i := 1 to Length(sVal) do // add each suffix
    begin
      FRoot.Add(@(sVal[i]), AValue, i = 1);
    end;
  end;

  Inc(FCount);
end;

function TSvStringTrie<T>.Exists(const S: String; out AValue: T; AExactMatch: Boolean): Boolean;
var
  node: TSuffixTrieNode<T>;
begin
  Result := Exists(S, node, AExactMatch);

  if Result then
    AValue := node.Value
  else
    AValue := System.Default(T);
end;

function TSvStringTrie<T>.Exists(const S: String; out AValue: TSuffixTrieNode<T>;
  AExactMatch: Boolean): Boolean;
var
  sVal: string;
begin
  Result := FRoot <> nil;
  if Result then
  begin
    if FCaseSensitive then
      sVal := S
    else
      sVal := UpperCase(S, loInvariantLocale);

    Result := FRoot.Exists(@sVal[1], AValue, AExactMatch);
  end;
end;

function TSvStringTrie<T>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

procedure TSvStringTrie<T>.Remove(const S: string);
var
  aNode, aNodeToFind, aNodeToFree: TSuffixTrieNode<T>;
  c: TAlpha;
begin
  if not FSearchFromStart then
    raise ESvTrieException.Create('Cannot remove from suffix trie')
  else
  begin
    if not Assigned(FRoot) then
      Exit;

    if Exists(S, aNode, True) then
    begin
      if aNode.LastCharAdded then
      begin
        Notification(aNode, nDeleted);
        aNode.FLastAdded := False;
      end;

      //check if aNode has leafs
      if not aNode.HasChildren then
      begin
        aNodeToFind := aNode.Parent;
        while Assigned(aNodeToFind) do
        begin
          if aNodeToFind.HasChildren then
          begin
            Break;
          end
          else
          begin
            if not aNodeToFind.LastCharAdded then
            begin
              aNodeToFind := aNodeToFind.Parent;
              aNodeToFree := aNodeToFind;
              aNodeToFree.Free;
              Continue;
            end;
          end;

          aNodeToFind := aNodeToFind.Parent;
        end;
      end;
      Dec(FCount);
    end;
  end;
end;

function TSvStringTrie<T>.ToString: String;
var
  c: TAlpha;
begin
  Result := '';

  if not Assigned(FRoot) then
    Exit;

  for c := Low(FRoot.Children) to High(FRoot.Children) do
  begin
    if Assigned(FRoot.Children[c]) then
    begin
      Result := Result + FRoot.GetChildren(c).ToString(Chr(c));
    end;
  end;
  Result := Trim(Result);
end;

procedure TSvStringTrie<T>.Traverse(AProc: TTraverseProc<T>);
begin
  if Assigned(FRoot) then
  begin
    FRoot.Traverse('', AProc);
  end;
end;

function TSvStringTrie<T>.TryGetValue(const S: string; out AValue: T; AExactMatch: Boolean): Boolean;
begin
    if S.IsEmpty then
        Exit(False);
  Result := Exists(S, AValue, AExactMatch);
end;

function TSvStringTrie<T>.TryGetValues(const S: string): TList<T>;
var
  aNode: TSuffixTrieNode<T>;
  lst: TList<T>;
begin
  Result := TList<T>.Create;
  if not Assigned(FRoot) then
    Exit;

  lst := Result;
  if Exists(S, aNode, False) then
  begin
    //if not aNode.LastCharAdded then
  //    lst.Add(aNode.Value);
    aNode.Traverse(S, procedure(const AKey: string; AFoundNode: TSuffixTrieNode<T>)
      begin
        lst.Add(AFoundNode.Value);
      end);
  end;
end;

{ TSvObjectStringTrie<T> }

constructor TSvObjectStringTrie<T>.Create(AOwnsObjects: Boolean; ASearchFromStartOnly: Boolean);
begin
  inherited Create(ASearchFromStartOnly);
  FOwnsObjects := AOwnsObjects;
end;

procedure TSvObjectStringTrie<T>.Notification(ANode: TSuffixTrieNode<T>; AAction: TNodeNotification);
begin
  case AAction of
    nAdded: ;
    nDeleted:
    begin
      if FOwnsObjects then
      begin
        if ANode.LastCharAdded and Assigned(ANode.Value) then
        begin
          ANode.Value.Free;
        end;
      end;
    end;
    nExtracted: ;
  end;
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
  FTrie.Traverse(procedure(const AKey: string; ANode: TSuffixTrieNode<T>)
    begin
      FItems[ix].Key := AKey;
      FItems[ix].Value := ANode.Value;
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

end.
