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
unit SvDelegates;

interface

uses
  SysUtils, Generics.Collections;

{ IDelegate<T> }

type
  ESvDelegateException = class(Exception);

  IDelegate<T> = interface
    ['{ADBC29C1-4F3D-4E4C-9A79-C805E8B9BD92}']
    procedure Add(const Handler: T);
    procedure Remove(const Handler: T);
  end;

{ IDelegateContainer<T> }

  IDelegateContainer<T> = interface
    ['{ED255F00-3112-4315-9E25-3C1B3064C932}']
    function GetDelegate: IDelegate<T> ;
    function GetEnumerator: TEnumerator<T>;
    property Delegate: IDelegate<T> read GetDelegate;
    property Enumerator: TEnumerator<T> read GetEnumerator;
    function Count: Integer;
    function IndexOf(const AHandler: T): Integer;
  end;

{ TDelegateImpl<T> }

  TDelegateImpl<T> = class(TInterfacedObject, IDelegate<T>)
  private
    FList: TList<T>;
  protected
    { IDelegate<T> }
    procedure Add(const Event: T);
    procedure Remove(const Event: T);
  public
    constructor Create(List: TList<T>);
  end;

{ TDelegateContainerImpl<T> }

  TDelegateContainerImpl<T> = class(TInterfacedObject, IDelegateContainer<T>)
  private
    FDelegate: IDelegate<T>;
    FList: TList<T>;
  protected
    { IDelegateContainer<T> }
    function GetDelegate: IDelegate<T>;
    function GetEnumerator: TEnumerator<T>;
    function Count: Integer;
    function IndexOf(const AHandler: T): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

{ TDelegate<T> }

  SvDelegate<T> = record
  private
    FContainer: IDelegateContainer<T>;
    function GetContainer: IDelegateContainer<T>;
    function GetCount: Integer;
  public
    class operator Implicit(var Delegate: SvDelegate<T>): IDelegate<T>;
    function GetEnumerator: TEnumerator<T>;

    procedure Add(const Handler: T);
    procedure Remove(const Handler: T);
    function IndexOf(const Handler: T): Integer;
    property Count: Integer read GetCount;
  end;

{ Example usage:

  TNotifyDelegate = TDelegate<TNotifyEvent>;
  INotifyDelegate = IDelegate<TNotifyEvent>; }

implementation

uses
  TypInfo;

{ TDelegateImpl<T> }

constructor TDelegateImpl<T>.Create(List: TList<T>);
begin
  inherited Create;
  FList := List;
end;

{ TDelegateImpl<T>.IDelegate<T> }

procedure TDelegateImpl<T>.Add(const Event: T);
var
  LTypeInfo: PTypeInfo;
begin
  LTypeInfo := TypeInfo(T);
  Assert(LTypeInfo.Kind in [tkInterface, tkMethod], 'Delegate must be a method');

  FList.Add(Event);
end;

procedure TDelegateImpl<T>.Remove(const Event: T);
begin
  FList.Remove(Event);
end;

{ TDelegateContainerImpl<T> }

function TDelegateContainerImpl<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TDelegateContainerImpl<T>.Create;
begin
  inherited Create;
  FList := TList<T>.Create;
end;

destructor TDelegateContainerImpl<T>.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

{ TDelegateContainerImpl<T>.IDelegateContainer<T> }

function TDelegateContainerImpl<T>.GetDelegate: IDelegate<T>;
begin
  if FDelegate = nil then
    FDelegate := TDelegateImpl<T>.Create(FList);
  Result := FDelegate;
end;

function TDelegateContainerImpl<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TDelegateContainerImpl<T>.IndexOf(const AHandler: T): Integer;
begin
  {TODO -oLinas -cGeneral : fix TMethod comparer for TList}
  Result := FList.IndexOf(AHandler);
end;

{ TDelegate<T> }

class operator SvDelegate<T>.Implicit(var Delegate: SvDelegate<T>): IDelegate<T>;
begin
  Result := Delegate.GetContainer.Delegate;
end;

function SvDelegate<T>.IndexOf(const Handler: T): Integer;
begin
  Result := GetContainer.IndexOf(Handler);
end;

function SvDelegate<T>.GetContainer: IDelegateContainer<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainerImpl<T>.Create;
  Result := FContainer;
end;

function SvDelegate<T>.GetCount: Integer;
begin
  Result := GetContainer.Count;
end;

function SvDelegate<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := GetContainer.GetEnumerator;
end;

procedure SvDelegate<T>.Add(const Handler: T);
begin
  GetContainer.Delegate.Add(Handler);
end;

procedure SvDelegate<T>.Remove(const Handler: T);
begin
  GetContainer.Delegate.Remove(Handler);
end;

end.
