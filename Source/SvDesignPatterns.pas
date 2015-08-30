(*
* Copyright (c) 2011, Linas Naginionis
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
unit SvDesignPatterns;

interface

uses
  SysUtils, Generics.Collections, Rtti, SyncObjs, SvRttiUtils;

type
  EFactoryMethodKeyAlreadyRegisteredException = class(Exception);
  EFactoryMethodKeyNotRegisteredException = class(Exception);

  TFactoryMethodKeyAlreadyRegisteredException = EFactoryMethodKeyAlreadyRegisteredException;
  TFactoryMethodKeyNotRegisteredException = EFactoryMethodKeyNotRegisteredException;

  TFactoryMethod<TBaseType> = reference to function: TBaseType;

  Inject = class(TCustomAttribute)
  private
    FValue: string;
    FHasValue: Boolean;
  public
    constructor Create(); overload;
    constructor Create(const AValue: string); overload;

    property HasValue: Boolean read FHasValue;
    property Value: string read FValue;
  end;

  /// <summary>
  /// Abstract factory class
  /// </summary>
  TAbstractFactory<TKey, TBaseType> = class abstract
  strict private
    type
      TFactoryEnumerator = class(TEnumerator<TPair<TKey,TBaseType>>)
      private
        FFactory: TAbstractFactory<TKey, TBaseType>;
        FEnum: TEnumerator<TPair<TKey,TFactoryMethod<TBaseType>>>;
        function GetCurrent: TPair<TKey,TBaseType>;
      protected
        function DoGetCurrent: TPair<TKey,TBaseType>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AFactory: TAbstractFactory<TKey, TBaseType>);
        destructor Destroy; override;

        property Current: TPair<TKey,TBaseType> read GetCurrent;
        function MoveNext: Boolean;
      end;
  private
    FFactoryMethods: TDictionary<TKey, TFactoryMethod<TBaseType>>;
    FInjectedValues: TObjectDictionary<TKey, TDictionary<string,TValue>>;
    FDefaultKey: TKey;
    FIsThreadSafe: Boolean;
    FCSection: TCriticalSection;
    function GetCount: Integer;
    function GetIsThreadSafe: Boolean;
    procedure SetIsThreadSafe(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure InjectValue(const AKey: TKey; const AFieldOrPropName: string; const AValue: TValue); virtual;
    /// <summary>
    /// Registers default key which will be used when getting factory through GetDefaultInstance()
    /// </summary>
    procedure RegisterDefaultKey(const AKey: TKey); virtual;
    /// <summary>
    /// Registers new factory method. If AFactoryMethod is nil, factory will use 
    ///   parameterless constructor
    /// </summary>
    /// <param name="AKey">Key to identify the element</param>
    /// <param name="AFactoryMethod">Anonymous method which returns the correct element</param>
    procedure RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>); virtual;
    procedure UnregisterFactoryMethod(const AKey: TKey); virtual;
    procedure UnregisterAll(); virtual;
    /// <summary>
    /// Checks if given key has had registered factory method
    /// </summary>
    function IsRegistered(const AKey: TKey): Boolean; virtual;

    class function CreateElement(): TBaseType;

    function GetEnumerator: TFactoryEnumerator;
  protected
    function DoGetInstance(const AKey: TKey): TBaseType; virtual; abstract;
    procedure DoInjectValues(const AKey: TKey; const ABaseType: TBaseType); virtual;
    procedure ClearInjectedValues(const AObject: TObject); virtual;
  public
    function GetInstance(const AKey: TKey): TBaseType;
    /// <summary>
    /// Retrieves default factory.
    /// </summary>
    /// <remarks>
    /// Default key must be registered for this method to work properly
    /// </remarks>
    function GetDefaultInstance(): TBaseType; virtual;
    /// <summary>
    /// Is GetInstance function Thread Safe?
    /// </summary>
    property IsThreadSafe: Boolean read GetIsThreadSafe write SetIsThreadSafe;
    /// <summary>
    /// Count of registered factory methods
    /// </summary>
    property Count: Integer read GetCount;
  end;

  /// <summary>
  /// Factory method class
  /// </summary>
  TFactory<TKey, TBaseType> = class(TAbstractFactory<TKey, TBaseType>)
  protected
    /// <summary>
    /// Gets instance of TBaseType
    /// </summary>
    /// <remarks>
    /// AFactoryMethod will be invoked on each and every instance retrieval.
    /// </remarks>
    /// <param name="AKey">Key of an element</param>
    /// <returns>Instance of TBaseType</returns>
    function DoGetInstance(const AKey: TKey): TBaseType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Multiton pattern. 
  /// </summary>
  TMultiton<TKey, TBaseType> = class(TAbstractFactory<TKey, TBaseType>)
  private
    FLifetimeWatcher: TDictionary<TKey,TValue>;
    FOwnsObjects: Boolean;
    procedure ClearObject(const AValue: TValue);
    procedure ClearObjects();
  protected
    /// <summary>
    /// Gets instance of TBaseType
    /// </summary>
    /// <remarks>
    /// AFactoryMethod will be invoked only on first instance retrieval. Further retrievals
    ///  will return the same instance as it was returned in AFactoryMethod
    /// </remarks>
    /// <param name="AKey">Key of an element</param>
    /// <returns>Instance of TBaseType</returns>
    function DoGetInstance(const AKey: TKey): TBaseType; override;
  public
    constructor Create(AOwnsObjects: Boolean = False); reintroduce; overload; 
    destructor Destroy; override;
    /// <summary>
    /// Registers new factory method. If AFactoryMethod is nil, factory will use an instance created 
    /// with parameterless constructor
    /// </summary>
    /// <param name="AKey">Key to identify the element</param>
    /// <param name="AFactoryMethod">Anonymous method which returns the correct element</param>
    procedure RegisterFactoryMethod(const AKey: TKey; AFactoryMethod: TFactoryMethod<TBaseType>); override;
    procedure UnregisterFactoryMethod(const AKey: TKey); override;
    procedure UnregisterAll(); override;


    /// <summary>
    /// If multiton owns object , it will try to free them.
    /// </summary>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;


  ISingleton<T: class> = interface
    ['{EFF4476A-694E-42EB-BC4E-2384E4860F92}']
    function GetIsThreadSafe: Boolean;
    procedure SetIsThreadSafe(const Value: Boolean);

    procedure RegisterConstructor(const AConstructorMethod: TFactoryMethod<T>);
    function GetInstance(): T;

    property IsThreadSafe: Boolean read GetIsThreadSafe write SetIsThreadSafe;
  end;
  /// <summary>
  /// Singleton implementation
  /// </summary>
  TSingleton<T: class> = class(TInterfacedObject, ISingleton<T>)
  private
    FMethod: TFactoryMethod<T>;
    FCreated: Boolean;
    FObject: T;
    FIsThreadSafe: Boolean;
    FCSection: TCriticalSection;
    function GetIsThreadSafe: Boolean;
    procedure SetIsThreadSafe(const Value: Boolean);
  protected
    function CreateNew(): T;
  public
    constructor Create(); overload;
    constructor Create(const AConstructorMethod: TFactoryMethod<T>); overload;
    destructor Destroy; override;

    procedure RegisterConstructor(const AConstructorMethod: TFactoryMethod<T>);
    function GetInstance(): T;

    property IsThreadSafe: Boolean read GetIsThreadSafe write SetIsThreadSafe;
  end;

  ISvLazy<T> = interface
    ['{433C34BB-E5F3-4594-814C-70F02C415CB8}']
    function OwnsObjects: Boolean;
    function ValueCreated: Boolean;
    procedure SetValue(const AValue: T);
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property Value: T read GetValue;
  end;

  TSvLazy<T> = class(TInterfacedObject, ISvLazy<T>)
  private
    FValueFactory: TFunc<T>;
    FValueCreated: Boolean;
    FOwnsObjects: Boolean;
    FValue: T;
    FDisableCache: Boolean;
    function GetValue: T;
  protected
    procedure SetValue(const AValue: T);
    function OwnsObjects: Boolean;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
  public
    constructor Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean = False);
    destructor Destroy; override;

    function ValueCreated: Boolean;

    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property Value: T read GetValue;
  end;
  /// <summary>
  /// Represents lazy variable
  /// </summary>
  SvLazy<T> = record
  private
    FLazy: ISvLazy<T>;
    function GetValue: T;
    function GetDisableCache: Boolean;
    procedure SetDisableCache(const Value: Boolean);
  public
    /// <summary>
    /// Initializes lazy value with anonymous getValue factory
    /// </summary>
    /// <param name="AValueFactory">Anonymous method which gets value</param>
    /// <param name="AOwnsObjects">Boolean property to specify if lazy value should free it's value when it goes out of scope</param>
    constructor Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean = False);
    constructor CreateLazy(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean = False);

    procedure Assign(const AValue: T);
    function IsValueCreated: Boolean;

    class operator Implicit(const ALazy: SvLazy<T>): T; inline;
    class operator Implicit(const AValueFactory: TFunc<T>): SvLazy<T>; inline;
    class operator Implicit(const AValue: T): SvLazy<T>; inline;

    property DisableCacheForNextGet: Boolean read GetDisableCache write SetDisableCache;
    property Value: T read GetValue;
  end;

implementation

uses
  Variants;

{ TAbstractFactory<TKey, TBaseType> }

procedure TAbstractFactory<TKey, TBaseType>.ClearInjectedValues(const AObject: TObject);
var
  val: TValue;
  rType: TRttiType;
  rField: TRttiField;
  rProp: TRttiProperty;
  attr: TCustomAttribute;
begin
  rType := TRttiContext.Create.GetType(AObject.ClassInfo);
  for rField in rType.GetFields do
  begin
    for attr in rField.GetAttributes do
    begin
      if attr is Inject then
      begin
        if not Inject(attr).HasValue then
        begin
          val := rField.GetValue(AObject);
          if not val.IsEmpty and val.IsObject then
          begin
            val.AsObject.Free;
          end;
        end;
        Break;
      end;
    end;
  end;

  for rProp in rType.GetProperties do
  begin
    for attr in rProp.GetAttributes do
    begin
      if attr is Inject then
      begin
        if not Inject(attr).HasValue then
        begin
          val := rProp.GetValue(AObject);
          if not val.IsEmpty and val.IsObject then
          begin
            val.AsObject.Free;
          end;
        end;
        Break;
      end;
    end;
  end;
end;

constructor TAbstractFactory<TKey, TBaseType>.Create;
begin
  inherited Create();
  FFactoryMethods := TDictionary<TKey, TFactoryMethod<TBaseType>>.Create();
  FInjectedValues := TObjectDictionary<TKey, TDictionary<string,TValue>>.Create([doOwnsValues]);
  FDefaultKey := System.Default(TKey);
  FCSection := TCriticalSection.Create;
end;

class function TAbstractFactory<TKey, TBaseType>.CreateElement: TBaseType;
begin
  Result := TSvRtti.CreateNewClass<TBaseType>;
end;

destructor TAbstractFactory<TKey, TBaseType>.Destroy;
begin
  FFactoryMethods.Free;
  FInjectedValues.Free;
  FCSection.Free;
  inherited Destroy;
end;

procedure TAbstractFactory<TKey, TBaseType>.DoInjectValues(const AKey: TKey; const ABaseType: TBaseType);
var
  list: TDictionary<string,TValue>;
  pair: TPair<string, TValue>;
  val, valBaseType: TValue;
  obj: TBaseType;
  newObj: TObject;
  rType: TRttiType;
  rField: TRttiField;
  rProp: TRttiProperty;
  attr: TCustomAttribute;
begin
  //enumerate attributes
  valBaseType := TValue.From<TBaseType>(ABaseType);
  if not valBaseType.IsEmpty and valBaseType.IsObject then
  begin
    rType := TRttiContext.Create.GetType(TypeInfo(TBaseType));

    for rField in rType.GetFields do
    begin
      for attr in rField.GetAttributes do
      begin
        if attr is Inject then
        begin
          if not Inject(attr).HasValue then
          begin
            newObj := TSvRtti.CreateNewObject(rField.FieldType.Handle);
            rField.SetValue(valBaseType.AsObject, newObj);
          end
          else
          begin
            //just assign value
            TSvRtti.SetValueFromString(valBaseType.AsObject, Inject(attr).Value, rField);
          end;

          Break;
        end;
      end;
    end;

    for rProp in rType.GetProperties do
    begin
      for attr in rProp.GetAttributes do
      begin
        if attr is Inject then
        begin
          if not Inject(attr).HasValue then
          begin
            newObj := TSvRtti.CreateNewObject(rProp.PropertyType.Handle);
            rProp.SetValue(valBaseType.AsObject, newObj);
          end
          else
          begin
            //just assign value
            TSvRtti.SetValueFromString(valBaseType.AsObject, Inject(attr).Value, rProp);
          end;

          Break;
        end;
      end;
    end;
  end;

  if FInjectedValues.TryGetValue(AKey, list) then
  begin
    for pair in list do
    begin
      val := pair.Value;
      if (val.IsEmpty) then
      begin
        //if value is empty and property is instance then create this property object
        if TSvRtti.IsInstanceProp<TBaseType>(pair.Key, ABaseType) then
        begin
          obj := TSvRtti.CreateNewClass<TBaseType>;
          val := TValue.From<TBaseType>(obj);
        end;
      end;

      TSvRtti.SetValue<TBaseType>(pair.Key, ABaseType, val);
    end;
  end;
end;

function TAbstractFactory<TKey, TBaseType>.GetCount: Integer;
begin
  Result := FFactoryMethods.Count;
end;

function TAbstractFactory<TKey, TBaseType>.GetDefaultInstance: TBaseType;
begin
  Result := GetInstance(FDefaultKey);
end;

function TAbstractFactory<TKey, TBaseType>.GetEnumerator: TFactoryEnumerator;
begin
  Result := TFactoryEnumerator.Create(Self);
end;

function TAbstractFactory<TKey, TBaseType>.GetInstance(const AKey: TKey): TBaseType;
begin
  if FIsThreadSafe then
    FCSection.Enter;
  try
    Result := DoGetInstance(AKey);

    DoInjectValues(AKey, Result);
  finally
    if FIsThreadSafe then
      FCSection.Leave;
  end;
end;

function TAbstractFactory<TKey, TBaseType>.GetIsThreadSafe: Boolean;
begin
  Result := FIsThreadSafe;
end;

procedure TAbstractFactory<TKey, TBaseType>.InjectValue(const AKey: TKey; const AFieldOrPropName: string; const AValue: TValue);
var
  list: TDictionary<string,TValue>;
begin
  if not FInjectedValues.TryGetValue(AKey, list) then
  begin
    list := TDictionary<string,TValue>.Create;
    FInjectedValues.Add(AKey, list);
  end;

  list.AddOrSetValue(AFieldOrPropName, AValue);
end;

function TAbstractFactory<TKey, TBaseType>.IsRegistered(const AKey: TKey): Boolean;
begin
  Result := FFactoryMethods.ContainsKey(AKey);
end;

procedure TAbstractFactory<TKey, TBaseType>.RegisterDefaultKey(const AKey: TKey);
begin
  if IsRegistered(AKey) then
    FDefaultKey := AKey
  else
    raise TFactoryMethodKeyNotRegisteredException.Create('Key not registered');
end;

procedure TAbstractFactory<TKey, TBaseType>.RegisterFactoryMethod(const AKey: TKey;
  AFactoryMethod: TFactoryMethod<TBaseType>);
begin
  if IsRegistered(AKey) then
    raise TFactoryMethodKeyAlreadyRegisteredException.Create('Factory already registered');

  FFactoryMethods.Add(AKey, AFactoryMethod);
end;

procedure TAbstractFactory<TKey, TBaseType>.SetIsThreadSafe(const Value: Boolean);
begin
  if FIsThreadSafe <> Value then
    FIsThreadSafe := Value;
end;

procedure TAbstractFactory<TKey, TBaseType>.UnregisterAll;
begin
  FFactoryMethods.Clear;
end;

procedure TAbstractFactory<TKey, TBaseType>.UnregisterFactoryMethod(const AKey: TKey);
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  FFactoryMethods.Remove(AKey);
end;

{ TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator }

constructor TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.Create(
  AFactory: TAbstractFactory<TKey, TBaseType>);
begin
  inherited Create();
  FFactory := AFactory;
  FEnum := FFactory.FFactoryMethods.GetEnumerator;
end;

destructor TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.Destroy;
begin
  FEnum.Free;
  inherited Destroy;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.DoGetCurrent: TPair<TKey, TBaseType>;
begin
  Result := GetCurrent;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.GetCurrent: TPair<TKey, TBaseType>;
begin
  Result.Key := FEnum.Current.Key;
  Result.Value := FFactory.GetInstance(Result.Key);
end;

function TAbstractFactory<TKey, TBaseType>.TFactoryEnumerator.MoveNext: Boolean;
begin
  Result := FEnum.MoveNext;
end;

{ TFactory<TKey, TBaseType> }

constructor TFactory<TKey, TBaseType>.Create;
begin
  inherited Create();
end;

destructor TFactory<TKey, TBaseType>.Destroy;
begin
  inherited Destroy;
end;

function TFactory<TKey, TBaseType>.DoGetInstance(const AKey: TKey): TBaseType;
var
  factoryMethod: TFactoryMethod<TBaseType>;
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');
  factoryMethod := fFactoryMethods.Items[AKey];
  if Assigned(factoryMethod) then
    Result := factoryMethod
  else
    Result := CreateElement;
end;

{ TMultiton<TKey, TBaseType> }

procedure TMultiton<TKey, TBaseType>.ClearObject(const AValue: TValue);
var
  obj: TObject;
begin
  if not (AValue.IsEmpty) and (AValue.IsObject) then
  begin
    obj := AValue.AsObject;
    if Assigned(obj) then
    begin
      ClearInjectedValues(obj);
      obj.Free;
    end;
  end;
end;

procedure TMultiton<TKey, TBaseType>.ClearObjects;
var
  pair: TPair<TKey,TValue>;
begin
  if FOwnsObjects then
  begin
    for pair in FLifetimeWatcher do
    begin
      ClearObject(pair.Value);
    end;
  end;
end;

constructor TMultiton<TKey, TBaseType>.Create(AOwnsObjects: Boolean);
begin
  inherited Create();
  FLifetimeWatcher := TDictionary<TKey,Rtti.TValue>.Create; 
  FOwnsObjects := AOwnsObjects;
end;

destructor TMultiton<TKey, TBaseType>.Destroy;
begin
  ClearObjects;
  FLifetimeWatcher.Free;
  inherited;
end;

function TMultiton<TKey, TBaseType>.DoGetInstance(const AKey: TKey): TBaseType;
var
  factoryMethod: TFactoryMethod<TBaseType>;
  AValue: TValue;
begin
  if not IsRegistered(AKey) then
    raise TFactoryMethodKeyNotRegisteredException.Create('Factory not registered');

  AValue := FLifetimeWatcher.Items[AKey];
  if AValue.IsEmpty then
  begin
    factoryMethod := fFactoryMethods.Items[AKey];
    if Assigned(factoryMethod) then
      Result := factoryMethod     
    else
      Result := CreateElement;
    AValue := TValue.From<TBaseType>(Result);
    FLifetimeWatcher.AddOrSetValue(AKey, AValue);
  end
  else
  begin
    Result := AValue.AsType<TBaseType>;
  end;
end;

procedure TMultiton<TKey, TBaseType>.RegisterFactoryMethod(const AKey: TKey;
  AFactoryMethod: TFactoryMethod<TBaseType>);
begin
  inherited;
  FLifetimeWatcher.Add(AKey, TValue.Empty);
end;

procedure TMultiton<TKey, TBaseType>.UnregisterAll;
begin
  inherited;
  ClearObjects;
  FLifetimeWatcher.Clear;
end;

procedure TMultiton<TKey, TBaseType>.UnregisterFactoryMethod(const AKey: TKey);
var
  AValue: TValue;
begin
  inherited;
  if FOwnsObjects then
  begin
    AValue := FLifetimeWatcher.Items[AKey];
    ClearObject(AValue);
  end;
  FLifetimeWatcher.Remove(AKey);
end;

{ TSingleton<T> }

constructor TSingleton<T>.Create;
begin
  inherited Create();
  FMethod := nil;
  FCreated := False;
  FCSection := TCriticalSection.Create;
end;

constructor TSingleton<T>.Create(const AConstructorMethod: TFactoryMethod<T>);
begin
  Create;
  FMethod := AConstructorMethod;
end;

function TSingleton<T>.CreateNew: T;
begin
  if Assigned(FMethod) then
    FObject := FMethod()
  else
    FObject := TSvRtti.CreateNewClass<T>;

  Result := FObject;
  FCreated := True;
end;

destructor TSingleton<T>.Destroy;
begin
  if FCreated then
    FObject.Free;

  FCSection.Free;
  inherited Destroy;
end;

function TSingleton<T>.GetInstance: T;
begin
  if FIsThreadSafe then
    FCSection.Enter;
  try
    if FCreated then
      Result := FObject
    else
      Result := CreateNew();
  finally
    if FIsThreadSafe then
      FCSection.Leave;
  end;
end;

function TSingleton<T>.GetIsThreadSafe: Boolean;
begin
  Result := FIsThreadSafe;
end;

procedure TSingleton<T>.RegisterConstructor(const AConstructorMethod: TFactoryMethod<T>);
begin
  FMethod := AConstructorMethod;
end;

procedure TSingleton<T>.SetIsThreadSafe(const Value: Boolean);
begin
  if Value <> FIsThreadSafe then
    FIsThreadSafe := Value;
end;

{ TSvLazy<T> }

constructor TSvLazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean);
begin
  inherited Create;
  FValueFactory := AValueFactory;
  FValueCreated := False;
  FOwnsObjects := AOwnsObjects;
end;

destructor TSvLazy<T>.Destroy;
begin
  if FOwnsObjects and FValueCreated then
  begin
    //free value
    TSvRtti.DestroyClass<T>(FValue);
  end;
  inherited;
end;

function TSvLazy<T>.GetDisableCache: Boolean;
begin
  Result := FDisableCache;
end;

function TSvLazy<T>.GetValue: T;
begin
  if (not FValueCreated) or FDisableCache then
  begin
    FValue := FValueFactory();
    FValueCreated := True;
  end;

  FDisableCache := False;
  Result := FValue;
end;

function TSvLazy<T>.OwnsObjects: Boolean;
begin
  Result := FOwnsObjects;
end;

procedure TSvLazy<T>.SetDisableCache(const Value: Boolean);
begin
  FDisableCache := Value;
end;

procedure TSvLazy<T>.SetValue(const AValue: T);
begin
  FValueCreated := True;
  FValue := AValue;
end;

function TSvLazy<T>.ValueCreated: Boolean;
begin
  Result := FValueCreated;
end;

{ SvLazy<T> }

procedure SvLazy<T>.Assign(const AValue: T);
var
  oldValue: T;
begin
  if Assigned(FLazy) then
  begin
    if FLazy.OwnsObjects and FLazy.ValueCreated then
    begin
      oldValue := FLazy.Value;
      TSvRtti.DestroyClass<T>(oldValue);
    end;

    FLazy.SetValue(AValue);
  end
  else
  begin
    FLazy := TSvLazy<T>.Create(function: T begin Result := AValue end);
  end;
end;

constructor SvLazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean);
begin
  FLazy := TSvLazy<T>.Create(AValueFactory, AOwnsObjects);
end;

constructor SvLazy<T>.CreateLazy(const AValueFactory: TFunc<T>; AOwnsObjects: Boolean);
begin
  Create(AValueFactory, AOwnsObjects);
end;

function SvLazy<T>.GetDisableCache: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
  begin
    Result := FLazy.DisableCacheForNextGet;
  end;
end;

function SvLazy<T>.GetValue: T;
begin
  Result := FLazy.Value;
end;

class operator SvLazy<T>.Implicit(const AValue: T): SvLazy<T>;
begin
  Result := SvLazy<T>.Create(function: T begin Result := AValue; end);
end;

function SvLazy<T>.IsValueCreated: Boolean;
begin
  Result := False;
  if Assigned(FLazy) then
    Result := FLazy.ValueCreated;
end;

procedure SvLazy<T>.SetDisableCache(const Value: Boolean);
begin
  Assert(Assigned(FLazy));
  FLazy.DisableCacheForNextGet := Value;
end;

class operator SvLazy<T>.Implicit(const ALazy: SvLazy<T>): T;
begin
  Result := ALazy.Value;
end;

class operator SvLazy<T>.Implicit(const AValueFactory: TFunc<T>): SvLazy<T>;
begin
  Result := SvLazy<T>.Create(AValueFactory);
end;

{ Inject }

constructor Inject.Create;
begin
  inherited Create;
  FValue := '';
  FHasValue := False;
end;

constructor Inject.Create(const AValue: string);
begin
  Create;
  FValue := AValue;
  FHasValue := True;
end;

end.
