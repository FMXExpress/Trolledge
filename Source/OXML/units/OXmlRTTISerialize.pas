unit OXmlRTTISerialize;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    commercial
    Please see the /license.txt file for more information.

}

{
  OXmlRTTISerialize.pas

  Automatic XML serializer/deserializer with enhanced RTTI
  (supported by Delphi 2010 and newer).

  Supported types:
    - Ordinal (Integer, enum, set, char, WideChar).
    - String (string, WideString).
    - Float (Date, Time, DateTime, Float).
    - Int64
    - Objects (TObject descendant).
    - Record
    - array[0..9] of T: constant arrays (one dimensional)
    - TArray<T>, array of T: dynamic arrays (one dimensional)
    - TList<T>, TList: generic and non-generic lists. The list MUST have
      an enumerator and add/clear methods defined.


  ! Properties in records are not supported due to a Delphi bug:
  ! http://qc.embarcadero.com/wc/qcmain.aspx?d=78110

  ! Properties in interfaces are not supported due to a Delphi bug:
  ! http://qc.embarcadero.com/wc/qcmain.aspx?d=90285

  ! WideString ist not supported in Delphi XE -> Delphi BUG !

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
  System.SysUtils, System.Classes, System.TypInfo, System.RTTI,
  System.Generics.Collections,
  {$ELSE}
  SysUtils, Classes, TypInfo, RTTI, Generics.Collections,
  {$ENDIF}
  OWideSupp, OEncoding, OHashedStrings, OTextReadWrite,
  OXmlReadWrite, OXmlPDOM, OXmlPSeq, OXmlUtils;

type
  TXMLRTTISerializer = class;
  
  TMemberVisibilitySet = set of TMemberVisibility;

  TCustomXMLRTTISerDes = class abstract(TObject)
  private
    fContext: TRttiContext;

    fVisibility: TMemberVisibilitySet;
    fUseRoot: Boolean;
    fCollectionStyle: TXMLSerializeCollectionStyle;
  protected
    procedure DoCreate; virtual;

    property Context: TRttiContext read fContext;
    procedure SetUseRoot(const aUseRoot: Boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //write properties only from a specific visibility
    property Visibility: TMemberVisibilitySet read fVisibility write fVisibility;
    //use root
    property UseRoot: Boolean read fUseRoot write SetUseRoot;
    //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
    //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
    property CollectionStyle: TXMLSerializeCollectionStyle read fCollectionStyle write fCollectionStyle;
  end;

  TXMLRTTISerializer = class(TCustomXMLRTTISerDes)
  private
    fWriter: TXMLWriter;
    fRootElementWritten: Boolean;

    fXMLDeclaration: TXMLWriterDeclaration;
    fRootNodeName: OWideString;
    fWriteDefaultValues: Boolean;
  private
    function GetWriterSettings: TXMLWriterSettings;
    procedure SetRootNodeName(const aRootNodeName: OWideString);
  protected
    procedure SetUseRoot(const aUseRoot: Boolean); override;

    procedure DoCreate; override;
    procedure DoInit; virtual;

    procedure WriteRootStartElement;
    procedure WriteRootEndElement;

    procedure WriteCollectionItems(const aCollection: TCollection);
    procedure WriteObjectEnumeration(const aObject: TObject; const aType: TRttiType);
    procedure WriteObjectProperty(
      aTagName: OWideString; aType: TRttiType;
      const aValue: TValue;
      const aIsDefaultValue, aWriteObjectType: Boolean);
    procedure _WriteObjectProperty(const aTagName, aValue: OWideString);
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLRTTISerializer or call ReleaseDocument!

    procedure InitFile(const aFileName: OWideString);
    procedure InitStream(const aStream: TStream; const aOwnsStream: Boolean = False);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    //Write an object to the XML file.
    //  aElementName: use custom XML element name
    //  aWriteObjectType: if true, write the real object type to "type" XML attribute
    procedure WriteObject<T>(const aObject: T); overload;
    procedure WriteObject<T>(const aObject: T;
      aElementName: OWideString; const aWriteObjectType: Boolean = True); overload;
  public
    //write XML declaration <?xml ?>
    property XMLDeclaration: TXMLWriterDeclaration read fXMLDeclaration;

    //use root
    //  - true: a document root (RootNodeName) will be written
    //  please note that if you disable it (UseRoot = false) and
    //  write more objects to the XML, the XML won't be valid because
    //  XML documents can have only one root
    property UseRoot;
    //custom root node
    property RootNodeName: OWideString read fRootNodeName write SetRootNodeName;
    //write object properties with default values?
    property WriteDefaultValues: Boolean read fWriteDefaultValues write fWriteDefaultValues;
    //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
    //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
    property CollectionStyle;
    //write properties only from a specific visibility
    property Visibility;

    //XML writer settings
    property WriterSettings: TXMLWriterSettings read GetWriterSettings;
  end;

  TXMLRTTIDeserializer = class(TCustomXMLRTTISerDes)
  private
    fXMLParser: TXMLSeqParser;
    fRootNode, fCurrentElementNode: PXMLNode;

    fCreateClasses: TDictionary<string,TClass>;

    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IOTextParseError;
  protected
    procedure SetUseRoot(const aUseRoot: Boolean); override;

    procedure DoCreate; override;
    procedure DoInit; virtual;

    function CreateObject(const aClassName: string; const aType: TRttiInstanceType): TValue;
    procedure CheckRecordForCreateClasses(
      const aInstance: Pointer; const aType: TRttiType; const aItemNode: PXMLNode);
    function CreateNewValue(const aType: TRttiType;
      aTypeName: string; const aItemNode: PXMLNode;
      var outAllocatedData: Pointer): TValue;
    procedure ReadCollectionItems(const aCollection: TCollection;
      const aEnumerationNode: PXMLNode; const aChildName: OWideString);
    procedure ReadObjectEnumeration(const aObject: TObject;
      const aType: TRttiType; const aEnumerationNode: PXMLNode);
    procedure ReadObjectProperties(const aInstance: Pointer;
      const aType: TRttiType;
      const aElementNode: PXMLNode);
    procedure ReadObjectProperty(const aInstance: Pointer;
      const aMember: TRttiMember;
      const aType: TRttiType; const aValue: TValue;
      const aElementNode: PXMLNode;
      var ioPropNameIndex: TXMLNodeIndex);
    procedure ReadObjectPropertyValue(
      aType: TRttiType;
      const aElementValueNode: PXMLNode;
      var ioValue: TValue;
      var outValueDataChanged: Boolean); overload;
    function ReadObjectPropertyValue(
      const aType: TRttiType;
      const aElementValueNode: PXMLNode;
      var ioValue: TValue): Boolean; overload;
    procedure SetPropertyValue(const aInstance: Pointer;
      const aMember: TRttiMember; const aNewValue: TValue);
  public
    destructor Destroy; override;
  public
    //The Init* procedures open and initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached or you call ReleaseDocument!

    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil);
    //init document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitStream(const aStream: TStream; const aForceEncoding: TEncoding = nil);
    //init XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    procedure InitXML(const aXML: OWideString);
    procedure InitXML_UTF8(const aXML: OUTF8Container);
    //init document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    procedure InitBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;

    //Register class for creation
    procedure RegisterClass(const aClass: TClass);
  public
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //use root - please use the same setting here as in TXMLSerialize
    property UseRoot;
  public
    //following functions and properties can be called only during parsing (after Init* has been called).

    //Find next element
    //  outElementName -> name of the XML element used (is the same with outType if TXMLSerializer.WriteObject was not executed with custom name)
    //  outType -> type of the object - differs from outElementName only if TXMLSerializer.WriteObject was executed with custom name
    function ReadObjectInfo(var outElementName: OWideString): Boolean; overload;
    function ReadObjectInfo(var outElementName, outType: OWideString): Boolean; overload;
    //If an element was found with ReadElementInfo, read it into an instance
    procedure ReadObject<T>(const aObject: T);
    //Read object from any other (externally-defined) XML element node
    procedure ReadObjectFromNode<T>(const aObject: T; const aElementNode: PXMLNode);

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read GetParseError;

    //write properties only from a specific visibility
    property Visibility;
  end;

  TRttiContextHelper = record helper for TRttiContext
  public
    //Get real object type -> especially for instances of classes
    function GetRealObjectType<T>(const aObject: T): TRttiType; overload;
    function GetRealObjectType(const aValue: TValue; const aDefType: TRttiType): TRttiType; overload;
  end;

  EXMLRTTISerializer = class(Exception);
  EXMLRTTIDeserializer = class(Exception);

  //use TSerializableDictionary if you want to serialize a dictionary
  // - the delphi own TDictionary can't be serialized because it does not have an Add method with TPair as parameter
  TSerializableDictionary<TKey,TValue> = class(TDictionary<TKey,TValue>)
  public
    procedure Add(const aPair: TPair<TKey,TValue>); reintroduce; overload;
    procedure Add(const aKey: TKey; aValue: TValue); reintroduce; overload;
  end;

  //use TSerializableObjectDictionary if you want to serialize an object dictionary
  // - the delphi own TObjectDictionary can't be serialized because it does not have an Add method with TPair as parameter
  TSerializableObjectDictionary<TKey,TValue> = class(TObjectDictionary<TKey,TValue>)
  public
    procedure Add(const aPair: TPair<TKey,TValue>); reintroduce; overload;
    procedure Add(const aKey: TKey; aValue: TValue); reintroduce; overload;
  end;

type
  PObject = ^TObject;
  PInterface = ^IInterface;

implementation

uses
  OXmlLng;

type
  TValueHelper = record helper for TValue
  public
    function AsSet: Integer;
  end;

function TRttiPropertyHelper_IsDefaultValue(const aProperty: TRttiProperty; const aValue: TValue): Boolean; inline;
begin
  Result :=
    (aProperty is TRttiInstanceProperty) and
    (aProperty.PropertyType.IsOrdinal) and
    (TRttiInstanceProperty(aProperty).Default = aValue.AsOrdinal);
end;

{ TXMLRTTISerializer }

constructor TXMLRTTISerializer.Create;
begin
  inherited Create;
end;

constructor TXMLRTTISerializer.Create(const aStream: TStream);
begin
  inherited Create;

  InitStream(aStream);
end;

destructor TXMLRTTISerializer.Destroy;
begin
  ReleaseDocument;
  fWriter.Free;
  fXMLDeclaration.Free;

  inherited;
end;

procedure TXMLRTTISerializer.DoCreate;
begin
  inherited DoCreate;

  fWriter := TXMLWriter.Create;
  fXMLDeclaration := TXMLWriterDeclaration.Create;
  fRootNodeName := 'oxmlserializer';
end;

procedure TXMLRTTISerializer.DoInit;
begin
  fRootElementWritten := False;
end;

function TXMLRTTISerializer.GetWriterSettings: TXMLWriterSettings;
begin
  Result := fWriter.WriterSettings;
end;

procedure TXMLRTTISerializer.InitFile(const aFileName: OWideString);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLRTTISerializer.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  fWriter.InitStream(aStream, aOwnsStream);

  DoInit;
end;

procedure TXMLRTTISerializer.ReleaseDocument;
begin
  WriteRootEndElement;

  fWriter.ReleaseDocument;
end;

procedure TXMLRTTISerializer.SetRootNodeName(const aRootNodeName: OWideString);
begin
  if fRootElementWritten then
    raise EXMLRTTISerializer.Create(OXmlLng_CannotChangeRootNodeName)
  else
    fRootNodeName := aRootNodeName;
end;

procedure TXMLRTTISerializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fRootElementWritten then
    raise EXMLRTTISerializer.Create(OXmlLng_CannotChangeUseRootDataWritten)
  else
    inherited SetUseRoot(aUseRoot);
end;

procedure TXMLRTTISerializer.WriteObject<T>(const aObject: T);
begin
  WriteObject<T>(aObject, '', False);
end;

procedure TXMLRTTISerializer.WriteCollectionItems(const aCollection: TCollection);
var
  xColItem: TCollectionItem;
  xItemType: TRttiType;
begin
  xItemType := nil;
  if CollectionStyle = csOXml then
    fWriter.OpenElement('_oxmlcollection', stFinish);

  for xColItem in aCollection do
  begin
    if not Assigned(xItemType) then//the collection has items only of 1 kind, no need to check all of them
      xItemType := Context.GetRealObjectType<TCollectionItem>(xColItem);

    case CollectionStyle of
      csOXml: WriteObjectProperty('i', xItemType, TValue.From(xColItem), False, False);
      csOmniXML: WriteObjectProperty(xColItem.ClassName, xItemType, TValue.From(xColItem), False, False);
    end;
  end;

  if CollectionStyle = csOXml then
    fWriter.CloseElement('_oxmlcollection');
end;

procedure TXMLRTTISerializer.WriteObject<T>(const aObject: T;
  aElementName: OWideString; const aWriteObjectType: Boolean);
var
  xType: TRttiType;
begin
  WriteRootStartElement;

  xType := Context.GetRealObjectType<T>(aObject);

  if aElementName = '' then
    aElementName := xType.ToString;

  WriteObjectProperty(aElementName, xType, TValue.From<T>(aObject), False, aWriteObjectType);
end;

procedure TXMLRTTISerializer.WriteObjectEnumeration(const aObject: TObject;
  const aType: TRttiType);
var
  xGetEnumerator, xAdd, xClear: TRttiMethod;
  xEnumObject: TObject;
  xEnumType, xItemType: TRttiType;
  xCurrent: TRttiProperty;
  xMoveNext: TRttiMethod;
  xValue: TValue;
begin
  //enumeration has to have add and clear methods
  xClear := aType.GetMethod('Clear');
  xAdd := aType.GetMethod('Add');
  if not Assigned(xClear) or
     not Assigned(xAdd) or
     (Length(xAdd.GetParameters) <> 1)
  then
    Exit;

  xGetEnumerator := aType.GetMethod('GetEnumerator');
  if not Assigned(xGetEnumerator) or
     (xGetEnumerator.MethodKind <> mkFunction) or
     (xGetEnumerator.ReturnType.Handle.Kind <> tkClass)
  then
    Exit;

  xEnumObject := xGetEnumerator.Invoke(aObject, []).AsObject;
  if not Assigned(xEnumObject) then
    Exit;

  try
    xEnumType := Context.GetType(xEnumObject.ClassInfo);

    xCurrent := xEnumType.GetProperty('Current');
    if not Assigned(xCurrent) then
      Exit;

    xMoveNext := xEnumType.GetMethod('MoveNext');
    if not Assigned(xMoveNext) or
       (Length(xMoveNext.GetParameters) <> 0) or
       (xMoveNext.MethodKind <> mkFunction) or
       (xMoveNext.ReturnType.Handle <> TypeInfo(Boolean))
    then
      Exit;

    fWriter.OpenElement('_oxmldefenum', stFinish);
    while xMoveNext.Invoke(xEnumObject, []).AsBoolean do
    begin
      xValue := xCurrent.GetValue(xEnumObject);
      xItemType := Context.GetRealObjectType(xValue, xCurrent.PropertyType);
      WriteObjectProperty(xItemType.Name,
        xItemType, xValue, False, True);
    end;
    fWriter.CloseElement('_oxmldefenum', True);

  finally
    xEnumObject.Free;
  end;
end;

procedure TXMLRTTISerializer.WriteObjectProperty(
  aTagName: OWideString; aType: TRttiType;
  const aValue: TValue;
  const aIsDefaultValue, aWriteObjectType: Boolean);
var
  xFloatValue: Extended;
  xProperty: TRttiProperty;
  xField: TRttiField;
  xValue: TValue;
  xInstance: Pointer;
  xElementType: TRttiType;
  I: Integer;
  xClassName: string;
begin
  aTagName := OXmlNameToXML(aTagName);
  aType := Context.GetRealObjectType(aValue, aType);

  case aType.TypeKind of
    tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
    begin
      if fWriteDefaultValues or not aIsDefaultValue then
      case aType.TypeKind of
        tkInteger: _WriteObjectProperty(aTagName, IntToStr(aValue.AsInteger));
        tkChar: _WriteObjectProperty(aTagName, OWideString(Char(aValue.AsOrdinal)));
        tkWChar: _WriteObjectProperty(aTagName, OWideString(WideChar(aValue.AsOrdinal)));
        tkEnumeration: _WriteObjectProperty(aTagName, GetEnumName(aType.Handle, aValue.AsOrdinal));
        tkSet: _WriteObjectProperty(aTagName, SetToString(aType.Handle, aValue.AsSet, False));
      end;
    end;
    tkString, tkLString, tkUString, tkWString:
      _WriteObjectProperty(aTagName, aValue.AsString);
    tkFloat:
    begin
      xFloatValue := aValue.AsExtended;
      if (aType = System.TypeInfo(TDateTime)) then
        _WriteObjectProperty(aTagName, ISODateTimeToStr(xFloatValue))
      else if (aType = System.TypeInfo(TTime)) then
        _WriteObjectProperty(aTagName, ISOTimeToStr(xFloatValue))
      else if (aType = System.TypeInfo(TDate)) then
        _WriteObjectProperty(aTagName, ISODateToStr(xFloatValue))
      else
        _WriteObjectProperty(aTagName, ISOFloatToStr(xFloatValue));
    end;
    tkInt64:
      _WriteObjectProperty(aTagName, IntToStr(aValue.AsInt64));
    tkClass, tkRecord, tkInterface:
    begin
      case aType.TypeKind of
        tkClass: xInstance := aValue.AsObject;
        tkRecord: xInstance := aValue.GetReferenceToRawData;
        tkInterface: xInstance := Pointer(aValue.AsInterface);
      else
        xInstance := nil;
      end;

      fWriter.OpenElement(aTagName, stOpenOnly);
      if (aType.TypeKind = tkClass) then
      begin
        xClassName := OXmlNameToXML(TObject(xInstance).ClassName);
        if aWriteObjectType and (aTagName <> xClassName) then
          fWriter.Attribute('type', xClassName);
      end;
      fWriter.FinishOpenElement;

      for xField in aType.GetFields do
      if (xField.Visibility in fVisibility) then
        WriteObjectProperty(xField.Name, xField.FieldType, xField.GetValue(xInstance), False, True);

      for xProperty in aType.GetProperties do
      if (xProperty.Visibility in fVisibility) and
         (xProperty.IsWritable or xProperty.PropertyType.IsInstance)
      then
      begin
        xValue := xProperty.GetValue(xInstance);
        if not((TObject(xInstance) is TCollectionItem) and  //DO NOT WRITE Collection property of TCollectionItem!!!
               (xValue.IsObject) and
               (TObject(xValue.AsObject) = TCollectionItem(xInstance).Collection))
        then
          WriteObjectProperty(xProperty.Name, xProperty.PropertyType, xProperty.GetValue(xInstance), False, True);
      end;

      if aType.TypeKind = tkClass then
      begin
        if TObject(xInstance) is TCollection then
          WriteCollectionItems(TCollection(xInstance))
        else
          WriteObjectEnumeration(TObject(xInstance), aType);
      end;

      fWriter.CloseElement(aTagName, True);
    end;
    tkArray, tkDynArray:
    begin
      if aType is TRttiDynamicArrayType then
        xElementType := TRttiDynamicArrayType(aType).ElementType
      else
      if aType is TRttiArrayType then
        xElementType := TRttiArrayType(aType).ElementType
      else
        raise EXMLRTTISerializer.Create(OXmlLng_RTTIInternalError);

      fWriter.OpenElement(aTagName, stFinish);
      for I := 0 to aValue.GetArrayLength-1 do
      begin
        xValue := aValue.GetArrayElement(I);
        WriteObjectProperty(SymbolNameToString(@xValue.TypeInfo.Name), xElementType, xValue, False, True);
      end;
      fWriter.CloseElement(aTagName, True);
    end;
  end;
end;

procedure TXMLRTTISerializer.WriteRootEndElement;
begin
  if UseRoot and fRootElementWritten then
    fWriter.CloseElement(fRootNodeName);
end;

procedure TXMLRTTISerializer.WriteRootStartElement;
begin
  if not fRootElementWritten then
  begin
    fXMLDeclaration.WriteIfEnabled(fWriter);

    if UseRoot then
      fWriter.OpenElement(fRootNodeName, stFinish);

    fRootElementWritten := True;
  end;
end;

procedure TXMLRTTISerializer._WriteObjectProperty(const aTagName,
  aValue: OWideString);
begin
  fWriter.OpenElement(aTagName, stFinish);
  fWriter.Text(aValue, False);
  fWriter.CloseElement(aTagName, False);
end;

{ TXMLRTTIDeserializer }

procedure TXMLRTTIDeserializer.CheckRecordForCreateClasses(
  const aInstance: Pointer; const aType: TRttiType; const aItemNode: PXMLNode);
var
  xField: TRttiField;
  xObjectValue: TValue;
  xPropNameIndex: TXMLNodeIndex;
  xClassElement: PXMLNode;
  xRealClassName: string;
begin
  xPropNameIndex := nil;
  try
    for xField in aType.GetFields do
    if xField.FieldType.Handle.Kind = tkClass then
    begin
      //find extra class type from "type" tag
      xClassElement := nil;
      xRealClassName := '';
      if aItemNode.FindChildWithIndex(xField.Name, {%H-}xClassElement, xPropNameIndex) then
        xRealClassName := xClassElement.GetAttribute('type');
      if xRealClassName = '' then
        xRealClassName := xField.FieldType.Name;

      xObjectValue := CreateObject(xRealClassName, xField.FieldType as TRttiInstanceType);
      xField.SetValue(aInstance, xObjectValue);
    end;
  finally
    xPropNameIndex.Free;
  end;
end;

function TXMLRTTIDeserializer.CreateNewValue(const aType: TRttiType;
  aTypeName: string; const aItemNode: PXMLNode;
  var outAllocatedData: Pointer): TValue;
begin
  aTypeName := OXmlXMLToName(aTypeName);
  outAllocatedData := nil;

  case aType.TypeKind of
    tkInteger: Result := TValue.From<Integer>(0);
    tkInt64: Result := TValue.From<Int64>(0);
    tkChar: Result := TValue.From<Char>(#0);
    tkWChar: Result := TValue.From<WideChar>(#0);
    tkFloat:
    begin
      if (aType = System.TypeInfo(TDateTime)) then
        Result := TValue.From<TDateTime>(0)
      else if (aType = System.TypeInfo(TTime)) then
        Result := TValue.From<TTime>(0)
      else if (aType = System.TypeInfo(TDate)) then
        Result := TValue.From<TDate>(0)
      else
        Result := TValue.From<Double>(0);
    end;
    tkString: Result := TValue.From<string>('');
    {$IFDEF O_HASBYTESTRINGS}
    tkWString: Result := TValue.From<WideString>('');
    tkLString: Result := TValue.From<AnsiString>('');
    {$ENDIF}
    tkUString: Result := TValue.From<UnicodeString>('');
    tkClass: Result := CreateObject(aTypeName, aType as TRttiInstanceType);
    tkRecord:
    begin
      outAllocatedData := AllocMem(aType.TypeSize);
      TValue.Make(outAllocatedData, aType.Handle, Result);
      CheckRecordForCreateClasses(Result.GetReferenceToRawData, aType, aItemNode);
    end;
  else
    //error reading
    raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerNotSupportedListItemType, [aType.Name]);
  end;
end;

function TXMLRTTIDeserializer.CreateObject(const aClassName: string; const aType: TRttiInstanceType): TValue;
var
  xCreateClass: TClass;
  xCreateClassType: TRttiType;
  xConstructorI, xConstructorFound: TRttiMethod;
  xConstructorParams: TArray<TRttiParameter>;
begin
  if aClassName <> aType.Name then
  begin
    if not fCreateClasses.TryGetValue(aClassName, xCreateClass) then
      raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerRegisterClass, [aClassName]);
    xCreateClassType := Context.GetType(xCreateClass);
  end else
  begin
    xCreateClassType := aType;
    xCreateClass := TClass(aType.MetaclassType);
  end;

  xConstructorFound := nil;
  for xConstructorI in xCreateClassType.GetMethods do
  if SameText(xConstructorI.Name, 'Create') and
     xConstructorI.IsConstructor
  then begin
    xConstructorParams := xConstructorI.GetParameters;
    if (
      (Length(xConstructorParams) = 0)
       or (
         (Length(xConstructorParams) = 1) and
         (xConstructorParams[0].ParamType.TypeKind = tkClass)
       ))
    then begin
      xConstructorFound := xConstructorI;
      Break;
    end;
  end;
  if not Assigned(xConstructorFound) then
    raise EXMLRTTIDeserializer.CreateFmt(OXmlLng_DeserializerNotSupportedListItemType, [xCreateClassType.Name]);

  case Length(xConstructorParams) of
    0: Result := xConstructorFound.Invoke(xCreateClass, []);
    1: Result := xConstructorFound.Invoke(xCreateClass, [nil]);
  end;
end;

destructor TXMLRTTIDeserializer.Destroy;
begin
  fXMLParser.Free;
  fCreateClasses.Free;

  inherited;
end;

procedure TXMLRTTIDeserializer.DoCreate;
begin
  inherited;

  fXMLParser := TXMLSeqParser.Create;
  fCreateClasses := TDictionary<string,TClass>.Create;
end;

procedure TXMLRTTIDeserializer.DoInit;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

function TXMLRTTIDeserializer.GetApproxStreamPosition: OStreamInt;
begin
  Result := fXMLParser.ApproxStreamPosition;
end;

function TXMLRTTIDeserializer.GetParseError: IOTextParseError;
begin
  Result := fXMLParser.ParseError;
end;

function TXMLRTTIDeserializer.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fXMLParser.ReaderSettings;
end;

function TXMLRTTIDeserializer.GetStreamSize: OStreamInt;
begin
  Result := fXMLParser.StreamSize;
end;

procedure TXMLRTTIDeserializer.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding);
begin
  fXMLParser.InitBuffer(aBuffer, aBufferLength, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitFile(aFileName, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fXMLParser.InitStream(aStream, aForceEncoding);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitXML(const aXML: OWideString);
begin
  fXMLParser.InitXML(aXML);
  DoInit;
end;

procedure TXMLRTTIDeserializer.InitXML_UTF8(const aXML: OUTF8Container);
begin
  fXMLParser.InitXML_UTF8(aXML);
  DoInit;
end;

procedure TXMLRTTIDeserializer.ReadCollectionItems(
  const aCollection: TCollection; const aEnumerationNode: PXMLNode;
  const aChildName: OWideString);
var
  xValue: TValue;
  xItemNode: PXMLNode;
  xItemType: TRttiType;
  xNewItem: TCollectionItem;
  xChildNameId: OHashedStringsIndex;
begin
  aCollection.Clear;

  xChildNameId := aEnumerationNode.OwnerDocument.IndexOfString(aChildName);
  if xChildNameId < 0 then
    Exit;

  xItemType := nil;
  xItemNode := aEnumerationNode.FirstChild;
  while Assigned(xItemNode) do
  begin
    if xItemNode.NodeNameId = xChildNameId then
    begin
      xNewItem := aCollection.Add;
      if not Assigned(xItemType) then//the collection has items only of 1 kind, no need to check all of them
        xItemType := Context.GetRealObjectType<TCollectionItem>(xNewItem);

      xValue := TValue.From(xNewItem);
      ReadObjectPropertyValue(xItemType, xItemNode, xValue);
    end;
    xItemNode := xItemNode.NextSibling;
  end;
end;

procedure TXMLRTTIDeserializer.ReadObject<T>(const aObject: T);
begin
  if not Assigned(fCurrentElementNode) then
    raise EXMLRTTIDeserializer.Create(OXmlLng_WrongDeserializerSequence);

  ReadObjectFromNode<T>(aObject, fCurrentElementNode);

  fCurrentElementNode := nil;
end;

procedure TXMLRTTIDeserializer.ReadObjectEnumeration(const aObject: TObject;
  const aType: TRttiType; const aEnumerationNode: PXMLNode);
var
  xClear, xAdd: TRttiMethod;
  xValue: TValue;
  xItemNode: PXMLNode;
  xItemType: TRttiType;
  xNewValueAllocatedMemory: Pointer;
begin
  xClear := aType.GetMethod('Clear');
  if not Assigned(xClear) then
    Exit;

  xClear.Invoke(aObject, []);//clear the list

  xAdd := aType.GetMethod('Add');
  if not Assigned(xAdd) or
     (Length(xAdd.GetParameters) <> 1)
  then
    Exit;

  xItemType := xAdd.GetParameters[0].ParamType;

  xItemNode := aEnumerationNode.FirstChild;
  while Assigned(xItemNode) do
  begin
    xValue := CreateNewValue(xItemType, xItemNode.NodeName, xItemNode, xNewValueAllocatedMemory);//add whatever the result is!
    ReadObjectPropertyValue(xItemType, xItemNode, xValue);//add whatever the result is!
    xAdd.Invoke(aObject, [xValue]);
    if Assigned(xNewValueAllocatedMemory) then
      FreeMem(xNewValueAllocatedMemory);

    xItemNode := xItemNode.NextSibling;
  end;
end;

procedure TXMLRTTIDeserializer.ReadObjectFromNode<T>(const aObject: T;
  const aElementNode: PXMLNode);
var
  xType: TRttiType;
  xInstance: Pointer;
begin
  xType := Context.GetRealObjectType<T>(aObject);

  case xType.TypeKind of
    tkClass: xInstance := PObject(@aObject)^;
    tkInterface: xInstance := Pointer(PInterface(@aObject)^);
  else
    xInstance := @aObject;
  end;
  ReadObjectProperties(xInstance, xType, aElementNode);
end;

function TXMLRTTIDeserializer.ReadObjectInfo(
  var outElementName: OWideString): Boolean;
var
  xType: OWideString;
begin
  Result := ReadObjectInfo(outElementName, xType);
end;

function TXMLRTTIDeserializer.ReadObjectInfo(var outElementName,
  outType: OWideString): Boolean;
var
  xRootNodeOpen: Boolean;
begin
  if UseRoot and not Assigned(fRootNode) then
  begin
    Result :=
      fXMLParser.ReadNextChildElementHeader({%H-}fRootNode, {%H-}xRootNodeOpen) and//no root element
      xRootNodeOpen;//there are no elements in root

    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;
  end;

  repeat
    Result := fXMLParser.ReadNextChildNode({%H-}fCurrentElementNode);
    if not Result then
    begin
      ReleaseDocument;
      Exit;
    end;

    Result := (fCurrentElementNode.NodeType = ntElement);
  until Result;

  //Result = true here
  outElementName := OXmlXMLToName(fCurrentElementNode.NodeName);
  outType := fCurrentElementNode.GetAttribute('type');
  if outType = '' then
    outType := outElementName;
end;

procedure TXMLRTTIDeserializer.ReadObjectProperties(const aInstance: Pointer;
  const aType: TRttiType;
  const aElementNode: PXMLNode);
var
  xField: TRttiField;
  xProperty: TRttiProperty;
  xEnumerationNode: PXMLNode;
  xPropNameIndex: TXMLNodeIndex;
begin
  xPropNameIndex := nil;
  try
    for xField in aType.GetFields do
    if (xField.Visibility in fVisibility) then
      ReadObjectProperty(aInstance, xField, xField.FieldType, xField.GetValue(aInstance), aElementNode, xPropNameIndex);

    for xProperty in aType.GetProperties do
    if (xProperty.Visibility in fVisibility) and
      (xProperty.IsWritable or xProperty.PropertyType.IsInstance)
    then
      ReadObjectProperty(aInstance, xProperty, xProperty.PropertyType, xProperty.GetValue(aInstance), aElementNode, xPropNameIndex);

    if (aType.TypeKind = tkClass) then
    begin
      if (TObject(aInstance) is TCollection) then
      begin
        case CollectionStyle of
          csOXml:
            if aElementNode.SelectNode('_oxmlcollection', xEnumerationNode) then
              ReadCollectionItems(TCollection(aInstance), xEnumerationNode, 'i');
          csOmniXML:
            ReadCollectionItems(TCollection(aInstance), aElementNode, TCollection(aInstance).ItemClass.ClassName);
        end;
      end else
      if aElementNode.SelectNode('_oxmldefenum', xEnumerationNode) then
        ReadObjectEnumeration(TObject(aInstance), aType, xEnumerationNode);
    end;
  finally
    xPropNameIndex.Free;
  end;
end;

procedure TXMLRTTIDeserializer.ReadObjectProperty(const aInstance: Pointer;
  const aMember: TRttiMember;
  const aType: TRttiType; const aValue: TValue; const aElementNode: PXMLNode;
  var ioPropNameIndex: TXMLNodeIndex);
var
  xPropElement: PXMLNode;
  xNewValue: TValue;
begin
  if not aElementNode.FindChildWithIndex(aMember.Name, {%H-}xPropElement, ioPropNameIndex) then
    Exit;

  xNewValue := aValue;
  if ReadObjectPropertyValue(aType, xPropElement, xNewValue) then
    SetPropertyValue(aInstance, aMember, xNewValue);
end;

function TXMLRTTIDeserializer.ReadObjectPropertyValue(const aType: TRttiType;
  const aElementValueNode: PXMLNode; var ioValue: TValue): Boolean;
begin
  ReadObjectPropertyValue(aType, aElementValueNode, ioValue, Result);
end;

procedure TXMLRTTIDeserializer.RegisterClass(const aClass: TClass);
begin
  fCreateClasses.Add(aClass.ClassName, aClass);
end;

procedure TXMLRTTIDeserializer.ReadObjectPropertyValue(
  aType: TRttiType;
  const aElementValueNode: PXMLNode;
  var ioValue: TValue;
  var outValueDataChanged: Boolean);
var
  xStrValue: OWideString;
  xOrdValue: Integer;
  xFloatValue: Extended;
  xNewValue: TValue;
  xItemType: TRttiType;
  xItemNode: PXMLNode;
  xArrayLength: ONativeInt;
  I: Integer;
  xNewValueAllocatedMemory: Pointer;
begin
  aType := Context.GetRealObjectType(ioValue, aType);

  outValueDataChanged := False;
  case aType.TypeKind of
    tkClass: ReadObjectProperties(ioValue.AsObject, aType, aElementValueNode);
    tkInterface: ReadObjectProperties(Pointer(ioValue.AsInterface), aType, aElementValueNode);
    tkRecord: begin
      ReadObjectProperties(ioValue.GetReferenceToRawData, aType, aElementValueNode);
      outValueDataChanged := True;
    end;
    tkArray, tkDynArray:
    begin
      if aType is TRttiDynamicArrayType then
      begin
        xItemType := TRttiDynamicArrayType(aType).ElementType;
        xArrayLength := aElementValueNode.ChildCount;
        DynArraySetLength(PPointer(ioValue.GetReferenceToRawData)^, ioValue.TypeInfo, 1, @xArrayLength);
      end else
      if aType is TRttiArrayType then
      begin
        xItemType := TRttiArrayType(aType).ElementType;
        xArrayLength := TRttiArrayType(aType).TotalElementCount;
      end else
        raise EXMLRTTISerializer.Create(OXmlLng_RTTIInternalError);

      I := 0;
      xItemNode := aElementValueNode.FirstChild;
      while Assigned(xItemNode) do
      begin
        if I >= xArrayLength then
          Break;

        if aType.TypeKind = tkArray then
        begin
          xNewValue := ioValue.GetArrayElement(I);
          xNewValueAllocatedMemory := nil;
        end else
          xNewValue := CreateNewValue(xItemType, xItemNode.NodeName, xItemNode, xNewValueAllocatedMemory);

        ReadObjectPropertyValue(xItemType, xItemNode, xNewValue);//add whatever the result is!
        ioValue.SetArrayElement(I, xNewValue);//add whatever the result is!
        if Assigned(xNewValueAllocatedMemory) then
          FreeMem(xNewValueAllocatedMemory);

        Inc(I);
        xItemNode := xItemNode.NextSibling;
      end;
      outValueDataChanged := True;
    end;
  else
    xStrValue := aElementValueNode.Text;

    case aType.TypeKind of
      tkInteger, tkChar, tkWChar, tkEnumeration, tkSet:
      begin
        case aType.TypeKind of
          tkInteger: xOrdValue := StrToInt(xStrValue);
          tkChar, tkWChar: xOrdValue := Integer(xStrValue[1]);
          tkEnumeration: xOrdValue := GetEnumValue(aType.Handle, xStrValue);
          tkSet: xOrdValue := StringToSet(aType.Handle, xStrValue);
        else
          xOrdValue := 0;
        end;

        TValue.Make(xOrdValue, aType.Handle, ioValue);
        outValueDataChanged := True;
      end;
      tkString, tkLString, tkUString, tkWString:
      begin
        ioValue := TValue.From<string>(xStrValue);
        outValueDataChanged := True;
      end;
      tkFloat:
      begin
        if (aType = System.TypeInfo(TDateTime)) then
          xFloatValue := ISOStrToDateTime(xStrValue)
        else if (aType = System.TypeInfo(TTime)) then
          xFloatValue := ISOStrToTime(xStrValue)
        else if (aType = System.TypeInfo(TDate)) then
          xFloatValue := ISOStrToDate(xStrValue)
        else
          xFloatValue := ISOStrToFloat(xStrValue);
        ioValue := TValue.From(xFloatValue);
        outValueDataChanged := True;
      end;
    end;
  end;
end;

procedure TXMLRTTIDeserializer.ReleaseDocument;
begin
  fRootNode := nil;
  fCurrentElementNode := nil;
end;

procedure TXMLRTTIDeserializer.SetPropertyValue(const aInstance: Pointer;
  const aMember: TRttiMember; const aNewValue: TValue);
begin
  if aMember is TRttiField then
    TRttiField(aMember).SetValue(aInstance, aNewValue)
  else
  if aMember is TRttiProperty then
    TRttiProperty(aMember).SetValue(aInstance, aNewValue)
end;

procedure TXMLRTTIDeserializer.SetUseRoot(const aUseRoot: Boolean);
begin
  if fXMLParser.ApproxStreamPosition > 0 then
    raise EXMLRTTISerializer.Create(OXmlLng_CannotChangeUseRootDataRead)
  else
    inherited SetUseRoot(aUseRoot);
end;

{ TValueHelper }

function TValueHelper.AsSet: Integer;
begin
  Result := Self.FData.FAsSLong;//get private field hook
end;

{ TRttiContextHelper }

function TRttiContextHelper.GetRealObjectType(const aValue: TValue;
  const aDefType: TRttiType): TRttiType;
var
  xObject: TObject;
begin
  Result := aDefType;
  if aValue.IsObject and (aDefType.TypeKind = tkClass) then
  begin
    xObject := aValue.AsObject;
    if Assigned(xObject) then
      Result := Self.GetType(xObject.ClassType)
  end;
end;

function TRttiContextHelper.GetRealObjectType<T>(
  const aObject: T): TRttiType;
var
  xObject: TObject;
begin
  Result := Self.GetType(TypeInfo(T));//TypeInfo
  if (Result.TypeKind = tkClass) then
  begin
    xObject := PObject(@aObject)^;
    if Assigned(xObject) then
      Result := Self.GetType(xObject.ClassType);
  end;
end;

{ TCustomXMLRTTISerDes }

constructor TCustomXMLRTTISerDes.Create;
begin
  DoCreate;

  inherited Create;
end;

destructor TCustomXMLRTTISerDes.Destroy;
begin
  fContext.Free;

  inherited;
end;

procedure TCustomXMLRTTISerDes.DoCreate;
begin
  fContext := TRttiContext.Create;
  fVisibility := [mvPublic, mvPublished];
  fUseRoot := True;
end;

procedure TCustomXMLRTTISerDes.SetUseRoot(const aUseRoot: Boolean);
begin
  fUseRoot := aUseRoot;
end;

{ TSerializableDictionary<TKey, TValue> }

procedure TSerializableDictionary<TKey, TValue>.Add(const aPair: TPair<TKey, TValue>);
begin
  inherited Add(aPair.Key, aPair.Value);
end;

procedure TSerializableDictionary<TKey, TValue>.Add(const aKey: TKey; aValue: TValue);
begin
  inherited Add(aKey, aValue);
end;

{ TSerializableObjectDictionary<TKey, TValue> }

procedure TSerializableObjectDictionary<TKey, TValue>.Add(
  const aPair: TPair<TKey, TValue>);
begin
  inherited Add(aPair.Key, aPair.Value);
end;

procedure TSerializableObjectDictionary<TKey, TValue>.Add(const aKey: TKey;
  aValue: TValue);
begin
  inherited Add(aKey, aValue);
end;

end.
