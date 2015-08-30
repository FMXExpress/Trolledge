unit OJSON;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OJSON.pas

  !!! Beta version !!!

  TJSONWriter
    - fast sequential JSON writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      and the writer takes care of valid JSON escaping

    - indentation is not supported yet
    - new line handling is not supported yet

  TJSONReader
    - a standalone direct JSON parser
    - new line handling is not supported yet
    - source file must be a valid JSON, there is no error handling
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
  {$ELSE}
  Contnrs,
  {$ENDIF}

  TypInfo,
  OBufferedStreams, OEncoding, OWideSupp, OXmlUtils, OTextReadWrite;

type
  TJSONWriterState = (wsDocument, wsArray, wsObject);
  TJSONWriter = class(TObject)
  private
    fStream: TStream;
    fOwnsStream: Boolean;

    fStateTree: array of TJSONWriterState;
    fStateTreeCurrent: Integer;
    fStateObjectValueAfterPairName: Boolean;
    fNextPairValueNeedsSeparator: Boolean;

    procedure EnterState(const aState: TJSONWriterState);
    procedure ExitState;
    function CheckState(const aState: TJSONWriterState): Boolean;
    procedure CheckWriteValue;
    procedure CheckWritePair;
  private
    procedure WriteString(const aString: string);//MUST BE ASCII
    procedure WriteChar(const aChar: Byte);
    procedure WriteChars(const aChar1, aChar2: Byte);
    procedure WriteText(const aText: OWideString);//escaped text
    procedure WriteTextUnicode(const aText: OUnicodeString);
    procedure WriteTextUTF8(const aText: OUTF8Container);
    procedure Separator;
    procedure WriteObject;
    procedure WriteArray;
    procedure WriteNumber(const aNumber: Integer); overload;
    procedure WriteNumber(const aNumber: Extended); overload;
    procedure WriteBoolean(const aBoolean: Boolean);
    procedure WriteNull;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function OpenArray: TJSONWriter; overload;// "["
    function OpenArray(const aPairName: OWideString): TJSONWriter; overload;
    function OpenArrayUnicode(const aPairName: OUnicodeString): TJSONWriter;
    function OpenArrayUTF8(const aPairName: OUTF8Container): TJSONWriter;
    function CloseArray: TJSONWriter;// "]"

    function OpenObject: TJSONWriter; overload;// "{"
    function OpenObject(const aPairName: OWideString): TJSONWriter; overload;
    function OpenObjectUnicode(const aPairName: OUnicodeString): TJSONWriter;
    function OpenObjectUTF8(const aPairName: OUTF8Container): TJSONWriter;
    function CloseObject: TJSONWriter;// "}"

    procedure PairName(const aString: OWideString);
    procedure PairNameUnicode(const aString: OUnicodeString);
    procedure PairNameUTF8(const aString: OUTF8Container);

    function Text(const aText: OWideString): TJSONWriter; overload;
    function TextUnicode(const aText: OUnicodeString): TJSONWriter; overload;
    function TextUTF8(const aText: OUTF8Container): TJSONWriter; overload;
    function Text(const aPairName, aText: OWideString): TJSONWriter; overload;
    function TextUnicode(const aPairName, aText: OUnicodeString): TJSONWriter; overload;
    function TextUTF8(const aPairName, aText: OUTF8Container): TJSONWriter; overload;

    function Number(const aNumber: Integer): TJSONWriter; overload;
    function Number(const aPairName: OWideString; const aNumber: Integer): TJSONWriter; overload;
    function NumberUnicode(const aPairName: OUnicodeString; const aNumber: Integer): TJSONWriter; overload;
    function NumberUTF8(const aPairName: OUTF8Container; const aNumber: Integer): TJSONWriter; overload;

    function Number(const aNumber: Extended): TJSONWriter; overload;
    function Number(const aPairName: OWideString; const aNumber: Extended): TJSONWriter; overload;
    function NumberUnicode(const aPairName: OUnicodeString; const aNumber: Extended): TJSONWriter; overload;
    function NumberUTF8(const aPairName: OUTF8Container; const aNumber: Extended): TJSONWriter; overload;

    function Boolean(const aBoolean: Boolean): TJSONWriter; overload;
    function Boolean(const aPairName: OWideString; const aBoolean: Boolean): TJSONWriter; overload;
    function BooleanUnicode(const aPairName: OUnicodeString; const aBoolean: Boolean): TJSONWriter;
    function BooleanUTF8(const aPairName: OUTF8Container; const aBoolean: Boolean): TJSONWriter;

    function Null: TJSONWriter; overload;
    function Null(const aPairName: OWideString): TJSONWriter; overload;
    function NullUnicode(const aPairName: OUnicodeString): TJSONWriter;
    function NullUTF8(const aPairName: OUTF8Container): TJSONWriter;
  public
    function BufferSize: OStreamInt;
    procedure AsBuffer(var Buffer);//Buffer must be of size BufferSize
    function AsJSON: OUTF8Container;
    function AsBytes: TBytes;
    function AsEncodingBuffer: TEncodingBuffer;
    function AsString: OWideString;
  end;

  TJSONReader = class;

  TJSONReaderTokenType = (ttOpenObject, ttCloseObject,
    ttOpenArray, ttCloseArray, ttPairName, ttValue, ttSeparator);
  TJSONReaderValueType = (vtString, vtNumber, vtObject, vtArray, vtBoolean, vtNull);

  TRPropList = class(TObject)
  private
    fPropList: PPropList;
    fPropCount: Integer;
  public
    property PropList: PPropList read fPropList;
    property PropCount: Integer read fPropCount;
  public
    constructor Create(const aClass: TClass);
    destructor Destroy; override;
  end;

  TMethodCreateAndReadItem = function(const aSender: TJSONReader; const aPropList: TRPropList): TObject of object;

  TJSONReaderToken = class(TObject)
  private
    fBooleanValue: Boolean;
    fExtendedValue: Extended;
    fPairNameUTF8: OUTF8Container;
    fStringValueUTF8: OUTF8Container;
    fTokenType: TJSONReaderTokenType;
    fValueType: TJSONReaderValueType;

    function GetPairNameUTF8: OUTF8Container;
    function GetPairName: OWideString;

    function GetBooleanValue: Boolean;
    function GetDoubleValue: Double;
    function GetExtendedValue: Extended;
    function GetIntegerValue: Integer;
    function GetInt64Value: Int64;
    function GetStringValueUTF8: OUTF8Container;
    function GetStringValue: OWideString;
  public
    property TokenType: TJSONReaderTokenType read fTokenType;
    property ValueType: TJSONReaderValueType read fValueType;

    property PairNameUTF8: OUTF8Container read GetPairNameUTF8;
    property PairName: OWideString read GetPairName;

    property StringValueUTF8: OUTF8Container read GetStringValueUTF8;
    property StringValue: OWideString read GetStringValue;
    property IntegerValue: Integer read GetIntegerValue;
    property Int64Value: Int64 read GetInt64Value;
    property DoubleValue: Double read GetDoubleValue;
    property ExtendedValue: Extended read GetExtendedValue;
    property BooleanValue: Boolean read GetBooleanValue;
  end;

  TJSONReader = class(TObject)
  {$IFDEF O_GENERICS}
  private type
    TObjectList = TObjectList<TObject>;
  {$ENDIF}
  private
    fReader: TOCustomUTF8Reader;

    fReaderToken: TJSONReaderToken;
    fReadBuffer: TOByteBuffer;

    function CheckRead(const aChars: string): Boolean;
    function ReadNumber(var outNumber: Extended): Boolean;
    function ReadString: Boolean;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream; const aOwnsStream: Boolean = False); overload;
    destructor Destroy; override;
  public
    //The Init* procedures initialize a JSON document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TJSONReader or you call ReleaseDocument!

    //init document from UTF8 file
    procedure InitFile(const aFileName: OWideString);
    //init document from UTF8 file
    procedure InitStream(const aStream: TStream; const aOwnsStream: Boolean = False);
    //init JSON from OWideString
    procedure InitString(const aJSON: OWideString);
    //init JSON from UTF8 string
    procedure InitUTF8Container(const aJSON: OUTF8Container);
    //init document from UTF8 TBytes buffer
    procedure InitBuffer(const aBuffer: TBytes); overload;
    procedure InitBuffer(const aBuffer; const aBufferLength: Integer); overload;

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    property LastToken: TJSONReaderToken read fReaderToken;

    //use ReadNextToken for reading next JSON token
    function ReadNextToken(var outToken: TJSONReaderToken): Boolean;

    function GoToObject(var outToken: TJSONReaderToken): Boolean;
    procedure ReadObject(const aObject: TObject); overload;
    procedure ReadObject(const aObject: TObject; const aPropList: TRPropList); overload;
    procedure ReadObjectOrArrayToObjectList(const aList: TObjectList; const aItemClass: TClass); overload;
    procedure ReadObjectOrArrayToObjectList(const aList: TObjectList; const aItemClass: TClass;
      const aCreateAndReadItem: TMethodCreateAndReadItem); overload;
    procedure ReadObjectProperty(const aObject: TObject; const aPropInfo: PPropInfo);
  end;

  EJSONWriterException = class(Exception);
  EJSONWriterWrongStructure = class(EJSONWriterException);

function OJSONIsWhiteSpaceChar(const aChar: Byte): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OJSONIsNumberChar(const aChar: Byte): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

implementation

uses
  OXmlLng
  {$IFDEF O_DELPHI_2007_DOWN}
  , Controls//definition of TTime and TDate
  {$ENDIF};

function OJSONIsWhiteSpaceChar(const aChar: Byte): Boolean;
begin
  case aChar of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OJSONIsNumberChar(const aChar: Byte): Boolean;
begin
  case aChar of
    Ord('0')..Ord('9'), Ord('e'), Ord('E'), Ord('.'), Ord('+'), Ord('-'): Result := True;
  else
    Result := False;
  end;
end;

{ TRPropList }

constructor TRPropList.Create(const aClass: TClass);
begin
  fPropCount := GetTypeData(aClass.ClassInfo)^.PropCount;
  GetMem(fPropList, PropCount*SizeOf(Pointer));
  GetPropInfos(aClass.ClassInfo, fPropList);
end;

destructor TRPropList.Destroy;
begin
  FreeMem(PropList);
end;

{ TJSONReaderToken }

function TJSONReaderToken.GetBooleanValue: Boolean;
begin
  Assert((fValueType = vtBoolean) and (fTokenType = ttValue));
  Result := fBooleanValue;
end;

function TJSONReaderToken.GetDoubleValue: Double;
begin
  Assert((fValueType = vtNumber) and (fTokenType = ttValue));
  Result := fExtendedValue;
end;

function TJSONReaderToken.GetExtendedValue: Extended;
begin
  Assert((fValueType = vtNumber) and (fTokenType = ttValue));
  Result := fExtendedValue;
end;

function TJSONReaderToken.GetInt64Value: Int64;
var
  xNumber: Double;
begin
  xNumber := GetDoubleValue;
  Assert(Abs(Frac(xNumber)) < 1e-12);//Assert(SameValue(Frac(xNumber)), 0)) <<-- SameValue is not in Delphi 5
  Result := Round(xNumber);
end;

function TJSONReaderToken.GetIntegerValue: Integer;
begin
  Result := GetInt64Value;
end;

function TJSONReaderToken.GetPairName: OWideString;
begin
  Result := OUTF8ContainerToWide(GetPairNameUTF8);
end;

function TJSONReaderToken.GetPairNameUTF8: OUTF8Container;
begin
  Assert(fTokenType = ttPairName);
  Result := fPairNameUTF8;
end;

function TJSONReaderToken.GetStringValue: OWideString;
begin
  Result := OUTF8ContainerToWide(GetStringValueUTF8);
end;

function TJSONReaderToken.GetStringValueUTF8: OUTF8Container;
begin
  Assert((fValueType = vtString) and (fTokenType = ttValue));
  Result := fStringValueUTF8;
end;

{ TJSONReader }

function TJSONReader.CheckRead(const aChars: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aChars) do
  begin
    fReader.IncCurrentChar;
    if fReader.EOF or (fReader.CurrentChar^ <> (Ord(aChars[I]) and $FF)) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

constructor TJSONReader.Create(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  Create;

  InitStream(aStream, aOwnsStream);
end;

constructor TJSONReader.Create;
begin
  inherited Create;

  fReader := nil;
  fReaderToken := TJSONReaderToken.Create;
  fReadBuffer := TOByteBuffer.Create(8);
end;

destructor TJSONReader.Destroy;
begin
  ReleaseDocument;
  fReaderToken.Free;
  fReadBuffer.Free;
  fReader.Free;

  inherited Destroy;
end;

function TJSONReader.GoToObject(var outToken: TJSONReaderToken): Boolean;
begin
  while ReadNextToken(outToken) do
    if outToken.TokenType = ttOpenObject then
    begin
      Result := True;
      Exit;
    end;

  Result := False
end;

procedure TJSONReader.InitBuffer(const aBuffer; const aBufferLength: Integer);
var
  xNewStream: TStream;
begin
  xNewStream := TMemoryStream.Create;

  if aBufferLength > 0 then
    xNewStream.WriteBuffer(aBuffer, aBufferLength);
  xNewStream.Position := 0;

  InitStream(xNewStream, True);
end;

procedure TJSONReader.InitBuffer(const aBuffer: TBytes);
begin
  InitBuffer(aBuffer[0], Length(aBuffer));
end;

procedure TJSONReader.InitFile(const aFileName: OWideString);
begin
  InitStream(TOFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone), True);
end;

procedure TJSONReader.InitUTF8Container(const aJSON: OUTF8Container);
begin
  ReleaseDocument;
  fReader.Free;
  fReader := TOUTF8StringReader.Create(aJSON);
end;

procedure TJSONReader.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  ReleaseDocument;

  fReader.Free;
  fReader := TOUTF8StreamReader.Create;
  TOUTF8StreamReader(fReader).InitStream(aStream, aOwnsStream);
end;

procedure TJSONReader.InitString(const aJSON: OWideString);
begin
  InitUTF8Container(OWideToUTF8Container(aJSON));
end;

function TJSONReader.ReadNextToken(var outToken: TJSONReaderToken): Boolean;
begin
  outToken := fReaderToken;
  Result := not fReader.EOF;
  if not Result then
    Exit;

  while OJSONIsWhiteSpaceChar(fReader.CurrentChar^) and fReader.IncCurrentChar do//jump over spaces
  begin
  end;

  case fReader.CurrentChar^ of
    Ord(','):
    begin
      outToken.fTokenType := ttSeparator;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('{'):
    begin
      outToken.fTokenType := ttOpenObject;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('}'):
    begin
      outToken.fTokenType := ttCloseObject;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('['):
    begin
      outToken.fTokenType := ttOpenArray;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord(']'):
    begin
      outToken.fTokenType := ttCloseArray;
      Result := True;
      fReader.IncCurrentChar;
    end;
    Ord('t'):
    begin
      Result := CheckRead('rue');
      if Result then//true
      begin
        outToken.fTokenType := ttValue;
        outToken.fValueType := vtBoolean;
        outToken.fBooleanValue := True;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('f'):
    begin
      Result := CheckRead('alse');
      if Result then//false
      begin
        outToken.fTokenType := ttValue;
        outToken.fValueType := vtBoolean;
        outToken.fBooleanValue := False;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('n'):
    begin
      Result := CheckRead('ull');
      if Result then//null
      begin
        outToken.fTokenType := ttValue;
        outToken.fValueType := vtNull;
        fReader.IncCurrentChar;
      end;
    end;
    Ord('0')..Ord('9'), Ord('+'), Ord('-'):
    begin
      Result := ReadNumber(outToken.fExtendedValue);
      if Result then
      begin
        outToken.fTokenType := ttValue;
        outToken.fValueType := vtNumber;
      end;
    end;
    Ord('"'):
    begin
      Result := ReadString;
      if Result then
      begin
        fReader.IncCurrentChar;
        while OJSONIsWhiteSpaceChar(fReader.CurrentChar^) and fReader.IncCurrentChar do//jump over spaces
        begin
        end;

        if fReader.CurrentChar^ = Ord(':') then
        begin
          outToken.fTokenType := ttPairName;
          fReadBuffer.GetOUTF8Container(outToken.fPairNameUTF8);
          fReader.IncCurrentChar;
        end else
        begin
          outToken.fTokenType := ttValue;
          outToken.fValueType := vtString;
          fReadBuffer.GetOUTF8Container(outToken.fStringValueUTF8);
        end;
      end;
    end;
  else
    Result := False;//error
  end;
end;

function TJSONReader.ReadNumber(var outNumber: Extended): Boolean;
begin
  fReadBuffer.Clear(False);
  repeat
    fReadBuffer.WriteChar(Char(fReader.CurrentChar^));//MUST BE WriteChar
  until not(fReader.IncCurrentChar and OJSONIsNumberChar(fReader.CurrentChar^));

  Result := ISOTryStrToFloat(fReadBuffer.GetString, outNumber);//MUST BE GetString <-> WriteChar
end;

procedure TJSONReader.ReadObject(const aObject: TObject);
var
  xPropList: TRPropList;
begin
  xPropList := TRPropList.Create(aObject.ClassType);
  try
    ReadObject(aObject, xPropList);
  finally
    xPropList.Free;
  end;
end;

procedure TJSONReader.ReadObject(const aObject: TObject;
  const aPropList: TRPropList);
var
  I: Integer;
  xPropInfo: PPropInfo;
  xNameToken: TJSONReaderToken;
begin
  if LastToken.TokenType = ttOpenObject then
    ReadNextToken(xNameToken{%H-})
  else
    xNameToken := LastToken;

  while (xNameToken.TokenType = ttPairName) do
  begin
    //find property
    for I := 0 to aPropList.PropCount-1 do
    begin
      xPropInfo := aPropList.PropList^[I];
      {$IFDEF O_HASBYTESTRINGS}
      if xPropInfo^.Name = xNameToken.PairNameUTF8 then
      {$ELSE}
      if SymbolNameToString(@xPropInfo^.Name) = xNameToken.PairName then
      {$ENDIF}
      begin
        ReadObjectProperty(aObject, xPropInfo);
        Break;
      end;
    end;

    if not (
      ReadNextToken(xNameToken) and//read separator
      (xNameToken.TokenType = ttSeparator) and
      ReadNextToken(xNameToken))//read next token (pair name / close object)
    then
      Break;
  end;
end;

procedure TJSONReader.ReadObjectOrArrayToObjectList(const aList: TObjectList;
  const aItemClass: TClass);
var
  x: TMethodCreateAndReadItem;
begin
  x := nil;
  ReadObjectOrArrayToObjectList(aList, aItemClass, x);
end;

procedure TJSONReader.ReadObjectOrArrayToObjectList(const aList: TObjectList;
  const aItemClass: TClass; const aCreateAndReadItem: TMethodCreateAndReadItem);
var
  xToken: TJSONReaderToken;
  xIsArray: Boolean;
  xNew: TObject;
  xPropList: TRPropList;
begin
  if not ReadNextToken(xToken{%H-}) then
    Exit;

  xIsArray := xToken.TokenType = ttOpenArray;
  if xIsArray then
    ReadNextToken(xToken);

  if xToken.TokenType <> ttOpenObject then
    Exit;

  xPropList := TRPropList.Create(aItemClass);
  try
    while xToken.TokenType = ttOpenObject do
    begin
      if Assigned(aCreateAndReadItem) then
        xNew := aCreateAndReadItem(Self, xPropList)
      else
      begin
        xNew := aItemClass.Create;
        ReadObject(xNew, xPropList);
      end;
      aList.Add(xNew);

      if not(
        xIsArray and
        ReadNextToken(xToken) and//read separator ','
        (xToken.TokenType = ttSeparator) and
        ReadNextToken(xToken))//read next token if last was separator (expected ttOpenObject)
      then
        Break;
    end;//while
  finally
    xPropList.Free;
  end;
end;

procedure TJSONReader.ReadObjectProperty(const aObject: TObject;
  const aPropInfo: PPropInfo);

  procedure _ReadClass;
  var
    xPropObject: TObject;
  begin
    xPropObject := GetObjectProp(aObject, aPropInfo);
    if Assigned(xPropObject) then
    begin
      ReadObject(xPropObject);

      (*if (xPropObject is TCollection) then
      begin
        case CollectionStyle of
          csOXml:
            if bPropElement.SelectNode('_oxmlcollection', {%H-}xEnumerationNode) then
              ReadCollectionItems(TCollection(xPropObject), xEnumerationNode, 'i');
          csOmniXML:
            ReadCollectionItems(TCollection(xPropObject), bPropElement, TCollection(xPropObject).ItemClass.ClassName);
        end;
      end;*)
    end;
  end;
var
  xPropType: PTypeInfo;
  xStrValue: OWideString;
  xOrdValue: Integer;
  xFloatValue: Double;
  xValueToken: TJSONReaderToken;
begin
  if not Assigned(aPropInfo^.GetProc) then
    Exit;

  xPropType := aPropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};

  if xPropType^.Kind = tkClass then
  begin
    _ReadClass;
  end else
  begin
    if not(ReadNextToken(xValueToken{%H-}) and (xValueToken.TokenType = ttValue)) then
      Exit;

    case xPropType^.Kind of
      {$IFDEF FPC}
      tkBool:
        SetOrdProp(aObject, aPropInfo, Integer(xValueToken.GetBooleanValue));
      {$ENDIF}
      tkInteger, tkEnumeration:
        SetOrdProp(aObject, aPropInfo, xValueToken.GetInt64Value);
      tkChar, tkWChar{$IFDEF FPC}, tkUChar{$ENDIF}:
      begin
        xStrValue := xValueToken.GetStringValue;
        case xPropType^.Kind of
          tkChar {$IFNDEF FPC}, tkWChar{$ENDIF}: xOrdValue := Integer(xStrValue[1]);
          {$IFDEF FPC}tkWChar, tkUChar: xOrdValue := Integer(UTF8Decode(xStrValue)[1]);{$ENDIF}
        else
          xOrdValue := 0;
        end;
        SetOrdProp(aObject, aPropInfo, xOrdValue);
      end;
      tkSet:
        SetSetProp(aObject, aPropInfo, xStrValue);
      tkString, tkLString
      {$IFDEF FPC}, tkAString{$ENDIF}
      {$IFDEF O_DELPHI_5_DOWN}, tkWString{$ENDIF}
      {$IFDEF O_DELPHI_2009_UP}, tkUString{$ENDIF}:
        SetStrProp(aObject, aPropInfo, xValueToken.GetStringValue);
      {$IFDEF O_HASBYTESTRINGS}{$IFNDEF O_DELPHI_5_DOWN}
      tkWString
      {$IFDEF FPC}, tkUString{$ENDIF}:
        SetWideStrProp(aObject, aPropInfo, {$IFDEF FPC}UTF8Decode{$ENDIF}(xValueToken.GetStringValue));
      {$ENDIF}{$ENDIF}
      tkFloat:
      begin
        if (xPropType = System.TypeInfo(TDateTime)) then
          xFloatValue := ISOStrToDateTime(xValueToken.GetStringValue)
        else if (xPropType = System.TypeInfo(TTime)) then
          xFloatValue := ISOStrToTime(xValueToken.GetStringValue)
        else if (xPropType = System.TypeInfo(TDate)) then
          xFloatValue := ISOStrToDate(xValueToken.GetStringValue)
        else
          xFloatValue := xValueToken.GetDoubleValue;
        SetFloatProp(aObject, aPropInfo, xFloatValue);
      end;
      tkInt64:
        SetInt64Prop(aObject, aPropInfo, StrToInt64(xStrValue));
      tkClass:
        _ReadClass;
    end;
  end;
end;

function TJSONReader.ReadString: Boolean;
var
  I: Integer;
  xHex: string;
  xHexC: Integer;
  xHexC_UTF8: OUTF8Container;
begin
  Result := False;

  fReadBuffer.Clear(False);
  while fReader.IncCurrentChar do
  begin
    case fReader.CurrentChar^ of
      Ord('\'):
      begin
        if not fReader.IncCurrentChar then Exit;
        case fReader.CurrentChar^ of
          Ord('\'), Ord('/'), Ord('"'): fReadBuffer.WriteByte(fReader.CurrentChar^);
          Ord('b'): fReadBuffer.WriteByte(8);
          Ord('t'): fReadBuffer.WriteByte(9);
          Ord('n'): fReadBuffer.WriteByte(10);
          Ord('f'): fReadBuffer.WriteByte(12);
          Ord('r'): fReadBuffer.WriteByte(13);
          Ord('u'):
          begin
            SetLength(xHex, 5);
            xHex[1] := '$';
            for I := 2 to Length(xHex) do
            begin
              if not fReader.IncCurrentChar then Exit;
              xHex[I] := Char(fReader.CurrentChar^);
            end;
            if not TryStrToInt(xHex, xHexC) then Exit;
            if (xHexC and $FF00) = 0 then//two-byte character
              fReadBuffer.WriteByte(xHexC and $FF)
            else
            begin//four-byte unicode character
              xHexC_UTF8 := OUnicodeToUTF8Container(OUnicodeChar(xHexC and $FFFF));
              for I := OUTF8Container_FirstElement to Length(xHexC_UTF8)-1+OUTF8Container_FirstElement do
                fReadBuffer.WriteByte(Ord(xHexC_UTF8[I]));
            end;
          end;
        else//not a valid escape
          Exit;//error
        end;
      end;
      Ord('"'): Break;
    else
      fReadBuffer.WriteByte(fReader.CurrentChar^);
    end;//case
  end;//while

  Result := True;
end;

procedure TJSONReader.ReleaseDocument;
begin
  if Assigned(fReader) then
    fReader.ReleaseDocument;
end;

{ TJSONWriter }

procedure TJSONWriter.WriteText(const aText: OWideString);
begin
  {$IFDEF FPC}
  WriteTextUTF8(aText);
  {$ELSE}
  WriteTextUnicode(aText);
  {$ENDIF}
end;

function TJSONWriter.TextUnicode(const aPairName, aText: OUnicodeString
  ): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteTextUnicode(aText);
  Result := Self;
end;

function TJSONWriter.TextUnicode(const aText: OUnicodeString): TJSONWriter;
begin
  CheckWriteValue;

  WriteTextUnicode(aText);
  Result := Self;
end;

procedure TJSONWriter.WriteTextUnicode(const aText: OUnicodeString);
begin
  WriteTextUTF8(OUnicodeToUTF8Container(aText));
end;

function TJSONWriter.TextUTF8(const aPairName, aText: OUTF8Container): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteTextUTF8(aText);
  Result := Self;
end;

function TJSONWriter.TextUTF8(const aText: OUTF8Container): TJSONWriter;
begin
  CheckWriteValue;

  WriteTextUTF8(aText);
  Result := Self;
end;

procedure TJSONWriter.WriteArray;
begin
  EnterState(wsArray);
  WriteChar(Ord('['));
  fStateObjectValueAfterPairName := False;
end;

procedure TJSONWriter.WriteBoolean(const aBoolean: Boolean);
begin
  if aBoolean then
    WriteString('true')
  else
    WriteString('false');
end;

procedure TJSONWriter.WriteTextUTF8(const aText: OUTF8Container);
var
  I: Integer;
  C: PByte;
const
  cHexCharsLower: array[0..$0F] of Byte =
    (Ord('0'), Ord('1'), Ord('2'), Ord('3'), Ord('4'), Ord('5'), Ord('6'), Ord('7'),
     Ord('8'), Ord('9'), Ord('a'), Ord('b'), Ord('c'), Ord('d'), Ord('e'), Ord('f'));
begin
  WriteChar(Ord('"'));
  if Length(aText) > 0 then
  begin
    C := @aText[OUTF8Container_FirstElement];
    for I := 0 to Length(aText)-1 do
    begin
      case C^ of
        Ord('"'), Ord('\'): WriteChars(Ord('\'), C^);
        8: WriteChars(Ord('\'), Ord('b'));
        9: WriteChars(Ord('\'), Ord('t'));
        10: WriteChars(Ord('\'), Ord('n'));
        12: WriteChars(Ord('\'), Ord('f'));
        13: WriteChars(Ord('\'), Ord('r'));
        0..7, 11, 14..31:
        begin
          WriteString('\u00');
          WriteChars(cHexCharsLower[C^ shr 4], cHexCharsLower[C^ and $0F]);
        end;
      else
        WriteChar(C^);
      end;
      Inc(C);
    end;
  end;
  WriteChar(Ord('"'));
  fStateObjectValueAfterPairName := False;
end;

procedure TJSONWriter.AsBuffer(var Buffer);
var
  xPos: OStreamInt;
begin
  if BufferSize > 0 then
  begin
    xPos := fStream.Position;
    fStream.Position := 0;
    fStream.Read(Buffer, BufferSize);
    fStream.Position := xPos;
  end;
end;

function TJSONWriter.AsBytes: TBytes;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[0]);
end;

function TJSONWriter.AsEncodingBuffer: TEncodingBuffer;
begin
  {$IFDEF O_DELPHI_2009_UP}
  Result := AsBytes;
  {$ELSE}
  Result := AsJSON;
  {$ENDIF}
end;

function TJSONWriter.AsJSON: OUTF8Container;
begin
  SetLength(Result, BufferSize);
  if BufferSize > 0 then
    AsBuffer(Result[OUTF8Container_FirstElement]);
end;

function TJSONWriter.AsString: OWideString;
begin
  Result := TEncoding.UTF8.BufferToString(AsEncodingBuffer);
end;

function TJSONWriter.OpenArray: TJSONWriter;
begin
  CheckWriteValue;
  WriteArray;
  Result := Self;
end;

function TJSONWriter.OpenArrayUnicode(const aPairName: OUnicodeString
  ): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteArray;
  Result := Self;
end;

function TJSONWriter.OpenArrayUTF8(const aPairName: OUTF8Container): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteArray;
  Result := Self;
end;

function TJSONWriter.OpenObject(const aPairName: OWideString): TJSONWriter;
begin
  PairName(aPairName);
  WriteObject;
  Result := Self;
end;

function TJSONWriter.OpenObject: TJSONWriter;
begin
  CheckWriteValue;
  WriteObject;
  Result := Self;
end;

function TJSONWriter.OpenObjectUnicode(const aPairName: OUnicodeString
  ): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteObject;
  Result := Self;
end;

function TJSONWriter.OpenObjectUTF8(const aPairName: OUTF8Container): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteObject;
  Result := Self;
end;

function TJSONWriter.Boolean(const aBoolean: Boolean): TJSONWriter;
begin
  CheckWriteValue;
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TJSONWriter.Boolean(const aPairName: OWideString;
  const aBoolean: Boolean): TJSONWriter;
begin
  PairName(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TJSONWriter.BooleanUnicode(const aPairName: OUnicodeString;
  const aBoolean: Boolean): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TJSONWriter.BooleanUTF8(const aPairName: OUTF8Container;
  const aBoolean: Boolean): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteBoolean(aBoolean);
  Result := Self;
end;

function TJSONWriter.BufferSize: OStreamInt;
begin
  Result := fStream.Size;
end;

function TJSONWriter.CheckState(const aState: TJSONWriterState): Boolean;
begin
  Result := (fStateTree[fStateTreeCurrent] = aState);
end;

procedure TJSONWriter.CheckWritePair;
begin
  if not (CheckState(wsObject) and not fStateObjectValueAfterPairName)//write pair only in object
  then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotWritePair);

  if fNextPairValueNeedsSeparator then
    Separator;

  fNextPairValueNeedsSeparator := True;
end;

procedure TJSONWriter.CheckWriteValue;
begin
  if not(
     (CheckState(wsObject) and fStateObjectValueAfterPairName) or
      CheckState(wsArray) or//write value in an array
     (CheckState(wsDocument) and not fNextPairValueNeedsSeparator))//write value at the beginning of a document
  then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotWriteValue);

  if fNextPairValueNeedsSeparator and not fStateObjectValueAfterPairName then
    Separator;

  fNextPairValueNeedsSeparator := True;
end;

constructor TJSONWriter.Create;
begin
  inherited Create;

  fStream := TMemoryStream.Create;
  fOwnsStream := True;
  SetLength(fStateTree, 1);
  fStateTree[0] := wsDocument;
end;

destructor TJSONWriter.Destroy;
begin
  if fOwnsStream then
    fStream.Free;

  inherited Destroy;
end;

function TJSONWriter.CloseArray: TJSONWriter;
begin
  if not CheckState(wsArray) then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotCloseArray);

  ExitState;
  WriteChar(Ord(']'));
  Result := Self;
end;

function TJSONWriter.CloseObject: TJSONWriter;
begin
  if not CheckState(wsObject) then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CannotCloseObject);

  ExitState;
  WriteChar(Ord('}'));
  Result := Self;
end;

procedure TJSONWriter.EnterState(const aState: TJSONWriterState);
begin
  if High(fStateTree) = fStateTreeCurrent then
    SetLength(fStateTree, Length(fStateTree)*2);

  Inc(fStateTreeCurrent);
  fStateTree[fStateTreeCurrent] := aState;
  fNextPairValueNeedsSeparator := False;
end;

procedure TJSONWriter.ExitState;
begin
  if fStateTreeCurrent = 0 then
    raise EJSONWriterWrongStructure.Create(OXmlLng_JSON_CloseTooMany);

  Dec(fStateTreeCurrent);
  fNextPairValueNeedsSeparator := True;
end;

function TJSONWriter.Null(const aPairName: OWideString): TJSONWriter;
begin
  PairName(aPairName);
  WriteNull;
  Result := Self;
end;

procedure TJSONWriter.PairName(const aString: OWideString);
begin
  CheckWritePair;

  WriteText(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TJSONWriter.PairNameUnicode(const aString: OUnicodeString);
begin
  CheckWritePair;

  WriteTextUnicode(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TJSONWriter.PairNameUTF8(const aString: OUTF8Container);
begin
  CheckWritePair;

  WriteTextUTF8(aString);
  WriteChars(Ord(':'), 32);
  fStateObjectValueAfterPairName := True;
end;

procedure TJSONWriter.Separator;
begin
  WriteChars(Ord(','), 32);
end;

function TJSONWriter.Text(const aPairName, aText: OWideString): TJSONWriter;
begin
  PairName(aPairName);
  WriteText(aText);
  Result := Self;
end;

function TJSONWriter.Text(const aText: OWideString): TJSONWriter;
begin
  CheckWriteValue;
  WriteText(aText);
  Result := Self;
end;

function TJSONWriter.Null: TJSONWriter;
begin
  CheckWriteValue;
  WriteNull;
  Result := Self;
end;

function TJSONWriter.NullUnicode(const aPairName: OUnicodeString): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNull;
  Result := Self;
end;

function TJSONWriter.NullUTF8(const aPairName: OUTF8Container): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNull;
  Result := Self;
end;

function TJSONWriter.Number(const aNumber: Extended): TJSONWriter;
begin
  CheckWriteValue;
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.Number(const aNumber: Integer): TJSONWriter;
begin
  CheckWriteValue;
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.Number(const aPairName: OWideString;
  const aNumber: Extended): TJSONWriter;
begin
  PairName(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.Number(const aPairName: OWideString; const aNumber: Integer
  ): TJSONWriter;
begin
  PairName(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.NumberUnicode(const aPairName: OUnicodeString;
  const aNumber: Extended): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.NumberUnicode(const aPairName: OUnicodeString;
  const aNumber: Integer): TJSONWriter;
begin
  PairNameUnicode(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.NumberUTF8(const aPairName: OUTF8Container;
  const aNumber: Extended): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.NumberUTF8(const aPairName: OUTF8Container;
  const aNumber: Integer): TJSONWriter;
begin
  PairNameUTF8(aPairName);
  WriteNumber(aNumber);
  Result := Self;
end;

function TJSONWriter.OpenArray(const aPairName: OWideString): TJSONWriter;
begin
  PairName(aPairName);
  WriteArray;
  Result := Self;
end;

procedure TJSONWriter.WriteChars(const aChar1, aChar2: Byte);
begin
  WriteChar(aChar1);
  WriteChar(aChar2);
end;

procedure TJSONWriter.WriteNull;
begin
  WriteString('null');
end;

procedure TJSONWriter.WriteNumber(const aNumber: Extended);
begin
  WriteString(ISOFloatToStr(aNumber));
end;

procedure TJSONWriter.WriteNumber(const aNumber: Integer);
begin
  WriteString(IntToStr(aNumber));
end;

procedure TJSONWriter.WriteObject;
begin
  EnterState(wsObject);
  WriteChar(Ord('{'));
  fStateObjectValueAfterPairName := False;
end;

procedure TJSONWriter.WriteString(const aString: string);
var
  I: Integer;
begin
  for I := 1 to Length(aString) do
    WriteChar(Ord(aString[I]) and $FF);
  fStateObjectValueAfterPairName := False;
end;

procedure TJSONWriter.WriteChar(const aChar: Byte);
begin
  fStream.WriteBuffer(aChar, SizeOf(aChar));
end;

end.

