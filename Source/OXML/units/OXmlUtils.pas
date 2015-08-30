unit OXmlUtils;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlUtils.pas

  Collection of types and methods for XML.
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

  OWideSupp, OEncoding;

type
  TXMLNodeType = (ntDocument, ntDocType, ntXMLDeclaration, ntElement,
    ntAttribute, ntText, ntEntityReference, ntCData, ntComment,
    ntProcessingInstruction);

  EXMLDOMException = class(Exception);

  TXMLIndentType = (itNone, itFlat, itIndent);
  TXMLWhiteSpaceHandling = (wsTrim, wsPreserveAll, wsPreserveInTextOnly, wsAutoTag);
  //brNone...read through all nodes
  //brAfterDocumentElement...stop after first root node
  //brAfterDocumentElementReleaseDocument...brAfterDocumentElement + release document
  TXMLBreakReading = (brNone, brAfterDocumentElement);
  TXMLLineBreak = (lbLF, lbCR, lbCRLF, lbDoNotProcess);
  TXMLChildType = (ctChild, ctAttribute);
  //wsInherit: inherit from parent element
  //wsPreserve: preserve white space
  //wsDefault: default handlign (do not preserve)
  TXMLPreserveWhiteSpace = (pwInherit, pwPreserve, pwDefault);
  TXMLCharKind =
    (ckTab, ckNewLine10, ckNewLine13, ckSingleQuote, ckDoubleQuote, ckAmpersand,
     ckLowerThan, ckGreaterThan, ckSquareBracketOpen, ckSquareBracketClose,
     ckCharacter, ckInvalid);

  //csOXml:    <MyCollection><_oxmlcollection><i>...</i><i>...</i></_oxmlcollection></MyCollection>
  //csOmniXML: <MyCollection><TColItem>...</TColItem><TColItem>...</TColItem></MyCollection>
  TXMLSerializeCollectionStyle = (csOXml, csOmniXML);

  {$IFDEF O_GENERICARRAY}
  TXMLIntArray = TArray<Integer>;
  {$ELSE}
  TXMLIntArray = array of Integer;
  {$ENDIF}

const
  {$IFDEF MSWINDOWS}
  XMLDefaultLineBreak = lbCRLF;
  {$ELSE}
  XMLDefaultLineBreak = lbLF;
  {$ENDIF}
  XML_XML: OWideString = 'xml';
  XML_XMLNS: OWideString = 'xmlns';
  XML_XML_SPACE: OWideString = 'xml:space';

  XML_ENTITY_AMP: OWideString = '&amp;';
  XML_ENTITY_LT: OWideString = '&lt;';
  XML_ENTITY_GT: OWideString = '&gt;';
  XML_ENTITY_QUOT: OWideString = '&quot;';
  XML_ENTITY_APOS: OWideString = '&apos;';
  XML_ENTITY_TAB: OWideString = '&#9;';
  XML_ENTITY_LF: OWideString = '&#xA;';
  XML_ENTITY_CR: OWideString = '&#xD;';
  XML_BOOLEAN_TRUE: string = 'true';
  XML_BOOLEAN_FALSE: string = 'false';

  XMLLineBreak: array[TXMLLineBreak] of OWideString = (#10, #13, #13#10, sLineBreak);

  XMLUseIndexNodeLimit = 256;

  // W3C DOM Level 1 :: http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html
  // index or size is negative, or greater than the allowed value
  INDEX_SIZE_ERR = 1;
  // the specified range of text does not fit into a DOMString
  DOMSTRING_SIZE_ERR = 2;
  // any node is inserted somewhere it doesn't belong
  HIERARCHY_REQUEST_ERR = 3;
  // a node is used in a different document than the one that created it (that doesn't support it)
  WRONG_DOCUMENT_ERR = 4;
  // an invalid character is specified, such as in a name
  INVALID_CHARACTER_ERR = 5;
  // data is specified for a node which does not support data
  NO_DATA_ALLOWED_ERR = 6;
  // an attempt is made to modify an object where modifications are not allowed
  NO_MODIFICATION_ALLOWED_ERR = 7;
  // an attempt was made to reference a node in a context where it does not exist
  NOT_FOUND_ERR = 8;
  // the implementation does not support the type of object requested
  NOT_SUPPORTED_ERR = 9;
  // an attempt is made to add an attribute that is already in use elsewhere
  INUSE_ATTRIBUTE_ERR = 10;
type

  //virtual MS above some custom buffer (may be string, array of byte etc.)
  //  MUST BE TCustomMemoryStream -> SO THAT THE MEMORY POINTER WOULD NOT GET DESTROYED IN .Destroy!!!
  TVirtualMemoryStream = class(TCustomMemoryStream)
  public
    procedure SetPointer(aPtr: Pointer; const aSize: Longint); reintroduce;//public
    function Write(const {%H-}Buffer; {%H-}Count: Longint): Longint; override;
  public
    procedure SetString(const aString: OWideString);
    procedure SetString_UTF8(const aString: OUTF8Container);
    procedure SetBuffer(const aBuffer: TBytes);
    procedure SetEncodingBuffer(const aBuffer: TEncodingBuffer);
  end;

  TXMLDeclaration = class(TObject)
  private
    fEncoding: Boolean;
    fVersion: OWideString;
    fStandAlone: OWideString;
  public
    constructor Create; virtual;
  public
    //encoding attribute
    property Encoding: Boolean read fEncoding write fEncoding;
    //version attribute (1.0)
    property Version: OWideString read fVersion write fVersion;
    //standalone attribute (yes/no/*empty string*)
    property StandAlone: OWideString read fStandAlone write fStandAlone;
  end;

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsNameChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsDecimalChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsSignChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsBreakChar(const aChar: OWideChar): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsChar(const aChar: OWideChar): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsChar(const aChar: Integer): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlCharKind(const aChar: OWideChar): TXMLCharKind; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
{$IFDEF O_UTF8}
function OXmlCharKind(const aChar: OUnicodeChar): TXMLCharKind; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
{$ENDIF}

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsWhiteSpace(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlIsNumber(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlValidEntityReference(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidCData(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidComment(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidPIContent(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlValidName(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlValidChars(const aText: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

function OXmlPreserveToStr(const aPreserveWhiteSpace: TXMLPreserveWhiteSpace): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OXmlStrToPreserve(const aStr: OWideString): TXMLPreserveWhiteSpace; {$IFDEF O_INLINE}inline;{$ENDIF}

procedure OXmlResolveNameSpace(const aNodeName: OWideString; var outNameSpacePrefix, outLocalName: OWideString);
function OXmlCheckNameSpace(const aNodeName, aNameSpacePrefix: OWideString; var outLocalName: OWideString): Boolean;
function OXmlApplyNameSpace(const aNameSpacePrefix, aLocalName: OWideString): OWideString;
function OXmlIsLocalName(const aNodeName, aLocalName: OWideString): Boolean;
function OXmlIsLocalNameNS(const aNodeName, aNameSpace, aLocalName: OWideString): Boolean;

//all ISO formatting functions are thread-safe
function ISOBoolToStr(const aValue: Boolean; const aWritten: Boolean = False): string; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOFloatToStr(const aValue: Extended): string; {$IFDEF FPC}inline;{$ENDIF}//MUST NOT BE INLINE FOR DELPHI
function ISODateToStr(const aDate: TDateTime): string; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISODateTimeToStr(const aDateTime: TDateTime): string; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOTimeToStr(const aTime: TDateTime): string; {$IFDEF O_INLINE}inline;{$ENDIF}

function ISOStrToBool(const aString: string): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToInt(const aString: string): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToFloat(const aString: string): Extended; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToDate(const aString: string): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToDateTime(const aString: string): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToTime(const aString: string): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}

function ISOStrToBoolDef(const aString: string; const aDefValue: Boolean): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToIntDef(const aString: string; const aDefValue: Integer): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToFloatDef(const aString: string; const aDefValue: Extended): Extended; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToDateDef(const aString: string; const aDefDate: TDateTime): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToDateTimeDef(const aString: string; const aDefDateTime: TDateTime): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOStrToTimeDef(const aString: string; const aDefTime: TDateTime): TDateTime; {$IFDEF O_INLINE}inline;{$ENDIF}

function ISOTryStrToBool(const aString: string; var outValue: Boolean): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOTryStrToInt(const aString: string; var outValue: Integer): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOTryStrToFloat(const aString: string; var outValue: Extended): Boolean; overload; {$IFDEF FPC}inline;{$ENDIF}//MUST NOT BE INLINE FOR DELPHI
{$IFDEF O_EXTENDEDTYPE}
function ISOTryStrToFloat(const aString: string; var outValue: Double): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
{$ENDIF}
function ISOTryStrToDate(const aString: string; var outDate: TDateTime): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOTryStrToDateTime(const aString: string; var outDateTime: TDateTime): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function ISOTryStrToTime(const aString: string; var outTime: TDateTime): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}

{$IFDEF O_DELPHI_5_DOWN}
//Delphi 5 compatibility functions
function TryStrToInt(const aStr: string; var outValue: Integer): Boolean; overload;
function TryStrToFloat(const aStr: string; var outValue: Extended): Boolean; overload;
function TryStrToFloat(const aStr: string; var outValue: Double): Boolean; overload;
function TryEncodeDate(aYear, aMonth, aDay: Word; var outDate: TDateTime): Boolean;
function TryEncodeTime(aHour, aMin, aSec, aMSec: Word; var outTime: TDateTime): Boolean;
{$ENDIF}
{$IFDEF O_TRYENCODEDATETIME}
function TryEncodeDateTime(aYear, aMonth, aDay, aHour, aMin, aSec,
  aMSec: Word; var outValue: TDateTime): Boolean;
{$ENDIF}

function SymbolNameToString(const aShortStringPointer: PByte): OWideString;

//solves problem with generic names TType<A,B> -> TType_-A-B-_
function OXmlNameToXML(const aName: string): string;
function OXmlXMLToName(const aXMLName: string): string;

function XMLLineBreakEntity(const aLineBreak: TXMLLineBreak): OWideString;

implementation

uses
  OXmlLng
  {$IFDEF O_DELPHI_6_UP}, DateUtils{$ENDIF};

{$IFNDEF O_DELPHI_6_DOWN}
var
  gxISOFormatSettings: TFormatSettings;//write only once in initialization section, then read-only => thread-safe!
{$ENDIF}

function XMLLineBreakEntity(const aLineBreak: TXMLLineBreak): OWideString;
begin
  case aLineBreak of
    lbLF: Result := XML_ENTITY_LF;
    lbCR: Result := XML_ENTITY_CR;
    lbCRLF: Result := XML_ENTITY_CR + XML_ENTITY_LF;
  else
    //lbDoNotProcess
    {$IFDEF MSWINDOWS}
    Result := XML_ENTITY_CR + XML_ENTITY_LF
    {$ELSE}
    Result := XML_ENTITY_LF
    {$ENDIF}
  end;
end;

function OXmlNameToXML(const aName: string): string;
begin
  Result := aName;
  Result := StringReplace(Result, '<', '_-', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '-_', [rfReplaceAll]);
  Result := StringReplace(Result, ',', '-', [rfReplaceAll]);
end;

function OXmlXMLToName(const aXMLName: string): string;
begin
  Result := aXMLName;
  Result := StringReplace(Result, '_-', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '-_', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '-', ',', [rfReplaceAll]);
end;

function SymbolNameToString(const aShortStringPointer: PByte): OWideString;
var
  xShortStringLength: Byte;
  xShortString: TEncodingBuffer;
begin
  if not Assigned(aShortStringPointer) then
    Result := ''
  else
  begin
    xShortStringLength := aShortStringPointer^;
    if xShortStringLength = 0 then
      Result := ''
    else
    begin
      SetLength(xShortString, xShortStringLength);
      Move({%H-}PByte({%H-}ONativeUInt(aShortStringPointer)+1)^, xShortString[TEncodingBuffer_FirstElement], xShortStringLength);

      {$IFDEF O_UNICODE}
      TEncoding.ASCII.BufferToString(xShortString, Result{%H-});
      {$ELSE}
      Result := xShortString;
      {$ENDIF}
    end;
  end;
end;

function ISOBoolToStr(const aValue: Boolean; const aWritten: Boolean = False): string;
begin
  if aWritten then
  begin
    if aValue then
      Result := XML_BOOLEAN_TRUE
    else
      Result := XML_BOOLEAN_FALSE;
  end else
  begin
    if aValue then
      Result := '1'
    else
      Result := '0';
  end;
end;

function ISOFloatToStr(const aValue: Extended): string;
{$IFDEF O_DELPHI_6_DOWN}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF O_DELPHI_6_DOWN}
  Result := FloatToStr(aValue);
  if DecimalSeparator <> '.' then
  begin
    I := Pos(DecimalSeparator, Result);
    if I > 0 then
      Result[I] := '.';
  end;
  {$ELSE}
  Result := FloatToStr(aValue, gxISOFormatSettings);
  {$ENDIF}
end;

function ISODateToStr(const aDate: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', aDate);
end;

function ISODateTimeToStr(const aDateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', aDateTime)
end;

function ISOTimeToStr(const aTime: TDateTime): string;
begin
  Result := FormatDateTime('hh:nn:ss', aTime);
end;

function ISOStrToBool(const aString: string): Boolean;
begin
  Result := ISOStrToBoolDef(aString, False);
end;

function ISOStrToInt(const aString: string): Integer;
begin
  Result := ISOStrToIntDef(aString, 0);
end;

function ISOStrToFloat(const aString: string): Extended;
begin
  Result := ISOStrToFloatDef(aString, 0);
end;

function ISOStrToDate(const aString: string): TDateTime;
begin
  Result := ISOStrToDateDef(aString, 0);
end;

function ISOStrToDateTime(const aString: string): TDateTime;
begin
  Result := ISOStrToDateTimeDef(aString, 0);
end;

function ISOStrToTime(const aString: string): TDateTime;
begin
  Result := ISOStrToTimeDef(aString, 0);
end;

function ISOStrToBoolDef(const aString: string; const aDefValue: Boolean): Boolean;
begin
  if not ISOTryStrToBool(aString, Result{%H-}) then
    Result := aDefValue;
end;

function ISOStrToIntDef(const aString: string; const aDefValue: Integer): Integer;
begin
  if not ISOTryStrToInt(aString, Result{%H-}) then
    Result := aDefValue;
end;

function ISOStrToFloatDef(const aString: string; const aDefValue: Extended): Extended;
begin
  if not ISOTryStrToFloat(aString, Result{%H-}) then
    Result := aDefValue;
end;

function ISOStrToDateDef(const aString: string; const aDefDate: TDateTime): TDateTime;
begin
  if not ISOTryStrToDate(aString, Result{%H-}) then
    Result := aDefDate;
end;

function ISOStrToDateTimeDef(const aString: string; const aDefDateTime: TDateTime): TDateTime;
begin
  if not ISOTryStrToDateTime(aString, Result{%H-}) then
    Result := aDefDateTime;
end;

function ISOStrToTimeDef(const aString: string; const aDefTime: TDateTime): TDateTime;
begin
  if not ISOTryStrToTime(aString, Result{%H-}) then
    Result := aDefTime;
end;

function ISOTryStrToBool(const aString: string; var outValue: Boolean): Boolean;
begin
  case Length(aString) of
    1: begin
      Result := True;
      case aString[1] of
        '1': outValue := True;
        '0': outValue := False;
      else
        Result := False;
      end;
    end;
    4: begin//true
      Result := CompareText(aString, XML_BOOLEAN_TRUE) = 0;
      if Result then
        outValue := True;
    end;
    5: begin//false
      Result := CompareText(aString, XML_BOOLEAN_FALSE) = 0;
      if Result then
        outValue := False;
    end;
  else
    Result := False;
  end;
end;

function ISOTryStrToInt(const aString: string; var outValue: Integer): Boolean;
begin
  Result := TryStrToInt(aString, outValue);
end;

function ISOTryStrToFloat(const aString: string; var outValue: Extended): Boolean;
{$IFDEF O_DELPHI_6_DOWN}
var
  xString: string;
{$ENDIF}
begin
  {$IFDEF O_DELPHI_6_DOWN}
  if DecimalSeparator <> '.' then
  begin
    xString := StringReplace(aString, '.', DecimalSeparator, []);
    Result := TryStrToFloat(xString, outValue);
  end else
    Result := TryStrToFloat(aString, outValue);
  {$ELSE}
  Result := TryStrToFloat(aString, outValue, gxISOFormatSettings);
  {$ENDIF}
end;

{$IFDEF O_EXTENDEDTYPE}
function ISOTryStrToFloat(const aString: string; var outValue: Double): Boolean;
var
  xValue: Extended;
begin
  Result := ISOTryStrToFloat(aString, xValue{%H-});
  if Result then
    outValue := xValue;
end;
{$ENDIF}

function ISOTryStrToDate(const aString: string; var outDate: TDateTime): Boolean;
var
  xYear, xMonth, xDay: Integer;
begin
  xYear := StrToIntDef(Copy(aString, 1, 4), 0);
  xMonth := StrToIntDef(Copy(aString, 6, 2), 0);
  xDay := StrToIntDef(Copy(aString, 9, 2), 0);

  Result := TryEncodeDate(xYear, xMonth, xDay, outDate);
  if not Result then
    outDate := 0;
end;

function ISOTryStrToDateTime(const aString: string; var outDateTime: TDateTime): Boolean;
var
  xYear, xMonth, xDay, xHour, xMinute, xSecond: Integer;
begin
  xYear := StrToIntDef(Copy(aString, 1, 4), 0);
  xMonth := StrToIntDef(Copy(aString, 6, 2), 0);
  xDay := StrToIntDef(Copy(aString, 9, 2), 0);

  xHour := StrToIntDef(Copy(aString, 12, 2), 0);
  xMinute := StrToIntDef(Copy(aString, 15, 2), 0);
  xSecond := StrToIntDef(Copy(aString, 18, 2), 0);

  Result := TryEncodeDateTime(xYear, xMonth, xDay, xHour, xMinute, xSecond, 0, outDateTime);
  if not Result then
    outDateTime := 0;
end;

function ISOTryStrToTime(const aString: string; var outTime: TDateTime): Boolean;
var
  xHour, xMinute, xSecond: Integer;
begin
  xHour := StrToIntDef(Copy(aString, 1, 2), 0);
  xMinute := StrToIntDef(Copy(aString, 4, 2), 0);
  xSecond := StrToIntDef(Copy(aString, 7, 2), 0);

  Result := TryEncodeTime(xHour, xMinute, xSecond, 0, outTime);
  if not Result then
    outTime := 0;
end;

procedure OXmlResolveNameSpace(const aNodeName: OWideString; var outNameSpacePrefix, outLocalName: OWideString);
var
  xPos: Integer;
begin
  xPos := Pos(':', aNodeName);
  if xPos >= 0 then
  begin
    outNameSpacePrefix := Copy(aNodeName, 1, xPos-1);
    outLocalName := Copy(aNodeName, xPos+1, High(Integer));
  end else
  begin
    outNameSpacePrefix := '';
    outLocalName := aNodeName;
  end;
end;

function OXmlCheckNameSpace(const aNodeName, aNameSpacePrefix: OWideString; var outLocalName: OWideString): Boolean;
begin
  Result := (Length(aNodeName) >= Length(aNameSpacePrefix)+1);
  if not Result then
  begin
    outLocalName := '';
    Exit;
  end;

  if Length(aNameSpacePrefix) > 0 then
  begin
    Result := CompareMem(POWideChar(aNodeName), POWideChar(aNameSpacePrefix), Length(aNameSpacePrefix)*SizeOf(OWideChar));
    if not Result then
    begin
      outLocalName := '';
      Exit;
    end;
  end;

  Result := (aNodeName[Length(aNameSpacePrefix)+1] = ':');
  if Result then
    outLocalName := Copy(aNodeName, Length(aNameSpacePrefix)+2, High(Integer));
end;

function OXmlApplyNameSpace(const aNameSpacePrefix, aLocalName: OWideString): OWideString;
begin
  if (aNameSpacePrefix <> '') and (aLocalName <> '') then
    Result := aNameSpacePrefix+':'+aLocalName
  else if aLocalName <> '' then
    Result := aLocalName
  else if aNameSpacePrefix <> '' then//must be here for OXmlApplyNameSpace('xmlns', '') -> must return "xmlns"
    Result := aNameSpacePrefix
  else
    Result := '';
end;

function OXmlIsLocalName(const aNodeName, aLocalName: OWideString): Boolean;
var
  xLengthNodeName, xLengthLocalName: Integer;
begin
  xLengthLocalName := Length(aLocalName);
  xLengthNodeName := Length(aNodeName);

  Result :=
    (xLengthNodeName > xLengthLocalName) and
    CompareMem(
      @(aLocalName[1]),
      @(aNodeName[1+xLengthNodeName-xLengthLocalName]),
      xLengthLocalName*SizeOf(OWideChar))
    and
      ((xLengthLocalName = xLengthNodeName) or
      (aNodeName[xLengthNodeName-xLengthLocalName] = ':'));
end;

function OXmlIsLocalNameNS(const aNodeName, aNameSpace, aLocalName: OWideString): Boolean;
var
  xLengthNodeName, xLengthLocalName, xLengthNameSpace: Integer;
begin
  xLengthNameSpace := Length(aNameSpace);
  xLengthLocalName := Length(aLocalName);
  xLengthNodeName := Length(aNodeName);

  if xLengthNameSpace = 0 then
  begin
    Result := (aNodeName = aLocalName);
  end else
  begin
    Result :=
      (xLengthNodeName = (xLengthLocalName+xLengthNameSpace+1)) and
      CompareMem(
        @(aLocalName[1]),
        @(aNodeName[2+xLengthNameSpace]),
        xLengthLocalName*SizeOf(OWideChar)) and
      CompareMem(
        @(aNameSpace[1]),
        @(aNodeName[1]),
        xLengthNameSpace*SizeOf(OWideChar)) and
      (aNodeName[1+xLengthNameSpace] = ':');
  end;
end;

function OXmlStrToPreserve(const aStr: OWideString): TXMLPreserveWhiteSpace;
begin
  if (aStr <> '') and ((aStr[1] = 'p') or (aStr[1] = 'P')) then//preserve = true
    Result := pwPreserve
  else
    Result := pwDefault;
end;

function OXmlPreserveToStr(const aPreserveWhiteSpace: TXMLPreserveWhiteSpace): OWideString;
begin
  if aPreserveWhiteSpace = pwPreserve then
    Result := 'preserve'
  else
    Result := 'default';
end;

function OXmlValidName(const aText: OWideString): Boolean;
var I: Integer;
begin
  if aText = '' then
  begin
    Result := False;
    Exit;
  end;

  Result := OXmlIsNameStartChar(aText[1]);
  if not Result then
    Exit;

  for I := 2 to Length(aText) do
  begin
    Result := OXmlIsNameChar(aText[I]);
    if not Result then
      Exit;
  end;
end;

function OXmlValidChars(const aText: OWideString): Boolean;
var I: Integer;
begin
  Result := True;
  for I := 1 to Length(aText) do
  begin
    Result := OXmlIsChar(aText[I]);
    if not Result then
      Exit;
  end;
end;

function OXmlNeedsPreserveAttribute(const aText: OWideString): Boolean;
var
  I, xLength: Integer;
  xThisCharWhiteSpace, xLastCharWhiteSpace: Boolean;
begin
  if aText = '' then
  begin
    Result := False;
    Exit;
  end;

  xLength := Length(aText);

  Result := OXmlIsWhiteSpaceChar(aText[1]) or OXmlIsWhiteSpaceChar(aText[xLength]);
  if Result then
    Exit;

  xLastCharWhiteSpace := False;
  I := 2;//we can omit first and last characters (already checked)!
  while I < xLength do//we can omit first and last characters (already checked)!
  begin
    if (aText[I] = #13) and (aText[I+1] = #10) then
      Inc(I);//step over #13#10
    xThisCharWhiteSpace := OXmlIsWhiteSpaceChar(aText[I]);
    if xThisCharWhiteSpace and xLastCharWhiteSpace then
    begin
      Result := True;
      Exit;
    end;
    xLastCharWhiteSpace := xThisCharWhiteSpace;
    Inc(I);
  end;
end;

function OXmlIsWhiteSpace(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not OXmlIsWhiteSpaceChar(aText[I]) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlIsNumber(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(aText) do
  if not (
    OXmlIsDecimalChar(aText[I]) or//'0'..'1'
    ((I = 1) and OXmlIsSignChar(aText[I])))//sign
  then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function OXmlValidEntityReference(const aText: OWideString): Boolean;
var
  I: Integer;
begin
  case Length(aText) of
    0: Result := False;
    1: Result := OXmlIsNameStartChar(aText[1]);
  else
    if aText[1] = '#' then
    begin
      if aText[2] = 'x' then
      begin
        //HEXADECIMAL
        for I := 3 to Length(aText)-1 do
        if not OXmlIsHexadecimalChar(aText[I]) then
        begin
          Result := False;
          Exit;
        end;
      end else
      begin
        //DECIMAL
        for I := 2 to Length(aText)-1 do
        if not OXmlIsDecimalChar(aText[I]) then
        begin
          Result := False;
          Exit;
        end;
      end;
    end else
    begin
      //TEXT
      Result := OXmlIsNameStartChar(aText[1]);
      if not Result then Exit;

      for I := 2 to Length(aText)-1 do
      if not OXmlIsNameChar(aText[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;

    Result := True;
  end;
end;

function OXmlValidCData(const aText: OWideString): Boolean;
begin
  Result := (Pos(']]>', aText) = 0);
end;

function OXmlValidComment(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or ((Pos('--', aText) = 0) and (aText[xL] <> '-'));
end;

function OXmlValidPIContent(const aText: OWideString): Boolean;
var
  xL: Integer;
begin
  xL := Length(aText);
  Result := (xL = 0) or (Pos('?>', aText) = 0);
end;

function OXmlIsDecimalChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    '0'..'9': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsHexadecimalChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    'a'..'f',
    'A'..'F',
    '0'..'9': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsSignChar(const aChar: OWideChar): Boolean;
begin
  case aChar of
    '-', '+': Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsBreakChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $00..$20,//0..space
    Ord('"'),
    Ord(''''),
    Ord('/'),
    Ord('?'),
    Ord('<'),
    Ord('='),
    Ord('>'): Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsChar(const aChar: OWideChar): Boolean;
begin
  Result := OXmlIsChar(Ord(aChar));
end;

function OXmlIsChar(const aChar: Integer): Boolean;
begin
  case aChar of
    Ord('<'), Ord('>'): Result := True;
    09, 10, 13,
    $20..$3B, $3D, $3F..$FF//except <>
    {$IFNDEF FPC}
    ,
    $0100..$FFFD
    {$ENDIF}
    : Result := True;
  else
    Result := False;
  end;
end;

function OXmlCharKind(const aChar: OWideChar): TXMLCharKind;
begin
  case Ord(aChar) of
    Ord('"'): Result := ckDoubleQuote;//#$22
    Ord('&'): Result := ckAmpersand;//#$26
    Ord(''''): Result := ckSingleQuote;//#$27
    Ord('<'): Result := ckLowerThan;//#$3C
    Ord('>'): Result := ckGreaterThan;//#$3E
    Ord('['): Result := ckSquareBracketOpen;
    Ord(']'): Result := ckSquareBracketClose;
    09: Result := ckTab;
    10: Result := ckNewLine10;
    13: Result := ckNewLine13;
    $20, $21, $23..$25, $28..$3B, $3D, $3F..$5A, $5C, $5E..$FF//except '"&<>[]
    {$IFNDEF FPC}
    ,
    $0100..$FFFD
    {$ENDIF}
    : Result := ckCharacter;
  else
    Result := ckInvalid;
  end;
end;

{$IFDEF FPC}
function OXmlCharKind(const aChar: OUnicodeChar): TXMLCharKind;
begin
  case Ord(aChar) of
    Ord('"'): Result := ckDoubleQuote;//#$22
    Ord('&'): Result := ckAmpersand;//#$26
    Ord(''''): Result := ckSingleQuote;//#$27
    Ord('<'): Result := ckLowerThan;//#$3C
    Ord('>'): Result := ckGreaterThan;//#$3E
    Ord('['): Result := ckSquareBracketOpen;
    Ord(']'): Result := ckSquareBracketClose;
    09: Result := ckTab;
    10: Result := ckNewLine10;
    13: Result := ckNewLine13;
    $20, $21, $23..$25, $28..$3B, $3D, $3F..$5A, $5C, $5E..$FF//except '"&<>[]
    ,
    $0100..$FFFD
    : Result := ckCharacter;
  else
    Result := ckInvalid;
  end;
end;
{$ENDIF}

function OXmlIsNameStartChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of//MUST BE Ord(aChar) because some Delphi show "E2030 Duplicate case label" error - > the performance is the same
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord(':'),
    Ord('_'),
    $C0..$D6,
    $D8..$F6,
    $F8..$FF
    {$IFNDEF FPC}
    ,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD
    {$ENDIF}
    : Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsWhiteSpaceChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of
    $09, $0A, $0D, $20: Result := True;
  else
    Result := False;
  end;
end;

function OXmlIsNameChar(const aChar: OWideChar): Boolean;
begin
  case Ord(aChar) of//MUST BE Ord(aChar) because some Delphi show "E2030 Duplicate case label" error - > the performance is the same
    Ord('A')..Ord('Z'),
    Ord('a')..Ord('z'),
    Ord('0')..Ord('9'),
    Ord(':'),
    Ord('_'),
    Ord('-'),
    Ord('.'),
    $B7,
    $C0..$D6,
    $D8..$F6,
    $F8..$FF
    {$IFNDEF FPC}
    ,
    $100..$2FF,
    $370..$37D,
    $37F..$1FFF,
    $200C..$200D,
    $2070..$218F,
    $2C00..$2FEF,
    $3001..$D7FF,
    $F900..$FDCF,
    $FDF0..$FFFD,
    $0300..$036F,
    $203F..$2040
    {$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

{$IFDEF O_DELPHI_5_DOWN}
//Delphi 5 compatibility functions

function TryStrToInt(const aStr: string; var outValue: Integer): Boolean; overload;
var
  E: Integer;
begin
  Val(aStr, outValue, E);
  Result := (E = 0);
end;

function TryStrToFloat(const aStr: string; var outValue: Extended): Boolean;
var
  xValue: Extended;
begin
  Result := TextToFloat(PChar(aStr), xValue, fvExtended);
  if Result then
    outValue := xValue;
end;

function TryStrToFloat(const aStr: string; var outValue: Double): Boolean;
var
  xValue: Extended;
begin
  Result := TextToFloat(PChar(aStr), xValue, fvExtended);
  if Result then
    outValue := xValue;
end;

function TryEncodeDate(aYear, aMonth, aDay: Word; var outDate: TDateTime): Boolean;
var
  I: Integer;
  xDayTable: PDayTable;
begin
  Result := False;
  xDayTable := @MonthDays[IsLeapYear(aYear)];
  if (aYear >= 1) and (aYear <= 9999) and (aMonth >= 1) and (aMonth <= 12) and
    (aDay >= 1) and (aDay <= xDayTable^[aMonth]) then
  begin
    for I := 1 to aMonth - 1 do Inc(aDay, xDayTable^[I]);
    I := aYear - 1;
    outDate := I * 365 + I div 4 - I div 100 + I div 400 + aDay - DateDelta;
    Result := True;
  end;
end;

function TryEncodeTime(aHour, aMin, aSec, aMSec: Word; var outTime: TDateTime): Boolean;
begin
  Result := False;
  if (aHour < 24) and (aMin < 60) and (aSec < 60) and (aMSec < 1000) then
  begin
    outTime := (aHour * 3600000 + aMin * 60000 + aSec * 1000 + aMSec) / MSecsPerDay;
    Result := True;
  end;
end;
{$ENDIF}

{$IFDEF O_TRYENCODEDATETIME}
function TryEncodeDateTime(aYear, aMonth, aDay, aHour, aMin, aSec,
  aMSec: Word; var outValue: TDateTime): Boolean;
var
  xTime: TDateTime;
begin
  Result := TryEncodeDate(aYear, aMonth, aDay, outValue);
  if Result then
  begin
    Result := TryEncodeTime(aHour, aMin, aSec, aMSec, xTime);
    if Result then
      outValue := outValue + xTime;
  end;
end;
{$ENDIF}

{ TVirtualMemoryStream }

procedure TVirtualMemoryStream.SetBuffer(const aBuffer: TBytes);
var
  xLength: Integer;
begin
  xLength := Length(aBuffer);
  if xLength > 0 then
    SetPointer(@aBuffer[0], xLength)
  else
    SetPointer(nil, 0);
end;

procedure TVirtualMemoryStream.SetEncodingBuffer(const aBuffer: TEncodingBuffer);
var
  xLength: Integer;
begin
  xLength := Length(aBuffer);
  if xLength > 0 then
    SetPointer(@aBuffer[TEncodingBuffer_FirstElement], xLength)
  else
    SetPointer(nil, 0);
end;

procedure TVirtualMemoryStream.SetPointer(aPtr: Pointer; const aSize: Longint);
begin
  inherited SetPointer(aPtr, aSize);
end;

procedure TVirtualMemoryStream.SetString(const aString: OWideString);
var
  xLength: Integer;
begin
  xLength := Length(aString);
  if xLength > 0 then
    SetPointer(@aString[1], xLength * SizeOf(OWideChar))
  else
    SetPointer(nil, 0);
end;

procedure TVirtualMemoryStream.SetString_UTF8(const aString: OUTF8Container);
var
  xLength: Integer;
begin
  xLength := Length(aString);
  if xLength > 0 then
    SetPointer(@aString[OUTF8Container_FirstElement], xLength)
  else
    SetPointer(nil, 0);
end;

function TVirtualMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  {$IFDEF FPC}
  Result := 0;//to supress compiler warnings
  {$ENDIF}
  raise Exception.Create(OXmlLng_CannotWriteToVirtualMemoryStream);
end;

{ TXMLDeclaration }

constructor TXMLDeclaration.Create;
begin
  fEncoding := True;
  fVersion := '1.0';
  fStandAlone := '';
end;

{$IFNDEF O_DELPHI_6_DOWN}
initialization
  FillChar(gxISOFormatSettings{%H-}, SizeOf(gxISOFormatSettings), 0);
  gxISOFormatSettings.DecimalSeparator := '.';
{$ENDIF}

end.
