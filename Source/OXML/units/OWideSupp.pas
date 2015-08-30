unit OWideSupp;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OWideSupp.pas

  A collection of types, classes and methods to support WideStrings across all
  compilers.

  OWideString type:
    Default string type that supports unicode characters
    - Delphi 2009+   string         (UTF-16)
    - Delphi 2007-   WideString     (UTF-16)
    - FPC            string         (UTF-8)

  OUnicodeString type:
    Always a UTF-16 string type
    - Delphi 2009+   string         (UTF-16)
    - Delphi 2007-   WideString     (UTF-16)
    - FPC            UnicodeString  (UTF-16)

  OFastString type:
    The fastest possible character container for unicode characters
    !!! must be converted with OFastToWide/OWideToFast to OWideString !!!
    - should be used as internal string storage where high performance is needed
      (basically only for D6-D2007 - their WideString performance is bad)
    - Delphi 2009+   string         (UTF-16)
    - Delphi 2007-   string         (UTF-16 is stored inside!!!)
    - FPC            string         (UTF-8)

  OUTF8Container type:
    Always a UTF-8 (string) container:
    - Delphi Mobile: TBytes
    - Delphi 2009+ Desktop: RawByteString
    - Delphi 2007-: AnsiString
    - FPC: string

  TOWideStringList
    - For Delphi 7-2007: TStringList replacement with WideStrings

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
  System.SysUtils, System.Classes
  {$ELSE}
  SysUtils, Classes
  {$ENDIF}

  {$IFNDEF O_DELPHI_5_DOWN}{$IF DEFINED(O_DELPHI_2006_UP) AND DEFINED(O_DELPHI_2007_DOWN)}
  , WideStrUtils
  {$IFEND}{$ENDIF}
  {$IFNDEF O_UNICODE}
  , Windows
  {$ENDIF}

  {$IFDEF O_DELPHI_XE3_UP}
  , Character
  {$ENDIF}

  {$IFDEF O_GENERICS}
    {$IFDEF O_NAMESPACES}
    , System.Generics.Collections
    {$ELSE}
    , Generics.Collections
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
  , LazUTF8Classes, FileUtil
  {$ENDIF}
  ;

type
  {$IFDEF FPC}
    OWideString = UTF8String;//UTF-8
    OUnicodeString = UnicodeString;//UTF-16
    OWideChar = AnsiChar;
    POWideChar = PAnsiChar;
    OUnicodeChar = UnicodeChar;
    POUnicodeChar = PWideChar;
    ORawByteString = AnsiString;
    ONativeInt = NativeInt;
    ONativeUInt = NativeUInt;
  {$ELSE}
    {$IFDEF O_UNICODE}
      OWideString = string;//UTF-16
      OUnicodeString = string;//UTF-16
      OWideChar = Char;
      POWideChar = PChar;
      OUnicodeChar = Char;
      POUnicodeChar = PChar;
      {$IFDEF O_HASBYTESTRINGS}
      ORawByteString = RawByteString;
      {$ENDIF}
      {$IFDEF O_DELPHI_2010_UP}
      ONativeInt = NativeInt;
      ONativeUInt = NativeUInt;
      {$ELSE}
      //D2009 bug
      ONativeInt = Integer;
      ONativeUInt = Cardinal;
      {$ENDIF}
    {$ELSE}
      OWideString = WideString;//UTF-16
      OUnicodeString = WideString;//UTF-16
      OWideChar = WideChar;
      POWideChar = PWideChar;
      OUnicodeChar = WideChar;
      POUnicodeChar = PWideChar;
      ORawByteString = AnsiString;
      ONativeInt = Integer;
      ONativeUInt = Cardinal;
    {$ENDIF}
  {$ENDIF}
  {$IFNDEF O_DELPHI_5_DOWN}//D6+ or FPC
  OStreamInt = Int64;
  {$ELSE}
  OStreamInt = Integer;
  {$ENDIF}

  {$IFDEF O_HASBYTESTRINGS}
  OUTF8Container = ORawByteString;
  const
  OUTF8Container_FirstElement = 1;//RawByteString
  {$ELSE}
  OUTF8Container = TBytes;
  const
  OUTF8Container_FirstElement = 0;//TBytes
  {$ENDIF}

type
  //OFastString is the fastest possible WideString replacement
  //Unicode Delphi: string
  //Non-unicode Delphi: WideString stored in AnsiString -> with double char size!!!
  //Lazarus: UTF8
  {$IFDEF O_DELPHI_2007_DOWN}
  OFastString = AnsiString;
  POFastChar = PAnsiChar;
  {$ELSE}
  OFastString = OWideString;
  POFastChar = POWideChar;
  {$ENDIF}

  TOWideStringArray = array of OWideString;

  {$IFDEF O_DELPHI_5_DOWN}
  IInterface = IUnknown;
  UTF8String = AnsiString;
  PByte = ^Byte;
  {$ENDIF}

  {$IFNDEF O_DELPHI_2009_UP}
  TBytes = array of Byte;
  {$ENDIF}

  {$IFDEF O_GENERICS}
  {$IFDEF O_DELPHI_2009}
  TList<T> = class(Generics.Collections.TList<T>)
  public
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
  end;
  {$ENDIF}
  {$ENDIF}

  {$IFNDEF O_UNICODE}
  TOWideStringList = class;
  TOWideStringListSortCompare = function(List: TOWideStringList; Index1, Index2: Integer): Integer;
  TOWideStringList = class(TPersistent)
  protected
    fList: TStringList;
  private
    function GetI(Index: Integer): OWideString;
    function GetCount: Integer;
    function GetText: OWideString;
    function GetCapacity: Integer;
    function GetCommaText: OWideString;
    {$IFDEF O_DELPHI_6_UP}
    function GetDelimitedText: OWideString;
    function GetDelimiter: Char;
    function GetQuoteChar: Char;
    function GetCaseSensitive: Boolean;
    {$ENDIF}
    function GetName(Index: Integer): OWideString;
    function GetObject(Index: Integer): TObject;
    function GetValue(const Name: OWideString): OWideString;
    function GetDuplicates: TDuplicates;
    function GetOnChange: TNotifyEvent;
    function GetOnChanging: TNotifyEvent;
    function GetSorted: Boolean;
    procedure SetI(Index: Integer; const Value: OWideString);
    procedure SetText(const Value: OWideString);
    procedure SetObject(Index: Integer; const Value: TObject);
    procedure SetCapacity(const Value: Integer);
    procedure SetCommaText(const Value: OWideString);
    {$IFDEF O_DELPHI_6_UP}
    procedure SetDelimitedText(const Value: OWideString);
    procedure SetDelimiter(const Value: Char);
    procedure SetQuoteChar(const Value: Char);
    procedure SetCaseSensitive(const Value: Boolean);
    {$ENDIF}
    procedure SetValue(const Name, Value: OWideString);
    procedure SetDuplicates(const Value: TDuplicates);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnChanging(const Value: TNotifyEvent);
    procedure SetSorted(const Value: Boolean);
    {$IFDEF O_DELPHI_7_UP}
    function GetNameValueSeparator: Char;
    function GetValueFromIndex(Index: Integer): OWideString;
    procedure SetNameValueSeparator(const Value: Char);
    procedure SetValueFromIndex(Index: Integer; const Value: OWideString);
    {$ENDIF}
    {$IFDEF O_DELPHI_2006_UP}
    function GetLineBreak: OWideString;
    procedure SetLineBreak(const Value: OWideString);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    {$ENDIF}
  protected
    procedure Changed;
    procedure Changing;
    function CompareStrings(const S1, S2: OWideString): Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(const S: OWideString): Integer;
    function AddObject(const S: OWideString; AObject: TObject): Integer;
    procedure AddStrings(Strings: TStrings); overload;
    procedure AddStrings(Strings: TOWideStringList); overload;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure EndUpdate;
    function Equals(Strings: TOWideStringList): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(const S: OWideString): Integer;
    function IndexOfName(const Name: OWideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: OWideString);
    procedure InsertObject(Index: Integer; const S: OWideString;
      AObject: TObject);
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

    procedure Sort;
    procedure CustomSort(Compare: TOWideStringListSortCompare); virtual;
    procedure QuickSort(L, R: Integer; SCompare: TOWideStringListSortCompare);
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;
    property CommaText: OWideString read GetCommaText write SetCommaText;
    {$IFDEF O_DELPHI_6_UP}
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: OWideString read GetDelimitedText write SetDelimitedText;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    {$ENDIF}
    property Names[Index: Integer]: OWideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Values[const Name: OWideString]: OWideString read GetValue write SetValue;
    property Strings[Index: Integer]: OWideString read GetI write SetI; default;
    property Text: OWideString read GetText write SetText;

    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;

    {$IFDEF O_DELPHI_7_UP}
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property ValueFromIndex[Index: Integer]: OWideString read GetValueFromIndex write SetValueFromIndex;
    {$ENDIF}
    {$IFDEF O_DELPHI_2006_UP}
    property LineBreak: OWideString read GetLineBreak write SetLineBreak;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    {$ENDIF}
  end;
  {$ELSE}
  TOWideStringList = TStringList;
  {$ENDIF}

  TOTextBuffer = class(TPersistent)//Text buffer, UTF-16 in Delphi, UTF-8 in FPC
  private
    fBuffer: array of OWideChar;//Faster in D7 than OWideString
    fAllocLength: Integer;//allocated length
    fUsedLength: Integer;//used length
    fRemaining: Integer;//fAllocLength-fUsedLength

    fDefBufferLength: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear(const aFullClear: Boolean = True);
    procedure GetBuffer(var outString: OWideString); overload;
    procedure GetBuffer(var outString: OWideString; const aPosition, aLength: Integer); overload;
    function GetBuffer: OWideString; overload;
    procedure RemoveLastChar;
    procedure RemoveLastString(const aLength: Integer);

    procedure WriteChar(const aChar: OWideChar);
    procedure WriteString(const aString: OWideString); overload;//outPosition 1-based!
    procedure WriteString(const aString: OWideString; var outPosition, outLength: Integer); overload;//outPosition 1-based!
    procedure Grow(const aMinChars: Integer);

    constructor Create(const aBufferLength: Integer = 1024);
  public
    property UsedLength: Integer read fUsedLength;
    property AllocLength: Integer read fAllocLength;
    property Remaining: Integer read fRemaining;
  end;

  TOByteBuffer = class(TPersistent)//Byte buffer
  private
    fBuffer: array of Byte;
    fAllocLength: ONativeInt;//allocated length
    fUsedLength: ONativeInt;//used length
    fRemaining: ONativeInt;//fAllocLength-fUsedLength

    fDefBufferLength: ONativeInt;

    procedure GetBuffer(var outBuffer; const aBufferSize: ONativeInt);
    procedure WriteBuffer(const aBuffer; const aBufferSize: ONativeInt); overload;//outPosition 0-based!
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Clear(const aFullClear: Boolean = True);
    procedure GetBytes(var outBytes: TBytes); overload;
    function GetBytes: TBytes; overload;
    procedure GetString(var outString: string); overload;
    function GetString: string; overload;
    procedure GetOWideString(var outString: OWideString); overload;
    function GetOWideString: OWideString; overload;
    procedure GetOUTF8Container(var outString: OUTF8Container); overload;
    function GetOUTF8Container: OUTF8Container; overload;
    procedure RemoveLastByte;
    procedure RemoveLastBytes(const aLength: ONativeInt);
    procedure RemoveLastChar;
    procedure RemoveLastString(const aLength: ONativeInt);

    procedure WriteByte(const aByte: Byte);
    procedure WriteChar(const aChar: Char);
    procedure WriteOWideChar(const aChar: OWideChar);
    procedure WriteString(const aString: string); overload;
    procedure WriteOWideString(const aString: OWideString); overload;
    procedure Grow(const aMinBytes: ONativeInt);

    constructor Create(const aBufferLength: ONativeInt = 1024);
  public
    property UsedLength: ONativeInt read fUsedLength;
    property AllocLength: ONativeInt read fAllocLength;
    property Remaining: ONativeInt read fRemaining;
  end;

  TOWideStringStackItem = packed record
    Position: Integer;
    Length: Integer;
  end;
  POWideStringStackItem = ^TOWideStringStackItem;

  TOWideStringStackArray = array of TOWideStringStackItem;
  POWideStringStackArray = ^TOWideStringStackArray;

  {$IFDEF O_DELPHI_2009_UP}
  TOFileStream = TFileStream;
  {$ELSE}{$IFDEF FPC}
  //FPC
  TOFileStream = class(TFileStreamUTF8)
  public
    constructor Create(const aFileName: OWideString; aMode: Word); overload;
    constructor Create(const aFileName: OWideString; aMode: Word; aRights: Cardinal); overload;
  end;
  {$ELSE}
  //non-unicode Delphi
  TOFileStream = class(THandleStream)
  private
    fFileName: string;
  public
    constructor Create(const aFileName: string; aMode: Word); overload;
    constructor Create(const aFileName: string; aMode: Word; aRights: Cardinal); overload;
    destructor Destroy; override;
    property FileName: string read fFileName;
  end;
  {$ENDIF FPC}{$ENDIF O_DELPHI_2009_UP}

{$IFDEF FPC}{$DEFINE DEF_TValueRelationship}{$ENDIF}
{$IFDEF O_DELPHI_5_DOWN}{$DEFINE DEF_TValueRelationship}{$ENDIF}
{$IFDEF DEF_TValueRelationship}
  TValueRelationship = -1..1;

const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);
{$ENDIF}

{$IFDEF O_DELPHI_5_DOWN}
  soBeginning = soFromBeginning;
  soCurrent = soFromCurrent;
  soEnd = soFromEnd;
{$ENDIF}

{$IFDEF O_DELPHI_4_DOWN}
type
  TListNotification = (lnAdded, lnExtracted, lnDeleted);
{$ENDIF}


function OStringReplace(const S, OldPattern, NewPattern: OWideString;
  Flags: TReplaceFlags): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OLowerCase(const aStr: OWideString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OUpperCase(const aStr: OWideString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OFastLowerCase(const aStr: OFastString): OFastString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OFastUpperCase(const aStr: OFastString): OFastString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OCompareText(const aStr1, aStr2: OWideString): Integer;
function OReverseString(const aStr: OWideString): OWideString;

//CharInSet for all compilers
type
  TByteSet = set of Byte;
{$IFDEF O_HASBYTESTRINGS}
function OCharInSet(const aChar: OWideChar; const aSet: TSysCharSet): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
{$ENDIF}
function OCharInSet(const aChar: OWideChar; const aSet: TByteSet): Boolean; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
//really WideString enabled CharInSet -> but slower!
function OCharInSetW(const aChar: OWideChar; const aCharArray: array of OWideChar): Boolean;

{$IFDEF O_DELPHI_6_DOWN}
type
  TFormatSettings = record
    DecimalSeparator: AnsiChar;
    ThousandSeparator: AnsiChar;
    TimeSeparator: AnsiChar;
    ListSeparator: AnsiChar;
    ShortDateFormat: AnsiString;
    LongDateFormat: AnsiString;
    ShortTimeFormat: AnsiString;
    LongTimeFormat: AnsiString;
  end;
{$ENDIF}

//GetLocaleFormatSettings for all compilers
function OGetLocaleFormatSettings: TFormatSettings; {$IFDEF O_INLINE}inline;{$ENDIF}

{$IFDEF O_HASBYTESTRINGS}
//wide string to owidestring and back
function WSToOWS(const aWS: WideString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OWSToWS(const aOWS: OWideString): WideString; {$IFDEF O_INLINE}inline;{$ENDIF}
//ansi string to owidestring and back
function ASToOWS(const aAS: AnsiString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OWSLength(const aOWS: OWideString): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
{$ENDIF}

//ofaststring to owidestring and back
{$IFNDEF O_UNICODE}
function OFastToWide(const aSourceFast: OFastString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}
function OWideToFast(const aSourceWide: OWideString): OFastString; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
procedure OWideToFast(const aSourceWide: OWideString; var outDestFast: OFastString); overload; {$IFDEF O_INLINE}inline;{$ENDIF}
{$ENDIF}

function OWideToUTF8Container(const aSourceWide: OWideString): OUTF8Container; {$IFDEF O_INLINE}inline;{$ENDIF}
function OUnicodeToUTF8Container(const aSourceUnicode: OUnicodeString): OUTF8Container; {$IFDEF O_INLINE}inline;{$ENDIF}
function OUTF8ContainerToWide(const aSourceUTF8: OUTF8Container): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}

function OWideToUnicode(const aSourceWide: OWideString): OUnicodeString; overload; {$IFDEF O_INLINE}inline;{$ENDIF}
function OUnicodeToWide(const aSourceUnicode: OUnicodeString): OWideString; {$IFDEF O_INLINE}inline;{$ENDIF}

//split a text to pieces with a delimiter
//if aConsiderQuotes=True, delimiters in quotes are ignored
//quotes must be escaped in XML-style, i.e. escaping with backslash is not considered as escaped: \" will not work
procedure OExplode(const aText: OWideString; const aDelimiter: OWideChar;
  const aStrList: TOWideStringList; const aConsiderQuotes: Boolean = False);
procedure OExpandPath(const aReferencePath, aVarPath: TOWideStringList);

{$IFDEF O_DELPHI_5_DOWN}
const
  sLineBreak = #13#10;
  PathDelim = '\';
  DriveDelim = ':';

function Utf8Encode(const WS: WideString): AnsiString;
function Utf8Decode(const S: AnsiString): WideString;
{$ENDIF}
function OWideCompareText(const S1, S2: OWideString): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
function OWideCompareStr(const S1, S2: OWideString): Integer; {$IFDEF O_INLINE}inline;{$ENDIF}
function OSameStr(const S1, S2: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OSameText(const S1, S2: OWideString): Boolean; {$IFDEF O_INLINE}inline;{$ENDIF}
function OReplaceLineBreaks(const aString: OWideString; const aLineBreak: OWideString = sLineBreak): OWideString;

implementation

uses
{$IFDEF FPC}
  LazUTF8,
{$ELSE}
  {$IFNDEF O_DELPHI_2009_UP}
    {$IFDEF O_DELPHI_6_UP}
    RTLConsts,
    {$ELSE}
    Consts,
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  OXmlLng;

function OReplaceLineBreaks(const aString: OWideString; const aLineBreak: OWideString = sLineBreak): OWideString;
var
  IRes, IStr: Integer;
  xStrLen: Integer;
  xLineBreakByteSize: Integer;
  xExtraLineBreakInc: Integer;

  procedure _CopyLineBreak;
  begin
    if xLineBreakByteSize > 0 then
      Move(aLineBreak[1], Result[IRes], xLineBreakByteSize);
    if xExtraLineBreakInc <> 0 then
      Inc(IRes, xExtraLineBreakInc);
  end;
begin
  xStrLen := Length(aString);
  xLineBreakByteSize := Length(aLineBreak)*SizeOf(OWideChar);
  xExtraLineBreakInc := Length(aLineBreak)-1;

  SetLength(Result, xStrLen*2);//worst case #10 -> #13#10
  IRes := 1;
  IStr := 1;
  while IStr <= xStrLen do
  begin
    case aString[IStr] of
      #13: begin
        _CopyLineBreak;
        if (IStr < xStrLen) and (aString[IStr+1] = #10) then
          Inc(IStr);
      end;
      #10: _CopyLineBreak;
    else
      Result[IRes] := aString[IStr];
    end;

    Inc(IRes);
    Inc(IStr);
  end;
  SetLength(Result, IRes-1);
end;

procedure OExpandPath(const aReferencePath, aVarPath: TOWideStringList);
var
  xNewPath: TOWideStringList;
  I: Integer;
begin
  if (aVarPath.Count > 0) and (aVarPath[aVarPath.Count-1] = '') then//delete last empty element ("root/name/")
    aVarPath.Delete(aVarPath.Count-1);

  if (aVarPath.Count = 0) then//current directory
  begin
    aVarPath.Assign(aReferencePath);
    Exit;
  end;

  xNewPath := TOWideStringList.Create;
  try
    if (aVarPath[0] <> '') then//is relative path
      xNewPath.Assign(aReferencePath);

    for I := 0 to aVarPath.Count-1 do
    begin
      if aVarPath[I] = '..' then
      begin
        //go up
        if xNewPath.Count > 0 then
          xNewPath.Delete(xNewPath.Count-1);
      end
      else
      if (aVarPath[I] <> '.') and (aVarPath[I] <> '') then//not current directory
      begin
        xNewPath.Add(aVarPath[I]);
      end;
    end;

    aVarPath.Assign(xNewPath);
  finally
    xNewPath.Free;
  end;
end;

procedure OExplode(const aText: OWideString; const aDelimiter: OWideChar;
  const aStrList: TOWideStringList; const aConsiderQuotes: Boolean);
var
  xI, xTextLength, xBufferBegin: Integer;
  xC: OWideChar;

  function _ReadChar: Boolean;
  begin
    Result := (xI <= xTextLength);
    if Result then
      xC := aText[xI];
    Inc(xI);
  end;

  procedure _AddBufferToStrList;
  begin
    if xBufferBegin < xI-1 then
      aStrList.Add(Copy(aText, xBufferBegin, xI-xBufferBegin-1))
    else
      aStrList.Add('');
    xBufferBegin := xI;
  end;
begin
  xC := #0;
  aStrList.Clear;

  xTextLength := Length(aText);
  if xTextLength = 0 then
    Exit;

  xI := 1;
  xBufferBegin := 1;
  while _ReadChar do
  begin
    if aConsiderQuotes then
    begin
      case xC of
        '"':
        begin
          while _ReadChar do
          if xC = '"' then
            Break;
        end;
        '''':
        begin
          while _ReadChar do
          if xC = '''' then
            Break;
        end;
      end;
    end;
    if xC = aDelimiter then
      _AddBufferToStrList;
  end;

  _AddBufferToStrList;//must be here
end;

{$IFDEF O_DELPHI_5_DOWN}
function WideLowerCase(const S: WideString): WideString;
var
  xLength: Integer;
begin
  xLength := Length(S);
  SetString(Result, PWideChar(S), xLength);
  if xLength > 0 then
    CharLowerBuffW(Pointer(Result), xLength);
end;
{$ENDIF}

function OFastLowerCase(const aStr: OFastString): OFastString;
{$IFNDEF O_UNICODE}
var
  xLength: Integer;
{$ENDIF}
begin
  {$IFDEF O_UNICODE}
  Result := OLowerCase(aStr);
  {$ELSE}
  xLength := Length(aStr);
  SetString(Result, POFastChar(aStr), xLength);
  if xLength > 0 then
    CharLowerBuffW(Pointer(Result), xLength div SizeOf(OWideChar));
  {$ENDIF}
end;

function OLowerCase(const aStr: OWideString): OWideString;
begin
  {$IFDEF O_UNICODE}
  {$IFDEF FPC}
  Result := UTF8LowerCase(aStr);
  {$ELSE}
  Result := LowerCase(aStr);
  {$ENDIF}
  {$ELSE}
  Result := WideLowerCase(aStr);
  {$ENDIF}
end;

function OFastUpperCase(const aStr: OFastString): OFastString;
{$IFNDEF O_UNICODE}
var
  xLength: Integer;
{$ENDIF}
begin
  {$IFDEF O_UNICODE}
  Result := OUpperCase(aStr);
  {$ELSE}
  xLength := Length(aStr);
  SetString(Result, POFastChar(aStr), xLength);
  if xLength > 0 then
    CharUpperBuffW(Pointer(Result), xLength div SizeOf(OWideChar));
  {$ENDIF}
end;

{$IFDEF O_DELPHI_5_DOWN}
function WideUpperCase(const S: WideString): WideString;
var
  xLength: Integer;
begin
  xLength := Length(S);
  SetString(Result, PWideChar(S), xLength);
  if xLength > 0 then
    CharUpperBuffW(Pointer(Result), xLength);
end;

function WideCompareText(const S1, S2: WideString): Integer;
  function _AnsiVersion(const S1, S2: WideString; CmpFlags: Integer): Integer;
  var
    a1, a2: AnsiString;
  begin
    a1 := s1;
    a2 := s2;
    Result := CompareStringA(LOCALE_USER_DEFAULT, CmpFlags, PChar(a1), Length(a1),
      PChar(a2), Length(a2)) - 2;
  end;
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED: Result := _AnsiVersion(S1, S2, NORM_IGNORECASE);
  else
    RaiseLastWin32Error;
  end;
end;
{$ENDIF O_DELPHI_5_DOWN}

function OUpperCase(const aStr: OWideString): OWideString;
begin
  {$IFDEF O_UNICODE}
  {$IFDEF FPC}
  Result := UTF8UpperCase(aStr);
  {$ELSE}
  Result := UpperCase(aStr);
  {$ENDIF}
  {$ELSE}
  Result := WideUpperCase(aStr);
  {$ENDIF}
end;

function OCompareText(const aStr1, aStr2: OWideString): Integer;
begin
  {$IFDEF O_UNICODE}
  {$IFDEF FPC}
  Result := UTF8CompareText(aStr1, aStr2);
  {$ELSE}
  Result := CompareText(aStr1, aStr2);
  {$ENDIF}
  {$ELSE}
  Result := WideCompareText(aStr1, aStr2);
  {$ENDIF}
end;

function OReverseString(const aStr: OWideString): OWideString;
{$IFDEF FPC}
var
  I, L, xStrLength, xCharLength: Integer;
  P: PChar;
begin
  xStrLength := Length(aStr);
  SetLength(Result, xStrLength);
  if xStrLength = 0 then
    Exit;

  P := PChar(aStr);
  I := 1;
  while I <= xStrLength do
  begin
    xCharLength := UTF8CharacterLength(P);
    for L := 1 to xCharLength do
    begin
      Result[xStrLength-I-xCharLength+L+1] := P^;
      Inc(P);
    end;
    Inc(I, xCharLength);
  end;
end;
{$ELSE}
var
  I, xStrLength: Integer;
begin
  xStrLength := Length(aStr);
  SetLength(Result, xStrLength);

  for I := 1 to xStrLength do
    Result[xStrLength-I+1] := aStr[I];
end;
{$ENDIF}

{$IFNDEF O_UNICODE}
function OFastToWide(const aSourceFast: OFastString): OWideString;
var
  xL: Integer;
  xS: PWideChar;
  I: Integer;
begin
  xL := Length(aSourceFast) div SizeOf(OWideChar);
  SetLength(Result, xL);
  if xL > 0 then
  begin
    xS := @aSourceFast[1];
    for I := 1 to xL do
    begin
      Result[I] := xS^;
      Inc(xS);
    end;
  end;
end;

function OWideToFast(const aSourceWide: OWideString): OFastString;
begin
  OWideToFast(aSourceWide, Result);
end;

procedure OWideToFast(const aSourceWide: OWideString; var outDestFast: OFastString);
var
  xL: Integer;
  xR: PWideChar;
  I: Integer;
begin
  xL := Length(aSourceWide);
  SetLength(outDestFast, xL*SizeOf(OWideChar));
  if xL > 0 then
  begin
    xR := @outDestFast[1];
    for I := 1 to xL do
    begin
      xR^ := aSourceWide[I];
      Inc(xR);
    end;
  end;
end;
{$ENDIF}

function OWideToUTF8Container(const aSourceWide: OWideString): OUTF8Container;
begin
  {$IFDEF FPC}
  Result := aSourceWide;
  {$ELSE}{$IFDEF O_HASBYTESTRINGS}
  Result := UTF8Encode(aSourceWide);
  {$ELSE}
  Result := TEncoding.UTF8.GetBytes(aSourceWide);
  {$ENDIF}{$ENDIF}
end;

function OUTF8ContainerToWide(const aSourceUTF8: OUTF8Container): OWideString;
begin
  {$IFDEF FPC}
  Result := aSourceUTF8;
  {$ELSE}{$IFDEF O_HASBYTESTRINGS}
  Result := {$IFDEF O_DELPHI_2009_UP}UTF8ToString(RawByteString({$ELSE}UTF8Decode(({$ENDIF}aSourceUTF8));//IMPORTANT there is a bug in UTF8ToString(S: array of Byte)
  {$ELSE}
  Result := TEncoding.UTF8.GetString(aSourceUTF8);
  {$ENDIF}{$ENDIF}
end;

function OUnicodeToUTF8Container(const aSourceUnicode: OUnicodeString): OUTF8Container;
begin
  {$IFDEF O_HASBYTESTRINGS}
  Result := UTF8Encode(aSourceUnicode);
  {$ELSE}
  Result := TEncoding.UTF8.GetBytes(aSourceUnicode);
  {$ENDIF}
end;

function OUnicodeToWide(const aSourceUnicode: OUnicodeString): OWideString;
begin
  {$IFDEF FPC}
  Result := UTF8Encode(aSourceUnicode);
  {$ELSE}
  Result := aSourceUnicode;
  {$ENDIF}
end;

function OWideToUnicode(const aSourceWide: OWideString): OUnicodeString; overload;
begin
  {$IFDEF FPC}
  Result := UTF8Decode(aSourceWide);
  {$ELSE}
  Result := aSourceWide;
  {$ENDIF}
end;

{$IFDEF O_HASBYTESTRINGS}
function OCharInSet(const aChar: OWideChar; const aSet: TSysCharSet): Boolean;
begin
  Result := {$ifndef FPC}(Ord(aChar) {%H-}<= 255) and{$endif} (AnsiChar(aChar) in aSet);
end;
{$ENDIF}

function OCharInSet(const aChar: OWideChar; const aSet: TByteSet): Boolean;
begin
  Result := {$ifndef FPC}(Ord(aChar) <= 255) and{$endif} (Ord(aChar) in aSet);
end;

function OCharInSetW(const aChar: OWideChar; const aCharArray: array of OWideChar): Boolean;
var I: Integer;
begin
  for I := Low(aCharArray) to High(aCharArray) do
  if aChar = aCharArray[I] then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
end;

function OWSLength(const aOWS: OWideString): Integer;
begin
  {$IFDEF FPC}
  Result := Length(OWSToWS(aOWS));
  {$ELSE}
  Result := Length(aOWS);
  {$ENDIF}
end;

{$IFDEF O_HASBYTESTRINGS}
function ASToOWS(const aAS: AnsiString): OWideString;
begin
  {$IFDEF FPC}
  Result := AnsiToUtf8(aAS);
  {$ELSE}
  Result := OWideString(aAS);
  {$ENDIF}
end;
function WSToOWS(const aWS: WideString): OWideString;
begin
  {$IFDEF FPC}
  Result := UTF16ToUTF8(aWS);
  {$ELSE}
  Result := aWS;
  {$ENDIF}
end;

function OWSToWS(const aOWS: OWideString): WideString;
begin
  {$IFDEF FPC}
  Result := UTF8Decode(aOWS);
  {$ELSE}
  Result := aOWS;
  {$ENDIF}
end;
{$ENDIF}

function OGetLocaleFormatSettings: TFormatSettings;
begin
{$IFDEF O_DELPHI_6_DOWN}
  Result.DecimalSeparator := DecimalSeparator;
  Result.ThousandSeparator := #0;
  Result.TimeSeparator := TimeSeparator;
  Result.ListSeparator := ListSeparator;
  Result.ShortDateFormat := ShortDateFormat;
  Result.LongDateFormat := LongDateFormat;
  Result.ShortTimeFormat := ShortTimeFormat;
  Result.LongTimeFormat := LongTimeFormat;
{$ELSE}
  {$IF DEFINED(FPC)}
    Result := DefaultFormatSettings;
  {$ELSEIF DEFINED(O_DELPHI_XE_UP)}
    Result := TFormatSettings.Create;
  {$ELSEIF NOT DEFINED(FPC)}//THIS EVALUATES ALWAYS TO TRUE (IT SHOULD REPLACE A SIMPLE "ELSE") -> DELPHI 5 COMPATIBILITY
    GetLocaleFormatSettings(0, Result);
  {$IFEND}
{$ENDIF}
end;

{$IFNDEF O_DELPHI_2006_UP}
//Delphi 6, 7
function WideStringReplace(const S, OldPattern, NewPattern: WideString;
  Flags: TReplaceFlags): WideString; {$IFDEF O_INLINE}inline;{$ENDIF}
var
  SearchStr, OldPatt: WideString;
  I, L, K, OldPattLength, NewPattLength, SLength: Integer;
begin
  if (OldPattern = '') or (S = '') then
  begin
    Result := S;
    Exit;
  end;

  if rfIgnoreCase in Flags then
  begin
    SearchStr := WideUpperCase(S);
    OldPatt := WideUpperCase(OldPattern);
  end
  else
  begin
    SearchStr := S;
    OldPatt := OldPattern;
  end;
  OldPattLength := Length(OldPatt);
  NewPattLength := Length(NewPattern);
  SLength := Length(S);

  if NewPattLength <= OldPattLength then
    SetLength(Result, SLength)
  else
    SetLength(Result, (SLength*NewPattLength) div OldPattLength + OldPattLength);//maximum length + some extra buffer just in case...

  I := 1;//position in S
  L := 1;//position in Result
  while I <= SLength-OldPattLength+1 do
  begin
    if CompareMem(@SearchStr[I], @OldPatt[1], OldPattLength*SizeOf(WideChar)) then
    begin
      //pattern found
      for K := 1 to NewPattLength do
      begin
        Result[L] := NewPattern[K];
        Inc(L);
      end;
      Inc(I, OldPattLength);
      if not(rfReplaceAll in Flags) then
        break;
    end
    else
    begin
      //pattern not found
      Result[L] := S[I];
      Inc(I);
      Inc(L);
    end;
  end;

  //write trail
  while I <= SLength do
  begin
    Result[L] := S[I];
    Inc(I);
    Inc(L);
  end;

  if Length(Result) <> L-1 then
    SetLength(Result, L-1);
end;
{$ENDIF}

function OStringReplace(const S, OldPattern, NewPattern: OWideString;
  Flags: TReplaceFlags): OWideString;
begin
{$IFDEF O_UNICODE}
  //D2009+, FPC
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
{$ELSE}
  //D6-D2007
  Result := WideStringReplace(S, OldPattern, NewPattern, Flags);
{$ENDIF}
end;

{$IFDEF O_DELPHI_5_DOWN}
function Utf8Encode(const WS: WideString): AnsiString;
var
  xLength: Integer;
begin
  Result := '';
  if WS = '' then Exit;

  //IMPORTANT: WS is WITH the NULL character -> xLength is ALSO WITH the NULL CHARACTER!!!
  xLength := WideCharToMultiByte(CP_UTF8, 0, PWideChar(WS), -1, nil, 0, nil, nil);

  SetLength(Result, xLength-1);
  if xLength > 1 then
    WideCharToMultiByte(CP_UTF8, 0, PWideChar(WS), -1, PAnsiChar(Result), xLength-1, nil, nil);
end;

function Utf8Decode(const S: AnsiString): WideString;
var
  xLength: Integer;
begin
  Result := '';
  if S = '' then Exit;

  //IMPORTANT: S is WITH the NULL character -> xLength is ALSO WITH the NULL CHARACTER!!!
  xLength := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(S), -1, nil, 0);
  SetLength(Result, xLength-1);
  if xLength > 1 then
    MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(S), -1, PWideChar(Result), xLength-1);
end;
{$ENDIF}

function OWideCompareText(const S1, S2: OWideString): Integer;
begin
  {$IFDEF O_DELPHI_5_DOWN}
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
  if GetLastError = ERROR_CALL_NOT_IMPLEMENTED then
    Result := CompareText(S1, S2);
  {$ELSE}
  {$IFDEF FPC}
  Result := UTF8CompareText(S1, S2);
  {$ELSE}
  {$IFDEF O_UNICODE}
  Result := CompareText(S1, S2);
  {$ELSE}
  Result := WideCompareText(S1, S2);
  {$ENDIF}{$ENDIF}{$ENDIF}
end;

function OWideCompareStr(const S1, S2: OWideString): Integer;
begin
  {$IFDEF O_DELPHI_5_DOWN}
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(S1),
    Length(S1), PWideChar(S2), Length(S2)) - 2;
  if GetLastError = ERROR_CALL_NOT_IMPLEMENTED then
    Result := CompareStr(S1, S2);
  {$ELSE}
  {$IFDEF FPC}
  Result := UTF8CompareStr(S1, S2);
  {$ELSE}
  {$IFDEF O_UNICODE}
  Result := CompareStr(S1, S2);
  {$ELSE}
  Result := WideCompareStr(S1, S2);
  {$ENDIF}{$ENDIF}{$ENDIF}
end;

function OSameStr(const S1, S2: OWideString): Boolean;
begin
  Result := (OWideCompareStr(S1, S2) = 0);
end;

function OSameText(const S1, S2: OWideString): Boolean;
begin
  Result := (OWideCompareText(S1, S2) = 0);
end;

{ TOByteBuffer }

procedure TOByteBuffer.AssignTo(Dest: TPersistent);
var
  xDest: TOByteBuffer;
begin
  if Dest is TOByteBuffer then
  begin
    xDest := TOByteBuffer(Dest);

    if xDest.fAllocLength < Self.fAllocLength then
    begin
      xDest.fAllocLength := Self.fAllocLength;
      SetLength(xDest.fBuffer, xDest.fAllocLength);
    end;

    xDest.fUsedLength := Self.fUsedLength;
    xDest.fRemaining := Self.fRemaining;

    xDest.fBuffer := Copy(Self.fBuffer, 0, Self.fUsedLength);
  end else
    inherited;
end;

procedure TOByteBuffer.Clear(const aFullClear: Boolean);
begin
  if aFullClear and (fAllocLength > fDefBufferLength) then
  begin
    fAllocLength := fDefBufferLength;
    SetLength(fBuffer, fAllocLength);
  end;

  fUsedLength := 0;
  fRemaining := fAllocLength;
end;

constructor TOByteBuffer.Create(const aBufferLength: ONativeInt);
begin
  inherited Create;

  fDefBufferLength := aBufferLength;
  fAllocLength := aBufferLength;
  fRemaining := fAllocLength;
  fUsedLength := 0;
  SetLength(fBuffer, fAllocLength);
end;

procedure TOByteBuffer.GetBytes(var outBytes: TBytes);
begin
  SetLength(outBytes, fUsedLength);
  if fUsedLength > 0 then
    GetBuffer(outBytes, fUsedLength);
end;

procedure TOByteBuffer.GetBuffer(var outBuffer; const aBufferSize: ONativeInt);
{$IFDEF O_DELPHI_2007_DOWN}
var
  I: ONativeInt;
  B: PByte;
{$ENDIF}
begin
  if aBufferSize > 0 then
  begin
    {$IFDEF O_DELPHI_2007_DOWN}
    //Move() is extremly slow in Delphi 7, copy char-by-char is faster
    B := @outBuffer;
    for I := 0 to aBufferSize-1 do
    begin
      B^ := fBuffer[I];
      Inc(B);
    end;
    {$ELSE}
    Move(fBuffer[0], outBuffer, aBufferSize);
    {$ENDIF}
  end;
end;

function TOByteBuffer.GetBytes: TBytes;
begin
  GetBytes(Result{%H-});
end;

procedure TOByteBuffer.GetOWideString(var outString: OWideString);
var
  xLength: ONativeInt;
begin
  xLength := fUsedLength div SizeOf(OWideChar);
  SetLength(outString, xLength);
  if xLength > 0 then
    GetBuffer(outString[1], xLength * SizeOf(OWideChar));
end;

procedure TOByteBuffer.GetOUTF8Container(var outString: OUTF8Container);
var
  xLength: ONativeInt;
begin
  xLength := fUsedLength;
  SetLength(outString, xLength);
  if xLength > 0 then
    GetBuffer(outString[OUTF8Container_FirstElement], xLength);
end;

function TOByteBuffer.GetOUTF8Container: OUTF8Container;
begin
  GetOUTF8Container(Result{%H-});
end;

function TOByteBuffer.GetOWideString: OWideString;
begin
  GetOWideString(Result{%H-});
end;

procedure TOByteBuffer.GetString(var outString: string);
var
  xLength: ONativeInt;
begin
  xLength := fUsedLength div SizeOf(Char);
  SetLength(outString, xLength);
  if xLength > 0 then
    GetBuffer(outString[1], xLength * SizeOf(Char));
end;

function TOByteBuffer.GetString: string;
begin
  GetString(Result{%H-});
end;

procedure TOByteBuffer.Grow(const aMinBytes: ONativeInt);
var
  xGrowSize: ONativeInt;
begin
  xGrowSize := 0;
  while fRemaining+xGrowSize < aMinBytes do
    Inc(xGrowSize, fAllocLength+xGrowSize);

  Inc(fRemaining, xGrowSize);
  Inc(fAllocLength, xGrowSize);
  SetLength(fBuffer, fAllocLength);
end;

procedure TOByteBuffer.RemoveLastByte;
begin
  RemoveLastBytes(1);
end;

procedure TOByteBuffer.RemoveLastBytes(const aLength: ONativeInt);
begin
  Dec(fUsedLength, aLength);
  Inc(fRemaining, aLength);
  if fUsedLength < 0 then
  begin
    fUsedLength := 0;
    fRemaining := fAllocLength;
  end;
end;

procedure TOByteBuffer.RemoveLastChar;
begin
  RemoveLastBytes(SizeOf(OWideChar));
end;

procedure TOByteBuffer.RemoveLastString(const aLength: ONativeInt);
begin
  RemoveLastBytes(aLength * SizeOf(OWideChar));
end;

procedure TOByteBuffer.WriteBuffer(const aBuffer;
  const aBufferSize: ONativeInt);
var
  xPosition: ONativeInt;
{$IFDEF O_DELPHI_2007_DOWN}
  I: ONativeInt;
  B: PByte;
{$ENDIF}
begin
  xPosition := fUsedLength;

  if aBufferSize > 0 then
  begin
    if fRemaining < aBufferSize then
      Grow(aBufferSize);

    Inc(fUsedLength, aBufferSize);
    Dec(fRemaining, aBufferSize);

    {$IFDEF O_DELPHI_2007_DOWN}
    //Move() is extremly slow here in Delphi 7, copy char-by-char is faster also for long strings!!! (this may be a delphi bug)
    B := @aBuffer;
    for I := 0 to aBufferSize-1 do
    begin
      fBuffer[xPosition+I] := B^;
      Inc(B);
    end;
    {$ELSE}
    Move(aBuffer, fBuffer[xPosition], aBufferSize);
    {$ENDIF}
  end;
end;

procedure TOByteBuffer.WriteByte(const aByte: Byte);
begin
  WriteBuffer(aByte, SizeOf(aByte));
end;

procedure TOByteBuffer.WriteChar(const aChar: Char);
begin
  WriteBuffer(aChar, SizeOf(aChar));
end;

procedure TOByteBuffer.WriteOWideChar(const aChar: OWideChar);
begin
  WriteBuffer(aChar, SizeOf(aChar));
end;

procedure TOByteBuffer.WriteOWideString(const aString: OWideString);
var
  xByteLength: ONativeInt;
begin
  xByteLength := Length(aString) * SizeOf(OWideChar);
  if xByteLength > 0 then
    WriteBuffer(aString[1], xByteLength);
end;

procedure TOByteBuffer.WriteString(const aString: string);
var
  xByteLength: ONativeInt;
begin
  xByteLength := Length(aString) * SizeOf(Char);
  if xByteLength > 0 then
    WriteBuffer(aString[1], xByteLength);
end;

{$IFNDEF O_DELPHI_2009_UP}
{ TOFileStream }

{$IFDEF FPC}
//FPC
constructor TOFileStream.Create(const aFileName: OWideString; aMode: Word);
begin
  {$IFDEF MSWINDOWS}
  Create(aFileName, aMode, 0);
  {$ELSE}
  Create(aFileName, aMode, 438);
  {$ENDIF}
end;

constructor TOFileStream.Create(const aFileName: OWideString; aMode: Word;
  aRights: Cardinal);
begin
  //SOLVES FPC BUG WITH "FILE NOT FOUND"
  if
    not (aMode and fmCreate = fmCreate) and
    not FileExistsUTF8(aFileName)
  then
    raise EFCreateError.CreateFmt(OXmlLng_FileNotFound, [aFileName]);

  inherited Create(aFileName, aMode, aRights);
end;
{$ELSE !FPC}
//non-unicode Delphi
constructor TOFileStream.Create(const aFileName: string; aMode: Word);
begin
  {$IFDEF MSWINDOWS}
  Create(aFileName, aMode, 0);
  {$ELSE}
  Create(aFileName, aMode, 438);
  {$ENDIF}
end;

const
  O_INVALID_FILE_HANDLE = -1;

function FileCreateW(const aFileName: WideString; aMode: LongWord; aRights: Cardinal): Integer;
{$IFDEF MSWINDOWS}
const
  cExclusive: array[0..1] of LongWord = (
    CREATE_ALWAYS,
    CREATE_NEW);
  cShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := O_INVALID_FILE_HANDLE;
  if (aMode and $F0) <= fmShareDenyNone then
  begin
    Result := CreateFileW(PWideChar(aFileName), GENERIC_READ or GENERIC_WRITE,
      cShareMode[(aMode and $F0) shr 4], nil, cExclusive[(aMode and $0004) shr 2], FILE_ATTRIBUTE_NORMAL, 0);
  end;
end;
{$ELSE}
begin
  Result := FileCreate(UTF8Encode(aFileName), aMode, aRights);
end;
{$ENDIF MSWINDOWS}

function FileOpenW(const aFileName: WideString; aMode: LongWord): Integer;
{$IFDEF MSWINDOWS}
const
  cAccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  cShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := O_INVALID_FILE_HANDLE;
  if ((aMode and 3) <= fmOpenReadWrite) and
    ((aMode and $F0) <= fmShareDenyNone) then
    Result := CreateFileW(PWideChar(aFileName), cAccessMode[aMode and 3],
      cShareMode[(aMode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0);
end;
{$ELSE !MSWINDOWS}
begin
  Result := FileOpen(UTF8Encode(aFileName), aMode);
end;
{$ENDIF MSWINDOWS}

constructor TOFileStream.Create(const aFileName: string; aMode: Word; aRights: Cardinal);
var
  xShareMode: Word;
begin
  if (aMode and fmCreate = fmCreate) then
  begin
    xShareMode := aMode and $FF;
    if xShareMode = $FF then
      xShareMode := fmShareExclusive; // For compat in case $FFFF passed as Mode
    inherited Create(FileCreateW(aFileName, xShareMode, aRights));
    if Handle = O_INVALID_FILE_HANDLE then
    begin
      {$IFDEF O_DELPHI_7_UP}
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(aFileName), SysErrorMessage(GetLastError)]);
      {$ELSE}
      raise EFCreateError.CreateResFmt(@SFCreateError, [ExpandFileName(aFileName)]);
      {$ENDIF}
    end;
  end else
  begin
    inherited Create(FileOpenW(aFileName, aMode));
    if Handle = O_INVALID_FILE_HANDLE then
    begin
      {$IFDEF O_DELPHI_7_UP}
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(aFileName), SysErrorMessage(GetLastError)]);
      {$ELSE}
      raise EFOpenError.CreateResFmt(@SFOpenError, [ExpandFileName(aFileName)]);
      {$ENDIF}
    end;
  end;
  fFileName := aFileName;
end;

destructor TOFileStream.Destroy;
begin
  if Handle <> O_INVALID_FILE_HANDLE then
    FileClose(Handle);
  inherited Destroy;
end;
{$ENDIF FPC}
{$ENDIF O_DELPHI_2009_UP}

{$IFNDEF O_UNICODE}
{ TOWideStringList }

function TOWideStringList.Add(const S: OWideString): Integer;
begin
  Result := fList.Add(UTF8Encode(S));
end;

function TOWideStringList.AddObject(const S: OWideString; AObject: TObject): Integer;
begin
  Result := fList.AddObject(UTF8Encode(S), AObject);
end;

procedure TOWideStringList.AddStrings(Strings: TOWideStringList);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      fList.AddObject(Strings.fList[I], Strings.fList.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TOWideStringList.Assign(Source: TPersistent);
begin
  if Source is TStrings then
    fList.Assign(Source)
  else if Source is TOWideStringList then
    fList.Assign(TOWideStringList(Source).fList);
end;

procedure TOWideStringList.BeginUpdate;
begin
  fList.BeginUpdate;
end;

procedure TOWideStringList.AddStrings(Strings: TStrings);
begin
  fList.AddStrings(Strings);
end;

type
  TMyStringList = class(TStringList);

procedure TOWideStringList.Changed;
begin
  TMyStringList(fList).Changed;
end;

procedure TOWideStringList.Changing;
begin
  TMyStringList(fList).Changing;
end;

procedure TOWideStringList.Clear;
begin
  fList.Clear;
end;

function TOWideStringList.CompareStrings(const S1, S2: OWideString): Integer;
begin
  Result := OWideCompareText(S1, S2);
end;

constructor TOWideStringList.Create;
begin
  inherited Create;

  fList := TStringList.Create;
end;

procedure TOWideStringList.CustomSort(Compare: TOWideStringListSortCompare);
begin
  if not Sorted and (Count > 1) then
  begin
    BeginUpdate;
    try
      QuickSort(0, Count - 1, Compare);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TOWideStringList.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

destructor TOWideStringList.Destroy;
begin
  fList.Destroy;

  inherited;
end;

procedure TOWideStringList.EndUpdate;
begin
  fList.EndUpdate;
end;

function TOWideStringList.Equals(Strings: TOWideStringList): Boolean;
begin
  Result := fList.Equals(Strings.fList);
end;

procedure TOWideStringList.Exchange(Index1, Index2: Integer);
begin
  fList.Exchange(Index1, Index2);
end;

function TOWideStringList.GetCapacity: Integer;
begin
  Result := fList.Capacity;
end;

function TOWideStringList.GetCommaText: OWideString;
begin
  Result := UTF8Decode(fList.CommaText);
end;

function TOWideStringList.GetCount: Integer;
begin
  Result := fList.Count;
end;

{$IFDEF O_DELPHI_6_UP}
function TOWideStringList.GetCaseSensitive: Boolean;
begin
  Result := fList.CaseSensitive;
end;

function TOWideStringList.GetDelimitedText: OWideString;
begin
  Result := UTF8Decode(fList.DelimitedText);
end;

function TOWideStringList.GetDelimiter: Char;
begin
  Result := fList.Delimiter;
end;
{$ENDIF}

function TOWideStringList.GetDuplicates: TDuplicates;
begin
  Result := fList.Duplicates;
end;

function TOWideStringList.GetI(Index: Integer): OWideString;
begin
  Result := UTF8Decode(fList[Index]);
end;

function TOWideStringList.GetName(Index: Integer): OWideString;
begin
  Result := UTF8Decode(fList.Names[Index]);
end;

function TOWideStringList.GetObject(Index: Integer): TObject;
begin
  Result := fList.Objects[Index];
end;

function TOWideStringList.GetOnChange: TNotifyEvent;
begin
  Result := fList.OnChange;
end;

function TOWideStringList.GetOnChanging: TNotifyEvent;
begin
  Result := fList.OnChanging;
end;

{$IFDEF O_DELPHI_6_UP}
function TOWideStringList.GetQuoteChar: Char;
begin
  Result := fList.QuoteChar;
end;

procedure TOWideStringList.SetCaseSensitive(const Value: Boolean);
begin
  fList.CaseSensitive := Value;
end;

procedure TOWideStringList.SetDelimitedText(const Value: OWideString);
begin
  fList.DelimitedText := UTF8Encode(Value);
end;

procedure TOWideStringList.SetDelimiter(const Value: Char);
begin
  fList.Delimiter := Value;
end;

procedure TOWideStringList.SetQuoteChar(const Value: Char);
begin
  fList.QuoteChar := Value;
end;
{$ENDIF}

function TOWideStringList.GetSorted: Boolean;
begin
  Result := fList.Sorted;
end;

function TOWideStringList.GetText: OWideString;
begin
  Result := UTF8Decode(fList.Text);
end;

function TOWideStringList.GetValue(const Name: OWideString): OWideString;
begin
  Result := UTF8Decode(fList.Values[UTF8Encode(Name)]);
end;

function TOWideStringList.IndexOf(const S: OWideString): Integer;
begin
  Result := fList.IndexOf(UTF8Encode(S));
end;

function TOWideStringList.IndexOfName(const Name: OWideString): Integer;
begin
  Result := fList.IndexOfName(UTF8Encode(Name));
end;

function TOWideStringList.IndexOfObject(AObject: TObject): Integer;
begin
  Result := fList.IndexOfObject(AObject);
end;

procedure TOWideStringList.Insert(Index: Integer; const S: OWideString);
begin
  fList.Insert(Index, UTF8Encode(S));
end;

procedure TOWideStringList.InsertObject(Index: Integer; const S: OWideString;
  AObject: TObject);
begin
  fList.InsertObject(Index, UTF8Encode(S), AObject);
end;

procedure TOWideStringList.LoadFromFile(const FileName: string);
begin
  fList.LoadFromFile(FileName);
end;

procedure TOWideStringList.LoadFromStream(Stream: TStream);
begin
  fList.LoadFromStream(Stream);
end;

procedure TOWideStringList.Move(CurIndex, NewIndex: Integer);
begin
  fList.Move(CurIndex, NewIndex);
end;

procedure TOWideStringList.QuickSort(L, R: Integer;
  SCompare: TOWideStringListSortCompare);
var
  I, J, P: Integer;
begin
  BeginUpdate;
  try
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while SCompare(Self, I, P) < 0 do Inc(I);
        while SCompare(Self, J, P) > 0 do Dec(J);
        if I <= J then
        begin
          if I <> J then
            Exchange(I, J);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(L, J, SCompare);
      L := I;
    until I >= R;
  finally
    EndUpdate;
  end;
end;

procedure TOWideStringList.SaveToFile(const FileName: string);
begin
  fList.SaveToFile(FileName);
end;

procedure TOWideStringList.SaveToStream(Stream: TStream);
begin
  fList.SaveToStream(Stream);
end;

procedure TOWideStringList.SetCapacity(const Value: Integer);
begin
  fList.Capacity := Value;
end;

procedure TOWideStringList.SetCommaText(const Value: OWideString);
begin
  fList.CommaText := UTF8Encode(Value);
end;

procedure TOWideStringList.SetDuplicates(const Value: TDuplicates);
begin
  fList.Duplicates := Value;
end;

procedure TOWideStringList.SetI(Index: Integer; const Value: OWideString);
begin
  fList[Index] := UTF8Encode(Value);
end;

procedure TOWideStringList.SetObject(Index: Integer; const Value: TObject);
begin
  fList.Objects[Index] := Value;
end;

procedure TOWideStringList.SetOnChange(const Value: TNotifyEvent);
begin
  fList.OnChange := Value;
end;

procedure TOWideStringList.SetOnChanging(const Value: TNotifyEvent);
begin
  fList.OnChanging := Value;
end;

procedure TOWideStringList.SetSorted(const Value: Boolean);
begin
  fList.Sorted := Value;
end;

procedure TOWideStringList.SetText(const Value: OWideString);
begin
  fList.Text := UTF8Encode(Value);
end;

procedure TOWideStringList.SetValue(const Name, Value: OWideString);
begin
  fList.Values[UTF8Encode(Name)] := UTF8Encode(Value);
end;

function OWideStringListCompareStrings(List: TOWideStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List[Index1], List[Index2]);
end;

procedure TOWideStringList.Sort;
begin
  CustomSort(OWideStringListCompareStrings);
end;

{$IFDEF O_DELPHI_7_UP}
function TOWideStringList.GetNameValueSeparator: Char;
begin
  Result := fList.NameValueSeparator;
end;

procedure TOWideStringList.SetNameValueSeparator(const Value: Char);
begin
  fList.NameValueSeparator := Value;
end;

function TOWideStringList.GetValueFromIndex(Index: Integer): OWideString;
begin
  Result := UTF8Decode(fList.ValueFromIndex[Index]);
end;

procedure TOWideStringList.SetValueFromIndex(Index: Integer;
  const Value: OWideString);
begin
  fList.ValueFromIndex[Index] := UTF8Encode(Value);
end;

{$ENDIF}

{$IFDEF O_DELPHI_2006_UP}
function TOWideStringList.GetLineBreak: OWideString;
begin
  Result := UTF8Decode(fList.LineBreak);
end;

function TOWideStringList.GetStrictDelimiter: Boolean;
begin
  Result := fList.StrictDelimiter;
end;

procedure TOWideStringList.SetLineBreak(const Value: OWideString);
begin
  fList.LineBreak := UTF8Encode(Value);
end;

procedure TOWideStringList.SetStrictDelimiter(const Value: Boolean);
begin
  fList.StrictDelimiter := Value;
end;

{$ENDIF}

{$ENDIF O_UNICODE}//TOWideStringList

{$IFDEF O_GENERICS}
{$IFDEF O_DELPHI_2009}
procedure TList<T>.Exchange(Index1, Index2: Integer);
var
  xTemp: T;
begin
  xTemp := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := xTemp;
end;

procedure TList<T>.Move(CurIndex, NewIndex: Integer);
var
  xTemp: T;
begin
  if CurIndex = NewIndex then
    Exit;
  if (NewIndex < 0) or (NewIndex >= FCount) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  xTemp := FItems[CurIndex];
  FItems[CurIndex] := Default(T);
  if CurIndex < NewIndex then
    System.Move(FItems[CurIndex + 1], FItems[CurIndex], (NewIndex - CurIndex) * SizeOf(T))
  else
    System.Move(FItems[NewIndex], FItems[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(T));

  FillChar(FItems[NewIndex], SizeOf(T), 0);
  FItems[NewIndex] := xTemp;
end;
{$ENDIF}
{$ENDIF}

{ TOTextBuffer }

procedure TOTextBuffer.AssignTo(Dest: TPersistent);
var
  xDest: TOTextBuffer;
begin
  if Dest is TOTextBuffer then
  begin
    xDest := TOTextBuffer(Dest);

    if xDest.fAllocLength < Self.fAllocLength then
    begin
      xDest.fAllocLength := Self.fAllocLength;
      SetLength(xDest.fBuffer, xDest.fAllocLength);
    end;

    xDest.fUsedLength := Self.fUsedLength;
    xDest.fRemaining := Self.fRemaining;

    xDest.fBuffer := Copy(Self.fBuffer, 0, Self.fUsedLength);
  end else
    inherited;
end;

procedure TOTextBuffer.Clear(const aFullClear: Boolean);
begin
  if aFullClear and (fAllocLength > fDefBufferLength) then
  begin
    fAllocLength := fDefBufferLength;
    SetLength(fBuffer, fAllocLength);
  end;

  fUsedLength := 0;
  fRemaining := fAllocLength;
end;

constructor TOTextBuffer.Create(const aBufferLength: Integer);
begin
  inherited Create;

  fDefBufferLength := aBufferLength;
  fAllocLength := aBufferLength;
  fRemaining := fAllocLength;
  fUsedLength := 0;
  SetLength(fBuffer, fAllocLength);
end;

procedure TOTextBuffer.GetBuffer(var outString: OWideString);
begin
  GetBuffer(outString, 1, fUsedLength);
end;

procedure TOTextBuffer.GetBuffer(var outString: OWideString; const aPosition,
  aLength: Integer);
{$IFDEF O_DELPHI_2007_DOWN}
var
  I: Integer;
{$ENDIF}
begin
  SetLength(outString, aLength);
  if aLength > 0 then
  begin
    {$IFDEF O_DELPHI_2007_DOWN}
    //Move() is extremly slow in Delphi 7, copy char-by-char is faster
    for I := 0 to aLength-1 do
      outString[I+1] := fBuffer[aPosition+I-1];
    {$ELSE}
    Move(fBuffer[aPosition-1], outString[1], aLength*SizeOf(OWideChar));
    {$ENDIF}
  end;
end;

function TOTextBuffer.GetBuffer: OWideString;
begin
  GetBuffer(Result{%H-});
end;

procedure TOTextBuffer.Grow(const aMinChars: Integer);
var
  xGrowSize: Integer;
begin
  xGrowSize := 0;
  while fRemaining+xGrowSize < aMinChars do
    Inc(xGrowSize, fAllocLength+xGrowSize);

  Inc(fRemaining, xGrowSize);
  Inc(fAllocLength, xGrowSize);
  SetLength(fBuffer, fAllocLength);
end;

procedure TOTextBuffer.RemoveLastChar;
begin
  Dec(fUsedLength);
  Inc(fRemaining);
  if fUsedLength < 0 then
  begin
    fUsedLength := 0;
    fRemaining := fAllocLength;
  end;
end;

procedure TOTextBuffer.RemoveLastString(const aLength: Integer);
begin
  Dec(fUsedLength, aLength);
  Inc(fRemaining, aLength);
  if fUsedLength < 0 then
  begin
    fUsedLength := 0;
    fRemaining := fAllocLength;
  end;
end;

procedure TOTextBuffer.WriteChar(const aChar: OWideChar);
begin
  if fRemaining = 0 then
    Grow(1);

  Inc(fUsedLength);
  Dec(fRemaining);
  fBuffer[fUsedLength-1] := aChar;
end;

procedure TOTextBuffer.WriteString(const aString: OWideString);
var
  xPos, xLen: Integer;
begin
  WriteString(aString, xPos{%H-}, xLen{%H-});
end;

procedure TOTextBuffer.WriteString(const aString: OWideString; var outPosition,
  outLength: Integer);
{$IFDEF O_DELPHI_2007_DOWN}
var
  I: Integer;
{$ENDIF}
begin
  outLength := Length(aString);
  outPosition := fUsedLength+1;

  if outLength > 0 then
  begin
    if fRemaining < outLength then
      Grow(outLength);

    Inc(fUsedLength, outLength);
    Dec(fRemaining, outLength);

    {$IFDEF O_DELPHI_2007_DOWN}
    //Move() is extremly slow here in Delphi 7, copy char-by-char is faster also for long strings!!! (this may be a delphi bug)
    for I := 0 to outLength-1 do
      fBuffer[outPosition-1+I] := aString[I+1];
    {$ELSE}
    Move(aString[1], fBuffer[outPosition-1], outLength*SizeOf(OWideChar));
    {$ENDIF}
  end;
end;

end.
