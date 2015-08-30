unit OXmlSAX;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlSAX.pas

  SAX implementation.

  Event-based XML parser.
    -> Events in FPC or older Delphi versions.
    -> Events + anonymous methods in D2009+.
    -> Use the StopParsing() procedure to pause the parsing.
       Parsing can be continued by calling TSAXParser.ContinueParsing() again.

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

  OWideSupp, OXmlUtils, OTextReadWrite, OXmlReadWrite, OEncoding, OHashedStrings;

type
  TSAXParser = class;

  //The clue of TSAXAttribute is to reduce string operations to an minimum.
  // -> therefore TSAXAttribute just uses TXMLReaderToken.TokenName and .TokenValue.
  //    You can explicitely cast TXMLReaderToken to TSAXAttribute and back!
  TSAXAttribute = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fToken: TXMLReaderToken;
  public
    property NodeName: OWideString read fToken.TokenName;
    property NodeValue: OWideString read fToken.TokenValue;
  end;

  PSAXAttribute = ^TSAXAttribute;
  TSAXAttributeEnum = class;
  TSAXAttributes = class(TObject)
  private
    fAttributeTokens: TXMLReaderTokenList;
    fIndex: TOVirtualHashedStrings;//for fast Find/IndexOf function -> use only for more than attribute limit
    fIndexUsed: Boolean;

    fIteratorCurrent: Integer;//for fast Next & Prev

    function GetPrevNext(var ioAttrEnum: PSAXAttribute; const aInc: Integer): Boolean;
    function GetAttributeItem(const aIndex: Integer): PSAXAttribute;
    function GetCount: Integer;

    function GetKeyByIndex(const aIndex: OStringIndex): OWideString;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure CreateIndex;
  public
    function IndexOf(const aAttrName: OWideString): Integer;
    function Has(const aAttrName: OWideString): Boolean;
    //find attribute
    function Find(const aAttrName: OWideString; var outAttrValue: OWideString): Boolean;
    //get attribute
    function Get(const aAttrName: OWideString): OWideString;
    //get attribute, if attr does not exist, return aDefaultValue
    function GetDef(const aAttrName, aDefaultValue: OWideString): OWideString;

    function First: PSAXAttribute;
    function Last: PSAXAttribute;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNext(var ioAttrEnum: PSAXAttribute): Boolean;
    function GetPrevious(var ioAttrEnum: PSAXAttribute): Boolean;

    property Count: Integer read GetCount;
    property Items[const aIndex: Integer]: PSAXAttribute read GetAttributeItem; default;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TSAXAttributeEnum;
    {$ENDIF}
  end;
  TSAXAttributeEnum = class(TObject)
  private
    fIndex: OHashedStringsIndex;
    fSAXAttributes: TSAXAttributes;
  public
    constructor Create(aSAXAttributes: TSAXAttributes);
    function GetCurrent: PSAXAttribute;
    function MoveNext: Boolean;
  public
    property Current: PSAXAttribute read GetCurrent;
  end;

  {$IFDEF O_ANONYMOUS_METHODS}
  TSAXNotifyEvent = reference to procedure(aSaxParser: TSAXParser);
  TSAXTextEvent = reference to procedure(aSaxParser: TSAXParser;
    const aText: OWideString);
  TSAXStartElementEvent = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString; const aAttributes: TSAXAttributes);
  TSAXEndElementEvent = reference to procedure(aSaxParser: TSAXParser;
    const aName: OWideString);
  TSAXXMLDeclarationEvent = reference to procedure(aSaxParser: TSAXParser;
    const aAttributes: TSAXAttributes);
  TSAXProcessingInstructionEvent = reference to procedure(aSaxParser: TSAXParser;
    const aTarget, aContent: OWideString);
  {$ELSE}
  TSAXNotifyEvent = procedure(Sender: TSAXParser) of object;
  TSAXTextEvent = procedure(Sender: TSAXParser; const aText: OWideString) of object;
  TSAXStartElementEvent = procedure(Sender: TSAXParser; const aName: OWideString;
    const aAttributes: TSAXAttributes) of object;
  TSAXEndElementEvent = procedure(Sender: TSAXParser; const aName: OWideString) of object;
  TSAXXMLDeclarationEvent = procedure(aSaxParser: TSAXParser;
    const aAttributes: TSAXAttributes) of object;
  TSAXProcessingInstructionEvent = procedure(Sender: TSAXParser; const aTarget, aContent: OWideString) of object;
  {$ENDIF}

  TSAXParser = class(TObject)
  private
    fURL: OWideString;

    fReader: TXMLReader;
    fDataRead: Boolean;
    fStopParsing: Boolean;

    fOnStartDocument: TSAXNotifyEvent;
    fOnEndDocument: TSAXNotifyEvent;
    fOnCharacters: TSAXTextEvent;
    fOnComment: TSAXTextEvent;
    fOnXMLDeclaration: TSAXXMLDeclarationEvent;
    fOnProcessingInstruction: TSAXProcessingInstructionEvent;
    fOnStartElement: TSAXStartElementEvent;
    fOnEndElement: TSAXEndElementEvent;

    fParseError: IOTextParseError;

    procedure DoOnStartDocument;
    procedure DoOnEndDocument;
    procedure DoOnCharacters(const aText: OWideString);
    procedure DoOnComment(const aText: OWideString);
    procedure DoOnXMLDeclaration(const aAttributes: TSAXAttributes);
    procedure DoOnProcessingInstruction(const aTarget, aContent: OWideString);
    procedure DoOnStartElement(const aName: OWideString;
      const aAttributes: TSAXAttributes);
    procedure DoOnEndElement(const aName: OWideString);

    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetReaderSettings: TXMLReaderSettings;
  protected
    function StartParsing: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //The Parse* functions open an XML document and start parsing it
    //  they return "True" if the document was sucessfully parsed to the end
    //  (they return "False" if the parsing has been stopped with aStop parameter)

    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil): Boolean;
    //parse document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //parse XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function ParseXML(const aXML: OWideString): Boolean;
    function ParseXML_UTF8(const aXML: OUTF8Container): Boolean;
    //parse document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function ParseBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean; overload;
    function ParseBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil): Boolean; overload;
  public
    //call StopParsing from an event or anonymous method to stop parsing
    //  When stopped, parsing cannot be continued again.
    procedure StopParsing;
  public
    //The beginning of a document. The SAX parser will invoke this method only once, before any other event callbacks.
    property OnStartDocument: TSAXNotifyEvent read fOnStartDocument write fOnStartDocument;
    //reached the end of the document
    property OnEndDocument: TSAXNotifyEvent read fOnEndDocument write fOnEndDocument;
    //text or CData
    property OnCharacters: TSAXTextEvent read fOnCharacters write fOnCharacters;
    //comment
    property OnComment: TSAXTextEvent read fOnComment write fOnComment;
    //XML Declaration <?xml ... ?>
    property OnXMLDeclaration: TSAXXMLDeclarationEvent read fOnXMLDeclaration write fOnXMLDeclaration;
    //Processing Instruction <?php ... ?>
    property OnProcessingInstruction: TSAXProcessingInstructionEvent read fOnProcessingInstruction write fOnProcessingInstruction;
    //start of an element <a href="title">
    property OnStartElement: TSAXStartElementEvent read fOnStartElement write fOnStartElement;
    //end of an element </a> or <a />
    property OnEndElement: TSAXEndElementEvent read fOnEndElement write fOnEndElement;

    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;
  public
    //following functions and properties can be called only from events or anonymous methods during parsing

    //functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;

    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //determines if parsing has been stopped with the StopParsing procedure
    property ParsingStopped: Boolean read fStopParsing;

    //ParseError has information about the error that occured when parsing a document
    property ParseError: IOTextParseError read fParseError;
  end;

  ESAXParserException = class(Exception);

implementation

{ TSAXParser }

constructor TSAXParser.Create;
begin
  inherited Create;

  fReader := TXMLReader.Create;
end;

destructor TSAXParser.Destroy;
begin
  fReader.Free;

  inherited;
end;

procedure TSAXParser.DoOnCharacters(const aText: OWideString);
begin
  if Assigned(fOnCharacters) then
    fOnCharacters(Self, aText);
end;

procedure TSAXParser.DoOnComment(const aText: OWideString);
begin
  if Assigned(fOnComment) then
    fOnComment(Self, aText);
end;

procedure TSAXParser.DoOnEndDocument;
begin
  if Assigned(fOnEndDocument) then
    fOnEndDocument(Self);
end;

procedure TSAXParser.DoOnEndElement(const aName: OWideString);
begin
  if Assigned(fOnEndElement) then
    fOnEndElement(Self, aName);
end;

procedure TSAXParser.DoOnProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if Assigned(fOnProcessingInstruction) then
    fOnProcessingInstruction(Self, aTarget, aContent);
end;

procedure TSAXParser.DoOnStartDocument;
begin
  if Assigned(fOnStartDocument) then
    fOnStartDocument(Self);
end;

procedure TSAXParser.DoOnStartElement(const aName: OWideString;
  const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnStartElement) then
    fOnStartElement(Self, aName, aAttributes);
end;

procedure TSAXParser.DoOnXMLDeclaration(const aAttributes: TSAXAttributes);
begin
  if Assigned(fOnXMLDeclaration) then
    fOnXMLDeclaration(Self, aAttributes);
end;

function TSAXParser.GetApproxStreamPosition: OStreamInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TSAXParser.GetNodePath(const aIndex: Integer): OWideString;
begin
  Result := fReader.NodePath[aIndex];
end;

function TSAXParser.GetNodePathCount: Integer;
begin
  Result := fReader.NodePathCount;
end;

function TSAXParser.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fReader.ReaderSettings;
end;

function TSAXParser.GetStreamSize: OStreamInt;
begin
  Result := fReader.StreamSize;
end;

function TSAXParser.ParseBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetBuffer(aBuffer);

    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetPointer(@aBuffer, aBufferLength);

    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TOFileStream;
begin
  fURL := aFileName;

  xStream := TOFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := ParseStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  try
    fReader.InitStream(aStream, aForceEncoding);
    fReader.URL := fURL;

    Result := StartParsing;
  finally
    fReader.ReleaseDocument;
  end;
end;

function TSAXParser.ParseXML(const aXML: OWideString): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString(aXML);

    Result := ParseStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

function TSAXParser.ParseXML_UTF8(const aXML: OUTF8Container): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString_UTF8(aXML);

    Result := ParseStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;

procedure TSAXParser.NodePathAssignTo(const aNodePath: TOWideStringList);
begin
  fReader.NodePathAssignTo(aNodePath);
end;

function TSAXParser.NodePathAsString: OWideString;
begin
  Result := fReader.NodePathAsString;
end;

function TSAXParser.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.NodePathMatch(const aNodePath: OWideString): Boolean;
begin
  Result := fReader.NodePathMatch(aNodePath);
end;

function TSAXParser.StartParsing: Boolean;
  procedure _StartDocument;
  begin
    DoOnStartDocument;
    fDataRead := True;
  end;
var
  xAttributes: TSAXAttributes;
  xReaderToken: PXMLReaderToken;
begin
  fParseError := nil;
  Result := True;
  fDataRead := False;
  fStopParsing := False;

  xAttributes := TSAXAttributes.Create;
  fReader.SetAttributeTokens(xAttributes.fAttributeTokens);
  try
    while (not fStopParsing) and fReader.ReadNextToken(xReaderToken{%H-}) do
    begin
      case xReaderToken.TokenType of
        rtText, rtCData, rtEntityReference:
          if fDataRead or not OXmlIsWhiteSpace(xReaderToken.TokenValue)
          then//omit empty text before root node
          begin
            if not fDataRead then
              _StartDocument;
            DoOnCharacters(xReaderToken.TokenValue);
          end;
      else//case
        if not fDataRead then
          _StartDocument;
        case xReaderToken.TokenType of
          rtFinishOpenElementClose, rtFinishOpenElement:
          begin
            xAttributes.CreateIndex;
            DoOnStartElement(xReaderToken.TokenName, xAttributes);
            if xReaderToken.TokenType = rtFinishOpenElementClose then
              DoOnEndElement(xReaderToken.TokenName);
          end;
          rtFinishXMLDeclarationClose:
          begin
            xAttributes.CreateIndex;
            DoOnXMLDeclaration(xAttributes);
          end;
          rtCloseElement: DoOnEndElement(xReaderToken.TokenName);
          rtComment: DoOnComment(xReaderToken.TokenValue);
          rtProcessingInstruction: DoOnProcessingInstruction(xReaderToken.TokenName, xReaderToken.TokenValue);
        end;
      end;
    end;

    if fDataRead and not fStopParsing then
      DoOnEndDocument;

  finally
    if Assigned(fReader.ParseError) then
    begin
      fParseError := fReader.ParseError;

      Result := False;
    end;

    fReader.SetAttributeTokens(nil);
    xAttributes.Free;

    fURL := '';
  end;
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsChildOfNodePath(aRefNodePath);
end;

function TSAXParser.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
begin
  Result := fReader.RefIsParentOfNodePath(aRefNodePath);
end;

procedure TSAXParser.StopParsing;
begin
  fStopParsing := True;
end;

{ TSAXAttributes }

constructor TSAXAttributes.Create;
begin
  inherited;

  fAttributeTokens := TXMLReaderTokenList.Create;
  fIndex := TOVirtualHashedStrings.Create(GetKeyByIndex);
end;

procedure TSAXAttributes.CreateIndex;
var
  I: Integer;
begin
  fIndexUsed := Count > XMLUseIndexNodeLimit;
  if fIndexUsed then
  begin
    fIndex.Clear(False);
    for I := 0 to fAttributeTokens.Count-1 do
      fIndex.Add(I);
  end;
end;

destructor TSAXAttributes.Destroy;
begin
  fAttributeTokens.Free;
  fIndex.Free;

  inherited;
end;

function TSAXAttributes.Find(const aAttrName: OWideString;
  var outAttrValue: OWideString): Boolean;
var
  I: Integer;
begin
  I := IndexOf(aAttrName);
  Result := I >= 0;
  if Result then
    outAttrValue := fAttributeTokens[I].TokenValue
  else
    outAttrValue := '';
end;

function TSAXAttributes.Has(const aAttrName: OWideString): Boolean;
begin
  Result := (IndexOf(aAttrName) >= 0);
end;

function TSAXAttributes.IndexOf(const aAttrName: OWideString): Integer;
var
  I: Integer;
begin
  if fIndexUsed then//hash index used
    Result := fIndex.StringIndexOf(aAttrName)
  else
  begin//hash index not used
    for I := 0 to Count-1 do
    if fAttributeTokens[I].TokenName = aAttrName then
    begin
      Result := I;
      Exit;
    end;
    Result := -1;
  end;
end;

function TSAXAttributes.First: PSAXAttribute;
begin
  if fAttributeTokens.Count >= 0 then
    Result := PSAXAttribute(fAttributeTokens[0])
  else
    Result := nil;
end;

function TSAXAttributes.GetAttributeItem(const aIndex: Integer): PSAXAttribute;
begin
  Result := PSAXAttribute(fAttributeTokens[aIndex]);
end;

function TSAXAttributes.Get(const aAttrName: OWideString): OWideString;
begin
  Find(aAttrName, Result{%H-});
end;

function TSAXAttributes.GetCount: Integer;
begin
  Result := fAttributeTokens.Count;
end;

function TSAXAttributes.GetDef(const aAttrName,
  aDefaultValue: OWideString): OWideString;
var
  I: Integer;
begin
  I := IndexOf(aAttrName);
  if I >= 0 then
    Result := fAttributeTokens[I].TokenValue
  else
    Result := aDefaultValue;
end;

function TSAXAttributes.GetNext(
  var ioAttrEnum: PSAXAttribute): Boolean;
begin
  Result := GetPrevNext(ioAttrEnum, +1);
end;

function TSAXAttributes.GetPrevious(var ioAttrEnum: PSAXAttribute): Boolean;
begin
  Result := GetPrevNext(ioAttrEnum, -1);
end;

function TSAXAttributes.GetPrevNext(var ioAttrEnum: PSAXAttribute;
  const aInc: Integer): Boolean;
var
  xCount: Integer;
begin
  //same code as TXMLResNodeList.GetPrevNext
  Result := False;
  xCount := Count;
  if xCount = 0 then
  begin
    ioAttrEnum := nil;
    Exit;
  end;

  if Assigned(ioAttrEnum) then
  begin
    //get prev/next
    if not(
       (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) and
       (PSAXAttribute(fAttributeTokens[fIteratorCurrent]) = ioAttrEnum))
    then//ioAttrEnum is NOT the last iterator -> we have to find it
      fIteratorCurrent := fAttributeTokens.IndexOf(PXMLReaderToken(ioAttrEnum));

    if (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) then
    begin
      fIteratorCurrent := fIteratorCurrent + aInc;
      Result := (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount);
      if Result then
        ioAttrEnum := PSAXAttribute(fAttributeTokens[fIteratorCurrent])
      else
        ioAttrEnum := nil;
    end;
  end else
  begin
    //return first or last element
    if aInc > 0 then
      fIteratorCurrent := 0
    else
      fIteratorCurrent := xCount-1;
    ioAttrEnum := PSAXAttribute(fAttributeTokens[fIteratorCurrent]);
    Result := True;
  end;
end;

function TSAXAttributes.GetKeyByIndex(const aIndex: OStringIndex): OWideString;
begin
  Result := fAttributeTokens[aIndex].TokenName;
end;

function TSAXAttributes.Last: PSAXAttribute;
begin
  if fAttributeTokens.Count >= 0 then
    Result := PSAXAttribute(fAttributeTokens[fAttributeTokens.Count-1])
  else
    Result := nil;
end;

{$IFDEF O_ENUMERATORS}
function TSAXAttributes.GetEnumerator: TSAXAttributeEnum;
begin
  Result := TSAXAttributeEnum.Create(Self);
end;
{$ENDIF}

{ TSAXAttributeEnum }

constructor TSAXAttributeEnum.Create(aSAXAttributes: TSAXAttributes);
begin
  inherited Create;

  fIndex := -1;
  fSAXAttributes := aSAXAttributes;
end;

function TSAXAttributeEnum.GetCurrent: PSAXAttribute;
begin
  Result := fSAXAttributes[fIndex];
end;

function TSAXAttributeEnum.MoveNext: Boolean;
begin
  Result := (fIndex < fSAXAttributes.Count - 1);
  if Result then
    Inc(fIndex);
end;

end.
