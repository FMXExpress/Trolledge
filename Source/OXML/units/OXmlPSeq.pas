unit OXmlPSeq;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlPSeq.pas

  Sequential DOM XML parser based on XmlPDOM.pas
    -> read particular XML elements into DOM and so parse huge XML documents
       with small memory usage but still take advantage of DOM capabilities.
    -> you can also omit some XML passages and get only the information
       that is insteresting to you
    -> OXmlPSeq is faster than OXmlPDOM - there is no significant performance
       penalty when using sequential parser instead of pure DOM. On the contrary
       OXmlPSeq stores less objects in the memory, therefore it is even faster.
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

  OWideSupp, OXmlUtils, OTextReadWrite, OXmlReadWrite, OEncoding, OXmlPDOM;

type

  TXMLSeqParser = class(TObject)
  private
    fReader: TXMLReader;
    fReaderToken: PXMLReaderToken;
    fDataRead: Boolean;
    fTempReaderPath: TOWideStringList;
    fTempNodePath: TOWideStringList;
    fXmlDoc: TXMLDocument;
    fParseError: IOTextParseError;

    function ReadNextChildNodeCustom(const aOnlyElementHeader: Boolean;
      var outElementIsOpen: Boolean): Boolean;

    function GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
    function GetReaderSettings: TXMLReaderSettings;
    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
  protected
    procedure DoCreate; virtual;
    procedure DoInit; virtual;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream; const aForceEncoding: TEncoding = nil); overload;

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
  public
    //seek forward through document to an element path.
    //  the path can be absolute or relative, no XPath support
    //  (with the exception of "//elementName" syntax)
    //  only XML elements supported, no attributes!
    //  (examples: "/root/node/child", "node/child", "//node" etc.)
    function GoToPath(const aPath: OWideString): Boolean;
    //seek to next child XML element and read its name, text nodes are ignored.
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element (i.e. no GoToPath('..') call
    //    is needed).
    function GoToNextChildElement(var outElementName: OWideString): Boolean;

    //seek to next child XML element and read the header, text nodes are ignored.
    //  (e.g. '<child attr="value">' will be read
    //  aElementIsOpen will be set to true if the element is open (<node>).
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element (i.e. no GoToPath('..') call
    //    is needed).
    function ReadNextChildElementHeader(var outNode: PXMLNode;
      var outElementIsOpen: Boolean): Boolean;
    //the same as ReadNextChildElementHeader, but no information is returned
    function SkipNextChildElementHeader(var outElementIsOpen: Boolean): Boolean;

    //seek to next child XML element and read the header, text nodes are ignored.
    //  (e.g. '<child attr="value">' will be read
    //  if element has child nodes, the parser will seek to the closing element
    //  so that the same parent level is reached again
    function ReadNextChildElementHeaderClose(var outNode: PXMLNode): Boolean;

    //seek to next XML node (element, text, CData, etc.) and read the whole
    //  element contents with attributes and children.
    //  (e.g. '<child attr="value">my text<br />2nd line</child>' will be read.
    //  if no child element is found (result=false), the reader position will
    //    be set after the parent's closing element.
    function ReadNextChildNode(var outNode: PXMLNode): Boolean;
    //the same as ReadNextChildNode, but no information is returned
    function SkipNextChildNode: Boolean;

  public
    //document whitespace handling
    //  -> used only in "ReadNextChildNode" for the resulting document
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;
  public
    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    property ParseError: IOTextParseError read fParseError;
  end;

implementation

{ TXMLSeqParser }

constructor TXMLSeqParser.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLSeqParser.Create(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream, aForceEncoding);
end;

destructor TXMLSeqParser.Destroy;
begin
  fReader.Free;
  fTempNodePath.Free;
  fTempReaderPath.Free;
  fXmlDoc.Free;

  inherited;
end;

procedure TXMLSeqParser.DoCreate;
begin
  fReader := TXMLReader.Create;
  fXmlDoc := TXMLDocument.Create;
  fTempNodePath := TOWideStringList.Create;
  fTempReaderPath := TOWideStringList.Create;
end;

procedure TXMLSeqParser.DoInit;
begin
  fDataRead := False;
  fTempReaderPath.Clear;
  fTempNodePath.Clear;
  fXmlDoc.Clear;
end;

function TXMLSeqParser.GetApproxStreamPosition: OStreamInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TXMLSeqParser.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fReader.ReaderSettings;//direct access to reader settings
end;

function TXMLSeqParser.GetStreamSize: OStreamInt;
begin
  Result := fReader.StreamSize;
end;

function TXMLSeqParser.GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
begin
  Result := fXmlDoc.WhiteSpaceHandling;
end;

function TXMLSeqParser.GoToNextChildElement(
  var outElementName: OWideString): Boolean;
begin
  fParseError := nil;

  while fReader.ReadNextToken(fReaderToken) do
  begin
    case fReaderToken.TokenType of
      rtOpenElement:
      begin
        outElementName := fReaderToken.TokenName;
        Result := True;
        Exit;
      end;
      rtCloseElement:
        Break;//Result = False
    end;
  end;

  if Assigned(fReader.ParseError) then
    fParseError := fReader.ParseError;

  outElementName := '';
  Result := False;
end;

function TXMLSeqParser.GoToPath(const aPath: OWideString): Boolean;
begin
  fParseError := nil;

  OExplode(aPath, '/', fTempNodePath);
  fReader.NodePathAssignTo(fTempReaderPath);
  OExpandPath(fTempReaderPath, fTempNodePath);

  if fReader.NodePathMatch(fTempNodePath) then
  begin
    Result := True;
    Exit;
  end;

  while fReader.ReadNextToken(fReaderToken) do
  if (fReaderToken.TokenType in [rtOpenElement, rtCloseElement, rtFinishOpenElementClose]) and
      fReader.NodePathMatch(fTempNodePath)
  then begin
    fDataRead := True;
    Result := True;
    Exit;
  end;

  if Assigned(fReader.ParseError) then
    fParseError := fReader.ParseError;

  Result := False;
  ReleaseDocument;
end;

procedure TXMLSeqParser.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer, aForceEncoding);
  DoInit;
end;

procedure TXMLSeqParser.InitBuffer(const aBuffer; const aBufferLength: Integer;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer, aBufferLength, aForceEncoding);
  DoInit;
end;

procedure TXMLSeqParser.InitFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding);
begin
  fReader.InitFile(aFileName, aForceEncoding);
  DoInit;
end;

procedure TXMLSeqParser.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fReader.InitStream(aStream, aForceEncoding);
  DoInit;
end;

procedure TXMLSeqParser.InitXML(const aXML: OWideString);
begin
  fReader.InitXML(aXML);
  DoInit;
end;

procedure TXMLSeqParser.InitXML_UTF8(const aXML: OUTF8Container);
begin
  fReader.InitXML_UTF8(aXML);
  DoInit;
end;

function TXMLSeqParser.SkipNextChildElementHeader(
  var outElementIsOpen: Boolean): Boolean;
var
  x: PXMLNode;
begin
  Result := ReadNextChildElementHeader(x{%H-}, outElementIsOpen);
end;

function TXMLSeqParser.SkipNextChildNode: Boolean;
var
  x: PXMLNode;
begin
  //use ReadNextChildElementHeaderClose instead of ReadNextChildNode
  //  -> the same result/functionality, but better performance because
  //  the inner nodes are not created
  Result := ReadNextChildElementHeaderClose(x{%H-});
end;

function TXMLSeqParser.ReadNextChildElementHeader(
  var outNode: PXMLNode; var outElementIsOpen: Boolean): Boolean;
begin
  Result := ReadNextChildNodeCustom(True, outElementIsOpen);
  if Result then
    outNode := fXmlDoc.DocumentElement;
end;

function TXMLSeqParser.ReadNextChildElementHeaderClose(
  var outNode: PXMLNode): Boolean;
var
  xElementIsOpen: Boolean;
begin
  Result := ReadNextChildElementHeader(outNode, xElementIsOpen{%H-});
  if Result and xElementIsOpen then
    GoToPath('..');//go back to parent
end;

function TXMLSeqParser.ReadNextChildNode(var outNode: PXMLNode): Boolean;
var
  x: Boolean;
begin
  Result := ReadNextChildNodeCustom(False, x{%H-}) and fXmlDoc.Node.HasChildNodes;

  if Result then
    outNode := fXmlDoc.Node.FirstChild;
end;

type
  TAccessXMLDoc = class(TXMLDocument);

function TXMLSeqParser.ReadNextChildNodeCustom(
  const aOnlyElementHeader: Boolean; var outElementIsOpen: Boolean): Boolean;
var
  xLastNode: PXMLNode;
  xBreakReading: TXMLBreakReading;
begin
  Result := False;
  fParseError := nil;

  TAccessXMLDoc(fXmlDoc).Loading := True;
  try
    fXmlDoc.Clear(False);

    if Assigned(fReaderToken) and (fReaderToken.TokenType = rtOpenElement) then
    begin
      //last found was opening element (most probably from GoToPath()), write it down!
      xLastNode := fXmlDoc.Node.AddChild(fReaderToken.TokenName)
    end else
    begin
      //last found was something else
      xLastNode := fXmlDoc.Node;

      //go to next child
      if aOnlyElementHeader then
      begin
        while fReader.ReadNextToken(fReaderToken) do
        begin
          case fReaderToken.TokenType of
            rtOpenElement://new element found
            begin
              xLastNode := fXmlDoc.Node.AddChild(fReaderToken.TokenName);
              Break;
            end;
            rtCloseElement://parent element may be closed
              Exit;
          end;
        end;

        if xLastNode = fXmlDoc.Node then//next child not found, exit
          Exit;
      end;
    end;

    if not aOnlyElementHeader then
    begin
      //read whole element contents
      xBreakReading := fReader.ReaderSettings.BreakReading;
      fReader.ReaderSettings.BreakReading := brAfterDocumentElement;
      fReader.ResetDocumentElement;
      try
        Result := xLastNode.LoadFromReader(fReader, fReaderToken, fXmlDoc.Node);
      finally
        fReader.ReaderSettings.BreakReading := xBreakReading;
      end;
    end else
    begin
      //read only element header
      while fReader.ReadNextToken(fReaderToken) do
      begin
        case fReaderToken.TokenType of
          rtXMLDeclarationAttribute, rtAttribute:
            xLastNode.Attributes[fReaderToken.TokenName] := fReaderToken.TokenValue;
          rtFinishOpenElement:
          begin
            outElementIsOpen := True;
            Result := True;
            Exit;
          end;
          rtFinishXMLDeclarationClose, rtFinishOpenElementClose, rtCloseElement:
          begin
            outElementIsOpen := False;
            Result := True;
            Exit;
          end;
        end;
      end;//while
    end;//if
  finally
    if Assigned(fReader.ParseError) then
      fParseError := fReader.ParseError;

    TAccessXMLDoc(fXmlDoc).Loading := False;
  end;
end;

procedure TXMLSeqParser.ReleaseDocument;
begin
  fReader.ReleaseDocument;
end;

procedure TXMLSeqParser.SetWhiteSpaceHandling(
  const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
begin
  fXmlDoc.WhiteSpaceHandling := aWhiteSpaceHandling;
end;

end.
