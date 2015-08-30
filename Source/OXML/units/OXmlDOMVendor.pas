unit OXmlDOMVendor;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlDOMVendor.pas

  DOM vendor for Delphi's xmldom.TXMLDocument based on OXml.

  The fastest and least memory hungry vendor available for Delphi.

  Please note that some features are not supported. E.g. XML namespaces.
  To find functions that are not implemented, look for the comment "nyi".

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
  {$IFDEF MSWINDOWS}
    {$IFDEF O_NAMESPACES}
    Winapi.ActiveX,
    System.Win.ComObj,
    {$ELSE}
    ActiveX,
    ComObj,
    {$ENDIF}
  {$ELSE}
    {$IFDEF O_NAMESPACES}
    System.Types,
    {$ELSE}
    Types,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF O_NAMESPACES}
  System.SysUtils, System.Classes, System.Variants, Xml.xmldom,
  {$ELSE}
  SysUtils, Classes, Variants, xmldom,
  {$ENDIF}

  OWideSupp, OXmlUtils, OTextReadWrite, OXmlPDOM;

const
  sOXmlDOMVendor = 'OXml';

type
  {$IF DEFINED(O_DELPHI_2010_UP) AND NOT DEFINED(O_DELPHI_XE4_UP)}
  loadxmlString = WideString;
  {$ELSE}
  loadxmlString = DOMString;
  {$IFEND}
  {$IFDEF NEXTGEN}
  xmlWideString = DOMString;
  {$ELSE}
  xmlWideString = WideString;
  {$ENDIF}

  IOXmlNodeRef = interface
    ['{570BE209-B302-4968-A968-F84691AC9A4B}']
    function GetOXmlNode: PXMLNode;
  end;

  TOXmlDOMInterface = class(TInterfacedObject)
  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;
  end;

  TOXmlDOMImplementation = class(TOXmlDOMInterface, IDOMImplementation)
  protected
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;
      doctype: IDOMDocumentType): IDOMDocument; safecall;
  end;

  TOXmlDOMNode = class;
  TOXmlDOMNodeClass = class of TOXmlDOMNode;

  TOXmlDOMNode = class(TOXmlDOMInterface, IOXmlNodeRef, IDOMNode, IDOMNodeEx,
    IDOMNodeSelect)
  private
    FOXmlNode: PXMLNode;
    FChildNodes: IDOMNodeList;
    FAttributes: IDOMNamedNodeMap;
    FOwnerDocument: IDOMDocument;
  protected
    { IOXmlNodeRef }
    function GetOXmlNode: PXMLNode;
    { IDOMNode }
    function get_nodeName: DOMString; safecall;
    function get_nodeValue: DOMString; safecall;
    procedure set_nodeValue(value: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_nodeType: DOMNodeType; safecall;
    function get_parentNode: IDOMNode; safecall;
    function get_childNodes: IDOMNodeList; safecall;
    function get_firstChild: IDOMNode; safecall;
    function get_lastChild: IDOMNode; safecall;
    function get_previousSibling: IDOMNode; safecall;
    function get_nextSibling: IDOMNode; safecall;
    function get_attributes: IDOMNamedNodeMap; safecall;
    function get_ownerDocument: IDOMDocument; safecall;
    function get_namespaceURI: DOMString; safecall;
    function get_prefix: DOMString; safecall;
    function get_localName: DOMString; safecall;
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    procedure normalize; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function supports(const feature, version: DOMString): WordBool;
    { IDOMNodeEx }
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure transformNode(const stylesheet: IDOMNode; var output: xmlWideString); overload; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); overload; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    { IDOMNodeSelect }
    function selectNode(const nodePath: xmlWideString): IDOMNode; safecall;
    function selectNodes(const nodePath: xmlWideString): IDOMNodeList; safecall;
    procedure set_text(const Value: DOMString); safecall;
  public
    constructor Create(ANode: PXMLNode);
    property OXmlNode: PXMLNode read FOXmlNode;
  end;

  TOXmlDOMResNodeList = class(TOXmlDOMInterface, IDOMNodeList)
  private
     FOXmlNodeList: IXMLNodeList;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(ANodeList: IXMLNodeList);
    property OXmlNodeList: IXMLNodeList read FOXmlNodeList;
  end;

  TOXmlDOMChildNodeList = class(TOXmlDOMInterface, IDOMNodeList)
  private
     FOXmlNodeList: TXMLChildNodeList;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(ANodeList: TXMLChildNodeList);
    property OXmlNodeList: TXMLChildNodeList read FOXmlNodeList;
  end;

  TOXmlDOMNameSpaceFunc = function(const name: DOMString): IDOMNode of object; safecall;

  TOXmlDOMAttrNamedNodeMap = class(TOXmlDOMInterface, IDOMNamedNodeMap)
  private
    FOXmlNode: PXMLNode;
  protected
    { IDOMNamedNodeMap }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function setNamedItem(const newItem: IDOMNode): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
  public
    constructor Create(ANode: PXMLNode);
    property OXmlNode: PXMLNode read FOXmlNode;
  end;

  TOXmlDOMCharacterData = class(TOXmlDOMNode, IDOMCharacterData)
  protected
    { IDOMCharacterData }
    function get_data: DOMString; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_data(const data: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_length: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function substringData(offset, count: Integer): DOMString; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure appendData(const data: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure insertData(offset: Integer; const data: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure deleteData(offset, count: Integer); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure replaceData(offset, count: Integer; const data: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
  end;

  TOXmlDOMAttr = class(TOXmlDOMNode, IDOMAttr)
  protected
    { Property Get/Set }
    function get_name: DOMString; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_specified: WordBool; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_value: DOMString; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_value(const attributeValue: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_ownerElement: IDOMElement; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    { Properties }
    property name: DOMString read get_name;
    property specified: WordBool read get_specified;
    property value: DOMString read get_value write set_value;
    property ownerElement: IDOMElement read get_ownerElement;
  end;

  TOXmlDOMElement = class(TOXmlDOMNode, IDOMElement)
  protected
    { IDOMElement }
    function get_tagName: DOMString; safecall;
    function getAttribute(const name: DOMString): DOMString; safecall;
    procedure setAttribute(const name, value: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure removeAttribute(const name: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;
    procedure setAttributeNS(const namespaceURI, qualifiedName, value: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure removeAttributeNS(const namespaceURI, localName: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall;
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function hasAttribute(const name: DOMString): WordBool; safecall;
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
  end;

  TOXmlDOMText = class(TOXmlDOMCharacterData, IDOMText)
  protected
    function splitText(offset: Integer): IDOMText; safecall;
  end;

  TOXmlDOMComment = class(TOXmlDOMCharacterData, IDOMComment)
  end;

  TOXmlDOMCDATASection = class(TOXmlDOMText, IDOMCDATASection)
  end;

  TOXmlDOMDocumentType = class(TOXmlDOMNode, IDOMDocumentType)
  protected
    { IDOMDocumentType }
    function get_name: DOMString; safecall;
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_internalSubset: DOMString; safecall;
  end;

  TOXmlDOMEntityReference = class(TOXmlDOMNode, IDOMEntityReference)
  end;

  TOXmlDOMProcessingInstruction = class(TOXmlDOMNode, IDOMProcessingInstruction)
  protected
    { IDOMProcessingInstruction }
    function get_target: DOMString; safecall;
    function get_data: DOMString; safecall;
    procedure set_data(const value: DOMString); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
  end;

  TOXmlDOMDocumentFragment = class(TOXmlDOMNode, IDOMDocumentFragment)
  end;

  TOXmlDOMDocument = class(TOXmlDOMNode, IDOMDocument, IDOMParseOptions, IDOMPersist,
    IDOMParseError)
  private
    FDOMImplementation: IDOMImplementation;
    FOXmlDocument: IXMLDocument;
  protected
    { IDOMDocument }
    function get_doctype: IDOMDocumentType; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function get_documentElement: IDOMElement; safecall;
    procedure set_documentElement(const DOMElement: IDOMElement); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function createElement(const tagName: DOMString): IDOMElement; safecall;
    function createDocumentFragment: IDOMDocumentFragment; safecall;
    function createTextNode(const data: DOMString): IDOMText; safecall;
    function createComment(const data: DOMString): IDOMComment; safecall;
    function createCDATASection(const data: DOMString): IDOMCDATASection; safecall;
    function createProcessingInstruction(const target,
      data: DOMString): IDOMProcessingInstruction; safecall;
    function createAttribute(const name: DOMString): IDOMAttr; safecall;
    function createEntityReference(const name: DOMString): IDOMEntityReference; safecall;
    function getElementsByTagName(const tagName: DOMString): IDOMNodeList; safecall;
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall;
    function createElementNS(const namespaceURI,
      qualifiedName: DOMString): IDOMElement; safecall;
    function createAttributeNS(const namespaceURI,
      qualifiedName: DOMString): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function getElementById(const elementId: DOMString): IDOMElement; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    { IDOMParseOptions }
    function get_async: Boolean; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_preserveWhiteSpace: Boolean; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_resolveExternals: Boolean; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_validate: Boolean; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_async(Value: Boolean); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_preserveWhiteSpace(Value: Boolean); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_resolveExternals(Value: Boolean); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    procedure set_validate(Value: Boolean); {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    { IDOMPersist }
    function get_xml: DOMString; safecall;
    function asyncLoadState: Integer; safecall;
    function load(source: OleVariant): WordBool; safecall;
    function loadFromStream(const stream: TStream): WordBool; overload;safecall;
    function loadxml(const Value: loadxmlString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    procedure saveToStream(const stream: TStream); overload;safecall;
    procedure set_OnAsyncLoad(const Sender: TObject;
      EventHandler: TAsyncEventHandler); safecall;
    function loadFromStream(const stream: IStream): WordBool; overload; safecall;
    procedure saveToStream(const stream: IStream); overload; safecall;
    { IDOMParseError }
    function get_errorCode: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_url: DOMString; safecall;
    function get_reason: DOMString; safecall;
    function get_srcText: DOMString; safecall;
    function get_line: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_linepos: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
    function get_filepos: Integer; {$IFDEF O_DELPHI_7_UP}safecall;{$ENDIF}
  public
    constructor Create(ADocument: IXMLDocument;
      ADomImplementation: IDOMImplementation);
  public
    property OXmlDocument: IXMLDocument read FOXmlDocument;
  end;

  TOXmlDOMImplementationFactory = class(TDOMVendor)
  public
    function DOMImplementation: IDOMImplementation; override;
    function Description: string; override;
  end;

  TDOMIStreamAdapter = class(TStream)
  private
    FStream: IStream;
  public
    constructor Create(const Stream: IStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

var
  OXmlFactory: TOXmlDOMImplementationFactory;

implementation

var
  SNodeExpected: OWideString = 'Node cannot be null';

var
  GlobalOXmlDOMImpl: IDOMImplementation;

function MakeNode(Node: PXMLNode): IDOMNode;
var
  xNewClass: TOXmlDOMNodeClass;
begin
  if Node <> nil then
  begin
    case Node.NodeType of
      ntDocument: xNewClass := TOXmlDOMDocument;
      ntDocType: xNewClass := TOXmlDOMDocumentType;
      ntXMLDeclaration: xNewClass := TOXmlDOMProcessingInstruction;
      ntElement: xNewClass := TOXmlDOMElement;
      ntAttribute: xNewClass := TOXmlDOMAttr;
      ntText: xNewClass := TOXmlDOMText;
      ntEntityReference: xNewClass := TOXmlDOMEntityReference;
      ntCData: xNewClass := TOXmlDOMCDATASection;
      ntComment: xNewClass := TOXmlDOMComment;
      ntProcessingInstruction: xNewClass := TOXmlDOMProcessingInstruction;
    else
      raise Exception.Create('MakeNode: wrong implementation');
    end;
    Result := xNewClass.Create(Node)
  end
  else
    Result := nil;
end;

function MakeChildNodeList(const NodeList: TXMLChildNodeList): IDOMNodeList;
begin
  Result := TOXmlDOMChildNodeList.Create(NodeList);
end;

function MakeResNodeList(const NodeList: IXMLNodeList): IDOMNodeList;
begin
  Result := TOXmlDOMResNodeList.Create(NodeList);
end;

function MakeOXmlNode(const Node: IDOMNode): PXMLNode;
begin
  if not Assigned(Node) then
    raise DOMException.Create(SNodeExpected);
  Result := (Node as IOXmlNodeRef).GetOXmlNode;
end;

function MakeAttrNamedNodeMap(const Node: PXMLNode): IDOMNamedNodeMap;
begin
  Result := TOXmlDOMAttrNamedNodeMap.Create(Node);
end;

{ TDOMIStreamAdapter }

constructor TDOMIStreamAdapter.Create(const Stream: IStream);
begin
  inherited Create;
  FStream := Stream;
end;

function TDOMIStreamAdapter.Read(var Buffer; Count: Longint): Longint;
begin
  FStream.Read(@Buffer, Count, @Result)
end;

function TDOMIStreamAdapter.Write(const Buffer; Count: Longint): Longint;
begin
  FStream.Write(@Buffer, Count, @Result);
end;

function TDOMIStreamAdapter.Seek(Offset: Longint; Origin: Word): Longint;
var
  Pos: {$IFDEF O_DELPHI_XE8_UP}UInt64{$ELSE}LargeInt{$ENDIF};
begin
  FStream.Seek(Offset, Origin, Pos);
  Result := Longint(Pos);
end;

{ TOXmlDOMInterface }

function TOXmlDOMInterface.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
begin
  Result := inherited SafeCallException(ExceptObject, ExceptAddr);//nyi ???
end;

{ TOXmlDOMImplementation }

function TOXmlDOMImplementation.createDocument(const namespaceURI,
  qualifiedName: DOMString; doctype: IDOMDocumentType): IDOMDocument;
var
  xXmlDoc: IXMLDocument;
begin
  xXmlDoc := CreateXMLDoc;
  xXmlDoc.ReaderSettings.ErrorHandling := ehRaiseAndEat;//to get more information about the XML error in debug-time
  Result := TOXmlDOMDocument.Create(xXmlDoc, Self);
end;

function TOXmlDOMImplementation.createDocumentType(const qualifiedName, publicId,
  systemId: DOMString): IDOMDocumentType;
begin
  DOMVendorNotSupported('createDocumentType', sOXmlDOMVendor);
end;

function TOXmlDOMImplementation.hasFeature(const feature,
  version: DOMString): WordBool;
begin
  Result := False;
end;

{ TOXmlDOMNode }

function TOXmlDOMNode.appendChild(const newChild: IDOMNode): IDOMNode;
var
  xNewOXmlChild,
  xReturnedChild: PXMLNode;
begin
  xNewOXmlChild := MakeOXmlNode(newChild);
  xReturnedChild := FOXmlNode.appendChild(xNewOXmlChild);
  if xReturnedChild = xNewOXmlChild then
    Result := newChild
  else
    Result := MakeNode(xReturnedChild);
end;

function TOXmlDOMNode.cloneNode(deep: WordBool): IDOMNode;
begin
  Result := MakeNode(FOXmlNode.CloneNode(deep));
end;

constructor TOXmlDOMNode.Create(ANode: PXMLNode);
begin
  Assert(Assigned(ANode));
  FOXmlNode := ANode;

  inherited Create;
end;

function TOXmlDOMNode.GetOXmlNode: PXMLNode;
begin
  Result := FOXmlNode;
end;

function TOXmlDOMNode.get_attributes: IDOMNamedNodeMap;
begin
  if not Assigned(FAttributes) and Assigned(FOXmlNode.AttributeNodes) then
    FAttributes := MakeAttrNamedNodeMap(FOXmlNode);
  Result := FAttributes;
end;

function TOXmlDOMNode.get_childNodes: IDOMNodeList;
begin
  if not Assigned(FChildNodes) then
    FChildNodes := MakeChildNodeList(FOXmlNode.ChildNodes);
  Result := FChildNodes;
end;

function TOXmlDOMNode.get_firstChild: IDOMNode;
begin
  Result := MakeNode(FOXmlNode.FirstChild);
end;

function TOXmlDOMNode.get_lastChild: IDOMNode;
begin
  Result := MakeNode(FOXmlNode.LastChild);
end;

function TOXmlDOMNode.get_localName: DOMString;
begin
  Result := FOXmlNode.LocalName;
end;

function TOXmlDOMNode.get_namespaceURI: DOMString;
begin
  Result := FOXmlNode.NameSpaceURI;
end;

function TOXmlDOMNode.get_nextSibling: IDOMNode;
begin
  Result := MakeNode(FOXmlNode.NextSibling);
end;

function TOXmlDOMNode.get_nodeName: DOMString;
begin
  Result := FOXmlNode.NodeName;
end;

function TOXmlDOMNode.get_nodeType: DOMNodeType;
begin
  case FOXmlNode.NodeType of
    ntDocument: Result := DOCUMENT_NODE;
    ntDocType: Result := DOCUMENT_TYPE_NODE;
    ntXMLDeclaration: Result := PROCESSING_INSTRUCTION_NODE;
    ntElement: Result := ELEMENT_NODE;
    ntAttribute: Result := ATTRIBUTE_NODE;
    ntText: Result := TEXT_NODE;
    ntEntityReference: Result := ENTITY_REFERENCE_NODE;
    ntCData: Result := CDATA_SECTION_NODE;
    ntComment: Result := COMMENT_NODE;
    ntProcessingInstruction: Result := PROCESSING_INSTRUCTION_NODE;
  else
    raise Exception.Create('TOXmlDOMNode.get_nodeType: wrong implementation');
  end;
end;

function TOXmlDOMNode.get_nodeValue: DOMString;
begin
  Result := FOXmlNode.NodeValue;
end;

function TOXmlDOMNode.get_ownerDocument: IDOMDocument;
begin
  if not Assigned(FOwnerDocument) then
    FOwnerDocument := TOXmlDOMDocument.Create(FOXmlNode.OwnerDocument, GlobalOXmlDOMImpl);
  Result := FOwnerDocument;
end;

function TOXmlDOMNode.get_parentNode: IDOMNode;
begin
  Result := MakeNode(FOXmlNode.ParentNode);
end;

function TOXmlDOMNode.get_prefix: DOMString;
begin
  Result := FOXmlNode.NameSpacePrefix;
end;

function TOXmlDOMNode.get_previousSibling: IDOMNode;
begin
  Result := MakeNode(FOXmlNode.PreviousSibling);
end;

function TOXmlDOMNode.get_text: DOMString;
begin
  Result := FOXmlNode.Text;
end;

function TOXmlDOMNode.get_xml: DOMString;
begin
  Result := FOXmlNode.XML(itNone);
end;

function TOXmlDOMNode.hasChildNodes: WordBool;
begin
  Result := FOXmlNode.HasChildNodes;
end;

function TOXmlDOMNode.insertBefore(const newChild,
  refChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(FOXmlNode.InsertBefore(MakeOXmlNode(newChild), MakeOXmlNode(refChild)));
end;

procedure TOXmlDOMNode.normalize;
begin
  FOXmlNode.Normalize;
end;

function TOXmlDOMNode.removeChild(const childNode: IDOMNode): IDOMNode;
begin
  Result := MakeNode(FOXmlNode.RemoveChild(MakeOXmlNode(childNode)));
end;

function TOXmlDOMNode.replaceChild(const newChild,
  oldChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(FOXmlNode.ReplaceChild(MakeOXmlNode(newChild), MakeOXmlNode(oldChild)));
end;

function TOXmlDOMNode.selectNode(const nodePath: xmlWideString): IDOMNode;
var
  xNode: PXMLNode;
begin
  if FOXmlNode.SelectNode(nodePath, xNode) then
    Result := MakeNode(xNode)
  else
    Result := nil;
end;

function TOXmlDOMNode.selectNodes(const nodePath: xmlWideString): IDOMNodeList;
var
  xNodeList: IXMLNodeList;
begin
  if FOXmlNode.SelectNodes(nodePath, xNodeList) then
    Result := MakeResNodeList(xNodeList)
  else
    Result := nil;
end;

procedure TOXmlDOMNode.set_nodeValue(value: DOMString);
begin
  FOXmlNode.NodeValue := value;
end;

procedure TOXmlDOMNode.set_text(const Value: DOMString);
begin
  FOXmlNode.Text := value;
end;

function TOXmlDOMNode.supports(const feature, version: DOMString): WordBool;
begin
  DOMVendorNotSupported('supports', sOXmlDOMVendor);
  Result := False;
end;

procedure TOXmlDOMNode.transformNode(const stylesheet: IDOMNode;
  var output: xmlWideString);
begin
  DOMVendorNotSupported('transformNode', sOXmlDOMVendor);
end;

procedure TOXmlDOMNode.transformNode(const stylesheet: IDOMNode;
  const output: IDOMDocument);
begin
  DOMVendorNotSupported('transformNode', sOXmlDOMVendor);
end;

{ TOXmlDOMResNodeList }

constructor TOXmlDOMResNodeList.Create(ANodeList: IXMLNodeList);
begin
  inherited Create;

  FOXmlNodeList := ANodeList;
end;

function TOXmlDOMResNodeList.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(FOXmlNodeList[index]);
end;

function TOXmlDOMResNodeList.get_length: Integer;
begin
  Result := FOXmlNodeList.Count;
end;

{ TOXmlDOMChildNodeList }

constructor TOXmlDOMChildNodeList.Create(ANodeList: TXMLChildNodeList);
begin
  inherited Create;

  FOXmlNodeList := ANodeList;
end;

function TOXmlDOMChildNodeList.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(FOXmlNodeList[index]);
end;

function TOXmlDOMChildNodeList.get_length: Integer;
begin
  Result := FOXmlNodeList.Count;
end;

{ TOXmlDOMAttrNamedNodeMap }

constructor TOXmlDOMAttrNamedNodeMap.Create(ANode: PXMLNode);
begin
  inherited Create;

  FOXmlNode := ANode;
end;

function TOXmlDOMAttrNamedNodeMap.getNamedItem(const name: DOMString): IDOMNode;
var
  xAttr: PXMLNode;
begin
  if FOXmlNode.FindAttribute(name, xAttr) then
    Result := MakeNode(xAttr)
  else
    Result := nil;
end;

function TOXmlDOMAttrNamedNodeMap.getNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
var
  xAttr: PXMLNode;
begin
  if FOXmlNode.FindAttributeNS(namespaceURI, localName, xAttr) then
    Result := MakeNode(xAttr)
  else
    Result := nil;
end;

function TOXmlDOMAttrNamedNodeMap.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(FOXmlNode.AttributeFromBegin[index]);
end;

function TOXmlDOMAttrNamedNodeMap.get_length: Integer;
begin
  Result := FOXmlNode.AttributeCount;
end;

function TOXmlDOMAttrNamedNodeMap.removeNamedItem(
  const name: DOMString): IDOMNode;
begin
  Result := getNamedItem(name);
  if Assigned(Result) then
    MakeOXmlNode(Result).RemoveSelfFromParent;
end;

function TOXmlDOMAttrNamedNodeMap.removeNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  Result := getNamedItemNS(namespaceURI, localName);
  if Assigned(Result) then
    MakeOXmlNode(Result).RemoveSelfFromParent;
end;

function TOXmlDOMAttrNamedNodeMap.setNamedItem(
  const newItem: IDOMNode): IDOMNode;
begin
  FOXmlNode.SetAttributeNode(MakeOXmlNode(newItem));
  Result := newItem;
end;

function TOXmlDOMAttrNamedNodeMap.setNamedItemNS(const arg: IDOMNode): IDOMNode;
begin
  Result := setNamedItem(arg);
end;

{ TOXmlDOMCharacterData }

procedure TOXmlDOMCharacterData.appendData(const data: DOMString);
begin
  FOXmlNode.Text := FOXmlNode.Text + data;
end;

procedure TOXmlDOMCharacterData.deleteData(offset, count: Integer);
{var
  xText: OWideString;}
begin
  //nyi - what about 0-based strings?
  //    - what about widestrings in D7-D2007?
  {xText := get_data;
  Delete(xText, offset, count);
  set_data(xText);}

  DOMVendorNotSupported('deleteData', sOXmlDOMVendor)
end;

function TOXmlDOMCharacterData.get_data: DOMString;
begin
  Result := FOXmlNode.NodeValue;
end;

function TOXmlDOMCharacterData.get_length: Integer;
begin
  Result := Length(get_data);
end;

procedure TOXmlDOMCharacterData.insertData(offset: Integer;
  const data: DOMString);
{var
  xText: OWideString;}
begin
  //nyi - what about 0-based strings?
  //    - what about widestrings in D7-D2007?
  {xText := get_data;
  Insert(data, xText, offset);
  set_data(xText);}

  DOMVendorNotSupported('insertData', sOXmlDOMVendor)
end;

procedure TOXmlDOMCharacterData.replaceData(offset, count: Integer;
  const data: DOMString);
{var
  xText: OWideString;}
begin
  //nyi - what about 0-based strings?
  //    - what about widestrings in D7-D2007?
  {xText := get_data;
  Delete(xText, offset, count);
  Insert(data, xText, offset);
  set_data(xText);}

  DOMVendorNotSupported('replaceData', sOXmlDOMVendor)
end;

procedure TOXmlDOMCharacterData.set_data(const data: DOMString);
begin
  FOXmlNode.NodeValue := data;
end;

function TOXmlDOMCharacterData.substringData(offset, count: Integer): DOMString;
begin
  //nyi - what about 0-based strings?
  //    - what about widestrings in D7-D2007?
  //Result := Copy(get_data, offset, count);

  DOMVendorNotSupported('substringData', sOXmlDOMVendor)
end;

{ TOXmlDOMAttr }

function TOXmlDOMAttr.get_name: DOMString;
begin
  Result := get_nodeName;
end;

function TOXmlDOMAttr.get_ownerElement: IDOMElement;
var
  xNode: IDOMNode;
begin
  xNode := get_parentNode;
  {$IFDEF O_NAMESPACES}System.{$ENDIF}SysUtils.Supports(xNode, IDOMElement, Result);
end;

function TOXmlDOMAttr.get_specified: WordBool;
begin
  DOMVendorNotSupported('get_specified', sOXmlDOMVendor);
  Result := True;
end;

function TOXmlDOMAttr.get_value: DOMString;
begin
  Result := get_nodeValue;
end;

procedure TOXmlDOMAttr.set_value(const attributeValue: DOMString);
begin
  set_nodeValue(attributeValue);
end;

{ TOXmlDOMElement }

function TOXmlDOMElement.getAttribute(const name: DOMString): DOMString;
begin
  FOXmlNode.FindAttribute(name, Result);
end;

function TOXmlDOMElement.getAttributeNode(const name: DOMString): IDOMAttr;
var
  xNode: PXMLNode;
begin
  if FOXmlNode.FindAttribute(name, xNode) then
    Result := MakeNode(xNode) as IDOMAttr
  else
    Result := nil;
end;

function TOXmlDOMElement.getAttributeNodeNS(const namespaceURI,
  localName: DOMString): IDOMAttr;
var
  xNode: PXMLNode;
begin
  if FOXmlNode.FindAttributeNS(namespaceURI, localName, xNode) then
    Result := MakeNode(xNode) as IDOMAttr
  else
    Result := nil;
end;

function TOXmlDOMElement.getAttributeNS(const namespaceURI,
  localName: DOMString): DOMString;
begin
  FOXmlNode.FindAttributeNS(namespaceURI, localName, Result);
end;

function TOXmlDOMElement.getElementsByTagName(
  const name: DOMString): IDOMNodeList;
var
  xNodeList: IXMLNodeList;
begin
  if FOXmlNode.GetElementsByTagName(name, xNodeList) then
    Result := MakeResNodeList(xNodeList)
  else
    Result := nil;
end;

function TOXmlDOMElement.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
var
  xNodeList: IXMLNodeList;
begin
  if FOXmlNode.GetElementsByTagNameNS(namespaceURI, localName, xNodeList) then
    Result := MakeResNodeList(xNodeList)
  else
    Result := nil;
end;

function TOXmlDOMElement.get_tagName: DOMString;
begin
  Result := get_nodeName;
end;

function TOXmlDOMElement.hasAttribute(const name: DOMString): WordBool;
begin
  Result := FOXmlNode.HasAttribute(name);
end;

function TOXmlDOMElement.hasAttributeNS(const namespaceURI,
  localName: DOMString): WordBool;
begin
  Result := FOXmlNode.HasAttributeNS(namespaceURI, localName);
end;

procedure TOXmlDOMElement.removeAttribute(const name: DOMString);
begin
  FOXmlNode.DeleteAttribute(name);
end;

function TOXmlDOMElement.removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr;
var
  xNode: PXMLNode;
begin
  xNode := FOXmlNode.RemoveAttribute(MakeOXmlNode(oldAttr));
  if Assigned(xNode) then
    Result := MakeNode(xNode) as IDOMAttr
  else
    Result := nil;
end;

procedure TOXmlDOMElement.removeAttributeNS(const namespaceURI,
  localName: DOMString);
begin
  FOXmlNode.DeleteAttributeNS(namespaceURI, localName);
end;

procedure TOXmlDOMElement.setAttribute(const name, value: DOMString);
begin
  FOXmlNode.SetAttribute(name, value);
end;

function TOXmlDOMElement.setAttributeNode(const newAttr: IDOMAttr): IDOMAttr;
var
  xNode: PXMLNode;
begin
  xNode := FOXmlNode.SetAttributeNode(MakeOXmlNode(newAttr));
  if Assigned(xNode) then
    Result := MakeNode(xNode) as IDOMAttr
  else
    Result := nil;
end;

function TOXmlDOMElement.setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr;
begin
  Result := setAttributeNode(newAttr);
end;

procedure TOXmlDOMElement.setAttributeNS(const namespaceURI, qualifiedName,
  value: DOMString);
begin
  FOXmlNode.SetAttributeNS(namespaceURI, qualifiedName, value);
end;

{ TOXmlDOMText }

function TOXmlDOMText.splitText(offset: Integer): IDOMText;
begin
  DOMVendorNotSupported('splitText', sOXmlDOMVendor);
end;

{ TOXmlDOMDocumentType }

function TOXmlDOMDocumentType.get_entities: IDOMNamedNodeMap;
begin
  Result := nil;//nyi
end;

function TOXmlDOMDocumentType.get_internalSubset: DOMString;
begin
  Result := '';//nyi
end;

function TOXmlDOMDocumentType.get_name: DOMString;
begin
  Result := '';//nyi
end;

function TOXmlDOMDocumentType.get_notations: IDOMNamedNodeMap;
begin
  Result := nil;//nyi
end;

function TOXmlDOMDocumentType.get_publicId: DOMString;
begin
  Result := '';//nyi
end;

function TOXmlDOMDocumentType.get_systemId: DOMString;
begin
  Result := '';//nyi
end;

{ TOXmlDOMProcessingInstruction }

function TOXmlDOMProcessingInstruction.get_data: DOMString;
begin
  Result := get_nodeValue;
end;

function TOXmlDOMProcessingInstruction.get_target: DOMString;
begin
  Result := get_nodeName;
end;

procedure TOXmlDOMProcessingInstruction.set_data(const value: DOMString);
begin
  set_nodeValue(value);
end;

{ TOXmlDOMDocument }

function TOXmlDOMDocument.asyncLoadState: Integer;
begin
  Result := 0;//not supported
end;

constructor TOXmlDOMDocument.Create(ADocument: IXMLDocument;
  ADomImplementation: IDOMImplementation);
begin
  Assert(Assigned(ADocument));
  Assert(Assigned(ADomImplementation));

  inherited Create(ADocument.Node);

  FOXmlDocument := ADocument;
  FDOMImplementation := ADomImplementation;
end;

function TOXmlDOMDocument.createAttribute(const name: DOMString): IDOMAttr;
begin
  Result := TOXmlDOMAttr.Create(FOXmlDocument.CreateAttribute(name));
end;

function TOXmlDOMDocument.createAttributeNS(const namespaceURI,
  qualifiedName: DOMString): IDOMAttr;
begin
  Result := TOXmlDOMAttr.Create(FOXmlDocument.CreateAttributeNS(namespaceURI, qualifiedName));
end;

function TOXmlDOMDocument.createCDATASection(
  const data: DOMString): IDOMCDATASection;
begin
  Result := TOXmlDOMCDATASection.Create(FOXmlDocument.CreateCDATASection(data));
end;

function TOXmlDOMDocument.createComment(const data: DOMString): IDOMComment;
begin
  Result := TOXmlDOMComment.Create(FOXmlDocument.CreateComment(data));
end;

function TOXmlDOMDocument.createDocumentFragment: IDOMDocumentFragment;
begin
  Result := TOXmlDOMDocumentFragment.Create(FOXmlDocument.CreateElement(''));
end;

function TOXmlDOMDocument.createElement(const tagName: DOMString): IDOMElement;
begin
  Result := TOXmlDOMElement.Create(FOXmlDocument.CreateElement(tagName));
end;

function TOXmlDOMDocument.createElementNS(const namespaceURI,
  qualifiedName: DOMString): IDOMElement;
begin
  Result := TOXmlDOMElement.Create(FOXmlDocument.CreateElementNS(namespaceURI, qualifiedName));
end;

function TOXmlDOMDocument.createEntityReference(
  const name: DOMString): IDOMEntityReference;
begin
  Result := TOXmlDOMEntityReference.Create(FOXmlDocument.CreateEntityReference(name));
end;

function TOXmlDOMDocument.createProcessingInstruction(const target,
  data: DOMString): IDOMProcessingInstruction;
var
  xNewNode: PXMLNode;
begin
  if target = XML_XML then
  begin
    xNewNode := FOXmlDocument.CreateXMLDeclaration;
    xNewNode.NodeValue := data;
  end
  else
    xNewNode := FOXmlDocument.CreateProcessingInstruction(target, data);

  Result := TOXmlDOMProcessingInstruction.Create(xNewNode);
end;

function TOXmlDOMDocument.createTextNode(const data: DOMString): IDOMText;
begin
  Result := TOXmlDOMText.Create(FOXmlDocument.CreateTextNode(data));
end;

function TOXmlDOMDocument.getElementById(
  const elementId: DOMString): IDOMElement;
begin
  DOMVendorNotSupported('getElementById', sOXmlDOMVendor)
end;

function TOXmlDOMDocument.getElementsByTagName(
  const tagName: DOMString): IDOMNodeList;
var
  xNodeList: IXMLNodeList;
begin
  if FOXmlDocument.Node.SelectNodes(tagName, xNodeList) then
    Result := MakeResNodeList(xNodeList)
  else
    Result := nil;
end;

function TOXmlDOMDocument.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
var
  xNodeList: IXMLNodeList;
begin
  if FOXmlDocument.Node.GetElementsByTagNameNS(namespaceURI, localName, xNodeList) then
    Result := MakeResNodeList(xNodeList)
  else
    Result := nil;
end;

function TOXmlDOMDocument.get_async: Boolean;
begin
  Result := False;
end;

function TOXmlDOMDocument.get_doctype: IDOMDocumentType;
var
  xNode: PXMLNode;
begin
  xNode := nil;
  while FOXmlDocument.Node.GetNextChild(xNode) do
  if xNode.NodeType = ntDocType then
  begin
    Result := TOXmlDOMDocumentType.Create(xNode);
    Exit;
  end;

  Result := nil;
end;

function TOXmlDOMDocument.get_documentElement: IDOMElement;
begin
  Result := MakeNode(FOXmlDocument.DocumentElement) as IDOMElement;
end;

function TOXmlDOMDocument.get_domImplementation: IDOMImplementation;
begin
  Result := FDOMImplementation;
end;

function TOXmlDOMDocument.get_errorCode: Integer;
begin
  Result := FOXmlDocument.ParseError.ErrorCode;
end;

function TOXmlDOMDocument.get_filepos: Integer;
begin
  Result := FOXmlDocument.ParseError.FilePos;
end;

function TOXmlDOMDocument.get_line: Integer;
begin
  Result := FOXmlDocument.ParseError.Line;
end;

function TOXmlDOMDocument.get_linepos: Integer;
begin
  Result := FOXmlDocument.ParseError.LinePos;
end;

function TOXmlDOMDocument.get_preserveWhiteSpace: Boolean;
begin
  Result := (FOXmlDocument.WhiteSpaceHandling = wsPreserveAll);
end;

function TOXmlDOMDocument.get_reason: DOMString;
begin
  Result := FOXmlDocument.ParseError.Reason;
end;

function TOXmlDOMDocument.get_resolveExternals: Boolean;
begin
  Result := False;
end;

function TOXmlDOMDocument.get_srcText: DOMString;
begin
  Result := FOXmlDocument.ParseError.SrcText;
end;

function TOXmlDOMDocument.get_url: DOMString;
begin
  Result := FOXmlDocument.ParseError.URL;
end;

function TOXmlDOMDocument.get_validate: Boolean;
begin
  Result := False;
end;

function TOXmlDOMDocument.get_xml: DOMString;
begin
  Result := FOXmlDocument.XML;
end;

function TOXmlDOMDocument.importNode(importedNode: IDOMNode;
  deep: WordBool): IDOMNode;
begin
  DOMVendorNotSupported('importNode', sOXmlDOMVendor);
end;

function TOXmlDOMDocument.load(source: OleVariant): WordBool;
begin
  if VarType(source) = varOleStr then
  begin
    Result := FOXmlDocument.LoadFromFile(source);
    FOXmlNode := FOXmlDocument.Node;//node must be refreshed!!!
  end
  else
    DOMVendorNotSupported('load(object)', sOXmlDOMVendor);
end;

function TOXmlDOMDocument.loadFromStream(const stream: IStream): WordBool;
var
  xStream: TStream;
begin
  xStream := TDOMIStreamAdapter.Create(stream);
  try
    Result := loadFromStream(xStream);
  finally
    xStream.Free;
  end;
end;

function TOXmlDOMDocument.loadFromStream(const stream: TStream): WordBool;
begin
  Result := FOXmlDocument.LoadFromStream(stream);
  FOXmlNode := FOXmlDocument.Node;//node must be refreshed!!!
end;

function TOXmlDOMDocument.loadxml(const Value: loadxmlString): WordBool;
begin
  Result := FOXmlDocument.LoadFromXML(Value);
  FOXmlNode := FOXmlDocument.Node;//node must be refreshed!!!
end;

procedure TOXmlDOMDocument.save(destination: OleVariant);
begin
  if VarType(destination) = varOleStr then
    FOXmlDocument.SaveToFile(destination)
  else
    DOMVendorNotSupported('save(object)', sOXmlDOMVendor);
end;

procedure TOXmlDOMDocument.saveToStream(const stream: IStream);
var
  xStream: TStream;
begin
  xStream := TDOMIStreamAdapter.Create(stream);
  try
    saveToStream(xStream);
  finally
    xStream.Free;
  end;
end;

procedure TOXmlDOMDocument.saveToStream(const stream: TStream);
begin
  FOXmlDocument.SaveToStream(stream)
end;

procedure TOXmlDOMDocument.set_async(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_async(True)', sOXmlDOMVendor);
end;

procedure TOXmlDOMDocument.set_documentElement(const DOMElement: IDOMElement);
begin
  if Assigned(DOMElement) then
    FOXmlDocument.DocumentElement := MakeOXmlNode(DOMElement)
  else if Assigned(FOXmlDocument.DocumentElement) then
    FOXmlDocument.DocumentElement.DeleteSelf;
end;

procedure TOXmlDOMDocument.set_OnAsyncLoad(const Sender: TObject;
  EventHandler: TAsyncEventHandler);
begin
  DOMVendorNotSupported('set_OnAsyncLoad', sOXmlDOMVendor);
end;

procedure TOXmlDOMDocument.set_preserveWhiteSpace(Value: Boolean);
begin
  if Value then
    FOXmlDocument.WhiteSpaceHandling := wsPreserveAll
  else
    FOXmlDocument.WhiteSpaceHandling := wsTrim;
end;

procedure TOXmlDOMDocument.set_resolveExternals(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_resolveExternals(True)', sOXmlDOMVendor);
end;

procedure TOXmlDOMDocument.set_validate(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_validate(True)', sOXmlDOMVendor);
end;

{ TOXmlDOMImplementationFactory }

function TOXmlDOMImplementationFactory.Description: string;
begin
  Result := sOXmlDOMVendor;
end;

function TOXmlDOMImplementationFactory.DOMImplementation: IDOMImplementation;
begin
  if not Assigned(GlobalOXmlDOMImpl) then
  begin
    GlobalOXmlDOMImpl := TOXmlDOMImplementation.Create;
  end;
  Result := GlobalOXmlDOMImpl;
end;

initialization
  OXmlFactory := TOXmlDOMImplementationFactory.Create;
  RegisterDOMVendor(OXmlFactory);

finalization

  UnRegisterDOMVendor(OXmlFactory);
  OXmlFactory.Free;
  GlobalOXmlDOMImpl := nil;
end.
