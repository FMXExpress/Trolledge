unit OXmlPDOM;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlPDOM.pas

  XML DOM record/pointer implementation

  Simplified W3C DOM (Core) Level 1 specification:
    http://www.w3.org/TR/REC-DOM-Level-1/level-one-core.html
  - OXmlPDOM uses record-based nodes for maximum performance.

  Very close to MSXML/OmniXML implementations but much faster.

  Uses a never-reallocated node buffer for very fast creation and destruction
  of nodes.

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
  ODictionary,
  {$ENDIF}

  OWideSupp, OTextReadWrite, OXmlReadWrite, OEncoding, OHashedStrings,
  OXmlUtils, OXmlXPath;

type

  IXMLDocument = interface;
  TXMLDocument = class;
  XMLNodeId = OXmlReadWrite.XMLNodeId;
  PXMLNode = ^TXMLNode;

  IXMLNodeList = interface;
  TXMLChildNodeList = class;
  TXMLAttributeIndex = class;

  {$IFDEF O_GENERICS}
  TXMLNodeIndex = TDictionary<OHashedStringsIndex,PXMLNode>;
  TXMLNodeList = TList<PXMLNode>;
  {$ELSE}
  TXMLNodeIndex = TODictionary;
  TXMLNodeList = TList;
  {$ENDIF}

  {$IFDEF O_ANONYMOUS_METHODS}
  TXMLNodeCompare = reference to function(const aNode1, aNode2: PXMLNode): Integer;
  {$ELSE}
  TXMLNodeCompare = function(const aNode1, aNode2: PXMLNode): Integer;
  {$ENDIF}

  TXMLNode = packed {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    fNodeType: TXMLNodeType;
    fNodeNameId: OHashedStringsIndex;
    fNodeValueId: OHashedStringsIndex;
    fParentNode: PXMLNode;

    fFirstCChild: Array[TXMLChildType] of PXMLNode;
    fLastCChild: Array[TXMLChildType] of PXMLNode;
    fNextSibling: PXMLNode;
    fPreviousSibling: PXMLNode;

    fPreserveWhiteSpace: TXMLPreserveWhiteSpace;

    fOwnerDocument: TXMLDocument;
  private
    //methods to work with child/attribute nodes
    procedure Append(const aNew: PXMLNode; const aChildType: TXMLChildType);
    procedure Insert(const aNew, aBeforeNode: PXMLNode; const aChildType: TXMLChildType);
    procedure Remove(const aOld: PXMLNode; const aChildType: TXMLChildType);

    function GetNextCChild(var ioChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetPreviousCChild(var ioChildEnum: PXMLNode; const aChildType: TXMLChildType): Boolean;
    function GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
    function GetLastCChild(const aChildType: TXMLChildType): PXMLNode;
    function GetCChildFromBegin(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;
    function GetCChildFromEnd(const aIndex: Integer; const aChildType: TXMLChildType): PXMLNode;
    {$IFDEF BCB}
    function GetChildFromBegin(const aIndex: Integer): PXMLNode; {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetChildFromEnd(const aIndex: Integer): PXMLNode; {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetAttributeFromBegin(const aIndex: Integer): PXMLNode; {$IFDEF O_INLINE}inline;{$ENDIF}
    function GetAttributeFromEnd(const aIndex: Integer): PXMLNode; {$IFDEF O_INLINE}inline;{$ENDIF}
    {$ENDIF}
    procedure DeleteCChildren(const aDestroyList: Boolean; const aChildType: TXMLChildType);
    function FindCChild(const aNodeNameId: OHashedStringsIndex; const aChildType: TXMLChildType;
      var outNode: PXMLNode; var outNodeSearchedCount: Integer): Boolean;
    procedure FillCChildList(const aList: IXMLNodeList; const aChildType: TXMLChildType);
    function FindTempNameSpaceURI(const aNameSpacePrefix: OWideString;
      var outNameSpace: OWideString; const aSearchInParent: Boolean): Boolean;
  private
    function GetId: XMLNodeId;
    function GetNodeName: OWideString;
    function GetNodeValue: OWideString;
    function GetLocalName: OWideString;
    function GetNameSpacePrefix: OWideString;
    function GetNameSpaceURI: OWideString;
    procedure SetNodeName(const aName: OWideString);
    procedure SetNodeNameId(const aNameId: OHashedStringsIndex);
    procedure SetNodeValue(const aValue: OWideString);
    function GetDoPreserveWhiteSpace: Boolean;
    function GetText: OWideString;
    procedure SetText(const aText: OWideString);
    function _AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
    procedure _SetAttribute(const aAttrName, aAttrValue: OWideString);

    function AddCustomChild(const aType: TXMLNodeType; const aName, aValue: OWideString): PXMLNode;
    function InsertCustomChild(const aType: TXMLNodeType; const aName, aValue: OWideString;
      const aBeforeNode: PXMLNode): PXMLNode;

    function GetAttributeNodes: TXMLChildNodeList;
    function GetChildNodes: TXMLChildNodeList;
    function GetAttributeCount: Integer;
    function GetChildCount: Integer;
    function TryGetChildNodes(var outList: TXMLChildNodeList; const aChildType: TXMLChildType): Boolean;
    function GetIsTextElement: Boolean;
    //assign only basic properties: PreserveWhiteSpace
    procedure AssignProperties(const aFromNode: PXMLNode);
    function GetNextNodeInTree: PXMLNode;
    function GetPreviousNodeInTree: PXMLNode;
    function GetAbsolutePath: OWideString;
    function GetNodeLevel: Integer;
    function BuildChildrenIndex: TXMLNodeIndex;

    procedure QuickSort(aLow, aHigh: Integer; const aCompare: TXMLNodeCompare;
      const aChildNodeList: IXMLNodeList);
  private
    //methods for direct reading/writing
    procedure WriteChildrenXML(const aWriter: TXMLWriter);
    procedure WriteAttributesXML(const aWriter: TXMLWriter);
  private
    procedure Init(const aNodeType: TXMLNodeType;
      const aNodeNameId, aNodeValueId: OHashedStringsIndex;
      const aOwnerDocument: TXMLDocument);

    function FindNameSpace(const aNameSpaceURIId: OHashedStringsIndex;
      const aNameSpacePrefix: OWideString): Boolean;
    procedure FindNameSpacePrefixes(const aNameSpaceURIId: OHashedStringsIndex;
      const aNameSpacePrefixes: TOWideStringList);
    function FindNameSpaceURI(const aAttrNameId: OHashedStringsIndex;
      var outNameSpaceURI: OWideString): Boolean;
    //find all possible (and existing) qualified names for a localname with URI
    procedure FindQualifiedNames(const aNameSpaceURIId: OHashedStringsIndex;
      const aLocalName: OWideString;
      var ioQualifiedNameIds: TXMLIntArray);
  public
    //create and append an element child
    function AddChild(const aElementName: OWideString): PXMLNode;
    //create and add an attribute.
    //  if attr does not exist, it will be appended to the end.
    //  if attr exists, its value will be replaced but the attr won't be moved to the end.
    //  return attribute node
    function AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
    //create and append an XML declaration child
    function AddXMLDeclaration: PXMLNode;
    //create and append a text child
    function AddText(const aText: OWideString): PXMLNode;
    //create and append an entity reference
    function AddEntityReference(const aEntityName: OWideString): PXMLNode;
    //create and append a CData child
    function AddCDATASection(const aText: OWideString): PXMLNode;
    //create and append a comment child
    function AddComment(const aText: OWideString): PXMLNode;
    //create and append a DOCTYPE child
    function AddDocType(const aDocTypeRawText: OWideString): PXMLNode;
    //create and append a processing instruction child
    function AddProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    //create and insert an element child
    function InsertChild(const aElementName: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    //create and insert an attribute
    //  return attribute node
    function InsertAttribute(const aAttrName, aAttrValue: OWideString; const aBeforeAttribute: PXMLNode): PXMLNode; overload;
    function InsertAttribute(const aAttrName, aAttrValue: OWideString; const aBeforeAttributeName: OWideString): PXMLNode; overload;
    //etc.
    function InsertXMLDeclaration(const aBeforeNode: PXMLNode): PXMLNode;
    function InsertText(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertEntityReference(const aEntityName: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertCDATASection(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertComment(const aText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertDocType(const aDocTypeRawText: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
    function InsertProcessingInstruction(const aTarget, aContent: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
  public
    //find all possible prefixes for a namespace URI in current element scope
    function FindNameSpacePrefixesByURI(const aNameSpaceURI: OWideString;
      const aNameSpacePrefixes: TOWideStringList): Boolean;
    //find the namespace URI from prefix in current element scope
    function FindNameSpaceURIByPrefix(const aNameSpacePrefix: OWideString;
      var outNameSpaceURI: OWideString): Boolean;
    //check if the URI-prefix namespace combination exists in current element scope
    function NameSpaceExists(const aNameSpaceURI, aNameSpacePrefix: OWideString): Boolean;
  public
    function HasChildNodes: Boolean;
    function HasAttributes: Boolean;
    function HasAttribute(const aName: OWideString): Boolean;
    function HasAttributeNS(const aNameSpaceURI, aLocalName: OWideString): Boolean;
    function FindAttribute(const aName: OWideString; var outAttr: PXMLNode): Boolean; overload;
    function FindAttribute(const aName: OWideString; var outValue: OWideString): Boolean; overload;
    function FindAttributeById(const aNameId: OHashedStringsIndex; var outAttr: PXMLNode): Boolean;
    function FindAttributeNS(const aNameSpaceURI, aLocalName: OWideString; var outAttr: PXMLNode): Boolean; overload;
    function FindAttributeNS(const aNameSpaceURI, aLocalName: OWideString; var outValue: OWideString): Boolean; overload;
    function FindAttributeByValue(const aValue: OWideString; var outAttr: PXMLNode): Boolean; overload;
    function FindAttributeByValue(const aValue: OWideString; var outName: OWideString): Boolean; overload;
    function FindAttributeByValueId(const aValueId: OHashedStringsIndex; var outAttr: PXMLNode): Boolean;
    //get attribute
    function GetAttribute(const aName: OWideString): OWideString;
    function GetAttributeNS(const aNameSpaceURI, aLocalName: OWideString): OWideString;
    //get attribute, if attr does not exist, return aDefaultValue
    function GetAttributeDef(const aName, aDefaultValue: OWideString): OWideString;
    function GetAttributeNSDef(const aNameSpaceURI, aLocalName, aDefaultValue: OWideString): OWideString;
    //set attribute and return self
    function SetAttribute(const aName, aValue: OWideString): PXMLNode;
    function SetAttributeNS(const aNameSpaceURI, aQualifiedName, aValue: OWideString): PXMLNode;

    //atribute nodes
    // for performance reasons please avoid using AttributeNodes - use FirstAttribute+NextSibling instead!
    // read "Performance optimizations" on http://www.kluug.net/oxml.php for further details
    property AttributeNodes: TXMLChildNodeList read GetAttributeNodes;
    property AttributeCount: Integer read GetAttributeCount;
    //element children
    // for performance reasons please avoid using ChildNodes - use FirstChild+NextSibling instead!
    // read "Performance optimizations" on http://www.kluug.net/oxml.php for further details
    property ChildNodes: TXMLChildNodeList read GetChildNodes;
    property ChildCount: Integer read GetChildCount;

    //iterate through all children from first to last (get first for ioChildEnum=nil)
    function GetNextChild(var ioChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from first to last (get first for aAttributeEnum=nil)
    function GetNextAttribute(var ioAttrEnum: PXMLNode): Boolean;
    //iterate through all children from last to first (get last for ioChildEnum=nil)
    function GetPreviousChild(var ioChildEnum: PXMLNode): Boolean;
    //iterate through all attributes from last to first (get last for ioAttrEnum=nil)
    function GetPreviousAttribute(var ioAttrEnum: PXMLNode): Boolean;

    procedure DeleteAttribute(const aName: OWideString); overload;
    procedure DeleteAttribute(const aAttr: PXMLNode); overload;
    procedure DeleteAttributeNS(const aNameSpaceURI, aLocalName: OWideString);
    procedure DeleteAttributes(const aDestroyList: Boolean = True);
    procedure DeleteChild(const aChild: PXMLNode);
    procedure DeleteChildren(const aDestroyList: Boolean = True);
    procedure DeleteSelf;//free current node, both for NextGen and "normal" Pascal
    procedure RemoveSelfFromParent;//remove self from parent node list, the node doesn't get destroyed
    procedure ExchangeWithNode(const aNode: PXMLNode);//exchange with another node

    //insert a node before another
    //  Inserts the node aNewChild before the existing child node aRefChild.
    //  If aRefChild is nil, insert aNewChild at the end of the list of children.
    //  If the aNewChild is already in the tree, it is first removed.
    function InsertBefore(const aNewChild, aRefChild: PXMLNode): PXMLNode;
    //replace a child
    //  Replaces the child node oldChild with aNewChild in the list of children, and returns the aOldChild node.
    //  If the aNewChild is already in the tree, it is first removed.
    //  The removed child is not destroyed in any case!
    function ReplaceChild(const aNewChild, aOldChild: PXMLNode): PXMLNode;
    //remove a child
    //  Removes the child node indicated by aOldChild from the list of children, and returns it.
    //  The removed child is not destroyed in any case!
    function RemoveChild(const aOldChild: PXMLNode): PXMLNode;
    //append a child
    //  Adds the node aNewChild to the end of the list of children of this node.
    //  If the aNewChild is already in the tree, it is first removed.
    function AppendChild(const aNewChild: PXMLNode): PXMLNode;
    //find child by name
    function FindChild(const aName: OWideString; var outNode: PXMLNode): Boolean;
    function FindChildById(const aNameId: OHashedStringsIndex; var outNode: PXMLNode): Boolean;
    //find child by name with index
    //  !!! important - the children names HAVE to be unique !!!
    //  the index must be assigned before the function call (to nil if it has to be generated automatically)
    //  the index is created only when necessary (OXml decides)
    //  the index must be destroyed manually with .Free when not needed any more!
    //  the index speeds up multiple search
    function FindChildWithIndex(const aName: OWideString; var outNode: PXMLNode; var ioIndex: TXMLNodeIndex): Boolean;
    function FindChildByIdWithIndex(const aNameId: OHashedStringsIndex; var outNode: PXMLNode; var ioIndex: TXMLNodeIndex): Boolean;
    //get attribute node by name
    function GetAttributeNode(const aAttrName: OWideString): PXMLNode;
    //set attribute
    //  if the aAttr replaces an existing attribute with the same name, the previously existing Attr node is returned, otherwise nil is returned.
    function SetAttributeNode(const aAttr: PXMLNode): PXMLNode;
    //remove an attribute
    //  Removes the attribute node indicated by aOldAttribute from the list of children, and returns it.
    //  The removed attribute is not destroyed in any case!
    function RemoveAttribute(const aOldAttribute: PXMLNode): PXMLNode;
    //clone node
    //  aDeep = false: only node with attributes,
    //  aDeep = true: node with attributes and all child tree
    //  aToDocument: the document that will be the owner of the created node, nil = the same document
    //  + append the created node after cloning with TXMLNode.AppendChild()
    function CloneNode(const aDeep: Boolean): PXMLNode; overload;
    function CloneNode(const aDeep: Boolean; const aToDocument: TXMLDocument): PXMLNode; overload;
    function CloneNode(const aDeep: Boolean; const aToDocument: IXMLDocument): PXMLNode; overload;
    //consolidate adjacent text nodes and remove any empty text nodes
    procedure Normalize;
  public
    //sort child nodes
    //  aDeep = false: only one level child nodes
    //  aDeep = true: whole node tree
    procedure SortChildNodes(const aCompare: TXMLNodeCompare; const aDeep: Boolean = False);
    procedure SortAttributeNodes(const aCompare: TXMLNodeCompare);
    procedure SortChildNodesByName(const aDeep: Boolean = False);
    procedure SortAttributeNodesByName;
  public
    //select the first node by XPath, if not found return false (and outNode=nil)
    function SelectNode(const aXPath: OWideString; var outNode: PXMLNode): Boolean; overload;
    //select the first node by XPath, if not found return nil
    function SelectNode(const aXPath: OWideString): PXMLNode; overload;
    //select the first node by XPath, if not found return a fake "dummy" node (name="", value="")
    function SelectNodeDummy(const aXPath: OWideString): PXMLNode;
    //select the first node (element or attribute) by name
    //  it is possible to use a normal path "element1/element2/element3"
    //  it is also possible to use an attribute path "element1/element2/@attribute"
    //  if not found the whole node path is created!
    function SelectNodeCreate(const aNodePath: OWideString): PXMLNode;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a aNodeList with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      var outNodeList: IXMLNodeList;
      const aMaxNodeCount: Integer = 0): Boolean; overload;
    //select all nodes by XPath, return maximum of aMaxNodeCount nodes
    //  if nothing found return a list with no items (count = 0)
    function SelectNodes(const aXPath: OWideString;
      const aMaxNodeCount: Integer = 0): IXMLNodeList; overload;
    //get child elements by tag name
    function GetElementsByTagName(const aName: OWideString; var outNodeList: IXMLNodeList): Boolean;
    function GetElementsByTagNameNS(const aNameSpaceURI, aLocalName: OWideString; var outNodeList: IXMLNodeList): Boolean;
  public
    //load document with custom reader
    //  outReaderToken -> the token enumerator
    //  aBreakWhenRefNodeReached -> break reading when reference node is reached (used in sequential parser)
    //return false if the XML document is invalid
    function LoadFromReader(const aReader: TXMLReader;
      var outReaderToken: PXMLReaderToken;
      const aBreakWhenRefNodeReached: PXMLNode = nil): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil): Boolean;
    //load document from file
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function LoadFromXML(const aXML: OWideString): Boolean;
    function LoadFromXML_UTF8(const aXML: OUTF8Container): Boolean;
    //load document from TBytes buffer
    // if aForceEncoding = nil: in encoding specified by the document
    // if aForceEncoding<>nil : enforce encoding (<?xml encoding=".."?> is ignored)
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean; overload;
    function LoadFromBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil): Boolean; overload;

    //save document with custom writer
    procedure SaveToWriter(const aWriter: TXMLWriter);
    //save document to file in encoding specified by the document
    procedure SaveToFile(const aFileName: OWideString);
    //save document to stream in encoding specified by the document
    procedure SaveToStream(const aStream: TStream);
    //returns XML as string (always in the system OWideString encoding and with system line breaks)
    procedure SaveToXML(var outXML: OWideString); overload;
    procedure SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType); overload;
    procedure SaveToXML_UTF8(var outXML: OUTF8Container); overload;
    procedure SaveToXML_UTF8(var outXML: OUTF8Container; const aIndentType: TXMLIndentType); overload;

    //returns XML as a buffer in encoding specified by the document
    procedure SaveToBuffer(var outBuffer: TBytes); overload;
    {$IFDEF O_HASBYTESTRINGS}
    procedure SaveToBuffer(var outBuffer: OUTF8Container); overload;
    {$ENDIF}
  public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    function XML_UTF8: OUTF8Container; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): OUTF8Container; overload;
  public
    property Id: XMLNodeId read GetId;
    property NodeType: TXMLNodeType read fNodeType;
    property NodeName: OWideString read GetNodeName write SetNodeName;
    property NodeValue: OWideString read GetNodeValue write SetNodeValue;
    property LocalName: OWideString read GetLocalName;
    property NameSpaceURI: OWideString read GetNameSpaceURI;
    property NameSpacePrefix: OWideString read GetNameSpacePrefix;
    property NodeNameId: OHashedStringsIndex read fNodeNameId write SetNodeNameId;
    property NodeValueId: OHashedStringsIndex read fNodeValueId write fNodeValueId;
    property Text: OWideString read GetText write SetText;
    property PreserveWhiteSpace: TXMLPreserveWhiteSpace read fPreserveWhiteSpace write fPreserveWhiteSpace;

    property ParentNode: PXMLNode read fParentNode;
    property OwnerDocument: TXMLDocument read fOwnerDocument;

    property FirstChild: PXMLNode index ctChild read GetFirstCChild;
    property LastChild: PXMLNode index ctChild read GetLastCChild;
    property FirstAttribute: PXMLNode index ctAttribute read GetFirstCChild;
    property LastAttribute: PXMLNode index ctAttribute read GetLastCChild;

    {$IFDEF BCB}
    //C++ Builder compatibility
    property ChildFromBegin[const aIndex: Integer]: PXMLNode read GetChildFromBegin;
    property ChildFromEnd[const aIndex: Integer]: PXMLNode read GetChildFromEnd;
    property AttributeFromBegin[const aIndex: Integer]: PXMLNode read GetAttributeFromBegin;
    property AttributeFromEnd[const aIndex: Integer]: PXMLNode read GetAttributeFromEnd;
    {$ELSE}
    property ChildFromBegin[const aIndex: Integer]: PXMLNode index ctChild read GetCChildFromBegin;
    property ChildFromEnd[const aIndex: Integer]: PXMLNode index ctChild read GetCChildFromEnd;
    property AttributeFromBegin[const aIndex: Integer]: PXMLNode index ctAttribute read GetCChildFromBegin;
    property AttributeFromEnd[const aIndex: Integer]: PXMLNode index ctAttribute read GetCChildFromEnd;
    {$ENDIF}

    property IsTextElement: Boolean read GetIsTextElement;

    property NextSibling: PXMLNode read fNextSibling;
    property PreviousSibling: PXMLNode read fPreviousSibling;
    //get next/previous node in DOM tree (without attributes)
    property NextNodeInTree: PXMLNode read GetNextNodeInTree;
    property PreviousNodeInTree: PXMLNode read GetPreviousNodeInTree;

    //Although you may use the Attributes[] list, it's faster when you use GetAttribute and SetAttribute directly!
    property Attributes[const aName: OWideString]: OWideString read GetAttribute write _SetAttribute;

    //absolute path of the node
    property AbsolutePath: OWideString read GetAbsolutePath;
    //node level in the tree, root element = 1
    property NodeLevel: Integer read GetNodeLevel;
  end;
  TXMLNodeArray = Array of TXMLNode;
  PXMLNodeArray = ^TXMLNodeArray;

  IXMLDocument = interface(ICustomXMLDocument)
    ['{490301A3-C95B-4E03-B09D-99E4682BC3FE}']

  //protected
    function GetDocumentNode: PXMLNode;
    function GetDocumentElement: PXMLNode;
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);

  //public
    //get id of a node
    function GetNodeId(const aNode: PXMLNode): XMLNodeId;
    //get node from id
    function GetNodeById(const aNodeId: XMLNodeId): PXMLNode;

  //public
    //attribute aName="aValue"
    function CreateAttribute(const aName: OWideString; const aValue: OWideString = ''): PXMLNode;
    function CreateAttributeNS(const aNameSpaceURI, aQualifiedName: OWideString; const aValue: OWideString = ''): PXMLNode;
    //element <aNodeName />
    function CreateElement(const aNodeName: OWideString): PXMLNode;
    function CreateElementNS(const aNameSpaceURI, aQualifiedName: OWideString): PXMLNode;
    //xml declaration <?xml ?>
    function CreateXMLDeclaration: PXMLNode;
    //text
    function CreateTextNode(const aText: OWideString): PXMLNode;
    //cdata <![CDATA[aText]]>
    function CreateCDATASection(const aData: OWideString): PXMLNode;
    //entity reference &aName;
    function CreateEntityReference(const aName: OWideString): PXMLNode;
    //comment <!--aText-->
    function CreateComment(const aText: OWideString): PXMLNode;
    //doctype <!DOCTYPE aDocTypeRawText>
    function CreateDocType(const aDocTypeRawText: OWideString): PXMLNode;
    //custom PI <?aTarget aContent?>
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    //create and append an element child to Self.Node (document node)
    function AddChild(const aElementName: OWideString): PXMLNode;

  //public

    //returns the very document node (parent of the DocumentElement)
    property Node: PXMLNode read GetDocumentNode;
    //returns the root node (first element in the document)
    property DocumentElement: PXMLNode read GetDocumentElement write SetDocumentElement;
  end;

  TXMLDocument = class(TInterfacedObject, IXMLDocument, ICustomXMLDocument)
  private
    fLoading: Boolean;
    fURL: OWideString;

    fDictionary: TOHashedStrings;

    {$IFDEF O_GENERICS}
    fNodes: TList<PXMLNodeArray>;//Memory blocks of 1024 elements. Do not reallocate its memory!
    fFreeNodes: TDictionary<PXMLNode,Boolean>;
    fTempChildNodes: Array[TXMLChildType] of TObjectDictionary<PXMLNode,TXMLChildNodeList>;
    fTempNameSpaceURIs: TDictionary<PXMLNode,OHashedStringsIndex>;
    {$ELSE}
    fNodes: TList;
    fFreeNodes: TODictionary;
    fTempChildNodes: Array[TXMLChildType] of TODictionary;
    fTempNameSpaceURIs: TODictionary;
    {$ENDIF}
    fTempAttributeIndex: TXMLAttributeIndex;

    fNextNodeId: XMLNodeId;//next NodeId to use
    fNodesLength: XMLNodeId;//= "Count(fNodes)*1024" - count of allocated nodes
    fBlankDocumentNode: PXMLNode;//the blank document element
    fDummyNode: PXMLNode;
    fDummyNodeList: IXMLNodeList;
    fWhiteSpaceHandling: TXMLWhiteSpaceHandling;
    fWriterSettings: TXMLDocumentWriterSettings;
    fReaderSettings: TXMLReaderSettings;

    fParseError: IOTextParseError;

    function FindXMLDeclarationNode(var outXMLDeclarationNode: PXMLNode): Boolean;
    function GetXMLDeclarationAttribute(const aAttributeName: OWideString): OWideString;
    procedure SetXMLDeclarationAttribute(const aAttributeName, aAttributeValue: OWideString);
    function GetCodePage: Word;
    procedure SetCodePage(const aCodePage: Word);
    function GetURL: OWideString;
    procedure SetURL(const aURL: OWideString);
    function GetVersion: OWideString;
    procedure SetVersion(const aVersion: OWideString);
    function GetEncoding: OWideString;
    procedure SetEncoding(const aEncoding: OWideString);
    function GetStandAlone: OWideString;
    procedure SetStandAlone(const aStandAlone: OWideString);
    function GetWhiteSpaceHandling: TXMLWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const aWhiteSpaceHandling: TXMLWhiteSpaceHandling);
    function GetLoading: Boolean;
    procedure SetLoading(const aLoading: Boolean);
    function GetDummyNode: PXMLNode;
    function GetDummyResNodeList: IXMLNodeList;
    function GetDocumentNode: PXMLNode;//absolute root element (= empty document)
    function GetDocumentElement: PXMLNode;//first element in document (=root)
    procedure SetDocumentElement(const aDocumentElement: PXMLNode);
    function GetWriterSettings: TXMLDocumentWriterSettings;
    function GetReaderSettings: TXMLReaderSettings;
    function GetParseError: IOTextParseError;
    function GetAbsoluteNodeCount: XMLNodeId;
  protected
    procedure FreeNode(const aNode: PXMLNode); virtual;
    procedure ClearNodes(const aFullClear: Boolean); virtual;

    function CreateNode(const aNodeType: TXMLNodeType;
      const aNodeName, aNodeValue: OWideString): PXMLNode; overload;
    function CreateNode(const aNodeType: TXMLNodeType;
      const aNodeNameId, aNodeValueId: OHashedStringsIndex): PXMLNode; overload;
    function SetString(const aString: OWideString): OHashedStringsIndex;

    procedure Grow;

    function GetCreateTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType): TXMLChildNodeList;
    function TryGetTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType; var outList: TXMLChildNodeList): Boolean;
    procedure DestroyTempChildNodeList(const aParentNode: PXMLNode;
      const aChildType: TXMLChildType);
    procedure ClearTempChildNodeLists(const aChildType: TXMLChildType);

    property Loading: Boolean read GetLoading write SetLoading;
    property DummyNode: PXMLNode read GetDummyNode;
    property DummyResNodeList: IXMLNodeList read GetDummyResNodeList;

    procedure DoCreate; virtual;
  public
    function IndexOfString(const aString: OWideString): OHashedStringsIndex;
    function GetString(const aStringId: OHashedStringsIndex): OWideString;
  public
    function CreateAttribute(const aName, aValue: OWideString): PXMLNode;
    function CreateAttributeNS(const aNameSpaceURI, aQualifiedName: OWideString; const aValue: OWideString = ''): PXMLNode;
    function CreateElement(const aNodeName: OWideString): PXMLNode;
    function CreateElementNS(const aNameSpaceURI, aQualifiedName: OWideString): PXMLNode;
    function CreateXMLDeclaration: PXMLNode;
    function CreateTextNode(const aText: OWideString): PXMLNode;
    function CreateCDATASection(const aData: OWideString): PXMLNode;
    function CreateEntityReference(const aName: OWideString): PXMLNode;
    function CreateComment(const aText: OWideString): PXMLNode;
    function CreateDocType(const aDocTypeRawText: OWideString): PXMLNode;
    function CreateProcessingInstruction(const aTarget, aContent: OWideString): PXMLNode;

    function AddChild(const aElementName: OWideString): PXMLNode;
  public
    function GetNodeId(const aNode: PXMLNode): XMLNodeId;
    function GetNodeById(const aNodeId: XMLNodeId): PXMLNode;
  public
    constructor Create({%H-}aDummy: TObject); overload;//aDummy to ge ignored - Delphi XML compatibility
    constructor Create(const aRootNodeName: OWideString = ''; const aAddXMLDeclaration: Boolean = False); overload;
    destructor Destroy; override;
  public
    procedure Clear(const aFullClear: Boolean = True);

  public
    function LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
    function LoadFromFile(const aFileName: OWideString; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromStream(const aStream: TStream; const aForceEncoding: TEncoding = nil): Boolean;
    function LoadFromXML(const aXML: OWideString): Boolean;
    function LoadFromXML_UTF8(const aXML: OUTF8Container): Boolean;
    function LoadFromBuffer(const aBuffer: TBytes; const aForceEncoding: TEncoding = nil): Boolean; overload;
    function LoadFromBuffer(const aBuffer; const aBufferLength: Integer; const aForceEncoding: TEncoding = nil): Boolean; overload;

    procedure SaveToWriter(const aWriter: TXMLWriter);
    procedure SaveToFile(const aFileName: OWideString);
    procedure SaveToStream(const aStream: TStream);

    procedure SaveToBuffer(var outBuffer: TBytes); overload;
    {$IFDEF O_HASBYTESTRINGS}
    procedure SaveToBuffer(var outBuffer: OUTF8Container); overload;
    {$ENDIF}
    procedure SaveToXML(var outXML: OWideString); overload;
    procedure SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType); overload;
    procedure SaveToXML_UTF8(var outXML: OUTF8Container); overload;
    procedure SaveToXML_UTF8(var outXML: OUTF8Container; const aIndentType: TXMLIndentType); overload;
  public
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    function XML_UTF8: OUTF8Container; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): OUTF8Container; overload;
  public
    property URL: OWideString read GetURL write SetURL;

    property Node: PXMLNode read fBlankDocumentNode;//GetDocumentNode; performance
    property DocumentElement: PXMLNode read GetDocumentElement write SetDocumentElement;
    property AbsoluteNodeCount: XMLNodeId read GetAbsoluteNodeCount;

    property WhiteSpaceHandling: TXMLWhiteSpaceHandling read fWhiteSpaceHandling write fWhiteSpaceHandling;//Get/Set; performance

    property CodePage: Word read GetCodePage write SetCodePage;
    property Encoding: OWideString read GetEncoding write SetEncoding;
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    property Version: OWideString read GetVersion write SetVersion;

    property WriterSettings: TXMLDocumentWriterSettings read fWriterSettings;
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;

    property ParseError: IOTextParseError read GetParseError;
  end;

  TXMLResNodeListEnumerator = class;
  IXMLNodeList = interface
    ['{9FD530D4-B35E-467E-916A-07B5E3D83AC6}']

    //protected
    function GetCount: Integer;

    //public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var outNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    procedure Exchange(const aIndex1, aIndex2: Integer); overload;
    procedure Exchange(const aNode1, aNode2: PXMLNode); overload;
    procedure Move(const aCurIndex, aNewIndex: Integer);

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNode: PXMLNode): Boolean;
    function GetPrevious(var ioNode: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLResNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLChildNodeListEnumerator = class(TObject)
  private
    fList: TXMLChildNodeList;
    fCurrent: PXMLNode;
  public
    constructor Create(aList: TXMLChildNodeList);
    function GetCurrent: PXMLNode;
    function MoveNext: Boolean;
  public
    property Current: PXMLNode read GetCurrent;
  end;

  TXMLChildNodeList = class(TObject)
  private
    fParent: PXMLNode;
    fChildType: TXMLChildType;

    fLastGetNodeIndex: Integer;
    fLastGetNode: PXMLNode;
    fTempCount: Integer;
  protected
    function GetCount: Integer;

    procedure ClearTempVariables;

    procedure ExtNodeAppended;
    procedure ExtNodeInserted;
    procedure ExtNodeRemoved;
  public
    constructor Create(const aParent: PXMLNode; const aChildType: TXMLChildType);
  public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;//important: the node gets automatically destroyed in all delete procedures!
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode; overload;
    function FindNode(const aName: OWideString; var outNode: PXMLNode): Boolean; overload;

    function IndexOf(const aNode: PXMLNode): Integer;
    function Remove(const aNode: PXMLNode): Integer; overload;//important: node gets automatically destroyed
    function Remove(const aName: OWideString; var outNode: PXMLNode): Boolean; overload;//important: node DOES NOT GET automatically destroyed

    procedure ExchangeNodes(const aIndex1, aIndex2: Integer); overload;
    procedure ExchangeNodes(const aNode1, aNode2: PXMLNode); overload;

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNode: PXMLNode): Boolean;
    function GetPrevious(var ioNode: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLChildNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLResNodeListEnumerator = class(TObject)
  private
    fList: IXMLNodeList;
    fIndex: Integer;
  public
    constructor Create(aList: IXMLNodeList);
    function GetCurrent: PXMLNode;
    function MoveNext: Boolean;
  public
    property Current: PXMLNode read GetCurrent;
  end;

  TXMLResNodeList = class(TInterfacedObject, IXMLNodeList)
  private
    fList: TXMLNodeList;
    fIteratorCurrent: Integer;//for fast Next & Prev

    function GetPrevNext(var ioNodeEnum: PXMLNode; const aInc: Integer): Boolean;
  protected
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(const aNode: PXMLNode): Integer;
    function AddNode(const aNode: PXMLNode): PXMLNode;
    procedure Clear;
    procedure Delete(const aNode: PXMLNode); overload;
    procedure Delete(const aName: OWideString); overload;
    procedure Delete(const aIndex: Integer); overload;
    function Get(const aIndex: Integer): PXMLNode;

    function FindNode(const aName: OWideString): PXMLNode;

    function IndexOf(const aNode: PXMLNode): Integer; overload;
    function IndexOf(const aName: OWideString): Integer; overload;
    function IndexOf(const aName: OWideString; var outNode: PXMLNode): Integer; overload;
    procedure Insert(const aIndex: Integer; const aNode: PXMLNode);
    function Remove(const aNode: PXMLNode): Integer;

    procedure Exchange(const aIndex1, aIndex2: Integer); overload;
    procedure Exchange(const aNode1, aNode2: PXMLNode); overload;
    procedure Move(const aCurIndex, aNewIndex: Integer);

    function GetFirst: PXMLNode;
    function GetLast: PXMLNode;
    function GetNext(var ioNodeEnum: PXMLNode): Boolean;
    function GetPrevious(var ioNodeEnum: PXMLNode): Boolean;

    {$IFDEF O_ENUMERATORS}
    function GetEnumerator: TXMLResNodeListEnumerator;
    {$ENDIF}

    property Nodes[const aIndex: Integer]: PXMLNode read Get; default;
    property Count: Integer read GetCount;
  end;

  TXMLXPathDOMAdapter = class(TXMLXPathAdapter)
  private
    fResNodeList: IXMLNodeList;
    fOwnerDocument: TXMLDocument;
  public
    constructor Create(const aOwnerDocument: TXMLDocument);
  public
    procedure BuildIdTree(const aStartWithNode: TXMLXPathNode; const aLevelsDeep: Integer;
      const aAttributes: Boolean; const aIdTree: TXMLXPathIdTree); override;
    function CreateResNodeList: TXMLXPathNodeList; override;
    procedure AddNodeToResList(const aNode: TXMLXPathNode); override;
    function GetNodeNameId(const aNode: TXMLXPathNode): OHashedStringsIndex; override;
    function GetNodeValueId(const aNode: TXMLXPathNode): OHashedStringsIndex; override;
    function GetStringId(const aString: OWideString): OHashedStringsIndex; override;
    function GetNodeType(const aNode: TXMLXPathNode): TXMLNodeType; override;
    procedure GetNodeInfo(const aNode: TXMLXPathNode; var outNodeInfo: TXMLXPathNodeInfo); override;
    function NodeHasAttributes(const aNode: TXMLXPathNode): Boolean; override;
    function NodeFindAttribute(const aNode: TXMLXPathNode; const aAttrNameId: OHashedStringsIndex): TXMLXPathNode; override;
    procedure GetNodeAttributes(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
    function GetNodeParent(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function GetNodeDOMDocument(const aNode: TXMLXPathNode): TXMLXPathNode; override;
    function NodeHasChildNodes(const aNode: TXMLXPathNode): Boolean; override;
    procedure GetNodeChildren(const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList); override;
  end;

  TXMLAttributeIndex = class(TObject)
  private
    fOwnerDocument: TXMLDocument;

    fParentElement: PXMLNode;
    fIndex: TXMLNodeIndex;
  public
    constructor Create(const aOwnerDocument: TXMLDocument);
    destructor Destroy; override;
  public
    procedure ExtAttributeAdded(const aAttributeNode: PXMLNode);
    procedure ExtAttributeInserted;
    procedure ExtAttributeRemoved;

    procedure SetParentElement(const aParentElement: PXMLNode);
    function FindAttribute(const aNameId: OHashedStringsIndex; var outAttr: PXMLNode): Boolean;
    procedure Clear;
  public
    property ParentElement: PXMLNode read fParentElement;
  end;

function CreateXMLDoc: IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument; overload;
function CreateXMLDoc(const aRootNodeName: OWideString; const aAddXMLDeclaration: Boolean): IXMLDocument; overload;

function CompareNodeNames(const aNode1, aNode2: PXMLNode): Integer;

implementation

uses OXmlLng;

function CompareNodeNames(const aNode1, aNode2: PXMLNode): Integer;
begin
  Result := OWideCompareStr(aNode1^.NodeName, aNode2^.NodeName);
end;

function CreateXMLDoc: IXMLDocument;
begin
  Result := TXMLDocument.Create;
end;

function CreateXMLDoc(const aRootNodeName: OWideString): IXMLDocument;
begin
  Result := TXMLDocument.Create(aRootNodeName);
end;

function CreateXMLDoc(const aRootNodeName: OWideString; const aAddXMLDeclaration: Boolean): IXMLDocument;
begin
  Result := TXMLDocument.Create(aRootNodeName, aAddXMLDeclaration);
end;

{ TXMLNode }

function TXMLNode.AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
begin
  Result := _AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.AddCDATASection(const aText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntCData, '', aText);
end;

function TXMLNode.AddComment(const aText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntComment, '', aText);
end;

function TXMLNode.AddChild(const aElementName: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntElement, aElementName, '');
end;

function TXMLNode.AddCustomChild(const aType: TXMLNodeType; const aName,
  aValue: OWideString): PXMLNode;
begin
  Result := fOwnerDocument.CreateNode(aType, aName, aValue);
  Append(Result, ctChild);
end;

function TXMLNode.AddXMLDeclaration: PXMLNode;
begin
  Result := AddCustomChild(ntXMLDeclaration, XML_XML, '');
end;

function TXMLNode.AddDocType(const aDocTypeRawText: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntDocType, '', aDocTypeRawText);
end;

function TXMLNode.AddEntityReference(const aEntityName: OWideString): PXMLNode;
begin
  Result := fOwnerDocument.CreateEntityReference(aEntityName);
  Append(Result, ctChild);
end;

function TXMLNode.AddProcessingInstruction(const aTarget,
  aContent: OWideString): PXMLNode;
begin
  Result := AddCustomChild(ntProcessingInstruction, aTarget, aContent);
end;

function TXMLNode.AddText(const aText: OWideString): PXMLNode;
var
  xText: OWideString;
begin
  if OwnerDocument.Loading then
  begin
    //document is reading XML
    if (OwnerDocument.WhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(aText)
    then begin
      xText := '';
    end else if
      (OwnerDocument.WhiteSpaceHandling = wsTrim) or
      ((OwnerDocument.WhiteSpaceHandling = wsAutoTag) and not GetDoPreserveWhiteSpace)
    then begin
      xText := Trim(aText);
    end else
    begin
      xText := aText;
    end;
  end else
  begin
    //programatically creating document
    if (OwnerDocument.WhiteSpaceHandling = wsPreserveInTextOnly) and OXmlIsWhiteSpace(aText) then
    begin
      xText := '';
    end else if (OwnerDocument.WhiteSpaceHandling = wsTrim) then
    begin
      xText := Trim(aText);
    end else
    begin
      xText := aText;

      if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and
        OXmlNeedsPreserveAttribute(xText) and
        (not Self.GetDoPreserveWhiteSpace)
      then
        Self.fPreserveWhiteSpace := pwPreserve;
    end;
  end;

  if xText <> '' then
    Result := AddCustomChild(ntText, '', xText)
  else
    Result := nil;
end;

procedure TXMLNode.Append(const aNew: PXMLNode; const aChildType: TXMLChildType);
var
  xLastChild: PXMLNode;
  xList: TXMLChildNodeList;
  xNSParent: PXMLNode;
  xNSURI, xNSPrefix, xLocalName: OWideString;
begin
  if TryGetChildNodes(xList{%H-}, aChildType) then
    xList.ExtNodeAppended;
  if (aChildType = ctAttribute) and (fOwnerDocument.fTempAttributeIndex.ParentElement = @Self) then
    fOwnerDocument.fTempAttributeIndex.ExtAttributeAdded(aNew);

  if Assigned(fFirstCChild[aChildType]) then
  begin
    //append to the end

    xLastChild := fLastCChild[aChildType];
    //set new as next sibling of last child
    xLastChild.fNextSibling := aNew;
    //set last id to new
    fLastCChild[aChildType] := aNew;
    //set prev sibling of new child to last
    aNew.fPreviousSibling := xLastChild;
  end else
  begin
    //no children

    fFirstCChild[aChildType] := aNew;
    fLastCChild[aChildType] := aNew;

    aNew.fPreviousSibling := nil;
  end;
  aNew.fParentNode := @Self;
  aNew.fNextSibling := nil;

  if (fOwnerDocument.fTempNameSpaceURIs.Count > 0) and
     aNew.FindTempNameSpaceURI(aNew.NameSpacePrefix, xNSURI{%H-}, False)
  then
  begin
    //namespace found
    fOwnerDocument.fTempNameSpaceURIs.Remove(aNew);

    case aNew.NodeType of
      ntElement: xNSParent := aNew;
      ntAttribute: xNSParent := aNew.ParentNode;
    else
      Exit;
    end;

    OXmlResolveNameSpace(aNew.NodeName, xNSPrefix{%H-}, xLocalName{%H-});
    if not xNSParent.NameSpaceExists(xNSURI, xNSPrefix) then
    begin
      //namespace not found
      xNSParent.SetAttribute(OXmlApplyNameSpace(XML_XMLNS, xNSPrefix), xNSURI);
    end;
  end;
end;

function TXMLNode.AppendChild(const aNewChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  Append(aNewChild, ctChild);
  Result := aNewChild;
end;

procedure TXMLNode.AssignProperties(const aFromNode: PXMLNode);
begin
  Self.fPreserveWhiteSpace := aFromNode.fPreserveWhiteSpace;
end;

function TXMLNode.BuildChildrenIndex: TXMLNodeIndex;
var
  xChildNode: PXMLNode;
begin
  Result := TXMLNodeIndex.Create;

  xChildNode := nil;
  while GetNextChild(xChildNode) do
    Result.Add(xChildNode.NodeNameId, xChildNode);
end;

function TXMLNode.CloneNode(const aDeep: Boolean): PXMLNode;
begin
  Result := CloneNode(aDeep, fOwnerDocument);
end;

function TXMLNode.CloneNode(const aDeep: Boolean; const aToDocument: TXMLDocument): PXMLNode;
var
  xIter, xNewNode: PXMLNode;
begin
  if (aToDocument = nil) or (aToDocument = fOwnerDocument) then
    Result := fOwnerDocument.CreateNode(fNodeType, fNodeNameId, fNodeValueId)
  else
    Result := aToDocument.CreateNode(fNodeType, NodeName, NodeValue);
  Result.AssignProperties(@Self);

  xIter := Self.FirstAttribute;
  while Assigned(xIter) do
  begin
    if (aToDocument = nil) or (aToDocument = fOwnerDocument) then
      xNewNode := fOwnerDocument.CreateNode(xIter.fNodeType, xIter.fNodeNameId, xIter.fNodeValueId)
    else
      xNewNode := aToDocument.CreateNode(xIter.fNodeType, xIter.NodeName, xIter.NodeValue);
    xNewNode.AssignProperties(xIter);
    Result.Append(xNewNode, ctAttribute);
    xIter := xIter.NextSibling;
  end;

  if aDeep then
  begin
    xIter := Self.FirstChild;
    while Assigned(xIter) do
    begin
      xNewNode := xIter.CloneNode(aDeep, aToDocument);
      Result.Append(xNewNode, ctChild);
      xIter := xIter.NextSibling;
    end;
  end;
end;

function TXMLNode.CloneNode(const aDeep: Boolean; const aToDocument: IXMLDocument): PXMLNode;
var
  xToDocument: TXMLDocument;
begin
  if Assigned(aToDocument) then
    xToDocument := aToDocument.Node.OwnerDocument//a trick to convert IXMLDocument to TXMLDocument
  else
    xToDocument := fOwnerDocument;

  Result := CloneNode(aDeep, xToDocument);
end;

procedure TXMLNode.DeleteAttribute(const aAttr: PXMLNode);
begin
  aAttr.DeleteSelf;
end;

procedure TXMLNode.DeleteAttributeNS(const aNameSpaceURI,
  aLocalName: OWideString);
var
  xAttr: PXMLNode;
begin
  if not FindAttributeNS(aNameSpaceURI, aLocalName, xAttr{%H-}) then
    Exit;

  DeleteAttribute(xAttr);
end;

procedure TXMLNode.DeleteAttributes(const aDestroyList: Boolean);
begin
  DeleteCChildren(aDestroyList, ctAttribute);
end;

procedure TXMLNode.DeleteChildren;
begin
  DeleteCChildren(aDestroyList, ctChild);
end;

procedure TXMLNode.DeleteSelf;
begin
  RemoveSelfFromParent;

  OwnerDocument.FreeNode(@Self);
end;

procedure TXMLNode.ExchangeWithNode(const aNode: PXMLNode);
var
  xSelfNextSibling, xSelfNextSiblingParent: PXMLNode;
begin
  if aNode.OwnerDocument <> Self.OwnerDocument then
    raise EXmlDOMException.Create(OXmlLng_ExchangeFromDifferentDocument);

  xSelfNextSibling := Self.NextSibling;
  xSelfNextSiblingParent := xSelfNextSibling.ParentNode;

  if xSelfNextSibling <> aNode then
  begin
    Self.RemoveSelfFromParent;
    aNode.ParentNode.InsertBefore(@Self, aNode);
  end else
    xSelfNextSibling := @Self;//insert before self if [Self, aNode]

  if aNode.NextSibling <> @Self then
  begin
    aNode.RemoveSelfFromParent;
    xSelfNextSiblingParent.InsertBefore(aNode, xSelfNextSibling);
  end;
end;

procedure TXMLNode.DeleteAttribute(const aName: OWideString);
var
  xAttr: PXMLNode;
begin
  if not FindAttribute(aName, xAttr{%H-}) then
    Exit;

  DeleteAttribute(xAttr);
end;

procedure TXMLNode.DeleteCChildren(const aDestroyList: Boolean;
  const aChildType: TXMLChildType);
var
  xChild, xNextChild: PXMLNode;
begin
  xChild := fFirstCChild[aChildType];
  while Assigned(xChild) do
  begin
    xNextChild := xChild.fNextSibling;
    OwnerDocument.FreeNode(xChild);
    xChild := xNextChild;
  end;

  if aDestroyList then
    OwnerDocument.DestroyTempChildNodeList(@Self, aChildType);

  fFirstCChild[aChildType] := nil;
  fLastCChild[aChildType] := nil;
end;

procedure TXMLNode.DeleteChild(const aChild: PXMLNode);
begin
  aChild.DeleteSelf;
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var outAttr: PXMLNode): Boolean;
begin
  Result := FindAttributeById(fOwnerDocument.IndexOfString(aName), outAttr);
end;

procedure TXMLNode.FillCChildList(const aList: IXMLNodeList;
  const aChildType: TXMLChildType);
var
  xChild: PXMLNode;
begin
  xChild := fFirstCChild[aChildType];
  while Assigned(xChild) do
  begin
    aList.Add(xChild);

    xChild := xChild.NextSibling;
  end;
end;

function TXMLNode.FindAttribute(const aName: OWideString;
  var outValue: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  Result := FindAttribute(aName, xAttr{%H-});
  if Result then
    outValue := xAttr.NodeValue
  else
    outValue := '';
end;

function TXMLNode.FindAttributeById(const aNameId: OHashedStringsIndex;
  var outAttr: PXMLNode): Boolean;
var
  xAttrCount: Integer;
begin
  if not HasAttributes or (aNameId < 0) then
  begin
    Result := False;
    outAttr := nil;
    Exit;
  end;

  if fOwnerDocument.fTempAttributeIndex.ParentElement = @Self then
  begin
    //attribute index used!
    Result := fOwnerDocument.fTempAttributeIndex.FindAttribute(aNameId, outAttr);
  end
  else
  begin
    //attribute index not used!
    Result := FindCChild(aNameId, ctAttribute, outAttr, xAttrCount{%H-});
    //if we looked through more then attribute limit (the whole attribute count is not necesarily xAttrCount), use index!
    if xAttrCount > XMLUseIndexNodeLimit then
      fOwnerDocument.fTempAttributeIndex.SetParentElement(@Self);
  end;
end;

function TXMLNode.FindAttributeByValue(const aValue: OWideString;
  var outAttr: PXMLNode): Boolean;
begin
  Result := FindAttributeByValueId(fOwnerDocument.IndexOfString(aValue), outAttr);
end;

function TXMLNode.FindAttributeByValue(const aValue: OWideString;
  var outName: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  Result := FindAttributeByValue(aValue, xAttr{%H-});
  if Result then
    outName := xAttr.NodeName
  else
    outName := '';
end;

function TXMLNode.FindAttributeByValueId(const aValueId: OHashedStringsIndex;
  var outAttr: PXMLNode): Boolean;
begin
  Result := False;
  outAttr := nil;

  if not HasAttributes or (aValueId < 0) then
    Exit;

  outAttr := FirstAttribute;
  while Assigned(outAttr) do
  begin
    if outAttr.NodeValueId = aValueId then//attribute value found, exit -> outAttr has correct value
    begin
      Result := True;
      Exit;
    end;

    outAttr := outAttr.NextSibling;
  end;
  //attribute value not found, -> outAttr has correct value (=nil), Result = False
end;

function TXMLNode.FindAttributeNS(const aNameSpaceURI, aLocalName: OWideString;
  var outAttr: PXMLNode): Boolean;
var
  xQualifiedNameIds: TXMLIntArray;
  I: Integer;
begin
  SetLength(xQualifiedNameIds, 0);
  FindQualifiedNames(fOwnerDocument.IndexOfString(aNameSpaceURI), aLocalName, xQualifiedNameIds);

  for I := Low(xQualifiedNameIds) to High(xQualifiedNameIds) do
  if FindAttributeById(xQualifiedNameIds[I], outAttr) then
  begin
    Result := True;
    Exit;
  end;

  Result := False;
  outAttr := nil;
end;

function TXMLNode.FindAttributeNS(const aNameSpaceURI, aLocalName: OWideString;
  var outValue: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  Result := FindAttributeNS(aNameSpaceURI, aLocalName, xAttr{%H-});
  if Result then
    outValue := xAttr.NodeValue
  else
    outValue := '';
end;

function TXMLNode.FindCChild(const aNodeNameId: OHashedStringsIndex;
  const aChildType: TXMLChildType; var outNode: PXMLNode;
  var outNodeSearchedCount: Integer): Boolean;
begin
  outNodeSearchedCount := 0;

  if aNodeNameId >= 0 then
  begin
    outNode := fFirstCChild[aChildType];
    while Assigned(outNode) do
    begin
      if outNode.fNodeNameId = aNodeNameId then
      begin
        Result := True;
        Exit;
      end;
      outNode := outNode.fNextSibling;
      Inc(outNodeSearchedCount);
    end;
  end;
  outNode := nil;//must be here
  Result := False;
end;

function TXMLNode.FindChild(const aName: OWideString;
  var outNode: PXMLNode): Boolean;
var
  x: Integer;
begin
  Result := FindCChild(OwnerDocument.IndexOfString(aName), ctChild, outNode, x{%H-});
end;

function TXMLNode.FindChildById(const aNameId: OHashedStringsIndex;
  var outNode: PXMLNode): Boolean;
var
  x: Integer;
begin
  Result := FindCChild(aNameId, ctChild, outNode, x{%H-});
end;

function TXMLNode.FindChildByIdWithIndex(const aNameId: OHashedStringsIndex;
  var outNode: PXMLNode; var ioIndex: TXMLNodeIndex): Boolean;
var
  xChildCount: Integer;
begin
  if not HasChildNodes or (aNameId < 0) then
  begin
    Result := False;
    outNode := nil;
    Exit;
  end;

  if Assigned(ioIndex) then
  begin
    //node index used!
    Result := ioIndex.TryGetValue(aNameId, {$IFNDEF O_GENERICS}Pointer{$ELSE}PXMLNode{$ENDIF}(outNode));
  end else
  begin
    //node index not used!
    Result := FindCChild(aNameId, ctChild, outNode, xChildCount{%H-});
    //if we looked through more then child limit (the whole child count is not necesarily xAttrCount), use index!
    if xChildCount > XMLUseIndexNodeLimit then
      ioIndex := BuildChildrenIndex;
  end;
end;

function TXMLNode.FindChildWithIndex(const aName: OWideString;
  var outNode: PXMLNode; var ioIndex: TXMLNodeIndex): Boolean;
var
  xNameId: Integer;
begin
  xNameId := OwnerDocument.IndexOfString(aName);

  Result := FindChildByIdWithIndex(xNameId, outNode, ioIndex);
end;

function TXMLNode.FindNameSpace(const aNameSpaceURIId: OHashedStringsIndex;
  const aNameSpacePrefix: OWideString): Boolean;
var
  xAttr: PXMLNode;
  xNodeName, xLocalName: OWideString;
begin
  if NodeType = ntElement then
  begin
    xAttr := FirstAttribute;
    while Assigned(xAttr) do
    begin
      if (xAttr.fNodeValueId = aNameSpaceURIId) then
      begin
        xNodeName := xAttr.NodeName;
        if (xNodeName = XML_XMLNS) and
           (aNameSpacePrefix = '')
        then
        begin
          Result := True;
          Exit;
        end else
        begin
          if OXmlCheckNameSpace(xNodeName, XML_XMLNS{%H-}, xLocalName{%H-}) and
             (aNameSpacePrefix = xLocalName)
          then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
      xAttr := xAttr.NextSibling;
    end;//while
  end;

  if Assigned(ParentNode) then
    Result := ParentNode.FindNameSpace(aNameSpaceURIId, aNameSpacePrefix)
  else
    Result := False;
end;

procedure TXMLNode.FindNameSpacePrefixes(
  const aNameSpaceURIId: OHashedStringsIndex;
  const aNameSpacePrefixes: TOWideStringList);
var
  xAttr: PXMLNode;
  xNodeName, xLocalName: OWideString;
begin
  if NodeType = ntElement then
  begin
    xAttr := FirstAttribute;
    while Assigned(xAttr) do
    begin
      if (xAttr.fNodeValueId = aNameSpaceURIId) then
      begin
        xNodeName := xAttr.NodeName;
        if (xNodeName = XML_XMLNS) then
          aNameSpacePrefixes.Add('')
        else
        if OXmlCheckNameSpace(xNodeName, XML_XMLNS, {%H-}xLocalName{%H-}) then
          aNameSpacePrefixes.Add(xLocalName);
      end;
      xAttr := xAttr.NextSibling;
    end;//while
  end;

  if Assigned(ParentNode) then
    ParentNode.FindNameSpacePrefixes(aNameSpaceURIId, aNameSpacePrefixes)
end;

function TXMLNode.FindNameSpacePrefixesByURI(const aNameSpaceURI: OWideString;
  const aNameSpacePrefixes: TOWideStringList): Boolean;
var
  xIdURI: OHashedStringsIndex;
begin
  xIdURI := fOwnerDocument.IndexOfString(aNameSpaceURI);
  aNameSpacePrefixes.Clear;
  aNameSpacePrefixes.Duplicates := dupIgnore;
  aNameSpacePrefixes.Sorted := True;
  if xIdURI >= 0 then
    FindNameSpacePrefixes(xIdURI, aNameSpacePrefixes);

  Result := aNameSpacePrefixes.Count > 0;
end;

function TXMLNode.FindNameSpaceURI(const aAttrNameId: OHashedStringsIndex;
  var outNameSpaceURI: OWideString): Boolean;
var
  xAttr: PXMLNode;
begin
  if NodeType = ntElement then
  begin
    if FindAttributeById(aAttrNameId, xAttr{%H-}) then
    begin
      outNameSpaceURI := xAttr.NodeValue;
      Result := True;
      Exit;
    end;
  end;

  if Assigned(ParentNode) then
    Result := ParentNode.FindNameSpaceURI(aAttrNameId, outNameSpaceURI)
  else
    Result := False;
end;

function TXMLNode.FindNameSpaceURIByPrefix(const aNameSpacePrefix: OWideString;
  var outNameSpaceURI: OWideString): Boolean;
var
  xAttrNameId: OHashedStringsIndex;
begin
  xAttrNameId := fOwnerDocument.IndexOfString(OXmlApplyNameSpace(XML_XMLNS, aNameSpacePrefix));
  Result := (xAttrNameId >= 0) and FindNameSpaceURI(xAttrNameId, outNameSpaceURI);
end;

procedure TXMLNode.FindQualifiedNames(const aNameSpaceURIId: OHashedStringsIndex;
  const aLocalName: OWideString;
  var ioQualifiedNameIds: TXMLIntArray);

  procedure _Add(const bQualifiedName: OWideString);
  var
    xQualifiedNameId: OHashedStringsIndex;
    I: Integer;
  begin
    xQualifiedNameId := fOwnerDocument.IndexOfString(bQualifiedName);
    if xQualifiedNameId >= 0 then
    begin
      //we don't expect too many prefixes connected with aNameSpaceURIId (basically only one),
      //  so using an array is better and faster than TODictionary or some other sorted/unique list/dictionary
      //  (the overload connected with creating an indexed list is higher)

      for I := Low(ioQualifiedNameIds) to High(ioQualifiedNameIds) do//be sure ids are unique
      if ioQualifiedNameIds[I] = xQualifiedNameId then
        Exit;

      SetLength(ioQualifiedNameIds, Length(ioQualifiedNameIds)+1);
      ioQualifiedNameIds[High(ioQualifiedNameIds)] := xQualifiedNameId;
    end;
  end;
var
  xAttr: PXMLNode;
  xNodeName, xNSPrefix: OWideString;
begin
  if aNameSpaceURIId < 0 then
    Exit;

  if NodeType = ntElement then
  begin
    xAttr := FirstAttribute;
    while Assigned(xAttr) do
    begin
      if (xAttr.fNodeValueId = aNameSpaceURIId) then
      begin
        xNodeName := xAttr.NodeName;
        if (xNodeName = XML_XMLNS) then
          _Add(aLocalName)
        else
        if OXmlCheckNameSpace(xNodeName, XML_XMLNS, xNSPrefix{%H-}) then
          _Add(OXmlApplyNameSpace(xNSPrefix, aLocalName));
      end;
      xAttr := xAttr.NextSibling;
    end;//while
  end;

  if Assigned(ParentNode) then
    ParentNode.FindQualifiedNames(aNameSpaceURIId, aLocalName, ioQualifiedNameIds);
end;

function TXMLNode.FindTempNameSpaceURI(const aNameSpacePrefix: OWideString;
  var outNameSpace: OWideString; const aSearchInParent: Boolean): Boolean;
var
  xNameSpaceId: OHashedStringsIndex;
begin
  if (fOwnerDocument.fTempNameSpaceURIs.Count = 0) then
  begin
    Result := False;
    Exit;
  end;

  Result :=
    (fOwnerDocument.fTempNameSpaceURIs.TryGetValue(@Self, xNameSpaceId{%H-})) and
    (xNameSpaceId >= 0) and
    (Self.NameSpacePrefix = aNameSpacePrefix);

  if Result then
  begin
    outNameSpace := fOwnerDocument.GetString(xNameSpaceId);
    Result := (outNameSpace <> '');
  end else
  if aSearchInParent and Assigned(fParentNode) then
    Result := fParentNode.FindTempNameSpaceURI(aNameSpacePrefix, outNameSpace, aSearchInParent);
end;

function TXMLNode.GetAbsolutePath: OWideString;
var
  xNode, xDocumentNode: PXMLNode;
begin
  xNode := @Self;
  xDocumentNode := OwnerDocument.Node;

  Result := '';
  while Assigned(xNode) and (xNode <> xDocumentNode) do
  begin
    Result := '/' + xNode.NodeName + Result;
    xNode := xNode.ParentNode;
  end;
end;

function TXMLNode.GetAttribute(const aName: OWideString): OWideString;
begin
  Result := GetAttributeDef(aName, '');
end;

function TXMLNode.GetAttributeCount: Integer;
var
  xList: TXMLChildNodeList;
  xAttr: PXMLNode;
begin
  if TryGetChildNodes(xList{%H-}, ctAttribute) then
  begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xAttr := fFirstCChild[ctAttribute];
  while Assigned(xAttr) do
  begin
    Inc(Result);
    xAttr := xAttr.NextSibling;
  end;
end;

function TXMLNode.GetAttributeDef(const aName,
  aDefaultValue: OWideString): OWideString;
begin
  if not FindAttribute(aName, Result{%H-}) then
    Result := aDefaultValue;
end;

{$IFDEF BCB}
function TXMLNode.GetAttributeFromBegin(const aIndex: Integer): PXMLNode;
begin
  Result := GetCChildFromBegin(aIndex, ctAttribute);
end;

function TXMLNode.GetAttributeFromEnd(const aIndex: Integer): PXMLNode;
begin
  Result := GetCChildFromEnd(aIndex, ctAttribute);
end;
{$ENDIF}

function TXMLNode.GetAttributeNode(const aAttrName: OWideString): PXMLNode;
begin
  if not FindAttribute(aAttrName, Result{%H-}) then
    Result := nil;
end;

function TXMLNode.GetAttributeNodes: TXMLChildNodeList;
begin
  case fNodeType of
    ntXMLDeclaration, ntElement:
      Result := OwnerDocument.GetCreateTempChildNodeList(@Self, ctAttribute);
  else
    Result := nil;
  end;
end;

function TXMLNode.GetAttributeNS(const aNameSpaceURI,
  aLocalName: OWideString): OWideString;
begin
  Result := GetAttributeNSDef(aNameSpaceURI, aLocalName, '');
end;

function TXMLNode.GetAttributeNSDef(const aNameSpaceURI, aLocalName,
  aDefaultValue: OWideString): OWideString;
begin
  if not FindAttributeNS(aNameSpaceURI, aLocalName, Result{%H-}) then
    Result := aDefaultValue;
end;

function TXMLNode.GetChildCount: Integer;
var
  xList: TXMLChildNodeList;
  xChild: PXMLNode;
begin
  if TryGetChildNodes(xList{%H-}, ctChild) then
  begin
    Result := xList.Count;
    Exit;
  end;

  Result := 0;
  xChild := fFirstCChild[ctChild];
  while Assigned(xChild) do
  begin
    Inc(Result);
    xChild := xChild.fNextSibling;
  end;
end;

{$IFDEF BCB}
function TXMLNode.GetChildFromBegin(const aIndex: Integer): PXMLNode;
begin
  Result := GetCChildFromBegin(aIndex, ctChild);
end;

function TXMLNode.GetChildFromEnd(const aIndex: Integer): PXMLNode;
begin
  Result := GetCChildFromEnd(aIndex, ctChild);
end;
{$ENDIF}

function TXMLNode.GetCChildFromBegin(const aIndex: Integer;
  const aChildType: TXMLChildType): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  //search forwards through all nodes
  I := -1;
  Result := nil;
  while (I < aIndex) and GetNextCChild(Result, aChildType) do
    Inc(I);

  if (I <> aIndex) or not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);
end;

function TXMLNode.GetCChildFromEnd(const aIndex: Integer;
  const aChildType: TXMLChildType): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  //search forwards through all nodes
  I := -1;
  Result := nil;
  while (I < aIndex) and GetPreviousCChild({%H-}Result, aChildType) do
    Inc(I);

  if (I <> aIndex) or not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);
end;

function TXMLNode.GetChildNodes: TXMLChildNodeList;
begin
  Result := OwnerDocument.GetCreateTempChildNodeList(@Self, ctChild);
end;

function TXMLNode.GetDoPreserveWhiteSpace: Boolean;
begin
  case fPreserveWhiteSpace of
    pwPreserve: Result := True;
    pwDefault: Result := False;
  else
    //pwInherit
    if Assigned(fParentNode) then
      Result := fParentNode.GetDoPreserveWhiteSpace
    else
      Result := False;
  end;
end;

function TXMLNode.GetElementsByTagName(const aName: OWideString;
  var outNodeList: IXMLNodeList): Boolean;
var
  xNodeNameId: OHashedStringsIndex;
  xNode: PXMLNode;
begin
  outNodeList := nil;

  xNodeNameId := fOwnerDocument.IndexOfString(aName);
  if xNodeNameId >= 0 then
  begin
    xNode := FirstChild;
    while Assigned(xNode) do
    begin
      if xNode.fNodeNameId = xNodeNameId then
      begin
        if not Assigned(outNodeList) then
          outNodeList := TXMLResNodeList.Create;
        outNodeList.Add(xNode);
      end;
      xNode := xNode.fNextSibling;
    end;
  end;

  Result := Assigned(outNodeList);
  if not Result then
    outNodeList := fOwnerDocument.DummyResNodeList;
end;

function TXMLNode.GetElementsByTagNameNS(const aNameSpaceURI,
  aLocalName: OWideString; var outNodeList: IXMLNodeList): Boolean;
var
  xNode: PXMLNode;
  xQualifiedNameIds: TXMLIntArray;
  I: Integer;
begin
  outNodeList := nil;

  SetLength(xQualifiedNameIds, 0);
  FindQualifiedNames(fOwnerDocument.IndexOfString(aNameSpaceURI), aLocalName, xQualifiedNameIds);

  if Length(xQualifiedNameIds) > 0 then
  begin
    xNode := FirstChild;
    while Assigned(xNode) do
    begin
      for I := Low(xQualifiedNameIds) to High(xQualifiedNameIds) do
      if xQualifiedNameIds[I] = xNode.fNodeNameId then
      begin
        if not Assigned(outNodeList) then
          outNodeList := TXMLResNodeList.Create;
        outNodeList.Add(xNode);
        Break;
      end;
      xNode := xNode.fNextSibling;
    end;
  end;

  Result := Assigned(outNodeList);
  if not Result then
    outNodeList := fOwnerDocument.DummyResNodeList;
end;

function TXMLNode.TryGetChildNodes(var outList: TXMLChildNodeList;
  const aChildType: TXMLChildType): Boolean;
begin
  Result := OwnerDocument.TryGetTempChildNodeList(@Self, aChildType, outList);
end;

function TXMLNode.GetFirstCChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fFirstCChild[aChildType];
end;

function TXMLNode.GetId: XMLNodeId;
begin
  Result := fOwnerDocument.GetNodeId(@Self);
end;

function TXMLNode.GetIsTextElement: Boolean;
begin
  Result := (NodeType = ntElement) and
    Assigned(fFirstCChild[ctChild]) and
    (fFirstCChild[ctChild] = fLastCChild[ctChild]) and
    (fFirstCChild[ctChild].NodeType = ntText);
end;

function TXMLNode.GetLastCChild(const aChildType: TXMLChildType): PXMLNode;
begin
  Result := fLastCChild[aChildType];
end;

function TXMLNode.GetLocalName: OWideString;
var
  xPrefix: OWideString;
begin
  OXmlResolveNameSpace(NodeName, xPrefix{%H-}, Result{%H-});
end;

function TXMLNode.GetNodeLevel: Integer;
var
  xNode: PXMLNode;
begin
  Result := 0;
  xNode := Self.ParentNode;
  while Assigned(xNode) do
  begin
    Inc(Result);
    xNode := xNode.ParentNode;
  end;
end;

function TXMLNode.GetNodeName: OWideString;
begin
  Result := fOwnerDocument.GetString(fNodeNameId);
end;

function TXMLNode.GetNameSpacePrefix: OWideString;
var
  xLocalName: OWideString;
begin
  OXmlResolveNameSpace(NodeName, Result{%H-}, xLocalName{%H-});
end;

function TXMLNode.GetNameSpaceURI: OWideString;
var
  xNSPrefix: OWideString;
begin
  xNSPrefix := NameSpacePrefix;
  if
    not ((fOwnerDocument.fTempNameSpaceURIs.Count > 0) and FindTempNameSpaceURI(xNSPrefix, Result{%H-}, True)) and
    not FindNameSpaceURIByPrefix(xNSPrefix, Result{%H-})
  then
    Result := '';
end;

function TXMLNode.GetNextAttribute(var ioAttrEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(ioAttrEnum, ctAttribute);
end;

function TXMLNode.GetNextCChild(var ioChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(ioChildEnum) then
    ioChildEnum := ioChildEnum.fNextSibling
  else
    ioChildEnum := GetFirstCChild(aChildType);

  Result := Assigned(ioChildEnum);
end;

function TXMLNode.GetNextChild(var ioChildEnum: PXMLNode): Boolean;
begin
  Result := GetNextCChild(ioChildEnum, ctChild);
end;

function TXMLNode.GetNextNodeInTree: PXMLNode;
var
  xParentNode: PXMLNode;
begin
  Result := fFirstCChild[ctChild];
  if Assigned(Result) then
    Exit;

  Result := fNextSibling;
  if Assigned(Result) then
    Exit;

  xParentNode := fParentNode;
  while Assigned(xParentNode) and not Assigned(Result) do
  begin
    Result := xParentNode.fNextSibling;
    xParentNode := xParentNode.ParentNode;
  end;
end;

function TXMLNode.GetPreviousAttribute(var ioAttrEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(ioAttrEnum, ctAttribute);
end;

function TXMLNode.GetPreviousCChild(var ioChildEnum: PXMLNode;
  const aChildType: TXMLChildType): Boolean;
begin
  if Assigned(ioChildEnum) then
    ioChildEnum := ioChildEnum.fPreviousSibling
  else
    ioChildEnum := fLastCChild[aChildType];

  Result := Assigned(ioChildEnum);
end;

function TXMLNode.GetPreviousChild(var ioChildEnum: PXMLNode): Boolean;
begin
  Result := GetPreviousCChild(ioChildEnum, ctChild);
end;

function TXMLNode.GetPreviousNodeInTree: PXMLNode;
var
  xPreviousSiblingLastChild: PXMLNode;
begin
  Result := nil;
  xPreviousSiblingLastChild := fPreviousSibling;
  while Assigned(xPreviousSiblingLastChild) do
  begin
    Result := xPreviousSiblingLastChild;
    xPreviousSiblingLastChild := xPreviousSiblingLastChild.fLastCChild[ctChild];
  end;

  if not Assigned(Result) then
    Result := fParentNode;
end;

function TXMLNode.GetText: OWideString;
var
  xChild: PXMLNode;
begin
  Result := '';
  case NodeType of
    ntText, ntCData, ntEntityReference: Result := NodeValue;
    ntDocument, ntElement:
    begin
      xChild := fFirstCChild[ctChild];
      while Assigned(xChild) do
      begin
        Result := Result + xChild.Text;
        xChild := xChild.fNextSibling;
      end;
    end;
  end
end;

function TXMLNode.XML: OWideString;
begin
  SaveToXML(Result{%H-});
end;

function TXMLNode.XML(const aIndentType: TXMLIndentType): OWideString;
begin
  SaveToXML(Result{%H-}, aIndentType);
end;

function TXMLNode.XML_UTF8: OUTF8Container;
begin
  SaveToXML_UTF8(Result{%H-});
end;

function TXMLNode.XML_UTF8(const aIndentType: TXMLIndentType): OUTF8Container;
begin
  SaveToXML_UTF8(Result{%H-}, aIndentType);
end;

function TXMLNode._AddAttribute(const aAttrName, aAttrValue: OWideString): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and OSameText(aAttrName, XML_XML_SPACE) then
  begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else
  begin
    if FindAttribute(aAttrName, Result) then
    begin
      Result.SetNodeValue(aAttrValue);
    end else
    begin
      Result := fOwnerDocument.CreateNode(ntAttribute, aAttrName, aAttrValue);
      Append(Result, ctAttribute);
    end;
  end;
end;

procedure TXMLNode._SetAttribute(const aAttrName, aAttrValue: OWideString);
begin
  _AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.GetNodeValue: OWideString;
begin
  case fNodeType of
    ntXMLDeclaration:
    begin
      Result := Self.XML;
      Result := Copy(Result, 7, Length(Result)-8);//extract content from "<?xml content?>"
    end;
  else
    Result := fOwnerDocument.GetString(fNodeValueId);
  end;
end;

function TXMLNode.HasAttribute(const aName: OWideString): Boolean;
var
  x: PXMLNode;
begin
  Result := FindAttribute(aName, x{%H-});
end;

function TXMLNode.HasAttributeNS(const aNameSpaceURI,
  aLocalName: OWideString): Boolean;
var
  x: PXMLNode;
begin
  Result := FindAttributeNS(aNameSpaceURI, aLocalName, x{%H-});
end;

function TXMLNode.HasAttributes: Boolean;
begin
  Result := Assigned(fFirstCChild[ctAttribute]);
end;

function TXMLNode.HasChildNodes: Boolean;
begin
  Result := Assigned(fFirstCChild[ctChild]);
end;

procedure TXMLNode.Init(const aNodeType: TXMLNodeType;
  const aNodeNameId, aNodeValueId: OHashedStringsIndex;
  const aOwnerDocument: TXMLDocument);
begin
  fOwnerDocument := aOwnerDocument;
  fNodeType := aNodeType;
  fNodeNameId := aNodeNameId;
  fNodeValueId := aNodeValueId;

  fParentNode := nil;
  fFirstCChild[ctChild] := nil;
  fLastCChild[ctChild] := nil;
  fFirstCChild[ctAttribute] := nil;
  fLastCChild[ctAttribute] := nil;
  fNextSibling := nil;
  fPreviousSibling := nil;
  fPreserveWhiteSpace := pwInherit;
end;

procedure TXMLNode.Insert(const aNew, aBeforeNode: PXMLNode;
  const aChildType: TXMLChildType);
var
  xAfterNode: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if aBeforeNode.fParentNode <> @Self then
    raise EXmlDOMException.Create(OXmlLng_NodeToInsertNotAChild);

  if TryGetChildNodes(xList{%H-}, aChildType) then
    xList.ExtNodeInserted;
  if (aChildType = ctAttribute) and (fOwnerDocument.fTempAttributeIndex.ParentElement = @Self) then
    fOwnerDocument.fTempAttributeIndex.ExtAttributeInserted;

  xAfterNode := aBeforeNode.PreviousSibling;
  if Assigned(xAfterNode) then
  begin
    xAfterNode.fNextSibling := aNew;
    aNew.fPreviousSibling := xAfterNode;
  end else
  begin
    aNew.fPreviousSibling := nil;
  end;

  if fFirstCChild[aChildType] = aBeforeNode then
    fFirstCChild[aChildType] := aNew;
  aBeforeNode.fPreviousSibling := aNew;
  aNew.fNextSibling := aBeforeNode;

  aNew.fParentNode := @Self;
end;

function TXMLNode.InsertAttribute(const aAttrName, aAttrValue: OWideString;
  const aBeforeAttribute: PXMLNode): PXMLNode;
begin
  Result := nil;
  if (OwnerDocument.WhiteSpaceHandling = wsAutoTag) and OSameText(aAttrName, XML_XML_SPACE) then
  begin
    Self.fPreserveWhiteSpace := OXmlStrToPreserve(aAttrValue);
  end else
  begin
    DeleteAttribute(aAttrName);

    Result := fOwnerDocument.CreateNode(ntAttribute, aAttrName, aAttrValue);

    Insert(Result, aBeforeAttribute, ctAttribute);
  end;
end;

function TXMLNode.InsertAttribute(const aAttrName, aAttrValue: OWideString;
  const aBeforeAttributeName: OWideString): PXMLNode;
var
  xBeforeAttr: PXMLNode;
begin
  if FindAttribute(aBeforeAttributeName, xBeforeAttr{%H-}) then
    Result := InsertAttribute(aAttrName, aAttrValue, xBeforeAttr)
  else
    Result := AddAttribute(aAttrName, aAttrValue);
end;

function TXMLNode.InsertBefore(const aNewChild, aRefChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  if (aNewChild = aRefChild) then
    raise EXmlDOMException.Create(OXmlLng_InsertEqualNodes);

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  Result := aNewChild;
  if Assigned(aRefChild) then
    Insert(Result, aRefChild, ctChild)
  else
    Append(Result, ctChild);
end;

function TXMLNode.InsertCDATASection(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntCData, '', aText, aBeforeNode);
end;

function TXMLNode.InsertCustomChild(const aType: TXMLNodeType; const aName,
  aValue: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := fOwnerDocument.CreateNode(aType, aName, aValue);
  Insert(Result, aBeforeNode, ctChild);
end;

function TXMLNode.InsertComment(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntComment, '', aText, aBeforeNode);
end;

function TXMLNode.InsertChild(const aElementName: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntElement, aElementName, '', aBeforeNode);
end;

function TXMLNode.InsertXMLDeclaration(const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntXMLDeclaration, '', '', aBeforeNode);
end;

function TXMLNode.LoadFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetBuffer(aBuffer);

    Result := LoadFromStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TXMLNode.LoadFromBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetPointer(@aBuffer, aBufferLength);

    Result := LoadFromStream(xStream, aForceEncoding);
  finally
    xStream.Free;
  end;
end;

function TXMLNode.LoadFromFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding): Boolean;
var
  xFS: TOFileStream;
begin
  fOwnerDocument.URL := aFileName;
  xFS := TOFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadFromStream(xFS, aForceEncoding);
  finally
    xFS.Free;
  end;
end;

function TXMLNode.LoadFromReader(const aReader: TXMLReader;
  var outReaderToken: PXMLReaderToken; const aBreakWhenRefNodeReached: PXMLNode): Boolean;
var
  xLastNode: PXMLNode;
begin
  fOwnerDocument.fParseError := nil;
  Result := True;

  if not (NodeType in [ntDocument, ntElement]) then
    raise EXmlDOMException.Create(OXmlLng_NodeMustBeDOMDocumentOrElement);

  DeleteChildren(True);

  fOwnerDocument.Loading := True;
  try
    xLastNode := @Self;
    while aReader.ReadNextToken({%H-}outReaderToken) do
    begin
      case outReaderToken.TokenType of
        rtText:
          if not Assigned(xLastNode.AddText(outReaderToken.TokenValue)) then
            Continue;//must be here sue to seq parser -> do not check for "aBreakWhenRefNodeReached" if text was not added!
        rtCData: xLastNode.AddCDATASection(outReaderToken.TokenValue);
        rtComment: xLastNode.AddComment(outReaderToken.TokenValue);
        rtDocType: xLastNode.AddDocType(outReaderToken.TokenValue);
        rtProcessingInstruction: xLastNode.AddProcessingInstruction(outReaderToken.TokenName, outReaderToken.TokenValue);
        rtEntityReference:
          xLastNode.Append(fOwnerDocument.CreateNode(ntEntityReference, outReaderToken.TokenName, outReaderToken.TokenValue), ctChild);
        rtOpenElement:
          xLastNode := xLastNode.AddChild(outReaderToken.TokenName);
        rtOpenXMLDeclaration:
          xLastNode := xLastNode.AddXMLDeclaration;
        rtXMLDeclarationAttribute, rtAttribute:
          xLastNode._AddAttribute(outReaderToken.TokenName, outReaderToken.TokenValue);
        rtFinishOpenElementClose, rtCloseElement, rtFinishXMLDeclarationClose:
          xLastNode := xLastNode.ParentNode;
      end;

      if Assigned(aBreakWhenRefNodeReached) and
        not (outReaderToken.TokenType in [rtOpenElement, rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtAttribute])
      then begin
        if Assigned(xLastNode) then
        begin
          if Assigned(aBreakWhenRefNodeReached) and (
            (xLastNode = aBreakWhenRefNodeReached)) then
          begin
            Break;
          end;
        end else
        begin
          Break;//This is not an error -> it may happen in the sequential reader (error would be raised already in TXMLReader!)
        end;
      end;
    end;

    if Assigned(aReader.ParseError) then
    begin
      fOwnerDocument.fParseError := aReader.ParseError;

      Result := False;

      Self.DeleteChildren;
    end;
  finally
    fOwnerDocument.Loading := False;
    fOwnerDocument.URL := '';
  end;
end;

function TXMLNode.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
var
  xReader: TXMLReader;
  xReaderToken: PXMLReaderToken;
begin
  xReader := TXMLReader.Create;
  try
    xReader.ReaderSettings.Assign(OwnerDocument.fReaderSettings);

    xReader.InitStream(aStream, aForceEncoding);
    xReader.URL := fOwnerDocument.URL;

    Result := LoadFromReader(xReader, xReaderToken{%H-});

    if Result then
      OwnerDocument.fReaderSettings.EntityList.Assign(xReader.ReaderSettings.EntityList);
  finally
    xReader.Free;
  end;
end;

function TXMLNode.LoadFromXML(const aXML: OWideString): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString(aXML);

    Result := LoadFromStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

function TXMLNode.NameSpaceExists(const aNameSpaceURI,
  aNameSpacePrefix: OWideString): Boolean;
var
  xIdURI: OHashedStringsIndex;
  xNameSpacePrefix: OWideString;
begin
  xIdURI := fOwnerDocument.IndexOfString(aNameSpaceURI);
  if xIdURI >= 0 then
  begin
    xNameSpacePrefix := aNameSpacePrefix;
    Result := FindNameSpace(xIdURI, xNameSpacePrefix);
  end else
    Result := False;
end;

procedure TXMLNode.Normalize;
var
  xText: OWideString;
  xPrevChild, xNextChild, xChild: PXMLNode;
begin
  xPrevChild := nil;
  xChild := FirstChild;
  while Assigned(xChild) do
  begin
    case xChild.NodeType of
      ntText:
      begin
        xText := Trim(xChild.Text);
        if xText = '' then
        begin
          xNextChild := xChild.NextSibling;
          xChild.DeleteSelf;
          xChild := xNextChild;
          Continue;//do not actualize xPrevChild
        end;

        if Assigned(xPrevChild) and (xPrevChild.NodeType = ntText) then
        begin
          xPrevChild.Text := xPrevChild.Text + xText;
          xNextChild := xChild.NextSibling;
          xChild.DeleteSelf;
          xChild := xNextChild;
          Continue;//do not actualize xPrevChild
        end;

        xChild.Text := xText;
      end;
      ntDocument, ntElement:
        xChild.Normalize;
    end;

    xPrevChild := xChild;
    xChild := xChild.NextSibling;
  end;
end;

procedure TXMLNode.QuickSort(aLow, aHigh: Integer;
  const aCompare: TXMLNodeCompare; const aChildNodeList: IXMLNodeList);
var
  xLow, xHigh, xMid: Integer;
begin
  repeat
    xLow := aLow;
    xHigh := aHigh;
    xMid := (aLow + aHigh) shr 1;
    repeat
      while aCompare(aChildNodeList[xLow], aChildNodeList[xMid]) < 0 do
        Inc(xLow);
      while aCompare(aChildNodeList[xHigh], aChildNodeList[xMid]) > 0 do
        Dec(xHigh);

      if xLow <= xHigh then
      begin
        if xLow <> xHigh then
        begin
          //exchange nodes in xml document
          aChildNodeList[xLow].ExchangeWithNode(aChildNodeList[xHigh]);
          //exchange nodes in temp list
          aChildNodeList.Exchange(xLow, xHigh);
        end;
        if xMid = xLow then
          xMid := xHigh
        else if xMid = xHigh then
          xMid := xLow;
        Inc(xLow);
        Dec(xHigh);
      end;
    until xLow > xHigh;
    if aLow < xHigh then
      QuickSort(aLow, xHigh, aCompare, aChildNodeList);
    aLow := xLow;
  until xLow >= aHigh;
end;

function TXMLNode.LoadFromXML_UTF8(const aXML: OUTF8Container): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString_UTF8(aXML);

    Result := LoadFromStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.Remove(const aOld: PXMLNode; const aChildType: TXMLChildType);
var
  xPrev, xNext: PXMLNode;
  xList: TXMLChildNodeList;
begin
  if aOld.fParentNode <> @Self then
    raise EXmlDOMException.Create(OXmlLng_NodeToDeleteNotAChild);

  if TryGetChildNodes(xList{%H-}, aChildType) then
    xList.ExtNodeRemoved;
  if (aChildType = ctAttribute) and (fOwnerDocument.fTempAttributeIndex.ParentElement = @Self) then
    fOwnerDocument.fTempAttributeIndex.ExtAttributeRemoved;

  if fFirstCChild[aChildType] = aOld then
    fFirstCChild[aChildType] := aOld.fNextSibling;
  if fLastCChild[aChildType] = aOld then
    fLastCChild[aChildType] := aOld.fPreviousSibling;

  xPrev := aOld.PreviousSibling;
  xNext := aOld.NextSibling;
  if Assigned(xPrev) then
  begin
    if Assigned(xNext) then
      xPrev.fNextSibling := xNext
    else
      xPrev.fNextSibling := nil;
  end;
  if Assigned(xNext) then
  begin
    if Assigned(xPrev) then
      xNext.fPreviousSibling := xPrev
    else
      xNext.fPreviousSibling := nil;
  end;

  aOld.fParentNode := nil;
  aOld.fNextSibling := nil;
  aOld.fPreviousSibling := nil;
end;

function TXMLNode.RemoveAttribute(const aOldAttribute: PXMLNode): PXMLNode;
begin
  Remove(aOldAttribute, ctAttribute);
  Result := aOldAttribute;
end;

function TXMLNode.RemoveChild(const aOldChild: PXMLNode): PXMLNode;
begin
  Remove(aOldChild, ctChild);
  Result := aOldChild;
end;

procedure TXMLNode.RemoveSelfFromParent;
begin
  if Assigned(fParentNode) then
  begin
    if fNodeType = ntAttribute then
      fParentNode.Remove(@Self, ctAttribute)
    else
      fParentNode.Remove(@Self, ctChild);
  end;
end;

function TXMLNode.ReplaceChild(const aNewChild, aOldChild: PXMLNode): PXMLNode;
begin
  if (aNewChild.OwnerDocument = nil) or (aNewChild.OwnerDocument <> Self.OwnerDocument) then
    raise EXmlDOMException.Create(OXmlLng_InsertFromDifferentDocument);

  Result := aOldChild;

  if Assigned(aNewChild.fParentNode) then
    aNewChild.ParentNode.RemoveChild(aNewChild);

  aOldChild.ParentNode.InsertBefore(aNewChild, aOldChild);
  aOldChild.ParentNode.RemoveChild(aOldChild);
end;

function TXMLNode.InsertDocType(const aDocTypeRawText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntDocType, '', aDocTypeRawText, aBeforeNode);
end;

function TXMLNode.InsertEntityReference(const aEntityName: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := fOwnerDocument.CreateEntityReference(aEntityName);
  Insert(Result, aBeforeNode, ctChild);
end;

function TXMLNode.InsertProcessingInstruction(const aTarget,
  aContent: OWideString; const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntComment, aTarget, aContent, aBeforeNode);
end;

function TXMLNode.InsertText(const aText: OWideString;
  const aBeforeNode: PXMLNode): PXMLNode;
begin
  Result := InsertCustomChild(ntElement, '', aText, aBeforeNode);
end;

function TXMLNode.SelectNode(const aXPath: OWideString;
  var outNode: PXMLNode): Boolean;
var
  xNodeList: IXMLNodeList;
  xChildType: TXMLChildType;
  xNodeName: OWideString;
begin
  if aXPath = '' then
    raise EXmlDOMException.Create(OXmlLng_XPathCannotBeEmpty);

  if XPathIsSimpleNode(aXPath, xNodeName{%H-}, xChildType{%H-}) then
  begin
    //speed optimization without true XPath support
    case xChildType of
      ctChild: Result := FindChild(xNodeName, outNode);
      ctAttribute: Result := FindAttribute(xNodeName, outNode);
    else
      Result := False;//delphi DCC warning
    end;
  end else
  begin
    xNodeList := nil;

    Result := SelectNodes(aXPath, xNodeList, 1);
    if Result and (xNodeList.Count > 0) then
      outNode := xNodeList[0]
    else
      outNode := nil;
  end;
end;

procedure TXMLNode.SaveToBuffer(var outBuffer: TBytes);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream);

    SetLength(outBuffer, xStream.Size);
    if xStream.Size > 0 then
    begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outBuffer[0], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

{$IFDEF O_HASBYTESTRINGS}
procedure TXMLNode.SaveToBuffer(var outBuffer: OUTF8Container);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    SaveToStream(xStream);

    SetLength(outBuffer, xStream.Size);
    if xStream.Size > 0 then
    begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outBuffer[OUTF8Container_FirstElement], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;
{$ENDIF}

procedure TXMLNode.SaveToFile(const aFileName: OWideString);
var
  xFS: TOFileStream;
begin
  xFS := TOFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(xFS);
  finally
    xFS.Free;
  end;
end;

procedure TXMLNode.SaveToStream(const aStream: TStream);
var
  xWriter: TXMLWriter;
begin
  xWriter := TXMLWriter.Create;
  try
    xWriter.InitStream(aStream);
    xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
    xWriter.Encoding := TEncoding.EncodingFromCodePage(OwnerDocument.CodePage);

    SaveToWriter(xWriter);
  finally
    xWriter.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var outXML: OWideString);
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TXMLWriter.Create;
    try
      xWriter.InitStream(xStream);
      xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.OWideStringEncoding;
      xWriter.WriterSettings.WriteBOM := False;
      xWriter.WriterSettings.LineBreak := XMLDefaultLineBreak;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(outXML, xStream.Size div SizeOf(OWideChar));
    if xStream.Size > 0 then
    begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outXML[1], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType);
var
  xIndentType: TXMLIndentType;
begin
  xIndentType := fOwnerDocument.fWriterSettings.IndentType;
  fOwnerDocument.fWriterSettings.IndentType := aIndentType;
  try
    SaveToXML(outXML);
  finally
    fOwnerDocument.fWriterSettings.IndentType := xIndentType;
  end;
end;

procedure TXMLNode.SaveToXML_UTF8(var outXML: OUTF8Container);
var
  xStream: TMemoryStream;
  xWriter: TXMLWriter;
begin
  xStream := TMemoryStream.Create;
  try
    xWriter := TXMLWriter.Create;
    try
      xWriter.InitStream(xStream);
      xWriter.WriterSettings.Assign(OwnerDocument.fWriterSettings);
      xWriter.Encoding := TEncoding.UTF8;
      xWriter.WriterSettings.WriteBOM := False;
      xWriter.WriterSettings.LineBreak := XMLDefaultLineBreak;

      SaveToWriter(xWriter);
    finally
      xWriter.Free;
    end;

    SetLength(outXML, xStream.Size);
    if xStream.Size > 0 then
    begin
      xStream.Seek(0, soFromBeginning);
      xStream.ReadBuffer(outXML[OUTF8Container_FirstElement], xStream.Size);
    end;
  finally
    xStream.Free;
  end;
end;

procedure TXMLNode.SaveToXML_UTF8(var outXML: OUTF8Container; const aIndentType: TXMLIndentType);
var
  xIndentType: TXMLIndentType;
begin
  xIndentType := fOwnerDocument.fWriterSettings.IndentType;
  fOwnerDocument.fWriterSettings.IndentType := aIndentType;
  try
    SaveToXML_UTF8(outXML);
  finally
    fOwnerDocument.fWriterSettings.IndentType := xIndentType;
  end;
end;

function TXMLNode.SelectNode(const aXPath: OWideString): PXMLNode;
begin
  SelectNode(aXPath, Result{%H-});
end;

function TXMLNode.SelectNodeCreate(const aNodePath: OWideString): PXMLNode;
var
  xNodeList: TOWideStringList;
  xNodeName: OWideString;
  xParentNode, xCurrentNode: PXMLNode;
  I: Integer;
begin
  xCurrentNode := nil;
  xNodeList := TOWideStringList.Create;
  try
    OExplode(aNodePath, '/', xNodeList);

    if xNodeList.Count = 0 then
      raise EXMLDOMException.Create(OXmlLng_PathCannotBeEmpty);

    xParentNode := @Self;
    for I := 0 to xNodeList.Count-1 do
    begin
      xNodeName := xNodeList[I];
      if xNodeName = '' then
        raise EXmlDOMException.Create(OXmlLng_NodeNameCannotBeEmpty);

      if not xParentNode.SelectNode(xNodeName, {%H-}xCurrentNode) then
      begin
        if xNodeName[1] = '@' then
          xCurrentNode := xParentNode.AddAttribute(Copy(xNodeName, 2, High(Integer)), '')
        else
          xCurrentNode := xParentNode.AddChild(xNodeName);
      end;
      xParentNode := xCurrentNode;
    end;
  finally
    xNodeList.Free;
  end;

  Result := xCurrentNode;
end;

function TXMLNode.SelectNodeDummy(const aXPath: OWideString): PXMLNode;
begin
  if not SelectNode(aXPath, Result{%H-}) then
    Result := OwnerDocument.DummyNode;
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  const aMaxNodeCount: Integer): IXMLNodeList;
begin
  SelectNodes(aXPath, Result{%H-}, aMaxNodeCount);
end;

function TXMLNode.SelectNodes(const aXPath: OWideString;
  var outNodeList: IXMLNodeList; const aMaxNodeCount: Integer): Boolean;
var
  xXPaths: TXMLXPathList;
  xCustomList: TXMLXPathNodeList;
begin
  xXPaths := TXMLXPathList.Create(TXMLXPathDOMAdapter.Create(OwnerDocument));
  try
    xXPaths.LoadFromString(aXPath);

    xCustomList := nil;//must be here -> list will be created in SelectNodes
    Result := xXPaths.SelectNodes(@Self, xCustomList, aMaxNodeCount);
    if Result then
      outNodeList := (IInterface(xCustomList) as IXMLNodeList)
    else
      outNodeList := OwnerDocument.DummyResNodeList;
  finally
    xXPaths.Free;
  end;
end;

function TXMLNode.SetAttribute(const aName, aValue: OWideString): PXMLNode;
begin
  AddAttribute(aName, aValue);
  Result := @Self;
end;

function TXMLNode.SetAttributeNode(const aAttr: PXMLNode): PXMLNode;
begin
  if aAttr.OwnerDocument <> Self.OwnerDocument then
    raise EXmlDOMException.Create(OXmlLng_AppendFromDifferentDocument);

  if Assigned(aAttr.fParentNode) then
    raise EXmlDOMException.Create(OXmlLng_ParentNodeMustBeNil);

  if FindAttribute(aAttr.NodeName, Result{%H-}) then
    Remove(Result, ctAttribute)
  else
    Result := nil;

  Append(aAttr, ctAttribute);
end;

function TXMLNode.SetAttributeNS(const aNameSpaceURI, aQualifiedName,
  aValue: OWideString): PXMLNode;
var
  xNSPrefix, xLocalName: OWideString;
begin
  Result := SetAttribute(aQualifiedName, aValue);

  OXmlResolveNameSpace(aQualifiedName, xNSPrefix{%H-}, xLocalName{%H-});
  if not NameSpaceExists(aNameSpaceURI, xNSPrefix) then
  begin
    //namespace not found, add namespace URI attribute
    SetAttribute(OXmlApplyNameSpace(XML_XMLNS, xNSPrefix), aNameSpaceURI);
  end;
end;

procedure TXMLNode.SetNodeName(const aName: OWideString);
begin
  NodeNameId := fOwnerDocument.SetString(aName);
end;

procedure TXMLNode.SetNodeNameId(const aNameId: OHashedStringsIndex);
begin
  case fNodeType of
    ntElement, ntProcessingInstruction: fNodeNameId := aNameId;
  else
    raise EXmlDOMException.Create(OXmlLng_SetNodeNameWrongType);
  end;
end;

procedure TXMLNode.SetNodeValue(const aValue: OWideString);
  procedure _SetTextAndTrim;
  begin
    if OwnerDocument.fWhiteSpaceHandling = wsTrim then
      fNodeValueId := fOwnerDocument.SetString(Trim(aValue))
    else
      fNodeValueId := fOwnerDocument.SetString(aValue);
  end;
  procedure _SetXMLDeclarationData;
  var
    xXML: IXMLDocument;
    xAttr: PXMLNode;
  begin
    Self.DeleteAttributes(False);
    if aValue <> '' then
    begin
      xXML := CreateXMLDoc;
      xXML.LoadFromXML('<x '+aValue+'/>');
      xAttr := nil;
      while xXML.Node.FirstChild.GetNextAttribute(xAttr) do
        Self.SetAttribute(xAttr.NodeName, xAttr.NodeValue);
    end;
  end;
begin
  case fNodeType of
    ntText: _SetTextAndTrim;
    ntXMLDeclaration: _SetXMLDeclarationData;
  else
    fNodeValueId := fOwnerDocument.SetString(aValue);
  end;
end;

procedure TXMLNode.SetText(const aText: OWideString);
begin
  case NodeType of
    ntText, ntCData: SetNodeValue(aText);
    ntDocument, ntElement:
    begin
      DeleteChildren(True);
      AddText(aText);
    end;
  else
    raise EXmlDOMException.Create(OXmlLng_CannotSetText);
  end;
end;

procedure TXMLNode.SortAttributeNodes(const aCompare: TXMLNodeCompare);
var
  xAttrList: IXMLNodeList;
begin
  if not Assigned(FirstAttribute) then
    Exit;

  fOwnerDocument.fTempAttributeIndex.SetParentElement(nil);//clear attribute index

  xAttrList := TXMLResNodeList.Create;
  FillCChildList(xAttrList, ctAttribute);
  QuickSort(0, xAttrList.Count-1, aCompare, xAttrList);
end;

procedure TXMLNode.SortAttributeNodesByName;
begin
  SortAttributeNodes(CompareNodeNames);
end;

procedure TXMLNode.SortChildNodes(const aCompare: TXMLNodeCompare; const aDeep: Boolean);
var
  xChildList: IXMLNodeList;
  I: Integer;
begin
  if not Assigned(FirstChild) then
    Exit;

  xChildList := TXMLResNodeList.Create;
  FillCChildList(xChildList, ctChild);
  QuickSort(0, xChildList.Count-1, aCompare, xChildList);

  if aDeep then
  for I := 0 to xChildList.Count-1 do
    xChildList[I].SortChildNodes(aCompare, aDeep);
end;

procedure TXMLNode.SortChildNodesByName(const aDeep: Boolean);
begin
  SortChildNodes(CompareNodeNames, aDeep);
end;

procedure TXMLNode.WriteChildrenXML(
  const aWriter: TXMLWriter);
var
  xChild: PXMLNode;
begin
  xChild := fFirstCChild[ctChild];
  while Assigned(xChild) do
  begin
    xChild.SaveToWriter(aWriter);
    xChild := xChild.fNextSibling;
  end;
end;

procedure TXMLNode.WriteAttributesXML(const aWriter: TXMLWriter);
var
  xAttr: PXMLNode;
begin
  xAttr := fFirstCChild[ctAttribute];
  while Assigned(xAttr) do
  begin
    aWriter.Attribute(xAttr.NodeName, xAttr.NodeValue);
    xAttr := xAttr.fNextSibling;
  end;

  if (fOwnerDocument.WhiteSpaceHandling = wsAutoTag) and
     (fNodeType = ntElement) and
     (Self.fPreserveWhiteSpace <> pwInherit) and
     (Self.fPreserveWhiteSpace <> ParentNode.fPreserveWhiteSpace)
  then
    aWriter.Attribute(XML_XML_SPACE, OXmlPreserveToStr(Self.fPreserveWhiteSpace));
end;

procedure TXMLNode.SaveToWriter(const aWriter: TXMLWriter);
var
  xDict: TOHashedStrings;
begin
  xDict := fOwnerDocument.fDictionary;
  case fNodeType of
    ntDocument: WriteChildrenXML(aWriter);
    ntElement: begin
      aWriter.OpenElement(xDict.GetItem(fNodeNameId).Text);
      WriteAttributesXML(aWriter);
      if HasChildNodes then
      begin
        aWriter.FinishOpenElement;
        WriteChildrenXML(aWriter);
        aWriter.CloseElement(xDict.GetItem(fNodeNameId).Text,
          (aWriter.WriterSettings.IndentType <> itNone) and//speed optimization
          not (//IsTextElement
            (fFirstCChild[ctChild] = fLastCChild[ctChild]) and
            (fFirstCChild[ctChild].NodeType in [ntText, ntCData])));
      end else
      begin
        aWriter.FinishOpenElementClose;
      end;
    end;
    ntXMLDeclaration: begin
      aWriter.OpenXMLDeclaration;
      WriteAttributesXML(aWriter);
      aWriter.FinishOpenXMLDeclaration;
    end;
    ntAttribute: aWriter.Attribute(
      xDict.GetItem(fNodeNameId).Text,
      xDict.GetItem(fNodeValueId).Text);
    ntText: begin
      aWriter.Text(xDict.GetItem(fNodeValueId).Text,
        //= not ParentNode.IsTextElement
        Assigned(fNextSibling) or Assigned(fPreviousSibling));
    end;
    ntCData: begin
      aWriter.CData(xDict.GetItem(fNodeValueId).Text,
        //= not ParentNode.IsTextElement
        Assigned(fNextSibling) or Assigned(fPreviousSibling));
    end;
    ntEntityReference: begin
      aWriter.EntityReference(xDict.GetItem(fNodeNameId).Text);
    end;
    ntComment: begin
      aWriter.Comment(xDict.GetItem(fNodeValueId).Text);
    end;
    ntDocType: begin
      aWriter.DocType(xDict.GetItem(fNodeValueId).Text);
    end;
    ntProcessingInstruction: begin
      aWriter.ProcessingInstruction(
        xDict.GetItem(fNodeNameId).Text,
        xDict.GetItem(fNodeValueId).Text);
    end;
  end;
end;

{ TXMLDocument }

constructor TXMLDocument.Create(const aRootNodeName: OWideString;
  const aAddXMLDeclaration: Boolean);
var
  xDec: PXMLNode;
begin
  inherited Create;

  DoCreate;

  if aAddXMLDeclaration then
  begin
    xDec := fBlankDocumentNode.AddXMLDeclaration;
    xDec.AddAttribute('version', '1.0');
    xDec.AddAttribute('encoding', 'utf-8');
    xDec.AddAttribute('standalone', 'yes');
  end;

  if aRootNodeName <> '' then
    fBlankDocumentNode.AddChild(aRootNodeName);
end;

function TXMLDocument.CreateAttribute(const aName,
  aValue: OWideString): PXMLNode;
begin
  Result := CreateNode(ntAttribute, aName, aValue);
end;

function TXMLDocument.CreateAttributeNS(const aNameSpaceURI, aQualifiedName,
  aValue: OWideString): PXMLNode;
begin
  Result := CreateAttribute(aQualifiedName, aValue);
  if aNameSpaceURI <> '' then
    fTempNameSpaceURIs.Add(Result, SetString(aNameSpaceURI));
end;

function TXMLDocument.CreateCDATASection(const aData: OWideString): PXMLNode;
begin
  Result := CreateNode(ntCData, '', aData);
end;

function TXMLDocument.CreateComment(const aText: OWideString): PXMLNode;
begin
  Result := CreateNode(ntComment, '', aText);
end;

function TXMLDocument.CreateDocType(
  const aDocTypeRawText: OWideString): PXMLNode;
begin
  Result := CreateNode(ntDocType, '', aDocTypeRawText);
end;

function TXMLDocument.CreateElement(const aNodeName: OWideString): PXMLNode;
begin
  Result := CreateNode(ntElement, aNodeName, '');
end;

function TXMLDocument.CreateElementNS(const aNameSpaceURI,
  aQualifiedName: OWideString): PXMLNode;
begin
  Result := CreateElement(aQualifiedName);
  if aNameSpaceURI <> '' then
    fTempNameSpaceURIs.Add(Result, SetString(aNameSpaceURI));
end;

function TXMLDocument.CreateEntityReference(const aName: OWideString): PXMLNode;
var
  xEntityValue: OWideString;
begin
  if fReaderSettings.EntityList.Find(aName, xEntityValue{%H-}) then
    Result := CreateNode(ntEntityReference, aName, xEntityValue)
  else
    raise EXmlDOMException.Create(OXmlLng_EntityNameNotFound);
end;

function TXMLDocument.CreateNode(const aNodeType: TXMLNodeType;
  const aNodeNameId, aNodeValueId: OHashedStringsIndex): PXMLNode;
{$IFDEF O_GENERICS}
var
  xEnum: TPair<PXMLNode,Boolean>;
{$ENDIF}
begin
  if fFreeNodes.Count = 0 then
  begin
    //use new id
    if fNextNodeId >= fNodesLength then
      Grow;
    Result := GetNodeById(fNextNodeId);
    Inc(fNextNodeId);
  end else
  begin
    //use last free id - from the end to be sure no memory must be moved
    {$IFDEF O_GENERICS}
    Result := nil;
    for xEnum in fFreeNodes do//trick to get the first key in fFreeNodes
    begin
      Result := xEnum.Key;
      fFreeNodes.Remove(Result);
      Break;
    end;
    {$ELSE}
    Result := {%H-}PXMLNode(fFreeNodes[fFreeNodes.Count-1]);
    fFreeNodes.Delete(fFreeNodes.Count-1);
    {$ENDIF}

  end;
  Result.Init(aNodeType, aNodeNameId, aNodeValueId, Self);
end;

constructor TXMLDocument.Create(aDummy: TObject);
begin
  inherited Create;

  DoCreate;
end;

function TXMLDocument.CreateNode(const aNodeType: TXMLNodeType;
  const aNodeName, aNodeValue: OWideString): PXMLNode;
begin
  Result := CreateNode(aNodeType, SetString(aNodeName), SetString(aNodeValue));
end;

function TXMLDocument.CreateProcessingInstruction(const aTarget,
  aContent: OWideString): PXMLNode;
begin
  Result := CreateNode(ntProcessingInstruction, aTarget, aContent);
end;

function TXMLDocument.GetCreateTempChildNodeList(
  const aParentNode: PXMLNode; const aChildType: TXMLChildType): TXMLChildNodeList;
begin
  if not TryGetTempChildNodeList(aParentNode, aChildType, Result{%H-}) then
  begin
    Result := TXMLChildNodeList.Create(aParentNode, aChildType);
    fTempChildNodes[aChildType].Add(aParentNode, Result);
  end;
end;

function TXMLDocument.CreateTextNode(const aText: OWideString): PXMLNode;
begin
  Result := CreateNode(ntText, '', aText);
end;

function TXMLDocument.CreateXMLDeclaration: PXMLNode;
begin
  Result := CreateNode(ntXMLDeclaration, XML_XML, '');
end;

function TXMLDocument.AddChild(const aElementName: OWideString): PXMLNode;
begin
  Result := Node.AddChild(aElementName);
end;

procedure TXMLDocument.Clear(const aFullClear: Boolean);
begin
  ClearNodes(aFullClear);
  fBlankDocumentNode := CreateNode(ntDocument, '', '');
end;

procedure TXMLDocument.ClearNodes(const aFullClear: Boolean);
var
  I: Integer;
  C: TXMLChildType;
begin
  fDictionary.Clear(aFullClear);

  fFreeNodes.Clear;
  fTempNameSpaceURIs.Clear;
  fNextNodeId := 0;

  if aFullClear then
  begin
    //clear nodes list
    fNodesLength := 0;
    for I := 0 to fNodes.Count-1 do
      Dispose(PXMLNodeArray(fNodes[I]));
    fNodes.Clear;
  end;

  //clear temp nodes
  fBlankDocumentNode := nil;
  fDummyNode := nil;

  //clear temp nodes list
  for C := Low(C) to High(C) do
    ClearTempChildNodeLists(C);
end;

procedure TXMLDocument.ClearTempChildNodeLists(const aChildType: TXMLChildType);
begin
  fTempChildNodes[aChildType].Clear;
end;

destructor TXMLDocument.Destroy;
var
  C: TXMLChildType;
begin
  ClearNodes(True);
  fDictionary.Free;
  fNodes.Free;
  fFreeNodes.Free;
  fTempNameSpaceURIs.Free;
  for C := Low(C) to High(C) do
    fTempChildNodes[C].Free;

  fWriterSettings.Free;
  fReaderSettings.Free;
  fTempAttributeIndex.Free;

  inherited;
end;

procedure TXMLDocument.DestroyTempChildNodeList(const aParentNode: PXMLNode;
  const aChildType: TXMLChildType);
begin
  if fTempChildNodes[aChildType].Count > 0 then
    fTempChildNodes[aChildType].Remove(aParentNode);
end;

procedure TXMLDocument.DoCreate;
var
  C: TXMLChildType;
begin
  fWriterSettings := TXMLDocumentWriterSettings.Create;
  fReaderSettings := TXMLReaderSettings.Create;

  fDictionary := TOHashedStrings.Create;
  fTempAttributeIndex := TXMLAttributeIndex.Create(Self);

  {$IFDEF O_GENERICS}
  fNodes := TList<PXMLNodeArray>.Create;
  fFreeNodes := TDictionary<PXMLNode,Boolean>.Create;
  fTempNameSpaceURIs := TDictionary<PXMLNode,OHashedStringsIndex>.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TObjectDictionary<PXMLNode,TXMLChildNodeList>.Create([doOwnsValues]);
  {$ELSE}
  fNodes := TList.Create;
  fFreeNodes := TODictionary.Create;
  fTempNameSpaceURIs := TODictionary.Create;
  for C := Low(C) to High(C) do
    fTempChildNodes[C] := TODictionary.Create(dupIgnore, soAscending, True);
  {$ENDIF}

  fWhiteSpaceHandling := wsPreserveInTextOnly;

  Clear;
end;

function TXMLDocument.FindXMLDeclarationNode(
  var outXMLDeclarationNode: PXMLNode): Boolean;
var
  xChild: PXMLNode;
begin
  if fBlankDocumentNode.HasChildNodes then
  begin
    xChild := nil;
    while fBlankDocumentNode.GetNextChild(xChild) do
    if (xChild.NodeType = ntXMLDeclaration)
    then begin
      outXMLDeclarationNode := xChild;
      Result := True;
      Exit;
    end;
  end;

  Result := False;
  outXMLDeclarationNode := nil;
end;

procedure TXMLDocument.FreeNode(const aNode: PXMLNode);
begin
  aNode.DeleteAttributes(True);
  aNode.DeleteChildren(True);
  aNode.fNodeNameId := -1;
  aNode.fNodeValueId := -1;
  aNode.fParentNode := nil;
  aNode.fNextSibling := nil;
  aNode.fPreviousSibling := nil;

  fFreeNodes.Add(aNode{$IFDEF O_GENERICS}, True{$ENDIF});
end;

function TXMLDocument.GetAbsoluteNodeCount: XMLNodeId;
begin
  Result := Int64(fNextNodeId) - Int64(fFreeNodes.Count);
  if Result > 0 then
    Dec(Result);
end;

function TXMLDocument.GetCodePage: Word;
var
  xEncodingAlias: OWideString;
begin
  xEncodingAlias := Encoding;

  if (xEncodingAlias <> '') then
    Result := TEncoding.AliasToCodePage(xEncodingAlias)
  else
    Result := 0;

  if Result = 0 then
    Result := CP_UTF_8;
end;

function TXMLDocument.GetXMLDeclarationAttribute(
  const aAttributeName: OWideString): OWideString;
var
  xDecNode: PXMLNode;
begin
  if FindXMLDeclarationNode(xDecNode{%H-}) then
    Result := xDecNode.GetAttribute(aAttributeName)
  else
    Result := '';
end;

function TXMLDocument.GetWriterSettings: TXMLDocumentWriterSettings;
begin
  Result := fWriterSettings;
end;

function TXMLDocument.GetDocumentElement: PXMLNode;
var
  xChild: PXMLNode;
begin
  xChild := nil;
  while fBlankDocumentNode.GetNextChild(xChild) do
  if xChild.NodeType = ntElement then
  begin
    Result := xChild;
    Exit;
  end;
  Result := nil;
end;

function TXMLDocument.GetDocumentNode: PXMLNode;
begin
  Result := fBlankDocumentNode;
end;

function TXMLDocument.GetEncoding: OWideString;
begin
  Result := GetXMLDeclarationAttribute('encoding');
end;

function TXMLDocument.GetLoading: Boolean;
begin
  Result := fLoading;
end;

function TXMLDocument.GetNodeById(const aNodeId: XMLNodeId): PXMLNode;
begin
  Result := @(PXMLNodeArray(fNodes[aNodeId shr 10])^)[aNodeId and 1023];//= [aNode div 1024][aNode mod 1024]
end;

function TXMLDocument.GetNodeId(const aNode: PXMLNode): XMLNodeId;
var
  I: Integer;
begin
  //search through all nodes in allocated memory
  for I := 0 to fNodes.Count-1 do
  begin
    if
      ({%H-}ONativeUInt(aNode) >= {%H-}ONativeUInt(@(PXMLNodeArray(fNodes[I])^[0]))) and
      ({%H-}ONativeUInt(aNode) <= {%H-}ONativeUInt(@(PXMLNodeArray(fNodes[I])^[1023])))//1024 nodes!
    then begin
      Result := ONativeUInt(I shl 10){I * 1024} + ({%H-}ONativeUInt(aNode) - {%H-}ONativeUInt(@(PXMLNodeArray(fNodes[I])^[0]))) div ONativeUInt(SizeOf(TXMLNode));
      Exit;
    end;
  end;

  //not found
  raise EXmlDOMException.Create(OXmlLng_NodeNotFound);
end;

function TXMLDocument.GetParseError: IOTextParseError;
begin
  Result := fParseError;
end;

function TXMLDocument.GetDummyNode: PXMLNode;
begin
  if not Assigned(fDummyNode) then
  begin
    fDummyNode := CreateNode(ntElement, '', '');
    fDummyNode.fParentNode := fBlankDocumentNode;
  end;
  Result := fDummyNode;
end;

function TXMLDocument.GetDummyResNodeList: IXMLNodeList;
begin
  if not Assigned(fDummyNodeList) then
    fDummyNodeList := TXMLResNodeList.Create
  else
    fDummyNodeList.Clear;

  Result := fDummyNodeList;
end;

function TXMLDocument.GetReaderSettings: TXMLReaderSettings;
begin
  Result := fReaderSettings;
end;

function TXMLDocument.GetStandAlone: OWideString;
begin
  Result := GetXMLDeclarationAttribute('standalone');
end;

function TXMLDocument.GetString(const aStringId: OHashedStringsIndex): OWideString;
begin
  if aStringId >= 0 then
    Result := fDictionary.Get(aStringId)
  else
    Result := '';
end;

function TXMLDocument.GetURL: OWideString;
begin
  Result := fURL;
end;

function TXMLDocument.TryGetTempChildNodeList(const aParentNode: PXMLNode;
  const aChildType: TXMLChildType; var outList: TXMLChildNodeList): Boolean;
begin
  if fTempChildNodes[aChildType].Count = 0 then
  begin
    Result := False;
    outList := nil;
  end else
  begin
    Result := fTempChildNodes[aChildType].TryGetValue(aParentNode, {$IFNDEF O_GENERICS}TObject{$ELSE}TXMLChildNodeList{$ENDIF}(outList));
  end;
end;

function TXMLDocument.GetVersion: OWideString;
begin
  Result := GetXMLDeclarationAttribute('version');
end;

function TXMLDocument.GetWhiteSpaceHandling: TXMLWhiteSpaceHandling;
begin
  Result := fWhiteSpaceHandling;
end;

function TXMLDocument.XML: OWideString;
begin
  Result := Node.XML;
end;

function TXMLDocument.XML(const aIndentType: TXMLIndentType): OWideString;
begin
  Result := Node.XML(aIndentType);
end;

function TXMLDocument.XML_UTF8: OUTF8Container;
begin
  Result := Node.XML_UTF8;
end;

function TXMLDocument.XML_UTF8(const aIndentType: TXMLIndentType): OUTF8Container;
begin
  Result := Node.XML_UTF8(aIndentType);
end;

procedure TXMLDocument.Grow;
var
  xNewArray: PXMLNodeArray;
begin
  fNodesLength := fNodesLength+1024;

  New(xNewArray);
  SetLength(xNewArray^, 1024);
  fNodes.Add(xNewArray);
end;

function TXMLDocument.IndexOfString(
  const aString: OWideString): OHashedStringsIndex;
begin
  Result := fDictionary.IndexOf(aString);
end;

function TXMLDocument.LoadFromBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromBuffer(aBuffer, aForceEncoding);
end;

function TXMLDocument.LoadFromBuffer(const aBuffer;
  const aBufferLength: Integer; const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromBuffer(aBuffer, aBufferLength, aForceEncoding);
end;

function TXMLDocument.LoadFromFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromFile(aFileName, aForceEncoding);
end;

function TXMLDocument.LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
begin
  Result := Node.LoadFromReader(aReader, outReaderToken);
end;

function TXMLDocument.LoadFromStream(const aStream: TStream;
  const aForceEncoding: TEncoding): Boolean;
begin
  Clear;
  Result := Node.LoadFromStream(aStream, aForceEncoding);
end;

function TXMLDocument.LoadFromXML(const aXML: OWideString): Boolean;
begin
  Clear;
  Result := Node.LoadFromXML(aXML);
end;

function TXMLDocument.LoadFromXML_UTF8(const aXML: OUTF8Container): Boolean;
begin
  Clear;
  Result := Node.LoadFromXML_UTF8(aXML);
end;

procedure TXMLDocument.SaveToBuffer(var outBuffer: TBytes);
begin
  Node.SaveToBuffer(outBuffer);
end;

{$IFDEF O_HASBYTESTRINGS}
procedure TXMLDocument.SaveToBuffer(var outBuffer: OUTF8Container);
begin
  Node.SaveToBuffer(outBuffer);
end;
{$ENDIF}

procedure TXMLDocument.SaveToFile(const aFileName: OWideString);
begin
  Node.SaveToFile(aFileName);
end;

procedure TXMLDocument.SaveToXML(var outXML: OWideString);
begin
  Node.SaveToXML(outXML);
end;

procedure TXMLDocument.SaveToXML(var outXML: OWideString; const aIndentType: TXMLIndentType);
begin
  Node.SaveToXML(outXML, aIndentType);
end;

procedure TXMLDocument.SaveToXML_UTF8(var outXML: OUTF8Container);
begin
  Node.SaveToXML_UTF8(outXML);
end;

procedure TXMLDocument.SaveToXML_UTF8(var outXML: OUTF8Container; const aIndentType: TXMLIndentType);
begin
  Node.SaveToXML_UTF8(outXML, aIndentType);
end;

procedure TXMLDocument.SaveToStream(const aStream: TStream);
begin
  Node.SaveToStream(aStream);
end;

procedure TXMLDocument.SaveToWriter(const aWriter: TXMLWriter);
begin
  Node.SaveToWriter(aWriter);
end;

procedure TXMLDocument.SetCodePage(const aCodePage: Word);
begin
  Encoding := TEncoding.CodePageToAlias(aCodePage);
end;

procedure TXMLDocument.SetDocumentElement(const aDocumentElement: PXMLNode);
var
  xChild, xNextChild: PXMLNode;
begin
  xChild := fBlankDocumentNode.FirstChild;
  while Assigned(xChild) do
  begin
    xNextChild := xChild.NextSibling;

    if xChild.NodeType = ntElement then
      xChild.DeleteSelf;

    xChild := xNextChild;
  end;

  fBlankDocumentNode.AppendChild(aDocumentElement);
end;

procedure TXMLDocument.SetXMLDeclarationAttribute(const aAttributeName,
  aAttributeValue: OWideString);
var
  xDecNode: PXMLNode;
begin
  if not FindXMLDeclarationNode(xDecNode{%H-}) then
  begin
    if fBlankDocumentNode.HasChildNodes then
      xDecNode := fBlankDocumentNode.InsertXMLDeclaration(fBlankDocumentNode.FirstChild)
    else
      xDecNode := fBlankDocumentNode.AddXMLDeclaration;
  end;

  xDecNode.AddAttribute(aAttributeName, aAttributeValue);
end;

procedure TXMLDocument.SetEncoding(const aEncoding: OWideString);
begin
  SetXMLDeclarationAttribute('encoding', aEncoding);
end;

procedure TXMLDocument.SetLoading(const aLoading: Boolean);
begin
  fLoading := aLoading;
end;

procedure TXMLDocument.SetStandAlone(const aStandAlone: OWideString);
begin
  SetXMLDeclarationAttribute('standalone', aStandAlone);
end;

function TXMLDocument.SetString(const aString: OWideString): OHashedStringsIndex;
begin
  Result := fDictionary.Add(aString);
end;

procedure TXMLDocument.SetURL(const aURL: OWideString);
begin
  fURL := aURL;
end;

procedure TXMLDocument.SetVersion(const aVersion: OWideString);
begin
  SetXMLDeclarationAttribute('version', aVersion);
end;

procedure TXMLDocument.SetWhiteSpaceHandling(
  const aWhiteSpaceHandling: TXMLWhiteSpaceHandling);
begin
  fWhiteSpaceHandling := aWhiteSpaceHandling;
end;

{ TXMLResNodeListEnumerator }

constructor TXMLResNodeListEnumerator.Create(aList: IXMLNodeList);
begin
  inherited Create;

  fList := aList;
  fIndex := -1;
end;

function TXMLResNodeListEnumerator.GetCurrent: PXMLNode;
begin
  Result := fList[fIndex];
end;

function TXMLResNodeListEnumerator.MoveNext: Boolean;
begin
  Result := (fIndex < fList.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TXMLResNodeList }

function TXMLResNodeList.Add(const aNode: PXMLNode): Integer;
begin
  Result := fList.Add(aNode);
end;

function TXMLResNodeList.AddNode(const aNode: PXMLNode): PXMLNode;
begin
  Add(aNode);
  Result := aNode;
end;

procedure TXMLResNodeList.Clear;
begin
  fList.Clear;
end;

constructor TXMLResNodeList.Create;
begin
  inherited Create;

  fList := TXMLNodeList.Create;
end;

procedure TXMLResNodeList.Delete(const aIndex: Integer);
begin
  if (aIndex >= 0) and  (aIndex < fList.Count) then
  begin
    fList.Delete(aIndex);
  end;
end;

procedure TXMLResNodeList.Delete(const aNode: PXMLNode);
var
  I: Integer;
begin
  I := IndexOf(aNode);
  if I >= 0 then
    Delete(I)
end;

destructor TXMLResNodeList.Destroy;
begin
  fList.Free;

  inherited;
end;

procedure TXMLResNodeList.Exchange(const aNode1, aNode2: PXMLNode);
begin
  Exchange(IndexOf(aNode1), IndexOf(aNode2));
end;

procedure TXMLResNodeList.Exchange(const aIndex1, aIndex2: Integer);
begin
  fList.Exchange(aIndex1, aIndex2);
end;

procedure TXMLResNodeList.Delete(const aName: OWideString);
var
  I: Integer;
begin
  I := IndexOf(aName);
  if I >= 0 then
    Delete(I)
end;

function TXMLResNodeList.FindNode(const aName: OWideString): PXMLNode;
begin
  if IndexOf(aName, Result{%H-}) < 0 then
    Result := nil;
end;

function TXMLResNodeList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXMLResNodeList.GetFirst: PXMLNode;
begin
  if Count > 0 then
    Result := Nodes[0]
  else
    Result := nil;
end;

function TXMLResNodeList.GetLast: PXMLNode;
begin
  if Count > 0 then
    Result := Nodes[Count-1]
  else
    Result := nil;
end;

{$IFDEF O_ENUMERATORS}
function TXMLResNodeList.GetEnumerator: TXMLResNodeListEnumerator;
begin
  Result := TXMLResNodeListEnumerator.Create(Self);
end;
{$ENDIF}

function TXMLResNodeList.GetNext(var ioNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(ioNodeEnum, +1);
end;

function TXMLResNodeList.Get(const aIndex: Integer): PXMLNode;
begin
  {$IFDEF O_GENERICS}
  Result := fList.Items[aIndex];
  {$ELSE}
  Result := PXMLNode(fList.Items[aIndex]);
  {$ENDIF}
end;

function TXMLResNodeList.GetPrevious(var ioNodeEnum: PXMLNode): Boolean;
begin
  Result := GetPrevNext(ioNodeEnum, -1);
end;

function TXMLResNodeList.GetPrevNext(var ioNodeEnum: PXMLNode;
  const aInc: Integer): Boolean;
var
  xCount: Integer;
begin
  //same code as TSAXAttributes.GetPrevNext
  Result := False;

  xCount := Count;
  if xCount = 0 then
  begin
    ioNodeEnum := nil;
    Exit;
  end;

  if Assigned(ioNodeEnum) then
  begin
    //get prev/next
    if not(
       (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount) and
       (Nodes[fIteratorCurrent] = ioNodeEnum))
    then//ioNodeEnum is NOT the last iterator -> we have to find it
      fIteratorCurrent := IndexOf(ioNodeEnum);

    if (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount)
    then begin
      fIteratorCurrent := fIteratorCurrent + aInc;
      Result := (0 <= fIteratorCurrent) and (fIteratorCurrent < xCount);
      if Result then
        ioNodeEnum := Nodes[fIteratorCurrent]
      else
        ioNodeEnum := nil;
    end;
  end else
  begin
    //return first or last element
    if aInc > 0 then
      fIteratorCurrent := 0
    else
      fIteratorCurrent := xCount-1;
    ioNodeEnum := Nodes[fIteratorCurrent];
    Result := True;
  end;
end;

function TXMLResNodeList.IndexOf(const aName: OWideString): Integer;
var x: PXMLNode;
begin
  Result := IndexOf(aName, x{%H-});
end;

function TXMLResNodeList.IndexOf(const aName: OWideString;
  var outNode: PXMLNode): Integer;
var
  xNameId: OHashedStringsIndex;
begin
  if Count = 0 then
  begin
    Result := -1;
    outNode := nil;
    Exit;
  end;

  xNameId := Nodes[0].fOwnerDocument.IndexOfString(aName);
  if xNameId < 0 then
  begin
    Result := -1;
    outNode := nil;
    Exit;
  end;

  for Result := 0 to Count-1 do
  if (Nodes[Result{%H-}].fNodeNameId = xNameId) then
  begin
    outNode := Nodes[Result];
    Exit;
  end;

  Result := -1;
  outNode := nil;
end;

function TXMLResNodeList.IndexOf(const aNode: PXMLNode): Integer;
begin
  Result := fList.IndexOf(aNode);
end;

procedure TXMLResNodeList.Insert(const aIndex: Integer; const aNode: PXMLNode);
begin
  fList.Insert(aIndex, aNode);
end;

procedure TXMLResNodeList.Move(const aCurIndex, aNewIndex: Integer);
begin
  fList.Move(aCurIndex, aNewIndex);
end;

function TXMLResNodeList.Remove(const aNode: PXMLNode): Integer;
begin
  Result := fList.Remove(aNode);
end;

{ TXMLXPathDOMAdapter }

procedure TXMLXPathDOMAdapter.AddNodeToResList(const aNode: TXMLXPathNode);
begin
  fResNodeList.Add(aNode);
end;

procedure TXMLXPathDOMAdapter.BuildIdTree(const aStartWithNode: TXMLXPathNode;
  const aLevelsDeep: Integer; const aAttributes: Boolean;
  const aIdTree: TXMLXPathIdTree);
var
  xId: XMLNodeId;

  procedure _ScanNode(const bNode: PXMLNode; const bLevelsDeepLeft: Integer);
  var
    xChild: PXMLNode;
  begin
    aIdTree.Add(TXMLXPathNode(bNode), xId);
    Inc(xId);

    if bLevelsDeepLeft < 0 then
      Exit;

    if aAttributes and bNode.HasAttributes then
    begin
      xChild := nil;
      while bNode.GetNextAttribute(xChild) do
      begin
        aIdTree.Add(TXMLXPathNode(xChild), xId);
        Inc(xId);
      end;
    end;

    if bNode.HasChildNodes then
    begin
      xChild := nil;
      while bNode.GetNextChild(xChild) do
      if xChild.NodeType in [ntElement, ntText, ntCData, ntEntityReference] then
        _ScanNode(xChild, bLevelsDeepLeft-1);
    end;
  end;
begin
  aIdTree.Clear;
  xId := 0;

  _ScanNode(PXMLNode(aStartWithNode), aLevelsDeep);
end;

constructor TXMLXPathDOMAdapter.Create(const aOwnerDocument: TXMLDocument);
begin
  inherited Create;

  fOwnerDocument := aOwnerDocument;
end;

function TXMLXPathDOMAdapter.CreateResNodeList: TXMLXPathNodeList;
begin
  if not Assigned(fResNodeList) then
    fResNodeList := TXMLResNodeList.Create;
  Result := TXMLXPathNodeList(fResNodeList as IXMLNodeList);
end;

procedure TXMLXPathDOMAdapter.GetNodeAttributes(
  const aParentNode: TXMLXPathNode; const aList: TXMLXPathResNodeList);
var
  xAttr: PXMLNode;
begin
  aList.Clear;
  xAttr := nil;
  while PXMLNode(aParentNode).GetNextAttribute(xAttr) do
    aList.Add(xAttr);
end;

procedure TXMLXPathDOMAdapter.GetNodeChildren(const aParentNode: TXMLXPathNode;
  const aList: TXMLXPathResNodeList);
var
  xChild: PXMLNode;
begin
  aList.Clear;
  xChild := nil;
  while PXMLNode(aParentNode).GetNextChild(xChild) do
    aList.Add(xChild);
end;

function TXMLXPathDOMAdapter.GetNodeDOMDocument(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := PXMLNode(aNode).OwnerDocument.fBlankDocumentNode;
end;

procedure TXMLXPathDOMAdapter.GetNodeInfo(const aNode: TXMLXPathNode;
  var outNodeInfo: TXMLXPathNodeInfo);
var
  xNode: PXMLNode;
begin
  xNode := PXMLNode(aNode);
  outNodeInfo.NodeNameId := xNode.fNodeNameId;
  outNodeInfo.NodeValueId := xNode.fNodeValueId;
  outNodeInfo.NodeType := xNode.NodeType;
end;

function TXMLXPathDOMAdapter.GetNodeNameId(
  const aNode: TXMLXPathNode): OHashedStringsIndex;
begin
  Result := PXMLNode(aNode).fNodeNameId;
end;

function TXMLXPathDOMAdapter.GetNodeParent(
  const aNode: TXMLXPathNode): TXMLXPathNode;
begin
  Result := PXMLNode(aNode).ParentNode;
end;

function TXMLXPathDOMAdapter.GetNodeType(
  const aNode: TXMLXPathNode): TXMLNodeType;
begin
  Result := PXMLNode(aNode).NodeType;
end;

function TXMLXPathDOMAdapter.GetNodeValueId(
  const aNode: TXMLXPathNode): OHashedStringsIndex;
begin
  Result := PXMLNode(aNode).fNodeValueId;
end;

function TXMLXPathDOMAdapter.GetStringId(
  const aString: OWideString): OHashedStringsIndex;
begin
  Result := fOwnerDocument.IndexOfString(aString);
end;

function TXMLXPathDOMAdapter.NodeFindAttribute(const aNode: TXMLXPathNode;
  const aAttrNameId: OHashedStringsIndex): TXMLXPathNode;
var
  xAttr: PXMLNode;
begin
  if PXMLNode(aNode).FindAttributeById(aAttrNameId, xAttr{%H-}) then
    Result := xAttr
  else
    Result := nil;
end;

function TXMLXPathDOMAdapter.NodeHasAttributes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := PXMLNode(aNode).HasAttributes;
end;

function TXMLXPathDOMAdapter.NodeHasChildNodes(
  const aNode: TXMLXPathNode): Boolean;
begin
  Result := PXMLNode(aNode).HasChildNodes;
end;

{ TXMLChildNodeList }

procedure TXMLChildNodeList.Delete(const aIndex: Integer);
var
  xNode: PXMLNode;
begin
  xNode := Nodes[aIndex];
  if Assigned(xNode) then
    Delete(xNode);
end;

procedure TXMLChildNodeList.ExchangeNodes(const aIndex1, aIndex2: Integer);
begin
  if (aIndex1 < 0) or (aIndex2 < 0) or
     (aIndex1 >= Count) or (aIndex2 >= Count) or
     (aIndex1 = aIndex2)
  then
    Exit;

  ExchangeNodes(Nodes[aIndex1], Nodes[aIndex2]);
end;

procedure TXMLChildNodeList.ExchangeNodes(const aNode1, aNode2: PXMLNode);
begin
  aNode1.ExchangeWithNode(aNode2);
end;

procedure TXMLChildNodeList.ExtNodeAppended;
begin
  Inc(fTempCount);
end;

procedure TXMLChildNodeList.ExtNodeRemoved;
begin
  Dec(fTempCount);
  ClearTempVariables;
end;

procedure TXMLChildNodeList.ExtNodeInserted;
begin
  Inc(fTempCount);
  ClearTempVariables;
end;

procedure TXMLChildNodeList.Delete(const aNode: PXMLNode);
begin
  aNode.DeleteSelf;
end;

procedure TXMLChildNodeList.Delete(const aName: OWideString);
var
  xNode: PXMLNode;
begin
  xNode := FindNode(aName);
  if Assigned(xNode) then
    Delete(xNode);
end;

function TXMLChildNodeList.FindNode(const aName: OWideString): PXMLNode;
begin
  FindNode(aName, Result{%H-});
end;

function TXMLChildNodeList.GetCount: Integer;
var
  xIter: PXMLNode;
begin
  if fTempCount >= 0 then
  begin
    Result := fTempCount;
  end else
  begin
    Result := 0;
    xIter := nil;
    while GetNext(xIter) do
      Inc(Result);
    fTempCount := Result;
  end;
end;

function TXMLChildNodeList.Add(const aNode: PXMLNode): Integer;
begin
  fParent.Append(aNode, fChildType);
  Result := Count-1;
end;

function TXMLChildNodeList.AddNode(const aNode: PXMLNode): PXMLNode;
begin
  fParent.Append(aNode, fChildType);
  Result := aNode;
end;

procedure TXMLChildNodeList.Clear;
begin
  if fChildType = ctAttribute then
    fParent.DeleteAttributes(False)
  else
    fParent.DeleteChildren(False);
end;

procedure TXMLChildNodeList.ClearTempVariables;
begin
  fLastGetNodeIndex := -1;
  fLastGetNode := nil;
  //do not clear fTempCount here
end;

constructor TXMLChildNodeList.Create(const aParent: PXMLNode; const aChildType: TXMLChildType);
begin
  inherited Create;

  fParent := aParent;
  fLastGetNodeIndex := -1;
  fChildType := aChildType;

  fTempCount := -1;
  GetCount;//load fTempCount -> must be here
end;

{$IFDEF O_ENUMERATORS}
function TXMLChildNodeList.GetEnumerator: TXMLChildNodeListEnumerator;
begin
  Result := TXMLChildNodeListEnumerator.Create(Self);
end;
{$ENDIF}

function TXMLChildNodeList.GetFirst: PXMLNode;
begin
  Result := fParent.fFirstCChild[fChildType];
end;

function TXMLChildNodeList.GetLast: PXMLNode;
begin
  Result := fParent.fLastCChild[fChildType];
end;

function TXMLChildNodeList.GetNext(var ioNode: PXMLNode): Boolean;
begin
  if ioNode = nil then
    ioNode := GetFirst
  else
    ioNode := ioNode.fNextSibling;
  Result := Assigned(ioNode);
end;

function TXMLChildNodeList.Get(const aIndex: Integer): PXMLNode;
var
  I: Integer;
begin
  if aIndex < 0 then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  Result := nil;
  if (fLastGetNodeIndex >= 0) and Assigned(fLastGetNode) and
     not ((aIndex < (fLastGetNodeIndex-aIndex)) or (aIndex < (aIndex-fLastGetNodeIndex))) and //performance -> search from the start if it needs less cycles
     not (((Count-aIndex) < (fLastGetNodeIndex-aIndex)) or ((Count-aIndex) < (aIndex-fLastGetNodeIndex)))//performance -> search from the end if it needs less cycles
  then begin
    if (aIndex = fLastGetNodeIndex) then
    begin
      //The same node
      Result := fLastGetNode;
    end else
    begin
      //you cannot run this code for (aIndex = fLastGetNodeIndex)!!!
      //find node as a relative sibling from fLastGetNode
      I := fLastGetNodeIndex;
      Result := fLastGetNode;
      while (I <> aIndex) and Assigned(Result) do
      begin
        if aIndex > fLastGetNodeIndex then
        begin
          //Next in list
          Result := Result.NextSibling;
          Inc(I);
        end else
        begin
          //Previous in list
          Result := Result.PreviousSibling;
          Dec(I);
        end;
      end;
    end;
  end;

  if not Assigned(Result) then
  begin
    if aIndex < Count div 2 then
    begin
      //search forwards through all nodes
      I := -1;
      while (I < aIndex) and GetNext(Result) do
        Inc(I);
    end else
    begin
      //search backwards through all nodes
      I := Count;
      while (I > aIndex) and GetPrevious(Result) do
        Dec(I);
    end;
    if I <> aIndex then
      raise EListError.Create(OXmlLng_ListIndexOutOfRange);
  end;

  if not Assigned(Result) then
    raise EListError.Create(OXmlLng_ListIndexOutOfRange);

  fLastGetNode := Result;
  fLastGetNodeIndex := aIndex;
end;

function TXMLChildNodeList.GetPrevious(var ioNode: PXMLNode): Boolean;
begin
  if ioNode = nil then
    ioNode := GetLast
  else
    ioNode := ioNode.fPreviousSibling;
  Result := Assigned(ioNode);
end;

function TXMLChildNodeList.IndexOf(const aNode: PXMLNode): Integer;
var
  xIter: PXMLNode;
begin
  Result := -1;
  xIter := nil;
  while (aNode <> xIter) and GetNext(xIter) do
    Inc(Result);

  if (aNode <> xIter) then
    Result := -1;
end;

function TXMLChildNodeList.FindNode(const aName: OWideString;
  var outNode: PXMLNode): Boolean;
begin
  case fChildType of
    ctChild: Result := fParent.FindChild(aName, outNode);
    ctAttribute: Result := fParent.FindAttribute(aName, outNode);
  else
    Result := False;//suppress DCC warning
  end;
end;

procedure TXMLChildNodeList.Insert(const aIndex: Integer;
  const aNode: PXMLNode);
var
  xNode: PXMLNode;
begin
  xNode := Nodes[aIndex];
  fParent.Insert(aNode, xNode, fChildType);
end;

function TXMLChildNodeList.Remove(const aName: OWideString;
  var outNode: PXMLNode): Boolean;
begin
  Result := FindNode(aName, outNode);
  if Result then
    outNode.RemoveSelfFromParent;
end;

function TXMLChildNodeList.Remove(const aNode: PXMLNode): Integer;
begin
  Result := IndexOf(aNode);
  if Result >= 0 then
    aNode.DeleteSelf;
end;

{ TXMLChildNodeListEnumerator }

constructor TXMLChildNodeListEnumerator.Create(aList: TXMLChildNodeList);
begin
  inherited Create;

  fList := aList;
  fCurrent := nil;
end;

function TXMLChildNodeListEnumerator.GetCurrent: PXMLNode;
begin
  Result := fCurrent;
end;

function TXMLChildNodeListEnumerator.MoveNext: Boolean;
begin
  if Assigned(fCurrent) then
    fCurrent := fCurrent.NextSibling
  else
    fCurrent := fList.GetFirst;

  Result := Assigned(fCurrent);
end;

{ TXMLAttributeIndex }

procedure TXMLAttributeIndex.Clear;
begin
  fIndex.Clear;
  fParentElement := nil;
end;

constructor TXMLAttributeIndex.Create(const aOwnerDocument: TXMLDocument);
begin
  inherited Create;

  fOwnerDocument := aOwnerDocument;
  fIndex := TXMLNodeIndex.Create;
end;

destructor TXMLAttributeIndex.Destroy;
begin
  fIndex.Free;

  inherited;
end;

procedure TXMLAttributeIndex.ExtAttributeAdded(const aAttributeNode: PXMLNode);
begin
  fIndex.Add(aAttributeNode.fNodeNameId, aAttributeNode);
end;

procedure TXMLAttributeIndex.ExtAttributeInserted;
begin
  Clear;
end;

procedure TXMLAttributeIndex.ExtAttributeRemoved;
begin
  Clear;
end;

function TXMLAttributeIndex.FindAttribute(const aNameId: OHashedStringsIndex;
  var outAttr: PXMLNode): Boolean;
begin
  Result := fIndex.TryGetValue(aNameId, {$IFNDEF O_GENERICS}Pointer{$ELSE}PXMLNode{$ENDIF}(outAttr));
end;

procedure TXMLAttributeIndex.SetParentElement(const aParentElement: PXMLNode);
var
  xAttrIter: PXMLNode;
begin
  if fParentElement = aParentElement then
    Exit;

  Clear;
  fParentElement := aParentElement;

  if not Assigned(aParentElement) then
    Exit;

  xAttrIter := aParentElement.FirstAttribute;
  while Assigned(xAttrIter) do
  begin
    ExtAttributeAdded(xAttrIter);

    xAttrIter := xAttrIter.NextSibling;
  end;
end;

end.
