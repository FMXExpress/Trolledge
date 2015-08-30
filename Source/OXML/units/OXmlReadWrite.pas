unit OXmlReadWrite;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlReadWrite.pas

  Basic XML reader/writer. OXmlIntfDOM.pas, OXmlPDOM.pas and OXmlSAX.pas use
  this unit to read and write XML.

  TXMLWriter
    - fast sequential XML writer
    - no real DOM validity checking - the programmer has to know what he is doing
    - supports escaping of text - you should pass unescaped text to every function
      (if not stated differently) and the writer takes care of valid XML escaping
    - all line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess
    - supports automatic indentation of XML


  TXMLReader
    - fast sequential XML reader/parser
    - the nodes are returned as they are found in the document
    - absolutely no whitespace handling - the document is parsed exactly 1:1
      -> white space is preserved also in the very beginning of the document
      -> you have to care for white space handling in end-level
    - only line breaks (#10, #13, #13#10) are automatically changed to LineBreak
      -> default value for LineBreak is your OS line break (sLineBreak)
      -> if you don't want to process them, set LineBreak to lbDoNotProcess
    - don't use it directly. If performance is crucial for you, use SAX
      which has the same performance.

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

  OHashedStrings,

  OWideSupp, OXmlUtils, OTextReadWrite, OEncoding;

type
  TXMLWriter = class;

  TXMLWriterElementMode = (
    stOpenOnly,    //"<node"
    stFinish,       //"<node>"
    stFinishClose); //"<node/>"

  TXMLWriterElement = {$IFDEF O_EXTRECORDS}record{$ELSE}object{$ENDIF}
  private
    {$IFDEF O_ARC}[Weak]{$ENDIF}
    fOwner: TXMLWriter;
    fElementName: OWideString;
    fOpenElementFinished: Boolean;
  public
    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElementR(const aElementName: OWideString;
      var outElement: TXMLWriterElement;
      const aMode: TXMLWriterElementMode = stOpenOnly); overload;
    function OpenElementR(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement; overload;

    //"/>" if not already written
    procedure FinishOpenElement;

    // </fElementName> or "/>" if no children were written
    procedure CloseElement(const aIndent: Boolean = True);

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);

    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);

    // write escaped text, do not escape quotes
    procedure Text(const aText: OWideString; const aIndent: Boolean = True);
  public
    property ElementName: OWideString read fElementName;
  end;

  TCustomXMLWriterSettings = class(TPersistent)
  private
    fIndentString: OWideString;
    fIndentType: TXMLIndentType;
    fLineBreak: TXMLLineBreak;
    fUseTabCRLFEntitiesInAttributes: Boolean;
    fUseGreaterThanEntity: Boolean;
    fStrictXML: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  protected
    function GetWriteBOM: Boolean; virtual; abstract;
    procedure SetWriteBOM(const aWriteBOM: Boolean); virtual; abstract;
  public
    constructor Create;
  public
    //write BOM (if applicable)
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;

    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is lbLF (#10) -> according to XML specification
    property LineBreak: TXMLLineBreak read fLineBreak write fLineBreak;

    //should be #9, #10 and #13 in attributes saved as entities? Default = True
    property UseTabCRLFEntitiesInAttributes: Boolean read fUseTabCRLFEntitiesInAttributes write fUseTabCRLFEntitiesInAttributes;
    //should the '>' character be saved as an entity? (it's completely fine according to XML spec to use the '>' character in text; Default = True
    property UseGreaterThanEntity: Boolean read fUseGreaterThanEntity write fUseGreaterThanEntity;

    //StrictXML: document must be valid XML
    //   = true: element names & values checking
    //   = false: no element names & values checking
    property StrictXML: Boolean read fStrictXML write fStrictXML;

    //indent type - default is none (no indent)
    property IndentType: TXMLIndentType read fIndentType write fIndentType;
    //indent string - default are two space characters (#32#32)
    property IndentString: OWideString read fIndentString write fIndentString;
  end;

  TXMLDocumentWriterSettings = class(TCustomXMLWriterSettings)
  private
    fWriteBOM: Boolean;
  protected
    function GetWriteBOM: Boolean; override;
    procedure SetWriteBOM(const aWriteBOM: Boolean); override;
  public
    constructor Create;
  end;

  TXMLWriterSettings = class(TCustomXMLWriterSettings)
  private
    {$IFDEF O_ARC}[Weak]{$ENDIF}
    fOwner: TXMLWriter;

    function GetEncoding: TEncoding;
    function GetOwnsEncoding: Boolean;
    procedure SetEncoding(const aEncoding: TEncoding);
    procedure SetOwnsEncoding(const aOwnsEncoding: Boolean);
  protected
    function GetWriteBOM: Boolean; override;
    procedure SetWriteBOM(const aWriteBOM: Boolean); override;
  public
    constructor Create(aOwner: TXMLWriter);
  public
    //Output encoding
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
  end;

  TOCharTable = class(TObject)
  private
    fIsSupported: array[OUnicodeChar] of Boolean;
  public
    constructor Create(const aEncoding: TEncoding); virtual;
  public
    function IsSupported(const aChar: OUnicodeChar): Boolean;
    function Entity(const aChar: OUnicodeChar): OWideString;
  end;

  TXMLWriter = class(TObject)
  private
    fWriter: TOTextWriter;
    fWriterSettings: TXMLWriterSettings;
    fDefaultIndentLevel: Integer;
    fWritten: Boolean;

    fIndentLevel: Integer;

    function GetEncoding: TEncoding;
    function GetOwnsEncoding: Boolean;
    function GetWriteBOM: Boolean;
    procedure SetEncoding(const aEncoding: TEncoding);
    procedure SetOwnsEncoding(const aOwnsEncoding: Boolean);
    procedure SetWriteBOM(const aWriteBOM: Boolean);
    procedure SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
  private
    //encoding entities support
    fUseSafeEntities: Boolean;
    fCharTable: TOCharTable;
    function GetCharTable: TOCharTable;
    // safe write raw text: if char not present in target encoding, write its entity
    procedure _DoTextSafeEntity(const aText: OUnicodeString; const aInAttributeValue: Boolean);
    procedure RawUnicodeChar(const aChar: OUnicodeChar);
    procedure RawUnicodeCharSafeEntity(const aChar: OUnicodeChar);
    //no safe entity
    procedure _DoText(const aText: OWideString; const aInAttributeValue: Boolean);
    procedure DoText(const aText: OWideString; const aInAttributeValue: Boolean);
  protected
    //manual indentation support - you can use Indent+IncIndentLevel+DecIndentLevel
    //  manually if you want to. Set IndentType to itNone in this case.
    procedure Indent;
    procedure IncIndentLevel;
    procedure DecIndentLevel;
  protected
    procedure DoCreate;
    procedure DoInit; virtual;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a document for writing.
    // Please note that the file/stream/... is locked until you destroy
    // TXMLWriter or call ReleaseDocument!

    procedure InitFile(const aFileName: OWideString);
    procedure InitStream(const aStream: TStream; const aOwnsStream: Boolean = False);

    //Release the current document (that was loaded with Init*)
    procedure ReleaseDocument;
  public
    // default <?xml ?> declaration
    procedure XMLDeclaration(
      const aEncoding: Boolean = True;
      const aVersion: OWideString = '1.0';
      const aStandAlone: OWideString = '');
    // <?xml
    procedure OpenXMLDeclaration;
    // ?>
    procedure FinishOpenXMLDeclaration;

    // <aElementName ... >, <aElementName ... /> etc.
    procedure OpenElement(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly);
    procedure OpenElementR(const aElementName: OWideString;
      var outElement: TXMLWriterElement;
      const aMode: TXMLWriterElementMode = stOpenOnly); overload;
    function OpenElementR(const aElementName: OWideString;
      const aMode: TXMLWriterElementMode = stOpenOnly): TXMLWriterElement; overload;
    // >
    procedure FinishOpenElement(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // />
    procedure FinishOpenElementClose(const {%H-}aElementName: OWideString = '');//you may pass a ElementName just to make it clear for you which element you want to close
    // </aElementName>, decide if you want to indent
    procedure CloseElement(const aElementName: OWideString; const aIndent: Boolean = True);

    // write attribute of an element or declaration
    procedure Attribute(const aAttrName, aAttrValue: OWideString);
    // &aEntityName;
    procedure EntityReference(const aEntityName: OWideString);
    // <![CDATA[aText]]>
    procedure CData(const aText: OWideString; const aIndent: Boolean = True);
    // <!--aText-->
    procedure Comment(const aText: OWideString);
    // <?aTarget aContent?>
    procedure ProcessingInstruction(const aTarget, aContent: OWideString);
    // <!DOCTYPE aDocTypeRawText> - aDocTypeRawText must be escaped, it won't be processed
    procedure DocType(const aDocTypeRawText: OWideString);

    // write escaped text, decide if you want to indent;
    procedure Text(const aText: OWideString; const aIndent: Boolean = True); overload;
    // write raw text, do not process it
    procedure RawText(const aText: OWideString);
    procedure RawChar(const aChar: OWideChar);
  public
    //encoding of the text writer
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    //should BOM be written
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;

    //indentation level - you can change it only if nothing has been written yet
    //  and after the Init* call
    property DefaultIndentLevel: Integer read fDefaultIndentLevel write SetDefaultIndentLevel;

    property WriterSettings: TXMLWriterSettings read fWriterSettings;
  end;

  TXMLWriterDeclaration = class(TXMLDeclaration)
  private
    fEnabled: Boolean;
  public
    constructor Create; override;
  public
    procedure Write(const aWriter: TXMLWriter);
    procedure WriteIfEnabled(const aWriter: TXMLWriter);
  public
    //Enabled -> set to true if XML declaration should be written
    property Enabled: Boolean read fEnabled write fEnabled;
  end;

  TXMLReaderTokenType = (
    rtDocumentStart,//start of reading
    rtOpenXMLDeclaration,//xml declaration open element: <?xml
    rtXMLDeclarationAttribute,//attribute in an xml declaration: name="value"
    rtFinishXMLDeclarationClose,//xml declaration element finished and closed: ?>
    rtOpenElement,//open element: <name
    rtAttribute,//attribute: name="value"
    rtFinishOpenElement,//open element finished but not closed: <node ... ">"
    rtFinishOpenElementClose,//open element finished and closed: <node ... "/>"
    rtCloseElement,//close element: "</node>"
    rtText,//text: value
    rtEntityReference,//&name; (value = the dereferenced entity value)
    rtCData,//cdata: <![CDATA[value]]>
    rtComment,//comment: <!--value-->
    rtProcessingInstruction,//custom processing instruction: <?target content?>
    rtDocType//doctype: <!DOCTYPE value> -> value is not unescaped by reader!!!
    );

  TXMLReaderToken = packed record
    TokenName: OWideString;
    TokenValue: OWideString;
    TokenType: TXMLReaderTokenType;
  end;
  PXMLReaderToken = ^TXMLReaderToken;
  TXMLReaderTokenList = class(TObject)
  private
    {$IFDEF O_GENERICS}
    fReaderTokens: TList<PXMLReaderToken>;
    {$ELSE}
    fReaderTokens: TList;
    {$ENDIF}

    fCount: Integer;

    function GetToken(const aIndex: Integer): PXMLReaderToken;

    procedure AddLast;
    function CreateNew: PXMLReaderToken;
  public
    procedure Clear;
    procedure DeleteLast;

    function IndexOf(const aToken: PXMLReaderToken): Integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Count: Integer read fCount;
    property Items[const aIndex: Integer]: PXMLReaderToken read GetToken; default;
  end;

  TXMLReader = class;
  TXMLReaderSettings = class;

  TXMLReaderEntityList = class(TPersistent)
  private
    fList: TOHashedStringDictionary;

    function GetCount: Integer;
    function GetItem(const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair;
    function GetName(const aIndex: OHashedStringsIndex): OWideString;
    function GetValue(const aIndex: OHashedStringsIndex): OWideString;
    procedure SetValue(const aIndex: OHashedStringsIndex;
      const aValue: OWideString);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure AddOrReplace(const aEntityName, aEntityValue: OWideString);
    function IndexOf(const aEntityName: OWideString): Integer;
    function Find(const aEntityName: OWideString; var outEntityValue: OWideString): Boolean;

    procedure Clear;//clear list and fill default XML entities
  public
    property EntityNames[const aIndex: OHashedStringsIndex]: OWideString read GetName;
    property EntityValues[const aIndex: OHashedStringsIndex]: OWideString read GetValue write SetValue;
    property Items[const aIndex: OHashedStringsIndex]: TOHashedStringDictionaryPair read GetItem;
    property Count: Integer read GetCount;
  end;

  TXMLReaderSettings = class(TPersistent)
  private
    fBreakReading: TXMLBreakReading;
    fLineBreak: TXMLLineBreak;
    fStrictXML: Boolean;
    fEntityList: TXMLReaderEntityList;
    fRecognizeXMLDeclaration: Boolean;
    fExpandEntities: Boolean;
    fErrorHandling: TOTextReaderErrorHandling;
  protected
    procedure LoadDTD(const aDTDReader: TOTextReader; const aIsInnerDTD: Boolean);
    procedure LoadDTDEntity(const aDTDReader: TOTextReader;
      const aBuffer1, aBuffer2: TOByteBuffer);
    procedure LoadDTDEntityReference(const aDTDReader: TOTextReader;
      const aWriteToBuffer, aTempBuffer: TOByteBuffer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    //Load external DTD
    //  DTD validation is NOT supported! Only defined entities will be read.

    function LoadDTDFromFile(const aFileName: OWideString; const aDefaultEncoding: TEncoding = nil): Boolean;
    function LoadDTDFromStream(const aStream: TStream; const aDefaultEncoding: TEncoding = nil): Boolean;
    //loads XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function LoadDTDFromString(const aString: OWideString): Boolean;
    function LoadDTDFromString_UTF8(const aString: OUTF8Container): Boolean;
    function LoadFromBuffer(const aBuffer: TBytes; const aDefaultEncoding: TEncoding = nil): Boolean; overload;
    function LoadFromBuffer(const aBuffer; const aBufferLength: Integer; const aDefaultEncoding: TEncoding = nil): Boolean; overload;
  public
    //process known entities. add user-defined entities here
    property EntityList: TXMLReaderEntityList read fEntityList;
    //decide if you want to read the document after the root element has been closed
    property BreakReading: TXMLBreakReading read fBreakReading write fBreakReading;
    //process line breaks (#10, #13, #13#10) to the LineBreak character
    //  set to lbDoNotProcess if you don't want any processing
    //  default is your OS line break (XmlDefaultLineBreak)
    property LineBreak: TXMLLineBreak read fLineBreak write fLineBreak;
    //StrictXML: document must be valid XML
    //   = true: raise Exceptions when document is not valid
    //   = false: try to fix and go over document errors.
    property StrictXML: Boolean read fStrictXML write fStrictXML;
    //RecognizeXMLDeclaration
    //  if set to true the processing instruction "<?xml ... ?>" will be detected as XMLDeclaration
    //   and following element types will be fired:
    //   rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtFinishXMLDeclarationClose
    //  if set to false, it will be handled as a normal processing instruction
    //   rtProcessingInstruction
    property RecognizeXMLDeclaration: Boolean read fRecognizeXMLDeclaration write fRecognizeXMLDeclaration;
    //ExpandEntities
    //  true: entities will be directly expanded to text
    //  false: entities will be detected as separate tokens
    //    Please note that not expanding entity references may cause text content
    //    problems in the DOM (in combination with normalization and/or automatic whitespace handling).
    //    Therefore when not expanding entities, always use "wsPreserveAll" for WhiteSpaceHandling in the DOM
    property ExpandEntities: Boolean read fExpandEntities write fExpandEntities;

    //ErrorHandling
    //  determine if Exception should be raised
    //  ehSilent: do not raise any exceptions (check TXMLReader.ParseError for an error
    //    -> this is the standard behaviour of MSXML
    //  ehRaiseAndEat: an exception will be raised but it will be eaten automatically
    //    so that the user doesn't see it it appears only when debugging and it is available to error handling/logging
    //    -> this is the standard behaviour of OmniXML
    //    -> this is also the default behaviour of OXml
    //  ehRaise: an exception will be raised and the end-user sees it as well
    //    -> this is the standard behaviour of Delphi's own TXmlDocument
    //  ! if an error was found, the TXMLReader.ParseError will contain it in any case !
    property ErrorHandling: TOTextReaderErrorHandling read fErrorHandling write fErrorHandling;
  end;

  TXMLReader = class(TObject)
  private
    fReaderSettings: TXMLReaderSettings;

    fAttributeTokens: TXMLReaderTokenList;//do not destroy!!!
    fOpenElementTokens: TXMLReaderTokenList;//destroy

    fReader: TOTextReader;
    fAllowSetEncodingFromFile: Boolean;//internal, for external use ForceEncoding

    fLastTokenType: TXMLReaderTokenType;//must be used because fReaderToken can be replaced! -> do not replace it with fReaderToken.TokenType!!!
    fDocumentElementFound: Boolean;
    fForceEncoding: Boolean;

    fElementsToClose: Integer;
    fReaderToken: PXMLReaderToken;//current reader
    fOwnsReaderToken: Boolean;

    fMainBuffer: TOByteBuffer;
    fEntityBuffer: TOByteBuffer;

    function GetEncoding: TEncoding;
    procedure SetEncoding(const aEncoding: TEncoding);
    function GetOwnsEncoding: Boolean;
    procedure SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
    function GetApproxStreamPosition: OStreamInt;
    function GetStreamSize: OStreamInt;
    function GetParseError: IOTextParseError;
    function GetURL: OWideString;
    procedure SetURL(const aURL: OWideString);
  private
    function GetNodePath(const aIndex: Integer): OWideString;
    function GetNodePathCount: Integer;
  private
    procedure EntityReferenceInText;
    procedure EntityReferenceStandalone;
    procedure ChangeEncoding(const aEncodingAlias: OWideString);
    procedure OpenElement;
    procedure Attribute;
    procedure FinishOpenElement;
    procedure FinishOpenElementClose;
    procedure CloseElement;
    procedure Text(const aClearCustomBuffer: Boolean = True);

    procedure ExclamationNode(const aTokenType: TXMLReaderTokenType;
      const aBeginTag, aEndTag: OWideString;
      const aWhiteSpaceAfterBeginTag, aIsDoctype: Boolean);
    procedure CData;
    procedure Comment;
    procedure DocType;
    procedure ProcessingInstruction;
    function GetFilePosition: OStreamInt;
    function GetLinePosition: OStreamInt;
    function GetLine: OStreamInt;
    procedure RaiseException(const aErrorClass: TOTextParseErrorClass;
      const aReason: string);
    procedure RaiseExceptionFmt(const aErrorClass: TOTextParseErrorClass;
      const aReason: string; const aArgs: array of OWideString);
  private
    procedure LoadDTD;
  protected
    procedure DoCreate;
    procedure DoDestroy;
    procedure DoInit(const aForceEncoding: TEncoding);
  protected
    procedure RemoveLastFromNodePath(const aCheckPath: Boolean);
    function NodePathIsEmpty: Boolean;
  public
    //create
    constructor Create; overload;
    //create and init
    constructor Create(const aStream: TStream; const aForceEncoding: TEncoding = nil); overload;

    destructor Destroy; override;
  public
    //The Init* procedures initialize a XML document for parsing.
    // Please note that the file/stream/... is locked until the end of the
    // document is reached, you destroy TXMLReader or you call ReleaseDocument!

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
    //use ReadNextToken for reading next XML token
    function ReadNextToken(var outToken: PXMLReaderToken): Boolean;

    //set custom attribute tokens
    procedure SetAttributeTokens(const aAttributeTokens: TXMLReaderTokenList);

    //reset document element -> use if you want to reset the information that the document element has been already read
    procedure ResetDocumentElement;
  public
    //following are functions to work with the current path in the XML document
    function NodePathMatch(const aNodePath: OWideString): Boolean; overload;
    function NodePathMatch(const aNodePath: TOWideStringList): Boolean; overload;
    function NodePathMatch(const aNodePath: Array of OWideString): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsChildOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: TOWideStringList): Boolean; overload;
    function RefIsParentOfNodePath(const aRefNodePath: Array of OWideString): Boolean; overload;
    procedure NodePathAssignTo(const aNodePath: TOWideStringList);
    function NodePathAsString: OWideString;

    //current path in XML document
    property NodePath[const aIndex: Integer]: OWideString read GetNodePath;
    //count of elements in path
    property NodePathCount: Integer read GetNodePathCount;
  public
    //(optional) URL parameter that will be shown if an error occurs
    property URL: OWideString read GetURL write SetURL;

    //encoding of the text file, when set, the file will be read again from the start
    property Encoding: TEncoding read GetEncoding write SetEncoding;
    property OwnsEncoding: Boolean read GetOwnsEncoding write SetOwnsEncoding;
    //if set to true, the encoding will not be changed automatically when
    //  <?xml encoding="..."?> is found
    property ForceEncoding: Boolean read fForceEncoding write fForceEncoding;

    //Reader Settings
    property ReaderSettings: TXMLReaderSettings read fReaderSettings;
  public
    //Approximate position in original read stream
    //  exact position cannot be determined because of variable UTF-8 character lengths
    property ApproxStreamPosition: OStreamInt read GetApproxStreamPosition;
    //size of original stream
    property StreamSize: OStreamInt read GetStreamSize;

    //Character position in text
    //  -> in Lazarus, the position is always in UTF-8 characters (no way to go around that since Lazarus uses UTF-8).
    //  -> in Delphi the position is always correct
    property FilePosition: OStreamInt read GetFilePosition;//absolute character position in file (in character units, not bytes), 1-based
    property LinePosition: OStreamInt read GetLinePosition;//current character in line, 1-based
    property Line: OStreamInt read GetLine;//current line, 1-based

    property ParseError: IOTextParseError read GetParseError;
  end;

  EXmlWriterException = class(Exception);
  EXmlWriterInvalidString = class(EXmlWriterException);

  XMLNodeId = ONativeUInt;

  ICustomXMLDocument = interface
    ['{7FA934A1-B42A-4D68-8B59-E33818836D6E}']

  //protected
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
    function GetWhiteSpaceHandling: TXmlWhiteSpaceHandling;
    procedure SetWhiteSpaceHandling(const aWhiteSpaceHandling: TXmlWhiteSpaceHandling);
    function GetWriterSettings: TXMLDocumentWriterSettings;
    function GetReaderSettings: TXMLReaderSettings;
    function GetAbsoluteNodeCount: XMLNodeId;

  //public
    function IndexOfString(const aString: OWideString): OHashedStringsIndex;
    function GetString(const aStringId: OHashedStringsIndex): OWideString;

  //public
    //clear the whole document
    procedure Clear(const aFullClear: Boolean = True);

    //load document with custom reader
    //  return false if the XML document is invalid
    function LoadFromReader(const aReader: TXMLReader; var outReaderToken: PXMLReaderToken): Boolean;
    //load document from file in encoding specified by the document
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

  //public
    //returns XML in default unicode encoding: UTF-16 for DELPHI, UTF-8 for FPC
    function XML: OWideString; overload;
    function XML(const aIndentType: TXMLIndentType): OWideString; overload;
    function XML_UTF8: OUTF8Container; overload;
    function XML_UTF8(const aIndentType: TXMLIndentType): OUTF8Container; overload;

  //public
    // aURL: this URL is shown in an error message when reading (optional) - set it before you call LoadFrom*
    property URL: OWideString read GetURL write SetURL;

    //document whitespace handling
    property WhiteSpaceHandling: TXmlWhiteSpaceHandling read GetWhiteSpaceHandling write SetWhiteSpaceHandling;

    //document encoding (as integer identifier) - from <?xml encoding="???"?>
    property CodePage: Word read GetCodePage write SetCodePage;
    //document encoding (as string alias) - from <?xml encoding="???"?>
    property Encoding: OWideString read GetEncoding write SetEncoding;
    //document standalone - from <?xml standalone="???"?>
    property StandAlone: OWideString read GetStandAlone write SetStandAlone;
    //document version - from <?xml version="???"?>
    property Version: OWideString read GetVersion write SetVersion;

    //XML writer settings
    property WriterSettings: TXMLDocumentWriterSettings read GetWriterSettings;
    //XML reader settings
    property ReaderSettings: TXMLReaderSettings read GetReaderSettings;

    //ParseError has information about the error that occured when parsing a document
    function GetParseError: IOTextParseError;
    property ParseError: IOTextParseError read GetParseError;

    //get node count that are created in the document - also those that are created but don't have any parent!
    property AbsoluteNodeCount: XMLNodeId read GetAbsoluteNodeCount;
  end;

  EXMLReaderInvalidCharacter = class(EOTextReaderException)
  public
    class function GetErrorCode: Integer; override;
  end;
  EXMLReaderInvalidStructure = class(EOTextReaderException)
  public
    class function GetErrorCode: Integer; override;
  end;

  TXMLParseErrorInvalidCharacter = class(TOTextParseError)
    function GetExceptionClass: EOTextReaderExceptionClass; override;
  end;

  TXMLParseErrorInvalidStructure = class(TOTextParseError)
    function GetExceptionClass: EOTextReaderExceptionClass; override;
  end;


implementation

uses OXmlLng;

procedure ProcessNewLineChar(
  const aLastChar: OWideChar;
  const aReaderSettings: TXMLReaderSettings;
  const aCustomReader: TOTextReader; const aCustomBuffer: TOByteBuffer);
var
  xC: OWideChar;
begin
  if aReaderSettings.fLineBreak <> lbDoNotProcess then
  begin
    if aLastChar = #13 then
    begin
      //search for #13#10 sequence
      if (aCustomReader.ReadNextChar(xC{%H-})) and (xC <> #10) then
        aCustomReader.UndoRead;
    end;

    aCustomBuffer.WriteOWideString(XmlLineBreak[aReaderSettings.fLineBreak]);
  end else
    aCustomBuffer.WriteOWideChar(aLastChar);
end;

{$IFDEF O_DELPHI_5_DOWN}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  xError: Integer;
begin
  Val(S, Value, xError);
  Result := (xError = 0);
end;
{$ENDIF}

function ProcessEntity(
  const aReaderSettings: TXMLReaderSettings;
  const aCustomReader: TOTextReader;
  const aCustomBuffer: TOByteBuffer;
  var outReadString, outEntityName, outEntityValue: OWideString): Boolean;
const
  cEntityPrefix: array[0..2] of OWideString = ('', '#', '#x');
  cEntityText = 0;
  cEntityDec = 1;
  cEntityHex = 2;
var
  xEntityType: Byte;
  xReaderStartPos: Integer;

  procedure _SetReadString;
  begin
    outReadString := '&'+aCustomReader.ReadPreviousString(aCustomReader.TempStringPosition - xReaderStartPos);
    aCustomReader.UnblockFlushTempBuffer;
  end;

  procedure _EntityError;
  begin
    outEntityName := cEntityPrefix[xEntityType]+outEntityName;
    Result := False;
    _SetReadString;
  end;
var
  xC: OWideChar;
  xOutputChar: Integer;
  xIsHex: Boolean;
begin
  Result := False;
  xOutputChar := -1;
  aCustomBuffer.Clear(False);
  aCustomReader.BlockFlushTempBuffer;
  xReaderStartPos := aCustomReader.TempStringPosition;

  outEntityName := '';
  outEntityValue := '';

  aCustomReader.ReadNextChar(xC{%H-});
  if xC = '#' then
  begin
    //integer decimal/hexadecimal entity

    aCustomReader.ReadNextChar(xC);
    xIsHex := (xC = 'x');
    if xIsHex then
    begin
      xEntityType := cEntityHex;

      aCustomReader.ReadNextChar(xC);
      while OXmlIsHexadecimalChar(xC) do
      begin
        aCustomBuffer.WriteOWideChar(xC);
        aCustomReader.ReadNextChar(xC);
      end;

      aCustomBuffer.GetOWideString(outEntityName);

      if (xC <> ';') or not TryStrToInt('$'+outEntityName, xOutputChar) then
      begin
        if (xC <> ';') then
          aCustomReader.UndoRead;
        _EntityError;
        Exit;
      end;
    end else
    begin
      xEntityType := cEntityDec;

      while OXmlIsDecimalChar(xC) do
      begin
        aCustomBuffer.WriteOWideChar(xC);
        aCustomReader.ReadNextChar(xC);
      end;

      aCustomBuffer.GetOWideString(outEntityName);

      if (xC <> ';') or not TryStrToInt(outEntityName, xOutputChar) then
      begin
        if (xC <> ';') then
          aCustomReader.UndoRead;
        _EntityError;
        Exit;
      end;
    end;

  end else
  begin
    //TEXT entity
    xEntityType := cEntityText;

    if not OXmlIsNameStartChar(xC) then
    begin
      aCustomReader.UndoRead;
      _EntityError;
      Exit;
    end;

    while OXmlIsNameChar(xC) do
    begin
      aCustomBuffer.WriteOWideChar(xC);
      aCustomReader.ReadNextChar(xC);
    end;

    aCustomBuffer.GetOWideString(outEntityName);
    if (xC <> ';') or not aReaderSettings.fEntityList.Find(outEntityName, outEntityValue) then
    begin
        if (xC <> ';') then
          aCustomReader.UndoRead;
      _EntityError;
      Exit;
    end;
  end;

  if (xOutputChar > -1) and (outEntityValue = '') then
  begin
    {$IFDEF FPC}
    //FPC, convert to UTF-8 first
    outEntityValue := UTF8Encode(WideString(WideChar(xOutputChar)));//MUST BE WideString + WideChar (convert utf16 to utf8)
    {$ELSE}
    outEntityValue := OWideString(OWideChar(xOutputChar));
    {$ENDIF}
    if aReaderSettings.fStrictXML and not OXmlValidChars(outEntityValue) then
    begin
      _EntityError;
      Exit;
    end;
  end;

  _SetReadString;
  outEntityName := cEntityPrefix[xEntityType]+outEntityName;
  Result := True;
end;

{ TXMLReader }

procedure TXMLReader.FinishOpenElement;
begin
  fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
  fReaderToken.TokenValue := '';
  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
    fReaderToken.TokenType := rtFinishXMLDeclarationClose
  else
    fReaderToken.TokenType := rtFinishOpenElement;
end;

procedure TXMLReader.FinishOpenElementClose;
var
  xC: OWideChar;
  x: PXMLReaderToken;
begin
  //opened after a '?' for PI or '/' for an element.

  fReader.ReadNextChar(xC{%H-});//must be '>'
  if xC <> '>' then
  begin
    if fReaderSettings.fStrictXML then
    begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInElement, ['?']);
        Exit;
      end else
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInElement, ['/']);
        Exit;
      end;
    end else
    begin
      //let's be generous and go over this invalid character
      fReader.UndoRead;
      ReadNextToken(x{%H-});
      Exit;
    end;
  end;

  fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
  fReaderToken.TokenValue := '';
  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
    fReaderToken.TokenType := rtFinishXMLDeclarationClose
  else
    fReaderToken.TokenType := rtFinishOpenElementClose;

  RemoveLastFromNodePath(False);
end;

procedure TXMLReader.Text(const aClearCustomBuffer: Boolean);
var
  xC: OWideChar;
  xCKind: TXMLCharKind;
  xSquareBracketCloseCount: Integer;//the character sequence ']]>' is not allowed in text
begin
  if aClearCustomBuffer then
    fMainBuffer.Clear(False);

  fReader.ReadNextChar(xC{%H-});
  xSquareBracketCloseCount := 0;
  while
    not Assigned(fReader.ParseError) and
    not fReader.EOF
  do begin
    xCKind := OXmlCharKind(xC);

    case xCKind of
      ckNewLine10, ckNewLine13: ProcessNewLineChar(xC, fReaderSettings, fReader, fMainBuffer);
      ckAmpersand:
      begin
        if fReaderSettings.fExpandEntities then
          EntityReferenceInText
        else
          Break;
      end;
      ckLowerThan: Break;
      ckTab, ckCharacter, ckSingleQuote, ckDoubleQuote,
        ckSquareBracketOpen, ckSquareBracketClose: fMainBuffer.WriteOWideChar(xC);
      ckGreaterThan:
      begin
        if fReaderSettings.fStrictXML and (xSquareBracketCloseCount >= 2) then
        begin
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInText, [xC]);
          Exit;
        end;
        fMainBuffer.WriteOWideChar(xC);
      end;
    else
      if not fReaderSettings.fStrictXML then
        fMainBuffer.WriteOWideChar(xC)
      else
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInText, ['0x'+IntToHex(Ord(xC), 4)]);
    end;

    if fReaderSettings.fStrictXML then//"]]>" support
    begin
      if xCKind = ckSquareBracketClose then
        Inc(xSquareBracketCloseCount)
      else
        xSquareBracketCloseCount := 0;
    end;

    fReader.ReadNextChar(xC);
  end;

  if not fReader.EOF then
    fReader.UndoRead;
  fReaderToken.TokenType := rtText;
  fReaderToken.TokenName := '';
  fMainBuffer.GetOWideString(fReaderToken.TokenValue);
end;

procedure TXMLReader.Attribute;
var
  xC: OWideChar;
  xQuotationMark: OWideChar;
begin
  if Assigned(fAttributeTokens) then
  begin
    fReaderToken := fAttributeTokens.CreateNew;
    fAttributeTokens.AddLast;
  end;

  fMainBuffer.Clear(False);
  fReader.ReadNextChar(xC{%H-});

  if fReaderSettings.fStrictXML and not OXmlIsNameStartChar(xC) then
  begin
    RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
      OXmlLng_InvalidAttributeStartChar, [xC]);
    Exit;
  end;

  if not fReaderSettings.fStrictXML then
  begin
    //not StrictXML

    repeat//read attribute name
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    until OXmlIsBreakChar(xC);
  end else
  begin
    //StrictXML
    while OXmlIsNameChar(xC) do
    begin//read attribute name
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    end;
  end;
  fMainBuffer.GetOWideString(fReaderToken.TokenName);

  while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "attr ="
    fReader.ReadNextChar(xC);

  if xC <> '=' then
  begin
    //let's be generous and allow attributes without values - even if they are not allowed by xml spec
    if fReaderSettings.fStrictXML then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_EqualSignMustFollowAttribute, [fReaderToken.TokenName]);
      Exit;
    end else
    begin
      fReaderToken.TokenValue := '';
      fReader.UndoRead;
    end;
  end else
  begin
    fReader.ReadNextChar(xC);
    while OXmlIsWhiteSpaceChar(xC) do//jump over spaces "= value"
      fReader.ReadNextChar(xC);

    xQuotationMark := xC;
    if (xQuotationMark = '''') or (xQuotationMark = '"') then
    begin
      //read attribute value in quotation marks
      fMainBuffer.Clear(False);
      fReader.ReadNextChar(xC);
      while not (xC = xQuotationMark) and not Assigned(fReader.ParseError) and not fReader.EOF do
      begin
        case OXmlCharKind(xC) of
          ckNewLine10, ckNewLine13: ProcessNewLineChar(xC, fReaderSettings, fReader, fMainBuffer);
          ckAmpersand: EntityReferenceInText;
          ckTab, ckCharacter, ckSingleQuote, ckDoubleQuote, ckGreaterThan,
            ckSquareBracketOpen, ckSquareBracketClose: fMainBuffer.WriteOWideChar(xC);
        else
          if not fReaderSettings.fStrictXML then
            fMainBuffer.WriteOWideChar(xC)
          else
            RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
              OXmlLng_InvalidCharacterInAttribute, [IntToHex(Ord(xC), 4)]);
        end;
        fReader.ReadNextChar(xC);
      end;
    end else
    begin
      if fReaderSettings.fStrictXML then
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_AttributeValueMustBeEnclosed, [fReaderToken.TokenName]);
        Exit;
      end else
      begin
        //let's be generous and allow attribute values that are not enclosed in quotes
        fMainBuffer.Clear(False);
        while not OXmlIsBreakChar(xC) and not Assigned(fReader.ParseError) and not fReader.EOF do
        begin
          case OXmlCharKind(xC) of
            ckNewLine10, ckNewLine13: ProcessNewLineChar(xC, fReaderSettings, fReader, fMainBuffer);
            ckAmpersand: EntityReferenceInText;
            ckTab, ckCharacter, ckSingleQuote, ckDoubleQuote,
              ckSquareBracketOpen, ckSquareBracketClose: fMainBuffer.WriteOWideChar(xC);
          else
            if not fReaderSettings.fStrictXML then
              fMainBuffer.WriteOWideChar(xC)
            else
              RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
                OXmlLng_InvalidCharacterInText, [IntToHex(Ord(xC), 4)]);
          end;
          fReader.ReadNextChar(xC);
        end;
      end;
    end;

    fMainBuffer.GetOWideString(fReaderToken.TokenValue);
  end;

  if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
  begin
    fReaderToken.TokenType := rtXMLDeclarationAttribute;

    if not fForceEncoding and fAllowSetEncodingFromFile and
      (fReaderToken.TokenName = 'encoding')
    then
      ChangeEncoding(fReaderToken.TokenValue);
  end else
    fReaderToken.TokenType := rtAttribute;
end;

procedure TXMLReader.CData;
begin
  ExclamationNode(rtCData, '<![CDATA[', ']]>', False, False);
end;

procedure TXMLReader.ChangeEncoding(const aEncodingAlias: OWideString);
var
  xLastName: OWideString;
  xEncoding: TEncoding;
  xInXMLDeclaration: Boolean;
  xReaderToken: PXMLReaderToken;
begin
  if
    TEncoding.EncodingFromAlias(aEncodingAlias, xEncoding{%H-}) and
    (fReader.Encoding <> xEncoding)
  then begin
    //reload document with new encoding
    fReader.Encoding := xEncoding;
    if fAllowSetEncodingFromFile then
    begin
      fAllowSetEncodingFromFile := False;
      fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
    end;
    //go back to current position
    xInXMLDeclaration := False;
    xLastName := fReaderToken.TokenName;
    fLastTokenType := rtDocumentStart;
    fOpenElementTokens.Clear;
    if Assigned(fAttributeTokens) then
      fAttributeTokens.Clear;
    //parse from beginning back to the encoding attribute
    while ReadNextToken(xReaderToken{%H-}) do
    begin
      case xReaderToken.TokenType of
        rtOpenXMLDeclaration: xInXMLDeclaration := True;
        rtOpenElement: xInXMLDeclaration := False;
        rtXMLDeclarationAttribute:
          if xInXMLDeclaration and (xReaderToken.TokenName = xLastName) then
            Exit;
      end;
    end;
  end;
end;

procedure TXMLReader.Comment;
begin
  ExclamationNode(rtComment, '<!--', '-->', False, False);
end;

constructor TXMLReader.Create;
begin
  inherited Create;

  DoCreate;
end;

constructor TXMLReader.Create(const aStream: TStream;
  const aForceEncoding: TEncoding = nil);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream, aForceEncoding);
end;

procedure TXMLReader.DoInit(const aForceEncoding: TEncoding);
begin
  fAllowSetEncodingFromFile := not fReader.BOMFound;
  fDocumentElementFound := False;
  fElementsToClose := 0;
  fLastTokenType := rtDocumentStart;
  fOpenElementTokens.Clear;
  fReaderToken := fOpenElementTokens.CreateNew;
  if Assigned(fAttributeTokens) then
    fAttributeTokens.Clear;

  fReader.ErrorHandling := fReaderSettings.ErrorHandling;

  fReader.BlockFlushTempBuffer;//will be unblocked when fAllowSetEncodingFromFile is set to false
  if Assigned(aForceEncoding) then
    Self.Encoding := aForceEncoding
  else
    Self.fForceEncoding := False;
end;

destructor TXMLReader.Destroy;
begin
  DoDestroy;

  inherited;
end;

procedure TXMLReader.DoCreate;
begin
  fReader := TOTextReader.Create;
  fOwnsReaderToken := True;
  fReaderSettings := TXMLReaderSettings.Create;

  fOpenElementTokens := TXMLReaderTokenList.Create;
  fReaderToken := fOpenElementTokens.CreateNew;

  fMainBuffer := TOByteBuffer.Create;
  fEntityBuffer := TOByteBuffer.Create(16);
end;

procedure TXMLReader.DocType;
begin
  ExclamationNode(rtDocType, '<!DOCTYPE', '>', True, True);
end;

procedure TXMLReader.DoDestroy;
begin
  fReader.Free;
  fReaderSettings.Free;
  fOpenElementTokens.Free;

  fMainBuffer.Free;
  fEntityBuffer.Free;
  //do not destroy fAttributeTokens here!!!
end;

procedure TXMLReader.SetAttributeTokens(
  const aAttributeTokens: TXMLReaderTokenList);
begin
  fAttributeTokens := aAttributeTokens;
  fReaderToken := fOpenElementTokens.CreateNew;//in case fReaderToken was in old fAttributeTokens
end;

procedure TXMLReader.SetEncoding(const aEncoding: TEncoding);
begin
  fReader.Encoding := aEncoding;
  fForceEncoding := True;
end;

procedure TXMLReader.SetOwnsEncoding(const aSetOwnsEncoding: Boolean);
begin
  fReader.OwnsEncoding := aSetOwnsEncoding;
end;

procedure TXMLReader.SetURL(const aURL: OWideString);
begin
  fReader.URL := aURL;
end;

procedure TXMLReader.EntityReferenceInText;
var
  xReadString, xEntityName, xEntityValue: OWideString;
begin
  if ProcessEntity(fReaderSettings, fReader, fEntityBuffer, xReadString{%H-}, xEntityName{%H-}, xEntityValue{%H-}) then
    fMainBuffer.WriteOWideString(xEntityValue)
  else
  begin
    if fReaderSettings.fStrictXML then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidEntity, [xReadString]);
      Exit;
    end else
    begin
      fMainBuffer.WriteOWideString(xReadString);
    end;
  end;
end;

procedure TXMLReader.EntityReferenceStandalone;
var
  xReadString, xEntityName, xEntityValue: OWideString;
begin
  if ProcessEntity(fReaderSettings, fReader, fEntityBuffer, xReadString{%H-}, xEntityName{%H-}, xEntityValue{%H-}) then
  begin
    fReaderToken.TokenType := rtEntityReference;
    fReaderToken.TokenName := xEntityName;
    fReaderToken.TokenValue := xEntityValue;
  end else
  begin
    if fReaderSettings.fStrictXML then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidEntity, [xReadString]);
      Exit;
    end else
    begin
      fReaderToken.TokenType := rtText;
      fReaderToken.TokenName := '';
      fReaderToken.TokenValue := xReadString;
    end;
  end;
end;

procedure TXMLReader.ExclamationNode(
  const aTokenType: TXMLReaderTokenType; const aBeginTag, aEndTag: OWideString;
  const aWhiteSpaceAfterBeginTag, aIsDoctype: Boolean);
var
  I: Integer;
  xC: OWideChar;
  xCKind: TXMLCharKind;
  xPreviousC: OWideString;
  xResult: Boolean;
begin
  fMainBuffer.Clear(False);
  fMainBuffer.WriteOWideChar('<');
  fMainBuffer.WriteOWideChar('!');
  xResult := True;
  for I := 3 to Length(aBeginTag) do
  begin
    fReader.ReadNextChar(xC{%H-});
    if aBeginTag[I] <> UpperCase(xC) then
    begin
      xResult := False;
      fReader.UndoRead;
      Break;
    end;
    fMainBuffer.WriteOWideChar(xC);
  end;

  if aWhiteSpaceAfterBeginTag and xResult then
  begin
    //must be followed by a whitespace character
    fReader.ReadNextChar(xC);
    fMainBuffer.WriteOWideChar(xC);
    xResult := OXmlIsWhiteSpaceChar(xC);
  end;

  if not xResult then
  begin
    //header not found
    if fReaderSettings.fStrictXML then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidCharacterInText, ['<']);
      Exit;
    end else
    begin
      //output as text
      if xC <> '<' then
      begin
        Text(False);
      end else
      begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetOWideString(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end;
  end else
  begin
    fMainBuffer.Clear(False);
    SetLength(xPreviousC, Length(aEndTag));
    for I := 1 to Length(xPreviousC) do
      xPreviousC[I] := #0;

    repeat
      if Length(xPreviousC) > 1 then
        Move(xPreviousC[2], xPreviousC[1], (Length(xPreviousC)-1)*SizeOf(OWideChar));
      fReader.ReadNextChar(xC);

      xCKind := OXmlCharKind(xC);
      case xCKind of
        ckInvalid:
          if fReaderSettings.fStrictXML then
          begin
            RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
              OXmlLng_InvalidCharacterInText, [IntToHex(Ord(xC), 4)]);
            Exit;
          end;
        ckSquareBracketOpen:
          if aIsDoctype then
          begin
            fMainBuffer.WriteOWideChar(xC);
            LoadDTD;
            for I := 1 to Length(xPreviousC) do
              xPreviousC[I] := #0;
            Continue;
          end;
      end;

      xPreviousC[Length(xPreviousC)] := xC;
      fMainBuffer.WriteOWideChar(xC);
    until (
      (xPreviousC = aEndTag) or
      Assigned(fReader.ParseError) or
      fReader.EOF);

    for I := 1 to Length(aEndTag) do
      fMainBuffer.RemoveLastChar;

    fReaderToken.TokenType := aTokenType;
    fReaderToken.TokenName := '';
    fMainBuffer.GetOWideString(fReaderToken.TokenValue);
  end;
end;

procedure TXMLReader.OpenElement;
var
  xC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar(xC{%H-});

  case xC of
    '!':
    begin
      //comment or cdata
      fReader.ReadNextChar(xC);
      fReader.UndoRead;
      case xC of
        '[': CData;
        '-': Comment;
        'D', 'd': DocType;
      else
        if fReaderSettings.fStrictXML then
        begin
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInText, ['<']);
          Exit;
        end else
        begin
          fMainBuffer.WriteOWideChar('<');
          fMainBuffer.WriteOWideChar('!');
          if xC <> '<' then
          begin
            Text(False);
          end else
          begin
            fReaderToken.TokenType := rtText;
            fMainBuffer.GetOWideString(fReaderToken.TokenValue);
            fReaderToken.TokenName := '';
          end;
          Exit;
        end;
      end;
      Exit;
    end;
    '/':
    begin
      //close element
      CloseElement;
      Exit;
    end;
    '?':
    begin
      //processing instruction
      ProcessingInstruction;
      Exit;
    end;
  end;

  if fReaderSettings.fStrictXML then
  begin
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidCharacterInText, ['<']);
      Exit;
    end;

    while OXmlIsNameChar(xC) do
    begin
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    end;

    if (xC = '/') or (xC = '>') then
      fReader.UndoRead
    else if not OXmlIsWhiteSpaceChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidElementName, [fMainBuffer.GetOWideString+xC]);
      Exit;
    end;
  end else
  begin
    if not OXmlIsNameChar(xC) then
    begin
      fMainBuffer.WriteOWideChar('<');
      fReader.UndoRead;
      if xC <> '<' then
      begin
        Text(False);
      end else
      begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetOWideString(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end else
    begin
      while not OXmlIsBreakChar(xC) do
      begin
        fMainBuffer.WriteOWideChar(xC);
        fReader.ReadNextChar(xC);
      end;

      if (xC = '/') or (xC = '>') then
        fReader.UndoRead
    end;
  end;

  if fAllowSetEncodingFromFile then
  begin
    fAllowSetEncodingFromFile := False;// -> first Node found, encoding change is not allowed any more
    fReader.UnblockFlushTempBuffer;//was blocked in TXMLReader.Create
  end;

  fDocumentElementFound := True;
  fMainBuffer.GetOWideString(fReaderToken.TokenName);
  fReaderToken.TokenValue := '';
  fReaderToken.TokenType := rtOpenElement;
  fOpenElementTokens.AddLast;
  if Assigned(fAttributeTokens) then
    fAttributeTokens.Clear;
end;

procedure TXMLReader.RaiseException(const aErrorClass: TOTextParseErrorClass;
  const aReason: string);
begin
  fReader.RaiseException(aErrorClass, aReason);
end;

procedure TXMLReader.RaiseExceptionFmt(const aErrorClass: TOTextParseErrorClass;
  const aReason: string; const aArgs: array of OWideString);
begin
  fReader.RaiseExceptionFmt(aErrorClass, aReason, aArgs);
end;

function TXMLReader.ReadNextToken(var outToken: PXMLReaderToken): Boolean;
var
  xC: OWideChar;
begin
  Result := True;

  if fElementsToClose > 0 then
  begin
    //close elements
    fReaderToken := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1);//has TokenName already set
    fReaderToken.TokenValue := '';
    fReaderToken.TokenType := rtCloseElement;
    RemoveLastFromNodePath(False);
    fLastTokenType := fReaderToken.TokenType;
    outToken := fReaderToken;
    Dec(fElementsToClose);
    Exit;
  end;

  if (fReaderSettings.fBreakReading = brAfterDocumentElement) and
     (fDocumentElementFound) and
     NodePathIsEmpty
  then begin
    //end of root element is reached, but do not release document!!!
    Result := False;
    outToken := fReaderToken;
    Exit;
  end;

  fReaderToken := fOpenElementTokens.CreateNew;//must be here and not in the beggining of the function -> due to attributes and open elements and sequential parser

  if not fReader.ReadNextChar(xC{%H-}) then
  begin
    //end of document
    Result := False;
    ReleaseDocument;
    if fReaderSettings.fStrictXML and (NodePathCount > 0) then
      RaiseExceptionFmt(TXMLParseErrorInvalidStructure,
        OXmlLng_UnclosedElementsInTheEnd, [xC]);
    Exit;
  end;

  case xC of
    '<':
    begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        if fReaderSettings.fStrictXML then
        begin
          Result := False;
          RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
            OXmlLng_InvalidCharacterInElement, [xC]);
          Exit;
        end else
        begin
          fReader.UndoRead;
          Attribute;
        end;
      end else
      begin
        OpenElement;
      end;
    end;
    '?':
    begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute] then
      begin
        FinishOpenElementClose
      end else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '/':
    begin
      if fLastTokenType in [rtOpenElement, rtAttribute] then
      begin
        FinishOpenElementClose
      end else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
    '>':
    begin
      if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute] then
      begin
        FinishOpenElement;
      end else
      begin
        //text
        fReader.UndoRead;
        Text;
      end;
    end;
  else//case
    if fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute, rtOpenElement, rtAttribute]
    then begin
      while OXmlIsWhiteSpaceChar(xC) do
        fReader.ReadNextChar(xC);

      if ((xC = '/') and (fLastTokenType in [rtOpenElement, rtAttribute])) or
         ((xC = '?') and (fLastTokenType in [rtOpenXMLDeclaration, rtXMLDeclarationAttribute]))
      then begin
        FinishOpenElementClose;
      end else if ((xC = '>') and (fLastTokenType in [rtOpenElement, rtAttribute])) then
      begin
        FinishOpenElement;
      end else
      begin
        fReader.UndoRead;
        Attribute;
      end;
    end else
    begin
      //text
      if not fReaderSettings.fExpandEntities and (xC = '&') then
      begin
        EntityReferenceStandalone;
      end else
      begin
        fReader.UndoRead;
        Text;
      end;
    end;
  end;

  outToken := fReaderToken;
  fLastTokenType := fReaderToken.TokenType;

  Result := not Assigned(fReader.ParseError);
end;

procedure TXMLReader.CloseElement;
var
  xC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar(xC{%H-});

  if fReaderSettings.fStrictXML then
  begin
    //strict
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['</'+xC]);
      Exit;
    end;

    while OXmlIsNameChar(xC) do
    begin
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    end;
    while OXmlIsWhiteSpaceChar(xC) do
    begin
      fReader.ReadNextChar(xC);
    end;
    if xC <> '>' then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['</'+fMainBuffer.GetOWideString]);
      Exit;
    end;
  end else
  begin
    //not strict
    if not OXmlIsNameChar(xC) then
    begin
      fMainBuffer.WriteOWideChar('<');
      fMainBuffer.WriteOWideChar('/');
      fReader.UndoRead;
      if xC <> '<' then
      begin
        Text(False);
      end else
      begin
        fReaderToken.TokenType := rtText;
        fMainBuffer.GetOWideString(fReaderToken.TokenValue);
        fReaderToken.TokenName := '';
      end;
      Exit;
    end else
    begin
      while not OXmlIsBreakChar(xC) do
      begin
        fMainBuffer.WriteOWideChar(xC);
        fReader.ReadNextChar(xC);
      end;
      while not((xC = '>') or fReader.EOF) do
      begin
        fReader.ReadNextChar(xC);
      end;
    end;
  end;

  fMainBuffer.GetOWideString(fReaderToken.TokenName);
  fReaderToken.TokenValue := '';
  fReaderToken.TokenType := rtCloseElement;

  RemoveLastFromNodePath(True);
end;

function TXMLReader.GetApproxStreamPosition: OStreamInt;
begin
  Result := fReader.ApproxStreamPosition;
end;

function TXMLReader.GetEncoding: TEncoding;
begin
  Result := fReader.Encoding;
end;

function TXMLReader.GetOwnsEncoding: Boolean;
begin
  Result := fReader.OwnsEncoding;
end;

function TXMLReader.GetParseError: IOTextParseError;
begin
  Result := fReader.ParseError;
end;

function TXMLReader.GetStreamSize: OStreamInt;
begin
  Result := fReader.StreamSize;
end;

function TXMLReader.GetURL: OWideString;
begin
  Result := fReader.URL;
end;

function TXMLReader.GetLinePosition: OStreamInt;
begin
  Result := fReader.LinePosition;
end;

function TXMLReader.GetLine: OStreamInt;
begin
  Result := fReader.Line;
end;

function TXMLReader.GetFilePosition: OStreamInt;
begin
  Result := fReader.FilePosition;
end;

procedure TXMLReader.InitBuffer(const aBuffer: TBytes;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer, TEncoding.UTF8);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitBuffer(const aBuffer; const aBufferLength: Integer;
  const aForceEncoding: TEncoding);
begin
  fReader.InitBuffer(aBuffer, aBufferLength, TEncoding.UTF8);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitFile(const aFileName: OWideString;
  const aForceEncoding: TEncoding);
begin
  fReader.InitFile(aFileName, TEncoding.UTF8);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitStream(const aStream: TStream;
  const aForceEncoding: TEncoding);
begin
  fReader.InitStream(aStream, TEncoding.UTF8);
  DoInit(aForceEncoding);
end;

procedure TXMLReader.InitXML(const aXML: OWideString);
begin
  fReader.InitString(aXML);
  DoInit(TEncoding.OWideStringEncoding);
end;

procedure TXMLReader.LoadDTD;
var
  xReaderStartPos: Integer;
begin
  fReader.BlockFlushTempBuffer;

  xReaderStartPos := fReader.TempStringPosition;

  try
    fReaderSettings.LoadDTD(fReader, True);

  finally
    fMainBuffer.WriteOWideString(fReader.ReadPreviousString(fReader.TempStringPosition - xReaderStartPos));
    fReader.UnblockFlushTempBuffer;
  end;
end;

function TXMLReader.NodePathIsEmpty: Boolean;
begin
  Result := fOpenElementTokens.Count = 0;
end;

procedure TXMLReader.InitXML_UTF8(const aXML: OUTF8Container);
begin
  fReader.InitString_UTF8(aXML);
  DoInit(TEncoding.UTF8);
end;

procedure TXMLReader.ProcessingInstruction;
var
  xC: OWideChar;
  xPreviousC: OWideChar;
begin
  fMainBuffer.Clear(False);
  fReader.ReadNextChar(xC{%H-});

  if fReaderSettings.fStrictXML then
  begin
    if not OXmlIsNameStartChar(xC) then
    begin
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['<?'+xC]);
      Exit;
    end;

    while OXmlIsNameChar(xC) do
    begin
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    end;

    if not OXmlIsWhiteSpaceChar(xC) and (xC <> '?') then
    begin
      //must be followed by a whitespace character
      fMainBuffer.WriteOWideChar(xC);
      RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidStringInText, ['<?'+fMainBuffer.GetOWideString]);
      Exit;
    end;
  end else
  begin
    while not OXmlIsBreakChar(xC) do
    begin
      fMainBuffer.WriteOWideChar(xC);
      fReader.ReadNextChar(xC);
    end;
  end;

  fMainBuffer.GetOWideString(fReaderToken.TokenName);
  if
    not fDocumentElementFound and
    fReaderSettings.fRecognizeXMLDeclaration and
    OSameText(fReaderToken.TokenName, XML_XML)
  then begin
    //xml declaration: <?xml attr="value"?>
    fReaderToken.TokenType := rtOpenXMLDeclaration;
    fReaderToken.TokenValue := '';
    fOpenElementTokens.AddLast;
    Exit;
  end;

  //custom processing instruction
  fReaderToken.TokenType := rtProcessingInstruction;
  fMainBuffer.Clear(False);
  if not fReaderSettings.fStrictXML and (fReaderToken.TokenName = '') then
    fReader.UndoRead;

  xPreviousC := xC;
  fReader.ReadNextChar(xC);
  while
    not((xPreviousC = '?') and (xC = '>')) and
    not fReader.EOF
  do begin
    fMainBuffer.WriteOWideChar(xC);
    xPreviousC := xC;
    fReader.ReadNextChar(xC);
  end;
  fMainBuffer.RemoveLastChar;

  fMainBuffer.GetOWideString(fReaderToken.TokenValue);
end;

procedure TXMLReader.ReleaseDocument;
begin
  fReader.ReleaseDocument;
end;

procedure TXMLReader.RemoveLastFromNodePath(const aCheckPath: Boolean);
var
  I: Integer;
begin
  if (fOpenElementTokens.Count = 0) then
  begin
    //there is no open element
    if fReaderSettings.fStrictXML then
    begin
      RaiseException(TXMLParseErrorInvalidStructure,
        OXmlLng_TooManyElementsClosed);
      Exit;
    end;
  end else
  begin
    if aCheckPath and
       (fReaderToken.TokenName <> '') and
       (fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName <> fReaderToken.TokenName)
    then begin
      //element names do not match
      if fReaderSettings.fStrictXML then
      begin
        RaiseExceptionFmt(TXMLParseErrorInvalidStructure,
          OXmlLng_WrongElementClosed, [fReaderToken.TokenName,
            fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName]);
        Exit;
      end else
      begin
        //trying to close parent element
        for I := fOpenElementTokens.Count-2 downto 0 do
        if (fOpenElementTokens.GetToken(I).TokenName = fReaderToken.TokenName) then
        begin
          //parent element with the same name found, we have to close more elements in the future!!!
          fElementsToClose := (fOpenElementTokens.Count - I - 1);
          Break;
        end;

        //delete the last one from fNodePath
        //  + rename the element if names differ
        if fReaderToken.TokenName <> '' then
          fReaderToken.TokenName := fOpenElementTokens.GetToken(fOpenElementTokens.Count-1).TokenName;
        fOpenElementTokens.DeleteLast;
      end;
    end else
    begin
      //everything is fine -> delete last from fNodePath
      fOpenElementTokens.DeleteLast;
    end;
  end;
end;

procedure TXMLReader.ResetDocumentElement;
begin
  fDocumentElementFound := False;
end;

function TXMLReader.GetNodePath(
  const aIndex: Integer): OWideString;
begin
  Result := fOpenElementTokens.GetToken(aIndex).TokenName;
end;

function TXMLReader.GetNodePathCount: Integer;
begin
  Result := fOpenElementTokens.Count;
end;

procedure TXMLReader.NodePathAssignTo(
  const aNodePath: TOWideStringList);
var
  I: Integer;
begin
  aNodePath.Clear;
  for I := 0 to fOpenElementTokens.Count-1 do
    aNodePath.Add(fOpenElementTokens.GetToken(I).TokenName);
end;

function TXMLReader.NodePathAsString: OWideString;
var
  I: Integer;
begin
  if fOpenElementTokens.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Result := fOpenElementTokens.GetToken(0).TokenName;
  for I := 1 to fOpenElementTokens.Count-1 do
    Result := Result + '/' + fOpenElementTokens.GetToken(I).TokenName;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aNodePath) = fOpenElementTokens.Count;
  if not Result then
    Exit;

  for I := 0 to Length(aNodePath)-1 do
  if aNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aNodePath.Count = fOpenElementTokens.Count;
  if not Result then
    Exit;

  for I := 0 to aNodePath.Count-1 do
  if aNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.NodePathMatch(
  const aNodePath: OWideString): Boolean;
var
  xNodePath: TOWideStringList;
begin
  xNodePath := TOWideStringList.Create;
  try
    OExplode(aNodePath, '/', xNodePath);

    Result := NodePathMatch(xNodePath);
  finally
    xNodePath.Free;
  end;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fOpenElementTokens.Count-1;
  if not Result then
    Exit;

  for I := 0 to Length(aRefNodePath)-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsChildOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fOpenElementTokens.Count-1;
  if not Result then
    Exit;

  for I := 0 to aRefNodePath.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: array of OWideString): Boolean;
var
  I: Integer;
begin
  Result := Length(aRefNodePath) = fOpenElementTokens.Count+1;
  if not Result then
    Exit;

  for I := 0 to fOpenElementTokens.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

function TXMLReader.RefIsParentOfNodePath(
  const aRefNodePath: TOWideStringList): Boolean;
var
  I: Integer;
begin
  Result := aRefNodePath.Count = fOpenElementTokens.Count+1;
  if not Result then
    Exit;

  for I := 0 to fOpenElementTokens.Count-1 do
  if aRefNodePath[I] <> fOpenElementTokens.GetToken(I).TokenName then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

{ TXMLWriter }

procedure TXMLWriter.CData(const aText: OWideString; const aIndent: Boolean);
  procedure _BeginCDATA;
  begin
    RawChar('<');
    RawChar('!');
    RawChar('[');
    RawChar('C');
    RawChar('D');
    RawChar('A');
    RawChar('T');
    RawChar('A');
    RawChar('[');
  end;
  procedure _EndCDATA;
  begin
    RawChar(']');
    RawChar(']');
    RawChar('>');
  end;

  procedure _WriteCDataSafe(const _Text: OUnicodeString);
  var
    I: Integer;
  begin
    for I := 1 to Length(_Text) do
    if GetCharTable.IsSupported(_Text[I]) then
      RawUnicodeChar(_Text[I])
    else
    begin
      _EndCDATA;
      RawText(GetCharTable.Entity(_Text[I]));
      _BeginCDATA;
    end;
  end;
begin
  if fWriterSettings.fStrictXML and not OXmlValidCData(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidCData, [aText]);

  if aIndent then
    Indent;

  _BeginCDATA;
  if not fUseSafeEntities then
    RawText(aText)//MUST BE RAWTEXT - must contain unescaped characters
  else begin
    _WriteCDataSafe({$IFDEF O_UTF8}OWideToUnicode{$ENDIF}(aText));
  end;
  _EndCDATA;
end;

procedure TXMLWriter.Comment(const aText: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidComment(aText) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidComment, [aText]);

  Indent;

  RawChar('<');
  RawChar('!');
  RawChar('-');
  RawChar('-');
  RawText(aText);//MUST BE RAWTEXT - must contain unescaped characters
  RawChar('-');
  RawChar('-');
  RawChar('>');
end;

constructor TXMLWriter.Create(const aStream: TStream);
begin
  inherited Create;

  DoCreate;

  InitStream(aStream);
end;

constructor TXMLWriter.Create;
begin
  inherited Create;

  DoCreate;
end;

procedure TXMLWriter.DecIndentLevel;
begin
  Dec(fIndentLevel);
end;

destructor TXMLWriter.Destroy;
begin
  fWriter.Free;
  fWriterSettings.Free;
  fCharTable.Free;

  inherited;
end;

procedure TXMLWriter.DoInit;
begin
  fWritten := False;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.DoText(const aText: OWideString;
  const aInAttributeValue: Boolean);
begin
  if fUseSafeEntities then
    _DoTextSafeEntity({$IFDEF O_UTF8}OWideToUnicode{$ENDIF}(aText), aInAttributeValue)
  else
    _DoText(aText, aInAttributeValue);
end;

procedure TXMLWriter._DoText(const aText: OWideString;
  const aInAttributeValue: Boolean);
var
  xC: OWideChar;
  I, xLength: Integer;
begin
  //"almost" same code as DoTextSafeEntity
  xLength := Length(aText);
  if xLength = 0 then
    Exit;

  for I := 1 to xLength do
  begin
    xC := aText[I];
    case OXmlCharKind(xC) of
      ckTab:
        if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
          RawText(XML_ENTITY_TAB)
        else
          RawChar(xC);
      ckAmpersand: RawText(XML_ENTITY_AMP);
      ckLowerThan: RawText(XML_ENTITY_LT);
      ckGreaterThan:
        if fWriterSettings.UseGreaterThanEntity then
          RawText(XML_ENTITY_GT)
        else
          RawChar(xC);
      ckDoubleQuote:
        if aInAttributeValue then
          RawText(XML_ENTITY_QUOT)
        else
          RawChar(xC);
      ckSingleQuote:
        RawChar(xC);
      ckNewLine10:
        if (fWriterSettings.fLineBreak = lbDoNotProcess) then//no line break handling
        begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XML_ENTITY_LF)
          else
            RawChar(xC);
        end else
        if ((I = 1) or (aText[I-1] <> #13)) then//previous character is not #13 (i.e. this is a simple #10 not #13#10) -> write fLineBreak
        begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XMLLineBreakEntity(fWriterSettings.fLineBreak))
          else
            RawText(XMLLineBreak[fWriterSettings.fLineBreak]);
        end;
      ckNewLine13:
        if fWriterSettings.fLineBreak = lbDoNotProcess then
          RawText(XML_ENTITY_CR)
        else
        begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XMLLineBreakEntity(fWriterSettings.fLineBreak))
          else
            RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
        end;
      ckCharacter, ckSquareBracketOpen, ckSquareBracketClose: RawChar(xC);
    else
      //invalid
      if not fWriterSettings.fStrictXML then
        RawChar(xC)
      else
        raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidText, [aText]);
    end;
  end;
end;

procedure TXMLWriter._DoTextSafeEntity(const aText: OUnicodeString;
  const aInAttributeValue: Boolean);
var
  xC: OUnicodeChar;
  I, xLength: Integer;
begin
  //"almost" same code as DoText
  xLength := Length(aText);
  if xLength = 0 then
    Exit;

  for I := 1 to xLength do
  begin
    xC := aText[I];
    case OXmlCharKind(xC) of
      ckTab:
        if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
          RawText(XML_ENTITY_TAB)
        else
          RawChar(OWideChar(Ord(xC) and $FF));
      ckAmpersand: RawText(XML_ENTITY_AMP);
      ckLowerThan: RawText(XML_ENTITY_LT);
      ckGreaterThan:
        if fWriterSettings.UseGreaterThanEntity then
          RawText(XML_ENTITY_GT)
        else
          RawChar(OWideChar(Ord(xC) and $FF));
      ckDoubleQuote:
        if aInAttributeValue then
          RawText(XML_ENTITY_QUOT)
        else
          RawChar(OWideChar(Ord(xC) and $FF));
      ckSingleQuote:
        RawChar(OWideChar(Ord(xC) and $FF));
      ckNewLine10:
        if (fWriterSettings.fLineBreak = lbDoNotProcess) then//no line break handling
        begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XML_ENTITY_LF)
          else
            RawChar(OWideChar(Ord(xC) and $FF));
        end else
        if ((I = 1) or (aText[I-1] <> #13)) then//previous character is not #13 (i.e. this is a simple #10 not #13#10) -> write fLineBreak
        begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XMLLineBreakEntity(fWriterSettings.fLineBreak))
          else
            RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
        end;
      ckNewLine13:
        if fWriterSettings.fLineBreak = lbDoNotProcess then
          RawText(XML_ENTITY_CR)
        else begin
          if aInAttributeValue and fWriterSettings.fUseTabCRLFEntitiesInAttributes then
            RawText(XMLLineBreakEntity(fWriterSettings.fLineBreak))
          else
            RawText(XmlLineBreak[fWriterSettings.fLineBreak]);
        end;
      ckCharacter, ckSquareBracketOpen, ckSquareBracketClose: RawUnicodeCharSafeEntity(xC);
    else
      //invalid
      if not fWriterSettings.fStrictXML then
        RawChar(xC)
      else
        raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidText, [aText]);
    end;
  end;
end;

procedure TXMLWriter.EntityReference(const aEntityName: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidEntityReference(aEntityName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidEntity, [aEntityName]);

  RawChar('&');
  RawText(aEntityName);//must be rawtext, because validated!
  RawChar(';');
end;

procedure TXMLWriter.DoCreate;
begin
  fWriter := TOTextWriter.Create;
  fWriterSettings := TXMLWriterSettings.Create(Self);

  Encoding := TEncoding.UTF8;
  WriteBOM := True;
end;

procedure TXMLWriter.DocType(const aDocTypeRawText: OWideString);
begin
  Indent;

  RawText('<!DOCTYPE ');
  RawText(aDocTypeRawText);//MUST BE RAW ESCAPED TEXT - the programmer has to be sure that aDocTypeRawText is valid
  RawChar('>');
end;

procedure TXMLWriter.CloseElement(const aElementName: OWideString; const aIndent: Boolean);
begin
  DecIndentLevel;
  if aIndent then
    Indent;
  RawChar('<');
  RawChar('/');
  RawText(aElementName);//must be rawtext, because validated (in OpenElement)!
  RawChar('>');
end;

function TXMLWriter.GetCharTable: TOCharTable;
begin
  if not Assigned(fCharTable) then
    fCharTable := TOCharTable.Create(Encoding);
  Result := fCharTable;
end;

function TXMLWriter.GetEncoding: TEncoding;
begin
  Result := fWriter.Encoding;
end;

function TXMLWriter.GetOwnsEncoding: Boolean;
begin
  Result := fWriter.OwnsEncoding;
end;

function TXMLWriter.GetWriteBOM: Boolean;
begin
  Result := fWriter.WriteBOM;
end;

procedure TXMLWriter.IncIndentLevel;
begin
  Inc(fIndentLevel);
end;

procedure TXMLWriter.Indent;
var I: Integer;
begin
  if (fWriterSettings.fIndentType in [itFlat, itIndent]) and
    fWritten//do not indent at the very beginning of the document
  then
    RawText(XmlLineBreak[fWriterSettings.fLineBreak]);

  if fWriterSettings.fIndentType = itIndent then
  for I := 1 to fIndentLevel do
    RawText(fWriterSettings.fIndentString);
end;

procedure TXMLWriter.InitFile(const aFileName: OWideString);
begin
  fWriter.InitFile(aFileName);

  DoInit;
end;

procedure TXMLWriter.InitStream(const aStream: TStream;
  const aOwnsStream: Boolean);
begin
  fWriter.InitStream(aStream, aOwnsStream);

  DoInit;
end;

procedure TXMLWriter.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aTarget) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPITarget, [aTarget]);

  if fWriterSettings.fStrictXML and not OXmlValidPIContent(aContent) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidPIContent, [aContent]);

  Indent;

  RawChar('<');
  RawChar('?');
  RawText(aTarget);
  if (aTarget <> '') and (aContent <> '') then
    RawChar(' ');
  RawText(aContent);//MUST BE RAWTEXT - must contain unescaped characters
  RawChar('?');
  RawChar('>');
end;

procedure TXMLWriter.RawChar(const aChar: OWideChar);
begin
  fWritten := True;

  fWriter.WriteChar(aChar);
end;

procedure TXMLWriter.RawUnicodeCharSafeEntity(const aChar: OUnicodeChar);
begin
  fWritten := True;

  if GetCharTable.IsSupported(aChar) then
    RawUnicodeChar(aChar)
  else
    RawText(GetCharTable.Entity(aChar));
end;

procedure TXMLWriter.RawText(const aText: OWideString);
begin
  fWritten := True;

  fWriter.WriteString(aText);
end;

procedure TXMLWriter.RawUnicodeChar(const aChar: OUnicodeChar);
begin
  fWritten := True;

  {$IFDEF O_UTF8}
  fWriter.WriteString(OUnicodeToWide(aChar));
  {$ELSE}
  fWriter.WriteChar(aChar);
  {$ENDIF}
end;

procedure TXMLWriter.ReleaseDocument;
begin
  fWriter.ReleaseDocument;
end;

procedure TXMLWriter.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aAttrName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidAttributeName, [aAttrName]);

  RawChar(' ');
  RawText(aAttrName);//can be rawtext, because validated!
  RawChar('=');
  RawChar('"');
  DoText(aAttrValue, True);
  RawChar('"');
end;

procedure TXMLWriter.SetEncoding(const aEncoding: TEncoding);
begin
  fWriter.Encoding := aEncoding;
  fCharTable.Free;
  fCharTable := nil;
  fUseSafeEntities := aEncoding.IsSingleByte;
end;

procedure TXMLWriter.SetDefaultIndentLevel(const aDefaultIndentLevel: Integer);
begin
  if fWritten then
    raise EXmlWriterException.Create(OXmlLng_CannotSetIndentLevelAfterWrite);
  fDefaultIndentLevel := aDefaultIndentLevel;
  fIndentLevel := fDefaultIndentLevel;
end;

procedure TXMLWriter.SetOwnsEncoding(const aOwnsEncoding: Boolean);
begin
  fWriter.OwnsEncoding := aOwnsEncoding;
end;

procedure TXMLWriter.SetWriteBOM(const aWriteBOM: Boolean);
begin
  fWriter.WriteBOM := aWriteBOM;
end;

procedure TXMLWriter.Text(const aText: OWideString; const aIndent: Boolean);
begin
  if aIndent then
    Indent;

  DoText(aText, False);
end;

procedure TXMLWriter.OpenXMLDeclaration;
begin
  Indent;
  RawText('<?xml');
end;

procedure TXMLWriter.FinishOpenXMLDeclaration;
begin
  RawText('?>');
end;

procedure TXMLWriter.OpenElement(const aElementName: OWideString; const aMode: TXMLWriterElementMode);
begin
  if fWriterSettings.fStrictXML and not OXmlValidName(aElementName) then
    raise EXmlWriterInvalidString.CreateFmt(OXmlLng_InvalidElementName, [aElementName]);

  Indent;

  RawChar('<');
  RawText(aElementName);
  case aMode of
    stOpenOnly: IncIndentLevel;
    stFinish:
    begin
      RawChar('>');
      IncIndentLevel;
    end;
    stFinishClose:
    begin
      RawChar('/');
      RawChar('>');
    end;
  end;
end;

procedure TXMLWriter.OpenElementR(const aElementName: OWideString;
  var outElement: TXMLWriterElement; const aMode: TXMLWriterElementMode);
begin
  OpenElement(aElementName, aMode);

  if aMode = stFinishClose then
  begin
    outElement.fOwner := nil;//do not use after close
  end else
  begin
    outElement.fOwner := Self;
    outElement.fElementName := aElementName;
    outElement.fOpenElementFinished := (aMode = stFinish);
  end;
end;

procedure TXMLWriter.FinishOpenElement(const aElementName: OWideString);
begin
  RawChar('>');
end;

procedure TXMLWriter.FinishOpenElementClose(const aElementName: OWideString);
begin
  DecIndentLevel;
  RawChar('/');
  RawChar('>');
end;

function TXMLWriter.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  OpenElementR(aElementName, Result{%H-}, aMode);
end;

procedure TXMLWriter.XMLDeclaration(const aEncoding: Boolean;
  const aVersion: OWideString; const aStandAlone: OWideString);
begin
  OpenXMLDeclaration;

  if aVersion <> '' then
    Attribute('version', aVersion);
  if aEncoding then
    Attribute('encoding', fWriter.Encoding.EncodingAlias);
  if aStandAlone <> '' then
    Attribute('standalone', aStandAlone);

  FinishOpenXMLDeclaration;
end;

{ TXMLWriterElement }

procedure TXMLWriterElement.Attribute(const aAttrName, aAttrValue: OWideString);
begin
  if fOpenElementFinished then
    raise EXmlDOMException.CreateFmt(OXmlLng_CannotWriteAttributesWhenFinished, [aAttrName, aAttrValue, fElementName])
  else
    fOwner.Attribute(aAttrName, aAttrValue);
end;

procedure TXMLWriterElement.CData(const aText: OWideString);
begin
  FinishOpenElement;
  fOwner.CData(aText);
end;

procedure TXMLWriterElement.Comment(const aText: OWideString);
begin
  FinishOpenElement;
  fOwner.Comment(aText);
end;

procedure TXMLWriterElement.CloseElement(const aIndent: Boolean);
begin
  if fOpenElementFinished then
    fOwner.CloseElement(fElementName, aIndent)
  else
    fOwner.FinishOpenElementClose;

  //DO NOT USE THIS RECORD ANY MORE
  fOwner := nil;
  fElementName := '';
end;

procedure TXMLWriterElement.ProcessingInstruction(const aTarget,
  aContent: OWideString);
begin
  FinishOpenElement;
  fOwner.ProcessingInstruction(aTarget, aContent);
end;

procedure TXMLWriterElement.OpenElementR(const aElementName: OWideString;
  var outElement: TXMLWriterElement; const aMode: TXMLWriterElementMode);
begin
  FinishOpenElement;
  fOwner.OpenElementR(aElementName, outElement, aMode);
end;

procedure TXMLWriterElement.FinishOpenElement;
begin
  if not fOpenElementFinished then
  begin
    fOwner.FinishOpenElement;
    fOpenElementFinished := True;
  end;
end;

function TXMLWriterElement.OpenElementR(const aElementName: OWideString;
  const aMode: TXMLWriterElementMode): TXMLWriterElement;
begin
  OpenElementR(aElementName, Result{%H-}, aMode);
end;

procedure TXMLWriterElement.Text(const aText: OWideString; const aIndent: Boolean);
begin
  FinishOpenElement;
  fOwner.Text(aText, aIndent);
end;

{ TCustomXMLWriterSettings }

procedure TCustomXMLWriterSettings.AssignTo(Dest: TPersistent);
var
  xDest: TCustomXMLWriterSettings;
begin
  if Dest is TCustomXMLWriterSettings then
  begin
    xDest := TCustomXMLWriterSettings(Dest);

    xDest.IndentString := Self.IndentString;
    xDest.IndentType := Self.IndentType;
    xDest.LineBreak := Self.LineBreak;
    xDest.UseTabCRLFEntitiesInAttributes := Self.UseTabCRLFEntitiesInAttributes;
    xDest.UseGreaterThanEntity := Self.UseGreaterThanEntity;
    xDest.StrictXML := Self.StrictXML;
    xDest.WriteBOM := Self.WriteBOM;
  end else
    inherited;
end;

constructor TCustomXMLWriterSettings.Create;
begin
  inherited Create;

  fLineBreak := lbLF;
  fStrictXML := True;
  fUseGreaterThanEntity := True;
  fUseTabCRLFEntitiesInAttributes := True;
  fIndentType := itNone;
  fIndentString := #32#32;
end;

{ TXMLReaderSettings }

procedure TXMLReaderSettings.AssignTo(Dest: TPersistent);
var
  xDest: TXMLReaderSettings;
begin
  if Dest is TXMLReaderSettings then
  begin
    xDest := TXMLReaderSettings(Dest);

    xDest.BreakReading := Self.BreakReading;
    xDest.LineBreak := Self.LineBreak;
    xDest.StrictXML := Self.StrictXML;
    xDest.RecognizeXMLDeclaration := Self.RecognizeXMLDeclaration;
    xDest.ErrorHandling := Self.ErrorHandling;
    xDest.ExpandEntities := Self.ExpandEntities;
    xDest.EntityList.Assign(Self.EntityList);
  end else
    inherited;
end;

constructor TXMLReaderSettings.Create;
begin
  inherited Create;

  fEntityList := TXMLReaderEntityList.Create;

  fBreakReading := brAfterDocumentElement;
  fLineBreak := XMLDefaultLineBreak;
  fStrictXML := True;
  fRecognizeXMLDeclaration := True;
  fExpandEntities := True;
  fErrorHandling := ehRaiseAndEat;
end;

destructor TXMLReaderSettings.Destroy;
begin
  fEntityList.Free;

  inherited Destroy;
end;

procedure TXMLReaderSettings.LoadDTD(const aDTDReader: TOTextReader;
  const aIsInnerDTD: Boolean);
var
  I: Integer;
  xC: OWideChar;
  xPreviousC: OWideString;//holds '<!ENTITY'
  xInQuotes: OWideChar;
  xDTDBuffer, xTempBuffer: TOByteBuffer;
const
  cEntityStr: OWideString = '<!ENTITY';
begin
  fEntityList.Clear;

  xDTDBuffer := TOByteBuffer.Create;
  xTempBuffer := TOByteBuffer.Create;
  try
    SetLength(xPreviousC, Length(cEntityStr));
    for I := 1 to Length(xPreviousC) do
      xPreviousC[I] := #0;

    aDTDReader.ReadNextChar(xC{%H-});

    xInQuotes := #0;//not in quotes

    while
      not (aIsInnerDTD and (xInQuotes = #0) and (xC = ']')) and//hold on ']' if inner DTD
      not Assigned(aDTDReader.ParseError) and
      not aDTDReader.EOF
    do begin
      if Length(xPreviousC) > 1 then
        Move(xPreviousC[2], xPreviousC[1], (Length(xPreviousC)-1)*SizeOf(OWideChar));
      xPreviousC[Length(xPreviousC)] := OWideChar(UpCase(Char(xC)));//use uppercase

      if xPreviousC = cEntityStr then
      begin
        aDTDReader.ReadNextChar(xC);
        if not OXmlIsWhiteSpaceChar(xC) then
        begin
          Continue;//go to next char
        end;
        LoadDTDEntity(aDTDReader, xDTDBuffer, xTempBuffer);
      end else
      begin
        case OXmlCharKind(xC) of
          ckInvalid:
            if fStrictXML then
              aDTDReader.RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
                OXmlLng_InvalidCharacterInText, ['0x'+IntToHex(Ord(xC), 4)]);
          ckDoubleQuote, ckSingleQuote:
            if xInQuotes = #0 then
              xInQuotes := xC
            else if (xInQuotes = xC) then
              xInQuotes := #0;
        end;
      end;

      aDTDReader.ReadNextChar(xC);
    end;//while

  finally
    xDTDBuffer.Free;
    xTempBuffer.Free;
  end;
end;

procedure TXMLReaderSettings.LoadDTDEntity(const aDTDReader: TOTextReader;
  const aBuffer1, aBuffer2: TOByteBuffer);
var
  xC, xQuotationMark: OWideChar;
  xEntityName, xEntityValue: OWideString;
  xIsParameterEntity: Boolean;//used only in DTD -> IGNORE!!!
begin
  if not aDTDReader.ReadNextChar(xC{%H-}) then
    Exit;

  //go over spaces
  while OXmlIsWhiteSpace(xC) and not aDTDReader.EOF do
    aDTDReader.ReadNextChar(xC);

  //read name
  xIsParameterEntity := (xC = '%');
  if xIsParameterEntity then
  begin
    //go over spaces
    aDTDReader.ReadNextChar(xC);
    while OXmlIsWhiteSpace(xC) and not aDTDReader.EOF do
      aDTDReader.ReadNextChar(xC);
  end;

  if not OXmlIsNameStartChar(xC) then
  begin
    aDTDReader.RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
      OXmlLng_InvalidCharacterInElement, [xC]);
    Exit;
  end;

  aBuffer1.Clear(False);
  while OXmlIsNameChar(xC) and not aDTDReader.EOF do
  begin
    aBuffer1.WriteOWideChar(xC);
    aDTDReader.ReadNextChar(xC);
  end;

  aBuffer1.GetOWideString(xEntityName{%H-});
  aBuffer1.Clear(False);

  //go over spaces
  while OXmlIsWhiteSpace(xC) and not aDTDReader.EOF do
    aDTDReader.ReadNextChar(xC);

  //read value
  if (xC <> '"') and (xC <> '''') then
  begin
    aDTDReader.RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
      OXmlLng_DTDEntityValueMustBeEnclosed, [xEntityName]);
    Exit;
  end;
  xQuotationMark := xC;
  aDTDReader.ReadNextChar(xC);

  while not (xC = xQuotationMark) and not Assigned(aDTDReader.ParseError) and not aDTDReader.EOF do
  begin
    case OXmlCharKind(xC) of
      ckNewLine10, ckNewLine13: ProcessNewLineChar(xC, Self, aDTDReader, aBuffer1);
      ckAmpersand: LoadDTDEntityReference(aDTDReader, aBuffer1, aBuffer2);
      ckTab, ckCharacter, ckSingleQuote, ckDoubleQuote, ckGreaterThan, ckLowerThan,
        ckSquareBracketOpen, ckSquareBracketClose:
          aBuffer1.WriteOWideChar(xC);
    else
      if not fStrictXML then
        aBuffer1.WriteOWideChar(xC)
      else
        aDTDReader.RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
          OXmlLng_InvalidCharacterInAttribute, ['0x'+IntToHex(Ord(xC), 4)]);
    end;
    aDTDReader.ReadNextChar(xC);
  end;
  aBuffer1.GetOWideString(xEntityValue{%H-});

  //add entity to entity list
  if not xIsParameterEntity then
  begin
    EntityList.AddOrReplace(xEntityName, xEntityValue);
  end;

  //find end of tag
  while (xC <> '>') and not aDTDReader.EOF do
    aDTDReader.ReadNextChar(xC);
end;

procedure TXMLReaderSettings.LoadDTDEntityReference(
  const aDTDReader: TOTextReader; const aWriteToBuffer, aTempBuffer: TOByteBuffer);
var
  xReadString, xEntityName, xEntityValue: OWideString;
begin
  if ProcessEntity(Self, aDTDReader, aTempBuffer, xReadString{%H-}, xEntityName{%H-}, xEntityValue{%H-}) then
    aWriteToBuffer.WriteOWideString(xEntityValue)
  else
  begin
    if fStrictXML then
    begin
      aDTDReader.RaiseExceptionFmt(TXMLParseErrorInvalidCharacter,
        OXmlLng_InvalidEntity, [xReadString]);
      Exit;
    end else
    begin
      aWriteToBuffer.WriteOWideString(xReadString);
    end;
  end;
end;

function TXMLReaderSettings.LoadDTDFromFile(const aFileName: OWideString;
  const aDefaultEncoding: TEncoding): Boolean;
var
  xFS: TOFileStream;
begin
  xFS := TOFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := LoadDTDFromStream(xFS, aDefaultEncoding);
  finally
    xFS.Free;
  end;
end;

function TXMLReaderSettings.LoadDTDFromStream(const aStream: TStream;
  const aDefaultEncoding: TEncoding): Boolean;
var
  xReader: TOTextReader;
begin
  xReader := TOTextReader.Create;
  try
    xReader.InitStream(aStream, aDefaultEncoding);
    xReader.ErrorHandling := Self.ErrorHandling;

    LoadDTD(xReader, False);
  finally
    Result := not Assigned(xReader.ParseError);

    xReader.Free;
  end;
end;

function TXMLReaderSettings.LoadDTDFromString(const aString: OWideString): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString(aString);

    Result := LoadDTDFromStream(xStream, TEncoding.OWideStringEncoding);
  finally
    xStream.Free;
  end;
end;

function TXMLReaderSettings.LoadDTDFromString_UTF8(
  const aString: OUTF8Container): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetString_UTF8(aString);

    Result := LoadDTDFromStream(xStream, TEncoding.UTF8);
  finally
    xStream.Free;
  end;
end;

function TXMLReaderSettings.LoadFromBuffer(const aBuffer;
  const aBufferLength: Integer; const aDefaultEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetPointer(@aBuffer, aBufferLength);

    Result := LoadDTDFromStream(xStream, aDefaultEncoding);
  finally
    xStream.Free;
  end;
end;

function TXMLReaderSettings.LoadFromBuffer(const aBuffer: TBytes;
  const aDefaultEncoding: TEncoding): Boolean;
var
  xStream: TVirtualMemoryStream;
begin
  xStream := TVirtualMemoryStream.Create;
  try
    xStream.SetBuffer(aBuffer);

    Result := LoadDTDFromStream(xStream, aDefaultEncoding);
  finally
    xStream.Free;
  end;
end;

{ TXMLReaderTokenList }

procedure TXMLReaderTokenList.AddLast;
begin
  Inc(fCount);
end;

procedure TXMLReaderTokenList.Clear;
begin
  fCount := 0;
end;

constructor TXMLReaderTokenList.Create;
begin
  inherited Create;

  {$IFDEF O_GENERICS}
  fReaderTokens := TList<PXMLReaderToken>.Create;
  {$ELSE}
  fReaderTokens := TList.Create;
  {$ENDIF}
end;

function TXMLReaderTokenList.CreateNew: PXMLReaderToken;
begin
  if fCount = fReaderTokens.Count then
  begin
    New(Result);
    fReaderTokens.Add(Result);
  end else
    Result := GetToken(fCount);
end;

procedure TXMLReaderTokenList.DeleteLast;
begin
  Dec(fCount);
  if fCount < 0 then
    fCount := 0;
end;

destructor TXMLReaderTokenList.Destroy;
var
  I: Integer;
begin
  for I := 0 to fReaderTokens.Count-1 do
    Dispose(PXMLReaderToken(fReaderTokens[I]));
  fReaderTokens.Free;

  inherited;
end;

function TXMLReaderTokenList.GetToken(const aIndex: Integer): PXMLReaderToken;
begin
  Result := PXMLReaderToken(fReaderTokens[aIndex]);
end;

function TXMLReaderTokenList.IndexOf(const aToken: PXMLReaderToken): Integer;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  if fReaderTokens[I] = aToken then
  begin
    Result := I;
    Exit;
  end;
  Result := -1;
end;

{ TXMLParseErrorInvalidCharacter }

function TXMLParseErrorInvalidCharacter.GetExceptionClass: EOTextReaderExceptionClass;
begin
  Result := EXMLReaderInvalidCharacter;
end;

{ TXMLParseErrorInvalidStructure }

function TXMLParseErrorInvalidStructure.GetExceptionClass: EOTextReaderExceptionClass;
begin
  Result := EXmlReaderInvalidStructure;
end;

{ EXmlReaderInvalidStructure }

class function EXmlReaderInvalidStructure.GetErrorCode: Integer;
begin
  Result := HIERARCHY_REQUEST_ERR;
end;

{ TXMLReaderEntityList }

procedure TXMLReaderEntityList.AddOrReplace(const aEntityName,
  aEntityValue: OWideString);
begin
  fList.Add(aEntityName, aEntityValue);
end;

procedure TXMLReaderEntityList.AssignTo(Dest: TPersistent);
var
  xDest: TXMLReaderEntityList;
begin
  if Dest is TXMLReaderEntityList then
  begin
    xDest := TXMLReaderEntityList(Dest);

    xDest.fList.Assign(Self.fList);
  end else
    inherited;
end;

procedure TXMLReaderEntityList.Clear;
begin
  fList.Clear;
  fList.Add('quot', '"');
  fList.Add('amp', '&');
  fList.Add('apos', '''');
  fList.Add('lt', '<');
  fList.Add('gt', '>');
end;

constructor TXMLReaderEntityList.Create;
begin
  inherited;

  fList := TOHashedStringDictionary.Create;
  Clear;
end;

destructor TXMLReaderEntityList.Destroy;
begin
  fList.Free;

  inherited;
end;

function TXMLReaderEntityList.Find(const aEntityName: OWideString;
  var outEntityValue: OWideString): Boolean;
begin
  Result := fList.TryGetValue(aEntityName, outEntityValue);
end;

function TXMLReaderEntityList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TXMLReaderEntityList.GetItem(
  const aIndex: OHashedStringsIndex): TOHashedStringDictionaryPair;
begin
  Result := fList.Pairs[aIndex];
end;

function TXMLReaderEntityList.GetName(
  const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fList.Keys[aIndex];
end;

function TXMLReaderEntityList.GetValue(
  const aIndex: OHashedStringsIndex): OWideString;
begin
  Result := fList.Values[aIndex];
end;

function TXMLReaderEntityList.IndexOf(const aEntityName: OWideString): Integer;
begin
  Result := fList.IndexOf(aEntityName);
end;

procedure TXMLReaderEntityList.SetValue(const aIndex: OHashedStringsIndex;
  const aValue: OWideString);
begin
  fList.Values[aIndex] := aValue;
end;

{ EXMLReaderInvalidCharacter }

class function EXMLReaderInvalidCharacter.GetErrorCode: Integer;
begin
  Result := INVALID_CHARACTER_ERR;
end;

{ TXMLWriterDeclaration }

constructor TXMLWriterDeclaration.Create;
begin
  inherited Create;

  Enabled := False;
end;

procedure TXMLWriterDeclaration.Write(const aWriter: TXMLWriter);
begin
  aWriter.XMLDeclaration(Encoding, Version, StandAlone);
end;

procedure TXMLWriterDeclaration.WriteIfEnabled(const aWriter: TXMLWriter);
begin
  if Enabled then
    Write(aWriter);
end;

{ TXMLWriterSettings }

constructor TXMLWriterSettings.Create(aOwner: TXMLWriter);
begin
  inherited Create;

  fOwner := aOwner;
end;

function TXMLWriterSettings.GetEncoding: TEncoding;
begin
  Result := fOwner.Encoding;
end;

function TXMLWriterSettings.GetOwnsEncoding: Boolean;
begin
  Result := fOwner.OwnsEncoding;
end;

function TXMLWriterSettings.GetWriteBOM: Boolean;
begin
  Result := fOwner.WriteBOM;
end;

procedure TXMLWriterSettings.SetEncoding(const aEncoding: TEncoding);
begin
  fOwner.Encoding := aEncoding;
end;

procedure TXMLWriterSettings.SetOwnsEncoding(const aOwnsEncoding: Boolean);
begin
  fOwner.OwnsEncoding := aOwnsEncoding;
end;

procedure TXMLWriterSettings.SetWriteBOM(const aWriteBOM: Boolean);
begin
  fOwner.WriteBOM := aWriteBOM;
end;

{ TXMLDocumentWriterSettings }

constructor TXMLDocumentWriterSettings.Create;
begin
  inherited Create;

  fWriteBOM := True;
end;

function TXMLDocumentWriterSettings.GetWriteBOM: Boolean;
begin
  Result := fWriteBOM;
end;

procedure TXMLDocumentWriterSettings.SetWriteBOM(const aWriteBOM: Boolean);
begin
  fWriteBOM := aWriteBOM;
end;

{ TOCharTable }

constructor TOCharTable.Create(const aEncoding: TEncoding);
var
  I: OUnicodeChar;
  {$IFDEF FPC}
  I_UTF8: string;
  {$ENDIF}
  xBytes: TEncodingBuffer;
  xString: OWideString;
begin
  inherited Create;

  for I := Low(I) to High(I) do
  begin
    {$IFDEF FPC}
    I_UTF8 := UTF8Encode(WideString(I));
    {$ENDIF}
    aEncoding.StringToBuffer({$ifdef FPC}I_UTF8{$else}I{$endif}, xBytes{%H-});
    aEncoding.BufferToString(xBytes, xString{%H-});
    fIsSupported[I] := (xString = {$ifdef FPC}I_UTF8{$else}I{$endif});
  end;
end;

function TOCharTable.Entity(const aChar: OUnicodeChar): OWideString;
begin
  Result := '&#'+IntToStr(Ord(aChar))+';'
end;

function TOCharTable.IsSupported(const aChar: OUnicodeChar): Boolean;
begin
  Result := fIsSupported[aChar];
end;

end.
