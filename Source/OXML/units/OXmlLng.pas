unit OXmlLng;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{
  OXmlLng.pas

  Language definitions for OXml library.

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

{$IFDEF O_DELPHI_5_DOWN}
uses OWideSupp;
{$ENDIF}

var
  OXmlLng_InvalidCData: string = '"%s" is not a valid CData text.';
  OXmlLng_InvalidText: string = '"%s" is not a valid text.';
  OXmlLng_InvalidComment: string = '"%s" is not a valid comment text.';
  OXmlLng_InvalidPITarget: string = '"%s" is not a valid processing instruction target.';
  OXmlLng_InvalidPIContent: string = '"%s" is not a valid processing instruction content.';
  OXmlLng_InvalidAttributeName: string = '"%s" is not a valid attribute name.';
  OXmlLng_InvalidElementName: string = '"%s" is not a valid element name.';
  OXmlLng_InvalidCharacterInText: string = 'The character "%s" cannot occur in text.';
  OXmlLng_InvalidCharacterInAttribute: string = 'The character "%s" cannot occur in attribute.';
  OXmlLng_InvalidStringInText: string = 'The string "%s" cannot occur in text.';
  OXmlLng_InvalidCharacterInElement: string = 'The character "%s" cannot occur in element header.';
  OXmlLng_InvalidAttributeStartChar: string = 'An attribute cannot start with the character "%s".';
  OXmlLng_EqualSignMustFollowAttribute: string = 'Equal sign must follow the attribute "%s".';
  OXmlLng_AttributeValueMustBeEnclosed: string = '"%s" attribute value must be enclosed in quotes.';
  OXmlLng_DTDEntityValueMustBeEnclosed: string = '"%s" entity value must be enclosed in quotes.';
  OXmlLng_TooManyElementsClosed: string = 'Too many elements closed.';
  OXmlLng_UnclosedElementsInTheEnd: string = 'There are unclosed elements in the document end.';
  OXmlLng_WrongElementClosed: string = 'Trying to close wrong element. Close="%s", open element="%s".';
  OXmlLng_InvalidEntity: string = '"%s" is not a valid entity.';
  OXmlLng_EntityNameNotFound: string = 'The entity name "%s" was not found.';

  OXmlLng_XPathPredicateNotSupported: string = 'XPath predicate "%s" is not supported.'+sLineBreak+'XPath: %s';
  OXmlLng_XPathPredicateNotValid: string = 'XPath predicate "%s" is not valid.'+sLineBreak+'XPath: %s';
  OXmlLng_XPathNotSupported: string = 'XPath is not supported.'+sLineBreak+'XPath: %s';

  OXmlLng_AppendFromDifferentDocument: string = 'You can''t append a node from a different XML document.';
  OXmlLng_InsertFromDifferentDocument: string = 'You can''t insert a node from a different XML document.';
  OXmlLng_ExchangeFromDifferentDocument: string = 'You can''t exchange nodes from different XML documents.';
  OXmlLng_InsertEqualNodes: string = 'Node to insert and reference node can''t be equal.';
  OXmlLng_ParentNodeCantBeNil: string = 'Parent node can''t be nil.';
  OXmlLng_ParentNodeMustBeNil: string = 'Parent node must be nil.';
  OXmlLng_NodeToDeleteNotAChild: string = 'You can''t delete a node that is not a child of current node.';
  OXmlLng_NodeToInsertNotAChild: string = 'You can''t insert node before a node that is not a child of current node.';
  OXmlLng_NodeMustBeDOMDocumentOrElement: string = 'Node must be a DOMDocument or an element.';
  OXmlLng_CannotSetText: string = 'You can''t set the text property of this node. Use NodeValue instead.';
  OXmlLng_ChildNotFound: string = 'Child not found.';
  OXmlLng_NodeNotFound: string = 'Node not found.';
  OXmlLng_ListIndexOutOfRange: string = 'List index out of range.';
  OXmlLng_FeatureNotSupported: string = 'This feature is not supported.';
  OXmlLng_CannotWriteAttributesWhenFinished: string = 'You can''t add an attribute %s="%s" when the element header ("%s") has been finished.';
  OXmlLng_CannotSetIndentLevelAfterWrite: string = 'You can''t set the IndentLevel after something has been already written to the document.';
  OXmlLng_NodeNameCannotBeEmpty: string = 'Node name cannot be empty.';
  OXmlLng_XPathCannotBeEmpty: string = 'XPath cannot be empty.';
  OXmlLng_PathCannotBeEmpty: string = 'Path cannot be empty.';
  OXmlLng_CannotWriteToVirtualMemoryStream: string = 'You cannot write to a TVirtualMemoryStream.';
  OXmlLng_SetNodeNameWrongType: string = 'Changing node names is supported only for element and PI nodes.';

  OXmlLng_CannotChangeRootNodeName: string = 'Cannot change RootNodeName after data has been written.';
  OXmlLng_CannotChangeUseRootDataWritten: string = 'Cannot change UseRoot after data has been written.';
  OXmlLng_CannotChangeUseRootDataRead: string = 'Cannot change UseRoot after data has been read.';
  OXmlLng_WrongDeserializerSequence: string = 'TXMLDeserializer: wrong ReadObjectInfo/ReadObject call sequence.';
  OXmlLng_RTTIInternalError: string = 'RTTI Internal Error';
  OXmlLng_DeserializerNotSupportedListItemType: string = 'TXMLRTTIDeserializer: the type %s is not supported for TList<T> or array enumeration.';
  OXmlLng_DeserializerRegisterClass: string = 'TXMLRTTIDeserializer: you have to register the class %s with the RegisterClass method.';

  OXmlLng_CannotUndo2Times: string = 'Unsupported: you tried to run the undo function two times in a row.';
  OXmlLng_ReadingAt: string = 'Reading at:';
  OXmlLng_URL: string = 'URL: %s';
  OXmlLng_LineCharPosSourceStub: string =
    'Line: %d'+sLineBreak+
    'Char: %d'+sLineBreak+
    'Position in source stub: %d'+sLineBreak+
    'Source stub:'+sLineBreak+
    '%s';

  OXmlLng_FileNotFound: string = 'The file "%s" was not found.';

  OXmlLng_JSON_CloseTooMany: string = 'You try to close too many objects or arrays.';
  OXmlLng_JSON_CannotOpenObject: string = 'You cannot open object in this state.';
  OXmlLng_JSON_CannotCloseObject: string = 'You cannot close object in this state.';
  OXmlLng_JSON_CannotOpenArray: string = 'You cannot open array in this state.';
  OXmlLng_JSON_CannotCloseArray: string = 'You cannot close array in this state.';
  OXmlLng_JSON_CannotWriteValue: string = 'You cannot write a value in this state.';
  OXmlLng_JSON_CannotWritePair: string = 'You cannot write a name-value pair in this state.';

implementation

end.
