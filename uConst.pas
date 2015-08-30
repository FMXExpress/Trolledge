unit uConst;

interface
uses
    System.Classes;
const
    CASHE_PATH : string = 'Cache';
    PAS_FILE : string = '.pas';
    SEARCH_PAS : string = '*.pas';
    JSON_FILE : string = '.json';
type
    TConst = class
    public
        class function DELIMITER : Char;
        class function SPACE_REPLACE : Char;
        class function GUID_CLASS_HELPER : string;
        class function PASCAL_CONST : TArray<string>;
        class function OPEN : Char;
        class function CLOSE : Char;
        class function TEMP : Char;
        class function AST_STRING_BUG : string;
        class function AST_STRING_FIX : string;
    end;

implementation

{ TConst }

class function TConst.AST_STRING_BUG: string;
begin
   Result := 'string.string';
end;

class function TConst.AST_STRING_FIX: string;
begin
   Result := 'string';
end;

class function TConst.CLOSE: Char;
begin
   Result := ')';
end;

class function TConst.DELIMITER: Char;
begin
    Result := '^';
end;

class function TConst.GUID_CLASS_HELPER: string;
begin
    Result := '{B27E58E2-635E-43EC-A515-2816E810056C}';
end;

class function TConst.OPEN: Char;
begin
    Result := '(';
end;

class function TConst.PASCAL_CONST: TArray<string>;
begin
    Result :=
        ['Absolute',
        'And',
        'Array',
        'As',
        'Asm',
        'Automated',
        'Begin',
        'Case',
        'Class',
        'Const',
        'Constructor',
        'Destructor',
        'Dispinterface',
        'Div',
        'Do',
        'Downto',
        'Else',
        'End',
        'Except',
        'Exports',
        'File',
        'Final',
        'Finalization',
        'Finally',
        'For',
        'Function',
        'Goto',
        'If',
        'Implementation',
        'In',
        'Inherited',
        'Initialization',
        'Inline',
        'Interface',
        'Is',
        'Label',
        'Library',
        'Mod',
        'Not',
        'Object',
        'Of',
        'On',
        'Or',
        'Operator',
        'Out',
        'Packed',
        'Private',
        'Procedure',
        'Program',
        'Property',
        'Protected',
        'Public',
        'Published',
        'Raise',
        'Record',
        'Repeat',
        'Resourcestring',
        'Sealed',
        'Set',
        'Shl',
        'Shr',
        'Static',
        'Strict',
        'String',
        'Then',
        'Threadvar',
        'To',
        'Try',
        'Type',
        'Unit',
        'Unsafe',
        'Until',
        'Uses',
        'Var',
        'While',
        'With',
        'Xor',
        'ABSTRACT',
        'ASSEMBLER',
        'AUTOMATED',
        'CDECL',
        'DELAYED',
        'DEPRECATED',
        'DISPID',
        'DYNAMIC',
        'EXPORT',
        'EXTERNAL',
        'FAR',
        'FORWARD',
        'MESSAGE',
        'NEAR',
        'OVERRIDE',
        'OVERLOAD',
        'PASCAL',
        'PLATFORM',
        'REGISTER',
        'REINTRODUCE',
        'RESIDENT',
        'SAFECALL',
        'STDCALL',
        'VARARGS',
        'VIRTUAL',
        'Contains',
        'Default',
        'Experimental',
        'Implements',
        'Index',
        'Local',
        'Name',
        'Nodefault',
        'Package',
        'Read',
        'Readonly',
        'Requires',
        'Resident',
        'Stored',
        'Write',
        'Writeonly']
end;

class function TConst.SPACE_REPLACE: Char;
begin
    Result := '~';
end;

class function TConst.TEMP: Char;
begin
    Result := '?';
end;

end.
