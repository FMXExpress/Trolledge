unit uHelper.SyntaxNode;

interface
uses
    DelphiAST, DelphiAST.Classes, DelphiAST.Consts, DelphiAST.Writer,
    System.Classes, System.IOUtils, System.SysUtils, uSerializer;
type
    TSyntaxNodeHelper = class helper for TSyntaxNode
        function FindMethodFromLine(ALine : Integer) : TSyntaxNode;
        function FindMethodFromParent : TSyntaxNode;

        class function ParseFile(const AFIleName : string; const APlatform: string) : string;
    end;
implementation

{ TSyntaxNodeHelper }
{------------------------------------------------------------------------------}
function TSyntaxNodeHelper.FindMethodFromLine(ALine: Integer): TSyntaxNode;
var
    LChildNode, tempNode : TSyntaxNode;
begin
    Result := nil;
    if Line = ALine then
        Result := Self
    else
        for LChildNode in ChildNodes do
        begin
            tempNode := LChildNode.FindMethodFromLine(ALine);
            if Assigned(tempNode) then
            begin
                Result := tempNode.FindMethodFromParent;
                Break;
            end;
        end;
end;
{------------------------------------------------------------------------------}
function TSyntaxNodeHelper.FindMethodFromParent: TSyntaxNode;
var
    LIsFind : Boolean;
    LParentNode, tempNode : TSyntaxNode;
begin
    Result := nil;
    LParentNode := self;
    while Assigned(LParentNode) do
    begin
        if LParentNode.Typ <> TSyntaxNodeType.ntMethod then
            LParentNode := LParentNode.ParentNode
        else
        begin
            Result := LParentNode;
            Break;
        end;

    end;
end;
{------------------------------------------------------------------------------}
class function TSyntaxNodeHelper.ParseFile(const AFileName: string; const APlatform: String): string;
var
  ASTBuilder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  SyntaxTree: TSyntaxNode;
  LStream : TStringStream;
  LData : string;
begin
    Result := '';
    if TSerializerUtils.LoadString(AFileName, LData) then
    begin
        StringStream := TStringStream.Create(LData, TEncoding.Unicode);
        try
            StringStream.Position := 0;
            ASTBuilder := TPasSyntaxTreeBuilder.Create;
            try
                ASTBuilder.AddDefine(APlatform);
                ASTBuilder.InitDefinesDefinedByCompiler;
                SyntaxTree := ASTBuilder.Run(StringStream);
                try
                    Result := TSyntaxTreeWriter.ToXML(SyntaxTree, True);
                finally
                    SyntaxTree.Free;
                end;
            finally
              ASTBuilder.Free;
            end;
        finally
            StringStream.Free;
        end;
    end;
end;
{------------------------------------------------------------------------------}
end.
