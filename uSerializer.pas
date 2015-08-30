unit uSerializer;

interface
uses
    System.SysUtils, System.Classes, System.Rtti, System.Types,
    Data.DBXJSON, Data.DBXJSONReflect,
    FMX.TreeView, SvCollections.Tries, System.JSON, System.IOUtils,
    uUnit;
type
    TSerializerType = (stJDO, stJSON);
    TSerializer = class
    public
        class function Marshal(const Value : TUnit) : string; virtual; abstract;
        class function MarshalToFile(const Value : TUnit; const AFileName : string) : string; virtual; abstract;
        class function UnMarshalFromFile(const AFileName : string) : TUnit; virtual; abstract;
        class function UnMarshal(const Value : string) : TUnit; virtual; abstract;
        class function SEARCH_PATTERN : string; virtual; abstract;
    end;
    TSerializerUtils = class
    public
        class function SaveString(const AString, AFileName : string) : Boolean;
        class function LoadString(const AFileName : string; var AString : string) : Boolean;
        class function FindFIle(const APathList : TArray<string>; AName, SearchPattern : string;
            out outName : string; var AWriteDate : TDateTime) : Boolean;
    end;

implementation
{------------------------------------------------------------------------------}
{ TSerializerUtils }
{------------------------------------------------------------------------------}
class function TSerializerUtils.FindFIle(const APathList: TArray<string>; AName,
  SearchPattern: string; out outName : string; var AWriteDate : TDateTime): Boolean;
var
    LPath, FoundFile : string;
    LFilesList : TStringDynArray;
begin
    Result := False;
    outName := EmptyStr;
    AWriteDate := MinDateTime;
    for LPath in APathList do
    begin
        if not TDirectory.Exists(LPath) then
            Continue;
        LFilesList := TDirectory.GetFiles(LPath,SearchPattern ,TSearchOption.soAllDirectories);
        for FoundFile in LFilesList do
        begin
            if CompareText(AName, TPath.GetFileNameWithoutExtension(FoundFile)) = 0 then
            begin
                outName := FoundFile;
                AWriteDate := TFile.GetLastWriteTime(FoundFile);
                Exit(True);
            end;
        end;
    end;
end;
{------------------------------------------------------------------------------}
class function TSerializerUtils.LoadString(const AFileName: string;
  var AString: string): Boolean;
function GetFIleStream(var AString : string; fileName : string) : Boolean;
var
    LSt : TStringList;
begin
    Result := false;
    if string.IsNullOrEmpty(fileName) then
        exit;
    try
        if TFile.Exists(fileName) then
        begin
            LSt := TStringList.Create;
            try
                LSt.LoadFromFile(fileName);
                AString := Lst.Text;
                Result := True;
            finally
                FreeAndNil(Lst);
            end;
        end;
    except
    end;
end;
begin
    Result := GetFIleStream(AString, AFileName);
end;
{------------------------------------------------------------------------------}
class function TSerializerUtils.SaveString(const AString,
  AFileName: string): Boolean;
    /// Сохранение потока в файл
    function SetFIleStream(var stream : TStringStream; fileName : string) : Boolean;
    var
        dirName, fName : string;
    begin
        Result := false;
        if fileName.IsEmpty then
            exit;
        dirName := System.IOUtils.TPath.GetDirectoryName(fileName);
        if ForceDirectories(dirName) then
        begin
            stream.SaveToFile(fileName);
            Result := True;
        end;
    end;
var
    LStream : TStringStream;
begin
    LStream := TStringStream.Create(AString);
    Result := SetFIleStream(LStream, AFileName);
    FreeAndNil(LStream);
end;
{------------------------------------------------------------------------------}
end.
