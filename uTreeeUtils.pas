unit uTreeeUtils;

interface
uses
    System.IOUtils, System.SysUtils, System.Generics.Collections,
    FMX.Types;
type
    TTreeUtils = class
    private
        FCancel : Boolean;
        FFiles : TArray<string>;
        FDirFunc : TFunc<string, string, TObject, TObject>;
        FFileProc : TProc<string, string, Integer, TObject>;
        function Stopped : Boolean; inline;
    public
        procedure BuildTreeFromFiles(AParent : TObject); overload;
        procedure BuildTreeFromFiles(AParent : TObject; AFiles : TArray<string>);
            overload;
    public
        class function GetTreeUtils(const AFiles : TArray<string>;
            ADirFunc : TFunc<string, string, TObject, TObject>;
            AFileProc : TProc<string, string, Integer, TObject>) : TTreeUtils;
    public
        property Cancel : Boolean read FCancel write FCancel;
        property DirFunc : TFunc<string, string, TObject, TObject>
            read FDirFunc write FDirFunc;
        property FileProc : TProc<string, string, Integer, TObject>
            read FFileProc write FFileProc;
    end;
implementation

{ TTreeUtils }

procedure TTreeUtils.BuildTreeFromFiles(AParent : TObject);
var
    lFullFileName, lFileName, lFullDirName, lDirName, lSplitDir : string;
    i, count : integer;
    lParent, item : TObject;
    lDirectories : TArray<string>;
    lDircDirectories : TDictionary<string,TObject>;
begin
    self.FCancel := False;
    lDircDirectories := TDictionary<string,TObject>.Create;
    try
        count := High(FFiles);
        for i := Low(FFiles) to count do
        begin
            if Stopped then
                exit;
            lFullFileName := FFiles[i];
            lFileName := system.IOUtils.TPath.GetFileName(lFullFileName);
            lFullDirName := system.IOUtils.TPath.GetDirectoryName(lFullFileName);

            lFullDirName := StringReplace(lFullDirName, System.IOUtils.TPath.AltDirectorySeparatorChar,
                System.IOUtils.TPath.DirectorySeparatorChar, [rfReplaceAll]);
            lParent := AParent;
            if not lFullDirName.IsEmpty then
            begin
                LDirectories := lFullDirName.Split(System.IOUtils.TPath.DirectorySeparatorChar);
                lDirName := string.Empty;
                for lSplitDir in LDirectories do
                begin
                    if Stopped then
                        exit;
                    case lDirName.IsEmpty of
                        True : lDirName := lSplitDir;
                        False : lDirName := IncludeTrailingPathDelimiter(lDirName) + lSplitDir;
                    end;
                    if not lDircDirectories.ContainsKey(lDirName) then
                    begin
                        item := DirFunc(lSplitDir, lDirName, lParent);
                        lDircDirectories.AddOrSetValue(lDirName, item);
                    end;
                    lParent := lDircDirectories.Items[lDirName];
                end;
            end;
            if lFileName.IsEmpty then
                Continue;
            FileProc(lFileName, lFullFileName, i, lParent);
        end;
    finally
        lDircDirectories.Free;
    end;
end;

procedure TTreeUtils.BuildTreeFromFiles(AParent: TObject;
  AFiles: TArray<string>);
begin
    self.FFiles := AFiles;
    Self.BuildTreeFromFiles(AParent);
end;

class function TTreeUtils.GetTreeUtils(const AFiles: TArray<string>;
  ADirFunc: TFunc<string, string, TObject, TObject>;
  AFileProc: TProc<string, string, Integer, TObject>): TTreeUtils;
begin
    Result := TTreeUtils.Create;
    Result.FFiles := AFiles;
    Result.FDirFunc := ADirFunc;
    Result.FFileProc := AFileProc;
    Result.FCancel := False;
end;

function TTreeUtils.Stopped: Boolean;
begin
    Result := FCancel or (not Assigned(DirFunc)) or
        (not Assigned(FileProc));
end;

end.
