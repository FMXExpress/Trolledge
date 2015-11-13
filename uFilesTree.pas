unit uFilesTree;
//{$DEFINE ONLYTOP}
interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Styles,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TreeView, FMX.Platform,
  FMX.Objects, System.IOUtils, System.StrUtils, System.Generics.Collections,
  FMX.ExtCtrls, FMX.Dialogs, System.Rtti, System.Threading, System.DateUtils,
  uTreeeUtils;

type
  TProtoFilesTree = class
  private
  const
    S_MAIN_ITEM : Integer = 123;
  private
    FTreeView: TTreeView;
    FFilesList: TStringDynARray;
    FFilesPatternList: TStringList;
    FIncludeDirList: TStringList;
    FExcludeDirlist: TStringList;
    FRtlDirList: TStringList;
    FCaseSensitive: boolean;

    FTasks : array[0..0] of ITask;
    FTreeUtils : TTreeUtils;
    FSearchString : string;
    FDirName : string;
    FSearchFiles : TList<string>;
    FTotalFiles: integer;
    FCountFiles: integer;
    FProgress: integer;
    FSearchRunning: boolean;
    FSearchCancel: boolean;
    FOnBeginEnumDir : TNotifyEvent;
    FOnEndENumDir : TNotifyEvent;
    function IsMathFileName(AFileName: string): boolean; inline;
    function IsDirExcludes(ADirName: string): boolean; inline;
    function IsDirIncludes(ADirName: string): boolean; inline;
    procedure AddSearchLine(AFoundLine: String; AFoundString: String; AParentItem: TTreeViewItem);
    procedure EnumDirSearch(ASearchString: string; ADirName: string);
    procedure InternalEnumDirSearch(ASearchString: string; ADirName: string; AParentItem: TTreeViewItem);
    procedure InternalEnumFilesList(AParentItem: TTreeViewItem);
    procedure TreeViewItemApplyStyleLookup(Sender: TObject);

    procedure SetOptionsList(AValues: string; AList: TStringList);
    function GetFilesPattern: string;
    procedure SetFilesPattern(Value: string);
    function GetIncludeFolders: string;
    procedure SetIncludeFolders(Value: string);
    function GetExcludeFolders: string;
    procedure SetExcludeFolders(Value: string);
    function GetRtlPathes: string;
    procedure SetRtlPathes(Value: string);
    procedure DoOnBeginEnumDir;
    procedure DoOnEndEnumDir;
    procedure EnumFolder(Sender : TObject);
    procedure AsynchEnumDir(ADirName : string; AItem : TTreeViewItem);
    //------------------------------------- FSearch ----------------------------
    procedure AsynchEnumDirSearch(ASearchString: string; ADirName: string);
    procedure BeginSearch;
    procedure StepSearch; inline;
    procedure EndSearch;
    procedure StopSearch;
    procedure SetSearchString(const Value: string);
    procedure OnTerminate(Sender : TObject);
    procedure AddFile(AFileName : string);
    procedure EnumFolderSearch(Sender : TObject);
    procedure CreateFileItem(AName, AFullName : string; AParent : TObject); inline;
    procedure CreateTreeUtils;
    procedure AddTask(AProc : TProc);
    function SearchFilter(AFileName : string) : Boolean; inline;
    function DirFilter(const Path: string; const SearchRec: TSearchRec): boolean; inline;
    function FileFilter(const Path: string; const SearchRec: TSearchRec): boolean; inline;
    function GetTask: ITask;

    procedure SetTask(const Value: ITask);
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure Clear;
    procedure ExpandAll;
    procedure EnumDir(ADirName: string);
    procedure TaskEnumDirSearch(ASearchString: string; ADirName: string);
    procedure ClearSearchOptions;
    procedure InternalEnumDir(ADirName: string; AParentItem: TTreeViewItem);
    function  GetFilesList: TStringDynArray;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property FilesPattern: string read GetFilesPattern write SetFilesPattern;
    property IncludeFolders: string read GetIncludeFolders write SetIncludeFolders;
    property ExcludeFolders: string read GetExcludeFolders write SetExcludeFolders;
    property PathToRTL: string read GetRtlPathes write SetRtlPathes;
    property SearchRunning: boolean read FSearchRunning;
  public
    property OnBeginEnumDir : TNotifyEvent read FOnBeginEnumDir write FOnBeginEnumDir;
    property OnEndEnumDir : TNotifyEvent read FOnEndENumDir write FOnEndENumDir;
    property SearchString : string read FSearchString write SetSearchString;
    property Task : ITask read GetTask write SetTask;
  end;
  TPathItem = class
  private
    FItems : TObjectList<TPathItem>;
    function GetIsDirectory: Boolean;
  public
    FName : string;
    FFullName : string;
  public
    constructor Create; overload;
    constructor Create(AName : string; AFullname : string = ''); overload;
    destructor Destroy; override;
    function Add(AName : string; AFullname : string = '') : TPathItem;
  public
    property Items : TObjectList<TPathItem> read FItems;
    property IsDirectory : Boolean read GetIsDirectory;
  end;
  TreeViewItemPath = class(TTreeViewItem)
  public
    FPath : string;
    FPathItem : TPathItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OnFolderClick(Sender : TObject);
    procedure CreateFakeItem(AEnumDir : TNotifyEvent); inline;
    procedure FolderApplyStyleLookup(Sender: TObject);
    function FoundFakeItem(var AEvent : TNotifyEvent) : Boolean;
  public
    class function CreateDir(AOwner, AParent : TFmxObject; ADirName : string;
        AEnumDirSearch : TNotifyEvent) : TreeViewItemPath; inline;
    property PathItem : TPathItem read FPathItem;
  end;
const
    FAKETEXT = 'è½|';
implementation

{ TProtoFilesTree }

uses uConsts, uMain, uFileTypeHelper;

constructor TProtoFilesTree.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FFilesPatternList := TStringList.Create;
  FIncludeDirList := TStringList.Create;
  FExcludeDirlist := TStringList.Create;
  FRtlDirList := TStringList.Create;
  FCaseSensitive := False;

  FSearchString := string.Empty;
  FDirName := string.Empty;
  Task := nil;
  FSearchFiles := TList<string>.Create;
  CreateTreeUtils;
  FTotalFiles := 0;
  FCountFiles := 0;
  FSearchRunning := False;
  FSearchCancel := False;
end;

procedure TProtoFilesTree.CreateFileItem(AName, AFullName: string;
  AParent: TObject);
var
    lItem : TTreeViewItem;
    FoundCount, i : Integer;
    lBodyFile : TStringList;
    lIsFound : Boolean;
begin
    if FSearchCancel then
        exit;
    lItem := TTreeViewItem.Create(FTreeView);
    lItem.Data := nil;
    lItem.TagString := AFullName;
    lItem.Text := AName;
    lItem.TagFloat := S_MAIN_ITEM;
    lBodyFile := TStringList.Create;
    try
        lBodyFile.Text := TFile.ReadAllText(AFullName);
        if not FCaseSensitive then
            lBodyFile.Text := lBodyFile.Text.ToUpper;
        FoundCount := 0;
        for I := 0 to lBodyFile.Count - 1 do
        begin
            if FSearchCancel then
                exit;
            lIsFound := lBodyFile[I].IndexOf(FSearchString) > -1;
            if lIsFound then
            begin
                Inc(FoundCount);
                if FoundCount<=10 then
                    AddSearchLine(IntToStr(I), lBodyFile[I], lItem);
            end;
            Application.ProcessMessages;
        end;
    finally
        FreeAndNil(lBodyFile);
    end;
    lItem.Tag := FoundCount;
    lItem.StyleLookup := 'TreeViewItemDetailStyle';
    lItem.OnApplyStyleLookup :=
      TreeViewItemApplyStyleLookup;
    lItem.Parent := AParent as TFmxObject;
    lItem.NeedStyleLookup;
    StepSearch;
end;

procedure TProtoFilesTree.CreateTreeUtils;
var
    lDirFunc : TFunc<string, string, TObject, TObject>;
    lFileProc : TProc<string, string, Integer, TObject>;
begin
    lDirFunc :=
        function (ADirName, AFullDirName : string; AParent : TObject) : TObject
        var
            lResult : TObject;
        begin
            if FSearchCancel then
                Exit;
            if (not AFullDirName.Contains(FDirName))then
                Exit(FTreeView);
            TThread.Synchronize(
                nil,
                procedure
                begin
                {$IFDEF ONLYTOP}
                    if AParent is TPathItem then
                        lResult := TPathItem(AParent).Add(ADirName)
                    else
                {$ENDIF}
                        lResult := TreeViewItemPath.CreateDir(FTreeView,
                            AParent as TFmxObject, ADirName, self.EnumFolderSearch){$IFDEF ONLYTOP}.PathItem{$ENDIF};
                    {lItem := TTreeViewItem.Create(FTreeView);
                    lItem.Parent := AParent as TFmxObject;
                    lItem.Data := nil;
                    lItem.Text := ADirName; }
                end
            );
            Result := lResult;
        end;
    lFileProc :=
        procedure (AFileName, AFullFileName : string; AIndex : Integer; AParent : TObject)
        begin
            if FSearchCancel then
                exit;
            TThread.Synchronize(
                nil,
                procedure
                begin
                {$IFDEF ONLYTOP}
                    if AParent is TPathItem then
                        TPathItem(AParent).Add(AFileName, AFullFileName)
                    else
                {$ENDIF}
                        CreateFileItem(AFileName, AFullFileName, AParent);
                end
            );
        end;
        self.FTreeUtils := TTreeUtils.GetTreeUtils([], lDirFunc, lFileProc);
end;

destructor TProtoFilesTree.Destroy;
begin
  StopSearch;
  FFilesPatternList.Free;
  FIncludeDirList.Free;
  FExcludeDirlist.Free;
  FRtlDirList.Free;
  FreeAndNil(FSearchFiles);
  {Task.Cancel;
  TTask.WaitForAll(FTasks);
    TThread.Synchronize(
        nil,
        procedure
        begin
            self.FSearchRunning := False;
            if Task <> nil then
            begin
                //Task.Cancel;
                TTask.WaitForAny(FTasks);
            end;
        end
    );}
  FreeAndNil(FTreeUtils);
  inherited;
end;

function TProtoFilesTree.DirFilter(const Path: string;
  const SearchRec: TSearchRec): boolean;
begin
  if IsDirExcludes(SearchRec.Name) then
    Exit(False)
  else
    Result := IsDirIncludes(SearchRec.Name);
end;

procedure TProtoFilesTree.DoOnBeginEnumDir;
begin
    if Assigned(OnBeginEnumDir) then
        OnBeginEnumDir(self);
end;

procedure TProtoFilesTree.DoOnEndEnumDir;
begin
    if Assigned(OnEndEnumDir) then
        OnEndEnumDir(self);
end;

procedure TProtoFilesTree.Clear;
begin
    StopSearch;
end;

procedure TProtoFilesTree.ClearSearchOptions;
begin
  FFilesPatternList.Clear;
  FIncludeDirList.Clear;
  FExcludeDirlist.Clear;
end;

procedure TProtoFilesTree.ExpandAll;
begin
  FTreeView.BeginUpdate;
  try
    FTreeView.ExpandAll;
  finally
    FTreeView.EndUpdate;
  end;
end;

function TProtoFilesTree.FileFilter(const Path: string;
  const SearchRec: TSearchRec): boolean;
var
  Percent: integer;
begin
  Inc(FCountFiles);
  Percent := Trunc((FCountFiles / FTotalFiles) * 100);
  if Percent <> FProgress then
  begin
    FProgress := Percent;
    frmMain.SearchProgressBar.Value := FProgress;
    Application.ProcessMessages;
  end;
  Result := IsMathFileName(SearchRec.Name);
end;

function TProtoFilesTree.GetExcludeFolders: string;
begin
  Result := String.Join(',', FExcludeDirlist.ToStringArray);
end;

function TProtoFilesTree.GetFilesList: TStringDynArray;
begin
  SetLength(FFilesList, 0);
  InternalEnumFilesList(FTreeView.Items[1]);
  Result := FFilesList;
end;

function TProtoFilesTree.GetFilesPattern: string;
begin
  Result := String.Join(CSearchFoldersDelim, FFilesPatternList.ToStringArray);
end;

function TProtoFilesTree.GetIncludeFolders: string;
begin
  Result := String.Join(CSearchFoldersDelim, FIncludeDirList.ToStringArray);
end;

function TProtoFilesTree.GetRtlPathes: string;
begin
  Result := String.Join(CSearchFoldersDelim, FRtlDirList.ToStringArray) ;
end;

function TProtoFilesTree.GetTask: ITask;
begin
    Result := FTasks[0];
end;

procedure TProtoFilesTree.EnumDirSearch(ASearchString: string; ADirName: string);
var
  FilesArray: TStringDynArray;
begin
  frmMain.SearchProgressBar.Value := 0;
  frmMain.SearchProgressBar.Repaint;

  Application.ProcessMessages;
  FTreeView.BeginUpdate;
  try
    FTreeView.Clear;
    FilesArray := TDirectory.GetFiles(ADirName, CAllFileMask, TSearchOption.soAllDirectories);
    if (FilesArray <> nil) then
    begin
      FTotalFiles := Length(FilesArray);
      FCountFiles := 0;
      FProgress := 0;
      InternalEnumDirSearch(ASearchString, ADirName, nil);
    end;
  finally
    frmMain.SearchProgressBar.Value := 0;
    frmMain.SearchProgressBar.Repaint;
    FTreeView.EndUpdate;
  end;
  Application.ProcessMessages;
end;

procedure TProtoFilesTree.EnumFolder(Sender: TObject);
var
    lItem : TreeViewItemPath;
    lPath : string;
begin
    lItem := Sender as TreeViewItemPath;
    if not Assigned(lItem) then
        exit;
    lPath := lItem.FPath;
    self.AsynchEnumDir(lPath, LItem);
end;

procedure TProtoFilesTree.EnumFolderSearch(Sender: TObject);
var
    lItem, lNewItem : TreeViewItemPath;
    lPath : string;
    lPathItem, lMainPathItem : TPathItem;
begin
    lItem := Sender as TreeViewItemPath;
    if not Assigned(lItem) then
        exit;
    //lPath := lItem.FPath;
    //self.AsynchEnumDir(lPath, LItem);
    lItem.BeginUpdate;
    try
        lMainPathItem := lItem.TagObject as TPathItem;
        for lPathItem in lMainPathItem.Items do
            if lPathItem.IsDirectory then
            begin
                lNewItem := TreeViewItemPath.CreateDir(FTreeView,
                    lItem, lPathItem.FName, self.EnumFolderSearch);
                lNewItem.TagObject := lPathItem;
            end
            else
                self.CreateFileItem(lPathItem.FName, lPathItem.FFullName, lItem)
    finally
        lItem.EndUpdate;
        lItem.Expand;
    end;
end;

procedure TProtoFilesTree.EndSearch;
begin
    TThread.Synchronize(nil,
        procedure
        begin
            frmMain.SearchProgressBar.Value := 0;
            frmMain.SearchProgressBar.Repaint;
            if FSearchCancel then
                FTreeView.Clear;
            FTotalFiles := 0;
            FCountFiles := 0;
            FSearchRunning := False;
        end
    );
end;

procedure TProtoFilesTree.EnumDir(ADirName: string);
begin
    self.AsynchEnumDir(ADirName, FTreeView.Items[1]);
end;

procedure TProtoFilesTree.AddFile(AFileName: string);
begin
    Inc(FTotalFiles);
    FSearchFiles.Add(AFileName);
end;

function TProtoFilesTree.SearchFilter(AFileName : string) : Boolean;
var
    lSearchFile, lSplitDir, lFullDirName : string;
    FT : TFileType;
    lDirectories : TArray<string>;
    SearchRec: TSearchRec;
begin
    if FSearchCancel then
        Exit(False);
    try
        //checktemp
        FT := FT.FileType(AFileName);
        if FT <> TFileType.ftScript then
            Exit(False);
        //check dir
        lFullDirName := system.IOUtils.TPath.GetDirectoryName(AFileName);

        lFullDirName := StringReplace(lFullDirName, System.IOUtils.TPath.AltDirectorySeparatorChar,
            System.IOUtils.TPath.DirectorySeparatorChar, [rfReplaceAll]);
        LDirectories := lFullDirName.Split(System.IOUtils.TPath.DirectorySeparatorChar);
        for lSplitDir in LDirectories do
        begin
            SearchRec.Name := lSplitDir;
            if not DirFilter(lSplitDir, SearchRec) then
                Exit(False);
        end;
        //check pattern files
        if not IsMathFileName(TPath.GetFileName(AFileName))then
            Exit(False);
        //check contains FSearchString
        lSearchFile := TFile.ReadAllText(AFileName);
        if not FCaseSensitive then
            lSearchFile := lSearchFile.ToUpper;
    except
        on E: Exception do
        begin
         // do nothing
        end;
    end;
    Result := lSearchFile.Contains(FSearchString);
    if Result then
        AddFile(AFileName);
end;

procedure TProtoFilesTree.AddSearchLine(AFoundLine: String; AFoundString: String; AParentItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
begin
  LItem := TTreeViewItem.Create(FTreeView);
  if AParentItem = nil then
    LItem.Parent := FTreeView
  else
    Litem.Parent := AParentItem;
  LItem.Data := nil;
  LItem.TagString := AFoundString;
  LItem.TagFloat := AFoundLine.ToSingle;
  LItem.Tag := 1;
  LItem.Text := AFoundString;
end;

procedure TProtoFilesTree.AddTask(
  AProc : TProc);
begin
   if Assigned(Task) and (Task.Status = TTaskStatus.Running) then
      Task.Cancel;
    Task := TTask.Create(AProc);
    Task.Start;
end;

procedure TProtoFilesTree.AsynchEnumDir(ADirName: string; AItem: TTreeViewItem);
var
  I: Integer;
  lThread : TThread;
begin
    lThread := TThread.CreateAnonymousThread(
        procedure
        var
            I, lIndex: Integer;
            item, mainItem : TTreeViewItem;
            lParent : TFmxObject;
        begin
            TThread.Synchronize(nil,
                procedure
                begin
                    mainItem := TTreeViewItem.Create(FTreeView);
                    DoOnBeginEnumDir;
                    Application.ProcessMessages;
                    FTreeView.BeginUpdate;
                    try
                        while AItem.Count > 0 do
                        begin
                            item := AItem.Items[0];
                            item.Parent := mainItem;
                        end;
                    finally
                        FTreeView.EndUpdate;
                    end;
                end);
            try
                while mainItem.Count > 0 do
                begin
                    item := mainItem.Items[0];
                    FreeAndNil(item);
                end;
                InternalEnumDir(ADirName, mainItem);
            finally
                TThread.Synchronize(nil,
                    procedure
                    var
                        i : integer;
                    begin
                        DoOnEndEnumDir;
                        frmMain.ProgressBar1.Visible := False;
                        FTreeView.BeginUpdate;
                        try
                            for i := 0 to mainItem.Count - 1 do
                            begin
                                mainItem.Items[0].Parent := AItem;
                            end;
                        finally
                            FTreeView.EndUpdate;
                            AItem.Expand;
                            FreeAndNil(mainItem);
                        end;
                    end);
            end;
        end
    );
    lThread.FreeOnTerminate := True;
    lThread.Start;
end;

procedure TProtoFilesTree.AsynchEnumDirSearch(ASearchString, ADirName: string);
begin
    BeginSearch;
    SearchString := ASearchString.Trim;
    FDirName := IncludeTrailingPathDelimiter(ADirName.Trim);
    AddTask
    (
        procedure
        var
            FilesArray: TStringDynArray;
        begin
            try
                FilesArray := TDirectory.GetFiles(ADirName, CAllFileMask, TSearchOption.soAllDirectories,
                    function(const Path: string;
                          const SearchRec: TSearchRec): Boolean
                    var
                        lFile : string;
                    begin
                        if FSearchCancel then
                            Exit;
                        lFile := IncludeTrailingPathDelimiter(Path) + SearchRec.Name;
                        SearchFilter(lFile);
                    end);
                if FSearchCancel then
                    Exit;
                FTreeUtils.BuildTreeFromFiles(FTreeView, Self.FSearchFiles.ToArray);
            finally
                EndSearch;
            end;
        end
    );
end;

procedure TProtoFilesTree.BeginSearch;
begin
    FSearchCancel := False;
    FSearchRunning := True;
    frmMain.SearchProgressBar.Value := 0;
    frmMain.SearchProgressBar.Repaint;
    FTotalFiles := 0;
    FCountFiles := 0;
    FProgress := 0;
    FTreeView.Clear;
    self.FSearchFiles.Clear;
    Application.ProcessMessages;
end;

procedure TProtoFilesTree.TaskEnumDirSearch(ASearchString, ADirName: string);
var
  Pool: TThreadPool;
begin
  if FSearchRunning then
    Exit;
  AsynchEnumDirSearch(ASearchString, ADirName);
  {FSearchCancel := False;
  FSearchRunning := True;
  EnumDirSearch(ASearchString, ADirName);
  FSearchRunning := False; }
 end;

procedure TProtoFilesTree.TreeViewItemApplyStyleLookup(Sender: TObject);
var
  LItem : TTreeViewItem;
  FoundCount : Integer;
  detailVisible : boolean;
begin
    LItem := Sender as TTreeViewItem;
    if not Assigned(LItem) then
        exit;
    FoundCount := LItem.Tag;
    detailVisible := (LItem.TagFloat = S_MAIN_ITEM) or (FoundCount>10);
    LItem.StylesData['detailvalue.visible'] :=
      TValue.From<Boolean>(detailVisible);
    LItem.StylesData['detailvalue.width'] :=
      TValue.From<Single>(IntToStr(FoundCount).Length*5 + 20);
    LItem.StylesData['detaillabel.text'] :=
      TValue.From<String>(IntToStr(FoundCount));
end;

procedure TProtoFilesTree.InternalEnumDirSearch(ASearchString: string; ADirName: string; AParentItem: TTreeViewItem);
var
  SPath, SFile: string;
  n: integer;
  DirList: TStringDynArray;
  FilesList: TStringDynArray;
  LItem: TTreeViewItem;
  SL: TStringList;
  I: Integer;
  FoundCount: Integer;
  ASearch, ASearchFile: String;
begin
  if FSearchCancel then
    Exit;

  try
    // Enum Subfolders
    DirList := TDirectory.GetDirectories(ADirName, CAllFileMask,
      TSearchOption.soTopDirectoryOnly, DirFilter);
    if Length(DirList) > 0 then
    begin
      for SPath in DirList do
      begin
        if FSearchCancel then
          Break;
        LItem := TTreeViewItem.Create(FTreeView);
        if AParentItem = nil then LItem.Parent := FTreeView
          else Litem.Parent := AParentItem;
        LItem.Data := nil;
        n := SPath.LastDelimiter(TPath.DirectorySeparatorChar);
        LItem.Text := SPath.Substring(n + 1, SPath.Length - n + 1);
        // Recursive call
        if not TDirectory.IsEmpty(SPath) then
          InternalEnumDirSearch(ASearchString, SPath, LItem);
      end;
    end;
    if FSearchCancel then
      Exit;

    if not FCaseSensitive then
     ASearch := ASearchString.ToUpper
    else
     ASearch := ASearchString;

    // Enum files into current directory
    FilesList := TDirectory.GetFiles(ADirName, FileFilter);
    if Length(FilesList) > 0 then
    begin
      SL := TStringList.Create;
      for SFile in FilesList do
      begin
        if FSearchCancel then
          Break;
          try
           SL.LoadFromFile(SFile);
           if not FCaseSensitive then
            ASearchFile := SL.Text.ToUpper
           else
            ASearchFile := SL.Text;
          except
           on E: Exception do
            begin
             // do nothing
            end;
          end;
          if ASearchFile.Contains(ASearch) = True then
          begin
            LItem := TTreeViewItem.Create(FTreeView);
            if AParentItem = nil then LItem.Parent := FTreeView
              else Litem.Parent := AParentItem;
            LItem.Data := nil;
            LItem.TagString := SFile;
            LItem.Text := TPath.GetFileName(SFile);
            LItem.TagFloat := S_MAIN_ITEM;

            FoundCount := 0;
            for I := 0 to SL.Count-1 do
              begin
                if FSearchCancel then
                  Break;

                if not FCaseSensitive then
                  begin
                    if SL[I].ToUpper.Contains(ASearch)=True then
                     begin
                      if FoundCount<=10 then
                       AddSearchLine(IntToStr(I), SL[I], Litem);
                      Inc(FoundCount);
                     end;
                  end
                else
                 begin
                  if SL[I].Contains(ASearch)=True then
                   begin
                    if FoundCount<=10 then
                     AddSearchLine(IntToStr(I), SL[I], Litem);
                    Inc(FoundCount);
                   end;
                 end;
              end;

            LItem.Tag := FoundCount;
            LItem.StyleLookup := 'TreeViewItemDetailStyle';
            LItem.OnApplyStyleLookup :=
              TreeViewItemApplyStyleLookup;
          end;
      end;
      SL.Free;
    end;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;

end;

procedure TProtoFilesTree.InternalEnumFilesList(AParentItem: TTreeViewItem);
var
  I: Integer;
  LItem: TTreeViewItem;
begin
  for I := 0 to AParentItem.Count - 1 do
  begin
    LItem := AParentItem.ItemByIndex(I);
    if LItem.TagString = EmptyStr then
      InternalEnumFilesList(LItem)
    else
    begin
      SetLength(FFilesList, Length(FFilesList) + 1);
      FFilesList[Length(FFilesList) - 1] := LItem.TagString;
    end;
  end;
end;

function TProtoFilesTree.IsDirExcludes(ADirName: string): boolean;
var
  S: string;
begin
  Result := False;
  if FExcludeDirList.Count > 0 then
  begin
    if FCaseSensitive then
    begin
      for S in FExcludeDirList do
        if CompareStr(S, ADirName) = 0 then
          begin
            Result := True;
            Break;
          end;
    end else
    begin
      for S in FExcludeDirList do
        if CompareText(S, ADirName) = 0 then
          begin
            Result := True;
            Break;
          end;
    end;
  end;
end;

function TProtoFilesTree.IsDirIncludes(ADirName: string): boolean;
var
  S: string;
begin
  Result := False;
  if FIncludeDirList.Count > 0 then
  begin
    if FCaseSensitive then
    begin
      for S in FIncludeDirList do
        if CompareStr(S, ADirName) = 0 then
          begin
            Result := True;
            Break;
          end;
    end else
    begin
      for S in FIncludeDirList do
        if CompareText(S, ADirName) = 0 then
          begin
            Result := True;
            Break;
          end;
    end;
  end else
    Result := True;
end;

function TProtoFilesTree.IsMathFileName(AFileName: string): boolean;
var
  S: string;
begin
  Result := False;
  if FFilesPatternList.Count > 0 then
  begin
    for S in FFilesPatternList do
      if TPath.MatchesPattern(AFileName, S, FCaseSensitive) then
        begin
          Result := True;
          Break;
        end;
  end else
    Result := True;
end;

procedure TProtoFilesTree.OnTerminate(Sender: TObject);
begin
end;

procedure TProtoFilesTree.SetExcludeFolders(Value: string);
begin
  SetOptionsList(Value, FExcludeDirList);
end;

procedure TProtoFilesTree.SetFilesPattern(Value: string);
begin
  SetOptionsList(Value, FFilesPatternList);
end;

procedure TProtoFilesTree.SetIncludeFolders(Value: string);
begin
  SetOptionsList(Value, FIncludeDirList);
end;

procedure TProtoFilesTree.SetOptionsList(AValues: string; AList: TStringList);
var
  m, n: integer;
  s, sValue: string;
begin
  if (AList = nil) then
    Exit;

  AList.Clear;
  s := Trim(AValues);
  if s.IsEmpty then
    Exit;

  n := Pos(CSearchFoldersDelim, s);
  if n > 0 then
  begin
    m := 1;
    repeat
      sValue := Copy(s, m, n - m);
      AList.Add(sValue);
      m := n + 1;
      n := PosEx(CSearchFoldersDelim, s, m);
    until n = 0;
    sValue := Copy(s, m, s.Length - m + 1);
    AList.Add(sValue);
  end else
    AList.Add(AValues);
end;

procedure TProtoFilesTree.SetRtlPathes(Value: string);
begin
  SetOptionsList(Value, FRtlDirList);
end;

procedure TProtoFilesTree.SetSearchString(const Value: string);
begin
    if not FCaseSensitive then
        FSearchString := Value.ToUpper
    else
        FSearchString := Value;
end;

procedure TProtoFilesTree.SetTask(const Value: ITask);
begin
    FTasks[0] := Value;
end;

procedure TProtoFilesTree.StepSearch;
var
  Percent: integer;
begin
    Inc(FCountFiles);
    Percent := Trunc((FCountFiles / FTotalFiles) * 100);
    if Percent <> FProgress then
    begin
        FProgress := Percent;
        frmMain.SearchProgressBar.Value := FProgress;
    end;
    Application.ProcessMessages;
end;

procedure TProtoFilesTree.StopSearch;
var
    count : integer;
begin
    self.FSearchCancel := True;
    self.FTreeUtils.Cancel := True;
    if Assigned(Task) and (Task.Status = TTaskStatus.Running) then
        Task.Cancel;
    Task := nil;
    count := 0;
    while self.FSearchRunning and (count < 10) do
    begin
        Inc(count);
        Application.ProcessMessages;
        Sleep(100);
    end;
end;

procedure TProtoFilesTree.InternalEnumDir(ADirName: string; AParentItem: TTreeViewItem);
var
  SPath, SFile: string;
  n: integer;
  DirList: TStringDynArray;
  FilesList: TStringDynArray;
  LItem: TreeViewItemPath;
begin
  try
    // Enum Subfolders
    DirList := TDirectory.GetDirectories(ADirName, CAllFileMask, TSearchOption.soTopDirectoryOnly);
    if Length(DirList) > 0 then
    begin
      for SPath in DirList do
      begin
        LItem := TreeViewItemPath.Create(FTreeView);
        if AParentItem = nil then LItem.Parent := FTreeView
          else Litem.Parent := AParentItem;
        LItem.Data := nil;
        LItem.FPath := SPath;
        n := SPath.LastDelimiter(TPath.DirectorySeparatorChar);
        LItem.Text := SPath.Substring(n + 1, SPath.Length - n + 1);
        // Recursive call
        if not TDirectory.IsEmpty(SPath) then
            LItem.CreateFakeItem(self.EnumFolder);
          //InternalEnumDir(SPath, LItem);
      end;
    end;

    // Enum files into current directory
    FilesList := TDirectory.GetFiles(ADirName);
    if Length(FilesList) > 0 then
    begin
      for SFile in FilesList do
      begin
        LItem := TreeViewItemPath.Create(FTreeView);
        if AParentItem = nil then LItem.Parent := FTreeView
          else Litem.Parent := AParentItem;

        LItem.Data := nil;

        LItem.TagString := SFile;

        LItem.Text := TPath.GetFileName(SFile);
      end;
    end;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;
end;
{ TreeViewItemHelper }
constructor TreeViewItemPath.Create(AOwner: TComponent);
begin
  inherited;
    FPathItem := TPathItem.Create;
    Self.TagObject := FPathItem;
end;

class function TreeViewItemPath.CreateDir(AOwner, AParent: TFmxObject;
  ADirName: string; AEnumDirSearch : TNotifyEvent): TreeViewItemPath;
begin
    Result := TreeViewItemPath.Create(AOwner);
    Result.Parent := AParent;
    Result.Data := nil;
    Result.Text := ADirName;
{$IFDEF ONLYTOP}
    Result.CreateFakeItem(AEnumDirSearch);
{$ENDIF}
end;

procedure TreeViewItemPath.CreateFakeItem(AEnumDir : TNotifyEvent);
var
    lItem : TTreeViewItem;
begin
    lItem := TTreeViewItem.Create(self);
    LItem.Parent := self;
    LItem.Text := FAKETEXT;
    LItem.OnClick := AEnumDir;
    self.OnApplyStyleLookup := self.FolderApplyStyleLookup;
    self.NeedStyleLookup;
end;

destructor TreeViewItemPath.Destroy;
begin
    FreeAndNil(FPathItem);
  inherited;
end;

procedure TreeViewItemPath.FolderApplyStyleLookup(Sender: TObject);
begin
    self.StylesData['button.onclick'] :=
        TValue.From<TNotifyEvent>(self.OnFolderClick)
end;

function TreeViewItemPath.FoundFakeItem(var AEvent : TNotifyEvent): Boolean;
begin
    Result := (self.Count > 0) and self.ItemByIndex(0).Text.Equals(FAKETEXT);
    if Result then
    begin
        AEvent := self.ItemByIndex(0).OnClick;
        self.ItemByIndex(0).DisposeOf;
    end;
end;

procedure TreeViewItemPath.OnFolderClick(Sender: TObject);
var
    lEvent : TNotifyEvent;
begin
    if Self.FoundFakeItem(lEvent) then
    begin
        lEvent(Self);
    end
    else
        self.IsExpanded := not Self.IsExpanded;
end;

{ TPathItem }
constructor TPathItem.Create;
begin
  inherited;
    FItems := TObjectList<TPathItem>.Create;
end;

function TPathItem.Add(AName, AFullname: string): TPathItem;
var
    lPathItem : TPathItem;
begin
    lPathItem := TPathItem.Create(AName, AFullname);
    FItems.Add(lPathItem);
    Result := lPathItem;
end;

constructor TPathItem.Create(AName, AFullname: string);
begin
    self.Create;
    FName := AName;
    FFullName := AFullname;
end;

destructor TPathItem.Destroy;
begin
    FreeAndNil(FItems);
  inherited;
end;

function TPathItem.GetIsDirectory: Boolean;
begin
    Result := FFullName.IsEmpty;
end;

end.

