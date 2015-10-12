unit uFilesTree;

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.Types,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Styles,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TreeView, FMX.Platform,
  FMX.Objects, System.IOUtils, System.StrUtils,
  FMX.ExtCtrls, FMX.Dialogs, System.Rtti, System.Threading;

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

    //FTask: ITask;
    FThread: TThread;
    FTotalFiles: integer;
    FCountFiles: integer;
    FProgress: integer;
    FSearchRunning: boolean;
    FSearchCancel: boolean;
    function IsMathFileName(AFileName: string): boolean;
    function IsDirExcludes(ADirName: string): boolean;
    function IsDirIncludes(ADirName: string): boolean;
    procedure AddSearchLine(AFoundLine: String; AFoundString: String; AParentItem: TTreeViewItem);
    procedure EnumDirSearch(ASearchString: string; ADirName: string);
    procedure InternalEnumDir(ADirName: string; AParentItem: TTreeViewItem);
    procedure InternalEnumDirSearch(ASearchString: string; ADirName: string; AParentItem: TTreeViewItem);
    procedure InternalEnumFilesList(AParentItem: TTreeViewItem);
    procedure AddDetailToTreeViewItem(ADetail: String; AItem: TTreeViewItem);
    procedure TreeViewItemApplyStyleLookup(Sender: TObject);

    function DirFilter(const Path: string; const SearchRec: TSearchRec): boolean;
    function FileFilter(const Path: string; const SearchRec: TSearchRec): boolean;
    procedure SetOptionsList(AValues: string; AList: TStringList);
    function GetFilesPattern: string;
    procedure SetFilesPattern(Value: string);
    function GetIncludeFolders: string;
    procedure SetIncludeFolders(Value: string);
    function GetExcludeFolders: string;
    procedure SetExcludeFolders(Value: string);
    function GetRtlPathes: string;
    procedure SetRtlPathes(Value: string);
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;
    procedure Clear;
    procedure ExpandAll;
    procedure EnumDir(ADirName: string);
    procedure TaskEnumDirSearch(ASearchString: string; ADirName: string);
    procedure ClearSearchOptions;
    function  GetFilesList: TStringDynArray;
    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property FilesPattern: string read GetFilesPattern write SetFilesPattern;
    property IncludeFolders: string read GetIncludeFolders write SetIncludeFolders;
    property ExcludeFolders: string read GetExcludeFolders write SetExcludeFolders;
    property PathToRTL: string read GetRtlPathes write SetRtlPathes;
    property SearchRunning: boolean read FSearchRunning;
  end;

implementation

{ TProtoFilesTree }

uses uConsts, uMain;

constructor TProtoFilesTree.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FFilesPatternList := TStringList.Create;
  FIncludeDirList := TStringList.Create;
  FExcludeDirlist := TStringList.Create;
  FRtlDirList := TStringList.Create;
  FCaseSensitive := False;

  //FTask := nil;
  FThread := nil;
  FTotalFiles := 0;
  FCountFiles := 0;
  FSearchRunning := False;
  FSearchCancel := False;
end;

destructor TProtoFilesTree.Destroy;
begin
  FFilesPatternList.Free;
  FIncludeDirList.Free;
  FExcludeDirlist.Free;
  FRtlDirList.Free;
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

procedure TProtoFilesTree.Clear;
begin
  FSearchCancel := True;
  FTreeView.BeginUpdate;
  try
    FTreeView.Clear;
  finally
    FTreeView.EndUpdate;
  end;
  FTotalFiles := 0;
  FCountFiles := 0;
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
    {
    if FRtlDirList.Count > 0 then
    begin

    end;
    }
  finally
    frmMain.SearchProgressBar.Value := 0;
    frmMain.SearchProgressBar.Repaint;
    FTreeView.EndUpdate;
  end;
  Application.ProcessMessages;
end;


procedure TProtoFilesTree.EnumDir(ADirName: string);
var
  I: Integer;
begin
  FTreeView.BeginUpdate;
  try
    //FTreeView.Clear;
    for I := FTreeView.Items[1].Count - 1 downto 0 do
      FTreeView.Items[1].ItemByIndex(i).DisposeOf;
    InternalEnumDir(ADirName, FTreeView.Items[1]);
    FTreeView.Items[1].Expand;
  finally
    FTreeView.EndUpdate;
  end;
end;

procedure TProtoFilesTree.AddDetailToTreeViewItem(ADetail: String; AItem: TTreeViewItem);
//var
//  DetailValue: TRoundRect;
//  DetailLabel: TLabel;
begin
  {DetailValue := TRoundRect.Create(nil);
  DetailValue.Parent := AItem;
  DetailValue.Align := TAlignLayout.Right;
  DetailValue.Opacity := 0.5;
  DetailValue.Width := 20 + (ADetail.Length*5);
  DetailLabel := TLabel.Create(DetailValue);
  DetailLabel.Text := ADetail;
  DetailLabel.Parent := DetailValue;
  DetailLabel.TextAlign := TTextAlign.Center;
  DetailLabel.StyledSettings := [TStyledSetting.Family,TStyledSetting.Size];
  DetailLabel.FontColor := TAlphaColorRec.Black;
  DetailLabel.Font.Style := [TFontStyle.fsBold];
  DetailLabel.Align := TAlignLayout.Client;}
end;

procedure TProtoFilesTree.AddSearchLine(AFoundLine: String; AFoundString: String; AParentItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
begin
  LItem := TTreeViewItem.Create(FTreeView);
  if AParentItem = nil then LItem.Parent := FTreeView
    else Litem.Parent := AParentItem;

  LItem.Data := nil;

  LItem.TagString := AFoundString;

  AddDetailToTreeViewItem(AFoundLine, LItem);
  LItem.TagFloat := AFoundLine.ToSingle;
  LItem.Tag := 1;

  LItem.Text := AFoundString;
 // LItem.StyleLookup := 'TreeViewItemDetailStyle';
 // LItem.OnApplyStyleLookup :=
 //   TreeViewItemApplyStyleLookup;
end;

procedure TProtoFilesTree.TaskEnumDirSearch(ASearchString, ADirName: string);
var
  Pool: TThreadPool;
begin
  if FSearchRunning then
    Exit;

  FSearchCancel := False;
  FSearchRunning := True;
  EnumDirSearch(ASearchString, ADirName);
  FSearchRunning := False;
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

procedure TProtoFilesTree.InternalEnumDir(ADirName: string; AParentItem: TTreeViewItem);
var
  SPath, SFile: string;
  n: integer;
  DirList: TStringDynArray;
  FilesList: TStringDynArray;
  LItem: TTreeViewItem;
begin
  try
    // Enum Subfolders
    DirList := TDirectory.GetDirectories(ADirName, CAllFileMask, TSearchOption.soTopDirectoryOnly);
    if Length(DirList) > 0 then
    begin
      for SPath in DirList do
      begin
        LItem := TTreeViewItem.Create(FTreeView);
        if AParentItem = nil then LItem.Parent := FTreeView
          else Litem.Parent := AParentItem;
        LItem.Data := nil;
        n := SPath.LastDelimiter(TPath.DirectorySeparatorChar);
        LItem.Text := SPath.Substring(n + 1, SPath.Length - n + 1);
        // Recursive call
        if not TDirectory.IsEmpty(SPath) then
          InternalEnumDir(SPath, LItem);
      end;
    end;

    // Enum files into current directory
    FilesList := TDirectory.GetFiles(ADirName);
    if Length(FilesList) > 0 then
    begin
      for SFile in FilesList do
      begin
        LItem := TTreeViewItem.Create(FTreeView);
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
end.
