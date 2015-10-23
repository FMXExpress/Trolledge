unit uZipView;

interface

uses
  System.SysUtils, System.Classes, System.Zip, System.IOUtils,
  System.Generics.Collections,
  FMX.TreeView, FMX.Types, FMX.Controls, FMX.Objects,
  uMemoFrame;

function ExtractFile(const AZipFile : string; AFileIndex : integer;
    var AExtractFile : string) : Boolean;
procedure ShowZipFile(const AFileName: string; const AMemoFrame: TMemoFrame);
procedure OnAplyStyleLookup(Sender: TObject);
procedure ShowTree(AParent : TFmxObject; AZipFile : TZipFile);
procedure BindTree(const ATree : TTreeView; AFileName : string);


implementation

uses uMain, uFileTypeHelper, uConsts;

procedure OnAplyStyleLookup(Sender: TObject);
var
  LItem: TTreeViewItem;
  Obj: TFMXObject;
  txt: TText;
begin
  if Sender is TTReeViewitem then
  begin
    LItem := TTReeViewItem(Sender);
    Obj := LItem.FindStyleResource('text');
    if Obj <> nil then
    begin
      txt := TText(Obj);
      txt.Font.Family := 'Lucida Console';
    end;
  end;
end;

procedure ShowZipFile(const AFileName: string; const AMemoFrame: TMemoFrame);
var
  ZipFile: TZipFile;
  ZipHeader: TZipHeader;
  LItem: TTreeViewItem;
  LChildItem: TTreeViewItem;
  I, n: integer;
begin
  if not TFile.Exists(AFileName) then
    raise Exception.Create('File does not exists: ' + sLineBreak + AFileName);

  n := 0;
  if AMemoFrame.TMSFMXMemo1.Lines.Count > 0 then
    n := 1
  else
    n := AMemoFrame.TMSFMXMemo1.Lines.Text.Length;

  if (not AMemoFrame.FMemoChanged) and (n = 0) then
  begin
    if AMemoFrame.FPredFileName.Contains(CDefFileName) then
      WorkFilesTree.DeleteFromWorkList(AMemoFrame.FPredFileName, AMemoFrame.Tag);
  end else
    WorkFilesTree.MoveToMemoStream(AMemoFrame);

  AMemoFrame.TMSFMXMemo1.Visible := False;
  AMemoFrame.TMSFMXMemo1.Lines.Text := string.Empty;
  AMemoFrame.pnlMemoFind.Visible := False;
  AMemoFrame.ImageViewer1.Visible := False;
  AMemoFrame.treeZipView.Visible := True;
  frmMain.ShowFileName(AFileName);

  AMemoFRame.FPredFileName := AFileName;
  frmMain.AddToHistoryList(AFileName);
  WorkFilesTree.AddToWorkList(AFileName, ftArchive, AMemoFrame.Tag);
  ZipFile := TZipFile.Create;
  if not ZipFile.IsValid(AFileName) then
      exit;
  AMemoFrame.treeZipView.BeginUpdate;
  try
    AMemoFrame.treeZipView.Clear;
    ZipFile.Open(AFileName, TZipMode.zmRead);
    ShowTree(AMemoFrame.treeZipView, ZipFile);
    BindTree(AMemoFrame.treeZipView, AFileName);
    exit;
    for I := 0 to ZipFile.FileCount - 1 do
    begin
      LItem := TTreeViewItem.Create(AMemoFrame.treeZipView);
      LItem.Parent := AMemoFrame.treeZipView;
      LItem.Text := ZipFile.FileName[I];
      ZipHeader := ZipFile.FileInfo[I];
      LChildItem := TTreeViewItem.Create(AMemoFrame.treeZipView);
      LChildItem.Parent := LItem;
      LChildItem.Text := Format('%s %s', ['Modified:', FormatDateTime(
        FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
        FileDateToDateTime(ZipHeader.ModifiedDateTime))]);
      LChildItem := TTreeViewItem.Create(AMemoFrame.treeZipView);
      LChildItem.Parent := LItem;
      LChildItem.Text := Format('%s %d', ['Compressed size:', ZipHeader.CompressedSize]);
      LChildItem := TTreeViewItem.Create(AMemoFrame.treeZipView);
      LChildItem.Parent := LItem;
      LChildItem.Text := Format('%s %d', ['Uncompressed size:', ZipHeader.UncompressedSize]);
      LChildItem := TTreeViewItem.Create(AMemoFrame.treeZipView);
      LChildItem.Parent := LItem;
      LChildItem.Text := Format('%s %s', ['CRC32:', IntToHex(ZipHeader.CRC32, 8)]);
    end;
  finally
    AMemoFrame.treeZipView.EndUpdate;
    ZipFile.Free;
  end;
end;

procedure ShowTree(AParent : TFmxObject; AZipFile : TZipFile);
var
    ZipHeader: TZipHeader;
    LItem: TTreeViewItem;
    LChildItem: TTreeViewItem;
    index, i, count : integer;
    lFileName, lDirName, lZipFile, lFullDirName, lSplitDir : string;
    FArray, LDirectories : TArray<string>;
    lDircDirectories : TDictionary<string,TFmxObject>;
    lParent : TFmxObject;
begin
    FArray := AZipFile.FileNames;
    lDircDirectories := TDictionary<string,TFmxObject>.Create;
    try
        count := High(FArray);
        for i := Low(FArray) to count do
        begin
            lZipFile := FArray[i];
            lFileName := system.IOUtils.TPath.GetFileName(lZipFile);
            lFullDirName := system.IOUtils.TPath.GetDirectoryName(lZipFile);

            lFullDirName := StringReplace(lFullDirName, System.IOUtils.TPath.AltDirectorySeparatorChar,
                System.IOUtils.TPath.DirectorySeparatorChar, [rfReplaceAll]);
            lParent := AParent;
            if not lFullDirName.IsEmpty then
            begin
                LDirectories := lFullDirName.Split(System.IOUtils.TPath.DirectorySeparatorChar);
                lDirName := string.Empty;
                for lSplitDir in LDirectories do
                begin
                    case lDirName.IsEmpty of
                        True : lDirName := lSplitDir;
                        False : lDirName := lDirName + System.IOUtils.TPath.DirectorySeparatorChar + lSplitDir;
                    end;
                    if not lDircDirectories.ContainsKey(lDirName) then
                    begin
                        LItem := TTreeViewItem.Create(AParent);
                        LItem.Parent := lParent;
                        LItem.Text := lSplitDir;
                        lDircDirectories.AddOrSetValue(lDirName, LItem);
                    end;
                    lParent := lDircDirectories.Items[lDirName];
                end;
            end;
            if lFileName.IsEmpty then
                Continue;
            LItem := TTreeViewItem.Create(AParent);
            LItem.Parent := lParent;
            LItem.Text := lFileName;
            LItem.Tag := I;
            LItem.TagString := lZipFile;
            ZipHeader := AZipFile.FileInfo[I];
            LChildItem := TTreeViewItem.Create(AParent);
            LChildItem.Parent := LItem;
            LChildItem.Text := Format('%s %s', ['Modified:', FormatDateTime(
            FormatSettings.ShortDateFormat + ' ' + FormatSettings.LongTimeFormat,
            FileDateToDateTime(ZipHeader.ModifiedDateTime))]);
            LChildItem := TTreeViewItem.Create(AParent);
            LChildItem.Parent := LItem;
            LChildItem.Text := Format('%s %d', ['Compressed size:', ZipHeader.CompressedSize]);
            LChildItem := TTreeViewItem.Create(AParent);
            LChildItem.Parent := LItem;
            LChildItem.Text := Format('%s %d', ['Uncompressed size:', ZipHeader.UncompressedSize]);
            LChildItem := TTreeViewItem.Create(AParent);
            LChildItem.Parent := LItem;
            LChildItem.Text := Format('%s %s', ['CRC32:', IntToHex(ZipHeader.CRC32, 8)]);
        end;
    finally
        lDircDirectories.Free;
    end;
end;

procedure BindTree(const ATree : TTreeView; AFileName : string);
begin
    if not Assigned(ATree) then
        Exit;
    ATree.OnDblClick := frmMain.actOpenZipFileExecute;
    ATree.TagString := AFileName.Trim;
end;

function ExtractFile(const AZipFile : string; AFileIndex : integer;
    var AExtractFile : string) : Boolean;
var
    ZipFile : TZipFile;
    exPath : string;
    lValid : boolean;
begin
    Result := False;
    ZipFile := TZipFile.Create;
    try
        lValid := TFile.Exists(AZipFile) and ZipFile.IsValid(AZipFile);
        if not lValid then
            exit;
        ZipFile.Open(AZipFile, zmRead);
        try
            exPath := System.IOUtils.TPath.GetTempPath;
            if not (ZipFile.FileCount > AFileIndex) then
                exit;
            ZipFile.Extract(AFileIndex, exPath, false);
            AExtractFile := exPath + system.IOUtils.TPath.GetFileName(ZipFile.FileName[AFileIndex]);
            Result := True;
        finally
            ZipFile.Close;
        end;
    finally
        ZipFile.Free;
    end;
end;

end.
