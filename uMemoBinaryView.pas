unit uMemoBinaryView;

interface

uses
  System.SysUtils, System.Classes, System.Character, System.UITypes,
  System.IOUtils, FMX.Dialogs, FMX.Forms
  {$IF DEFINED(MSWINDOWS)}
  , Winapi.Windows
  {$ENDIF}
  , uMemoFrame
  ;

procedure ShowBinaryFile(const AFileName: string; const AMemoFrame: TMemoFrame);

implementation

uses uMain, uMemoHexView, uConsts, uWorkFiles, uFileTypeHelper;

procedure ShowBinaryFile(const AFileName: string; const AMemoFrame: TMemoFrame);
{$IFDEF ANDROID}
begin

end;
{$ELSE}
var
  n: integer;
  LItem: TWorkItem;
  FSize: NativeInt;
begin
  if HexViewer.IsBusy then
    Exit;

  if SameText(AMemoFrame.FPredFileName, AFileName) then
  begin
    if frmMain.FAddToWorkFiles then
      WorkFilesTree.AddToWorkList(AFileName, ftBinary, AMemoFrame.Tag);
    frmMain.FAddToWorkFiles := False;
    Exit;
  end;

  frmMain.SetMemoFocus(AMemoFrame);
  AMemoFrame.ImageViewer1.Visible := False;
  AMemoFrame.ImageViewer1.Bitmap.FreeHandle;
  AMemoFrame.treeZipView.Visible := False;
  AMemoFrame.TMSFMXMemo1.Visible := True;

  n := AMemoFrame.TMSFMXMemo1.Lines.Count;
  if n = 1 then
  begin
    if AMemoFrame.TMSFMXMemo1.Lines.Text.Length < 3 then
      n := Trim(AMemoFrame.TMSFMXMemo1.Lines.Text).Length;
  end else
    if n > 0 then
      n := AMemoFrame.TMSFMXMemo1.Lines.Text.Length;

  if (not AMemoFrame.FMemoChanged) and (n = 0) then
  begin
    if AMemoFrame.FPredFileName.Contains(CDefFileName) then
      WorkFilesTree.DeleteFromWorkList(AMemoFrame.FPredFileName, AMemoFrame.Tag);
  end else
    WorkFilesTree.MoveToMemoStream(AMemoFrame);

  frmMain.ShowFileName(AFileName);
  LItem := WorkFilesTree.WorkItemByFileName(AFileName);
  if LItem <> nil then
  begin
    LItem.Stream.Seek(0, TSeekOrigin.soBeginning);
    AMemoFrame.TMSFMXMemo1.Lines.LoadFromStream(LItem.Stream);
    AMemoFrame.TMSFMXMemo1.SyntaxStyles := nil;
    AMemoFrame.TMSFMXMemo1.ReadOnly := True;
    frmMain.lbStyleSetting.Text := CDefNone;
    AMemoFRame.FPredFileName := AFileName;
    Exit;
  end else
  begin
    FSize := THexViewer.GetMapFileSize(AFileName);
    if FSize > 1048576 * 20 then // Over 20 MB
    begin
      if FMX.Dialogs.MessageDlg(Format(COpenLargeBinFile, [
        FormatFloat(',0', Trunc(FSize / 1048576))]),
        TMsgDlgType.mtWarning, [System.UITypes.TMsgDlgBtn.mbYes,
        System.UITypes.TMsgDlgBtn.mbNo], 0) = mrNo then
      Exit;
    end;

    HexViewer.FileName := AFileName;
    frmMain.ProgressBar1.Value := 0;
    frmMain.ProgressBar1.Visible := True;
    frmMain.txtHexViewProgress.Visible := True;
    frmMain.txtHexViewProgress.Text := 'Loading hex data...';
    frmMain.ProgressBar1.Repaint;
    frmMain.txtHexViewProgress.Repaint;
    Application.ProcessMessages;
    AMemoFrame.TMSFMXMemo1.BeginUpdate;
    try
      if HexViewer.ComposeHexData  then
      begin
        try
          AMemoFrame.TMSFMXMemo1.SyntaxStyles := nil;
          AMemoFrame.TMSFMXMemo1.ReadOnly := True;
          frmMain.lbStyleSetting.Text := CDefNone;
          AMemoFrame.TMSFMXMemo1.Lines.Assign(HexViewer.LinesList);
          AMemoFRame.FPredFileName := AFileName;
          frmMain.AddToHistoryList(AFileName);
          if frmMain.FAddToWorkFiles then
            WorkFilesTree.AddToWorkList(AFileName, ftBinary, AMemoFrame.Tag);
          frmMain.FAddToWorkFiles := False;
        except
          on E: Exception do
          begin
            ShowMessage(E.Message);
            AMemoFrame.TMSFMXMemo1.Lines.Text := string.Empty;
            HexViewer.LinesList.Clear;
          end;
        end;
      end;
    finally
      AMemoFrame.TMSFMXMemo1.EndUpdate;
    end;
  end;
  AMemoFrame.TMSFMXMemo1.Repaint;
  frmMain.ProgressBar1.Visible := False;
  frmMain.txtHexViewProgress.Text := EmptyStr;
  Application.ProcessMessages;
end;
{$ENDIF ANDROID}

end.
