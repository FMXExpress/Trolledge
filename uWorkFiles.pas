unit uWorkFiles;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Styles,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.TreeView, FMX.Platform,
  FMX.Objects, System.IOUtils, FMX.ExtCtrls, FMX.Dialogs, FMX.TMSMemo,
  System.Generics.Collections, uMemoFrame, uConsts, uFileTypeHelper, duck;

type
  TWorkFilesTree = class;

  TWorkItem = class
  private
    FTreeViewItem: TTreeViewItem;
    FStream: TMemoryStream;
    FModified: boolean;
    FFileChanged : Boolean;
    FStyler: TTMSFMXMemoCustomStyler;
    FFileType: TFileType;
    function GetFileName: string;
    function GetTag: integer;
    procedure SetFileName(Value: string);
    procedure SetTag(Value: integer);
    procedure SetModified(const Value: boolean);
  public
    constructor Create(AWorkTree: TWorkFilesTree; AFilename: string;
      AFileType: TFileType; AFrameTag: integer);
    destructor Destroy; override;
    procedure SaveToFile;
    property Modified: boolean read FModified write SetModified;
    property FileChanged: boolean read FFileChanged write FFileChanged;
    property FileName: string read GetFileName write SetFileName;
    property Styler: TTMSFMXMemoCustomStyler read FStyler write FStyler;
    property Stream: TMemoryStream read FStream write FStream;
    property Tag: integer read GetTag write SetTag;
    property TreeViewitem: TTreeViewItem read FTreeViewItem write FTreeViewItem;
  end;

  // TreeView must contains two TreeViewitems before use this class.
  TWorkFilesTree = class
  private
    FTreeView: TTreeView;
    FWorkList: TList<TWorkItem>;
    function GetModifiedCount: integer;
    procedure ClearWorkList;
    function  ItemsCountByFrameTag(AFrameTag: integer): integer;
    procedure ItemMouseEnter(Sender: TObject);
    procedure ItemMouseLeave(Sender: TObject);
    procedure ItemCloseClick(Sender: TObject);
  public
    constructor Create(ATreeView: TTreeView);
    destructor Destroy; override;

    procedure AddToWorkList(AFileName: string; AFileType: TFileType; AFrameTag: integer);
    procedure DeleteFromWorkList(AFileName: string; AFrameTag: integer);
    procedure MoveToMemoStream(AFrame: TMemoFrame);
    procedure RenameWorkItem(AOldFileName, ANewFileName: string; AFrameTag: integer);
    procedure SetFileChanges(AFileName: string; AEnabled : Boolean = False);
    procedure ReloadStream(AFileName : string);

    function IndexOfByFileName(AFileName: string): integer;
    function WorkItemByFileName(AFileName: string): TWorkItem;
    function FilenameExists(AFileName: string; AFrameTag: integer): boolean;

    property ModifiedCount: integer read GetModifiedCount;
  end;

implementation

{ TWorkFilesTree }

uses uMain;

procedure TWorkFilesTree.AddToWorkList(AFileName: string; AFileType: TFileType;
  AFrameTag: integer);
var
  LItem: TWorkItem;
begin
  if FileNameExists(AFileName, AFrameTag) then
    Exit;

  LItem := TWorkItem.Create(Self, AFileName, AFileType, AFrameTag);
  FWorkList.Add(LItem);
  FTreeView.Items[0].Expand;
end;

function TWorkFilesTree.FilenameExists(AFileName: string; AFrameTag: integer): boolean;
var
  LItem: TWorkItem;
  I, N: integer;
begin
  N := -1;
  for I := 0 to FWorkList.Count - 1 do
  begin
    LItem := FWorkList[I];
    if SameText(LItem.FileName, AFileName) then
    begin
      N := I;
      Break;
    end;
  end;
  Result := N <> -1;
end;

function TWorkFilesTree.GetModifiedCount: integer;
var
  LItem: TWorkItem;
begin
  Result := 0;
  for LItem in FWorkList do
    if LItem.Modified then Inc(Result);
end;

function TWorkFilesTree.IndexOfByFileName(AFileName: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := 0 to FWorkList.Count - 1 do
    if SameText(FWorkList[I].FileName, AFileName) then
      begin
        Result := I;
        Break;
      end;
end;

procedure TWorkFilesTree.ItemCloseClick(Sender: TObject);
var
  Img: TImage;
  LItem: TWorkItem;
  LFrame: TMemoFrame;
  LCount: integer;
  DlgResult, lTag: integer;
  lFileName : string;
begin
  if Sender is TImage then
  begin
    Img := Sender as TImage;
    if Img <> nil then
    begin
      LItem := Img.TagObject as TWorkItem;
      if LItem <> nil then
      begin
        LFrame := frmMain.GetMemoFrameByTag(LItem.Tag);
        if LItem.Modified or LFrame.FMemoChanged then
        begin
          DlgResult := FMX.Dialogs.MessageDlg(CWorkItemCloseText,
            TMsgDlgType.mtWarning, [System.UITypes.TMsgDlgBtn.mbYes,
            System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel], 0);
          case DlgResult of
            // Cancel - Don't close the WorkFile
            mrCancel: Exit;
            // Save changes
            mrYes: LItem.SaveToFile;
            // No save. Lose changes.
            mrNo: ;
          end;
        end;
        frmMain.actClosePane.Tag := LFrame.Tag;
        frmMain.actClosePane.Target := LFrame;
        //need for fix error Untitled files
        lTag := LItem.Tag;
        lFileName := LItem.FileName;
        if SameText(LItem.FileName, frmMain.FSelectedFrame.FPredFileName)  then
          frmMain.actClosePaneExecute(frmMain.actClosePane);
        DeleteFromWorkList(lFileName, lTag);
      end;
    end;
  end;
end;

procedure TWorkFilesTree.ItemMouseEnter(Sender: TObject);
var
  LItem: TTreeViewitem;
  I: integer;
begin
  if Sender is TTreeViewItem then
  begin
    LItem := TTreeViewitem(Sender);
    for I := 0 to LItem.Children.Count - 1 do
      if LItem.Children[I].ClassNameIs('TImage') then
        TImage(LItem.Children[I]).Visible := True;
  end;
end;

procedure TWorkFilesTree.ItemMouseLeave(Sender: TObject);
var
  LItem: TTreeViewitem;
  I: integer;
begin
  if Sender is TTreeViewItem then
  begin
    LItem := TTreeViewitem(Sender);
    for I := 0 to LItem.Children.Count - 1 do
      if LItem.Children[I].ClassNameIs('TImage') then
        TImage(LItem.Children[I]).Visible := False;
  end;
end;

function TWorkFilesTree.ItemsCountByFrameTag(AFrameTag: integer): integer;
var
  LItem: TWorkItem;
begin
  Result := 0;
  for LItem in FWorkList do
    if Litem.Tag = AFrameTag then Inc(Result);
end;

procedure TWorkFilesTree.MoveToMemoStream(AFrame: TMemoFrame);
var
  N: integer;
  LItem: TWorkItem;
begin
  if AFrame <> nil then
  try
    N := IndexOfByFileName(AFrame.FPredFileName);
    if N <> -1 then
    begin
      LItem := FWorkList[N];
      Litem.Stream.Clear;
      if AFrame.FMemoChanged or AFrame.TMSFMXMemo1.Modified then
        LItem.Modified := True;
      LItem.FFileChanged := AFrame.FFileChanged;
      LItem.Styler := AFrame.TMSFMXMemo1.SyntaxStyles;
      AFrame.TMSFMXMemo1.Lines.SaveToStream(FWorkList[N].Stream);
    end;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TWorkFilesTree.ReloadStream(AFileName: string);
var
    LWorkItem : TWorkItem;
begin
    LWorkItem := WorkItemByFileName(AFileName);
    if (LWorkItem <> nil) and TFile.Exists(AFileName) then
    begin
        LWorkItem.Stream.LoadFromFile(AFileName);
    end;
end;

procedure TWorkFilesTree.RenameWorkItem(AOldFileName, ANewFileName: string;
  AFrameTag: integer);
var
  LItem: TWorkItem;
begin
  for LItem in FWorkList do
  begin
    if SameText(LItem.FileName, AOldFileName) and (LItem.Tag = AFrameTag) then
    begin
      LItem.FileName := ANewFileName;
      Break;
    end;
  end;
end;

function TWorkFilesTree.WorkItemByFileName(AFileName: string): TWorkItem;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FWorkList.Count - 1 do
    if SameText(FWorkList[I].FileName, AFileName) then
      begin
        Result := FWorkList[I];
        Break;
      end;
end;

procedure TWorkFilesTree.ClearWorkList;
var
  LItem: TWorkItem;
begin
  for LItem in FWorkList do
    LItem.Free;
  FWorkList.Clear;
end;

constructor TWorkFilesTree.Create(ATreeView: TTreeView);
begin
  FTreeView := ATreeView;
  FWorkList := TList<TWorkItem>.Create;
end;

procedure TWorkFilesTree.DeleteFromWorkList(AFileName: string; AFrameTag: integer);
var
  DelIndex, I: integer;
begin
  DelIndex := -1;
  for I := 0 to FWorkList.Count - 1 do
    if SameText(FWorkList[I].FileName, AFileName) and (FWorkList[I].Tag = AFrameTag) then
      begin
        DelIndex := I;
        Break;
      end;
  if DelIndex <> -1 then
  begin
    FWorkList[DelIndex].Free;
    FWorkList[DelIndex] := nil;
    FWorkList.Pack;
  end;
end;

destructor TWorkFilesTree.Destroy;
begin
  ClearWorkList;
  FWorkList.Free;
  inherited;
end;

procedure TWorkFilesTree.SetFileChanges(AFileName: string; AEnabled : Boolean);
var
    LWorkItem : TWorkItem;
begin
    LWorkItem := WorkItemByFileName(AFileName);
    if LWorkItem <> nil then
        LWorkItem.FileChanged := AEnabled;
end;

{ TWorkItem }

constructor TWorkItem.Create(AWorkTree: TWorkFilesTree; AFilename: string;
  AFileType: TFileType; AFrameTag: integer);
var
  LImage: TImage;
begin
  inherited Create;

  FFileType := AFileType;
  FModified := False;
  FileChanged := False;
  FStyler := nil;
  FTreeViewItem := TTreeViewItem.Create(AWorkTree.FTreeView);
  FTreeViewItem.Parent := AWorkTree.FTreeView.Items[0];
  Tag := AFrameTag;
  FTreeViewItem.OnMouseEnter := AWorkTree.ItemMouseEnter;
  FTreeViewItem.OnMouseLeave := AWorkTree.ItemMouseLeave;
  SetFileName(AFileName);

  LImage := TImage.Create(nil);
  LImage.Parent := FTreeViewItem;
  LImage.Align := TAlignLayout.Left;
  LImage.Width := 24;
  LImage.Tag := 13;
  LImage.Bitmap := frmMain.ImageListWhite.Bitmap(TSize.Create(32, 32), LImage.Tag);
  LImage.Visible := False;
  LImage.OnClick := AWorkTree.ItemCloseClick;
  LImage.TagObject := Self;

  FTreeViewItem.AddObject(LImage);

  FStream := TMemoryStream.Create;
end;

destructor TWorkItem.Destroy;
begin
  FTreeViewItem.DisposeOf;
  FStream.Free;
  inherited;
end;

function TWorkItem.GetFileName: string;
begin
  Result := FTreeViewItem.TagString;
end;

function TWorkItem.GetTag: integer;
begin
  Result := FTreeViewItem.Tag;
end;

procedure TWorkItem.SaveToFile;
begin
  if FStream <> nil then
  try

  except
    on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TWorkItem.SetFileName(Value: string);
begin
  FTreeViewItem.Text := TPath.GetFileName(Value);
  FTreeViewItem.TagString := Value;
end;

procedure TWorkItem.SetModified(const Value: boolean);
begin
  FModified := Value;
  //set flag after save events.... if Modified = false not only after save - need change logic
  if not Value then
    FFileChanged := False;
end;

procedure TWorkItem.SetTag(Value: integer);
begin
  FTreeViewItem.Tag := Value;
end;

end.
