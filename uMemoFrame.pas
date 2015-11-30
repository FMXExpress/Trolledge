unit uMemoFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ExtCtrls, FMX.Edit, FMX.TMSBaseControl, FMX.TMSMemo,
  FMX.Controls.Presentation, FMX.Objects, FMX.ListView.Types, FMX.ListView,
  FMX.TMSWebBrowser, FMX.TreeView, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base;

type
  TGotoKind = (gtFile = 1, gtLine, gtCommand, gtCode);

  TMemoFrame = class(TFrame)
    pnlMemoStatus: TPanel;
    lbFileName: TLabel;
    lbFilePath: TLabel;
    TMSFMXMemo1: TTMSFMXMemo;
    pnlMemoFind: TPanel;
    edFindText: TEdit;
    lbFindText: TLabel;
    btnFindText: TButton;
    btnCloseFindBar: TButton;
    ImageViewer1: TImageViewer;
    btnCloseFrame: TSpeedButton;
    Image1: TImage;
    btnSplitEditor: TSpeedButton;
    Image2: TImage;
    pnlGoTo: TPanel;
    lbGoToHint: TLabel;
    ListView1: TListView;
    edGoToFilter: TEdit;
    treeZipView: TTreeView;
    procedure TMSFMXMemo1Click(Sender: TObject);
    procedure edGoToFilterTyping(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edGoToFilterKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure TMSFMXMemo1CursorChange(Sender: TObject);
    procedure TMSFMXMemo1DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure TMSFMXMemo1DragEnter(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure TMSFMXMemo1DragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FrameDragEnter(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure FrameDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure FrameDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure TMSFMXMemo1ApplyStyleLookup(Sender: TObject);
  private
    { Private declarations }
    FCanDrop: boolean;
    FOnAfterSave : TNotifyEvent;
    FSaveFileName : string;
    FGotoKind: TGotoKind;
    procedure DoOnAfterSave;

    procedure SetGotoKind(const Value: TGotoKind);  public
    { Public declarations }
    FMemoChanged: Boolean;
    FFileChanged : Boolean;
    FPredFileName: string;
    FEncoding: String;
    FCRLF: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// if AFileName.IsEmpty then
    ///    AFileName := FPredFileName;
    /// </summary>
    procedure SaveMemoLines(AFileName : string = '');
  public
    property OnAfterSave : TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property SaveFileName : string read FSaveFileName;
    property  GotoKind : TGotoKind read FGotoKind write SetGotoKind;
  end;
const
    BTM_MARGID = 200;
implementation

{$R *.fmx}

uses uMain, CCR.Clipboard;

constructor TMemoFrame.Create(AOwner: TComponent);
begin
  inherited;
  FMemoChanged := False;
  FFileChanged := False;
  FPredFileName := '';
  FCRLF := '';
  FSaveFileName := string.Empty;
  FOnAfterSave := nil;
end;

destructor TMemoFrame.Destroy;
begin

  inherited;
end;

procedure TMemoFrame.DoOnAfterSave;
begin
    if Assigned(OnAfterSave) then
        OnAfterSave(self);
end;

procedure TMemoFrame.edGoToFilterKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkEscape: frmMain.GoToListHide(Self);
    vkReturn:
        if Assigned(self.ListView1.Selected) then
            frmMain.GoToListExec(self.ListView1, Self)
        else
            frmMain.GoToListExec(Sender, Self);
    vkUp, vkDown: frmMain.GoToListUpDown(Key, self);
  end;
end;

procedure TMemoFrame.edGoToFilterTyping(Sender: TObject);
begin
  ListView1.Items.Filter := function(S: string): boolean
    var
      Lower: string;
    begin
      Lower := Trim(edGoToFilter.Text).ToLower;
      Result := Lower.IsEmpty or S.ToLower.Contains(Lower);
    end;
end;

procedure TMemoFrame.FrameDragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  TMSFMXMemo1DragDrop(Sender, Data, Point);
end;

procedure TMemoFrame.FrameDragEnter(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
begin
  TMSFMXMemo1DragEnter(Sender, Data, Point);
end;

procedure TMemoFrame.FrameDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  TMSFMXMemo1DragOver(Sender, Data, Point, Operation);
end;

procedure TMemoFrame.ListView1DblClick(Sender: TObject);
begin
  if ListView1.Selected <> nil then
  begin
    frmMain.GoToListExec(Sender, Self);
  end;
end;

procedure TMemoFrame.ListView1KeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  case Key of
    vkEscape: frmMain.GoToListHide(Self);
    vkReturn: frmMain.GoToListExec(Sender, Self);
  end;
end;

/// <summary>
/// if AFileName.IsEmpty then
///    AFileName := FPredFileName;
/// </summary>
procedure TMemoFrame.SaveMemoLines(AFileName: string);
begin
    if AFileName.IsEmpty then
        AFileName := self.FPredFileName;
    FSaveFileName := AFileName;
    DoOnAfterSave;
    TMSFMXMemo1.Lines.SaveToFile(AFileName);
    FFileChanged := False;
end;

procedure TMemoFrame.SetGotoKind(const Value: TGotoKind);
begin
  FGotoKind := Value;
  case Value of
    gtLine :
    begin
        pnlGoTo.Align := TAlignLayout.MostTop;
        pnlGoTo.Height := self.ListView1.Position.Y;
        pnlGoTo.Margins.Bottom := - (pnlGoTo.Margins.Top + pnlGoTo.Height);
    end;
  else
      begin
        pnlGoTo.Align := TAlignLayout.Contents;
        pnlGoTo.Margins.Bottom := BTM_MARGID;
      end;
  end;
  pnlGoTo.BringToFront;
end;

procedure TMemoFrame.TMSFMXMemo1ApplyStyleLookup(Sender: TObject);
var
  Obj: TFMXObject;
  Rect: TRectangle;
  Line: TLine;
begin
  // Here we fix white lines around Memo and Gutter.
  if (Sender is TTMSFMXMemo) then
  begin
    Obj := TTMSFMXMemo(Sender).FindStyleResource('memo');
    if (Obj <> nil) and (Obj is TRectangle) then
      TRectangle(Obj).Stroke.Kind := TBrushKind.None;
    Obj := TTMSFMXMemo(Sender).FindStyleResource('gutter');
    if (Obj <> nil) and (Obj is TRectangle) then
    begin
      Rect := Obj as TRectangle;
      Rect.Stroke.Kind := TBrushKind.None;
      if TRectangle(Obj).ChildrenCount = 0 then
      begin
        Line := TLine.Create(nil);
        Line.Parent := Rect;
        Line.LineType := TLineType.Right;
        Line.Align := TAlignLayout.Right;
        Line.Width := 1;
        Line.Stroke.Color := TAlphaColorRec.DarkGray;
      end;
    end;
  end;
end;

procedure TMemoFrame.TMSFMXMemo1Click(Sender: TObject);
begin
  if pnlGoTo.Visible then frmMain.GoToListHide(Self);
  //frmMain.ShowFileName(FPredFileName);
end;

procedure TMemoFrame.TMSFMXMemo1CursorChange(Sender: TObject);
var
  s: string;
  c: Char;
begin
  s := TTMSFMXMemo(Sender).FullWordAtCursor;
  if not s.IsEmpty then
  begin
    if s.Equals('//') or s.Contains('-----') then
      s := ''
    else
    begin
      c := s[s.Length];
      if CharInSet(c, ['}', ';', ',']) then
        Delete(s, s.Length, 1);
    end;
    TTMSFMXMemo(Sender).HighlightText := s;
  end;
end;

procedure TMemoFrame.TMSFMXMemo1DragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  Clipboard: TClipboard;
begin
  Clipboard := Data.Source as TClipboard;
  if Clipboard.ObjectBeingDragged <> Sender then
  begin
    frmMain.LoadFrameFromFile(Data.Files[0], Self);
  end;
end;

procedure TMemoFrame.TMSFMXMemo1DragEnter(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
begin
  FCanDrop := (Data.Source is TClipboard);
end;

procedure TMemoFrame.TMSFMXMemo1DragOver(Sender: TObject;
  const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Copy;
end;

end.
