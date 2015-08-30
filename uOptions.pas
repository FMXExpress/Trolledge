unit uOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.TabControl, FMX.Controls.Presentation, FMX.Ani,
  FMX.Colors, FMX.TMSBaseControl, FMX.TMSMemo, FMX.TMSMemoStyles,
  FMX.Objects, duck
  {$IFDEF MSWINDOWS}
  ,FMX.Platform.Win, Winapi.Windows, Winapi.ShellAPI, ComObj, ShlObj, ActiveX
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  ,Macapi.AppKit, Macapi.Foundation, Macapi.ObjectiveC
  {$ENDIF MACOS}
  ;

const
  DyIncrement   = 8;

type
  TOptionsFrame = class(TFrame)
    ColorAnimation1: TColorAnimation;
    BottomLayout: TLayout;
    Button1: TButton;
    Button2: TButton;
    TabControl1: TTabControl;
    tbiGeneral: TTabItem;
    GroupBox4: TGroupBox;
    chkGutterVisible: TCheckBox;
    spinGutterWidth: TSpinBox;
    Label2: TLabel;
    chkGutterLineNumbers: TCheckBox;
    chkGutterZeroes: TCheckBox;
    chkMemoRightMargin: TCheckBox;
    Label3: TLabel;
    spinMemoRightColumn: TSpinBox;
    GroupBox5: TGroupBox;
    chkMemoTabStop: TCheckBox;
    Label4: TLabel;
    SpinMemoTabSize: TSpinBox;
    chkMemoSmartTabs: TCheckBox;
    chkMemoWantTab: TCheckBox;
    GroupBox3: TGroupBox;
    chkMemoActiveLine: TCheckBox;
    chkMemoActiveLineIndicator: TCheckBox;
    tbiPaths: TTabItem;
    Label5: TLabel;
    edSearchPath: TEdit;
    btnSearchPath: TButton;
    tbiHighlighting: TTabItem;
    GroupBox1: TGroupBox;
    TMSFMXMemo1: TTMSFMXMemo;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    cbStylesList: TComboBox;
    ScrollBox1: TScrollBox;
    tbiPlugins: TTabItem;
    GPL: TGridPanelLayout;
    MainLayout: TLayout;
    BGRect: TRectangle;
    GreenRect: TRectangle;
    chkAutoOpen: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure cbStylesListChange(Sender: TObject);
    procedure chkGutterVisibleClick(Sender: TObject);
    procedure chkMemoRightMarginClick(Sender: TObject);
    procedure btnSearchPathClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowStylerStyles(AStylerName: string);
    function SelectDirectory(const ATitle: string; var ADir: string): boolean;
  public
    { Public declarations }
    {$IFDEF MSWINDOWS}
    Handle: TWindowHandle;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

implementation

{$R *.fmx}

uses uMain;

procedure TOptionsFrame.btnSearchPathClick(Sender: TObject);
var
  NewPath: string;
begin
  if SelectDirectory('Please select directory...', NewPath) then
  begin
    edSearchPath.Text := NewPath;
  end;
end;

procedure TOptionsFrame.cbStylesListChange(Sender: TObject);
begin
  if cbStylesList.ItemIndex <> -1 then
    ShowStylerStyles(cbStylesList.Items[cbStylesList.ItemIndex]);
end;

procedure TOptionsFrame.chkGutterVisibleClick(Sender: TObject);
begin
  chkGutterLineNumbers.Enabled := not chkGutterVisible.IsChecked;
  chkGutterZeroes.Enabled := not chkGutterVisible.IsChecked;
  spinGutterWidth.Enabled := not chkGutterVisible.IsChecked;
end;

procedure TOptionsFrame.chkMemoRightMarginClick(Sender: TObject);
begin
  spinMemoRightColumn.Enabled := not chkMemoRightMargin.IsChecked;
end;

procedure TOptionsFrame.FormCreate(Sender: TObject);
begin
  frmMain.duck.all.isa(TTMSFMXMemoCustomStyler).each(
  procedure(Obj: TObject)
  begin
    cbStylesList.Items.Add(
      TTMSFMXMemoCustomStyler(obj).StylerName);
  end
  );
{
  for i := 0 to frmMain.SLExtention.Count - 1 do
    cbStylesList.Items.Add(
      TTMSFMXMemoCustomStyler(frmMain.SLExtention.Objects[i]).StylerName);
}
end;

procedure TOptionsFrame.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  //if Key = System.UITypes.vkEscape then
  //  ModalResult := mrCancel;
end;

procedure TOptionsFrame.FormShow(Sender: TObject);
begin
  cbStylesList.ItemIndex := 0;
 // ShowStylerStyles('Pascal');
end;

{$IFDEF MSWINDOWS}
function BI_CallBack_Proc(hwnd: HWND; uMsg: UINT; lParam: DWORD;
  lpData: DWORD): integer; stdcall;
var
  PathName: array[0..MAX_PATH] of Char;
begin
  case uMsg of
    BFFM_INITIALIZED:
      SendMessage(Hwnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
    BFFM_SELCHANGED:
      begin
        SHGetPathFromIDList(PItemIDList(lParam), @PathName);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Longint(PChar(@PathName)));
      end;
  end;
  Result := 0;
end;
{$ENDIF MSWINDOWS}

function TOptionsFrame.SelectDirectory(const ATitle: string;
  var ADir: string): boolean;
{$IFDEF MSWINDOWS}
var
  hr: HRESULT;
  FormHandle: THandle;
  IDList: PItemIDList;
  RootIDList: PItemIDList;
  Malloc: IMalloc;
  lpBuf: LPTSTR;
  BI: TBrowseInfo;
  sCaption: string;
begin
  Result := False;
  FormHandle := FMX.Platform.Win.WindowHandleToPlatform(Handle).Wnd;
  ADir := EmptyStr;
  if (SHGetMalloc(Malloc) = S_OK) and (Malloc <> nil) then
  begin
    sCaption := ATitle;
    FillChar(BI, SizeOf(BI), 0);
    lpBuf := Malloc.Alloc(MAX_PATH);
    RootIDList := nil;
    SHGetSpecialFolderLocation(FormHandle, CSIDL_DESKTOP, RootIDList);
    with BI do
    begin
      hwndOwner := FormHandle;
      pidlRoot := RootIDList;
      pszDisplayName := lpBuf;
      lpszTitle := PWideChar(sCaption);
      ulFlags := BIF_NEWDIALOGSTYLE or BIF_USENEWUI;
      lpfn := @BI_CallBack_Proc;
      lParam := 0;
      iImage := 0;
    end;
    try
      hr := CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      if (hr = S_OK) or (hr = S_FALSE) then
      begin
        IDList := SHBrowseForFolder(BI);
        Result := IDList <> nil;
        if Result  then
        begin
          SHGetPathFromIDList(IDList, lpBuf);
          ADir := StrPas(lpBuf);
          Malloc.Free(RootIDList);
          RootIDList := nil;
          Malloc.Free(IDList);
          IDList := nil;
        end;
        CoUnInitialize();
      end;
    finally
      Malloc.Free(lpBuf);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
var
  LOpenDir: NSOpenPanel;
  LInitialDir: NSURL;
  LDlgResult: Integer;
begin
  Result := False;
  LOpenDir := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);
  LOpenDir.setAllowsMultipleSelection(False);
  LOpenDir.setCanChooseFiles(False);
  LOpenDir.setCanChooseDirectories(True);
  if ADir <> '' then
  begin
    LInitialDir := TNSURL.Create;
    LInitialDir.initFileURLWithPath(NSSTR(ADir));
    LOpenDir.setDirectoryURL(LInitialDir);
  end;
  if ATitle <> '' then
    LOpenDir.setTitle(NSSTR(ATitle));
  LOpenDir.retain;
  try
    LDlgResult := LOpenDir.runModal;
    if LDlgResult = NSOKButton then
    begin
      ADir := string(TNSUrl.Wrap(LOpenDir.URLs.objectAtIndex(0)).relativePath.UTF8String);
      Result := True;
    end;
  finally
    LOpenDir.release;
  end;
{$ENDIF MACOS}
{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
begin
{$ENDIF}
end;

procedure TOptionsFrame.ShowStylerStyles(AStylerName: string);
var
  Styler: TTMSFMXMemoCustomStyler;
  i: integer;
  dy: single;
  LText: TText;
  LComboColorBox: TComboColorBox;
begin
  for i := ComponentCount - 1 downto 0 do
    if Components[i] is TText then Components[i].Free;

  for i := ComponentCount - 1 downto 0 do
    if Components[i] is TComboColorBox then Components[i].Free;

  Styler := frmMain.GetSyntaxStylerByName(AStylerName);
  TMSFMXMemo1.SyntaxStyles := Styler;
  dy := DyIncrement;
  for i := 0 to Styler.AllStyles.Count - 1 do
  begin
    LText := TText.Create(Self);
    LText.Parent := ScrollBox1;
    Ltext.Name := 'MyText' + i.ToString;
    LText.Height := 22;
    LText.Width := ScrollBox1.Width - 32;
    LText.Position.X := 8;
    LText.Position.Y := dy;
    LText.HorzTextAlign := TTextAlign.Leading;
    LText.Text := Styler.AllStyles[i].Info;

    LComboColorBox := TComboColorBox.Create(Self);
    LComboColorBox.Parent := LText;
    LComboColorBox.Align := TAlignLayout.Right;
    LComboColorBox.Width := 94;
    LComboColorBox.Name := 'MyColorBox' + i.ToString;
    LComboColorBox.Color := Styler.AllStyles[i].FontColor;

    dy := dy + LText.Height + DyIncrement;
  end;
end;


end.
