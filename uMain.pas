unit uMain;

interface

uses
 System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Styles,
  FMX.Controls.Presentation, FMX.MultiView, FMX.TMSBaseControl, FMX.TMSMemo,
  FMX.StdCtrls, FMX.Layouts, FMX.TreeView, FMX.TMSMemoStyles, FMX.Platform,
  FMX.Objects, System.Actions, FMX.ActnList, FMX.TMSMemoDialogs, FMX.ListBox,
  FMX.TMSFindDialog, System.IOUtils, System.TypInfo, System.Rtti, FMX.Menus,
  FMX.Edit, FMX.ExtCtrls, System.ImageList, FMX.ImgList, System.IniFiles,
  FMX.Colors, FMX.TabControl, System.Threading, System.Generics.Collections,
  uMemoFrame, System.RegularExpressions, FMX.TextLayout, System.DateUtils,
  FMX.ListView.Types, FMX.ListView, FMX.Searchbox, Data.Bind.Components,
  Data.Bind.ObjectScope, PaxInterpreter, uPlugin,
  uWorkFiles, uMemoHexView, duck, OXmlPDOM, uCodeCompleteInfo, uMonitoring
  {$IFDEF MSWINDOWS}
  , FMX.Platform.Win
  {$ENDIF}
  {$IFDEF VER300}
   , FMX.ScrollBox, FMX.Memo, uOptions, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.Ani
  {$ENDIF}
  ;

type
  TGotoKind = (gtFile = 1, gtLine, gtCommand, gtCode);

  TMemoSettings = record
    FShowActiveLine: boolean;
    FShowActiveIndicator: boolean;
    FAutoOpen: boolean;
    FMarginVisible: boolean;
    FMarginColumn: integer;
    FGutterVisible: boolean;
    FGutterShowLineNumbers: boolean;
    FGutterShowLeadingZeroes: boolean;
    FGutterWidth: single;
    FTabStop: boolean;
    FTabSize: integer;
    FSmartTabs: boolean;
    FWantTab: boolean;
  end;

  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    TMSFMXMemoPascalStyler1: TTMSFMXMemoPascalStyler;
    Splitter1: TSplitter;
    Panel4: TPanel;
    TreeView1: TTreeView;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ActionList1: TActionList;
    actOpenFolder: TAction;
    actSaveFile: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actUndo: TAction;
    actRedo: TAction;
    Label1: TLabel;
    actFind: TAction;
    TMSFMXMemoFindDialog1: TTMSFMXMemoFindDialog;
    TMSFMXMemoCSSStyler1: TTMSFMXMemoCSSStyler;
    TMSFMXMemoBasicStyler1: TTMSFMXMemoBasicStyler;
    TMSFMXMemoJavaScriptStyler1: TTMSFMXMemoJavaScriptStyler;
    TMSFMXMemoWebStyler1: TTMSFMXMemoWebStyler;
    TMSFMXMemoXMLStyler1: TTMSFMXMemoXMLStyler;
    TMSFMXMemoSQLStyler1: TTMSFMXMemoSQLStyler;
    TMSFMXMemoCSharpStyler1: TTMSFMXMemoCSharpStyler;
    actSyntaxSelect: TAction;
    SyntaxSelectPopup: TPopupMenu;
    PascalSynMI: TMenuItem;
    CSharpSynMI: TMenuItem;
    CSSSynMI: TMenuItem;
    VBSynMI: TMenuItem;
    JSSynMI: TMenuItem;
    HTMLSynMI: TMenuItem;
    XMLSynMI: TMenuItem;
    SQLSynMI: TMenuItem;
    MemoStyleBook: TStyleBook;
    ApplicationDarkStyle: TStyleBook;
    MainMenu1: TMainMenu;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    actQuit: TAction;
    TMSFMXMemoFindAndReplaceDialog1: TTMSFMXMemoFindAndReplaceDialog;
    actFindText: TAction;
    ImageListWhite: TImageList;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    ImageListBlack: TImageList;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    actReplace: TAction;
    MenuItem25: TMenuItem;
    lbCaretPos: TLabel;
    actOptions: TAction;
    MenuItem26: TMenuItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    pnlSearch: TPanel;
    edFindInFiles: TEdit;
    actSearchInFiles: TAction;
    TreeView2: TTreeView;
    PlainSynMI: TMenuItem;
    Label2: TLabel;
    SearchLayout: TLayout;
    Rectangle1: TRectangle;
    MenuItem28: TMenuItem;
    actNewFile: TAction;
    actSaveAs: TAction;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    actSelectAll: TTMSFMXMemoSelectAll;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    actToggleFullscreen: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    ZoomLayout: TLayout;
    actClosePane: TAction;
    actCloseFindBar: TAction;
    actLightTheme: TAction;
    actDarkTheme: TAction;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    actAutoSave: TAction;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    actSplitEditor: TAction;
    Layout2: TLayout;
    MemoFrame1: TMemoFrame;
    Splitter2: TSplitter;
    MemoFrame2: TMemoFrame;
    Splitter3: TSplitter;
    MemoFrame3: TMemoFrame;
    ApplicationLightStyle: TStyleBook;
    ImageListGray: TImageList;
    AutosaveTimer: TTimer;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    Rectangle2: TRectangle;
    actClearSearch: TAction;
    actExpandAllSearch: TAction;
    actSearchRefresh: TAction;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    actSaveAll: TAction;
    actOpenFile: TAction;
    MenuItem47: TMenuItem;
    mnuGoto: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    actGoBack: TAction;
    actGoForward: TAction;
    actGotoLine: TAction;
    actGotoFile: TAction;
    actToggleSideBar: TAction;
    actTogglePluginBar: TAction;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    lbStyleSetting: TLabel;
    OnCreateTimer: TTimer;
    btnStyleSetting: TRectangle;
    actGotoCommand: TAction;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    actAddPlugin: TAction;
    actEditPlugin: TAction;
    actLoadPlugin: TAction;
    actSavePlugin: TAction;
    dlgOpenPlugin: TOpenDialog;
    actExplore: TAction;
    MemoPopup: TPopupMenu;
    PluginsPanel: TPanel;
    lvPlugins: TListView;
    MenuBar1: TMenuBar;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    actShowAbout: TAction;
    MenuItem60: TMenuItem;
    rectAbout: TRectangle;
    btnCloseAbout: TButton;
    Layout4: TLayout;
    Label3: TLabel;
    pnlSearchOption: TPanel;
    Label4: TLabel;
    edFileNameMask: TEdit;
    Label5: TLabel;
    edFoldersInclude: TEdit;
    Label6: TLabel;
    edFoldersExclude: TEdit;
    chkCaseSensitive: TCheckBox;
    Label7: TLabel;
    edPathToRtl: TEdit;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    SpeedButton13: TSpeedButton;
    Image13: TImage;
    SearchProgressBar: TProgressBar;
    actSwitchToSearch: TAction;
    lbEncoding: TLabel;
    MenuItem61: TMenuItem;
    txtNameVer: TText;
    lbCRLF: TLabel;
    actGoToCode: TAction;
    MenuItem62: TMenuItem;
    TMSFMXMemoCPPStyler: TTMSFMXMemoXMLStyler;
    TMSFMXMemoJavaStyler: TTMSFMXMemoJavaScriptStyler;
    ProgressBar1: TProgressBar;
    txtHexViewProgress: TText;
    CPPSynMI: TMenuItem;
    JavaSynMI: TMenuItem;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    Image1: TImage;
    SpeedButton2: TSpeedButton;
    Image2: TImage;
    SpeedButton3: TSpeedButton;
    Image3: TImage;
    SpeedButton4: TSpeedButton;
    Image4: TImage;
    SpeedButton5: TSpeedButton;
    Image5: TImage;
    SpeedButton6: TSpeedButton;
    Image6: TImage;
    SpeedButton7: TSpeedButton;
    Image7: TImage;
    SpeedButton8: TSpeedButton;
    Image8: TImage;
    SpeedButton9: TSpeedButton;
    Image9: TImage;
    actExportToHTML: TAction;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    BGLogo: TRectangle;
    AboutLogo: TRectangle;
    Rectangle3: TRectangle;
    ParseTimer: TTimer;
    Memo1: TMemo;
    lblCompile: TLabel;
    lblChangeCCPlatform: TLabel;
    popupCCPlatform: TPopupMenu;
    WindowsMI: TMenuItem;
    AndroidMI: TMenuItem;
    MacosMI: TMenuItem;
    IOSMI: TMenuItem;
    actChangeCCPlatform: TAction;
    btnCCPlatform: TRectangle;
    GetTokenMemo: TTMSFMXMemo;
    FrameOptions: TOptionsFrame;
    MenuItem67: TMenuItem;
    actHelp: TAction;
    Layout1: TLayout;
    actCPP: TAction;
    actXML: TAction;
    actHTML: TAction;
    actPAS: TAction;
    actJava: TAction;
    actCSharp: TAction;
    actJS: TAction;
    actVB: TAction;
    actCSS: TAction;
    actSQL: TAction;
    actPlain: TAction;
    actRemovePlugin: TAction;
    actOpenZipFile: TAction;
    actOnAfterSaveMemo: TAction;
    actReloadFrame: TAction;
    fltnmtnPB: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure actOpenFolderExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actFindExecute(Sender: TObject);
    procedure actCutUpdate(Sender: TObject);
    procedure actCopyUpdate(Sender: TObject);
    procedure actUndoUpdate(Sender: TObject);
    procedure actRedoUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TMSFMXMemo1GetAutoCompletionList(Sender: TObject; AToken: string;
      AList: TStringList);
    procedure actSyntaxSelectExecute(Sender: TObject);
    procedure CSharpSynMIClick(Sender: TObject);
    procedure CSSSynMIClick(Sender: TObject);
    procedure VBSynMIClick(Sender: TObject);
    procedure JSSynMIClick(Sender: TObject);
    procedure HTMLSynMIClick(Sender: TObject);
    procedure XMLSynMIClick(Sender: TObject);
    procedure SQLSynMIClick(Sender: TObject);
    procedure actQuitExecute(Sender: TObject);
    procedure actFindTextExecute(Sender: TObject);
    procedure actFindTextUpdate(Sender: TObject);
    procedure edFindTextChange(Sender: TObject);
    procedure edFindTextKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure actReplaceExecute(Sender: TObject);
    procedure actReplaceUpdate(Sender: TObject);
    procedure actFindUpdate(Sender: TObject);
    procedure TMSFMXMemo1KeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure TMSFMXMemo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actOptionsExecute(Sender: TObject);
    procedure edFindInFilesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure btnFindInFilesKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure actSearchInFilesExecute(Sender: TObject);
    procedure PlainSynMIClick(Sender: TObject);
    procedure actToggleFullscreenExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actLightThemeExecute(Sender: TObject);
    procedure actDarkThemeExecute(Sender: TObject);
    procedure actSplitEditorExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSaveFileUpdate(Sender: TObject);
    procedure actSaveAsUpdate(Sender: TObject);
    procedure AutosaveTimerTimer(Sender: TObject);
    procedure actAutoSaveExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actClosePaneExecute(Sender: TObject);
    procedure actCloseFindBarExecute(Sender: TObject);
    procedure actSearchRefreshExecute(Sender: TObject);
    procedure actClearSearchExecute(Sender: TObject);
    procedure actExpandAllSearchExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actTogglePluginBarExecute(Sender: TObject);
    procedure actToggleSideBarExecute(Sender: TObject);
    procedure OnCreateTimerTimer(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure actGoBackExecute(Sender: TObject);
    procedure actGoForwardExecute(Sender: TObject);
    procedure actGoBackUpdate(Sender: TObject);
    procedure actGoForwardUpdate(Sender: TObject);
    procedure actGotoFileExecute(Sender: TObject);
    procedure actGotoLineExecute(Sender: TObject);
    procedure actGotoCommandExecute(Sender: TObject);
    procedure actAddPluginExecute(Sender: TObject);
    procedure actLoadPluginExecute(Sender: TObject);
    procedure actSavePluginExecute(Sender: TObject);
    procedure actEditPluginExecute(Sender: TObject);
    procedure pmPluginsPopup(Sender: TObject);
    procedure PaxCompiler1UndefineDirective(Sender: TPaxInterpreter;
      const Directive: string; var ok: Boolean);
    procedure actExploreExecute(Sender: TObject);
    procedure ZoomLayoutResize(Sender: TObject);
    procedure actShowAboutExecute(Sender: TObject);
    procedure btnCloseAboutClick(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure TabControl1Resize(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure lvPluginsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure lvPluginsChange(Sender: TObject);
    procedure lvPluginsDblClick(Sender: TObject);
    procedure actSwitchToSearchExecute(Sender: TObject);
    procedure actGoToCodeExecute(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure JavaSynMIClick(Sender: TObject);
    procedure actExportToHTMLExecute(Sender: TObject);
    procedure ParseTimerTimer(Sender: TObject);
    procedure MemoFrame2ImageViewer1MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoFrame2treeZipViewMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure actChangeCCPlatformExecute(Sender: TObject);
    procedure WindowsMIClick(Sender: TObject);
    procedure AndroidMIClick(Sender: TObject);
    procedure MacosMIClick(Sender: TObject);
    procedure IOSMIClick(Sender: TObject);
    procedure FrameOptionsButton1Click(Sender: TObject);
    procedure FrameOptionsButton2Click(Sender: TObject);
    procedure actHelpExecute(Sender: TObject);
    procedure actCPPExecute(Sender: TObject);
    procedure actPASExecute(Sender: TObject);
    procedure TreeView2Click(Sender: TObject);
    procedure actRemovePluginExecute(Sender: TObject);
    procedure actOpenZipFileExecute(Sender: TObject);
    procedure actOnAfterSaveMemoExecute(Sender: TObject);
    procedure actReloadFrameExecute(Sender: TObject);
  private
    { Private declarations }
    //--------------------------------------------------------------------------
    FMemoGutterLightFontColor: TAlphaColor;
    FMemoGutterDarkFontColor: TAlphaColor;
    //--------------------------------------------------------------------------
    SLFormProperties: TStringList;
    SLButtonProperties: TStringList;
    SLEditProperties: TStringList;
    SLFormMethods: TStringList;
    SLButtonMethods: TStringList;
    SLEditMethods: TStringList;
    SLFormEvents: TStringList;
    SLButtonEvents: TStringList;
    SLEditEvents: TStringList;
    SLFormHintEvents: TStringList;
    SLButtonHintEvents: TStringList;
    SLEditHintEvents: TStringList;

    FActiveProjectDir: String;
    FBreakSearchInFiles: boolean;
    FAutoSave: boolean;
    FNewFileCount: Integer;

    FSearchPath: string;
    FRTLPath: string;
    FCurrModuleName: string;
    FPlugins: TPlugins;

    FCodeList : TStringList;
    FToken : string;
    FLock : Boolean;
    FOpenParam : Boolean;
    FBktPos : TPoint;

    FMemoSettings: TMemoSettings;

    FMonitoring : TMonitoring;
    //--------------------------------------------------------------------------
    function  GetSyntaxStyler(const AFileExt: string): TTMSFMXMemoCustomStyler;
    function  ShowImage(const AFileName: string): boolean;
    procedure ShowCaretPos;
    procedure MemoLoadFile(const AFileName: string; const AMemoFrame: TMemoFrame;
      const ALineNumber: Integer = 0);
    procedure MemoExportFile(const AFileName, AExtention: string);
    procedure MemoSaveFile(const AFilename: string);
    function  MemoCheckModified(AMemoFrame: TMemoFrame): Boolean;
    procedure MemoFindText;
    procedure MemoUndoEdit;
    procedure MemoRedoEdit;
    procedure MemoCutEdit;
    procedure MemoCopyEdit;
    procedure MemoPasteEdit;
    procedure MemoSearchEdit;
    procedure MemoReplaceEdit;
    //--------------------------------------------------------------------------
    procedure NewFile(AMemoFrame: TMemoFrame = nil);
    function  NewDefFileName: string;
    procedure SearchInFiles;
    procedure SearchTextInFile(AFileName, AText: string);
    procedure ShowSearchOptions(AVisible: boolean);
    procedure WalkSearch(AItem: TTreeViewItem);
    procedure SaveAll;
    procedure SetTheme(AThemeIndex: integer);
    //-------------------------------------------------------------------------
    function  GetPasUnitFile(AFormFileName: string): string;
    function  GetFormUnitFile(AUnitFileName: string): string;
    procedure SelectLine(lNumber: Integer);
    procedure EmptyButtonsText;
    procedure ReArrangeMemoFrames;
    function  SplitEditor(Sender: TObject): TMemoFrame; overload;
    function  SplitEditor(ATag: integer): TMemoFrame; overload;
    procedure TryOpenFormUnit(AFileName: string);
    procedure UpdateImages(AImageList: TImagelist);
    procedure UpdateActiveProjectDir;
    //------------------------------------- Plugins ----------------------------
    procedure PluginChange(Sender: TObject; const Item: TPlugin;
        Action: TCollectionNotification);
    procedure PluginLoadListView; overload;
    procedure PluginLoadListView(const ALv : TListView); overload;
    procedure PluginAdd;
    procedure PluginEdit;
    procedure PluginLoad;
    procedure PluginOpen;
    procedure PluginSave;
    procedure PluginRemove;
    procedure ShowPluginsPanel(AVisible: boolean);
    //------------------------------------- Help (About) -----------------------
    procedure ShowAbout;
    //--------------------------------------------------------------------------
    procedure TreeViewClick(Sender: TObject; AIndex: integer);
    function GetGutterVisible(AMemo: TTMSFMXMemo): boolean;
    function GetGutterWidth(AMemo: TTMSFMXMemo): single;
    procedure SetGutterWidth(AMemo: TTMSFMXMemo; AValue: single);
    procedure SetGutterVisible(AMemo: TTMSFMXMemo; AValue: boolean);
    procedure SetMemoSettings(AMemo: TTMSFMXMemo);
    //------------------------------------- Auto Complete ----------------------
    procedure CursorChange(Sender: TObject);
    procedure GetAutoCompletionList(Sender: TObject; AToken: string;
      AList: TStringList);
    procedure InsertAutoCompletionEntry(Sender: TObject;
      var AEntry: string);
    procedure AutoCompletionCustomizeList(Sender: TObject;
      AAutoCompletionList: TMemoComboListBox);
    procedure AutoCompletionCustomizeItem(Sender: TObject;
      AAutoCompletionItem: TListBoxItem);
    procedure AutoCompleteListApplyStyleLookup(Sender: TObject);
    procedure BeforeAutoCompletion(Sender: TObject; AToken: string;
      var Show: Boolean);
    function GetWIdthText(AObj : TText) : Single;
    procedure ChangeCode(const ACode: string);
    function FindFullToken(const CurX, CurY: Integer): string;
    function ParseContent(const Content: string): string;
    procedure ChangeCodeCompletionPlatform(Sender: TObject);
    procedure SetOpenParam(const Value: Boolean);
    procedure MemoKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    //------------------------------------- Monitoring (File changes) -----------------
     procedure ChangeFile(const AFName : string);
     procedure SetFileChanged(const AFName : string);
     procedure InitMonitoring;
     procedure CheckFileChanged(AMemoFrame: TMemoFrame);
    function GetLoading: Boolean;
    //--------------------------------------------------------------------------
  public
    { Public declarations }
    FAddToWorkFiles: boolean;
    FSelectedMemo: TTMSFMXMemo;
    FSelectedFrame: TMemoFrame;
    FThemeIndex: integer;     // 1 - White, 2 - Black

    FileHistory: TStringList;
    FCurrHistPos: integer;
    SLExtention: TStringList;

    property OpenParam : Boolean read FOpenParam write SetOpenParam;

    procedure OpenFile(const AFileName: String);

    procedure SetMemoFocus(AMemoFrame: TMemoFrame);
    function GetMemoFrameByTag(ATag: integer): TMemoFrame;
    procedure LoadFrameFromFile(const AFileName: String; const AMemoFrame: TMemoFrame;
      const ALineNumber: Integer = 0; AReadOnly : Boolean = False);
    procedure AddHints(Sender: TObject);
    function GetSyntaxStylerByName(const AStylerName: string): TTMSFMXMemoCustomStyler;
    //-------------------------------------- History routine -------------------
    procedure AddToHistoryList(const AFileName: string);
    procedure MoveHistoryForward;
    procedure MoveHistoryBackward;
    procedure RemoveFromHistoryList(const AFileName: string); overload;
    procedure RemoveFromHistoryList(AIndex: integer); overload;
    procedure ShowHistoryFile;
    //-------------------------------------- GoTo routine ----------------------
    procedure GoToListHide(AMemoFrame: TMemoFrame = nil);
    procedure GoToListShow(GotoKind: TGotoKind; AMemoFrame: TMemoFrame = nil);
    procedure GoToListFileName(AFileName: string; AMemoFrame: TMemoFrame = nil);
    procedure GoToListLineNumber(ALineNumber: integer; AMemoFrame: TMemoFrame = nil);
    procedure GoToListCommand(AMemoFrame: TMemoFrame = nil);
    procedure GoToCode(AMethodName: string);
    procedure GoToListExec(Sender: TObject; AMemoFrame: TMemoFrame = nil);
    procedure GoToListUpDown(AKey : Word; AMemoFrame: TMemoFrame = nil);
    //--------------------------------------------------------------------------
    function  MemoFrameVisibleCount: integer;
    procedure OnCreateEvent(Sender: TObject);
    procedure SelectFrame(ATMSFMXMemo: TTMSFMXMemo);
    procedure SaveSettings;
    procedure LoadSettings;
    //------------------------------------- Progress Bar -----------------------
    procedure ProgressWorkBegin(Sender: TObject; AText : string;
        Animated : Boolean = False);
    procedure ProgressAnimenteBegin(Sender : TObject);
    //------------------------------------- Hex Viewer -------------------------
    procedure HexVWorkBegin(Sender: TObject);
    procedure HexVWorkEnd(Sender: TObject);
    procedure HexVProgress(Sender: TObject; BytesCount, Percent: integer);
    //--------------------------------------------------------------------------
    procedure ShowFileName(const AFileName: string);
    procedure ShowCaption;
    procedure SaveOptionsFrame(FOK: Boolean);
    procedure CloseOptionsFrame;

    property Plugins : TPlugins read FPlugins;
    property Loading : Boolean read GetLoading;
  end;

var
  frmMain: TfrmMain;
  WorkFilesTree: TWorkFilesTree = nil;
  HexViewer: THexViewer = nil;
  Info : TCodeCompleteInfo;

implementation

uses
  // code completion section
  DelphiAST, DelphiAST.Writer, DelphiAST.Classes, DelphiAST.Consts,
  uHelper.SyntaxNode, uCOnst, uSerializer, uUnit,
  // editor units
  System.StrUtils, uFilesTree, uConsts, uUtils
{$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
  , HS_FMXHints
{$ENDIF}
  , uPluginEdit, uPluginUtils, OpenViewUrl, uScripts, uZipView,
    uFileTypeHelper, uMemoBinaryView;

{$R *.fmx}
{$R *.LgXhdpiTb.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}

var
  FilesTree: TProtoFilesTree = nil;
  SearchFilesTree: TProtoFilesTree = nil;
  SyntaxTree_: TSyntaxNode;


procedure TfrmMain.LoadSettings;
var
  IniFile: TMemIniFile;
  SettingsDir: string;
  DefLeft: integer;
  DefTop: integer;
  FullScr: boolean;
begin
 //{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
 // SettingsDir := TPath.GetDirectoryName(ParamStr(0));
 //{$ELSE}
  SettingsDir := System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim;
 //{$ENDIF}
 IniFile := TMemIniFile.Create(TPath.Combine(SettingsDir,'settings.ini'));
 try
 Self.duck.all.has('AllStyles').each(
    procedure(obj: TObject)
    var
    I: Integer;
    Styler: TTMSFMXMemoCustomStyler;
    begin
      Styler := TTMSFMXMemoCustomStyler(obj);
      for I := 0 to Styler.AllStyles.Count-1 do
       begin
        Styler.AllStyles[I].FontColor := IniFile.ReadInteger('Stylers',
        Styler.Name + '.' + (Styler.AllStyles[I]).DisplayName,
        Styler.AllStyles[I].FontColor);
       end;
        Styler.CommentStyle.TextColor := IniFile.ReadInteger('Stylers',Styler.Name + '.' + 'CommentStyle',Styler.CommentStyle.TextColor);
        Styler.NumberStyle.TextColor := IniFile.ReadInteger('Stylers',Styler.Name + '.' + 'NumberStyle',Styler.NumberStyle.TextColor);
        Styler.HighlightStyle.TextColor := IniFile.ReadInteger('Stylers',Styler.Name + '.' + 'HighlightStyle',Styler.HighlightStyle.TextColor);
        Styler.HighlightStyle.BkColor := IniFile.ReadInteger('Stylers',Styler.Name + '.' + 'HighlightStyleBackground',Styler.HighlightStyle.BkColor);
        Styler.URLStyle.TextColor := IniFile.ReadInteger('Stylers',Styler.Name + '.' + 'URLStyle',Styler.URLStyle.TextColor);
    end
  );

  FThemeIndex := IniFile.ReadInteger('Theme', 'Index', 2);
  PluginsPanel.Visible := IniFile.ReadBool('Controls',PluginsPanel.Name+'.Visible', False);
  TabControl1.Visible := IniFile.ReadBool('Controls',TabControl1.Name+'.Visible',TabControl1.Visible);
  Splitter1.Visible := IniFile.ReadBool('Controls',Splitter1.Name+'.Visible',Splitter1.Visible);
  FMemoGutterLightFontColor := IniFile.ReadInteger('MemoGutterFontColor', 'Light', claBlack);
  FMemoGutterDarkFontColor := IniFile.ReadInteger('MemoGutterFontColor', 'Dark', claDarkGray);


  FRTLPath := IniFile.ReadString('SearchInFiles', 'RTL', CRTLPath);
  edPathToRtl.Text := FRTLPath;
  FSearchPath := IniFile.ReadString('SearchInFiles', 'SearchPath', '');
  edFileNameMask.Text := IniFile.ReadString('SearchInFiles', 'FileNameMask', '');
  edFoldersInclude.Text := IniFile.ReadString('SearchInFiles', 'FoldersInclude', '');
  edFoldersExclude.Text := IniFile.ReadString('SearchInFiles', 'FoldersExclude', '__history;__recovery');
  chkCasesensitive.IsChecked := IniFile.ReadBool('SearchInFiles', 'CaseSensitive', False);


  with FMemoSettings do
  begin
    FShowActiveLine := IniFile.ReadBool('General', 'ShowActiveLine', False);
    FShowActiveIndicator := IniFile.ReadBool('General', 'ShowActiveLineIndicator', False);
    FAutoOpen := IniFile.ReadBool('General', 'AutoOpenForm', True);
    FMarginVisible := IniFile.ReadBool('General', 'MarginVisible', False);
    FMarginColumn := IniFile.ReadInteger('General', 'MarginColumn', DefRightMargin);
    FGutterVisible := IniFile.ReadBool('General', 'GutterVisible', True);
    FGutterShowLineNumbers := IniFile.ReadBool('General', 'GutterLine', True);
    FGutterShowLeadingZeroes := IniFile.ReadBool('General', 'GutterZeroes', False);
    FGutterWidth := IniFile.ReadFloat('General', 'GutterWidth', DefGutterWidth);
    FTabStop := IniFile.ReadBool('General', 'TabStop', False);
    FTabSize := IniFile.ReadInteger('General', 'TabSize', DefTabSize);
    FSmartTabs := IniFile.ReadBool('General', 'SmartSize', False);
    FWantTab := IniFile.ReadBool('General', 'WantTab', True);
  end;
  SetMemoSettings(MemoFrame1.TMSFMXMemo1);
  SetMemoSettings(MemoFrame2.TMSFMXMemo1);
  SetMemoSettings(MemoFrame3.TMSFMXMemo1);

  DefLeft := Trunc((Screen.Width / 2) - (Width / 2));
  DefTop := Trunc((Screen.Height / 2) - (Height / 2));
  FullScr := IniFile.ReadBool('Position', 'FullScreen', False);
  if FullScr then
    FullScreen := True
  else
  begin
    Left := IniFile.ReadInteger('Position', 'Left', DefLeft);
    Top := IniFile.ReadInteger('Position', 'Top', DefTop);
    Width := IniFile.ReadInteger('Position', 'Width', 712);
    Height := IniFile.ReadInteger('Position', 'Height', 480);
  end;
 finally
  IniFile.Free;
 end;
end;

procedure TfrmMain.lvPluginsChange(Sender: TObject);
begin
  FPlugins.ItemIndex := TListView(Sender).ItemIndex;
end;

procedure TfrmMain.lvPluginsDblClick(Sender: TObject);
begin
  PluginOpen;
end;

procedure TfrmMain.lvPluginsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  FPlugins.ItemIndex := TListView(Sender).ItemIndex;
  //actOpenPluginExecute(Sender);
end;

procedure TfrmMain.SaveAll;
begin
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      if TMemoFrame(obj).Visible and
        (not SameText(TMemoFrame(obj).FPredFileName, CDefFileName)) then
        try
          //TMemoFrame(obj).TMSFMXMemo1.Lines.SaveToFile(TMemoFrame(obj).FPredFileName);
          TMemoFrame(obj).SaveMemoLines;
          TMemoFrame(obj).TMSFMXMemo1.ClearUndoRedo;
          TMemoFrame(obj).FMemoChanged := False;
        except
          //
        end;
    end
  );
end;

procedure TfrmMain.SaveSettings;
var
  IniFile: TMemIniFile;
  SettingsDir: string;
  AMemo: TTMSFMXMemo;
begin
 //{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
 // SettingsDir := TPath.GetDirectoryName(ParamStr(0));
 //{$ELSE}
  SettingsDir := System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim;
 //{$ENDIF}
 IniFile := TMemIniFile.Create(TPath.Combine(SettingsDir,'settings.ini'));
 try
  Self.duck.all.has('AllStyles').each(
    procedure(obj: TObject)
    var
    I: Integer;
    Styler: TTMSFMXMemoCustomStyler;
    begin
      Styler := TTMSFMXMemoCustomStyler(obj);
      for I := 0 to Styler.AllStyles.Count-1 do
       begin
        IniFile.WriteInteger('Stylers',
        Styler.Name + '.' + TElementStyle(Styler.AllStyles[I]).DisplayName,
        Styler.AllStyles[I].FontColor);
       end;
      IniFile.WriteInteger('Stylers',Styler.Name + '.' + 'CommentStyle',Styler.CommentStyle.TextColor);
      IniFile.WriteInteger('Stylers',Styler.Name + '.' + 'NumberStyle',Styler.NumberStyle.TextColor);
      IniFile.WriteInteger('Stylers',Styler.Name + '.' + 'HighlightStyle',Styler.HighlightStyle.TextColor);
      IniFile.WriteInteger('Stylers',Styler.Name + '.' + 'HighlightStyleBackground',Styler.HighlightStyle.BkColor);
      IniFile.WriteInteger('Stylers',Styler.Name + '.' + 'URLStyle',Styler.URLStyle.TextColor);
    end
  );

  IniFile.WriteInteger('Theme', 'Index', FThemeIndex);
  IniFile.WriteBool('Controls',PluginsPanel.Name+'.Visible',PluginsPanel.Visible);
  IniFile.WriteBool('Controls',TabControl1.Name+'.Visible',TabControl1.Visible);
  IniFile.WriteBool('Controls',Splitter1.Name+'.Visible',Splitter1.Visible);
  IniFile.WriteInteger('MemoGutterFontColor', 'Light', FMemoGutterLightFontColor);
  IniFile.WriteInteger('MemoGutterFontColor', 'Dark', FMemoGutterDarkFontColor);

  FRTLPath := edPathToRtl.Text;
  IniFile.WriteString('SearchInFiles', 'RTL', FRTLPath);
  IniFile.WriteString('SearchInFiles', 'SearchPath', FSearchPath);
  IniFile.WriteString('SearchInFiles', 'FileNameMask', edFileNameMask.Text);
  IniFile.WriteString('SearchInFiles', 'FoldersInclude', edFoldersInclude.Text);
  IniFile.WriteString('SearchInFiles', 'FoldersExclude', edFoldersExclude.Text);
  IniFile.WriteBool('SearchInFiles', 'CaseSensitive', chkCasesensitive.IsChecked);


  AMemo := FSelectedMemo;
  if AMemo = nil then
    AMemo := MemoFrame2.TMSFMXMemo1;
  IniFile.WriteBool('General', 'ShowActiveLine', AMemo.ActiveLineSettings.ShowActiveLine);
  IniFile.WriteBool('General', 'ShowActiveLineIndicator', AMemo.ActiveLineSettings.ShowActiveLineIndicator);
  IniFile.WriteBool('General', 'AutoOpenForm', FMemoSettings.FAutoOpen);
  IniFile.WriteBool('General', 'MarginVisible', AMemo.ShowRightMargin);
  IniFile.WriteInteger('General', 'MarginColumn', AMemo.RightMargin);
  IniFile.WriteBool('General', 'GutterVisible', GetGutterVisible(AMemo));
  IniFile.WriteBool('General', 'GutterLine', AMemo.Gutter.ShowLineNumbers);
  IniFile.WriteBool('General', 'GutterZeroes', AMemo.Gutter.ShowLeadingZeros);
  IniFile.WriteFloat('General', 'GutterWidth', GetGutterWidth(AMemo));
  IniFile.WriteBool('General', 'TabStop', AMemo.TabStop);
  IniFile.WriteInteger('General', 'TabSize', AMemo.TabSize);
  IniFile.WriteBool('General', 'SmartSize', AMemo.SmartTabs);
  IniFile.WriteBool('General', 'WantTab', AMemo.WantTab);

  if FullScreen then
    IniFile.WriteBool('Position', 'FullScreen', True)
  else
  begin
    IniFile.WriteBool('Position', 'FullScreen', False);
    IniFile.WriteInteger('Position', 'Left', Left);
    IniFile.WriteInteger('Position', 'Top', Top);
    IniFile.WriteInteger('Position', 'Width', Width);
    IniFile.WriteInteger('Position', 'Height', Height);
  end;
 finally
  IniFile.UpdateFile;
  IniFile.Free;
 end;
end;

procedure TfrmMain.actAddPluginExecute(Sender: TObject);
begin
  PluginAdd;
end;

procedure TfrmMain.actAutoSaveExecute(Sender: TObject);
begin
  FAutoSave := not FAutoSave;
  actAutoSave.Checked := FAutoSave;
  AutosaveTimer.Enabled := FAutoSave;
end;

procedure TfrmMain.actClearSearchExecute(Sender: TObject);
begin
  SearchFilesTree.Clear;
  edFindInFiles.Text := EmptyStr;
end;

procedure TfrmMain.actCloseFindBarExecute(Sender: TObject);
begin
  if Sender.ClassType=TButton then
    TPanel(TButton(Sender).Parent).Visible := False;
end;

procedure TfrmMain.actClosePaneExecute(Sender: TObject);
var
  VictimFrame: TMemoFrame;
  SelectedSet: Boolean;
begin
  VictimFrame := GetMemoFrameByTag(TAction(Sender).Target.Tag);
  if VictimFrame = nil then
    VictimFrame := FSelectedFrame;
  if (VictimFrame = nil) then// or (not VictimFrame.Visible) then
    Exit;

  if MemoCheckModified(VictimFrame) then
   begin
    ParseTimer.Enabled := False;
    WorkFilesTree.MoveToMemoStream(VictimFrame);

    if (not VictimFrame.FMemoChanged) and (VictimFrame.TMSFMXMemo1.Lines.Count = 1) then
    begin
      if VictimFrame.FPredFileName.Contains(CDefFileName) then
        WorkFilesTree.DeleteFromWorkList(VictimFrame.FPredFileName, VictimFrame.Tag);
    end;

    VictimFrame.Visible := False;
    //Application.ProcessMessages;
    //VictimFrame.TMSFMXMemo1.Lines.Clear;
    VictimFrame.TMSFMXMemo1.Lines.Text := string.Empty;
    VictimFrame.TMSFMXMemo1.ClearUndoRedo;
    VictimFrame.FMemoChanged := False;
    VictimFrame.FPredFileName := EmptyStr;

    case VictimFrame.Tag of
      1: begin
            Splitter2.Visible := False;
      end;
      2: begin
            if MemoFrame3.Visible then Splitter3.Visible := False;
            if MemoFrame1.Visible then Splitter2.Visible := False;
      end;
      3: begin
            Splitter3.Visible := False;
      end;
    end;
    ReArrangeMemoFrames;

    Self.duck.all.isa(TMemoFrame).each(
      procedure(obj: TObject)
      begin
        if (TMemoFrame(obj).Visible=True) AND (SelectedSet=False) then
         begin
          SetMemoFocus(TMemoFrame(obj));
          ShowFileName(FSelectedFrame.FPredFileName);
          SelectedSet := True;
         end;
      end
    );

    ParseTimer.Enabled := True;
  end;

end;

procedure TfrmMain.actCopyExecute(Sender: TObject);
begin
  MemoCopyEdit;
end;

procedure TfrmMain.actCopyUpdate(Sender: TObject);
begin
 if FSelectedMemo <> nil then
  actCopy.Enabled := FSelectedMemo.CanCopy;
end;

procedure TfrmMain.actCutExecute(Sender: TObject);
begin
  MemoCutEdit;
end;

procedure TfrmMain.actCutUpdate(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  actCut.Enabled := FSelectedMemo.CanCut;
end;

procedure TfrmMain.actDarkThemeExecute(Sender: TObject);
begin
  FThemeIndex := 2;
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      obj.duck.all.isa(TTMSFMXMemo).each(
        procedure(obj: TObject)
        begin
          TTMSFMXMemo(obj).StyleLookup := 'TMSFMXMemo1Style1';
          TTMSFMXMemo(obj).FontColor := TAlphaColorRec.Whitesmoke;
          TTMSFMXMemo(obj).Gutter.FontColor := FMemoGutterDarkFontColor;
        end
      );
    end
  );
  StyleBook := ApplicationDarkStyle;
  UpdateImages(ImageListWhite);
end;

procedure TfrmMain.actToggleFullscreenExecute(Sender: TObject);
begin
  Self.FullScreen := not Self.FullScreen;
end;

procedure TfrmMain.actEditPluginExecute(Sender: TObject);
begin
  PluginEdit;
end;

procedure TfrmMain.actExpandAllSearchExecute(Sender: TObject);
begin
  SearchFilesTree.ExpandAll;
end;

procedure TfrmMain.actExploreExecute(Sender: TObject);
begin
  if not TabControl1.Visible then
   TabControl1.Visible := True;
  TabControl1.ActiveTab := TabItem1;
  Application.ProcessMessages;
end;

procedure TfrmMain.actSearchInFilesExecute(Sender: TObject);
begin
  if SearchFilesTree.SearchRunning then
    Exit;

  if FActiveProjectDir <> EmptyStr then
  begin
    SearchFilesTree.FilesPattern := Trim(edFileNameMask.Text);
    SearchFilesTree.IncludeFolders := Trim(edFoldersInclude.Text);
    SearchFilesTree.ExcludeFolders := Trim(edFoldersExclude.Text);
    SearchFilesTree.CaseSensitive := chkCasesensitive.IsChecked;
    SearchFilesTree.PathToRTL := FSearchPath;
    if edPathToRTL.Text.IsEmpty then
      Trim(edPathToRtl.Text);
    ShowSearchOptions(False);
    if not Trim(edFindInFiles.Text).IsEmpty then
      SearchFilesTree.TaskEnumDirSearch(edFindInFiles.Text, FActiveProjectDir);
  end;
end;

procedure TfrmMain.actFindTextExecute(Sender: TObject);
begin
 MemoFindText;
end;

procedure TfrmMain.actFindTextUpdate(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    actFindText.Enabled := (FSelectedMemo.Lines.Count > 0) and
      (FSelectedFrame.edFindText.Text <> EMptyStr);
  end;
end;

procedure TfrmMain.actGoForwardExecute(Sender: TObject);
begin
  MoveHistoryForward;
end;

procedure TfrmMain.actGoForwardUpdate(Sender: TObject);
begin
  actGoForward.Enabled := (FileHistory.Count > 1) and
    (FCurrHistPos < Pred(FileHistory.Count));
end;

procedure TfrmMain.actGoToCodeExecute(Sender: TObject);
begin
  GoToListShow(gtCode);
end;

procedure TfrmMain.actGotoCommandExecute(Sender: TObject);
begin
  GoToListShow(gtCommand);
end;

procedure TfrmMain.actGotoFileExecute(Sender: TObject);
begin
  GoToListShow(gtFile);
end;

procedure TfrmMain.actGotoLineExecute(Sender: TObject);
begin
  GoToListShow(gtLine);
end;

procedure TfrmMain.actHelpExecute(Sender: TObject);
begin
OpenURL('http://docwiki.embarcadero.com/',False);
end;

procedure TfrmMain.actCPPExecute(Sender: TObject);
begin
  if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoCppStyler;
    lbStyleSetting.Text := 'C++';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.actLightThemeExecute(Sender: TObject);
begin
  FThemeIndex := 1;
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      obj.duck.all.isa(TTMSFMXMemo).each(
        procedure(obj: TObject)
        begin
          TTMSFMXMemo(obj).StyleLookup := '';
          TTMSFMXMemo(obj).FontColor := TAlphaColorRec.Black;
          TTMSFMXMemo(obj).Gutter.FontColor := FMemoGutterLightFontColor;
        end
      );
    end
  );
  StyleBook := ApplicationLightStyle;
  UpdateImages(ImageListGray);
end;

procedure TfrmMain.actLoadPluginExecute(Sender: TObject);
begin
  PluginLoad;
end;

procedure TfrmMain.actNewFileExecute(Sender: TObject);
begin
  NewFile();
end;

procedure TfrmMain.actGoBackExecute(Sender: TObject);
begin
  MoveHistoryBackward;
end;

procedure TfrmMain.actGoBackUpdate(Sender: TObject);
begin
  actGoBack.Enabled := FCurrHistPos > 0;
end;

procedure TfrmMain.actReplaceExecute(Sender: TObject);
begin
  MemoReplaceEdit;
end;

procedure TfrmMain.actReplaceUpdate(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  actReplace.Enabled := FSelectedMemo.Lines.Count > 0;
end;

procedure TfrmMain.actRedoExecute(Sender: TObject);
begin
  MemoRedoEdit;
end;

procedure TfrmMain.actRedoUpdate(Sender: TObject);
begin
 if FSelectedMemo <> nil then
  actRedo.Enabled := FSelectedMemo.CanRedo;
end;

procedure TfrmMain.actReloadFrameExecute(Sender: TObject);
var
    lMemoFrame : TMemoFrame;
    lFileName : string;
begin
    lMemoFrame := Sender as TMemoFrame;
    if not Assigned(lMemoFrame) then
        Exit;
    lMemoFrame.FFileChanged := False;
    //need create //temp
    try
        if TFile.Exists(lMemoFrame.FPredFileName) then
        begin
          lFileName := lMemoFrame.FPredFileName;
          lMemoFrame.FPredFileName := string.Empty;
          LoadFrameFromFile(lFileName, lMemoFrame,
            lMemoFrame.TMSFMXMemo1.CurY, lMemoFrame.TMSFMXMemo1.ReadOnly);
          TryOpenFormUnit(lFileName);
        end;
    except
        lMemoFrame.FFileChanged := True;
    end
end;

procedure TfrmMain.actRemovePluginExecute(Sender: TObject);
begin
    PluginRemove;
end;

procedure TfrmMain.OpenFile(const AFileName: String);
begin
    FActiveProjectDir := TPath.GetDirectoryName(AFileName);
    UpdateActiveProjectDir;
    ShowCaption;

    FAddToWorkFiles := False;
    FilesTree.EnumDir(TPath.GetDirectoryName(AFileName));
    LoadFrameFromFile(AFileName, FSelectedFrame);
    TryOpenFormUnit(AFileName);
end;

procedure TfrmMain.actOnAfterSaveMemoExecute(Sender: TObject);
begin
    if Assigned(FMonitoring) then
        FMonitoring.PauseWatch(TMemoFrame(Sender).SaveFileName);
end;

procedure TfrmMain.actOpenFileExecute(Sender: TObject);
begin
  if Loading then
    exit;
  if not TabControl1.Visible then TabControl1.Visible := True;
  TabControl1.ActiveTab := TabItem1;

  if OpenDialog1.Execute then
  begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmMain.actOpenFolderExecute(Sender: TObject);
var
NewPath: String;
begin
  if Loading then
    exit;
  if not TabControl1.Visible then TabControl1.Visible := True;
  TabControl1.ActiveTab := TabItem1;

  if SelectDirectory('Open Folder', NewPath{$IFDEF MSWINDOWS}, FMX.Platform.Win.WindowHandleToPlatform(Handle).Wnd{$ENDIF}) then
  begin
    FActiveProjectDir := NewPath;
    UpdateActiveProjectDir;
    ShowCaption;

    FilesTree.EnumDir(NewPath);
    //LoadFrameFromFile(OpenDialog1.FileName,FSelectedFrame);
  end;
end;

procedure TfrmMain.actOpenZipFileExecute(Sender: TObject);
var
  LFrame: TMemoFrame;
  LTag : integer;
  LItem : TTreeViewItem;
  LTree : TTreeView;
  LExtractFileName, LZipFileName : string;
  LIsExtract : Boolean;
begin
  LTree := Sender as TTreeView;
  if not Assigned(LTree) then
    exit;
  LTag := LTree.Tag;
  LItem := LTree.Selected;
  LZipFileName := LTree.TagString;
  if  Assigned(LItem) and (not LItem.TagString.IsEmpty) and (not LZipFileName.IsEmpty) then
  begin
    LIsExtract := ExtractFile(LZipFileName, LItem.Tag, LExtractFileName);
    if not LIsExtract then
        exit;
    LFrame := SplitEditor(LTag);
    if not Assigned(LFrame) then
        exit;
    LoadFrameFromFile(LExtractFileName, LFrame, 0, True);
    ReArrangeMemoFrames;
  end;
end;

procedure TfrmMain.actPASExecute(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoPascalStyler1;
    lbStyleSetting.Text := 'Pascal';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.actPasteExecute(Sender: TObject);
begin
  MemoPasteEdit;
end;

procedure TfrmMain.actQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actSaveAllExecute(Sender: TObject);
begin
  SaveAll;
end;

procedure TfrmMain.actSaveAsExecute(Sender: TObject);
begin
  SaveDialog1.FileName := TPath.GetFileName(FSelectedFrame.FPredFileName);
  if SaveDialog1.Execute then
    MemoSaveFile(SaveDialog1.FileName);
end;

procedure TfrmMain.actSaveAsUpdate(Sender: TObject);
begin
 if FSelectedFrame <> nil then
  actSaveAs.Enabled := FSelectedFrame.FMemoChanged or FSelectedFrame.TMSFMXMemo1.Modified;
end;

procedure TfrmMain.actSaveFileExecute(Sender: TObject);
var
  LWorkItem: TWorkItem;
begin
  if FSelectedFrame <> nil then
  begin
    if FSelectedFrame.FPredFileName <> '' then
    try
      // Prevent to save newly created file (that aren't saved yet)
      if TFile.Exists(FSelectedFrame.FPredFileName) then
      begin
        //FSelectedMemo.Lines.SaveToFile(FSelectedFrame.FPredFileName);
        FSelectedFrame.SaveMemoLines;
        FSelectedMemo.ClearUndoRedo;
        FSelectedFrame.FMemoChanged := False;
        LWorkItem := WorkFilesTree.WorkItemByFileName(FSelectedFrame.FPredFileName);
        if LWorkItem <> nil then LWorkItem.Modified := False;
      end;
    except
      FSelectedFrame.FMemoChanged := True;
    end;
  end;
end;

procedure TfrmMain.actSaveFileUpdate(Sender: TObject);
begin
 if FSelectedFrame <> nil then
  actSaveFile.Enabled := FSelectedFrame.FMemoChanged or FSelectedFrame.TMSFMXMemo1.Modified;
end;

procedure TfrmMain.actSavePluginExecute(Sender: TObject);
begin
  PluginSave;
end;

procedure TfrmMain.actExportToHTMLExecute(Sender: TObject);
var
  SFilter: string;
begin
  SFilter := SaveDialog1.Filter;
  try
    SaveDialog1.Filter := 'HTML file (*.html)|*.html';
    SaveDialog1.FileName := TPath.GetFileName(FSelectedFrame.FPredFileName);
    if SaveDialog1.Execute then
      MemoExportFile(SaveDialog1.FileName, '.html');
  finally
    SaveDialog1.Filter := SFilter;
  end;
end;

procedure TfrmMain.actFindExecute(Sender: TObject);
begin
 // MemoSearchEdit;
  MemoFindText;
//   TabControl1.ActiveTab := TabItem2;
end;

procedure TfrmMain.actSearchRefreshExecute(Sender: TObject);
begin
  actSearchInFiles.Execute;
end;

procedure TfrmMain.actFindUpdate(Sender: TObject);
begin
  if FSelectedMemo<>nil then
    ActFind.Enabled := FSelectedMemo.Lines.Count > 0;
end;

procedure TfrmMain.actShowAboutExecute(Sender: TObject);
begin
  ShowAbout;
end;

procedure TfrmMain.actSplitEditorExecute(Sender: TObject);
var
  LFrame: TMemoFrame;
begin
  LFrame := SplitEditor(TAction(Sender).Target.Tag);
  if LFrame <> nil then
  begin
    LoadFrameFromFile(GetMemoFrameByTag(TAction(Sender).Target.Tag).FPredFileName, LFrame);
    ReArrangeMemoFrames;
  end;
end;

procedure TfrmMain.actSwitchToSearchExecute(Sender: TObject);
begin
  if not TabControl1.Visible then
   TabControl1.Visible := True;
  TabControl1.ActiveTab := TabItem2;
  edFindInFiles.SetFocus;
  Application.ProcessMessages;
  if SearchFilesTree.SearchRunning then
    Exit;
  self.edFindInFiles.Text := string.EMpty;
  if FActiveProjectDir = EmptyStr then
  begin
   if OpenDialog1.Execute then
      FActiveProjectDir := TPath.GetDirectoryName(OpenDialog1.FileName)
   else
    Exit;
  end;
end;

procedure TfrmMain.actOptionsExecute(Sender: TObject);
begin
//  frmSyntaxColoring := TfrmSyntaxColoring.Create(Application);
  try
    FrameOptions.chkMemoActiveLine.IsChecked := FMemoSettings.FShowActiveLine;
    FrameOptions.chkMemoActiveLineIndicator.IsChecked := FMemoSettings.FShowActiveIndicator;
    FrameOptions.chkAutoOpen.IsChecked := FMemoSettings.FAutoOpen;
    FrameOptions.chkMemoRightMargin.IsChecked := FMemoSettings.FMarginVisible;
    FrameOptions.spinMemoRightColumn.Value := FMemoSettings.FMarginColumn;
    FrameOptions.chkGutterVisible.IsChecked := FMemoSettings.FGutterVisible;
    FrameOptions.chkGutterLineNumbers.IsChecked := FMemoSettings.FGutterShowLineNumbers;
    FrameOptions.chkGutterZeroes.IsChecked := FMemoSettings.FGutterShowLeadingZeroes;
    FrameOptions.spinGutterWidth.Value := FMemoSettings.FGutterWidth;
    FrameOptions.chkMemoTabStop.IsChecked := FMemoSettings.FTabStop;
    FrameOptions.spinMemoTabSize.Value := FMemoSettings.FTabSize;
    FrameOptions.chkMemoSmartTabs.IsChecked := FMemoSettings.FSmartTabs;
    FrameOptions.chkMemoWantTab.IsChecked := FMemoSettings.FWantTab;
    FrameOptions.edSearchPath.Text := FRTLPath;
    //StyleBook := StyleBook;
    FrameOptions.TMSFMXMemo1.StyleLookup := FSelectedMemo.StyleLookup;
    FrameOptions.TMSFMXMemo1.FontColor := FSelectedMemo.FontColor;
    FrameOptions.TMSFMXMemo1.Gutter.FontColor := FSelectedmemo.Gutter.FontColor;

    FrameOptions.Visible := True;
    {$IFDEF MSWINDOWS}
    FrameOptions.Handle := Handle;
    {$ENDIF}
    FrameOptions.FormShow(FrameOptions);

  finally
  //  FreeAndNil(frmSyntaxColoring);
  end;
end;

procedure TfrmMain.SaveOptionsFrame(FOK: Boolean);
begin
      //----- Miscellaneous ----------------------------------------------
      FMemoSettings.FShowActiveLine := FrameOptions.chkMemoActiveLine.IsChecked;
      FMemoSettings.FShowActiveIndicator := FrameOptions.chkMemoActiveLineIndicator.IsChecked;
      FMemoSettings.FAutoOpen := FrameOptions.chkAutoOpen.IsChecked;
      //----- Gutter and Margin ------------------------------------------
      FMemoSettings.FMarginVisible := FrameOptions.chkMemoRightMargin.IsChecked;
      FMemoSettings.FMarginColumn := Trunc(FrameOptions.spinMemoRightColumn.Value);
      FMemoSettings.FGutterVisible := FrameOptions.chkGutterVisible.IsChecked;
      FMemoSettings.FGutterShowLineNumbers := FrameOptions.chkGutterLineNumbers.IsChecked;
      FMemoSettings.FGutterShowLeadingZeroes := FrameOptions.chkGutterZeroes.IsChecked;
      FMemoSettings.FGutterWidth := FrameOptions.spinGutterWidth.Value;
      //----- Tab Settings -----------------------------------------------
      FMemoSettings.FTabStop := FrameOptions.chkMemoTabStop.IsChecked;
      FMemoSettings.FTabSize := Trunc(FrameOptions.SpinMemoTabSize.Value);
      FMemoSettings.FWantTab := FrameOptions.chkMemoWantTab.IsChecked;
      FMemoSettings.FSmartTabs := FrameOptions.chkMemoSmartTabs.IsChecked;
      FRTLPath := FrameOptions.edSearchPath.Text;

      //------ Highlight Tab ---------------------------------------------------
      Self.duck.all.isa(TTMSFMXMemo).each(
        procedure(Obj: TObject)
        var
          I: integer;
          LComboColorBox: TComboColorBox;
        begin
          with TTMSFMXMemo(Obj), FrameOptions do
          begin
            //----- Syntax Coloring --------------------------------------------
            if SyntaxStyles <> nil then
            begin
              for i := 0 to FrameOptions.ComponentCount - 1 do
              begin
                LComboColorBox :=
                  TComboColorBox(FrameOptions.FindComponent('MyColorBox' + I.ToString));
                if LComboColorBox <> nil then
                  SyntaxStyles.AllStyles[i].FontColor := LComboColorBox.Color;
              end;
            end;
          end;
        end
      );

  if FOK then
   begin
    SetMemoSettings(MemoFrame1.TMSFMXMemo1);
    SetMemoSettings(MemoFrame2.TMSFMXMemo1);
    SetMemoSettings(MemoFrame3.TMSFMXMemo1);
    FrameOptions.Visible := False;
   end;
end;

procedure TfrmMain.actSyntaxSelectExecute(Sender: TObject);
var
  PT: TPointF;
begin
  PT := Screen.MousePos;
  SyntaxSelectPopup.Popup(PT.X, PT.Y);
end;

procedure TfrmMain.actTogglePluginBarExecute(Sender: TObject);
begin
  if not PluginsPanel.Visible then
  begin
    ShowPluginsPanel(True);

  end else
    ShowPluginsPanel(False);
end;

procedure TfrmMain.actToggleSideBarExecute(Sender: TObject);
begin
TabControl1.Visible := not TabControl1.Visible;
Splitter1.Visible := not Splitter1.Visible;
end;

procedure TfrmMain.actUndoExecute(Sender: TObject);
begin
  MemoUndoEdit;
end;

procedure TfrmMain.actUndoUpdate(Sender: TObject);
begin
  if FSelectedMemo <> nil then
   begin
    actUndo.Enabled := FSelectedMemo.CanUndo;
    if FSelectedFrame <> nil then
      if FSelectedMemo.CanUndo then FSelectedFrame.FMemoChanged := True;
   end;
end;

procedure TfrmMain.actZoomInExecute(Sender: TObject);
begin
  ZoomLayout.Scale.X := ZoomLayout.Scale.X+0.2;
  ZoomLayout.Scale.Y := ZoomLayout.Scale.Y+0.2;
  ZoomLayout.Align := TAlignLayout.None;
  Application.ProcessMessages;
  ZoomLayout.Align := TAlignLayout.Client;
end;

procedure TfrmMain.actZoomOutExecute(Sender: TObject);
begin
  ZoomLayout.Scale.X := ZoomLayout.Scale.X-0.2;
  ZoomLayout.Scale.Y := ZoomLayout.Scale.Y-0.2;
  ZoomLayout.Align := TAlignLayout.None;
  Application.ProcessMessages;
  ZoomLayout.Align := TAlignLayout.Client;
end;

procedure TfrmMain.AutosaveTimerTimer(Sender: TObject);
begin
  actSaveFileExecute(Self);
end;

procedure TfrmMain.btnCloseAboutClick(Sender: TObject);
begin
  AboutLogo.Fill.Bitmap.Bitmap.SetSize(0,0);
  rectAbout.Visible := False;
end;

procedure TfrmMain.btnFindInFilesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = System.UITypes.vkReturn then
    if actSearchInFiles.Enabled then actSearchInFilesExecute(Self);
end;

procedure TfrmMain.edFindInFilesKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = System.UITypes.vkReturn then
    actSearchInFilesExecute(Self);
    //if actFindText.Enabled then actFindTextExecute(Self);
end;

procedure TfrmMain.edFindTextChange(Sender: TObject);
begin
 if ((FSelectedFrame<>nil) AND (FSelectedMemo<>nil)) then
  if FSelectedFrame.edFindText.Text = EmptyStr then FSelectedMemo.HighlightText := EmptyStr;
end;

procedure TfrmMain.edFindTextKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = System.UITypes.vkReturn then
    if actFindText.Enabled then actFindTextExecute(Self);
end;

procedure TfrmMain.EmptyButtonsText;
begin
  Self.duck.all.isa(TSpeedButton).setTo('Text', EmptyStr);
  Self.duck.all.isa(TFrame).each(
    procedure(obj: TObject)
    begin
      TFrame(obj).duck.all.isa(TSpeedButton).setTo('Text', EmptyStr);
    end
  );
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  dlgResult: integer;
  chkResult: boolean;
begin
  SaveSettings;

  chkResult := True;
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      if not MemoCheckModified(TMemoFrame(obj)) then chkResult := False;
    end
   );

  // if we push <Cancel> - we must continue working
  if not chkResult then
  begin
    CanClose := False;
    Exit;
  end;

  if WorkFilesTree.ModifiedCount > 0 then
  begin
    DlgResult := FMX.Dialogs.MessageDlg(CWorkFilesChanged,
      TMsgDlgType.mtWarning, [System.UITypes.TMsgDlgBtn.mbYes,
      System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel], 0);
    case DlgResult of
      // Cancel - continue working with modified text
      mrCancel: CanClose := False;
      // Save changes
      mrYes: CanClose := False;
      // No save. Lose changes.
      mrNo: CanClose := True;
    end;
  end;

end;

procedure TfrmMain.SelectFrame(ATMSFMXMemo: TTMSFMXMemo);
begin
  FSelectedMemo := ATMSFMXMemo;
end;

procedure TfrmMain.SetGutterVisible(AMemo: TTMSFMXMemo; AValue: boolean);
var
  O: TFMXObject;
begin
  O := AMemo.FindStyleResource('gutter');
  if (O <> nil) and (O is TRectangle) then
    TRectangle(O).Visible := AValue;
end;

procedure TfrmMain.SetGutterWidth(AMemo: TTMSFMXMemo; AValue: single);
var
  O: TFMXObject;
begin
  if (AValue >= MinGutterWidth) and (AValue <= MaxGutterWidth) then
  begin
    O := AMemo.FindStyleResource('gutter');
    if (O <> nil) and (O is TRectangle) then
      TRectangle(O).Width := AValue;
  end;
end;

procedure TfrmMain.SetMemoFocus(AMemoFrame: TMemoFrame);
begin
  CheckFileChanged(AMemoFrame);
  if FSelectedFrame = AMemoFrame then
   Exit;
  FSelectedFrame := AMemoFrame;
  FSelectedMemo := FSelectedFrame.TMSFMXMemo1;
  lbEncoding.Text := AMemoFrame.FEncoding;
  lbCRLF.Text := AMemoFrame.FCRLF;
  if FSelectedMemo.SyntaxStyles <> nil then
    begin
     lbStyleSetting.Text := FSelectedMemo.SyntaxStyles.Description;
     if lbStyleSetting.Text='Pascal' then
      begin
        Info.PathList.Text := FRTLPath + #13#10 + FSelectedFrame.lbFilePath.Text;
      end;
    end
  else
    lbStyleSetting.Text := CDefNone;
end;

procedure TfrmMain.SetMemoSettings(AMemo: TTMSFMXMemo);
begin
  AMemo.ActiveLineSettings.ShowActiveLine := FMemoSettings.FShowActiveLine;
  AMemo.ActiveLineSettings.ShowActiveLineIndicator := FMemoSettings.FShowActiveIndicator;
  AMemo.ShowRightMargin := FMemoSettings.FMarginVisible;
  AMemo.RightMargin := FMemoSettings.FMarginColumn;
  SetGutterVisible(AMemo, FMEmoSettings.FGutterVisible);
  AMemo.Gutter.ShowLineNumbers := FMemoSettings.FGutterShowLineNumbers;
  AMemo.Gutter.ShowLeadingZeros := FMemoSettings.FGutterShowLeadingZeroes;
  SetGutterWidth(AMemo, FMEmoSettings.FGutterWidth);
  AMemo.TabStop := FMemoSettings.FTabStop;
  AMemo.TabSize := FMemoSettings.FTabSize;
  AMemo.SmartTabs := FMemoSettings.FSmartTabs;
  AMemo.WantTab := FMEmoSettings.FWantTab;
end;

procedure TfrmMain.SetTheme(AThemeIndex: integer);
begin
  case AThemeIndex of
    1: begin
        actLightThemeExecute(Self);
        //UpdateImages(ImageListGray);
    end;
    2: begin
        actDarkThemeExecute(Self);
        //UpdateImages(ImageListWhite);
    end;
  end;
end;

procedure TfrmMain.OnCreateEvent(Sender: TObject);
var
  SDefFileName: string;
begin
  //ReportMemoryLeaksOnShutdown := True;

 //{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
 //{$ELSE}
  if not TDirectory.Exists(System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim) then
   TDirectory.CreateDirectory(System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName);
 //{$ENDIF}


  SetMemoFocus(MemoFrame2);
  FNewFileCount := 0;

  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      TMemoFrame(obj).pnlGoTo.Visible := False;

      TMemoFrame(obj).TMSFMXMemo1.OnKeyDown := MemoKeyDown;
      TMemoFrame(obj).TMSFMXMemo1.OnEnter := CursorChange;
      TMemoFrame(obj).TMSFMXMemo1.OnCursorChange := CursorChange;
      TMemoFrame(obj).TMSFMXMemo1.OnGetAutoCompletionList := GetAutoCompletionList;
      TMemoFrame(obj).TMSFMXMemo1.OnInsertAutoCompletionEntry := InsertAutoCompletionEntry;
      TMemoFrame(obj).TMSFMXMemo1.OnAutoCompletionCustomizeList := AutoCompletionCustomizeList;
      TMemoFrame(obj).TMSFMXMemo1.OnAutoCompletionCustomizeItem := AutoCompletionCustomizeItem;
      TMemoFrame(obj).TMSFMXMemo1.OnBeforeAutoCompletion := BeforeAutoCompletion;
      TMemoFrame(obj).OnAfterSave := self.actOnAfterSaveMemoExecute;
    end
  );

  FilesTree := TProtoFilesTree.Create(TreeView1);
  FilesTree.OnBeginEnumDir := self.ProgressAnimenteBegin;
  FilesTree.OnEndEnumDir := Self.HexVWorkEnd;

  WorkFilesTree := TWorkFilesTree.Create(TreeView1);
  SearchFilesTree := TProtoFilesTree.Create(TreeView2);
  FSelectedFrame.lbFileName.Text := CDefFileName + FNewFileCount.ToString;
  HexViewer := THexViewer.Create('');
  HexViewer.OnComposeBegin := HexVWorkBegin;
  HexViewer.OnComposeEnd := HexVWorkEnd;
  HexViewer.OnComposing := HexVProgress;

  FileHistory := TStringList.Create;
  FileHistory.Capacity := MaxHistoryFiles;
  FileHistory.CaseSensitive := False;
  FCurrHistPos := 0;

  // Prevent draw Action's text on the button
  EmptyButtonsText;
  lbStyleSetting.Text := CDefNone;

 //{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
 // SDefFileName := System.SysUtils.IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + CDefFileName + FNewFileCount.ToString;
 //{$ELSE}
  SDefFileName := System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim + CDefFileName + FNewFileCount.ToString;
 //{$ENDIF}
  //{$IF (DEFINED(MACOS) AND NOT DEFINED(IOS))}
  //FActiveProjectDir := TPath.GetDirectoryName(ParamStr(0));
  //{$ELSE}
  FActiveProjectDir := System.SysUtils.IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + CAppName + PathDelim;
  //{$ENDIF}
  UpdateActiveProjectDir;

  //SetMemoFocus(MemoFrame2);
  if FSelectedMemo <> nil then
   FSelectedMemo.SetCursor(0, 0);
  NewFile(FSelectedFrame);

  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      {
      obj.duck.all.isa(TTMSFMXMemo).each(
        procedure(obj: TObject)
        begin
          TTMSFMXMemo(obj).ActiveLineSettings.ShowActiveLine := True;
          TTMSFMXMemo(obj).ActiveLineSettings.ActiveLineAtCursor := True;
        end
        );
      }
      AddHints(obj);
    end
   );

  //FPredFileName := EmptyStr;
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      obj.duck.all.isa(TImageViewer).has('Visible').setTo(False);
    end
   );

  ProgressBar1.Visible := False;
  FAutoSave := False;
  //AutocompleteInit;

  AddHints(Self);
  LoadSettings;
  rectAbout.Visible := False;
  pnlSearchOption.Visible := False;
  FPlugins := TPlugins.Create(uPluginUtils.GetPath + CFilePlugins);
  //FPlugins.FileName := uPluginUtils.GetPath + CFilePlugins;
  FPlugins.OnNotify := self.PluginChange;
  FPlugins.OnChangeCurrent :=
    procedure (AIndex : Integer)
    begin
      lvPlugins.ItemIndex := AIndex;
      FrameOptions.lvPlugins.ItemIndex := AIndex;
      FrameOptions.lvPluginsChange(FrameOptions.lvPlugins);
    end;
  PluginLoadListView;
  InitMonitoring;
  // --------------- auto completion code -------------------------------
  FLock := False;
  OpenParam := False;
  FBktPos.X := -1;
  FBktPos.Y := -1;
  FToken := EmptyStr;
  FCodeList := TStringList.Create;
  Info := TCodeCompleteInfo.Create;
  TMSFMXMemoPascalStyler1.AutoCompletion.AddStrings(TConst.PASCAL_CONST);

  Info.PathList.Text := FRTLPath;
 { info.ThreadLoad.OnExecuteStep :=
      procedure (AValue : string; ASuccess : Boolean)
      begin
          TThread.Synchronize(nil,
          procedure
          begin
             //mmo3.Lines.Add(Format('Unit: %s, Success: %s', [AValue, string.Parse(ASuccess)]));
          end
          );
      end;
  //--------------------------------------------------------------------
     }
  FrameOptions.FormCreate(FrameOptions);

  if ParamCount>0 then
   begin
    OpenFile(ParamStr(1));
   end;

end;

procedure TfrmMain.AddHints(Sender: TObject);
begin
  {$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
  Sender.duck.all.isa(TSpeedButton).each(
    procedure(obj: TObject)
    begin
      SetAHint(Obj,TSpeedButton(Obj).Text);
    end
  );
  Sender.duck.all.isa(TButton).each(
    procedure(obj: TObject)
    begin
      SetAHint(Obj,TSpeedButton(Obj).Text);
    end
  );
  {$ENDIF}
end;

procedure TfrmMain.AddToHistoryList(const AFileName: string);
var
  N: integer;
begin
  if FileHistory.Count < MaxHistoryFiles then
  begin
    N := FileHistory.IndexOf(AFileName);
    if N = -1 then
    begin
      FileHistory.Append(AFileName);
      FCurrHistPos := Pred(FileHistory.Count);
    end;
  end;
end;

procedure TfrmMain.actChangeCCPlatformExecute(Sender: TObject);
var
  PT: TPointF;
begin
  if SameText(lbStyleSetting.Text, 'Pascal') then
   begin
    PT := Screen.MousePos;
    popupCCPlatform.Popup(PT.X, PT.Y);
   end;
end;

procedure TfrmMain.OnCreateTimerTimer(Sender: TObject);
begin
  OnCreateEvent(Sender);

  {$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
   // In OnFormShow
  {$ELSE}
   UpdateImages(ImageListWhite);
  {$ENDIF}

  OnCreateTimer.Enabled := False;
end;

procedure TfrmMain.PaxCompiler1UndefineDirective(Sender: TPaxInterpreter;
  const Directive: string; var ok: Boolean);
begin
  ok := True;
//  FCurrModuleName := Sender.CurrModuleName;
end;

procedure TfrmMain.PluginAdd;
var
  fPluginEdit: TfPluginEdit;
begin
  fPluginEdit := TfPluginEdit.Create(self);
  try
    if fPluginEdit.ShowModal = mrOk then
    begin
      FPlugins.Add(fPluginEdit.FPlugin);
      //PluginLoadListView;
    end;
  finally
    FreeAndNil(fPluginEdit);
  end;
end;

procedure TfrmMain.PluginEdit;
var
  fPluginEdit : TfPluginEdit;
begin
  fPluginEdit := TfPluginEdit.Create(self);
  try
    fPluginEdit.LoadPLugin(FPlugins.Current);
    if fPluginEdit.ShowModal = mrOk then
    begin
      PluginLoadListView;
    end;
  finally
    FreeAndNil(fPluginEdit);
  end;
end;

procedure TfrmMain.PluginLoad;
begin
  if dlgOpenPlugin.Execute then
    FPlugins.FileName := dlgOpenPlugin.Filename;
end;

procedure TfrmMain.PluginChange(Sender: TObject; const Item: TPlugin;
        Action: TCollectionNotification);
begin
    PluginLoadListView;
end;

procedure TfrmMain.PluginLoadListView;
begin
  PluginLoadListView(lvPlugins);
  PluginLoadListView(FrameOptions.lvPlugins);
end;

procedure TfrmMain.PluginLoadListView(const ALv : TListView);
var
  Plugin: TPlugin;
  Item: TListViewItem;
begin
  ALv.BeginUpdate;
  try
    ALv.Items.Clear;
    for Plugin in FPlugins do
    begin
      Item := ALv.Items.Add;
      Item.Text := Plugin.FName;
      Item.Bitmap := Plugin.FIcon;
    end;
  finally
    ALv.EndUpdate;
  end;
end;

procedure TfrmMain.PluginOpen;
var
  plugin : TPlugin;
  P: Pointer;
begin
  if FSelectedMemo <> nil then
    RunMemoUppercase(FSelectedMemo);
  {
  plugin := FPlugins.Current;
  if not Assigned(plugin) then
      exit;

  case plugin.FType of
    0: OpenViewUrl.OpenURL(plugin.FPath);
    1: begin
        PaxInterpreter1.Reset;
        PaxInterpreter1.RegisterLanguage(PaxPascalLanguage1);

        PaxInterpreter1.AddModule('1', 'Pascal');
        PaxInterpreter1.AddCodeFromFile('1', plugin.FPath);
        if PaxInterpreter1.Compile(PaxProgram1) then
        begin
          P := PaxProgram1.GetAddress('frmMain');
          TfrmMain(P^) := Self; // change script-defind variable

          PaxProgram1.Run;
        end else
          ShowMessage(PaxInterpreter1.ErrorMessage[0]);
       end;
    end;
  }
end;

procedure TfrmMain.PluginRemove;
begin
  FPlugins.RemoveCurrent;
  //PluginLoadListView;
end;

procedure TfrmMain.PluginSave;
begin
  FPlugins.SaveToFile;
end;

procedure TfrmMain.pmPluginsPopup(Sender: TObject);
begin
  //menuEditPlugin.Visible := lvPlugins.ItemIndex <> -1;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
   OnCreateEvent(Sender);
  {$ELSE}
   OnCreateTimer.Enabled := True;
  {$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ParseTimer.Enabled := False;

  HexViewer.Free;
  FileHistory.Free;
  SearchFilesTree.Free;
  FilesTree.Free;
  WorkFilesTree.Free;
  //AutocompleteDrop;
  FPlugins.OnNotify := nil;
  FPlugins.SaveToFile;
  FreeAndNil(FPlugins);
  FreeAndNil(FMonitoring);
  try
    Info.SaveUnits;
    FreeAndNil(FCodeList);
    FreeAndNil(Info);
  except
    on e : Exception do
     begin

     end;
  end;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  {$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
   SetTheme(FThemeIndex);
  {$ELSE}
   // In OnCreateTimer
  {$ENDIF}
  ShowCaption;
end;

procedure TfrmMain.FrameOptionsButton1Click(Sender: TObject);
begin
    SaveOptionsFrame(True);
end;

procedure TfrmMain.FrameOptionsButton2Click(Sender: TObject);
begin
    CloseOptionsFrame;
end;

procedure TfrmMain.CloseOptionsFrame;
begin
FrameOptions.Visible := False;
end;

function TfrmMain.GetPasUnitFile(AFormFileName: string): string;
var
  AFile: string;
  SUnit, SForm: string;
  SExt: string;
begin
  Result := '';
  SExt := TPath.GetExtension(AFormFileName);
  if ContainsText(CPascalFormExt, SExt) then
   begin
    SUnit := TPath.GetFileNameWithoutExtension(AFormFileName);
    AFile := ExtractFilePath(AFormFileName) + SUnit + CPascalUnitExt;
    if TFile.Exists(AFile) then
     begin
      Result := AFile;
     end;
   end;
end;

function TfrmMain.GetFormUnitFile(AUnitFileName: string): string;
var
  LFilesList: TStringDynArray;
  s: string;
  SUnit, SForm: string;
  SExt: string;
begin
  Result := EmptyStr;
  if not SameText(TPath.GetExtension(AUnitFileName), CPascalUnitExt) then
    begin
      Result := GetPasUnitFile(AUnitFileName);
      Exit;
    end;

  try
    SUnit := TPath.GetFileNameWithoutExtension(AUnitFileName);
    LFilesList := TDirectory.GetFiles(ExtractFilePath(AUnitFileName));
    for s in LFilesList do
    begin
      SForm := TPath.GetFileNameWithoutExtension(s);
      SExt := TPath.GetExtension(s);
      if CompareText(SUnit, SForm) = 0 then
      begin
        if ContainsText(CPascalFormExt, SExt) then
        begin
          Result := s;
          Break;
        end;
      end;
    end;
  except
    On E: Exception do
    begin
      Result := EmptyStr;
      ShowMessage(E.Message);
    end;
  end;
end;

function TfrmMain.GetGutterVisible(AMemo: TTMSFMXMemo): boolean;
var
  Obj: TFMXObject;
begin
  Obj := AMemo.FindStyleResource('gutter');
  if (Obj <> nil) and (Obj is TRectangle) then
    Result := TRectangle(Obj).Visible
  else
    Result := False;
end;

function TfrmMain.GetGutterWidth(AMemo: TTMSFMXMemo): single;
var
  Obj: TFMXObject;
begin
  Obj := AMemo.FindStyleResource('gutter');
  if (Obj <> nil) and (Obj is TRectangle) then
    Result := TRectangle(Obj).Width
  else
    Result := DefGutterWidth;
end;

function TfrmMain.GetLoading: Boolean;
begin
    Result := self.ProgressBar1.Visible;
end;

function TfrmMain.GetMemoFrameByTag(ATag: integer): TMemoFrame;
begin
  case ATag of
    1: Result := MemoFrame1;
    2: Result := MemoFrame2;
    3: Result := MemoFrame3;
    else
      Result := nil;
  end;
end;

function TfrmMain.GetSyntaxStyler(
  const AFileExt: string): TTMSFMXMemoCustomStyler;
var
  s: string;
  TempSyntaxStyler: TTMSFMXMemoCustomStyler;
begin
  TempSyntaxStyler := nil;
  s := Copy(AFileExt, 2, AFileExt.Length - 1) + ';';

  Self.duck.all.isa(TTMSFMXMemoCustomStyler).each(
  procedure(obj: TObject)
  begin
    if ContainsText(TTMSFMXMemoCustomStyler(Obj).Extensions, s) then
    begin
      TempSyntaxStyler := TTMSFMXMemoCustomStyler(Obj);
      Exit;
    end;
  end
  );

  Result := TempSyntaxStyler;
end;

function TfrmMain.GetSyntaxStylerByName(
  const AStylerName: string): TTMSFMXMemoCustomStyler;
var
  TempSyntaxStyler: TTMSFMXMemoCustomStyler;
begin
  TempSyntaxStyler := nil;
  Self.duck.all.isa(TTMSFMXMemoCustomStyler).each(
  procedure(obj: TObject)
  begin
    if SameText(TTMSFMXMemoCustomStyler(Obj).StylerName, AStylerName) then
    begin
      TempSyntaxStyler := TTMSFMXMemoCustomStyler(Obj);
      Exit;
    end;
  end
  );

  Result := TempSyntaxStyler;
end;

procedure TfrmMain.GoToCode(AMethodName: string);
var
  FDO: TTMSFMXFindDialogOptions;
begin
  try
    if ((FSelectedFrame <> nil) and (FSelectedMemo <> nil)) then
    begin
      FDO := [fdoWrapAtEndOfFile, fdoDown];
      FSelectedMemo.FindText(AMethodName, FDO);
      FSelectedMemo.SetFocus;
    end;
  except
    On E: Exception do
      ShowMessage(E.ClassName + ' : ' + E.Message);
  end;
end;

procedure TfrmMain.GoToListCommand(AMemoFrame: TMemoFrame);
var
  SActionHint: string;
begin
  if AMemoFrame = nil then AMemoFrame := FSelectedFrame;
  if AMemoFrame <> nil then
  begin
    if AMemoFrame.ListView1.Selected <> nil then
    begin
      SActionHint := AMemoFrame.ListView1.Items[AMemoFrame.ListView1.Selected.Index].Text;
      Self.duck.all.isa(Taction).each(
        procedure(obj: TObject)
        begin
          if SameText(TAction(obj).Hint, SActionHint) then
          begin
            TAction(obj).Execute;
            GoToListHide(AMemoFrame);
          end;
        end
        );
    end;
  end;
end;

procedure TfrmMain.GoToListUpDown(AKey : Word; AMemoFrame: TMemoFrame = nil);
var
    lItemIndex : integer;
begin
  if AMemoFrame = nil then
    AMemoFrame := FselectedFrame;
  if Assigned(AMemoFrame) then
  begin
    lItemIndex := AMemoFrame.ListView1.ItemIndex;
    case AKey of
        vkDown :
            if lItemIndex + 1 < AMemoFrame.ListView1.ItemCount then
                lItemIndex := lItemIndex + 1;
        vkUp :
            if lItemIndex > 0 then
                lItemIndex := lItemIndex - 1;
    end;
    AMemoFrame.ListView1.ItemIndex := lItemIndex;
  end;
end;

procedure TfrmMain.GoToListExec(Sender: TObject; AMemoFrame: TMemoFrame = nil);
var
  LineNumber: integer;
  LItem: TListViewItem;
  S: string;
begin
  if AMemoFrame = nil then AMemoFrame := FSelectedFrame;
  if AMemoFrame <> nil then
  begin
    // Tag property sets by GoToListShow method
    case AMemoFrame.edGoToFilter.Tag of
      // GoTo FileName
      1: begin
           if Sender is TEdit then
           begin
            if Trim(AMemoFrame.edGoToFilter.Text) = EmptyStr then
              raise Exception.Create('Empty file name!');

            for LItem in AMemoFrame.ListView1.Items do
              if SameText(AMemoFrame.edGoToFilter.Text, LItem.Text) then
              begin
                GoToListFileName(Litem.Detail + AMemoFrame.edGoToFilter.Text, AMemoFrame);
                Break;
              end;
           end else if Sender is TListView then
            begin
              if AMemoFrame.ListView1.ItemCount = 0 then
                Exit;
              //Why not LItem := AMemoFrame.ListView1.Selected; ?
              LItem := AMemoFrame.ListView1.Items[AMemoFrame.ListView1.Selected.Index];
              if LItem <> nil then
                GoToListFileName(LItem.Detail + LItem.Text, AMemoFrame);
            end;
          end;
      // GoTo LineNumber
      2: begin
          if not TryStrToInt(AMemoFrame.edGoToFilter.Text, LineNumber) then
            ShowMessage('Invalid line number: ' + AMemoFrame.edGoToFilter.Text)
          else
          begin
            GoToListHide(AMemoFrame);
            GoToListLineNumber(LineNumber, AMemoFrame);
          end;
      end;
      // GoTo Command
      3: begin
          GoToListCommand(AMemoFrame);
      end;
      // GoTo Code
      4: begin
          if Sender is TEdit then
          begin
            S := Trim(AMemoFrame.edGoToFilter.Text);
          end
          else
          if Sender is TListView then
          begin
            LItem := AMemoFrame.ListView1.Items[AMemoFrame.ListView1.Selected.Index];
            if LItem <> nil then
              S := LItem.Text;
          end;
          GoToCode(S);
          GoToListHide(AMemoFrame);
      end;
    end;
  end;
end;

procedure TfrmMain.GoToListFileName(AFileName: string; AMemoFrame: TMemoFrame = nil);
begin
  if AMemoFrame = nil then AMemoFrame := FSelectedFrame;
  if AMemoFrame <> nil then
  begin
    GoToListHide(AMemoFrame);
    if TFile.Exists(AFileName) then LoadFrameFromFile(AFileName, AMemoFrame)
      else ShowMessage('File doesn''t exists: ' + AFilename);
  end;
end;

procedure TfrmMain.GoToListHide(AMemoFrame: TMemoFrame = nil);
begin
  if AMemoFrame = nil then AMemoFrame := FselectedFrame;
  if AMemoFrame <> nil then
  begin
    AMemoFrame.ListView1.Items.Clear;
    AMemoFrame.edGoToFilter.Text := EmptyStr;
    AMemoFrame.pnlGoTo.Visible := False;
  end;
end;

procedure TfrmMain.GoToListLineNumber(ALineNumber: integer; AMemoFrame: TMemoFrame = nil);
begin
  GoToListHide(AMemoFrame);
  if AMemoFrame = nil then AMemoFrame := FSelectedframe;
  if AMemoFrame <> nil then
  begin
    if AlineNumber > 0 then
    begin
      if AMemoFrame.TMSFMXMemo1.Lines.Count >= Pred(ALineNumber) then
      begin
        AMemoFrame.TMSFMXMemo1.SetCursor(1, Pred(ALineNumber));
        AMemoFrame.TMSFMXMemo1.SetFocus;
      end;
    end;
  end;
end;

procedure TfrmMain.GoToListShow(GotoKind: TGotoKind; AMemoFrame: TMemoFrame = nil);
var
  SFile: string;
  SArray: TstringDynArray;
  LItem: TListViewItem;
begin
  if AMemoFrame = nil then AMemoFrame := FSelectedFrame;
  if AMemoFrame <> nil then
  begin
    case GotoKind of
      gtFile: begin
                AMemoFrame.edGoToFilter.Tag := 1;
                AMemoFrame.lbGoToHint.Text := 'Type a file name.';
                AMemoFrame.ListView1.BeginUpdate;
                try
                  AMemoFrame.ListView1.Items.Clear;
                  AMemoFrame.ListView1.ItemAppearance.ItemHeight := 44;
                  AMemoFrame.ListView1.ItemEditAppearanceName := 'ImageListItemBottomDetail';
                  SArray := FilesTree.GetFilesList;
                  if Length(SArray) > 0 then
                  begin
                    for SFile in SArray do
                    begin
                      LItem := AMemoFrame.ListView1.Items.Add;
                      LItem.Text := TPath.GetFileName(SFile);
                      LItem.Detail := ExtractFilePath(SFile);
                    end;
                  end;
                finally
                  AMemoFrame.ListView1.EndUpdate;
                end;
              end;
      gtLine: begin
                AMemoFrame.edGoToFilter.Tag := 2;
                AMemoFrame.ListView1.Items.Clear;
                AMemoFrame.ListView1.ItemAppearance.ItemHeight := 44;
                AMemoFrame.ListView1.ItemEditAppearanceName := 'ImageListItemBottomDetail';
                if AMemoFrame.TMSFMXMemo1 <> nil then
                  AMemoFrame.lbGoToHint.Text := 'Type a line number between 1 and ' +
                    AMemoFrame.TMSFMXMemo1.Lines.Count.ToString + ' to navigate to.'
                else
                  AMemoFrame.lbGoToHint.Text := 'Type a line number.';
              end;
      gtCommand: begin
                  AMemoFrame.edGoToFilter.Tag := 3;
                  AMemoFrame.ListView1.BeginUpdate;
                  AMemoFrame.lbGoToHint.Text := 'Select a command.';
                  try
                    AMemoFrame.ListView1.Items.Clear;
                    AMemoFrame.ListView1.ItemAppearance.ItemHeight := 32;
                    AMemoFrame.ListView1.ItemEditAppearanceName := 'ListItem';
                    Self.duck.all.isa(TAction).each(
                      procedure(obj: TObject)
                      begin
                        LItem := AMemoFrame.ListView1.Items.Add;
                        LItem.Text := TAction(obj).Hint;
                      end
                    );
                  finally
                    AMemoFrame.ListView1.EndUpdate;
                  end;
                 end;
      gtCode: begin
                AMemoFrame.edGoToFilter.Tag := 4;
                AMemoFrame.lbGoToHint.Text := 'Goto Code';
                AMemoFrame.ListView1.BeginUpdate;
                try
                  AMemoFrame.ListView1.Items.Clear;
                  AMemoFrame.ListView1.ItemAppearance.ItemHeight := 32;
                  AMemoFrame.ListView1.ItemEditAppearanceName := 'ListItem';
                  //------------------------------------------------------------
                  SArray := GetMethodsList(AMemoFrame.FPredFileName);
                  if SArray <> nil then
                  begin
                    for SFile in SArray do
                    begin
                      LItem := AMemoFrame.ListView1.Items.Add;
                      LItem.Text := SFile;
                    end;
                  end;
                  //------------------------------------------------------------
                finally
                  AMemoFrame.ListView1.EndUpdate;
                end;
              end;
    end;
    AMemoFrame.pnlGoto.Visible := True;
    AMemoFrame.edGoToFilter.SetFocus;
  end;
end;

procedure TfrmMain.ProgressWorkBegin(Sender: TObject; AText : string;
    Animated : Boolean);
begin
  ProgressBar1.Value := 0;
  fltnmtnPB.Enabled := Animated;
  ProgressBar1.Visible := True;
  txtHexViewProgress.Text := AText;
  Application.ProcessMessages;
end;

procedure TfrmMain.ProgressAnimenteBegin(Sender: TObject);
begin
    ProgressBar1.Value := 0;
    ProgressWorkBegin(Sender, 'Prepearing files data...', True);
end;

procedure TfrmMain.HexVProgress(Sender: TObject; BytesCount, Percent: integer);
begin
  ProgressBar1.Value := Percent;
  ProgressBar1.Repaint;
  txtHexViewProgress.Text := Percent.ToString;
  txtHexViewProgress.Repaint;
  Application.ProcessMessages;
end;

procedure TfrmMain.HexVWorkBegin(Sender: TObject);
begin
    ProgressWorkBegin(Sender, 'Prepearing hex data...');
end;

procedure TfrmMain.HexVWorkEnd(Sender: TObject);
begin
  fltnmtnPB.Enabled := False;
  Application.ProcessMessages;
end;

function TfrmMain.MemoCheckModified(AMemoFrame: TMemoFrame): Boolean;
var
  DlgResult: Integer;
  LItem: TWorkItem;
begin
  Result := True;

  if ((AMemoFrame <> nil) AND (AMemoFrame.TMSFMXMemo1 <> nil)) then
  begin
    if (not AMemoFrame.FMemoChanged) or (not AMemoFrame.TMSFMXMemo1.Visible) then
        Exit;

    //if AMemoFrame.TMSFMXMemo1.Modified then
    if AMemoFrame.FMemoChanged then
    begin
      {$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
        DlgResult := FMX.Dialogs.MessageDlg(CSaveDialogText,
          TMsgDlgType.mtWarning, [System.UITypes.TMsgDlgBtn.mbYes,
          System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel], 0);
        case DlgResult of
          // Cancel - continue working with modified text
          mrCancel: Result := False;
          // Save changes
          mrYes: actSaveAsExecute(Self);
          // No save. Lose changes.
          mrNo: begin
            AMemoFrame.FMemoChanged := False;
            LItem := WorkFilesTree.WorkItemByFileName(AMemoFrame.FPredFileName);
            if LItem <> nil then LItem.Modified := False;
           end;
        end;
      {$ELSE}
      FMX.Dialogs.MessageDlg(CSaveDialogText,
      TMsgDlgType.mtWarning, [System.UITypes.TMsgDlgBtn.mbYes,
        System.UITypes.TMsgDlgBtn.mbNo, System.UITypes.TMsgDlgBtn.mbCancel], 0,
        procedure(const AResult: TModalResult)
        begin
          case AResult of
            // Cancel - continue working with modified text
            mrCancel: begin end;
            // Save changes
            mrYes: actSaveFileExecute(Self);
            // No save. Lose changes.
            mrNo: begin
              AMemoFrame.FMemoChanged := False;
              LItem := WorkFilesTree.WorkItemByFileName(AMemoFrame.FPredFileName);
              if LItem <> nil then LItem.Modified := False;
           end;
          end;
        end);
      {$ENDIF}
    end;
   end;
end;

procedure TfrmMain.MemoCopyEdit;
begin
  try
    FSelectedMemo.CopyToClipboard;
  except
    On E: Exception do
      ShowMessage('Error: Copy edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoCutEdit;
begin
  try
    FSelectedMemo.CutToClipboard;
  except
    On E: Exception do
      ShowMessage('Error: Cut edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoExportFile(const AFileName, AExtention: string);
var
  SavedFileName: string;
  LWorkItem: TWorkItem;
begin
  SavedFileName := AFileName;
  if not TPath.HasExtension(SavedFileName) then
    SavedFileName := SavedFileName + AExtention;
  if ((FSelectedFrame <> nil) and (FSelectedMemo <> nil)) then
  try
    FSelectedMemo.SaveToHTML(SavedFileName);
    LWorkItem := WorkFilesTree.WorkItemByFileName(FSelectedFrame.FPredFileName);
    if LWorkItem <> nil then
      LWorkItem.Modified := False;
    FSelectedFrame.TMSFMXMemo1.ClearUndoRedo;
    FSelectedFrame.FMemoChanged := False;
    WorkFilesTree.RenameWorkItem(FSelectedFrame.FPredFileName, SavedFileName,
      FSelectedFrame.Tag);
    FSelectedFrame.lbFileName.Text := TPath.GetFileName(SavedFileName);
    FSelectedFrame.lbFilePath.Text := ExtractFilePath(SavedFileName);
  except
    On E: Exception do
    begin
      if FSelectedFrame <> nil then
        FSelectedFrame.FMemoChanged := True;
      ShowMessage('Error: Save file: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.MemoFindText;
var
  FDO: TTMSFMXFindDialogOptions;
begin
 if ((FSelectedFrame<>nil) AND (FSelectedMemo<>nil)) then
  begin
    FSelectedFrame.pnlMemoFind.Visible := True;
    //FSelectedMemo.HighlightText := FSelectedFrame.edFindText.Text;
  //  FDO := [fdovWrapAtEndOfFile];
    FDO := [fdoWrapAtEndOfFile, fdoDown];
    FSelectedMemo.FindText(FSelectedFrame.edFindText.Text,FDO);
    FSelectedFrame.edFindText.SetFocus;
  end;
end;

procedure TfrmMain.MemoFrame2ImageViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  SetMemoFocus(TMemoFrame(TImageViewer(Sender).Parent));
  ShowFileName(FSelectedFrame.FPredFileName);
  ShowCaretPos;
end;

procedure TfrmMain.MemoFrame2treeZipViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  SetMemoFocus(TMemoFrame(TTreeView(Sender).Parent));
  ShowFileName(FSelectedFrame.FPredFileName);
  ShowCaretPos;
end;

function TfrmMain.MemoFrameVisibleCount: integer;
var
  Sum: integer;
begin
  Sum := 0;
  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      if TMemoFrame(obj).Visible then
        Sum := Sum + 1;
    end
  );
  Result := Sum;
end;

procedure TfrmMain.MemoLoadFile(const AFileName: string; const AMemoFrame: TMemoFrame;
  const ALineNumber: Integer = 0);
var
  LFileExt: string;
  LItem: TWorkItem;
  SLineFeed: string;
  n: integer;
  SS: TStringStream;
begin
  SetMemoFocus(AMemoFrame);
  FSelectedFrame.ImageViewer1.Visible := False;
  FSelectedFrame.ImageViewer1.Bitmap.FreeHandle;
  FSelectedFrame.treeZipView.Visible := False;
  FSelectedMemo.Visible := True;
  if SameText(FSelectedFrame.FPredFileName, AFileName) then
  begin
    if FAddToWorkFiles then
      WorkFilesTree.AddToWorkList(AFileName, ftScript, FSelectedFrame.Tag);
    FAddToWorkFiles := False;
    if (ALineNumber <> 0) then SelectLine(ALineNumber);
    Exit;
  end;

  n := AMemoFrame.TMSFMXMemo1.Lines.Count;
  if n = 1 then
  begin
    if AMemoFrame.TMSFMXMemo1.Lines.Text.Length < 3 then
      n := Trim(AMemoFrame.TMSFMXMemo1.Lines.Text).Length;
  end else
    if n > 0 then
      n := AMemoFrame.TMSFMXMemo1.Lines.Text.Length;

  if (not FSelectedFrame.FMemoChanged) and (n = 0) then
  begin
    if FSelectedFrame.FPredFileName.Contains(CDefFileName) then
      WorkFilesTree.DeleteFromWorkList(FSelectedFrame.FPredFileName, FSelectedFrame.Tag);
  end else
    WorkFilesTree.MoveToMemoStream(FSelectedFrame);

  try
    if ((AFileName.StartsWith(CDefFileName) = True) AND
      (ExtractFileExt(AFileName) = '') AND (TFile.Exists(AFilename) = True)) then
     begin
      // do nothing
     end
    else
     begin
      LItem := WorkFilesTree.WorkItemByFileName(AFileName);
      if (LItem <> nil) and (SameText(FSelectedFrame.FPredFileName, AFileName)) then
      begin
        LItem.Stream.Seek(0, TSeekOrigin.soBeginning);
        if LItem.Stream.Size = 0 then
        begin
          //FSelectedMemo.Lines.Clear
          FSelectedMemo.Lines.Text := string.Empty;
        end
        else
          FSelectedMemo.Lines.LoadFromStream(LItem.Stream);
        FSelectedMemo.SyntaxStyles := LItem.Styler;
        FSelectedMemo.Modified := LItem.Modified;
        FSelectedFrame.FMemoChanged := LItem.Modified;
      end else
      begin
        FSelectedMemo.Lines.LoadFromFile(AFilename);
        FSelectedMemo.ReadOnly := False;
        LFileExt := TPath.GetExtension(AFileName);
        FSelectedMemo.SyntaxStyles := GetSyntaxStyler(LFileExt);
        FSelectedFrame.FMemoChanged := False;
      end;
     end;

    ShowFileName(AFileName);

    if AMemoFrame.TMSFMXMemo1.Lines.Encoding<>nil then
     begin
      AMemoFrame.FEncoding := AMemoFrame.TMSFMXMemo1.Lines.Encoding.EncodingName;
      AMemoFrame.FCRLF := GetFileLineEndStr(AFileName);
     end
    else
     begin
      AMemoFrame.FEncoding := GetFileEncodingStr(AFileName, SLineFeed);
      AMemoFrame.FCRLF := SLineFeed;
     end;

    lbEncoding.Text := AMemoFrame.FEncoding;
    lbCRLF.Text := AMemoFrame.FCRLF;

    if ALineNumber = 0 then
     begin
      FSelectedMemo.SetCursor(0, 0);
      // This line raises an AV
      //FSelectedMemo.GetVScroll.Value := 0;
     end
    else
      SelectLine(ALineNumber);

    FSelectedFrame.pnlMemoFind.Visible := False;
    if FSelectedMemo.SyntaxStyles <> nil then
      lbStyleSetting.Text := FSelectedMemo.SyntaxStyles.Description
    else
      lbStyleSetting.Text := CDefNone;

    if FAddToWorkFiles then
      WorkFilesTree.AddToWorkList(AFileName, ftScript, FSelectedFrame.Tag);
    FAddToWorkFiles := False;
    FSelectedFrame.FPredFileName := AFilename;
    AddToHistoryList(AFileName);
  except
    On E: Exception do
    begin
      RemoveFromHistoryList(AFileName);
      ShowMessage('Error: Open file: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.SelectLine(lNumber: Integer);
begin
   if FSelectedMemo <> nil then
    begin
     FSelectedMemo.ClearSelection;
     FSelectedMemo.SetCursor(0, lNumber);
     FSelectedMemo.ActiveLine := lNumber;
    end;
end;

procedure TfrmMain.MemoPasteEdit;
begin
  try
    FSelectedMemo.PasteFromClipboard;
  except
    On E: Exception do
      ShowMessage('Error: Paste edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoRedoEdit;
begin
  try
    FSelectedMemo.Redo;
  except
    On E: Exception do
      ShowMessage('Error: Redo edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoReplaceEdit;
begin
  try
    if TMSFMXMemoFindAndReplaceDialog1.Execute > 0 then
    begin

    end;
  except
    On E: Exception do
      ShowMessage('Error: Replace edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoSaveFile(const AFilename: string);
var
  SExt: string;
  SavedFileName: string;
  LWorkItem: TWorkItem;
begin
  case SaveDialog1.FilterIndex of
    1: SExt := '.pas';
    2: SExt := '.dpr';
    3: SExt := '.dpk';
    4: SExt := '.dfm';
    5: SExt := '.fmx';
    6: SExt := '.inc';
    7: SExt := '.cpp';
    8: SExt := '.h';
    9: SExt := '.cs';
    10: SExt := '.css';
    11: SExt := '.bas';
    12: SExt := '.js';
    13: SExt := '.htm';
    14: SExt := '.html';
    15: SExt := '.xml';
    16: SExt := '.sql';
  end;

  if not TPath.HasExtension(AFileName) then
    SavedFileName := AFileName + SExt
  else
    SavedFileName := AFileName;

  try
   if ((FSelectedFrame <> nil) AND (FSelectedMemo <> nil)) then
    begin
     //FSelectedMemo.Lines.SaveToFile(SavedFileName);
     FSelectedFrame.SaveMemoLines(SavedFileName);
     LWorkItem := WorkFilesTree.WorkItemByFileName(FSelectedFrame.FPredFileName);
     if LWorkItem <> nil then LWorkItem.Modified := False;
     FSelectedFrame.TMSFMXMemo1.ClearUndoRedo;
     FSelectedFrame.FMemoChanged := False;
     WorkFilesTree.RenameWorkItem(FSelectedFrame.FPredFileName, SavedFileName,
      FSelectedFrame.Tag);
     FSelectedFrame.lbFileName.Text := TPath.GetFileName(SavedFileName);
     FSelectedFrame.lbFilePath.Text := ExtractFilePath(SavedFileName);
    end;
  except
    On E: Exception do
    begin
     if FSelectedFrame <> nil then
      FSelectedFrame.FMemoChanged := True;
      ShowMessage('Error: Save file: ' + E.Message);
    end;
  end;
end;

procedure TfrmMain.MemoSearchEdit;
begin
  try
    if TMSFMXMemoFindDialog1.Execute > 0 then
    begin

    end;
  except
    On E: Exception do
      ShowMessage('Error: Search edit: ' + E.Message);
  end;
end;

procedure TfrmMain.MemoUndoEdit;
begin
  try
    FSelectedMemo.Undo;
  except
    On E: Exception do
      ShowMessage('Error: Undo edit: ' + E.Message);
  end;
end;

procedure TfrmMain.PlainSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := nil;
    lbStyleSetting.Text := CDefNone;
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.CSharpSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoCSharpStyler1;
    lbStyleSetting.Text := 'C#';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.CSSSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoCssStyler1;
    lbStyleSetting.Text := 'CSS';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.VBSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoBasicStyler1;
    lbStyleSetting.Text := 'Basic';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.JSSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoJavaScriptStyler1;
    lbStyleSetting.Text := 'JavaScript';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.JavaSynMIClick(Sender: TObject);
begin
  if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoJavaStyler;
    lbStyleSetting.Text := 'Java';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.WindowsMIClick(Sender: TObject);
begin
  ChangeCodeCompletionPlatform(Sender);
end;

procedure TfrmMain.AndroidMIClick(Sender: TObject);
begin
  ChangeCodeCompletionPlatform(Sender);
end;

procedure TfrmMain.MacosMIClick(Sender: TObject);
begin
  ChangeCodeCompletionPlatform(Sender);
end;

procedure TfrmMain.HTMLSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoWebStyler1;
    lbStyleSetting.Text := 'Web';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.IOSMIClick(Sender: TObject);
begin
  ChangeCodeCompletionPlatform(Sender);
end;

procedure TfrmMain.XMLSynMIClick(Sender: TObject);
begin
 if FSelectedMemo<>nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoXMLStyler1;
    lbStyleSetting.Text := 'XML';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.SQLSynMIClick(Sender: TObject);
begin
 if FSelectedMemo <> nil then
  begin
    FSelectedMemo.SyntaxStyles := TMSFMXMemoSqlStyler1;
    lbStyleSetting.Text := 'SQL';
    FSelectedMemo.Repaint;
  end;
end;

procedure TfrmMain.MoveHistoryBackward;
begin
  if FCurrHistPos > 0 then
  begin
    Dec(FCurrHistPos);
    ShowHistoryFile;
  end;
end;

procedure TfrmMain.MoveHistoryForward;
begin
  if FCurrHistPos < FileHistory.Count then
  begin
    Inc(FCurrHistPos);
    ShowHistoryFile;
  end;
end;

function TfrmMain.NewDefFileName: string;
begin
  Inc(FNewFileCount);
  Result := System.SysUtils.IncludeTrailingPathDelimiter(FActiveProjectDir) +
    CDefFileName + FNewFileCount.ToString;
end;

procedure TfrmMain.NewFile(AMemoFrame: TMemoFrame = nil);
var
  SDefFileName: string;
begin
  // Check opened MemoFrames...
  if MemoFrameVisibleCount = 0 then
  begin
    if AMemoFrame <> nil then AMemoFrame.Visible := True
    else
      AMemoFrame := MemoFrame2;
    MemoFrame2.Visible := True;
  end else
  begin
    if AMemoFrame = nil then AMemoFrame := FSelectedFrame;
  end;
  SetMemoFocus(AMemoFrame);

  if FSelectedFrame <> nil then
  begin
    WorkFilesTree.MoveToMemoStream(AMemoFrame);

    FSelectedMemo.Clear;
    FSelectedMemo.ClearUndoRedo;
    FSelectedFrame.FMemoChanged := False;
    FSelectedMemo.SetCursor(0, 0);
    SDefFileName := NewDefFileName;
    AddToHistoryList(SDefFileName);
    FSelectedFrame.FPredFileName := SDefFileName;
    FSelectedFrame.lbFileName.Text := TPath.GetFileName(SDefFileName);
    WorkFilesTree.AddToWorkList(SDefFileName, ftUnknown, FSelectedFrame.Tag);
  end;
end;

procedure TfrmMain.ReArrangeMemoFrames;
var
  FrameCount: integer;
  FrameWidth: Single;
begin
  FrameCount := MemoFrameVisibleCount;
  if FrameCount = 0 then
    Exit;

  if FrameCount>1 then
   begin
    FrameWidth := (Layout2.Width / FrameCount) - 8;
   end
  else
   begin
    FrameWidth := Layout2.Width;
   end;


  Self.duck.all.isa(TMemoFrame).each(
    procedure(obj: TObject)
    begin
      if TMemoFrame(obj).Visible then TMemoFrame(obj).Width := FrameWidth;
      if FrameCount>=3 then
       begin
         TMemoFrame(obj).btnSplitEditor.Visible := False;
       end
      else
       begin
         TMemoFrame(obj).btnSplitEditor.Visible := True;
       end;
    end
  );

end;

procedure TfrmMain.RemoveFromHistoryList(const AFileName: string);
var
  RemIndex: integer;
begin
  RemIndex := FileHistory.IndexOf(AFileName);
  if RemIndex <> -1 then RemoveFromHistoryList(RemIndex);
end;

procedure TfrmMain.RemoveFromHistoryList(AIndex: integer);
begin
  if (FileHistory.Count > 0) and (AIndex < FileHistory.Count) then
    FileHistory.Delete(AIndex);
end;

procedure TfrmMain.ShowAbout;
var
  sVer: string;
begin
  if not RectAbout.Visible then
  begin
    sVer := GetFileVersionStr;
    txtNameVer.Text := 'v' + sVer;
    AboutLogo.Fill.Bitmap.Bitmap.Assign(BGLogo.Fill.Bitmap.Bitmap);
    AboutLogo.Fill.Bitmap.WrapMode := TWrapMode.TileStretch;
    AboutLogo.Fill.Kind := TBrushKind.Bitmap;
    RectAbout.Visible := True;
  end;
end;

procedure TfrmMain.ShowCaption;
var
  sDir, sVer: string;
  n, len: integer;
begin
  n := FActiveProjectDir.LastIndexOf(System.SysUtils.PathDelim);
  if n < FActiveProjectDir.Length then
  begin
    len := FactiveProjectDir.Length - n;
    sDir := Copy(FActiveProjectDir, n + 2, len);
  end;
  sVer := GetFileVersionStr;
  Caption := Format(CCaption, [sDir, sVer]);
end;

procedure TfrmMain.ShowCaretPos;
begin
 if FSelectedMemo <> nil then
  LbCaretPos.Text := System.SysUtils.Format(CCaretPos, [Succ(FSelectedMemo.CurY), Succ(FSelectedMemo.CurX)]);
end;

procedure TfrmMain.ShowFileName(const AFileName: string);
begin
 if FSelectedFrame <> nil then
  begin
    Self.duck.all.isa(TMemoFrame).each(
      procedure(obj: TObject)
      begin
        TMemoFrame(obj).lbFileName.TextSettings.Font.Style := [];
      end
    );
    FSelectedFrame.lbFileName.Text := TPath.GetFileName(AFileName);
    if TFile.Exists(AFileName) then
      FSelectedFrame.lbFilePath.Text := ExtractFilePath(AFileName)
    else
      FSelectedFrame.lbFilePath.Text := EmptyStr;
    FSelectedFrame.lbFileName.TextSettings.Font.Style := [TFontStyle.fsBold];
  end;
end;

procedure TfrmMain.ShowHistoryFile;
begin
  if FCurrHistPos < FileHistory.Count then
  begin
    if (FSelectedFrame <> nil) then
      LoadFrameFromFile(FileHistory[FCurrHistPos], FSelectedFrame);
  end;
end;

function TfrmMain.ShowImage(const AFileName: string): boolean;
var
  n: integer;
begin
  Result := False;
  if SameText(FSelectedFrame.FPredFileName, AFileName) then
  begin
    if FAddToWorkFiles then
      WorkFilesTree.AddToWorkList(AFileName, ftImage, FSelectedFrame.Tag);
    FAddToWorkFiles := False;
    Exit;
  end;

  try
    n := FSelectedMemo.Lines.Count;
  if n = 1 then
  begin
    if FSelectedMemo.Lines.Text.Length < 3 then
      n := Trim(FSelectedMemo.Lines.Text).Length;
  end else
    if n > 0 then
      n := FSelectedMemo.Lines.Text.Length;

    if (not FSelectedFrame.FMemoChanged) and (n = 0) then
    begin
      if FSelectedFrame.FPredFileName.Contains(CDefFileName) then
        WorkFilesTree.DeleteFromWorkList(FSelectedFrame.FPredFileName, FSelectedFrame.Tag);
    end else
      WorkFilesTree.MoveToMemoStream(FSelectedFrame);

    FSelectedFrame.ImageViewer1.Bitmap.LoadFromFile(AFileName);
    ShowFileName(AFileName);
    FSelectedMemo.Visible := False;
    //FSelectedMemo.Lines.Clear;
    FSelectedMemo.Lines.Text := string.Empty;
    FSelectedFrame.pnlMemoFind.Visible := False;
    FSelectedFrame.treeZipView.Visible := False;
    if not FSelectedFrame.ImageViewer1.Visible then
      FSelectedFrame.ImageViewer1.Visible := True;

    FSelectedFrame.FPredFileName := AFilename;
    AddToHistoryList(AFileName);
    if FAddToWorkFiles then
      WorkFilesTree.AddToWorkList(AFileName, ftImage, FSelectedFrame.Tag);
    FAddToWorkFiles := False;
    Result := True;
  except
    On E: Exception do
      ShowMessage('Error: Show image: ' + E.Message);
  end;
end;

procedure TfrmMain.TabControl1Resize(Sender: TObject);
begin
  if TabControl1.Width < 100 then
    TabControl1.Width := 100;
  ShowSearchOptions(pnlSearchOption.Visible);
end;

procedure TfrmMain.TMSFMXMemo1GetAutoCompletionList(Sender: TObject;
  AToken: string; AList: TStringList);
//var
//  ridx, spos: integer; subs: string;
begin
{  if pos('FORM',UpperCase(AToken)) > 0 then
  begin
    for ridx := 0 to SLFormMethods.Count - 1 do
      AList.AddObject(SLFormMethods.Strings[ridx], TObject(ttMethod));

    for ridx := 0 to SLFormProperties.Count - 1 do
    begin
      spos := pos(': ', SLFormProperties.Strings[ridx]) + 1 ;
      subs := RightStr(SLFormProperties.Strings[ridx], length(SLFormProperties.Strings[ridx]) - spos);
      AList.AddObject('Property ' + LeftStr(SLFormProperties.Strings[ridx], spos-2) + ': ' + subs, TObject(ttProp));
    end;

    for ridx := 0 to SLFormEvents.Count-1 do
    begin
      spos := pos(': ', SLFormEvents.Strings[ridx]) + 1 ;
      subs := RightStr(SLFormEvents.Strings[ridx], length(SLFormEvents.Strings[ridx]) - spos);
      AList.AddObject('Event ' + LeftSTr(SLFormEvents.Strings[ridx], spos) + ' ' + Subs, TObject(ttEvent));
    end;

    for ridx := 0 to SLFormMethods.Count-1 do
    begin
      spos := pos('(', SLFormMethods.Strings[ridx]);
      if spos > 0 then
      begin
        spos := pos(' ', SLFormMethods.Strings[ridx]);
        subs := RightStr(SLFormMethods.Strings[ridx],length(SLFormMethods.Strings[ridx]) - spos);
        TMSFMXMemoPascalStyler1.HintParameter.Parameters.Add(subs);
      end;
    end;
  end;

  if pos('EDIT',UpperCase(AToken)) > 0 then
  begin
    ALIst.AddObject('procedure Show;', TObject(ttMethod));
    ALIst.AddObject('procedure SetFocus;', TObject(ttMethod));
    ALIst.AddObject('property Text string', TObject(ttProp));
    ALIst.AddObject('property Name string', TObject(ttProp));
    ALIst.AddObject('property Top integer', TObject(ttProp));
    ALIst.AddObject('property Left integer', TObject(ttProp));
    ALIst.AddObject('property Enabled boolean', TObject(ttProp));
    ALIst.AddObject('event OnCreate TNotifyEvent', TObject(ttEvent));
  end;

  if pos('BUTTON',UpperCase(AToken)) > 0 then
  begin
    ALIst.AddObject('procedure Show;', TObject(ttMethod));
    ALIst.AddObject('procedure SetFocus;', TObject(ttMethod));
    ALIst.AddObject('property Caption string', TObject(ttProp));
    ALIst.AddObject('property Name string', TObject(ttProp));
    ALIst.AddObject('property Top integer', TObject(ttProp));
    ALIst.AddObject('property Left integer', TObject(ttProp));
    ALIst.AddObject('property Enabled boolean', TObject(ttProp));
    ALIst.AddObject('event OnCreate TNotifyEvent', TObject(ttEvent));
  end;  }
end;

procedure TfrmMain.TMSFMXMemo1KeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=vkF1 then
   begin
     OpenURL('https://www.google.com/#q=site:docwiki.embarcadero.com+'+FSelectedMemo.FullWordAtCursor,False);
   end;
  SetMemoFocus(TMemoFrame(TTMSFMXMemo(Sender).Parent));
  ShowCaretPos;
end;

procedure TfrmMain.TMSFMXMemo1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SetMemoFocus(TMemoFrame(TTMSFMXMemo(Sender).Parent));
//  ShowFileName(FSelectedFrame.FPredFileName);
  ShowFileName(TMemoFrame(TTMSFMXMemo(Sender).Parent).FPredFileName);
  ShowCaretPos;
end;

procedure TfrmMain.Label3Click(Sender: TObject);
begin
  if not pnlSearchOption.Visible then
  begin
    ShowSearchOptions(True);
  end else
    ShowSearchOptions(False);
end;

procedure TfrmMain.LoadFrameFromFile(const AFileName: String; const AMemoFrame: TMemoFrame;
  const ALineNumber: Integer = 0; AReadOnly : Boolean = False);
var
  WorkItem: TWorkItem;
  FT: TFileType;
begin
  //restart monitoring
  FMonitoring.StopWatch(AMemoFrame.FPredFileName);
  AMemoFrame.FFileChanged := False;
  FMonitoring.StartWatch(AFileName);
  if MemoFrameVisibleCount = 0 then
  begin
    MemoFrame2.Visible := True;
    SetMemoFocus(MemoFrame2);
  end;

  WorkItem := WorkFilesTree.WorkItemByFileName(AFileName);
  if TFile.Exists(AFileName) or ((WorkItem <> nil) and (WorkItem.Stream.Size > 0)) then
  begin
    FT := FT.FileType(AFileName);
    // Let's load image
    if FT = ftImage  then
     begin
       ShowImage(AFileName);
       lbEncoding.Text := ExtractFileExt(AFileName);
       lbCRLF.Text := '';
     end else
     // Let's load zip-file
     if FT = ftArchive then
     begin
       ShowZipFile(AFileName, AMemoFrame);
       lbEncoding.Text := 'ZIP';
       lbCRLF.Text := '';
     end else
     // Let's load binary
    if (FT = ftBinary) or (FT = ftUnknown) then
    begin
      ShowBinaryFile(AFileName, AMemoFrame);
      lbEncoding.Text := 'Hex';
      lbCRLF.Text := '';
    end else
    if (FT = ftScript) then
    begin
      MemoLoadFile(AFileName, AMemoFrame, ALineNumber);
    end;
    if AReadOnly then
        AMemoFrame.TMSFMXMemo1.ReadOnly := True;
  end
  else
     // Create a new editor
  begin
    if AMemoFrame.FPredFileName = EmptyStr then
     NewFile(AMemoFrame);
  end;
end;

procedure TfrmMain.TreeView1Click(Sender: TObject);
var
  LItem: TTreeViewItem;
begin
  if ProgressBar1.Visible then
   Exit;
  LItem := TreeView1.Selected;
  if (LItem <> nil) and (LItem.ParentItem <> nil) then
  begin
    if (not Litem.TagString.IsEmpty) then
      TreeViewClick(LItem, LItem.ParentItem.Index);
  end;
end;

procedure TfrmMain.TreeView1DblClick(Sender: TObject);
var
  LItem: TTreeViewItem;
begin
  if ProgressBar1.Visible then
   Exit;

  LItem := TreeView1.Selected;
  if (LItem <> nil) and (LItem.ParentItem <> nil) then
  begin
    FAddToWorkFiles := True;
    if (not Litem.TagString.IsEmpty) then
      TreeViewClick(LItem, LItem.ParentItem.Index);
  end;
end;

procedure TfrmMain.TreeView2Click(Sender: TObject);
var
  LItem: TTreeViewItem;
  ALineNumber: Integer;
begin
  ALineNumber := 0;
  LItem := nil;
  if TreeView2.Selected<>nil then
    if TreeView2.Selected.Tag=1 then
      begin
        LItem := TreeView2.Selected.ParentItem;
        ALineNumber := Trunc(TreeView2.Selected.TagFloat);
      end
    else
      LItem := TreeView2.Selected;

  if LItem <> nil then
  begin
    LoadFrameFromFile(LItem.TagString, FSelectedFrame, ALineNumber);
    if FSelectedFrame <> nil then
    begin
        FSelectedFrame.edFindText.Text := Self.TreeView2.TagString;
        self.actFindTextExecute(nil);
    end;
  end;
end;

procedure TfrmMain.TreeViewClick(Sender: TObject; AIndex: integer);
var
  LItem: TTreeViewItem;
begin
  if (Sender <> nil) and (Sender is TTreeViewItem) then
  begin
    LItem := Sender as TTreeViewItem;
    if TFile.Exists(LItem.TagString) then
    begin
      LoadFrameFromFile(LItem.TagString, FSelectedFrame);
      TryOpenFormUnit(LItem.TagString);
    end;
  end;
end;

procedure TfrmMain.TryOpenFormUnit(AFileName: string);
var
  VictimFrame: TMemoFrame;
  SelFrame: TMemoFrame;
  SForm: string;
begin
  if FMemoSettings.FAutoOpen then
    SForm := GetFormUnitFile(AFileName);
  if SForm = '' then
    Exit;

  SelFrame := FSelectedFrame;
  try
    VictimFrame := nil;
    case FSelectedFrame.btnSplitEditor.Tag of
      1: begin
          if (MemoFrame2.Visible) or (MemoFrame3.Visible) then
          begin
            if MemoFrame2.Visible then VictimFrame := MemoFrame2
              else if MemoFrame3.Visible then VictimFrame := MemoFrame3;
          end;
         end;
      2: begin
          if (MemoFrame1.Visible) or (MemoFrame3.Visible) then
          begin
            if MemoFrame1.Visible then VictimFrame := MemoFrame1
              else if MemoFrame3.Visible then VictimFrame := MemoFrame3;
          end;
         end;
      3: begin
          if (MemoFrame2.Visible) or (MemoFrame1.Visible) then
          begin
            if MemoFrame2.Visible then VictimFrame := MemoFrame2
              else if MemoFrame1.Visible then VictimFrame := MemoFrame1;
          end;
         end;
    end;

    if VictimFrame = nil then
      VictimFrame := SplitEditor(FSelectedFrame.btnSplitEditor.Tag);

    if VictimFrame <> nil then
    begin
      LoadFrameFromFile(SForm, VictimFrame);
      ShowFileName(VictimFrame.FPredFileName);
      ReArrangeMemoFrames;
    end;
  finally
    SetMemoFocus(SelFrame);
    ShowFileName(FSelectedFrame.FPredFileName);
  end;
end;

procedure TfrmMain.UpdateActiveProjectDir;
var
  s: string;
  n, len: integer;
begin
  n := FActiveProjectDir.LastIndexOf(System.SysUtils.PathDelim);
  if n < FActiveProjectDir.Length then
  begin
    len := FactiveProjectDir.Length - n;
    s := Copy(FActiveProjectDir, n + 2, len);
    TreeView1.Items[1].Text := s;
  end;
end;

procedure TfrmMain.UpdateImages(AImageList: TImagelist);
begin
  Self.duck.all.isa(TImage).each(
    procedure(obj: TObject)
    begin
      if AImageList.BitmapExists(TImage(obj).Tag) then
        TImage(obj).Bitmap := AImageList.Bitmap(TSize.Create(32, 32), TImage(obj).Tag);
    end
  );
  Self.duck.all.isa(TFrame).each(
    procedure(obj: TObject)
    begin
      TFrame(obj).duck.all.isa(TImage).each(
        procedure(obj: TObject)
        begin
          if AImageList.BitmapExists(TImage(obj).Tag) then
            TImage(obj).Bitmap := AImageList.Bitmap(TSize.Create(32, 32), TImage(obj).Tag);
        end
      )
    end
  );
end;

procedure TfrmMain.ShowPluginsPanel(AVisible: boolean);
begin
  if AVisible then
  begin
    PluginsPanel.Position.X := ClientWidth - PluginsPanel.Width;
    PluginsPanel.Position.Y := 0;
    PluginsPanel.Height := ClientHeight;
    PluginsPanel.Scale.X := ZoomLayout.Scale.X;
    PluginsPanel.Scale.Y := ZoomLayout.Scale.Y;
  end;
  PluginsPanel.Visible := AVisible;
end;

procedure TfrmMain.ShowSearchOptions(AVisible: boolean);
begin
  if AVisible then
  begin
    pnlSearchOption.Position.X := SearchLayout.Position.X;
    pnlSearchOption.Position.Y := pnlSearch.Position.Y + pnlSearch.Height;
    pnlSearchOption.Width := SearchLayout.Width;
    pnlSearchOption.Scale.X := ZoomLayout.Scale.X;
    pnlSearchOption.Scale.Y := ZoomLayout.Scale.Y;
  end;
  pnlSearchOption.Visible := AVisible;
end;

function TfrmMain.SplitEditor(Sender: TObject): TMemoFrame;
begin
  if (Sender is TAction) then
    Result := SplitEditor(TAction(Sender).Target.Tag)
  else
    Result := nil;
end;

function TfrmMain.SplitEditor(ATag: integer): TMemoFrame;
begin
  Result := nil;
  case ATag of
    1:
    begin
      if MemoFrame2.Visible = False then
      begin
        Splitter2.Visible := True;
        MemoFrame2.Visible := True;
        Result := MemoFrame2;
      end
      else
      if MemoFrame3.Visible = False then
      begin
        Splitter3.Visible := True;
        MemoFrame3.Visible := True;
        Result := MemoFrame3;
      end;
    end;
    2:
    begin
      if MemoFrame1.Visible = False then
      begin
        Splitter2.Visible := True;
        MemoFrame1.Visible := True;
        Result := MemoFrame1;
      end
      else
      if MemoFrame3.Visible = False then
      begin
        Splitter3.Visible := True;
        MemoFrame3.Visible := True;
        Result := MemoFrame3;
      end;
    end;
    3:
    begin
      if MemoFrame1.Visible = False then
      begin
        Splitter3.Visible := True;
        MemoFrame1.Visible := True;
        Result := MemoFrame1;
      end
      else
      if MemoFrame2.Visible = False then
      begin
        Splitter2.Visible := True;
        MemoFrame2.Visible := True;
        Result := MemoFrame2;
      end;
    end
    else
      Result := nil;
  end;
end;


procedure TfrmMain.SearchInFiles;
var
  I: integer;
begin
  FBreakSearchInFiles := False;
  if edFindInFiles.Text.IsEmpty then
    Exit;

  TreeView2.Clear;
  TreeView2.BeginUpdate;
  try
  // Start from Item-1 because Item-0 is parent to WorkItems
    for I := 1 to TreeView1.Count - 1 do
    begin
      if FBreakSearchInFiles then Break;
      WalkSearch(TreeView1.Items[I]);
    end;
  finally
    TreeView2.EndUpdate;
  end;

end;

procedure TfrmMain.SearchTextInFile(AFileName, AText: string);
var
  LNewNode: TTreeViewItem;
  LNewChildNode: TTreeViewItem;
  FReader: TextFile;
  m, n: integer;
  TotalCount, FoundCount: integer;
  s: string;
begin
  TotalCount := 0;

      AssignFile(FReader, AFileName);
      try
        Reset(FReader);
        LNewNode := TTreeViewItem.Create(TreeView2);
        LNewNode.Parent := TreeView2;
        LNewNode.Text := TPath.GetFileName(AFileName);
        FoundCount := 0;
        while not EOF(FReader) do
        begin
          if FBreakSearchInFiles then Break;
          ReadLn(FReader, s);
          n := PosEx(AText, s, 1);
          if n > 0 then
          begin
            FoundCount := 0;
            repeat
              Inc(FoundCount);
              m := n + 1;
              n := PosEx(AText, s, m);
              until n = 0;
              if FoundCount > 0 then
              begin
                Inc(TotalCount, FoundCount);
                LNewChildNode := TTreeViewItem.Create(TreeView2);
                LNewChildNode.Parent := LNewNode;
                LNewChildNode.Text := s;
              end;
          end;
          Application.ProcessMessages;
        end;
        if FoundCount = 0 then LNewNode.DisposeOf
          else LNewNode.Text := LNewNode.Text + '  found: ' + TotalCount.ToString;
      finally
        CloseFile(FReader);
      end;
end;

procedure TfrmMain.WalkSearch(AItem: TTreeViewItem);
var
  ItemList: TList<TTreeViewItem>;
  LItem: TTreeViewItem;
  I: integer;
begin
  ItemList := TList<TTreeViewItem>.Create;
  try
    for I := 0 to AItem.Count - 1 do
      ItemList.Add(AItem.Items[I]);

    if ItemList.Count > 0 then
    begin
      for LItem in ItemList do
        if LItem.Count > 0 then
          WalkSearch(LItem)
        else
        //if LItem.ChildrenCount > 2 then
        if not LItem.TagString.IsEmpty then
        begin
          SearchTextInFile(LItem.TagString, edFindInFiles.Text);
        end;
    end else if not AItem.TagString.IsEmpty then
      SearchTextInFile(AItem.TagString, edFindInFiles.Text)
  finally
    ItemList.Free;
  end;
end;

procedure TfrmMain.ZoomLayoutResize(Sender: TObject);
begin
  ShowPluginsPanel(PluginsPanel.Visible);
end;

procedure TfrmMain.ParseTimerTimer(Sender: TObject);
begin
 if FSelectedMemo <> nil then
  begin
      ChangeCode(FSelectedMemo.Lines.Text);
  end;
end;

procedure TfrmMain.ChangeCode(const ACode: string);
var
    temp : string;
    LmSecCount : Integer;
    LNow : TDateTime;
    LTask : ITask;
    ASourceCode, LFileExt: String;
begin
    LFileExt := ExtractFileExt(FSelectedFrame.lbFilename.Text);
    if (lbStyleSetting.Text<>'Pascal') OR (LFileExt ='.fmx') or (LFileExt ='.dfm') then
     begin
      ASourceCode := '';
     end
    else
     ASourceCode := ACode;
    
     
    if FCodeList.Text.Contains(ASourceCode) then
        Exit;
    FCodeList.Text := ASourceCode;
    if Assigned(LTask) then
        Exit;
    LTask := TTask.Create(
        procedure
        var
            LText : string;
        begin
            try
                LNow := Now;
                //temp := ParseContent(ACode);
                //if Info.Parse(temp, True) then
                //    LText := 'Success';
                if Info.ParseFromText(ACode, True) then
                    LText := 'Success';
            except
                on E: EParserException do
                    LText := (Format('[%d, %d] %s', [E.Line, E.Col, E.Message]));
            end;
            LmSecCount := MilliSecondsBetween(Now, LNow);
            if FSelectedMemo<>nil then
              if (FSelectedMemo.Popup.Visible=False) AND (FSelectedMemo.Lines.Text<>'') then
               begin
                TThread.Queue(nil,
                    procedure
                    begin
                        try
                          lblCompile.Text := LText + ' Time : ' + string.Parse(LmSecCount);
                        except
                        end;
                    end);
               end;
        end
    );
    LTask.Start;
end;

function TfrmMain.FindFullToken(const CurX, CurY: Integer): string;
    procedure FillList(var AList : TList<Integer>; APos : Integer;
        AText, AFindText : string);
    var
        i :Integer;
    begin
        i := APos;
        while i > 0 do
        begin
            AList.Add(i);
            i := Pos(AFindText, AText, i + 1);
        end;
    end;
    function GetToken(const AText : string) : string;
    begin
        self.GetTokenMemo.Clear;
        GetTokenMemo.Lines.Text := AText;
        Result := GetTokenMemo.WordAtXY(AText.Length - 1, 0);
        //need for view params after "("
        FToken := GetTokenMemo.TokenAtXY(AText.Length, 0);
    end;
var
    LCurX, LCurY, i, j, LFirstOpen, LFirstClose, iCLose, iOpen : Integer;
    listOpen, listClose : TList<Integer>;
    LLineText : string;
begin
    LCurX := CurX;
    listOpen := TList<Integer>.Create;
    listClose := TList<Integer>.Create;
    LLineText := self.FSelectedMemo.Lines[CurY];
    if LLineText.Length - 1 > CurX then
        Delete(LLineText, CUrX + 1, LLineText.Length - 1);
    LFirstOpen := Pos(TConst.OPEN, LLineText);
    LFirstClose := Pos(TConst.Close, LLineText, LFirstOpen);
    FillList(listOpen, LFirstOpen, LLineText, TConst.Open);
    FillList(listClose, LFirstClose, LLineText, TConst.Close);
    for iCLose in listClose do
    begin
        for i := listOpen.Count - 1 downto 0 do
        begin
            iOpen := listOpen[i];
            if iOpen < iCLose then
            begin
                for j := iOpen to iClose do
                    LLineText[j] := TConst.Temp;
                listOpen.Delete(i);
                Break;
            end;
        end;
    end;
    LLineText := StringReplace(LLineText, TConst.Temp, '', [rfReplaceAll]);
    Result := GetToken(LLineText.Trim);
    if Result.LastIndexOf(TConst.Open) = Result.Length - 1 then
        SetLength(Result, Result.Length- 1);
    FreeAndNil(listOpen);
    FreeAndNil(listCLose);
end;

procedure TfrmMain.ChangeCodeCompletionPlatform(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    lblCHangeCCPlatform.Text := TMenuItem(Sender).Text;
    Info.CurrentPlatform := lblCHangeCCPlatform.Text;
  end;
end;

procedure TfrmMain.ChangeFile(const AFName: string);
begin
    //need show dialog and reload frame
    SetFileChanged(AFName);
end;

procedure TfrmMain.CheckFileChanged(AMemoFrame: TMemoFrame);
var
    DlgResult : integer;
    Msg : string;
begin
    if (not Assigned(AMemoFrame)) or (not AMemoFrame.FFileChanged) then
        Exit;
    Msg := Format(CChangeDialogText, [AMemoFrame.FPredFileName]);
    DlgResult :=
        FMX.Dialogs.MessageDlg(
            Msg,
            TMsgDlgType.mtInformation, [System.UITypes.TMsgDlgBtn.mbYes,
            System.UITypes.TMsgDlgBtn.mbNo], 0
{$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
        );
{$ELSE}
        ,
        procedure(const AResult: TModalResult)
        begin
            DlgResult := AResult;
{$ENDIF}
            case DlgResult of
              // Reload file
              mrYes: actReloadFrameExecute(AMemoFrame);
              // No save. Lose changes.
              mrNo: AMemoFrame.FFileChanged := False;
            end;
{$IF DEFINED(MSWINDOWS) OR (DEFINED(MACOS) AND NOT DEFINED(IOS))}
{$ELSE}
        end);
{$ENDIF}
end;

procedure TfrmMain.SetFileChanged(const AFName : string);
begin
    Self.duck.all.isa(TMemoFrame).each(
        procedure(obj: TObject)
        var
            lMemoFrame : TMemoFrame;
        begin
            lMemoFrame := TMemoFrame(obj);
            if lMemoFrame.FPredFileName.Trim.Equals(AFName.Trim) then
                lMemoFrame.FFileChanged := True;
        end
    );
end;

function TfrmMain.GetWidthText(AObj : TText): Single;
var
  TextLayout: TTextLayout;
begin
  if not Assigned(AObj) then
    exit;
  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    try
      TextLayout.Text := AObj.Text;
      TextLayout.MaxSize := TPointF.Create(1000, AObj.Height);
      TextLayout.WordWrap := false;
      TextLayout.Font := AObj.Font;
      TextLayout.HorizontalAlign := AObj.TextSettings.HorzAlign;
      TextLayout.VerticalAlign := AObj.TextSettings.VertAlign;
    finally
      TextLayout.EndUpdate;
    end;
    AObj.Width := TextLayout.TextRect.Width + AObj.Margins.Right;
    AObj.UpdateRect;
  finally
    TextLayout.Free;
  end;
end;

procedure TfrmMain.AutoCompleteListApplyStyleLookup(Sender: TObject);
var
    LListItem : TListBoxItem;
    LText : TText;
begin
    LListItem := TListBoxItem(Sender);
    if not Assigned(LListItem) then
        Exit;
    LListItem.StylesData['descript'] := TValue.From<string>(LListItem.TagString);
    LListItem.StylesData['laydetail.visible'] := TValue.From<boolean>(not LListItem.ItemData.Detail.IsEmpty);
    //LListItem.StylesData['types'] := TValue.From<string>(LListItem.ItemData.Detail);
    LText := LListItem.FindStyleResource('text') as TText;
    GetWidthText(LText);
    LListItem.StylesData['descript.margins.left'] := LListItem.StylesData['text.width'];
end;

procedure TfrmMain.AutoCompletionCustomizeItem(Sender: TObject;
  AAutoCompletionItem: TListBoxItem);
var
    LFindList : TArray<string>;
    LText, LDetail, LDesc : string;
    i, count : Integer;
    LDelimiter : Char;
begin
 if lbStyleSetting.Text<>'Pascal' then Exit;

    AAutoCompletionItem.OnApplyStyleLookup := AutoCompleteListApplyStyleLookup;
    AAutoCompletionItem.BeginUpdate;
    LText := AAutoCompletionItem.Text;
    LDelimiter := uCOnst.TConst.DELIMITER;
    LFindList := LText.Split(LDelimiter);
    count := Length(LFindList);
    for i := 0 to count - 1 do
    begin
        LFindList[i] := StringReplace(LFindList[i], TConst.SPACE_REPLACE, ' ', [rfReplaceAll]);
        case i of
            0:
            begin
                LText := LFindList[i];
                //if OpenParam then
                    LText := StringReplace(LText, FToken, '', []);
            end;
            1: LDesc := LFindList[i];
            2: LDetail := LFindList[i];
        end;
    end;
    AAutoCompletionItem.Text := LText.Trim;
    AAutoCompletionItem.ItemData.Detail := LDetail;
    AAutoCompletionItem.TagString := LDesc;
    AAutoCompletionItem.EndUpdate;
    //AAutoCompletionItem.StyleName := 'menuAutoCompleteListStyle';
    AAutoCompletionItem.NeedStyleLookup;
    AAutoCompletionItem.ApplyStyleLookup;
end;

procedure TfrmMain.AutoCompletionCustomizeList(Sender: TObject;
  AAutoCompletionList: TMemoComboListBox);
begin
 if lbStyleSetting.Text<>'Pascal' then Exit;

    AAutoCompletionList.DefaultItemStyles.ItemStyle := 'menuAutoCompleteListStyle';
  //AAutoCompletionList.DefaultItemStyles.ItemStyle := 'menuAutoCompleteListStyle';
    //AAutoCompletionList.ItemHeight := 35;
end;

procedure TfrmMain.BeforeAutoCompletion(Sender: TObject; AToken: string;
  var Show: Boolean);
var
    temp, temp2 : string;
begin
exit;           //temp;

 if lbStyleSetting.Text<>'Pascal' then Exit;

    lblCompile.Text := AToken;
    temp2 := TTMSFMXMemo(Sender).WordAtXY(TTMSFMXMemo(Sender).CurX , TTMSFMXMemo(Sender).CurY);
    temp := TTMSFMXMemo(Sender).WordAtXY(TTMSFMXMemo(Sender).CurX - 1, TTMSFMXMemo(Sender).CurY);
end;

procedure TfrmMain.CursorChange(Sender: TObject);
begin
 if lbStyleSetting.Text<>'Pascal' then Exit;
    FLock := FLock and (TTMSFMXMemo(Sender).CurY = FBktPos.Y) and (TTMSFMXMemo(Sender).CurX > FBktPos.X);
    OpenParam := OpenParam and FLock;
    Info.FindMethod(TPoint.Create(TTMSFMXMemo(Sender).CurX,
        TTMSFMXMemo(Sender).CurY + 1));
end;

procedure TfrmMain.GetAutoCompletionList(Sender: TObject; AToken: string;
  AList: TStringList);
var
    LList : TList<string>;
    LPosPoint, i : Integer;
    LWord, TextItem : string;
begin
 if lbStyleSetting.Text<>'Pascal' then Exit;

    AToken := FindFullToken(FSelectedMemo.CurX, FSelectedMemo.CurY);

    if Info.TryGetValue(AToken, LList, True, OpenParam) then
    begin
        AList.Clear;
        AList.AddStrings(LList.ToArray);
        // space create error fro TMSMEMO(
        AList.Text := StringReplace(AList.Text,' ', TConst.SPACE_REPLACE, [rfReplaceAll]);
        AList.Text := stringreplace(AList.Text, TConst.AST_STRING_BUG, TConst.AST_STRING_FIX, [rfReplaceAll]);
        //if OpenParam then
            for i := 0 to AList.Count - 1 do
                AList[i] := FToken + AList[i];
        FreeAndNil(LList);
    end ;

end;

procedure TfrmMain.InitMonitoring;
begin
    FMonitoring := TMonitoring.Create;
    FMonitoring.OnFileChanges := ChangeFile;
end;

procedure TfrmMain.InsertAutoCompletionEntry(Sender: TObject;
  var AEntry: string);
var
    LFindList : TArray<string>;
    temp : string;
    i, count : integer;
begin
    if OpenParam then
        AEntry := EmptyStr;
end;

procedure TfrmMain.MemoKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
    if KeyChar = '(' then
    begin
        FLock := True;
        OpenParam := True;
        FBktPos.X := TTMSFMXMemo(Sender).CurX;
        FBktPos.Y := TTMSFMXMemo(Sender).CurY;
    end;
    if KeyChar = ')' then
    begin
        OpenParam := False;
        TTMSFMXMemo(Sender).Popup.HidePopup;
        //FLock := False;
    end;
end;

function TfrmMain.ParseContent(const Content: string): string;
var
  ASTBuilder: TPasSyntaxTreeBuilder;
  StringStream: TStringStream;
  SyntaxTree: TSyntaxNode;
begin
  Result := '';

  StringStream := TStringStream.Create(Content, TEncoding.Unicode);
  try
    StringStream.Position := 0;

    ASTBuilder := TPasSyntaxTreeBuilder.Create;
    try    //ASTBuilder.Lexer.OnElseDirect
      ASTBuilder.InitDefinesDefinedByCompiler;
      //ASTBuilder.UseDefines := True;
      //ASTBuilder.AddDefine('ANDROID');
      //ASTBuilder.AddDefine('CPUX86');
      SyntaxTree := ASTBuilder.Run(StringStream);
      SyntaxTree_ := SyntaxTree.Clone;
      try       //TTMSFMXMemo().TokenAtXY()
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

procedure TfrmMain.SetOpenParam(const Value: Boolean);
begin
    FOpenParam := Value;
    if not Value and Assigned(Info) then
        Info.PrevList.IsParam := Value;
end;


end.
