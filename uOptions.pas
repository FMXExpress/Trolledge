
unit uOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.TabControl, FMX.Controls.Presentation, FMX.Ani,
  FMX.Colors, FMX.TMSBaseControl, FMX.TMSMemo, FMX.TMSMemoStyles,
  FMX.Objects, uUtils, duck
  {$IFDEF MSWINDOWS}
  , FMX.Platform.Win
  {$ENDIF}
  , FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView
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
    Layout1: TLayout;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure cbStylesListChange(Sender: TObject);
    procedure chkGutterVisibleClick(Sender: TObject);
    procedure chkMemoRightMarginClick(Sender: TObject);
    procedure btnSearchPathClick(Sender: TObject);

    procedure TabControl1Change(Sender: TObject);
    procedure cbStylesListClosePopup(Sender: TObject);

    procedure lvPluginsChange(Sender: TObject);

  private
    { Private declarations }
    procedure ShowStylerStyles(AStylerName: string);
  public
    { Public declarations }
    {$IFDEF MSWINDOWS}
    Handle: TWindowHandle;
    {$ENDIF}
    lvPlugins: TListView;
    lytBtn: TLayout;
    btnAdd: TButton;
    btnEdit: TButton;
    btnRemove: TButton;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  if SelectDirectory('Please select directory...', NewPath{$IFDEF MSWINDOWS}, FMX.Platform.Win.WindowHandleToPlatform(Handle).Wnd{$ENDIF}) then
  begin
    edSearchPath.Text := NewPath;
  end;
end;

procedure TOptionsFrame.cbStylesListChange(Sender: TObject);
begin
  if cbStylesList.ItemIndex <> -1 then
    ShowStylerStyles(cbStylesList.Items[cbStylesList.ItemIndex]);
end;

procedure TOptionsFrame.cbStylesListClosePopup(Sender: TObject);
begin
cbStylesListChange(Sender);
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

constructor TOptionsFrame.Create(AOwner: TComponent);
begin
  inherited;
{$REGION 'Plugins'}
    lvPlugins:= TListView.Create(self);
    with lvPlugins do
    begin
      ItemAppearanceClassName := 'TImageListItemAppearance';
      ItemEditAppearanceClassName := 'TImageListItemDeleteAppearance';
      HeaderAppearanceClassName := 'TListHeaderObjects';
      FooterAppearanceClassName := 'TListHeaderObjects';
      Align := TAlignLayout.Client;
      Size.Width := 577.585632324218800000;
      Size.Height := 344.691680908203100000;
      Size.PlatformDefault := False;
      TabOrder := 0;
      ItemAppearance.ItemHeight := 100;
      ItemAppearance.ItemEditHeight := 100;
      ItemAppearanceObjects.ItemObjects.Image.ScalingMode := TImageScalingMode.Stretch;
      ItemAppearanceObjects.ItemObjects.Image.Width := 80.000000000000000000;
      ItemAppearanceObjects.ItemObjects.Image.Height := 80.000000000000000000;
      OnClick := lvPluginsChange;
      OnChange := lvPluginsChange;
      OnItemClick := frmMain.lvPluginsItemClick;
      Parent := self.tbiPlugins;
    end ;

    lytBtn := TLayout.Create(self);
    with lytBtn do
    begin
        Align := TAlignLayout.Bottom;
        Margins.Top := 4.000000000000000000;
        Margins.Bottom := 4.000000000000000000;
        Position.Y := 348.691680908203100000;
        Size.Width := 577.585632324218800000;
        Size.Height := 37.000000000000000000;
        Size.PlatformDefault := False;
        TabOrder := 1;
        Parent := lvPlugins;
    end;
    btnRemove := TButton.Create(self);
    with btnRemove do
    begin
        Action := frmMain.actRemovePlugin;
        OnClick := frmMain.actRemovePluginExecute;
        Align := TAlignLayout.Right;
        Enabled := True;
        ImageIndex := -1;
        Margins.Left := 8.000000000000000000;
        Margins.Right := 8.000000000000000000;
        Position.X := 489.585632324218800000;
        Size.Width := 80.000000000000000000;
        Size.Height := 37.000000000000000000;
        Size.PlatformDefault := False;
        TabOrder := 1;
        Text := 'Remove';
        Visible := True;
        Enabled := False;
        Parent := lytBtn;
    end;
    btnEdit := TButton.Create(self);
    with btnEdit do
    begin
        Action := frmMain.actEditPlugin;
        OnClick := frmMain.actEditPluginExecute;
        Enabled := True;
        ImageIndex := -1;
        Margins.Left := 8.000000000000000000;
        Position.X := 401.585632324218800000;
        Size.Width := 80.000000000000000000;
        Size.Height := 37.000000000000000000;
        Align := TAlignLayout.Right;
        Size.PlatformDefault := False;
        TabOrder := 2;
        Text := 'Edit';
        Visible := True;
        Enabled := False;
        Parent := lytBtn;
    end;

    btnAdd := TButton.Create(self);
    with btnAdd do
    begin
        Action := frmMain.actAddPlugin;
        OnClick := frmMain.actAddPluginExecute;
        Enabled := True;
        ImageIndex := -1;
        Position.X := 313.585632324218800000;
        Size.Width := 80.000000000000000000;
        Size.Height := 37.000000000000000000;
        Align := TAlignLayout.Right;
        Size.PlatformDefault := False;
        TabOrder := 0;
        Text := 'Add';
        Visible := True;
        Parent := lytBtn;
    end;
{$ENDREGION}
end;

destructor TOptionsFrame.Destroy;
begin
  inherited;
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
  self.TabControl1.TabIndex := 0;
 // ShowStylerStyles('Pascal');
end;

procedure TOptionsFrame.lvPluginsChange(Sender: TObject);
var
    LCanEdit : Boolean;
begin
    frmMain.lvPluginsChange(Sender);
    LCanEdit := TListView(Sender).ItemCount > 0;
    btnEdit.Enabled := LCanEdit;
    btnRemove.Enabled := LCanEdit;
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


procedure TOptionsFrame.TabControl1Change(Sender: TObject);
begin
cbStylesListChange(Sender);
end;

end.
