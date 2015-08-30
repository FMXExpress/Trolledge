unit uPluginEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  uPlugin, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox,
  FMX.Objects, FMX.Layouts, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions;

type
  TfPluginEdit = class(TForm)
    lyt1: TLayout;
    img: TImage;
    cbbTypes: TComboBox;
    txtName: TEdit;
    txtPath: TEdit;
    btnSave: TButton;
    btnCancel: TButton;
    lyt2: TLayout;
    btnLoadImage: TButton;
    dlgOpen: TOpenDialog;
    btnAddScript: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbbTypesChange(Sender: TObject);
    procedure btnAddScriptClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FPlugin : TPlugin;
    procedure LoadPLugin(pPlugin : Pointer);
  end;

var
  fPluginEdit: TfPluginEdit;

implementation
uses
    uMain;
{$R *.fmx}
{------------------------------------------------------------------------------}
procedure TfPluginEdit.btnAddScriptClick(Sender: TObject);
begin
  {$IFDEF MACOS}
    if dlgOpen.Execute then
    begin
        txtPath.Text := dlgOpen.FileName;
    end;
    exit;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    if dlgOpen.Execute then
    begin
        txtPath.Text := dlgOpen.FileName;
    end;
  {$ENDIF}
end;

procedure TfPluginEdit.btnLoadImageClick(Sender: TObject);
var
    sJson, encodeString : string;
begin
  {$IFDEF MACOS}
    if dlgOpen.Execute then
    begin
        img.Bitmap.LoadFromFile(dlgOpen.FileName);
    end;
    exit;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    if dlgOpen.Execute then
    begin
        img.Bitmap.LoadFromFile(dlgOpen.FileName);
    end;
  {$ELSE}
    //fMain.TakePhotoFromLibraryAction.ExecuteTarget(img);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TfPluginEdit.btnSaveClick(Sender: TObject);
begin
    FPlugin.FName := txtName.Text;
    FPlugin.FIcon.Assign(img.Bitmap);
    FPlugin.FPath := txtPath.Text;
    FPlugin.FType := cbbTypes.ItemIndex;
end;
{------------------------------------------------------------------------------}
procedure TfPluginEdit.cbbTypesChange(Sender: TObject);
begin
    btnAddScript.Visible := cbbTypes.ItemIndex = 1;
end;
{------------------------------------------------------------------------------}
procedure TfPluginEdit.FormCreate(Sender: TObject);
var
    iType : string;
begin
    FPlugin := TPlugin.Create;
    cbbTypes.Clear;
    cbbTypes.Items.AddStrings(uPlugin.TPlugins.FTypes.ToStringArray);
    cbbTypes.ItemIndex := 0;
end;
{------------------------------------------------------------------------------}
procedure TfPluginEdit.FormDestroy(Sender: TObject);
begin
    FPlugin := nil;
end;

procedure TfPluginEdit.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkEscape then
    ModalResult := mrCancel;
end;

{------------------------------------------------------------------------------}
procedure TfPluginEdit.LoadPLugin(pPlugin : Pointer);
var
    plugin : TPlugin;
begin
    plugin := TPlugin(pPlugin);
    if not Assigned(plugin) then
        exit;
    FPlugin := plugin;
    txtName.Text := FPlugin.FName;
    img.Bitmap.Assign(FPlugin.FIcon);
    txtPath.Text := FPlugin.FPath;
    cbbTypes.ItemIndex := FPlugin.FType;
end;
{------------------------------------------------------------------------------}
end.
