unit uPluginsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.ListView.Types, FMX.Menus, FMX.ListView,
  PaxRunner, PaxProgram, PaxCompiler, Data.Bind.Components,
  Data.Bind.ObjectScope,
  FMX.Features.BitmapHelper, OpenViewUrl, uPlugin, uPluginEdit, uPluginUtils,
  Fmx.Bind.GenData, Data.Bind.GenData;

const
  filePlugins: string = 'filePlugins.plugins';

type
  TPluginsFrame = class(TFrame)
    lvPlugins: TListView;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    procedure lvPluginsChange(Sender: TObject);
    procedure lvPluginsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//var
//  PluginsFrame: TPluginsFrame;

implementation

{$R *.fmx}

procedure TPluginsFrame.lvPluginsChange(Sender: TObject);
begin
  //FPlugins.ItemIndex := lvPlugins.ItemIndex;
end;

procedure TPluginsFrame.lvPluginsClick(Sender: TObject);
begin
  //FPlugins.ItemIndex := lvPlugins.ItemIndex;
  //PluginOpen;
end;

end.
