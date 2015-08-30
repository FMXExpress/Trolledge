unit uUtils;

// Android and IOS code by Jim McKeeth
// Mac OSX code by Malcom Groves


interface

uses
  System.Types,
  IdURI, SysUtils, Classes, FMX.Dialogs,
{$IFDEF ANDROID}
  Androidapi.Helpers,
  FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.JavaTypes
{$ELSE}
{$IFDEF IOS}
  Macapi.Helpers, iOSapi.Foundation, FMX.Helpers.iOS
{$ELSE}
{$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows, System.Generics.Collections,
  System.AnsiStrings, System.StrUtils, System.IOUtils
{$ELSE}
{$IFDEF MACOS}
  Macapi.Foundation, Macapi.AppKit, Macapi.Helpers, Macapi.ObjectiveC,
  Macapi.CocoaTypes, Macapi.CoreFoundation
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
{$ENDIF IOS}
{$ENDIF ANDROID}
  ;

function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
function GetFileVersionStr: string;
function GetMethodsList(AUnitName: string): TStringDynArray;
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
function NSToDelphiString(const NSStr: NSString): string;
{$ENDIF MACOS}

implementation

function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
{$IFDEF ANDROID}
var
  Intent: JIntent;
begin
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(URL))));
  try
    SharedActivity.startActivity(Intent);
    exit(true);
  except
    on e: Exception do
    begin
      if DisplayError then ShowMessage('Error: ' + e.Message);
      exit(false);
    end;
  end;
end;
{$ELSE}
{$IFDEF IOS}
var
  NSU: NSUrl;
begin
  // iOS doesn't like spaces, so URL encode is important.
  NSU := StrToNSUrl(TIdURI.URLEncode(URL));
  if SharedApplication.canOpenURL(NSU) then
    exit(SharedApplication.openUrl(NSU))
  else
  begin
    if DisplayError then
      ShowMessage('Error: Opening "' + URL + '" not supported.');
    exit(false);
  end;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
begin
  ShellExecute(0, 'OPEN', PChar(URL), '', '', SW_SHOWNORMAL);
end;
{$ELSE}
{$IFDEF MACOS}
var
  MacURL: NSURL;
  Workspace : NSWorkspace;
begin
    MacURL := TNSURL.Create;
    MacURL.initWithString(StrToNSSTR(URL));

    Workspace := TNSWorkspace.Create;
    Workspace.openURL(MacURL);
end;
{$ELSE}
begin
  raise Exception.Create('Not supported!');
end;
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
{$ENDIF IOS}
{$ENDIF ANDROID}

function GetFileVersionStr: string;
{$IFDEF ANDROID}
begin

end;
{$ELSE}
{$IFDEF IOS}
begin

end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi   //release
     {LongRec(FixedPtr.dwFileVersionLS).Lo}]) //build
end;
{$ELSE}
{$IFDEF MACOS}
var
  CFStr: CFStringRef;
  Range: CFRange;
begin
  CFStr := CFBundleGetValueForInfoDictionaryKey(
    CFBundleGetMainBundle, kCFBundleVersionKey);
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
{$ENDIF IOS}
{$ENDIF ANDROID}

function GetMethodsList(AUnitName: string): TStringDynArray;
{$IFDEF ANDROID}
begin

end;
{$ELSE}
{$IFDEF IOS}
begin

end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  F: TextFile;
  S: string;
begin
  Result := nil;
  if not TFile.Exists(AUnitName) then
    Exit;

  AssignFile(F, AUnitName);
  Reset(F);
  try
  // FIrst - findout implementation section.
    while not EOF(F) do
    begin
      ReadLn(F, S);
      if System.StrUtils.ContainsText(S, 'implementation') then
        Break;
    end;
  // Then - enum functions
    if not EOF(F) then
    begin
      while not EOF(F) do
      begin
        ReadLn(F, S);
        if System.StrUtils.ContainsText(S, 'procedure') or
          System.StrUtils.ContainsText(S, 'function') then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := S;
        end;
      end;
    end;
  finally
    CloseFile(F);
  end;
end;
{$ELSE}
{$IFDEF MACOS}
begin

end;
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
{$ENDIF IOS}
{$ENDIF ANDROID}

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
function CFToDelphiString(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  if Range.length = 0 then Exit;
  CFStringGetCharacters(CFStr, Range, PWideChar(Result));
end;

function NSToDelphiString(const NSStr: NSString): string;
begin
  Result := CFToDelphiString((NSStr as ILocalObject).GetObjectID);
end;
{$ENDIF MACOS}
end.
