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
  , ComObj, ShlObj, ActiveX
{$ELSE}
{$IFDEF MACOS}
  Macapi.Foundation, Macapi.AppKit, Macapi.Helpers, Macapi.ObjectiveC,
  Macapi.CocoaTypes, Macapi.CoreFoundation
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
{$ENDIF IOS}
{$ENDIF ANDROID}
  ;
function LevenshteinDistanceCheck(ASubStr, AStr : String;
    ADiffCount : integer = 2) : Boolean; //inline;
function SelectDirectory(const ATitle: string; var ADir: string; FormHandle: THandle = 0): boolean;
function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
function GetFileVersionStr: string;
function GetMethodsList(AUnitName: string): TStringDynArray;
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
function NSToDelphiString(const NSStr: NSString): string;
{$ENDIF MACOS}

implementation

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

function LevenshteinDistanceCheck(ASubStr, AStr : String; ADiffCount : integer) : Boolean;
var
    Length1, Length2      : Integer;
    WorkMatrix            : array of array of Integer;
    I, J                  : Integer;
    Cost, Distance        : Integer;
    Val1, Val2, Val3      : Integer;
begin
    ASubStr := ASubStr.ToUpper;
    AStr := AStr.ToUpper;
    Length1 := Length (ASubStr);
    Length2 := Length (AStr);
    if Length1 < ADiffCount + 1 then
    begin
        Result := SameText(ASubStr, copy(AStr, 1, Length1));
        Exit;
    end;
    SetLength (WorkMatrix, Length1+1, Length2+1);
    for I := 0 to Length1 do
      WorkMatrix [I, 0] := I;
    for J := 0 to Length2 do
      WorkMatrix [0, J] := J;
    for I := 1 to Length1 do
      for J := 1 to Length2 do
        begin
        if (ASubStr [I] = AStr [J]) then
          Cost := 0
        else
          Cost := 1;
        Val1 := WorkMatrix [I-1, J] + 1;
        Val2 := WorkMatrix [I, J-1] + 1;
        Val3 := WorkMatrix[I-1, J-1] + Cost;
        if (Val1 < Val2) then
          if (Val1 < Val3) then
            WorkMatrix [I, J] := Val1
          else
            WorkMatrix [I, J] := Val3
        else
          if (Val2 < Val3) then
            WorkMatrix [I, J] := Val2
          else
            WorkMatrix [I, J] := Val3;
        end;
    Distance := WorkMatrix [Length1, Length2];
    Result := (Distance - Abs(Length1 - Length2)) < (ADiffCount + 1);
end;

function SelectDirectory(const ATitle: string;
  var ADir: string; FormHandle: THandle): boolean;
{$IFDEF MSWINDOWS}
var
  hr: HRESULT;
  IDList: PItemIDList;
  RootIDList: PItemIDList;
  Malloc: IMalloc;
  lpBuf: LPTSTR;
  BI: TBrowseInfo;
  sCaption: string;
begin
  Result := False;
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
