unit OpenViewUrl;
 
interface
 
// URLEncode is performed on the URL
// so you need to format it   protocol://path
function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;

implementation

uses
  IdURI, SysUtils, Classes, FMX.Dialogs,
{$IFDEF MSWINDOWS}
    ActiveX, ShellApi, Windows;
{$ENDIF MSWINDOWS}
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
    Macapi.AppKit, macapi.Foundation;
{$ENDIF}
{$IFDEF ANDROID}
  FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.JavaTypes
  , Androidapi.Helpers
  ;
{$ELSE}
{$IFDEF IOS}
  Macapi.Helpers, iOSapi.Foundation, FMX.Helpers.iOS;
{$ENDIF IOS}
{$ENDIF ANDROID}

function OpenURL(const URL: string; const DisplayError: Boolean = False): Boolean;
{$IFDEF ANDROID}
var
  Intent : JIntent;
begin
// There may be an issue with the geo: prefix and URLEncode.
// will need to research
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    Intent.setData(StrToJURI(URL));
  {Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW,
    TJnet_Uri.JavaClass.parse(StringToJString(TIdURI.URLEncode(URL))));  }
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
{$ENDIF ANDROID}
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
{$ENDIF IOS}
{$IFDEF MSWINDOWS}
    procedure ShellExecute(const AWnd: HWND; const AOperation, AFileName: String; const AParameters: String = ''; const ADirectory: String = ''; const AShowCmd: Integer = SW_SHOWNORMAL);
    var
      ExecInfo: TShellExecuteInfo;
      NeedUninitialize: Boolean;
    begin
      Assert(AFileName <> '');

      NeedUninitialize := SUCCEEDED(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
      try
        FillChar(ExecInfo, SizeOf(ExecInfo), 0);
        ExecInfo.cbSize := SizeOf(ExecInfo);

        ExecInfo.Wnd := AWnd;
        ExecInfo.lpVerb := Pointer(AOperation);
        ExecInfo.lpFile := PChar(AFileName);
        ExecInfo.lpParameters := Pointer(AParameters);
        ExecInfo.lpDirectory := Pointer(ADirectory);
        ExecInfo.nShow := AShowCmd;
        ExecInfo.fMask := SEE_MASK_NOASYNC { = SEE_MASK_FLAG_DDEWAIT для старых версий Delphi }
                       or SEE_MASK_FLAG_NO_UI;
        {$IFDEF UNICODE}
        ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_UNICODE;
        {$ENDIF}

        {$WARN SYMBOL_PLATFORM OFF}
        Win32Check(ShellExecuteEx(@ExecInfo));
        {$WARN SYMBOL_PLATFORM ON}
      finally
        if NeedUninitialize then
          CoUninitialize;
      end;
    end;
begin
    try
        ShellExecute(0, 'open', PChar(URL));
        exit(true);
    except
        on e: Exception do
        begin
            if DisplayError then
                ShowMessage('Error: ' + e.Message);
            exit(false);
        end;
    end;
end;
{$ENDIF MSWINDOWS}
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
var
  WS: NSWorkspace;
  AURL: NSURL;
begin
  WS:=TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
  AURL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSSTR(URL)));
  WS.openURL(AURL);
end;
{$ENDIF MACOS}

end.