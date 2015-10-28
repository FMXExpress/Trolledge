unit uMonitoring;

interface
uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  System.IOUtils
  {$IFDEF MSWINDOWS}
    ,Winapi.Windows
  {$ENDIF}
  ,FMX.Types
  ;
type
 // The structure change information in the file system (passed in a callback procedure)

  PInfoCallback = ^TInfoCallback;
  TInfoCallback = record  
    FAction      : Integer; // type changing (const FILE_ACTION_XXX)
    FDrive       : string;  // disk where been a change
    FOldFileName : string;  // filename before rename
    FNewFileName : string;  // filename after rename
  end;

  // callback procedure
  TWatchFileSystemCallback = procedure (pInfo: TInfoCallback) of object;

  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset : DWORD;
    Action          : DWORD;
    FileNameLength  : DWORD;
    FileName        : array[0..0] of WideChar;
  end;

  WFSError = class(Exception);
{$IFDEF MSWINDOWS}
  TWFS = class(TThread)
  private
    FName           : string;
    FFilter         : Cardinal;
    FSubTree        : boolean;
    FInfoCallback   : TWatchFileSystemCallback;
    FWatchHandle    : THandle;
    FWatchBuf       : array[0..4096] of Byte;
    FOverLapp       : TOverlapped;
    FPOverLapp      : POverlapped;
    FBytesWritte    : DWORD;
    FCompletionPort : THandle;
    FNumBytes       : Cardinal;
    FOldFileName    : string;
    function CreateDirHandle(aDir: string): THandle;
    procedure WatchEvent;
    procedure HandleEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallback: TWatchFileSystemCallback);
    destructor Destroy; override;
  end;
{$ENDIF}
  TFileChange = procedure (const AFName : string) of object;
  TMonitoring = class
  private
{$IFDEF MSWINDOWS}
    FWFS : TWFS;
{$ENDIF}
    FTimer : TTimer;
    FDir : string;
    FFiles : TDictionary<string, Boolean>;
    FIgnoreFiles : TList<string>;
    FOnFileChanges : TFileChange;
    procedure DoOnFileChanges(const AFName : string);
    procedure StartTimer; inline;
    procedure OnTimer(Sender: TObject);
    procedure TrySetChanges(const AFName : string);
    procedure InitTimer;
  public
    constructor Create;
    destructor Destroy; override;
    { start monitoring
    parameters:
    pName    - folder name fro monitoring
    pFilter  - combination of the constants FILE_NOTIFY_XXX
    pSubTree - is subfolders
    pInfoCallback - pointer callback procedure}
    procedure StartWatch(pName: string; pFilter: cardinal; pSubTree: boolean;
        pInfoCallback: TWatchFileSystemCallback); overload;
    procedure StartWatch(pName: string); overload;
    // Остановка мониторинга
    procedure StopWatch; overload;
    procedure StopWatch(pName: string); overload;
    procedure PauseWatch(pName : string);
    procedure InfoCallback(pInfo: TInfoCallback);
  public
    property OnFileChanges : TFileChange read FOnFileChanges write FOnFileChanges;
  end;
const
    FIX_MSEC : integer = 500;
implementation

procedure TMonitoring.StartWatch(pName: string; pFilter: cardinal; pSubTree: boolean;
    pInfoCallback: TWatchFileSystemCallback);
var
    lDir : string;
begin
    if pName.IsEmpty or FFiles.ContainsKey(pName) then
        exit;
    lDir := TPath.GetDirectoryName(pName);
    if not FDir.Equals(lDir) then
    begin
        FDir := lDir;
        StopWatch;
{$IFDEF MSWINDOWS}
        FWFS := TWFS.Create(FDir, pFilter, pSubTree, pInfoCallback);
{$ENDIF}
    end;
    FFiles.Add(pName, False);
end;

constructor TMonitoring.Create;
begin
    FDir := string.Empty;
    FFiles := TDictionary<string, Boolean>.Create;
    FIgnoreFiles := TList<string>.Create;
    InitTimer;
{$IFDEF MSWINDOWS}
    FWFS := nil;
{$ENDIF}
end;

destructor TMonitoring.Destroy;
begin
    StopWatch;
    FreeAndNil(FTimer);
    FreeAndNil(FFiles);
    FreeAndNil(FIgnoreFiles);
    inherited;
end;

procedure TMonitoring.DoOnFileChanges(const AFName: string);
begin
    if Assigned(OnFileChanges) then
        OnFileChanges(AFName);
end;

procedure TMonitoring.StopWatch(pName: string);
begin
    if not pName.IsEmpty then
        FFiles.Remove(pName);
end;

procedure TMonitoring.PauseWatch(pName: string);
begin
    if not FIgnoreFiles.Contains(pName) then
        FIgnoreFiles.Add(pName);
end;

procedure TMonitoring.TrySetChanges(const AFName: string);
var
    lNeedUpdate : Boolean;
    lNow : TDateTime;
begin
    if not FFiles.ContainsKey(AFName) then
        exit;
    StartTimer;
    FFiles.Items[AFName] := True;
end;

{$REGION 'WINDOWS'}
{$IFDEF MSWINDOWS}
procedure TMonitoring.InfoCallback(pInfo: TInfoCallback);
var
    lFName : string;
begin
    if pInfo.FAction <> FILE_ACTION_MODIFIED then
        exit;
    lFName := pInfo.FDrive + pInfo.FNewFileName;
    TrySetChanges(lFName);
end;

procedure TMonitoring.InitTimer;
begin
    FTimer := TTimer.Create(nil);
    FTimer.Enabled := False;
    FTimer.Interval := FIX_MSEC;
    FTimer.OnTimer := self.OnTimer;
end;

procedure TMonitoring.OnTimer(Sender: TObject);
var
    item : TPair<string, Boolean>;
    ignoreFile : string;
begin
    FTimer.Enabled := False;
    //remove changedflag from my saving
    for ignoreFile in FIgnoreFiles do
        if FFiles.ContainsKey(ignoreFile) then
            FFiles.Items[ignoreFile] := False;
    FIgnoreFiles.Clear;
    //call notifyevent
    for item in FFiles do
        if item.Value then
        begin
            FFiles.Items[item.Key] := False;
            DoOnFileChanges(item.Key);
        end;
end;

procedure TMonitoring.StartTimer;
begin
    FTimer.Enabled := True;
end;

procedure TMonitoring.StartWatch(pName: string);
begin
    StartWatch(pName, FILE_NOTIFY_CHANGE_LAST_WRITE, False, InfoCallback);
end;

procedure TMonitoring.StopWatch;
var
  Temp : TWFS;
begin
  if Assigned(FWFS) then
  begin
   PostQueuedCompletionStatus(FWFS.FCompletionPort, 0, 0, nil);
   Temp := FWFS;
   FWFS := nil;
   Temp.Terminate;
  end;
  FFiles.Clear;
end;

constructor TWFS.Create(pName: string; pFilter: cardinal; pSubTree: boolean; pInfoCallback: TWatchFileSystemCallback);
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FName:=IncludeTrailingBackslash(pName);
  FFilter:=pFilter;
  FSubTree:=pSubTree;
  FOldFileName:=EmptyStr;
  ZeroMemory(@FOverLapp, SizeOf(TOverLapped));
  FPOverLapp:=@FOverLapp;
  ZeroMemory(@FWatchBuf, SizeOf(FWatchBuf));
  FInfoCallback:=pInfoCallback;
  Resume
end;

destructor TWFS.Destroy;
begin
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  CloseHandle(FWatchHandle);
  FWatchHandle:=0;
  CloseHandle(FCompletionPort);
  FCompletionPort:=0;
  inherited Destroy;
end;

function TWFS.CreateDirHandle(aDir: string): THandle;
begin
    Result:=CreateFile(PChar(aDir), FILE_LIST_DIRECTORY, FILE_SHARE_READ+FILE_SHARE_DELETE+FILE_SHARE_WRITE,
                   nil,OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_OVERLAPPED, 0);
end;

procedure TWFS.Execute;
begin
  FWatchHandle:=CreateDirHandle(FName);
  WatchEvent;
end;

procedure TWFS.HandleEvent;
var
  FileNotifyInfo : PFileNotifyInformation;
  InfoCallback   : TInfoCallback;
  Offset         : Longint;
begin
  Pointer(FileNotifyInfo) := @FWatchBuf[0];
  repeat
    Offset:=FileNotifyInfo^.NextEntryOffset;
    InfoCallback.FAction:=FileNotifyInfo^.Action;
    InfoCallback.FDrive:=FName;
    SetString(InfoCallback.FNewFileName,FileNotifyInfo^.FileName,FileNotifyInfo^.FileNameLength);
    InfoCallback.FNewFileName:=Trim(InfoCallback.FNewFileName);
    case FileNotifyInfo^.Action of
      FILE_ACTION_RENAMED_OLD_NAME: FOldFileName:=Trim(WideCharToString(@(FileNotifyInfo^.FileName[0])));
      FILE_ACTION_RENAMED_NEW_NAME: InfoCallback.FOldFileName:=FOldFileName;
    end;
    FInfoCallback(InfoCallback);
    PChar(FileNotifyInfo):=PChar(FileNotifyInfo)+Offset;
  until (Offset=0) or Terminated;
end;

procedure TWFS.WatchEvent;
var
 CompletionKey: ULONG_PTR;
begin
  FCompletionPort:=CreateIoCompletionPort(FWatchHandle, 0, Longint(pointer(self)), 0);
  ZeroMemory(@FWatchBuf, SizeOf(FWatchBuf));
  if not ReadDirectoryChanges(FWatchHandle, @FWatchBuf, SizeOf(FWatchBuf), FSubTree,
    FFilter, @FBytesWritte,  @FOverLapp, nil) then
  begin
    raise WFSError.Create(SysErrorMessage(GetLastError));
    Terminate;
  end else
  begin
    while not Terminated do
    begin
      GetQueuedCompletionStatus(FCompletionPort, FNumBytes, CompletionKey, FPOverLapp, INFINITE);
      if CompletionKey<>0 then
      begin
        Synchronize(HandleEvent);
        ZeroMemory(@FWatchBuf, SizeOf(FWatchBuf));
        FBytesWritte:=0;
        ReadDirectoryChanges(FWatchHandle, @FWatchBuf, SizeOf(FWatchBuf), FSubTree, FFilter,
                             @FBytesWritte, @FOverLapp, nil);
      end else Terminate;
    end
  end
end;
{$ENDREGION}

{$ELSE}
procedure TMonitoring.InfoCallback(pInfo: TInfoCallback);
begin
end;
procedure TMonitoring.StartWatch(pName: string);
begin
end;

procedure TMonitoring.StopWatch;
begin
end;
{$ENDIF}
end.
