unit uMemoHexView;

// *****************************************************************************
//      HEX VIEW
// *****************************************************************************
//
//
//
//
// Written by Ivan Kulyba 06/26/2015
// *****************************************************************************
interface

uses
  System.SysUtils, System.Classes, System.Character, System.Generics.Collections,
  System.IOUtils, FMX.Dialogs, FMX.Forms
  {$IFDEF MSWINDOWS}
  ,Winapi.Windows
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  ,Macapi.AppKit, Macapi.Foundation, Macapi.ObjectiveC
  {$ENDIF MACOS}
  ;

const
  MaxPosition     = 58;
  MaxLineLen      = 76;
  AddrPos         = 1;
  AddrLen         = 10;
  AddrIncrement   = 16;
  DataPos         = 11;
  DataLen         = 48;
  CharsPos        = 59;
  CharsLen        = 16;

type
  TOnComposeBegin = procedure(Sender: TObject) of Object;
  TOnComposing = procedure(Sender: TObject; BytesCount, Percent: integer) of Object;
  TOnComposeEnd = procedure(Sender: TObject) of Object;
  TOnReadBegin = procedure(Sender: TObject) of Object;
  TOnReading = procedure(Sender: TObject; BytesCount, Percent: integer) of Object;
  TOnReadEnd = procedure(Sender: TObject) of Object;

  //TDataArray = array[1..MaxLineLen] of byte;

  THexViewer = class
  private
    FFileName: string;
    FBaseAddress: PByte;
    FFileHandle: THandle;
    FMapHandle: THandle;
    FFileSize: NativeInt;
    FLinesCount: NativeInt;
    FLinesList: TStringList;
    FCurPosition: NativeInt;
    FEOF: boolean;
    FOnComposeBegin: TOnComposeBegin;
    FOnComposing: TOnComposing;
    FOnComposeEnd: TOnComposeEnd;
    FOnReadBegin: TOnReadBegin;
    FOnReading: TOnReading;
    FOnReadEnd: TOnReadEnd;
    FBusy: boolean;
    FBuffer: string;
    procedure DoComposeBegin;
    procedure DoComposing(BytesCount, Percent: integer);
    procedure DoComposeEnd;
    procedure DoReadBegin;
    procedure DoReading(BytesCount, Percent: integer);
    procedure DoReadEnd;
    function GetLinesCount: NativeInt;
  public
    constructor Create; overload;
    constructor Create(AFileName: string); overload;
    destructor Destroy; override;
    {$IFDEF MSWINDOWS}
    function OpenMapping: boolean;
    procedure CloseMapping; overload;
    {$ENDIF MSWINDOWS}
    class function GetMapFileSize(AFileHandle: THandle): UInt64; overload;
    class function GetMapFileSize(AFileName: string): UInt64; overload;
    procedure First;
    function ComposeHexData: boolean;
    function ReadStr: string;
    property BaseAddr: PByte read FBaseAddress;
    property IsBusy: boolean read FBusy;
    property EOF: boolean read FEOF;
    property FileName: string read FFileName write FFileName;
    property LinesCount: NativeInt read GetLinesCount;
    property OnComposeBegin: TOnComposeBegin read FOnComposeBegin write FOnComposeBegin;
    property OnComposing: TOnComposing read FOnComposing write FOnComposing;
    property OnComposeEnd: TOnComposeEnd read FOnComposeEnd write FOnComposeEnd;
    property OnReadBegin: TOnReadBegin read FOnReadBegin write FOnReadBegin;
    property OnReading: TOnReading read FOnReading write FOnReading;
    property OnReadEnd: TOnReadEnd read FOnReadEnd write FOnReadEnd;
    property LinesList: TStringList read FLinesList;
    property Buffer: string read FBuffer;
  end;


implementation

{ THexViewer }

{$IFDEF MSWINDOWS}
procedure THexViewer.CloseMapping;
begin
  if FBaseAddress <> nil then
  begin
    UnMapViewOfFile(FBaseAddress);
    FBaseAddress := nil;
  end;
  if (FMapHandle <> 0) or (FMapHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(FMapHandle);
    FMapHandle := 0;
  end;
  if (FFileHandle <> 0) or (FFileHandle <> INVALID_HANDLE_VALUE) then
  begin
    CloseHandle(FFileHandle);
    FFileHandle := 0;
  end;
  FFileSize := 0;
end;
{$ENDIF MSWINDOWS}

constructor THexViewer.Create(AFileName: string);
begin
  FFileName := AFileName;
  FBaseAddress := nil;
  FFileHandle := 0;
  FLinesCount := 0;
  FMapHandle := 0;
  FFileSize := 0;
  FLinesList := TStringList.Create;
end;

destructor THexViewer.Destroy;
begin
  FLinesList.Free;
  {$IFDEF MSWINDOWS}
  CloseMapping;
  {$ENDIF MSWINDOWS}
  inherited;
end;

procedure THexViewer.DoComposeBegin;
begin
  if Assigned(FOnComposeBegin) then
    FOnComposeBegin(Self);
end;

procedure THexViewer.DoComposeEnd;
begin
  if Assigned(FOnComposeEnd) then
    FOnComposeEnd(Self);
end;

procedure THexViewer.DoComposing(BytesCount, Percent: integer);
begin
  if Assigned(FOnComposing) then
    FOnComposing(Self, BytesCount, Percent);
end;

procedure THexViewer.DoReadBegin;
begin
  if Assigned(FOnReadBegin) then
    FOnReadBegin(Self);
end;

procedure THexViewer.DoReadEnd;
begin
  if Assigned(FOnReadEnd) then
    FOnReadEnd(Self);
end;

procedure THexViewer.DoReading(BytesCount, Percent: integer);
begin
  if Assigned(FOnReading) then
    FOnReading(Self, BytesCount, Percent);
end;

procedure THexViewer.First;
begin
  FEOF := False;
  FCurPosition := 0;
end;

function THexViewer.ComposeHexData: boolean;
var
  PosIncrement: NativeInt;
  CharIncrement: integer;
  I, N: NativeInt;
  LProgress, LPercent: Integer;

  {$IFDEF MSWINDOWS}
  S: AnsiString;
  Arr: AnsiString;
  Addr: NativeInt;
  SAddr: AnsiString;
  SChars: AnsiString;
  {$ENDIF MSWINDOWS}

  {$IFDEF MACOS}
  SA: string;
  FS: TFileStream;
  BytesReaded: integer;
  Buf: TBytes;
  {$ENDIF MACOS}
begin
  Result := False;
  FEOF := True;
  FLinesList.Clear;
  DoComposeBegin;
  {$IFDEF MSWINDOWS}
  if OpenMapping then
  try
    FBusy := True;
    PosIncrement := DataPos;
    CharIncrement := 1;
    Addr := 0;
    FCurPosition := 0;
    SetLength(SChars, CharsLen);
    SetLength(Arr, MaxLineLen);
    Arr := AnsiString(StringOfChar(' ', MaxlineLen));
    LPercent := 0;
    for I := 0 to FFileSize - 1 do
    begin
      // IntToHex is a very slowly function...
      S := AnsiString(IntToHex(FBaseAddress[I], 2));
      Arr[PosIncrement] := S[1];
      Inc(PosIncrement);
      Arr[PosIncrement] := S[2];
      Inc(PosIncrement);
      if System.Character.TCharacter.IsLetterOrDigit(FBaseAddress[I]) then
        SChars[CharIncrement] := AnsiChar(FBaseAddress[I])
      else
        SChars[CharIncrement] := '.';
      Inc(CharIncrement);
      if PosIncrement > (MaxPosition) then
      begin
        PosIncrement := DataPos;
      // Write an address
        SAddr := AnsiString(IntToHex(Addr, 8) + ': ');
        Move(SAddr[1], Arr[1], AddrLen);
        Inc(Addr, AddrIncrement);
      // Write characters
        Move(SChars[1], Arr[MaxPosition + 1], CharsLen);
        CharIncrement := 1;
      // Write CR/LF
        Arr[MaxLineLen - 1] := ' ';//$D;
        Arr[MaxLineLen] := ' ';//$A;
        Inc(FLinesCount);
      // Write bytes to stream
        FLinesList.Add(String(Arr));
        Arr := AnsiString(StringOfChar(' ', MaxlineLen));
      end else
      begin
        Arr[PosIncrement] := ' ';//Byte(' ');
        Inc(PosIncrement);
      end;
      LProgress := Trunc((I / FFileSize) * 100);
      if LPercent <> LProgress then
      begin
        LPercent := LProgress;
        DoComposing(I, LPercent);
      end;
    end;
    SAddr := AnsiString(IntToHex(Addr, 8) + ': ');
    Move(SAddr[1], Arr[1], AddrLen);
    Move(SChars[1], Arr[MaxPosition + 1], CharIncrement);
    FLinesList.Add(String(Arr));
    FEOF := False;
    Result := True;
  finally
    CloseMapping;
    FBusy := False;
  end;
  {$ENDIF MSWINDOWS}

  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  FBusy := True;
  FBuffer := '';
  SetLength(SA, 32);
  FS := TFileStream.Create(FFileName, fmOpenRead);
  try
    SetLength(Buf, 16);
    I := 0;
    BytesReaded := FS.Read(Buf, 16);
    while BytesReaded = 16 do
    begin
      BinToHex(Buf, PChar(@SA[1]), 16);
      FLinesList.Add(SA);
      Inc(I, 16);
      LProgress := Trunc((I / FS.Size) * 100);
      if LPercent <> LProgress then
      begin
        LPercent := LProgress;
        DoComposing(I, LPercent);
      end;
      BytesReaded := FS.Read(Buf, 16);
    end;
    SetLength(SA, BytesReaded * 2);
    BinToHex(Buf, PChar(@SA[1]), BytesReaded);
    FLinesList.Add(SA);

    Result := True;
  finally
    FS.Free;
    SetLength(Buf, 0);
    FBusy := False;
    FEOF := False;
  end;
  {$ENDIF MACOS}
  DoComposeEnd;
end;

constructor THexViewer.Create;
begin
  Create('');
end;

class function THexViewer.GetMapFileSize(AFileHandle: THandle): UInt64;
var
  FileSizeLow: NativeInt;
  FileSizeHigh: NativeInt;
begin
  {$IFDEF MSWINDOWS}
  FileSizeLow := GetFileSize(AFileHandle, @FileSizeHigh);
  Result := FileSizeHigh;
  Result := (Result shl 32) + FileSizeLow;
  {$ENDIF MSWINDOWS}
end;

function THexViewer.GetLinesCount: NativeInt;
begin
  Result := FLinesList.Count;
end;

class function THexViewer.GetMapFileSize(AFileName: string): UInt64;
var
  {$IFDEF MSWINDOWS}
  FHandle: THandle;
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  FileMgr : NSFileManager;
  Dict: NSDictionary;
  {$ENDIF MACOS}
  {$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Shim: Integer;
  {$ENDIF}
begin
  Result := 0;
  {$IFDEF MSWINDOWS}
  if AFileName = '' then
    raise Exception.Create('Filename is not assigned!');

  FHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := GetMapFileSize(FHandle);
    finally
      CloseHandle(FHandle);
    end;
  end else
    raise Exception.Create('** Could not open file ' + AFileName +
      sLineBreak + SysErrorMessage(GetLastError));
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  FileMgr := TNSFileManager.Create;
  Dict := FileMgr.fileAttributesAtPath(NSSTR(AFileName), False);
  Result := Dict.fileSize;
  {$ENDIF MACOS}
end;

{$IFDEF MSWINDOWS}
function THexViewer.OpenMapping: boolean;
begin
  Result := False;
  if FFileName = '' then
    raise Exception.Create('Filename is not assigned!');

  try
    FFileHandle := CreateFile(PChar(FFileName), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if FFileHandle = INVALID_HANDLE_VALUE then
      raise Exception.Create('** Could not open file ' + FFileName +
        sLineBreak + SysErrorMessage(GetLastError));

    FFileSize := GetMapFileSize(FFileHandle);
    if FFileSize = 0 then
      Exit;

    FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY,
      0, FFileSize, nil);

    if FMapHandle = INVALID_HANDLE_VALUE then
      raise Exception.Create('*** Could not create file mapping ' + FFileName +
        sLineBreak + SysErrorMessage(GetLastError));

    FBaseAddress := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, FFileSize);

    if FBaseAddress = nil then
      raise Exception.Create('*** Could not get access to memory mapped file  ' +
        FFileName + sLineBreak + SysErrorMessage(GetLastError));

    Result := True;
  except
    On E: Exception do
      ShowMessage(E.Message);
  end;
end;
{$ENDIF MSWINDOWS}

function THexViewer.ReadStr: string;
var
  LProgress, LPercent: integer;
begin
  Result := '';
  LPercent := 0;
  if not FEOF then
  begin
    Result := FLinesList[FCurPosition];
    Inc(FCurPosition);
    LProgress := Trunc((FCurPosition / FLinesList.Count) * 100);
    if LProgress <> LPercent then
    begin
      LPercent := LProgress;
      DoReading(0, LPercent);
    end;
    FEOF := FCurPosition = FLinesList.Count;
  end;
end;

end.
