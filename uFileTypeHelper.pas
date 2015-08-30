unit uFileTypeHelper;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, FMX.Dialogs
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  , Macapi.AppKit, Macapi.Foundation, Macapi.ObjectiveC
  {$ENDIF MACOS}
  , uConsts, uUtils;

const
  cDOSMagic = $5A4D;      // magic number for a DOS executable
  cNEMagic = $454E;       // magic number for a NE executable (Win 16)
  cPEMagic = $4550;       // magic nunber for a PE executable (Win 32)
  cLEMagic = $454C;       // magic number for a Virtual Device Driver

type
  TFileType = (ftUnknown, ftBinary, ftArchive, ftImage, ftScript);

  TExtentionData = record
    FileType: TFileType;
    FileExt: string;
  end;

const
  CFileTypeArray: array[0..46] of TExtentionData =
  (
    //.jpeg;.jpg;.jpe;.tif;.tiff;.png;.bmp;.ico;.gif
    (FileType: ftImage; FileExt: '.jpeg'), (FileType: ftImage; FileExt: '.jpg'),
    (FileType: ftImage; FileExt: '.jpe'),  (FileType: ftImage; FileExt: '.tif'),
    (FileType: ftImage; FileExt: '.tiff'), (FileType: ftImage; FileExt: '.png'),
    (FileType: ftImage; FileExt: '.bmp'),  (FileType: ftImage; FileExt: '.ico'),
    (FileType: ftImage; FileExt: '.gif'),
    //.exe;.bin;.dll;*.dcu;*.res;*.bpl;*.dcl;*.dmg;*.pkg
    (FileType: ftBinary; FileExt: '.exe'), (FileType: ftBinary; FileExt: '.bin'),
    (FileType: ftBinary; FileExt: '.dll'), (FileType: ftBinary; FileExt: '.dcu'),
    (FileType: ftBinary; FileExt: '.res'), (FileType: ftBinary; FileExt: '.bpl'),
    (FileType: ftBinary; FileExt: '.dcl'), (FileType: ftBinary; FileExt: '.dmg'),
    (FileType: ftBinary; FileExt: '.pkg'),
    //*.zip;*.apk;*.ipa
    (FileTYpe: ftArchive; FileExt: '.zip'), (FileType: ftArchive; FileExt: '.apk'),
    (FileType: ftArchive; FileExt: '.ipa'),
    //pas;dpr;dpk;dfm;fmx;inc;lpr;lfm;spr;
    (FileType: ftScript; FileExt: '.pas'), (FileType: ftScript; FileExt: '.dpr'),
    (FileType: ftScript; FileExt: '.dpk'), (FileType: ftScript; FileExt: '.dfm'),
    (FileType: ftScript; FileExt: '.fmx'), (FileType: ftScript; FileExt: '.inc'),
    (FileType: ftScript; FileExt: '.lpr'), (FileType: ftScript; FileExt: '.spr'),
    (FileType: ftScript; FileExt: '.cpp'), (FileType: ftScript; FileExt: '.h'),
    (FileType: ftScript; FileExt: '.bas'), (FileType: ftScript; FileExt: '.cs'),
    (FileType: ftScript; FileExt: '.js'),  (FileType: ftScript; FileExt: '.java'),
    (FileType: ftScript; FileExt: '.css'), (FileType: ftScript; FileExt: '.xml'),
    //dproj;deployproj;lpi;sproj;dsk;sfm;stat;todo;vlb
    (FileType: ftScript; FileExt: '.dproj'),(FileType: ftScript; FileExt: '.deployproj'),
    (FileType: ftScript; FileExt: '.lpi'),  (FileType: ftScript; FileExt: '.sproj'),
    (FileType: ftScript; FileExt: '.dsk'),  (FileType: ftScript; FileExt: '.sfm'),
    (FileType: ftScript; FileExt: '.stat'), (FileType: ftScript; FileExt: '.todo'),
    (FileType: ftScript; FileExt: '.vlb'),
    //sql;
    (FileType: ftScript; FileExt: '.sql')
  );

type
  TFileTypeHelper = record helper for TFileType
  public
    function FileType(const AFileName: string): TFileType;
    function TypeString: string; inline;
  end;



function GetFileCRLF(const AFileName: string): string;
function GetFileEncodingStr(const AFileName: string): string; overload;
function GetFileEncodingStr(const AFileName: string; var CRLF: string): string; overload;
function GetFileLineEndStr(const AFileName: string): string;
{$IFDEF MSWINDOWS}
function GetFileTypeWin(const AFileName: string): TFileType;
{$ENDIF MSWINDOWS}
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
function GetFileTypeMac(const AFileName: string): TFileType;
{$ENDIF MACOS}

function GetFileType(const AFileName: string): TFileType;

implementation

function GetFileCRLF(const AFileName: string): string;
var
  LStream: TFileStream;
  B: Byte;
begin
  Result := '';
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LStream.Seek(0, TSeekOrigin.soBeginning);
    while LStream.Read(B, 1) = 1 do
    begin
      if B = $A then
      begin
        Result := 'LF';
        Break;
      end;
      if B = $D then
      begin
        if (LStream.Read(B, 1) = 1) and (B = $A) then
          Result := 'CRLF'
        else
          Result := 'CR';
        Break;
      end;
    end;
  finally
    LStream.Free;
  end;
end;

function GetFileEncodingStr(const AFileName: string): string;
var
  LBuffer: TBytes;
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(LBuffer, 3);
    LStream.Seek(0, TSeekOrigin.soBeginning);
    LStream.ReadBuffer(Pointer(LBuffer)^, 3);
    case LBuffer[0] of
      $EF: begin
        if (LBuffer[1] = $BB) and (LBuffer[2] = $BF) then
          Result := 'UTF-8';
      end;
      $FE: begin
        if (LBuffer[1] = $FF) then
          Result := 'UTF-16 BE';
      end;
      $FF: begin
        if LBuffer[1] = $FE then
          Result := 'UTF-16 LE';
      end;
        else
          Result := 'default';
    end;
  finally
    LStream.Free;
  end;
end;

function GetFileEncodingStr(const AFileName: string; var CRLF: string): string;
var
  LBuffer: TBytes;
  LStream: TFileStream;
  B: byte;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(LBuffer, 3);
    LStream.Seek(0, TSeekOrigin.soBeginning);
    LStream.ReadBuffer(Pointer(LBuffer)^, 3);
    case LBuffer[0] of
      $EF: begin
        if (LBuffer[1] = $BB) and (LBuffer[2] = $BF) then
          Result := 'UTF-8';
      end;
      $FE: begin
        if (LBuffer[1] = $FF) then
          Result := 'UTF-16 BE';
      end;
      $FF: begin
        if LBuffer[1] = $FE then
          Result := 'UTF-16 LE';
      end;
        else
          Result := 'default';
    end;
    LStream.Seek(0, TSeekOrigin.soBeginning);
    CRLF := '';
    while LStream.Read(B, 1) = 1 do
    begin
      if B = $A then
      begin
        CRLF := 'LF';
        Break;
      end;
      if B = $D then
      begin
        if (LStream.Read(B, 1) = 1) and (B = $A) then
          CRLF := 'CRLF'
        else
          CRLF := 'CR';
        Break;
      end;
    end;
  finally
    LStream.Free;
  end;
end;

function GetFileLineEndStr(const AFileName: string): string;
var
  LBuffer: TBytes;
  LStream: TFileStream;
  B: byte;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(LBuffer, 3);
    LStream.Seek(0, TSeekOrigin.soBeginning);
    Result := '';
    while LStream.Read(B, 1) = 1 do
    begin
      if B = $A then
      begin
        Result := 'LF';
        Break;
      end;
      if B = $D then
      begin
        if (LStream.Read(B, 1) = 1) and (B = $A) then
          Result := 'CRLF'
        else
          Result := 'CR';
        Break;
      end;
    end;
  finally
    LStream.Free;
  end;
end;


{$IFDEF MSWINDOWS}
function GetFileTypeWin(const AFileName: string): TFileType;
var
  LHeader: TImageDosHeader;
  LStream: TFileStream;
  S: string;
begin
  Result := ftUnknown;
  LStream := TFileStream.Create(AFileName, fmOpenread);
  try
    if LStream.Size > SizeOf(LHeader) then
    begin
      LStream.ReadBuffer(LHeader, SizeOf(LHeader));
      case LHeader.e_magic of
        cDOSMagic: Result := ftBinary;
        cNEMagic: Result := ftBinary;
        cPEMagic: Result := ftBinary;
        cLEMagic: Result := ftBinary;
      end;
    end;
  finally
    LStream.Free;
  end;
  if Result = ftBinary then
    Exit;

  if TPath.HasExtension(AFileName) then
  begin
    S := TPath.GetExtension(AFileName);
  end else
    Result := ftUnknown;
end;
{$ENDIF}

{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
function GetFileTypeMac(const AFileName: string): TFileType;
var
  FileMgr : NSFileManager;
  Dict: NSDictionary;
begin
  FileMgr := TNSFileManager.Create;
  Dict := FileMgr.fileAttributesAtPath(NSSTR(AFileName), False);
  Result := ftUnknown;
end;
{$ENDIF MACOS}

function GetFileType(const AFileName: string): TFileType;
begin
  {$IFDEF MSWINDOWS}
  Result := GetFileTypeWin(AFileName);
  {$ENDIF MSWINDOWS}
  {$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
  Result := GetFileTypeMac(AFileName);
  {$ENDIF MACOS}
end;

{ TFileTypeHelper }

function TFileTypeHelper.FileType(const AFileName: string): TFileType;
var
  I: integer;
  S: string;
begin
  Result := ftUnknown;
  if TPath.HasExtension(AFileName) then
  begin
    S := TPath.GetExtension(AFileName);
    for I := Low(CFileTypeArray) to High(CFileTypeArray) do
      if SameText(S, CFileTypeArray[I].FileExt) then
      begin
        Result := CFileTypeArray[I].FileType;
        Break;
      end;
  end;
end;

function TFileTypeHelper.TypeString: string;
begin
  case Self of
    ftUnknown: Result := 'Unknown';
    ftBinary: Result := 'Binary';
    ftArchive: Result := 'Archive';
    ftImage: Result := 'Image';
    ftScript: Result := 'Script';
  end;
end;

end.
