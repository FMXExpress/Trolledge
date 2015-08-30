(* SvHelpers.pas
* Created: 2012-01-27 14:36:02
* Copyright (c) 2012, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net or linas@vikarina.lt
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit SvHelpers;

interface

uses
  Classes,
  Graphics,
  DB;

type
  TFieldHelper = class helper for TField
  public
    function ValueOrDef<T>(const ADefault: T): T;
  end;

  TPictureHelper = class helper for TPicture
  public
    procedure LoadFromBlobField(Field: TBlobField);
    /// <summary>
    /// Detects image format from stream and loads it correctly
    ///  Supported formats: jpeg, png, gif, tiff, bmp, ico, wmf
    /// </summary>
    /// <param name="AStream">Stream containing image</param>
    procedure LoadFromStreamSmart(AStream: TStream);
    procedure LoadFromFileSmart(const AFilename: string);
    procedure SaveToStream(AStream: TStream);
  end;


implementation

uses
  Consts,
  GIFImg,
  JPEG,
  PngImage,
  SysUtils,
  Rtti;

{ TPictureHelper }

const
  MinGraphicSize = 44; //we may test up to & including the 11th longword

function FindGraphicClass(const Buffer; const BufferSize: Int64;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  LongWords: array[Byte] of LongWord absolute Buffer;
  Words: array[Byte] of Word absolute Buffer;
begin
  GraphicClass := nil;
  Result := False;
  if BufferSize < MinGraphicSize then Exit;
  case Words[0] of
    $4D42: GraphicClass := TBitmap;
    $D8FF: GraphicClass := TJPEGImage;
    $4949: if Words[1] = $002A then GraphicClass := TWicImage; //i.e., TIFF
    $4D4D: if Words[1] = $2A00 then GraphicClass := TWicImage; //i.e., TIFF
  else
    if Int64(Buffer) = $A1A0A0D474E5089 then
      GraphicClass := TPNGImage
    else if LongWords[0] = $9AC6CDD7 then
      GraphicClass := TMetafile
    else if (LongWords[0] = 1) and (LongWords[10] = $464D4520) then
      GraphicClass := TMetafile
    else if StrLComp(PAnsiChar(@Buffer), 'GIF', 3) = 0 then
      GraphicClass := TGIFImage
    else if Words[1] = 1 then
      GraphicClass := TIcon;
  end;
  Result := (GraphicClass <> nil);
end;

function FindGraphicClass(Stream: TStream;
  out GraphicClass: TGraphicClass): Boolean; overload;
var
  Buffer: PByte;
  CurPos: Int64;
  BytesRead: Integer;
begin
  if Stream is TCustomMemoryStream then
  begin
    Buffer := TCustomMemoryStream(Stream).Memory;
    CurPos := Stream.Position;
    Inc(Buffer, CurPos);
    Result := FindGraphicClass(Buffer^, Stream.Size - CurPos, GraphicClass);
    Exit;
  end;
  GetMem(Buffer, MinGraphicSize);
  try
    BytesRead := Stream.Read(Buffer^, MinGraphicSize);
    Stream.Seek(-BytesRead, soCurrent);
    Result := FindGraphicClass(Buffer^, BytesRead, GraphicClass);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TPictureHelper.LoadFromBlobField(Field: TBlobField);
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Graphic := nil;
  Stream := TMemoryStream.Create;
  try
    Field.SaveToStream(Stream);
    if Stream.Size = 0 then
    begin
      Self.Assign(nil);
      Exit;
    end;
    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      raise EInvalidGraphic.Create(SInvalidImage);
    Graphic := GraphicClass.Create;
    Stream.Position := 0;
    Graphic.LoadFromStream(Stream);
    Self.Assign(Graphic);
  finally
    Stream.Free;
    Graphic.Free;
  end;
end;

procedure TPictureHelper.LoadFromFileSmart(const AFilename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    fs.Position := 0;
    LoadFromStreamSmart(fs);
  finally
    fs.Free;
  end;
end;

procedure TPictureHelper.LoadFromStreamSmart(AStream: TStream);
var
  Graphic: TGraphic;
  GraphicClass: TGraphicClass;
  Stream: TMemoryStream;
begin
  Graphic := nil;
  Stream := TMemoryStream.Create;
  try
    Stream.CopyFrom(AStream, AStream.Size);
    if Stream.Size = 0 then
    begin
      Self.Assign(nil);
      Exit;
    end;
    if not FindGraphicClass(Stream.Memory^, Stream.Size, GraphicClass) then
      raise EInvalidGraphic.Create(SInvalidImage);
    Graphic := GraphicClass.Create;
    Stream.Position := 0;
    Graphic.LoadFromStream(Stream);
    Self.Assign(Graphic);
  finally
    Stream.Free;
    Graphic.Free;
  end;
end;

procedure TPictureHelper.SaveToStream(AStream: TStream);
begin
  if Graphic <> nil then
    Graphic.SaveToStream(AStream);
end;

{ TFieldHelper }

function TFieldHelper.ValueOrDef<T>(const ADefault: T): T;
var
  v: TValue;
begin
  if not IsNull then
  begin
    v := TValue.FromVariant(Value);
    Result := v.AsType<T>;
  end
  else
  begin
    Result := ADefault;
  end;
end;

end.
