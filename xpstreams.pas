{  $Id$

   OpenXP TStream Support Library
   Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$I xpdefine.inc }

unit xpstreams;

{ ---------------------------} interface { --------------------------- }

uses
  XPGlobal,
  Classes;

type

{ ------------------- Use PASCAL 'file' as stream -------------------- }

  PFile = ^file;

  TPascalFileStream = class(TStream)
  private
    FPFile: PFile;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(var AFile: file);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ------------------- Deletes file at destruction -------------------- }

  TTemporaryFileStream = class(TFileStream)
  private
    FFileName: String;
  public
    constructor Create; overload;
    constructor Create(const FileName: string; Mode: System.Word); overload;
//  constructor Create(const FileName: string; Mode: System.Word; Rights: Cardinal); overload;
    destructor Destroy; override;
  end;

{ ---------------- Streams that can own other streams ---------------- }

type TConnectableStream = class (TStream)
  private
    FOtherStream: TStream;
    FDestroyOtherStream: boolean;
    FOtherStreamStartPos: Longint;
    
  protected
    procedure Connect(AnOtherStream:TStream); virtual;

  public
    constructor Create; overload;
    destructor Destroy; override;

    property OtherStream: TStream read FOtherStream write Connect;
    property DestroyOtherStream: boolean read FDestroyOtherStream write FDestroyOtherStream;
  end;

procedure ConnectStream(var AStream:TStream;AFilter:TConnectableStream);
procedure UnConnectStream(var AStream:TStream);

{ --------------------------- CODEC streams -------------------------- }

type TCoDecStream = class(TConnectableStream)
  protected
    FPosition: Longint;
    procedure SetSize(NewSize: Longint); override;
  public
    function Read(var Buffer; Count: Longint): Longint; override; // only raises exception
    function Write(const Buffer; Count: Longint): Longint; override; // only raises exception
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ --------------------------- Null CODECS ---------------------------- }

  TNullCoDecStream = class(TCODECStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ---------------------- Cut part of an stream ----------------------- }

  TPartialStream = class(TConnectableStream)
  private
    FStart,FEnd: Longint;
  public
    constructor Create(AStart,AnEnd: Longint);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ---------------------- Stream helper functions --------------------- }

procedure write_s(stream:TStream;const s:string);
procedure writeln_s(stream:TStream;const s: string);
function  readln_s(stream:TStream):string;

procedure CopyStream(InStream,OutStream:TStream);
procedure CopyStreamMult(InStream:TStream;OutStreams:array of TStream);

{ ------------------------} implementation { ------------------------- }

uses
  xp1,fileio,sysutils
  {$IFDEF Delphi}
  {$IFDEF Kylix}
  {$ELSE}
  ,strutils
  {$ENDIF}
  {$ENDIF}
  ,typeform
  ;

{ ------------------- Use PASCAL 'file' as stream -------------------- }

{$I-}
procedure TPascalFileStream.SetSize(NewSize: Longint);
var curPos: Longint;
    OldSize: Longint;
    AddSize: Integer;

    Buffer: array [1..4096] of byte;
    Counter: Integer;

begin
  IOResult;

  OldSize := System.FileSize(FPFile^);                  IOExcept(EStreamError);
  if OldSize=NewSize then exit;

  curPos := System.FilePos(FPFile^);                    IOExcept(EStreamError);

  if NewSize>OldSize then
  begin
    System.Seek(FPFile^,System.FileSize(FPFile^));      IOExcept(EStreamError);

    AddSize:=NewSize-OldSize;
    if AddSize>High(Buffer)-Low(Buffer) then
      AddSize:=High(Buffer)-Low(Buffer);

    for Counter := Low(Buffer) to Low(Buffer)+AddSize do
      Buffer[Counter]:=0;

    while OldSize<NewSize do
    begin
      AddSize:=NewSize-OldSize;
      if AddSize>High(Buffer)-Low(Buffer) then
        AddSize:=High(Buffer)-Low(Buffer);
      System.BlockWrite(FPFile^,Buffer,AddSize,AddSize); IOExcept(EStreamError);
      OldSize:=OldSize+AddSize;
    end;
  end else
  begin
    System.Seek(FPFile^,NewSize);                       IOExcept(EStreamError);
    System.Truncate(FPFile^);                           IOExcept(EStreamError);

    if curPos>NewSize then
      curPos:=NewSize;
  end;

  System.Seek(FPFile^,curPos);                          IOExcept(EStreamError);
end;

constructor TPascalFileStream.Create(var AFile: file);
begin
  FPFile := @AFile;
end;

function TPascalFileStream.Read(var Buffer; Count: Longint): Longint;
var R: Integer;
begin
  IOResult;
  System.BlockRead(FPFile^,Buffer,Count,R);             IOExcept(EReadError);
  Result := R;
end;

function TPascalFileStream.Write(const Buffer; Count: Longint): Longint;
var
  R: Integer;
  Buffervar: Pointer;
begin
  IOResult;
  Buffervar := @Buffer; // fpc gets error if assigning const to var-param
  System.BlockWrite(FPFile^,Buffervar^,Count,R);
  IOExcept(EWriteError);
  Result := R;
end;

function TPascalFileStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  IOResult;

  case Origin of
    soFromBeginning:    Result:=Offset;
    soFromCurrent:      begin Result:=System.FilePos(FPFile^)+Offset;
                                                        IOExcept(EWriteError); end;
    soFromEnd:          begin Result:=System.FileSize(FPFile^)+Offset;
                                                        IOExcept(EWriteError); end;
  end;

  if (Offset=0) and (Origin=soFromCurrent) then
    exit;

  System.Seek(FPFile^,Result);                          IOExcept(EWriteError);
end;

{ ------------------- Deletes file at destruction -------------------- }

constructor TTemporaryFileStream.Create;
begin
  FFileName := TempS(10000);
  MakeFile(FFileName);
  inherited Create(FFileName,fmOpenReadWrite);
end;

constructor TTemporaryFileStream.Create(const FileName: string; Mode: System.Word);
begin
  FFileName := FileName;
  inherited Create(FileName,Mode);
end;

// constructor TTemporaryFileStream.Create(const FileName: string; Mode: System.Word; Rights: Cardinal);
// begin
//   FFileName := FileName;
//   inherited Create(FileName,Mode,Rights);
// end;

destructor TTemporaryFileStream.Destroy;
begin
  inherited;
  DeleteFile(FFileName);
end;

constructor TPartialStream.Create(AStart,AnEnd: Longint);
begin
  FStart:=AStart;
  FEnd:=AnEnd;
end;

function TPartialStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (OtherStream.Position + Count) > FEnd then
    Count := FEnd - OtherStream.Position;

  if Count<=0 then 
    result := 0
  else
    result := OtherStream.Read(Buffer,Count);
end;

function TPartialStream.Write(const Buffer; Count: Longint): Longint;
begin
  if (OtherStream.Position + Count) > FEnd then
    raise EStreamError.Create('too much data');

  if Count<=0 then 
    result := 0
  else
    result := OtherStream.Write(Buffer,Count);
end;

function TPartialStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  case Origin of
  soFromBeginning: Offset := Offset+FStart;
  soFromCurrent:   if Offset=0 then begin
                     Result := OtherStream.Position-FStart;
                     exit;
                   end else
                     Offset := Offset+OtherStream.Position;
  soFromEnd:       Offset := Offset+FEnd;
  else raise EStreamError.Create('Illegal stream operation');
  end;

  if Offset < FStart then Offset:=FStart else
  if Offset > FEnd   then Offset:=FEnd;

  result := OtherStream.Seek(Offset,soFromBeginning) - FStart;
end;

{ ---------------- Streams that can own other streams ---------------- }

constructor TConnectableStream.Create;
begin
  FOtherStream:=nil;
  FDestroyOtherStream:=false;
  FOtherStreamStartPos:=0;
end;

procedure TConnectableStream.Connect(AnOtherStream:TStream);
begin
  if FDestroyOtherStream and assigned(FOtherStream) then
  begin
    SetSize(0);
    FOtherStream.Free;
  end;

  Assert(Assigned(AnotherStream));

  FOtherStream:=AnOtherStream;
  FDestroyOtherStream:=false;
  try FOtherStreamStartPos:=FOtherStream.Position; except FOtherStreamStartPos:=-1; end;
end;

destructor TConnectableStream.Destroy;
begin
  if assigned(FOtherStream) then
    SetSize(0);

  if assigned(FOtherStream) and FDestroyOtherStream then
    FOtherStream.Free;

  inherited;
end;

procedure ConnectStream(var AStream:TStream;AFilter:TConnectableStream);
begin
  if not assigned(AFilter) then
    exit;

  if AFilter is TNullCoDecStream then
  begin
  // If the filter stream is a null codec, just destroy it and keep
  // using the original stream
    AFilter.Free;
    exit;
  end else
  if AStream is TNullCoDecStream then
  begin
  // If the original stream is a null codec, just copy it's properties,
  // destroy it, and use the filter stream
    AFilter.OtherStream := TNullCoDecStream(AStream).OtherStream;
    AFilter.DestroyOtherStream := TNullCoDecStream(AStream).DestroyOtherStream;

    TNullCoDecStream(AStream).DestroyOtherStream := false;
    TNullCoDecStream(AStream).Free;

    AStream := AFilter;
  end else
  begin
  // If original and filter streams are not null codecs, connect them.
    AFilter.OtherStream := AStream;
    AFilter.DestroyOtherStream := true;
    AStream := AFilter;
  end;
end;

procedure UnConnectStream(var AStream:TStream);
var Temp:TConnectableStream;
begin
  Temp := (AStream as TConnectableStream);

  if Temp.DestroyOtherStream then
    AStream := Temp.OtherStream
  else
  begin
    AStream := TNullCodecStream.Create;
    TNullCodecStream(AStream).OtherStream := Temp.OtherStream;
    TNullCodecStream(AStream).DestroyOtherStream := false;
  end;

  Temp.DestroyOtherStream := false;
  Temp.Free;
end;

{ --------------------------- CODEC streams -------------------------- }

procedure TCODECStream.SetSize(NewSize: Longint);
begin
  if NewSize<>0 then
    raise EStreamError.Create('Stream does not allow to set size');
  FPosition:=0;
end;

{$HINTS OFF}{$WARNINGS OFF}
function TCODECStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EReadError.Create('Stream does not support reading.');
end;

function TCODECStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EWriteError.Create('Stream does not support writing.');
end;
{$WARNINGS ON}{$HINTS ON}

function TCODECStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  Result := FPosition;

  if (Origin = soFromBeginning) and (Offset = 0) then 
  begin
    FOtherStream.Seek(soFromBeginning, FOtherStreamStartPos);
    SetSize(0);
    Result:=0;    
  end else 
  if not (
    ((Origin = soFromCurrent  ) and (Offset = 0     )) or
    ((Origin = soFromBeginning) and (Offset = Result)) ) then
    raise EStreamError.Create('Stream does not support seeking.');
end;

function TNullCoDecStream.Write(const Buffer; Count: Longint): Longint;
begin Result := FOtherStream.Write(Buffer,Count); end;

function TNullCODECStream.Read(var Buffer; Count: Longint): Longint;
begin Result := FOtherStream.Read(Buffer,Count); end;

function TNullCODECStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin Result := FOtherStream.Seek(Offset,Origin); end;

{ ---------------------- Stream helper functions --------------------- }

procedure write_s(stream:TStream;const s:string);
begin
  if length(s)>0 then stream.WriteBuffer(s[1],length(s));
end;

procedure writeln_s(stream:TStream;const s: string);
begin
  write_s(stream,s+#13#10);
end;

function readln_s(stream:TStream):string;
const
  DefaultLength = 16;
var
  i: Integer;
begin
  SetLength(Result, DefaultLength);
  try
    Stream.ReadBuffer(Result[1], 2);
    i := 2;
    while (Result[i] <> #10) and (Result[i-1] <> #13) do
    begin
      Inc(i);
      stream.ReadBuffer(Result[i], 1);
      if (i mod DefaultLength)=0 then
        SetLength(Result, ((i div DefaultLength) + 1) * DefaultLength);
    end;
  except
    Result := '';
    raise;
    exit;
  end;
  SetLength(Result,i-2);
end;

procedure CopyStream(InStream,OutStream:TStream);
var b: array [1..32768] of char;
    n: longint;
begin
  while InStream is TNullCodecStream do
    InStream:=TNullCodecStream(InStream).OtherStream;
  while OutStream is TNullCodecStream do
    OutStream:=TNullCodecStream(OutStream).OtherStream;

  repeat
    n := InStream.Read(b,sizeof(b));
    if n<= 0 then break;
    OutStream.WriteBuffer(b,n);
  until false;
end;

procedure CopyStreamMult(InStream:TStream;OutStreams:array of TStream);
var b: array [1..8192] of char;
    n: longint;
    i: integer;
begin
  while InStream is TNullCodecStream do
    InStream:=TNullCodecStream(InStream).OtherStream;
  for i:=Low(OutStreams) to High(OutStreams) do
    while OutStreams[i] is TNullCodecStream do
      OutStreams[i]:=TNullCodecStream(OutStreams[i]).OtherStream;

  repeat
    n := InStream.Read(b,sizeof(b));
    if n<= 0 then break;
    for i:=Low(OutStreams) to High(OutStreams) do
      OutStreams[i].WriteBuffer(b,n);
  until false;
end;

{ --------------------------------------------------------------- } end.
