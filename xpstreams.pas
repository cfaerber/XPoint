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

{$I XPDEFINE.INC }

unit xpstreams;

{ ---------------------------} interface { --------------------------- }

uses
  XPGlobal,
  Classes;

type

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

{ ---------------------- Cut part of an stream ----------------------- }

  TPartialStream = class(TStream)
  private
    FStream: TStream;
    FStart,FEnd: Longint;
  public
    constructor Create(AnOtherStream: TStream;AStart,AnEnd: Longint);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: System.Word): Longint; override;
  end;

{ ---------------------- Stream helper functions --------------------- }

procedure write_s(stream:TStream;const s:string);
procedure writeln_s(stream:TStream;const s: string);
function readln_s(stream:TStream):string;

procedure CopyStream(InStream,OutStream:TStream);
procedure CopyStreamMult(InStream:TStream;OutStreams:array of TStream);

{ ------------------------} implementation { ------------------------- }

uses
  xp1,fileio,sysutils
  {$IFDEF Delphi}
  {$IFDEF Kylix}
  ,IdGlobal
  {$ELSE}
  ,strutils
  {$ENDIF}
  {$ENDIF}
  ;

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
  DeleteFile(FFileName);
  inherited Destroy
end;

constructor TPartialStream.Create(AnOtherStream: TStream;AStart,AnEnd: Longint);
begin
  FStream:=AnOtherStream;
  FStart:=AStart;
  FEnd:=AnEnd;
end;

function TPartialStream.Read(var Buffer; Count: Longint): Longint;
begin
  if (FStream.Position + Count) > FEnd then
    Count := FEnd - FStream.Position;

  if Count<=0 then 
    result := 0
  else
    result := FStream.Read(Buffer,Count);
end;

function TPartialStream.Write(const Buffer; Count: Longint): Longint;
begin
  if (FStream.Position + Count) > FEnd then
    raise EStreamError.Create('too much data');

  if Count<=0 then 
    result := 0
  else
    result := FStream.Write(Buffer,Count);
end;

function TPartialStream.Seek(Offset: Longint; Origin: System.Word): Longint;
begin
  case Origin of
  soFromBeginning: Offset := Offset+FStart;
  soFromCurrent:   if Offset=0 then begin
                     Result := FStream.Position-FStart;
                     exit;
                   end else
                     Offset := Offset+FStream.Position;
  soFromEnd:       Offset := Offset+FEnd;
  else raise EStreamError.Create('Illegal stream operation');
  end;

  if Offset < FStart then Offset:=FStart else
  if Offset > FEnd then Offset:=FEnd;

  result := FStream.Seek(Offset,soFromBeginning) - FStart;
end;

{ ---------------------- Stream helper functions --------------------- }

procedure write_s(stream:TStream;const s:string);
begin
  stream.WriteBuffer(s[1],length(s));
end;

procedure writeln_s(stream:TStream;const s: string);
begin
  write_s(stream,s+#13#10);
end;

function readln_s(stream:TStream):string;

  function GetByte:Char;
  begin
    stream.ReadBuffer(result,1);
  end;

begin
  result:='';
  repeat
    try
      result:=result+GetByte;
    except
      if result='' then raise;
      exit;
    end;
  until RightStr(result,2)=#13#10;
  SetLength(Result,Length(Result)-2);
end;

procedure CopyStream(InStream,OutStream:TStream);
var b: array [1..8192] of char;
    n: longint;
begin
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
  repeat
    n := InStream.Read(b,sizeof(b));
    if n<= 0 then break;
    for i:=Low(OutStreams) to High(OutStreams) do
      OutStreams[i].WriteBuffer(b,n);
  until false;
end;

end.
