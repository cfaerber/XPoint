{
  $Id$

  XPLib TStream Utilities

  Copyright (C) 2001-2003 OpenXP Team <www.openxp.de>
  see CVS log below for authors

  This file is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this library; see the file COPYING.  If not, write to the Free Software
  Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$I xpdefine.inc }

{** Utility functions and classes for TStreams.
  @cvs($Date$) }
unit xpstreams;

{ ---------------------------} interface { --------------------------- }

uses
  classes;

{$IFDEF FPC}
  {$MODE Delphi}
  {$IFNDEF ver1_0}
    {$DEFINE SEEK64}
  {$ELSE}
    {$UNDEF SEEK64}
  {$ENDIF}
{$ELSE}
  {$DEFINE SEEK64}
{$ENDIF}

{$IFDEF SEEK64}
{** Type used for file positions }
type filepos_t = Int64;
{$ELSE}
type filepos_t = Longint;
{$ENDIF}

{** @name is the base class for stream objects that use another stream object
  to read from and/or to write to. }
type TConnectableStream = class (TStream)
  protected
    FOtherStream: TStream;
    FDestroyOtherStream: boolean;
    FOtherStreamStartPos: Longint;

  protected
    {** Connect this object to another stream object.
      @param(AnOtherStream is the stream this stream connects to) }
    procedure Connect(AnOtherStream:TStream); virtual;

  public
    {** Creates an instance of @classname . }
    constructor Create; overload;

    {** Destroys an instance of @classname . }
    destructor Destroy; override;

    {** The stream this stream is connected to. }
    property OtherStream: TStream read FOtherStream write Connect;
    {** Indicates whether @link(OtherStream) should be destroyed when
      this instance of @classname is destroyed. }
    property DestroyOtherStream: boolean read FDestroyOtherStream write FDestroyOtherStream;
  end;

{** Replaces a stream with a @link(TConnectableStream) connected to this
  stream.

  This allows putting a @link(TConnectableStream) object "in front" of a stream
  object so that writing to or reading from that object can be filtered by
  the @link(TConnectableStream) object.

  The @link(TConnectableStream) object will destroy the connected stream object
  when it is destroyed.

  @param(AStream is a stream to connect to. It is replaced by the instance of TConnectableStream specified by @link(AFilter).)
  @param(AFilter is an instance of TConnectableStream which is connected to the stream specified by AName)
}
procedure ConnectStream(var AStream:TStream;AFilter:TConnectableStream);

{** Replaces a @link(TConnectableStream) object with the stream object
  connected to.
  This is basically the reverse of the @link(ConnectStream) procedure.
  @param(AStream is the instance of TConnectableStream. It is replaced by the stream owned by that object.)
}
procedure UnConnectStream(var AStream:TStream);

{ ---------------------- Stream helper functions --------------------- }

{** Write a string to a stream object }
procedure write_s(stream:TStream;const s:string);

{** Write a string, followed by CRLF (#10#13) to a stream object. }
procedure writeln_s(stream:TStream;const s: string);

{** Read a line, terminated by CRLF (#10#13) from a stream object. Please note
  that this function does not perform well on unbuffered streams. }
function  readln_s(stream:TStream):string;

{** Copies data from InStream to OutStream }
procedure CopyStream(InStream,OutStream:TStream);

{** Copies data from InStream to all OutStreams }
procedure CopyStreamMult(InStream:TStream;OutStreams:array of TStream);

{ ------------------------} implementation { ------------------------- }

uses
  sysutils,
  xpstreams_codec;

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
//  SetSize(0);
    FOtherStream.Free;
  end;

  Assert(Assigned(AnotherStream));

  FOtherStream:=AnOtherStream;
  FDestroyOtherStream:=false;
  try FOtherStreamStartPos:=FOtherStream.Position; except FOtherStreamStartPos:=-1; end;
end;

destructor TConnectableStream.Destroy;
begin
//if assigned(FOtherStream) then
//  SetSize(0);

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
var b: array [1..65536] of char;
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

//
// $Log$
// Revision 1.2  2003/08/26 22:56:18  cl
// - fixes for Free PASCAL
//
// Revision 1.1  2003/08/26 22:46:31  cl
// - moved xpstreams to xplib/
// - split xpstreams into individual small files to remove some dependencies
// - added pasdoc documentation
//
end.
