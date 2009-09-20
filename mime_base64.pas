{ $Id: mime_base64.pas,v 1.11 2003/11/15 14:32:41 cl Exp $

  OpenXP MIME Library: Quoted-Printable en-/decoding
  Copyright (C) 2001 OpenXP team (www.openxp.de) and Claus F"arber

  This file is derieved from parts of the Free Component Library (FCL)
  Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl
  base64 encoder & decoder (c) 1999 Sebastian Guenther

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

unit mime_base64;

interface

uses
  classes, xpstreams, mime;

function EncodeBase64(var source; len: integer): string;

{ --------------------- Encoding/Decoding Streams -------------------- }

type
  TBase64EncoderStream = class(TMimeTransferEncoderStream)
  protected
    LineLength: Cardinal;
    Buf: array[0..2] of Byte;
    BufSize: Integer;    // # of bytes used in Buf
  protected
    destructor Destroy; override;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize:Longint); override;
  end;

  TBase64DecoderStream = class(TMimeTransferDecoderStream)
  protected
    Buf: Byte;          // unread bits
    BufBits: ShortInt;  // number of unread bits
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize:Longint); override;
  end;

{ --------------------- Encoding/Decoding Tables --------------------- }

const
  Base64EncodingTable: array[0..63] of char =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  Base64DecodingTable: array[Byte] of shortint =
  ( -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1,

    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 );

implementation

uses
  SysUtils, typeform, xpglobal;

//todo: drop this procedure?
function EncodeBase64(var source; len: integer): string;
var
  i, j, l:    integer;
  b:          array[0..3] of byte;
  bytestream: array[0..63] of byte;
begin
  result:='';
  if len=0 then exit;
  fillchar(bytestream,sizeof(bytestream),0);
  move(source,bytestream,len);

  l:=0;
  for i:=0 to (len-1) div 3 do begin
    inc(l,3);
    if l>len then l:=len;
    b[0]:=(bytestream[i*3] and $fc) shr 2;
    b[1]:=((bytestream[i*3] and $03) shl 4)
          or ((bytestream[i*3+1] and $f0) shr 4);
    b[2]:=((bytestream[i*3+1] and $0f) shl 2)
          or ((bytestream[i*3+2] and $c0) shr 6);
    b[3]:=bytestream[i*3+2] and $3f;
    for j:=0 to (l-1) mod 3+1 do
     result:=result+Base64EncodingTable[b[j]];
    for j:=1 to 2-(l-1) mod 3 do
     result:=result+'=';
  end;
end;

{ TBase64EncoderStream }

procedure TBase64EncoderStream.SetSize(NewSize:Longint);
var WriteBuf: array[0..3] of Char;
begin
  if NewSize = 0 then
  begin
    case BufSize of
      1: begin
          WriteBuf[0] := Base64EncodingTable[Buf[0] shr 2];
          WriteBuf[1] := Base64EncodingTable[(Buf[0] and 3) shl 4];
          WriteBuf[2] := '=';
          WriteBuf[3] := '=';
          OtherStream.WriteBuffer(WriteBuf, 4);
        end;
      2: begin
          WriteBuf[0] := Base64EncodingTable[Buf[0] shr 2];
          WriteBuf[1] := Base64EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
          WriteBuf[2] := Base64EncodingTable[(Buf[1] and 15) shl 2];
          WriteBuf[3] := '=';
          OtherStream.WriteBuffer(WriteBuf, 4);
        end;
      3: assert(false);
    end;
    FPosition := 0;
    BufSize := 0;
  end else
    assert(NewSize = FPosition);
end;

function TBase64EncoderStream.Write(const Buffer; Count: Longint): Longint;
var
  ReadNow: LongInt;
  p: Pointer;
  WriteBuf: array[0..3] of Char;
begin
  Inc(FPosition, Count);
  Result := Count;

  p := @Buffer;
  while count > 0 do begin
    // Fetch data into the Buffer
    ReadNow := 3 - BufSize;
    if ReadNow > Count then break;    // Not enough data available
    Move(p^, Buf[BufSize], ReadNow);
//  Inc(p, ReadNow);
    p:=PChar(p)+ReadNow;

    Dec(Count, ReadNow);

    // Encode the 3 bytes in Buf
    WriteBuf[0] := Base64EncodingTable[Buf[0] shr 2];
    WriteBuf[1] := Base64EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
    WriteBuf[2] := Base64EncodingTable[(Buf[1] and 15) shl 2 or (Buf[2] shr 6)];
    WriteBuf[3] := Base64EncodingTable[Buf[2] and 63];
    OtherStream.WriteBuffer(WriteBuf, 4);
    Inc(LineLength,4);

    if LineLength>=76 then
    begin
      WriteBuf[0]:=#13;
      WriteBuf[1]:=#10;
      OtherStream.Write(WriteBuf,2);
      LineLength:=0;
    end;

    BufSize := 0;
  end;
  Move(p^, Buf[BufSize], count);
  Inc(BufSize, count);
end;

destructor TBase64EncoderStream.Destroy;
begin
  try
    SetSize(0);
  finally
    inherited;
  end;
end;

{ TBase64DecoderStream }

procedure TBase64DecoderStream.SetSize(NewSize:Longint);
begin
  inherited;
  BufBits:=0;
end;

function TBase64DecoderStream.Read(var Buffer; Count: Longint): Longint;
var b:Byte;
    d:ShortInt;

    BBuf:array[0..8191] of Byte;
    BBeg,BEnd: Longint;

    r: Longint;

  function GetByte: Boolean;
  var ReadCount: longint;
  begin
    if BBeg>=BEnd then
    begin
      ReadCount:=((Count-R)*8-BufBits+5)div 6;
      ReadCount:=Min(ReadCount,High(BBuf)-Low(BBuf)+1);
      ReadCount:=Max(ReadCount,1);
      ReadCount:=OtherStream.Read(BBuf[Low(BBuf)],ReadCount);
      BBeg:=Low(BBuf);
      BEnd:=Low(BBuf)+ReadCount;
    end;

    if BBeg<BEnd then
    begin
      b:=BBuf[BBeg];
      inc(BBeg);
      result:=true;
    end else
      result:=false;
  end;

  procedure AddByte(a:Byte);
  begin
    ((PChar(@Buffer))+R)^:=Chr(a);
    Inc(R);
  end;

begin
  R:=0;
  BBeg:=0;
  BEnd:=-1;

  while R<Count do
  begin
    if not GetByte then
      break;

    d:=Base64DecodingTable[b];

    if d>=0 then case BufBits of
      0: begin Buf:=d; BufBits:=6; end;
      6: begin AddByte((Byte(Buf) shl 2) or (Byte(d) shr 4)); Buf:=Byte(d)and $F; BufBits:=4; end;
      4: begin AddByte((Byte(Buf) shl 4) or (Byte(d) shr 2)); Buf:=Byte(d)and $3; BufBits:=2; end;
      2: begin AddByte((Byte(Buf) shl 6) or Byte(d)); BufBits:=0; end;
    end;
  end;

  Inc(FPosition,R);
  Assert(BBeg<=BEnd);
  Result := R;
end;

//
// $Log: mime_base64.pas,v $
// Revision 1.11  2003/11/15 14:32:41  cl
// - Fixed Base64 encoding for file sizes mod 3 <> 0
//
// Revision 1.10  2003/08/28 00:39:41  mk
// - lowercase uses "typeform" for Kylix
//
// Revision 1.9  2002/12/14 09:25:17  dodi
// - removed gpltools and encoder units
//
// Revision 1.8  2002/12/06 14:27:26  dodi
// - updated uses, comments and todos
//
// Revision 1.7  2002/12/04 16:57:00  dodi
// - updated uses, comments and todos
//
// Revision 1.6  2002/02/28 21:26:09  cl
// - fixed Base64 decoder
//
// Revision 1.5  2001/09/11 14:22:15  cl
// - BUGFIX: Reading small amounts of data with TBase64DecoderStream.Read
//
// Revision 1.4  2001/09/10 15:58:01  ml
// - Kylix-compatibility (xpdefines written small)
// - removed div. hints and warnings
//
// Revision 1.3  2001/09/09 17:40:47  cl
// - moved common code between alle en-/decoding streams to a base class
// - all en-/decoding streams can now destruct the other stream
// - much more elegant way to connect en-/decoding streams to each other
//
// Revision 1.2  2001/09/08 18:46:43  cl
// - small bug/compiler warning fixes
//
// Revision 1.1  2001/09/08 15:06:14  cl
// - Moved MIME functions/types/consts to mime*.pas
//

{ ------------------------------} end.
