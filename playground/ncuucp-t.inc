{  $Id$

   OpenXP UUCP netcall 't' protocol include file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de) and Claus Färber
   Copyright (C) 1991-1999 Peter Mandrella (www.crosspoint.de)

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

type TUUCProtocolT = class(TUUCProtocolSimple)
  private
    procedure SendCommand(s:string);                override;
    function  GetCommand            : string;       override;
    procedure SendFile(var f:file; offset:longint); override;
    procedure RecFile (var f:file);                 override;
end;

procedure TUUCProtocolT.SendCommand(s:string);
begin
  s:=s+StringOfChar(#0,512-(length(s) mod 512));
  CommObj.SendString(s,false);
  Netcall.TestBreak;
end;

function TUUCProtocolT.GetCommand:string;
const bsize = 512;
var   block: packed array [0..(bsize-1)] of char;
      len  : longint;
begin
  block[bsize-1]:=#0;
  repeat
    repeat
      len:=CommObj.CharCount;
      Netcall.TestBreak;
      mdelay(0);
    until len>=bsize;

    CommObj.ReadBlock(block,bsize,len);
    AppendStr(result,block);
  until block[bsize-1]=#0;
end;

procedure TUUCProtocolT.SendFile(var f:file; offset:longint);
var   rd   : LongInt;
      sd   : LongInt;
      buf  : packed record
        len: Integer32;
        dat: packed array[0..1023] of char;
      end;
begin
  seek(f,offset);

  repeat
    BlockRead(f,buf.dat,1024,rd);
    buf.len:=HostToBigEndian32(rd);
    CommObj.SendBlock(buf,4+rd,sd);
    FileAdvance(buf.dat,rd);
    Netcall.TestBreak;
  until eof(f);

  buf.len:=HostToBigEndian32(0);
  CommObj.SendBlock(buf,4,sd);
end;

procedure TUUCProtocolT.RecFile(var f:file);
var rd:  LongInt;
    len: LongInt;
    dat: packed array[1..1024] of char;
begin
  seek(f,0); truncate(f);

  repeat
    CommObj.ReadBlock(len,4,rd);
    Netcall.TestBreak;

    len:=BigEndianToHost32(len);
    if len=0 then  
      exit else
    if (len<0) or (len>1024) then 
      raise EUUCProtocol.Create('UUCP-t: Illegal block size: '+StrS(Len));

    CommObj.ReadBlock(dat,len,rd);
    BlockWrite(f,dat,rd);
    FileAdvance(dat,rd);
    Netcall.TestBreak;
  until false;
end;

{
  $Log$
  Revision 1.3  2001/03/16 23:02:34  cl
  - transfer statistics
  - fixes

  Revision 1.2  2001/03/13 00:21:49  cl
  - first working UUCP-t version

  Revision 1.1  2001/02/28 22:33:38  cl
  - first things for UUCP-t protocol
}
