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
  DebugLog('uucp-t','Sending Command: '+s,dlInform);

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
  result:='';
  repeat
    TimerObj.SetTimeout(DataTimeout);
    while CommObj.CharCount<bsize do
    begin
      len:=CommObj.CharCount;
      Netcall.TestTimeout;
    end;

    CommObj.ReadBlock(block[low(block)],bsize,len);
    AppendStr(result,block);
  until block[high(block)]=#0;
  
  DebugLog('uucp-t','Got Command: '+result,dlInform);
end;

procedure TUUCProtocolT.SendFile(var f:file; offset:longint);
var   rd   : LongInt;
      sd   : LongInt;
      buf  : packed record
        len: Integer32;
        dat: packed array[0..1023] of char;
      end;
begin
  DebugLog('uucp-t','Sending File',dlInform);
 
  while not eof(f) do begin
    Netcall.TestBreak;
    BlockRead(f,buf.dat,1024,rd);
    buf.len:=HostToBigEndian32(rd);
    CommObj.SendBlock(buf,4+rd,sd);
    FileAdvance(buf.dat,rd);
  end;

  buf.len:=HostToBigEndian32(0);
  CommObj.SendBlock(buf,4,sd);
end;

procedure TUUCProtocolT.RecFile(var f:file);
var rd:  LongInt;
    len: LongInt;
    dat: packed array[1..1024] of char;
begin
  DebugLog('uucp-t','Receiving File',dlInform);

  repeat
    Netcall.Timer.SetTimeout(ProtTimeout);
    while CommObj.CharCount<4 do
      Netcall.TestTimeout;
  
    CommObj.ReadBlock(len,4,rd);
    len:=BigEndianToHost32(len);

    if len=0 then
      exit else
    if (len<1) or (len>1024) then
      raise EUUCProtocol.Create('UUCP-t: Illegal block size: '+StrS(Len));
  
    DebugLog('uucp-t','Block: '+StrS(len)+' byte(s)',dlInform);

    repeat
      Netcall.Timer.SetTimeout(DataTimeout);
      
      rd:=CommObj.CharCount;
      while(rd<=0) do begin
        multi2;
 	Netcall.TestTimeout;
	rd:=CommObj.CharCount; 
      end;
    
      CommObj.ReadBlock(dat[low(dat)],min(len,rd),rd);
      BlockWrite(f,dat,rd);
      FileAdvance(dat,rd);
      len:=len-rd;

    until len<=0;

  until false;
end;

{
  $Log$
  Revision 1.3  2001/03/26 22:21:08  cl
  - minor fixes and cleanups

  Revision 1.2  2001/03/25 18:44:04  cl
  - moved ncuucp-fz.inc from playground to main
  - enabled UUCP-f/z in ncuucp.pas
  - cleanups, removed some unnecessary variables
  - some minor fixes

  Revision 1.1  2001/03/24 22:55:29  cl
  - moved from playground to main

  --- import from playground
}
