{  $Id$

   OpenXP UUCP netcall 'e' protocol include file
   Copyright (C) 2000-2001 OpenXP team (www.openxp.de)
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

function e_SendCommand(s:string):boolean;   { 0-terminierten Befehl senden }
begin
  SendStr(s+#0);
  e_SendCommand:=not NoCarrier;
end;


function e_GetCommand:string;               { 0-terminierten Befehl holen }
var s   : string;
    l,b : byte;
begin
  l:=0;
  time(eProtTimeout);
  b:=255;
  repeat
    if receive(comnr,b) then begin
      time(eprotTimeout);
      if (b>=32) and (l<255) then begin
        inc(l);
        s[l]:=chr(b);
        end;
      end
    else begin
      multi2;
      testbrk;
      end;
  until (b=0) or timeout(true) or break;
  if timeout(true) or break then l:=0;
  setlength(s,l);
  e_GetCommand:=s;
end;


procedure WreSize;
begin
  if ParDebug then begin
    write(#13,transdata.transferred,' Bytes');
    writeln(deblog^,strs(transdata.transferred)+' Bytes');
    end
  else
    WriteTransfer;
end;


function e_SendFile(fn:pathstr; offset:longint):shortint;   { Datei senden }
const bufsize = 1024;
var f   : file;
    buf : array[0..bufsize-1] of byte;
    rr  : word;
    i   : integer;
begin
  assign(f,fn);
  resetfm(f,0);
  seek(f,offset);
  transdata.filesize:=filesize(f)-offset;
  if not ParDebug then
    WriteTransfer;         { Groesse anzeigen }
  SendStr(LeftStr(strs(transdata.filesize)+dup(20,#0),20));   { Laenge senden }
  if not NoCarrier then
    repeat
      blockread(f,buf,bufsize,rr);
      inc(transdata.transferred,rr);
      inc(transdata.total,rr);
      transdata.blocksize:=rr;
      for i:=0 to rr-1 do
        SendByte(buf[i]);
      WreSize;
    until eof(f) or NoCarrier or break;
  close(f);
  WrdLn;
  e_SendFile:=iif(NoCarrier,fileError,fileOK);
end;


function e_RecFile(fn:pathstr):shortint;
const bufsize = 1024;
var len   : string[20];
    i     : integer;
    fs    : longint;
    b     : byte;
    f     : file;
    buf   : array[0..bufsize-1] of byte;
    bp    : word;
    ftyped: boolean;
begin
  e_RecFile:=fileError;
  time(eProtTimeout);
  i:=0;
  while (i<20) and not timeout(true) and not break do
    if receive(comnr,b) then begin
      inc(i);
      len[i]:=chr(b);
      time(eProtTimeout);
      end
    else begin
      multi2;
      testbrk;
      end;
  if timeout(true) or break then exit;
  while (i>0) and (len[i]=#0) do dec(i);
  setlength(len,i);
  fs:=ival(len);        { Dateigroesse }
  transdata.filesize:=fs;
  WriteTransfer;      { Groesse anzeigen }
  assign(f,fn);
  rewrite(f,1);
  time(eProtTimeout);
  ftyped:=false;
  bp:=0;
  while (fs>0) and not timeout(true) and not break do begin
    if receive(comnr,b) then begin
      buf[bp]:=b; inc(bp);
      dec(fs);
      time(eProtTimeout);
      end
    else
      testbrk;
    if not ftyped and (bp>110) then begin
      ShowFtype(buf,bp);
      ftyped:=true;
      end;
    if (bp=bufsize) or (fs=0) then begin
      inc(transdata.transferred,bp);
      inc(transdata.total,bp);
      transdata.blocksize:=bp;
      WreSize;
      blockwrite(f,buf,bp);
      bp:=0;
      end;
    end;
  if not timeout(true) then
    e_Recfile:=fileOK;
  if bp>0 then begin
    WreSize;
    blockwrite(f,buf,bp);
    end;
  close(f);
  WrdLn;
end;

{
  $Log$
  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground
}
