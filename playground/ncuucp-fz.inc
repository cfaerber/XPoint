{  $Id$

   OpenXP UUCP netcall 'f'/'z' protocol include file
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


function fz_SendCommand(s:string):boolean;  { CR-terminierten Befehl senden }
begin
  SendStr(s+#13);
  fz_SendCommand:=not NoCarrier;
end;


function fz_GetCommand:string;              { CR-terminierten Befehl holen }
var s   : string;
    l,b : byte;
begin
  l:=0;
  time(eProtTimeout);
  b:=255;
  repeat
    if receive(comnr,b) then begin
      time(eProtTimeout);
      if (b>=32) and (l<255) then begin
        inc(l);
        s[l]:=chr(b);
        end;
      end
    else begin
      multi2;
      testbrk;
      end;
  until (b=13) or timeout(true) or break;
  if timeout(true) then l:=0;
  setlength(s,l);
  fz_GetCommand:=s;
end;


procedure WrzSize;
begin
  if ParDebug then begin
    write(#13,transdata.transferred,' Bytes');
    writeln(deblog^,strs(transdata.transferred)+' Bytes');
    end
  else
    WriteTransfer;
end;


function fz_SendFile(fprot:boolean; fn:pathstr; offset:longint):shortint;
const bufsize = 1024;                                     { Datei senden }
var f   : file;
    buf : array[0..bufsize-1] of byte;
    rr  : word;
    i   : integer;
    chk : word;
    b   : byte;
    cmd : string[10];
begin
  assign(f,fn);
  resetfm(f,0);
  seek(f,offset);
  transdata.filesize:=filesize(f)-offset;
  WriteTransfer;         { Groesse anzeigen }
  chk:=$ffff;
  if not NoCarrier then
    repeat
      blockread(f,buf,bufsize,rr);
      inc(transdata.transferred,rr);
      inc(transdata.total,rr);
      transdata.blocksize:=rr;
      for i:=0 to rr-1 do begin
        b:=buf[i];
        { R-}
        if chk>$7fff then chk:=chk shl 1 + 1
        else chk:=chk shl 1;
        inc(chk,b);
        if fprot then                        { f-Protokoll }
          case b of
              0..$1f : begin SendByte($7a); SendByte(b+$40); end;
            $20..$79 : SendByte(b);
            $7a..$7f : begin SendByte($7b); SendByte(b-$40); end;
            $80..$9f : begin SendByte($7c); SendByte(b-$40); end;
            $a0..$f9 : begin SendByte($7d); SendByte(b-$80); end;
            $fa..$ff : begin SendByte($7e); SendByte(b-$c0); end;
          end
        else
          if (b<$7a) or (b>$7f) then         { z-Protokoll }
            SendByte(b)
          else begin
            SendByte($7b);                   { $7a..$7f escapen }
            SendByte(b-$40);
            end;
        { R+}
        end;
      WrzSize;
    until eof(f) or NoCarrier or break;
  if not NoCarrier then
    SendStr(#$7e#$7e+hex(chk,4)+#13);      { Checksumme senden }
  close(f);
  WrdLn;
  if NoCarrier then
    fz_SendFile:=fileError
  else begin
    cmd:=fz_GetCommand;
    if cmd='G' then fz_SendFile:=fileOK else
    if cmd='R' then fz_SendFile:=fileRepeat
    else fz_SendFile:=fileError;
    end;
end;


function fz_RecFile(fprot:boolean; fn:pathstr):shortint;
const bufsize = 1024;
var b       : byte;
    f       : file;
    buf     : array[0..bufsize-1] of byte;
    bp,chk  : word;
    ende    : boolean;
    fehler  : boolean;
    special : byte;
    chex    : string[5];
    len     : byte;
    ftyped  : boolean;

  procedure putchar(b:byte);
  begin
    buf[bp]:=b; inc(bp);
    { R-}
    if chk>$7fff then chk:=chk shl 1 + 1
    else chk:=chk shl 1;
    inc(chk,b);
    { R+}
  end;

  procedure wrongbyte;
  begin
    wrldebug(' got wrong byte: '+lowercase(hex(b,2)));
  end;

  procedure fescape(range0,range1:byte; ofs:integer);
  begin
    if fprot then begin
      if (b>=range0) and (b<=range1) then inc(b,ofs)
      else wrongbyte;
      putchar(b);
      special:=0;
      end
    else begin
      wrongbyte;
      if (b>=$7a) and (b<=$7e) then
        special:=b
      else begin
        putchar(b);
        special:=0;
        end;
      end;
  end;

begin
  assign(f,fn);
  rewrite(f,1);
  time(eProtTimeout);
  bp:=0;
  ende:=false; fehler:=false; ftyped:=false;
  chk:=$ffff; special:=0;

  while not (ende or fehler or timeout(true) or break) do begin
    if receive(comnr,b) then begin
      case special of
        0   : if (b>=$7a) and (b<=$7e) then    { 0 oder anderes Zeichen }
                special:=b
              else begin
                putchar(b);
                special:=0;
                end;
        $7a : fescape($40,$5f,-$40);
        $7b : begin
                if (b>=$3a) and (b<=$3f) then inc(b,$40)
                else wrongbyte;
                putchar(b);
                special:=0;
              end;
        $7c : fescape($40,$5f,$40);
        $7d : fescape($20,$79,$80);
        $7e : if b=$7e then begin      { Ende }
                len:=0;
                while not timeout(true) and (len<5) do
                  if receive(comnr,b) then begin
                    inc(len); chex[len]:=chr(b and $7f);
                    end;
                setlength(chex,len);
                wrldebug('Got Checksum: '+LeftStr(chex,4)+'  expected: '+lowercase(hex(chk,4)));
                if timeout(true) or (chex[5]<>#13) or
                   (hexval(LeftStr(chex,4))<>chk) then
                  fehler:=true
                else
                  ende:=true;
                end
              else
                fescape($3a,$3f,$c0);
        else  begin
                wrongbyte;
                putchar(b);
                special:=0;
              end;
      end;
      time(eProtTimeout);
      end
    else begin
      multi2;
      testbrk;
      end;
    if not ftyped and (bp>110) then begin
      ShowFtype(buf,bp);
      ftyped:=true;
      end;
    if (bp=bufsize) or ende then begin
      inc(transdata.transferred,bp);
      inc(transdata.total,bp);
      transdata.blocksize:=bp;
      WrzSize;
      blockwrite(f,buf,bp);
      bp:=0;
      end;
    end;    { while }

  if bp>0 then begin
    WrzSize;
    blockwrite(f,buf,bp);
    end;
  fz_Recfile:=fileError;
  if not NoCarrier then
    if timeout(false) or fehler then
      if FileRetries<iif(filesize(f)<10000,4,3) then begin
        flushserial(1);
        wrldebug('File error - Repeating ...');
        if fz_SendCommand('R') then     { Repeat }
          fz_Recfile:=fileRepeat;
        end
      else begin
        wrldebug('Error - quitting ...');
        if fz_SendCommand('Q') then;    { Quit }
        end
    else begin
      wrldebug('File good');
      if fz_SendCommand('G') then;      { Good }
      fz_Recfile:=fileOK;
      end;
  close(f);
  WrdLn;
end;


function f_InitProtocol:boolean;
begin
  mdelay(1000);
  f_Initprotocol:=true;
end;

procedure f_ExitProtocol;
begin
  mdelay(1000);
end;

{
  $Log$
  Revision 1.1  2001/01/19 18:00:00  ma
  - added TUUCPNetcall sources (from uucico)

  ----- moved to playground
}
